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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: versailles_reparations_clauses__punitive_liability_reading
 *   human_readable: Versailles Reparations: Punitive Liability Reading
 *   domain: international_relations/legal_history/political_economy
 *
 * SUMMARY:
 *   This constraint story instantiates the 'punitive liability' reading of
 *   the Versailles reparations clauses. This reading asserts that Germany
 *   bore unique moral and financial responsibility for the total costs of
 *   World War I, and that Article 231 of the Treaty of Versailles provided a
 *   legitimate legal basis for imposing quasi-unlimited reparations claims.
 *   This perspective emphasizes justice for the victors and compensation for
 *   damages, often downplaying Germany's economic capacity or the long-term
 *   destabilizing effects of the demands.
 *
 * KEY AGENTS:
 *   - Allied_creditor_states: Primary beneficiary/agenda_setter (institutional/arbitrage) — imposed and collected reparations.
 *   - German_government: Primary target/payer (institutional/trapped) — forced to accept and implement reparations.
 *   - German_workers_taxpayers: Ultimate target/payer (powerless/trapped) — bore the economic burden.
 *   - International_financial_institutions: Observer (institutional/analytical) — monitored and advised on payments.
 *   - German_nationalists: Excluded (organized/identity_locked) — rejected the treaty's legitimacy.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(versailles_reparations_clauses__punitive_liability_reading, 0.85).
domain_priors:suppression_score(versailles_reparations_clauses__punitive_liability_reading, 0.9).
domain_priors:theater_ratio(versailles_reparations_clauses__punitive_liability_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(versailles_reparations_clauses__punitive_liability_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(versailles_reparations_clauses__punitive_liability_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(versailles_reparations_clauses__punitive_liability_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(versailles_reparations_clauses__punitive_liability_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(versailles_reparations_clauses__punitive_liability_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(versailles_reparations_clauses__punitive_liability_reading, tangled_rope).
narrative_ontology:human_readable(versailles_reparations_clauses__punitive_liability_reading, "Versailles Reparations: Punitive Liability Reading").
narrative_ontology:topic_domain(versailles_reparations_clauses__punitive_liability_reading, "international_relations/legal_history/political_economy").

domain_priors:requires_active_enforcement(versailles_reparations_clauses__punitive_liability_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(versailles_reparations_clauses__punitive_liability_reading, 'af454b8d-2776-44c3-ae1b-07c92ebc5f67').
narrative_ontology:cs_kernel_codification('af454b8d-2776-44c3-ae1b-07c92ebc5f67', fixed_text).
narrative_ontology:cs_authority_grounding('af454b8d-2776-44c3-ae1b-07c92ebc5f67', extraction).
narrative_ontology:cs_interpretation_layer_present('af454b8d-2776-44c3-ae1b-07c92ebc5f67').
narrative_ontology:cs_reading_relation('af454b8d-2776-44c3-ae1b-07c92ebc5f67', versailles_reparations_clauses__limited_responsibility_reading, coexists_with).
narrative_ontology:cs_reading_relation('af454b8d-2776-44c3-ae1b-07c92ebc5f67', versailles_reparations_clauses__repudiation_reading, coexists_with).
narrative_ontology:cs_axiom('af454b8d-2776-44c3-ae1b-07c92ebc5f67', foundational, germany_solely_responsible_for_war).
narrative_ontology:cs_axiom_status(germany_solely_responsible_for_war, holdable).
narrative_ontology:cs_axiom_grounding('af454b8d-2776-44c3-ae1b-07c92ebc5f67', germany_solely_responsible_for_war, conventional).
narrative_ontology:cs_axiom('af454b8d-2776-44c3-ae1b-07c92ebc5f67', foundational, reparations_as_punishment_and_compensation).
narrative_ontology:cs_axiom_status(reparations_as_punishment_and_compensation, holdable).
narrative_ontology:cs_axiom_grounding('af454b8d-2776-44c3-ae1b-07c92ebc5f67', reparations_as_punishment_and_compensation, deontological).
narrative_ontology:cs_reference_frame('af454b8d-2776-44c3-ae1b-07c92ebc5f67', punitive_justice_framework).
narrative_ontology:cs_drift_state('af454b8d-2776-44c3-ae1b-07c92ebc5f67', post_great_depression_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('af454b8d-2776-44c3-ae1b-07c92ebc5f67', '').
narrative_ontology:cs_kernel_id(versailles_reparations_clauses__punitive_liability_reading, versailles_reparations_clauses).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(versailles_reparations_clauses__punitive_liability_reading, allied_creditor_states).
narrative_ontology:constraint_victim(versailles_reparations_clauses__punitive_liability_reading, german_workers_taxpayers).
narrative_ontology:constraint_victim(versailles_reparations_clauses__punitive_liability_reading, german_government).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% As the victors of WWI, they imposed the Treaty of Versailles, including Article 231, which they interpreted as assigning sole war guilt to Germany and justifying extensive reparations. They actively enforced collection mechanisms and benefited from the transfer of German economic output.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__punitive_liability_reading, allied_creditor_states, agenda_setter,
    institutional, generational, arbitrage, global).

% The Weimar Republic was forced to accept the treaty terms under duress, leading to immense political instability and economic hardship as it attempted to meet the reparations schedule. Its fiscal sovereignty was subordinated to external claims.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__punitive_liability_reading, german_government, payer,
    institutional, immediate, trapped, national).

% Bore the ultimate economic burden of reparations through increased taxes, inflation, and reduced public services, leading to widespread poverty and resentment. Their ability to exit the system was non-existent.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__punitive_liability_reading, german_workers_taxpayers, payer,
    powerless, biographical, trapped, national).

% Monitored Germany's economic capacity, facilitated loans (e.g., Dawes and Young Plans) to enable payments, and advised on financial stability, often caught between the punitive demands of creditors and Germany's economic realities.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__punitive_liability_reading, international_financial_institutions, observer,
    institutional, biographical, analytical, global).

% Strongly rejected the legitimacy of the Treaty of Versailles and the reparations claims, viewing them as an unjust 'Diktat'. They gained significant political traction by advocating for repudiation and were structurally excluded from the formal negotiation processes.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__punitive_liability_reading, german_nationalists, excluded,
    organized, biographical, identity_locked, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To establish a legal and financial framework for settling war debts and compensating Allied nations for damages incurred during World War I, aiming to restore a semblance of post-war international economic order.
% TRANSFER_FUNCTION: Transfers substantial financial resources and economic output from Germany to the Allied creditor states, primarily France, Britain, and Belgium, as compensation for war costs and damages.
% ABSENT_VOICES: The broader German public, particularly those who felt unjustly burdened and humiliated by the treaty's terms, were excluded from the negotiation process. Their objections were channeled through domestic political movements rather than international forums.
% DISAPPEARANCE_RATIONALE: If the reparations claims and their enforcement had vanished overnight, the post-WWI European economic and political landscape would have been fundamentally different. Germany's economic recovery would have been unburdened, potentially altering the rise of extremist political movements, and the financial stability of Allied nations would have been severely impacted, necessitating alternative funding for reconstruction.
% FOUNDING_PROBLEM: To assign responsibility for the immense human and material costs of World War I and to provide financial compensation to the victorious Allied powers for their losses and reconstruction efforts.
% FOUNDING_PROBLEM_CORROBORATION: Allied governments and their populations largely corroborated the view that Germany bore sole responsibility and should pay. However, German historians, economists (e.g., John Maynard Keynes), and some international observers contested the economic viability and moral basis of the punitive claims, arguing they were excessive and destabilizing.
narrative_ontology:disappearance_verdict(versailles_reparations_clauses__punitive_liability_reading, world_rearranges).
narrative_ontology:founding_problem_status(versailles_reparations_clauses__punitive_liability_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(versailles_reparations_clauses__punitive_liability_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
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
 *   Base extractiveness is high (0.85) because the claims were vast and largely decoupled from Germany's ability to pay, leading to a significant transfer of wealth. Suppression is very high (0.90) due to the coercive nature of the treaty's imposition and the lack of viable alternatives for Germany, whose fiscal sovereignty was effectively subordinated. Theater ratio is low (0.10) as the claims were genuinely pursued and enforced, not merely performative. Accessibility collapse is high (0.80) because Germany had few options other than compliance or military confrontation. Resistance is moderate-high (0.70) reflecting the continuous internal and external opposition to the reparations regime.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the Allied creditor states, the reparations were a just and necessary consequence of German aggression, a legitimate coordination mechanism for post-war settlement. From the German perspective, particularly that of the workers and taxpayers, it was an unjust, punitive extraction that crippled their economy and fostered resentment. The engine's classification will highlight this divergence between the claimed 'tangled_rope' (coordination with extraction) and the high metrics suggesting a 'snare' (pure extraction).
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's initial mandate was to compensate for war damages. However, under the punitive liability reading, the claims quickly outstripped any reasonable measure of compensation or Germany's capacity, transforming the mechanism from a potentially legitimate (if harsh) coordination for debt settlement into a pure extraction mechanism. The persistence of these claims, despite their economic infeasibility, indicates a shift where the original coordination function atrophied, leaving primarily extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    war_guilt_interpretation_ambiguity,
    'Is Article 231 of the Treaty of Versailles a genuine admission of sole German war guilt, or primarily a legal basis for reparations claims, irrespective of full moral culpability?',
    'Historical analysis of diplomatic correspondence and legal interpretations from the time, particularly from non-Allied legal scholars, to determine the intent and reception of the clause beyond the punitive reading.',
    'If primarily a legal basis, it weakens the moral grounding of the ''punitive liability'' reading, potentially shifting the constraint''s perceived legitimacy and reducing its ''claimed_type'' to a more explicit ''snare''. If a genuine admission, it reinforces the moral justification for high extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(war_guilt_interpretation_ambiguity, conceptual, 'Ambiguity regarding the interpretation of Article 231''s ''war guilt'' clause.').

omega_variable(
    economic_capacity_vs_claims_gap,
    'Were the reparations claims, as interpreted by the punitive liability reading, genuinely within Germany''s long-term economic capacity without causing severe destabilization?',
    'Retrospective economic modeling and counterfactual analysis, comparing actual German economic performance and social costs under reparations with hypothetical scenarios of lower or no reparations.',
    'If the claims demonstrably exceeded capacity, it would further undermine the ''tangled_rope'' coordination narrative, pushing the constraint closer to a ''snare'' by highlighting the impossibility of sustainable compliance. If capacity was higher than perceived, it would lend more credence to the coordination aspect.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(economic_capacity_vs_claims_gap, empirical, 'Discrepancy between reparations demands and Germany''s economic capacity.').

omega_variable(
    long_term_stability_impact,
    'Did the punitive nature of the reparations, as enforced under this reading, contribute to long-term political and economic instability in Europe, ultimately undermining its own stated goals of peace and security?',
    'Historical and political science analysis correlating the severity of reparations enforcement with the rise of extremist movements and subsequent international conflicts.',
    'If a strong causal link is established, it would highlight the self-defeating nature of the punitive approach, reclassifying the constraint''s long-term effect as counter-productive to its stated coordination function, further emphasizing its extractive nature.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(long_term_stability_impact, empirical, 'Impact of punitive reparations on European stability.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(versailles_reparations_clauses__punitive_liability_reading, 1919, 1932).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vers_tr_t1919, versailles_reparations_clauses__punitive_liability_reading, theater_ratio, 1919, 0.05).
narrative_ontology:measurement(vers_tr_t1922, versailles_reparations_clauses__punitive_liability_reading, theater_ratio, 1922, 0.08).
narrative_ontology:measurement(vers_tr_t1925, versailles_reparations_clauses__punitive_liability_reading, theater_ratio, 1925, 0.1).
narrative_ontology:measurement(vers_tr_t1928, versailles_reparations_clauses__punitive_liability_reading, theater_ratio, 1928, 0.12).
narrative_ontology:measurement(vers_tr_t1932, versailles_reparations_clauses__punitive_liability_reading, theater_ratio, 1932, 0.1).

% Extraction over time
narrative_ontology:measurement(vers_be_t1919, versailles_reparations_clauses__punitive_liability_reading, base_extractiveness, 1919, 0.75).
narrative_ontology:measurement(vers_be_t1922, versailles_reparations_clauses__punitive_liability_reading, base_extractiveness, 1922, 0.8).
narrative_ontology:measurement(vers_be_t1925, versailles_reparations_clauses__punitive_liability_reading, base_extractiveness, 1925, 0.83).
narrative_ontology:measurement(vers_be_t1928, versailles_reparations_clauses__punitive_liability_reading, base_extractiveness, 1928, 0.87).
narrative_ontology:measurement(vers_be_t1932, versailles_reparations_clauses__punitive_liability_reading, base_extractiveness, 1932, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(vers_su_t1919, versailles_reparations_clauses__punitive_liability_reading, suppression_requirement, 1919, 0.8).
narrative_ontology:measurement(vers_su_t1922, versailles_reparations_clauses__punitive_liability_reading, suppression_requirement, 1922, 0.85).
narrative_ontology:measurement(vers_su_t1925, versailles_reparations_clauses__punitive_liability_reading, suppression_requirement, 1925, 0.88).
narrative_ontology:measurement(vers_su_t1928, versailles_reparations_clauses__punitive_liability_reading, suppression_requirement, 1928, 0.9).
narrative_ontology:measurement(vers_su_t1932, versailles_reparations_clauses__punitive_liability_reading, suppression_requirement, 1932, 0.87).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(versailles_reparations_clauses__punitive_liability_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(versailles_reparations_clauses__punitive_liability_reading, german_hyperinflation).
narrative_ontology:affects_constraint(versailles_reparations_clauses__punitive_liability_reading, rise_of_nazism).
narrative_ontology:affects_constraint(versailles_reparations_clauses__punitive_liability_reading, limited_responsibility_reading).
narrative_ontology:affects_constraint(versailles_reparations_clauses__punitive_liability_reading, repudiation_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the 'versailles_reparations_clauses' kernel, each with different structural properties and classifications. This 'punitive_liability_reading' emphasizes Germany's sole responsibility and quasi-unlimited claims, contrasting with readings focused on economic capacity or treaty illegitimacy.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
