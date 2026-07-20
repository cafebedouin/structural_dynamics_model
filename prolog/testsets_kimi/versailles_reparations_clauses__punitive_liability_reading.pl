% ============================================================================
% CONSTRAINT STORY: versailles_reparations_clauses__punitive_liability_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
    narrative_ontology:stakeholder_gain_flow/2,
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
 *   human_readable: Versailles Reparations Clauses â Punitive Liability Reading
 *   domain: international_relations/legal_history/political_economy
 *
 * SUMMARY:
 *   The Versailles Treaty (1919) assigned Germany sole responsibility for the
 *   war under Article 231, grounding reparations claims that the Allied
 *   Reparations Commission scaled to levels exceeding German fiscal capacity.
 *   This constraint story instantiates the punitive liability reading: a
 *   commitment system in which a fixed legal kernel (the treaty text) is
 *   interpreted to impose quasi-unlimited moral and financial obligations on
 *   the defeated party. The reading treats German fiscal sovereignty as
 *   subordinated to external creditor oversight, with Allied states as
 *   beneficiaries and German workers and taxpayers as the primary victims.
 *   The constraint exhibits high extractiveness and moderate-to-high
 *   suppression, enforced through territorial occupation threats,
 *   international financial control, and the institutional machinery of the
 *   Reparations Commission. It is claimed as tangled_rope because the
 *   reparations framework did solve a genuine coordination problem among the
 *   Allies (centralizing claims and preventing a scramble for German assets),
 *   but the punitive liability reading specifically loads asymmetric
 *   extraction onto Germany through an unlimited moral liability frame.
 *
 * KEY AGENTS:
 *   - allied_creditor_states: Primary beneficiary (institutional/arbitrage) â collect reparations and vindicate war guilt
 *   - reparations_commission: Agenda setter (institutional/constrained) â administers and enforces the liability schedule
 *   - german_workers_taxpayers: Primary target (powerless/trapped) â bear the fiscal extraction through taxes and austerity
 *   - weimar_republic_government: Intermediate payer (moderate/constrained) â collects and transfers revenue under external supervision
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(versailles_reparations_clauses__punitive_liability_reading, 0.82).
domain_priors:suppression_score(versailles_reparations_clauses__punitive_liability_reading, 0.75).
domain_priors:theater_ratio(versailles_reparations_clauses__punitive_liability_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(versailles_reparations_clauses__punitive_liability_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(versailles_reparations_clauses__punitive_liability_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(versailles_reparations_clauses__punitive_liability_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(versailles_reparations_clauses__punitive_liability_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(versailles_reparations_clauses__punitive_liability_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(versailles_reparations_clauses__punitive_liability_reading, tangled_rope).
narrative_ontology:human_readable(versailles_reparations_clauses__punitive_liability_reading, "Versailles Reparations Clauses â Punitive Liability Reading").
narrative_ontology:topic_domain(versailles_reparations_clauses__punitive_liability_reading, "international_relations/legal_history/political_economy").

domain_priors:requires_active_enforcement(versailles_reparations_clauses__punitive_liability_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(versailles_reparations_clauses__punitive_liability_reading, '12116dc0-b097-4964-9cd1-cf3ac85f96dd').
narrative_ontology:cs_kernel_codification('12116dc0-b097-4964-9cd1-cf3ac85f96dd', fixed_text).
narrative_ontology:cs_authority_grounding('12116dc0-b097-4964-9cd1-cf3ac85f96dd', extraction).
narrative_ontology:cs_interpretation_layer_present('12116dc0-b097-4964-9cd1-cf3ac85f96dd').
narrative_ontology:cs_reading_relation('12116dc0-b097-4964-9cd1-cf3ac85f96dd', versailles_reparations_clauses__limited_responsibility_reading, coexists_with).
narrative_ontology:cs_reading_relation('12116dc0-b097-4964-9cd1-cf3ac85f96dd', versailles_reparations_clauses__repudiation_reading, forecloses).
narrative_ontology:cs_axiom('12116dc0-b097-4964-9cd1-cf3ac85f96dd', foundational, germany_unique_war_guilt).
narrative_ontology:cs_axiom_status(germany_unique_war_guilt, holdable).
narrative_ontology:cs_axiom_grounding('12116dc0-b097-4964-9cd1-cf3ac85f96dd', germany_unique_war_guilt, empirically_contingent).
narrative_ontology:cs_axiom('12116dc0-b097-4964-9cd1-cf3ac85f96dd', foundational, article_231_grounds_unlimited_reparations).
narrative_ontology:cs_axiom_status(article_231_grounds_unlimited_reparations, holdable).
narrative_ontology:cs_axiom_grounding('12116dc0-b097-4964-9cd1-cf3ac85f96dd', article_231_grounds_unlimited_reparations, conventional).
narrative_ontology:cs_reference_frame('12116dc0-b097-4964-9cd1-cf3ac85f96dd', punitive_liability_framework).
narrative_ontology:cs_drift_state('12116dc0-b097-4964-9cd1-cf3ac85f96dd', post_lausanne_1932, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('12116dc0-b097-4964-9cd1-cf3ac85f96dd', '').
narrative_ontology:cs_kernel_id(versailles_reparations_clauses__punitive_liability_reading, versailles_reparations_clauses).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(versailles_reparations_clauses__punitive_liability_reading, allied_creditor_states).
narrative_ontology:constraint_victim(versailles_reparations_clauses__punitive_liability_reading, german_workers_taxpayers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(versailles_reparations_clauses__punitive_liability_reading, weimar_republic_government).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Collect reparations payments and war-debt settlements from Germany under the treaty framework; set the liability terms through the Supreme Council and Conference of Ambassadors; benefit from fiscal transfer and the moral vindication of assigned war guilt.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__punitive_liability_reading, allied_creditor_states, beneficiary,
    institutional, generational, arbitrage, continental).

% Administers the reparations schedule, assesses German capacity, certifies compliance and default, and sets the technical parameters through which the punitive liability reading is enforced on a recurring basis.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__punitive_liability_reading, reparations_commission, agenda_setter,
    institutional, generational, constrained, continental).

% Bear the direct tax burden and austerity measures required to generate reparations transfers; have no voice in Commission decisions and extremely limited mobility to escape the fiscal extraction imposed under Article 231.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__punitive_liability_reading, german_workers_taxpayers, payer,
    powerless, biographical, trapped, national).

% Legally bound to collect internal revenue and deliver it to the Reparations Commission; administers domestic extraction but does not control the liability quantum; politically trapped between Allied enforcement threats and domestic taxpayer resistance.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__punitive_liability_reading, weimar_republic_government, payer,
    moderate, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(versailles_reparations_clauses__punitive_liability_reading, allied_creditor_states).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Centralizes and coordinates the distribution of war costs among Allied powers through a single claims framework against the defeated state, preventing unilateral territorial seizures and standardizing creditor recovery procedures.
% TRANSFER_FUNCTION: Moves wealth from the German fiscal base (workers, taxpayers, industrial output) to Allied creditor states via scheduled reparations payments, legally justified by the attribution of unique moral and financial responsibility for the war.
% ABSENT_VOICES: German workers and taxpayers were excluded from the Versailles negotiations entirely; the Weimar delegation was present but lacked bargaining parity; neutral financial observers and later US commercial interests were excluded from the liability framework's initial design.
% DISAPPEARANCE_RATIONALE: If the punitive liability reading vanished overnight, Allied fiscal claims would lose their primary legal anchor, German tax burdens would immediately restructure, the interwar debt web would require complete renegotiation, and the moral economy of the war would shift away from sole German guilt.
% FOUNDING_PROBLEM: How to distribute the massive costs of total war among the defeated power and the victorious alliance without triggering immediate renewed conflict or a chaotic scramble for unilateral annexations and seizures.
% FOUNDING_PROBLEM_CORROBORATION: Allied statesmen publicly asserted the need for reparations to cover war destruction; however, John Maynard Keynes attested from outside the beneficiary set that the punitive scale exceeded any coherent economic recovery rationale, and German diplomatic and historiographical sources consistently contested the proportionality and evidentiary basis of sole German guilt.
narrative_ontology:disappearance_verdict(versailles_reparations_clauses__punitive_liability_reading, world_rearranges).
narrative_ontology:founding_problem_status(versailles_reparations_clauses__punitive_liability_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(versailles_reparations_clauses__punitive_liability_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(versailles_reparations_clauses__punitive_liability_reading, 'none', 1).
narrative_ontology:epsilon_provenance(versailles_reparations_clauses__punitive_liability_reading, 0.82, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness is high (0.82 at peak) because the punitive reading decoupled liability from German capacity, assigning total war costs as a moral debt. Suppression is substantial (0.75) because Germany's alternatives (default, renegotiation, refusal) were met with occupation (Ruhr 1923) and blockade threats. Theater_ratio is moderate-to-high (0.60 peak during the Dawes-Young circular flow period) because a significant share of 'reparations' represented accounting transfers funded by US loans to Germany, making the constraint partly performative. The measurement series tracks the rise and collapse of the punitive frame from 1919 to 1939, showing extraction accumulation through the London Ultimatum (1921), institutionalization under Dawes/Young (1924-1929), and terminal decay after Lausanne (1932) and Nazi repudiation (1933).
 *
 * PERSPECTIVAL GAP:
 *   The Allied creditor seat experiences the constraint as a legitimate collective framework for distributing war costs and preventing unilateral seizures; the German payer seat experiences it as coerced subordination of national fiscal sovereignty to external moral claims. The engine computes this divergence from the same structural data: beneficiaries with arbitrage-grade exit versus trapped powerless payers.
 *
 * DIRECTIONALITY LOGIC:
 *   Allied creditor states sit at the beneficiary end because they collect the reparations and control the revision mechanisms; their institutional power and arbitrage-grade exit options mean the constraint subsidizes their fiscal position. German workers and taxpayers sit at the full-target end because they are identity-locked to the German fiscal base and bear the extraction directly. The Weimar government sits in between (moderate power, constrained exit) as the forced domestic administrator of the extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The punitive liability reading prevents mandatrophy mislabeling by distinguishing the genuine coordination function (Allied claim centralization) from the extraction function (unlimited German liability). Without this distinction, the framework would read the reparations system as either pure coordination (ignoring the asymmetric victimization of German taxpayers) or pure extraction (ignoring the real problem of unilateral Allied seizures that the treaty prevented). The temporal measurements show the coordination function atrophying after 1924 as the Dawes/Young plans increasingly served financial theater rather than real transfer, but the constraint persisted until 1933 due to political investment in the war guilt narrative.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    treaty_text_unlimited_liability,
    'Does Article 231 and the reparations chapters structurally encode unlimited liability, or is the punitive reading a political extrapolation beyond the treaty text?',
    'Forensic textual analysis of the treaty articles versus the London Schedule of Payments and subsequent Commission interpretations; comparison with the limited responsibility reading''s textual evidence.',
    'If the text itself bounds liability, the punitive reading''s extractiveness is lower and the constraint shifts toward misreading/extraction; if the text truly encodes unlimited claims, the reading is textually grounded.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(treaty_text_unlimited_liability, conceptual, 'Whether the treaty text supports unlimited liability or the punitive reading exceeds it').

omega_variable(
    allied_coordination_or_extraction,
    'Is the primary function of the reparations framework genuine coordination of Allied claims, or is the coordination story cover for extraction from Germany?',
    'Counterfactual analysis: would the Allies have accepted a liability-capped framework if it equally prevented unilateral seizures? Historical evidence of Allied negotiation positions on caps versus seizure rights.',
    'If coordination was the dominant function, tangled_rope classification holds; if extraction dominated from inception, the constraint is more snare-like.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(allied_coordination_or_extraction, empirical, 'Whether the reparations framework was primarily coordination or extraction').

omega_variable(
    german_subordination_internalized,
    'Did Weimar political elites internalize the punitive liability frame, or was their compliance purely coerced by occupation and blockade threats?',
    'Discourse analysis of Weimar diplomatic and parliamentary rhetoric across 1919-1933; measure of voluntary compliance versus explicitly coerced signature.',
    'If internalized, effective suppression is higher than structural measures suggest and the constraint operated partly through identity-lock; if purely coerced, the constraint relied on raw enforcement.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(german_subordination_internalized, empirical, 'Whether German compliance was internalized or purely coerced').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(versailles_reparations_clauses__punitive_liability_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(versailles_rep_punitive_tr_t0, versailles_reparations_clauses__punitive_liability_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(versailles_rep_punitive_tr_t2, versailles_reparations_clauses__punitive_liability_reading, theater_ratio, 2, 0.35).
narrative_ontology:measurement(versailles_rep_punitive_tr_t5, versailles_reparations_clauses__punitive_liability_reading, theater_ratio, 5, 0.55).
narrative_ontology:measurement(versailles_rep_punitive_tr_t10, versailles_reparations_clauses__punitive_liability_reading, theater_ratio, 10, 0.6).
narrative_ontology:measurement(versailles_rep_punitive_tr_t13, versailles_reparations_clauses__punitive_liability_reading, theater_ratio, 13, 0.45).
narrative_ontology:measurement(versailles_rep_punitive_tr_t15, versailles_reparations_clauses__punitive_liability_reading, theater_ratio, 15, 0.7).
narrative_ontology:measurement(versailles_rep_punitive_tr_t20, versailles_reparations_clauses__punitive_liability_reading, theater_ratio, 20, 0.85).

% Extraction over time
narrative_ontology:measurement(versailles_rep_punitive_be_t0, versailles_reparations_clauses__punitive_liability_reading, base_extractiveness, 0, 0.72).
narrative_ontology:measurement(versailles_rep_punitive_be_t2, versailles_reparations_clauses__punitive_liability_reading, base_extractiveness, 2, 0.88).
narrative_ontology:measurement(versailles_rep_punitive_be_t5, versailles_reparations_clauses__punitive_liability_reading, base_extractiveness, 5, 0.8).
narrative_ontology:measurement(versailles_rep_punitive_be_t10, versailles_reparations_clauses__punitive_liability_reading, base_extractiveness, 10, 0.75).
narrative_ontology:measurement(versailles_rep_punitive_be_t13, versailles_reparations_clauses__punitive_liability_reading, base_extractiveness, 13, 0.5).
narrative_ontology:measurement(versailles_rep_punitive_be_t15, versailles_reparations_clauses__punitive_liability_reading, base_extractiveness, 15, 0.25).
narrative_ontology:measurement(versailles_rep_punitive_be_t20, versailles_reparations_clauses__punitive_liability_reading, base_extractiveness, 20, 0.1).

% Suppression requirement over time
narrative_ontology:measurement(versailles_rep_punitive_su_t0, versailles_reparations_clauses__punitive_liability_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(versailles_rep_punitive_su_t2, versailles_reparations_clauses__punitive_liability_reading, suppression_requirement, 2, 0.85).
narrative_ontology:measurement(versailles_rep_punitive_su_t5, versailles_reparations_clauses__punitive_liability_reading, suppression_requirement, 5, 0.7).
narrative_ontology:measurement(versailles_rep_punitive_su_t10, versailles_reparations_clauses__punitive_liability_reading, suppression_requirement, 10, 0.65).
narrative_ontology:measurement(versailles_rep_punitive_su_t13, versailles_reparations_clauses__punitive_liability_reading, suppression_requirement, 13, 0.4).
narrative_ontology:measurement(versailles_rep_punitive_su_t15, versailles_reparations_clauses__punitive_liability_reading, suppression_requirement, 15, 0.2).
narrative_ontology:measurement(versailles_rep_punitive_su_t20, versailles_reparations_clauses__punitive_liability_reading, suppression_requirement, 20, 0.05).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(versailles_reparations_clauses__punitive_liability_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(versailles_reparations_clauses__punitive_liability_reading, versailles_reparations_clauses__limited_responsibility_reading).
narrative_ontology:affects_constraint(versailles_reparations_clauses__punitive_liability_reading, versailles_reparations_clauses__repudiation_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the versailles_reparations_clauses kernel. The kernel (Article 231 and associated reparations chapters) supports multiple structurally distinct interpretations: punitive liability (this file), limited responsibility, and repudiation. Each reading instantiates a different beneficiary/victim structure and Îµ profile. They compete for institutional dominance rather than standing in causal dependency.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
