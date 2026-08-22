% ============================================================================
% CONSTRAINT STORY: commerce_clause_text__originalist_narrow_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_commerce_clause_text__originalist_narrow_reading, []).

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
 *   constraint_id: commerce_clause_text__originalist_narrow_reading
 *   human_readable: Originalist Narrow Reading of the Commerce Clause
 *   domain: constitutional law / federalism / commerce regulation
 *
 * SUMMARY:
 *   This constraint story models the originalist narrow reading of the
 *   Commerce Clause: the constitutional claim that federal power extends only
 *   to trade crossing state borders and the instrumentalities of interstate
 *   movement, excluding manufacturing, labor, and other intrastate activity.
 *   The reading is one of three structurally distinct interpretations of the
 *   commerce_clause_text kernel. It functions as an active constraint on
 *   federal legislation, enforced by judicial review. Its beneficiaries are
 *   state governments and anti-federal-consolidation advocates; its costs are
 *   borne by interstate businesses facing fragmented regulation and federal
 *   agencies blocked from national standards. The claim/metric independence
 *   is observed: the reading is claimed by its proponents as a fixed textual
 *   limit (Mountain-like in their framing), but the authored metrics describe
 *   a tangled rope â genuine federalism coordination riding on asymmetric
 *   extraction from national-regulation advocates.
 *
 * KEY AGENTS:
 *   - state_governments: Primary beneficiary (institutional/constrained) â retain police power
 *   - interstate_commerce_participants: Primary target (powerful/constrained) â bear fragmented compliance costs
 *   - federal_regulators: Secondary target (institutional/constrained) â truncated enforcement domain
 *   - federal_judiciary: Agenda-setter (institutional/constrained) â administers the interpretive boundary
 *   - anti_federal_consolidation_advocates: Ideological beneficiary (organized/mobile) â benefit from limited federal doctrine
 *   - national_regulatory_advocates: Excluded voice (organized/constrained) â structurally barred from policy success
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(commerce_clause_text__originalist_narrow_reading, 0.45).
domain_priors:suppression_score(commerce_clause_text__originalist_narrow_reading, 0.5).
domain_priors:theater_ratio(commerce_clause_text__originalist_narrow_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(commerce_clause_text__originalist_narrow_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(commerce_clause_text__originalist_narrow_reading, suppression_requirement, 0.5).
narrative_ontology:constraint_metric(commerce_clause_text__originalist_narrow_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(commerce_clause_text__originalist_narrow_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(commerce_clause_text__originalist_narrow_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(commerce_clause_text__originalist_narrow_reading, tangled_rope).
narrative_ontology:human_readable(commerce_clause_text__originalist_narrow_reading, "Originalist Narrow Reading of the Commerce Clause").
narrative_ontology:topic_domain(commerce_clause_text__originalist_narrow_reading, "constitutional law / federalism / commerce regulation").

domain_priors:requires_active_enforcement(commerce_clause_text__originalist_narrow_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(commerce_clause_text__originalist_narrow_reading, '85b89a50-f796-47f7-9687-31851ea6688d').
narrative_ontology:cs_kernel_codification('85b89a50-f796-47f7-9687-31851ea6688d', fixed_text).
narrative_ontology:cs_authority_grounding('85b89a50-f796-47f7-9687-31851ea6688d', lineage).
narrative_ontology:cs_interpretation_layer_present('85b89a50-f796-47f7-9687-31851ea6688d').
narrative_ontology:cs_reading_relation('85b89a50-f796-47f7-9687-31851ea6688d', commerce_clause_text__expansive_federal_reading, forecloses).
narrative_ontology:cs_reading_relation('85b89a50-f796-47f7-9687-31851ea6688d', commerce_clause_text__substantial_effects_limited_reading, forecloses).
narrative_ontology:cs_axiom('85b89a50-f796-47f7-9687-31851ea6688d', foundational, commerce_original_public_meaning_limited_to_trade).
narrative_ontology:cs_axiom_status(commerce_original_public_meaning_limited_to_trade, holdable).
narrative_ontology:cs_axiom_grounding('85b89a50-f796-47f7-9687-31851ea6688d', commerce_original_public_meaning_limited_to_trade, empirically_contingent).
narrative_ontology:cs_axiom('85b89a50-f796-47f7-9687-31851ea6688d', foundational, enumeration_principle_limits_federal_power).
narrative_ontology:cs_axiom_status(enumeration_principle_limits_federal_power, holdable).
narrative_ontology:cs_axiom_grounding('85b89a50-f796-47f7-9687-31851ea6688d', enumeration_principle_limits_federal_power, deontological).
narrative_ontology:cs_reference_frame('85b89a50-f796-47f7-9687-31851ea6688d', founding_era_public_meaning).
narrative_ontology:cs_drift_state('85b89a50-f796-47f7-9687-31851ea6688d', contemporary_doctrine, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('85b89a50-f796-47f7-9687-31851ea6688d', '').
narrative_ontology:cs_kernel_id(commerce_clause_text__originalist_narrow_reading, commerce_clause_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(commerce_clause_text__originalist_narrow_reading, state_governments).
narrative_ontology:constraint_beneficiary(commerce_clause_text__originalist_narrow_reading, anti_federal_consolidation_advocates).
narrative_ontology:constraint_victim(commerce_clause_text__originalist_narrow_reading, interstate_commerce_participants).
narrative_ontology:constraint_victim(commerce_clause_text__originalist_narrow_reading, federal_regulators).
narrative_ontology:constraint_vindicates(commerce_clause_text__originalist_narrow_reading, state_police_power).
narrative_ontology:constraint_vindicates(commerce_clause_text__originalist_narrow_reading, enumerated_powers_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Retain police power over intrastate economic activity under this reading; freed from federal preemption in manufacturing, labor, and local commerce. Cannot exit the federal system but benefit from its jurisdictional limits.
narrative_ontology:constraint_stakeholder(commerce_clause_text__originalist_narrow_reading, state_governments, beneficiary,
    institutional, generational, constrained, national).

% Ideological and political advocates for limited federal government; benefit from constitutional doctrine that enforces enumerated powers and restrains national regulatory expansion.
narrative_ontology:constraint_stakeholder(commerce_clause_text__originalist_narrow_reading, anti_federal_consolidation_advocates, beneficiary,
    organized, generational, mobile, national).

% National and multi-state firms that must navigate fragmented state regulatory regimes because federal uniform standards are judicially blocked; compliance costs rise with regulatory multiplicity.
narrative_ontology:constraint_stakeholder(commerce_clause_text__originalist_narrow_reading, interstate_commerce_participants, payer,
    powerful, biographical, constrained, national).

% Federal agencies tasked with national labor, environmental, and economic regulation; their enforcement domain is truncated when courts reject Commerce Clause authority over intrastate activity.
narrative_ontology:constraint_stakeholder(commerce_clause_text__originalist_narrow_reading, federal_regulators, payer,
    institutional, biographical, constrained, national).

% Interprets and enforces the Commerce Clause through judicial review; under this reading, strikes down federal statutes that lack border-crossing nexus. Bound by Article III and precedent but exercises interpretive discretion.
narrative_ontology:constraint_stakeholder(commerce_clause_text__originalist_narrow_reading, federal_judiciary, agenda_setter,
    institutional, generational, constrained, national).

% Labor, environmental, and consumer advocates seeking uniform national standards; their preferred regulatory solutions are structurally excluded from constitutional viability under the narrow reading.
narrative_ontology:constraint_stakeholder(commerce_clause_text__originalist_narrow_reading, national_regulatory_advocates, excluded,
    organized, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(commerce_clause_text__originalist_narrow_reading, diffuse).
narrative_ontology:fixing_cost_class(commerce_clause_text__originalist_narrow_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allocates regulatory jurisdiction between federal and state governments by limiting federal commerce power to border-crossing transactions and instrumentalities of movement; prevents federal aggregation of general police power.
% TRANSFER_FUNCTION: Transfers regulatory authority over intrastate economic activity from the federal government to state governments; transfers compliance costs from federal uniformity to interstate actors facing fragmented state regimes.
% ABSENT_VOICES: National regulatory advocates seeking uniform labor, environmental, and consumer standards are excluded from policy success when courts define commerce narrowly; their arguments are heard in dissent but do not control.
% DISAPPEARANCE_RATIONALE: Federal statutes previously struck down under the narrow reading would survive judicial review; states would lose exclusive police power over manufacturing, labor, and local economic activity; national markets would reorganize under uniform federal regulatory regimes or competing state regimes with federal minimum standards.
% FOUNDING_PROBLEM: Prevent federal overreach into local police powers and preserve state sovereignty by ensuring the federal government exercises only enumerated powers; protect local self-government from distant national regulation.
% FOUNDING_PROBLEM_CORROBORATION: Anti-federal consolidation advocates and originalist jurists attest the problem remains live, citing modern federal expansion. Progressive constitutional scholars, historians, and proponents of the expansive and substantial-effects readings attest the problem is superseded by economic integration and the modern administrative state; legislative practice and the bulk of twentieth-century jurisprudence corroborate the dead-status reading from outside the beneficiary set.
narrative_ontology:disappearance_verdict(commerce_clause_text__originalist_narrow_reading, world_rearranges).
narrative_ontology:founding_problem_status(commerce_clause_text__originalist_narrow_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(commerce_clause_text__originalist_narrow_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(commerce_clause_text__originalist_narrow_reading, 'none', 1).
narrative_ontology:epsilon_provenance(commerce_clause_text__originalist_narrow_reading, 0.45, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(commerce_clause_text__originalist_narrow_reading_tests).
:- end_tests(commerce_clause_text__originalist_narrow_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.45) reflects the real cost of regulatory fragmentation imposed on interstate actors and the suppression of national regulatory solutions. Suppression (0.50) measures the judicial blocking of federal statutes; it is moderate because Congress can sometimes attach jurisdictional hooks. Theater ratio (0.25) acknowledges that the federalism function is genuine but a growing share of originalist argumentation is performative doctrinal maintenance. Accessibility collapse (0.60) is moderate: constitutional amendment is the formal exit but is practically inaccessible; judicial reversal is the real path. Resistance (0.70) is high because the reading faces persistent opposition from federal regulators, Congress, and national advocacy groups. The temporal series shows a U-shape: the reading was dominant in the Lochner era, collapsed during the New Deal, and partially revived in the late twentieth century.
 *
 * PERSPECTIVAL GAP:
 *   State governments experience this constraint as protective constitutional architecture (low effective extraction, possibly rope-like or mountain-like from their seat). Interstate commerce participants and federal regulators experience it as an active barrier to necessary coordination (high effective extraction, snare-like from their seat). The engine computes this divergence from identical structural data using directionality: beneficiaries receive damped extraction, targets receive amplified extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   State governments and anti-federal advocates are structural beneficiaries (d near 0.0) because the constraint subsidizes their regulatory autonomy. Interstate businesses and federal regulators are structural targets (d near 1.0) because the constraint extracts compliance costs and truncates enforcement domains. The federal judiciary sits near symmetric (d ~0.5) as administrator; it does not collect the extraction but enforces the boundary. National regulatory advocates are excluded from the directionality calculation entirely (role: excluded).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â limiting federal overreach â is contested. From the beneficiary seat, the problem remains live (modern federal expansion). From the payer seat, the problem is obsolete (modern economy requires national standards). The mismatch between contested founding_problem_status and world_rearranges disappearance verdict signals that the constraint persists partly through ideological commitment rather than purely live coordination need, but the genuine federalism function prevents classification as pure snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    historical_meaning_or_political_construction,
    'Is the narrow reading a recovery of original public meaning or a contemporary political construction using historical vocabulary?',
    'Corpus linguistics and historical legal scholarship on late-eighteenth-century usage of ''commerce'' and ''among the several States''; comparison with sibling readings'' historical claims.',
    'If the historical claim is refuted, the reading''s authority grounding shifts from lineage to extraction, raising effective extraction and theater ratio.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(historical_meaning_or_political_construction, conceptual, 'Whether the reading reflects genuine historical meaning or retrofitted construction').

omega_variable(
    economic_integration_mootness,
    'Has modern economic integration rendered the border-crossing/instrumentality distinction functionally unworkable?',
    'Empirical analysis of supply chains and economic activity to determine what share of ''local'' activity lacks interstate nexus under an originalist test.',
    'If the distinction covers almost no activity, the coordination function is moot and the constraint operates as pure extraction; if significant local activity remains, the federalism function is live.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(economic_integration_mootness, empirical, 'Whether modern economy has outgrown the narrow reading''s categories').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(commerce_clause_text__originalist_narrow_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(commerce_clause_orig_tr_t0, commerce_clause_text__originalist_narrow_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(commerce_clause_orig_tr_t6, commerce_clause_text__originalist_narrow_reading, theater_ratio, 6, 0.25).
narrative_ontology:measurement(commerce_clause_orig_tr_t12, commerce_clause_text__originalist_narrow_reading, theater_ratio, 12, 0.15).
narrative_ontology:measurement(commerce_clause_orig_tr_t18, commerce_clause_text__originalist_narrow_reading, theater_ratio, 18, 0.1).
narrative_ontology:measurement(commerce_clause_orig_tr_t24, commerce_clause_text__originalist_narrow_reading, theater_ratio, 24, 0.2).
narrative_ontology:measurement(commerce_clause_orig_tr_t30, commerce_clause_text__originalist_narrow_reading, theater_ratio, 30, 0.25).

% Extraction over time
narrative_ontology:measurement(commerce_clause_orig_be_t0, commerce_clause_text__originalist_narrow_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(commerce_clause_orig_be_t6, commerce_clause_text__originalist_narrow_reading, base_extractiveness, 6, 0.6).
narrative_ontology:measurement(commerce_clause_orig_be_t12, commerce_clause_text__originalist_narrow_reading, base_extractiveness, 12, 0.2).
narrative_ontology:measurement(commerce_clause_orig_be_t18, commerce_clause_text__originalist_narrow_reading, base_extractiveness, 18, 0.15).
narrative_ontology:measurement(commerce_clause_orig_be_t24, commerce_clause_text__originalist_narrow_reading, base_extractiveness, 24, 0.35).
narrative_ontology:measurement(commerce_clause_orig_be_t30, commerce_clause_text__originalist_narrow_reading, base_extractiveness, 30, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(commerce_clause_orig_su_t0, commerce_clause_text__originalist_narrow_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(commerce_clause_orig_su_t6, commerce_clause_text__originalist_narrow_reading, suppression_requirement, 6, 0.65).
narrative_ontology:measurement(commerce_clause_orig_su_t12, commerce_clause_text__originalist_narrow_reading, suppression_requirement, 12, 0.25).
narrative_ontology:measurement(commerce_clause_orig_su_t18, commerce_clause_text__originalist_narrow_reading, suppression_requirement, 18, 0.2).
narrative_ontology:measurement(commerce_clause_orig_su_t24, commerce_clause_text__originalist_narrow_reading, suppression_requirement, 24, 0.4).
narrative_ontology:measurement(commerce_clause_orig_su_t30, commerce_clause_text__originalist_narrow_reading, suppression_requirement, 30, 0.5).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(commerce_clause_text__originalist_narrow_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(commerce_clause_text__originalist_narrow_reading, expansive_federal_reading).
narrative_ontology:affects_constraint(commerce_clause_text__originalist_narrow_reading, substantial_effects_limited_reading).

% DUAL FORMULATION NOTE:
% The commerce_clause_text kernel decomposes into three structurally distinct readings: originalist_narrow_reading, expansive_federal_reading, and substantial_effects_limited_reading. Each reading assigns a different scope to the Commerce Clause and produces different beneficiary/victim structures. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
