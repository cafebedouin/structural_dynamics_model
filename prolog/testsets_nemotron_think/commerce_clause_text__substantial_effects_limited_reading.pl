% ============================================================================
% CONSTRAINT STORY: commerce_clause_text__substantial_effects_limited_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_commerce_clause_text__substantial_effects_limited_reading, []).

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
 *   constraint_id: commerce_clause_text__substantial_effects_limited_reading
 *   human_readable: Commerce Clause Substantial Effects Limited Reading
 *   domain: constitutional_law/federalism/commerce_regulation
 *
 * SUMMARY:
 *   This constraint story captures the 'substantial effects limited reading'
 *   of the Commerce Clause: federal power reaches intrastate economic
 *   activity that substantially affects interstate commerce, but stops at
 *   non-economic regulation and pretextual assertions of commerce power. The
 *   reading occupies the middle ground between the expansive federal reading
 *   (which would allow regulation of any activity with aggregate economic
 *   effects) and the originalist narrow reading (which confines commerce to
 *   cross-border trade). The constraint is a legal doctrine, not a natural
 *   law; it is actively enforced by courts and has identifiable beneficiaries
 *   (federal government, national businesses) and victims (states, local
 *   businesses, regulated individuals). The claimed type is tangled_rope
 *   because the doctrine both coordinates national economic regulation and
 *   extracts regulatory authority from states.
 *
 * KEY AGENTS:
 *   - federal_government: Primary agenda setter (institutional/arbitrage) — defines and enforces the regulatory boundary
 *   - national_businesses: Primary beneficiary (powerful/mobile) — gain uniform standards and reduced compliance costs
 *   - state_governments: Primary payer (organized/constrained) — lose regulatory autonomy over intrastate economic activity
 *   - local_businesses: Payer (moderate/constrained) — bear federal compliance costs without proportional influence
 *   - individuals_subject_to_federal_regulation: Payer (powerless/trapped) — directly regulated with no exit
 *   - supreme_court: Observer (institutional/analytical) — adjudicates the doctrine's boundaries
 *   - originalist_scholars: Excluded (moderate/analytical) — advocate a competing reading but are outside doctrinal decision-making
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(commerce_clause_text__substantial_effects_limited_reading, 0.55).
domain_priors:suppression_score(commerce_clause_text__substantial_effects_limited_reading, 0.6).
domain_priors:theater_ratio(commerce_clause_text__substantial_effects_limited_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(commerce_clause_text__substantial_effects_limited_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(commerce_clause_text__substantial_effects_limited_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(commerce_clause_text__substantial_effects_limited_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(commerce_clause_text__substantial_effects_limited_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(commerce_clause_text__substantial_effects_limited_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(commerce_clause_text__substantial_effects_limited_reading, tangled_rope).
narrative_ontology:human_readable(commerce_clause_text__substantial_effects_limited_reading, "Commerce Clause Substantial Effects Limited Reading").
narrative_ontology:topic_domain(commerce_clause_text__substantial_effects_limited_reading, "constitutional_law/federalism/commerce_regulation").

domain_priors:requires_active_enforcement(commerce_clause_text__substantial_effects_limited_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(commerce_clause_text__substantial_effects_limited_reading, '7772daf5-14b2-41b0-9ec1-f4e5c8c4e637').
narrative_ontology:cs_kernel_codification('7772daf5-14b2-41b0-9ec1-f4e5c8c4e637', fixed_text).
narrative_ontology:cs_authority_grounding('7772daf5-14b2-41b0-9ec1-f4e5c8c4e637', lineage).
narrative_ontology:cs_interpretation_layer_present('7772daf5-14b2-41b0-9ec1-f4e5c8c4e637').
narrative_ontology:cs_reading_relation('7772daf5-14b2-41b0-9ec1-f4e5c8c4e637', commerce_clause_text__expansive_federal_reading, coexists_with).
narrative_ontology:cs_reading_relation('7772daf5-14b2-41b0-9ec1-f4e5c8c4e637', commerce_clause_text__originalist_narrow_reading, coexists_with).
narrative_ontology:cs_axiom('7772daf5-14b2-41b0-9ec1-f4e5c8c4e637', foundational, economic_nature_requirement).
narrative_ontology:cs_axiom_status(economic_nature_requirement, holdable).
narrative_ontology:cs_axiom_grounding('7772daf5-14b2-41b0-9ec1-f4e5c8c4e637', economic_nature_requirement, conventional).
narrative_ontology:cs_axiom('7772daf5-14b2-41b0-9ec1-f4e5c8c4e637', secondary, jurisdictional_nexus_requirement).
narrative_ontology:cs_axiom_status(jurisdictional_nexus_requirement, holdable).
narrative_ontology:cs_axiom_grounding('7772daf5-14b2-41b0-9ec1-f4e5c8c4e637', jurisdictional_nexus_requirement, conventional).
narrative_ontology:cs_reference_frame('7772daf5-14b2-41b0-9ec1-f4e5c8c4e637', substantial_effects_limited_doctrine).
narrative_ontology:cs_drift_state('7772daf5-14b2-41b0-9ec1-f4e5c8c4e637', contemporary_roberts_court_era, gap(practice_drift, minor, false)).
narrative_ontology:cs_created_at('7772daf5-14b2-41b0-9ec1-f4e5c8c4e637', '2026-08-04T12:00:00Z').
narrative_ontology:cs_kernel_id(commerce_clause_text__substantial_effects_limited_reading, commerce_clause_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(commerce_clause_text__substantial_effects_limited_reading, federal_government).
narrative_ontology:constraint_beneficiary(commerce_clause_text__substantial_effects_limited_reading, national_businesses).
narrative_ontology:constraint_beneficiary(commerce_clause_text__substantial_effects_limited_reading, consumers_benefiting_from_uniform_regulation).
narrative_ontology:constraint_victim(commerce_clause_text__substantial_effects_limited_reading, state_governments).
narrative_ontology:constraint_victim(commerce_clause_text__substantial_effects_limited_reading, local_businesses).
narrative_ontology:constraint_victim(commerce_clause_text__substantial_effects_limited_reading, individuals_subject_to_federal_regulation).
narrative_ontology:constraint_vindicates(commerce_clause_text__substantial_effects_limited_reading, federal_supremacy_in_economic_regulation).
narrative_ontology:constraint_vindicates(commerce_clause_text__substantial_effects_limited_reading, national_market_coordination).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enacts and enforces federal regulations on intrastate economic activity under the substantial effects doctrine; sets the regulatory agenda and benefits from expanded regulatory authority.
narrative_ontology:constraint_stakeholder(commerce_clause_text__substantial_effects_limited_reading, federal_government, agenda_setter,
    institutional, generational, arbitrage, national).

% Benefit from uniform national regulation that reduces compliance costs across states; lobby for federal standards that preempt patchwork state laws.
narrative_ontology:constraint_stakeholder(commerce_clause_text__substantial_effects_limited_reading, national_businesses, beneficiary,
    powerful, biographical, mobile, national).

% Lose regulatory autonomy over intrastate economic activity when federal law preempts; must comply with federal mandates and may lose policy flexibility.
narrative_ontology:constraint_stakeholder(commerce_clause_text__substantial_effects_limited_reading, state_governments, payer,
    organized, generational, constrained, regional).

% Subject to federal regulation that may impose higher compliance costs than state laws; lack the scale to influence federal policy.
narrative_ontology:constraint_stakeholder(commerce_clause_text__substantial_effects_limited_reading, local_businesses, payer,
    moderate, biographical, constrained, local).

% Directly regulated by federal laws justified under the substantial effects doctrine; have no practical exit from federal jurisdiction.
narrative_ontology:constraint_stakeholder(commerce_clause_text__substantial_effects_limited_reading, individuals_subject_to_federal_regulation, payer,
    powerless, biographical, trapped, national).

% Adjudicates the boundaries of the substantial effects test; its interpretations shape the constraint's operational scope.
narrative_ontology:constraint_stakeholder(commerce_clause_text__substantial_effects_limited_reading, supreme_court, observer,
    institutional, generational, analytical, national).

% Advocate for a narrow originalist reading that would eliminate the substantial effects doctrine; excluded from doctrinal decision-making but influence public discourse.
narrative_ontology:constraint_stakeholder(commerce_clause_text__substantial_effects_limited_reading, originalist_scholars, excluded,
    moderate, biographical, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a workable framework for federal regulation of national economic problems that spill across state lines, avoiding a race to the bottom and ensuring uniform standards for interstate commerce.
% TRANSFER_FUNCTION: Moves regulatory authority over intrastate economic activity from states to the federal government, along with compliance costs borne by regulated entities.
% ABSENT_VOICES: Originalist scholars and states' rights advocates who would argue for a narrower commerce power are excluded from the doctrinal framework; they operate outside the Court's precedent-bound discourse.
% DISAPPEARANCE_RATIONALE: The substantial effects doctrine underpins vast swaths of federal regulation (e.g., civil rights, environmental, labor). Its removal would require Congress to rely on other powers or leave regulation to states, fundamentally reshaping the federal-state balance.
% FOUNDING_PROBLEM: The need for federal power to address national economic crises (e.g., the Great Depression) that states could not effectively regulate individually, while preserving a zone of state autonomy for non-economic local affairs.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians and political scientists outside the federal government attest that the doctrine emerged from the New Deal crisis; contemporary debate among scholars across the ideological spectrum confirms the contested status of the economic/non-economic distinction.
narrative_ontology:disappearance_verdict(commerce_clause_text__substantial_effects_limited_reading, world_rearranges).
narrative_ontology:founding_problem_status(commerce_clause_text__substantial_effects_limited_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(commerce_clause_text__substantial_effects_limited_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(commerce_clause_text__substantial_effects_limited_reading, 'none', 1).
narrative_ontology:epsilon_provenance(commerce_clause_text__substantial_effects_limited_reading, 0.55, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(commerce_clause_text__substantial_effects_limited_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(commerce_clause_text__substantial_effects_limited_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(commerce_clause_text__substantial_effects_limited_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.55) reflects the transfer of regulatory authority from states to the federal government; suppression (0.6) captures the preemptive force that displaces state law; theater_ratio (0.2) is low because the doctrine is genuinely litigated and not merely performative. Accessibility_collapse (0.7) is high because once the substantial effects test is satisfied, state alternatives are largely foreclosed. Resistance (0.4) is moderate: states' rights challenges persist but have not overturned the core doctrine. The measurement series shows extractiveness rising during the New Deal era and stabilizing after Lopez/Morrison, while suppression_requirement tracks the growth of federal preemption.
 *
 * PERSPECTIVAL GAP:
 *   From the federal government's seat, the constraint is a coordination mechanism solving collective-action problems in a national economy. From state governments' seats, it is an extraction of their reserved police powers. The engine will compute this divergence from the structural data (beneficiary/victim declarations, power levels, exit options).
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (federal_government, national_businesses, consumers) receive the coordination gains and thus have low directionality (d near 0). Victims (state_governments, local_businesses, individuals) bear the compliance costs and loss of autonomy, giving them high directionality (d near 1). The Supreme Court as observer sits at d=0.5. Originalist scholars are excluded, so their directionality is not computed; their structural position would be high d if they were subject to the constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (national economic crises of the 1930s) is historically resolved, but new coordination problems (e.g., climate change, digital markets) are cited to justify the doctrine's continuation. The economic/non-economic distinction is contested — if it is abandoned, the constraint collapses into the expansive federal reading (higher extraction). If it is tightened, the constraint moves toward the originalist narrow reading (lower extraction). The mandatrophy is unresolved: the constraint persists because it solves live coordination problems, but its extraction of state autonomy is real.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    economic_non_economic_boundary_ambiguity,
    'Is the distinction between economic and non-economic activity a coherent, administrable line, or does it collapse under scrutiny?',
    'Supreme Court jurisprudence testing the boundary in future cases; empirical study of regulatory coverage gaps.',
    'If the boundary collapses, the constraint becomes either an expansive federal reading (no limit) or an originalist narrow reading (no intrastate reach), changing its classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(economic_non_economic_boundary_ambiguity, conceptual, 'Whether the economic/non-economic distinction can sustain the constraint''s coordination function without becoming a pretext.').

omega_variable(
    federal_extraction_vs_coordination_balance,
    'Does the constraint primarily coordinate national economic regulation or extract regulatory authority from states for federal benefit?',
    'Longitudinal analysis of federal preemption patterns and state regulatory innovation.',
    'If extraction dominates, the constraint is a snare; if coordination dominates, it remains a tangled rope or rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(federal_extraction_vs_coordination_balance, empirical, 'The core mandatrophy ambiguity for this constraint.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(commerce_clause_text__substantial_effects_limited_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(commerce_clause_substantial_effects_limited_tr_t0, commerce_clause_text__substantial_effects_limited_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(commerce_clause_substantial_effects_limited_tr_t20, commerce_clause_text__substantial_effects_limited_reading, theater_ratio, 20, 0.18).
narrative_ontology:measurement(commerce_clause_substantial_effects_limited_tr_t40, commerce_clause_text__substantial_effects_limited_reading, theater_ratio, 40, 0.2).
narrative_ontology:measurement(commerce_clause_substantial_effects_limited_tr_t60, commerce_clause_text__substantial_effects_limited_reading, theater_ratio, 60, 0.2).
narrative_ontology:measurement(commerce_clause_substantial_effects_limited_tr_t80, commerce_clause_text__substantial_effects_limited_reading, theater_ratio, 80, 0.2).
narrative_ontology:measurement(commerce_clause_substantial_effects_limited_tr_t100, commerce_clause_text__substantial_effects_limited_reading, theater_ratio, 100, 0.2).

% Extraction over time
narrative_ontology:measurement(commerce_clause_substantial_effects_limited_be_t0, commerce_clause_text__substantial_effects_limited_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(commerce_clause_substantial_effects_limited_be_t20, commerce_clause_text__substantial_effects_limited_reading, base_extractiveness, 20, 0.5).
narrative_ontology:measurement(commerce_clause_substantial_effects_limited_be_t40, commerce_clause_text__substantial_effects_limited_reading, base_extractiveness, 40, 0.55).
narrative_ontology:measurement(commerce_clause_substantial_effects_limited_be_t60, commerce_clause_text__substantial_effects_limited_reading, base_extractiveness, 60, 0.55).
narrative_ontology:measurement(commerce_clause_substantial_effects_limited_be_t80, commerce_clause_text__substantial_effects_limited_reading, base_extractiveness, 80, 0.55).
narrative_ontology:measurement(commerce_clause_substantial_effects_limited_be_t100, commerce_clause_text__substantial_effects_limited_reading, base_extractiveness, 100, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(commerce_clause_substantial_effects_limited_su_t0, commerce_clause_text__substantial_effects_limited_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(commerce_clause_substantial_effects_limited_su_t20, commerce_clause_text__substantial_effects_limited_reading, suppression_requirement, 20, 0.55).
narrative_ontology:measurement(commerce_clause_substantial_effects_limited_su_t40, commerce_clause_text__substantial_effects_limited_reading, suppression_requirement, 40, 0.6).
narrative_ontology:measurement(commerce_clause_substantial_effects_limited_su_t60, commerce_clause_text__substantial_effects_limited_reading, suppression_requirement, 60, 0.6).
narrative_ontology:measurement(commerce_clause_substantial_effects_limited_su_t80, commerce_clause_text__substantial_effects_limited_reading, suppression_requirement, 80, 0.6).
narrative_ontology:measurement(commerce_clause_substantial_effects_limited_su_t100, commerce_clause_text__substantial_effects_limited_reading, suppression_requirement, 100, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(commerce_clause_text__substantial_effects_limited_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(commerce_clause_text__substantial_effects_limited_reading, 0.1).
narrative_ontology:affects_constraint(commerce_clause_text__substantial_effects_limited_reading, commerce_clause_text__expansive_federal_reading).
narrative_ontology:affects_constraint(commerce_clause_text__substantial_effects_limited_reading, commerce_clause_text__originalist_narrow_reading).

% DUAL FORMULATION NOTE:
% This constraint is one member of the commerce_clause_text constraint family. The expansive_federal_reading and originalist_narrow_reading are sibling constraints with different ε values and beneficiary/victim structures. All three share the same kernel (the constitutional text) but instantiate different constraints per the ε-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
