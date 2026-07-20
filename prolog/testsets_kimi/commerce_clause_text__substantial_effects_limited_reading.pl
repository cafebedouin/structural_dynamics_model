% ============================================================================
% CONSTRAINT STORY: commerce_clause_text__substantial_effects_limited_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
 *   constraint_id: commerce_clause_text__substantial_effects_limited_reading
 *   human_readable: Commerce Clause Substantial Effects Limited Reading
 *   domain: constitutional/law/federalism
 *
 * SUMMARY:
 *   This constraint instantiates the limited reading of the Commerce Clause
 *   kernel: federal power reaches intrastate activity only when it has
 *   substantial effects on interstate commerce, possesses a genuine
 *   jurisdictional nexus, and regulates economic rather than non-economic
 *   conduct. The constraint is enforced judicially by the Supreme Court and
 *   operates as a category-boundary mechanism that allocates authority
 *   between federal and state governments. It is claimed as tangled_rope
 *   because it simultaneously coordinates a genuine federal interest in
 *   interstate economic regulation and asymmetrically extracts regulatory
 *   capacity from the federal Congress to preserve state police powers.
 *
 * KEY AGENTS:
 *   - supreme_court: Agenda-setter (institutional/analytical) â defines and enforces the doctrine
 *   - state_governments: Primary beneficiary (institutional/constrained) â retains police powers shielded by the economic/non-economic line
 *   - federal_congress: Primary payer (institutional/constrained) â bears the cost of limited commerce authority and jurisdictional nexus requirements
 *   - local_non_economic_actors: Secondary beneficiary (moderate/constrained) â intrastate non-economic activity shielded from federal regulation
 *   - progressive_legal_scholars: Analytical observer (organized/mobile) â contests the legitimacy of the limitation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(commerce_clause_text__substantial_effects_limited_reading, 0.52).
domain_priors:suppression_score(commerce_clause_text__substantial_effects_limited_reading, 0.55).
domain_priors:theater_ratio(commerce_clause_text__substantial_effects_limited_reading, 0.5).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(commerce_clause_text__substantial_effects_limited_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(commerce_clause_text__substantial_effects_limited_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(commerce_clause_text__substantial_effects_limited_reading, theater_ratio, 0.5).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(commerce_clause_text__substantial_effects_limited_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(commerce_clause_text__substantial_effects_limited_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(commerce_clause_text__substantial_effects_limited_reading, tangled_rope).
narrative_ontology:human_readable(commerce_clause_text__substantial_effects_limited_reading, "Commerce Clause Substantial Effects Limited Reading").
narrative_ontology:topic_domain(commerce_clause_text__substantial_effects_limited_reading, "constitutional/law/federalism").

domain_priors:requires_active_enforcement(commerce_clause_text__substantial_effects_limited_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(commerce_clause_text__substantial_effects_limited_reading, '09588b3c-5c7a-43e4-a643-5b445ca2b119').
narrative_ontology:cs_kernel_codification('09588b3c-5c7a-43e4-a643-5b445ca2b119', fixed_text).
narrative_ontology:cs_authority_grounding('09588b3c-5c7a-43e4-a643-5b445ca2b119', lineage).
narrative_ontology:cs_interpretation_layer_present('09588b3c-5c7a-43e4-a643-5b445ca2b119').
narrative_ontology:cs_reading_relation('09588b3c-5c7a-43e4-a643-5b445ca2b119', commerce_clause_text__expansive_federal_reading, coexists_with).
narrative_ontology:cs_reading_relation('09588b3c-5c7a-43e4-a643-5b445ca2b119', commerce_clause_text__originalist_narrow_reading, coexists_with).
narrative_ontology:cs_axiom('09588b3c-5c7a-43e4-a643-5b445ca2b119', foundational, economic_activity_predicate).
narrative_ontology:cs_axiom_status(economic_activity_predicate, holdable).
narrative_ontology:cs_axiom_grounding('09588b3c-5c7a-43e4-a643-5b445ca2b119', economic_activity_predicate, conventional).
narrative_ontology:cs_axiom('09588b3c-5c7a-43e4-a643-5b445ca2b119', foundational, non_pretextual_commerce_doctrine).
narrative_ontology:cs_axiom_status(non_pretextual_commerce_doctrine, holdable).
narrative_ontology:cs_axiom_grounding('09588b3c-5c7a-43e4-a643-5b445ca2b119', non_pretextual_commerce_doctrine, conventional).
narrative_ontology:cs_reference_frame('09588b3c-5c7a-43e4-a643-5b445ca2b119', federalism_balanced_commerce_authority).
narrative_ontology:cs_drift_state('09588b3c-5c7a-43e4-a643-5b445ca2b119', post_federalism_revival_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('09588b3c-5c7a-43e4-a643-5b445ca2b119', '').
narrative_ontology:cs_kernel_id(commerce_clause_text__substantial_effects_limited_reading, commerce_clause_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(commerce_clause_text__substantial_effects_limited_reading, state_governments).
narrative_ontology:constraint_beneficiary(commerce_clause_text__substantial_effects_limited_reading, local_non_economic_actors).
narrative_ontology:constraint_victim(commerce_clause_text__substantial_effects_limited_reading, federal_congress).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Defines and enforces the boundaries of federal commerce power through doctrine, deciding whether intrastate activity has the requisite jurisdictional nexus and whether federal regulation is pretextual or genuinely economic.
narrative_ontology:constraint_stakeholder(commerce_clause_text__substantial_effects_limited_reading, supreme_court, agenda_setter,
    institutional, civilizational, analytical, national).

% Retain police powers and regulatory authority over non-economic intrastate activity; protected from federal preemption when the Court finds a lack of substantial interstate effects or pretextual commerce character.
narrative_ontology:constraint_stakeholder(commerce_clause_text__substantial_effects_limited_reading, state_governments, beneficiary,
    institutional, generational, constrained, national).

% Must demonstrate jurisdictional nexus and non-pretextual economic character when legislating under the Commerce Clause; risks invalidation when regulating non-economic intrastate activity such as criminal law or education.
narrative_ontology:constraint_stakeholder(commerce_clause_text__substantial_effects_limited_reading, federal_congress, payer,
    institutional, biographical, constrained, national).

% Engage in intrastate non-economic activity such as local education, criminal justice, or family law without direct federal regulatory interference; the economic-non-economic distinction shields their domains from federal commerce authority.
narrative_ontology:constraint_stakeholder(commerce_clause_text__substantial_effects_limited_reading, local_non_economic_actors, beneficiary,
    moderate, biographical, constrained, local).

% Contest the legitimacy of the economic-non-economic and non-pretextual limitations, arguing for an expansive reading that permits federal regulation of all intrastate activity with aggregate economic effects.
narrative_ontology:constraint_stakeholder(commerce_clause_text__substantial_effects_limited_reading, progressive_legal_scholars, observer,
    organized, biographical, mobile, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(commerce_clause_text__substantial_effects_limited_reading, state_governments).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the federal-state boundary by providing a doctrinal test that allocates regulatory jurisdiction: federal authority over intrastate economic activity with substantial interstate effects, state authority over non-economic police power matters.
% TRANSFER_FUNCTION: Moves regulatory authority over non-economic intrastate activity away from the federal Congress and preserves it for state governments and local actors; legitimizes federal authority only where economic character and jurisdictional nexus are shown.
% ABSENT_VOICES: National regulatory coalitions seeking federal solutions to non-economic social problems, such as gender-based violence or gun-free school zones, are effectively excluded from the commerce power when the Court applies the non-pretextual economic requirement.
% DISAPPEARANCE_RATIONALE: If the limitation vanished overnight, Congress could regulate non-economic intrastate activity under commerce pretexts, consolidating traditional state police powers at the federal level; states would lose their constitutional shield and the federal-state balance would reorganize.
% FOUNDING_PROBLEM: The need to empower federal regulation of genuinely interstate markets while preventing the Commerce Clause from becoming a general police power that obliterates state sovereignty and local self-governance.
% FOUNDING_PROBLEM_CORROBORATION: Independent federalist constitutional scholars attest to the problem of federal overreach from outside the immediate state-beneficiary apparatus; progressive legal scholars and federal legislators dispute that the problem is genuine, arguing the Commerce Clause was intended to be broadly empowering.
narrative_ontology:disappearance_verdict(commerce_clause_text__substantial_effects_limited_reading, world_rearranges).
narrative_ontology:founding_problem_status(commerce_clause_text__substantial_effects_limited_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(commerce_clause_text__substantial_effects_limited_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(commerce_clause_text__substantial_effects_limited_reading, 'none', 1).
narrative_ontology:epsilon_provenance(commerce_clause_text__substantial_effects_limited_reading, 0.52, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness (0.52) reflects the doctrine's meaningful curtailment of federal legislative freedom, tempered by the fact that it still authorizes broad economic regulation. Suppression (0.55) is moderate: the constraint suppresses federal regulatory alternatives in non-economic domains but does not suppress state alternatives. Theater_ratio rises to 0.50 because the economic/non-economic distinction has proven manipulable (e.g., Raich treating local medical marijuana as economic), suggesting an increasing performative component in category policing. Resistance is high (0.70) because federal legislators and progressive scholars actively contest the limitation. Measurements share a single time grid spanning the 1995â2025 interval.
 *
 * PERSPECTIVAL GAP:
 *   The federal Congress seat experiences this constraint as a structural limitation on legislative capacity â a high-d, high-chi extraction of its constitutional authority. The state governments seat experiences it as a protective rope preserving regulatory space â low-d, subsidized. The Supreme Court seat, as agenda-setter and doctrinal administrator, experiences near-zero extraction. The engine will compute divergent per-seat classifications from this structural asymmetry.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations (state_governments, local_non_economic_actors) derive low directionality because the constraint subsidizes their retained authority. Victim declaration (federal_congress) derives high directionality because the constraint structurally extracts federal regulatory capacity. The Court is neither beneficiary nor victim but agenda_setter with analytical exit, placing it at the beneficiary end. Progressive scholars are observers with mobile exit, giving them neutral/analytical directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â preventing federal consolidation of all governance under commerce pretexts â remains contested rather than dead. The constraint has not outlived its function because federalism tensions are structurally recurrent and the doctrine is still invoked to strike or save legislation. Mandatrophy is not declared. The hybrid structure prevents mislabeling: pure coordination (rope) would ignore the asymmetric cost to federal power; pure extraction (snare) would ignore the genuine coordination of interstate economic regulation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    economic_line_stability,
    'Is the economic/non-economic distinction a stable, administrable boundary, or a formalistic construct that collapses under pressure?',
    'Comparative analysis of judicial application consistency across circuits and time; measure inter-coder agreement on economic classification of challenged activities.',
    'If unstable, the constraint''s coordination function is weak and its primary effect is performative category-policing, pushing classification toward piton or snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(economic_line_stability, conceptual, 'Stability of the economic-non-economic doctrinal line').

omega_variable(
    federal_beneficiary_paradox,
    'Does the federal government derive net benefit from the doctrinal clarity this reading provides, or net cost from the authority it forecloses?',
    'Legislative tracking of how often Congress relies on vs. avoids commerce power in light of the doctrine; comparison with pre-Lopez legislative uncertainty.',
    'If net beneficiary, asymmetric extraction weakens and the constraint moves toward rope classification; if net cost, tangled_rope classification strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(federal_beneficiary_paradox, empirical, 'Whether federal clarity offsets federal authority loss').

omega_variable(
    constitutional_naturalness,
    'Does this constraint emerge from the constitutional text and structure, or is it a judicial construction that benefits federalism interests?',
    'Historical-originalist analysis of the Commerce Clause''s understood scope at founding; comparison with structural constitutional arguments.',
    'If purely constructed, the constraint is more susceptible to mandatrophy or piton classification if enforcement decays; if textually grounded, it behaves more like a durable commitment-system rule.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(constitutional_naturalness, conceptual, 'Textual vs constructed origin of the limitation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(commerce_clause_text__substantial_effects_limited_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cc_substantial_effects_tr_t0, commerce_clause_text__substantial_effects_limited_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(cc_substantial_effects_tr_t5, commerce_clause_text__substantial_effects_limited_reading, theater_ratio, 5, 0.25).
narrative_ontology:measurement(cc_substantial_effects_tr_t10, commerce_clause_text__substantial_effects_limited_reading, theater_ratio, 10, 0.45).
narrative_ontology:measurement(cc_substantial_effects_tr_t15, commerce_clause_text__substantial_effects_limited_reading, theater_ratio, 15, 0.5).
narrative_ontology:measurement(cc_substantial_effects_tr_t20, commerce_clause_text__substantial_effects_limited_reading, theater_ratio, 20, 0.55).
narrative_ontology:measurement(cc_substantial_effects_tr_t25, commerce_clause_text__substantial_effects_limited_reading, theater_ratio, 25, 0.52).
narrative_ontology:measurement(cc_substantial_effects_tr_t30, commerce_clause_text__substantial_effects_limited_reading, theater_ratio, 30, 0.5).

% Extraction over time
narrative_ontology:measurement(cc_substantial_effects_be_t0, commerce_clause_text__substantial_effects_limited_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(cc_substantial_effects_be_t5, commerce_clause_text__substantial_effects_limited_reading, base_extractiveness, 5, 0.62).
narrative_ontology:measurement(cc_substantial_effects_be_t10, commerce_clause_text__substantial_effects_limited_reading, base_extractiveness, 10, 0.42).
narrative_ontology:measurement(cc_substantial_effects_be_t15, commerce_clause_text__substantial_effects_limited_reading, base_extractiveness, 15, 0.45).
narrative_ontology:measurement(cc_substantial_effects_be_t20, commerce_clause_text__substantial_effects_limited_reading, base_extractiveness, 20, 0.5).
narrative_ontology:measurement(cc_substantial_effects_be_t25, commerce_clause_text__substantial_effects_limited_reading, base_extractiveness, 25, 0.48).
narrative_ontology:measurement(cc_substantial_effects_be_t30, commerce_clause_text__substantial_effects_limited_reading, base_extractiveness, 30, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(cc_substantial_effects_su_t0, commerce_clause_text__substantial_effects_limited_reading, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(cc_substantial_effects_su_t5, commerce_clause_text__substantial_effects_limited_reading, suppression_requirement, 5, 0.72).
narrative_ontology:measurement(cc_substantial_effects_su_t10, commerce_clause_text__substantial_effects_limited_reading, suppression_requirement, 10, 0.5).
narrative_ontology:measurement(cc_substantial_effects_su_t15, commerce_clause_text__substantial_effects_limited_reading, suppression_requirement, 15, 0.55).
narrative_ontology:measurement(cc_substantial_effects_su_t20, commerce_clause_text__substantial_effects_limited_reading, suppression_requirement, 20, 0.6).
narrative_ontology:measurement(cc_substantial_effects_su_t25, commerce_clause_text__substantial_effects_limited_reading, suppression_requirement, 25, 0.58).
narrative_ontology:measurement(cc_substantial_effects_su_t30, commerce_clause_text__substantial_effects_limited_reading, suppression_requirement, 30, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(commerce_clause_text__substantial_effects_limited_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(commerce_clause_text__substantial_effects_limited_reading, expansive_federal_reading).
narrative_ontology:affects_constraint(commerce_clause_text__substantial_effects_limited_reading, originalist_narrow_reading).

% DUAL FORMULATION NOTE:
% This reading is one of three structurally distinct interpretations of the Commerce Clause kernel. The expansive reading treats the clause as authorizing broad federal economic regulation without the economic-predicate or non-pretextual limits; the originalist reading restricts it to trade crossing state borders; this reading occupies the intermediate position. Each reading has a distinct epsilon and stakeholder structure and must be modeled as a separate constraint story.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
