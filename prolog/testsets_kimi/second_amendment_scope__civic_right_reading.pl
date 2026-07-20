% ============================================================================
% CONSTRAINT STORY: second_amendment_scope__civic_right_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_second_amendment_scope__civic_right_reading, []).

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
 *   constraint_id: second_amendment_scope__civic_right_reading
 *   human_readable: Second Amendment Civic Right Reading (Individual Right Conditioned on Militia Participation)
 *   domain: constitutional_law/political_theory/rights_jurisprudence
 *
 * SUMMARY:
 *   This constraint instantiates the civic-right reading of the Second
 *   Amendment kernel: the right to keep and bear arms is an individual right,
 *   but its exercise is conditioned on participation in civic militia
 *   service. The reading attempts to thread between the collective-right
 *   reading (state authority only) and the individual-right reading
 *   (unconnected individual possession). It names militia-eligible
 *   individuals as beneficiaries and non-militia individuals as payers, with
 *   regulatory authorities serving as gatekeepers. Because the right is gated
 *   by a regulatory definition of militia status, the constraint carries
 *   moderate extractiveness: those outside the eligible class are denied
 *   constitutional protection. Active enforcement is required to maintain the
 *   eligibility boundary. The constraint is claimed as tangled_rope because
 *   it couples a genuine coordination function (civic defense through
 *   militia) with asymmetric extraction (disarmament of the non-eligible).
 *
 * KEY AGENTS:
 *   - militia_eligible_individuals: Beneficiary (moderate/constrained) â gain conditional right
 *   - non_militia_individuals: Payer (powerless/trapped) â bear exclusion cost
 *   - militia_regulatory_authority: Agenda setter (institutional/arbitrage) â defines and enforces eligibility
 *   - unconditioned_rights_advocates: Excluded (organized/mobile) â oppose militia condition
 *   - collective_rights_advocates: Excluded (organized/mobile) â oppose individual possession
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_scope__civic_right_reading, 0.5).
domain_priors:suppression_score(second_amendment_scope__civic_right_reading, 0.45).
domain_priors:theater_ratio(second_amendment_scope__civic_right_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_scope__civic_right_reading, extractiveness, 0.5).
narrative_ontology:constraint_metric(second_amendment_scope__civic_right_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(second_amendment_scope__civic_right_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_scope__civic_right_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(second_amendment_scope__civic_right_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_scope__civic_right_reading, tangled_rope).
narrative_ontology:human_readable(second_amendment_scope__civic_right_reading, "Second Amendment Civic Right Reading (Individual Right Conditioned on Militia Participation)").
narrative_ontology:topic_domain(second_amendment_scope__civic_right_reading, "constitutional_law/political_theory/rights_jurisprudence").

domain_priors:requires_active_enforcement(second_amendment_scope__civic_right_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_scope__civic_right_reading, '0d589e24-d457-4bd2-87c2-f91489651533').
narrative_ontology:cs_kernel_codification('0d589e24-d457-4bd2-87c2-f91489651533', fixed_text).
narrative_ontology:cs_authority_grounding('0d589e24-d457-4bd2-87c2-f91489651533', lineage).
narrative_ontology:cs_interpretation_layer_present('0d589e24-d457-4bd2-87c2-f91489651533').
narrative_ontology:cs_reading_relation('0d589e24-d457-4bd2-87c2-f91489651533', second_amendment_scope__individual_right_reading, forecloses).
narrative_ontology:cs_reading_relation('0d589e24-d457-4bd2-87c2-f91489651533', second_amendment_scope__collective_right_reading, forecloses).
narrative_ontology:cs_axiom('0d589e24-d457-4bd2-87c2-f91489651533', foundational, arms_bearing_conditioned_on_militia_status).
narrative_ontology:cs_axiom_status(arms_bearing_conditioned_on_militia_status, holdable).
narrative_ontology:cs_axiom_grounding('0d589e24-d457-4bd2-87c2-f91489651533', arms_bearing_conditioned_on_militia_status, conventional).
narrative_ontology:cs_axiom('0d589e24-d457-4bd2-87c2-f91489651533', secondary, regulatory_gatekeeping_legitimate_for_public_safety).
narrative_ontology:cs_axiom_status(regulatory_gatekeeping_legitimate_for_public_safety, holdable).
narrative_ontology:cs_axiom_grounding('0d589e24-d457-4bd2-87c2-f91489651533', regulatory_gatekeeping_legitimate_for_public_safety, instrumental).
narrative_ontology:cs_reference_frame('0d589e24-d457-4bd2-87c2-f91489651533', civic_republican_militia_tradition).
narrative_ontology:cs_drift_state('0d589e24-d457-4bd2-87c2-f91489651533', contemporary_post_heller_era, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('0d589e24-d457-4bd2-87c2-f91489651533', '').
narrative_ontology:cs_kernel_id(second_amendment_scope__civic_right_reading, second_amendment_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_scope__civic_right_reading, militia_eligible_individuals).
narrative_ontology:constraint_victim(second_amendment_scope__civic_right_reading, non_militia_individuals).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals who meet regulatory definitions of militia eligibility and thereby gain a conditional constitutional right to keep and bear arms under the civic-right reading. Their possession is protected only so long as they maintain the specified civic status or readiness, and they must comply with whatever training, enrollment, or equipment rules regulators impose.
narrative_ontology:constraint_stakeholder(second_amendment_scope__civic_right_reading, militia_eligible_individuals, beneficiary,
    moderate, biographical, constrained, national).

% Individuals who fall outside militia eligibility criteriaâwhether by age, disability, gender under historical definitions, conscientious objection, or regulatory exclusionâand are consequently denied Second Amendment protection. They bear the cost of disarmament or criminal liability for conduct that would be protected under an unconditioned individual-right reading.
narrative_ontology:constraint_stakeholder(second_amendment_scope__civic_right_reading, non_militia_individuals, payer,
    powerless, biographical, trapped, national).

% Federal and state courts together with legislative and executive actors that define who counts as militia, what constitutes service, and how the condition is enforced. They set the boundaries of the right and adjudicate claims, possessing wide discretion to expand or contract the beneficiary class.
narrative_ontology:constraint_stakeholder(second_amendment_scope__civic_right_reading, militia_regulatory_authority, agenda_setter,
    institutional, generational, arbitrage, national).

% Advocacy organizations and legal scholars who argue for an individual right to firearms wholly unconnected to militia service. They are structurally excluded from the civic-right framework because their preferred reading removes the regulatory gate entirely, collapsing the distinction between eligible and non-eligible individuals.
narrative_ontology:constraint_stakeholder(second_amendment_scope__civic_right_reading, unconditioned_rights_advocates, excluded,
    organized, generational, mobile, national).

% Advocates who assert that the Second Amendment protects only state authority to organize and maintain official militias, denying any individual right to possess arms. Under the civic-right reading they are sidelined because the reading explicitly recognizes an individual possessory right, albeit a conditional one.
narrative_ontology:constraint_stakeholder(second_amendment_scope__civic_right_reading, collective_rights_advocates, excluded,
    organized, generational, mobile, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To ensure a 'well regulated Militia' is available for civil defense by conditioning individual arms-bearing on civic militia participation, thereby linking private armament to public security and creating a regulatory hook for eligibility.
% TRANSFER_FUNCTION: Transfers the constitutional right to keep and bear arms from a general individual entitlement to a conditional grant tied to regulatory militia status, and transfers gatekeeping authority to the state to define eligibility and enforce exclusion of the non-eligible.
% ABSENT_VOICES: Unconditioned individual-rights advocates are excluded because the civic condition forecloses their claim that the right is untethered from service. Collective-rights advocates are excluded because the reading recognizes individual possession, undermining the state-monopoly claim. Non-militia individuals are present as payers but lack effective voice in the interpretive framework that defines their exclusion.
% DISAPPEARANCE_RATIONALE: If this reading vanished overnight, militia-eligible individuals would lose their conditional constitutional protection unless another reading immediately replaced it. Regulatory authorities would lose their gating power. Non-militia individuals would gain potential access if an unconditioned reading took hold, or lose even conditional access if a collective reading prevailed. The distribution of firearms rights and regulatory authority would reorganize around whichever alternative reading filled the void.
% FOUNDING_PROBLEM: The founding generation sought to prevent the federal government from disarming the citizenry that formed the state militias, ensuring local defense capability without maintaining a large standing army.
% FOUNDING_PROBLEM_CORROBORATION: Historical historians and textual scholars outside the direct beneficiary set corroborate the militia-purpose origin; however, competing historians argue the right was always individual. The corroboration is contested by organized legal advocacy groups with divergent ideological commitments, and no single corroborating source is accepted by all parties.
narrative_ontology:disappearance_verdict(second_amendment_scope__civic_right_reading, world_rearranges).
narrative_ontology:founding_problem_status(second_amendment_scope__civic_right_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_scope__civic_right_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(second_amendment_scope__civic_right_reading, 'none', 1).
narrative_ontology:epsilon_provenance(second_amendment_scope__civic_right_reading, 0.5, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_scope__civic_right_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(second_amendment_scope__civic_right_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(second_amendment_scope__civic_right_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is set at 0.50 to reflect the service-based gate: the constraint does not extract money but extracts the right itself from non-militia individuals. Suppression is 0.45 because the boundary requires active regulatory enforcement and judicial policing of eligibility, though it is not a totalitarian suppression. Theater ratio is 0.30 (rising to 0.40 at peak) because much public argumentation invokes revolutionary-era militia imagery that exceeds contemporary militia functionality. Accessibility collapse is 0.60: once this reading is adopted in a jurisdiction, alternative readings (especially unconditioned individual right) are foreclosed for regulated parties. Resistance is 0.55 because both individual-rights absolutists and collective-rights advocates actively contest the reading.
 *
 * PERSPECTIVAL GAP:
 *   The militia-eligible beneficiary experiences the constraint as a protective constitutional privilege; the excluded payer experiences it as a denial of equal rights. The regulatory authority experiences it as a legitimate discretionary power derived from the text. The gap is structural: the same legal provision that arms one class disarms another, and the distinction rests on a regulatory definition that the agenda setter controls.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations map to militia_eligible_individuals (low d, subsidized by the constraint's protection) and victim declarations map to non_militia_individuals (high d, extraction via exclusion). The regulatory authority sits near the beneficiary end in terms of power and scope because the constraint expands its authority. Excluded advocates sit at moderate d because they are not directly governed by the constraint but are silenced by its adoption.
 *
 * MANDATROPHY ANALYSIS:
 *   The civic-right reading resists simple mandatrophy classification because it explicitly tethers the modern right to the original militia function. However, if the founding problemâfederal disarmament of local militiasâis dead (contested status), then the militia condition risks becoming performative. The reading then functions as a scaffold that forgot to sunset, or a piton if maintained only by historical theater. The contested founding_problem_status prevents a clean verdict, which is why the constraint is claimed as tangled_rope rather than piton.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    civic_militia_viability,
    'Is the civic militia a functioning contemporary institution that justifies conditioning a constitutional right on participation, or has it become a historical artifact making the condition a hollow gate?',
    'Empirical inventory of state militia and organized civilian training programs combined with historical usage data for militia call-ups versus standing-army deployment.',
    'If the militia is non-viable, the coordination function collapses and the constraint shifts from tangled_rope toward snare or piton; if viable, the service-based gating retains structural justification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(civic_militia_viability, empirical, 'Whether militia service is a live coordination mechanism or a historical fiction.').

omega_variable(
    sibling_reading_foreclosure,
    'Does adoption of the civic-right reading logically foreclose the individual-right and collective-right readings within a single authoritative framework, or can all three persist as interpretive options?',
    'Jurisprudential analysis of whether a single court can consistently hold that the right is individual-and-conditioned while also treating it as individual-and-unconditioned or collective-and-state-only.',
    'If genuine foreclosure exists, the civic reading''s resistance and suppression metrics are amplified because advocates of sibling readings are structurally silenced; if coexistence is possible, the constraint behaves more like a contested rope with lower suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure, conceptual, 'Whether this reading logically excludes its siblings in the same commitment framework.').

omega_variable(
    regulatory_gatekeeper_capture,
    'Does the regulatory authority defining militia eligibility operate with neutral civic criteria, or does it systematically exclude disfavored groups to convert the militia condition into a tool of social control?',
    'Historical and contemporary demographic analysis of militia eligibility rules against patterns of political, racial, or ideological exclusion.',
    'Systematic capture would raise extractiveness and suppressiveness, pushing classification toward snare; neutral operation supports the tangled-rope assessment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_gatekeeper_capture, empirical, 'Whether militia eligibility gatekeeping is captured by partisan social control.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_scope__civic_right_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sa_civic_tr_t0, second_amendment_scope__civic_right_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(sa_civic_tr_t10, second_amendment_scope__civic_right_reading, theater_ratio, 10, 0.3).
narrative_ontology:measurement(sa_civic_tr_t20, second_amendment_scope__civic_right_reading, theater_ratio, 20, 0.4).
narrative_ontology:measurement(sa_civic_tr_t30, second_amendment_scope__civic_right_reading, theater_ratio, 30, 0.35).

% Extraction over time
narrative_ontology:measurement(sa_civic_be_t0, second_amendment_scope__civic_right_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(sa_civic_be_t10, second_amendment_scope__civic_right_reading, base_extractiveness, 10, 0.55).
narrative_ontology:measurement(sa_civic_be_t20, second_amendment_scope__civic_right_reading, base_extractiveness, 20, 0.6).
narrative_ontology:measurement(sa_civic_be_t30, second_amendment_scope__civic_right_reading, base_extractiveness, 30, 0.5).

% Suppression requirement over time
narrative_ontology:measurement(sa_civic_su_t0, second_amendment_scope__civic_right_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(sa_civic_su_t10, second_amendment_scope__civic_right_reading, suppression_requirement, 10, 0.45).
narrative_ontology:measurement(sa_civic_su_t20, second_amendment_scope__civic_right_reading, suppression_requirement, 20, 0.5).
narrative_ontology:measurement(sa_civic_su_t30, second_amendment_scope__civic_right_reading, suppression_requirement, 30, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_scope__civic_right_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(second_amendment_scope__civic_right_reading, second_amendment_scope__individual_right_reading).
narrative_ontology:affects_constraint(second_amendment_scope__civic_right_reading, second_amendment_scope__collective_right_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the second_amendment_scope kernel. The kernel decomposes into three structurally distinct constraints because the epsilon values and beneficiary/victim structures differ across readings: the civic-right reading conditions individual possession on militia service (moderate epsilon, mixed beneficiary/victim), the individual-right reading removes the militia nexus (lower epsilon for beneficiaries, different victim set), and the collective-right reading denies individual possession entirely (high epsilon, state beneficiary). Each reading gets its own constraint story linked by the network edges.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
