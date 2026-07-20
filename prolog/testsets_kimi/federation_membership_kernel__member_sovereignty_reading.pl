% ============================================================================
% CONSTRAINT STORY: federation_membership_kernel__member_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_federation_membership_kernel__member_sovereignty_reading, []).

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
 *   constraint_id: federation_membership_kernel__member_sovereignty_reading
 *   human_readable: Member-State Sovereignty Reading of Federal Free Movement
 *   domain: political_economy/federalism/migration_policy
 *
 * SUMMARY:
 *   This constraint instantiates the member_sovereignty_reading of the
 *   federation_membership_kernel: within a federal polity with asymmetric
 *   welfare states, free movement rights are bounded by national welfare
 *   capacity and labor-market protection. Member states retain legal
 *   authority to exclude economically inactive migrants, protecting fiscal
 *   solvency and social solidarity institutions at the cost of constrained
 *   mobility for migrants and sending-state workers. The constraint is
 *   claimed as a necessary coordination mechanism for asymmetric federalism,
 *   while the metrics capture its substantially extractive and actively
 *   enforced character.
 *
 * KEY AGENTS:
 *   - receiving_state_governments (institutional/constrained) â agenda setters asserting exclusion authority
 *   - economically_inactive_migrants (powerless/trapped) â primary targets of exclusion
 *   - sending_state_workers (moderate/constrained) â secondary targets facing restricted labor-market access
 *   - receiving_state_labor_insiders (organized/constrained) â beneficiaries of reduced competition
 *   - receiving_state_welfare_recipients (moderate/constrained) â beneficiaries of protected solidarity institutions
 *   - supranational_judiciary (institutional/analytical) â excluded interpreter pushing expansive mobility
 *   - free_movement_advocates (organized/constrained) â excluded voice arguing for unbounded citizenship rights
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federation_membership_kernel__member_sovereignty_reading, 0.62).
domain_priors:suppression_score(federation_membership_kernel__member_sovereignty_reading, 0.75).
domain_priors:theater_ratio(federation_membership_kernel__member_sovereignty_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federation_membership_kernel__member_sovereignty_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(federation_membership_kernel__member_sovereignty_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(federation_membership_kernel__member_sovereignty_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(federation_membership_kernel__member_sovereignty_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(federation_membership_kernel__member_sovereignty_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federation_membership_kernel__member_sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(federation_membership_kernel__member_sovereignty_reading, "Member-State Sovereignty Reading of Federal Free Movement").
narrative_ontology:topic_domain(federation_membership_kernel__member_sovereignty_reading, "political_economy/federalism/migration_policy").

domain_priors:requires_active_enforcement(federation_membership_kernel__member_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federation_membership_kernel__member_sovereignty_reading, 'f8d4e61f-28d9-4c32-a43d-7d7b554934ec').
narrative_ontology:cs_kernel_codification('f8d4e61f-28d9-4c32-a43d-7d7b554934ec', formalized).
narrative_ontology:cs_authority_grounding('f8d4e61f-28d9-4c32-a43d-7d7b554934ec', lineage).
narrative_ontology:cs_interpretation_layer_present('f8d4e61f-28d9-4c32-a43d-7d7b554934ec').
narrative_ontology:cs_reading_relation('f8d4e61f-28d9-4c32-a43d-7d7b554934ec', federation_membership_kernel__integration_reading, forecloses).
narrative_ontology:cs_reading_relation('f8d4e61f-28d9-4c32-a43d-7d7b554934ec', federation_membership_kernel__welfare_coordination_reading, influences).
narrative_ontology:cs_axiom('f8d4e61f-28d9-4c32-a43d-7d7b554934ec', foundational, national_welfare_solidarity_precedes_mobility).
narrative_ontology:cs_axiom_status(national_welfare_solidarity_precedes_mobility, holdable).
narrative_ontology:cs_axiom_grounding('f8d4e61f-28d9-4c32-a43d-7d7b554934ec', national_welfare_solidarity_precedes_mobility, deontological).
narrative_ontology:cs_axiom('f8d4e61f-28d9-4c32-a43d-7d7b554934ec', foundational, member_state_exclusion_authority_treaty_reserved).
narrative_ontology:cs_axiom_status(member_state_exclusion_authority_treaty_reserved, holdable).
narrative_ontology:cs_axiom_grounding('f8d4e61f-28d9-4c32-a43d-7d7b554934ec', member_state_exclusion_authority_treaty_reserved, conventional).
narrative_ontology:cs_reference_frame('f8d4e61f-28d9-4c32-a43d-7d7b554934ec', member_state_welfare_autonomy).
narrative_ontology:cs_drift_state('f8d4e61f-28d9-4c32-a43d-7d7b554934ec', post_lisbon_citizenship_expansion, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('f8d4e61f-28d9-4c32-a43d-7d7b554934ec', '').
narrative_ontology:cs_kernel_id(federation_membership_kernel__member_sovereignty_reading, federation_membership_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federation_membership_kernel__member_sovereignty_reading, receiving_state_governments).
narrative_ontology:constraint_beneficiary(federation_membership_kernel__member_sovereignty_reading, receiving_state_labor_insiders).
narrative_ontology:constraint_beneficiary(federation_membership_kernel__member_sovereignty_reading, receiving_state_welfare_recipients).
narrative_ontology:constraint_victim(federation_membership_kernel__member_sovereignty_reading, economically_inactive_migrants).
narrative_ontology:constraint_victim(federation_membership_kernel__member_sovereignty_reading, sending_state_workers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Assert and administer legal authority to exclude economically inactive migrants and condition residence on self-sufficiency, citing protection of national welfare-state solvency and labor-market institutions.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__member_sovereignty_reading, receiving_state_governments, agenda_setter,
    institutional, generational, constrained, continental).

% Seek residence in receiving states but are refused or removed based on lack of economic activity; face administrative barriers and limited legal standing to challenge exclusion decisions.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__member_sovereignty_reading, economically_inactive_migrants, payer,
    powerless, immediate, trapped, national).

% Encounter restricted access to receiving-state labor markets due to protectionist measures justified by welfare-state preservation; legal mobility exists but practical entry is narrowed.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__member_sovereignty_reading, sending_state_workers, payer,
    moderate, biographical, constrained, national).

% Experience reduced labor-market competition and preserved wage-bargaining institutions under the exclusion regime, insulating their employment conditions from broader mobility pressures.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__member_sovereignty_reading, receiving_state_labor_insiders, beneficiary,
    organized, biographical, constrained, national).

% Receive social transfers within contribution-based systems whose fiscal boundaries are defended against claims from non-contributory migrants.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__member_sovereignty_reading, receiving_state_welfare_recipients, beneficiary,
    moderate, biographical, constrained, national).

% Would interpret free movement as a fundamental citizenship right with expansive scope; this reading structurally overrides that role by asserting member-state exclusion authority.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__member_sovereignty_reading, supranational_judiciary, excluded,
    institutional, generational, analytical, continental).

% Argue for unbounded free movement as a constitutive citizenship right; their position is sidelined by sovereignty-framed justifications in the current policy conversation.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__member_sovereignty_reading, free_movement_advocates, excluded,
    organized, generational, constrained, continental).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a federal free-movement zone with heterogeneous national welfare states by permitting states to restrict access for economically inactive migrants, thereby preventing fiscal free-riding and preserving contribution-based solidarity institutions.
% TRANSFER_FUNCTION: Moves mobility rights away from economically inactive migrants and sending-state workers toward receiving-state governments and labor-market insiders, in exchange for preserved welfare-state solvency.
% ABSENT_VOICES: Economically inactive migrants who are administratively excluded before gaining political voice; supranational judiciary advocates who would expand mobility rights beyond national welfare boundaries; sending-state governments whose labor pools are depleted.
% DISAPPEARANCE_RATIONALE: If the constraint vanished, receiving states would lose the legal tool to exclude non-contributory migrants, welfare systems would face immediate fiscal pressure, labor markets would absorb new competition, and the federal balance between mobility and solidarity would require fundamental renegotiation.
% FOUNDING_PROBLEM: How to sustain a federal free-movement area when member states operate distinct, contribution-based welfare systems with divergent fiscal capacities and labor-market protections, without triggering a race to the bottom in social standards.
% FOUNDING_PROBLEM_CORROBORATION: Comparative political economists and federalism theorists outside the benefiting parties (e.g., Scharpf, Pierson) attest to the structural tension between market integration and national welfare states; receiving-state governments assert the problem is live; excluded migrants and sending-state representatives dispute the framing but acknowledge the fiscal divergence.
narrative_ontology:disappearance_verdict(federation_membership_kernel__member_sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(federation_membership_kernel__member_sovereignty_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(federation_membership_kernel__member_sovereignty_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(federation_membership_kernel__member_sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(federation_membership_kernel__member_sovereignty_reading, 0.62, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(federation_membership_kernel__member_sovereignty_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(federation_membership_kernel__member_sovereignty_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(federation_membership_kernel__member_sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) is moderate-to-high because a significant class of people (economically inactive migrants) is actively excluded from a fundamental federal right, and sending-state workers face de facto restricted access. Suppression (0.75) is high because the constraint depends on administrative verification of economic activity, deportation machinery, and legal barriers to entry. Theater (0.42) reflects growing performative sovereignty rhetoric that sometimes exceeds demonstrated fiscal necessity. Accessibility collapse (0.45) is moderate: unconditional free movement remains a known alternative (enshrined in treaty text) but is increasingly inaccessible in practice. Resistance (0.60) captures sustained contestation from excluded migrants, sending states, and supranational institutions.
 *
 * PERSPECTIVAL GAP:
 *   Receiving-state governments experience the constraint as necessary protection of democratically legitimate solidarity institutions; excluded migrants experience it as arbitrary closure of federal rights; sending-state workers experience it as a ceiling on economic opportunity. The engine computes per-seat classification from these structural asymmetries â the same legal provision reads as coordination from the beneficiary seat and extraction from the target seat.
 *
 * DIRECTIONALITY LOGIC:
 *   Receiving-state governments, labor insiders, and welfare recipients are declared beneficiaries and sit near the beneficiary end of directionality (low d, damped effective extraction). Economically inactive migrants and sending-state workers are declared victims with constrained or trapped exit, placing them near the full-target end (high d, amplified effective extraction). The supranational judiciary and free-movement advocates are excluded from the benefit/cost structure entirely.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint is not a piton: it has concentrated beneficiaries who actively defend it, a live founding problem, and real functional content. It is not a pure snare because there is a genuine coordination problem (asymmetric welfare states within a federal mobility zone) that the constraint addresses. It is a tangled rope because the same legal structure that coordinates federal mobility simultaneously extracts from the mobility rights of specific categories of people.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'This constraint is the member_sovereignty_reading of the federation_membership_kernel. The integration_reading treats free movement as a fundamental citizenship right with supranational enforcement, while the welfare_coordination_reading preserves member state autonomy through system coordination rather than exclusion. Is the structural delta identified here (constrained mobility as victim, protected receiving-state welfare) an inevitable feature of federal membership with heterogeneous welfare states, or a contingent political choice?',
    'Comparative analysis of federal systems with and without mobility exclusions; empirical measurement of welfare-state fiscal impacts from non-contributory migration.',
    'If inevitable, the constraint approaches a structural feature of asymmetric federalism; if contingent, it is a tangled rope or snare whose classification depends on the balance of genuine coordination versus protectionist extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Whether the member-sovereignty reading is structurally necessitated by federal welfare heterogeneity or is a contingent extraction mechanism.').

omega_variable(
    welfare_solvency_pretext,
    'Is the asserted necessity of excluding economically inactive migrants to preserve welfare-state solvency empirically grounded, or is it a pretextual justification for labor-market protectionism?',
    'Cross-national fiscal-impact studies of non-contributory migration; comparison of welfare-spending trajectories in member states with stricter versus looser exclusion regimes.',
    'If pretextual, the coordination function is cover for extraction by receiving-state labor insiders, shifting classification toward snare; if grounded, the tangled-rope diagnosis holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(welfare_solvency_pretext, empirical, 'Empirical basis of the welfare-solvency justification for migrant exclusion.').

omega_variable(
    supranational_authority_exclusion,
    'Does the effective exclusion of supranational judicial review (ECJ expansive interpretation) represent a necessary boundary for welfare-state survival or an erosion of federal legal authority?',
    'Jurisprudential analysis of ECJ case-law divergence across policy areas; treaty-revision history and opt-out patterns.',
    'If the exclusion is unstable and reverses, the constraint''s enforcement structure weakens and directionality shifts for receiving-state governments.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(supranational_authority_exclusion, conceptual, 'Stability of the state-level exclusion authority against supranational legal integration.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership_kernel__member_sovereignty_reading, 0, 34).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(federation_membership_kernel_member_sovereignty_tr_t0, federation_membership_kernel__member_sovereignty_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(federation_membership_kernel_member_sovereignty_tr_t5, federation_membership_kernel__member_sovereignty_reading, theater_ratio, 5, 0.22).
narrative_ontology:measurement(federation_membership_kernel_member_sovereignty_tr_t10, federation_membership_kernel__member_sovereignty_reading, theater_ratio, 10, 0.25).
narrative_ontology:measurement(federation_membership_kernel_member_sovereignty_tr_t15, federation_membership_kernel__member_sovereignty_reading, theater_ratio, 15, 0.3).
narrative_ontology:measurement(federation_membership_kernel_member_sovereignty_tr_t20, federation_membership_kernel__member_sovereignty_reading, theater_ratio, 20, 0.35).
narrative_ontology:measurement(federation_membership_kernel_member_sovereignty_tr_t25, federation_membership_kernel__member_sovereignty_reading, theater_ratio, 25, 0.38).
narrative_ontology:measurement(federation_membership_kernel_member_sovereignty_tr_t30, federation_membership_kernel__member_sovereignty_reading, theater_ratio, 30, 0.4).
narrative_ontology:measurement(federation_membership_kernel_member_sovereignty_tr_t34, federation_membership_kernel__member_sovereignty_reading, theater_ratio, 34, 0.42).

% Extraction over time
narrative_ontology:measurement(federation_membership_kernel_member_sovereignty_be_t0, federation_membership_kernel__member_sovereignty_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(federation_membership_kernel_member_sovereignty_be_t5, federation_membership_kernel__member_sovereignty_reading, base_extractiveness, 5, 0.41).
narrative_ontology:measurement(federation_membership_kernel_member_sovereignty_be_t10, federation_membership_kernel__member_sovereignty_reading, base_extractiveness, 10, 0.45).
narrative_ontology:measurement(federation_membership_kernel_member_sovereignty_be_t15, federation_membership_kernel__member_sovereignty_reading, base_extractiveness, 15, 0.5).
narrative_ontology:measurement(federation_membership_kernel_member_sovereignty_be_t20, federation_membership_kernel__member_sovereignty_reading, base_extractiveness, 20, 0.54).
narrative_ontology:measurement(federation_membership_kernel_member_sovereignty_be_t25, federation_membership_kernel__member_sovereignty_reading, base_extractiveness, 25, 0.58).
narrative_ontology:measurement(federation_membership_kernel_member_sovereignty_be_t30, federation_membership_kernel__member_sovereignty_reading, base_extractiveness, 30, 0.6).
narrative_ontology:measurement(federation_membership_kernel_member_sovereignty_be_t34, federation_membership_kernel__member_sovereignty_reading, base_extractiveness, 34, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(federation_membership_kernel_member_sovereignty_su_t0, federation_membership_kernel__member_sovereignty_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(federation_membership_kernel_member_sovereignty_su_t5, federation_membership_kernel__member_sovereignty_reading, suppression_requirement, 5, 0.55).
narrative_ontology:measurement(federation_membership_kernel_member_sovereignty_su_t10, federation_membership_kernel__member_sovereignty_reading, suppression_requirement, 10, 0.6).
narrative_ontology:measurement(federation_membership_kernel_member_sovereignty_su_t15, federation_membership_kernel__member_sovereignty_reading, suppression_requirement, 15, 0.64).
narrative_ontology:measurement(federation_membership_kernel_member_sovereignty_su_t20, federation_membership_kernel__member_sovereignty_reading, suppression_requirement, 20, 0.68).
narrative_ontology:measurement(federation_membership_kernel_member_sovereignty_su_t25, federation_membership_kernel__member_sovereignty_reading, suppression_requirement, 25, 0.71).
narrative_ontology:measurement(federation_membership_kernel_member_sovereignty_su_t30, federation_membership_kernel__member_sovereignty_reading, suppression_requirement, 30, 0.73).
narrative_ontology:measurement(federation_membership_kernel_member_sovereignty_su_t34, federation_membership_kernel__member_sovereignty_reading, suppression_requirement, 34, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(federation_membership_kernel__member_sovereignty_reading, integration_reading).
narrative_ontology:affects_constraint(federation_membership_kernel__member_sovereignty_reading, welfare_coordination_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the federation_membership_kernel, decomposed from the colloquial label 'EU free movement' into three structurally distinct claims: integration_reading (supranational citizenship right), member_sovereignty_reading (state authority bounded by welfare capacity), and welfare_coordination_reading (inter-state coordination preserving autonomy). Each reading has a distinct epsilon, beneficiary/victim structure, and classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
