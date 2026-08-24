% ============================================================================
% CONSTRAINT STORY: federation_membership_kernel__welfare_coordination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-01-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_federation_membership_kernel__welfare_coordination_reading, []).

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
 *   constraint_id: federation_membership_kernel__welfare_coordination_reading
 *   human_readable: EU Welfare Coordination under Free Movement
 *   domain: political_economy/federalism/migration_policy/welfare_state
 *
 * SUMMARY:
 *   The EU's welfare coordination reading of free movement treats national
 *   welfare systems as sovereign design spaces that must be coordinated — not
 *   harmonized — to prevent social dumping. The Posting of Workers Directive
 *   (1996, revised 2014, 2018) and Enforcement Directive (2014) create a
 *   framework where receiving states apply core labor standards to posted
 *   workers while sending states retain social security competence for 24
 *   months. This preserves member state autonomy but generates a structural
 *   extraction pattern: posted workers bear the cost of the coordination gap
 *   (lower wages, missing protections), sending states lose human capital
 *   without compensation, and receiving-state labor markets face dual
 *   pressure from posting and displacement. The constraint is claimed as
 *   coordination (tangled_rope) but operates with measurable extraction that
 *   has grown over three decades.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federation_membership_kernel__welfare_coordination_reading, 0.65).
domain_priors:suppression_score(federation_membership_kernel__welfare_coordination_reading, 0.55).
domain_priors:theater_ratio(federation_membership_kernel__welfare_coordination_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federation_membership_kernel__welfare_coordination_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(federation_membership_kernel__welfare_coordination_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(federation_membership_kernel__welfare_coordination_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(federation_membership_kernel__welfare_coordination_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(federation_membership_kernel__welfare_coordination_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federation_membership_kernel__welfare_coordination_reading, tangled_rope).
narrative_ontology:human_readable(federation_membership_kernel__welfare_coordination_reading, "EU Welfare Coordination under Free Movement").
narrative_ontology:topic_domain(federation_membership_kernel__welfare_coordination_reading, "political_economy/federalism/migration_policy/welfare_state").

domain_priors:requires_active_enforcement(federation_membership_kernel__welfare_coordination_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federation_membership_kernel__welfare_coordination_reading, 'e2b56606-3b3d-4ca5-b5ea-1b7749f2d885').
narrative_ontology:cs_kernel_codification('e2b56606-3b3d-4ca5-b5ea-1b7749f2d885', formalized).
narrative_ontology:cs_authority_grounding('e2b56606-3b3d-4ca5-b5ea-1b7749f2d885', lineage).
narrative_ontology:cs_interpretation_layer_present('e2b56606-3b3d-4ca5-b5ea-1b7749f2d885').
narrative_ontology:cs_reading_relation('e2b56606-3b3d-4ca5-b5ea-1b7749f2d885', federation_membership_kernel__integration_reading, coexists_with).
narrative_ontology:cs_reading_relation('e2b56606-3b3d-4ca5-b5ea-1b7749f2d885', federation_membership_kernel__member_sovereignty_reading, coexists_with).
narrative_ontology:cs_axiom('e2b56606-3b3d-4ca5-b5ea-1b7749f2d885', foundational, welfare_coordination_preserves_autonomy).
narrative_ontology:cs_axiom_status(welfare_coordination_preserves_autonomy, holdable).
narrative_ontology:cs_axiom_grounding('e2b56606-3b3d-4ca5-b5ea-1b7749f2d885', welfare_coordination_preserves_autonomy, conventional).
narrative_ontology:cs_axiom('e2b56606-3b3d-4ca5-b5ea-1b7749f2d885', foundational, anti_dumping_as_coordination_not_harmonization).
narrative_ontology:cs_axiom_status(anti_dumping_as_coordination_not_harmonization, holdable).
narrative_ontology:cs_axiom_grounding('e2b56606-3b3d-4ca5-b5ea-1b7749f2d885', anti_dumping_as_coordination_not_harmonization, conventional).
narrative_ontology:cs_reference_frame('e2b56606-3b3d-4ca5-b5ea-1b7749f2d885', coordinated_welfare_federalism).
narrative_ontology:cs_drift_state('e2b56606-3b3d-4ca5-b5ea-1b7749f2d885', post_laval_viking_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('e2b56606-3b3d-4ca5-b5ea-1b7749f2d885', '').
narrative_ontology:cs_kernel_id(federation_membership_kernel__welfare_coordination_reading, federation_membership_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federation_membership_kernel__welfare_coordination_reading, eu_institutions).
narrative_ontology:constraint_beneficiary(federation_membership_kernel__welfare_coordination_reading, receiving_member_states).
narrative_ontology:constraint_beneficiary(federation_membership_kernel__welfare_coordination_reading, employers_in_receiving_states).
narrative_ontology:constraint_victim(federation_membership_kernel__welfare_coordination_reading, posted_workers).
narrative_ontology:constraint_victim(federation_membership_kernel__welfare_coordination_reading, receiving_state_labor_markets).
narrative_ontology:constraint_victim(federation_membership_kernel__welfare_coordination_reading, sending_member_states).
narrative_ontology:constraint_victim(federation_membership_kernel__welfare_coordination_reading, permanent_migrants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(federation_membership_kernel__welfare_coordination_reading, receiving_member_states).
narrative_ontology:constraint_vindicates(federation_membership_kernel__welfare_coordination_reading, welfare_design_autonomy).
narrative_ontology:constraint_vindicates(federation_membership_kernel__welfare_coordination_reading, anti_social_dumping).
narrative_ontology:constraint_vindicates(federation_membership_kernel__welfare_coordination_reading, coordinated_free_movement).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Commission and ECJ set and enforce the posting directives (1996, 2014, 2018) that coordinate national welfare systems while policing social dumping. They collect no direct revenue but their authority and legitimacy depend on maintaining the single market's social dimension. Exit is analytical — they observe the constraint from the rule-making seat.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__welfare_coordination_reading, eu_institutions, agenda_setter,
    institutional, generational, analytical, universal).

% Benefit from flexible labor supply for sectors with shortages (construction, transport, care) while preserving autonomy over their welfare designs. Simultaneously bear fiscal and social costs when posted workers undercut local standards and permanent migrants are displaced. Cannot exit the coordination without leaving the single market; treaty change is the only structural exit.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__welfare_coordination_reading, receiving_member_states, beneficiary,
    institutional, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(federation_membership_kernel__welfare_coordination_reading, receiving_member_states, payer).

% Lose skilled and semi-skilled workers to posting without fiscal compensation for the training and social investment those workers represent. Remittances partially offset but do not replace lost tax base and care infrastructure strain. Treaty-constrained exit; political voice in Council but outvoted on posting enforcement.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__welfare_coordination_reading, sending_member_states, payer,
    institutional, generational, constrained, continental).

% Employed by sending-state firms to work temporarily in receiving states under 2-year social security exemption (posting directive) and cabotage rules that allow wage undercutting. Earn less than local equivalents, lack access to receiving-state social protections, and face precarious contract chains. Exit options limited by language, qualification recognition, and dependence on posting employer.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__welfare_coordination_reading, posted_workers, payer,
    moderate, biographical, constrained, continental).

% Face dual pressure: posted workers undercut collectively bargained wages in sectors like construction and road transport, while permanent migrants — who have full social rights — are displaced from entry-level positions. Trade unions and works councils resist but enforcement of equal-pay rules is patchy and litigation slow.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__welfare_coordination_reading, receiving_state_labor_markets, payer,
    organized, biographical, constrained, national).

% Hold full free movement rights and social integration but are structurally disadvantaged when employers prefer posted workers who cost less due to social levy exemptions. Their voice in policy is mediated through integration councils and unions but they are not a constituted party in posting directive negotiations.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__welfare_coordination_reading, permanent_migrants, excluded,
    moderate, biographical, constrained, national).

% Access posted labor at lower effective cost (social security savings, wage differentials) while benefiting from single market regulatory stability. Can relocate production or subcontract across borders; exit is arbitrage-grade. Lobby at EU and national levels to preserve posting flexibility.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__welfare_coordination_reading, employers_in_receiving_states, beneficiary,
    powerful, biographical, mobile, continental).

% Advocate for equal treatment and enforcement of anti-dumping rules (posted workers directive, enforcement directive). Provide legal support to posted workers and pressure national governments. Their analytical seat is informed by cross-border organizing but they do not set the agenda.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__welfare_coordination_reading, trade_unions, observer,
    organized, biographical, mobile, continental).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates national welfare systems to prevent a race-to-the-bottom in labor standards while preserving each member state's autonomy to design its own social protection architecture — avoiding supranational harmonization that would require treaty-level consensus.
% TRANSFER_FUNCTION: Moves labor cost advantages from posted workers (who forego receiving-state social protections for 2 years) and sending states (which lose trained workers without fiscal compensation) to employers in receiving states (who access cheaper labor) and receiving-state budgets (which avoid social insurance costs for temporary workers).
% ABSENT_VOICES: Posted workers themselves — often employed by letterbox companies, linguistically isolated, and dependent on posting employers — are structurally excluded from the tripartite negotiations that shape posting directives. Sending-state communities experiencing care drain and fiscal erosion have no formal representation in receiving-state labor ministries or EU comitology.
% DISAPPEARANCE_RATIONALE: If the coordination framework vanished, receiving states would face unchecked social dumping, likely triggering either (a) national closure measures violating free movement, or (b) pressure for supranational welfare harmonization that member states have rejected for decades. The single market's social legitimacy would fracture.
% FOUNDING_PROBLEM: Post-1992 single market completion created a regulatory gap: free movement of workers and services without welfare harmonization risked social dumping — firms posting workers to undercut host-state standards — while member states refused to surrender welfare design competence to Brussels.
% FOUNDING_PROBLEM_CORROBORATION: European Commission's 2020 evaluation of the enforcement directive documents persistent non-compliance and enforcement gaps. ETUC and national trade union confederations attest the problem has mutated (platform work, bogus self-employment) rather than been solved. Academic literature (Ferrera, Leibfried, Pennings) argues the coordination architecture contains the problem but does not resolve the underlying asymmetry. No corroborating source outside the Commission's own directorates claims the founding problem is fully live in its original form.
narrative_ontology:disappearance_verdict(federation_membership_kernel__welfare_coordination_reading, world_rearranges).
narrative_ontology:founding_problem_status(federation_membership_kernel__welfare_coordination_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(federation_membership_kernel__welfare_coordination_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(federation_membership_kernel__welfare_coordination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(federation_membership_kernel__welfare_coordination_reading, 0.65, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(federation_membership_kernel__welfare_coordination_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(federation_membership_kernel__welfare_coordination_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(federation_membership_kernel__welfare_coordination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.65) reflects the cumulative advantage shift: the 2-year social security exemption and cabotage wage differentials create a structural cost advantage for posting employers that is not justified by service cost differences. Suppression (0.55) is moderate — alternatives (national closure, supranational harmonization) are legally blocked or politically toxic, but the coordination framework itself is the only legal path, not a coercively imposed one. Theater ratio (0.38) captures the gap between the anti-dumping rhetoric and the enforcement reality: the 2018 revision strengthened rules but letterbox companies, bogus posting, and platform-mediated work create new evasion surfaces. Accessibility collapse (0.48) and resistance (0.58) reflect that alternatives exist (posted workers can become permanent migrants, unions can litigate) but are costly and slow.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (EU institutions) experiences this as genuine coordination preserving the single market's social legitimacy. The payer seats (posted workers, sending states, receiving-state labor markets) experience the same structure as enforced extraction enabled by the coordination framework. The engine computes this divergence from the structural data — the claimed_type (tangled_rope) acknowledges both coordination and extraction are real; the per-seat effective extraction will reveal which function dominates for whom.
 *
 * DIRECTIONALITY LOGIC:
 *   EU institutions sit at the analytical/agenda-setter pole (d near 0.0) — they design and enforce the coordination but extract no direct rents. Receiving member states are dual-positioned: beneficiary of labor flexibility and welfare autonomy (d ~ 0.3), payer of social costs and displacement (d ~ 0.7). The engine will compute a blended d from the structural data. Sending states are payers (d ~ 0.8) — they lose workers and fiscal base with no compensation mechanism. Posted workers are the most extracted (d ~ 0.9) — trapped in temporary status, excluded from receiving-state protections, dependent on posting employer. Receiving-state labor markets (d ~ 0.75) bear wage undercutting and displacement. Permanent migrants (d ~ 0.6) are excluded from the coordination bargain but suffer its externalities. Employers in receiving states are beneficiaries (d ~ 0.2) with arbitrage-grade exit.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (social dumping risk in a non-harmonized single market) remains live but has mutated: platform work, bogus self-employment, and third-country posting create evasion surfaces the 1996 architecture did not anticipate. The coordination function is real — without it, race-to-bottom dynamics would force either market fragmentation or unwanted harmonization. But the extraction function has grown because the enforcement architecture (reliant on national labor inspectorates with cross-border coordination gaps) has not kept pace with posting business models. This is not mandatrophy (the mandate is not obsolete) but mandate drift: the coordination machinery persists while the extraction it enables has expanded.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_committer_structure,
    'How does the welfare_coordination_reading structurally relate to the federation_membership_kernel and its sibling readings (integration_reading, member_sovereignty_reading)?',
    'Committer-frame analysis: this reading instantiates one constraint from the kernel; sibling readings instantiate different constraints with different beneficiary/victim sets and ε values. The kernel is the contested commitment (free movement); each reading is a distinct constraint story.',
    'If the kernel is treated as a single constraint, ε becomes observer-relative and classification becomes unstable. Decomposing into three constraint stories linked by network.affects_constraints preserves ε-invariance.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'Committer structure: this story is one reading of a contested kernel; sibling readings are separate constraints.').

omega_variable(
    coordination_vs_extraction_boundary,
    'Is the anti-social-dumping coordination function genuine (prevents race-to-bottom) or does it primarily legitimate an extraction architecture that benefits employers and receiving states?',
    'Counterfactual: if posting directives were repealed but free movement remained, would labor standards converge downward (coordination was real) or would member states erect barriers (coordination was preventing closure)? Compare posting-intensive sectors with non-posting sectors.',
    'If coordination is genuine, the tangled_rope classification holds — both functions coexist. If coordination is cover, the constraint reclassifies toward snare (extraction with coordination theater).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_vs_extraction_boundary, empirical, 'Whether the coordination function is structurally necessary or a legitimating cover for extraction.').

omega_variable(
    posted_worker_exploitation_structural_vs_enforcement_gap,
    'Is posted worker exploitation (wage undercutting, missing protections) a structural feature of the 2-year exemption architecture or an enforcement gap that better implementation would close?',
    'Track enforcement directive (2014) implementation across member states: if exploitation persists in high-enforcement states (e.g., Nordic), it is structural; if it correlates with enforcement capacity, it is a gap. ECJ case law (Laval, Viking, Rüffert) also illuminates whether the treaty architecture itself permits the exploitation.',
    'If structural, the extraction is intrinsic to the coordination design (tangled_rope confirmed). If enforcement gap, the extraction is contingent and the constraint could evolve toward rope with stronger enforcement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(posted_worker_exploitation_structural_vs_enforcement_gap, empirical, 'Whether posted worker exploitation is designed into the coordination or results from implementation failure.').

omega_variable(
    sending_state_compensation_feasibility,
    'Could a fiscal compensation mechanism for sending states (e.g., posting levy, training cost reimbursement) be designed without violating the coordination principle of welfare design autonomy?',
    'Analyze whether the EU''s own resources system or a dedicated posting fund could compensate sending states while preserving national welfare competence. Test against treaty constraints (Article 153 TFEU, subsidiarity) and political feasibility in Council.',
    'If feasible, the sending-state victim status is addressable within the current reading — the constraint has a reform path. If infeasible, sending-state extraction is a structural feature of the coordination architecture.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sending_state_compensation_feasibility, conceptual, 'Whether sending state losses can be compensated without breaking the welfare coordination logic.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership_kernel__welfare_coordination_reading, 1996, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fmk_wcr_tr_t1996, federation_membership_kernel__welfare_coordination_reading, theater_ratio, 1996, 0.22).
narrative_ontology:measurement(fmk_wcr_tr_t2004, federation_membership_kernel__welfare_coordination_reading, theater_ratio, 2004, 0.28).
narrative_ontology:measurement(fmk_wcr_tr_t2010, federation_membership_kernel__welfare_coordination_reading, theater_ratio, 2010, 0.33).
narrative_ontology:measurement(fmk_wcr_tr_t2014, federation_membership_kernel__welfare_coordination_reading, theater_ratio, 2014, 0.36).
narrative_ontology:measurement(fmk_wcr_tr_t2018, federation_membership_kernel__welfare_coordination_reading, theater_ratio, 2018, 0.37).
narrative_ontology:measurement(fmk_wcr_tr_t2024, federation_membership_kernel__welfare_coordination_reading, theater_ratio, 2024, 0.38).

% Extraction over time
narrative_ontology:measurement(fmk_wcr_be_t1996, federation_membership_kernel__welfare_coordination_reading, base_extractiveness, 1996, 0.42).
narrative_ontology:measurement(fmk_wcr_be_t2004, federation_membership_kernel__welfare_coordination_reading, base_extractiveness, 2004, 0.51).
narrative_ontology:measurement(fmk_wcr_be_t2010, federation_membership_kernel__welfare_coordination_reading, base_extractiveness, 2010, 0.58).
narrative_ontology:measurement(fmk_wcr_be_t2014, federation_membership_kernel__welfare_coordination_reading, base_extractiveness, 2014, 0.62).
narrative_ontology:measurement(fmk_wcr_be_t2018, federation_membership_kernel__welfare_coordination_reading, base_extractiveness, 2018, 0.64).
narrative_ontology:measurement(fmk_wcr_be_t2024, federation_membership_kernel__welfare_coordination_reading, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(fmk_wcr_su_t1996, federation_membership_kernel__welfare_coordination_reading, suppression_requirement, 1996, 0.35).
narrative_ontology:measurement(fmk_wcr_su_t2004, federation_membership_kernel__welfare_coordination_reading, suppression_requirement, 2004, 0.42).
narrative_ontology:measurement(fmk_wcr_su_t2010, federation_membership_kernel__welfare_coordination_reading, suppression_requirement, 2010, 0.48).
narrative_ontology:measurement(fmk_wcr_su_t2014, federation_membership_kernel__welfare_coordination_reading, suppression_requirement, 2014, 0.52).
narrative_ontology:measurement(fmk_wcr_su_t2018, federation_membership_kernel__welfare_coordination_reading, suppression_requirement, 2018, 0.54).
narrative_ontology:measurement(fmk_wcr_su_t2024, federation_membership_kernel__welfare_coordination_reading, suppression_requirement, 2024, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(federation_membership_kernel__welfare_coordination_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(federation_membership_kernel__welfare_coordination_reading, 0.15).
narrative_ontology:affects_constraint(federation_membership_kernel__welfare_coordination_reading, federation_membership_kernel__integration_reading).
narrative_ontology:affects_constraint(federation_membership_kernel__welfare_coordination_reading, federation_membership_kernel__member_sovereignty_reading).

% DUAL FORMULATION NOTE:
% This constraint story is one member of the federation_membership_kernel constraint family. The kernel (free movement as stabilized commitment) admits three structurally distinct readings, each with its own ε, beneficiary/victim structure, and claimed_type. This reading (welfare_coordination_reading) claims tangled_rope with coordination + extraction. integration_reading claims rope (coordination dominant, extraction minimal). member_sovereignty_reading claims scaffold or mountain (national welfare autonomy as boundary condition). The three stories are linked via affects_constraints; each has its own cs_structure with reading_relations and axioms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(federation_membership_kernel__welfare_coordination_reading, institutional, 0.35).
constraint_indexing:directionality_override(federation_membership_kernel__welfare_coordination_reading, organized, 0.7).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
