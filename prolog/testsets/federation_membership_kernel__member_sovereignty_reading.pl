% ============================================================================
% CONSTRAINT STORY: federation_membership_kernel__member_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-13
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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: federation_membership_kernel__member_sovereignty_reading
 *   human_readable: Federation Member Sovereignty Over Migration and Welfare Access
 *   domain: political_economy/federalism/migration_policy
 *
 * SUMMARY:
 *   This constraint embodies the member sovereignty reading of the
 *   federation's free movement kernel: member states retain authority to
 *   exclude economically inactive migrants and protect social solidarity
 *   institutions, bounded only by non-discrimination on nationality grounds.
 *   The reading asserts that free movement rights are not absolute — they are
 *   conditional on economic self-sufficiency, and member states' welfare
 *   sovereignty trumps supranational labor market completion. The
 *   constraint's persistence depends on active enforcement of welfare
 *   eligibility screening and exclusion of migrants deemed unable to
 *   contribute. The conflict between this reading and the integration reading
 *   (which treats free movement as a fundamental citizenship right) is not
 *   resolvable within a single constitutional framework — they are genuinely
 *   forecloses pairs.
 *
 * KEY AGENTS:
 *   - member_state_governments: enforce the exclusion authority and set 'economically inactive' definitions; power is institutional and mobile (can renegotiate with EU)
 *   - incumbent_citizens: the primary beneficiaries, protected from welfare competition and employment pressure; power is organized and localized
 *   - economically_inactive_migrants: the primary victims, excluded from free movement rights or severely constrained; power is powerless and trapped
 *   - sending_state_workers: secondary victims, facing constrained access and downward wage pressure in pre-arranged employment models
 *   - national_welfare_systems: institutional beneficiaries, protected from fiscal stress by contribution-based gatekeeping
 *   - supranational_enforcement_authority (ECJ): observer seat, interpreting and enforcing the boundary between member sovereignty and supranational citizenship
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federation_membership_kernel__member_sovereignty_reading, 0.68).
domain_priors:suppression_score(federation_membership_kernel__member_sovereignty_reading, 0.71).
domain_priors:theater_ratio(federation_membership_kernel__member_sovereignty_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federation_membership_kernel__member_sovereignty_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(federation_membership_kernel__member_sovereignty_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(federation_membership_kernel__member_sovereignty_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(federation_membership_kernel__member_sovereignty_reading, accessibility_collapse, 0.64).
narrative_ontology:constraint_metric(federation_membership_kernel__member_sovereignty_reading, resistance, 0.73).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federation_membership_kernel__member_sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(federation_membership_kernel__member_sovereignty_reading, "Federation Member Sovereignty Over Migration and Welfare Access").
narrative_ontology:topic_domain(federation_membership_kernel__member_sovereignty_reading, "political_economy/federalism/migration_policy").

domain_priors:requires_active_enforcement(federation_membership_kernel__member_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federation_membership_kernel__member_sovereignty_reading, '53bbcf25-fc21-49dc-9fa5-7e1300cf0998').
narrative_ontology:cs_kernel_codification('53bbcf25-fc21-49dc-9fa5-7e1300cf0998', formalized).
narrative_ontology:cs_authority_grounding('53bbcf25-fc21-49dc-9fa5-7e1300cf0998', lineage).
narrative_ontology:cs_interpretation_layer_present('53bbcf25-fc21-49dc-9fa5-7e1300cf0998').
narrative_ontology:cs_reading_relation('53bbcf25-fc21-49dc-9fa5-7e1300cf0998', federation_membership_kernel__integration_reading, forecloses).
narrative_ontology:cs_reading_relation('53bbcf25-fc21-49dc-9fa5-7e1300cf0998', federation_membership_kernel__welfare_coordination_reading, influences).
narrative_ontology:cs_axiom('53bbcf25-fc21-49dc-9fa5-7e1300cf0998', foundational, member_state_welfare_authority_binding).
narrative_ontology:cs_axiom_status(member_state_welfare_authority_binding, holdable).
narrative_ontology:cs_axiom_grounding('53bbcf25-fc21-49dc-9fa5-7e1300cf0998', member_state_welfare_authority_binding, conventional).
narrative_ontology:cs_axiom('53bbcf25-fc21-49dc-9fa5-7e1300cf0998', foundational, free_movement_conditional_on_economic_self_sufficiency).
narrative_ontology:cs_axiom_status(free_movement_conditional_on_economic_self_sufficiency, holdable).
narrative_ontology:cs_axiom_grounding('53bbcf25-fc21-49dc-9fa5-7e1300cf0998', free_movement_conditional_on_economic_self_sufficiency, empirically_contingent).
narrative_ontology:cs_reference_frame('53bbcf25-fc21-49dc-9fa5-7e1300cf0998', member_state_welfare_sovereignty_framework).
narrative_ontology:cs_drift_state('53bbcf25-fc21-49dc-9fa5-7e1300cf0998', contemporary_ecj_jurisprudence_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('53bbcf25-fc21-49dc-9fa5-7e1300cf0998', '').
narrative_ontology:cs_kernel_id(federation_membership_kernel__member_sovereignty_reading, federation_membership_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federation_membership_kernel__member_sovereignty_reading, incumbent_citizens).
narrative_ontology:constraint_beneficiary(federation_membership_kernel__member_sovereignty_reading, national_welfare_systems).
narrative_ontology:constraint_victim(federation_membership_kernel__member_sovereignty_reading, economically_inactive_migrants).
narrative_ontology:constraint_victim(federation_membership_kernel__member_sovereignty_reading, sending_state_workers).
narrative_ontology:constraint_victim(federation_membership_kernel__member_sovereignty_reading, single_parent_families_from_peripheral_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Retain priority access to social housing, unemployment benefits, child allowances, and healthcare. The constraint protects the implicit contract that welfare contributions funnel benefits to nationals and their dependents. Face electoral pressure to protect these systems from what they perceive as resource-diluting inflows. Exit is unavailable — national identity and citizenship are bound to the inherited welfare covenant.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__member_sovereignty_reading, incumbent_citizens, beneficiary,
    organized, generational, trapped, national).

% Institutionalized systems of social insurance and redistribution designed for closed, demographically stable populations. The constraint protects their fiscal sustainability by excluding non-contributors and those presumed to draw more than they contribute. Actuarial models assume a stable contributor-beneficiary ratio; open access disrupts the ratio.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__member_sovereignty_reading, national_welfare_systems, beneficiary,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_non_agent(federation_membership_kernel__member_sovereignty_reading, national_welfare_systems).

% Retain formal authority to conduct nationality and welfare eligibility screening, to set residency thresholds for benefit access, and to exclude migrants deemed unable to contribute. Exercise this authority through administrative gatekeeping and legislative definition of 'economically inactive.' Face EU pressure to reduce friction but retain veto over substantive exclusion standards.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__member_sovereignty_reading, member_state_governments, agenda_setter,
    institutional, generational, mobile, national).

% Excluded from free movement rights or severely restricted in duration of stay unless able to demonstrate economic self-sufficiency (employment or substantial savings). Carry immediate costs: inability to relocate for family or safety reasons, denial of emergency welfare, legal precarity. Have no channel to contest exclusion standards and no political voice in the member state whose welfare system they are excluded from.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__member_sovereignty_reading, economically_inactive_migrants, payer,
    powerless, immediate, trapped, local).

% Face restricted access to higher-income labor markets in wealthy member states once those states' welfare gatekeeping tightens. When destination states adopt stricter 'economically active' definitions, workers must secure formal employment contracts in advance rather than seeking work on arrival — reducing their bargaining power and driving acceptance of below-market wages. Brain drain accelerates as they compete for the shrinking pool of pre-arranged positions.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__member_sovereignty_reading, sending_state_workers, payer,
    moderate, biographical, constrained, regional).

% Gain protection from temporary or involuntary unemployment by limiting access during downturns. Incumbent workers in protected sectors face reduced wage competition. However, the constraint also reduces labor market flexibility — employers in expanding sectors cannot rapidly recruit from abroad, and demographic decline in aging member states is not offset by immigration.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__member_sovereignty_reading, receiving_state_labor_markets, beneficiary,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_non_agent(federation_membership_kernel__member_sovereignty_reading, receiving_state_labor_markets).

% The European Court of Justice observes and interprets EU free movement law. Under this reading, interprets member state exclusion rights narrowly and guards against discrimination, but recognizes member state authority to define 'economically inactive' and to protect welfare. Negotiates constantly between supranational citizenship ideals and national welfare sovereignty. Sees itself as enforcer of a boundary, not a maximizer of labor mobility.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__member_sovereignty_reading, supranational_enforcement_authority, observer,
    institutional, generational, analytical, continental).

% Would argue that labor mobility is essential to convergence and that skilled workers are a public investment, not private goods to be exported at their own cost. Are structurally excluded from the negotiation — they cannot set receiving-state welfare access rules and must watch their youngest and most educated citizens face restricted opportunities. Their population and tax base shrink as a result.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__member_sovereignty_reading, sending_state_governments, excluded,
    moderate, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(federation_membership_kernel__member_sovereignty_reading, incumbent_citizens).
narrative_ontology:fixing_cost_class(federation_membership_kernel__member_sovereignty_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Protects national welfare systems' fiscal sustainability and the redistribution mechanisms that underpin social cohesion by enabling member states to screen access based on expected contribution-to-benefit ratios. Solves the collective problem of how to maintain welfare institutions across a federation with heterogeneous labor markets, aging demographics, and different welfare regime types.
% TRANSFER_FUNCTION: Redistributes from economically inactive migrants and from sending-state workers (who face constrained access and downward wage pressure) to incumbent citizens (who retain priority welfare access) and to receiving-state welfare systems (whose actuarial stability is protected). The transfer is mediated through gatekeeping authority held by receiving-state governments.
% ABSENT_VOICES: Sending-state governments, who would argue that labor mobility serves convergence; workers in expanding sectors of receiving states, who would want unrestricted access to labor from abroad; future receiving-state citizens facing demographic decline and labor shortages who would benefit from unrestricted inflows; and economically inactive people in sending states (the elderly, disabled, caregivers) who are themselves presumed to lack 'economic activity' and are affected by the constraint's framing.
% DISAPPEARANCE_RATIONALE: If the member sovereignty reading and its welfare-protection authorities vanished overnight, labor mobility would expand substantially — workers could move freely regardless of welfare status. Welfare systems would face immediate fiscal pressure and would restructure (either converging upward, converging downward, or adopting means-testing). Political coalitions defending welfare would destabilize in wealthy member states. Sending states would gain working-age population retention and expanded remittance channels. The federation would reorganize around a different equilibrium of mobility, welfare design, and labor market clearing.
% FOUNDING_PROBLEM: Early federation expansion to lower-wage member states created fears that welfare migrants would deplete high-welfare systems; wealthy members needed reassurance that free movement would not force welfare harmonization upward or unsustainable costs. Simultaneously, sending states needed reassurance that their workers would not be displaced by welfare criteria rather than economics. The constraint was built to permit exclusion of 'economically inactive' people — those presumed to draw welfare without contributing to the tax base.
% FOUNDING_PROBLEM_CORROBORATION: Member state governments attest the problem remains live: welfare systems are under fiscal pressure and require gatekeeping. The ECJ acknowledges the welfare sovereignty concern in case law (Grzelczyk, Zambrano framing) but interprets it narrowly, suggesting the problem's urgency has diminished. Independent economists and demographers attest that the founding problem's empirical basis (large-scale welfare migration) has not materialized at significant scale, and that aging demographics make labor inflow beneficial even at welfare cost. Sending-state governments and migrant advocacy organizations attest the problem is invoked to justify protectionism unrelated to actual welfare risk.
narrative_ontology:disappearance_verdict(federation_membership_kernel__member_sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(federation_membership_kernel__member_sovereignty_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(federation_membership_kernel__member_sovereignty_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(federation_membership_kernel__member_sovereignty_reading, 'none', 1).

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
 *   Extractiveness is high (0.68 endpoint) and rising over the interval because the constraint concentrates gains (welfare access, employment priority) on a fixed group (incumbents) while imposing costs on expanding groups (migrants, excluded sending-state workers). The measurement trajectory shows extractiveness stabilizing around 0.68 after 20 years, suggesting the constraint reached equilibrium — initial tightening gave way to negotiated stability. Suppression is similarly high (0.71) and rising because the constraint's persistence depends on active exclusion enforcement and on legal gatekeeping that blocks alternative channels (welfare migration, family reunification, long-term informal residence). Theater rises moderately (0.28 to 0.42) over the interval, indicating that the discourse around welfare protection increasingly masks the exclusion's employment and demographic effects. The constraint requires active member state administrative machinery to screen and exclude — it is not a natural outcome of labor market forces.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter (member state government) and beneficiary seats experience this constraint as legitimate welfare protection, needed to preserve social solidarity and prevent adverse selection. The payer seats (economically inactive migrants, sending-state workers) experience it as arbitrary exclusion, because the 'economically inactive' label is applied prospectively to exclude rather than based on actual behavior. The supranational observer seat sees the boundary: member sovereignty is permitted, but only within non-discrimination guardrails. Directionality diverges sharply: the member state government has d ≈ 0.2–0.3 (benefits from rule-setting authority, mobile exit via renegotiation with EU), while economically inactive migrants have d ≈ 0.85–0.95 (trapped, excluded without voice). This divergence is the engine's measure of per-seat experience and should emerge directly from the structural data, not be tuned by the author.
 *
 * DIRECTIONALITY LOGIC:
 *   Member state governments benefit from the constraint (set the rules, collect no rents but preserve state capacity) with relatively mobile exit (can negotiate terms with EU). Incumbent citizens benefit from priority access and face trapped exit (national identity), yielding moderate-to-high directionality as beneficiaries. Economically inactive migrants face absolute exclusion from free movement rights or severe constraint, with trapped exit (no alternative channels), yielding directionality near 1.0 (full targets). Sending-state workers face constrained access and competitive disadvantage, with constrained exit (must pre-arrange employment or stay home), yielding directionality around 0.7–0.8. The national welfare systems and receiving-state labor markets do not have directionality themselves (they are non-agent institutional beneficiaries), but the constraint's directionality for agents derives from their relationship to these systems.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint is not mandatrophy — the founding problem (welfare system sustainability) remains live, and the constraint's function persists. However, the contested status of the founding problem creates ambiguity: if the welfare system's fiscal crisis was never as large as claimed (the omega questions this), the constraint becomes a mechanism for rent-protection rather than crisis management. The rising theater ratio (0.28 to 0.42) suggests increasing proportion of enforcement activity devoted to performative welfare protection rather than genuine fiscal sustainability. This is not mandatrophy (the function has not been fully displaced) but is consistent with partial function drift — the constraint increasingly serves political solidarity narratives rather than actuarial necessity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    founding_problem_empirical_magnitude,
    'Is the founding problem (welfare migrants depleting fiscal capacity) empirically real at the scale that justifies exclusion authority, or has it been invoked primarily for political protection?',
    'Comparative fiscal analysis of welfare transfers to mobile EU citizens vs. native citizens; analysis of welfare system stress by period and member state; cross-national econometric studies of mobility''s fiscal impact.',
    'If welfare migration is negligible, the constraint''s justification shifts from fiscal necessity to political preference — reclassifying it from legitimate welfare protection to Tangled Rope with primarily extractive function. If welfare migration is substantial, the constraint remains a genuine coordination response to an asymmetric problem.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_empirical_magnitude, empirical, 'Whether the constraint solves a real welfare-system crisis or protects rents under welfare justification.').

omega_variable(
    economically_active_definition_drift,
    'Is the definition of ''economically active'' stable and objective (e.g., formal employment), or does it drift to serve exclusionary purposes (e.g., income thresholds, contract types)?',
    'Historical analysis of member state welfare eligibility standards; comparative law review of formal vs. functional definitions; case law analysis showing whether ECJ permits drift or constrains it.',
    'If the definition is politically malleable, the constraint operates as a tool for incumbent protection rather than welfare sustainability — pure extraction dressed in technical language. If the definition is stable, the constraint functions as claimed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(economically_active_definition_drift, empirical, 'Whether ''economically inactive'' is a stable welfare-protection criterion or a political gating tool.').

omega_variable(
    sending_state_brain_drain_causality,
    'Is the observed brain drain (emigration of skilled workers) caused by constrained mobility under this reading, or by wage differentials and opportunity structures that would drive mobility regardless of welfare gatekeeping?',
    'Time-series analysis of emigration patterns relative to welfare exclusion tightening; natural experiments from ECJ rulings that expanded access; comparative analysis with non-EU federal systems and their mobility patterns.',
    'If constrained mobility is a primary driver, the constraint''s hidden cost is demographic decline in sending states — a victim group (sending state populations) not visible in the immediate welfare debate. If wage differentials alone drive brain drain, the welfare constraint is not the causal mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sending_state_brain_drain_causality, empirical, 'Whether welfare gatekeeping drives brain drain or merely accompanies it.').

omega_variable(
    alternative_welfare_coordination_feasibility,
    'Could the coordination problem (fiscal sustainability of welfare across heterogeneous labor markets) be solved through welfare system coordination or harmonization rather than through member state exclusion authority?',
    'Comparative institutional analysis of federal systems with harmonized welfare (Canada, Australia); simulation of alternative federation designs; ECJ jurisprudence on harmonization authority.',
    'If alternative coordination is feasible, the member sovereignty reading is one choice among several — reclassifying it as a reading rather than a necessity. If alternative coordination is infeasible (political economy barriers are genuine), the reading''s function is confirmed.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_welfare_coordination_feasibility, conceptual, 'Whether the constraint''s coordination function requires member state authority or could be achieved differently.').

omega_variable(
    suppression_mechanism_internalization,
    'Is the measured suppression (0.71) primarily structural (legal barriers, welfare gatekeeping machinery) or primarily internalized (economically inactive migrants accept exclusion as legitimate, sending-state workers accept competition restriction as necessary)?',
    'Post-exit trajectory analysis: if migrants denied access later attest suppression persisted after denial (internalized shame, identity fusion with welfare system they were excluded from), suppression is partially internalized. Survey evidence on legitimacy beliefs among migrants and workers in peripheral states.',
    'If suppression is structural, opening the legal barriers might rapidly increase mobility and redistribution. If suppression is internalized, opening barriers leaves migration constrained because targets have absorbed the constraint. The distinction determines the real cost of removing the constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether suppression of economically inactive migration is structural or internalized in migrant identity.').

omega_variable(
    member_sovereignty_reading_vs_integration_reading_committer,
    'Are the member sovereignty reading and the integration reading genuinely foreclosed pairs (logically incompatible in a single federation), or can they coexist as different institutional layers (member state authority over welfare, supranational authority over discrimination)?',
    'Constitutional theory analysis of federation design; ECJ jurisprudence trajectory (whether the Court has moved toward integration_reading or toward member_sovereignty_reading); analysis of member states'' actual renegotiation of Article 21 TFEU scope.',
    'If they are foreclosed pairs, the federation faces a constitutional choice: one reading must dominate. If they coexist, the constraint is nested within a larger system of competing authorities. This determines whether the contradiction is resolvable or endemic.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(member_sovereignty_reading_vs_integration_reading_committer, conceptual, 'Whether the member sovereignty and integration readings of the free movement kernel are logically exclusive or can be nested.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership_kernel__member_sovereignty_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fede_tr_t0, federation_membership_kernel__member_sovereignty_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement_basis(fede_tr_t0, observed).
narrative_ontology:measurement(fede_tr_t5, federation_membership_kernel__member_sovereignty_reading, theater_ratio, 5, 0.32).
narrative_ontology:measurement_basis(fede_tr_t5, observed).
narrative_ontology:measurement(fede_tr_t10, federation_membership_kernel__member_sovereignty_reading, theater_ratio, 10, 0.37).
narrative_ontology:measurement_basis(fede_tr_t10, observed).
narrative_ontology:measurement(fede_tr_t15, federation_membership_kernel__member_sovereignty_reading, theater_ratio, 15, 0.4).
narrative_ontology:measurement_basis(fede_tr_t15, observed).
narrative_ontology:measurement(fede_tr_t20, federation_membership_kernel__member_sovereignty_reading, theater_ratio, 20, 0.41).
narrative_ontology:measurement_basis(fede_tr_t20, observed).
narrative_ontology:measurement(fede_tr_t25, federation_membership_kernel__member_sovereignty_reading, theater_ratio, 25, 0.42).
narrative_ontology:measurement_basis(fede_tr_t25, observed).

% Extraction over time
narrative_ontology:measurement(fede_be_t0, federation_membership_kernel__member_sovereignty_reading, base_extractiveness, 0, 0.58).
narrative_ontology:measurement_basis(fede_be_t0, observed).
narrative_ontology:measurement(fede_be_t5, federation_membership_kernel__member_sovereignty_reading, base_extractiveness, 5, 0.62).
narrative_ontology:measurement_basis(fede_be_t5, observed).
narrative_ontology:measurement(fede_be_t10, federation_membership_kernel__member_sovereignty_reading, base_extractiveness, 10, 0.65).
narrative_ontology:measurement_basis(fede_be_t10, observed).
narrative_ontology:measurement(fede_be_t15, federation_membership_kernel__member_sovereignty_reading, base_extractiveness, 15, 0.67).
narrative_ontology:measurement_basis(fede_be_t15, observed).
narrative_ontology:measurement(fede_be_t20, federation_membership_kernel__member_sovereignty_reading, base_extractiveness, 20, 0.68).
narrative_ontology:measurement_basis(fede_be_t20, observed).
narrative_ontology:measurement(fede_be_t25, federation_membership_kernel__member_sovereignty_reading, base_extractiveness, 25, 0.68).
narrative_ontology:measurement_basis(fede_be_t25, observed).

% Suppression requirement over time
narrative_ontology:measurement(fede_su_t0, federation_membership_kernel__member_sovereignty_reading, suppression_requirement, 0, 0.62).
narrative_ontology:measurement_basis(fede_su_t0, observed).
narrative_ontology:measurement(fede_su_t5, federation_membership_kernel__member_sovereignty_reading, suppression_requirement, 5, 0.66).
narrative_ontology:measurement_basis(fede_su_t5, observed).
narrative_ontology:measurement(fede_su_t10, federation_membership_kernel__member_sovereignty_reading, suppression_requirement, 10, 0.69).
narrative_ontology:measurement_basis(fede_su_t10, observed).
narrative_ontology:measurement(fede_su_t15, federation_membership_kernel__member_sovereignty_reading, suppression_requirement, 15, 0.7).
narrative_ontology:measurement_basis(fede_su_t15, observed).
narrative_ontology:measurement(fede_su_t20, federation_membership_kernel__member_sovereignty_reading, suppression_requirement, 20, 0.71).
narrative_ontology:measurement_basis(fede_su_t20, observed).
narrative_ontology:measurement(fede_su_t25, federation_membership_kernel__member_sovereignty_reading, suppression_requirement, 25, 0.71).
narrative_ontology:measurement_basis(fede_su_t25, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(federation_membership_kernel__member_sovereignty_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(federation_membership_kernel__member_sovereignty_reading, 0.18).
narrative_ontology:affects_constraint(federation_membership_kernel__member_sovereignty_reading, federation_membership_kernel__integration_reading).
narrative_ontology:affects_constraint(federation_membership_kernel__member_sovereignty_reading, federation_membership_kernel__welfare_coordination_reading).
narrative_ontology:affects_constraint(federation_membership_kernel__member_sovereignty_reading, welfare_state_eligibility_gatekeeping).
narrative_ontology:affects_constraint(federation_membership_kernel__member_sovereignty_reading, sending_state_demographic_decline).

% DUAL FORMULATION NOTE:
% This constraint is one of three structurally distinct readings of the federation_membership_kernel. The sibling constraints (integration_reading and welfare_coordination_reading) have different ε values, different victim sets, and different beneficiary structures because they model different authority allocations. All three share the same underlying kernel (federation member state authority over free movement) but instantiate it differently. The member_sovereignty_reading lodges authority in the member state; integration_reading lodges it in supranational citizenship; welfare_coordination_reading distributes it among coordinating parties. Network edges link all three as a family, enabling the corpus to measure which reading the federation actually instantiates at different points in time.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(federation_membership_kernel__member_sovereignty_reading, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
