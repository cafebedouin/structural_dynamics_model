% ============================================================================
% CONSTRAINT STORY: ihl_distinction_proportionality__human_agency_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ihl_distinction_proportionality__human_agency_reading, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: ihl_distinction_proportionality__human_agency_reading
 *   human_readable: Human Agency Reading of IHL Targeting Obligations (Meaningful Human Control Requirement)
 *   domain: international_humanitarian_law/military_ethics/technology_governance
 *
 * SUMMARY:
 *   Under the human-agency reading, IHL's distinction and proportionality
 *   rules are read to embed an irreducible requirement: a human must exercise
 *   moral judgment at the moment lethal force is applied. This is presented
 *   as coordination — a workable, auditable standard for assigning
 *   responsibility and preserving accountability chains in an area (lethal
 *   force against uncertain targets) where clear standards genuinely matter.
 *   But the same standard forecloses fully autonomous targeting regardless of
 *   its measured accuracy, locks in the operational advantage of states with
 *   large trained personnel bases, and entrenches the interpretive authority
 *   of the institutions (principally the ICRC and allied advocacy networks)
 *   that certify what counts as adequate human control. The claim is authored
 *   as tangled_rope: there is a real coordination function (attributable
 *   responsibility for lethal decisions) bundled with asymmetric extraction
 *   (foreclosing a technically viable alternative that would benefit
 *   personnel-constrained states and autonomous-systems developers) sustained
 *   by active diplomatic and legal enforcement (UN CCW proceedings, national
 *   weapons-review processes, export-control regimes).
 *
 * KEY AGENTS:
 *   - icrc_and_ihl_interpretive_authorities: agenda-setter/beneficiary (institutional/analytical) — certifies compliance, gains interpretive centrality
 *   - militaries_seeking_autonomous_targeting_capability: primary payer (powerful/constrained) — bears capability constraint and operational latency cost
 *   - states_with_personnel_shortages_facing_peer_adversaries: secondary payer (moderate/trapped) — bears asymmetric cost relative to manpower-rich rivals
 *   - civilians_in_conflict_zones: excluded (powerless/trapped) — bears the downstream consequences of whichever regime is fielded, absent from the interpretive contest
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ihl_distinction_proportionality__human_agency_reading, 0.68).
domain_priors:suppression_score(ihl_distinction_proportionality__human_agency_reading, 0.72).
domain_priors:theater_ratio(ihl_distinction_proportionality__human_agency_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ihl_distinction_proportionality__human_agency_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(ihl_distinction_proportionality__human_agency_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(ihl_distinction_proportionality__human_agency_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ihl_distinction_proportionality__human_agency_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(ihl_distinction_proportionality__human_agency_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ihl_distinction_proportionality__human_agency_reading, tangled_rope).
narrative_ontology:human_readable(ihl_distinction_proportionality__human_agency_reading, "Human Agency Reading of IHL Targeting Obligations (Meaningful Human Control Requirement)").
narrative_ontology:topic_domain(ihl_distinction_proportionality__human_agency_reading, "international_humanitarian_law/military_ethics/technology_governance").

domain_priors:requires_active_enforcement(ihl_distinction_proportionality__human_agency_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ihl_distinction_proportionality__human_agency_reading, '36ab9f94-f367-4d99-b023-2c1db7742135').
narrative_ontology:cs_kernel_codification('36ab9f94-f367-4d99-b023-2c1db7742135', distributed).
narrative_ontology:cs_authority_grounding('36ab9f94-f367-4d99-b023-2c1db7742135', lineage).
narrative_ontology:cs_interpretation_layer_present('36ab9f94-f367-4d99-b023-2c1db7742135').
narrative_ontology:cs_reading_relation('36ab9f94-f367-4d99-b023-2c1db7742135', ihl_distinction_proportionality__categorical_prohibition_reading, coexists_with).
narrative_ontology:cs_reading_relation('36ab9f94-f367-4d99-b023-2c1db7742135', ihl_distinction_proportionality__outcomes_based_reading, forecloses).
narrative_ontology:cs_axiom('36ab9f94-f367-4d99-b023-2c1db7742135', foundational, human_moral_judgment_irreducible_at_point_of_lethal_decision).
narrative_ontology:cs_axiom_status(human_moral_judgment_irreducible_at_point_of_lethal_decision, holdable).
narrative_ontology:cs_axiom_grounding('36ab9f94-f367-4d99-b023-2c1db7742135', human_moral_judgment_irreducible_at_point_of_lethal_decision, deontological).
narrative_ontology:cs_axiom('36ab9f94-f367-4d99-b023-2c1db7742135', secondary, performance_parity_insufficient_to_satisfy_distinction_obligation).
narrative_ontology:cs_axiom_status(performance_parity_insufficient_to_satisfy_distinction_obligation, holdable).
narrative_ontology:cs_axiom_grounding('36ab9f94-f367-4d99-b023-2c1db7742135', performance_parity_insufficient_to_satisfy_distinction_obligation, deontological).
narrative_ontology:cs_reference_frame('36ab9f94-f367-4d99-b023-2c1db7742135', martens_clause_human_dignity_floor).
narrative_ontology:cs_drift_state('36ab9f94-f367-4d99-b023-2c1db7742135', post_laws_emergence_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('36ab9f94-f367-4d99-b023-2c1db7742135', '').
narrative_ontology:cs_kernel_id(ihl_distinction_proportionality__human_agency_reading, ihl_distinction_proportionality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ihl_distinction_proportionality__human_agency_reading, icrc_and_ihl_interpretive_authorities).
narrative_ontology:constraint_beneficiary(ihl_distinction_proportionality__human_agency_reading, human_rights_advocacy_coalitions).
narrative_ontology:constraint_beneficiary(ihl_distinction_proportionality__human_agency_reading, states_with_manpower_advantage).
narrative_ontology:constraint_victim(ihl_distinction_proportionality__human_agency_reading, militaries_seeking_autonomous_targeting_capability).
narrative_ontology:constraint_victim(ihl_distinction_proportionality__human_agency_reading, autonomous_weapons_developers).
narrative_ontology:constraint_victim(ihl_distinction_proportionality__human_agency_reading, states_with_personnel_shortages_facing_peer_adversaries).
narrative_ontology:constraint_vindicates(ihl_distinction_proportionality__human_agency_reading, irreducibility_of_moral_judgment_in_lethal_force).
narrative_ontology:constraint_vindicates(ihl_distinction_proportionality__human_agency_reading, martens_clause_dignity_floor).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Publishes commentary, convenes expert meetings, and advises states and courts on what distinction and proportionality require. Under this reading, its interpretive centrality is preserved because compliance still requires a human moral judgment that only doctrine and training — areas where it holds authority — can certify. Bears no operational cost itself; its position is strengthened by the requirement it champions.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__human_agency_reading, icrc_and_ihl_interpretive_authorities, agenda_setter,
    institutional, civilizational, analytical, global).
narrative_ontology:stakeholder_secondary_role(ihl_distinction_proportionality__human_agency_reading, icrc_and_ihl_interpretive_authorities, beneficiary).

% Campaigns for restrictions on lethal autonomy, citing accountability gaps and dignity concerns. This reading gives them a workable legal hook short of outright treaty prohibition — 'meaningful human control' — that can be litigated and lobbied for without requiring the harder categorical ban. They incur reputational and organizing costs but no operational costs from the rule itself.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__human_agency_reading, human_rights_advocacy_coalitions, beneficiary,
    organized, generational, mobile, global).

% States with large trained officer corps and established command structures can satisfy a human-in-the-loop requirement at comparatively low marginal cost — they already have humans positioned to make targeting decisions. The rule locks in a status quo that favors their existing force structure over adversaries seeking to substitute automation for scarce personnel.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__human_agency_reading, states_with_manpower_advantage, beneficiary,
    powerful, generational, constrained, global).

% Develops or seeks to field systems capable of independent target selection and engagement to compress the kill chain against fast-moving threats (drone swarms, hypersonic tracking, contested-EM environments where human-in-the-loop links are jammed). Under this reading, any system without a human making the final lethal decision is categorically noncompliant regardless of its measured accuracy. They can still deploy human-supervised systems, but must accept latency and bandwidth dependence that adversaries without the same legal culture may not accept.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__human_agency_reading, militaries_seeking_autonomous_targeting_capability, payer,
    powerful, biographical, constrained, global).

% Builds targeting and engagement systems for defense contractors and states. The human-agency requirement forecloses the most lucrative fully-autonomous product lines regardless of demonstrated performance, forcing continued investment in human-interface and communication-link architecture even where it degrades system responsiveness. Their exit option is to sell into markets or states that reject this reading, which exists but carries reputational and export-control risk.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__human_agency_reading, autonomous_weapons_developers, payer,
    moderate, biographical, constrained, global).

% Faces peer or near-peer adversaries while lacking the trained personnel base to staff continuous human-supervised targeting across contested domains at scale. This reading forces a choice between accepting slower, human-bottlenecked engagement (operational risk) or fielding systems in a posture other states will characterize as unlawful (legal and diplomatic risk). Their exit is effectively trapped: neither strict compliance nor open defection is cost-free, and they lack the interpretive standing to contest the reading itself.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__human_agency_reading, states_with_personnel_shortages_facing_peer_adversaries, payer,
    moderate, biographical, trapped, national).

% Bears the consequences of whichever targeting regime is actually fielded above them, but has no voice in whether the applicable standard is human-agency, outcomes-based, or categorical prohibition. Would plausibly care most about which regime actually reduces wrongful deaths, but that empirical question is not what any of the three readings is litigated on.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__human_agency_reading, civilians_in_conflict_zones, excluded,
    powerless, immediate, trapped, local).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ihl_distinction_proportionality__human_agency_reading, icrc_and_ihl_interpretive_authorities).
narrative_ontology:fixing_cost_class(ihl_distinction_proportionality__human_agency_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates state and industry behavior around a single, auditable proxy for lawful targeting — a human decision-maker whose presence and reasoning can be reconstructed after the fact — which solves the genuine problem of assigning individual criminal and command responsibility for lethal force under uncertainty.
% TRANSFER_FUNCTION: Moves interpretive and normative authority toward IHL institutions and advocacy coalitions that certify what counts as adequate human judgment, and moves operational cost and capability constraint onto militaries and developers who would otherwise field faster or cheaper autonomous systems.
% ABSENT_VOICES: Civilians in conflict zones, whose deaths or survival are the actual subject matter of distinction and proportionality, are not party to the interpretive contest between human-agency, outcomes-based, and categorical readings. Systems engineers and human-factors researchers who could speak to whether human-in-the-loop actually improves battlefield outcomes (versus merely being visible and attributable) are also largely outside the interpretive process, which is dominated by legal and diplomatic actors.
% DISAPPEARANCE_RATIONALE: If the human-agency requirement were dropped in favor of a pure outcomes standard, states and developers would rapidly reallocate investment toward fully autonomous targeting systems wherever they could demonstrate statistical parity or superiority to human operators, accountability practice would shift from 'who decided' to 'what was the system's certified performance,' and the IHL interpretive bodies that currently anchor compliance on human judgment would lose a central lever of relevance.
% FOUNDING_PROBLEM: Weapons technology was outpacing the codified law of war (as it did before Hague and Geneva revisions), and the Martens Clause was invoked historically to prevent a gap where new means of warfare escaped judgment merely because no specific treaty text yet named them — later reinterpreted for autonomous weapons to insist a human moral agent remain answerable for every lethal decision.
% FOUNDING_PROBLEM_CORROBORATION: ICRC legal advisers and Martens Clause scholars attest the problem is live: reviewing panels and UN CCW GGE delegations from states developing autonomous targeting systems attest, by contrast, that the underlying problem (preventing indiscriminate or disproportionate killing) is separable from the means used to prevent it, and that a demonstrated-performance standard could satisfy the same underlying goal without a human bottleneck — an assessment corroborated by independent human-factors researchers noting human operators are not reliably better at rapid-timescale distinction judgments than well-validated automated systems, a finding that comes from outside both the IHL interpretive community and the defense-industrial beneficiary set.
narrative_ontology:disappearance_verdict(ihl_distinction_proportionality__human_agency_reading, world_rearranges).
narrative_ontology:founding_problem_status(ihl_distinction_proportionality__human_agency_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ihl_distinction_proportionality__human_agency_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(ihl_distinction_proportionality__human_agency_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ihl_distinction_proportionality__human_agency_reading, 0.68, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ihl_distinction_proportionality__human_agency_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ihl_distinction_proportionality__human_agency_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ihl_distinction_proportionality__human_agency_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored moderately-high (0.68 at 2026) because the requirement does more than solve a coordination problem: it forecloses a technically live alternative (validated autonomous targeting) in a way that redistributes operational advantage toward states already structured around human decision chains. Suppression is authored high (0.72) because the requirement's persistence depends on active diplomatic enforcement — CCW proceedings, weapons-review regimes, export controls — rather than voluntary convergence; states that would prefer the outcomes-based standard face real legal and reputational cost for departing from this reading. Theater ratio is kept moderate-low (0.28): the accountability function is largely real, but a growing share of compliance activity (review boards, doctrine language) has become about demonstrating adherence to the human-agency frame rather than measurably reducing civilian harm — hence the rising trajectory. Accessibility collapse is moderate (0.6): the outcomes-based and categorical readings remain live alternatives argued by identifiable parties, so alternatives have not fully collapsed, but the human-agency reading has become the dominant operative standard in most national weapons-review practice. Resistance is high (0.71), driven chiefly by militaries and developers who argue the standard is not calibrated to actual harm-reduction.
 *
 * DIRECTIONALITY LOGIC:
 *   ICRC and allied interpretive authorities sit at the beneficiary end: the requirement is unenforceable without their certifying role, so its persistence strengthens their institutional position at no operational cost to them. Manpower-advantaged states and human-rights coalitions likewise benefit — the former because compliance is cheap given their existing force structure, the latter because the standard gives them a workable legal lever short of an outright ban. Militaries seeking autonomous capability, autonomous-weapons developers, and personnel-constrained states sit at the target end: the rule directly forecloses a capability path they would otherwise pursue, and their exit options (developing noncompliant systems, or exporting to states with different legal postures) carry real diplomatic and reputational cost, which is why they are authored as constrained/trapped rather than mobile.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — preventing a legal vacuum in which new means of warfare escape any accountability standard — is contested rather than resolved or dead. Attribution of individual/command responsibility for lethal outcomes remains a genuine unmet need (supporting a live-problem reading), but independent human-factors evidence suggesting human operators are not reliably superior to well-validated automated systems at rapid-timescale distinction judgments complicates the claim that human-in-the-loop is the mechanism actually solving the underlying problem, versus merely being the mechanism that is currently auditable. Classifying this as tangled_rope rather than snare preserves the genuine coordination function (attributable responsibility matters) while not obscuring the asymmetric cost distribution and active enforcement that keep the standard in place against a technically viable competing reading.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    human_judgment_superiority_ambiguity,
    'Does human presence at the point of lethal decision actually improve distinction/proportionality outcomes relative to validated automated systems, or does it primarily provide an auditable locus of responsibility independent of comparative performance?',
    'Controlled comparison of human-supervised versus validated-autonomous targeting performance in matched operational conditions, adjudicated by parties outside both the IHL interpretive community and the defense-industrial beneficiary set.',
    'If human presence does not reliably improve outcomes, the human-agency reading''s coordination claim collapses to a pure attribution mechanism, strengthening the case that this reading functions primarily to preserve interpretive centrality rather than to reduce civilian harm — pushing the classification toward snare. If human presence does reliably improve outcomes, the coordination function is substantiated and the tangled_rope classification''s beneficiary/victim asymmetry is better understood as a necessary cost of a genuine safety requirement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(human_judgment_superiority_ambiguity, empirical, 'Whether human-in-the-loop is functionally superior or merely auditable.').

omega_variable(
    kernel_reading_selection_ambiguity,
    'Is the choice of the human-agency reading over the outcomes-based or categorical-prohibition readings driven by the actual moral/legal content of distinction and proportionality, or by which reading best preserves the interpretive authority of existing IHL institutions?',
    'Trace historical drafting and advocacy records for state submissions to CCW GGE sessions and ICRC expert meetings to determine whether institutional-authority preservation was an articulated or observable consideration in reading selection, versus purely doctrinal reasoning from Martens Clause text.',
    'If institutional preservation is a demonstrable factor, this strengthens an FSM-adjacent reading of this constraint (a claimed principled requirement that also identifiably benefits the certifying authorities); if reading selection tracks purely doctrinal reasoning, the beneficiary structure is incidental rather than causal.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection_ambiguity, conceptual, 'Whether institutional self-interest shaped reading selection among the three kernel readings.').

omega_variable(
    personnel_constrained_state_defection_risk,
    'Will states facing acute personnel shortages against peer adversaries defect from the human-agency reading in practice, regardless of its formal doctrinal status, once operational pressure exceeds legal-compliance cost?',
    'Monitor procurement and deployment patterns of contested-autonomy systems by personnel-constrained states over the next decade; track whether public doctrine diverges from fielded capability.',
    'Widespread defection would indicate the reading''s accessibility_collapse and suppression values are overstated relative to actual operational practice, suggesting the standard functions more as declaratory law than binding constraint for the states most pressured by it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(personnel_constrained_state_defection_risk, empirical, 'Whether operational pressure will erode compliance among the most cost-burdened states.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ihl_distinction_proportionality__human_agency_reading, 1977, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ihl__tr_t1977, ihl_distinction_proportionality__human_agency_reading, theater_ratio, 1977, 0.1).
narrative_ontology:measurement(ihl__tr_t1995, ihl_distinction_proportionality__human_agency_reading, theater_ratio, 1995, 0.12).
narrative_ontology:measurement(ihl__tr_t2010, ihl_distinction_proportionality__human_agency_reading, theater_ratio, 2010, 0.16).
narrative_ontology:measurement(ihl__tr_t2016, ihl_distinction_proportionality__human_agency_reading, theater_ratio, 2016, 0.2).
narrative_ontology:measurement(ihl__tr_t2021, ihl_distinction_proportionality__human_agency_reading, theater_ratio, 2021, 0.24).
narrative_ontology:measurement(ihl__tr_t2026, ihl_distinction_proportionality__human_agency_reading, theater_ratio, 2026, 0.28).

% Extraction over time
narrative_ontology:measurement(ihl__be_t1977, ihl_distinction_proportionality__human_agency_reading, base_extractiveness, 1977, 0.35).
narrative_ontology:measurement(ihl__be_t1995, ihl_distinction_proportionality__human_agency_reading, base_extractiveness, 1995, 0.42).
narrative_ontology:measurement(ihl__be_t2010, ihl_distinction_proportionality__human_agency_reading, base_extractiveness, 2010, 0.52).
narrative_ontology:measurement(ihl__be_t2016, ihl_distinction_proportionality__human_agency_reading, base_extractiveness, 2016, 0.58).
narrative_ontology:measurement(ihl__be_t2021, ihl_distinction_proportionality__human_agency_reading, base_extractiveness, 2021, 0.63).
narrative_ontology:measurement(ihl__be_t2026, ihl_distinction_proportionality__human_agency_reading, base_extractiveness, 2026, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(ihl__su_t1977, ihl_distinction_proportionality__human_agency_reading, suppression_requirement, 1977, 0.3).
narrative_ontology:measurement(ihl__su_t1995, ihl_distinction_proportionality__human_agency_reading, suppression_requirement, 1995, 0.38).
narrative_ontology:measurement(ihl__su_t2010, ihl_distinction_proportionality__human_agency_reading, suppression_requirement, 2010, 0.5).
narrative_ontology:measurement(ihl__su_t2016, ihl_distinction_proportionality__human_agency_reading, suppression_requirement, 2016, 0.58).
narrative_ontology:measurement(ihl__su_t2021, ihl_distinction_proportionality__human_agency_reading, suppression_requirement, 2021, 0.66).
narrative_ontology:measurement(ihl__su_t2026, ihl_distinction_proportionality__human_agency_reading, suppression_requirement, 2026, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ihl_distinction_proportionality__human_agency_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(ihl_distinction_proportionality__human_agency_reading, 0.12).
narrative_ontology:affects_constraint(ihl_distinction_proportionality__human_agency_reading, outcomes_based_reading).
narrative_ontology:affects_constraint(ihl_distinction_proportionality__human_agency_reading, categorical_prohibition_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the ihl_distinction_proportionality kernel. categorical_prohibition_reading forecloses autonomous weapons entirely on dignity grounds regardless of human supervision architecture; outcomes_based_reading permits full autonomy wherever demonstrated performance meets or exceeds human operators. This reading (human_agency_reading) occupies the middle position: it permits human-supervised autonomy but forecloses full delegation of the final lethal decision. Each reading has a distinct ε, distinct beneficiary/victim structure, and must be evaluated as a separate constraint; they are linked here for contamination-propagation and family-comparison purposes only.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
