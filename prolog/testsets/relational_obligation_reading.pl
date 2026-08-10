% ============================================================================
% CONSTRAINT STORY: relational_obligation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_relational_obligation_reading, []).

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
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: relational_obligation_reading
 *   human_readable: Relational-Obligation Reading of Personal Freedom (Exit Costs Externalized onto Dependents)
 *   domain: applied_philosophy/libertarian_ethics/self_help_ideology
 *
 * SUMMARY:
 *   Popular libertarian ethics and the self-help industry evaluate personal
 *   freedom at the level of the isolated chooser: an individual's right to
 *   exit a job, relationship, or obligation is assessed by whether the
 *   individual consents and is not physically coerced, with no accounting for
 *   costs the exit imposes on dependents, partners, or communities
 *   structurally bound to the arrangement. This story authors the
 *   relational-obligation reading of the freedom_locus_kernel: it introduces
 *   the victim set that the sovereign-agency and structural-conditions
 *   readings omit, and reclassifies the 'box' the individual exits not as a
 *   unilateral trap but as a site of mutual obligation whose costs the
 *   sovereign framing externalizes rather than resolves. This is one reading
 *   among several (see kernel_context); it is not a synthesis of the sibling
 *   readings and does not attempt to average across them.
 *
 * KEY AGENTS:
 *   - exiting_individuals: primary beneficiary (moderate/mobile) — captures the benefit of exit under the sovereign framing
 *   - dependents_of_exiting_individuals: primary victim (powerless/trapped) — bears exit costs with no voice in the decision
 *   - partners_of_exiting_individuals: secondary victim (powerless/constrained) — bears reorganization costs from joint-arrangement foreclosure
 *   - self_help_industry and individualist_ideology_producers: institutional beneficiaries who monetize and culturally reproduce the sovereign framing
 *   - philosophical_observer: analytical seat naming the accounting structure itself
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(relational_obligation_reading, 0.58).
domain_priors:suppression_score(relational_obligation_reading, 0.42).
domain_priors:theater_ratio(relational_obligation_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(relational_obligation_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(relational_obligation_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(relational_obligation_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(relational_obligation_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(relational_obligation_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(relational_obligation_reading, tangled_rope).
narrative_ontology:human_readable(relational_obligation_reading, "Relational-Obligation Reading of Personal Freedom (Exit Costs Externalized onto Dependents)").
narrative_ontology:topic_domain(relational_obligation_reading, "applied_philosophy/libertarian_ethics/self_help_ideology").

domain_priors:requires_active_enforcement(relational_obligation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(relational_obligation_reading, '4bc1cf9e-d9fe-4c24-b045-937933d7481c').
narrative_ontology:cs_kernel_codification('4bc1cf9e-d9fe-4c24-b045-937933d7481c', distributed).
narrative_ontology:cs_authority_grounding('4bc1cf9e-d9fe-4c24-b045-937933d7481c', distributed).
narrative_ontology:cs_reading_relation('4bc1cf9e-d9fe-4c24-b045-937933d7481c', relational_obligation_reading__sovereign_agency_reading, coexists_with).
narrative_ontology:cs_reading_relation('4bc1cf9e-d9fe-4c24-b045-937933d7481c', relational_obligation_reading__structural_conditions_reading, influences).
narrative_ontology:cs_reading_relation('4bc1cf9e-d9fe-4c24-b045-937933d7481c', relational_obligation_reading__negotiated_agency_reading, influences).
narrative_ontology:cs_axiom('4bc1cf9e-d9fe-4c24-b045-937933d7481c', foundational, exit_costs_to_dependents_are_constitutive_of_freedom_assessment).
narrative_ontology:cs_axiom_status(exit_costs_to_dependents_are_constitutive_of_freedom_assessment, holdable).
narrative_ontology:cs_axiom_grounding('4bc1cf9e-d9fe-4c24-b045-937933d7481c', exit_costs_to_dependents_are_constitutive_of_freedom_assessment, deontological).
narrative_ontology:cs_axiom('4bc1cf9e-d9fe-4c24-b045-937933d7481c', secondary, relational_boxes_are_sites_of_mutual_obligation_not_unilateral_traps).
narrative_ontology:cs_axiom_status(relational_boxes_are_sites_of_mutual_obligation_not_unilateral_traps, holdable).
narrative_ontology:cs_axiom_grounding('4bc1cf9e-d9fe-4c24-b045-937933d7481c', relational_boxes_are_sites_of_mutual_obligation_not_unilateral_traps, conventional).
narrative_ontology:cs_reference_frame('4bc1cf9e-d9fe-4c24-b045-937933d7481c', care_ethics_relational_baseline).
narrative_ontology:cs_drift_state('4bc1cf9e-d9fe-4c24-b045-937933d7481c', contemporary_self_help_platform_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('4bc1cf9e-d9fe-4c24-b045-937933d7481c', '').
narrative_ontology:cs_kernel_id(relational_obligation_reading, freedom_locus_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(relational_obligation_reading, exiting_individuals).
narrative_ontology:constraint_beneficiary(relational_obligation_reading, self_help_industry).
narrative_ontology:constraint_beneficiary(relational_obligation_reading, individualist_ideology_producers).
narrative_ontology:constraint_victim(relational_obligation_reading, dependents_of_exiting_individuals).
narrative_ontology:constraint_victim(relational_obligation_reading, partners_of_exiting_individuals).
narrative_ontology:constraint_victim(relational_obligation_reading, local_communities_absorbing_exit_costs).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Frames a job change, relationship exit, or relocation as a purely self-regarding exercise of autonomy, drawing on a self-help and libertarian vocabulary that treats 'my life, my choice' as sufficient justification. Captures the immediate benefit of the exit (relief, opportunity, freedom from an unwanted obligation) while the costs of the exit land on others who are structurally bound to the relationship or arrangement being exited.
narrative_ontology:constraint_stakeholder(relational_obligation_reading, exiting_individuals, beneficiary,
    moderate, biographical, mobile, local).
narrative_ontology:stakeholder_secondary_role(relational_obligation_reading, exiting_individuals, agenda_setter).

% Children, aging parents, or others whose care or material support depended on the exiting individual's continued presence absorb the disruption directly — lost income, lost caregiving, disrupted schooling, relocation trauma. They have no vote in the exit decision and typically cannot exit the resulting situation themselves; the vocabulary of individual freedom that authorized the exit has no vocabulary for their claim.
narrative_ontology:constraint_stakeholder(relational_obligation_reading, dependents_of_exiting_individuals, payer,
    powerless, biographical, trapped, local).

% A spouse, co-parent, or long-term partner who built joint plans, finances, or caregiving arrangements around the continuation of the relationship bears the reorganization cost when the other party exercises unilateral 'freedom to leave.' Their own exit options were often foreclosed by the joint arrangement (career sacrifice, relocation, shared debt) in ways the leaving party's individual-freedom framing does not count.
narrative_ontology:constraint_stakeholder(relational_obligation_reading, partners_of_exiting_individuals, payer,
    powerless, biographical, constrained, local).

% Extended family networks, care cooperatives, or small communities end up absorbing the diffuse costs of repeated individual exits — picking up caregiving slack, informal financial support, or social reorganization. No single exit is attributed to them as a cost, so the aggregate burden is invisible in any single transaction's accounting.
narrative_ontology:constraint_stakeholder(relational_obligation_reading, local_communities_absorbing_exit_costs, payer,
    organized, generational, constrained, regional).

% Publishers, coaches, and platforms selling books, courses, and content premised on 'boundaries,' 'walking away,' and 'choosing yourself' monetize the individual-freedom framing directly. They have no exposure to the externalized costs their advice generates and substantial commercial incentive to keep the framing uncomplicated by relational obligation.
narrative_ontology:constraint_stakeholder(relational_obligation_reading, self_help_industry, beneficiary,
    organized, generational, arbitrage, national).

% Libertarian philosophical and popular-media institutions that produce and circulate the sovereign-individual account of freedom benefit from its continued cultural dominance — it undergirds broader arguments about market freedom, minimal state obligation, and personal responsibility. They are not directly exposed to any single exit's costs but gain from the framework's persistence.
narrative_ontology:constraint_stakeholder(relational_obligation_reading, individualist_ideology_producers, beneficiary,
    institutional, civilizational, analytical, national).

% Care ethicists, feminist philosophers, and communitarian critics who argue that freedom talk which ignores dependents is not more free, only less accountable, are rarely consulted in the popular self-help discourse that shapes how ordinary people narrate their own exits. Their critique exists mainly in academic venues disconnected from the self-help and libertarian ideology production pipeline.
narrative_ontology:constraint_stakeholder(relational_obligation_reading, relational_ethicists, excluded,
    moderate, generational, analytical, national).

% Analyzes the freedom claim structurally: notes that the individual-level accounting of 'my exit, my right' is only coherent if the costs of the exit are assumed away rather than assigned, and that this assumption is itself doing normative work rather than being a neutral description of what freedom is.
narrative_ontology:constraint_stakeholder(relational_obligation_reading, philosophical_observer, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(relational_obligation_reading, diffuse).
narrative_ontology:fixing_cost_class(relational_obligation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The relational-obligation framing solves a real accounting problem: without it, exit costs imposed on dependents, partners, and communities are invisible in the ledger used to evaluate whether an exit was 'free.' Naming the cost-bearers is what makes it possible to ask whether an exit is genuinely justified all things considered, not merely permitted by an individual-level rights claim.
% TRANSFER_FUNCTION: Moves the accounting burden of an exit's costs from the exiting individual (who captures the benefit of the exit under the sovereign-freedom framing) onto dependents, partners, and communities who did not choose the exit and often cannot leave the resulting situation themselves.
% ABSENT_VOICES: Dependents (especially children) and often partners have no seat in the decision the exit represents; relational ethicists who would formalize their claim are largely absent from the popular discourse (self-help, libertarian commentary) that actually shapes how people narrate and justify their own exits.
% DISAPPEARANCE_RATIONALE: If the relational-obligation reading disappeared, sovereign-agency framing would simply go unchallenged in ordinary moral reasoning about exits — the world of actual costs to dependents would not change, but the vocabulary for naming those costs, contesting exits, or demanding compensation/accommodation would vanish. Whether that constitutes 'the world rearranging' is exactly what the sibling readings dispute: sovereign-agency proponents would say nothing changes because the costs were never properly assignable to the exit in the first place; relational-obligation proponents would say the invisibility of a real cost is itself a rearrangement.
% FOUNDING_PROBLEM: Individual-freedom discourse (libertarian ethics, self-help ideology) evaluates exits and 'direct alternatives' as if the exiting person exists in isolation, producing a systematic blind spot for costs imposed on people who did not consent to and cannot exit the resulting arrangement.
% FOUNDING_PROBLEM_CORROBORATION: Care ethicists and communitarian critics (relational_ethicists, an excluded seat, not a beneficiary of the reading) corroborate that the blind spot is live and worsening as self-help and libertarian-adjacent discourse scales through media platforms; the self-help industry and individualist ideology producers, who benefit from the sovereign framing's dominance, do not corroborate the problem's continued relevance and instead characterize dependency claims as manipulation or lack of boundaries.
narrative_ontology:disappearance_verdict(relational_obligation_reading, contested).
narrative_ontology:founding_problem_status(relational_obligation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(relational_obligation_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-10',
    'unspecified', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'unspecified').
narrative_ontology:story_seed(relational_obligation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(relational_obligation_reading, 0.58, 'claude-sonnet-5', 'harry_browne_freedom_kernel_2026_20260810_020156', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(relational_obligation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(relational_obligation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(relational_obligation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.58) reflects that a substantial share of exit costs are real, structurally imposed, and asymmetrically borne — but not total: many exits are genuinely justified even after dependents' costs are counted, so the constraint is not purely extractive. Suppression (0.42) is moderate: dependents are not physically prevented from objecting, but the sovereign-freedom vocabulary that dominates popular discourse makes their objection illegible as a claim rather than forbidden as an act — this is suppression by vocabulary-exclusion, not by coercive enforcement. Accessibility collapse is comparatively low (0.35) because the relational-obligation framing itself remains available in academic and some clinical discourse even as it is excluded from the dominant popular register; the alternative has not disappeared, it has been marginalized. Resistance (0.55) reflects active pushback from care ethicists, feminist philosophers, and family-systems practitioners against the unmarked individual-freedom framing.
 *
 * PERSPECTIVAL GAP:
 *   From the exiting individual's seat, the arrangement they are leaving looks like a unilateral trap justifying a rights-based exit narrative; from the dependent's or partner's seat, the same exit is an imposition that was never negotiated and cannot be declined. The engine's per-seat computation should register this as seat divergence rather than resolve it: the sovereign-agency reading and this reading are not disagreeing about facts so much as disagreeing about which facts belong in the accounting.
 *
 * DIRECTIONALITY LOGIC:
 *   Exiting individuals derive low d under this reading because they capture the benefit of the exit and set the terms of how it is narrated (freedom, boundaries, self-actualization). Dependents and partners derive high d because the costs of the exit land on them without their consent and, for dependents especially, without any exit option of their own. Local communities sit at moderate-high d: the costs they absorb are diffuse and unattributed to any single exit, which is precisely the invisibility this reading is built to name. The self-help industry and ideology producers are structural beneficiaries once removed — they profit from the framing's persistence without being exposed to any specific exit's costs.
 *
 * MANDATROPHY ANALYSIS:
 *   The relational-obligation reading is not itself a mandate that has outlived its function — the founding problem (costs of exit falling invisibly on dependents) remains live. The risk of mandatrophy runs the other direction: if a relational-obligation framing hardens into a permanent veto for anyone with a claim on another's continued presence, it could become an extraction mechanism of its own (trapping the would-be exiter in obligation regardless of how the underlying relationship has changed). This story does not model that inversion; it is flagged as a live boundary condition via omega.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    obligation_hardening_into_extraction,
    'Could the relational-obligation framing itself harden into a permanent claim that traps the would-be exiter regardless of how the relationship has evolved, becoming an extraction mechanism symmetric to the one it critiques?',
    'Track cases longitudinally: does invoking relational obligation typically resolve into renegotiated, time-bound accommodation, or into indefinite veto power over the other party''s exit?',
    'If obligation claims trend toward indefinite veto rather than negotiated accommodation, this reading itself risks reclassification toward tangled_rope with the dependent/partner seat as an unacknowledged beneficiary rather than only a victim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(obligation_hardening_into_extraction, conceptual, 'Whether relational-obligation claims can themselves become extractive if unbounded in time or scope.').

omega_variable(
    kernel_framing_underdetermination,
    'Is the freedom_locus_kernel genuinely decomposed into four independent readings, or do sovereign_agency_reading and structural_conditions_reading actually collapse into one another under close analysis (both centering the individual, differing only on whether constraints are named), making the real fault line binary (individual-centered vs. relationally-centered) rather than fourfold?',
    'Compare the axiom sets and reading_relations across all four sibling files once authored; if sovereign_agency_reading and structural_conditions_reading share every foundational axiom, they are the same reading under two labels and should be merged.',
    'A merge would change this reading''s coexists_with/influences edges and could reveal that relational_obligation_reading and negotiated_agency_reading are the more genuinely distinct pair, with the individual-centered pair as the real single alternative.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_underdetermination, conceptual, 'Whether the four-reading decomposition of the kernel is itself the correct cut, or an artifact of the source material''s own categories.').

omega_variable(
    dependent_voice_corroboration_gap,
    'How much of the ''live founding problem'' status rests on corroboration from adult critics (ethicists, practitioners) speaking for dependents, versus direct testimony from dependents (especially children) themselves?',
    'Distinguish corroboration sources: academic/clinical literature citing outcomes for children of exited parents versus first-person accounts; weight the founding_problem_status assessment toward direct-outcome data where available.',
    'If the corroboration is mostly third-party academic argument rather than outcome data, the founding_problem_status claim is weaker than ''live'' suggests and should be treated as contested rather than settled.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dependent_voice_corroboration_gap, empirical, 'Whether the corroboration for dependents'' costs is outcome-based or argument-based.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(relational_obligation_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rela_tr_t0, relational_obligation_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(rela_tr_t8, relational_obligation_reading, theater_ratio, 8, 0.19).
narrative_ontology:measurement(rela_tr_t16, relational_obligation_reading, theater_ratio, 16, 0.22).
narrative_ontology:measurement(rela_tr_t24, relational_obligation_reading, theater_ratio, 24, 0.25).
narrative_ontology:measurement(rela_tr_t32, relational_obligation_reading, theater_ratio, 32, 0.28).
narrative_ontology:measurement(rela_tr_t40, relational_obligation_reading, theater_ratio, 40, 0.3).

% Extraction over time
narrative_ontology:measurement(rela_be_t0, relational_obligation_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(rela_be_t8, relational_obligation_reading, base_extractiveness, 8, 0.46).
narrative_ontology:measurement(rela_be_t16, relational_obligation_reading, base_extractiveness, 16, 0.5).
narrative_ontology:measurement(rela_be_t24, relational_obligation_reading, base_extractiveness, 24, 0.53).
narrative_ontology:measurement(rela_be_t32, relational_obligation_reading, base_extractiveness, 32, 0.56).
narrative_ontology:measurement(rela_be_t40, relational_obligation_reading, base_extractiveness, 40, 0.58).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(relational_obligation_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(relational_obligation_reading, attachment_coordination).
narrative_ontology:boltzmann_floor_override(relational_obligation_reading, 0.08).
narrative_ontology:affects_constraint(relational_obligation_reading, sovereign_agency_reading).
narrative_ontology:affects_constraint(relational_obligation_reading, structural_conditions_reading).
narrative_ontology:affects_constraint(relational_obligation_reading, negotiated_agency_reading).

% DUAL FORMULATION NOTE:
% This story is one of (at least) four readings of freedom_locus_kernel. sovereign_agency_reading omits the victim set introduced here entirely (dependents/partners are not structural parties to the freedom question). structural_conditions_reading names constraints ON the individual but not costs the individual's exit imposes on others. negotiated_agency_reading treats obligations as continuously renegotiable rather than as a fixed cost ledger, and is the reading most likely to influence how this reading's obligations get discharged over time without foreclosing it. Each sibling should carry its own ε, its own beneficiary/victim structure, and link back here.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
