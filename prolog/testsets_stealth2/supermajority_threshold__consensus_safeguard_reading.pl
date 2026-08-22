% ============================================================================
% CONSTRAINT STORY: supermajority_threshold__consensus_safeguard_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_supermajority_threshold__consensus_safeguard_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
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
 *   constraint_id: supermajority_threshold__consensus_safeguard_reading
 *   human_readable: Supermajority Amendment Threshold — Consensus Safeguard Reading
 *   domain: political/constitutional
 *
 * SUMMARY:
 *   A constitutional amendment rule requires approval well beyond a simple
 *   majority — typically supermajorities in the national legislature plus
 *   ratification across constituent units — before fundamental law may
 *   change. The consensus_safeguard_reading holds that this barrier is a
 *   democratic quality filter: it converts raw, possibly transient majorities
 *   into validated deep consensus, so that only changes commanding unusually
 *   broad and durable support alter the constitutional framework. The
 *   arrangement has diffuse beneficiaries (everyone with long-horizon stakes
 *   in rule stability, political minorities shielded from majority swings,
 *   future generations who inherit the framework) and, on this reading's
 *   account, no specific victim set: a proposal that fails is read as lacking
 *   deep consensus, not as having been extracted from. KEY AGENTS (by
 *   structural relationship): see key_agents. Claim/metric independence is
 *   preserved: the claimed type is authored from this reading's structural
 *   assessment of the arrangement; the metrics are authored independently as
 *   descriptive estimates, and the engine computes per-seat classifications
 *   from the structural data without reference to the claim.
 *
 * KEY AGENTS:
 *   - - long_horizon_citizens: Primary beneficiary (moderate/constrained) — stakes careers, savings, and civic commitments on rule stability; bears no per-use cost of the barrier
 *   - - political_minorities: Protected beneficiary (powerless/constrained) — relies on amendment difficulty to keep won protections in place against recurring majority swings
 *   - - future_generations: Disenfranchised beneficiary (powerless/trapped) — inherits whatever framework survives the present era, with no present voice in it
 *   - - amendment_proponents: Cost-bearing seat (organized/constrained) — assembles supermajority coalitions across jurisdictions and years; absorbs the full cost of failed campaigns
 *   - - national_legislatures: Administrator/agenda-setter (institutional/arbitrage) — initiates and certifies amendment attempts; simultaneously operates the ordinary-law channel that substitutes for most would-be constitutional change
 *   - - constitutional_courts: Observer-beneficiary (institutional/analytical) — adjudicates amendment-procedure disputes; a slowly changing text secures and enlarges the interpretive domain
 *   - - disenfranchised_residents: Excluded voice (powerless/trapped) — bound by the constitution's rules yet absent from every ratification seat
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(supermajority_threshold__consensus_safeguard_reading, 0.26).
domain_priors:suppression_score(supermajority_threshold__consensus_safeguard_reading, 0.18).
domain_priors:theater_ratio(supermajority_threshold__consensus_safeguard_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(supermajority_threshold__consensus_safeguard_reading, extractiveness, 0.26).
narrative_ontology:constraint_metric(supermajority_threshold__consensus_safeguard_reading, suppression_requirement, 0.18).
narrative_ontology:constraint_metric(supermajority_threshold__consensus_safeguard_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(supermajority_threshold__consensus_safeguard_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(supermajority_threshold__consensus_safeguard_reading, resistance, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(supermajority_threshold__consensus_safeguard_reading, rope).
narrative_ontology:human_readable(supermajority_threshold__consensus_safeguard_reading, "Supermajority Amendment Threshold — Consensus Safeguard Reading").
narrative_ontology:topic_domain(supermajority_threshold__consensus_safeguard_reading, "political/constitutional").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(supermajority_threshold__consensus_safeguard_reading, 'cc32ee08-233e-411d-b89f-6802317252d2').
narrative_ontology:cs_kernel_codification('cc32ee08-233e-411d-b89f-6802317252d2', formalized).
narrative_ontology:cs_authority_grounding('cc32ee08-233e-411d-b89f-6802317252d2', lineage).
narrative_ontology:cs_interpretation_layer_present('cc32ee08-233e-411d-b89f-6802317252d2').
narrative_ontology:cs_reading_relation('cc32ee08-233e-411d-b89f-6802317252d2', supermajority_threshold__minoritarian_veto_reading, forecloses).
narrative_ontology:cs_reading_relation('cc32ee08-233e-411d-b89f-6802317252d2', supermajority_threshold__adaptive_gradient_reading, influences).
narrative_ontology:cs_axiom('cc32ee08-233e-411d-b89f-6802317252d2', foundational, transient_majorities_lack_constitutional_authority).
narrative_ontology:cs_axiom_status(transient_majorities_lack_constitutional_authority, holdable).
narrative_ontology:cs_axiom_grounding('cc32ee08-233e-411d-b89f-6802317252d2', transient_majorities_lack_constitutional_authority, deontological).
narrative_ontology:cs_axiom('cc32ee08-233e-411d-b89f-6802317252d2', secondary, amendment_difficulty_signals_deliberative_quality).
narrative_ontology:cs_axiom_status(amendment_difficulty_signals_deliberative_quality, holdable).
narrative_ontology:cs_axiom_grounding('cc32ee08-233e-411d-b89f-6802317252d2', amendment_difficulty_signals_deliberative_quality, instrumental).
narrative_ontology:cs_reference_frame('cc32ee08-233e-411d-b89f-6802317252d2', deep_consensus_amendment_standard).
narrative_ontology:cs_drift_state('cc32ee08-233e-411d-b89f-6802317252d2', contemporary_polarization_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('cc32ee08-233e-411d-b89f-6802317252d2', '').
narrative_ontology:cs_kernel_id(supermajority_threshold__consensus_safeguard_reading, supermajority_threshold).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(supermajority_threshold__consensus_safeguard_reading, long_horizon_citizens).
narrative_ontology:constraint_beneficiary(supermajority_threshold__consensus_safeguard_reading, political_minorities).
narrative_ontology:constraint_beneficiary(supermajority_threshold__consensus_safeguard_reading, future_generations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(supermajority_threshold__consensus_safeguard_reading, constitutional_courts).
narrative_ontology:constraint_victim(supermajority_threshold__consensus_safeguard_reading, amendment_proponents).
narrative_ontology:constraint_vindicates(supermajority_threshold__consensus_safeguard_reading, constitutional_precommitment_doctrine).
narrative_ontology:constraint_vindicates(supermajority_threshold__consensus_safeguard_reading, passion_cycle_filtering_hypothesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Plan careers, families, savings, and civic commitments on the assumption that fundamental rules will hold roughly steady across decades. They vote in ordinary politics but rarely engage amendment politics; their stake is that the rules they invested under survive electoral swings. Leaving the jurisdiction to escape a rewritten constitution is costly and rarely considered.
narrative_ontology:constraint_stakeholder(supermajority_threshold__consensus_safeguard_reading, long_horizon_citizens, beneficiary,
    moderate, generational, constrained, national).

% Hold religious, ideological, or cultural positions that lose recurring ordinary-majority votes. They cannot win amendment contests on their own and rely on the difficulty of rewriting fundamental law to keep previously secured protections in place. Exit — emigration or withdrawal from public life — is costly and partial.
narrative_ontology:constraint_stakeholder(supermajority_threshold__consensus_safeguard_reading, political_minorities, beneficiary,
    powerless, biographical, constrained, national).

% Will inherit whatever fundamental framework survives the present era but have no vote, lobby, or seat in any ratification body today. Their interests reach the process only through the durability of the rules themselves and through advocates who speak on their behalf.
narrative_ontology:constraint_stakeholder(supermajority_threshold__consensus_safeguard_reading, future_generations, beneficiary,
    powerless, civilizational, trapped, national).

% Organize coalitions to change fundamental law — new rights, new institutions, revised structures. Each attempt requires assembling supermajorities across jurisdictions and often across years; failed campaigns absorb money, organizing labor, and political capital. Ordinary statutes and litigation remain open for lesser aims but cannot deliver constitutional-level change.
narrative_ontology:constraint_stakeholder(supermajority_threshold__consensus_safeguard_reading, amendment_proponents, payer,
    organized, biographical, constrained, national).

% Initiate formal amendment proposals and certify ratification results, applying the numerical threshold in each attempt. The same bodies legislate ordinarily, and most policy ambitions are pursued through that cheaper channel, so the formal gate is exercised rarely and administered routinely when it is.
narrative_ontology:constraint_stakeholder(supermajority_threshold__consensus_safeguard_reading, national_legislatures, agenda_setter,
    institutional, generational, arbitrage, national).

% Adjudicate disputes over amendment procedure and interpret the standing text. A slowly changing text enlarges and stabilizes the court's interpretive role; judges frequently defend the difficulty of amendment as a design feature in opinions and public lectures.
narrative_ontology:constraint_stakeholder(supermajority_threshold__consensus_safeguard_reading, constitutional_courts, observer,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(supermajority_threshold__consensus_safeguard_reading, constitutional_courts, beneficiary).

% Live under the constitution's rules — paying taxes, subject to its courts — but hold no vote in the legislatures or referenda that would ratify a change. They would object to entrenchments that lock in their subordinate status yet appear in the process only as subjects, never as participants.
narrative_ontology:constraint_stakeholder(supermajority_threshold__consensus_safeguard_reading, disenfranchised_residents, excluded,
    powerless, biographical, trapped, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(supermajority_threshold__consensus_safeguard_reading, diffuse).
narrative_ontology:fixing_cost_class(supermajority_threshold__consensus_safeguard_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the precommitment problem of constitutional politics: a polity needs its fundamental rules to be hard enough to change that ordinary majorities cannot rewrite them session by session, so that rights, structures, and long-term plans remain reliable anchors. The threshold operationalizes that hardness as a number.
% TRANSFER_FUNCTION: Moves decision rights over fundamental law from present simple majorities to durable cross-jurisdiction supermajorities; moves the cost of constitutional change onto proponents (coalition-building across time and territory); confers security of expectations on everyone governed by the rules.
% ABSENT_VOICES: Disenfranchised residents — territorial populations and non-citizen residents bound by the constitution — are absent from every ratification seat; future generations cannot appear; supporters of repeatedly blocked amendments enter the record only as defeated vote totals. They sit outside the franchise the threshold presupposes.
% DISAPPEARANCE_RATIONALE: Overnight removal would let ordinary legislative majorities rewrite fundamental law at will: rights protections would churn with each electoral swing, long-term contracts and institutional designs would lose their anchor, courts would lose the fixed text that disciplines interpretation, and political minorities would face recurring refighting of settled protections. The constitutional order would reorganize around whichever coalition held temporary control.
% FOUNDING_PROBLEM: Early constitutional orders showed rapid, passion-driven revision: factions capturing temporary majorities rewrote fundamental rules within single sessions, destabilizing rights and expectations. The threshold was built to ensure that only changes commanding unusually broad and durable support could alter the fundamental framework.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set: comparative-politics research correlating amendment difficulty with constitutional endurance and rights stability; historical archives documenting pre-threshold constitutional churn (frequent state-constitution replacement in the 1780s–90s); and backsliding studies showing how easily-amended constitutions were serially manipulated by elected incumbents (Venezuela 1999–2009, Hungary 2010–2013). None of these sources sits inside the arrangement's beneficiary groups.
narrative_ontology:disappearance_verdict(supermajority_threshold__consensus_safeguard_reading, world_rearranges).
narrative_ontology:founding_problem_status(supermajority_threshold__consensus_safeguard_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(supermajority_threshold__consensus_safeguard_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(supermajority_threshold__consensus_safeguard_reading, 'none', 1).
narrative_ontology:epsilon_provenance(supermajority_threshold__consensus_safeguard_reading, 0.26, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(supermajority_threshold__consensus_safeguard_reading_tests).
:- end_tests(supermajority_threshold__consensus_safeguard_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low-to-moderate (0.26 at interval end) because, on this reading's own accounting, the barrier's principal cost — the frustration of change-proponents — is the filter operating as designed rather than rent collection; the slow rise across the interval reflects accumulating cases where durable supermajority-level opinion failed to clear the gate, each registering as a real cost the filter imposed. Suppression is low (0.18): nothing is coerced, no exit is barred, the amendment path remains formally open, and the rule is transparent arithmetic rather than enforced conformity. Theater is low (0.12): the procedure is functional; the mild rise tracks growing ceremonial defense of the threshold in civic rhetoric. Accessibility_collapse is moderate (0.40): understanding the threshold does not eliminate alternatives — ordinary legislation, litigation, and informal constitutional change remain available for lesser aims — but every route to constitutional-level change collapses onto the single gate. Resistance is moderate (0.38): recurring reform movements target the threshold itself whenever blocked majorities accumulate. Suppression is authored as a raw structural property, unscaled; only extractiveness is scaled downstream by directionality and scope. The three temporal series share one grid (points 0–48 step 8) so every metric is authored at every examined time point; trajectories are mildly monotone, reflecting gradual tightening of the gate's practical force as consensus formation degraded — no oscillation, so no cyclical documentation is required. Receipt surface: re-reading every stakeholder situation, no seat captures the barrier's product — stability accrues across the governed population and the courts' interpretive gain is incidental — so gain_flow='diffuse' is an affirmative checked finding, not a default. Fixing cost is prohibitive: any revision of the threshold must itself clear the threshold (or an extraordinary convention process), so the cost of fixing exceeds any plausible benefit to a prospective fixer.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the amendment_proponents seat the barrier is an obstacle that consumes decades of organizing for uncertain yield; from the beneficiary seats it is a shield that keeps settled protections settled; from the national_legislatures seat it is routine procedural administration shadowed by a cheaper arbitrage channel; from the constitutional_courts seat it is professional infrastructure. Same numbers, different experienced arrangements — the engine derives this divergence from power, exit, and role data, and the divergence is the measurement, not noise.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive the derivation: long_horizon_citizens, political_minorities, and future_generations sit near the beneficiary end (d low, effective extraction damped or inverted into subsidy), with future_generations furthest out — fully trapped, unable even in principle to exit or bargain. amendment_proponents bear the barrier's costs and sit toward the target end, but bounded: their exit options include the ordinary-law channel, and success converts them into beneficiaries of the amended order, so their d is elevated but not maximal. national_legislatures sit near symmetric — they administer the gate yet routinely route around it. constitutional_courts occupy the analytical seat with an incidental beneficiary tilt. No directionality overrides were needed: the beneficiary/cost-bearing declarations plus exit options already produce the correct relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — passion-driven revision of fundamental law — remains live, so no mandate-atrophy is declared and mandatrophy_resolved is left unset. The classification guards against mislabeling in both directions: the rope claim resists reading the barrier as pure extraction (its coordination function — precommitment against majority-cycle rewrites — is genuine and primary on this reading's account), while the blocking_victim_emergence and kernel_reading_contest omegas resist complacent rope certification by specifying exactly what evidence would convert diffuse coordination costs into targeted extraction. The informal_substitution_drift omega additionally tracks the decay path: if informal channels come to do the constitutional work, the formal gate persists as maintained ceremony, theater_ratio climbs, and the structure drifts toward inertial maintenance — the temporal series is the tripwire for that transition.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'This story instantiates the consensus_safeguard_reading of the supermajority_threshold kernel; the minoritarian_veto_reading describes the identical numerical rule as converting historical advantage into a standing veto against majoritarian will. Which description matches the rule''s actual operation?',
    'Longitudinal opinion data on repeatedly blocked amendments: if proposals opposed by durable supermajorities fail, the safeguard account holds; if proposals backed by stable supermajority-level opinion fail repeatedly across decades, the veto account gains.',
    'Resolved toward the veto reading, this constraint acquires a victim set (persistently blocked majorities), epsilon rises sharply, and classification moves from rope toward tangled_rope or snare; resolved toward the safeguard, the current profile stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, empirical, 'Whether the amendment barrier is a consensus filter or an entrenched-privilege veto — the core disagreement between this reading and its minoritarian sibling.').

omega_variable(
    threshold_calibration_fit,
    'Does the threshold''s magnitude match actual consensus-formation rates and the reversibility cost of mistaken constitutional change, or is it miscalibrated in either direction?',
    'Comparative analysis of amendment success rates against measured durability of public opinion on the same questions; natural experiments from jurisdictions that raised or lowered amendment thresholds.',
    'Over-calibration (threshold too high) means deep consensus is blocked and extraction lands on blocked majorities; under-calibration means transient waves pass and the filter fails its function. Either finding reshapes epsilon and strengthens the adaptive_gradient_reading''s instrument-framing of the same rule.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(threshold_calibration_fit, empirical, 'Whether the threshold level is calibrated to real consensus dynamics — the adaptive-gradient sibling''s central concern.').

omega_variable(
    blocking_victim_emergence,
    'Under what conditions does a blocked amendment produce a specific, identifiable victim set rather than legitimate filtering of insufficient consensus?',
    'Case audit of amendments blocked for multiple decades despite sustained supermajority-level public support, isolating whether failure traces to opinion insufficiency or to jurisdictional malapportionment and veto-point stacking beyond the headline threshold.',
    'Confirmed victim cases convert diffuse coordination costs into targeted extraction, requiring victims[] declarations and reclassification toward tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(blocking_victim_emergence, empirical, 'Boundary condition at which this reading''s no-victim structure breaks and the barrier acquires targets.').

omega_variable(
    informal_substitution_drift,
    'Has informal constitutional change (judicial reinterpretation, convention, ordinary-statute accretion) substituted so thoroughly for formal amendment that the threshold no longer governs actual constitutional development?',
    'Measure the share of consequential constitutional-level change over recent decades accomplished through non-formal-amendment channels versus formally ratified amendments.',
    'If substitution dominates, the formal gate persists as maintained ceremony while real change routes elsewhere — theater_ratio climbs and the structure decays toward inertial maintenance; the temporal series in this story tracks that trajectory.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(informal_substitution_drift, empirical, 'Whether the formal threshold still does the constitutional work or has been bypassed by informal channels.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(supermajority_threshold__consensus_safeguard_reading, 0, 48).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(supe_tr_t0, supermajority_threshold__consensus_safeguard_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(supe_tr_t0, observed).
narrative_ontology:measurement(supe_tr_t8, supermajority_threshold__consensus_safeguard_reading, theater_ratio, 8, 0.09).
narrative_ontology:measurement_basis(supe_tr_t8, observed).
narrative_ontology:measurement(supe_tr_t16, supermajority_threshold__consensus_safeguard_reading, theater_ratio, 16, 0.1).
narrative_ontology:measurement_basis(supe_tr_t16, observed).
narrative_ontology:measurement(supe_tr_t24, supermajority_threshold__consensus_safeguard_reading, theater_ratio, 24, 0.1).
narrative_ontology:measurement_basis(supe_tr_t24, observed).
narrative_ontology:measurement(supe_tr_t32, supermajority_threshold__consensus_safeguard_reading, theater_ratio, 32, 0.11).
narrative_ontology:measurement_basis(supe_tr_t32, observed).
narrative_ontology:measurement(supe_tr_t40, supermajority_threshold__consensus_safeguard_reading, theater_ratio, 40, 0.11).
narrative_ontology:measurement_basis(supe_tr_t40, observed).
narrative_ontology:measurement(supe_tr_t48, supermajority_threshold__consensus_safeguard_reading, theater_ratio, 48, 0.12).
narrative_ontology:measurement_basis(supe_tr_t48, observed).

% Extraction over time
narrative_ontology:measurement(supe_be_t0, supermajority_threshold__consensus_safeguard_reading, base_extractiveness, 0, 0.19).
narrative_ontology:measurement_basis(supe_be_t0, observed).
narrative_ontology:measurement(supe_be_t8, supermajority_threshold__consensus_safeguard_reading, base_extractiveness, 8, 0.2).
narrative_ontology:measurement_basis(supe_be_t8, observed).
narrative_ontology:measurement(supe_be_t16, supermajority_threshold__consensus_safeguard_reading, base_extractiveness, 16, 0.22).
narrative_ontology:measurement_basis(supe_be_t16, observed).
narrative_ontology:measurement(supe_be_t24, supermajority_threshold__consensus_safeguard_reading, base_extractiveness, 24, 0.23).
narrative_ontology:measurement_basis(supe_be_t24, observed).
narrative_ontology:measurement(supe_be_t32, supermajority_threshold__consensus_safeguard_reading, base_extractiveness, 32, 0.24).
narrative_ontology:measurement_basis(supe_be_t32, observed).
narrative_ontology:measurement(supe_be_t40, supermajority_threshold__consensus_safeguard_reading, base_extractiveness, 40, 0.25).
narrative_ontology:measurement_basis(supe_be_t40, observed).
narrative_ontology:measurement(supe_be_t48, supermajority_threshold__consensus_safeguard_reading, base_extractiveness, 48, 0.26).
narrative_ontology:measurement_basis(supe_be_t48, observed).

% Suppression requirement over time
narrative_ontology:measurement(supe_su_t0, supermajority_threshold__consensus_safeguard_reading, suppression_requirement, 0, 0.12).
narrative_ontology:measurement_basis(supe_su_t0, observed).
narrative_ontology:measurement(supe_su_t8, supermajority_threshold__consensus_safeguard_reading, suppression_requirement, 8, 0.13).
narrative_ontology:measurement_basis(supe_su_t8, observed).
narrative_ontology:measurement(supe_su_t16, supermajority_threshold__consensus_safeguard_reading, suppression_requirement, 16, 0.14).
narrative_ontology:measurement_basis(supe_su_t16, observed).
narrative_ontology:measurement(supe_su_t24, supermajority_threshold__consensus_safeguard_reading, suppression_requirement, 24, 0.15).
narrative_ontology:measurement_basis(supe_su_t24, observed).
narrative_ontology:measurement(supe_su_t32, supermajority_threshold__consensus_safeguard_reading, suppression_requirement, 32, 0.16).
narrative_ontology:measurement_basis(supe_su_t32, observed).
narrative_ontology:measurement(supe_su_t40, supermajority_threshold__consensus_safeguard_reading, suppression_requirement, 40, 0.17).
narrative_ontology:measurement_basis(supe_su_t40, observed).
narrative_ontology:measurement(supe_su_t48, supermajority_threshold__consensus_safeguard_reading, suppression_requirement, 48, 0.18).
narrative_ontology:measurement_basis(supe_su_t48, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(supermajority_threshold__consensus_safeguard_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(supermajority_threshold__consensus_safeguard_reading, supermajority_threshold__minoritarian_veto_reading).
narrative_ontology:affects_constraint(supermajority_threshold__consensus_safeguard_reading, supermajority_threshold__adaptive_gradient_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'supermajority threshold' covers three structurally distinct claims that share one numerical rule: (1) this consensus_safeguard_reading — the barrier validates deep consensus (diffuse beneficiaries, no victim set, low epsilon, rope); (2) minoritarian_veto_reading — the barrier entrenches historical privilege against majoritarian will (identifiable blocked victims, high epsilon); (3) adaptive_gradient_reading — the barrier is a calibration-dependent tool (epsilon indexed to threshold-fit). The readings disagree on the location and existence of extraction, so each requires its own epsilon, beneficiary/victim structure, and classification; this file decomposes the label accordingly and links the family. Upstream/downstream: the safeguard reading, when dominant, supplies the legitimacy narrative that weakens the calibration program's premise (an influences edge), and directly contradicts the veto reading's core premise (a forecloses edge).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
