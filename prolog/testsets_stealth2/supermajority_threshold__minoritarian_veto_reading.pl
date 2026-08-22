% ============================================================================
% CONSTRAINT STORY: supermajority_threshold__minoritarian_veto_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_supermajority_threshold__minoritarian_veto_reading, []).

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
 *   constraint_id: supermajority_threshold__minoritarian_veto_reading
 *   human_readable: Supermajority Amendment Threshold as Minoritarian Veto (Minoritarian-Veto Reading)
 *   domain: political/constitutional
 *
 * SUMMARY:
 *   A constitutional amendment rule requiring assent far beyond a numerical
 *   majority — supermajorities in both chambers plus ratification by a fixed
 *   count of federated units — is read here through the minoritarian-veto
 *   reading of the supermajority_threshold kernel. On this reading the
 *   threshold's operative effect is to place constitutional revision in the
 *   hands of whichever coalition of low-population units can supply the
 *   blocking count: a bloc representing a small and shrinking fraction of the
 *   national population can defeat any proposal, however sustained its
 *   majority support. Arrangements favorable to historically privileged units
 *   and incumbent interests therefore become practically irrevocable, and
 *   each blocked reform transfers continued governance of the contested
 *   domain to those the status quo protects. The epsilon referent is the
 *   standing arrangement — the amendment threshold as it actually operates in
 *   the mass-suffrage era — assessed by this reading's own lights; the
 *   reading's endorsed alternative plays no role in the value. Sibling
 *   readings (consensus_safeguard, adaptive_gradient) instantiate different
 *   constraints with different victim sets and are authored separately; this
 *   file links them through network.affects_constraints. Interval mapping:
 *   time point 0 = 1960, point 65 = 2025, with intermediate points at 13-year
 *   census-anchored steps. KEY AGENTS (by structural relationship): -
 *   blocking_minority_legislators: Primary beneficiary and effective
 *   agenda-holder (organized/constrained) — assent mathematically necessary
 *   for any amendment; trades it for concessions -
 *   status_quo_entrenched_interests: Secondary beneficiary
 *   (powerful/constrained) — collects preservation of entrenched position
 *   from every blocked reform without operating the rule -
 *   overrepresented_constituency_voters: Structural beneficiary
 *   (moderate/constrained) — ballots amplified in constitutional matters
 *   relative to population, irrespective of individual preferences -
 *   contemporary_reform_majorities: Primary target (organized/trapped) —
 *   sustained national majorities whose revisions cannot clear the
 *   unit-weighted gate - blocked_reform_movements: Repeated target
 *   (moderate/trapped) — multi-decade movements consuming resources in
 *   near-successes killed at ratification -
 *   unrepresented_territory_residents: Excluded voice (powerless/trapped) —
 *   governed by the law with no seat in either amendment route -
 *   constitutional_judiciary: Institutional observer
 *   (institutional/analytical) — doctrinally declines to police the
 *   threshold's distributive effects - comparative_constitutional_scholars:
 *   Analytical observer (analytical/global) — supplies the external record of
 *   success rates and blocking-coalition demographics
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(supermajority_threshold__minoritarian_veto_reading, 0.76).
domain_priors:suppression_score(supermajority_threshold__minoritarian_veto_reading, 0.7).
domain_priors:theater_ratio(supermajority_threshold__minoritarian_veto_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(supermajority_threshold__minoritarian_veto_reading, extractiveness, 0.76).
narrative_ontology:constraint_metric(supermajority_threshold__minoritarian_veto_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(supermajority_threshold__minoritarian_veto_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(supermajority_threshold__minoritarian_veto_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(supermajority_threshold__minoritarian_veto_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(supermajority_threshold__minoritarian_veto_reading, snare).
narrative_ontology:human_readable(supermajority_threshold__minoritarian_veto_reading, "Supermajority Amendment Threshold as Minoritarian Veto (Minoritarian-Veto Reading)").
narrative_ontology:topic_domain(supermajority_threshold__minoritarian_veto_reading, "political/constitutional").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(supermajority_threshold__minoritarian_veto_reading, 'f7bb8b60-af2a-4109-8d34-958b46046277').
narrative_ontology:cs_kernel_codification('f7bb8b60-af2a-4109-8d34-958b46046277', fixed_text).
narrative_ontology:cs_authority_grounding('f7bb8b60-af2a-4109-8d34-958b46046277', lineage).
narrative_ontology:cs_interpretation_layer_present('f7bb8b60-af2a-4109-8d34-958b46046277').
narrative_ontology:cs_reading_relation('f7bb8b60-af2a-4109-8d34-958b46046277', supermajority_threshold__consensus_safeguard_reading, coexists_with).
narrative_ontology:cs_reading_relation('f7bb8b60-af2a-4109-8d34-958b46046277', supermajority_threshold__adaptive_gradient_reading, influences).
narrative_ontology:cs_axiom('f7bb8b60-af2a-4109-8d34-958b46046277', foundational, unit_weighted_veto_is_illegitimate_privilege).
narrative_ontology:cs_axiom_status(unit_weighted_veto_is_illegitimate_privilege, holdable).
narrative_ontology:cs_axiom_grounding('f7bb8b60-af2a-4109-8d34-958b46046277', unit_weighted_veto_is_illegitimate_privilege, deontological).
narrative_ontology:cs_axiom('f7bb8b60-af2a-4109-8d34-958b46046277', foundational, entrenchment_without_renewal_loses_consent).
narrative_ontology:cs_axiom_status(entrenchment_without_renewal_loses_consent, holdable).
narrative_ontology:cs_axiom_grounding('f7bb8b60-af2a-4109-8d34-958b46046277', entrenchment_without_renewal_loses_consent, deontological).
narrative_ontology:cs_axiom('f7bb8b60-af2a-4109-8d34-958b46046277', secondary, blocking_share_below_population_third).
narrative_ontology:cs_axiom_status(blocking_share_below_population_third, holdable).
narrative_ontology:cs_axiom_grounding('f7bb8b60-af2a-4109-8d34-958b46046277', blocking_share_below_population_third, empirically_contingent).
narrative_ontology:cs_reference_frame('f7bb8b60-af2a-4109-8d34-958b46046277', majoritarian_self_government_baseline).
narrative_ontology:cs_drift_state('f7bb8b60-af2a-4109-8d34-958b46046277', contemporary_mass_suffrage_era, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('f7bb8b60-af2a-4109-8d34-958b46046277', '').
narrative_ontology:cs_kernel_id(supermajority_threshold__minoritarian_veto_reading, supermajority_threshold).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(supermajority_threshold__minoritarian_veto_reading, blocking_minority_legislators).
narrative_ontology:constraint_beneficiary(supermajority_threshold__minoritarian_veto_reading, status_quo_entrenched_interests).
narrative_ontology:constraint_beneficiary(supermajority_threshold__minoritarian_veto_reading, overrepresented_constituency_voters).
narrative_ontology:constraint_victim(supermajority_threshold__minoritarian_veto_reading, contemporary_reform_majorities).
narrative_ontology:constraint_victim(supermajority_threshold__minoritarian_veto_reading, blocked_reform_movements).
narrative_ontology:constraint_vindicates(supermajority_threshold__minoritarian_veto_reading, federalism_entrenchment_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Legislators representing the least populous units of the federation. Because amendment requires assent from a fixed number of units regardless of population, a compact of small units can defeat any proposal; their assent is mathematically necessary, so they trade it for concessions — appropriations, jurisdictional carve-outs, deferral of rival reforms. The veto power persists as long as they hold office in the unit-weighted chamber; leaving office surrenders it, and it cannot be exercised outside the constitutional order they sit atop.
narrative_ontology:constraint_stakeholder(supermajority_threshold__minoritarian_veto_reading, blocking_minority_legislators, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(supermajority_threshold__minoritarian_veto_reading, blocking_minority_legislators, agenda_setter).

% Incumbent industries, regions, and officeholders whose legal position is written into the fundamental law or shielded by arrangements the amendment rule protects. Every failed reform leaves their position intact at no cost to them; they fund the defense of the threshold and supply public arguments for it, but they do not operate the rule — they collect its output.
narrative_ontology:constraint_stakeholder(supermajority_threshold__minoritarian_veto_reading, status_quo_entrenched_interests, beneficiary,
    powerful, generational, constrained, national).

% Voters in low-population units whose ballots carry more weight in the unit-weighted chamber than voters elsewhere. Individually they hold ordinary policy preferences, and many favor the very reforms the threshold blocks; structurally, however, the arrangement amplifies their voice in constitutional matters relative to their numbers, and they bear none of the denial imposed on reform supporters in denser units.
narrative_ontology:constraint_stakeholder(supermajority_threshold__minoritarian_veto_reading, overrepresented_constituency_voters, beneficiary,
    moderate, biographical, constrained, national).

% National majorities that sustain support for specific constitutional reforms across years and election cycles — polled consistently, mobilized repeatedly, represented in the popular chamber — yet cannot clear the unit-weighted assent requirement. Their options inside the constitutional order are nil: the alternate amendment route runs through the same unit-weighted gate, ordinary legislation cannot reach constitutional questions, and exit from the order altogether carries costs no movement will pay.
narrative_ontology:constraint_stakeholder(supermajority_threshold__minoritarian_veto_reading, contemporary_reform_majorities, payer,
    organized, biographical, trapped, national).

% Organized movements that have repeatedly carried proposed amendments to the point of passage — equal-rights guarantees, representation for the capital district, succession and franchise repairs — and watched them die at the ratification stage after decades of effort. Their resources are consumed by repeated near-successes; successive generations of organizers have spent careers on measures that cleared the elected chamber but not the unit gate.
narrative_ontology:constraint_stakeholder(supermajority_threshold__minoritarian_veto_reading, blocked_reform_movements, payer,
    moderate, generational, trapped, national).

% Residents of territories and the capital district who live under the fundamental law but hold no vote in either amendment route: no senators, no ratifying legislature, no convention seat. They would object that the consensus filter counts units and historical boundaries while excluding them entirely from decisions about the law that governs them; they are outside the room because the same apportionment that manufactures the veto manufactures their exclusion.
narrative_ontology:constraint_stakeholder(supermajority_threshold__minoritarian_veto_reading, unrepresented_territory_residents, excluded,
    powerless, generational, trapped, national).

% Courts review amendment-related disputes — ratification timing, convention validity, the boundary of the revision power — and uniformly decline to police the threshold's distributive effects, treating the amendment article as beyond judicial management. They take no side in reform contests; their doctrinal deference is part of the environment every other seat operates in.
narrative_ontology:constraint_stakeholder(supermajority_threshold__minoritarian_veto_reading, constitutional_judiciary, observer,
    institutional, generational, analytical, national).

% Scholars comparing amendment rules across federations document success rates, blocking-coalition demographics, and the fate of majority-supported reforms, supplying the external record the other seats argue over. They hold no vote and collect nothing; their analyses are cited by all sides and owned by none.
narrative_ontology:constraint_stakeholder(supermajority_threshold__minoritarian_veto_reading, comparative_constitutional_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(supermajority_threshold__minoritarian_veto_reading, blocking_minority_legislators).
narrative_ontology:fixing_cost_class(supermajority_threshold__minoritarian_veto_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Stabilizes the fundamental law by requiring assent spanning both chambers of the national legislature and a fixed count of federated units, ensuring no constitutional change occurs over the objection of a geographically broad minority of units; addresses the problem of protecting a federal bargain from revision by concentrated population centers alone.
% TRANSFER_FUNCTION: Moves authoritative control over constitutional revision from national majorities, and the popularly elected chamber that represents them, to whatever coalition of low-population units can supply the blocking count; each blocked reform additionally transfers continued governance of the contested domain to the interests the status quo protects, at no cost to those interests.
% ABSENT_VOICES: Residents of territories and the capital district — governed by the law with no vote in either amendment route — and future generations, who inherit arrangements they had no part in ratifying, would object that the consensus filter counts units and historical boundaries rather than persons. They are absent because the same unit-weighted apportionment that manufactures the veto manufactures their exclusion; within the process, the voices of populous-state majorities are present but diluted to near-irrelevance on constitutional questions.
% DISAPPEARANCE_RATIONALE: If the threshold vanished overnight, amendment would track national majorities: the queue of reforms with sustained majority support — equal-rights guarantees, representation for the capital district, succession and franchise repairs — would move toward enactment, entrenched arrangements shielding incumbent regions and industries would become revisable, and the bargaining position of low-population units in ordinary legislation would collapse, since their assent would no longer be purchasable at the constitutional level. The distribution of constitutional power across units, chambers, and generations would reorganize within a few political cycles.
% FOUNDING_PROBLEM: Secure the assent of small and economically vulnerable units to a durable federal union by guaranteeing that the fundamental bargain could not be rewritten over their objection by transient national majorities or concentrated factional interest.
% FOUNDING_PROBLEM_CORROBORATION: Ratification-era records and comparative-federalism scholarship corroborate the founding problem as stated — small-unit consent was the explicit price of union, and the anti-factional motive is documented in the framers' own papers. Attestation that the problem remains live comes chiefly from the threshold's beneficiaries and from civic-canonical sources inside the tradition; external corroboration of continued liveness is thin. Democratic-theory literature, cross-national amendment-success studies, and reform-commission testimony from outside the benefiting parties attest instead that the original problem persists only in attenuated form while the veto now blocks reforms with demonstrated sustained majority support — a split the contested status records honestly.
narrative_ontology:disappearance_verdict(supermajority_threshold__minoritarian_veto_reading, world_rearranges).
narrative_ontology:founding_problem_status(supermajority_threshold__minoritarian_veto_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(supermajority_threshold__minoritarian_veto_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(supermajority_threshold__minoritarian_veto_reading, 'none', 1).
narrative_ontology:epsilon_provenance(supermajority_threshold__minoritarian_veto_reading, 0.76, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(supermajority_threshold__minoritarian_veto_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(supermajority_threshold__minoritarian_veto_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(supermajority_threshold__minoritarian_veto_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (0.76 at interval end) because the referent — the operating threshold — denies nationally sustained majorities the capacity to revise the fundamental law while transferring the exercise of that capacity to a geographically concentrated blocking bloc; the transfer is continuous rather than episodic and compounds as population divergence widens the gap between blocking share and majority share. Suppression (0.70) is authored as a raw structural property, unscaled by power or scope: the alternate amendment route runs through the same unit-weighted gate, ordinary legislation is incompetent on constitutional questions, and judicial reinterpretation reaches only the margins, so understood alternatives collapse substantially for reform majorities (accessibility_collapse 0.60 — partial, because interstate compacts, statutory workarounds, and subnational experimentation remain available). Resistance (0.58) reflects decades of organized retry behavior rather than acquiescence. Theater (0.35): the stability and deliberation functions are real, but a growing share of the threshold's public defense is ritual invocation of founding wisdom detached from any articulable present-day consensus condition. The measurement series run on one shared six-point grid (1960-2025 at 13-year steps) with all three tracked metrics authored at every point; trajectories are monotonic rather than cyclical — amendment-attempt bursts occur, but the net drift of every tracked metric is upward, driven by demographic divergence, accumulating blocked reforms, and the hardening of the justification apparatus as legitimacy challenges mounted.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the beneficiary seats should compute divergent types from identical structural facts. From a blocking-minority legislator's seat the threshold is experienced as protective federalism: a hard-won guarantee that dense population centers cannot rewrite the bargain unilaterally — closer to a coordination device they would defend at real cost. From a reform-majority seat the same arithmetic is experienced as governed exclusion: participation in a process whose outcome is predetermined by unit geography. The overrepresented-voter seat splits the difference — many of its members support the blocked reforms personally while structurally collecting amplified weight. Coalition potential among the payer seats is real but blunted: reform movements do form episodic coalitions around single-issue amendment waves, yet the blocking units are geographically concentrated and institutionally disciplined, so coalition scale does not translate into gate-crossing votes. The engine computes these per-seat classifications from the structural data; nothing in the authored claim adjudicates them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality for the three beneficiary seats: blocking_minority_legislators sit nearest the subsidy end (from their seat the arrangement exists to preserve their assent-power), with status_quo_entrenched_interests and overrepresented_constituency_voters close behind — collectors who run nothing. Victim declarations drive high directionality for contemporary_reform_majorities and blocked_reform_movements; their trapped exit classification places them near the full-target end, since no arbitrage or mobility modulates their exposure. Spatial scope is national, which modestly amplifies effective extraction through verification difficulty — whether a reform truly commands sustained majority support is contestable at national scale, and that contestability is itself defended by the threshold's beneficiaries. No directionality overrides are authored: the beneficiary/victim declarations plus exit options already produce the intended structural relationships, and the dual-positioned legislator seat (beneficiary with agenda-setting secondary role) is captured by its declarations rather than by an override.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — securing small-unit assent to a durable union by guaranteeing a veto over future revision — is authored as contested rather than dead: the anti-factional and stability concerns it served have modern analogues, but the protection now operates against reforms with demonstrated, sustained majority support, a situation the founding generation did not confront. Classifying as a snare rather than a piton matters here: a piton reading would require the function to have atrophied with no concentrated beneficiary, whereas the veto remains an actively traded asset with identifiable collectors, and fixing remains prohibitively self-referential — the body that could lower the threshold is the body the threshold protects. The mandatrophy lens guards the inverse error as well: reading the contested founding problem as proof of pure rent ignores the genuine stabilization function the sibling readings articulate; that dispute is routed to the kernel omegas rather than settled inside this file.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This story instantiates the minoritarian_veto_reading of the supermajority_threshold kernel; which reading correctly characterizes the threshold''s operative structure — minoritarian veto, consensus safeguard, or calibrated instrument?',
    'Cross-reading comparison on the shared referent: amendment success rates weighted by sustained majority support, demographic share of blocking coalitions over time, and reversibility-cost audits; convergence of independent analyses on one structural description.',
    'If the consensus_safeguard reading is structurally accurate, the victim set dissolves and effective extraction collapses toward coordination cost; if the adaptive_gradient reading is accurate, the arrangement is a mistuned transitional tool requiring recalibration rather than removal; this file''s snare claim stands only under the minoritarian structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Which reading of the supermajority kernel describes the constraint''s actual structure.').

omega_variable(
    sibling_structural_delta,
    'What would each sibling reading change structurally if adopted as the operative description of the same kernel?',
    'Authoring the two sibling stories and diffing beneficiary/victim sets, epsilon, and computed types against this file.',
    'consensus_safeguard_reading removes the victim set (blocked majorities are recast as insufficiently deep consensus) and drops epsilon toward the coordination floor; adaptive_gradient_reading retains partial victims (reforms blocked despite high consensus formation) but reframes the remedy as tuning, shifting the cost-to-fix assessment from prohibitive to cheap-in-principle.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_structural_delta, conceptual, 'Structural consequences of adopting sibling readings of the same kernel.').

omega_variable(
    blocking_coalition_population_share,
    'What share of the national population can, at each historical point, block a constitutional amendment through the least-populated unit coalition?',
    'Demographic-apportionment time series: compute the minimum population share controlling the required number of unit votes at each census anchor.',
    'Quantifies the minoritarian charge: if the blocking share has fallen well below a third of the population while majority-supported reforms continue to fail, the permanent-veto characterization is confirmed and epsilon rises; a large blocking share would soften the minoritarian framing and lower effective extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(blocking_coalition_population_share, empirical, 'Demographic size of the coalition capable of blocking amendments.').

omega_variable(
    counterfactual_majority_passage,
    'Would the reforms blocked by the threshold actually have been enacted under simple-majority amendment rules, or do they lack intra-majority agreement?',
    'Legislative preference mapping and longitudinal survey series on the blocked reform queue: estimate passage probability under majority rule for each repeatedly blocked measure.',
    'Epsilon is measured against achievable majority will; if many blocked reforms would have failed anyway, part of the measured extraction is attributable to majority disunity rather than the threshold, lowering effective extraction for the payer seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(counterfactual_majority_passage, empirical, 'Whether blocked reforms would have passed under majority rule.').

omega_variable(
    rupture_reset_possibility,
    'Is the entrenchment permanent, or do periodic constitutional moments — crisis-driven convention calls, mass-mobilized amendment waves — reset the lock?',
    'Historical-comparative analysis of amendment-cluster episodes following wars, depressions, and legitimacy crises across federations with supermajority rules.',
    'If ruptures reliably reset the lock, the arrangement behaves as a punctuated transitional structure rather than a permanent one, lowering long-run extraction estimates; if no reset mechanism operates in practice, the permanent-veto reading is strengthened.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(rupture_reset_possibility, empirical, 'Whether crisis-driven constitutional moments periodically break the entrenchment.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(supermajority_threshold__minoritarian_veto_reading, 0, 65).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(smvt_mvr_tr_t0, supermajority_threshold__minoritarian_veto_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement_basis(smvt_mvr_tr_t0, observed).
narrative_ontology:measurement(smvt_mvr_tr_t13, supermajority_threshold__minoritarian_veto_reading, theater_ratio, 13, 0.25).
narrative_ontology:measurement_basis(smvt_mvr_tr_t13, observed).
narrative_ontology:measurement(smvt_mvr_tr_t26, supermajority_threshold__minoritarian_veto_reading, theater_ratio, 26, 0.28).
narrative_ontology:measurement_basis(smvt_mvr_tr_t26, observed).
narrative_ontology:measurement(smvt_mvr_tr_t39, supermajority_threshold__minoritarian_veto_reading, theater_ratio, 39, 0.31).
narrative_ontology:measurement_basis(smvt_mvr_tr_t39, observed).
narrative_ontology:measurement(smvt_mvr_tr_t52, supermajority_threshold__minoritarian_veto_reading, theater_ratio, 52, 0.33).
narrative_ontology:measurement_basis(smvt_mvr_tr_t52, observed).
narrative_ontology:measurement(smvt_mvr_tr_t65, supermajority_threshold__minoritarian_veto_reading, theater_ratio, 65, 0.35).
narrative_ontology:measurement_basis(smvt_mvr_tr_t65, observed).

% Extraction over time
narrative_ontology:measurement(smvt_mvr_be_t0, supermajority_threshold__minoritarian_veto_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(smvt_mvr_be_t0, observed).
narrative_ontology:measurement(smvt_mvr_be_t13, supermajority_threshold__minoritarian_veto_reading, base_extractiveness, 13, 0.55).
narrative_ontology:measurement_basis(smvt_mvr_be_t13, observed).
narrative_ontology:measurement(smvt_mvr_be_t26, supermajority_threshold__minoritarian_veto_reading, base_extractiveness, 26, 0.62).
narrative_ontology:measurement_basis(smvt_mvr_be_t26, observed).
narrative_ontology:measurement(smvt_mvr_be_t39, supermajority_threshold__minoritarian_veto_reading, base_extractiveness, 39, 0.68).
narrative_ontology:measurement_basis(smvt_mvr_be_t39, observed).
narrative_ontology:measurement(smvt_mvr_be_t52, supermajority_threshold__minoritarian_veto_reading, base_extractiveness, 52, 0.73).
narrative_ontology:measurement_basis(smvt_mvr_be_t52, observed).
narrative_ontology:measurement(smvt_mvr_be_t65, supermajority_threshold__minoritarian_veto_reading, base_extractiveness, 65, 0.76).
narrative_ontology:measurement_basis(smvt_mvr_be_t65, observed).

% Suppression requirement over time
narrative_ontology:measurement(smvt_mvr_su_t0, supermajority_threshold__minoritarian_veto_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement_basis(smvt_mvr_su_t0, observed).
narrative_ontology:measurement(smvt_mvr_su_t13, supermajority_threshold__minoritarian_veto_reading, suppression_requirement, 13, 0.5).
narrative_ontology:measurement_basis(smvt_mvr_su_t13, observed).
narrative_ontology:measurement(smvt_mvr_su_t26, supermajority_threshold__minoritarian_veto_reading, suppression_requirement, 26, 0.56).
narrative_ontology:measurement_basis(smvt_mvr_su_t26, observed).
narrative_ontology:measurement(smvt_mvr_su_t39, supermajority_threshold__minoritarian_veto_reading, suppression_requirement, 39, 0.61).
narrative_ontology:measurement_basis(smvt_mvr_su_t39, observed).
narrative_ontology:measurement(smvt_mvr_su_t52, supermajority_threshold__minoritarian_veto_reading, suppression_requirement, 52, 0.66).
narrative_ontology:measurement_basis(smvt_mvr_su_t52, observed).
narrative_ontology:measurement(smvt_mvr_su_t65, supermajority_threshold__minoritarian_veto_reading, suppression_requirement, 65, 0.7).
narrative_ontology:measurement_basis(smvt_mvr_su_t65, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(supermajority_threshold__minoritarian_veto_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(supermajority_threshold__minoritarian_veto_reading, supermajority_threshold__consensus_safeguard_reading).
narrative_ontology:affects_constraint(supermajority_threshold__minoritarian_veto_reading, supermajority_threshold__adaptive_gradient_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'supermajority requirement' conflates three structurally distinct claims about one textual kernel (the constitutional amendment clause). Per the epsilon-invariance principle this family decomposes: consensus_safeguard_reading (upstream, traditional legitimating account, negligible extraction, no victim set), adaptive_gradient_reading (mediating, instrumental account, extraction indexed to calibration evidence), and this file (downstream contestation, high extraction, full victim set). Each story carries its own beneficiaries, victims, and claimed type; the upstream account is routinely cited as evidence by the downstream accounts, hence the edges. The decomposition hazard: amendment-success-rate observables belong naturally to the upstream and mediating members, while blocked-reform-backlog observables weighted by sustained majority support belong to this one — assigning an observable to the wrong family member produces an apparent epsilon instability that is actually a category error.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
