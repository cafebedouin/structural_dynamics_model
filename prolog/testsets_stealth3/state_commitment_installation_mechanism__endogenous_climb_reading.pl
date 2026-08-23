% ============================================================================
% CONSTRAINT STORY: state_commitment_installation_mechanism__endogenous_climb_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_state_commitment_installation_mechanism__endogenous_climb_reading, []).

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
 *   constraint_id: state_commitment_installation_mechanism__endogenous_climb_reading
 *   human_readable: Endogenous Climb Mechanism for Commitment Legitimacy
 *   domain: historical_sociology/state_formation/cultural_authority
 *
 * SUMMARY:
 *   In state formation and cultural authority, new normative commitments —
 *   religious reforms, legal doctrines, scientific paradigms, administrative
 *   techniques — rarely win by seizing the apex first. This story
 *   instantiates the endogenous_climb_reading of the
 *   state_commitment_installation_mechanism kernel: legitimacy accumulates at
 *   institutional fringes through demonstrations of superiority, climbs along
 *   adoption curves carried by advocacy networks, is certified by mid-level
 *   validators, and is ratified by apex authorities only after the outcome is
 *   no longer contestable. Per the committer-frame rules, this file generates
 *   this one reading alone as a clean epsilon-invariant constraint; the
 *   exogenous-imposition and hybrid-cascade readings are separate stories
 *   linked through the network, each authoring its own epsilon over its own
 *   arrangement. Claim/metric independence holds: claimed_type tangled_rope
 *   states this reading's own structural assessment — a genuine coordination
 *   function (correction without revolution) fused with asymmetric extraction
 *   (proof burden on the challenger class, gatekeeper rents collected from
 *   every verdict, apex rent erosion) — while the metrics are authored
 *   independently as descriptive best estimates of the arrangement's actual
 *   operation.
 *
 * KEY AGENTS:
 *   - institutional_fringe_reformers: primary beneficiary with dual payer exposure (moderate/identity_locked) — mount demonstrations from the margins
 *   - grassroots_advocacy_networks: beneficiary (organized/constrained) — carry the adoption curve between venues
 *   - credibility_gatekeepers: agenda_setter and receipt-of-gain seat (institutional/mobile) — certify what counts as demonstrated superiority
 *   - incumbent_apex_elites: primary payer (powerful/trapped) — resist erosion of gatekeeping rents
 *   - failed_demonstrators: payer (powerless/identity_locked) — bear unrewarded proof costs
 *   - late_adopting_public: beneficiary with secondary payer exposure (moderate/constrained)
 *   - comparative_historical_sociologists: analytical observer (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_commitment_installation_mechanism__endogenous_climb_reading, 0.42).
domain_priors:suppression_score(state_commitment_installation_mechanism__endogenous_climb_reading, 0.37).
domain_priors:theater_ratio(state_commitment_installation_mechanism__endogenous_climb_reading, 0.36).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__endogenous_climb_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__endogenous_climb_reading, suppression_requirement, 0.37).
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__endogenous_climb_reading, theater_ratio, 0.36).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__endogenous_climb_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__endogenous_climb_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_commitment_installation_mechanism__endogenous_climb_reading, tangled_rope).
narrative_ontology:human_readable(state_commitment_installation_mechanism__endogenous_climb_reading, "Endogenous Climb Mechanism for Commitment Legitimacy").
narrative_ontology:topic_domain(state_commitment_installation_mechanism__endogenous_climb_reading, "historical_sociology/state_formation/cultural_authority").

domain_priors:requires_active_enforcement(state_commitment_installation_mechanism__endogenous_climb_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_commitment_installation_mechanism__endogenous_climb_reading, 'ee182401-d3fb-4c42-b19c-a74648cecaf3').
narrative_ontology:cs_kernel_codification('ee182401-d3fb-4c42-b19c-a74648cecaf3', distributed).
narrative_ontology:cs_authority_grounding('ee182401-d3fb-4c42-b19c-a74648cecaf3', practice).
narrative_ontology:cs_interpretation_layer_present('ee182401-d3fb-4c42-b19c-a74648cecaf3').
narrative_ontology:cs_reading_relation('ee182401-d3fb-4c42-b19c-a74648cecaf3', state_commitment_installation_mechanism__exogenous_imposition_reading, forecloses).
narrative_ontology:cs_reading_relation('ee182401-d3fb-4c42-b19c-a74648cecaf3', state_commitment_installation_mechanism__hybrid_cascade_reading, forecloses).
narrative_ontology:cs_axiom('ee182401-d3fb-4c42-b19c-a74648cecaf3', foundational, legitimacy_accrues_through_demonstrated_superiority).
narrative_ontology:cs_axiom_status(legitimacy_accrues_through_demonstrated_superiority, holdable).
narrative_ontology:cs_axiom_grounding('ee182401-d3fb-4c42-b19c-a74648cecaf3', legitimacy_accrues_through_demonstrated_superiority, empirically_contingent).
narrative_ontology:cs_axiom('ee182401-d3fb-4c42-b19c-a74648cecaf3', secondary, apex_ratification_follows_rather_than_constitutes_legitimacy).
narrative_ontology:cs_axiom_status(apex_ratification_follows_rather_than_constitutes_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('ee182401-d3fb-4c42-b19c-a74648cecaf3', apex_ratification_follows_rather_than_constitutes_legitimacy, conventional).
narrative_ontology:cs_reference_frame('ee182401-d3fb-4c42-b19c-a74648cecaf3', fringe_demonstration_precedence).
narrative_ontology:cs_drift_state('ee182401-d3fb-4c42-b19c-a74648cecaf3', contemporary_credentialed_validation_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('ee182401-d3fb-4c42-b19c-a74648cecaf3', '').
narrative_ontology:cs_kernel_id(state_commitment_installation_mechanism__endogenous_climb_reading, state_commitment_installation_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_commitment_installation_mechanism__endogenous_climb_reading, institutional_fringe_reformers).
narrative_ontology:constraint_beneficiary(state_commitment_installation_mechanism__endogenous_climb_reading, grassroots_advocacy_networks).
narrative_ontology:constraint_beneficiary(state_commitment_installation_mechanism__endogenous_climb_reading, credibility_gatekeepers).
narrative_ontology:constraint_beneficiary(state_commitment_installation_mechanism__endogenous_climb_reading, late_adopting_public).
narrative_ontology:constraint_victim(state_commitment_installation_mechanism__endogenous_climb_reading, incumbent_apex_elites).
narrative_ontology:constraint_victim(state_commitment_installation_mechanism__endogenous_climb_reading, failed_demonstrators).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(state_commitment_installation_mechanism__endogenous_climb_reading, institutional_fringe_reformers).
narrative_ontology:constraint_victim(state_commitment_installation_mechanism__endogenous_climb_reading, late_adopting_public).
narrative_ontology:constraint_vindicates(state_commitment_installation_mechanism__endogenous_climb_reading, diffusion_of_innovations_theory).
narrative_ontology:constraint_vindicates(state_commitment_installation_mechanism__endogenous_climb_reading, meritocratic_progress_narrative).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Advocates positioned at institutional margins — junior faculty, provincial professionals, dissenting sub-disciplines — who advance a rival commitment by building demonstration cases of its superiority. When their commitment climbs, they inherit offices, standing, and agenda influence. Exit means abandoning the cause that constitutes their professional identity, so they remain mounted on demonstrations for years regardless of prospects.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__endogenous_climb_reading, institutional_fringe_reformers, beneficiary,
    moderate, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(state_commitment_installation_mechanism__endogenous_climb_reading, institutional_fringe_reformers, payer).

% Voluntary associations, pamphleteering circles, study groups, and itinerant lecturers who carry the commitment between venues and supply the adoption curve's early mass. They gain members, purpose, and donated resources as momentum builds; disbanding forfeits accumulated networks that have no value under any other commitment.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__endogenous_climb_reading, grassroots_advocacy_networks, beneficiary,
    organized, biographical, constrained, national).

% Editors, professional associations, credentialing bodies, and court-adjacent intelligentsia who decide what counts as a demonstrated superiority. They convert validation authority into careers, appointments, and agenda control, collecting from every verdict regardless of which commitment wins, and they serve whichever challengers clear their bar.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__endogenous_climb_reading, credibility_gatekeepers, agenda_setter,
    institutional, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(state_commitment_installation_mechanism__endogenous_climb_reading, credibility_gatekeepers, beneficiary).

% Holders of apex offices whose authority rests on the existing commitment structure. Each climbed commitment erodes their gatekeeping rents and forces costly adaptation of doctrine and personnel. They resist through delay, ridicule, and selective recognition, but they cannot exit their own positions without abdicating the authority being contested.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__endogenous_climb_reading, incumbent_apex_elites, payer,
    powerful, generational, trapped, national).

% Challengers whose commitments never win. They bear the full cost of demonstration — years of unrewarded proof work, spent reputations, forgone careers — and their sunk effort stabilizes the very testing apparatus that rejected them. Identity fused with a lost cause makes exit psychologically prohibitive even after defeat is plain.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__endogenous_climb_reading, failed_demonstrators, payer,
    powerless, biographical, identity_locked, national).

% The broad population that receives validated improvements only after the adoption curve matures, absorbing transition costs along the way — instability, whiplash between standards, institutions half-reformed. They adopt whatever wins and have little leverage over which demonstrations get mounted or certified.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__endogenous_climb_reading, late_adopting_public, beneficiary,
    moderate, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(state_commitment_installation_mechanism__endogenous_climb_reading, late_adopting_public, payer).

% Scholars who reconstruct adoption sequences across episodes — religious reforms, legal doctrines, scientific paradigms, administrative techniques — comparing fringe-emergence dates against apex-ratification dates. They hold no stake in which commitments win, and their archives are the principal outside check on the mechanism's self-description.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__endogenous_climb_reading, comparative_historical_sociologists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(state_commitment_installation_mechanism__endogenous_climb_reading, credibility_gatekeepers).
narrative_ontology:fixing_cost_class(state_commitment_installation_mechanism__endogenous_climb_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared, low-violence protocol for revising collective commitments: challengers demonstrate superiority at the margins, advocacy networks carry adoption curves, mid-level validators certify results, and apex authorities ratify after the outcome is no longer contestable. It solves the problem of how a polity changes its mind without civil war or pure fiat.
% TRANSFER_FUNCTION: Moves legitimacy, and eventually offices and resources, from incumbent apex holders to demonstrated fringe challengers; moves the proof burden and risk of testing onto the challenger class as a whole; moves validation deference and its attendant standing to the gatekeeper intermediaries who operate the certification apparatus.
% ABSENT_VOICES: Failed demonstrators and displaced practice-communities would object that the examination was rigged or the fee unjust, but they sit outside the validation venues and their testimony is discounted until — and unless — a later victory retroactively redeems only the winners. Apex voices are heard during the climb but read as vested interest rather than argument, so their substantive objections enter the record pre-discredited.
% DISAPPEARANCE_RATIONALE: If the climb mechanism vanished overnight, commitment revision would revert to apex seizure or authoritative imposition; the credibility economy of validators, advocacy networks, and demonstration venues would collapse; reform latency would stretch from decades toward generations, and every correction would carry rupture-risk.
% FOUNDING_PROBLEM: How can a state or culture adopt better commitments when the apex controls legitimacy, has no incentive to admit error, and cannot be trusted to evaluate challenges to its own position?
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties by comparative-historical scholarship — diffusion and adoption-curve studies documenting fringe-emergence preceding apex ratification across religious, legal, and scientific episodes — conducted by researchers with no stake in any current reform movement. Apex elites do not attest the founding problem; they dispute the mechanism's very existence, which is itself signal that the problem they pose for others remains unsolved by any apex-controlled alternative.
narrative_ontology:disappearance_verdict(state_commitment_installation_mechanism__endogenous_climb_reading, world_rearranges).
narrative_ontology:founding_problem_status(state_commitment_installation_mechanism__endogenous_climb_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(state_commitment_installation_mechanism__endogenous_climb_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(state_commitment_installation_mechanism__endogenous_climb_reading, 'none', 1).
narrative_ontology:epsilon_provenance(state_commitment_installation_mechanism__endogenous_climb_reading, 0.42, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(state_commitment_installation_mechanism__endogenous_climb_reading_tests).
:- end_tests(state_commitment_installation_mechanism__endogenous_climb_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is 0.42 at interval end: the coordination function is real — a polity-wide update protocol far cheaper than rupture — but the arrangement charges the challenger class for the testing service. Winners repay themselves from the spoils of office; losers subsidize the apparatus with sunk proof work; gatekeepers collect standing from every verdict. Suppression is 0.37: no coercion of belief, but the demonstration requirement de-legitimizes alternative routes to legitimacy (tradition, appointment, force), and the credential gates enforcing that requirement hardened over the interval — hence the rising suppression_requirement series modeling enforcement maturation, not decay. Theater is 0.36 and rising: as validation professionalized, demonstration became a producible artifact — curated cases, narrative management, validator cultivation — classic Goodhart drift visible in the series. Accessibility_collapse is 0.30 because alternative legitimacy routes persist (imposition episodes continue alongside climbs), so understanding the mechanism does not close the option space. Resistance is 0.58: apex resistance is this reading's signature — delay, ridicule, selective recognition — compounded by disillusioned failed challengers. All three tracked metrics share one six-point grid; trajectories are monotonic with no oscillation, so no intermittent-reinforcement reading applies.
 *
 * PERSPECTIVAL GAP:
 *   From the winning reformer's seat the mechanism computes as a rope — the fair price of having a voice at all. From the apex seat it computes as expropriation of gatekeeping property by attrition. From the failed challenger's seat it computes as a lottery billed as an examination. The engine derives these per-seat classifications from the power, exit, and role data; the authored claim does not adjudicate among them, and the divergence between the beneficiary seats' experience and the payer seats' experience is the measurement this story exists to take.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary declarations (fringe reformers, advocacy networks, gatekeepers, late-adopting public) drive low directionality for those seats; the victim declarations (apex elites, failed demonstrators) combined with trapped and identity_locked exits drive those seats toward the full-target end. The gatekeepers sit lowest despite administering the arrangement — they collect from every outcome, which is why gain_flow names that seat rather than diffuse. The late-adopting public sits near symmetric: genuine benefit from validated improvements against diffuse transition costs. No directionality overrides were needed: role plus exit data already separates the seats, and the dual-positioned agents (reformers, public, gatekeepers) carry secondary_role rather than overrides.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards in both directions. Against mislabeling as pure rope: the extraction is not incidental overhead — the proof-burden asymmetry and the gatekeepers' verdict-independent collection are load-bearing features of the arrangement, which is why the claim is tangled_rope rather than rope. Against mislabeling as snare: the coordination function is genuine, alternatives are devalued rather than coercively suppressed, and the arrangement does not depend on victimizing a fixed class — today's failed challenger's commitment may climb tomorrow, so the victim set is rotating, not captive. The founding problem (correction without revolution) remains live, so no mandatrophy resolution is declared; the R5 mismatch consumer should find status=live paired with verdict=world_rearranges, a coherent pairing with no zombie flag expected.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position_endogenous_climb,
    'This constraint is the endogenous_climb_reading of the state_commitment_installation_mechanism kernel; what structural facts would differ if the exogenous_imposition_reading or hybrid_cascade_reading were instantiated instead?',
    'Compare the three sibling stories'' beneficiary/victim declarations and adoption-sequence claims. The disagreement is located in the direction of legitimacy flow: bottom-up demonstration (this reading) versus top-down installation (exogenous) versus install-then-validate (hybrid).',
    'Under the exogenous reading, beneficiaries relocate to apex authorities and victims to suppressed local practices; under the hybrid reading, fringe validation demotes from legitimacy source to stabilizing after-check. Each sibling authors its own epsilon over its own arrangement; resolving the contest re-partitions the whole family''s structural data.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position_endogenous_climb, conceptual, 'Committer-frame omega: this story is one reading of a three-reading kernel; sibling readings would invert the beneficiary/victim structure and the legitimacy-flow direction.').

omega_variable(
    manufactured_grassroots_momentum,
    'Is observed fringe-to-apex climbing genuinely endogenous, or is apparent grassroots momentum manufactured by resourced sponsors staging demonstrations of superiority?',
    'Funding-trail and network analysis of the advocacy infrastructure behind documented adoption episodes; compare organic versus sponsored diffusion signatures across matched cases.',
    'If sponsorship dominates, the mechanism operates as the exogenous reading in endogenous costume; measured epsilon understates extraction, and the beneficiary declaration shifts toward sponsor interests rather than fringe actors.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(manufactured_grassroots_momentum, empirical, 'Whether the climb''s bottom-up appearance reflects actual bottom-up causation.').

omega_variable(
    demonstration_burden_resource_filter,
    'Does the cost of mounting a credible demonstration filter which commitments ever get tested, so that superior but poorly resourced commitments never enter the climb at all?',
    'Compare the resource profiles of attempted versus successful demonstrations across episodes; reconstruct the counterfactual set of commitments that never reached the testing stage.',
    'If filtering is strong, the mechanism doubly penalizes poor challengers — their superior commitments die untested — and effective extraction on the challenger class exceeds what the scalar epsilon registers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(demonstration_burden_resource_filter, empirical, 'Whether access to the climb is resource-gated ahead of any merit test.').

omega_variable(
    superiority_signal_goodhart,
    'Does ''demonstrated superiority'' track the intrinsic merit of the commitment, or skill at performing demonstration — curated cases, narrative control, validator cultivation?',
    'Retrospective audit of climbed commitments: did validated superiority survive later scrutiny, and did demonstrator skill predict success beyond content quality after controlling for it?',
    'If performance skill dominates, theater_ratio is understated and the mechanism selects for marketing rather than correction — the arrangement drifts toward extraction sustained by credential theater, and the vindicated meritocratic-progress proposition weakens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(superiority_signal_goodhart, conceptual, 'Whether the certification signal measures merit or performative capacity.').

omega_variable(
    survivorship_bias_in_adoption_curves,
    'Do adoption-curve data overstate the mechanism''s reliability because only climbed commitments are recorded, leaving the population of superior-but-unclimbed commitments unobserved?',
    'Reconstruct failed-contender archives — rejected manuscripts, ignored petitions, defunct movements — and test whether their assessed-quality distribution overlaps the winners''.',
    'If overlap is high, the climb is substantially chancy rather than merit-tracking, weakening the vindicated propositions and shifting the extraction accounting further toward uncompensated challenger burden.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(survivorship_bias_in_adoption_curves, empirical, 'Survivorship bias in the evidentiary base for demonstrated-superiority claims.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_commitment_installation_mechanism__endogenous_climb_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(scim_endo_climb_tr_t0, state_commitment_installation_mechanism__endogenous_climb_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(scim_endo_climb_tr_t10, state_commitment_installation_mechanism__endogenous_climb_reading, theater_ratio, 10, 0.19).
narrative_ontology:measurement(scim_endo_climb_tr_t20, state_commitment_installation_mechanism__endogenous_climb_reading, theater_ratio, 20, 0.24).
narrative_ontology:measurement(scim_endo_climb_tr_t30, state_commitment_installation_mechanism__endogenous_climb_reading, theater_ratio, 30, 0.29).
narrative_ontology:measurement(scim_endo_climb_tr_t40, state_commitment_installation_mechanism__endogenous_climb_reading, theater_ratio, 40, 0.33).
narrative_ontology:measurement(scim_endo_climb_tr_t50, state_commitment_installation_mechanism__endogenous_climb_reading, theater_ratio, 50, 0.36).

% Extraction over time
narrative_ontology:measurement(scim_endo_climb_be_t0, state_commitment_installation_mechanism__endogenous_climb_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(scim_endo_climb_be_t10, state_commitment_installation_mechanism__endogenous_climb_reading, base_extractiveness, 10, 0.28).
narrative_ontology:measurement(scim_endo_climb_be_t20, state_commitment_installation_mechanism__endogenous_climb_reading, base_extractiveness, 20, 0.33).
narrative_ontology:measurement(scim_endo_climb_be_t30, state_commitment_installation_mechanism__endogenous_climb_reading, base_extractiveness, 30, 0.37).
narrative_ontology:measurement(scim_endo_climb_be_t40, state_commitment_installation_mechanism__endogenous_climb_reading, base_extractiveness, 40, 0.4).
narrative_ontology:measurement(scim_endo_climb_be_t50, state_commitment_installation_mechanism__endogenous_climb_reading, base_extractiveness, 50, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(scim_endo_climb_su_t0, state_commitment_installation_mechanism__endogenous_climb_reading, suppression_requirement, 0, 0.18).
narrative_ontology:measurement(scim_endo_climb_su_t10, state_commitment_installation_mechanism__endogenous_climb_reading, suppression_requirement, 10, 0.22).
narrative_ontology:measurement(scim_endo_climb_su_t20, state_commitment_installation_mechanism__endogenous_climb_reading, suppression_requirement, 20, 0.27).
narrative_ontology:measurement(scim_endo_climb_su_t30, state_commitment_installation_mechanism__endogenous_climb_reading, suppression_requirement, 30, 0.31).
narrative_ontology:measurement(scim_endo_climb_su_t40, state_commitment_installation_mechanism__endogenous_climb_reading, suppression_requirement, 40, 0.34).
narrative_ontology:measurement(scim_endo_climb_su_t50, state_commitment_installation_mechanism__endogenous_climb_reading, suppression_requirement, 50, 0.37).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(state_commitment_installation_mechanism__endogenous_climb_reading, resource_allocation).
narrative_ontology:affects_constraint(state_commitment_installation_mechanism__endogenous_climb_reading, state_commitment_installation_mechanism__exogenous_imposition_reading).
narrative_ontology:affects_constraint(state_commitment_installation_mechanism__endogenous_climb_reading, state_commitment_installation_mechanism__hybrid_cascade_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the natural-language concept 'how new commitments gain legitimacy in state formation' decomposes into three structurally distinct readings of one kernel, per the epsilon-invariance principle. This file authors the endogenous_climb_reading's epsilon (0.42) over the climb arrangement as this reading sees it; the exogenous_imposition_reading authors its own epsilon over the imposition arrangement (expected higher extraction, apex beneficiaries, suppressed-practice victims) and the hybrid_cascade_reading over the cascade arrangement. The exogenous reading is historically upstream — older imposition narratives supplied the frame the endogenous reading was formulated against — so family influence runs primarily from the exogenous sibling into this one, with this reading feeding back as the corrective that redefined whose demonstrations count.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
