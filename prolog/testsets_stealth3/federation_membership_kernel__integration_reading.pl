% ============================================================================
% CONSTRAINT STORY: federation_membership_kernel__integration_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-06
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_federation_membership_kernel__integration_reading, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: federation_membership_kernel__integration_reading
 *   human_readable: EU Free Movement as Constitutive Citizenship Right (Integration Reading)
 *   domain: political economy/federalism/migration policy/welfare state theory
 *
 * SUMMARY:
 *   The arrangement under contest is the EU free-movement regime as the
 *   integration reading holds it: movement, residence, and equal treatment as
 *   constitutive elements of Union citizenship, with scope fixed by
 *   supranational adjudication rather than national discretion. The regime
 *   solves a real coordination problem — allocating labor across twenty-seven
 *   national markets whose goods and capital already move freely — and it
 *   does so through active enforcement: Commission infringement proceedings
 *   against national restrictions, and a jurisprudence that has widened the
 *   worker concept, extended equal treatment to family members, and struck
 *   down residence-based benefit filters. The same structure displaces costs
 *   onto parties with no seat in its governance: entry-level local workers in
 *   receiving regions compete without exit options, receiving-state welfare
 *   contributors absorb uncompensated caseload extension, and sending-region
 *   communities lose working-age populations to a right exercised
 *   individually but borne collectively. This file instantiates the
 *   integration_reading of federation_membership_kernel; the
 *   member_sovereignty_reading and welfare_coordination_reading are separate
 *   constraints over the same treaty text, linked in
 *   network.affects_constraints. Claim/metric independence: the reading
 *   claims constitutive-right coordination; the authored metrics describe
 *   substantial asymmetric extraction; the divergence is the datum the engine
 *   measures.
 *
 * KEY AGENTS:
 *   - - labor_intensive_employers: Primary beneficiary (powerful/arbitrage) — recruits from a continental pool; captures the wage-moderation surplus
 *   - - mobile_eu_workers: Direct beneficiary (moderate/mobile) — captures wage differentials; retains the return option locals lack
 *   - - european_commission: Agenda-setter (institutional/arbitrage) — enforces compliance; portfolio and authority grow with each expansion
 *   - - ecj: Agenda-setter (institutional/arbitrage) — fixes scope by preliminary ruling; each judgment becomes precedent for the next
 *   - - displaced_local_workers: Primary target (powerless/trapped) — bears wage competition in gateway regions
 *   - - receiving_state_welfare_contributors: Target (moderate/trapped) — funds benefit extension without fiscal correction
 *   - - sending_state_peripheral_communities: Target (powerless/trapped) — bears externalized depopulation and skill depletion
 *   - - member_state_governments: Administrator and residual payer (institutional/constrained) — implements lines drawn elsewhere; exit proven ruinous
 *   - - trade_unions_in_receiving_states: Excluded voice (organized/trapped) — lost the posted-worker litigation; objects from venues without leverage
 *   - - migration_economists: Analytical observer (analytical/analytical) — measures wage, fiscal, and remittance effects that both camps cite selectively
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federation_membership_kernel__integration_reading, 0.68).
domain_priors:suppression_score(federation_membership_kernel__integration_reading, 0.72).
domain_priors:theater_ratio(federation_membership_kernel__integration_reading, 0.31).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federation_membership_kernel__integration_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(federation_membership_kernel__integration_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(federation_membership_kernel__integration_reading, theater_ratio, 0.31).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(federation_membership_kernel__integration_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(federation_membership_kernel__integration_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federation_membership_kernel__integration_reading, tangled_rope).
narrative_ontology:human_readable(federation_membership_kernel__integration_reading, "EU Free Movement as Constitutive Citizenship Right (Integration Reading)").
narrative_ontology:topic_domain(federation_membership_kernel__integration_reading, "political economy/federalism/migration policy/welfare state theory").

domain_priors:requires_active_enforcement(federation_membership_kernel__integration_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federation_membership_kernel__integration_reading, '4cb65391-36b0-48c2-8737-efb209323c21').
narrative_ontology:cs_kernel_codification('4cb65391-36b0-48c2-8737-efb209323c21', fixed_text).
narrative_ontology:cs_authority_grounding('4cb65391-36b0-48c2-8737-efb209323c21', lineage).
narrative_ontology:cs_interpretation_layer_present('4cb65391-36b0-48c2-8737-efb209323c21').
narrative_ontology:cs_reading_relation('4cb65391-36b0-48c2-8737-efb209323c21', federation_membership_kernel__member_sovereignty_reading, forecloses).
narrative_ontology:cs_reading_relation('4cb65391-36b0-48c2-8737-efb209323c21', federation_membership_kernel__welfare_coordination_reading, influences).
narrative_ontology:cs_axiom('4cb65391-36b0-48c2-8737-efb209323c21', foundational, free_movement_constitutive_of_union_citizenship).
narrative_ontology:cs_axiom_status(free_movement_constitutive_of_union_citizenship, holdable).
narrative_ontology:cs_axiom_grounding('4cb65391-36b0-48c2-8737-efb209323c21', free_movement_constitutive_of_union_citizenship, deontological).
narrative_ontology:cs_axiom('4cb65391-36b0-48c2-8737-efb209323c21', foundational, supranational_interpretive_finality).
narrative_ontology:cs_axiom_status(supranational_interpretive_finality, holdable).
narrative_ontology:cs_axiom_grounding('4cb65391-36b0-48c2-8737-efb209323c21', supranational_interpretive_finality, conventional).
narrative_ontology:cs_reference_frame('4cb65391-36b0-48c2-8737-efb209323c21', constitutive_citizenship_single_market).
narrative_ontology:cs_drift_state('4cb65391-36b0-48c2-8737-efb209323c21', contemporary_post_secession_enforcement_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('4cb65391-36b0-48c2-8737-efb209323c21', '').
narrative_ontology:cs_kernel_id(federation_membership_kernel__integration_reading, federation_membership_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federation_membership_kernel__integration_reading, mobile_eu_workers).
narrative_ontology:constraint_beneficiary(federation_membership_kernel__integration_reading, labor_intensive_employers).
narrative_ontology:constraint_beneficiary(federation_membership_kernel__integration_reading, eu_supranational_institutions).
narrative_ontology:constraint_victim(federation_membership_kernel__integration_reading, displaced_local_workers).
narrative_ontology:constraint_victim(federation_membership_kernel__integration_reading, receiving_state_welfare_contributors).
narrative_ontology:constraint_victim(federation_membership_kernel__integration_reading, sending_state_peripheral_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(federation_membership_kernel__integration_reading, member_state_governments).
narrative_ontology:constraint_vindicates(federation_membership_kernel__integration_reading, eu_citizenship_doctrine).
narrative_ontology:constraint_vindicates(federation_membership_kernel__integration_reading, four_freedom_market_completion).
narrative_ontology:constraint_vindicates(federation_membership_kernel__integration_reading, supremacy_and_direct_effect_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Monitors member-state compliance with movement and equal-treatment obligations, opens infringement proceedings against national restrictions, publishes mobility scorecards, and frames the arrangement as the personal core of Union citizenship. Its portfolio and enforcement remit grow with each expansion it defends; it answers to no national electorate.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__integration_reading, european_commission, agenda_setter,
    institutional, generational, arbitrage, continental).

% Adjudicates scope questions through preliminary rulings. Successive judgments have widened who counts as a worker, extended equal treatment to family members and former workers, and struck down national residence-based benefit filters. Each ruling becomes precedent the next case builds on; no national parliament can reverse one short of unanimous treaty amendment.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__integration_reading, ecj, agenda_setter,
    institutional, generational, arbitrage, continental).

% Move to where wages and opportunities are higher, gaining income differentials often several multiples of home-country pay. They face credential-recognition hurdles, language barriers, and in posting arrangements sometimes precarious terms; they can return home if conditions sour, which is precisely the option their local competitors lack.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__integration_reading, mobile_eu_workers, beneficiary,
    moderate, biographical, mobile, continental).

% Staff agriculture, construction, care, logistics, and food processing from a continental labor pool. Wage bills sit below what closed national labor markets would support. They can relocate production or shift recruitment corridors if any single country tightens rules, giving them the widest option set of any seat in the arrangement.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__integration_reading, labor_intensive_employers, beneficiary,
    powerful, biographical, arbitrage, global).

% Compete for entry-level and manual work in gateway regions against incoming workers accepting lower reservation wages, concentrated in hospitality, warehousing, and seasonal agriculture. Moving away means leaving family networks, housing tenancies, and local knowledge they cannot easily replace.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__integration_reading, displaced_local_workers, payer,
    powerless, immediate, trapped, local).

% Fund social insurance and public services through taxes and contributions. Court rulings extend certain benefits to newly arrived and formerly contributing migrants on equal terms, and no fiscal-correction payment arrives sized to the added caseload. Their lever is national politics, which treaty supremacy overrules on scope questions.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__integration_reading, receiving_state_welfare_contributors, payer,
    moderate, biographical, trapped, national).

% Watch working-age adults depart for western wages. Remittances arrive, but the local tax base, school cohorts, and care economy thin year over year; villages in eastern Poland, Romania, and Bulgaria age in place. The departure decisions were made by individuals exercising their own right, but the community bears the residue.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__integration_reading, sending_state_peripheral_communities, payer,
    powerless, generational, trapped, regional).

% Administer registration, benefit eligibility, and labor-market policy inside lines drawn elsewhere. Attempts to filter access draw infringement proceedings. Net fiscal position varies sharply by corridor — some states gain a safety valve for unemployment, others absorb service loads. Leaving the arrangement entirely was attempted once and proved ruinously expensive.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__integration_reading, member_state_governments, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(federation_membership_kernel__integration_reading, member_state_governments, payer).

% Represent the competing local workforce. They litigated against posted-worker pay regimes and lost at the supranational court. Their objections now surface in national politics and parliamentary elections, venues where they hold little leverage over the treaty architecture they contest.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__integration_reading, trade_unions_in_receiving_states, excluded,
    organized, biographical, trapped, national).

% Measure wage effects of inflows (aggregate estimates small, distributional effects concentrated), fiscal incidence of benefit extension, and remittance flows. They publish findings that both camps cite selectively and hold no stake in outcomes.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__integration_reading, migration_economists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(federation_membership_kernel__integration_reading, labor_intensive_employers).
narrative_ontology:fixing_cost_class(federation_membership_kernel__integration_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allocates labor across twenty-seven national markets whose goods and capital already move freely: removes border frictions, lets firms match skills to demand continent-wide, and gives shrinking regions an emigration valve and growing regions an immigration supply without negotiating each flow bilaterally.
% TRANSFER_FUNCTION: Moves labor and dependent family members from lower-wage to higher-wage regions; moves welfare obligations onto receiving-state social insurance without compensating fiscal transfers; moves scope-setting authority over membership conditions from national parliaments to a supranational court.
% ABSENT_VOICES: Trade unions and low-wage local workers in receiving regions held no seat when the scope-setting jurisprudence consolidated — the decisive rulings ran through judicial chambers and the commission legal service, not legislatures. Sending-region communities were represented only by governments that traded mobility rights for accession advantages; the individuals who actually left never ratified that bargain, and the communities that aged in place had no voice at all.
% DISAPPEARANCE_RATIONALE: Millions of settled cross-border lives, whole sectoral labor supplies (agriculture, care, construction, logistics), the labor dimension of the single market, and the citizenship status itself presuppose the arrangement. Overnight removal would strand residence rights mid-stream, break sectoral staffing within a season, and force twenty-seven bilateral labor regimes into existence under emergency conditions.
% FOUNDING_PROBLEM: Post-1945 Western Europe needed to make interstate war materially impossible and rebuild faster than national labor pools allowed; the founders designed worker mobility as the interdependence mechanism binding states together and completing a common market alongside free capital movement.
% FOUNDING_PROBLEM_CORROBORATION: Integration historians working from national archives and the Spaak report corroborate the founding bargain from outside the benefiting parties. On present status the attestation splits along the same line as the kernel contest: the Commission cites unfinished market completion and renewed geopolitical stakes, while receiving-state treasuries and national audit offices attest that the original problem is solved and current expansion serves institutional maintenance. No arbiter outside the dispute exists — stated plainly.
narrative_ontology:disappearance_verdict(federation_membership_kernel__integration_reading, world_rearranges).
narrative_ontology:founding_problem_status(federation_membership_kernel__integration_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(federation_membership_kernel__integration_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(federation_membership_kernel__integration_reading, 'none', 1).
narrative_ontology:epsilon_provenance(federation_membership_kernel__integration_reading, 0.68, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(federation_membership_kernel__integration_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(federation_membership_kernel__integration_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(federation_membership_kernel__integration_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored at 0.68: three distinct victim channels (localized wage competition, uncompensated welfare extension, externalized brain drain) ride on a coordination function that is genuinely real, which is why the value sits well below pure-extraction territory despite the breadth of the target set. Suppression at 0.72 reflects the enforcement architecture rather than participant preference: treaty supremacy forecloses national restriction, infringement proceedings punish attempts, and the one full-exit test case confirmed the cost of leaving. Theater at 0.31: mobility itself is functional and large, but a growing share of activity is discursive — citizenship anniversaries, mobility scorecards — decoupled from movement that concentrates in a few corridors. Accessibility_collapse 0.55: national-protection alternatives are legally foreclosed once supremacy binds, but the completed secession kept full exit imaginable, and individuals always retain the do-not-move option. Resistance 0.62: sustained and recurring — opt-outs, renegotiation demands, posted-worker revolts, one finished departure — yet uniformly unsuccessful against the treaty architecture. All three tracked metrics share one time grid (seven points across the interval); the suppression_requirement series is authored because this story specifically traces enforcement intensification — the infringement and posted-worker machinery matured over the interval — not merely extraction drift. Receipt: the wage-moderation surplus is the largest captured flow and lands with the employers recruiting from the pool; welfare-extension draws accrue diffusely to migrant households and authority rents to the enforcing institutions, neither dominating the monetary flow — hence gain_flow names the employer seat rather than diffuse. Fixing cost is prohibitive: treaty change requires the unanimity every government holds, the fiscal-correction fix requires the same unanimity against concentrated beneficiary opposition, and the single full-exit demonstration cost a decade of dislocation.
 *
 * PERSPECTIVAL GAP:
 *   Four seats inhabit four different constraints under one treaty text. From the Court's and Commission's seat the arrangement is constitution-building: each ruling completes the market and deepens citizenship, and the seat's own authority compounds with each success. From the employer seat it is input-market design: an elastic labor supply that disciplines wages without any single hiring decision doing so. From the displaced-local seat it is wage competition arrived by legal instrument, with exit priced beyond reach. From the sending-periphery seat it is demographic subtraction — a right exercised by the young and borne by the old. The engine computes these divergences from the structural data; nothing in the authored claim adjudicates among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive the low-directionality seats: mobile workers (mobile exit) and employers (arbitrage exit) sit nearest the subsidized end, with the employers' wider option set placing them lowest. Victim declarations drive the high-directionality seats: trapped exit pushes displaced locals, welfare contributors, and sending peripheries toward the full-target end. The Commission and Court collect authority rather than money; their beneficiary listing captures the competence rent, and their arbitrage exit (reframing, prioritizing enforcement) keeps them off the target end. One override is authored: organized to 0.78. The sole organized seat, trade_unions_in_receiving_states, holds role excluded and therefore derives no placement from the beneficiary/victim arrays, yet its members bear near-target exposure through the lost posting litigation and the wage competition facing the workforces they represent; keying the override on the power atom is safe because it is this story's only organized seat. Suppression is authored as a raw structural property and left unscaled; only extractiveness rides directionality and scope in the engine's computation.
 *
 * MANDATROPHY ANALYSIS:
 *   Mislabeling risk runs both ways. Reading the arrangement as pure extraction erases the real coordination achievement — a continental labor market that actually clears, remittance lifelines, sectoral staffing that national pools could not supply — and would license destructive remedies. Accepting the integration reading's own framing at face value launders the three victim channels as constitutional necessity. The hybrid structure keeps both errors visible: coordination function and asymmetric extraction are authored as independent facts requiring active enforcement to hold together. On genealogy: the founding problem (post-war interdependence, market completion) is partially transformed — the interdependence logic regained geopolitical salience while the market-completion phase is substantially delivered — hence founding_problem_status is authored contested rather than dead, which keeps the dead-plus-rearrangement capture flag from firing on a dispute that is genuinely unresolved. The rising theater series is the quantity to watch: if performative maintenance keeps climbing while actual mobility concentrates further, the arrangement drifts toward inertia maintained by anniversary rhetoric.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This story instantiates the integration_reading of federation_membership_kernel; the member_sovereignty_reading and welfare_coordination_reading instantiate structurally different constraints over the same treaty text. Which reading''s arrangement does the standing legal order actually enforce?',
    'Comparative authoring of the two sibling stories plus doctrinal tracing: count which reading''s premises recent rulings operationalize (bounding lines that uphold national filters versus expansion lines that strike them down) and which premises the Commission''s enforcement practice follows.',
    'If the enforced arrangement converges on member_sovereignty premises, the victim set contracts (welfare contributors exit), epsilon falls materially, and this story''s classification drifts toward coordination-dominant; if convergence runs the other way, the authored metrics stand.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'One-of-three-readings structure of the federation membership kernel; sibling readings are separate constraints with different victim sets and epsilon.').

omega_variable(
    fiscal_compensation_counterfactual,
    'How much of the measured extraction survives under a working fiscal-correction mechanism that pays receiving states for extending benefits to non-contributing arrivals?',
    'Scale the existing EU adjustment instruments to welfare-incidence estimates and compare compensated against uncompensated corridors on caseload cost and contribution ratios.',
    'Full compensation collapses the welfare-channel asymmetry and pushes the arrangement toward pure coordination; symbolic compensation leaves the asymmetry intact and the authored values stand.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fiscal_compensation_counterfactual, empirical, 'Whether the uncompensated welfare-cost channel is structural or an artifact of missing fiscal machinery.').

omega_variable(
    brain_drain_net_accounting,
    'Do remittances, return migration, and circular movement offset sending-region human-capital losses, or does the drain operate as a net externalized cost?',
    'Longitudinal cohort tracking of post-enlargement sending regions: population pyramids, municipal fiscal capacity, care-economy load, and return rates.',
    'Net-positive accounting demotes sending_state_peripheral_communities from the victim set and lowers epsilon; net-negative accounting confirms the externalization and supports the authored values.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(brain_drain_net_accounting, empirical, 'Whether the sending-state brain-drain channel nets out or accumulates as externalized cost.').

omega_variable(
    jurisprudential_ratchet_reversibility,
    'Is the expansive-interpretation path reversible — can later rulings or political override narrow scope — or does each widening ruling lock in the next?',
    'Track whether the bounding rulings that upheld national benefit filters generalize or remain bounded exceptions, and whether any national restriction survives appellate review.',
    'Reversibility raises member-state exit options, lowers suppression, and drifts the arrangement toward coordination-dominant; irreversibility entrenches the trapped condition and the authored suppression series.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(jurisprudential_ratchet_reversibility, empirical, 'Path-dependence of the supranational interpretive ratchet.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership_kernel__integration_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fede_tr_t0, federation_membership_kernel__integration_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(fede_tr_t0, observed).
narrative_ontology:measurement(fede_tr_t5, federation_membership_kernel__integration_reading, theater_ratio, 5, 0.15).
narrative_ontology:measurement_basis(fede_tr_t5, observed).
narrative_ontology:measurement(fede_tr_t10, federation_membership_kernel__integration_reading, theater_ratio, 10, 0.19).
narrative_ontology:measurement_basis(fede_tr_t10, observed).
narrative_ontology:measurement(fede_tr_t15, federation_membership_kernel__integration_reading, theater_ratio, 15, 0.23).
narrative_ontology:measurement_basis(fede_tr_t15, observed).
narrative_ontology:measurement(fede_tr_t20, federation_membership_kernel__integration_reading, theater_ratio, 20, 0.27).
narrative_ontology:measurement_basis(fede_tr_t20, observed).
narrative_ontology:measurement(fede_tr_t25, federation_membership_kernel__integration_reading, theater_ratio, 25, 0.29).
narrative_ontology:measurement_basis(fede_tr_t25, observed).
narrative_ontology:measurement(fede_tr_t30, federation_membership_kernel__integration_reading, theater_ratio, 30, 0.31).
narrative_ontology:measurement_basis(fede_tr_t30, observed).

% Extraction over time
narrative_ontology:measurement(fede_be_t0, federation_membership_kernel__integration_reading, base_extractiveness, 0, 0.34).
narrative_ontology:measurement_basis(fede_be_t0, observed).
narrative_ontology:measurement(fede_be_t5, federation_membership_kernel__integration_reading, base_extractiveness, 5, 0.38).
narrative_ontology:measurement_basis(fede_be_t5, observed).
narrative_ontology:measurement(fede_be_t10, federation_membership_kernel__integration_reading, base_extractiveness, 10, 0.5).
narrative_ontology:measurement_basis(fede_be_t10, observed).
narrative_ontology:measurement(fede_be_t15, federation_membership_kernel__integration_reading, base_extractiveness, 15, 0.56).
narrative_ontology:measurement_basis(fede_be_t15, observed).
narrative_ontology:measurement(fede_be_t20, federation_membership_kernel__integration_reading, base_extractiveness, 20, 0.61).
narrative_ontology:measurement_basis(fede_be_t20, observed).
narrative_ontology:measurement(fede_be_t25, federation_membership_kernel__integration_reading, base_extractiveness, 25, 0.65).
narrative_ontology:measurement_basis(fede_be_t25, observed).
narrative_ontology:measurement(fede_be_t30, federation_membership_kernel__integration_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement_basis(fede_be_t30, observed).

% Suppression requirement over time
narrative_ontology:measurement(fede_su_t0, federation_membership_kernel__integration_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement_basis(fede_su_t0, observed).
narrative_ontology:measurement(fede_su_t5, federation_membership_kernel__integration_reading, suppression_requirement, 5, 0.47).
narrative_ontology:measurement_basis(fede_su_t5, observed).
narrative_ontology:measurement(fede_su_t10, federation_membership_kernel__integration_reading, suppression_requirement, 10, 0.55).
narrative_ontology:measurement_basis(fede_su_t10, observed).
narrative_ontology:measurement(fede_su_t15, federation_membership_kernel__integration_reading, suppression_requirement, 15, 0.6).
narrative_ontology:measurement_basis(fede_su_t15, observed).
narrative_ontology:measurement(fede_su_t20, federation_membership_kernel__integration_reading, suppression_requirement, 20, 0.64).
narrative_ontology:measurement_basis(fede_su_t20, observed).
narrative_ontology:measurement(fede_su_t25, federation_membership_kernel__integration_reading, suppression_requirement, 25, 0.68).
narrative_ontology:measurement_basis(fede_su_t25, observed).
narrative_ontology:measurement(fede_su_t30, federation_membership_kernel__integration_reading, suppression_requirement, 30, 0.72).
narrative_ontology:measurement_basis(fede_su_t30, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(federation_membership_kernel__integration_reading, resource_allocation).
narrative_ontology:affects_constraint(federation_membership_kernel__integration_reading, member_sovereignty_reading).
narrative_ontology:affects_constraint(federation_membership_kernel__integration_reading, welfare_coordination_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of federation_membership_kernel per the epsilon-invariance principle: the colloquial label 'EU free movement' covers three structurally distinct arrangements depending on where bounding authority sits. This file instantiates the integration_reading (supranational expansive interpretation; victim set includes displaced locals, uncompensated welfare contributors, and sending peripheries). The member_sovereignty_reading instantiates a different constraint with a contracted victim set and lower epsilon; the welfare_coordination_reading instantiates a third with the welfare channel routed through coordination rather than harmonized equal treatment. This reading is upstream of the welfare_coordination_reading: its expansive jurisprudence created the portability and social-dumping problems that made the coordination apparatus necessary. All three files link one another through network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(federation_membership_kernel__integration_reading, organized, 0.78).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
