% ============================================================================
% CONSTRAINT STORY: ietf_openness_commitment__legitimacy_erosion_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ietf_openness_commitment__legitimacy_erosion_reading, []).

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
 *   constraint_id: ietf_openness_commitment__legitimacy_erosion_reading
 *   human_readable: IETF Rough-Consensus Procedure as Contested Legitimacy Commons (Legitimacy-Erosion Reading)
 *   domain: technology governance/internet standards/institutional economics
 *
 * SUMMARY:
 *   The IETF's decision procedure — 'rough consensus and running code,'
 *   codified in RFC 7282 and administered by working-group chairs under the
 *   Tao of IETF — is the standing arrangement under contest. This story
 *   instantiates the legitimacy_erosion_reading of the
 *   ietf_openness_commitment kernel: the procedure itself is the contested
 *   object, vulnerable to organized capture despite its procedural
 *   safeguards, and the resource being consumed is the procedure's own
 *   credibility. On this reading, well-resourced factions convert sustained
 *   paid attendance into ratified text favoring their architectures and
 *   patent positions; the extraction lands not primarily in money but in
 *   ratification authority, and the bearing parties are implementers and
 *   affected non-participants bound by text they had no proportional voice in
 *   shaping. The epsilon referent is the existing rough-consensus arrangement
 *   as this reading sees it — not the reformed process this reading would
 *   prefer. Sibling readings (commons_stewardship, capture_substrate) are
 *   separate constraints with separate files; they are not averaged into this
 *   one. The claim/metric gap is deliberate: claimed_type states what this
 *   reading believes is structurally true (a real coordination function fused
 *   with asymmetric, actively enforced extraction), while the metrics
 *   describe the arrangement's actual operation as this reading measures it.
 *
 * KEY AGENTS:
 *   - - large_vendor_delegations: Primary beneficiary (institutional/mobile) — converts sustained paid attendance into editorial control over ratified text
 *   - - patent_holding_incumbents: Secondary beneficiary (institutional/arbitrage) — shapes specification text that widens licensing bases
 *   - - ietf_working_group_chairs: Agenda setter with dual position (institutional/constrained) — administers consensus determinations while employed inside the participating ecosystem
 *   - - independent_implementers: Primary target (moderate/trapped) — bound by ratified text without proportional voice
 *   - - academic_public_interest_participants: Persistent-objector target (moderate/identity_locked) — stays attached to an institution their influence within is thinning
 *   - - downstream_equipment_manufacturers: Organized cost-bearer (organized/constrained) — absorbs vendor-favorable mandatory-feature costs
 *   - - unrepresented_global_south_engineers: Excluded party (powerless/trapped) — affected by every ratified text, present at none
 *   - - standards_policy_researchers: Analytical observer (analytical/analytical) — documents the participation asymmetry the other seats dispute
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ietf_openness_commitment__legitimacy_erosion_reading, 0.68).
domain_priors:suppression_score(ietf_openness_commitment__legitimacy_erosion_reading, 0.45).
domain_priors:theater_ratio(ietf_openness_commitment__legitimacy_erosion_reading, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ietf_openness_commitment__legitimacy_erosion_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(ietf_openness_commitment__legitimacy_erosion_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(ietf_openness_commitment__legitimacy_erosion_reading, theater_ratio, 0.52).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ietf_openness_commitment__legitimacy_erosion_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(ietf_openness_commitment__legitimacy_erosion_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ietf_openness_commitment__legitimacy_erosion_reading, tangled_rope).
narrative_ontology:human_readable(ietf_openness_commitment__legitimacy_erosion_reading, "IETF Rough-Consensus Procedure as Contested Legitimacy Commons (Legitimacy-Erosion Reading)").
narrative_ontology:topic_domain(ietf_openness_commitment__legitimacy_erosion_reading, "technology governance/internet standards/institutional economics").

domain_priors:requires_active_enforcement(ietf_openness_commitment__legitimacy_erosion_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ietf_openness_commitment__legitimacy_erosion_reading, 'e935bcab-6fd4-4da4-a9ed-e3461726fed3').
narrative_ontology:cs_kernel_codification('e935bcab-6fd4-4da4-a9ed-e3461726fed3', fixed_text).
narrative_ontology:cs_authority_grounding('e935bcab-6fd4-4da4-a9ed-e3461726fed3', practice).
narrative_ontology:cs_interpretation_layer_present('e935bcab-6fd4-4da4-a9ed-e3461726fed3').
narrative_ontology:cs_reading_relation('e935bcab-6fd4-4da4-a9ed-e3461726fed3', ietf_openness_commitment__commons_stewardship_reading, coexists_with).
narrative_ontology:cs_reading_relation('e935bcab-6fd4-4da4-a9ed-e3461726fed3', ietf_openness_commitment__capture_substrate_reading, coexists_with).
narrative_ontology:cs_axiom('e935bcab-6fd4-4da4-a9ed-e3461726fed3', foundational, rough_consensus_legitimacy_is_depletable_commons).
narrative_ontology:cs_axiom_status(rough_consensus_legitimacy_is_depletable_commons, holdable).
narrative_ontology:cs_axiom_grounding('e935bcab-6fd4-4da4-a9ed-e3461726fed3', rough_consensus_legitimacy_is_depletable_commons, empirically_contingent).
narrative_ontology:cs_axiom('e935bcab-6fd4-4da4-a9ed-e3461726fed3', secondary, attendance_is_not_consent).
narrative_ontology:cs_axiom_status(attendance_is_not_consent, holdable).
narrative_ontology:cs_axiom_grounding('e935bcab-6fd4-4da4-a9ed-e3461726fed3', attendance_is_not_consent, deontological).
narrative_ontology:cs_reference_frame('e935bcab-6fd4-4da4-a9ed-e3461726fed3', founder_peer_community_consensus).
narrative_ontology:cs_drift_state('e935bcab-6fd4-4da4-a9ed-e3461726fed3', commercial_scale_participation_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('e935bcab-6fd4-4da4-a9ed-e3461726fed3', '').
narrative_ontology:cs_kernel_id(ietf_openness_commitment__legitimacy_erosion_reading, ietf_openness_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ietf_openness_commitment__legitimacy_erosion_reading, large_vendor_delegations).
narrative_ontology:constraint_beneficiary(ietf_openness_commitment__legitimacy_erosion_reading, patent_holding_incumbents).
narrative_ontology:constraint_victim(ietf_openness_commitment__legitimacy_erosion_reading, independent_implementers).
narrative_ontology:constraint_victim(ietf_openness_commitment__legitimacy_erosion_reading, academic_public_interest_participants).
narrative_ontology:constraint_victim(ietf_openness_commitment__legitimacy_erosion_reading, downstream_equipment_manufacturers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(ietf_openness_commitment__legitimacy_erosion_reading, ietf_working_group_chairs).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Send teams of salaried engineers to working-group meetings and mailing-list threads year-round. Sustained presence buys familiarity with document editors, authorship seats, and the ability to keep preferred text alive through successive revisions. When ratified text encodes their product architectures, the return on attendance compounds. Leaving for friendlier forums is costly but feasible; several maintain parallel activity in industry consortia.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__legitimacy_erosion_reading, large_vendor_delegations, beneficiary,
    institutional, biographical, mobile, global).

% Hold portfolios of patents declared essential to specifications they help shape. Positions ratified under the consensus banner widen the licensing base and strengthen the fairness narrative around royalty demands. Licensing income arrives regardless of which forum ratifies the text, which leaves them free to shop for venues whose procedures favor their contributions.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__legitimacy_erosion_reading, patent_holding_incumbents, beneficiary,
    institutional, generational, arbitrage, global).

% Small firms and individual developers who must implement whatever text is ratified to remain interoperable. They lack the staff to attend meetings continuously, so their objections arrive late in last call or not at all. Exit is not realistic: the specifications define the network they connect to, and no alternative text governs the same protocols.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__legitimacy_erosion_reading, independent_implementers, payer,
    moderate, biographical, trapped, global).

% Researchers and civil-society technologists who participate out of conviction about the open Internet. Chronically out-hoursed by corporate delegations, they persist in the objector role and absorb reputational cost for slowing agreement. Leaving would mean conceding an institution they regard as partly theirs, so they stay attached even as their influence thins.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__legitimacy_erosion_reading, academic_public_interest_participants, payer,
    moderate, generational, identity_locked, global).

% Build routers, handsets, and appliances that must conform to ratified specifications. Mandatory features favored by sponsoring vendors raise bill-of-materials and integration costs. Industry associations give them some collective voice, and switching forums is possible for peripheral technologies but not for core connectivity standards.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__legitimacy_erosion_reading, downstream_equipment_manufacturers, payer,
    organized, biographical, constrained, global).

% Run working groups: judge when agreement has been reached, rule objections in or out of scope, and shepherd documents to publication. Formally neutral servants of the process, they are typically drawn from, and employed by, participating companies, and their careers advance inside the same ecosystem whose text they referee. Resignation removes the person, not the role.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__legitimacy_erosion_reading, ietf_working_group_chairs, agenda_setter,
    institutional, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(ietf_openness_commitment__legitimacy_erosion_reading, ietf_working_group_chairs, beneficiary).

% Build services and networks on top of ratified Internet standards without the funded presence that participation requires. Their interests reach the process only through proxies, if at all. They would object that decisions bind them without hearing them; they are absent because attendance, not affectedness, is the price of a voice.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__legitimacy_erosion_reading, unrepresented_global_south_engineers, excluded,
    powerless, biographical, trapped, global).

% Study standards bodies from the outside: participation statistics, dissent records, appeal outcomes, forum-shopping episodes. They publish the asymmetry data that participants dispute. They neither set the rules nor bear them; their stake is explanatory.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__legitimacy_erosion_reading, standards_policy_researchers, observer,
    analytical, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ietf_openness_commitment__legitimacy_erosion_reading, large_vendor_delegations).
narrative_ontology:fixing_cost_class(ietf_openness_commitment__legitimacy_erosion_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Produces interoperable technical standards for a decentralized network without central authority: thousands of organizations must agree on protocol behavior, and rough consensus lets specification work proceed among peers without votes, hierarchies, or unanimous sign-off.
% TRANSFER_FUNCTION: Moves ratification authority — the effective power to define binding technical text — toward whichever factions can sustain organized participation. Engineer-hours, meeting attendance, and editorial labor are converted into control over specification content; the costs of the resulting text fall on parties bound by it without proportional voice.
% ABSENT_VOICES: Unrepresented implementers, end users, Global-South engineers, and security researchers outside the delegate economy would object that 'consensus' measures attendance rather than consent. They are absent because meaningful participation requires sustained funded presence — travel budgets, salaried time, and multi-year continuity that individuals and small firms cannot supply.
% DISAPPEARANCE_RATIONALE: If the rough-consensus procedure vanished overnight, Internet protocol standardization would reorganize around whatever replaced it: heavier voting regimes, consortium dominance by the largest funders, or fragmentation into rival incompatible specification tracks. Every stakeholder seat listed here is positioned relative to this procedure; its removal forces each to find a new anchor.
% FOUNDING_PROBLEM: A small research community building a novel network needed a way to make collective technical decisions among trusted peers without corporate hierarchy or voting blocs. Rough consensus emerged to keep protocol development moving when everyone at the table knew everyone else and reputational ties were personal.
% FOUNDING_PROBLEM_CORROBORATION: Outside the benefiting parties: peer-reviewed studies of standards-body participation asymmetry, RFC 7282's own acknowledgment that consensus determination is chair judgment rather than measurement, archived last-call dissent statements and formal appeal rulings, and public reflections of former senior participants all corroborate that the founding community's conditions no longer hold. The benefiting parties — vendor delegations and the administrative structures their funding sustains — attest that the scaling problem remains live and the process still works; the dispute itself is the signal.
narrative_ontology:disappearance_verdict(ietf_openness_commitment__legitimacy_erosion_reading, world_rearranges).
narrative_ontology:founding_problem_status(ietf_openness_commitment__legitimacy_erosion_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ietf_openness_commitment__legitimacy_erosion_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(ietf_openness_commitment__legitimacy_erosion_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ietf_openness_commitment__legitimacy_erosion_reading, 0.68, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ietf_openness_commitment__legitimacy_erosion_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ietf_openness_commitment__legitimacy_erosion_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ietf_openness_commitment__legitimacy_erosion_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.68) because the procedure transfers ratification authority toward organized attendance, and the transfer is decoupled from the affected population: the ratio of corporate to independent voices in working groups has widened across the interval while the output text binds everyone. Suppression is moderate (0.45), not high: nothing forbids dissent, but the machinery channels and closes it — chair discretion over when consensus 'has been reached,' the burden of sustaining objection across last calls, and the professional cost of being labeled obstructive. Theater is just above half (0.52): last-call periods, humble-statements norms, and the appeals path perform openness while, on this reading, a growing share of decisions track organized weight; the coordination work of producing interoperable specifications is nonetheless real, which caps theater below piton territory. Accessibility_collapse is 0.55: alternatives exist (W3C, ISO, consortia, de facto implementations) but the IETF's legitimacy concentration on core Internet protocols collapses them substantially for anything touching connectivity. Resistance is 0.50: appeals, dissent statements, splinter efforts, and the scholarly capture literature constitute real, continuing pushback that has not displaced the procedure. The temporal series run on one shared grid (t=0..30, approximating the mid-1990s commercialization surge to the present) with all three metrics authored at every point. Suppression_requirement is tracked deliberately: the interval saw the procedural apparatus accrete — formal appeal channels, ombudsteam, chair mediation training, ever-more-elaborate consensus guidance — so the enforcement/compliance machinery genuinely matured and hardened; this is an enforcement-capacity trajectory, not merely shifting extraction.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the vendor-delegation seat, the procedure is coordination it funds and staffs — the same structure reads as a service it purchases. From the independent-implementer seat, the identical structure operates as binding text issued without a hearing. From the chair seat, it is administration: a stream of judgment calls made under employment relationships the neutrality norm asks everyone to discount. From the researcher seat, it is a measurable asymmetry. The engine computes per-seat classifications from the structural data; this story's claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive the derivation: large_vendor_delegations and patent_holding_incumbents sit near the beneficiary end (low d, subsidized or inverted effective extraction), amplified by their mobile-to-arbitrage exit positions — the patent holders especially can monetize any forum's output. Victims sit near the target end: independent_implementers are trapped (the specifications define their market's physics), academic_public_interest_participants are identity_locked (exit means abandoning an institution constitutive of their self-concept), and downstream_equipment_manufacturers are constrained with partial collective voice. The chair seat is the structural hinge: agenda_setter by role, secondarily positioned inside the beneficiary ecosystem by employment, which is why the capture vector runs through personnel rather than through rules. The excluded seat carries the highest latent exposure — fully targeted, zero voice. No directionality overrides were needed: the beneficiary/victim declarations plus exit atoms already place every seat correctly, and the schema's override mechanism keys on power atoms, which would smear corrections across unrelated institutional seats.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — collective decision-making among a small peer community — has been partially outlived: the community scaled past peer equality decades ago, and the procedure's legitimacy now rests on inherited form rather than founding conditions. But the status is honestly contested, not dead: defenders credibly argue the scaling problem is permanent and the process still ships interoperable reality. The classification prevents mislabeling in both directions. Reading the arrangement as pure coordination (the stewardship claim) would hide the legitimacy transfer entirely; reading it as pure extraction (a snare framing) would erase the genuine, load-bearing coordination function that keeps the Internet interoperable. The tangled_rope claim keeps both faces visible and forces the analytic question this corpus exists to ask: how much of the procedure's persistence is coordination and how much is organized factions defending a ratification channel they have learned to operate. Because founding_problem_status is contested rather than dead, the mismatch consumer should not fire the zombie flag here — but the rising theater_ratio series marks the direction of travel: if safeguard performance continues replacing safeguard function, the arrangement drifts toward inertial maintenance of a legitimacy ritual.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This story instantiates the legitimacy_erosion_reading of the ietf_openness_commitment kernel; would the commons_stewardship_reading or the capture_substrate_reading assign a materially different epsilon and victim set to the same standing arrangement?',
    'Author the sibling stories as separate epsilon-invariant constraints and compare computed classifications across the family; divergence localized to the decision-dynamics axis confirms the decomposition was correct.',
    'The stewardship reading would drive epsilon toward the coordination floor and shrink the victim set to near-empty; the substrate reading would relocate extraction from legitimacy consumption to encoded gatekeeping and shift victims from credibility-bearers to excluded entrants. Classification of THIS file is conditional on the erosion lens.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer structure: one reading of a contested kernel; sibling readings instantiate different constraints.').

omega_variable(
    legitimacy_metric_selection,
    'How is depletion of a procedural legitimacy commons measured — participation-skew indices, appeal and dissent frequencies, defection of workloads to consortia, or adoption friction in ratified specifications?',
    'Longitudinal comparison of candidate indicators against observed events (forum-shopping episodes, failed last calls, splinter standardization efforts) to establish which track the underlying quantity rather than its symptoms.',
    'Different observables yield different trajectories; if appeal-rate data dominates, measured extraction is lower than participation-skew data suggests, and the erosion verdict weakens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_metric_selection, empirical, 'Observable selection for an intangible extracted resource.').

omega_variable(
    competence_capture_confound,
    'Is vendor-dominated working-group composition evidence of organized capture, or of competence concentrating where the deepest protocol expertise is employed?',
    'Matched comparison of heavily vendor-dominated working groups against comparable less-dominated ones on defect rates, interoperability failures, and post-ratification revision frequency.',
    'If dominated groups produce equivalent or better specifications, a large share of measured extraction is the price of expertise aggregation rather than factional rent, and the tangled_rope balance shifts toward its rope component.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(competence_capture_confound, conceptual, 'Whether organized participation reflects rent-seeking or competence concentration.').

omega_variable(
    exit_threat_discipline,
    'Are large vendors'' implicit threats to move standardization to friendlier consortia credible enough that the procedure''s concessions are priced bargaining rather than one-sided consumption?',
    'Track forum-shopping episodes across the interval: cases where workload actually migrated versus cases where the IETF retained or recaptured it (as with QUIC''s migration from proprietary origin into the IETF).',
    'If exit threats are credible and reciprocal, part of the apparent extraction is equilibrium bargaining, lowering effective extraction for the beneficiary seats; if threats are hollow, the concessions are pure consumption of the commons.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exit_threat_discipline, empirical, 'Whether beneficiary exit options discipline or merely enable the extraction.').

omega_variable(
    dissent_dampening_mechanism,
    'Is the observed dampening of persistent objection structural (chair rulings, procedural burden, last-call exhaustion) or internalized (politeness culture, fear of the obstructionist label, identification with the institution)?',
    'Post-participation surveys of former contributors and natural experiments around chair turnover: if objection rates jump when specific chairs rotate out, the mechanism is structural; if they stay flat, it is carried internally by participants.',
    'If substantially internalized, the constraint''s effective suppression exceeds the structural measure — targets carry the dampening with them after disengagement, and reform of rules alone would not restore contestation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dissent_dampening_mechanism, conceptual, 'Structural versus internalized suppression of persistent objection.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ietf_openness_commitment__legitimacy_erosion_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ietf_tr_t0, ietf_openness_commitment__legitimacy_erosion_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(ietf_tr_t5, ietf_openness_commitment__legitimacy_erosion_reading, theater_ratio, 5, 0.3).
narrative_ontology:measurement(ietf_tr_t10, ietf_openness_commitment__legitimacy_erosion_reading, theater_ratio, 10, 0.34).
narrative_ontology:measurement(ietf_tr_t15, ietf_openness_commitment__legitimacy_erosion_reading, theater_ratio, 15, 0.4).
narrative_ontology:measurement(ietf_tr_t20, ietf_openness_commitment__legitimacy_erosion_reading, theater_ratio, 20, 0.45).
narrative_ontology:measurement(ietf_tr_t25, ietf_openness_commitment__legitimacy_erosion_reading, theater_ratio, 25, 0.49).
narrative_ontology:measurement(ietf_tr_t30, ietf_openness_commitment__legitimacy_erosion_reading, theater_ratio, 30, 0.52).

% Extraction over time
narrative_ontology:measurement(ietf_be_t0, ietf_openness_commitment__legitimacy_erosion_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(ietf_be_t5, ietf_openness_commitment__legitimacy_erosion_reading, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(ietf_be_t10, ietf_openness_commitment__legitimacy_erosion_reading, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(ietf_be_t15, ietf_openness_commitment__legitimacy_erosion_reading, base_extractiveness, 15, 0.54).
narrative_ontology:measurement(ietf_be_t20, ietf_openness_commitment__legitimacy_erosion_reading, base_extractiveness, 20, 0.6).
narrative_ontology:measurement(ietf_be_t25, ietf_openness_commitment__legitimacy_erosion_reading, base_extractiveness, 25, 0.64).
narrative_ontology:measurement(ietf_be_t30, ietf_openness_commitment__legitimacy_erosion_reading, base_extractiveness, 30, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(ietf_su_t0, ietf_openness_commitment__legitimacy_erosion_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(ietf_su_t5, ietf_openness_commitment__legitimacy_erosion_reading, suppression_requirement, 5, 0.32).
narrative_ontology:measurement(ietf_su_t10, ietf_openness_commitment__legitimacy_erosion_reading, suppression_requirement, 10, 0.35).
narrative_ontology:measurement(ietf_su_t15, ietf_openness_commitment__legitimacy_erosion_reading, suppression_requirement, 15, 0.38).
narrative_ontology:measurement(ietf_su_t20, ietf_openness_commitment__legitimacy_erosion_reading, suppression_requirement, 20, 0.41).
narrative_ontology:measurement(ietf_su_t25, ietf_openness_commitment__legitimacy_erosion_reading, suppression_requirement, 25, 0.43).
narrative_ontology:measurement(ietf_su_t30, ietf_openness_commitment__legitimacy_erosion_reading, suppression_requirement, 30, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ietf_openness_commitment__legitimacy_erosion_reading, information_standard).
narrative_ontology:affects_constraint(ietf_openness_commitment__legitimacy_erosion_reading, ietf_openness_commitment__commons_stewardship_reading).
narrative_ontology:affects_constraint(ietf_openness_commitment__legitimacy_erosion_reading, ietf_openness_commitment__capture_substrate_reading).

% DUAL FORMULATION NOTE:
% Constraint family decomposition per the epsilon-invariance principle: the colloquial label 'IETF openness' conflates three structurally distinct claims. The commons_stewardship_reading assesses the openness commitment by its outputs (interoperability preserved for all implementers; epsilon near the coordination floor). The capture_substrate_reading assesses it by its input structure (resource advantage translating into encoded gatekeeping; extraction located in the conversion mechanism). This story, the legitimacy_erosion_reading, assesses it by its decision dynamics (organized factions consuming procedural legitimacy to ratify self-serving outcomes; the victim is the mechanism's credibility and the unrepresented parties bound by its output). Each member gets its own epsilon, its own beneficiary/victim structure, and its own classification; the family is linked through affects_constraints edges, with the upstream stewardship claim historically cited as cover by the downstream erosion dynamic.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
