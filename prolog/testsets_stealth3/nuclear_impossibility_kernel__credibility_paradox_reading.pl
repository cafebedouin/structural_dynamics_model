% ============================================================================
% CONSTRAINT STORY: nuclear_impossibility_kernel__credibility_paradox_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nuclear_impossibility_kernel__credibility_paradox_reading, []).

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
 *   constraint_id: nuclear_impossibility_kernel__credibility_paradox_reading
 *   human_readable: Nuclear Deterrence Posture Regime — Credibility Paradox Reading
 *   domain: strategic_studies/international_relations
 *
 * SUMMARY:
 *   This story authors the credibility_paradox_reading of the
 *   nuclear_impossibility_kernel: the standing arrangement under contest is
 *   the post-1945 deterrence-posture regime itself — the arsenals,
 *   declaratory doctrines, signaling apparatus, alliance assurances, and
 *   modernization pipelines — assessed by this reading's own lights. On this
 *   reading the arrangement rests on a contradiction it cannot resolve: the
 *   peace is officially attributed to credible threats of use, but a threat
 *   whose execution guarantees the threatener's own destruction cannot carry
 *   the credibility the official story assigns it; the gap is managed
 *   rhetorically ('unthinkability') while operational practice continuously
 *   manufactures substitute credibility through usable options, counterforce
 *   investment, and escalation-ladder planning, which keeps war reachable
 *   rather than foreclosed. The epsilon referent is this standing
 *   arrangement, not any abolitionist alternative; values are reading-indexed
 *   over that fixed referent. Sibling readings
 *   (structural_contraction_reading, rational_dropout_reading) are separate
 *   constraints in separate files and are not folded into this
 *   classification; committer structure lives in the omegas and cs_structure.
 *   Claim and metrics are independent authored facts: the claimed type
 *   reflects what I judge structurally true (a genuine coordination core
 *   wrapped in asymmetric, enforced extraction), while the metric values
 *   report what I judge descriptively true of the arrangement's actual
 *   operation, including a substantial performative share — the engine
 *   computes per-seat verdicts from the structural data, and divergence
 *   between claim and computed type is signal, not error.
 *
 * KEY AGENTS:
 *   - nuclear_state_executives: Agenda setter (institutional/identity_locked) — owns declaratory policy and launch authority; collects status, alliance leverage, and crisis bargaining power; personally exposed to the same tail risk they impose on others
 *   - strategic_military_commands: Agenda setter with beneficiary secondary role (powerful/identity_locked) — runs readiness, exercises, targeting, and modernization intake; organizationally fused with the mission; collects budget share and institutional relevance
 *   - defense_industrial_base: Beneficiary (powerful/constrained) — collects recapitalization and sustainment contracts; purpose-built facilities tie it to program continuity; lobbies for continuation
 *   - deterrence_intellectual_establishment: Beneficiary (organized/identity_locked) — produces doctrine, staffs rotations, legitimates posture; careers and schools of thought are constituted by the credibility framework
 *   - civilian_populations_arsenal_states: Payer (powerless/trapped) — fund the arrangement through taxation, bear targeting exposure and fallout-scale tail risk, and hold no lever over launch authority; no jurisdiction lies outside blast, fallout, or climate reach
 *   - non_nuclear_alliance_populations: Payer with beneficiary secondary role (moderate/constrained) — host basing, appear on target sets, cede crisis decision rights to patrons, and receive umbrella reassurance in exchange
 *   - disarmament_movements_tpnw_states: Excluded (organized/constrained) — campaign for prohibition and achieved a treaty the arsenal states boycott; structurally outside the rooms where posture is set
 *   - arms_control_verification_bodies: Observer (institutional/analytical) — inspect, count, certify, and document compliance gaps across the regime
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nuclear_impossibility_kernel__credibility_paradox_reading, 0.67).
domain_priors:suppression_score(nuclear_impossibility_kernel__credibility_paradox_reading, 0.64).
domain_priors:theater_ratio(nuclear_impossibility_kernel__credibility_paradox_reading, 0.63).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__credibility_paradox_reading, extractiveness, 0.67).
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__credibility_paradox_reading, suppression_requirement, 0.64).
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__credibility_paradox_reading, theater_ratio, 0.63).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__credibility_paradox_reading, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__credibility_paradox_reading, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nuclear_impossibility_kernel__credibility_paradox_reading, tangled_rope).
narrative_ontology:human_readable(nuclear_impossibility_kernel__credibility_paradox_reading, "Nuclear Deterrence Posture Regime — Credibility Paradox Reading").
narrative_ontology:topic_domain(nuclear_impossibility_kernel__credibility_paradox_reading, "strategic_studies/international_relations").

domain_priors:requires_active_enforcement(nuclear_impossibility_kernel__credibility_paradox_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nuclear_impossibility_kernel__credibility_paradox_reading, '1d0eb439-3149-4afa-9bc1-fb4bef02ba65').
narrative_ontology:cs_kernel_codification('1d0eb439-3149-4afa-9bc1-fb4bef02ba65', distributed).
narrative_ontology:cs_authority_grounding('1d0eb439-3149-4afa-9bc1-fb4bef02ba65', distributed).
narrative_ontology:cs_reading_relation('1d0eb439-3149-4afa-9bc1-fb4bef02ba65', nuclear_impossibility_kernel__structural_contraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('1d0eb439-3149-4afa-9bc1-fb4bef02ba65', nuclear_impossibility_kernel__rational_dropout_reading, influences).
narrative_ontology:cs_axiom('1d0eb439-3149-4afa-9bc1-fb4bef02ba65', foundational, deterrence_threat_inherently_incredible).
narrative_ontology:cs_axiom_status(deterrence_threat_inherently_incredible, holdable).
narrative_ontology:cs_axiom_grounding('1d0eb439-3149-4afa-9bc1-fb4bef02ba65', deterrence_threat_inherently_incredible, empirically_contingent).
narrative_ontology:cs_axiom('1d0eb439-3149-4afa-9bc1-fb4bef02ba65', foundational, usability_seeking_sustains_war_reachability).
narrative_ontology:cs_axiom_status(usability_seeking_sustains_war_reachability, holdable).
narrative_ontology:cs_axiom_grounding('1d0eb439-3149-4afa-9bc1-fb4bef02ba65', usability_seeking_sustains_war_reachability, empirically_contingent).
narrative_ontology:cs_reference_frame('1d0eb439-3149-4afa-9bc1-fb4bef02ba65', managed_credibility_deficit_equilibrium).
narrative_ontology:cs_drift_state('1d0eb439-3149-4afa-9bc1-fb4bef02ba65', contemporary_multipolar_rearmament, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('1d0eb439-3149-4afa-9bc1-fb4bef02ba65', '').
narrative_ontology:cs_kernel_id(nuclear_impossibility_kernel__credibility_paradox_reading, nuclear_impossibility_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nuclear_impossibility_kernel__credibility_paradox_reading, nuclear_state_executives).
narrative_ontology:constraint_beneficiary(nuclear_impossibility_kernel__credibility_paradox_reading, strategic_military_commands).
narrative_ontology:constraint_beneficiary(nuclear_impossibility_kernel__credibility_paradox_reading, defense_industrial_base).
narrative_ontology:constraint_beneficiary(nuclear_impossibility_kernel__credibility_paradox_reading, deterrence_intellectual_establishment).
narrative_ontology:constraint_victim(nuclear_impossibility_kernel__credibility_paradox_reading, civilian_populations_arsenal_states).
narrative_ontology:constraint_victim(nuclear_impossibility_kernel__credibility_paradox_reading, non_nuclear_alliance_populations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(nuclear_impossibility_kernel__credibility_paradox_reading, non_nuclear_alliance_populations).
narrative_ontology:constraint_vindicates(nuclear_impossibility_kernel__credibility_paradox_reading, escalation_ladder_doctrine).
narrative_ontology:constraint_vindicates(nuclear_impossibility_kernel__credibility_paradox_reading, counterforce_usability_program).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Own declaratory policy, force posture decisions, and sole or shared launch authority. Draw prestige, alliance leadership, and crisis bargaining leverage from arsenal stewardship. Renouncing the arsenal would read domestically and internationally as a demotion in rank, and the national self-image of the states involved is bound to great-power standing the weapons signify. They are also personally resident inside the same destruction their instruments threaten.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__credibility_paradox_reading, nuclear_state_executives, agenda_setter,
    institutional, biographical, identity_locked, global).

% Run readiness cycles, exercises, targeting review, and modernization intake; convert appropriations into posture. The mission defines the institution — no peacetime portfolio absorbs the specialized skill set at comparable scale or status, and officer identity, promotion pathways, and institutional lore are built around it. Budget share and organizational relevance flow from the mission's continuation.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__credibility_paradox_reading, strategic_military_commands, agenda_setter,
    powerful, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(nuclear_impossibility_kernel__credibility_paradox_reading, strategic_military_commands, beneficiary).

% Collects recapitalization, sustainment, and delivery-system contracts across decades-long procurement cycles. Warhead-pit production, assembly, and delivery-platform lines are purpose-built and poorly convertible; margins and workforce continuity depend on program continuation, which the firms actively lobby to secure. Diversification exists at the edges but the captive niches dominate.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__credibility_paradox_reading, defense_industrial_base, beneficiary,
    powerful, biographical, constrained, national).

% Produces the doctrine, war-games the ladders, staffs the revolving door between think tanks, academies, and government posts. Schools of thought transmit across cohorts through curricula and mentorship; professional standing, publication venues, and advisory access all presuppose the credibility framework's centrality. An analyst whose life work is escalation management has no comparably endowed home in a prohibition-centered paradigm.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__credibility_paradox_reading, deterrence_intellectual_establishment, beneficiary,
    organized, generational, identity_locked, global).

% Fund the arsenals through taxation, live under the targeting exposure the posture generates, and inherit the fallout, climatic, and economic tail risks passed to descendants. They hold no lever over launch authority or force posture: the relevant decisions are classified, technocratic, and executive-held, and electoral signals on the subject never bind. Emigration offers no refuge, since blast radius, fallout plumes, nuclear winter, and financial collapse ignore borders.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__credibility_paradox_reading, civilian_populations_arsenal_states, payer,
    powerless, generational, trapped, global).

% Host forward basing, appear as entries in an ally's adversary target logic, and cede crisis decision rights to the patron's capital, in exchange for extended reassurance they cannot independently verify or revoke. Leaving the umbrella means rebuilding autonomous defense or accommodating the adversary — both prohibitively costly paths — so the position persists even where opinion polls tilt against the hosting.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__credibility_paradox_reading, non_nuclear_alliance_populations, payer,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(nuclear_impossibility_kernel__credibility_paradox_reading, non_nuclear_alliance_populations, beneficiary).

% Campaign for a prohibition-based order and negotiated a ban treaty that every arsenal state boycotted. Their participation channels — review conferences, civil-society forums — confer presence without effect on posture decisions, which are made in closed national processes. They would rebuild the arrangement around elimination timelines and de-alerting rather than credibility management.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__credibility_paradox_reading, disarmament_movements_tpnw_states, excluded,
    organized, generational, constrained, global).

% Inspect facilities, count warheads and delivery systems, certify treaty compliance, and document gaps when regimes lapse. Their vantage spans the arsenal states' declarations against physical reality, giving the analytical seat from which the distance between declaratory story and deployed fact is most precisely measurable.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__credibility_paradox_reading, arms_control_verification_bodies, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(nuclear_impossibility_kernel__credibility_paradox_reading, defense_industrial_base).
narrative_ontology:fixing_cost_class(nuclear_impossibility_kernel__credibility_paradox_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains common knowledge among nuclear-armed adversaries about thresholds, capabilities, and resolve — through declaratory doctrine, signaling protocols, hotlines, exercise notification, and crisis communication — sufficient that encounters terminate short of deliberate great-power war; separately, allocates security reassurance from arsenal holders to non-nuclear allies.
% TRANSFER_FUNCTION: Moves wealth from general taxpayers to defense industries, strategic bureaucracies, and the surrounding expert ecosystem via modernization and readiness appropriations; moves existential risk onto arsenal-state and allied populations and onto future generations, who hold no decision rights over use; moves status and epistemic authority to the officials, commands, and experts who steward the arrangement.
% ABSENT_VOICES: Hibakusha and testing-downwind communities carry the lived testimony the posture discussions exclude; Treaty on the Prohibition of Nuclear Weapons state parties are formally outside every forum where arsenal doctrine is decided; arsenal-state publics are present as taxpayers and risk-bearers but absent from every room where launch authority and posture are set. Where are they: outside closed national decision processes, admitted to review conferences with voice but no vote, and addressed only through declaratory documents drafted without them.
% DISAPPEARANCE_RATIONALE: Overnight disappearance would force immediate repricing across the system: alliances would renegotiate or dissolve, several capable states would face acute proliferation temptation, the crisis-communication machinery would vanish precisely as uncertainty spiked, and the trillion-dollar appropriation streams would redirect. Every party agrees the world rearranges; they dispute whether the rearrangement lands safer (prohibitionist claim) or catastrophically less stable (establishment claim) — the dependency itself is not in doubt.
% FOUNDING_PROBLEM: After 1945, how to possess weapons capable of annihilating both sides while preventing their use and containing great-power rivalry. The arrangement's founding answer was to base prevention on the threatened use of the weapons — to make the threat of annihilation credible enough to restrain adversaries.
% FOUNDING_PROBLEM_CORROBORATION: Attested from outside the benefiting parties: the Science and Security Board's Doomsday Clock assessments, UN disarmament-body reporting, the prohibition-treaty state parties' standing declarations, physician associations' casualty modeling, and the declassified near-miss archive assembled by archival historians independent of the strategic establishments. No corroborating source outside the beneficiary set attests that the problem is solved; the ones consulted uniformly attest it is live and worsening.
narrative_ontology:disappearance_verdict(nuclear_impossibility_kernel__credibility_paradox_reading, world_rearranges).
narrative_ontology:founding_problem_status(nuclear_impossibility_kernel__credibility_paradox_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nuclear_impossibility_kernel__credibility_paradox_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(nuclear_impossibility_kernel__credibility_paradox_reading, 'none', 1).
narrative_ontology:epsilon_provenance(nuclear_impossibility_kernel__credibility_paradox_reading, 0.67, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nuclear_impossibility_kernel__credibility_paradox_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(nuclear_impossibility_kernel__credibility_paradox_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(nuclear_impossibility_kernel__credibility_paradox_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.67) is high because the arrangement transfers wealth (recapitalization programs measured in trillions across the arsenal states) and uncompensated existential risk from publics to a concentrated beneficiary set, while the security delivered at the margin — on this reading — purchases usability rather than safety. Suppression (0.64) records the political machinery that contains alternatives: classification of targeting and posture, the absence of any electoral lever over launch doctrine, and the marginalization of prohibitionist politics; suppression is authored as a raw structural property and is deliberately not scaled by power, scope, or anything else — only extractiveness receives contextual scaling downstream. Theater ratio (0.63 and rising) is the reading's central observational claim: the visible layer of the arrangement (solemn declaratory horror, anniversary ritual, 'unthinkable' rhetoric) increasingly performs the function that operational activity quietly contradicts (low-yield deployments, counterforce refinement, open discussion of limited use) — a textbook Goodhart trajectory in which the declaratory proxy has been drifting away from the thing it was supposed to express since the 1960s. Accessibility collapse (0.38) is honestly low: unlike a natural law, understanding the paradox does not foreclose the alternatives (deep cuts, minimal deterrence, prohibition) — they remain conceptually intact and are blocked politically, not logically. Resistance (0.48) reflects a sustained, occasionally victorious (a negotiated treaty exists) but contained oppositional tradition. The temporal series share one ten-point grid spanning 1947-2025 and display a recurring cycle — buildup, crisis, arms-control relaxation, renewed accumulation (peaks at the early 1960s and 1983, trough at 1991) — driven by geopolitical tension cycles; critically, the oscillation functions as an extraction ratchet rather than noise: each crisis licenses a capability increment, each relaxation plateaus above the previous floor, so successive cycles terminate higher than they began (intermittent-reinforcement dynamics in which crisis events are the reinforcement schedule). Base_properties are measured at interval end, i.e., on the rising phase of the current cycle.
 *
 * PERSPECTIVAL GAP:
 *   Per-seat verdicts should diverge sharply. From the agenda-setter and beneficiary seats — executives, commands, industry, the intellectual establishment — the arrangement presents as indispensable stewardship: the coordination machinery (hotlines, exercise notification, declaratory thresholds) visibly works, crises terminate, and the alliance structure holds; the establishment additionally experiences the framework as constitutive of professional and national identity, so no internal vantage discloses the extraction. From the payer seats the identical structure presents as unconsented risk transfer: publics fund and house the machinery while holding no lever over its use, and allied populations discover their territory inscribed in someone else's target logic as the price of an umbrella they cannot independently verify. The identity-locked seats are least able to perceive the gap; the trapped seats are most able to perceive it and least able to act on it. The engine computes this divergence from power, exit, and role data — nothing in the authored claim adjudicates it.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries (executives, commands, industry, establishment) derive directionality near the subsidized end: the arrangement channels money, status, mission relevance, and epistemic authority toward them. Declared victims (arsenal-state publics, allied populations) derive near the full-target end: they supply the funding and the risk surface. Three structural refinements qualify the derivation without needing overrides. First, executives carry catastrophic tail exposure personally — they sit inside the same destruction their instruments threaten — which damps their subsidy below what a pure beneficiary derivation would assign; the qualitative adjustment is left to the engine rather than forced by an override, because the beneficiary declaration remains the dominant relationship. Second, identity_lock amplifies effective extraction asymmetrically: the commands, the establishment, and the executives cannot exit even cognitively — professional identity (career path dependence in strategy as a discipline), institutional identity (organizations that became their mission), and national-prestige identity (arsenals as rank markers) each fuse the seat to the arrangement, so exit-option degradation raises the extraction they will tolerate and defend. Third, the publics are trapped at species scale: emigration does not escape fallout, nuclear winter, or systemic economic collapse, so no spatial arbitrage softens their target position. Allied populations sit mid-range with a genuine dual structure — reassurance received, sovereignty transferred — which the secondary role encodes and the engine weighs from both declarations.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — possessing annihilation-capable weapons without using them, while containing great-power rivalry — remains live, so this is not a resolved-mandatrophy case and no sunset or obsolescence flag is asserted. The classification work the analysis performs is boundary-keeping in both directions. Against the pure-snare reading: the coordination core is real — adversary-expectation management has repeatedly terminated crises short of war, and alliance assurance is a service someone would have to provide in any successor arrangement — so labeling the whole arrangement extraction would discard the function any reform must preserve. Against the rope-laundering reading: the extraction is equally real — risk and revenue flow uphill to a concentrated set while the declaratory justification (credible deterrence) is, on this reading, substantially performed rather than delivered — so calling the arrangement mere coordination would launder the asymmetry. The rising theater series marks where the hybrid is degrading: as the performative share grows, the arrangement drifts toward maintaining the story of deterrence rather than its substance, which is the signature this corpus watches for when a tangled rope begins feeding a piton or snare endpoint.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_contestation_locus,
    'This story instantiates the credibility_paradox_reading of kernel nuclear_impossibility_kernel. Is the kernel''s binding structure located in the incredibility of retaliatory threats (this reading), in guaranteed mutual annihilation foreclosing any victory path (structural_contraction_reading), or in costs exceeding benefits while victory remains structurally possible (rational_dropout_reading)?',
    'Comparative prediction test across the three readings: this reading predicts continued investment in usable options and erosion of the firebreak; the contraction reading predicts convergence on irreversible no-first-use restraint; the dropout reading predicts stable cost-driven restraint without usability programs. Observed doctrine and procurement adjudicate.',
    'Under the contraction reading the arrangement trends toward fixity and the victim set shrinks to accident exposure; under the dropout reading extraction is bounded by rational self-restraint and the firebreak is robust; under this reading the firebreak is rhetorical, extraction is unbounded by any structural guarantee, and war stays reachable through the escalation machinery itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_contestation_locus, conceptual, 'Which of the three declared readings captures the kernel''s binding structure; the disagreement is located in whether the barrier to nuclear use is rhetorical, physical, or economic.').

omega_variable(
    credibility_evidence_status,
    'Do episodes of coercive nuclear signaling (Cold War crises, contemporary ultimatum rhetoric) demonstrate that use-threats can be made credible enough to extract concessions, challenging the reading''s foundational incredibility axiom?',
    'Systematic coding of coercion episodes attributing outcomes to nuclear signaling versus conventional and diplomatic factors; archival release of decision-side assessments of whether threats were believed.',
    'If nuclear signaling reliably coerces, the incredibility premise weakens and this reading collapses toward the rational_dropout sibling; if signaling succeeds only where the threatener retains escalation control the audience doubts, the incredibility axiom holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(credibility_evidence_status, empirical, 'Whether the incredibility thesis survives contact with observed coercive-signaling outcomes.').

omega_variable(
    taboo_function_vs_performance,
    'How much of the declaratory ''unthinkability'' layer is sincere norm construction that functionally constrains leaders, versus rhetorical maintenance covering operational usability-seeking?',
    'Behavioral test: examine rejected nuclear-use proposals (Korea, Vietnam, Gulf War, Ukraine-support deliberations) for whether normative aversion or anticipated consequences did the constraining, and track whether declared taboos survive the next serious crisis.',
    'If the taboo functionally binds, the theater_ratio attributed to this arrangement falls and a stronger coordination account wins; if it is purely rhetorical, this reading''s instability diagnosis is confirmed and usability-seeking is unconstrained.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(taboo_function_vs_performance, empirical, 'Attribution of the visible declaratory layer between functional norm and cover performance.').

omega_variable(
    establishment_identity_lock_reversibility,
    'Is the identity fusion binding the strategic commands and the intellectual establishment to the credibility framework reversible through generational and cohort turnover, or self-perpetuating?',
    'Track doctrinal revision across cohort replacement: whether officers and analysts formed after the Cold War revise usability assumptions at measurable rates, or reproduce the received framework through promotion gates, curricula, and revolving-door placement.',
    'If the lock is reversible, the arrangement could shed its degraded strands within one professional generation and its persistence is contingent; if self-perpetuating, extraction persists regardless of external evidence and inertia becomes the load-bearing wall.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(establishment_identity_lock_reversibility, empirical, 'Whether professional and organizational identity fusion is a transient or permanent feature of the arrangement.').

omega_variable(
    public_quiescence_mechanism,
    'Is the observed quiescence of arsenal-state publics toward launch authority and posture decisions primarily structural (secrecy, classification, complexity, absence of any electoral lever) or internalized (fatalism and learned helplessness that would survive disclosure)?',
    'Post-disclosure attitude tracking: compare concern and mobilization after declassified near-miss revelations and leaked posture documents against baseline; if revealed risk durably raises demand for control reforms, the structural share dominates.',
    'An internalized component means suppression travels with the public even after transparency reforms, inflating effective suppression beyond what secrecy legislation alone measures and slowing any reform pathway.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(public_quiescence_mechanism, empirical, 'Structural versus internalized composition of public-side suppression.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nuclear_impossibility_kernel__credibility_paradox_reading, 1947, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nucl_tr_t1947, nuclear_impossibility_kernel__credibility_paradox_reading, theater_ratio, 1947, 0.25).
narrative_ontology:measurement(nucl_tr_t1955, nuclear_impossibility_kernel__credibility_paradox_reading, theater_ratio, 1955, 0.32).
narrative_ontology:measurement(nucl_tr_t1962, nuclear_impossibility_kernel__credibility_paradox_reading, theater_ratio, 1962, 0.4).
narrative_ontology:measurement(nucl_tr_t1972, nuclear_impossibility_kernel__credibility_paradox_reading, theater_ratio, 1972, 0.44).
narrative_ontology:measurement(nucl_tr_t1983, nuclear_impossibility_kernel__credibility_paradox_reading, theater_ratio, 1983, 0.46).
narrative_ontology:measurement(nucl_tr_t1991, nuclear_impossibility_kernel__credibility_paradox_reading, theater_ratio, 1991, 0.38).
narrative_ontology:measurement(nucl_tr_t2000, nuclear_impossibility_kernel__credibility_paradox_reading, theater_ratio, 2000, 0.5).
narrative_ontology:measurement(nucl_tr_t2010, nuclear_impossibility_kernel__credibility_paradox_reading, theater_ratio, 2010, 0.52).
narrative_ontology:measurement(nucl_tr_t2019, nuclear_impossibility_kernel__credibility_paradox_reading, theater_ratio, 2019, 0.6).
narrative_ontology:measurement(nucl_tr_t2025, nuclear_impossibility_kernel__credibility_paradox_reading, theater_ratio, 2025, 0.63).

% Extraction over time
narrative_ontology:measurement(nucl_be_t1947, nuclear_impossibility_kernel__credibility_paradox_reading, base_extractiveness, 1947, 0.45).
narrative_ontology:measurement(nucl_be_t1955, nuclear_impossibility_kernel__credibility_paradox_reading, base_extractiveness, 1955, 0.55).
narrative_ontology:measurement(nucl_be_t1962, nuclear_impossibility_kernel__credibility_paradox_reading, base_extractiveness, 1962, 0.6).
narrative_ontology:measurement(nucl_be_t1972, nuclear_impossibility_kernel__credibility_paradox_reading, base_extractiveness, 1972, 0.57).
narrative_ontology:measurement(nucl_be_t1983, nuclear_impossibility_kernel__credibility_paradox_reading, base_extractiveness, 1983, 0.62).
narrative_ontology:measurement(nucl_be_t1991, nuclear_impossibility_kernel__credibility_paradox_reading, base_extractiveness, 1991, 0.49).
narrative_ontology:measurement(nucl_be_t2000, nuclear_impossibility_kernel__credibility_paradox_reading, base_extractiveness, 2000, 0.54).
narrative_ontology:measurement(nucl_be_t2010, nuclear_impossibility_kernel__credibility_paradox_reading, base_extractiveness, 2010, 0.57).
narrative_ontology:measurement(nucl_be_t2019, nuclear_impossibility_kernel__credibility_paradox_reading, base_extractiveness, 2019, 0.64).
narrative_ontology:measurement(nucl_be_t2025, nuclear_impossibility_kernel__credibility_paradox_reading, base_extractiveness, 2025, 0.67).

% Suppression requirement over time
narrative_ontology:measurement(nucl_su_t1947, nuclear_impossibility_kernel__credibility_paradox_reading, suppression_requirement, 1947, 0.35).
narrative_ontology:measurement(nucl_su_t1955, nuclear_impossibility_kernel__credibility_paradox_reading, suppression_requirement, 1955, 0.55).
narrative_ontology:measurement(nucl_su_t1962, nuclear_impossibility_kernel__credibility_paradox_reading, suppression_requirement, 1962, 0.58).
narrative_ontology:measurement(nucl_su_t1972, nuclear_impossibility_kernel__credibility_paradox_reading, suppression_requirement, 1972, 0.5).
narrative_ontology:measurement(nucl_su_t1983, nuclear_impossibility_kernel__credibility_paradox_reading, suppression_requirement, 1983, 0.6).
narrative_ontology:measurement(nucl_su_t1991, nuclear_impossibility_kernel__credibility_paradox_reading, suppression_requirement, 1991, 0.42).
narrative_ontology:measurement(nucl_su_t2000, nuclear_impossibility_kernel__credibility_paradox_reading, suppression_requirement, 2000, 0.5).
narrative_ontology:measurement(nucl_su_t2010, nuclear_impossibility_kernel__credibility_paradox_reading, suppression_requirement, 2010, 0.52).
narrative_ontology:measurement(nucl_su_t2019, nuclear_impossibility_kernel__credibility_paradox_reading, suppression_requirement, 2019, 0.62).
narrative_ontology:measurement(nucl_su_t2025, nuclear_impossibility_kernel__credibility_paradox_reading, suppression_requirement, 2025, 0.64).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nuclear_impossibility_kernel__credibility_paradox_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(nuclear_impossibility_kernel__credibility_paradox_reading, structural_contraction_reading).
narrative_ontology:affects_constraint(nuclear_impossibility_kernel__credibility_paradox_reading, rational_dropout_reading).

% DUAL FORMULATION NOTE:
% Constraint-family note: the colloquial label 'nuclear weapons made war impossible' decomposes into three structurally distinct claims with different epsilon values — this file (credibility paradox: the barrier is rhetorical, hence erodible, and usability-seeking keeps war reachable), structural_contraction_reading (physical impossibility of victory), and rational_dropout_reading (economic dominance of costs over achievable benefit). Each is authored as its own story with its own beneficiaries, victims, and metrics; they are linked pairwise through network edges and typed reading_relations rather than merged, because a single story would need a measurement-dependent epsilon, violating invariance. Upstream/downstream structure: this reading exerts downstream pressure on the rational_dropout_reading, since its instability diagnosis is what generates the usable-option landscape the cost-benefit sibling then evaluates; it competes laterally with the contraction reading without foreclosing it.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
