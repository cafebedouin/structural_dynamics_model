% ============================================================================
% CONSTRAINT STORY: tordesillas_demarcation_kernel__portuguese_exploration_legitimation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_tordesillas_demarcation_kernel__portuguese_exploration_legitimation, []).

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
 *   constraint_id: tordesillas_demarcation_kernel__portuguese_exploration_legitimation
 *   human_readable: Tordesillas Demarcation — Portuguese Exploration-Legitimation Reading
 *   domain: international law/colonial history/sovereignty theory
 *
 * SUMMARY:
 *   After Columbus's landfalls created overlapping Iberian claims, Portuguese
 *   diplomacy secured first the 1493 papal bulls and then the 1494 bilateral
 *   treaty moving the demarcation line to 370 leagues west of Cape Verde.
 *   This story authors the arrangement as the Portuguese reading holds it:
 *   the papal-treaty complex CONFIRMS rights Portugal had already earned
 *   through decades of African-coast exploration and protects its eastern
 *   route, while licensing exclusion of European rivals from the zone east of
 *   the line. Operationally the arrangement ran through the Estado da India:
 *   a fortress chain (Goa, Malacca, Hormuz), naval patrols, and the cartaz
 *   safe-conduct system. The declared victim set is the rival European
 *   maritime powers — France, England, the Netherlands — bound by an
 *   instrument they never signed; Castile sits on the beneficiary side as
 *   co-signatory. This file belongs to a decomposed constraint family: the
 *   demarcation commitment splits into this reading and a Spanish
 *   conquest-legitimation sibling with a different victim set and different
 *   epsilon (see kernel_context and network.dual_formulation_note). Claim and
 *   metrics are independent authored facts: claimed_type tangled_rope states
 *   the structure I believe true (genuine bilateral coordination plus
 *   asymmetric third-party extraction plus active enforcement); the metrics
 *   state what I believe descriptively occurred.
 *
 * KEY AGENTS:
 *   - portuguese_crown: agenda-setter and principal beneficiary (institutional/arbitrage) — negotiated the line, directs enforcement, receives the monopoly revenues
 *   - estado_da_india: administering beneficiary (institutional/identity_locked) — runs the fortress chain, patrols, and cartaz licensing; its officers' careers fuse with the monopoly mission
 *   - castilian_crown: co-beneficiary of the bilateral settlement (institutional/mobile) — accepted eastern exclusion in exchange for western recognition
 *   - papal_see: warrant-granting beneficiary (institutional/constrained) — collects jurisdictional precedent from the donation framework
 *   - rival_european_maritime_powers: primary payers (powerful/constrained) — France, England, Netherlands, excluded without consent
 *   - asian_ocean_trading_networks: secondary payers under the licensing machinery (organized/constrained)
 *   - venetian_mamluk_intermediaries: excluded voices (powerful/trapped) — Levant spice intermediation displaced by the Cape route
 *   - international_law_historians: analytical observer (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, 0.56).
domain_priors:suppression_score(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, 0.68).
domain_priors:theater_ratio(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, extractiveness, 0.56).
narrative_ontology:constraint_metric(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, tangled_rope).
narrative_ontology:human_readable(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, "Tordesillas Demarcation — Portuguese Exploration-Legitimation Reading").
narrative_ontology:topic_domain(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, "international law/colonial history/sovereignty theory").

domain_priors:requires_active_enforcement(tordesillas_demarcation_kernel__portuguese_exploration_legitimation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, '45637709-1d32-42a1-9c0a-43ddfdc8b196').
narrative_ontology:cs_kernel_codification('45637709-1d32-42a1-9c0a-43ddfdc8b196', fixed_text).
narrative_ontology:cs_authority_grounding('45637709-1d32-42a1-9c0a-43ddfdc8b196', lineage).
narrative_ontology:cs_interpretation_layer_present('45637709-1d32-42a1-9c0a-43ddfdc8b196').
narrative_ontology:cs_reading_relation('45637709-1d32-42a1-9c0a-43ddfdc8b196', tordesillas_demarcation_kernel__spanish_conquest_legitimation, influences).
narrative_ontology:cs_axiom('45637709-1d32-42a1-9c0a-43ddfdc8b196', foundational, prior_exploration_confers_exclusive_title).
narrative_ontology:cs_axiom_status(prior_exploration_confers_exclusive_title, holdable).
narrative_ontology:cs_axiom_grounding('45637709-1d32-42a1-9c0a-43ddfdc8b196', prior_exploration_confers_exclusive_title, conventional).
narrative_ontology:cs_axiom('45637709-1d32-42a1-9c0a-43ddfdc8b196', foundational, papal_warrant_binds_all_christian_powers).
narrative_ontology:cs_axiom_status(papal_warrant_binds_all_christian_powers, overridden).
narrative_ontology:cs_axiom_grounding('45637709-1d32-42a1-9c0a-43ddfdc8b196', papal_warrant_binds_all_christian_powers, theological).
narrative_ontology:cs_axiom('45637709-1d32-42a1-9c0a-43ddfdc8b196', secondary, enforced_exclusion_of_unconsenting_rivals_is_lawful).
narrative_ontology:cs_axiom_status(enforced_exclusion_of_unconsenting_rivals_is_lawful, holdable).
narrative_ontology:cs_axiom_grounding('45637709-1d32-42a1-9c0a-43ddfdc8b196', enforced_exclusion_of_unconsenting_rivals_is_lawful, conventional).
narrative_ontology:cs_reference_frame('45637709-1d32-42a1-9c0a-43ddfdc8b196', prior_rights_confirmation_framework).
narrative_ontology:cs_drift_state('45637709-1d32-42a1-9c0a-43ddfdc8b196', post_reformation_challenge, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('45637709-1d32-42a1-9c0a-43ddfdc8b196', '').
narrative_ontology:cs_kernel_id(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, tordesillas_demarcation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, portuguese_crown).
narrative_ontology:constraint_beneficiary(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, estado_da_india).
narrative_ontology:constraint_beneficiary(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, castilian_crown).
narrative_ontology:constraint_beneficiary(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, papal_see).
narrative_ontology:constraint_victim(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, rival_european_maritime_powers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, asian_ocean_trading_networks).
narrative_ontology:constraint_vindicates(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, papal_donation_doctrine).
narrative_ontology:constraint_vindicates(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, prior_discovery_title).
narrative_ontology:constraint_vindicates(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, exclusive_sphere_allocation_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds the treaty title and directs the regime: negotiated the line's placement, commissions the fleet and fortress network, licenses the India-run convoys, and receives the customs duties, monopoly margins, and licensing fees the arrangement yields. Its exit is rewriting or renegotiating the terms, since it authored them.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, portuguese_crown, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, portuguese_crown, beneficiary).

% The viceregal administration headquartered at Goa: runs the fortresses at Goa, Malacca, and Hormuz, patrols the sea lanes, issues cartaz safe-conducts, seizes unlicensed shipping, and staffs the monopoly factories. Officers' careers, pensions, and honor are bound to the monopoly mission; repeated proposals to abandon the fortress chain for open factory trade read internally as betrayal of the enterprise rather than as strategy.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, estado_da_india, beneficiary,
    institutional, generational, identity_locked, continental).

% Co-signed the settlement: accepts exclusion from the eastern route in exchange for recognition of its western claims and relief from Iberian naval rivalry. It expands westward and, over time, treats the eastern line as binding mainly where enforcement actually reaches.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, castilian_crown, beneficiary,
    institutional, generational, mobile, global).

% Grants and ratifies the warrants (Inter caetera, Dudum siquidem, Julius II's 1506 ratification), collecting jurisdictional precedent: the donation doctrine asserts papal authority to allocate newly encountered non-Christian lands and routes. Its authority depends on Catholic monarchs continuing to seek its warrants, which the Reformation progressively undermines.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, papal_see, beneficiary,
    institutional, civilizational, constrained, continental).

% France, England, and the Netherlands: never party to the treaty, yet subject to its enforcement when their ships enter the eastern zone. Their alternatives — overland Levant trade, searching for other passages, privateering — are costly and slow to mature; within this interval their eastern commerce runs through smuggling, corsairing, and captured carriage.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, rival_european_maritime_powers, payer,
    powerful, biographical, constrained, global).

% Gujarati, Arab, Persian, Malay, and Chinese merchants operating the Indian Ocean carrying trade: they may purchase cartaz passes, pay port and passage dues at Portuguese forts, or risk seizure of ship and cargo. Some shift routes and seasons to evade patrols; most treat the fees as one more toll among many.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, asian_ocean_trading_networks, payer,
    organized, biographical, constrained, regional).

% Venice and the Mamluk sultanate profit from the Levant-Alexandria spice intermediation that the Cape route bypasses. Neither was consulted in the negotiation; both protest, mount or contemplate naval responses, and watch their margins erode as the route matures.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, venetian_mamluk_intermediaries, excluded,
    powerful, biographical, trapped, continental).

% Scholars of sovereignty and the law of nations: reconstruct the negotiations, weigh the warrant's legal force against balance-of-power reality, and trace the demarcation's afterlife through mare clausum debates to the 1750 Treaty of Madrid.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, international_law_historians, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, portuguese_crown).
narrative_ontology:fixing_cost_class(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Settles, without war between the signatories, which crown may exploit which oceanic zone: converts overlapping claims left by Columbus's landfalls into mutually recognized spheres, shields Portugal's decades of African-route investment from free-riding Castilian expeditions, and gives both crowns a cheap, legible title to police against each other.
% TRANSFER_FUNCTION: Moves exclusive commercial access to the eastern sea route from all European comers to the two signatory crowns; moves spice-trade margins, convoy customs, cartaz licensing fees, and seized cargoes from rival European entrants and levied Asian shippers to the Portuguese crown and its Indian Ocean administration.
% ABSENT_VOICES: Non-party European powers — France, England, the Netherlands — are bound by an instrument they never signed; Francis I's demand to be shown Adam's will voices their objection from outside the room. The Asian polities whose waters the arrangement purports to license are absent entirely; Venice and the Mamluk sultanate, whose intermediation the route displaces, learned of the settlement as accomplished fact.
% DISAPPEARANCE_RATIONALE: Overnight disappearance reopens the Castile-Portugal dispute a generation early, exposes the India-run convoys to immediate rival expedition, collapses cartaz revenue and the fortress-chain financing, and pulls Dutch, English, and French competition into the Indian Ocean decades ahead of schedule — the Estado da India's entire revenue architecture rearranges around open access.
% FOUNDING_PROBLEM: Two Catholic crowns faced war over overlapping claims after 1492: Portugal needed its ten-year African-Indian exploration program shielded from free-riding; Castile needed its western discoveries secured; both preferred a divinely warranted, arbitration-style division to a naval war neither could afford while also fighting infidel powers and financing fleets.
% FOUNDING_PROBLEM_CORROBORATION: Venetian envoy dispatches of 1493 (Trevisan's report on the panic and subsequent negotiation) corroborate the founding crisis from outside both courts. Francisco de Vitoria's Salamanca lectures (1539), delivered from outside the beneficiary set, attest that the papal-warrant solution was already contested on its own terms. The 1580 dynastic union of the crowns and the 1750 Treaty of Madrid's explicit replacement of the line attest the founding problem's death; no party outside the arrangement's administrators defends the original problem as still live.
narrative_ontology:disappearance_verdict(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, world_rearranges).
narrative_ontology:founding_problem_status(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, 'none', 1).
narrative_ontology:epsilon_provenance(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, 0.56, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(tordesillas_demarcation_kernel__portuguese_exploration_legitimation_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(tordesillas_demarcation_kernel__portuguese_exploration_legitimation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Epsilon is moderate (0.56 at interval end) because two things are simultaneously true: the settlement genuinely solved a war-avoidance and investment-protection problem between the signatories, and it transferred enormous spice-route rents from unconsenting third parties to Lisbon. Suppression (0.68) reflects that persistence depended on active naval exclusion — patrols, seizures, the fortress chain — not on voluntary compliance by those excluded; suppression is authored as a raw structural property and is not scaled by power or scope anywhere in this story. Theater ratio (0.30 and rising) tracks the growing gap between the papal-ceremonial layer, which performed less work as Protestant and Gallican challengers dismissed it, and the treaty-enforcement layer, which remained functional throughout. Accessibility collapse is partial (0.45): overland Levant trade, westward expansion, and eventual passage-searches kept alternatives alive for the excluded. Resistance (0.60) is substantial — corsair warfare, state-sponsored interlopers, and the beginnings of the Dutch challenge. The measurement series run on one shared six-point grid (t=0..100, i.e., 1494-1594) with all three metrics authored at every point; extractiveness peaks mid-interval as the Albuquerque-era monopoly consolidates, then leaks as smuggling and rival probes grow.
 *
 * PERSPECTIVAL GAP:
 *   The payer and beneficiary seats compute differently from the same structure. From the Portuguese and papal seats the arrangement is legitimate confirmation of earned rights and a cheap substitute for Iberian war; from the rival-power seats the same instrument is pure external imposition — a line drawn over them by others' authority. Same-level divergence is sharpest between Castile and France: both were great powers facing the same line, but Castile's signature purchased western security and mutual recognition while France's non-consent left it bearing the full cost of exclusion with none of the settlement's protections. Exit asymmetry follows the same fault lines: Portugal holds arbitrage-grade exit (it authored the terms), Castile is mobile (its sphere lay west), the rivals are constrained (alternative routes were costly and unproven within this interval), and the Asian carrying trade is constrained between paying cartaz dues and risking seizure.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries (Portuguese crown, Estado da India, Castilian crown, papal see) derive low directionality — the arrangement subsidizes them; the declared victim set (rival European maritime powers) derives high directionality amplified by their constrained exit. One override is authored: the organized-power atom is set to d=0.78 because the only organized-power seat in this story is asian_ocean_trading_networks, which bears real levies under threat of seizure but is deliberately NOT placed in the declared victim set — the kernel's structural delta for this reading fixes the victim set as the European rivals, and the Asian-side licensing extraction is flagged (omega cartaz_extraction_boundary) as a candidate separate constraint. The override corrects what the derivation would otherwise miss: an undeclared payer would fall to canonical fallback despite sitting near the full-target end structurally.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — war-avoidance between the Iberian crowns and protection of sunk exploration investment — was genuinely solved, and died as a live problem with the 1580 dynastic union; the arrangement nonetheless persisted formally until the 1750 Treaty of Madrid. The tangled_rope classification is what prevents mislabeling here: calling the arrangement pure coordination ignores that its rents were extracted from parties who never consented; calling it pure extraction ignores the real bilateral settlement it delivered. Keeping both halves visible lets the lifecycle detector date the transition — the coordination half finished mid-interval while the extraction half continued — instead of flattening the genealogy into a single verdict. The founding_problem_status=dead combined with disappearance_verdict=world_rearranges is the mismatch signature of a regime whose extraction architecture outlived its reason.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    committer_reading_delta_vs_spanish_sibling,
    'This constraint is the portuguese_exploration_legitimation reading of the tordesillas_demarcation_kernel: what structural differences would instantiate the sibling spanish_conquest_legitimation reading?',
    'Author the sibling story against the same referent (the demarcation arrangement as operated) and compare victim sets, epsilon, and enforcement mode.',
    'The sibling reading shifts the victim set to indigenous populations, raises epsilon well above this reading''s moderate value, and pushes classification toward snare; this file''s metrics are valid only for the Portuguese reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_reading_delta_vs_spanish_sibling, conceptual, 'Kernel membership and cross-reading structural delta.').

omega_variable(
    cartaz_extraction_boundary,
    'Does this reading''s epsilon include the cartaz licensing levies on Asian Ocean shipping, or is that a structurally distinct arrangement warranting its own story?',
    'Decomposition test: if counting Asian-side levies moves epsilon materially, author a separate cartaz-licensing constraint story and link it via network edges.',
    'Including Asian-side levies raises epsilon toward 0.7 and adds victim seats; excluding them keeps the declared victim set purely European and epsilon moderate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cartaz_extraction_boundary, conceptual, 'Boundary between European-rival exclusion and Asian-shipping licensing levies.').

omega_variable(
    papal_warrant_operative_force,
    'How much of the arrangement''s binding force on non-party European powers came from the papal warrant itself rather than from Portuguese naval deterrence?',
    'Counterfactual comparison of enforcement episodes: French, English, and Dutch conduct toward the line as such versus toward Portuguese squadrons.',
    'If warrant force was negligible among third parties, the legitimation layer functions closer to cover, theater_ratio trends higher, and classification drifts toward the snare flank of tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(papal_warrant_operative_force, empirical, 'Relative causal weight of papal title versus naval deterrence.').

omega_variable(
    post_union_inertial_tail,
    'After the 1580 dynastic union merged the Iberian crowns, did the demarcation regime persist as inertial ceremony until the 1750 Treaty of Madrid replaced it?',
    'Extend temporal measurement beyond this interval''s end: track theater_ratio and enforcement activity from 1580 to 1750.',
    'An inertial tail would date the mandate-obsolescence transition precisely and support reading the regime''s final phase as performance maintained by bureaucratic habit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(post_union_inertial_tail, empirical, 'Lifecycle tail: inertia versus function after the founding problem died.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tord_tr_t0, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, theater_ratio, 0, 0.1).
narrative_ontology:measurement_basis(tord_tr_t0, observed).
narrative_ontology:measurement(tord_tr_t20, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, theater_ratio, 20, 0.14).
narrative_ontology:measurement_basis(tord_tr_t20, observed).
narrative_ontology:measurement(tord_tr_t40, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, theater_ratio, 40, 0.18).
narrative_ontology:measurement_basis(tord_tr_t40, observed).
narrative_ontology:measurement(tord_tr_t60, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, theater_ratio, 60, 0.22).
narrative_ontology:measurement_basis(tord_tr_t60, observed).
narrative_ontology:measurement(tord_tr_t80, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, theater_ratio, 80, 0.26).
narrative_ontology:measurement_basis(tord_tr_t80, observed).
narrative_ontology:measurement(tord_tr_t100, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, theater_ratio, 100, 0.3).
narrative_ontology:measurement_basis(tord_tr_t100, observed).

% Extraction over time
narrative_ontology:measurement(tord_be_t0, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, base_extractiveness, 0, 0.42).
narrative_ontology:measurement_basis(tord_be_t0, observed).
narrative_ontology:measurement(tord_be_t20, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, base_extractiveness, 20, 0.55).
narrative_ontology:measurement_basis(tord_be_t20, observed).
narrative_ontology:measurement(tord_be_t40, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, base_extractiveness, 40, 0.62).
narrative_ontology:measurement_basis(tord_be_t40, observed).
narrative_ontology:measurement(tord_be_t60, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, base_extractiveness, 60, 0.64).
narrative_ontology:measurement_basis(tord_be_t60, observed).
narrative_ontology:measurement(tord_be_t80, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, base_extractiveness, 80, 0.6).
narrative_ontology:measurement_basis(tord_be_t80, observed).
narrative_ontology:measurement(tord_be_t100, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, base_extractiveness, 100, 0.56).
narrative_ontology:measurement_basis(tord_be_t100, observed).

% Suppression requirement over time
narrative_ontology:measurement(tord_su_t0, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, suppression_requirement, 0, 0.45).
narrative_ontology:measurement_basis(tord_su_t0, observed).
narrative_ontology:measurement(tord_su_t20, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, suppression_requirement, 20, 0.62).
narrative_ontology:measurement_basis(tord_su_t20, observed).
narrative_ontology:measurement(tord_su_t40, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, suppression_requirement, 40, 0.7).
narrative_ontology:measurement_basis(tord_su_t40, observed).
narrative_ontology:measurement(tord_su_t60, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, suppression_requirement, 60, 0.72).
narrative_ontology:measurement_basis(tord_su_t60, observed).
narrative_ontology:measurement(tord_su_t80, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, suppression_requirement, 80, 0.7).
narrative_ontology:measurement_basis(tord_su_t80, observed).
narrative_ontology:measurement(tord_su_t100, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, suppression_requirement, 100, 0.68).
narrative_ontology:measurement_basis(tord_su_t100, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, resource_allocation).
narrative_ontology:affects_constraint(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, tordesillas_demarcation_kernel__spanish_conquest_legitimation).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'Tordesillas/Papal demarcation' conflates two structurally distinct claims and is decomposed per the epsilon-invariance principle. This story (portuguese_exploration_legitimation) authors the eastern-route reading: moderate epsilon, victim set = rival European maritime powers, extraction via trade-monopoly exclusion. The sibling (spanish_conquest_legitimation) authors the western reading: substantially higher epsilon, victim set = indigenous populations, extraction via territorial conquest. The papal bulls are the upstream common source feeding both; this reading is upstream-configurational for the sibling because the negotiated line location defines the sibling's territorial scope. A third candidate decomposition — the cartaz licensing system as its own constraint — is flagged in omega cartaz_extraction_boundary rather than forced into this file.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, organized, 0.78).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
