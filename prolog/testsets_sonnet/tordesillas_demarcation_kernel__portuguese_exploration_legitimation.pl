% ============================================================================
% CONSTRAINT STORY: tordesillas_demarcation_kernel__portuguese_exploration_legitimation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
 *   constraint_id: tordesillas_demarcation_kernel__portuguese_exploration_legitimation
 *   human_readable: Tordesillas Line as Confirmation of Portuguese Exploration Rights and Exclusion of Rivals East of the Meridian
 *   domain: international_law/colonial_history/sovereignty_theory
 *
 * SUMMARY:
 *   This story instantiates the Portuguese-facing reading of the Tordesillas
 *   kernel: the 1494 treaty (and its papal antecedent, Inter Caetera 1493)
 *   treated as confirmation of prior Portuguese exploration investment and as
 *   a mechanism excluding rival European crowns from the eastern trade
 *   sphere. The extraction here runs through trade monopoly and naval
 *   interdiction of rival shipping, not through land conquest or indigenous
 *   subjugation — that is a structurally distinct claim, authored separately
 *   as the sibling reading (spanish_conquest_legitimation), where the
 *   beneficiary is the Castilian conquest apparatus and the victim set is
 *   indigenous American populations. The two readings share one kernel (the
 *   meridian line and papal sanction) but diverge sharply in beneficiary,
 *   victim set, and extraction mechanism, which is exactly why they are
 *   authored as separate constraints per the ε-invariance principle rather
 *   than as one story with a measurement parameter.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, 0.58).
domain_priors:suppression_score(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, 0.62).
domain_priors:theater_ratio(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, extractiveness, 0.58).
narrative_ontology:constraint_metric(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, tangled_rope).
narrative_ontology:human_readable(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, "Tordesillas Line as Confirmation of Portuguese Exploration Rights and Exclusion of Rivals East of the Meridian").
narrative_ontology:topic_domain(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, "international_law/colonial_history/sovereignty_theory").

domain_priors:requires_active_enforcement(tordesillas_demarcation_kernel__portuguese_exploration_legitimation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, '1e9df4c3-d725-4d3d-81d1-fd686b628a6c').
narrative_ontology:cs_kernel_codification('1e9df4c3-d725-4d3d-81d1-fd686b628a6c', formalized).
narrative_ontology:cs_authority_grounding('1e9df4c3-d725-4d3d-81d1-fd686b628a6c', lineage).
narrative_ontology:cs_interpretation_layer_present('1e9df4c3-d725-4d3d-81d1-fd686b628a6c').
narrative_ontology:cs_reading_relation('1e9df4c3-d725-4d3d-81d1-fd686b628a6c', tordesillas_demarcation_kernel__spanish_conquest_legitimation, coexists_with).
narrative_ontology:cs_axiom('1e9df4c3-d725-4d3d-81d1-fd686b628a6c', foundational, exploration_priority_confers_exclusive_trade_right).
narrative_ontology:cs_axiom_status(exploration_priority_confers_exclusive_trade_right, holdable).
narrative_ontology:cs_axiom_grounding('1e9df4c3-d725-4d3d-81d1-fd686b628a6c', exploration_priority_confers_exclusive_trade_right, conventional).
narrative_ontology:cs_axiom('1e9df4c3-d725-4d3d-81d1-fd686b628a6c', secondary, papal_adjudication_binds_only_consenting_catholic_crowns).
narrative_ontology:cs_axiom_status(papal_adjudication_binds_only_consenting_catholic_crowns, overridden).
narrative_ontology:cs_axiom_grounding('1e9df4c3-d725-4d3d-81d1-fd686b628a6c', papal_adjudication_binds_only_consenting_catholic_crowns, theological).
narrative_ontology:cs_reference_frame('1e9df4c3-d725-4d3d-81d1-fd686b628a6c', papal_temporal_donation_authority).
narrative_ontology:cs_drift_state('1e9df4c3-d725-4d3d-81d1-fd686b628a6c', post_grotius_freedom_of_seas_era, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('1e9df4c3-d725-4d3d-81d1-fd686b628a6c', '').
narrative_ontology:cs_kernel_id(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, tordesillas_demarcation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, portuguese_crown).
narrative_ontology:constraint_beneficiary(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, estado_da_india_trading_networks).
narrative_ontology:constraint_beneficiary(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, papal_curia).
narrative_ontology:constraint_victim(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, castilian_atlantic_traders).
narrative_ontology:constraint_victim(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, english_crown_expeditions).
narrative_ontology:constraint_victim(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, french_crown_expeditions).
narrative_ontology:constraint_victim(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, dutch_merchant_ventures).
narrative_ontology:constraint_vindicates(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, papal_donation_doctrine).
narrative_ontology:constraint_vindicates(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, prior_discovery_confers_title).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Negotiated the 1494 treaty line at 370 leagues west of Cape Verde, converting decades of prior exploration along the African coast and into the Indian Ocean into a papally sanctioned exclusive sphere. Enforces the line with naval patrols (Carreira da India armed convoys) against interloping ships and treats any European vessel trading east of the line without license as a trespasser to be seized or sunk.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, portuguese_crown, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, portuguese_crown, beneficiary).

% Merchant consortia and crown factors operating the spice and pepper trade from Goa to Lisbon. They collect monopoly rents on cinnamon, pepper, and spice cargoes precisely because rival European buyers are treaty-barred from establishing competing factories along the same routes; their profit margin is a direct function of the exclusion's enforcement.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, estado_da_india_trading_networks, beneficiary,
    organized, biographical, mobile, continental).

% Issues the bulls (Inter Caetera, and brokers the subsequent treaty) that convert temporal exploration claims into spiritually sanctioned title, extending papal authority over the disposition of newly encountered lands and seas. Collects deference and tribute obligations (missionary patronage, ecclesiastical appointment rights under padroado) in exchange for adjudicating the line.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, papal_curia, beneficiary,
    institutional, civilizational, analytical, universal).
narrative_ontology:stakeholder_secondary_role(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, papal_curia, agenda_setter).

% Barred from the eastern route to the Indies by the same treaty their own crown co-signed; forced instead toward westward and Pacific routes (eventually the Magellan-Elcano circumnavigation to reach the Spice Islands from the other direction), at far greater cost and risk, purely to respect a demarcation their commercial interest did not want.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, castilian_atlantic_traders, payer,
    organized, biographical, constrained, continental).

% Not party to the treaty at all; English monarchs from Henry VII onward explicitly reject the papal division as binding on non-signatories, yet English ships operating east of the line face Portuguese naval interdiction as though the bull carried universal force. Their only recourse is unilateral defiance (privateering, later chartered companies) rather than legal remedy, since the adjudicating authority (the Pope) is also a co-author of the exclusion.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, english_crown_expeditions, payer,
    powerful, biographical, trapped, global).

% Francis I's famous demand to see 'the clause in Adam's will' that divided the world between Portugal and Castile captures the structural bind: French traders and explorers are excluded from a sphere they never agreed to, adjudicated by a religious authority whose ruling favors the two Iberian claimants who funded and lobbied for it.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, french_crown_expeditions, payer,
    powerful, biographical, trapped, global).

% Later entrants (16th century) facing an entrenched exclusion regime they had no part in negotiating; their eventual response is armed commercial rivalry (VOC) rather than appeal to the treaty's legitimacy, since the demarcation offers them no forum for redress.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, dutch_merchant_ventures, payer,
    organized, biographical, trapped, global).

% Swahili coast city-states, Gujarati merchant networks, and Indian Ocean port polities whose existing trade relationships and sovereignty are entirely absent from a treaty negotiated between two European crowns and a European pope over a line running through the Atlantic. They are not consulted, not named, and not considered parties, though the line's downstream enforcement (Portuguese fortresses, cartaz licensing system) directly restructures their commerce.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, east_african_and_indian_ocean_polities, excluded,
    organized, generational, trapped, regional).

% Study the treaty as a foundational case in the law of nations debate over discovery-based title, later cited (and largely rejected) in the development of freedom-of-the-seas doctrine (Grotius's Mare Liberum was written in direct response to Portuguese claims of exclusive navigation rights grounded in this very demarcation).
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, maritime_law_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, portuguese_crown).
narrative_ontology:fixing_cost_class(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, third-party-adjudicated rule (a meridian line, papally ratified) that lets Portugal and Castile avoid direct naval war over overlapping exploration claims by assigning each an exclusive sphere — a real coordination problem between two rival crowns with overlapping fleets and no other mutually trusted arbiter.
% TRANSFER_FUNCTION: Moves exclusive trading and navigation rights east of the line to Portugal (and the correlative exclusion of every other European maritime power from that same sphere), converting Portugal's decades of exploration investment (Henry the Navigator's voyages, the Cape route) into monopoly rent on the spice trade, backed by naval interdiction against interlopers.
% ABSENT_VOICES: English, French, and Dutch crowns were never signatories and explicitly rejected papal authority to bind non-Catholic or non-consulted powers to the division; Indian Ocean and East African polities whose ports, markets, and sovereignty the line's enforcement directly reorganized were not represented in the negotiation at all.
% DISAPPEARANCE_RATIONALE: Without the treaty's confirmation, Portugal's claim to exclusivity east of the line rests only on unilateral assertion and naval force — rival powers would have entered the Cape and Indian Ocean routes decades earlier without the (weak, but real) deterrent of papal sanction, altering the timing and structure of European entry into Asian trade and very likely accelerating Anglo-Dutch commercial rivalry with Portugal.
% FOUNDING_PROBLEM: Two Iberian crowns had overlapping exploration claims following Columbus's 1492 voyage and decades of prior Portuguese Atlantic and African exploration; without an arbiter, the overlap risked direct war between Portugal and Castile over navigation and trade rights.
% FOUNDING_PROBLEM_CORROBORATION: Contemporary rival crowns (England under Henry VII, France under Francis I) attested from outside the beneficiary set that the papal division carried no binding force over non-signatories, undercutting the claim that a genuine dispute-resolution problem for ALL parties was being solved rather than a bilateral Iberian settlement dressed as universal law; later jurists (Grotius, writing for the Dutch East India Company) formalized this outside critique into the freedom-of-the-seas doctrine that eventually displaced the demarcation entirely.
narrative_ontology:disappearance_verdict(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, world_rearranges).
narrative_ontology:founding_problem_status(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, 'none', 1).
narrative_ontology:epsilon_provenance(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, 0.58, 'claude-sonnet-5', 'none', direct).

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
 *   Extraction (0.58 at interval end) reflects sustained monopoly rent on the spice trade, decoupled from any service Portugal renders to excluded rivals — the treaty gives Portugal no obligation toward England, France, or the Dutch beyond keeping them out. Suppression (0.62) is high because the exclusion depends on active naval interdiction (Carreira convoys, cartaz licensing, occasional seizure of interloping vessels) rather than voluntary rival forbearance; it is NOT scaled by scope in the underlying metric even though the constraint's spatial scope is global. Theater ratio is modest but rising (0.28 by 1600) as enforcement increasingly serves to maintain a legal fiction of exclusivity that rival powers no longer respect in practice — French and English ships trade east of the line with growing impunity by the late 16th century even as Portugal continues to assert the treaty's force diplomatically.
 *
 * PERSPECTIVAL GAP:
 *   From the Portuguese/papal seat, the demarcation is a legitimate, negotiated resolution of a real dispute-risk between two Catholic crowns. From the excluded powers' seat, the same instrument is a self-dealing arrangement in which two claimants and their shared religious authority jointly declared a monopoly over a hemisphere neither had lawfully surveyed nor previously governed. The engine computes these as different seat-level classifications from the same structural data; the divergence is the analytical payload, not an error to reconcile.
 *
 * DIRECTIONALITY LOGIC:
 *   Portugal and its trading networks sit near the full-beneficiary end: they collect the monopoly rent and administer the enforcement. The Papal Curia is also a beneficiary — it collects deference to its adjudicating authority and patronage rights (padroado) as the price of its ruling. Castilian Atlantic traders are a partial victim: their own crown co-signed the treaty, so their exclusion from the eastern sphere is a self-imposed cost of the coordination that also grants them the western sphere — hence their d sits closer to symmetric than the non-signatory powers. English, French, and Dutch traders sit at the full-target end: trapped exit options, no voice in the adjudication, and direct naval interdiction when they attempt to trade in the excluded sphere.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (avoiding direct Iberian war over overlapping claims) was real in 1494 but became obsolete once non-Iberian powers began operating east of the line regardless of papal sanction — by the mid-16th century the treaty's coordination function (preventing Iberian conflict) persists, but its exclusionary function (keeping non-Iberian rivals out) increasingly requires force rather than consent, which is the tangled-rope signature: real coordination between two original parties, layered with extraction from parties who never consented to the coordination and bear its cost.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    papal_authority_vs_bilateral_treaty,
    'Does the demarcation''s binding force derive from papal spiritual authority (Inter Caetera) or from the bilateral treaty instrument itself (Tordesillas, negotiated directly between Portugal and Castile without papal mediation of the final line)?',
    'Textual and diplomatic-historical analysis of whether non-signatory powers'' rejections targeted the papal bull specifically or the treaty as an inter-crown compact; examine whether Portugal''s later diplomatic appeals invoked papal sanction or treaty reciprocity with Castile.',
    'If the binding claim rests on papal authority, the constraint is a commitment-system kernel reading proper (authority_grounding: lineage/extraction via the Church); if it rests purely on bilateral treaty consent, the papal role is closer to ceremonial ratification and the kernel''s authority structure shifts toward a diplomatic convention with much weaker claim to bind non-parties.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(papal_authority_vs_bilateral_treaty, conceptual, 'Whether the demarcation''s authority is fundamentally papal or fundamentally bilateral-diplomatic.').

omega_variable(
    exploration_priority_vs_papal_grant,
    'Is Portugal''s exclusion right grounded in prior demonstrated exploration and infrastructure investment (Henry the Navigator''s decades of Atlantic voyages, the established Cape route) or purely in the papal/treaty grant considered independent of any prior activity?',
    'Compare the treaty''s negotiating record: did Portuguese negotiators argue from investment/discovery priority, or did the treaty simply assign a line without reference to who had explored what? Examine whether the line''s specific placement (370 leagues, moved west from the original 100) tracked known Portuguese discoveries or was an arbitrary compromise.',
    'If grounded in genuine prior investment, the coordination function is stronger (rewarding first-mover exploration cost, akin to a property-rights-in-discovery regime) and the constraint leans more rope-like from Portugal''s seat; if the grant is decoupled from actual exploration priority, the ''confirmation of prior rights'' framing is closer to post-hoc legitimation of what force and negotiation had already secured, strengthening the tangled_rope/extraction reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exploration_priority_vs_papal_grant, empirical, 'Whether Portuguese exclusivity tracks genuine discovery priority or is decoupled legitimating rhetoric.').

omega_variable(
    sibling_reading_shared_kernel_divergent_epsilon,
    'Given that this reading and the spanish_conquest_legitimation sibling share the identical kernel text and papal authority structure, how much of the observed ε divergence (moderate trade-monopoly extraction here vs. severe conquest/subjugation extraction in the sibling) reflects genuine structural difference in what each crown actually did with its granted sphere, versus an artifact of which victim population each reading foregrounds?',
    'Compare documented enforcement mechanisms and victim outcomes on each side of the line over the same interval: Portuguese naval interdiction and licensing (cartaz system) against rival traders versus Castilian encomienda and territorial conquest against indigenous populations. If the mechanisms are genuinely different in kind (commercial exclusion vs. subjugation), the epsilon divergence is structurally warranted, not a framing artifact.',
    'If the divergence is warranted, the two-story decomposition per the ε-invariance principle is the correct authoring choice. If the divergence turns out to be an artifact of observer selection rather than structural difference, the two stories should be reconsidered as a single constraint with a contested victim set rather than two constraints.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_shared_kernel_divergent_epsilon, conceptual, 'Whether the Portuguese/Spanish reading split reflects genuine structural divergence or observer-dependent framing.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, 1494, 1600).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tord_tr_t1494, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, theater_ratio, 1494, 0.15).
narrative_ontology:measurement(tord_tr_t1510, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, theater_ratio, 1510, 0.18).
narrative_ontology:measurement(tord_tr_t1525, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, theater_ratio, 1525, 0.22).
narrative_ontology:measurement(tord_tr_t1550, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, theater_ratio, 1550, 0.26).
narrative_ontology:measurement(tord_tr_t1575, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, theater_ratio, 1575, 0.3).
narrative_ontology:measurement(tord_tr_t1600, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, theater_ratio, 1600, 0.28).

% Extraction over time
narrative_ontology:measurement(tord_be_t1494, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, base_extractiveness, 1494, 0.42).
narrative_ontology:measurement(tord_be_t1510, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, base_extractiveness, 1510, 0.48).
narrative_ontology:measurement(tord_be_t1525, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, base_extractiveness, 1525, 0.53).
narrative_ontology:measurement(tord_be_t1550, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, base_extractiveness, 1550, 0.57).
narrative_ontology:measurement(tord_be_t1575, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, base_extractiveness, 1575, 0.6).
narrative_ontology:measurement(tord_be_t1600, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, base_extractiveness, 1600, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(tord_su_t1494, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, suppression_requirement, 1494, 0.45).
narrative_ontology:measurement(tord_su_t1510, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, suppression_requirement, 1510, 0.52).
narrative_ontology:measurement(tord_su_t1525, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, suppression_requirement, 1525, 0.58).
narrative_ontology:measurement(tord_su_t1550, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, suppression_requirement, 1550, 0.64).
narrative_ontology:measurement(tord_su_t1575, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, suppression_requirement, 1575, 0.66).
narrative_ontology:measurement(tord_su_t1600, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, suppression_requirement, 1600, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, resource_allocation).
narrative_ontology:boltzmann_floor_override(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, 0.12).
narrative_ontology:affects_constraint(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, spanish_conquest_legitimation).

% DUAL FORMULATION NOTE:
% This constraint and spanish_conquest_legitimation are the two readings of the tordesillas_demarcation_kernel, sharing the same papal bull and meridian line but diverging in beneficiary (Portuguese trading crown vs. Castilian conquest apparatus), victim set (rival European powers vs. indigenous American populations), and extraction mechanism (trade monopoly vs. land/labor appropriation). Both stories declare the other in affects_constraints to preserve the kernel-family link; contamination or purity shifts in one reading's authority structure (e.g., erosion of papal donation doctrine) should propagate analytically to the other.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
