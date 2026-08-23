% ============================================================================
% CONSTRAINT STORY: tordesillas_demarcation_kernel__portuguese_exploration_legitimation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-13
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
 *   constraint_id: tordesillas_demarcation_kernel__portuguese_exploration_legitimation
 *   human_readable: Tordesillas Demarcation — Portuguese Exploration-Legitimation Reading (Eastern Traffic Exclusion)
 *   domain: international_law/colonial_history/sovereignty_theory
 *
 * SUMMARY:
 *   The 1494 Treaty of Tordesillas moved the papal demarcation line to 370
 *   leagues west of the Cape Verde islands, and the confirming instruments
 *   (Alexander VI's 1493 bulls, Julius II's 1506 ratification) gave Portugal
 *   an acknowledged eastern sphere. THIS STORY INSTANTIATES ONE READING ONLY:
 *   the Portuguese legitimation reading, under which the instrument confirms
 *   rights already earned through eight decades of exploration down Africa
 *   and into the Indian Ocean, and excludes European rivals from the
 *   Cape-route traffic. The arrangement operated as the Estado da India:
 *   chokepoint garrisons at Hormuz, Malacca, and Goa; the compulsory cartaz
 *   pass system; the annual carreira convoys; and the Casa da India's Lisbon
 *   monopoly sales. Its costs fell on rival European powers barred from the
 *   route and on Asian shippers compelled to buy passes; its proceeds accrued
 *   to the Portuguese crown, its administrators, and its creditors.
 *   Enforcement was naval and continuous; the arrangement contracted sharply
 *   after the Dutch and English broke through after 1595, persisting in
 *   reduced form at interval end. KEY AGENTS (by structural relationship): -
 *   portuguese_crown: Agenda-setting beneficiary (institutional/arbitrage) —
 *   negotiates the line, commissions the fleets, collects the customs and
 *   royal shares - papal_curia: Co-agenda-setter (institutional/constrained)
 *   — issues and interprets the confirming instruments; collects
 *   jurisdictional acknowledgment - estado_da_india_administrators: Operating
 *   beneficiary (organized/constrained) — runs the forts, passes, and
 *   convoys; takes salary and cargo shares - genoese_welser_financiers:
 *   Passive beneficiary (organized/mobile) — finances the voyages and holds
 *   the asientos; capital exits freely - venetian_spice_intermediaries: Payer
 *   (powerful/constrained) — displaced from the eastern supply they carried
 *   for centuries; Levant reroute is partial exit - castilian_crown: Payer
 *   (powerful/arbitrage) — barred from the eastern sphere; monetizes its
 *   claim by selling the Moluccas rights and redirects west -
 *   northern_european_interlopers: Payer (organized/mobile) — Dutch, English,
 *   French shipping treated as contraband; eventually builds its own route -
 *   asian_cartaz_subjects: Payer (organized/trapped) — Indian Ocean shippers
 *   who must purchase Portuguese passes to sail through garrisoned
 *   chokepoints - salamanca_school_jurists: Analytical observer
 *   (analytical/analytical) — audits from outside both courts whether the
 *   instruments ground any title at all
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, 0.55).
domain_priors:suppression_score(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, 0.62).
domain_priors:theater_ratio(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, extractiveness, 0.55).
narrative_ontology:constraint_metric(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, theater_ratio, 0.52).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, tangled_rope).
narrative_ontology:human_readable(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, "Tordesillas Demarcation — Portuguese Exploration-Legitimation Reading (Eastern Traffic Exclusion)").
narrative_ontology:topic_domain(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, "international_law/colonial_history/sovereignty_theory").

domain_priors:requires_active_enforcement(tordesillas_demarcation_kernel__portuguese_exploration_legitimation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, 'e2619568-054b-4c68-bcfc-8b980f478fc1').
narrative_ontology:cs_kernel_codification('e2619568-054b-4c68-bcfc-8b980f478fc1', fixed_text).
narrative_ontology:cs_authority_grounding('e2619568-054b-4c68-bcfc-8b980f478fc1', lineage).
narrative_ontology:cs_interpretation_layer_present('e2619568-054b-4c68-bcfc-8b980f478fc1').
narrative_ontology:cs_reading_relation('e2619568-054b-4c68-bcfc-8b980f478fc1', tordesillas_demarcation_kernel__spanish_conquest_legitimation, influences).
narrative_ontology:cs_axiom('e2619568-054b-4c68-bcfc-8b980f478fc1', foundational, exploration_effort_confirms_title).
narrative_ontology:cs_axiom_status(exploration_effort_confirms_title, holdable).
narrative_ontology:cs_axiom_grounding('e2619568-054b-4c68-bcfc-8b980f478fc1', exploration_effort_confirms_title, conventional).
narrative_ontology:cs_axiom('e2619568-054b-4c68-bcfc-8b980f478fc1', secondary, exclusive_return_funds_discovery).
narrative_ontology:cs_axiom_status(exclusive_return_funds_discovery, holdable).
narrative_ontology:cs_axiom_grounding('e2619568-054b-4c68-bcfc-8b980f478fc1', exclusive_return_funds_discovery, instrumental).
narrative_ontology:cs_reference_frame('e2619568-054b-4c68-bcfc-8b980f478fc1', ratified_prior_exploration_rights).
narrative_ontology:cs_drift_state('e2619568-054b-4c68-bcfc-8b980f478fc1', post_mare_liberum_free_seas_era, gap(repudiation_pressure, severe, false)).
narrative_ontology:cs_created_at('e2619568-054b-4c68-bcfc-8b980f478fc1', '').
narrative_ontology:cs_kernel_id(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, tordesillas_demarcation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, portuguese_crown).
narrative_ontology:constraint_beneficiary(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, estado_da_india_administrators).
narrative_ontology:constraint_beneficiary(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, genoese_welser_financiers).
narrative_ontology:constraint_beneficiary(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, papal_curia).
narrative_ontology:constraint_victim(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, venetian_spice_intermediaries).
narrative_ontology:constraint_victim(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, castilian_crown).
narrative_ontology:constraint_victim(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, northern_european_interlopers).
narrative_ontology:constraint_victim(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, asian_cartaz_subjects).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Negotiated the line's placement in 1494, commissioned the annual India fleets, issued the cartaz safe-conducts, and collected customs, pass fees, and the royal share of cargoes through the Casa da India in Lisbon; the same revenues financed the Asian fort network. Its exit is arbitrage-grade: it reallocates between Atlantic, Brazilian, and Asian ventures as returns shift, and it converted a rival's claim into cash by buying Castile's Moluccas rights in 1529 rather than fighting over them.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, portuguese_crown, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, portuguese_crown, beneficiary).

% Issued the 1493 bulls and confirmed the 1494 treaty; its canonists interpret the instruments' language of discovery, occupation, and donation. Every appeal to the line reaffirms the office that drew it, a flow of jurisdictional acknowledgment to Rome. Its exit is poor: having staked its arbitration office on the partition, withdrawal would cost it the precedent across all of Christendom's disputes.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, papal_curia, agenda_setter,
    institutional, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, papal_curia, beneficiary).

% Viceroys at Goa and the captains of Hormuz, Malacca, and the African stations run the pass system, patrol the approaches, staff the feitorias, and take salaries, cargo shares, and private-trade perquisites inside the system they administer. Their careers and patronage networks are built within the arrangement; leaving means abandoning rank and accumulated claims.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, estado_da_india_administrators, beneficiary,
    organized, biographical, constrained, continental).

% Genoese and south-German houses advance the outfitting capital for the India fleets and hold pepper asientos and exclusive sales contracts. They collect contractual returns without operating anything at sea; their capital moves freely to the next concession if returns sag.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, genoese_welser_financiers, beneficiary,
    organized, biographical, mobile, continental).

% For two centuries Venice distributed Asian spices through the Levant and Alexandria. The Cape route's exclusive appropriation cuts the eastern supply out from under them; they retain a diminished overland trade at higher cost and face the Atlantic powers as customers turned competitors. Their exit is constrained: rerouting to the Cape means competing on the interloper's terms under passes they do not issue.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, venetian_spice_intermediaries, payer,
    powerful, biographical, constrained, continental).

% Barred by the line from the African-Asian approach, it first contested the eastern sphere by sailing west to the Moluccas, then sold its eastern claims to Portugal in 1529 for a cash settlement and turned its reach wholly to the western hemisphere. Its exit proved arbitrage-grade: the claim itself was convertible.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, castilian_crown, payer,
    powerful, generational, arbitrage, global).

% Dutch, English, and French shipping entering the Indian Ocean after 1595 is intercepted, seized, or forced to fight. They reject the instrument's premise rather than petition under it, and their exit is mobility: they duplicate the Cape route with their own companies and forts, breaking the arrangement's coverage from outside.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, northern_european_interlopers, payer,
    organized, biographical, mobile, global).

% Gujarati, Malay, Red Sea, and Bay of Bengal shippers trade waters now patrolled by a power that requires them to purchase passes and submits them to seizure without one. Their routes run through straits and gulfs garrisoned by the pass issuer; compliance is the price of sailing at all, and no comparable protection alternative exists at scale.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, asian_cartaz_subjects, payer,
    organized, biographical, trapped, regional).

% Vitoria and his successors at Salamanca examine the instruments' premises from outside both courts: whether papal donation grounds civil title, whether discovery without occupation confers rights, whether the partition serves the common good of peoples never consulted. They hold no stake and enforce nothing; their seat is the record.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, salamanca_school_jurists, observer,
    analytical, generational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, portuguese_crown).
narrative_ontology:fixing_cost_class(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Partitions newly reachable oceanic space between two crowns before their claims collide, preventing intra-Christian war over discoveries; operates a single protected convoy line (the carreira da India) and a standardized safe-conduct (the cartaz) so long-haul shipping can predict protection and cost along the Cape route.
% TRANSFER_FUNCTION: Moves exclusive-traffic proceeds on the Eurasian spice and luxury trades — paid by European buyers through inflated prices, by rival European powers denied direct access, and by Asian shippers purchasing compulsory passes — to the Portuguese crown, its licensed carriers, and its creditors; transfers jurisdictional acknowledgment to the papacy.
% ABSENT_VOICES: No ruler of the Indian Ocean littoral was party to or consulted on the partition of waters they had traded for millennia; their objection survives only in the record of resistance (the Ottoman-Mamluk naval cooperation, Aceh, Johor). Venetian intermediaries, whose existing trade was reallocated by the instrument, had no seat at the table. The later Protestant maritime powers rejected the premise outright rather than argue within it.
% DISAPPEARANCE_RATIONALE: If the demarcation and its enforcement vanished overnight, the Cape-route trade would reopen to all comers within a season: Venetian-Levant middlemen would regain share, Castilian and northern European expeditions would enter the Indian Ocean without legal pretext for interception, and the Portuguese crown would lose the customs, pass fees, and exclusive-sale margins that financed its Asian establishment — the fiscal-military apparatus built on the arrangement would contract abruptly.
% FOUNDING_PROBLEM: After eight decades of Portuguese exploration down Africa and Columbus's landfalls, two crowns with oceanic reach faced overlapping claims to the same unknown spaces, with no agreed rule for dividing them and a live risk of war between the peninsula's powers.
% FOUNDING_PROBLEM_CORROBORATION: Castilian negotiating correspondence and the 1494/1529 treaty record attest that the inter-Iberian allocation problem was settled and closed; Venetian ambassadorial dispatches of the 1490s treat the line as a war-avoidance device between the two crowns. Outside both beneficiary sets, the Salamanca jurists and later Grotius attest that whatever war-avoidance function the instrument had, its continuation as an exclusive-traffic regime lacked independent justification — no disinterested source attests the founding problem remained live after 1529.
narrative_ontology:disappearance_verdict(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, world_rearranges).
narrative_ontology:founding_problem_status(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, 'none', 1).
narrative_ontology:epsilon_provenance(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, 0.55, 'stealth/ox-alpha', 'none', direct).

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
 *   Extraction is authored moderate (0.55 at interval end): the arrangement charges real exclusion costs to rival European powers and compulsory pass fees to Asian shipping, but it also delivers convoy protection, chokepoint security, and route standardization that participants would otherwise have to provision privately. Suppression (0.62) is high because persistence depends on naval patrol, garrison, and pass compulsion rather than participant preference; the suppression mechanism is structural throughout, with negligible internalized component, so no internalization omega is warranted. Theater (0.52) crosses the functional threshold late: after the Reformation stripped papal sanction of force over Protestant powers, and after the 1580 Iberian Union drained the line of internal significance, the legitimating layer became largely ceremonial while forts and passes did the operative work — a Goodhart drift signature in the rising tail of the series. Accessibility collapse is moderate-low (0.45): alternatives never fully closed — Venice's Levant route persisted at higher cost, Castile routed west, and the northern powers eventually duplicated the Cape passage outright. Resistance is high (0.70): the arrangement met armed contestation continuously, from the joint Mamluk-Ottoman fleet at Diu (1509) through Magellan's westward approach to the Moluccas to the Dutch-English breakthrough. The temporal series share one seven-point grid (1494-1650); extraction arcs up and slightly down, suppression builds through the Albuquerque era then plateaus and eases as the Estado contracts, theater rises monotonically — no oscillation, so no intermittent-reinforcement reading applies. Base properties report end-state (1650) values. One identity-lock dynamic deserves note: by the mid-sixteenth century the Estado's officer corps exhibited institutional identity fusion — the padroado was not a policy the administration executed but the administration's self-definition, with careers, honor claims, and patronage denominated in its offices; had that frame broken earlier, contraction would likely have preceded the Dutch-forced version by decades.
 *
 * PERSPECTIVAL GAP:
 *   From the crown and Estado seats the arrangement is infrastructure they built and defend: convoys, forts, passes — a service with a price. From the Venetian and Castilian seats it is a fence across an ocean they had reached by other means; from the Asian shipper's seat it is a toll booth on waters no one consulted them about. Equal-nominal-power seats diverge on exit, which is what separates their experienced positions: Castile arbitraged (sold its Moluccas claim in 1529 and redirected west), Venice stayed constrained (its Levant reroute preserved partial access at rising cost), the northern powers stayed mobile and eventually built parallel infrastructure. Same power, different exits, different experienced arrangement — the engine computes this divergence from the structural data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations (crown, Estado administrators, financiers, curia) place those seats near the subsidy end; the crown's arbitrage-grade exit and dual agenda-setting role anchor it nearest zero. Victim declarations (Venetian intermediaries, Castilian crown, northern interlopers, Asian pass-subjects) place those seats near the target end; trapped exit amplifies the Asian seat, arbitrage exit damps the Castilian seat, and mobile exit leaves the northern seat high but defeasible. The papal curia carries a mild beneficiary position (jurisdictional acknowledgment flows to it) alongside its agenda-setting role, which the structural declarations capture without needing directionality overrides — none are authored because the derivation chain produces accurate placements from the beneficiary/victim and exit data alone. Suppression is authored raw and unscaled; the arrangement's global spatial scope feeds the engine's verification-difficulty modifier on extractiveness only.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — intra-Iberian war over discoveries — was settled by the 1494 line and definitively closed by the 1529 Zaragoza sale of Castile's eastern claims; the arrangement persisted for another century as an exclusive-traffic regime. Reading it as pure extraction misses the genuine coordination services (convoy protection, pass standardization, chokepoint security) that real participants bought into; reading it as pure coordination misses the asymmetric rent capture that outlived the problem it was built to solve. The tangled-rope classification holds both facts simultaneously. The rising theater ratio after 1570 tracks the mandate outliving its function; the mismatch between a dead founding problem and a world-rearranging disappearance marks the arrangement as mandate-resolved but structurally persistent — the corpus signal this story is authored to carry.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This story instantiates the portuguese_exploration_legitimation reading of the tordesillas_demarcation_kernel; what structural facts of the arrangement change under the sibling spanish_conquest_legitimation reading?',
    'Read the sibling story alongside this one and compare victim sets, operative mechanisms, and epsilon over the shared kernel text.',
    'Under the sibling reading the primary victims become the indigenous populations west of the line, the operative mechanism becomes licensed territorial conquest rather than trade exclusion, and epsilon is expected to run materially higher; per-seat classifications computed here do not transfer across readings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Committer-frame indexicality: one kernel, two structurally distinct constraints.').

omega_variable(
    confirmatory_vs_creative_grant,
    'Does the papal instrument, as this reading holds, confirm rights already acquired through demonstrated exploration effort, or does it create rights by fiat — and where exactly in the bull-and-treaty sequence does the difference turn?',
    'Textual comparison of Dum diversas and Romanus pontifex (1452/1455, keyed to discovery and occupation) against Inter caetera and Dudum siquidem (1493), plus the 1493-94 diplomatic record showing Portugal negotiating the line as a correction of a grant that misdescribed its prior effort.',
    'If confirmatory, the arrangement''s legitimacy rests on a desert principle and its take reads as rent on earned infrastructure; if creative, the same structure reads as pure fiat exclusion and epsilon rises toward the snare boundary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(confirmatory_vs_creative_grant, conceptual, 'Location of the confirmatory-versus-creative disagreement between readings.').

omega_variable(
    asian_victim_set_boundary,
    'Are Asian shippers compelled to buy cartazes inside this reading''s victim set, or does the reading''s framing confine victims to rival European powers?',
    'Archival accounting of cartaz revenues and pass enforcement against the diplomatic and chronicle framing of whom the arrangement was understood to exclude.',
    'If Asian shipping is inside the victim set, effective extraction broadens beyond the declared European-rival structure and the asymmetry deepens; if outside, the reading''s epsilon understates the arrangement''s total take.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(asian_victim_set_boundary, empirical, 'Boundary of the victim set under this reading.').

omega_variable(
    papal_authority_constructed_status,
    'Was papal arbitration authority over oceanic partitions a durable coordination institution or a constructed arrangement that held only where interested crowns chose to enforce it?',
    'Post-Reformation behavioral test: northern maritime powers ignored the line entirely without diplomatic cost to themselves, revealing the arrangement''s dependence on enforcement by the parties it benefited.',
    'If constructed-and-enforced, any naturality claim fails and the arrangement sits firmly on the constructed side of the natural-law boundary; classification pressure runs toward enforced hybrid rather than natural limit.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(papal_authority_constructed_status, conceptual, 'Constructed versus natural status of the legitimating authority.').

omega_variable(
    mandate_outlives_function_timing,
    'When exactly did the arrangement''s founding problem die relative to its enforcement expenditure — did the crown keep patrolling for war-avoidance reasons after 1529, or purely for defense of its exclusive traffic?',
    'Correlate Estado da India patrol and garrison budgets against the post-Zaragoza diplomatic record: spending justified by Iberian rivalry should vanish after 1529; spending keyed to interloper interdiction should persist and grow.',
    'Confirms the mandatrophy reading and dates the transition from coordination-serving to rent-defending enforcement, sharpening the late-interval theater diagnosis.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandate_outlives_function_timing, empirical, 'Dating the decoupling of enforcement from the founding problem.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, 1494, 1650).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tord_tr_t1494, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, theater_ratio, 1494, 0.15).
narrative_ontology:measurement(tord_tr_t1520, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, theater_ratio, 1520, 0.18).
narrative_ontology:measurement(tord_tr_t1545, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, theater_ratio, 1545, 0.24).
narrative_ontology:measurement(tord_tr_t1570, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, theater_ratio, 1570, 0.3).
narrative_ontology:measurement(tord_tr_t1595, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, theater_ratio, 1595, 0.38).
narrative_ontology:measurement(tord_tr_t1620, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, theater_ratio, 1620, 0.46).
narrative_ontology:measurement(tord_tr_t1650, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, theater_ratio, 1650, 0.52).

% Extraction over time
narrative_ontology:measurement(tord_be_t1494, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, base_extractiveness, 1494, 0.42).
narrative_ontology:measurement(tord_be_t1520, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, base_extractiveness, 1520, 0.5).
narrative_ontology:measurement(tord_be_t1545, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, base_extractiveness, 1545, 0.58).
narrative_ontology:measurement(tord_be_t1570, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, base_extractiveness, 1570, 0.63).
narrative_ontology:measurement(tord_be_t1595, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, base_extractiveness, 1595, 0.61).
narrative_ontology:measurement(tord_be_t1620, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, base_extractiveness, 1620, 0.57).
narrative_ontology:measurement(tord_be_t1650, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, base_extractiveness, 1650, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(tord_su_t1494, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, suppression_requirement, 1494, 0.4).
narrative_ontology:measurement(tord_su_t1520, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, suppression_requirement, 1520, 0.58).
narrative_ontology:measurement(tord_su_t1545, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, suppression_requirement, 1545, 0.68).
narrative_ontology:measurement(tord_su_t1570, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, suppression_requirement, 1570, 0.72).
narrative_ontology:measurement(tord_su_t1595, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, suppression_requirement, 1595, 0.7).
narrative_ontology:measurement(tord_su_t1620, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, suppression_requirement, 1620, 0.66).
narrative_ontology:measurement(tord_su_t1650, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, suppression_requirement, 1650, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, resource_allocation).
narrative_ontology:affects_constraint(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, tordesillas_demarcation_kernel__spanish_conquest_legitimation).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the colloquial label 'Treaty of Tordesillas.' The label conflates two structurally distinct arrangements sharing one kernel text: (1) THIS story — the eastern-sphere traffic-exclusion regime, moderate epsilon, victims are rival European powers and Asian pass-subjects, beneficiary is the Portuguese Estado da India, extraction rides a trade monopoly; (2) the sibling story — the western-sphere conquest-licensing regime, expected higher epsilon, primary victims are indigenous populations, beneficiary is the Castilian colonial apparatus, extraction rides land conquest. Different epsilon, different victim sets, different failure modes; linked here via affects_constraints because the shared text is cited as evidence across both and degradation of one reading's legitimacy propagates to the other.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
