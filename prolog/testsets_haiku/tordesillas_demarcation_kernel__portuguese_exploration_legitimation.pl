% ============================================================================
% CONSTRAINT STORY: tordesillas_demarcation_kernel__portuguese_exploration_legitimation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: tordesillas_demarcation_kernel__portuguese_exploration_legitimation
 *   human_readable: Treaty of Tordesillas as Papal Confirmation of Portuguese Exploration Rights
 *   domain: international_law/colonial_history/sovereignty
 *
 * SUMMARY:
 *   The Treaty of Tordesillas (1494) is read in THIS constraint story as a
 *   papal confirmation of Portuguese prior exploration rights and as a
 *   mechanism for excluding rival European powers from Eastern trade through
 *   demarcation. The treaty divides the non-European world along a
 *   north-south line, granting Portugal everything east of it (Africa, India,
 *   the Cape route, the Eastern spice trade) and Castile everything west (the
 *   Americas). Under this reading, the treaty is not primarily a license to
 *   conquer or convert (that is the sibling Spanish reading); it is a trade
 *   monopoly enforced through papal sanction and Portuguese naval power
 *   against both rival Europeans and independent merchants. The constraint
 *   operates as a tangled rope because it solves a genuine coordination
 *   problem among European powers (allocation without warfare) while
 *   simultaneously extracting from those powers by excluding them from the
 *   most valuable trade and forcing them into alternative routes (westward,
 *   around Africa's southern cape). The Portuguese and their Estado benefit
 *   from monopoly rents; rival European powers and independent merchants pay
 *   by being excluded; the Papacy collects legitimacy. This is a kernel
 *   reading: the same treaty can be framed as Spanish license for conquest
 *   (the sibling reading) or as Portuguese confirmation of prior exploration
 *   rights (this reading). The two readings instantiate different constraints
 *   with different victim sets, different beneficiary structures, and
 *   different ε values.
 *
 * KEY AGENTS:
 *   - Portuguese crown: organizes and controls the monopoly through the Estado da Índia; negotiates the papal sanction
 *   - Portuguese Estado da Índia: collects rents from trade monopoly; operates naval enforcement machinery
 *   - Papal authority: grants the legitimacy; pronounces on the boundary; enforces through spiritual sanction
 *   - Castilian crown: excluded from Eastern trade but accommodated with Western Hemisphere access
 *   - Rival European powers (England, France, Dutch): trapped outside the demarcation; face naval enforcement and legal prohibition
 *   - Independent maritime traders: identity-locked out of the trade system entirely; subject to interdiction and execution
 *   - Indigenous populations: structurally excluded from the negotiation; not primary victims in this reading
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, 0.62).
domain_priors:suppression_score(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, 0.71).
domain_priors:theater_ratio(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, extractiveness, 0.62).
narrative_ontology:constraint_metric(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, resistance, 0.54).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, tangled_rope).
narrative_ontology:human_readable(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, "Treaty of Tordesillas as Papal Confirmation of Portuguese Exploration Rights").
narrative_ontology:topic_domain(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, "international_law/colonial_history/sovereignty").

domain_priors:requires_active_enforcement(tordesillas_demarcation_kernel__portuguese_exploration_legitimation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, 'a0b76b98-d822-45d3-a0ea-58c53da9f0ce').
narrative_ontology:cs_kernel_codification('a0b76b98-d822-45d3-a0ea-58c53da9f0ce', formalized).
narrative_ontology:cs_authority_grounding('a0b76b98-d822-45d3-a0ea-58c53da9f0ce', extraction).
narrative_ontology:cs_interpretation_layer_present('a0b76b98-d822-45d3-a0ea-58c53da9f0ce').
narrative_ontology:cs_reading_relation('a0b76b98-d822-45d3-a0ea-58c53da9f0ce', tordesillas_demarcation_kernel__spanish_conquest_legitimation, coexists_with).
narrative_ontology:cs_axiom('a0b76b98-d822-45d3-a0ea-58c53da9f0ce', foundational, prior_exploration_legitimates_monopoly).
narrative_ontology:cs_axiom_status(prior_exploration_legitimates_monopoly, holdable).
narrative_ontology:cs_axiom_grounding('a0b76b98-d822-45d3-a0ea-58c53da9f0ce', prior_exploration_legitimates_monopoly, conventional).
narrative_ontology:cs_axiom('a0b76b98-d822-45d3-a0ea-58c53da9f0ce', foundational, papal_demarcation_binds_christian_powers).
narrative_ontology:cs_axiom_status(papal_demarcation_binds_christian_powers, holdable).
narrative_ontology:cs_axiom_grounding('a0b76b98-d822-45d3-a0ea-58c53da9f0ce', papal_demarcation_binds_christian_powers, deontological).
narrative_ontology:cs_reference_frame('a0b76b98-d822-45d3-a0ea-58c53da9f0ce', papally_sanctioned_exploration_rights).
narrative_ontology:cs_drift_state('a0b76b98-d822-45d3-a0ea-58c53da9f0ce', mid_sixteenth_century_enforcement_hardening, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('a0b76b98-d822-45d3-a0ea-58c53da9f0ce', '').
narrative_ontology:cs_kernel_id(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, tordesillas_demarcation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, portuguese_estado_da_india).
narrative_ontology:constraint_beneficiary(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, portuguese_crown).
narrative_ontology:constraint_victim(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, castilian_crown).
narrative_ontology:constraint_victim(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, rival_european_powers).
narrative_ontology:constraint_victim(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, independent_maritime_traders).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Negotiates with the Papacy to formalize prior exploration claims and secure papal sanction for trade monopoly east of the demarcation line. Acts as the primary beneficiary of the treaty's legitimacy claim, using papal authority to exclude rivals from the Indian Ocean trade routes that Portuguese explorers opened. The crown controls enforcement through naval power and claims exclusive right to establish trading posts and govern Portuguese subjects in eastern territories.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, portuguese_crown, agenda_setter,
    institutional, generational, arbitrage, global).

% The institutional apparatus that collects rents from the Eastern trade monopoly. Receives the exclusive right to establish factories, control sea routes, and tax all Portuguese trade east of the line. The estado operates as both a commercial and military entity, using the treaty's papal sanction to legitimize its monopoly against competing European traders and independent merchants.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, portuguese_estado_da_india, beneficiary,
    institutional, generational, arbitrage, global).

% Accepts geographic exclusion from Eastern trade through the demarcation line, ceding the more lucrative Asian spice trade to Portugal while retaining Western Hemisphere access. Pays the cost through foregone monopoly rents and permanent subordination in the race for Indian Ocean trade dominance. The constraint forces Castilian expansion westward, altering its geopolitical trajectory and limiting its economic options in the Eastern trade.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, castilian_crown, payer,
    institutional, generational, constrained, global).

% England, France, the Dutch, and other emerging maritime powers are excluded from Eastern trade through the treaty's demarcation mechanism and papal authority claim. They face active naval enforcement by the Portuguese, legal prohibition under the treaty, and lack papal sanction for their own exploration claims. Their exit option is to either wait for the treaty's enforcement to weaken through power shifts (trapped but not identity-locked) or to openly defy papal authority and risk excommunication.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, rival_european_powers, payer,
    moderate, biographical, trapped, global).

% Merchants operating outside the Portuguese or Castilian crowns are prohibited from independent trade in both Eastern and Western zones. They are identity-locked as non-state actors in a sovereignty framework that recognizes only crown-sanctioned exploration and trade. Their suppression is enforced through Portuguese naval interdiction, cargo seizure, and execution as pirates. Exit would require abandoning maritime commerce entirely or operating under a crown's banner.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, independent_maritime_traders, payer,
    powerless, immediate, identity_locked, global).

% The Pope as theological-political authority grants and legitimizes the demarcation, treating the divided world as a gift from divine authority to be distributed among Christian princes. The Papacy's role is to bless prior exploration as legitimate and to pronounce upon the legitimacy of future claims. The constraint depends on the Papacy's continued willingness to enforce the boundary through threat of spiritual sanction and on European acceptance of papal supremacy in these matters.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, papal_authority, agenda_setter,
    institutional, generational, analytical, universal).
narrative_ontology:stakeholder_non_agent(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, papal_authority).

% Inhabitants of Eastern territories subject to Portuguese Estado claims have no seat in the treaty negotiations. They are treated as subjects of conquered territories rather than as negotiating parties. Their exclusion is structural: they are neither beneficiaries of the trade monopoly nor explicitly named victims in this reading (where the victim set is rival European powers), but they bear costs through subordination to Portuguese colonial administration.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, indigenous_populations_eastern_territories, excluded,
    powerless, immediate, trapped, global).

% The broader system of interstate competition watching how the treaty holds. European powers observe whether papal authority continues to bind, whether military enforcement holds the line, and whether the treaty's legitimacy claim actually succeeds in preventing rival claims. Future challengers will test the constraint's durability and the credibility of the papal sanction.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, european_maritime_competition, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, portuguese_estado_da_india).
narrative_ontology:fixing_cost_class(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a system for recognizing and allocating exploration rights among Christian European powers by reference to papal authority. Instead of free-for-all maritime conflict over newly discovered territories, the treaty creates a rule: the Pope recognizes prior exploration and grants exclusive rights to trade and settlement based on that recognition. This solves the coordination problem of how to allocate the non-European world among competing European princes without constant warfare.
% TRANSFER_FUNCTION: Transfers exclusive access to Eastern trade monopoly rents from the set of all European powers to the Portuguese crown and Estado da Índia. The transfer mechanism is the papal sanction that (1) confirms Portuguese prior exploration as valid, (2) declares Portuguese holdings east of the line as exclusively Portuguese, and (3) prohibits other Christian powers from independent exploration or trade in those waters. The constraint moves trade revenue and prestige from the competitive arena into Portuguese hands.
% ABSENT_VOICES: Indigenous populations of the Eastern territories are structurally excluded from the negotiation entirely. They are treated as resources to be distributed, not as parties with claims. Independent merchants and rival European powers excluded by the line are not consulted. The Papacy is the only non-crown agent present, and its role is to legitimize rather than to negotiate peer-to-peer. The absent voices would argue for open maritime access, for indigenous sovereignty, and for breaking the papal monopoly on legitimacy-granting authority.
% DISAPPEARANCE_RATIONALE: If the treaty and its papal sanction disappeared, the Portuguese monopoly on Eastern trade would collapse immediately. Rival European powers would challenge Portuguese control within years; independent merchants would attempt clandestine trade; the Estado would lose its exclusive rents and its legitimacy claim to suppress competitors. The geopolitical map would reorganize around renewed maritime competition rather than the papal demarcation. The Western Hemisphere would be freed from Castilian dominion and open to rival claims.
% FOUNDING_PROBLEM: How to allocate newly discovered non-European territories and trade monopolies among competing European princes without constant armed conflict. The problem is especially acute because exploration is ongoing and claims are contested — who owns the Indian Ocean? Who discovered the route to India? Without a recognized authority to adjudicate claims, every territorial claim risks war.
% FOUNDING_PROBLEM_CORROBORATION: The Portuguese crown attests the founding problem is live and the treaty solves it: orderly allocation based on prior exploration prevents chaos. Rival European powers (England, France, the Dutch) contest the diagnosis: they argue the treaty merely entrenches Portuguese monopoly and does not solve conflict, only redistributes it (they are pushed toward the Atlantic and Americas). Modern historians and legal scholars outside the benefiting parties document that the treaty was a power-consolidation mechanism dressed as a coordination rule, not a genuine conflict-prevention device — the problem it solved was 'how to stop Portugal and Castile from fighting each other,' not 'how to allocate the world fairly.'
narrative_ontology:disappearance_verdict(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, world_rearranges).
narrative_ontology:founding_problem_status(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, 'none', 1).

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
 *   Extractiveness starts at 0.48 in 1494 (treaty just signed; coordination benefit is real and high, extraction is the future) and rises to 0.62 by 1580 (as the monopoly rent grows and the exploration justification becomes retrospective theater). Suppression rises throughout (0.52 → 0.71) as Portuguese naval enforcement hardens against increasingly powerful rivals. Theater ratio rises in the middle period (0.12 → 0.29) as the 'confirmation of prior exploration' narrative becomes more elaborated and less tied to actual new exploration — by 1580, most of the Estado's naval activity is defending the monopoly boundary, not discovering new routes. Suppression drops slightly at the end (0.74 → 0.71) because the constraint is beginning to crack — French privateers, Dutch merchant-adventurers, and English explorers are challenging the line openly and the papal sanction's force is fading. The measurement series uses a shared time grid: every metric is authored at every time point (1494, 1510, 1530, 1550, 1570, 1580), enabling the drift detection system to track how the constraint's character shifts from coordination-heavy to extraction-heavy to defensively theatrical.
 *
 * PERSPECTIVAL GAP:
 *   The Portuguese agenda-setter seat should compute to rope or light tangled_rope (genuine coordination with modest extraction); the rival European power seats should compute to snare or dark tangled_rope (pure extraction dressed in coordination language); the independent merchant seat should compute to snare (extraction, suppression, identity-locking, no coordination benefit). The engine computes per-seat from the structural data, so the claim/metric gap and the perspectival gap are the same phenomenon. The authored claim is tangled_rope (the story presents it as such), which is correct at the institutional level (it does solve a coordination problem), but the payer seats experience it as much closer to pure extraction because suppression is high and their exit options are extremely constrained.
 *
 * DIRECTIONALITY LOGIC:
 *   Portuguese crown and Estado da Índia are full beneficiaries (d near 0.0): they collect monopoly rents, control the rule-making, have arbitrage-grade exit (they can abandon the monopoly if conditions change, though they have no incentive to). Castilian crown is a partial payer (d = 0.4-0.5): it is excluded from Eastern trade but accommodated with Western access, so it bears a cost (foregone Eastern monopoly rents) but also receives a benefit (secure Western dominion without competing for it). Rival European powers are near-complete targets (d = 0.85-0.95): they are trapped outside the demarcation, face active naval suppression, have constrained exit (they can attempt to defy the treaty and face war with Portugal, or they can yield and accept exclusion), and receive no compensating benefit. Independent merchants are complete targets (d = 1.0): they are identity-locked (they cannot exist as maritime traders outside the crown-sanctioned system), face the highest suppression (execution as pirates), and have zero benefit. The engine derives these d values from the beneficiary/victim declarations and exit options; the commentary explains the structural asymmetry that justifies the divergent directionalities.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (allocation without warfare among European princes) is arguably DEAD by 1580: the treaty itself was the solution, and it worked (Portugal and Castile did not fight over Eastern spice trade; they divided the world along the line). However, the constraint persists and even intensifies because it has transformed into a trade monopoly machine that the Portuguese Estado would not abandon even if the coordination problem it was born from no longer mattered. The theater_ratio rising in the middle period (0.21 → 0.26) indicates increasing performative maintenance: the Estado justifies its monopoly by reference to 'prior exploration' and 'papal sanction' even as both become retrospective narratives. The founding problem's death and the constraint's persistence are structurally masked by the fact that the Papacy's legitimacy claim ITSELF becomes the function — the coordination problem shifts from 'how do we prevent Portuguese-Castilian war' to 'how do we maintain the fiction that European powers have the right to divide the non-European world.' This is a mandate collapse scenario where the original problem is solved but the institutional machinery persists because it serves extractive functions the beneficiaries now depend on.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    papal_authority_legitimacy_contest,
    'Does the Papacy''s proclamation of the demarcation line actually confer legitimacy in the eyes of European powers, or is the papal sanction merely theater for an arrangement already settled by Portuguese naval power?',
    'Historical evidence on whether rival powers explicitly acknowledge papal authority as the source of the boundary (they do in diplomatic letters) versus treating the boundary as a power fact enforced by Portuguese galleons (the actual enforcement record). If powers cite papal authority while simultaneously challenging it militarily, the answer is mixed.',
    'If papal authority is performative and the real constraint is Portuguese naval power, then suppression is higher than authored (it is pure military enforcement, not legitimacy-based) and the coordination function is weaker (the rule is maintained by force, not by agreement). If papal authority is real, suppression is lower because it includes spiritual sanction as a cost to defection. This affects whether the constraint computes as tangled_rope (coordination + enforcement) or snare (pure enforcement).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(papal_authority_legitimacy_contest, empirical, 'Whether the papal sanction is the actual source of the constraint''s persistence or merely a cover story for Portuguese military dominance.').

omega_variable(
    prior_exploration_as_legitimacy_basis,
    'Is Portuguese prior exploration (via da Gama, earlier voyagers) a genuinely superior claim to Eastern trade, or is it retrospective narrative construction to justify a monopoly Portugal secured by naval power?',
    'Historians examining contemporaneous documents: do Portuguese and papal sources from 1494 cite specific prior explorations as the basis for the grant, or is the exploration narrative developed later to justify an arrangement already settled? The evidence suggests the treaty was primarily a compromise between two power competitors, with exploration used as the justifying rhetoric.',
    'If prior exploration is genuine legitimacy, the constraint is real confirmation (the treaty recognizes something that existed). If prior exploration is narrative construction, the constraint is pure extraction dressed in discovery language. The ε-invariance implication: if exploration is retrospective, this reading''s foundational claim collapses and merges with the Spanish reading (both are papal grants to monopoly, regardless of exploration history).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(prior_exploration_as_legitimacy_basis, empirical, 'Whether ''prior exploration'' is a real legitimacy basis or a narrative cover for power-based allocation.').

omega_variable(
    sibling_reading_coexistence_vs_foreclosure,
    'Can the Portuguese exploration-confirmation reading and the Spanish conquest-license reading both be held within the same institutional framework, or does adoption of one reading logically require rejection of the other?',
    'If a single institution (e.g., the Papacy) issues statements that presume both readings simultaneously, they coexist; if an institution adopts one reading in governance and explicitly rejects the other, they foreclose. Historical evidence: the Papacy uses different language with Portugal (confirming exploration) and Castile (granting conquest), suggesting coexistence in a diplomatic frame that avoids forcing a choice.',
    'If readings coexist, neither reading is foreclosed and the contest is political/historical. If readings foreclose, the contest has a logical winner and the losing reading would be incoherent if adopted by the same institution. The engine''s `reading_relations` field depends on this resolution: coexists_with vs. forecloses.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_coexistence_vs_foreclosure, conceptual, 'Whether the two readings of the Tordesillas kernel are logically compatible or mutually exclusive.').

omega_variable(
    suppression_mechanism_structural_vs_legitimacy,
    'How much of the measured suppression (0.71 at interval end) is structural (Portuguese naval power and ability to interdict vessels) versus legitimacy-based (the threat of papal excommunication or social exclusion)?',
    'Historical record of defiance: do powers that explicitly reject papal authority (e.g., Protestant powers, Ottoman competitors) face the same suppression as those nominally accepting it? Do independent merchants face execution as pirates (structural suppression) or merely legal prohibition (legitimacy-based)?',
    'If suppression is mostly structural, the constraint persists via force and would collapse if Portugal lost naval dominance. If suppression is mostly legitimacy-based, the constraint persists via social/religious authority and is more durable against power shifts. This affects the theater_ratio interpretation: theatrical maintenance makes sense only if legitimacy is the fragile mechanism being defended; if power is the mechanism, theater is secondary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_legitimacy, empirical, 'Whether suppression of rivals operates through military enforcement or through legitimacy denial.').

omega_variable(
    indigenous_populations_in_this_reading,
    'Are indigenous populations of Eastern territories victims of this constraint, or are they a separate constraint (colonial subjugation) that this reading excludes by treating the victim set as rival European powers only?',
    'The Portuguese Estado da Índia did subjugate and exploit indigenous populations (forced labor, tribute, religious conversion). The question is whether that exploitation is structurally part of the Tordesillas constraint (the demarcation grant) or a separate extractive constraint (colonial governance). If separate, this reading''s victim set is correct (European powers); if integral, indigenous populations should be listed as victims and the constraint''s ε would rise.',
    'If indigenous subjugation is integral, the reading merges with the Spanish conquest reading (both are about papal grants for territorial subjugation) and the distinction collapses. If indigenous subjugation is separate, this reading''s focus on trade monopoly (excluding European rivals) is structurally sound. The ε-invariance principle requires separate constraints if the empirical phenomena are distinct.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(indigenous_populations_in_this_reading, conceptual, 'Whether indigenous population subjugation is structurally part of the Tordesillas demarcation constraint or a separate colonial governance constraint.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, 1494, 1580).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tord_tr_t1494, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, theater_ratio, 1494, 0.12).
narrative_ontology:measurement_basis(tord_tr_t1494, observed).
narrative_ontology:measurement(tord_tr_t1510, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, theater_ratio, 1510, 0.16).
narrative_ontology:measurement_basis(tord_tr_t1510, observed).
narrative_ontology:measurement(tord_tr_t1530, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, theater_ratio, 1530, 0.21).
narrative_ontology:measurement_basis(tord_tr_t1530, observed).
narrative_ontology:measurement(tord_tr_t1550, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, theater_ratio, 1550, 0.26).
narrative_ontology:measurement_basis(tord_tr_t1550, observed).
narrative_ontology:measurement(tord_tr_t1570, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, theater_ratio, 1570, 0.29).
narrative_ontology:measurement_basis(tord_tr_t1570, observed).
narrative_ontology:measurement(tord_tr_t1580, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, theater_ratio, 1580, 0.28).
narrative_ontology:measurement_basis(tord_tr_t1580, observed).

% Extraction over time
narrative_ontology:measurement(tord_be_t1494, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, base_extractiveness, 1494, 0.48).
narrative_ontology:measurement_basis(tord_be_t1494, observed).
narrative_ontology:measurement(tord_be_t1510, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, base_extractiveness, 1510, 0.55).
narrative_ontology:measurement_basis(tord_be_t1510, observed).
narrative_ontology:measurement(tord_be_t1530, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, base_extractiveness, 1530, 0.61).
narrative_ontology:measurement_basis(tord_be_t1530, observed).
narrative_ontology:measurement(tord_be_t1550, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, base_extractiveness, 1550, 0.63).
narrative_ontology:measurement_basis(tord_be_t1550, observed).
narrative_ontology:measurement(tord_be_t1570, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, base_extractiveness, 1570, 0.64).
narrative_ontology:measurement_basis(tord_be_t1570, observed).
narrative_ontology:measurement(tord_be_t1580, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, base_extractiveness, 1580, 0.62).
narrative_ontology:measurement_basis(tord_be_t1580, observed).

% Suppression requirement over time
narrative_ontology:measurement(tord_su_t1494, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, suppression_requirement, 1494, 0.52).
narrative_ontology:measurement_basis(tord_su_t1494, observed).
narrative_ontology:measurement(tord_su_t1510, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, suppression_requirement, 1510, 0.6).
narrative_ontology:measurement_basis(tord_su_t1510, observed).
narrative_ontology:measurement(tord_su_t1530, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, suppression_requirement, 1530, 0.68).
narrative_ontology:measurement_basis(tord_su_t1530, observed).
narrative_ontology:measurement(tord_su_t1550, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, suppression_requirement, 1550, 0.72).
narrative_ontology:measurement_basis(tord_su_t1550, observed).
narrative_ontology:measurement(tord_su_t1570, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, suppression_requirement, 1570, 0.74).
narrative_ontology:measurement_basis(tord_su_t1570, observed).
narrative_ontology:measurement(tord_su_t1580, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, suppression_requirement, 1580, 0.71).
narrative_ontology:measurement_basis(tord_su_t1580, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, resource_allocation).
narrative_ontology:boltzmann_floor_override(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, 0.12).
narrative_ontology:affects_constraint(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, tordesillas_demarcation_kernel__spanish_conquest_legitimation).

% DUAL FORMULATION NOTE:
% This constraint (portuguese_exploration_legitimation) and its sibling (spanish_conquest_legitimation) are two readings of the Treaty of Tordesillas kernel. They share the same treaty text and the same formal papal pronouncement but instantiate different constraints because they rest on different interpretations of what the treaty legitimizes. This reading focuses on the demarcation as a trade monopoly mechanism excluding European rivals; the sibling reading focuses on the demarcation as a conquest grant. Both are live readings in historical discourse. The two stories are linked via network.affects_constraints and omega variable commentary documenting the interpretive contest. The epsilon-invariance principle requires separate constraint stories precisely because the readings' ε values differ: this reading's extraction is trade monopoly (~0.62), while the sibling reading's extraction would be territorial conquest and subjugation (higher ε, ~0.75+). A single constraint story cannot hold both ε values — the correct response is two stories linked by the network.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, institutional, 0.42).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
