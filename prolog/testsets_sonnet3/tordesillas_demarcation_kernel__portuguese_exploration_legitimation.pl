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
 *   constraint_id: tordesillas_demarcation_kernel__portuguese_exploration_legitimation
 *   human_readable: Tordesillas Line as Papal Confirmation of Portuguese Exploration Priority and Rival Exclusion East of the Meridian
 *   domain: international_law/colonial_history/sovereignty_theory
 *
 * SUMMARY:
 *   In 1493-94, Pope Alexander VI's bulls and the subsequent Treaty of
 *   Tordesillas divided the non-Christian world along a meridian, granting
 *   Portugal exclusive rights east of the line. Read from the
 *   Portuguese/Estado da Índia seat, this was primarily a confirmation of
 *   decades of prior Portuguese caravel exploration down the African coast
 *   and into the Indian Ocean, converting sunk exploration costs and de facto
 *   naval presence into a papally-legitimated trade monopoly excluding
 *   Castile, England, France, and later the Dutch Republic from the Cape
 *   route and spice trade. The mechanism of extraction is commercial monopoly
 *   rent and exclusion enforcement (naval patrols, fortified feitorias), not
 *   the conquest and subjugation of colonized peoples that characterizes the
 *   sibling Spanish reading west of the line.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, 0.58).
domain_priors:suppression_score(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, 0.62).
domain_priors:theater_ratio(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, extractiveness, 0.58).
narrative_ontology:constraint_metric(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, tangled_rope).
narrative_ontology:human_readable(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, "Tordesillas Line as Papal Confirmation of Portuguese Exploration Priority and Rival Exclusion East of the Meridian").
narrative_ontology:topic_domain(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, "international_law/colonial_history/sovereignty_theory").

domain_priors:requires_active_enforcement(tordesillas_demarcation_kernel__portuguese_exploration_legitimation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, '513d36da-79ba-4248-a0ae-574dd4c8c38d').
narrative_ontology:cs_kernel_codification('513d36da-79ba-4248-a0ae-574dd4c8c38d', formalized).
narrative_ontology:cs_authority_grounding('513d36da-79ba-4248-a0ae-574dd4c8c38d', lineage).
narrative_ontology:cs_interpretation_layer_present('513d36da-79ba-4248-a0ae-574dd4c8c38d').
narrative_ontology:cs_reading_relation('513d36da-79ba-4248-a0ae-574dd4c8c38d', tordesillas_demarcation_kernel__spanish_conquest_legitimation, coexists_with).
narrative_ontology:cs_axiom('513d36da-79ba-4248-a0ae-574dd4c8c38d', foundational, prior_exploration_investment_confers_priority_claim).
narrative_ontology:cs_axiom_status(prior_exploration_investment_confers_priority_claim, holdable).
narrative_ontology:cs_axiom_grounding('513d36da-79ba-4248-a0ae-574dd4c8c38d', prior_exploration_investment_confers_priority_claim, conventional).
narrative_ontology:cs_axiom('513d36da-79ba-4248-a0ae-574dd4c8c38d', secondary, papal_confirmation_legitimates_preexisting_naval_fact_rather_than_creates_new_right).
narrative_ontology:cs_axiom_status(papal_confirmation_legitimates_preexisting_naval_fact_rather_than_creates_new_right, holdable).
narrative_ontology:cs_axiom_grounding('513d36da-79ba-4248-a0ae-574dd4c8c38d', papal_confirmation_legitimates_preexisting_naval_fact_rather_than_creates_new_right, conventional).
narrative_ontology:cs_reference_frame('513d36da-79ba-4248-a0ae-574dd4c8c38d', papal_plenitudo_potestatis_temporal_arbitration).
narrative_ontology:cs_drift_state('513d36da-79ba-4248-a0ae-574dd4c8c38d', post_reformation_naval_competition, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('513d36da-79ba-4248-a0ae-574dd4c8c38d', '').
narrative_ontology:cs_kernel_id(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, tordesillas_demarcation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, estado_da_india).
narrative_ontology:constraint_beneficiary(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, portuguese_crown).
narrative_ontology:constraint_beneficiary(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, lisbon_merchant_houses).
narrative_ontology:constraint_victim(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, castilian_crown_eastward_ambitions).
narrative_ontology:constraint_victim(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, english_merchant_adventurers).
narrative_ontology:constraint_victim(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, dutch_trading_interests).
narrative_ontology:constraint_victim(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, french_atlantic_traders).
narrative_ontology:constraint_vindicates(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, papal_temporal_arbitration_authority).
narrative_ontology:constraint_vindicates(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, prior_discovery_confers_priority_right).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The Portuguese crown's overseas administrative and commercial apparatus for the Indian Ocean, African coast, and (post-1494 line extension) Brazil. It treats the papal-confirmed demarcation as the legal basis for excluding rival European shipping from the Cape route, the spice trade, and the eastern Atlantic exploration corridor it had already invested decades in charting. It enforces the exclusion with naval patrols and fortified trading posts (feitorias) rather than land conquest — the extraction is monopoly rent on trade routes, not tribute from subjugated territory.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, estado_da_india, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, estado_da_india, agenda_setter).

% Negotiated the 1494 revision of the papal bull's line with Castile at Tordesillas, moving it further west to capture Brazil and secure the entire African/Asian route. Uses the treaty and its papal confirmation as the diplomatic instrument to demand other Catholic powers recognize its priority claim, and to request papal censure against violators. Bears the cost of maintaining the fleets and forts that make exclusion real.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, portuguese_crown, agenda_setter,
    institutional, civilizational, arbitrage, global).

% Merchant capital financing the spice and slave-trade voyages benefits from the crown-enforced exclusion of Castilian, English, French, and later Dutch competitors from the eastern routes. Their profits depend on the demarcation holding; they lobby the crown to maintain naval enforcement and diplomatic pressure on Rome to reaffirm the grant when challenged.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, lisbon_merchant_houses, beneficiary,
    organized, biographical, constrained, continental).

% Spain accepted the westward line at Tordesillas in exchange for its own confirmed hemisphere, but this foreclosed Castilian claims to the Moluccas and the eastern spice routes that Magellan's voyage later reopened as a dispute (resolved at Zaragoza, 1529, with Spain selling its claim). Castile's eastward commercial ambitions are the direct casualty of the papal confirmation of Portuguese priority — a real transfer of exclusive trading rights it might otherwise have contested by force or exploration.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, castilian_crown_eastward_ambitions, payer,
    institutional, generational, constrained, global).

% English crown and merchant interests were never party to the papal bull and rejected its authority outright ("the Pope's line does not bind us"), but were structurally excluded from the Cape route and Indian Ocean trade by Portuguese naval enforcement of the demarcation for over a century. Their only recourse was competing exploration (seeking a Northeast or Northwest Passage) or later privateering and armed intrusion, both direct responses to the exclusion.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, english_merchant_adventurers, excluded,
    powerful, biographical, trapped, global).

% Dutch merchants, formerly distributors of Portuguese-imported spice within Europe, were cut off from direct access when Iberian union (1580) closed Lisbon to them, a downstream consequence of the same demarcation logic. They eventually broke the exclusion by force, founding the VOC and seizing Portuguese trading posts — the clearest evidence the exclusion was extractive rather than a stable coordination equilibrium.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, dutch_trading_interests, excluded,
    organized, biographical, constrained, global).

% French crown and Breton/Norman merchant fleets contested the treaty's validity from the outset (Francis I's remark demanding to see 'Adam's will' naming Spain and Portugal sole heirs), but lacked the papal standing or naval reach to break the eastern exclusion directly, redirecting instead toward North Atlantic fishing and furs largely outside the contested line.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, french_atlantic_traders, excluded,
    moderate, biographical, trapped, regional).

% Issued the bulls (Inter Caetera, 1493) that Spain and Portugal then privately renegotiated into the Tordesillas line, retroactively legitimizing exploration claims already substantially made. The papacy's temporal-arbitration authority is the vindicated proposition the whole structure depends on; it collects no trade revenue itself but its authority is what other Catholic powers were meant to feel bound by.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, papacy_alexander_vi_and_successors, agenda_setter,
    institutional, civilizational, analytical, universal).
narrative_ontology:stakeholder_secondary_role(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, papacy_alexander_vi_and_successors, observer).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, estado_da_india).
narrative_ontology:fixing_cost_class(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents a two-way naval and diplomatic collision between Portugal and Castile — the two powers with active exploration fleets in 1493-94 — by dividing the unexplored world into exclusive spheres along a meridian, so neither invests in contesting the other's already-substantial sunk costs in navigation, fort-building, and route knowledge.
% TRANSFER_FUNCTION: Moves exclusive trading and exploration rights east of the line from a contestable multi-power free-for-all into a Portuguese monopoly, financed by excluding English, French, Castilian, and (after 1580) Dutch merchants from the spice trade and the Cape route, and enforced by Portuguese naval patrols and fortified trading posts rather than papal police power.
% ABSENT_VOICES: English, French, and Dutch crowns and merchant communities were never signatories to the bull or the treaty and never accepted papal temporal jurisdiction over non-Catholic or rival-Catholic trade; their objections are recorded in diplomatic protest and eventually in armed competition (privateering, the VOC's seizure of Portuguese posts) but had no standing in the instrument itself.
% DISAPPEARANCE_RATIONALE: Without the papal confirmation, Portugal's claim to exclusive eastern routes rests on unilateral assertion and naval presence alone — precisely the situation that in fact obtained once English and Dutch power grew enough to ignore the line; the treaty's disappearance is observably what happened by the early 1600s, and the world did rearrange into competitive multi-power trading companies (EIC, VOC) contesting routes by force and charter rather than papal grant.
% FOUNDING_PROBLEM: Two Iberian Catholic crowns with active, expensive overseas exploration programs needed a mutually binding way to avoid war with each other over claims neither could yet fully verify or hold, and a legitimating authority both recognized to make the division stick.
% FOUNDING_PROBLEM_CORROBORATION: The Treaty of Zaragoza (1529) itself, negotiated by Spain and Portugal to resolve the Moluccas dispute the original line left ambiguous, is direct evidence from the benefiting parties that the line was already contested and required renegotiation. Outside corroboration comes from English and Dutch state papers of the sixteenth and seventeenth centuries flatly denying papal jurisdiction over non-Catholic sovereigns' trade rights, and from the historical fact of Dutch/English naval seizure of Portuguese posts, which no party to the original bull could prevent by treaty alone — the exclusion function died the moment it met a rival with comparable naval capacity.
narrative_ontology:disappearance_verdict(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, world_rearranges).
narrative_ontology:founding_problem_status(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
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
 *   Extractiveness is authored as moderate (peaking near 0.62 mid-16th century) because the primary extraction is monopoly trade rent collected from excluded rival European mercantile interests, not extraction from a colonized population lacking any comparable countervailing power — rival crowns and merchant companies eventually mounted real naval and commercial competition, capping how extractive the arrangement could remain. Theater ratio rises sharply across the interval (0.15 to 0.70) because after roughly 1600 the papal-legal basis for exclusion became increasingly performative — England and the Netherlands ignored it outright and Portugal's actual hold on the routes depended on fortresses and fleets, not the bull's authority, yet Portugal continued to invoke the treaty diplomatically long after it had stopped functioning as real deterrence. Suppression_requirement tracks the naval/diplomatic enforcement effort needed to hold the line, rising through the Estado da Índia's peak and collapsing after the Dutch seizure of Malacca (1641) and the effective end of Portuguese monopoly enforcement by the 1663 Anglo-Portuguese and Dutch-Portuguese settlements.
 *
 * DIRECTIONALITY LOGIC:
 *   Estado da Índia, the Portuguese crown, and Lisbon merchant capital sit near the beneficiary end: they collect monopoly rents and control the enforcement apparatus, with arbitrage-level exit (they can and did adjust routes, forts, and diplomatic pressure as circumstances shifted). Castile is a partial payer — it received its own confirmed hemisphere in exchange but lost the specific eastward ambitions (the Moluccas dispute) that Zaragoza later had to buy out, so its directionality sits closer to symmetric-but-net-payer on the eastern claim specifically. English, French, and Dutch merchant interests sit at the full-target end: excluded entirely from the instrument's benefits, bearing the cost of exclusion enforcement, with only trapped or constrained exit until they built comparable naval capacity to simply ignore the line.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — averting Iberian war over unverified, expensive, simultaneously-pursued exploration claims — was real and substantially resolved by 1500 once the practical division held and both crowns' fleets stopped colliding. But Portugal continued invoking the papal grant against England, France, and the Dutch Republic long after those powers explicitly rejected papal temporal jurisdiction over their trade, and long after Portugal's actual naval capacity to enforce exclusion had become the load-bearing element rather than the treaty's legal authority. This is a clean case of the founding coordination function (Iberian war-avoidance) dying while the constraint's rhetorical invocation persisted as diplomatic cover for what had become raw naval competition — exactly the founding_problem_status: dead classification with disappearance_verdict: world_rearranges, since the actual rearrangement (VOC and EIC seizing routes and posts by force and charter) is the historical record, not a hypothetical.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    papal_authority_vs_naval_fact,
    'Did the papal confirmation ever provide real deterrent legitimacy against non-Catholic powers, or was it from the outset merely retroactive cover for a naval fait accompli that Portugal would have pursued and defended regardless of papal sanction?',
    'Comparative analysis of Portuguese diplomatic correspondence invoking the bull against Catholic rivals (Spain, France) versus against Protestant England and the Dutch Republic — if invocation only ever had traction with Catholic powers while non-Catholic exclusion relied entirely on naval force, the papal layer was cosmetic for roughly half the victim set from the start.',
    'If the papal legitimation never functioned against non-Catholic rivals, the true coordination function was narrower (Iberian intra-Catholic war avoidance only) and the exclusion of England/France/Netherlands was pure naval extraction dressed in a legal claim that never applied to them — pushing this reading closer to snare with respect to the non-Catholic victim subset.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(papal_authority_vs_naval_fact, conceptual, 'Whether papal authority ever bound non-Catholic rival powers or only ever functioned intra-Iberian.').

omega_variable(
    prior_exploration_priority_as_natural_vs_constructed_right,
    'Is ''prior exploration confers priority right'' a defensible natural-law-adjacent principle of international custom, or a constructed doctrine that happened to favor whichever power had a head start, retroactively dressed as principle?',
    'Comparative examination of whether the principle was applied consistently when it disfavored Portugal (e.g., in later disputes over first-discovery claims by other powers) versus only invoked when it favored the incumbent.',
    'If applied only self-servingly, the vindicated proposition of prior-discovery priority is itself an extraction artifact rather than a genuine emergent norm of the era''s law of nations, which would deepen the tangled_rope reading rather than support any mountain-adjacent naturalization of the doctrine.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(prior_exploration_priority_as_natural_vs_constructed_right, conceptual, 'Whether the prior-exploration-priority doctrine is a genuine customary norm or a self-serving construction.').

omega_variable(
    kernel_framing_divergence,
    'Given that the same instrument (papal bull + Tordesillas treaty) grounds two structurally distinct constraints depending on hemisphere and reading, is the appropriate unit of analysis the single 1494 treaty or two independently-evolving regional enforcement regimes that only shared a founding document?',
    'Track whether Portuguese eastern enforcement and Spanish western enforcement diverged in legal justification, enforcement mechanism, and victim set early enough that treating them as one kernel obscures more than it reveals — the Zaragoza renegotiation (1529) suggests the eastern line was functionally distinct and separately contested from the western division almost immediately.',
    'Supports the decomposition into this reading (portuguese_exploration_legitimation) and its sibling (spanish_conquest_legitimation) as genuinely separate constraints rather than two observational angles on one constraint — consistent with the ε-invariance principle, since the two readings'' extractiveness, victim sets, and mechanisms diverge substantially rather than being reconcilable measurements of a shared quantity.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_framing_divergence, conceptual, 'Whether the kernel is genuinely one instrument or two structurally divergent regional regimes sharing only a founding document.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, 1494, 1663).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tord_tr_t1494, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, theater_ratio, 1494, 0.15).
narrative_ontology:measurement(tord_tr_t1520, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, theater_ratio, 1520, 0.2).
narrative_ontology:measurement(tord_tr_t1560, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, theater_ratio, 1560, 0.28).
narrative_ontology:measurement(tord_tr_t1600, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, theater_ratio, 1600, 0.4).
narrative_ontology:measurement(tord_tr_t1630, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, theater_ratio, 1630, 0.55).
narrative_ontology:measurement(tord_tr_t1663, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, theater_ratio, 1663, 0.7).

% Extraction over time
narrative_ontology:measurement(tord_be_t1494, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, base_extractiveness, 1494, 0.35).
narrative_ontology:measurement(tord_be_t1520, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, base_extractiveness, 1520, 0.45).
narrative_ontology:measurement(tord_be_t1560, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, base_extractiveness, 1560, 0.6).
narrative_ontology:measurement(tord_be_t1600, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, base_extractiveness, 1600, 0.62).
narrative_ontology:measurement(tord_be_t1630, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, base_extractiveness, 1630, 0.5).
narrative_ontology:measurement(tord_be_t1663, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, base_extractiveness, 1663, 0.3).

% Suppression requirement over time
narrative_ontology:measurement(tord_su_t1494, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, suppression_requirement, 1494, 0.3).
narrative_ontology:measurement(tord_su_t1520, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, suppression_requirement, 1520, 0.42).
narrative_ontology:measurement(tord_su_t1560, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, suppression_requirement, 1560, 0.58).
narrative_ontology:measurement(tord_su_t1600, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, suppression_requirement, 1600, 0.65).
narrative_ontology:measurement(tord_su_t1630, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, suppression_requirement, 1630, 0.55).
narrative_ontology:measurement(tord_su_t1663, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, suppression_requirement, 1663, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, enforcement_mechanism).
narrative_ontology:affects_constraint(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, tordesillas_demarcation_kernel__spanish_conquest_legitimation).

% DUAL FORMULATION NOTE:
% This story and tordesillas_demarcation_kernel__spanish_conquest_legitimation decompose the natural-language concept 'the Tordesillas line' into two structurally distinct constraints sharing one founding kernel (the 1493 papal bulls and 1494 treaty). This story (Portuguese eastern reading) authors moderate extractiveness (peaking ~0.62) with rival European mercantile powers as the victim set and trade-monopoly rent as the extraction mechanism. The sibling story (Spanish western reading) is expected to author substantially higher extractiveness with indigenous American populations as the primary victim set and territorial conquest/tribute/labor extraction as the mechanism. Per the ε-invariance principle, these are two files, not one story with a hemisphere parameter.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
