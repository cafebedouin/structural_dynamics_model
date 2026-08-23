% ============================================================================
% CONSTRAINT STORY: statute_of_anne_ip_foundation__conceptual_emergence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_statute_of_anne_ip_foundation__conceptual_emergence_reading, []).

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
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: statute_of_anne_ip_foundation__conceptual_emergence_reading
 *   human_readable: Statute of Anne Frame: Copyright as Limited Regulatory Instrument for Learning (Conceptual Emergence Reading)
 *   domain: legal_history/intellectual_property/institutional_economics
 *
 * SUMMARY:
 *   This story instantiates the conceptual-emergence reading of the Statute
 *   of Anne (1710): the act did not merely adjust who held publishing
 *   privileges - it constituted copyright as a new legal category, a bounded
 *   statutory instrument subordinate to the encouragement of learning,
 *   displacing the perpetual-property conception embodied in the Stationers'
 *   Company register. The standing arrangement under assessment is that frame
 *   itself: exclusive, term-limited rights (fourteen years for new works,
 *   twenty-one for existing stock) that terminate by design into a public
 *   domain. Its declared beneficiary is public learning, borne by the reading
 *   public and working authors; its declared casualty is perpetual monopoly
 *   in texts, borne by the London booksellers who held the perpetual
 *   registrations and by the provincial printers whose reprint trade the
 *   frame's enforcement suppressed. Assumptions stated: the interval maps
 *   0-64 onto 1710-1774 (founding to Donaldson v Becket); provenance commit
 *   fields are pipeline placeholders pending stamping.
 *
 * KEY AGENTS:
 *   - parliament_frame_setters: agenda setter (institutional/arbitrage) - enacted the frame, set term lengths, retained amendment power
 *   - london_booksellers: pivot seat (organized/constrained) - dispossessed of perpetual rights yet compensated with twenty-one-year terms; captured most term rents; funded the sixty-year perpetuity litigation
 *   - reading_public: primary beneficiary (powerless/mobile) - gains publication incentives and guaranteed eventual free access; pays elevated prices during each term
 *   - working_authors: secondary beneficiary (moderate/constrained) - gained a novel statutory asset, the saleable fourteen-year right of first publication
 *   - provincial_pirate_printers: excluded target (moderate/mobile) - Edinburgh and Dublin reprint networks outside the Westminster bargain, bearing enforcement
 *   - historiographic_analysts: analytical observer - modern legal historians assessing the frame from outside it
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(statute_of_anne_ip_foundation__conceptual_emergence_reading, 0.22).
domain_priors:suppression_score(statute_of_anne_ip_foundation__conceptual_emergence_reading, 0.3).
domain_priors:theater_ratio(statute_of_anne_ip_foundation__conceptual_emergence_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__conceptual_emergence_reading, extractiveness, 0.22).
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__conceptual_emergence_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__conceptual_emergence_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__conceptual_emergence_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__conceptual_emergence_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(statute_of_anne_ip_foundation__conceptual_emergence_reading, tangled_rope).
narrative_ontology:human_readable(statute_of_anne_ip_foundation__conceptual_emergence_reading, "Statute of Anne Frame: Copyright as Limited Regulatory Instrument for Learning (Conceptual Emergence Reading)").
narrative_ontology:topic_domain(statute_of_anne_ip_foundation__conceptual_emergence_reading, "legal_history/intellectual_property/institutional_economics").

domain_priors:requires_active_enforcement(statute_of_anne_ip_foundation__conceptual_emergence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(statute_of_anne_ip_foundation__conceptual_emergence_reading, '9372162c-2d1f-4df4-853f-ff08415e8ea0').
narrative_ontology:cs_kernel_codification('9372162c-2d1f-4df4-853f-ff08415e8ea0', formalized).
narrative_ontology:cs_authority_grounding('9372162c-2d1f-4df4-853f-ff08415e8ea0', lineage).
narrative_ontology:cs_interpretation_layer_present('9372162c-2d1f-4df4-853f-ff08415e8ea0').
narrative_ontology:cs_reading_relation('9372162c-2d1f-4df4-853f-ff08415e8ea0', statute_of_anne_ip_foundation__institutional_reallocation_reading, forecloses).
narrative_ontology:cs_reading_relation('9372162c-2d1f-4df4-853f-ff08415e8ea0', statute_of_anne_ip_foundation__entangled_event_reading, forecloses).
narrative_ontology:cs_axiom('9372162c-2d1f-4df4-853f-ff08415e8ea0', foundational, copyright_constituted_by_limited_statutory_grant).
narrative_ontology:cs_axiom_status(copyright_constituted_by_limited_statutory_grant, holdable).
narrative_ontology:cs_axiom_grounding('9372162c-2d1f-4df4-853f-ff08415e8ea0', copyright_constituted_by_limited_statutory_grant, conventional).
narrative_ontology:cs_axiom('9372162c-2d1f-4df4-853f-ff08415e8ea0', foundational, exclusive_rights_must_expire_for_learning).
narrative_ontology:cs_axiom_status(exclusive_rights_must_expire_for_learning, holdable).
narrative_ontology:cs_axiom_grounding('9372162c-2d1f-4df4-853f-ff08415e8ea0', exclusive_rights_must_expire_for_learning, instrumental).
narrative_ontology:cs_reference_frame('9372162c-2d1f-4df4-853f-ff08415e8ea0', copyright_as_limited_regulatory_instrument).
narrative_ontology:cs_drift_state('9372162c-2d1f-4df4-853f-ff08415e8ea0', contemporary_extended_term_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('9372162c-2d1f-4df4-853f-ff08415e8ea0', '').
narrative_ontology:cs_kernel_id(statute_of_anne_ip_foundation__conceptual_emergence_reading, statute_of_anne_ip_foundation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(statute_of_anne_ip_foundation__conceptual_emergence_reading, reading_public).
narrative_ontology:constraint_beneficiary(statute_of_anne_ip_foundation__conceptual_emergence_reading, working_authors).
narrative_ontology:constraint_beneficiary(statute_of_anne_ip_foundation__conceptual_emergence_reading, london_booksellers).
narrative_ontology:constraint_victim(statute_of_anne_ip_foundation__conceptual_emergence_reading, london_booksellers).
narrative_ontology:constraint_victim(statute_of_anne_ip_foundation__conceptual_emergence_reading, provincial_pirate_printers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(statute_of_anne_ip_foundation__conceptual_emergence_reading, reading_public).
narrative_ontology:constraint_vindicates(statute_of_anne_ip_foundation__conceptual_emergence_reading, limited_term_encourages_learning).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Drafted and enacted the 1710 act after the Licensing Act lapsed and the trade descended into piracy and uncertain title. Fixed the term lengths (fourteen years for new works, twenty-one for existing stock), designated the Stationers' Company register as the administrative record, and attached penalties to unauthorized reprinting. Retained power to amend or let the scheme lapse; motivated by trade order and the stated goal of encouraging learned men to compose and publish useful books.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__conceptual_emergence_reading, parliament_frame_setters, agenda_setter,
    institutional, generational, arbitrage, national).

% Held the perpetual registrations that the new terms displaced, and received twenty-one-year confirmations on existing stock in exchange. Dominated the London trade, bought most authors' copy, and collected the bulk of exclusive-market revenue during each term. Fought a sixty-year parliamentary and courtroom campaign to have their holdings recognized as perpetual literary property, funding pamphlets, petitions, and test cases. Their capital and status were sunk in the registered-stock system, so leaving the trade meant writing off that capital.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__conceptual_emergence_reading, london_booksellers, payer,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(statute_of_anne_ip_foundation__conceptual_emergence_reading, london_booksellers, beneficiary).

% Buys books under exclusive terms at prices the rights-holder sets, and receives each work freely once its term expires. Gains from the widened incentive to print new and imported learning, and from the guarantee that no text is locked up forever. Has no vote, no guild, and no lobby; its interests are voiced indirectly, if at all, and it can respond only by buying, borrowing, waiting out terms, or turning to reprints.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__conceptual_emergence_reading, reading_public, beneficiary,
    powerless, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(statute_of_anne_ip_foundation__conceptual_emergence_reading, reading_public, payer).

% Gained for the first time a personal, statutory right to control first publication of a new work for fourteen years, saleable to a bookseller for ready money - previously their bargaining position ran through patronage or outright sale of manuscript. Dependent on the London trade to realize the right's value; authors of works already in print before 1710 received nothing, since the twenty-one-year terms went to current stockholders.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__conceptual_emergence_reading, working_authors, beneficiary,
    moderate, biographical, constrained, national).

% Printers and booksellers in Edinburgh, Dublin, and beyond who reprinted London titles without license, underselling the rights-holders. Outside the Westminster bargain that set the terms, they nonetheless bore its enforcement: prosecutions, seizures, and customs actions. Their cross-border operations let them continue largely regardless of what London enacted, and their cheap editions were often the only editions many readers could afford.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__conceptual_emergence_reading, provincial_pirate_printers, excluded,
    moderate, immediate, mobile, continental).

% Modern legal historians and intellectual-property scholars who examine the 1710 act from outside the frame it created, reconstructing its drafting, its passage, and its reception. They produce the competing accounts of what the act structurally accomplished and supply the evidentiary basis on which any adjudication between those accounts proceeds.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__conceptual_emergence_reading, historiographic_analysts, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(statute_of_anne_ip_foundation__conceptual_emergence_reading, london_booksellers).
narrative_ontology:fixing_cost_class(statute_of_anne_ip_foundation__conceptual_emergence_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the post-Licensing-Act publication problem: getting new and foreign learning composed, printed, and distributed in a trade where copying is cheap and titles are uncertain, by granting bounded exclusive windows that make publication investment recoverable while guaranteeing every text eventually enters common circulation.
% TRANSFER_FUNCTION: Moves exclusive-market revenue from book buyers to rights-holders - authors selling copy, booksellers holding terms - for fourteen to twenty-one years per work, after which the text itself moves, without payment, to the reading public.
% ABSENT_VOICES: Book buyers had no franchise, guild, or lobby at Westminster; provincial and Irish printers were outside the bargain that set the terms they would be prosecuted under; and future generations, who would inherit each text at expiry, were represented by no one - the twenty-one-year confirmations for existing stock were negotiated by men who would not live to see them lapse. All three seats are absent from the legislative record except as objects of policy.
% DISAPPEARANCE_RATIONALE: Overnight removal of the frame reverts text rights to the pre-1710 equilibrium: perpetual trade monopoly through the Stationers' register where enforcement exists, open piracy where it does not, and patronage-dependent publication otherwise. Every downstream arrangement - term-based licensing, the public domain as a legal category, the modern copyright architecture descended from this act - presupposes the frame; none of it survives its removal intact.
% FOUNDING_PROBLEM: With the Licensing Act lapsed in 1695, pre-publication censorship ended and the Stationers' licensed monopoly ended with it; the trade faced piracy, uncertain title, and collapsing investment in new works, while Parliament declined to restore licensing. The act was built to encourage learning by granting limited statutory terms instead of reviving either censorship or perpetuity.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated outside the beneficiary set: the parliamentary journals of 1709-1710 record the trade-disorder and encouragement-of-learning debates; the act's own preamble states the problem; and modern legal-historical scholarship (Patterson, Rose, Deazley) attests both the founding problem and the dispute over what the act actually did, from academic seats with no stake in the frame. The Stationers' petitions also attest the disorder, but from inside the trade parties, and are weighted accordingly.
narrative_ontology:disappearance_verdict(statute_of_anne_ip_foundation__conceptual_emergence_reading, world_rearranges).
narrative_ontology:founding_problem_status(statute_of_anne_ip_foundation__conceptual_emergence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(statute_of_anne_ip_foundation__conceptual_emergence_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(statute_of_anne_ip_foundation__conceptual_emergence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(statute_of_anne_ip_foundation__conceptual_emergence_reading, 0.22, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(statute_of_anne_ip_foundation__conceptual_emergence_reading_tests).
:- end_tests(statute_of_anne_ip_foundation__conceptual_emergence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Epsilon is authored at 0.22 with referent fixed to the standing arrangement - the limited-term frame as instituted - assessed by this reading's own lights, which credit the frame with genuinely serving learning; the residual extraction is the bookseller capture documented in the stakeholder surface. Suppression 0.3 is a raw structural property, unscaled by power or scope: statutory penalties and Stationer policing were real but light next to the licensing regime the frame replaced. Theater 0.15: the learning rationale was substantially operative at founding, with a growing rhetorical share covering trade rent-seeking by mid-interval. Accessibility_collapse 0.4: alternatives - the perpetual-property conception above all - remained partly arguable throughout, as the sixty-year litigation campaign demonstrates; the frame narrowed the option space without closing it. Resistance 0.6 is the highest-authored metric, reflecting organized, well-funded counter-mobilization by the dispossessed proprietors - notably a coalition of propertied losers rather than powerless victims. The measurement series share one grid (t=0,10,20,30,40,50,64); suppression_requirement series are deliberately omitted because the story does not track enforcement-capacity change - the static picture is carried by the scalar. The extraction arc rises to a mid-interval peak during the perpetuity-litigation decades and falls after the 1774 rejection of perpetual copyright; base_properties values are end-state readings taken at the post-consolidation trough.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute different types from identical structure. From the parliament seat the frame is deliberate institutional design - a solved coordination problem. From the bookseller seat it is simultaneously dispossession (perpetuity destroyed) and subsidy (enforceable terms, protected markets) - the pivot seat experiences cost and benefit through the same clauses, which is why its directionality is overridden toward symmetry. From the reader seat the frame is deferred access: monopoly prices now, free texts later, a trade whose fairness depends on a generational horizon the payer cannot personally reach. From the provincial printer seat it is jurisdictional exclusion enforced at their cost. The perpetuity faction's resistance fused professional identity with economic interest - proprietorship argued as natural right - so their exit was psychologically nearer identity-lock than their legal position implied; the frame's consolidation required breaking that fusion, which Donaldson v Becket did.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations: reading_public and working_authors derive low d (subsidized seats; readers additionally carry a secondary payer role that lifts them off zero). london_booksellers carry both victim and beneficiary declarations; the derivation cannot weigh dispossession against compensation, so an explicit override sets the organized seat to d=0.5 - within this story the organized atom belongs to the booksellers alone, making the override surgical. provincial_pirate_printers derive high d as enforcement targets, damped by genuine cross-border mobility (arbitrage-grade reprinting outside the frame's jurisdiction). parliament_frame_setters sit near the beneficiary end as the frame's author and administrator. The declared casualty 'perpetual monopoly' is a practice, not an agent; its bearers are seated as london_booksellers (payer) and its suppression is the frame's celebrated achievement under this reading - which is exactly why the reading's low epsilon and the frame's real enforcement must be authored side by side rather than reconciled.
 *
 * MANDATROPHY ANALYSIS:
 *   Claiming tangled_rope keeps both faces visible: the coordination function (incentive-compatible publication with guaranteed expiry) prevents the frame from being misread as pure extraction against the booksellers, while the victim declarations prevent the celebratory category-creation story from laundering the old-term capture and reader pricing as pure public service. The frame is deliberately NOT scaffold despite its per-work sunsets: term expiry is internal mechanics of the steady state, not a transition mandate - mislabeling it scaffold would falsely predict frame retirement, when in fact the frame became permanent constitutional architecture. The founding problem (incentivizing publication while preserving access) remains live, so no mandatrophy is declared; the R5 interview records status live paired with world_rearranges, a consistent pairing that flags no zombie condition.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest_delta,
    'Which structural delta does the Statute of Anne instantiate: category-creation (this reading), occupant-reallocation within a pre-existing space (institutional_reallocation_reading), or inseparable simultaneous dimensions (entangled_event_reading)?',
    'Comparative compilation of the sibling stories plus historiographic adjudication along the Rose/Feather versus Deazley/Patterson lines; convergence on a single delta reading would retire the others.',
    'If reallocation is correct, this story''s epsilon referent shifts to a rights-transfer arrangement with the Stationers as primary losers and no public-learning beneficiary primacy; if entanglement is correct, neither dimension is separately classifiable and this file dissolves into the entangled story.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest_delta, conceptual, 'Which of three rival structural deltas the statute''s operation instantiates.').

omega_variable(
    prior_existence_of_ip_category,
    'Did a distinct literary-property category exist in pre-1710 discourse such that the statute changed occupants rather than creating the space?',
    'Period-source analysis of pre-1710 trade and legal discourse: whether ''copy'' was conceived as a property category or as a guild registration privilege.',
    'Attested pre-existence collapses this reading into the reallocation sibling and flips the foreclosure edge; confirmed absence secures this reading''s foundational axiom.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(prior_existence_of_ip_category, empirical, 'Whether the IP category pre-existed the statute - the pivot premise separating this reading from its reallocation sibling.').

omega_variable(
    public_learning_beneficiary_primacy,
    'Did the frame''s benefits accrue primarily to public learning at founding, or to the trade, with public benefit arriving only as terms began expiring?',
    'Price and output series 1710-1774 against pre-statute baselines, plus term-expiry cohort analysis of when texts actually reached the public domain.',
    'If trade-first, effective extraction from readers runs higher than authored in early intervals and the frame''s coordination character weakens at earlier time points; the end-state classification is less affected.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(public_learning_beneficiary_primacy, empirical, 'Timing and incidence of the public-learning benefit this reading declares.').

omega_variable(
    term_extension_drift_status,
    'Is contemporary extended-term practice a departure from the limited-regulatory-tool frame (practice_drift) or its continuation under changed economics?',
    'Compare statutory rationales across the 1710 preamble and modern term legislation; measure term-length elasticity to rightsholder lobbying.',
    'Reading as departure supports the substantial drift magnitude authored here and pressures the computed terminal attractor toward frame-replacement; reading as continuation stabilizes this reading''s frame and softens the drift vector.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(term_extension_drift_status, conceptual, 'Whether modern term extension constitutes drift from, or evolution of, the frame this reading institutes.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(statute_of_anne_ip_foundation__conceptual_emergence_reading, 0, 64).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t0, statute_of_anne_ip_foundation__conceptual_emergence_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(stat_tr_t0, observed).
narrative_ontology:measurement(stat_tr_t10, statute_of_anne_ip_foundation__conceptual_emergence_reading, theater_ratio, 10, 0.11).
narrative_ontology:measurement_basis(stat_tr_t10, observed).
narrative_ontology:measurement(stat_tr_t20, statute_of_anne_ip_foundation__conceptual_emergence_reading, theater_ratio, 20, 0.15).
narrative_ontology:measurement_basis(stat_tr_t20, observed).
narrative_ontology:measurement(stat_tr_t30, statute_of_anne_ip_foundation__conceptual_emergence_reading, theater_ratio, 30, 0.19).
narrative_ontology:measurement_basis(stat_tr_t30, observed).
narrative_ontology:measurement(stat_tr_t40, statute_of_anne_ip_foundation__conceptual_emergence_reading, theater_ratio, 40, 0.18).
narrative_ontology:measurement_basis(stat_tr_t40, observed).
narrative_ontology:measurement(stat_tr_t50, statute_of_anne_ip_foundation__conceptual_emergence_reading, theater_ratio, 50, 0.16).
narrative_ontology:measurement_basis(stat_tr_t50, observed).
narrative_ontology:measurement(stat_tr_t64, statute_of_anne_ip_foundation__conceptual_emergence_reading, theater_ratio, 64, 0.15).
narrative_ontology:measurement_basis(stat_tr_t64, observed).

% Extraction over time
narrative_ontology:measurement(stat_be_t0, statute_of_anne_ip_foundation__conceptual_emergence_reading, base_extractiveness, 0, 0.16).
narrative_ontology:measurement_basis(stat_be_t0, observed).
narrative_ontology:measurement(stat_be_t10, statute_of_anne_ip_foundation__conceptual_emergence_reading, base_extractiveness, 10, 0.2).
narrative_ontology:measurement_basis(stat_be_t10, observed).
narrative_ontology:measurement(stat_be_t20, statute_of_anne_ip_foundation__conceptual_emergence_reading, base_extractiveness, 20, 0.26).
narrative_ontology:measurement_basis(stat_be_t20, observed).
narrative_ontology:measurement(stat_be_t30, statute_of_anne_ip_foundation__conceptual_emergence_reading, base_extractiveness, 30, 0.31).
narrative_ontology:measurement_basis(stat_be_t30, observed).
narrative_ontology:measurement(stat_be_t40, statute_of_anne_ip_foundation__conceptual_emergence_reading, base_extractiveness, 40, 0.29).
narrative_ontology:measurement_basis(stat_be_t40, observed).
narrative_ontology:measurement(stat_be_t50, statute_of_anne_ip_foundation__conceptual_emergence_reading, base_extractiveness, 50, 0.25).
narrative_ontology:measurement_basis(stat_be_t50, observed).
narrative_ontology:measurement(stat_be_t64, statute_of_anne_ip_foundation__conceptual_emergence_reading, base_extractiveness, 64, 0.22).
narrative_ontology:measurement_basis(stat_be_t64, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(statute_of_anne_ip_foundation__conceptual_emergence_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(statute_of_anne_ip_foundation__conceptual_emergence_reading, resource_allocation).
narrative_ontology:affects_constraint(statute_of_anne_ip_foundation__conceptual_emergence_reading, statute_of_anne_ip_foundation__institutional_reallocation_reading).
narrative_ontology:affects_constraint(statute_of_anne_ip_foundation__conceptual_emergence_reading, statute_of_anne_ip_foundation__entangled_event_reading).

% DUAL FORMULATION NOTE:
% Kernel family: statute_of_anne_ip_foundation decomposes into three reading-constraints (conceptual_emergence, institutional_reallocation, entangled_event). This file instantiates the conceptual-emergence reading with epsilon referenced to the limited-term frame as instituted; sibling files carry their own epsilon, victim sets, and classifications. Edges here are family-membership pointers; typed premise relations are declared in cs_structure.reading_relations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(statute_of_anne_ip_foundation__conceptual_emergence_reading, organized, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
