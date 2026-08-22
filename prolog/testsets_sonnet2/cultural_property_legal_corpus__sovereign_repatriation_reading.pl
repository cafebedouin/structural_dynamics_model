% ============================================================================
% CONSTRAINT STORY: cultural_property_legal_corpus__sovereign_repatriation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_cultural_property_legal_corpus__sovereign_repatriation_reading, []).

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
 *   constraint_id: cultural_property_legal_corpus__sovereign_repatriation_reading
 *   human_readable: Sovereign Repatriation Reading of Cultural Property Legitimacy
 *   domain: international_law/cultural_property/post_colonial_studies
 *
 * SUMMARY:
 *   This story instantiates the sovereign-repatriation reading of the
 *   cultural property legitimacy kernel: cultural artifacts removed during
 *   colonial administration are sovereign property of the successor state,
 *   colonial acquisition was illegitimate extraction regardless of
 *   period-legal formalities, and legitimate authority to reclaim and hold
 *   the objects rests with the state that can assert historical continuity
 *   with the expropriated people. This is a distinct constraint from the
 *   universal_heritage_reading (which locates legitimacy in
 *   preservation/access institutions regardless of origin) and the
 *   indigenous_stewardship_reading (which locates legitimacy in the specific
 *   descendant community rather than the state apparatus). Under this
 *   reading, successor states are structural beneficiaries and encyclopedic
 *   holding institutions are the parties bearing extraction; the ε (0.52) is
 *   moderate rather than high because the transfer restores symbolic/cultural
 *   capital more than economic capital, and the process runs through
 *   negotiated, often reciprocal, diplomatic channels rather than unilateral
 *   seizure.
 *
 * KEY AGENTS:
 *   - successor_state_governments: primary beneficiary and agenda-setter (institutional/arbitrage) — files and prosecutes claims
 *   - holding_institution_encyclopedic_museums: primary target (powerful/constrained) — bears the custody and reputational transfer
 *   - diaspora_source_communities_without_state_recognition: excluded voice (powerless/trapped) — no standing under the state-to-state frame
 *   - market_intermediaries_auction_houses: secondary beneficiary (organized/mobile) — gains from provenance premium
 *   - international_cultural_law_bodies: analytical observer (institutional/analytical) — sets evidentiary standards for claims
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(cultural_property_legal_corpus__sovereign_repatriation_reading, 0.52).
domain_priors:suppression_score(cultural_property_legal_corpus__sovereign_repatriation_reading, 0.48).
domain_priors:theater_ratio(cultural_property_legal_corpus__sovereign_repatriation_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(cultural_property_legal_corpus__sovereign_repatriation_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(cultural_property_legal_corpus__sovereign_repatriation_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(cultural_property_legal_corpus__sovereign_repatriation_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(cultural_property_legal_corpus__sovereign_repatriation_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(cultural_property_legal_corpus__sovereign_repatriation_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(cultural_property_legal_corpus__sovereign_repatriation_reading, tangled_rope).
narrative_ontology:human_readable(cultural_property_legal_corpus__sovereign_repatriation_reading, "Sovereign Repatriation Reading of Cultural Property Legitimacy").
narrative_ontology:topic_domain(cultural_property_legal_corpus__sovereign_repatriation_reading, "international_law/cultural_property/post_colonial_studies").

domain_priors:requires_active_enforcement(cultural_property_legal_corpus__sovereign_repatriation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(cultural_property_legal_corpus__sovereign_repatriation_reading, 'ccd4e2e9-4c71-43c2-ba47-4107f8d44209').
narrative_ontology:cs_kernel_codification('ccd4e2e9-4c71-43c2-ba47-4107f8d44209', distributed).
narrative_ontology:cs_authority_grounding('ccd4e2e9-4c71-43c2-ba47-4107f8d44209', distributed).
narrative_ontology:cs_reading_relation('ccd4e2e9-4c71-43c2-ba47-4107f8d44209', cultural_property_legal_corpus__universal_heritage_reading, coexists_with).
narrative_ontology:cs_reading_relation('ccd4e2e9-4c71-43c2-ba47-4107f8d44209', cultural_property_legal_corpus__indigenous_stewardship_reading, influences).
narrative_ontology:cs_axiom('ccd4e2e9-4c71-43c2-ba47-4107f8d44209', foundational, colonial_title_is_void_ab_initio).
narrative_ontology:cs_axiom_status(colonial_title_is_void_ab_initio, holdable).
narrative_ontology:cs_axiom_grounding('ccd4e2e9-4c71-43c2-ba47-4107f8d44209', colonial_title_is_void_ab_initio, deontological).
narrative_ontology:cs_axiom('ccd4e2e9-4c71-43c2-ba47-4107f8d44209', foundational, state_succession_confers_cultural_standing).
narrative_ontology:cs_axiom_status(state_succession_confers_cultural_standing, holdable).
narrative_ontology:cs_axiom_grounding('ccd4e2e9-4c71-43c2-ba47-4107f8d44209', state_succession_confers_cultural_standing, conventional).
narrative_ontology:cs_reference_frame('ccd4e2e9-4c71-43c2-ba47-4107f8d44209', colonial_era_title_by_possession).
narrative_ontology:cs_drift_state('ccd4e2e9-4c71-43c2-ba47-4107f8d44209', post_1970_unesco_convention_era, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('ccd4e2e9-4c71-43c2-ba47-4107f8d44209', '').
narrative_ontology:cs_kernel_id(cultural_property_legal_corpus__sovereign_repatriation_reading, cultural_property_legal_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(cultural_property_legal_corpus__sovereign_repatriation_reading, successor_state_governments).
narrative_ontology:constraint_beneficiary(cultural_property_legal_corpus__sovereign_repatriation_reading, national_museums_of_origin_states).
narrative_ontology:constraint_victim(cultural_property_legal_corpus__sovereign_repatriation_reading, holding_institution_encyclopedic_museums).
narrative_ontology:constraint_victim(cultural_property_legal_corpus__sovereign_repatriation_reading, diaspora_source_communities_without_state_recognition).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(cultural_property_legal_corpus__sovereign_repatriation_reading, market_intermediaries_auction_houses).
narrative_ontology:constraint_vindicates(cultural_property_legal_corpus__sovereign_repatriation_reading, state_succession_doctrine).
narrative_ontology:constraint_vindicates(cultural_property_legal_corpus__sovereign_repatriation_reading, restitution_as_corrective_justice).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Files formal repatriation claims through diplomatic channels and international bodies (UNESCO 1970 Convention framework), asserting continuity with the pre-colonial polity from which artifacts were taken. Gains custody, symbolic legitimacy, and tourism/cultural capital when claims succeed. Can escalate through bilateral pressure, trade linkage, or multilateral fora; largely insulated from the internal costs of the claim process.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__sovereign_repatriation_reading, successor_state_governments, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(cultural_property_legal_corpus__sovereign_repatriation_reading, successor_state_governments, beneficiary).

% Holds contested objects acquired during colonial administration, often with paper title under period-legal export or purchase. Bears reputational, legal, and collection-integrity costs when claims are pressed; loses objects, narrative control, and donor confidence upon repatriation. Can resist through legal title defenses, loan arrangements, or partial-return diplomacy, but faces mounting normative pressure that narrows those options over time.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__sovereign_repatriation_reading, holding_institution_encyclopedic_museums, payer,
    powerful, biographical, constrained, global).

% Communities whose ancestors made or used the artifacts but who lack a recognized successor state (stateless nations, sub-national indigenous groups within states that do not represent their interests, diasporic populations). Have no standing under the state-to-state repatriation framework; when a state claim succeeds, the artifact often returns to a national museum rather than to the community with direct cultural continuity. Cannot independently file claims under the sovereign framework.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__sovereign_repatriation_reading, diaspora_source_communities_without_state_recognition, excluded,
    powerless, generational, trapped, regional).

% Benefits indirectly from heightened attention to provenance, which raises the value and legitimacy premium of clean-title objects and creates new due-diligence business lines. Can relocate transactions across jurisdictions with weaker enforcement of the sovereign claims framework.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__sovereign_repatriation_reading, market_intermediaries_auction_houses, beneficiary,
    organized, biographical, mobile, global).

% UNESCO, ICOM, and treaty secretariats mediate claims, draft soft-law instruments, and adjudicate disputed provenance. They do not own artifacts but shape which claims succeed by setting evidentiary and procedural standards for state successor status.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__sovereign_repatriation_reading, international_cultural_law_bodies, observer,
    institutional, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a legible, state-to-state channel for resolving contested title over cultural artifacts acquired under colonial administration, replacing ad hoc bilateral disputes with a recognized doctrine (state succession + illegitimate original acquisition) that institutions and governments can both invoke.
% TRANSFER_FUNCTION: Moves physical custody, symbolic capital, and narrative authority over artifacts from holding institutions in the Global North to the national museums and governments of the states claiming historical continuity with the peoples from whom the artifacts were taken.
% ABSENT_VOICES: Sub-national indigenous and diasporic communities whose historical continuity with the artifacts is often more direct than the successor state's, but who have no standing to file claims under a framework that transacts only in state sovereignty. Their objection — that repatriation to a state is not repatriation to a people — is rarely part of the formal proceeding.
% DISAPPEARANCE_RATIONALE: If the sovereign-repatriation doctrine vanished, dozens of active claims (Benin Bronzes, Parthenon Marbles, Koh-i-Noor-adjacent disputes) would lose their legal and rhetorical anchor; holding institutions would revert to pure possession-and-title defenses, and successor states would lose their primary lever for reclaiming both objects and the narrative of historical wrong. Bilateral negotiations, museum de-accessioning policy, and treaty practice would all reorganize around whatever doctrine replaced it.
% FOUNDING_PROBLEM: Colonial-era removal of cultural artifacts under conditions of military conquest, unequal treaties, or administrative expropriation left encyclopedic museums holding vast collections with contested legitimacy of title, while the polities of origin had no legal mechanism to contest that title once formal decolonization occurred.
% FOUNDING_PROBLEM_CORROBORATION: Independent provenance historians and the UNESCO 1970 Convention's own drafting record corroborate that colonial-era acquisition frequently involved coercion or unequal bargaining power, external to the successor states' own advocacy. However, provenance scholars and anthropologists outside the state apparatus also attest that the 'successor state' frame itself does not always track the actual descendant community, which is a distinct and contested layer of the founding problem's resolution.
narrative_ontology:disappearance_verdict(cultural_property_legal_corpus__sovereign_repatriation_reading, world_rearranges).
narrative_ontology:founding_problem_status(cultural_property_legal_corpus__sovereign_repatriation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(cultural_property_legal_corpus__sovereign_repatriation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(cultural_property_legal_corpus__sovereign_repatriation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(cultural_property_legal_corpus__sovereign_repatriation_reading, 0.52, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(cultural_property_legal_corpus__sovereign_repatriation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(cultural_property_legal_corpus__sovereign_repatriation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(cultural_property_legal_corpus__sovereign_repatriation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.52) because the sovereign-repatriation framework operates through legible legal and diplomatic channels rather than coercive seizure — holding institutions retain procedural recourse (title defense, loan agreements) even as normative pressure mounts. Suppression (0.48) reflects the genuine but partial closing-off of the 'keep by possession' alternative as international soft law hardens; it is not yet a hard legal mandate in most jurisdictions. Theater ratio is low-moderate (0.28): claims and returns are substantively consequential (title and custody actually transfer), though ceremonial handover events carry a performative component. Accessibility collapse (0.40) is moderate: holding institutions still have live legal and negotiating alternatives, which keeps this well below mountain-level closure.
 *
 * DIRECTIONALITY LOGIC:
 *   Successor state governments derive low d (near-beneficiary) because the doctrine's entire function is to route custody, legitimacy, and narrative capital to them — they set the terms and bear little structural cost. Holding institutions derive high d (near-target) because the same doctrine that others experience as coordination operates on them as an enforced extraction of collection assets and narrative authority — arbitrage exit is foreclosed by treaty commitments and reputational lock-in. Diaspora/sub-state communities occupy an anomalous position: they are neither beneficiaries nor conventional targets — the doctrine simply does not model them, which is why they sit in the excluded role rather than payer or beneficiary.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (illegitimate colonial-era extraction without a state-to-state remedy) remains substantially live — active claims continue to surface and succeed. This blocks a mandatrophy verdict at the doctrine level: the arrangement has not simply outlived a solved problem. However, the corroboration record flags a narrower mandatrophy risk within the successor-state framing itself: as states increasingly use repatriation claims for legitimacy-building purposes disconnected from the actual descendant community's interests, the doctrine's original justice rationale is at risk of being retained as cover for a state-capital-accumulation function distinct from restitution to the people actually harmed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    state_vs_community_continuity,
    'Does the successor state''s claim to historical continuity with the expropriated people track the actual descendant community, or does it substitute state sovereignty for a continuity claim that more properly belongs to a sub-national or diasporic group?',
    'Case-by-case anthropological and historical assessment of whether the claiming state''s population, governance, and cultural institutions descend from the specific group whose artifacts were taken, versus cases where post-colonial state boundaries do not track pre-colonial polities.',
    'Where continuity is genuine, the sovereign framework functions as intended restitution. Where continuity is thin or contested, the successor state functions as a second extractor of identity capital that was never fully returned to the community that suffered the original expropriation — this would push the classification toward tangled_rope with an additional, unaddressed victim class.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_vs_community_continuity, conceptual, 'Whether state successor status reliably tracks the actual harmed community or substitutes for it.').

omega_variable(
    colonial_acquisition_illegitimacy_scope,
    'Is ''colonial acquisition was illegitimate extraction'' a claim about all colonial-era transfers uniformly, or does it require case-by-case proof of coercion, given that some acquisitions involved documented consensual exchange even under colonial administration?',
    'Provenance research distinguishing documented coercive seizure (military looting, forced sale under occupation) from contested-but-formally-consensual transactions (purchase, gift, commissioned work) within the colonial period.',
    'A uniform illegitimacy premise raises ε and strengthens the sovereign claim''s suppression of holding-institution alternatives; a case-by-case standard would lower effective extraction for well-documented consensual transfers and narrow the doctrine''s scope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(colonial_acquisition_illegitimacy_scope, empirical, 'Whether the illegitimacy premise applies categorically or requires per-case coercion evidence.').

omega_variable(
    reading_framing_underdetermination,
    'Is the choice to frame legitimate authority around the SUCCESSOR STATE (rather than the specific descendant community, or the preserving institution) itself a contestable framing choice that the sovereign-repatriation reading naturalizes?',
    'Compare outcomes across the three sibling readings'' claim-resolution records: track how often successor-state-led claims deliver custody to entities other than the sub-national community with the most direct cultural continuity.',
    'If sovereign-state framing systematically diverts custody away from direct descendant communities, the sovereign_repatriation_reading''s cs_pattern may itself function as an intermediate extractor layered between the colonial-era wrong and its actual remedy, which would be a distinct finding from either sibling reading''s own internal analysis.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_framing_underdetermination, conceptual, 'Whether the state-centered framing itself introduces a novel extraction layer distinct from the colonial wrong it purports to remedy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(cultural_property_legal_corpus__sovereign_repatriation_reading, 1970, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cult_tr_t1970, cultural_property_legal_corpus__sovereign_repatriation_reading, theater_ratio, 1970, 0.2).
narrative_ontology:measurement(cult_tr_t1985, cultural_property_legal_corpus__sovereign_repatriation_reading, theater_ratio, 1985, 0.22).
narrative_ontology:measurement(cult_tr_t2000, cultural_property_legal_corpus__sovereign_repatriation_reading, theater_ratio, 2000, 0.24).
narrative_ontology:measurement(cult_tr_t2010, cultural_property_legal_corpus__sovereign_repatriation_reading, theater_ratio, 2010, 0.26).
narrative_ontology:measurement(cult_tr_t2018, cultural_property_legal_corpus__sovereign_repatriation_reading, theater_ratio, 2018, 0.27).
narrative_ontology:measurement(cult_tr_t2024, cultural_property_legal_corpus__sovereign_repatriation_reading, theater_ratio, 2024, 0.28).

% Extraction over time
narrative_ontology:measurement(cult_be_t1970, cultural_property_legal_corpus__sovereign_repatriation_reading, base_extractiveness, 1970, 0.3).
narrative_ontology:measurement(cult_be_t1985, cultural_property_legal_corpus__sovereign_repatriation_reading, base_extractiveness, 1985, 0.34).
narrative_ontology:measurement(cult_be_t2000, cultural_property_legal_corpus__sovereign_repatriation_reading, base_extractiveness, 2000, 0.4).
narrative_ontology:measurement(cult_be_t2010, cultural_property_legal_corpus__sovereign_repatriation_reading, base_extractiveness, 2010, 0.45).
narrative_ontology:measurement(cult_be_t2018, cultural_property_legal_corpus__sovereign_repatriation_reading, base_extractiveness, 2018, 0.49).
narrative_ontology:measurement(cult_be_t2024, cultural_property_legal_corpus__sovereign_repatriation_reading, base_extractiveness, 2024, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(cult_su_t1970, cultural_property_legal_corpus__sovereign_repatriation_reading, suppression_requirement, 1970, 0.32).
narrative_ontology:measurement(cult_su_t1985, cultural_property_legal_corpus__sovereign_repatriation_reading, suppression_requirement, 1985, 0.35).
narrative_ontology:measurement(cult_su_t2000, cultural_property_legal_corpus__sovereign_repatriation_reading, suppression_requirement, 2000, 0.38).
narrative_ontology:measurement(cult_su_t2010, cultural_property_legal_corpus__sovereign_repatriation_reading, suppression_requirement, 2010, 0.42).
narrative_ontology:measurement(cult_su_t2018, cultural_property_legal_corpus__sovereign_repatriation_reading, suppression_requirement, 2018, 0.45).
narrative_ontology:measurement(cult_su_t2024, cultural_property_legal_corpus__sovereign_repatriation_reading, suppression_requirement, 2024, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(cultural_property_legal_corpus__sovereign_repatriation_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(cultural_property_legal_corpus__sovereign_repatriation_reading, 0.12).
narrative_ontology:affects_constraint(cultural_property_legal_corpus__sovereign_repatriation_reading, cultural_property_legal_corpus__universal_heritage_reading).
narrative_ontology:affects_constraint(cultural_property_legal_corpus__sovereign_repatriation_reading, cultural_property_legal_corpus__indigenous_stewardship_reading).

% DUAL FORMULATION NOTE:
% This story is one of three siblings decomposing the natural-language 'cultural property legitimacy' concept per the ε-invariance principle. universal_heritage_reading locates legitimacy in preservation-optimizing institutions (holding institutions become beneficiaries, not victims); indigenous_stewardship_reading locates legitimacy in the specific descendant community rather than the successor state (successor states themselves become a second-order extractor in that reading). Each reading has a distinct beneficiary/victim structure and a distinct ε; they are linked here as a constraint family rather than merged into one story with an observable parameter, per DP-001.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
