% ============================================================================
% CONSTRAINT STORY: territorial_legitimacy_dual__zionist_refuge_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_territorial_legitimacy_dual__zionist_refuge_reading, []).

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
 *   constraint_id: territorial_legitimacy_dual__zionist_refuge_reading
 *   human_readable: Zionist Refuge Reading of Territorial Legitimacy
 *   domain: political theory/international relations/territorial sovereignty
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the territorial_legitimacy_dual
 *   kernel: the Zionist refuge reading, on which Israel's sovereignty is
 *   legitimate because it answered a real persecution catastrophe (historical
 *   record), fulfills a covenantal claim (divine promise), and was sanctioned
 *   by the international system (UN partition acceptance). Per Rule 1, the
 *   contest is NOT described inside the constraint: this file authors a
 *   single clean epsilon over the fixed referent — the standing sovereignty
 *   arrangement — assessed by this reading's own lights. By those lights the
 *   1948 core is legitimate and uncontested, the displacement is framed as a
 *   consequence of Arab rejection of partition, and post-1967 control is
 *   security-justified and nominally negotiable; the costs this reading
 *   nonetheless registers (occupied-population administration, foreclosed
 *   return, expanding settlement) concentrate after 1967. The claimed type
 *   (tangled_rope) and the metrics are authored independently: the claim
 *   states what I believe is structurally true of the arrangement (genuine
 *   refuge/identity coordination fused with asymmetric costs borne by
 *   displaced and occupied populations, held up by active enforcement), while
 *   the metrics describe its actual operation. KEY AGENTS (by structural
 *   relationship): see key_agents; the same seven seats populate
 *   stakeholders[].
 *
 * KEY AGENTS:
 *   - israeli_state_institutions: agenda-setter (institutional / identity_locked) — administers territory, transmits the founding narrative, and runs the enforcement that keeps the arrangement intact
 *   - diaspora_jewish_communities: beneficiary carrying payer costs (organized / identity_locked) — draws identity continuity and refuge assurance, funds the arrangement's defense, absorbs backlash abroad
 *   - western_allied_governments: beneficiary (institutional / mobile) — collects alliance stability and post-Holocaust moral closure, pays diplomatic defense costs in multilateral forums
 *   - palestinian_refugee_displaced: primary target (powerless / trapped) — bears permanent displacement and foreclosed return under the reading's attribution frame
 *   - west_bank_palestinians_under_occupation: target (moderate / trapped) — bears administered land, movement restriction, and settlement expansion the reading justifies as security
 *   - binational_state_advocates: excluded voice (moderate / constrained) — proposes the single-state alternative the reading's frame treats as category error
 *   - international_legal_bodies: analytical observer (institutional / analytical) — adjudicates and documents the contest without enforcement power
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(territorial_legitimacy_dual__zionist_refuge_reading, 0.54).
domain_priors:suppression_score(territorial_legitimacy_dual__zionist_refuge_reading, 0.68).
domain_priors:theater_ratio(territorial_legitimacy_dual__zionist_refuge_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(territorial_legitimacy_dual__zionist_refuge_reading, extractiveness, 0.54).
narrative_ontology:constraint_metric(territorial_legitimacy_dual__zionist_refuge_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(territorial_legitimacy_dual__zionist_refuge_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(territorial_legitimacy_dual__zionist_refuge_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(territorial_legitimacy_dual__zionist_refuge_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(territorial_legitimacy_dual__zionist_refuge_reading, tangled_rope).
narrative_ontology:human_readable(territorial_legitimacy_dual__zionist_refuge_reading, "Zionist Refuge Reading of Territorial Legitimacy").
narrative_ontology:topic_domain(territorial_legitimacy_dual__zionist_refuge_reading, "political theory/international relations/territorial sovereignty").

domain_priors:requires_active_enforcement(territorial_legitimacy_dual__zionist_refuge_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(territorial_legitimacy_dual__zionist_refuge_reading, '5a339aa8-d43b-42f8-9328-4e051f458046').
narrative_ontology:cs_kernel_codification('5a339aa8-d43b-42f8-9328-4e051f458046', fixed_text).
narrative_ontology:cs_authority_grounding('5a339aa8-d43b-42f8-9328-4e051f458046', lineage).
narrative_ontology:cs_interpretation_layer_present('5a339aa8-d43b-42f8-9328-4e051f458046').
narrative_ontology:cs_reading_relation('5a339aa8-d43b-42f8-9328-4e051f458046', territorial_legitimacy_dual__palestinian_autochthony_reading, coexists_with).
narrative_ontology:cs_reading_relation('5a339aa8-d43b-42f8-9328-4e051f458046', territorial_legitimacy_dual__two_state_coexistence_reading, influences).
narrative_ontology:cs_axiom('5a339aa8-d43b-42f8-9328-4e051f458046', foundational, persecution_grounds_sovereignty_entitlement).
narrative_ontology:cs_axiom_status(persecution_grounds_sovereignty_entitlement, holdable).
narrative_ontology:cs_axiom_grounding('5a339aa8-d43b-42f8-9328-4e051f458046', persecution_grounds_sovereignty_entitlement, deontological).
narrative_ontology:cs_axiom('5a339aa8-d43b-42f8-9328-4e051f458046', foundational, partition_resolution_confers_legitimacy).
narrative_ontology:cs_axiom_status(partition_resolution_confers_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('5a339aa8-d43b-42f8-9328-4e051f458046', partition_resolution_confers_legitimacy, conventional).
narrative_ontology:cs_axiom('5a339aa8-d43b-42f8-9328-4e051f458046', secondary, security_necessity_permits_territorial_control).
narrative_ontology:cs_axiom_status(security_necessity_permits_territorial_control, holdable).
narrative_ontology:cs_axiom_grounding('5a339aa8-d43b-42f8-9328-4e051f458046', security_necessity_permits_territorial_control, instrumental).
narrative_ontology:cs_reference_frame('5a339aa8-d43b-42f8-9328-4e051f458046', partition_sanctioned_refuge_sovereignty).
narrative_ontology:cs_drift_state('5a339aa8-d43b-42f8-9328-4e051f458046', contemporary_occupation_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('5a339aa8-d43b-42f8-9328-4e051f458046', '2026-08-12T00:00:00Z').
narrative_ontology:cs_kernel_id(territorial_legitimacy_dual__zionist_refuge_reading, territorial_legitimacy_dual).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(territorial_legitimacy_dual__zionist_refuge_reading, israeli_state_institutions).
narrative_ontology:constraint_beneficiary(territorial_legitimacy_dual__zionist_refuge_reading, diaspora_jewish_communities).
narrative_ontology:constraint_beneficiary(territorial_legitimacy_dual__zionist_refuge_reading, western_allied_governments).
narrative_ontology:constraint_victim(territorial_legitimacy_dual__zionist_refuge_reading, palestinian_refugee_displaced).
narrative_ontology:constraint_victim(territorial_legitimacy_dual__zionist_refuge_reading, west_bank_palestinians_under_occupation).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(territorial_legitimacy_dual__zionist_refuge_reading, diaspora_jewish_communities).
narrative_ontology:constraint_vindicates(territorial_legitimacy_dual__zionist_refuge_reading, partition_legitimacy_doctrine).
narrative_ontology:constraint_vindicates(territorial_legitimacy_dual__zionist_refuge_reading, persecution_grounds_sovereignty_principle).
narrative_ontology:constraint_vindicates(territorial_legitimacy_dual__zionist_refuge_reading, secure_boundaries_security_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the territory and the narrative together: runs the education and commemoration system that transmits the founding account, maintains the legal architecture that bars return of the displaced, litigates and lobbies in international forums to preserve recognition, and directs the security establishment whose operations the account justifies. Renouncing the founding account is not a live option — the governing coalitions, the officer corps, and the electorate are all constituted through it.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__zionist_refuge_reading, israeli_state_institutions, agenda_setter,
    institutional, generational, identity_locked, regional).

% Draw communal continuity and a standing assurance of refuge from the arrangement's existence; organize philanthropy, advocacy, and political lobbying in their countries of residence to defend it. When the conflict generates hostility abroad, these communities absorb it directly — synagogue security, campus conflict, accusations of divided loyalty. Leaving the communal fold would mean surrendering the identity the arrangement anchors, not merely changing an opinion.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__zionist_refuge_reading, diaspora_jewish_communities, beneficiary,
    organized, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(territorial_legitimacy_dual__zionist_refuge_reading, diaspora_jewish_communities, payer).

% Receive a stable aligned state in a strategic region, intelligence and technology cooperation, and post-Holocaust moral closure; pay diplomatic costs defending the arrangement in multilateral forums and domestic political costs when their publics dissent. Realignment is possible — several European governments have shifted posture over decades — but carries alliance costs they weigh continuously.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__zionist_refuge_reading, western_allied_governments, beneficiary,
    institutional, generational, mobile, global).

% Descendants of those displaced in the 1948 war, registered across camps in Lebanon, Jordan, Syria, and beyond. The arrangement's account assigns the displacement to Arab rejection of partition and bars their return through property law and border control; host-state citizenship remains partial or withheld. Their organizations hold no seat in the legitimacy conversation conducted about them.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__zionist_refuge_reading, palestinian_refugee_displaced, payer,
    powerless, generational, trapped, regional).

% Live under military administration that the arrangement justifies as security-necessitated: land registration, movement permits, water allocation, and settlement expansion are governed from outside their political system. Their self-governing institutions operate on limited mandates; the political horizon the arrangement offers — boundaries described as negotiable — has in practice receded as the settlement grid expands.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__zionist_refuge_reading, west_bank_palestinians_under_occupation, payer,
    moderate, biographical, trapped, local).

% Propose single-state constitutional frameworks with equal civic membership that would dissolve the dual-legitimacy contest altogether. They publish, organize, and testify, but sit outside the arrangement's frame, which treats partition-based sovereignty as the settled question and their proposal as a category error rather than an alternative.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__zionist_refuge_reading, binational_state_advocates, excluded,
    moderate, generational, constrained, regional).

% Adjudicate and record: advisory opinions, General Assembly resolutions, treaty-body reviews, and commission-of-inquiry reports all reference the arrangement. Both constituencies cite their outputs selectively; the bodies themselves hold no enforcement arm and observe the contest they document.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__zionist_refuge_reading, international_legal_bodies, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(territorial_legitimacy_dual__zionist_refuge_reading, israeli_state_institutions).
narrative_ontology:fixing_cost_class(territorial_legitimacy_dual__zionist_refuge_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves a real collective-action problem: converting a scattered, stateless, recently exterminated population into a single sovereign member of the international system with a defensible claim to membership, and coordinating a worldwide diaspora's identity and protection around that member.
% TRANSFER_FUNCTION: Moves international recognition, military assistance, and territorial administrative control toward the Israeli state and its constituency; moves the costs of the 1948 displacement — return rights, restitution claims, camp residency — onto displaced Palestinians and their host states; moves the security burden of the occupied territories onto their residents.
% ABSENT_VOICES: The displaced of 1948 and their descendants had no seat in the partition process — no plebiscite was held in the territory allocated — and remain outside the arrangement's adjudicating conversations. Binational-equality advocates are outside this reading's frame entirely. Mizrahi Jewish refugees from Arab states appear prominently in the narrative but their claims are deployed instrumentally rather than adjudicated on their own terms.
% DISAPPEARANCE_RATIONALE: Overnight removal would force simultaneous renegotiation of Israel's treaty and aid architecture, the diaspora's identity anchor, the UNRWA mandate and the camp system in Lebanon, Jordan, and Syria, and every Palestinian claim structure — the regional order is arranged around this arrangement and would rearrange around its absence.
% FOUNDING_PROBLEM: After the Holocaust, hundreds of thousands of Jewish displaced persons in Europe could not or would not return to their countries of origin; British Mandate restrictions barred mass entry to Palestine. The founding problem was where a persecuted, stateless people could exercise sovereignty — answered by this arrangement as refuge realized through partition-sanctioned statehood.
% FOUNDING_PROBLEM_CORROBORATION: Corroboration of the historical founding problem comes from outside the beneficiary set: UNSCOP 1947 records, British Mandate administration files, and Holocaust-era refugee documentation independently attest the statelessness problem was real. Attestation that the problem REMAINS live in its original form comes almost exclusively from within the beneficiary set; outside sources — UNRWA registration data, host-state demographic records — corroborate the persistence of displacement itself, not the sufficiency of the refuge solution. That asymmetry is stated plainly rather than resolved.
narrative_ontology:disappearance_verdict(territorial_legitimacy_dual__zionist_refuge_reading, world_rearranges).
narrative_ontology:founding_problem_status(territorial_legitimacy_dual__zionist_refuge_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(territorial_legitimacy_dual__zionist_refuge_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(territorial_legitimacy_dual__zionist_refuge_reading, 'none', 1).
narrative_ontology:epsilon_provenance(territorial_legitimacy_dual__zionist_refuge_reading, 0.54, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(territorial_legitimacy_dual__zionist_refuge_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(territorial_legitimacy_dual__zionist_refuge_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(territorial_legitimacy_dual__zionist_refuge_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Epsilon is 0.54 at interval end, measured by this reading's own lights: the 1948 stratum is conceded legitimate (low extraction at t=0, 0.38), costs step up sharply with the 1967 occupation (0.50 by t=22), dip during the Oslo recognition window (0.46 at t=44) as the reading's negotiability premise briefly operated, then resume climbing with settlement expansion and the erosion of the two-state horizon (0.54 at t=77). Suppression (0.68) is authored as a raw structural property, unscaled: the enforcement machinery demonstrably matured across the interval — absentee-property and return-barrier law, military government giving way to civil administration, the separation barrier, and anti-delegitimization legislation abroad — which is why suppression_requirement is tracked temporally here (enforcement-capacity change is the traced dynamic, not mere extraction drift). Theater ratio (0.40) rises monotonically as the survivor generation passes and maintenance shifts from lived refuge function toward commemoration, advocacy industries, and memory politics; the founding function remains real, so theater stays below the proxy-domination threshold. Accessibility_collapse is low (0.35): the rival readings remain fully articulable and institutionally alive — understanding this reading does not close off the alternatives, which is exactly what distinguishes a contested kernel from a natural law. Resistance is high (0.70): Palestinian civil and armed resistance, diplomatic campaigns, boycott movements, and adverse multilateral majorities all press against the arrangement continuously. Refugee dispersal across hostile or precarious host states fragments potential coalition power among the powerless seats — noted here because it shapes the payer seats' effective leverage despite their numbers. All three metric series run on one shared time grid (t = 0, 11, 22, 33, 44, 55, 66, 77; 1948–2025) so no row borrows another row's end-state.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute divergent types from identical structure. From the agenda-setter seat the arrangement is a refuge realized: persecution answered, sovereignty achieved, recognition secured — coordination it built and defends. From the refugee seat the same structure is enforced exclusion: the attribution frame assigns their dispossession to others' choices and the legal architecture bars their return indefinitely. From the occupied seat it is open-ended administration whose stated terminus ('negotiable boundaries') recedes as the settlement grid hardens. Western allied seats experience it as stable alliance architecture worth diplomatic spend. The engine computes this divergence from power, exit, and directional data; nothing in the authored claim adjudicates it. The identity-lock mechanism on the two locked seats differs in kind: for the state it is institutional identity (the governing coalition, officer corps, and electorate are constituted through the founding account — break the frame and the state's self-description dissolves); for the diaspora it is relational-ideological identity (communal continuity and safety assurance are fused with the arrangement's existence — exit means leaving the community, not merely disagreeing with it). Were either lock to break, that seat's classification would migrate sharply toward the target side.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary/victim declarations drive the derivation: israeli_state_institutions sits nearest the beneficiary pole (collects recognition, territorial control, and the enforcement rents of its own arrangement); western_allied_governments likewise (alliance returns, minimal exposure); palestinian_refugee_displaced and west_bank_palestinians_under_occupation sit nearest the target pole (bear the transfer directly, trapped exit amplifies their effective extraction). One override is declared: diaspora_jewish_communities (power atom 'organized', unique to that seat in this story) is overridden to d = 0.30. The structural derivation would read them as near-full beneficiaries (beneficiary declaration plus identity_locked exit pushes the derived d toward the subsidized end, ~0.10), but the derivation misses that the diaspora funds the enforcement apparatus out of its own resources and absorbs the arrangement's externalities — synagogue security, campus hostility, accusations of dual loyalty — as recurring costs. A d of 0.30 encodes a seat that genuinely receives identity and refuge returns while paying real, recurring costs into the same structure. Suppression, again, enters the computation unscaled; only extractiveness is scaled by directionality and scope, and the global diplomatic surface of the beneficiary seats versus the regional confinement of the payer seats is part of what the scope atoms encode.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (where can a persecuted, stateless people exercise sovereignty?) is authored as CONTESTED, not dead: the historical problem is documented and real, the refuge solution exists, but the parties dispute whether the problem persists in its original form or has been transformed into boundary and security questions. Because status is contested rather than dead, the (status x disappearance_verdict) mismatch consumer finds no zombie flag here — and correctly so: the arrangement's function is live, its theater growth notwithstanding. The tangled_rope classification is what prevents mislabeling in both directions: a pure-snare reading would erase the genuine coordination half (a real refuge function for a real persecuted population, real identity coordination for a worldwide diaspora, real alliance stability), while a pure-rope reading would erase the asymmetric extraction half (foreclosed return, administered occupation, externally-attributed displacement). The rising theater series is monitored as a symptom — if the refuge function continues to atrophy while commemorative maintenance grows, the structure drifts toward piton; the interval-end data does not support that verdict yet.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This constraint is the zionist_refuge_reading of the territorial_legitimacy_dual kernel; what structural changes would the sibling readings (palestinian_autochthony_reading, two_state_coexistence_reading) introduce into the same referent?',
    'Author the sibling stories over the same referent and compare computed classifications across the kernel family; the disagreement is located in the victim/beneficiary partition and in which temporal stratum (1948 vs 1967) carries the legitimacy burden.',
    'Under palestinian_autochthony_reading the beneficiary/victim sets invert and epsilon rises sharply; under two_state_coexistence_reading extraction concentrates on post-1967 control only and 1948 legitimacy is treated as mutual. This file''s epsilon is reading-indexed and must not be averaged across the family.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer structure: one reading of a contested kernel; sibling deltas documented rather than folded into this story.').

omega_variable(
    divine_promise_load_bearing,
    'Does the divine-promise pillar do independent legitimating work, or is the arrangement''s operative warrant carried entirely by persecution history plus UN partition acceptance?',
    'Discourse analysis of official and communal justification: if secular-legal warrants suffice in every binding context (treaties, UN credentials, alliance charters), the theological pillar is ornamental; if appeals to the promise appear where legal warrants fail, it is load-bearing.',
    'If load-bearing, the authority grounding shifts toward theological lineage, the claim becomes unfalsifiable within international law, and suppression requirements against secular counter-frames rise.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(divine_promise_load_bearing, conceptual, 'Whether the theological component of the legitimacy triad is structural or decorative.').

omega_variable(
    nakba_causality_attribution,
    'Was the 1948 displacement primarily a consequence of Arab rejection of partition and ensuing war, as this reading holds, or of organized expulsion?',
    'Archival historiography: captured IDF archives, village-depopulation studies, and the Israeli New Historians'' documentary record cross-checked against Arab-state and UN mediation files.',
    'Systematic-expulsion findings would collapse the reading''s externalization of responsibility, raising epsilon from every seat including internal ones and materially strengthening the autochthony sibling''s claim structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(nakba_causality_attribution, empirical, 'Causal attribution of the founding displacement, the pivot on which this reading''s exoneration frame turns.').

omega_variable(
    settlement_reversibility,
    'Does post-1967 territorial control remain ''negotiable'' as this reading asserts, or has settlement infrastructure made it practically irreversible?',
    'Settlement population and build-out trajectories, evacuation feasibility studies, and annexation legislation tracked against declared negotiating positions.',
    'Demonstrated irreversibility would convert the reading''s ''negotiable boundaries'' premise into cover, pushing payer-seat computations toward the snare end of the spectrum even while the reading''s own seat holds steady.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(settlement_reversibility, empirical, 'Whether the reading''s central concession (1967 boundaries negotiable) remains structurally true.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(territorial_legitimacy_dual__zionist_refuge_reading, 0, 77).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(terr_tr_t0, territorial_legitimacy_dual__zionist_refuge_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(terr_tr_t0, observed).
narrative_ontology:measurement(terr_tr_t11, territorial_legitimacy_dual__zionist_refuge_reading, theater_ratio, 11, 0.18).
narrative_ontology:measurement_basis(terr_tr_t11, observed).
narrative_ontology:measurement(terr_tr_t22, territorial_legitimacy_dual__zionist_refuge_reading, theater_ratio, 22, 0.21).
narrative_ontology:measurement_basis(terr_tr_t22, observed).
narrative_ontology:measurement(terr_tr_t33, territorial_legitimacy_dual__zionist_refuge_reading, theater_ratio, 33, 0.24).
narrative_ontology:measurement_basis(terr_tr_t33, observed).
narrative_ontology:measurement(terr_tr_t44, territorial_legitimacy_dual__zionist_refuge_reading, theater_ratio, 44, 0.27).
narrative_ontology:measurement_basis(terr_tr_t44, observed).
narrative_ontology:measurement(terr_tr_t55, territorial_legitimacy_dual__zionist_refuge_reading, theater_ratio, 55, 0.31).
narrative_ontology:measurement_basis(terr_tr_t55, observed).
narrative_ontology:measurement(terr_tr_t66, territorial_legitimacy_dual__zionist_refuge_reading, theater_ratio, 66, 0.36).
narrative_ontology:measurement_basis(terr_tr_t66, observed).
narrative_ontology:measurement(terr_tr_t77, territorial_legitimacy_dual__zionist_refuge_reading, theater_ratio, 77, 0.4).
narrative_ontology:measurement_basis(terr_tr_t77, observed).

% Extraction over time
narrative_ontology:measurement(terr_be_t0, territorial_legitimacy_dual__zionist_refuge_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement_basis(terr_be_t0, observed).
narrative_ontology:measurement(terr_be_t11, territorial_legitimacy_dual__zionist_refuge_reading, base_extractiveness, 11, 0.42).
narrative_ontology:measurement_basis(terr_be_t11, observed).
narrative_ontology:measurement(terr_be_t22, territorial_legitimacy_dual__zionist_refuge_reading, base_extractiveness, 22, 0.5).
narrative_ontology:measurement_basis(terr_be_t22, observed).
narrative_ontology:measurement(terr_be_t33, territorial_legitimacy_dual__zionist_refuge_reading, base_extractiveness, 33, 0.51).
narrative_ontology:measurement_basis(terr_be_t33, observed).
narrative_ontology:measurement(terr_be_t44, territorial_legitimacy_dual__zionist_refuge_reading, base_extractiveness, 44, 0.46).
narrative_ontology:measurement_basis(terr_be_t44, observed).
narrative_ontology:measurement(terr_be_t55, territorial_legitimacy_dual__zionist_refuge_reading, base_extractiveness, 55, 0.49).
narrative_ontology:measurement_basis(terr_be_t55, observed).
narrative_ontology:measurement(terr_be_t66, territorial_legitimacy_dual__zionist_refuge_reading, base_extractiveness, 66, 0.52).
narrative_ontology:measurement_basis(terr_be_t66, observed).
narrative_ontology:measurement(terr_be_t77, territorial_legitimacy_dual__zionist_refuge_reading, base_extractiveness, 77, 0.54).
narrative_ontology:measurement_basis(terr_be_t77, observed).

% Suppression requirement over time
narrative_ontology:measurement(terr_su_t0, territorial_legitimacy_dual__zionist_refuge_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement_basis(terr_su_t0, observed).
narrative_ontology:measurement(terr_su_t11, territorial_legitimacy_dual__zionist_refuge_reading, suppression_requirement, 11, 0.42).
narrative_ontology:measurement_basis(terr_su_t11, observed).
narrative_ontology:measurement(terr_su_t22, territorial_legitimacy_dual__zionist_refuge_reading, suppression_requirement, 22, 0.5).
narrative_ontology:measurement_basis(terr_su_t22, observed).
narrative_ontology:measurement(terr_su_t33, territorial_legitimacy_dual__zionist_refuge_reading, suppression_requirement, 33, 0.53).
narrative_ontology:measurement_basis(terr_su_t33, observed).
narrative_ontology:measurement(terr_su_t44, territorial_legitimacy_dual__zionist_refuge_reading, suppression_requirement, 44, 0.48).
narrative_ontology:measurement_basis(terr_su_t44, observed).
narrative_ontology:measurement(terr_su_t55, territorial_legitimacy_dual__zionist_refuge_reading, suppression_requirement, 55, 0.58).
narrative_ontology:measurement_basis(terr_su_t55, observed).
narrative_ontology:measurement(terr_su_t66, territorial_legitimacy_dual__zionist_refuge_reading, suppression_requirement, 66, 0.63).
narrative_ontology:measurement_basis(terr_su_t66, observed).
narrative_ontology:measurement(terr_su_t77, territorial_legitimacy_dual__zionist_refuge_reading, suppression_requirement, 77, 0.68).
narrative_ontology:measurement_basis(terr_su_t77, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(territorial_legitimacy_dual__zionist_refuge_reading, identity_coordination).
narrative_ontology:affects_constraint(territorial_legitimacy_dual__zionist_refuge_reading, palestinian_autochthony_reading).
narrative_ontology:affects_constraint(territorial_legitimacy_dual__zionist_refuge_reading, two_state_coexistence_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the colloquial label 'Israel's legitimacy': measuring legitimacy through the refuge-persecution observable (this story) yields a different epsilon than measuring through habitation-and-return (palestinian_autochthony_reading) or mutual-recognition-compromise (two_state_coexistence_reading). Per the epsilon-invariance principle these are separate constraints with separate beneficiary/victim structures, linked here as one kernel family; the upstream reading (this one, with the highest institutional entrenchment) exerts structural pressure on the two-state sibling via facts-on-ground while coexisting with the autochthony sibling as competing live positions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(territorial_legitimacy_dual__zionist_refuge_reading, organized, 0.3).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
