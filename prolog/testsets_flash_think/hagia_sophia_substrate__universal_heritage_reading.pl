% ============================================================================
% CONSTRAINT STORY: hagia_sophia_substrate__universal_heritage_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hagia_sophia_substrate__universal_heritage_reading, []).

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
 *   constraint_id: hagia_sophia_substrate__universal_heritage_reading
 *   human_readable: Hagia Sophia as Universal Cultural Heritage Site (1934-2020)
 *   domain: cultural_heritage/sovereignty/religious_authority
 *
 * SUMMARY:
 *   This constraint story instantiates the 'universal heritage' reading of
 *   the Hagia Sophia's status, which prevailed from its conversion to a
 *   museum in 1934 until its reconversion to a mosque in 2020. Under this
 *   reading, the site's legitimacy derived from its status as a shared human
 *   cultural heritage, transcending any single religious or national claim.
 *   While presented as a neutral coordination mechanism (claimed_type: rope),
 *   the metrics reflect substantial extraction and suppression, as this
 *   framing actively sidelined competing religious and nationalistic claims,
 *   benefiting secular Turkish elites and the global tourism/scholarship
 *   sector.
 *
 * KEY AGENTS:
 *   - secularist_turkish_elites: Primary agenda_setter (institutional/arbitrage) — benefited from and enforced the secular museum status.
 *   - global_tourism_sector: Primary beneficiary (organized/mobile) — profited from open access.
 *   - international_scholarly_community: Beneficiary (organized/mobile) — benefited from research access.
 *   - islamic_worship_advocates: Primary payer (organized/constrained) — had their religious claims suppressed.
 *   - orthodox_christian_advocates: Payer (organized/constrained) — had their historical claims suppressed.
 *   - unesco: Observer (institutional/analytical) — monitored heritage but lacked direct enforcement.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hagia_sophia_substrate__universal_heritage_reading, 0.7).
domain_priors:suppression_score(hagia_sophia_substrate__universal_heritage_reading, 0.8).
domain_priors:theater_ratio(hagia_sophia_substrate__universal_heritage_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hagia_sophia_substrate__universal_heritage_reading, extractiveness, 0.7).
narrative_ontology:constraint_metric(hagia_sophia_substrate__universal_heritage_reading, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(hagia_sophia_substrate__universal_heritage_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hagia_sophia_substrate__universal_heritage_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(hagia_sophia_substrate__universal_heritage_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hagia_sophia_substrate__universal_heritage_reading, rope).
narrative_ontology:human_readable(hagia_sophia_substrate__universal_heritage_reading, "Hagia Sophia as Universal Cultural Heritage Site (1934-2020)").
narrative_ontology:topic_domain(hagia_sophia_substrate__universal_heritage_reading, "cultural_heritage/sovereignty/religious_authority").

domain_priors:requires_active_enforcement(hagia_sophia_substrate__universal_heritage_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hagia_sophia_substrate__universal_heritage_reading, '8658d582-1bdb-42d0-a80c-cd88b7b6e042').
narrative_ontology:cs_kernel_codification('8658d582-1bdb-42d0-a80c-cd88b7b6e042', formalized).
narrative_ontology:cs_authority_grounding('8658d582-1bdb-42d0-a80c-cd88b7b6e042', extraction).
narrative_ontology:cs_interpretation_layer_present('8658d582-1bdb-42d0-a80c-cd88b7b6e042').
narrative_ontology:cs_reading_relation('8658d582-1bdb-42d0-a80c-cd88b7b6e042', hagia_sophia_substrate__islamic_sovereignty_reading, forecloses).
narrative_ontology:cs_reading_relation('8658d582-1bdb-42d0-a80c-cd88b7b6e042', hagia_sophia_substrate__orthodox_restitution_reading, forecloses).
narrative_ontology:cs_axiom('8658d582-1bdb-42d0-a80c-cd88b7b6e042', foundational, cultural_heritage_transcends_religious_claims).
narrative_ontology:cs_axiom_status(cultural_heritage_transcends_religious_claims, holdable).
narrative_ontology:cs_axiom_grounding('8658d582-1bdb-42d0-a80c-cd88b7b6e042', cultural_heritage_transcends_religious_claims, deontological).
narrative_ontology:cs_axiom('8658d582-1bdb-42d0-a80c-cd88b7b6e042', secondary, secular_governance_is_neutral_arbiter).
narrative_ontology:cs_axiom_status(secular_governance_is_neutral_arbiter, holdable).
narrative_ontology:cs_axiom_grounding('8658d582-1bdb-42d0-a80c-cd88b7b6e042', secular_governance_is_neutral_arbiter, conventional).
narrative_ontology:cs_reference_frame('8658d582-1bdb-42d0-a80c-cd88b7b6e042', secular_museum_status_1934).
narrative_ontology:cs_drift_state('8658d582-1bdb-42d0-a80c-cd88b7b6e042', post_2020_reconversion, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('8658d582-1bdb-42d0-a80c-cd88b7b6e042', '').
narrative_ontology:cs_kernel_id(hagia_sophia_substrate__universal_heritage_reading, hagia_sophia_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hagia_sophia_substrate__universal_heritage_reading, global_tourism_sector).
narrative_ontology:constraint_beneficiary(hagia_sophia_substrate__universal_heritage_reading, secularist_turkish_elites).
narrative_ontology:constraint_beneficiary(hagia_sophia_substrate__universal_heritage_reading, international_scholarly_community).
narrative_ontology:constraint_beneficiary(hagia_sophia_substrate__universal_heritage_reading, turkish_citizens_secular).
narrative_ontology:constraint_victim(hagia_sophia_substrate__universal_heritage_reading, islamic_worship_advocates).
narrative_ontology:constraint_victim(hagia_sophia_substrate__universal_heritage_reading, orthodox_christian_advocates).
narrative_ontology:constraint_victim(hagia_sophia_substrate__universal_heritage_reading, turkish_citizens_religious).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administered the site as a museum, leveraging its universal heritage status for international prestige, tourism revenue, and as a symbol of Turkey's secular identity. Actively suppressed religious claims to maintain this status.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__universal_heritage_reading, secularist_turkish_elites, agenda_setter,
    institutional, generational, arbitrage, national).

% Profited significantly from the site's status as a major international tourist attraction, facilitating global travel and cultural exchange. Benefited from its secular, accessible nature.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__universal_heritage_reading, global_tourism_sector, beneficiary,
    organized, biographical, mobile, global).

% Benefited from open access for research, conservation, and study of the site's unique architectural and historical layers, free from immediate religious or nationalistic pressures.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__universal_heritage_reading, international_scholarly_community, beneficiary,
    organized, generational, mobile, global).

% Experienced the suppression of their claims for the site to function as a mosque, viewing its museum status as an affront to Ottoman heritage and religious freedom. Engaged in persistent advocacy for reconversion.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__universal_heritage_reading, islamic_worship_advocates, payer,
    organized, generational, constrained, national).

% Experienced the suppression of their claims for the site's restitution as a church or recognition of its primary Christian identity, viewing the museum status as a denial of its Byzantine origins.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__universal_heritage_reading, orthodox_christian_advocates, payer,
    organized, generational, constrained, global).

% Supported the site's museum status as a symbol of modern, secular Turkey and a point of national pride, enjoying its accessibility as a cultural monument.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__universal_heritage_reading, turkish_citizens_secular, beneficiary,
    moderate, biographical, constrained, national).

% Desired the site to be a mosque, aligning with the Islamic sovereignty narrative and feeling their religious identity was marginalized by the secular museum status.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__universal_heritage_reading, turkish_citizens_religious, payer,
    moderate, biographical, constrained, national).

% Monitored the site as a World Heritage property, advocating for its preservation and universal accessibility, but with limited direct enforcement power over national sovereignty decisions.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__universal_heritage_reading, unesco, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(hagia_sophia_substrate__universal_heritage_reading, secularist_turkish_elites).
narrative_ontology:fixing_cost_class(hagia_sophia_substrate__universal_heritage_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To manage a site of immense historical and cultural significance by framing it as a shared global asset, thereby coordinating international preservation efforts and broad public access, while attempting to de-escalate exclusive religious or nationalistic claims.
% TRANSFER_FUNCTION: Transferred cultural and economic value (tourism revenue, scholarly prestige, ideological signaling of secular modernity) to the global heritage sector and secular Turkish institutions, while suppressing and displacing the religious claims of Islamic and Orthodox communities.
% ABSENT_VOICES: The primary voices absent from the decision-making that established and maintained the universal heritage framing were the religious communities (both Islamic and Orthodox) whose exclusive claims were actively sidelined. They would have argued for religious restitution or re-conversion.
% DISAPPEARANCE_RATIONALE: If the universal heritage framing and its associated administrative structure (museum status) vanished overnight, the site would immediately revert to being a focal point of intense religious and nationalistic contestation, leading to its re-conversion to a mosque (as happened in 2020), fundamentally altering its function, accessibility, and the geopolitical dynamics surrounding it.
% FOUNDING_PROBLEM: The problem of managing a site of immense historical and cultural significance claimed by multiple religious and national groups, preventing its destruction or exclusive capture by any single party after the fall of the Ottoman Empire and the rise of the Turkish Republic.
% FOUNDING_PROBLEM_CORROBORATION: International heritage organizations and secular scholars corroborate the problem of managing diverse claims for such a site. However, religious groups (Islamic and Orthodox) dispute the *solution* offered by the universal heritage framing, arguing it unjustly suppressed their primary claims. The 2020 reconversion to a mosque further highlights the contested nature of the 'problem solved' by the museum status.
narrative_ontology:disappearance_verdict(hagia_sophia_substrate__universal_heritage_reading, world_rearranges).
narrative_ontology:founding_problem_status(hagia_sophia_substrate__universal_heritage_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hagia_sophia_substrate__universal_heritage_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(hagia_sophia_substrate__universal_heritage_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hagia_sophia_substrate__universal_heritage_reading, 0.7, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hagia_sophia_substrate__universal_heritage_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(hagia_sophia_substrate__universal_heritage_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(hagia_sophia_substrate__universal_heritage_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.70) is high because the universal heritage framing, while ostensibly neutral, generated significant tourism revenue and ideological capital for the secular Turkish state, effectively extracting value from the site's contested nature. Suppression (0.80) was consistently high, as the state actively enforced the museum status against persistent religious demands. Theater ratio (0.40) was moderate; while genuine preservation efforts occurred, a significant portion of the administrative activity was performative, defending the secular framing against internal and external pressures. The measurement series track the increasing leverage of the site for tourism and secular identity over the interval.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of secular Turkish elites and the global heritage community, the museum status was a legitimate and beneficial coordination mechanism for a universal treasure. However, from the perspective of Islamic and Orthodox advocates, the same structure was an act of suppression and extraction, denying their fundamental religious and historical claims. The engine's computation of per-seat classifications will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Secularist Turkish elites and the global tourism/scholarship sector were clear beneficiaries, gaining prestige, revenue, and access. Islamic and Orthodox worship advocates, along with religious Turkish citizens, were targets, as their claims were actively suppressed by the prevailing framework. Secular Turkish citizens were also beneficiaries, aligning with the state's secular identity.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as a 'rope' (claimed type) with high extraction and suppression (metrics) prevents mislabeling a politically enforced status quo as pure coordination. The 'universal heritage' mandate, while noble in principle, became a vehicle for state-level ideological and economic extraction, rather than a purely neutral arrangement. The contest over its 'founding problem status' (contested) further highlights this tension.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    universal_heritage_vs_ideological_tool,
    'To what extent was the ''universal heritage'' framing a neutral principle for preservation, versus an ideological tool to assert secular modernity and suppress religious identity?',
    'Analysis of state expenditures on preservation versus tourism promotion, and the political rhetoric surrounding the site''s status in domestic and international contexts. Examination of the enforcement mechanisms used against religious claims.',
    'If primarily an ideological tool, the effective extraction and suppression are higher, and the constraint operates more as a Snare than a Rope, even if it provides some coordination benefits.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(universal_heritage_vs_ideological_tool, conceptual, 'Ambiguity between genuine heritage preservation and ideological leverage.').

omega_variable(
    suppression_of_religious_identity,
    'Was the suppression of religious claims primarily structural (legal/administrative barriers) or did it also involve internalized components (e.g., self-censorship by religious communities due to state pressure)?',
    'Post-2020 reconversion analysis: if religious expression flourished immediately, suppression was largely structural. If internalized patterns of self-censorship persisted, it suggests a deeper, internalized component.',
    'If internalized, the constraint''s effective suppression was higher than the structural measure suggests, as the target communities carried the suppression with them even in less overtly coercive environments.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_of_religious_identity, empirical, 'Structural vs. internalized suppression mechanism for religious claims.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hagia_sophia_substrate__universal_heritage_reading, 1934, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hagi_tr_t1934, hagia_sophia_substrate__universal_heritage_reading, theater_ratio, 1934, 0.2).
narrative_ontology:measurement(hagi_tr_t1950, hagia_sophia_substrate__universal_heritage_reading, theater_ratio, 1950, 0.25).
narrative_ontology:measurement(hagi_tr_t1970, hagia_sophia_substrate__universal_heritage_reading, theater_ratio, 1970, 0.3).
narrative_ontology:measurement(hagi_tr_t1990, hagia_sophia_substrate__universal_heritage_reading, theater_ratio, 1990, 0.35).
narrative_ontology:measurement(hagi_tr_t2010, hagia_sophia_substrate__universal_heritage_reading, theater_ratio, 2010, 0.4).
narrative_ontology:measurement(hagi_tr_t2020, hagia_sophia_substrate__universal_heritage_reading, theater_ratio, 2020, 0.4).

% Extraction over time
narrative_ontology:measurement(hagi_be_t1934, hagia_sophia_substrate__universal_heritage_reading, base_extractiveness, 1934, 0.55).
narrative_ontology:measurement(hagi_be_t1950, hagia_sophia_substrate__universal_heritage_reading, base_extractiveness, 1950, 0.6).
narrative_ontology:measurement(hagi_be_t1970, hagia_sophia_substrate__universal_heritage_reading, base_extractiveness, 1970, 0.65).
narrative_ontology:measurement(hagi_be_t1990, hagia_sophia_substrate__universal_heritage_reading, base_extractiveness, 1990, 0.68).
narrative_ontology:measurement(hagi_be_t2010, hagia_sophia_substrate__universal_heritage_reading, base_extractiveness, 2010, 0.7).
narrative_ontology:measurement(hagi_be_t2020, hagia_sophia_substrate__universal_heritage_reading, base_extractiveness, 2020, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(hagi_su_t1934, hagia_sophia_substrate__universal_heritage_reading, suppression_requirement, 1934, 0.7).
narrative_ontology:measurement(hagi_su_t1950, hagia_sophia_substrate__universal_heritage_reading, suppression_requirement, 1950, 0.75).
narrative_ontology:measurement(hagi_su_t1970, hagia_sophia_substrate__universal_heritage_reading, suppression_requirement, 1970, 0.78).
narrative_ontology:measurement(hagi_su_t1990, hagia_sophia_substrate__universal_heritage_reading, suppression_requirement, 1990, 0.8).
narrative_ontology:measurement(hagi_su_t2010, hagia_sophia_substrate__universal_heritage_reading, suppression_requirement, 2010, 0.8).
narrative_ontology:measurement(hagi_su_t2020, hagia_sophia_substrate__universal_heritage_reading, suppression_requirement, 2020, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hagia_sophia_substrate__universal_heritage_reading, identity_coordination).
narrative_ontology:affects_constraint(hagia_sophia_substrate__universal_heritage_reading, hagia_sophia_substrate__islamic_sovereignty_reading).
narrative_ontology:affects_constraint(hagia_sophia_substrate__universal_heritage_reading, hagia_sophia_substrate__orthodox_restitution_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the Hagia Sophia's contested status. It is linked to sibling readings that represent alternative framings of the site's legitimacy.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
