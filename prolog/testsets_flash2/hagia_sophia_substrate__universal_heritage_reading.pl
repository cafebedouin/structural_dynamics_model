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
 *   human_readable: Hagia Sophia as Universal Cultural Heritage (Secular Reading)
 *   domain: cultural_heritage/sovereignty/religious_authority
 *
 * SUMMARY:
 *   This constraint represents the 'universal heritage' reading of Hagia
 *   Sophia's status, where its legitimacy derives from its shared human
 *   cultural heritage, transcending specific religious or national claims.
 *   This reading was dominant during its period as a museum (1934-2020),
 *   enforced by secular Turkish authorities and supported by international
 *   heritage bodies. It coordinated access for global tourism and scholarship
 *   but suppressed active religious worship claims. The claimed type is
 *   'tangled_rope' because it genuinely coordinated a complex multi-claim
 *   problem while simultaneously extracting from specific religious groups.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hagia_sophia_substrate__universal_heritage_reading, 0.68).
domain_priors:suppression_score(hagia_sophia_substrate__universal_heritage_reading, 0.75).
domain_priors:theater_ratio(hagia_sophia_substrate__universal_heritage_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hagia_sophia_substrate__universal_heritage_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(hagia_sophia_substrate__universal_heritage_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(hagia_sophia_substrate__universal_heritage_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hagia_sophia_substrate__universal_heritage_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(hagia_sophia_substrate__universal_heritage_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hagia_sophia_substrate__universal_heritage_reading, tangled_rope).
narrative_ontology:human_readable(hagia_sophia_substrate__universal_heritage_reading, "Hagia Sophia as Universal Cultural Heritage (Secular Reading)").
narrative_ontology:topic_domain(hagia_sophia_substrate__universal_heritage_reading, "cultural_heritage/sovereignty/religious_authority").

domain_priors:requires_active_enforcement(hagia_sophia_substrate__universal_heritage_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hagia_sophia_substrate__universal_heritage_reading, '9681d1fd-19d2-4341-9f37-553ddea8ddfa').
narrative_ontology:cs_kernel_codification('9681d1fd-19d2-4341-9f37-553ddea8ddfa', formalized).
narrative_ontology:cs_authority_grounding('9681d1fd-19d2-4341-9f37-553ddea8ddfa', lineage).
narrative_ontology:cs_interpretation_layer_present('9681d1fd-19d2-4341-9f37-553ddea8ddfa').
narrative_ontology:cs_reading_relation('9681d1fd-19d2-4341-9f37-553ddea8ddfa', hagia_sophia_substrate__islamic_sovereignty_reading, coexists_with).
narrative_ontology:cs_reading_relation('9681d1fd-19d2-4341-9f37-553ddea8ddfa', hagia_sophia_substrate__orthodox_restitution_reading, coexists_with).
narrative_ontology:cs_axiom('9681d1fd-19d2-4341-9f37-553ddea8ddfa', foundational, cultural_heritage_transcends_religious_national_claims).
narrative_ontology:cs_axiom_status(cultural_heritage_transcends_religious_national_claims, holdable).
narrative_ontology:cs_axiom_grounding('9681d1fd-19d2-4341-9f37-553ddea8ddfa', cultural_heritage_transcends_religious_national_claims, deontological).
narrative_ontology:cs_axiom('9681d1fd-19d2-4341-9f37-553ddea8ddfa', secondary, secular_administration_ensures_universal_access).
narrative_ontology:cs_axiom_status(secular_administration_ensures_universal_access, holdable).
narrative_ontology:cs_axiom_grounding('9681d1fd-19d2-4341-9f37-553ddea8ddfa', secular_administration_ensures_universal_access, instrumental).
narrative_ontology:cs_reference_frame('9681d1fd-19d2-4341-9f37-553ddea8ddfa', secular_universalist_framework).
narrative_ontology:cs_drift_state('9681d1fd-19d2-4341-9f37-553ddea8ddfa', post_2010_religious_revival, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('9681d1fd-19d2-4341-9f37-553ddea8ddfa', '').
narrative_ontology:cs_kernel_id(hagia_sophia_substrate__universal_heritage_reading, hagia_sophia_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hagia_sophia_substrate__universal_heritage_reading, global_tourism_sector).
narrative_ontology:constraint_beneficiary(hagia_sophia_substrate__universal_heritage_reading, international_heritage_organizations).
narrative_ontology:constraint_beneficiary(hagia_sophia_substrate__universal_heritage_reading, secularist_turkish_elites).
narrative_ontology:constraint_victim(hagia_sophia_substrate__universal_heritage_reading, islamic_worship_advocates).
narrative_ontology:constraint_victim(hagia_sophia_substrate__universal_heritage_reading, orthodox_christian_advocates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(hagia_sophia_substrate__universal_heritage_reading, international_scholarly_community).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Advocates for the site's status as a secular museum, emphasizing its universal historical and artistic value. Benefits from the international prestige and tourism revenue this status brings, aligning with a secular national identity. Actively enforces policies to maintain this status against religious claims.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__universal_heritage_reading, secularist_turkish_elites, agenda_setter,
    institutional, generational, constrained, national).

% Benefits from the site's accessibility as a major tourist attraction, generating revenue for travel agencies, airlines, and local businesses. Supports its status as a museum for broad public access.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__universal_heritage_reading, global_tourism_sector, beneficiary,
    organized, biographical, mobile, global).

% Advocates for the preservation and interpretation of the site as a UNESCO World Heritage site, emphasizing its universal value to humanity. Benefits from its role in promoting cultural understanding and conservation, and from the authority derived from its designation.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__universal_heritage_reading, international_heritage_organizations, beneficiary,
    institutional, civilizational, analytical, global).

% Seeks to restore the site's status as an active mosque, viewing its current secular status as a suppression of religious freedom and historical right. Bears the cost of denied access for worship and the perceived ideological slight.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__universal_heritage_reading, islamic_worship_advocates, payer,
    organized, generational, identity_locked, national).

% Seeks restitution of the site to Orthodox Christian control or a return to its original status as a neutral monument honoring its Byzantine origins. Bears the cost of denied ecclesiastical control and the perceived historical injustice.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__universal_heritage_reading, orthodox_christian_advocates, payer,
    organized, generational, identity_locked, global).

% Benefits from the site's accessibility for research, conservation, and academic study, contributing to a broader understanding of its history and architecture. Supports its secular museum status for unfettered access.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__universal_heritage_reading, international_scholarly_community, beneficiary,
    moderate, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the preservation, study, and public access to a site of immense historical and architectural significance, allowing diverse groups to appreciate its universal cultural value without prioritizing any single religious or national claim.
% TRANSFER_FUNCTION: Transfers control and interpretive authority from specific religious or national claimants to a secular, technocratic administration, generating tourism revenue and international prestige for the host nation, while suppressing religious worship claims.
% ABSENT_VOICES: Future generations who might interpret the site differently, or indigenous cultural groups whose heritage might be similarly universalized without their consent, are absent. Their perspectives would challenge the universalizing framework itself.
% DISAPPEARANCE_RATIONALE: If the 'universal heritage' framing and its enforcement vanished, the site would immediately become a focal point of intense religious and nationalistic contestation, likely leading to its re-conversion to a mosque or a church, fundamentally altering its public function and accessibility.
% FOUNDING_PROBLEM: The problem of managing a site with multiple, deeply contested historical and religious claims, preventing its capture by any single group and ensuring its preservation for all humanity.
% FOUNDING_PROBLEM_CORROBORATION: International heritage organizations and secular scholars corroborate the ongoing challenge of balancing competing claims. While religious advocates contest the 'solution,' they acknowledge the underlying problem of multi-layered historical significance.
narrative_ontology:disappearance_verdict(hagia_sophia_substrate__universal_heritage_reading, world_rearranges).
narrative_ontology:founding_problem_status(hagia_sophia_substrate__universal_heritage_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hagia_sophia_substrate__universal_heritage_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(hagia_sophia_substrate__universal_heritage_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hagia_sophia_substrate__universal_heritage_reading, 0.68, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness (0.68) is high because the secular museum status, while coordinating universal access, also generated significant tourism revenue and ideological signal for secularist Turkish elites, at the cost of denying religious claims. Suppression (0.75) was substantial, requiring active enforcement to prevent religious re-conversion. Theater ratio (0.20) was relatively low, as the museum function was genuinely performed, though the underlying contestation was always present. The temporal measurements reflect a gradual increase in extractiveness and suppression as religious claims intensified over the decades, requiring more active defense of the secular status.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of secularist elites and international heritage bodies, this was a legitimate coordination mechanism. From the perspective of religious advocates, it was an act of suppression and extraction. The engine's classification will reflect this divergence based on the declared structural relationships and metrics.
 *
 * DIRECTIONALITY LOGIC:
 *   Secularist Turkish elites and international heritage organizations are beneficiaries, gaining prestige, revenue, and authority. The global tourism and scholarly sectors also benefit from access. Islamic and Orthodox Christian advocates are victims, as their claims to religious use or restitution were suppressed. The constraint's operation directly extracts from their ability to exercise those claims.
 *
 * MANDATROPHY ANALYSIS:
 *   The 'universal heritage' framing prevented the site from being re-captured by a single religious or national claim, thus resolving the immediate problem of contested ownership. However, the persistence of strong religious counter-claims indicates that the coordination function was always intertwined with an extractive suppression of alternative uses, making it a tangled rope rather than a pure rope or scaffold. The mandatrophy was not fully resolved, but rather managed through a specific, enforced framing.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    universal_vs_particular_value,
    'Is the ''universal heritage'' value truly universal, or does it implicitly privilege a secular, Western-centric view of heritage over particular religious or national claims?',
    'Analysis of the interpretive narratives and curatorial choices within the museum period, and comparison with how other contested sites are managed under different frameworks.',
    'If it implicitly privileges a specific worldview, the ''universal'' claim itself becomes a form of soft power extraction, increasing the effective extractiveness from those whose particular claims are marginalized.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(universal_vs_particular_value, conceptual, 'Whether the universal heritage framing is truly neutral or ideologically loaded.').

omega_variable(
    tourism_revenue_allocation,
    'How were the substantial tourism revenues generated by Hagia Sophia''s museum status allocated, and did they disproportionately benefit specific groups within the Turkish state or secular elite?',
    'Detailed financial audits of the museum''s operations and state tourism budgets during the museum period, tracing the flow of funds.',
    'If revenues were disproportionately captured by specific groups, it would confirm a higher degree of extraction for secularist elites, further solidifying the ''tangled_rope'' classification and potentially shifting it towards ''snare'' for the most impacted victims.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(tourism_revenue_allocation, empirical, 'Transparency of tourism revenue distribution.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of religious claims structural (legal barriers, administrative policies) or internalized (cognitive patterns among some secularists that persist after barrier removal)?',
    'Post-conversion analysis: if religious claims persist and intensify after the museum status is removed, it suggests the suppression was primarily structural, not internalized by the claimants.',
    'If internalized, the constraint''s effective suppression was higher than the structural measure suggests for some secularist groups, as they carried the suppression with them after the museum status was removed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for religious claims.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hagia_sophia_substrate__universal_heritage_reading, 1934, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hagi_tr_t1934, hagia_sophia_substrate__universal_heritage_reading, theater_ratio, 1934, 0.1).
narrative_ontology:measurement(hagi_tr_t1950, hagia_sophia_substrate__universal_heritage_reading, theater_ratio, 1950, 0.12).
narrative_ontology:measurement(hagi_tr_t1970, hagia_sophia_substrate__universal_heritage_reading, theater_ratio, 1970, 0.15).
narrative_ontology:measurement(hagi_tr_t1990, hagia_sophia_substrate__universal_heritage_reading, theater_ratio, 1990, 0.18).
narrative_ontology:measurement(hagi_tr_t2010, hagia_sophia_substrate__universal_heritage_reading, theater_ratio, 2010, 0.2).
narrative_ontology:measurement(hagi_tr_t2020, hagia_sophia_substrate__universal_heritage_reading, theater_ratio, 2020, 0.2).

% Extraction over time
narrative_ontology:measurement(hagi_be_t1934, hagia_sophia_substrate__universal_heritage_reading, base_extractiveness, 1934, 0.5).
narrative_ontology:measurement(hagi_be_t1950, hagia_sophia_substrate__universal_heritage_reading, base_extractiveness, 1950, 0.55).
narrative_ontology:measurement(hagi_be_t1970, hagia_sophia_substrate__universal_heritage_reading, base_extractiveness, 1970, 0.6).
narrative_ontology:measurement(hagi_be_t1990, hagia_sophia_substrate__universal_heritage_reading, base_extractiveness, 1990, 0.65).
narrative_ontology:measurement(hagi_be_t2010, hagia_sophia_substrate__universal_heritage_reading, base_extractiveness, 2010, 0.68).
narrative_ontology:measurement(hagi_be_t2020, hagia_sophia_substrate__universal_heritage_reading, base_extractiveness, 2020, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(hagi_su_t1934, hagia_sophia_substrate__universal_heritage_reading, suppression_requirement, 1934, 0.6).
narrative_ontology:measurement(hagi_su_t1950, hagia_sophia_substrate__universal_heritage_reading, suppression_requirement, 1950, 0.65).
narrative_ontology:measurement(hagi_su_t1970, hagia_sophia_substrate__universal_heritage_reading, suppression_requirement, 1970, 0.7).
narrative_ontology:measurement(hagi_su_t1990, hagia_sophia_substrate__universal_heritage_reading, suppression_requirement, 1990, 0.73).
narrative_ontology:measurement(hagi_su_t2010, hagia_sophia_substrate__universal_heritage_reading, suppression_requirement, 2010, 0.75).
narrative_ontology:measurement(hagi_su_t2020, hagia_sophia_substrate__universal_heritage_reading, suppression_requirement, 2020, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hagia_sophia_substrate__universal_heritage_reading, identity_coordination).
narrative_ontology:affects_constraint(hagia_sophia_substrate__universal_heritage_reading, turkish_secularism_doctrine).
narrative_ontology:affects_constraint(hagia_sophia_substrate__universal_heritage_reading, unesco_world_heritage_framework).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
