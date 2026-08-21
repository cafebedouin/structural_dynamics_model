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
 *   constraint_id: hagia_sophia_substrate__universal_heritage_reading
 *   human_readable: Hagia Sophia as Universal Cultural Heritage (Secular Reading)
 *   domain: cultural_heritage/sovereignty/religious_authority
 *
 * SUMMARY:
 *   This constraint story instantiates the 'universal_heritage_reading' of
 *   the Hagia Sophia kernel, where the site's legitimacy derives from its
 *   status as shared human cultural heritage, transcending any single
 *   religious or national claim. This reading, primarily enforced by the
 *   secular Turkish state and supported by international heritage bodies,
 *   positions the site as a museum. It benefits the global tourism and
 *   scholarship sectors and secularist Turkish elites, while actively
 *   suppressing Islamic and Orthodox religious claims to the site. The high
 *   extractiveness reflects the economic and ideological gains derived from
 *   this framing, and high suppression reflects the active enforcement
 *   required to maintain it against religious counter-claims.
 *
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
narrative_ontology:constraint_metric(hagia_sophia_substrate__universal_heritage_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hagia_sophia_substrate__universal_heritage_reading, tangled_rope).
narrative_ontology:human_readable(hagia_sophia_substrate__universal_heritage_reading, "Hagia Sophia as Universal Cultural Heritage (Secular Reading)").
narrative_ontology:topic_domain(hagia_sophia_substrate__universal_heritage_reading, "cultural_heritage/sovereignty/religious_authority").

domain_priors:requires_active_enforcement(hagia_sophia_substrate__universal_heritage_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hagia_sophia_substrate__universal_heritage_reading, '4396f902-7b7e-48fa-88ec-b83913f137ec').
narrative_ontology:cs_kernel_codification('4396f902-7b7e-48fa-88ec-b83913f137ec', formalized).
narrative_ontology:cs_authority_grounding('4396f902-7b7e-48fa-88ec-b83913f137ec', extraction).
narrative_ontology:cs_interpretation_layer_present('4396f902-7b7e-48fa-88ec-b83913f137ec').
narrative_ontology:cs_reading_relation('4396f902-7b7e-48fa-88ec-b83913f137ec', hagia_sophia_substrate__islamic_sovereignty_reading, forecloses).
narrative_ontology:cs_reading_relation('4396f902-7b7e-48fa-88ec-b83913f137ec', hagia_sophia_substrate__orthodox_restitution_reading, forecloses).
narrative_ontology:cs_axiom('4396f902-7b7e-48fa-88ec-b83913f137ec', foundational, cultural_heritage_transcends_religious_claims).
narrative_ontology:cs_axiom_status(cultural_heritage_transcends_religious_claims, holdable).
narrative_ontology:cs_axiom_grounding('4396f902-7b7e-48fa-88ec-b83913f137ec', cultural_heritage_transcends_religious_claims, conventional).
narrative_ontology:cs_axiom('4396f902-7b7e-48fa-88ec-b83913f137ec', secondary, secular_administration_ensures_neutrality).
narrative_ontology:cs_axiom_status(secular_administration_ensures_neutrality, holdable).
narrative_ontology:cs_axiom_grounding('4396f902-7b7e-48fa-88ec-b83913f137ec', secular_administration_ensures_neutrality, conventional).
narrative_ontology:cs_reference_frame('4396f902-7b7e-48fa-88ec-b83913f137ec', secular_universal_heritage_paradigm).
narrative_ontology:cs_drift_state('4396f902-7b7e-48fa-88ec-b83913f137ec', post_2020_reconversion, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('4396f902-7b7e-48fa-88ec-b83913f137ec', '').
narrative_ontology:cs_kernel_id(hagia_sophia_substrate__universal_heritage_reading, hagia_sophia_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hagia_sophia_substrate__universal_heritage_reading, global_tourism_scholarship_sector).
narrative_ontology:constraint_beneficiary(hagia_sophia_substrate__universal_heritage_reading, secularist_turkish_elites).
narrative_ontology:constraint_victim(hagia_sophia_substrate__universal_heritage_reading, islamic_worship_advocates).
narrative_ontology:constraint_victim(hagia_sophia_substrate__universal_heritage_reading, orthodox_restitution_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from the site's status as a secular museum, attracting international tourists and scholars. This framing provides a stable, accessible environment for research and visitation, generating significant revenue and academic prestige.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__universal_heritage_reading, global_tourism_scholarship_sector, beneficiary,
    powerful, biographical, arbitrage, global).

% Actively promoted and maintained the site's museum status as a symbol of Turkey's secular identity and its integration into global cultural norms. They benefit from the ideological signal of modernity and the economic gains from tourism, but face political pressure from religious factions.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__universal_heritage_reading, secularist_turkish_elites, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(hagia_sophia_substrate__universal_heritage_reading, secularist_turkish_elites, beneficiary).

% Bear the cost of suppressed religious expression, as their claim to use the site as a mosque (based on Ottoman conquest and waqf status) was denied under the museum framing. Their identity is deeply tied to this claim, making exit unthinkable.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__universal_heritage_reading, islamic_worship_advocates, payer,
    organized, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(hagia_sophia_substrate__universal_heritage_reading, islamic_worship_advocates, excluded).

% Bear the cost of their claim for the site's return to Orthodox ecclesiastical control or neutral Christian reverence being denied. Their historical and religious identity is fused with this demand, making compromise difficult.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__universal_heritage_reading, orthodox_restitution_advocates, payer,
    organized, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(hagia_sophia_substrate__universal_heritage_reading, orthodox_restitution_advocates, excluded).

% Responsible for the day-to-day management, preservation, and interpretation of the site as a museum. They operate under the directives of the secular state, enforcing the universal heritage narrative and managing the flow of visitors and research.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__universal_heritage_reading, technocratic_museum_administration, agenda_setter,
    institutional, biographical, constrained, national).

% Monitors the site's preservation and adherence to its World Heritage status, which aligns with the universal heritage reading. They can exert diplomatic pressure and influence international opinion regarding the site's management.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__universal_heritage_reading, unesco_world_heritage_committee, observer,
    institutional, civilizational, analytical, global).
narrative_ontology:stakeholder_secondary_role(hagia_sophia_substrate__universal_heritage_reading, unesco_world_heritage_committee, agenda_setter).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(hagia_sophia_substrate__universal_heritage_reading, secularist_turkish_elites).
narrative_ontology:fixing_cost_class(hagia_sophia_substrate__universal_heritage_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates international efforts for the preservation, study, and public access of Hagia Sophia as a shared human legacy, transcending specific religious or national claims.
% TRANSFER_FUNCTION: Transfers symbolic capital (secular modernity, global cultural integration) and significant tourism revenue to the Turkish state and associated sectors, while suppressing and displacing religious claims to the site.
% ABSENT_VOICES: Religious communities (both Islamic and Orthodox) whose claims to the site's primary function are actively suppressed by this secular, universal framing. They would argue for the site's re-sacralization or restitution.
% DISAPPEARANCE_RATIONALE: If the universal heritage framing and its enforcement vanished, the site would immediately become a flashpoint for intense, competing religious and national claims, leading to significant political, social, and diplomatic reorganization around its status and control.
% FOUNDING_PROBLEM: To manage a historically contested site by de-sacralizing it and presenting it as a neutral, shared cultural asset, thereby avoiding religious conflict, promoting secular modernity, and integrating Turkey into international heritage norms.
% FOUNDING_PROBLEM_CORROBORATION: Secular Turkish intellectuals, international heritage organizations, and some diplomatic bodies corroborate this founding problem and its status. Religious groups and some nationalist factions contest it, arguing the museum status was a political act to suppress religious identity rather than a neutral solution to conflict.
narrative_ontology:disappearance_verdict(hagia_sophia_substrate__universal_heritage_reading, world_rearranges).
narrative_ontology:founding_problem_status(hagia_sophia_substrate__universal_heritage_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hagia_sophia_substrate__universal_heritage_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
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
 *   Extractiveness is high (0.7) because the museum status generates substantial tourism revenue and reinforces a secular national identity, effectively monetizing and ideologizing the site while denying its religious functions. Suppression is also high (0.8) as the state actively enforced the museum status, legally barring religious worship and resisting calls for reconversion. Theater ratio is moderate (0.4): while genuine preservation and scholarly access occurred, a significant portion of the maintenance and defense of the museum status was performative, aimed at projecting secular modernity and managing political contestation. The temporal measurements show a gradual increase in extractiveness and theater as the site's tourism value grew and political contestation intensified over the decades.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of secularist Turkish elites and the global tourism/scholarship sector, the museum status is a legitimate and beneficial coordination mechanism for cultural preservation and access. However, from the perspective of Islamic and Orthodox worship advocates, the same arrangement is a profound act of suppression and extraction, denying their fundamental religious and historical claims to the site. The engine's per-seat classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The global tourism/scholarship sector and secularist Turkish elites are clear beneficiaries (low directionality), gaining revenue, prestige, and ideological reinforcement. Islamic and Orthodox worship advocates are targets (high directionality), as their claims are suppressed and their access to the site for religious purposes is denied. The technocratic museum administration acts as an agenda-setter, enforcing the constraint on behalf of the secular state.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    secular_neutrality_vs_suppression,
    'Is the secular museum status of Hagia Sophia a genuinely neutral act of cultural preservation, or does it inherently suppress religious identity and claims?',
    'Analysis of historical state policies regarding religious sites, comparative studies of secularization processes, and ethnographic research into the lived experiences of affected religious communities.',
    'If primarily suppressive, the constraint''s effective extraction and suppression are higher than a ''neutral'' reading would suggest, reinforcing its classification as a Snare or Tangled Rope. If genuinely neutral, the coordination function is stronger.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(secular_neutrality_vs_suppression, conceptual, 'Ambiguity of secularism''s role in heritage management.').

omega_variable(
    tourism_revenue_vs_cultural_value,
    'To what extent was the ''universal heritage'' framing driven by genuine cultural preservation goals versus the economic benefits of international tourism?',
    'Economic analysis of tourism revenue vs. preservation costs, archival research into policy debates, and comparison with other heritage sites managed under different models.',
    'If primarily driven by economic gain, the extractiveness metric is more firmly grounded in material transfer, strengthening the Snare/Tangled Rope classification. If cultural value was primary, the coordination function is more salient.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(tourism_revenue_vs_cultural_value, empirical, 'Drivers of the universal heritage framing.').

omega_variable(
    natural_vs_constructed_heritage,
    'Is ''universal cultural heritage'' an inherent, objective status of a site like Hagia Sophia, or is it a political and discursive construct applied by specific actors?',
    'Philosophical analysis of heritage concepts, critical discourse analysis of international heritage institutions, and examination of how different cultures define and value historical sites.',
    'If a constructed status, the ''naturalness'' of the constraint is undermined, making it more clearly a human-made arrangement with beneficiaries and victims. If inherent, it leans closer to a Mountain, though still with beneficiaries.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_vs_constructed_heritage, conceptual, 'Ontological status of universal cultural heritage.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hagia_sophia_substrate__universal_heritage_reading, 1934, 2019).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hagi_tr_t1934, hagia_sophia_substrate__universal_heritage_reading, theater_ratio, 1934, 0.2).
narrative_ontology:measurement(hagi_tr_t1950, hagia_sophia_substrate__universal_heritage_reading, theater_ratio, 1950, 0.25).
narrative_ontology:measurement(hagi_tr_t1970, hagia_sophia_substrate__universal_heritage_reading, theater_ratio, 1970, 0.3).
narrative_ontology:measurement(hagi_tr_t1990, hagia_sophia_substrate__universal_heritage_reading, theater_ratio, 1990, 0.35).
narrative_ontology:measurement(hagi_tr_t2005, hagia_sophia_substrate__universal_heritage_reading, theater_ratio, 2005, 0.38).
narrative_ontology:measurement(hagi_tr_t2019, hagia_sophia_substrate__universal_heritage_reading, theater_ratio, 2019, 0.4).

% Extraction over time
narrative_ontology:measurement(hagi_be_t1934, hagia_sophia_substrate__universal_heritage_reading, base_extractiveness, 1934, 0.55).
narrative_ontology:measurement(hagi_be_t1950, hagia_sophia_substrate__universal_heritage_reading, base_extractiveness, 1950, 0.6).
narrative_ontology:measurement(hagi_be_t1970, hagia_sophia_substrate__universal_heritage_reading, base_extractiveness, 1970, 0.65).
narrative_ontology:measurement(hagi_be_t1990, hagia_sophia_substrate__universal_heritage_reading, base_extractiveness, 1990, 0.68).
narrative_ontology:measurement(hagi_be_t2005, hagia_sophia_substrate__universal_heritage_reading, base_extractiveness, 2005, 0.69).
narrative_ontology:measurement(hagi_be_t2019, hagia_sophia_substrate__universal_heritage_reading, base_extractiveness, 2019, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(hagi_su_t1934, hagia_sophia_substrate__universal_heritage_reading, suppression_requirement, 1934, 0.7).
narrative_ontology:measurement(hagi_su_t1950, hagia_sophia_substrate__universal_heritage_reading, suppression_requirement, 1950, 0.75).
narrative_ontology:measurement(hagi_su_t1970, hagia_sophia_substrate__universal_heritage_reading, suppression_requirement, 1970, 0.78).
narrative_ontology:measurement(hagi_su_t1990, hagia_sophia_substrate__universal_heritage_reading, suppression_requirement, 1990, 0.8).
narrative_ontology:measurement(hagi_su_t2005, hagia_sophia_substrate__universal_heritage_reading, suppression_requirement, 2005, 0.8).
narrative_ontology:measurement(hagi_su_t2019, hagia_sophia_substrate__universal_heritage_reading, suppression_requirement, 2019, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hagia_sophia_substrate__universal_heritage_reading, identity_coordination).
narrative_ontology:affects_constraint(hagia_sophia_substrate__universal_heritage_reading, hagia_sophia_substrate__islamic_sovereignty_reading).
narrative_ontology:affects_constraint(hagia_sophia_substrate__universal_heritage_reading, hagia_sophia_substrate__orthodox_restitution_reading).

% DUAL FORMULATION NOTE:
% This story is one of three readings of the 'hagia_sophia_substrate' kernel, each representing a distinct structural constraint. This 'universal_heritage_reading' focuses on the site's secular museum status and its implications for global tourism, scholarship, and national identity, contrasting with religious and national sovereignty claims.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
