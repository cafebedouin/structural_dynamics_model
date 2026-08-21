% ============================================================================
% CONSTRAINT STORY: hagia_sophia_substrate__orthodox_restitution_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hagia_sophia_substrate__orthodox_restitution_reading, []).

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
 *   constraint_id: hagia_sophia_substrate__orthodox_restitution_reading
 *   human_readable: Hagia Sophia: Orthodox Restitution Claim
 *   domain: cultural_heritage/sovereignty/religious_authority
 *
 * SUMMARY:
 *   This constraint represents the normative claim that Hagia Sophia's
 *   legitimacy derives from its founding as a Christian cathedral and should
 *   return to Orthodox ecclesiastical control or remain neutral to honor its
 *   Byzantine origins. It is one reading of the 'Hagia Sophia Substrate'
 *   kernel, which encompasses competing claims over the site's identity and
 *   control. While this claim has low material enforcement, it is highly
 *   active ideologically and diplomatically, serving as a persistent point of
 *   contention in Greek-Turkish relations and a rallying cry for the Eastern
 *   Orthodox diaspora.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hagia_sophia_substrate__orthodox_restitution_reading, 0.68).
domain_priors:suppression_score(hagia_sophia_substrate__orthodox_restitution_reading, 0.6).
domain_priors:theater_ratio(hagia_sophia_substrate__orthodox_restitution_reading, 0.75).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hagia_sophia_substrate__orthodox_restitution_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(hagia_sophia_substrate__orthodox_restitution_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(hagia_sophia_substrate__orthodox_restitution_reading, theater_ratio, 0.75).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hagia_sophia_substrate__orthodox_restitution_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(hagia_sophia_substrate__orthodox_restitution_reading, resistance, 0.85).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hagia_sophia_substrate__orthodox_restitution_reading, snare).
narrative_ontology:human_readable(hagia_sophia_substrate__orthodox_restitution_reading, "Hagia Sophia: Orthodox Restitution Claim").
narrative_ontology:topic_domain(hagia_sophia_substrate__orthodox_restitution_reading, "cultural_heritage/sovereignty/religious_authority").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hagia_sophia_substrate__orthodox_restitution_reading, 'aecc7d4f-3d0e-40fc-a59e-7d95dd6ebf77').
narrative_ontology:cs_kernel_codification('aecc7d4f-3d0e-40fc-a59e-7d95dd6ebf77', fixed_text).
narrative_ontology:cs_authority_grounding('aecc7d4f-3d0e-40fc-a59e-7d95dd6ebf77', lineage).
narrative_ontology:cs_interpretation_layer_present('aecc7d4f-3d0e-40fc-a59e-7d95dd6ebf77').
narrative_ontology:cs_reading_relation('aecc7d4f-3d0e-40fc-a59e-7d95dd6ebf77', hagia_sophia_substrate__islamic_sovereignty_reading, forecloses).
narrative_ontology:cs_reading_relation('aecc7d4f-3d0e-40fc-a59e-7d95dd6ebf77', hagia_sophia_substrate__universal_heritage_reading, coexists_with).
narrative_ontology:cs_axiom('aecc7d4f-3d0e-40fc-a59e-7d95dd6ebf77', foundational, byzantine_christian_primacy).
narrative_ontology:cs_axiom_status(byzantine_christian_primacy, holdable).
narrative_ontology:cs_axiom_grounding('aecc7d4f-3d0e-40fc-a59e-7d95dd6ebf77', byzantine_christian_primacy, theological).
narrative_ontology:cs_axiom('aecc7d4f-3d0e-40fc-a59e-7d95dd6ebf77', foundational, ecclesiastical_property_inalienability).
narrative_ontology:cs_axiom_status(ecclesiastical_property_inalienability, holdable).
narrative_ontology:cs_axiom_grounding('aecc7d4f-3d0e-40fc-a59e-7d95dd6ebf77', ecclesiastical_property_inalienability, conventional).
narrative_ontology:cs_reference_frame('aecc7d4f-3d0e-40fc-a59e-7d95dd6ebf77', hagia_sophia_as_orthodox_cathedral).
narrative_ontology:cs_drift_state('aecc7d4f-3d0e-40fc-a59e-7d95dd6ebf77', contemporary, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('aecc7d4f-3d0e-40fc-a59e-7d95dd6ebf77', '').
narrative_ontology:cs_kernel_id(hagia_sophia_substrate__orthodox_restitution_reading, hagia_sophia_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hagia_sophia_substrate__orthodox_restitution_reading, eastern_orthodox_diaspora).
narrative_ontology:constraint_beneficiary(hagia_sophia_substrate__orthodox_restitution_reading, greek_state).
narrative_ontology:constraint_victim(hagia_sophia_substrate__orthodox_restitution_reading, turkish_sovereignty).
narrative_ontology:constraint_victim(hagia_sophia_substrate__orthodox_restitution_reading, islamic_worship_continuity).
narrative_ontology:constraint_vindicates(hagia_sophia_substrate__orthodox_restitution_reading, byzantine_heritage_primacy).
narrative_ontology:constraint_vindicates(hagia_sophia_substrate__orthodox_restitution_reading, ecclesiastical_property_rights).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Identifies strongly with the historical status of Hagia Sophia as an Orthodox cathedral. Benefits symbolically from the claim's persistence, even without material enforcement, as it maintains a narrative of historical right and cultural continuity. Exit from this identity is unthinkable.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__orthodox_restitution_reading, eastern_orthodox_diaspora, beneficiary,
    powerless, generational, identity_locked, global).

% Uses the claim as diplomatic leverage in broader geopolitical tensions with Turkey. Benefits from the international attention and moral support for the restitution narrative, enhancing its cultural and historical standing. Direct enforcement is not feasible, but diplomatic pressure is maintained.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__orthodox_restitution_reading, greek_state, beneficiary,
    institutional, generational, constrained, national).

% Currently exercises sovereign control over Hagia Sophia. Bears the cost of persistent international and diplomatic pressure from proponents of restitution. Its legitimacy is challenged by the claim, forcing it to continuously defend its historical and legal position.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__orthodox_restitution_reading, turkish_state, agenda_setter,
    institutional, generational, constrained, national).

% The abstract entity of Turkish sovereignty is the primary target of the restitution claim, which seeks to diminish its authority over a key national monument. It is 'trapped' by external normative claims on its territory and cultural heritage.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__orthodox_restitution_reading, turkish_sovereignty, payer,
    institutional, generational, trapped, national).

% Represents the continuity of Islamic worship at Hagia Sophia since 1453, particularly after its re-conversion to a mosque in 2020. The restitution claim seeks to interrupt this continuity, imposing a symbolic and practical cost on the global Muslim community that views it as a sacred site.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__orthodox_restitution_reading, islamic_worship_continuity, payer,
    powerless, generational, identity_locked, global).

% Advocate for Hagia Sophia's status as a neutral, shared cultural heritage site, transcending national or religious claims. Their perspective is often sidelined in the binary contest between restitution and sovereignty, as both primary claimants prioritize their own exclusive rights.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__orthodox_restitution_reading, universal_heritage_advocates, excluded,
    moderate, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None. This claim does not solve a collective action problem for all parties; rather, it asserts a historical right that creates conflict.
% TRANSFER_FUNCTION: Seeks to transfer symbolic and, ideally, ecclesiastical control of Hagia Sophia from Turkish state/Islamic community to Eastern Orthodox ecclesiastical authority, based on historical and religious claims.
% ABSENT_VOICES: Universal heritage advocates, who propose a neutral, shared status for Hagia Sophia, are excluded from the primary contest between exclusive religious/national claims. They would argue for a solution that transcends the binary of restitution vs. sovereignty.
% DISAPPEARANCE_RATIONALE: If the Orthodox restitution claim vanished overnight, the geopolitical and cultural landscape surrounding Hagia Sophia would rearrange. The Greek state would lose a significant diplomatic tool, the Eastern Orthodox diaspora would lose a central rallying point for historical grievance, and the Turkish state would face less external pressure regarding the site's status. While the physical control would remain unchanged, the ideological contest would be significantly altered.
% FOUNDING_PROBLEM: The historical loss of Hagia Sophia as an Orthodox Christian cathedral following the Ottoman conquest of Constantinople in 1453, and its subsequent conversion to a mosque.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested as 'live' by the Eastern Orthodox Church, the Greek state, and various international cultural heritage organizations (though the latter may not endorse restitution, they corroborate the historical facts of its original function). The Turkish state and Islamic community contest its 'live' status, asserting the legitimacy of the post-conquest status quo.
narrative_ontology:disappearance_verdict(hagia_sophia_substrate__orthodox_restitution_reading, world_rearranges).
narrative_ontology:founding_problem_status(hagia_sophia_substrate__orthodox_restitution_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hagia_sophia_substrate__orthodox_restitution_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(hagia_sophia_substrate__orthodox_restitution_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hagia_sophia_substrate__orthodox_restitution_reading, 0.68, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hagia_sophia_substrate__orthodox_restitution_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(hagia_sophia_substrate__orthodox_restitution_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(hagia_sophia_substrate__orthodox_restitution_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The `extractiveness` is high (0.68) because the claim demands a significant transfer of control and symbolic ownership from the current holders. `Suppression` is moderate (0.60); while there's no direct physical enforcement, the Turkish state faces persistent diplomatic and ideological pressure, limiting its alternatives for unchallenged control. `Theater_ratio` is high (0.75) as the claim's persistence is largely symbolic and performative, with little realistic pathway to material implementation, yet it remains a potent ideological force. `Resistance` is very high (0.85) due to strong opposition from the Turkish state and Islamic community. The `claimed_type` is `snare` because it is an extractive claim with identifiable victims, even if its enforcement mechanism is primarily ideological and diplomatic rather than physical.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the Eastern Orthodox diaspora and the Greek state, this claim represents a just historical restitution. From the perspective of the Turkish state and Islamic community, it is an external challenge to national sovereignty and religious freedom, seen as an attempt to extract control over a site they consider their own. The engine will compute these divergent classifications based on the declared structural relationships.
 *
 * DIRECTIONALITY LOGIC:
 *   The Eastern Orthodox diaspora and the Greek state are beneficiaries, gaining symbolic and diplomatic leverage from the claim's persistence. Turkish sovereignty and Islamic worship continuity are the victims, bearing the cost of challenged legitimacy and potential disruption. The Turkish state acts as the agenda-setter, currently controlling the site and defending its status quo against external claims.
 *
 * MANDATROPHY ANALYSIS:
 *   The 'mandate' of this claim (restitution of Hagia Sophia to Orthodox control) is still very much 'live' for its proponents. However, its *function* as a practically enforceable constraint has atrophied significantly since 1453. Its persistence is due to deep-seated historical grievances and ongoing ideological performance, rather than active enforcement capacity. This aligns with aspects of a piton, but the claim's inherent extractiveness and identifiable victims push it towards a snare in its current, ideologically active form.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    implementation_feasibility,
    'What is the realistic pathway for the Orthodox restitution claim to be materially implemented, given current geopolitical realities?',
    'Analysis of international legal precedents, diplomatic leverage, and the political will of involved states. A shift in Turkish domestic policy or a major international intervention would be required.',
    'If a realistic pathway were identified, the constraint''s effective suppression and extractiveness would dramatically increase, potentially reclassifying it as a Tangled Rope or even a fully enforced Snare. If no pathway exists, its classification as a Snare with high theater is reinforced.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(implementation_feasibility, empirical, 'Assesses the practical enforceability of the restitution claim.').

omega_variable(
    symbolic_vs_material_extraction,
    'What proportion of the measured extractiveness is symbolic (e.g., challenge to legitimacy, diplomatic cost) versus material (e.g., actual loss of revenue, physical control)?',
    'Detailed economic and political analysis quantifying the tangible costs borne by the Turkish state and Islamic community due to the claim, versus the intangible costs of challenged identity and historical narrative.',
    'If symbolic extraction dominates, the constraint''s classification as a Snare with high theater is reinforced, highlighting its ideological rather than physical coercive power. If material extraction were found to be higher, it would indicate a more potent, albeit unacknowledged, coercive force.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(symbolic_vs_material_extraction, conceptual, 'Distinguishes between symbolic and material components of extraction.').

omega_variable(
    framing_under_determination_hagia_sophia,
    'Is the current framing of this constraint as a ''Snare'' the only defensible one, or could an alternative framing (e.g., as a ''Piton'' of historical grievance) produce a different classification?',
    'Re-evaluating the constraint through the lens of its functional atrophy versus its active ideological role. If the ''atrophied function'' aspect were prioritized over ''active extraction'', it might shift towards a Piton.',
    'If reclassified as a Piton, it would emphasize the constraint''s persistence due to inertia and performance rather than active extraction, altering the analysis of its impact on victims. The current Snare classification emphasizes the active, albeit non-physical, extractive intent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(framing_under_determination_hagia_sophia, conceptual, 'Alternative framing of the constraint''s primary structural function.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hagia_sophia_substrate__orthodox_restitution_reading, 1453, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hagi_tr_t1453, hagia_sophia_substrate__orthodox_restitution_reading, theater_ratio, 1453, 0.6).
narrative_ontology:measurement(hagi_tr_t1934, hagia_sophia_substrate__orthodox_restitution_reading, theater_ratio, 1934, 0.65).
narrative_ontology:measurement(hagi_tr_t1980, hagia_sophia_substrate__orthodox_restitution_reading, theater_ratio, 1980, 0.7).
narrative_ontology:measurement(hagi_tr_t2000, hagia_sophia_substrate__orthodox_restitution_reading, theater_ratio, 2000, 0.75).
narrative_ontology:measurement(hagi_tr_t2020, hagia_sophia_substrate__orthodox_restitution_reading, theater_ratio, 2020, 0.8).
narrative_ontology:measurement(hagi_tr_t2024, hagia_sophia_substrate__orthodox_restitution_reading, theater_ratio, 2024, 0.75).

% Extraction over time
narrative_ontology:measurement(hagi_be_t1453, hagia_sophia_substrate__orthodox_restitution_reading, base_extractiveness, 1453, 0.5).
narrative_ontology:measurement(hagi_be_t1934, hagia_sophia_substrate__orthodox_restitution_reading, base_extractiveness, 1934, 0.55).
narrative_ontology:measurement(hagi_be_t1980, hagia_sophia_substrate__orthodox_restitution_reading, base_extractiveness, 1980, 0.6).
narrative_ontology:measurement(hagi_be_t2000, hagia_sophia_substrate__orthodox_restitution_reading, base_extractiveness, 2000, 0.65).
narrative_ontology:measurement(hagi_be_t2020, hagia_sophia_substrate__orthodox_restitution_reading, base_extractiveness, 2020, 0.7).
narrative_ontology:measurement(hagi_be_t2024, hagia_sophia_substrate__orthodox_restitution_reading, base_extractiveness, 2024, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(hagi_su_t1453, hagia_sophia_substrate__orthodox_restitution_reading, suppression_requirement, 1453, 0.3).
narrative_ontology:measurement(hagi_su_t1934, hagia_sophia_substrate__orthodox_restitution_reading, suppression_requirement, 1934, 0.4).
narrative_ontology:measurement(hagi_su_t1980, hagia_sophia_substrate__orthodox_restitution_reading, suppression_requirement, 1980, 0.5).
narrative_ontology:measurement(hagi_su_t2000, hagia_sophia_substrate__orthodox_restitution_reading, suppression_requirement, 2000, 0.55).
narrative_ontology:measurement(hagi_su_t2020, hagia_sophia_substrate__orthodox_restitution_reading, suppression_requirement, 2020, 0.65).
narrative_ontology:measurement(hagi_su_t2024, hagia_sophia_substrate__orthodox_restitution_reading, suppression_requirement, 2024, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hagia_sophia_substrate__orthodox_restitution_reading, identity_coordination).
narrative_ontology:affects_constraint(hagia_sophia_substrate__orthodox_restitution_reading, turkish_sovereignty_claim).
narrative_ontology:affects_constraint(hagia_sophia_substrate__orthodox_restitution_reading, islamic_waqf_legitimacy).
narrative_ontology:affects_constraint(hagia_sophia_substrate__orthodox_restitution_reading, hagia_sophia_substrate__universal_heritage_reading).
narrative_ontology:affects_constraint(hagia_sophia_substrate__orthodox_restitution_reading, hagia_sophia_substrate__islamic_sovereignty_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'Hagia Sophia Substrate' kernel. Its structural properties differ significantly from the 'Islamic Sovereignty' and 'Universal Heritage' readings, necessitating separate constraint stories.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
