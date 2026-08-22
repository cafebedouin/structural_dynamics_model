% ============================================================================
% CONSTRAINT STORY: waitangi_sovereignty_allocation__rangatiratanga_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_waitangi_sovereignty_allocation__rangatiratanga_reading, []).

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
 *   constraint_id: waitangi_sovereignty_allocation__rangatiratanga_reading
 *   human_readable: Treaty of Waitangi Article II (Māori Text) - Tino Rangatiratanga
 *   domain: constitutional_law/indigenous_rights/post_colonial_governance
 *
 * SUMMARY:
 *   This constraint story represents the 'rangatiratanga reading' of Article
 *   II of the Treaty of Waitangi, focusing on the Māori text. From this
 *   perspective, Māori retained full authority (tino rangatiratanga) over
 *   their lands, resources, and cultural treasures (taonga), while the Crown
 *   gained only governorship (kāwanatanga) over its own settlers. The
 *   constraint, as it has operated historically, is a Tangled Rope because it
 *   has a genuine coordination function (establishing a basis for
 *   coexistence) but has been used by the Crown for asymmetric extraction of
 *   sovereignty and resources, requiring active enforcement to suppress Māori
 *   claims. The metrics reflect the historical and ongoing extraction and
 *   suppression experienced by Māori.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(waitangi_sovereignty_allocation__rangatiratanga_reading, 0.65).
domain_priors:suppression_score(waitangi_sovereignty_allocation__rangatiratanga_reading, 0.78).
domain_priors:theater_ratio(waitangi_sovereignty_allocation__rangatiratanga_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__rangatiratanga_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__rangatiratanga_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__rangatiratanga_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__rangatiratanga_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__rangatiratanga_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(waitangi_sovereignty_allocation__rangatiratanga_reading, tangled_rope).
narrative_ontology:human_readable(waitangi_sovereignty_allocation__rangatiratanga_reading, "Treaty of Waitangi Article II (Māori Text) - Tino Rangatiratanga").
narrative_ontology:topic_domain(waitangi_sovereignty_allocation__rangatiratanga_reading, "constitutional_law/indigenous_rights/post_colonial_governance").

domain_priors:requires_active_enforcement(waitangi_sovereignty_allocation__rangatiratanga_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(waitangi_sovereignty_allocation__rangatiratanga_reading, '03b21ddb-c7e0-4c53-840b-fb218cad3df4').
narrative_ontology:cs_kernel_codification('03b21ddb-c7e0-4c53-840b-fb218cad3df4', fixed_text).
narrative_ontology:cs_authority_grounding('03b21ddb-c7e0-4c53-840b-fb218cad3df4', lineage).
narrative_ontology:cs_interpretation_layer_present('03b21ddb-c7e0-4c53-840b-fb218cad3df4').
narrative_ontology:cs_reading_relation('03b21ddb-c7e0-4c53-840b-fb218cad3df4', waitangi_sovereignty_allocation__crown_sovereignty_reading, forecloses).
narrative_ontology:cs_reading_relation('03b21ddb-c7e0-4c53-840b-fb218cad3df4', waitangi_sovereignty_allocation__partnership_reading, coexists_with).
narrative_ontology:cs_axiom('03b21ddb-c7e0-4c53-840b-fb218cad3df4', foundational, tino_rangatiratanga_retained).
narrative_ontology:cs_axiom_status(tino_rangatiratanga_retained, holdable).
narrative_ontology:cs_axiom_grounding('03b21ddb-c7e0-4c53-840b-fb218cad3df4', tino_rangatiratanga_retained, deontological).
narrative_ontology:cs_axiom('03b21ddb-c7e0-4c53-840b-fb218cad3df4', foundational, kawanatanga_limited_to_settlers).
narrative_ontology:cs_axiom_status(kawanatanga_limited_to_settlers, holdable).
narrative_ontology:cs_axiom_grounding('03b21ddb-c7e0-4c53-840b-fb218cad3df4', kawanatanga_limited_to_settlers, conventional).
narrative_ontology:cs_reference_frame('03b21ddb-c7e0-4c53-840b-fb218cad3df4', maori_text_original_intent).
narrative_ontology:cs_drift_state('03b21ddb-c7e0-4c53-840b-fb218cad3df4', contemporary_new_zealand, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('03b21ddb-c7e0-4c53-840b-fb218cad3df4', '').
narrative_ontology:cs_kernel_id(waitangi_sovereignty_allocation__rangatiratanga_reading, waitangi_sovereignty_allocation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(waitangi_sovereignty_allocation__rangatiratanga_reading, crown_government_of_new_zealand).
narrative_ontology:constraint_victim(waitangi_sovereignty_allocation__rangatiratanga_reading, tangata_whenua_maori).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(waitangi_sovereignty_allocation__rangatiratanga_reading, non_maori_settler_population).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% As the indigenous people of Aotearoa, Māori assert their inherent authority (tino rangatiratanga) over their lands, resources, and cultural treasures as guaranteed by the Māori text of Article II. They bear the costs of Crown overreach and the ongoing struggle for recognition and implementation of their rights.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__rangatiratanga_reading, tangata_whenua_maori, payer,
    organized, generational, identity_locked, national).

% The Crown, through its government, claims ultimate sovereignty over all of New Zealand, often interpreting kāwanatanga (governorship) as full sovereignty. While acknowledging some Māori rights, it benefits from maintaining a unitary state structure and resisting full recognition of tino rangatiratanga, which would necessitate significant power devolution.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__rangatiratanga_reading, crown_government_of_new_zealand, agenda_setter,
    institutional, generational, constrained, national).

% The courts are tasked with interpreting the Treaty of Waitangi and its implications for modern New Zealand law. Their rulings can either uphold or challenge the Crown's interpretation of sovereignty, influencing the practical application of tino rangatiratanga.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__rangatiratanga_reading, new_zealand_judiciary, observer,
    institutional, generational, analytical, national).

% Benefits from the Crown's assertion of unitary sovereignty, which underpins the existing legal and property systems. While not directly involved in Treaty interpretation, their interests are often aligned with maintaining the status quo of Crown authority.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__rangatiratanga_reading, non_maori_settler_population, beneficiary,
    powerful, biographical, mobile, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To establish a framework for governance and coexistence between Māori and the British Crown, ensuring Māori retained authority over their affairs while the Crown exercised governorship over its settlers.
% TRANSFER_FUNCTION: The constraint, as read by the rangatiratanga perspective, intended to transfer limited kāwanatanga (governorship) to the Crown over its own people, while retaining tino rangatiratanga (full authority) for Māori over their lands, resources, and taonga. In practice, the Crown has extracted sovereignty and resources from Māori.
% ABSENT_VOICES: The full voice of Māori as sovereign entities, capable of self-determination and independent governance, has been historically suppressed and continues to be marginalized within the Crown's dominant constitutional framework. Their full authority is often treated as a 'special interest' rather than an inherent right.
% DISAPPEARANCE_RATIONALE: If the rangatiratanga reading of Article II were fully and universally implemented overnight, the entire constitutional and governance structure of New Zealand would fundamentally rearrange. Māori would exercise full authority over their territories and resources, leading to significant shifts in land ownership, resource management, and the distribution of political power, potentially establishing co-governance or independent Māori governance structures.
% FOUNDING_PROBLEM: To establish a basis for British settlement in Aotearoa while securing Māori authority and protecting their lands and resources from unfettered colonial expansion.
% FOUNDING_PROBLEM_CORROBORATION: Māori leaders and scholars consistently attest that the founding problem of securing tino rangatiratanga remains live, as the Crown has not fully honored its obligations under the Māori text of the Treaty. International indigenous rights bodies and some independent legal scholars also corroborate the ongoing nature of this struggle.
narrative_ontology:disappearance_verdict(waitangi_sovereignty_allocation__rangatiratanga_reading, world_rearranges).
narrative_ontology:founding_problem_status(waitangi_sovereignty_allocation__rangatiratanga_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(waitangi_sovereignty_allocation__rangatiratanga_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_gemini+stakeholder_backfill', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(waitangi_sovereignty_allocation__rangatiratanga_reading, 'none', 1).
narrative_ontology:epsilon_provenance(waitangi_sovereignty_allocation__rangatiratanga_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(waitangi_sovereignty_allocation__rangatiratanga_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(waitangi_sovereignty_allocation__rangatiratanga_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(waitangi_sovereignty_allocation__rangatiratanga_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.65) is high because the Crown's historical and ongoing actions have systematically undermined tino rangatiratanga, leading to significant loss of Māori land, resources, and self-governance. Suppression (0.78) is also high, reflecting the active legal, political, and military enforcement used to assert Crown sovereignty and marginalize Māori authority. The theater ratio (0.4) indicates that while some efforts are made to acknowledge Māori rights, a substantial portion of Crown activity is performative, maintaining the appearance of Treaty compliance while resisting fundamental power shifts. The historical measurements show a rise in extraction and suppression as colonial power consolidated, with a slight decrease in recent decades due to increased Māori activism and Treaty settlements, but the core imbalance remains.
 *
 * PERSPECTIVAL GAP:
 *   The Crown's perspective (crown_sovereignty_reading) would likely classify this as a Rope or even a Mountain, emphasizing the 'cession' of sovereignty and the stability of the Westminster system. The rangatiratanga reading, however, highlights the ongoing extraction and suppression, leading to a Tangled Rope classification. This divergence is central to the contest over the Treaty's meaning.
 *
 * DIRECTIONALITY LOGIC:
 *   Tangata Whenua Māori are the primary targets (payers) of this constraint, as their inherent authority and resources have been extracted. The Crown Government of New Zealand is the primary beneficiary and agenda-setter, having gained de facto sovereignty beyond the scope of kāwanatanga. The non-Māori settler population benefits from the stability of the Crown's asserted sovereignty. The New Zealand Judiciary acts as an observer, mediating interpretations.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    scope_of_kawanatanga,
    'What is the precise and legitimate scope of ''kāwanatanga'' (governorship) as understood by Māori in 1840, and how does it relate to the Crown''s current assertion of ''sovereignty''?',
    'Deep historical and linguistic analysis of 19th-century Māori political concepts, corroborated by contemporary Māori constitutional scholarship and traditional knowledge holders.',
    'A narrow interpretation of kāwanatanga would further delegitimize the Crown''s expansive claims to sovereignty, strengthening the case for Māori self-determination and potentially reclassifying the constraint as a Snare due to the lack of legitimate coordination. A broader interpretation, while still distinct from full sovereignty, might suggest a more complex, albeit still extractive, Tangled Rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(scope_of_kawanatanga, conceptual, 'Ambiguity in the scope of Crown authority granted by the Māori text.').

omega_variable(
    mandatrophy_of_crown_sovereignty,
    'Has the Crown''s claim to full sovereignty, as asserted against tino rangatiratanga, outlived its original justification (e.g., maintaining order for settlers) and now primarily serves to maintain an extractive power imbalance?',
    'Analysis of contemporary governance challenges in New Zealand: if Māori-led initiatives demonstrate superior outcomes for Māori and the wider population in areas where the Crown currently asserts exclusive control, it suggests the Crown''s ''coordination'' function has atrophied.',
    'If the Crown''s sovereignty claim is found to be largely mandatrophied and primarily extractive, the constraint would shift closer to a Snare, as its coordination story would be revealed as cover for ongoing extraction. If a genuine, non-extractive coordination function remains, it would remain a Tangled Rope, but with a higher theater ratio.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandatrophy_of_crown_sovereignty, empirical, 'Whether the Crown''s assertion of sovereignty is still functionally justified or primarily extractive.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(waitangi_sovereignty_allocation__rangatiratanga_reading, 1840, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(wait_tr_t1840, waitangi_sovereignty_allocation__rangatiratanga_reading, theater_ratio, 1840, 0.1).
narrative_ontology:measurement(wait_tr_t1870, waitangi_sovereignty_allocation__rangatiratanga_reading, theater_ratio, 1870, 0.2).
narrative_ontology:measurement(wait_tr_t1900, waitangi_sovereignty_allocation__rangatiratanga_reading, theater_ratio, 1900, 0.3).
narrative_ontology:measurement(wait_tr_t1950, waitangi_sovereignty_allocation__rangatiratanga_reading, theater_ratio, 1950, 0.45).
narrative_ontology:measurement(wait_tr_t2000, waitangi_sovereignty_allocation__rangatiratanga_reading, theater_ratio, 2000, 0.4).
narrative_ontology:measurement(wait_tr_t2024, waitangi_sovereignty_allocation__rangatiratanga_reading, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(wait_be_t1840, waitangi_sovereignty_allocation__rangatiratanga_reading, base_extractiveness, 1840, 0.2).
narrative_ontology:measurement(wait_be_t1870, waitangi_sovereignty_allocation__rangatiratanga_reading, base_extractiveness, 1870, 0.4).
narrative_ontology:measurement(wait_be_t1900, waitangi_sovereignty_allocation__rangatiratanga_reading, base_extractiveness, 1900, 0.6).
narrative_ontology:measurement(wait_be_t1950, waitangi_sovereignty_allocation__rangatiratanga_reading, base_extractiveness, 1950, 0.75).
narrative_ontology:measurement(wait_be_t2000, waitangi_sovereignty_allocation__rangatiratanga_reading, base_extractiveness, 2000, 0.7).
narrative_ontology:measurement(wait_be_t2024, waitangi_sovereignty_allocation__rangatiratanga_reading, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(wait_su_t1840, waitangi_sovereignty_allocation__rangatiratanga_reading, suppression_requirement, 1840, 0.3).
narrative_ontology:measurement(wait_su_t1870, waitangi_sovereignty_allocation__rangatiratanga_reading, suppression_requirement, 1870, 0.6).
narrative_ontology:measurement(wait_su_t1900, waitangi_sovereignty_allocation__rangatiratanga_reading, suppression_requirement, 1900, 0.8).
narrative_ontology:measurement(wait_su_t1950, waitangi_sovereignty_allocation__rangatiratanga_reading, suppression_requirement, 1950, 0.85).
narrative_ontology:measurement(wait_su_t2000, waitangi_sovereignty_allocation__rangatiratanga_reading, suppression_requirement, 2000, 0.8).
narrative_ontology:measurement(wait_su_t2024, waitangi_sovereignty_allocation__rangatiratanga_reading, suppression_requirement, 2024, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(waitangi_sovereignty_allocation__rangatiratanga_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(waitangi_sovereignty_allocation__rangatiratanga_reading, waitangi_sovereignty_allocation__crown_sovereignty_reading).
narrative_ontology:affects_constraint(waitangi_sovereignty_allocation__rangatiratanga_reading, waitangi_sovereignty_allocation__partnership_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the 'waitangi_sovereignty_allocation' kernel. This 'rangatiratanga reading' emphasizes Māori retention of full authority, contrasting with the 'crown_sovereignty_reading' (full cession) and the 'partnership_reading' (ongoing consultation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
