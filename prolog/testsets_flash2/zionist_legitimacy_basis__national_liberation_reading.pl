% ============================================================================
% CONSTRAINT STORY: zionist_legitimacy_basis__national_liberation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_zionist_legitimacy_basis__national_liberation_reading, []).

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
 *   constraint_id: zionist_legitimacy_basis__national_liberation_reading
 *   human_readable: Zionism: National Liberation Reading
 *   domain: political_history/nationalism/settler_colonialism
 *
 * SUMMARY:
 *   This constraint models Zionism as a national liberation movement for a
 *   persecuted indigenous people returning to their ancestral homeland.
 *   Within this reading, the establishment of Israel is justified by
 *   historical connection and the imperative for Jewish self-determination in
 *   the face of historical persecution. Arab opposition is largely
 *   delegitimized as a denial of Jewish rights. The constraint's high
 *   extractiveness and suppression reflect the costs borne by the Palestinian
 *   people, whose displacement and dispossession are framed as necessary for
 *   Jewish national self-realization. The claimed type is 'snare' because the
 *   coordination story (Jewish self-determination) serves as cover for the
 *   extraction and suppression of another people, requiring active
 *   enforcement and suppressing alternatives.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(zionist_legitimacy_basis__national_liberation_reading, 0.85).
domain_priors:suppression_score(zionist_legitimacy_basis__national_liberation_reading, 0.92).
domain_priors:theater_ratio(zionist_legitimacy_basis__national_liberation_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(zionist_legitimacy_basis__national_liberation_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(zionist_legitimacy_basis__national_liberation_reading, suppression_requirement, 0.92).
narrative_ontology:constraint_metric(zionist_legitimacy_basis__national_liberation_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(zionist_legitimacy_basis__national_liberation_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(zionist_legitimacy_basis__national_liberation_reading, resistance, 0.88).

% --- Constraint claim ---
narrative_ontology:constraint_claim(zionist_legitimacy_basis__national_liberation_reading, snare).
narrative_ontology:human_readable(zionist_legitimacy_basis__national_liberation_reading, "Zionism: National Liberation Reading").
narrative_ontology:topic_domain(zionist_legitimacy_basis__national_liberation_reading, "political_history/nationalism/settler_colonialism").

domain_priors:requires_active_enforcement(zionist_legitimacy_basis__national_liberation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(zionist_legitimacy_basis__national_liberation_reading, '20c8cdcc-b4d3-4839-911e-ffd2af25611c').
narrative_ontology:cs_kernel_codification('20c8cdcc-b4d3-4839-911e-ffd2af25611c', formalized).
narrative_ontology:cs_authority_grounding('20c8cdcc-b4d3-4839-911e-ffd2af25611c', lineage).
narrative_ontology:cs_interpretation_layer_present('20c8cdcc-b4d3-4839-911e-ffd2af25611c').
narrative_ontology:cs_reading_relation('20c8cdcc-b4d3-4839-911e-ffd2af25611c', zionist_legitimacy_basis__settler_colonial_reading, coexists_with).
narrative_ontology:cs_reading_relation('20c8cdcc-b4d3-4839-911e-ffd2af25611c', zionist_legitimacy_basis__religious_restoration_reading, coexists_with).
narrative_ontology:cs_axiom('20c8cdcc-b4d3-4839-911e-ffd2af25611c', foundational, jewish_people_are_an_indigenous_nation).
narrative_ontology:cs_axiom_status(jewish_people_are_an_indigenous_nation, holdable).
narrative_ontology:cs_axiom_grounding('20c8cdcc-b4d3-4839-911e-ffd2af25611c', jewish_people_are_an_indigenous_nation, conventional).
narrative_ontology:cs_axiom('20c8cdcc-b4d3-4839-911e-ffd2af25611c', foundational, self_determination_requires_sovereign_state).
narrative_ontology:cs_axiom_status(self_determination_requires_sovereign_state, holdable).
narrative_ontology:cs_axiom_grounding('20c8cdcc-b4d3-4839-911e-ffd2af25611c', self_determination_requires_sovereign_state, instrumental).
narrative_ontology:cs_reference_frame('20c8cdcc-b4d3-4839-911e-ffd2af25611c', post_holocaust_national_self_determination).
narrative_ontology:cs_drift_state('20c8cdcc-b4d3-4839-911e-ffd2af25611c', contemporary_human_rights_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('20c8cdcc-b4d3-4839-911e-ffd2af25611c', '').
narrative_ontology:cs_kernel_id(zionist_legitimacy_basis__national_liberation_reading, zionist_legitimacy_basis).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(zionist_legitimacy_basis__national_liberation_reading, israeli_state).
narrative_ontology:constraint_beneficiary(zionist_legitimacy_basis__national_liberation_reading, jewish_diaspora_supporters).
narrative_ontology:constraint_victim(zionist_legitimacy_basis__national_liberation_reading, palestinian_people).
narrative_ontology:constraint_victim(zionist_legitimacy_basis__national_liberation_reading, arab_states_opposing_zionism).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(zionist_legitimacy_basis__national_liberation_reading, international_community_supporters).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The primary institutional actor that frames Zionism as a national liberation movement, enacting policies that secure Jewish self-determination in the ancestral homeland. Benefits from the narrative's legitimizing power on the international stage, justifying its existence and actions.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__national_liberation_reading, israeli_state, agenda_setter,
    institutional, generational, constrained, national).

% Individuals and organizations in the Jewish diaspora who actively support the Zionist project, viewing it as the fulfillment of Jewish national aspirations and a refuge from antisemitism. They benefit from a sense of collective identity and security, contributing politically and financially.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__national_liberation_reading, jewish_diaspora_supporters, beneficiary,
    organized, biographical, mobile, global).

% The indigenous population displaced and dispossessed by the establishment and expansion of the Israeli state. Their national aspirations and rights are delegitimized by the national liberation narrative, which frames their resistance as an attack on Jewish self-determination.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__national_liberation_reading, palestinian_people, payer,
    powerless, generational, trapped, regional).

% Neighboring states that historically opposed the Zionist project, viewing it as an imposition on the Arab world and a cause of regional instability. They bear political and military costs from the ongoing conflict, with their opposition often framed as antisemitic or anti-peace by the national liberation narrative.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__national_liberation_reading, arab_states_opposing_zionism, payer,
    powerful, generational, constrained, regional).

% States and international organizations that recognize and support Israel's right to exist as a Jewish state, often influenced by the national liberation narrative. They benefit from perceived stability in the region and alignment with a narrative of self-determination for a persecuted people.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__national_liberation_reading, international_community_supporters, beneficiary,
    institutional, generational, analytical, global).

% Organizations and individuals who analyze the conflict through the lens of human rights and international law, often critiquing the impact of Israeli policies on Palestinians. They observe the constraint's operation and its effects, providing alternative framings and advocating for change.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__national_liberation_reading, international_human_rights_advocates, observer,
    organized, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the collective action of Jewish people globally towards establishing and maintaining a sovereign state in their ancestral homeland, providing a shared identity and purpose for a historically persecuted group.
% TRANSFER_FUNCTION: Transfers land, resources, and political sovereignty from the indigenous Palestinian population to the Jewish people, justified by historical connection and the need for self-determination.
% ABSENT_VOICES: The full historical narrative and ongoing experience of the Palestinian people are often marginalized or delegitimized within this framework, as their claims to indigeneity and self-determination are seen as conflicting with Jewish rights. Their voices would articulate the costs of displacement and occupation.
% DISAPPEARANCE_RATIONALE: If the national liberation framing of Zionism disappeared, the legitimacy of the Israeli state would be fundamentally challenged, leading to a radical re-evaluation of land claims, citizenship, and the rights of both Jewish Israelis and Palestinians. The entire political and social structure of the region would be forced to rearrange.
% FOUNDING_PROBLEM: The historical persecution of Jewish people, culminating in the Holocaust, and the absence of a secure homeland where they could exercise self-determination.
% FOUNDING_PROBLEM_CORROBORATION: The Israeli state and Jewish diaspora organizations consistently attest that the founding problem of Jewish insecurity and the need for self-determination remains live, citing ongoing antisemitism and threats to Israel's existence. This is corroborated by historical evidence of persecution and the continued existence of antisemitic movements globally, though the specific solution (a Jewish ethno-state) is contested by many outside the benefiting parties.
narrative_ontology:disappearance_verdict(zionist_legitimacy_basis__national_liberation_reading, world_rearranges).
narrative_ontology:founding_problem_status(zionist_legitimacy_basis__national_liberation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(zionist_legitimacy_basis__national_liberation_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(zionist_legitimacy_basis__national_liberation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(zionist_legitimacy_basis__national_liberation_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(zionist_legitimacy_basis__national_liberation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(zionist_legitimacy_basis__national_liberation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(zionist_legitimacy_basis__national_liberation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (0.85) reflects the extensive transfer of land, resources, and sovereignty from Palestinians to the Israeli state. Suppression (0.92) is extremely high due to the ongoing military occupation, legal frameworks that privilege Jewish citizens, and the systematic denial of Palestinian rights of return and self-determination. The theater ratio (0.4) indicates that while elements of national liberation rhetoric persist, a significant portion of the state's actions are dedicated to maintaining control and suppressing resistance rather than purely securing a homeland. Accessibility collapse is high (0.75) because alternatives for Palestinians (e.g., a sovereign state, right of return) are systematically undermined. Resistance is also high (0.88) due to continuous Palestinian struggle against occupation and displacement.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the Israeli state and its supporters, the constraint is a legitimate act of national liberation, a 'rope' or even a 'mountain' of historical necessity. From the perspective of the Palestinian people, it is a 'snare' of dispossession and oppression. The engine's classification will highlight this divergence by computing a 'snare' classification from the structural metrics, regardless of the 'rope' claim, thereby exposing the extractive reality beneath the liberation narrative.
 *
 * DIRECTIONALITY LOGIC:
 *   The Israeli state and its global Jewish diaspora supporters are clear beneficiaries, gaining a secure homeland and a strengthened collective identity. The Palestinian people are the primary victims, bearing the costs of displacement, loss of land, and denial of self-determination. Arab states opposing Zionism also bear significant political and military costs. International community supporters benefit from perceived regional stability and alignment with a narrative of self-determination. International human rights advocates serve as observers, analyzing the constraint's impact.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mislabeling by explicitly acknowledging the high extractiveness and suppression inherent in the 'national liberation' project when it involves the displacement of an indigenous population. It avoids treating the 'liberation' aspect as a pure coordination function by detailing the victims and the active enforcement required to maintain the status quo. The 'snare' classification highlights that the coordination story is cover for extraction, rather than a genuine collective-action solution for all parties involved.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    indigeneity_claim_ambiguity,
    'Is the claim of Jewish indigeneity to the land, after two millennia of diaspora, structurally equivalent to the indigeneity of the Palestinian people who have continuously inhabited it?',
    'Historical and anthropological analysis of continuous habitation, cultural practice, and land stewardship, independent of religious or national narratives.',
    'If not equivalent, the ''indigenous return'' justification for displacement weakens, shifting the constraint''s legitimacy basis and potentially increasing its perceived extractiveness from the Palestinian seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(indigeneity_claim_ambiguity, conceptual, 'Ambiguity in competing claims of indigeneity.').

omega_variable(
    national_liberation_vs_settler_colonialism,
    'At what point does a national liberation movement, when it involves the displacement of another people, transition into a settler-colonial project?',
    'Comparative historical analysis of other national liberation movements and settler-colonial states, focusing on patterns of land acquisition, population transfer, and the legal status of indigenous populations.',
    'If the structural patterns align more closely with settler-colonialism, the ''national liberation'' framing becomes theatrical, increasing the constraint''s effective extractiveness and suppression from the victim''s perspective.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(national_liberation_vs_settler_colonialism, conceptual, 'The conceptual boundary between national liberation and settler-colonialism.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of Palestinian resistance structural (military occupation, legal discrimination) or internalized (psychological impact of prolonged conflict, despair)?',
    'Post-occupation suppression trajectory: if resistance persists and escalates after the formal end of occupation, reclassify as partially internalized; if it diminishes, it was primarily structural.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the target carries the suppression with them after exit, making the constraint more resilient.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in the context of occupation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(zionist_legitimacy_basis__national_liberation_reading, 1948, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(zion_tr_t1948, zionist_legitimacy_basis__national_liberation_reading, theater_ratio, 1948, 0.2).
narrative_ontology:measurement(zion_tr_t1967, zionist_legitimacy_basis__national_liberation_reading, theater_ratio, 1967, 0.3).
narrative_ontology:measurement(zion_tr_t1993, zionist_legitimacy_basis__national_liberation_reading, theater_ratio, 1993, 0.35).
narrative_ontology:measurement(zion_tr_t2000, zionist_legitimacy_basis__national_liberation_reading, theater_ratio, 2000, 0.38).
narrative_ontology:measurement(zion_tr_t2014, zionist_legitimacy_basis__national_liberation_reading, theater_ratio, 2014, 0.39).
narrative_ontology:measurement(zion_tr_t2024, zionist_legitimacy_basis__national_liberation_reading, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(zion_be_t1948, zionist_legitimacy_basis__national_liberation_reading, base_extractiveness, 1948, 0.7).
narrative_ontology:measurement(zion_be_t1967, zionist_legitimacy_basis__national_liberation_reading, base_extractiveness, 1967, 0.8).
narrative_ontology:measurement(zion_be_t1993, zionist_legitimacy_basis__national_liberation_reading, base_extractiveness, 1993, 0.82).
narrative_ontology:measurement(zion_be_t2000, zionist_legitimacy_basis__national_liberation_reading, base_extractiveness, 2000, 0.83).
narrative_ontology:measurement(zion_be_t2014, zionist_legitimacy_basis__national_liberation_reading, base_extractiveness, 2014, 0.84).
narrative_ontology:measurement(zion_be_t2024, zionist_legitimacy_basis__national_liberation_reading, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(zion_su_t1948, zionist_legitimacy_basis__national_liberation_reading, suppression_requirement, 1948, 0.75).
narrative_ontology:measurement(zion_su_t1967, zionist_legitimacy_basis__national_liberation_reading, suppression_requirement, 1967, 0.85).
narrative_ontology:measurement(zion_su_t1993, zionist_legitimacy_basis__national_liberation_reading, suppression_requirement, 1993, 0.88).
narrative_ontology:measurement(zion_su_t2000, zionist_legitimacy_basis__national_liberation_reading, suppression_requirement, 2000, 0.9).
narrative_ontology:measurement(zion_su_t2014, zionist_legitimacy_basis__national_liberation_reading, suppression_requirement, 2014, 0.91).
narrative_ontology:measurement(zion_su_t2024, zionist_legitimacy_basis__national_liberation_reading, suppression_requirement, 2024, 0.92).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(zionist_legitimacy_basis__national_liberation_reading, identity_coordination).
narrative_ontology:affects_constraint(zionist_legitimacy_basis__national_liberation_reading, zionist_legitimacy_basis__settler_colonial_reading).
narrative_ontology:affects_constraint(zionist_legitimacy_basis__national_liberation_reading, zionist_legitimacy_basis__religious_restoration_reading).
narrative_ontology:affects_constraint(zionist_legitimacy_basis__national_liberation_reading, israeli_occupation_policies).
narrative_ontology:affects_constraint(zionist_legitimacy_basis__national_liberation_reading, palestinian_right_of_return).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'zionist_legitimacy_basis' kernel. Its 'national_liberation' framing is distinct from the 'settler_colonial_reading' and 'religious_restoration_reading', which offer alternative justifications or critiques of Zionism. All three are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
