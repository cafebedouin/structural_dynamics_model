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
 *   human_readable: Zionism as National Liberation (National Liberation Reading)
 *   domain: political_history/nationalism/settler_colonialism_studies
 *
 * SUMMARY:
 *   This constraint story models Zionism as a national liberation movement
 *   for a persecuted indigenous people returning to their ancestral homeland.
 *   This reading emphasizes Jewish historical connection to the land and the
 *   imperative of self-determination in the face of historical persecution
 *   and antisemitism. Within this frame, Palestinian opposition is often
 *   delegitimized as a denial of Jewish rights or an extension of historical
 *   antisemitism. The constraint is claimed as a 'rope' by its proponents,
 *   but the metrics reflect a 'tangled_rope' or 'snare' due to the high
 *   extractiveness and suppression experienced by Palestinian Arabs. This is
 *   one reading of the 'zionist_legitimacy_basis' kernel.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(zionist_legitimacy_basis__national_liberation_reading, 0.7).
domain_priors:suppression_score(zionist_legitimacy_basis__national_liberation_reading, 0.8).
domain_priors:theater_ratio(zionist_legitimacy_basis__national_liberation_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(zionist_legitimacy_basis__national_liberation_reading, extractiveness, 0.7).
narrative_ontology:constraint_metric(zionist_legitimacy_basis__national_liberation_reading, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(zionist_legitimacy_basis__national_liberation_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(zionist_legitimacy_basis__national_liberation_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(zionist_legitimacy_basis__national_liberation_reading, resistance, 0.9).

% --- Constraint claim ---
narrative_ontology:constraint_claim(zionist_legitimacy_basis__national_liberation_reading, tangled_rope).
narrative_ontology:human_readable(zionist_legitimacy_basis__national_liberation_reading, "Zionism as National Liberation (National Liberation Reading)").
narrative_ontology:topic_domain(zionist_legitimacy_basis__national_liberation_reading, "political_history/nationalism/settler_colonialism_studies").

domain_priors:requires_active_enforcement(zionist_legitimacy_basis__national_liberation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(zionist_legitimacy_basis__national_liberation_reading, '9ae953fb-b47c-49cf-a545-dfa645d12703').
narrative_ontology:cs_kernel_codification('9ae953fb-b47c-49cf-a545-dfa645d12703', formalized).
narrative_ontology:cs_authority_grounding('9ae953fb-b47c-49cf-a545-dfa645d12703', lineage).
narrative_ontology:cs_interpretation_layer_present('9ae953fb-b47c-49cf-a545-dfa645d12703').
narrative_ontology:cs_reading_relation('9ae953fb-b47c-49cf-a545-dfa645d12703', zionist_legitimacy_basis__settler_colonial_reading, coexists_with).
narrative_ontology:cs_reading_relation('9ae953fb-b47c-49cf-a545-dfa645d12703', zionist_legitimacy_basis__religious_restoration_reading, coexists_with).
narrative_ontology:cs_axiom('9ae953fb-b47c-49cf-a545-dfa645d12703', foundational, jewish_people_are_an_indigenous_nation).
narrative_ontology:cs_axiom_status(jewish_people_are_an_indigenous_nation, holdable).
narrative_ontology:cs_axiom_grounding('9ae953fb-b47c-49cf-a545-dfa645d12703', jewish_people_are_an_indigenous_nation, deontological).
narrative_ontology:cs_axiom('9ae953fb-b47c-49cf-a545-dfa645d12703', foundational, self_determination_requires_sovereign_state).
narrative_ontology:cs_axiom_status(self_determination_requires_sovereign_state, holdable).
narrative_ontology:cs_axiom_grounding('9ae953fb-b47c-49cf-a545-dfa645d12703', self_determination_requires_sovereign_state, conventional).
narrative_ontology:cs_reference_frame('9ae953fb-b47c-49cf-a545-dfa645d12703', post_holocaust_national_self_determination).
narrative_ontology:cs_drift_state('9ae953fb-b47c-49cf-a545-dfa645d12703', contemporary_human_rights_discourse, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('9ae953fb-b47c-49cf-a545-dfa645d12703', '').
narrative_ontology:cs_kernel_id(zionist_legitimacy_basis__national_liberation_reading, zionist_legitimacy_basis).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(zionist_legitimacy_basis__national_liberation_reading, jewish_people_in_israel).
narrative_ontology:constraint_beneficiary(zionist_legitimacy_basis__national_liberation_reading, zionist_movement_leadership).
narrative_ontology:constraint_victim(zionist_legitimacy_basis__national_liberation_reading, palestinian_arabs).
narrative_ontology:constraint_victim(zionist_legitimacy_basis__national_liberation_reading, arab_states).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(zionist_legitimacy_basis__national_liberation_reading, international_supporters_of_israel).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Perceive themselves as having achieved self-determination and security in their ancestral homeland, a right denied for centuries. Their identity is deeply intertwined with the existence of the state of Israel. They benefit from the state's security apparatus and international recognition.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__national_liberation_reading, jewish_people_in_israel, beneficiary,
    institutional, generational, identity_locked, national).

% Articulates and defends the narrative of Zionism as a national liberation movement, securing international support and legitimizing state actions. They administer the state's policies, including those related to land, citizenship, and security, which are framed as necessary for national self-preservation.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__national_liberation_reading, zionist_movement_leadership, agenda_setter,
    institutional, generational, constrained, global).

% Experience displacement, loss of land, and denial of self-determination. Their resistance is delegitimized as an attack on Jewish national rights. They bear the direct costs of the constraint through military occupation, restrictions on movement, and loss of property.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__national_liberation_reading, palestinian_arabs, payer,
    powerless, generational, trapped, regional).

% Initially opposed the establishment of Israel, viewing it as a colonial imposition. Their opposition has been framed as antisemitic or a rejection of Jewish self-determination. They bear political and military costs from the ongoing conflict, with limited options for effective intervention.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__national_liberation_reading, arab_states, payer,
    organized, generational, constrained, regional).

% Provide diplomatic, financial, and military support to Israel, often aligning with the national liberation narrative. They benefit from perceived stability in the region or shared ideological commitments, with relatively low direct costs.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__national_liberation_reading, international_supporters_of_israel, beneficiary,
    institutional, biographical, mobile, global).

% Document and critique the human rights impacts of Israeli policies on Palestinians, often challenging the national liberation framing by highlighting issues of occupation and discrimination. Their influence is primarily through advocacy and reporting.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__national_liberation_reading, international_human_rights_organizations, observer,
    moderate, biographical, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the collective action of Jewish people globally to establish and maintain a sovereign state in their ancestral homeland, providing a refuge from persecution and a center for cultural and national identity.
% TRANSFER_FUNCTION: Transfers land, resources, and political sovereignty from the indigenous Palestinian population to the Jewish people, justified by historical connection and the imperative of national self-determination for a persecuted group.
% ABSENT_VOICES: Palestinian voices, particularly those advocating for a single, secular democratic state or the right of return, are largely excluded from the dominant discourse that frames the conflict as a clash between two national liberation movements, or as a religious conflict. Their narratives are suppressed or delegitimized.
% DISAPPEARANCE_RATIONALE: If the national liberation framing of Zionism disappeared overnight, the legitimacy of the state of Israel would be fundamentally challenged, leading to a radical re-evaluation of land claims, citizenship rights, and the status of Palestinian refugees. The entire political and social structure of the region would be forced to rearrange.
% FOUNDING_PROBLEM: The historical persecution and statelessness of the Jewish people, culminating in the Holocaust, necessitated the establishment of a sovereign Jewish state to ensure their safety and self-determination.
% FOUNDING_PROBLEM_CORROBORATION: The Jewish people in Israel and their international supporters attest that the founding problem of antisemitism and the need for a safe haven remains live, citing ongoing threats and historical trauma. Palestinian Arabs and many international observers contest this, arguing that while Jewish safety is paramount, the means of achieving it have created a new problem of statelessness and oppression for another people; they corroborate the historical persecution of Jews but dispute its justification for displacement.
narrative_ontology:disappearance_verdict(zionist_legitimacy_basis__national_liberation_reading, world_rearranges).
narrative_ontology:founding_problem_status(zionist_legitimacy_basis__national_liberation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(zionist_legitimacy_basis__national_liberation_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(zionist_legitimacy_basis__national_liberation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(zionist_legitimacy_basis__national_liberation_reading, 0.7, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is high (0.7) because the establishment and expansion of the state of Israel, justified by this narrative, involved significant displacement and dispossession of Palestinian Arabs. Suppression is very high (0.8) due to the active military and political enforcement required to maintain control over contested territories and manage Palestinian resistance. The theater ratio is moderate (0.2) as the core function of national self-determination is real, but aspects of its justification (e.g., 'making the desert bloom') have become performative in the face of indigenous displacement. Resistance is extremely high (0.9) from the Palestinian side, reflecting the ongoing struggle against the constraint.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of Jewish people in Israel, this constraint is a legitimate act of national liberation, a 'rope' providing security and self-determination. From the perspective of Palestinian Arabs, it is a 'snare' or 'tangled_rope' that extracts land and rights through coercive means. The engine's classification will reflect this divergence based on the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Jewish people in Israel and Zionist leadership are the primary beneficiaries, experiencing self-determination and security (low directionality). Palestinian Arabs are the primary victims, experiencing displacement and loss (high directionality). Arab states are also targets, bearing political and military costs. International supporters of Israel are beneficiaries, aligning with the narrative. International human rights organizations act as observers, analyzing the constraint's impact.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    indigeneity_claim_ambiguity,
    'Is the claim of indigenous return for Jewish people compatible with the indigenous status of Palestinian Arabs, or does it inherently displace it?',
    'Historical and anthropological analysis of pre-Zionist demographics and land use, combined with legal frameworks for indigenous rights that address overlapping claims.',
    'If incompatible, the ''national_liberation_reading'' would be reclassified as inherently extractive, as it necessitates the displacement of another indigenous population. If compatible, it would support a framework for shared sovereignty.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(indigeneity_claim_ambiguity, conceptual, 'Ambiguity of indigenous claims in a contested territory.').

omega_variable(
    historical_connection_vs_contemporary_rights,
    'To what extent does historical connection to a land (2000+ years ago) justify contemporary displacement of its current inhabitants?',
    'International legal precedent on self-determination and indigenous rights, particularly cases involving historical claims versus established populations.',
    'If historical claims are deemed insufficient to override contemporary rights, the extractiveness of the constraint would be amplified, and its legitimacy as national liberation diminished. If sufficient, it would reinforce the current framing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(historical_connection_vs_contemporary_rights, preference, 'Balancing historical claims against contemporary human rights.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (external barriers) or internalized (cognitive patterns that persist after barrier removal) for Palestinian Arabs?',
    'Post-exit suppression trajectory: if suppression persists after the extractive mechanism is removed (e.g., through psychological impacts of trauma or identity-lock), reclassify as partially internalized.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the target carries the suppression with them after exit, making the constraint more deeply entrenched.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for Palestinian Arabs.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(zionist_legitimacy_basis__national_liberation_reading, 1948, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(zion_tr_t1948, zionist_legitimacy_basis__national_liberation_reading, theater_ratio, 1948, 0.1).
narrative_ontology:measurement(zion_tr_t1967, zionist_legitimacy_basis__national_liberation_reading, theater_ratio, 1967, 0.15).
narrative_ontology:measurement(zion_tr_t1993, zionist_legitimacy_basis__national_liberation_reading, theater_ratio, 1993, 0.2).
narrative_ontology:measurement(zion_tr_t2000, zionist_legitimacy_basis__national_liberation_reading, theater_ratio, 2000, 0.25).
narrative_ontology:measurement(zion_tr_t2024, zionist_legitimacy_basis__national_liberation_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(zion_be_t1948, zionist_legitimacy_basis__national_liberation_reading, base_extractiveness, 1948, 0.6).
narrative_ontology:measurement(zion_be_t1967, zionist_legitimacy_basis__national_liberation_reading, base_extractiveness, 1967, 0.7).
narrative_ontology:measurement(zion_be_t1993, zionist_legitimacy_basis__national_liberation_reading, base_extractiveness, 1993, 0.65).
narrative_ontology:measurement(zion_be_t2000, zionist_legitimacy_basis__national_liberation_reading, base_extractiveness, 2000, 0.75).
narrative_ontology:measurement(zion_be_t2024, zionist_legitimacy_basis__national_liberation_reading, base_extractiveness, 2024, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(zion_su_t1948, zionist_legitimacy_basis__national_liberation_reading, suppression_requirement, 1948, 0.7).
narrative_ontology:measurement(zion_su_t1967, zionist_legitimacy_basis__national_liberation_reading, suppression_requirement, 1967, 0.85).
narrative_ontology:measurement(zion_su_t1993, zionist_legitimacy_basis__national_liberation_reading, suppression_requirement, 1993, 0.75).
narrative_ontology:measurement(zion_su_t2000, zionist_legitimacy_basis__national_liberation_reading, suppression_requirement, 2000, 0.88).
narrative_ontology:measurement(zion_su_t2024, zionist_legitimacy_basis__national_liberation_reading, suppression_requirement, 2024, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(zionist_legitimacy_basis__national_liberation_reading, identity_coordination).
narrative_ontology:affects_constraint(zionist_legitimacy_basis__national_liberation_reading, zionist_legitimacy_basis__settler_colonial_reading).
narrative_ontology:affects_constraint(zionist_legitimacy_basis__national_liberation_reading, zionist_legitimacy_basis__religious_restoration_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'zionist_legitimacy_basis' kernel. This 'national_liberation_reading' emphasizes Jewish historical connection and persecution, justifying the establishment of Israel. It is linked to the 'settler_colonial_reading' and 'religious_restoration_reading' as alternative interpretations of Zionism's legitimacy.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
