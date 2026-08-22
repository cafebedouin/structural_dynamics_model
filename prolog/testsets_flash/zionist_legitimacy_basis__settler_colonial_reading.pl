% ============================================================================
% CONSTRAINT STORY: zionist_legitimacy_basis__settler_colonial_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_zionist_legitimacy_basis__settler_colonial_reading, []).

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
 *   constraint_id: zionist_legitimacy_basis__settler_colonial_reading
 *   human_readable: Zionist Legitimacy Basis (Settler-Colonial Reading)
 *   domain: political_history/nationalism/settler_colonialism
 *
 * SUMMARY:
 *   This constraint story analyzes Zionism from a settler-colonial
 *   perspective, viewing it as a movement that established an ethno-state
 *   through the systematic displacement of indigenous Palestinians. This
 *   reading emphasizes the structural violence inherent in the process, the
 *   ongoing dispossession, and the mechanisms of control and suppression
 *   required to maintain the state's ethno-national character. The high
 *   extractiveness and suppression reflect the continuous nature of this
 *   process since 1948. This is one reading of the 'Zionist Legitimacy Basis'
 *   kernel.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(zionist_legitimacy_basis__settler_colonial_reading, 0.92).
domain_priors:suppression_score(zionist_legitimacy_basis__settler_colonial_reading, 0.95).
domain_priors:theater_ratio(zionist_legitimacy_basis__settler_colonial_reading, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(zionist_legitimacy_basis__settler_colonial_reading, extractiveness, 0.92).
narrative_ontology:constraint_metric(zionist_legitimacy_basis__settler_colonial_reading, suppression_requirement, 0.95).
narrative_ontology:constraint_metric(zionist_legitimacy_basis__settler_colonial_reading, theater_ratio, 0.65).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(zionist_legitimacy_basis__settler_colonial_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(zionist_legitimacy_basis__settler_colonial_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(zionist_legitimacy_basis__settler_colonial_reading, snare).
narrative_ontology:human_readable(zionist_legitimacy_basis__settler_colonial_reading, "Zionist Legitimacy Basis (Settler-Colonial Reading)").
narrative_ontology:topic_domain(zionist_legitimacy_basis__settler_colonial_reading, "political_history/nationalism/settler_colonialism").

domain_priors:requires_active_enforcement(zionist_legitimacy_basis__settler_colonial_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(zionist_legitimacy_basis__settler_colonial_reading, 'eddd7153-665c-42d5-9b89-d5e195bc3909').
narrative_ontology:cs_kernel_codification('eddd7153-665c-42d5-9b89-d5e195bc3909', formalized).
narrative_ontology:cs_authority_grounding('eddd7153-665c-42d5-9b89-d5e195bc3909', extraction).
narrative_ontology:cs_interpretation_layer_present('eddd7153-665c-42d5-9b89-d5e195bc3909').
narrative_ontology:cs_reading_relation('eddd7153-665c-42d5-9b89-d5e195bc3909', zionist_legitimacy_basis__national_liberation_reading, forecloses).
narrative_ontology:cs_reading_relation('eddd7153-665c-42d5-9b89-d5e195bc3909', zionist_legitimacy_basis__religious_restoration_reading, coexists_with).
narrative_ontology:cs_axiom('eddd7153-665c-42d5-9b89-d5e195bc3909', foundational, settler_colonialism_is_inherently_unjust).
narrative_ontology:cs_axiom_status(settler_colonialism_is_inherently_unjust, holdable).
narrative_ontology:cs_axiom_grounding('eddd7153-665c-42d5-9b89-d5e195bc3909', settler_colonialism_is_inherently_unjust, deontological).
narrative_ontology:cs_axiom('eddd7153-665c-42d5-9b89-d5e195bc3909', foundational, indigenous_displacement_is_constitutive).
narrative_ontology:cs_axiom_status(indigenous_displacement_is_constitutive, holdable).
narrative_ontology:cs_axiom_grounding('eddd7153-665c-42d5-9b89-d5e195bc3909', indigenous_displacement_is_constitutive, empirically_contingent).
narrative_ontology:cs_reference_frame('eddd7153-665c-42d5-9b89-d5e195bc3909', anti_colonial_liberation_framework).
narrative_ontology:cs_drift_state('eddd7153-665c-42d5-9b89-d5e195bc3909', contemporary_international_law_discourse, gap(revival_pressure, substantial, false)).
narrative_ontology:cs_created_at('eddd7153-665c-42d5-9b89-d5e195bc3909', '').
narrative_ontology:cs_kernel_id(zionist_legitimacy_basis__settler_colonial_reading, zionist_legitimacy_basis).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(zionist_legitimacy_basis__settler_colonial_reading, israeli_state_institutions).
narrative_ontology:constraint_beneficiary(zionist_legitimacy_basis__settler_colonial_reading, jewish_israeli_citizens).
narrative_ontology:constraint_victim(zionist_legitimacy_basis__settler_colonial_reading, indigenous_palestinians).
narrative_ontology:constraint_victim(zionist_legitimacy_basis__settler_colonial_reading, palestinian_diaspora).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers and enforces policies that maintain the ethno-national character of the state, including land laws, citizenship regulations, and security doctrines. Benefits directly from the control of territory and resources, and the demographic majority achieved through displacement.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__settler_colonial_reading, israeli_state_institutions, agenda_setter,
    institutional, generational, constrained, national).

% Benefit from preferential access to land, housing, and state resources, as well as security provisions and national identity tied to the state's existence. Their self-conception and material well-being are deeply intertwined with the settler-colonial project.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__settler_colonial_reading, jewish_israeli_citizens, beneficiary,
    organized, biographical, constrained, national).

% Bear the direct costs of displacement, land confiscation, and denial of self-determination. They experience ongoing military occupation, administrative control, and legal discrimination, with limited to no avenues for redress or political participation within the existing state structure.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__settler_colonial_reading, indigenous_palestinians, payer,
    powerless, generational, trapped, local).

% Denied the right of return to their ancestral lands, they maintain a collective identity and political aspiration tied to the land from which they were expelled. Their costs are the loss of homeland, cultural continuity, and the ongoing struggle for recognition and return.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__settler_colonial_reading, palestinian_diaspora, payer,
    powerless, generational, identity_locked, global).

% Document and condemn human rights abuses, land confiscations, and discriminatory policies. They advocate for international law and Palestinian rights, but lack direct enforcement power over the Israeli state.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__settler_colonial_reading, international_human_rights_organizations, observer,
    organized, biographical, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the establishment and maintenance of a Jewish ethno-state in historic Palestine, providing a secure national home for Jewish people through the acquisition and control of land and resources.
% TRANSFER_FUNCTION: Transfers land, resources, and political sovereignty from indigenous Palestinians to Jewish settlers and the Israeli state, along with the associated benefits of national self-determination and security for the latter.
% ABSENT_VOICES: The voices of indigenous Palestinians, particularly those dispossessed and exiled, are systematically marginalized or suppressed within the dominant narratives of the state's founding. Their historical accounts and claims to land are actively denied or reframed.
% DISAPPEARANCE_RATIONALE: If the settler-colonial basis of Zionism vanished overnight, the entire structure of the Israeli state, its land laws, citizenship regime, and demographic policies would be fundamentally challenged. The relationship between Jewish Israelis and Palestinians would be radically reconfigured, leading to a profound rearrangement of political, social, and economic life in the region.
% FOUNDING_PROBLEM: To establish a secure national homeland for Jewish people in historic Palestine, addressing centuries of antisemitism and persecution in Europe.
% FOUNDING_PROBLEM_CORROBORATION: The Israeli state and its supporters attest the problem is live, citing ongoing security threats and the historical necessity of a Jewish state. Indigenous Palestinians and many international observers attest that while the problem of antisemitism is real, its 'solution' through settler-colonialism created a new, ongoing problem of dispossession, and the founding problem's status is therefore contested by its victims.
narrative_ontology:disappearance_verdict(zionist_legitimacy_basis__settler_colonial_reading, world_rearranges).
narrative_ontology:founding_problem_status(zionist_legitimacy_basis__settler_colonial_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(zionist_legitimacy_basis__settler_colonial_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_gemini+stakeholder_backfill', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(zionist_legitimacy_basis__settler_colonial_reading, 'none', 1).
narrative_ontology:epsilon_provenance(zionist_legitimacy_basis__settler_colonial_reading, 0.92, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(zionist_legitimacy_basis__settler_colonial_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(zionist_legitimacy_basis__settler_colonial_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(zionist_legitimacy_basis__settler_colonial_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very high (0.92) because the very existence and expansion of the state, from this perspective, is predicated on the expropriation of land and resources from indigenous inhabitants. Suppression is also very high (0.95) due to the extensive military, legal, and administrative apparatus required to control and dispossess Palestinians, prevent their return, and suppress resistance. The theater ratio is high (0.65) as justifications for state actions often mask the underlying colonial logic with narratives of security or historical right, while the primary function remains territorial control and demographic engineering.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of Israeli state institutions and Jewish Israeli citizens, the constraint might be framed as a national liberation movement (a 'rope' or 'scaffold' for a persecuted people). However, from the perspective of indigenous Palestinians, it is a 'snare' of ongoing dispossession and oppression. The engine's classification will highlight this divergence based on the declared structural relationships and metrics.
 *
 * DIRECTIONALITY LOGIC:
 *   Israeli state institutions and Jewish Israeli citizens are beneficiaries, as they gain land, resources, and national identity from the constraint's operation. Indigenous Palestinians and the Palestinian diaspora are the primary payers, bearing the costs of displacement, loss of sovereignty, and denial of rights. Their exit options are severely limited, ranging from trapped (for those under occupation) to identity_locked (for the diaspora whose identity is tied to return).
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    historical_causality_of_displacement,
    'To what extent was indigenous displacement a constitutive, intentional goal of the Zionist project, versus an unintended consequence of national self-determination?',
    'Archival research into early Zionist planning documents, analysis of land acquisition policies, and comparative studies of other settler-colonial movements.',
    'If displacement is proven constitutive, it strengthens the ''snare'' classification and the settler-colonial reading''s claim to structural truth. If largely unintended, it might shift the classification towards a ''tangled_rope'' with severe, but not primary, extractive outcomes.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(historical_causality_of_displacement, empirical, 'Determining the intentionality and centrality of indigenous displacement in the Zionist project.').

omega_variable(
    legitimacy_of_ethno_national_states,
    'Is the concept of an ethno-national state inherently extractive when established in a territory with an existing indigenous population, or can such a state be legitimate under certain conditions?',
    'Conceptual analysis of international law, indigenous rights frameworks, and political philosophy concerning self-determination and state formation in contested territories.',
    'If ethno-national states are deemed inherently extractive in such contexts, it reinforces the ''snare'' classification. If conditional legitimacy is possible, it opens pathways for alternative readings to gain conceptual traction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_of_ethno_national_states, conceptual, 'The inherent legitimacy (or lack thereof) of ethno-national states in settler-colonial contexts.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (military occupation, legal discrimination) or internalized (psychological impact of prolonged conflict, normalization of occupation)?',
    'Post-occupation trajectory: if suppression persists after military and legal mechanisms are removed, reclassify as partially internalized. Analysis of Palestinian narratives and resistance movements.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the target carries the suppression with them after exit. This would deepen the ''snare'' classification by highlighting the pervasive nature of control.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in the context of occupation and dispossession.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(zionist_legitimacy_basis__settler_colonial_reading, 1948, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(zion_tr_t1948, zionist_legitimacy_basis__settler_colonial_reading, theater_ratio, 1948, 0.4).
narrative_ontology:measurement(zion_tr_t1967, zionist_legitimacy_basis__settler_colonial_reading, theater_ratio, 1967, 0.5).
narrative_ontology:measurement(zion_tr_t1987, zionist_legitimacy_basis__settler_colonial_reading, theater_ratio, 1987, 0.6).
narrative_ontology:measurement(zion_tr_t2000, zionist_legitimacy_basis__settler_colonial_reading, theater_ratio, 2000, 0.63).
narrative_ontology:measurement(zion_tr_t2014, zionist_legitimacy_basis__settler_colonial_reading, theater_ratio, 2014, 0.65).
narrative_ontology:measurement(zion_tr_t2024, zionist_legitimacy_basis__settler_colonial_reading, theater_ratio, 2024, 0.65).

% Extraction over time
narrative_ontology:measurement(zion_be_t1948, zionist_legitimacy_basis__settler_colonial_reading, base_extractiveness, 1948, 0.85).
narrative_ontology:measurement(zion_be_t1967, zionist_legitimacy_basis__settler_colonial_reading, base_extractiveness, 1967, 0.88).
narrative_ontology:measurement(zion_be_t1987, zionist_legitimacy_basis__settler_colonial_reading, base_extractiveness, 1987, 0.9).
narrative_ontology:measurement(zion_be_t2000, zionist_legitimacy_basis__settler_colonial_reading, base_extractiveness, 2000, 0.91).
narrative_ontology:measurement(zion_be_t2014, zionist_legitimacy_basis__settler_colonial_reading, base_extractiveness, 2014, 0.92).
narrative_ontology:measurement(zion_be_t2024, zionist_legitimacy_basis__settler_colonial_reading, base_extractiveness, 2024, 0.92).

% Suppression requirement over time
narrative_ontology:measurement(zion_su_t1948, zionist_legitimacy_basis__settler_colonial_reading, suppression_requirement, 1948, 0.8).
narrative_ontology:measurement(zion_su_t1967, zionist_legitimacy_basis__settler_colonial_reading, suppression_requirement, 1967, 0.88).
narrative_ontology:measurement(zion_su_t1987, zionist_legitimacy_basis__settler_colonial_reading, suppression_requirement, 1987, 0.92).
narrative_ontology:measurement(zion_su_t2000, zionist_legitimacy_basis__settler_colonial_reading, suppression_requirement, 2000, 0.94).
narrative_ontology:measurement(zion_su_t2014, zionist_legitimacy_basis__settler_colonial_reading, suppression_requirement, 2014, 0.95).
narrative_ontology:measurement(zion_su_t2024, zionist_legitimacy_basis__settler_colonial_reading, suppression_requirement, 2024, 0.95).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(zionist_legitimacy_basis__settler_colonial_reading, identity_coordination).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'zionist_legitimacy_basis' kernel, alongside 'national_liberation_reading' and 'religious_restoration_reading'.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
