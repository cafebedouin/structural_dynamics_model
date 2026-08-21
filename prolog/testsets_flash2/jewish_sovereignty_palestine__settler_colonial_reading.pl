% ============================================================================
% CONSTRAINT STORY: jewish_sovereignty_palestine__settler_colonial_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jewish_sovereignty_palestine__settler_colonial_reading, []).

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
 *   constraint_id: jewish_sovereignty_palestine__settler_colonial_reading
 *   human_readable: Jewish Sovereignty in Palestine: Settler-Colonial Reading
 *   domain: political_philosophy/nationalism_studies/postcolonial_theory
 *
 * SUMMARY:
 *   This constraint story instantiates the 'settler-colonial reading' of
 *   Jewish sovereignty in Palestine. It posits that Zionism, regardless of
 *   its stated intentions or the historical context of Jewish persecution,
 *   functions as a settler-colonial project. Jewish immigration is viewed as
 *   constituting a displacement regime, leading to the structural
 *   dispossession of the indigenous Palestinian population. The constraint is
 *   classified as a Snare due to its high extractiveness and suppression,
 *   with identifiable victims and beneficiaries. This reading focuses on the
 *   material and structural outcomes of the Zionist project, rather than its
 *   ideological justifications.
 *
 * KEY AGENTS:
 *   - palestinian_people: Primary victims of structural dispossession (powerless/trapped)
 *   - jewish_immigrants: Beneficiaries of the settler-colonial structure (moderate/constrained)
 *   - israeli_state: Agenda-setter and enforcer of the regime (institutional/constrained)
 *   - global_imperial_powers: Beneficiaries of geopolitical interests (institutional/arbitrage)
 *   - international_solidarity_movements: Analytical observers and critics (organized/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_sovereignty_palestine__settler_colonial_reading, 0.95).
domain_priors:suppression_score(jewish_sovereignty_palestine__settler_colonial_reading, 0.9).
domain_priors:theater_ratio(jewish_sovereignty_palestine__settler_colonial_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__settler_colonial_reading, extractiveness, 0.95).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__settler_colonial_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__settler_colonial_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__settler_colonial_reading, accessibility_collapse, 0.85).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__settler_colonial_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_sovereignty_palestine__settler_colonial_reading, snare).
narrative_ontology:human_readable(jewish_sovereignty_palestine__settler_colonial_reading, "Jewish Sovereignty in Palestine: Settler-Colonial Reading").
narrative_ontology:topic_domain(jewish_sovereignty_palestine__settler_colonial_reading, "political_philosophy/nationalism_studies/postcolonial_theory").

domain_priors:requires_active_enforcement(jewish_sovereignty_palestine__settler_colonial_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_sovereignty_palestine__settler_colonial_reading, 'c8f2233b-14d2-4c09-91b6-beae8a1eb52c').
narrative_ontology:cs_kernel_codification('c8f2233b-14d2-4c09-91b6-beae8a1eb52c', formalized).
narrative_ontology:cs_authority_grounding('c8f2233b-14d2-4c09-91b6-beae8a1eb52c', extraction).
narrative_ontology:cs_interpretation_layer_present('c8f2233b-14d2-4c09-91b6-beae8a1eb52c').
narrative_ontology:cs_reading_relation('c8f2233b-14d2-4c09-91b6-beae8a1eb52c', jewish_sovereignty_palestine__liberal_nationalist_reading, forecloses).
narrative_ontology:cs_reading_relation('c8f2233b-14d2-4c09-91b6-beae8a1eb52c', jewish_sovereignty_palestine__religious_zionist_reading, forecloses).
narrative_ontology:cs_reading_relation('c8f2233b-14d2-4c09-91b6-beae8a1eb52c', jewish_sovereignty_palestine__cultural_zionist_reading, influences).
narrative_ontology:cs_reading_relation('c8f2233b-14d2-4c09-91b6-beae8a1eb52c', jewish_sovereignty_palestine__post_zionist_reading, coexists_with).
narrative_ontology:cs_axiom('c8f2233b-14d2-4c09-91b6-beae8a1eb52c', foundational, zionism_is_settler_colonialism).
narrative_ontology:cs_axiom_status(zionism_is_settler_colonialism, holdable).
narrative_ontology:cs_axiom_grounding('c8f2233b-14d2-4c09-91b6-beae8a1eb52c', zionism_is_settler_colonialism, empirically_contingent).
narrative_ontology:cs_axiom('c8f2233b-14d2-4c09-91b6-beae8a1eb52c', foundational, indigenous_dispossession_is_unjust).
narrative_ontology:cs_axiom_status(indigenous_dispossession_is_unjust, holdable).
narrative_ontology:cs_axiom_grounding('c8f2233b-14d2-4c09-91b6-beae8a1eb52c', indigenous_dispossession_is_unjust, deontological).
narrative_ontology:cs_reference_frame('c8f2233b-14d2-4c09-91b6-beae8a1eb52c', european_colonial_expansion).
narrative_ontology:cs_drift_state('c8f2233b-14d2-4c09-91b6-beae8a1eb52c', contemporary_postcolonial_critique, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('c8f2233b-14d2-4c09-91b6-beae8a1eb52c', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(jewish_sovereignty_palestine__settler_colonial_reading, jewish_sovereignty_palestine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_sovereignty_palestine__settler_colonial_reading, israeli_state).
narrative_ontology:constraint_beneficiary(jewish_sovereignty_palestine__settler_colonial_reading, global_imperial_powers).
narrative_ontology:constraint_victim(jewish_sovereignty_palestine__settler_colonial_reading, palestinian_people).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(jewish_sovereignty_palestine__settler_colonial_reading, jewish_immigrants).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Experience ongoing dispossession of land, resources, and self-determination. Their existence is criminalized, and their resistance met with overwhelming force. Exit means abandoning their homeland and identity, which is structurally foreclosed.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__settler_colonial_reading, palestinian_people, payer,
    powerless, generational, trapped, regional).

% Are positioned as settlers, regardless of their individual intent or refugee status, benefiting from the structural dispossession of Palestinians through access to land, housing, and state resources. Their presence reinforces the colonial structure.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__settler_colonial_reading, jewish_immigrants, beneficiary,
    moderate, biographical, constrained, local).

% Administers and enforces the settler-colonial regime, consolidating control over land and resources, and maintaining a demographic majority through policies of immigration and exclusion. Benefits directly from the extraction and suppression.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__settler_colonial_reading, israeli_state, agenda_setter,
    institutional, generational, constrained, national).

% Benefit from the geopolitical stability and strategic interests served by the Israeli state's role in the region, providing diplomatic, military, and economic support that underwrites the settler-colonial project. This includes historical British imperial interests and contemporary US imperial interests.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__settler_colonial_reading, global_imperial_powers, beneficiary,
    institutional, civilizational, arbitrage, global).

% Analyze and critique the settler-colonial nature of Zionism, advocating for Palestinian rights and decolonization. They operate outside the direct power structures but influence global discourse and apply pressure through boycotts and advocacy.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__settler_colonial_reading, international_solidarity_movements, observer,
    organized, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: From this reading's perspective, the constraint's primary function is not coordination but the systematic organization of territorial acquisition and demographic engineering to establish and maintain a Jewish-majority state, which requires coordinating the displacement of the indigenous population.
% TRANSFER_FUNCTION: Transfers land, resources, and sovereignty from the Palestinian people to the Israeli state and Jewish immigrants, facilitated by the support of global imperial powers.
% ABSENT_VOICES: The indigenous Palestinian population, whose historical claims and rights are systematically denied and suppressed within the framework of the Israeli state. Their voices are excluded from the foundational narratives and decision-making processes that shape the constraint.
% DISAPPEARANCE_RATIONALE: If the settler-colonial framework vanished overnight, the entire geopolitical structure of the region would fundamentally rearrange. Palestinians would reclaim land and sovereignty, the Israeli state's legitimacy would collapse, and global power dynamics would shift dramatically.
% FOUNDING_PROBLEM: The perceived problem of Jewish statelessness and vulnerability in Europe, leading to the aspiration for a sovereign Jewish homeland.
% FOUNDING_PROBLEM_CORROBORATION: While proponents of Zionism attest to the ongoing problem of Jewish vulnerability, this reading argues that the 'solution' to Jewish statelessness was achieved through the creation of a new problem of Palestinian dispossession. Postcolonial scholars and Palestinian historians corroborate that the founding problem's 'solution' created a new, ongoing injustice, making its status contested.
narrative_ontology:disappearance_verdict(jewish_sovereignty_palestine__settler_colonial_reading, world_rearranges).
narrative_ontology:founding_problem_status(jewish_sovereignty_palestine__settler_colonial_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_sovereignty_palestine__settler_colonial_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(jewish_sovereignty_palestine__settler_colonial_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jewish_sovereignty_palestine__settler_colonial_reading, 0.95, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jewish_sovereignty_palestine__settler_colonial_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jewish_sovereignty_palestine__settler_colonial_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jewish_sovereignty_palestine__settler_colonial_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very high (0.95) because the constraint fundamentally involves the transfer of land, resources, and self-determination from one group to another, operating on a zero-sum territorial logic. Suppression is also very high (0.90) as the persistence of the regime relies on active military, legal, and administrative enforcement to contain and dispossess the indigenous population, and to suppress their resistance. Theater ratio is low (0.10) because the primary function is direct, active extraction and control, with minimal performative cover; the 'security' narrative is seen as a direct justification for suppression rather than a theatrical diversion. Accessibility collapse is high (0.85) as alternatives for Palestinians (e.g., self-determination, return) are systematically foreclosed by the state's actions. Resistance is high (0.75) reflecting the ongoing, active struggle of the Palestinian people against the regime.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the Palestinian people, this constraint is a clear Snare, characterized by extreme extraction and suppression. For Jewish immigrants, while they benefit structurally, their individual experience may be framed as seeking refuge or building a homeland, leading to a different perceived classification. The Israeli state and global imperial powers perceive it as a legitimate exercise of national sovereignty or a strategic alliance, respectively, masking the extractive and suppressive elements.
 *
 * DIRECTIONALITY LOGIC:
 *   The Palestinian people are the full targets (d=1.0) as they bear the brunt of dispossession and violence. Jewish immigrants are beneficiaries (d=0.0-0.2) as they gain access to land and resources within the colonial structure. The Israeli state is a primary beneficiary and agenda-setter (d=0.0) as it directly controls and profits from the system. Global imperial powers are also beneficiaries (d=0.0-0.1) due to strategic interests. The directionality for Jewish immigrants is modulated by their individual agency and awareness of the structural context, but the structural position remains one of benefit within the colonial framework.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading argues that the original mandate of Zionism (solving Jewish statelessness) has been superseded by its function as a settler-colonial project. The constraint persists not because its founding problem is still genuinely live in a way that justifies the current structure, but because of the ongoing benefits it provides to the Israeli state and its imperial backers, and the active suppression of alternatives. The classification as a Snare prevents mislabeling this as a coordination mechanism or a legitimate national project, highlighting its extractive and coercive nature.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    intent_vs_outcome_ambiguity,
    'Does the historical intent of early Zionist movements (e.g., refuge from persecution) mitigate the settler-colonial outcome, or is the outcome determinative regardless of intent?',
    'Analysis of historical documents and contemporary policies: if policies consistently prioritize demographic engineering and territorial control over equitable coexistence, outcome is determinative.',
    'If intent mitigates, the extractiveness might be re-evaluated as a tragic byproduct rather than a core function, potentially shifting classification towards a Tangled Rope. If outcome is determinative, the Snare classification is reinforced.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(intent_vs_outcome_ambiguity, conceptual, 'Ambiguity regarding the role of historical intent versus observed structural outcomes in defining the constraint.').

omega_variable(
    metropole_identification_ambiguity,
    'Is the primary ''metropole'' benefiting from this settler-colonial project still Britain (historical), or has it shifted to the United States (contemporary), or is it a diffuse global imperial interest?',
    'Analysis of diplomatic, military, and economic aid flows, as well as UN voting patterns and geopolitical alignments over time.',
    'Identifying a clear, singular metropole would sharpen the analysis of external beneficiaries and their leverage. A diffuse metropole would suggest a more self-sustaining, albeit still colonial, structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(metropole_identification_ambiguity, empirical, 'Uncertainty regarding the primary external imperial beneficiary of the settler-colonial project.').

omega_variable(
    internalized_suppression_palestinians,
    'To what extent is the suppression experienced by Palestinians structural (external barriers) versus internalized (psychological impacts of prolonged occupation and dispossession)?',
    'Post-liberation psychological and sociological studies: if patterns of self-censorship or learned helplessness persist after structural barriers are removed, internalized suppression is significant.',
    'If internalized suppression is substantial, the effective suppression is higher than the structural measure suggests, as the victims carry the suppression with them, making exit even harder.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalized_suppression_palestinians, empirical, 'Structural vs. internalized suppression mechanism for the Palestinian people.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_sovereignty_palestine__settler_colonial_reading, 1917, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jewi_tr_t1917, jewish_sovereignty_palestine__settler_colonial_reading, theater_ratio, 1917, 0.2).
narrative_ontology:measurement(jewi_tr_t1948, jewish_sovereignty_palestine__settler_colonial_reading, theater_ratio, 1948, 0.15).
narrative_ontology:measurement(jewi_tr_t1967, jewish_sovereignty_palestine__settler_colonial_reading, theater_ratio, 1967, 0.12).
narrative_ontology:measurement(jewi_tr_t2000, jewish_sovereignty_palestine__settler_colonial_reading, theater_ratio, 2000, 0.1).
narrative_ontology:measurement(jewi_tr_t2024, jewish_sovereignty_palestine__settler_colonial_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(jewi_be_t1917, jewish_sovereignty_palestine__settler_colonial_reading, base_extractiveness, 1917, 0.7).
narrative_ontology:measurement(jewi_be_t1948, jewish_sovereignty_palestine__settler_colonial_reading, base_extractiveness, 1948, 0.85).
narrative_ontology:measurement(jewi_be_t1967, jewish_sovereignty_palestine__settler_colonial_reading, base_extractiveness, 1967, 0.9).
narrative_ontology:measurement(jewi_be_t2000, jewish_sovereignty_palestine__settler_colonial_reading, base_extractiveness, 2000, 0.93).
narrative_ontology:measurement(jewi_be_t2024, jewish_sovereignty_palestine__settler_colonial_reading, base_extractiveness, 2024, 0.95).

% Suppression requirement over time
narrative_ontology:measurement(jewi_su_t1917, jewish_sovereignty_palestine__settler_colonial_reading, suppression_requirement, 1917, 0.6).
narrative_ontology:measurement(jewi_su_t1948, jewish_sovereignty_palestine__settler_colonial_reading, suppression_requirement, 1948, 0.8).
narrative_ontology:measurement(jewi_su_t1967, jewish_sovereignty_palestine__settler_colonial_reading, suppression_requirement, 1967, 0.85).
narrative_ontology:measurement(jewi_su_t2000, jewish_sovereignty_palestine__settler_colonial_reading, suppression_requirement, 2000, 0.88).
narrative_ontology:measurement(jewi_su_t2024, jewish_sovereignty_palestine__settler_colonial_reading, suppression_requirement, 2024, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
