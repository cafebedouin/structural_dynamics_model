% ============================================================================
% CONSTRAINT STORY: territorial_sovereignty_legitimacy__self_determination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_territorial_sovereignty_legitimacy__self_determination_reading, []).

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
 *   constraint_id: territorial_sovereignty_legitimacy__self_determination_reading
 *   human_readable: Territorial Sovereignty Legitimacy: Self-Determination Reading
 *   domain: political_theory/international_relations/territorial_sovereignty
 *
 * SUMMARY:
 *   This constraint represents the 'self-determination' reading of
 *   territorial sovereignty legitimacy in the Israeli-Palestinian conflict.
 *   It asserts that legitimacy derives from the modern principle of
 *   self-determination applied to the Arab population with demographic
 *   majority and continuous residence in the territory during the modern
 *   period (19th-20th centuries). This reading frames the Israeli state as a
 *   colonial project and supports the right of return as a restoration of the
 *   status quo ante. The constraint is classified as a Snare due to its high
 *   extractiveness and suppression of alternative narratives and claims.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(territorial_sovereignty_legitimacy__self_determination_reading, 0.85).
domain_priors:suppression_score(territorial_sovereignty_legitimacy__self_determination_reading, 0.92).
domain_priors:theater_ratio(territorial_sovereignty_legitimacy__self_determination_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__self_determination_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__self_determination_reading, suppression_requirement, 0.92).
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__self_determination_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__self_determination_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__self_determination_reading, resistance, 0.88).

% --- Constraint claim ---
narrative_ontology:constraint_claim(territorial_sovereignty_legitimacy__self_determination_reading, snare).
narrative_ontology:human_readable(territorial_sovereignty_legitimacy__self_determination_reading, "Territorial Sovereignty Legitimacy: Self-Determination Reading").
narrative_ontology:topic_domain(territorial_sovereignty_legitimacy__self_determination_reading, "political_theory/international_relations/territorial_sovereignty").

domain_priors:requires_active_enforcement(territorial_sovereignty_legitimacy__self_determination_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(territorial_sovereignty_legitimacy__self_determination_reading, 'a7d33acd-4953-4dc3-89fb-7e1266903fb9').
narrative_ontology:cs_kernel_codification('a7d33acd-4953-4dc3-89fb-7e1266903fb9', distributed).
narrative_ontology:cs_authority_grounding('a7d33acd-4953-4dc3-89fb-7e1266903fb9', extraction).
narrative_ontology:cs_interpretation_layer_present('a7d33acd-4953-4dc3-89fb-7e1266903fb9').
narrative_ontology:cs_reading_relation('a7d33acd-4953-4dc3-89fb-7e1266903fb9', territorial_sovereignty_legitimacy__covenant_continuity_reading, forecloses).
narrative_ontology:cs_reading_relation('a7d33acd-4953-4dc3-89fb-7e1266903fb9', territorial_sovereignty_legitimacy__existential_matrix_reading, coexists_with).
narrative_ontology:cs_axiom('a7d33acd-4953-4dc3-89fb-7e1266903fb9', foundational, modern_self_determination_is_primary).
narrative_ontology:cs_axiom_status(modern_self_determination_is_primary, holdable).
narrative_ontology:cs_axiom_grounding('a7d33acd-4953-4dc3-89fb-7e1266903fb9', modern_self_determination_is_primary, deontological).
narrative_ontology:cs_axiom('a7d33acd-4953-4dc3-89fb-7e1266903fb9', foundational, continuous_modern_demographic_presence_confers_legitimacy).
narrative_ontology:cs_axiom_status(continuous_modern_demographic_presence_confers_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('a7d33acd-4953-4dc3-89fb-7e1266903fb9', continuous_modern_demographic_presence_confers_legitimacy, empirically_contingent).
narrative_ontology:cs_reference_frame('a7d33acd-4953-4dc3-89fb-7e1266903fb9', post_colonial_self_determination_framework).
narrative_ontology:cs_drift_state('a7d33acd-4953-4dc3-89fb-7e1266903fb9', contemporary_geopolitical_context, gap(stable, minor, true)).
narrative_ontology:cs_created_at('a7d33acd-4953-4dc3-89fb-7e1266903fb9', '').
narrative_ontology:cs_kernel_id(territorial_sovereignty_legitimacy__self_determination_reading, territorial_sovereignty_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(territorial_sovereignty_legitimacy__self_determination_reading, palestinian_national_movement).
narrative_ontology:constraint_beneficiary(territorial_sovereignty_legitimacy__self_determination_reading, arab_states).
narrative_ontology:constraint_victim(territorial_sovereignty_legitimacy__self_determination_reading, israeli_state).
narrative_ontology:constraint_victim(territorial_sovereignty_legitimacy__self_determination_reading, jewish_population_in_israel).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Advocates for the right of self-determination for the Arab population, viewing the Israeli state as a colonial imposition. This reading forms the core of their political and diplomatic strategy, demanding a return to pre-1948 status quo or a one-state solution.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__self_determination_reading, palestinian_national_movement, agenda_setter,
    organized, generational, identity_locked, regional).

% Support the self-determination reading as a matter of pan-Arab solidarity and international law, using it to legitimize their diplomatic pressure against Israel. They benefit from the moral high ground and regional influence derived from this stance.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__self_determination_reading, arab_states, beneficiary,
    institutional, generational, constrained, regional).

% Is fundamentally challenged by this reading, which denies its legitimacy and frames its existence as a violation of international law. It bears the cost of continuous diplomatic and military pressure, and the delegitimization of its founding narrative.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__self_determination_reading, israeli_state, payer,
    institutional, generational, trapped, national).

% Their right to self-determination and continuous presence is implicitly denied or subordinated by this reading, which views them as an occupying force. They bear the psychological and security costs of a contested existence and the threat of displacement.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__self_determination_reading, jewish_population_in_israel, payer,
    powerful, biographical, identity_locked, national).

% Often align with aspects of the self-determination reading, particularly regarding the rights of indigenous populations and the principle of non-discrimination. They analyze the conflict through the lens of human rights and international law, often criticizing Israeli policies.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__self_determination_reading, international_human_rights_organizations, observer,
    organized, biographical, analytical, global).

% Navigate the tension between supporting the principle of self-determination and maintaining alliances with Israel. Their policies often reflect a compromise, leading to diplomatic ambiguity and inconsistent application of international law.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__self_determination_reading, western_governments, observer,
    institutional, generational, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the political and diplomatic efforts of the Palestinian national movement and its allies by providing a coherent, internationally recognized framework for their claims to sovereignty and self-determination.
% TRANSFER_FUNCTION: Transfers legitimacy and moral authority to the Arab population's claim over the territory, while simultaneously delegitimizing the Israeli state's claim, thereby shifting diplomatic and political capital.
% ABSENT_VOICES: The historical Jewish presence in the land prior to the modern period, and the Jewish right to self-determination, are largely absent or marginalized in this reading's framing, as they would challenge the temporal and demographic premises.
% DISAPPEARANCE_RATIONALE: If this reading vanished, the Palestinian national movement would lose a foundational pillar of its legitimacy and international support, forcing a radical re-evaluation of its political strategy. The diplomatic landscape of the conflict would fundamentally shift, potentially opening new avenues for resolution or exacerbating other forms of contestation.
% FOUNDING_PROBLEM: The perceived injustice of colonial partition and the denial of self-determination to the indigenous Arab population in the wake of the collapse of the Ottoman Empire and the British Mandate.
% FOUNDING_PROBLEM_CORROBORATION: The Palestinian national movement and many Arab states attest that the problem is live, citing ongoing occupation and denial of statehood. International human rights organizations and UN resolutions also corroborate the continued relevance of self-determination for the Palestinian people, from outside the directly benefiting parties.
narrative_ontology:disappearance_verdict(territorial_sovereignty_legitimacy__self_determination_reading, world_rearranges).
narrative_ontology:founding_problem_status(territorial_sovereignty_legitimacy__self_determination_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(territorial_sovereignty_legitimacy__self_determination_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(territorial_sovereignty_legitimacy__self_determination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(territorial_sovereignty_legitimacy__self_determination_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(territorial_sovereignty_legitimacy__self_determination_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(territorial_sovereignty_legitimacy__self_determination_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(territorial_sovereignty_legitimacy__self_determination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.85) is high because this reading fundamentally denies the legitimacy of the Israeli state and its Jewish population's claims to the land, imposing significant political, diplomatic, and existential costs. Suppression (0.92) is also very high, as this reading actively works to exclude and delegitimize counter-narratives, particularly those based on ancient Jewish ties or existential necessity. The theater ratio (0.4) reflects that while there is genuine advocacy for self-determination, a significant portion of the discourse serves to maintain a specific political framing that benefits certain actors. Accessibility collapse (0.7) is high because this reading, when adopted, significantly narrows the range of acceptable solutions, making alternatives like a two-state solution or shared sovereignty difficult to envision or accept. Resistance (0.88) is very high, as the Israeli state and its supporters actively contest this reading through diplomatic, military, and public relations efforts.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the Palestinian national movement, this reading is a just application of international law and a path to liberation. From the perspective of the Israeli state, it is an existential threat that denies their history and right to exist. The engine's classification as a Snare reflects the structural impact of this reading on those it targets, regardless of the beneficiaries' internal justification.
 *
 * DIRECTIONALITY LOGIC:
 *   The Palestinian national movement and allied Arab states are beneficiaries (d near 0.0) as this reading provides a strong, internationally recognized basis for their claims. The Israeli state and the Jewish population in Israel are victims (d near 1.0) as their legitimacy and existence are directly challenged. International human rights organizations and Western governments act as observers, with varying degrees of alignment and constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is not subject to mandatrophy in the traditional sense, as the 'founding problem' (colonial injustice, denial of self-determination) is considered 'live' by its proponents. The classification as a Snare prevents mislabeling it as a legitimate coordination mechanism, highlighting its extractive and suppressive nature from the perspective of those it targets, even if it coordinates the efforts of its beneficiaries.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    temporal_scope_legitimacy,
    'Is the ''modern period'' (19th-20th centuries) the appropriate and exclusive temporal scope for determining sovereignty legitimacy, or should ancient historical ties also be considered?',
    'International legal consensus on the weight of historical claims versus modern self-determination principles, or a negotiated political settlement that integrates multiple historical narratives.',
    'If ancient historical ties are given equal weight, this reading''s claim to exclusive legitimacy would weaken, potentially shifting its classification towards a Tangled Rope or even a Rope if a more inclusive framework emerges. If the modern period remains exclusive, the Snare classification is reinforced.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(temporal_scope_legitimacy, conceptual, 'Ambiguity regarding the relevant historical period for sovereignty claims.').

omega_variable(
    demographic_majority_definition,
    'How is ''demographic majority'' defined and measured in a contested territory, especially considering population movements and displacements?',
    'Independent, internationally supervised demographic studies and a clear legal definition of ''continuous residence'' that accounts for historical displacement.',
    'A redefinition or more precise measurement could alter the perceived strength of the Arab population''s claim to demographic majority, potentially weakening the constraint''s persuasive power and reducing its extractiveness if the factual basis is challenged.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(demographic_majority_definition, empirical, 'Ambiguity in defining and measuring demographic majority in a contested territory.').

omega_variable(
    colonial_project_framing,
    'Is the framing of the Israeli state as a ''colonial project'' an accurate historical and political assessment, or does it obscure other dimensions of the conflict, such as indigenous self-determination for Jewish people?',
    'Comprehensive historical scholarship that integrates multiple perspectives, and a shift in international discourse towards recognizing the complexity of the conflict beyond a simple colonizer/colonized binary.',
    'If the colonial framing is challenged or nuanced, the moral and political force of this reading would diminish, potentially reducing its suppressive power and opening space for alternative narratives that acknowledge Jewish indigeneity and self-determination, leading to a less extractive classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(colonial_project_framing, conceptual, 'Contestation over the ''colonial project'' framing of the Israeli state.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(territorial_sovereignty_legitimacy__self_determination_reading, 1948, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(terr_tr_t1948, territorial_sovereignty_legitimacy__self_determination_reading, theater_ratio, 1948, 0.2).
narrative_ontology:measurement(terr_tr_t1967, territorial_sovereignty_legitimacy__self_determination_reading, theater_ratio, 1967, 0.25).
narrative_ontology:measurement(terr_tr_t1987, territorial_sovereignty_legitimacy__self_determination_reading, theater_ratio, 1987, 0.3).
narrative_ontology:measurement(terr_tr_t2000, territorial_sovereignty_legitimacy__self_determination_reading, theater_ratio, 2000, 0.35).
narrative_ontology:measurement(terr_tr_t2014, territorial_sovereignty_legitimacy__self_determination_reading, theater_ratio, 2014, 0.38).
narrative_ontology:measurement(terr_tr_t2024, territorial_sovereignty_legitimacy__self_determination_reading, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(terr_be_t1948, territorial_sovereignty_legitimacy__self_determination_reading, base_extractiveness, 1948, 0.7).
narrative_ontology:measurement(terr_be_t1967, territorial_sovereignty_legitimacy__self_determination_reading, base_extractiveness, 1967, 0.78).
narrative_ontology:measurement(terr_be_t1987, territorial_sovereignty_legitimacy__self_determination_reading, base_extractiveness, 1987, 0.82).
narrative_ontology:measurement(terr_be_t2000, territorial_sovereignty_legitimacy__self_determination_reading, base_extractiveness, 2000, 0.85).
narrative_ontology:measurement(terr_be_t2014, territorial_sovereignty_legitimacy__self_determination_reading, base_extractiveness, 2014, 0.86).
narrative_ontology:measurement(terr_be_t2024, territorial_sovereignty_legitimacy__self_determination_reading, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(terr_su_t1948, territorial_sovereignty_legitimacy__self_determination_reading, suppression_requirement, 1948, 0.8).
narrative_ontology:measurement(terr_su_t1967, territorial_sovereignty_legitimacy__self_determination_reading, suppression_requirement, 1967, 0.85).
narrative_ontology:measurement(terr_su_t1987, territorial_sovereignty_legitimacy__self_determination_reading, suppression_requirement, 1987, 0.9).
narrative_ontology:measurement(terr_su_t2000, territorial_sovereignty_legitimacy__self_determination_reading, suppression_requirement, 2000, 0.93).
narrative_ontology:measurement(terr_su_t2014, territorial_sovereignty_legitimacy__self_determination_reading, suppression_requirement, 2014, 0.94).
narrative_ontology:measurement(terr_su_t2024, territorial_sovereignty_legitimacy__self_determination_reading, suppression_requirement, 2024, 0.92).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
