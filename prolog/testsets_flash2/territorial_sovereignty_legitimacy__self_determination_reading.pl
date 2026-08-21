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
 *   constraint_id: territorial_sovereignty_legitimacy__self_determination_reading
 *   human_readable: Territorial Sovereignty Legitimacy: Self-Determination Reading
 *   domain: political_theory/international_relations/territorial_sovereignty
 *
 * SUMMARY:
 *   This constraint represents the 'self-determination' reading of
 *   territorial sovereignty legitimacy, focusing on the Arab population's
 *   demographic majority and continuous residence in the modern period
 *   (19th-20th centuries). It frames the Israeli state as a colonial project
 *   and asserts the right of return as a restoration of the status quo ante.
 *   The constraint is classified as a Snare due to its high extractiveness
 *   and suppression, as it fundamentally delegitimizes the existence of one
 *   party while empowering another, requiring active enforcement to maintain
 *   its narrative dominance.
 *
 * KEY AGENTS:
 *   - palestinian_national_movement: Primary beneficiary/agenda_setter (organized/identity_locked)
 *   - arab_states: Secondary beneficiary (institutional/constrained)
 *   - israeli_state: Primary target/victim (institutional/trapped)
 *   - jewish_population_in_israel: Secondary target/victim (powerful/identity_locked)
 *   - international_human_rights_organizations: Observer (organized/analytical)
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
narrative_ontology:cs_story_uid(territorial_sovereignty_legitimacy__self_determination_reading, 'f8bb151e-208f-44fc-90d5-1d1c217dc554').
narrative_ontology:cs_kernel_codification('f8bb151e-208f-44fc-90d5-1d1c217dc554', distributed).
narrative_ontology:cs_authority_grounding('f8bb151e-208f-44fc-90d5-1d1c217dc554', extraction).
narrative_ontology:cs_interpretation_layer_present('f8bb151e-208f-44fc-90d5-1d1c217dc554').
narrative_ontology:cs_reading_relation('f8bb151e-208f-44fc-90d5-1d1c217dc554', territorial_sovereignty_legitimacy__covenant_continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('f8bb151e-208f-44fc-90d5-1d1c217dc554', territorial_sovereignty_legitimacy__existential_matrix_reading, coexists_with).
narrative_ontology:cs_axiom('f8bb151e-208f-44fc-90d5-1d1c217dc554', foundational, self_determination_for_indigenous_majority_population).
narrative_ontology:cs_axiom_status(self_determination_for_indigenous_majority_population, holdable).
narrative_ontology:cs_axiom_grounding('f8bb151e-208f-44fc-90d5-1d1c217dc554', self_determination_for_indigenous_majority_population, deontological).
narrative_ontology:cs_axiom('f8bb151e-208f-44fc-90d5-1d1c217dc554', foundational, colonial_settler_states_lack_legitimacy).
narrative_ontology:cs_axiom_status(colonial_settler_states_lack_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('f8bb151e-208f-44fc-90d5-1d1c217dc554', colonial_settler_states_lack_legitimacy, deontological).
narrative_ontology:cs_reference_frame('f8bb151e-208f-44fc-90d5-1d1c217dc554', post_colonial_self_determination_framework).
narrative_ontology:cs_drift_state('f8bb151e-208f-44fc-90d5-1d1c217dc554', contemporary_international_relations, gap(stable, minor, true)).
narrative_ontology:cs_created_at('f8bb151e-208f-44fc-90d5-1d1c217dc554', '').
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

% Their right to self-determination and continuous presence is implicitly denied or subordinated by this reading, which focuses exclusively on the Arab population's claims. They bear the psychological and political cost of being framed as occupiers or colonizers in their homeland.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__self_determination_reading, jewish_population_in_israel, payer,
    powerful, biographical, identity_locked, national).

% Often align with aspects of the self-determination reading, particularly regarding the rights of indigenous populations and the principle of non-discrimination. They contribute to the international discourse that challenges the legitimacy of the Israeli occupation of Palestinian territories.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__self_determination_reading, international_human_rights_organizations, observer,
    organized, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a coherent legal and moral framework for the Palestinian national struggle, unifying various factions and garnering international support for their claims to statehood and the right of return.
% TRANSFER_FUNCTION: Transfers moral and political legitimacy from the Israeli state to the Palestinian national movement, and seeks to transfer territorial control and sovereignty from Israel to a Palestinian entity.
% ABSENT_VOICES: The historical Jewish presence in the land prior to the modern period, and the Jewish right to self-determination, are largely absent or dismissed in this reading, which focuses exclusively on the modern Arab demographic majority. Voices advocating for a two-state solution based on mutual recognition are also often marginalized by the maximalist implications of this reading.
% DISAPPEARANCE_RATIONALE: If this reading of sovereignty legitimacy vanished, the Palestinian national movement would lose its primary legal and moral grounding, forcing a fundamental re-evaluation of its goals and strategies. International discourse on the conflict would shift dramatically, potentially opening pathways for alternative resolutions not predicated on this specific historical and demographic framing.
% FOUNDING_PROBLEM: The dispossession and statelessness of the Palestinian Arab population following the 1948 Arab-Israeli War and the establishment of the State of Israel.
% FOUNDING_PROBLEM_CORROBORATION: The Palestinian national movement and many international bodies (e.g., UNRWA, various human rights organizations) attest that the problem of Palestinian dispossession and statelessness remains live. Independent historians and legal scholars corroborate the historical facts of displacement and the ongoing lack of a sovereign Palestinian state.
narrative_ontology:disappearance_verdict(territorial_sovereignty_legitimacy__self_determination_reading, world_rearranges).
narrative_ontology:founding_problem_status(territorial_sovereignty_legitimacy__self_determination_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(territorial_sovereignty_legitimacy__self_determination_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
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
 *   The extractiveness (0.85) is high because this reading, if fully implemented, would entail the dismantling of the Israeli state and the displacement of its Jewish population, representing a maximalist claim. Suppression (0.92) is also very high, as this reading actively suppresses alternative historical narratives and claims to legitimacy, particularly those of the Jewish people. The theater ratio (0.4) reflects that while there is genuine advocacy for self-determination, a significant portion of the discourse serves to maintain a specific, exclusionary narrative rather than seeking a mutually agreeable resolution. Accessibility collapse (0.7) is high because this reading leaves little room for alternative frameworks that would recognize both Palestinian and Jewish rights to self-determination in the same territory. Resistance (0.88) is very high, as the Israeli state and its supporters actively resist this delegitimizing narrative.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the Palestinian national movement and Arab states, this reading is a just application of international law and a necessary step towards decolonization. From the perspective of the Israeli state and its Jewish population, it is an existential threat that denies their history, presence, and right to self-determination, effectively functioning as a snare that seeks to dismantle their collective existence.
 *
 * DIRECTIONALITY LOGIC:
 *   The Palestinian National Movement and Arab States are beneficiaries, as this reading provides the moral and legal foundation for their claims and diplomatic efforts. The Israeli State and the Jewish population in Israel are victims, as their legitimacy and existence are directly challenged and undermined by this reading. International human rights organizations act as observers, often lending support to the self-determination narrative.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is not subject to mandatrophy in the traditional sense, as its founding problem (Palestinian dispossession) is considered 'live' by its proponents. However, the persistence of this maximalist reading, despite decades of conflict, suggests that its function has shifted from purely solving the founding problem to maintaining a specific political identity and narrative, which itself becomes a source of extraction and suppression against the opposing party. The classification as a Snare prevents mislabeling this as a coordination mechanism, highlighting its inherently extractive and suppressive nature from the perspective of its targets.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    historical_narrative_ambiguity,
    'Is the historical narrative presented by this reading (focus on modern Arab demographic majority, Israeli state as colonial project) the only valid interpretation of the region''s history, or are there equally valid alternative historical narratives?',
    'Comprehensive, multi-disciplinary historical and archaeological research, critically examining all primary sources and oral traditions, and acknowledging the limitations and biases of each narrative.',
    'If alternative narratives (e.g., continuous Jewish presence, indigenous claims) are found equally valid, the self-determination reading''s foundational claims would be weakened, potentially shifting its classification towards a more contested or even a tangled_rope if a coordination function for multiple narratives could be identified. If it remains the dominant narrative, its snare classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(historical_narrative_ambiguity, empirical, 'Ambiguity regarding the exclusivity and completeness of the historical narrative grounding this reading.').

omega_variable(
    self_determination_scope_ambiguity,
    'Does the principle of self-determination apply exclusively to the Arab population in this territory, or does it also apply to the Jewish population, and if so, how are these competing claims to be reconciled?',
    'International legal consensus or a negotiated political settlement that explicitly defines the scope and application of self-determination for all relevant populations in the territory.',
    'If self-determination is recognized as applying to both populations, this reading''s exclusionary nature would be challenged, potentially leading to a reclassification towards a tangled_rope (if a shared coordination problem is identified) or even a rope (if a mutually beneficial framework emerges). If it remains exclusive, the snare classification is reinforced.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(self_determination_scope_ambiguity, conceptual, 'Ambiguity regarding the universal vs. exclusive application of the self-determination principle.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(territorial_sovereignty_legitimacy__self_determination_reading, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(terr_tr_t0, territorial_sovereignty_legitimacy__self_determination_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(terr_tr_t15, territorial_sovereignty_legitimacy__self_determination_reading, theater_ratio, 15, 0.32).
narrative_ontology:measurement(terr_tr_t30, territorial_sovereignty_legitimacy__self_determination_reading, theater_ratio, 30, 0.35).
narrative_ontology:measurement(terr_tr_t45, territorial_sovereignty_legitimacy__self_determination_reading, theater_ratio, 45, 0.38).
narrative_ontology:measurement(terr_tr_t60, territorial_sovereignty_legitimacy__self_determination_reading, theater_ratio, 60, 0.39).
narrative_ontology:measurement(terr_tr_t75, territorial_sovereignty_legitimacy__self_determination_reading, theater_ratio, 75, 0.4).

% Extraction over time
narrative_ontology:measurement(terr_be_t0, territorial_sovereignty_legitimacy__self_determination_reading, base_extractiveness, 0, 0.7).
narrative_ontology:measurement(terr_be_t15, territorial_sovereignty_legitimacy__self_determination_reading, base_extractiveness, 15, 0.75).
narrative_ontology:measurement(terr_be_t30, territorial_sovereignty_legitimacy__self_determination_reading, base_extractiveness, 30, 0.8).
narrative_ontology:measurement(terr_be_t45, territorial_sovereignty_legitimacy__self_determination_reading, base_extractiveness, 45, 0.83).
narrative_ontology:measurement(terr_be_t60, territorial_sovereignty_legitimacy__self_determination_reading, base_extractiveness, 60, 0.84).
narrative_ontology:measurement(terr_be_t75, territorial_sovereignty_legitimacy__self_determination_reading, base_extractiveness, 75, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(terr_su_t0, territorial_sovereignty_legitimacy__self_determination_reading, suppression_requirement, 0, 0.8).
narrative_ontology:measurement(terr_su_t15, territorial_sovereignty_legitimacy__self_determination_reading, suppression_requirement, 15, 0.85).
narrative_ontology:measurement(terr_su_t30, territorial_sovereignty_legitimacy__self_determination_reading, suppression_requirement, 30, 0.88).
narrative_ontology:measurement(terr_su_t45, territorial_sovereignty_legitimacy__self_determination_reading, suppression_requirement, 45, 0.9).
narrative_ontology:measurement(terr_su_t60, territorial_sovereignty_legitimacy__self_determination_reading, suppression_requirement, 60, 0.91).
narrative_ontology:measurement(terr_su_t75, territorial_sovereignty_legitimacy__self_determination_reading, suppression_requirement, 75, 0.92).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(territorial_sovereignty_legitimacy__self_determination_reading, identity_coordination).
narrative_ontology:affects_constraint(territorial_sovereignty_legitimacy__self_determination_reading, territorial_sovereignty_legitimacy__covenant_continuity_reading).
narrative_ontology:affects_constraint(territorial_sovereignty_legitimacy__self_determination_reading, territorial_sovereignty_legitimacy__existential_matrix_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'territorial_sovereignty_legitimacy' kernel. This 'self_determination_reading' focuses on modern Arab demographic majority and continuous residence, framing the Israeli state as a colonial project. It directly contests the 'covenant_continuity_reading' and interacts with the 'existential_matrix_reading'.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
