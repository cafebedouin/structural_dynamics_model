% ============================================================================
% CONSTRAINT STORY: jewish_sovereignty_palestine__religious_zionist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jewish_sovereignty_palestine__religious_zionist_reading, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: jewish_sovereignty_palestine__religious_zionist_reading
 *   human_readable: Divine Promise of Eretz Yisrael (Religious Zionist Reading)
 *   domain: political_philosophy/nationalism_studies/postcolonial_theory
 *
 * SUMMARY:
 *   This constraint represents the religious Zionist reading of Jewish
 *   sovereignty over Eretz Yisrael, where the divine promise to the Jewish
 *   people is understood as an inalienable territorial claim, and the
 *   establishment of the State of Israel is seen as a theological
 *   fulfillment. This reading grounds policies of territorial maximalism and
 *   settlement expansion, inherently subordinating or excluding Palestinian
 *   claims to land and self-determination. The constraint is classified as a
 *   Snare due to its very high extractiveness and suppression, with
 *   identifiable victims and active enforcement to maintain the territorial
 *   status quo.
 *
 * KEY AGENTS:
 *   - religious_zionist_settlers: Primary agenda-setter and beneficiary (organized/identity_locked)
 *   - israeli_state_institutions: Enforcer and beneficiary (institutional/constrained)
 *   - palestinian_population: Primary target/victim (powerless/trapped)
 *   - secular_israeli_citizens: Payer (moderate/constrained)
 *   - international_law_bodies: Analytical observer (institutional/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_sovereignty_palestine__religious_zionist_reading, 0.95).
domain_priors:suppression_score(jewish_sovereignty_palestine__religious_zionist_reading, 0.9).
domain_priors:theater_ratio(jewish_sovereignty_palestine__religious_zionist_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__religious_zionist_reading, extractiveness, 0.95).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__religious_zionist_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__religious_zionist_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__religious_zionist_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__religious_zionist_reading, resistance, 0.85).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_sovereignty_palestine__religious_zionist_reading, snare).
narrative_ontology:human_readable(jewish_sovereignty_palestine__religious_zionist_reading, "Divine Promise of Eretz Yisrael (Religious Zionist Reading)").
narrative_ontology:topic_domain(jewish_sovereignty_palestine__religious_zionist_reading, "political_philosophy/nationalism_studies/postcolonial_theory").

domain_priors:requires_active_enforcement(jewish_sovereignty_palestine__religious_zionist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_sovereignty_palestine__religious_zionist_reading, '927e4f37-cb86-4620-ba75-e82a679e40fb').
narrative_ontology:cs_kernel_codification('927e4f37-cb86-4620-ba75-e82a679e40fb', fixed_text).
narrative_ontology:cs_authority_grounding('927e4f37-cb86-4620-ba75-e82a679e40fb', lineage).
narrative_ontology:cs_interpretation_layer_present('927e4f37-cb86-4620-ba75-e82a679e40fb').
narrative_ontology:cs_reading_relation('927e4f37-cb86-4620-ba75-e82a679e40fb', jewish_sovereignty_palestine__liberal_nationalist_reading, forecloses).
narrative_ontology:cs_reading_relation('927e4f37-cb86-4620-ba75-e82a679e40fb', jewish_sovereignty_palestine__settler_colonial_reading, coexists_with).
narrative_ontology:cs_reading_relation('927e4f37-cb86-4620-ba75-e82a679e40fb', jewish_sovereignty_palestine__cultural_zionist_reading, influences).
narrative_ontology:cs_reading_relation('927e4f37-cb86-4620-ba75-e82a679e40fb', jewish_sovereignty_palestine__post_zionist_reading, forecloses).
narrative_ontology:cs_axiom('927e4f37-cb86-4620-ba75-e82a679e40fb', foundational, divine_covenant_territorial_title).
narrative_ontology:cs_axiom_status(divine_covenant_territorial_title, holdable).
narrative_ontology:cs_axiom_grounding('927e4f37-cb86-4620-ba75-e82a679e40fb', divine_covenant_territorial_title, theological).
narrative_ontology:cs_axiom('927e4f37-cb86-4620-ba75-e82a679e40fb', secondary, statehood_as_messianic_fulfillment).
narrative_ontology:cs_axiom_status(statehood_as_messianic_fulfillment, holdable).
narrative_ontology:cs_axiom_grounding('927e4f37-cb86-4620-ba75-e82a679e40fb', statehood_as_messianic_fulfillment, theological).
narrative_ontology:cs_reference_frame('927e4f37-cb86-4620-ba75-e82a679e40fb', biblical_covenant_land_inheritance).
narrative_ontology:cs_drift_state('927e4f37-cb86-4620-ba75-e82a679e40fb', contemporary_political_realities, gap(practice_drift, minor, false)).
narrative_ontology:cs_created_at('927e4f37-cb86-4620-ba75-e82a679e40fb', '').
narrative_ontology:cs_kernel_id(jewish_sovereignty_palestine__religious_zionist_reading, jewish_sovereignty_palestine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_sovereignty_palestine__religious_zionist_reading, religious_zionist_settlers).
narrative_ontology:constraint_beneficiary(jewish_sovereignty_palestine__religious_zionist_reading, israeli_state_institutions).
narrative_ontology:constraint_victim(jewish_sovereignty_palestine__religious_zionist_reading, palestinian_population).
narrative_ontology:constraint_victim(jewish_sovereignty_palestine__religious_zionist_reading, secular_israeli_citizens).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Actively pursue and expand Jewish settlement across all of Eretz Yisrael, viewing it as a divine commandment. They are the primary beneficiaries of policies that expand Israeli control and restrict Palestinian presence, and they exert significant political influence to maintain this trajectory. Their identity is deeply fused with the territorial claim.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__religious_zionist_reading, religious_zionist_settlers, agenda_setter,
    organized, generational, identity_locked, regional).

% Implement policies that support Jewish settlement and assert sovereignty over disputed territories, often aligning with the religious Zionist interpretation of the land's destiny. They benefit from the ideological coherence and political support provided by this reading, even if not all state actors fully subscribe to its theological basis. The state apparatus enforces the territorial claim.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__religious_zionist_reading, israeli_state_institutions, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(jewish_sovereignty_palestine__religious_zionist_reading, israeli_state_institutions, agenda_setter).

% Bear the direct costs of territorial expansion and dispossession, including land confiscation, movement restrictions, and denial of self-determination. Their claims to land and sovereignty are systematically suppressed by the constraint's operation. Exit options are severely limited, often to forced displacement or living under military occupation.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__religious_zionist_reading, palestinian_population, payer,
    powerless, generational, trapped, local).

% May not subscribe to the religious-theological basis of the claim but are bound by its political and military implications. They bear the costs of ongoing conflict, international isolation, and the erosion of democratic norms, often without direct benefit from the expansionist policies. Their ability to alter the constraint is limited by the political power of the religious Zionist bloc.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__religious_zionist_reading, secular_israeli_citizens, payer,
    moderate, biographical, constrained, national).

% Observe and critique the constraint's operation through the lens of international law, human rights, and self-determination principles. They issue resolutions and reports but lack direct enforcement power over the constraint's primary actors. Their analysis often highlights the extractive and suppressive nature of the territorial claim.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__religious_zionist_reading, international_law_bodies, observer,
    institutional, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the actions and beliefs of a significant segment of the Jewish population around a shared, divinely mandated territorial vision, providing a powerful ideological basis for collective action and state policy.
% TRANSFER_FUNCTION: Transfers land, resources, and sovereignty from the Palestinian population to the Jewish people, particularly religious Zionist settlers, based on a theological claim of divine ownership.
% ABSENT_VOICES: Palestinian voices are systematically excluded from the discourse that legitimizes this constraint; they would articulate a counter-claim based on indigenous rights, self-determination, and international law. Their absence is crucial for the constraint's persistence.
% DISAPPEARANCE_RATIONALE: If the divine promise as a grounding for territorial claim vanished, the entire ideological and legal framework for Israeli sovereignty over disputed territories would collapse. This would necessitate a fundamental re-evaluation of borders, land ownership, and the rights of all inhabitants, leading to a profound rearrangement of the political landscape.
% FOUNDING_PROBLEM: The historical dispersion and persecution of the Jewish people, culminating in the Holocaust, created a perceived existential need for a secure, sovereign Jewish homeland in their ancestral land.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem of Jewish insecurity and the need for a homeland is widely attested by Jewish historical narratives and international recognition (e.g., UN Partition Plan). However, the specific religious-theological interpretation of this need as an inalienable divine right to all of Eretz Yisrael is primarily attested by religious Zionist leaders and communities, with limited corroboration from secular or international bodies regarding the divine mandate itself.
narrative_ontology:disappearance_verdict(jewish_sovereignty_palestine__religious_zionist_reading, world_rearranges).
narrative_ontology:founding_problem_status(jewish_sovereignty_palestine__religious_zionist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_sovereignty_palestine__religious_zionist_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(jewish_sovereignty_palestine__religious_zionist_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jewish_sovereignty_palestine__religious_zionist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jewish_sovereignty_palestine__religious_zionist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jewish_sovereignty_palestine__religious_zionist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is extremely high (0.95) because the constraint claims absolute title to land, denying any competing claims and leading to systematic dispossession. Suppression is also very high (0.90) as the constraint's persistence relies on active military and legal enforcement to control territory and restrict Palestinian agency. Theater ratio is low (0.10) because the theological claim is genuinely held and directly drives policy, with minimal performative cover for other functions. Accessibility collapse is high (0.90) as alternatives like a two-state solution or shared sovereignty are largely foreclosed by the maximalist territorial claim. Resistance is high (0.85) due to ongoing Palestinian opposition and international condemnation.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of religious Zionist settlers, this constraint is a divinely mandated imperative, a Mountain or Rope that coordinates the fulfillment of a sacred promise. From the Palestinian perspective, it is a pure Snare, an imposed structure of dispossession and control. The engine's classification as Snare reflects the objective structural reality of extraction and suppression, independent of the internal framing of its beneficiaries.
 *
 * DIRECTIONALITY LOGIC:
 *   Religious Zionist settlers and Israeli state institutions are clear beneficiaries, driving the agenda and collecting territorial gains. The Palestinian population is the primary victim, bearing the full cost of dispossession and lacking exit options. Secular Israeli citizens are payers, bearing the costs of conflict and international isolation without necessarily sharing the theological benefits. International law bodies are observers, analyzing the constraint's impact without direct participation.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (divine promise, existential security) is considered 'live' by its proponents, but its operational form has shifted from securing a homeland to continuous territorial expansion. The high extractiveness and suppression, coupled with the 'live' founding problem status, indicate that the constraint is not a Piton. Instead, it is an actively maintained Snare where the original mandate is leveraged to justify ongoing extraction, preventing mislabeling as a benign coordination mechanism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    divine_mandate_empirical_status,
    'Is the divine promise of Eretz Yisrael an empirically verifiable or a purely theological claim?',
    'No empirical resolution possible; depends on theological or philosophical commitment.',
    'If treated as an empirical claim, its lack of falsifiability would undermine its grounding. As a theological claim, it remains outside empirical challenge but its legitimacy for non-adherents is zero.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(divine_mandate_empirical_status, conceptual, 'The epistemic status of the divine mandate.').

omega_variable(
    territorial_maximalism_necessity,
    'Is the maximalist territorial claim (all of Eretz Yisrael) a necessary component of Jewish security and identity, or an expansionist interpretation?',
    'Historical analysis of alternative Zionist visions (e.g., Uganda Plan, partition proposals) and contemporary security assessments that decouple land area from security.',
    'If not necessary, the ''security'' justification for extraction is weakened, reclassifying the constraint closer to pure Snare. If necessary, it reinforces the ''Tangled Rope'' aspect of coordinating security with extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(territorial_maximalism_necessity, empirical, 'Whether territorial maximalism is essential or an interpretation.').

omega_variable(
    palestinian_identity_lock,
    'To what extent is Palestinian identity ''identity_locked'' to the land, making exit options structurally equivalent to ''trapped''?',
    'Sociological studies of Palestinian attachment to land and historical narratives of displacement; analysis of legal and administrative barriers to return or compensation.',
    'If identity-locked, the effective suppression and extractiveness are amplified, as the cost of ''exit'' (relinquishing land claims) is existential. If less so, exit options are merely ''constrained''.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(palestinian_identity_lock, empirical, 'The degree of identity fusion between Palestinians and the land.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_sovereignty_palestine__religious_zionist_reading, 1967, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jewi_tr_t1967, jewish_sovereignty_palestine__religious_zionist_reading, theater_ratio, 1967, 0.2).
narrative_ontology:measurement(jewi_tr_t1977, jewish_sovereignty_palestine__religious_zionist_reading, theater_ratio, 1977, 0.18).
narrative_ontology:measurement(jewi_tr_t1987, jewish_sovereignty_palestine__religious_zionist_reading, theater_ratio, 1987, 0.15).
narrative_ontology:measurement(jewi_tr_t1997, jewish_sovereignty_palestine__religious_zionist_reading, theater_ratio, 1997, 0.12).
narrative_ontology:measurement(jewi_tr_t2007, jewish_sovereignty_palestine__religious_zionist_reading, theater_ratio, 2007, 0.11).
narrative_ontology:measurement(jewi_tr_t2017, jewish_sovereignty_palestine__religious_zionist_reading, theater_ratio, 2017, 0.1).
narrative_ontology:measurement(jewi_tr_t2024, jewish_sovereignty_palestine__religious_zionist_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(jewi_be_t1967, jewish_sovereignty_palestine__religious_zionist_reading, base_extractiveness, 1967, 0.75).
narrative_ontology:measurement(jewi_be_t1977, jewish_sovereignty_palestine__religious_zionist_reading, base_extractiveness, 1977, 0.8).
narrative_ontology:measurement(jewi_be_t1987, jewish_sovereignty_palestine__religious_zionist_reading, base_extractiveness, 1987, 0.85).
narrative_ontology:measurement(jewi_be_t1997, jewish_sovereignty_palestine__religious_zionist_reading, base_extractiveness, 1997, 0.88).
narrative_ontology:measurement(jewi_be_t2007, jewish_sovereignty_palestine__religious_zionist_reading, base_extractiveness, 2007, 0.91).
narrative_ontology:measurement(jewi_be_t2017, jewish_sovereignty_palestine__religious_zionist_reading, base_extractiveness, 2017, 0.93).
narrative_ontology:measurement(jewi_be_t2024, jewish_sovereignty_palestine__religious_zionist_reading, base_extractiveness, 2024, 0.95).

% Suppression requirement over time
narrative_ontology:measurement(jewi_su_t1967, jewish_sovereignty_palestine__religious_zionist_reading, suppression_requirement, 1967, 0.7).
narrative_ontology:measurement(jewi_su_t1977, jewish_sovereignty_palestine__religious_zionist_reading, suppression_requirement, 1977, 0.75).
narrative_ontology:measurement(jewi_su_t1987, jewish_sovereignty_palestine__religious_zionist_reading, suppression_requirement, 1987, 0.8).
narrative_ontology:measurement(jewi_su_t1997, jewish_sovereignty_palestine__religious_zionist_reading, suppression_requirement, 1997, 0.85).
narrative_ontology:measurement(jewi_su_t2007, jewish_sovereignty_palestine__religious_zionist_reading, suppression_requirement, 2007, 0.88).
narrative_ontology:measurement(jewi_su_t2017, jewish_sovereignty_palestine__religious_zionist_reading, suppression_requirement, 2017, 0.89).
narrative_ontology:measurement(jewi_su_t2024, jewish_sovereignty_palestine__religious_zionist_reading, suppression_requirement, 2024, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
