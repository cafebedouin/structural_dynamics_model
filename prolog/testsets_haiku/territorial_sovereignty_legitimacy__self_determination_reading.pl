% ============================================================================
% CONSTRAINT STORY: territorial_sovereignty_legitimacy__self_determination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: territorial_sovereignty_legitimacy__self_determination_reading
 *   human_readable: Territorial Sovereignty via Self-Determination (Arab Majority, Modern Continuous Residence)
 *   domain: political/territorial
 *
 * SUMMARY:
 *   This constraint story instantiates ONE READING of the contested kernel
 *   'territorial_sovereignty_legitimacy': the self-determination reading. It
 *   applies the modern principle of self-determination to the Arab population
 *   with demographic majority and continuous residence in the territory
 *   during the 19th-20th centuries. Under this reading, sovereignty
 *   legitimacy flows from demographic modernity and continuous presence, not
 *   from ancient covenant or existential necessity. Partition is framed as an
 *   unjust imposition by external powers; the Israeli state is reframed as a
 *   colonial project; Palestinian displacement and statelessness are treated
 *   as the founding problem the reading solves. This reading COEXISTS WITH
 *   the covenant_continuity_reading and existential_matrix_reading as live
 *   positions held by different parties in the territorial dispute. The
 *   claim/metric divergence is intentional: the constraint is CLAIMED as
 *   tangled_rope (coordinates the Arab self-determination principle while
 *   extracting from Jewish populations) while the metrics describe
 *   substantially extractive, actively enforced operation with moderate
 *   theater (legitimacy work). The engine measures this claimed-to-computed
 *   gap; do not reconcile them.
 *
 * KEY AGENTS:
 *   - arab_population_territorial_control — primary beneficiary and agenda-setter (organized power, identity-locked exit)
 *   - palestinian_stateless_diaspora — primary victim (powerless, trapped exit)
 *   - israeli_jewish_population_territorial_displacement — victim under this reading (powerful, constrained exit due to zero-sum structure)
 *   - external_colonial_powers — payers/delegitimized under this reading (institutional, arbitrage exit)
 *   - international_legal_community — observer computing jurisdiction and authority
 *   - host_countries_refugee_populations — secondary payers bearing institutional burden
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(territorial_sovereignty_legitimacy__self_determination_reading, 0.82).
domain_priors:suppression_score(territorial_sovereignty_legitimacy__self_determination_reading, 0.79).
domain_priors:theater_ratio(territorial_sovereignty_legitimacy__self_determination_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__self_determination_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__self_determination_reading, suppression_requirement, 0.79).
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__self_determination_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__self_determination_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__self_determination_reading, resistance, 0.88).

% --- Constraint claim ---
narrative_ontology:constraint_claim(territorial_sovereignty_legitimacy__self_determination_reading, tangled_rope).
narrative_ontology:human_readable(territorial_sovereignty_legitimacy__self_determination_reading, "Territorial Sovereignty via Self-Determination (Arab Majority, Modern Continuous Residence)").
narrative_ontology:topic_domain(territorial_sovereignty_legitimacy__self_determination_reading, "political/territorial").

domain_priors:requires_active_enforcement(territorial_sovereignty_legitimacy__self_determination_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(territorial_sovereignty_legitimacy__self_determination_reading, '079f76d8-1cca-4999-b0e7-66f51d8f6346').
narrative_ontology:cs_kernel_codification('079f76d8-1cca-4999-b0e7-66f51d8f6346', formalized).
narrative_ontology:cs_authority_grounding('079f76d8-1cca-4999-b0e7-66f51d8f6346', extraction).
narrative_ontology:cs_interpretation_layer_present('079f76d8-1cca-4999-b0e7-66f51d8f6346').
narrative_ontology:cs_reading_relation('079f76d8-1cca-4999-b0e7-66f51d8f6346', territorial_sovereignty_legitimacy__covenant_continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('079f76d8-1cca-4999-b0e7-66f51d8f6346', territorial_sovereignty_legitimacy__existential_matrix_reading, coexists_with).
narrative_ontology:cs_axiom('079f76d8-1cca-4999-b0e7-66f51d8f6346', foundational, modern_self_determination_principle_temporally_bounded).
narrative_ontology:cs_axiom_status(modern_self_determination_principle_temporally_bounded, holdable).
narrative_ontology:cs_axiom_grounding('079f76d8-1cca-4999-b0e7-66f51d8f6346', modern_self_determination_principle_temporally_bounded, deontological).
narrative_ontology:cs_axiom('079f76d8-1cca-4999-b0e7-66f51d8f6346', foundational, demographic_majority_and_continuous_modern_residence_decisive).
narrative_ontology:cs_axiom_status(demographic_majority_and_continuous_modern_residence_decisive, holdable).
narrative_ontology:cs_axiom_grounding('079f76d8-1cca-4999-b0e7-66f51d8f6346', demographic_majority_and_continuous_modern_residence_decisive, empirically_contingent).
narrative_ontology:cs_reference_frame('079f76d8-1cca-4999-b0e7-66f51d8f6346', arab_self_determination_within_historic_palestine).
narrative_ontology:cs_drift_state('079f76d8-1cca-4999-b0e7-66f51d8f6346', contemporary_post_oslo, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('079f76d8-1cca-4999-b0e7-66f51d8f6346', '').
narrative_ontology:cs_kernel_id(territorial_sovereignty_legitimacy__self_determination_reading, territorial_sovereignty_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(territorial_sovereignty_legitimacy__self_determination_reading, arab_population_territorial_control).
narrative_ontology:constraint_victim(territorial_sovereignty_legitimacy__self_determination_reading, palestinian_stateless_diaspora).
narrative_ontology:constraint_victim(territorial_sovereignty_legitimacy__self_determination_reading, israeli_jewish_population_territorial_displacement).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(territorial_sovereignty_legitimacy__self_determination_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(territorial_sovereignty_legitimacy__self_determination_reading, 'none', 1).

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
 *   Extractiveness rises steeply from 1900 (pre-partition, reading is not yet active) through 1945 (partition, reading crystallizes) to 2026 (reading has accumulated 78 years of enforcement history). The measurement series model the reading's temporal emergence and hardening. Suppression is high and rising because the reading's enforcement requires active exclusion of rival readings' legitimacy claims—particularly the covenant_continuity reading, which must be delegitimized as 'colonial' for the self-determination reading to hold. Theater_ratio is moderate (0.41) because legitimate coordination work exists (resolving territorial dispute via self-determination principle) but is increasingly accompanied by performance maintenance (annual UN General Assembly votes, symbolic reaffirmations, international legal theater defending against competing readings). Accessibility_collapse is high (0.72) because the temporal boundary ('modern period' = 19th-20th centuries) forecloses alternative historical temporalities (ancient covenant is ruled out of consideration by fiat); continuous residence becomes the operative criterion, making covenant-based and existential claims unavailable to those who cannot marshal it. Resistance is highest (0.88) because multiple powerful seats reject this reading's legitimacy: Israeli state, great powers supporting Israel, and the existential_matrix reading (which denies that juridical legitimacy can resolve zero-sum survival questions).
 *
 * PERSPECTIVAL GAP:
 *   The Arab beneficiary seat and the Israeli victim seat compute radically different types from identical structural data. From the Arab perspective, the reading solves a genuine coordination problem (collective self-determination) and restores justice; from the Israeli perspective, it is pure extraction disguised as juridical principle. The international_legal_community observer seat must choose which reading is authoritative, which becomes the functional arbiter of whether the constraint is enforced. This perspectival gap is NOT an error; it is the core finding the corpus measures: a single normative claim generates different effective types depending on which reading frames it.
 *
 * DIRECTIONALITY LOGIC:
 *   Arab beneficiary sits near d=0.0 (full beneficiary: gains sovereignty, territorial control, and legitimacy from the reading). Palestinian diaspora sits near d=1.0 (full target: they are victimized by partition and statelessness, but the reading's solution—right of return—would require Israeli displacement, making the asymmetry zero-sum). Israeli Jewish population sits near d=0.95 (nearly full target: territorial displacement is existentially threatening; constrained exit because relocation of an entire population cannot be freely chosen despite institutional power). The external powers sit near d=0.6 (partial payers: delegitimized as colonial architects, but retain geopolitical arbitrage). Host countries sit near d=0.65 (payers: bear refugee burden but can exit via regional peace settlement). The reading itself (international_self_determination_doctrine) is not an agent and does not carry directionality; it is a vindicated proposition that amplifies the beneficiary's legitimacy claim.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem_status is 'live' (Palestinian statelessness and diaspora are ongoing facts), but the founding_problem_corroboration splits radically: it is corroborated by Palestinian movements, Arab states, UN General Assembly, and postcolonial scholarship, but CONTESTED by Israeli state, supporting powers, and alternative readings. This split corroboration is the signature of a contested reading—the reading exists because multiple parties hold it, not because neutral authorities endorse it. The constraint avoids mandatrophy (founding problem dead but constraint persists) only insofar as Palestinian statelessness remains active; if a two-state settlement were to crystallize and Palestinian state established, the founding_problem would shift from 'live' to 'resolved' and this constraint would face mandatrophy risk (persist only as theater/legacy, no longer solving the problem it was built to solve). Theater_ratio's moderate-rising trajectory (0.28 → 0.41 over 81 years) signals that diplomatic and legal performance is increasingly maintaining the constraint relative to functional problem-solving. The constraint satisfies the tangled_rope gate: it coordinates a genuine collective-action problem (Arab self-determination) AND extracts asymmetrically (Jewish population loses territorial claim); enforcement is active (suppression_requirement=0.79); beneficiaries and victims are distinct.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    demographic_modernity_temporality,
    'Is the boundary of ''modern period'' (19th-20th centuries) a natural historical division or an arbitrary framing chosen to advantage the Arab majority demographic?',
    'Historical analysis of when continuous Arab residence can be documented; comparison with Jewish historical presence claims over longer timescales; examination of what temporal boundaries would be justified by neutral historical methodology vs. reading-motivated selection.',
    'If the modern temporal boundary is arbitrary or motivated by the reading''s desired outcome, the legitimacy of using demographic majority as the operative criterion collapses—the reading becomes circular (we count only modern populations because that''s the population that has the majority). If the boundary is justified by neutral historical standards, the reading''s use of demographic majority is more defensible.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(demographic_modernity_temporality, empirical, 'Whether the modern-period temporal boundary is historically justified or reading-motivated selection.').

omega_variable(
    continuous_residence_measurement,
    'What counts as ''continuous residence'' and how is it verified for populations that experienced expulsion, refuge, displacement, and return across the modern period?',
    'Demographic and genealogical documentation; comparison of evidentiary standards applied to Arab vs. Jewish residence claims; assessment of whether ''continuous'' is genealogical (unbroken family presence) or political (recognized administrative presence).',
    'If continuous residence is defined narrowly and genealogically verified, the Arab claim may be substantially validated. If defined broadly to include displacement-and-return sequences, both Arab and Jewish claims can satisfy it. The measurement choice determines whether the criterion discriminates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(continuous_residence_measurement, empirical, 'The operationalization of ''continuous residence'' and what population counts satisfy it.').

omega_variable(
    self_determination_scope_ambiguity,
    'Does self-determination apply at the individual level (each person''s right to self-govern within their chosen polity) or collective level (peoples'' right to govern territory)? And if collective, which collective—Arabs as a continent-spanning people, or Palestinians as a territorial sub-group, or Muslims as a religious collective?',
    'International law analysis of how self-determination has been operationalized in similar territorial disputes; examination of whether individual and collective self-determination produce conflicting claims (individual Palestinian''s right to live in-territory vs. collective Palestinian right to govern territory exclusively).',
    'If self-determination is individualist, both Arab and Jewish populations have claims to live and self-govern within the territory, which undermines the exclusive sovereignty claim. If collective, the reading must define the relevant collective (likely producing different results if ''Arab world,'' ''Palestinian people,'' or ''Muslim umma'' are the unit). The scope choice determines whether the reading''s application is coherent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(self_determination_scope_ambiguity, conceptual, 'Ambiguity in the scope and level of application of the self-determination principle.').

omega_variable(
    extraction_asymmetry_vs_coordination,
    'Is the reading''s core function to coordinate Arab collective self-governance (genuine coordination problem), or to extract territorial control from Jewish populations and external powers (pure allocation problem)? Or both?',
    'Counterfactual analysis: would the reading be proposed and sustained if it did not result in territorial control shifting to Arab majorities? Would it be applied to other territorial disputes where demographic majority did not align with Arab interests? Examination of whether non-Arab majority populations invoking self-determination receive equal legitimacy support from the reading''s proponents.',
    'If the reading is selective in applying self-determination principle (Arab majorities yes, Uyghur or Rohingya majorities no), it is primarily extractive with coordination rhetoric. If applied universally regardless of alignment with Arab interests, it is genuinely coordinate. The asymmetry or universality determines the measured χ and type.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(extraction_asymmetry_vs_coordination, preference, 'Whether the reading is applied consistently as a principle or selectively as a tool for Arab majority advantage.').

omega_variable(
    covenant_reading_foreclosure,
    'Does the self-determination reading''s treatment of ''modern period'' as the relevant temporal scope FORECLOSE or merely INFLUENCE the covenant_continuity reading?',
    'Examination of whether a single legal/normative framework could hold both readings simultaneously (e.g., ''self-determination governs modern territorial arrangements, while covenant governs non-territorial religious/cultural claims''). If yes, they coexist; if no, they foreclose each other.',
    'If they foreclose, the kernel exhibits genuine logical incompatibility and the territorial dispute is fundamentally about which reading is true/valid. If they coexist, the dispute is about which reading should be given priority or how to balance them—a different kind of contestation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(covenant_reading_foreclosure, conceptual, 'Logical relationship between the temporal-scope boundary chosen by self-determination reading and the timeless covenant of the covenant_continuity reading.').

omega_variable(
    israeli_state_colonial_classification,
    'Is the framing of the Israeli state as a ''colonial project'' a descriptive classification of its historical origins, or a normative judgment that delegitimizes it regardless of its current character as an established, internationally recognized state?',
    'Analysis of how colonialism is defined and applied in international law; comparison of whether the Israeli state''s post-1948 institutional development, recognition, and integration into international law affect its colonial classification. Examination of whether the reading would maintain the classification if Israel had not engaged in occupation or settlement expansion post-1967.',
    'If colonial classification is origin-based and permanent, the reading treats the Israeli state as inherently illegitimate regardless of international recognition or state-like behavior. If it is contingent on ongoing colonial practices, the reading could accommodate recognized statehood if colonial practices cease. The distinction determines whether the reading offers a path to coexistence or enforces permanent delegitimization.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(israeli_state_colonial_classification, preference, 'Whether the self-determination reading''s classification of Israel as colonial is origin-based and permanent or practice-contingent and revisable.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(territorial_sovereignty_legitimacy__self_determination_reading, 1900, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(terr_tr_t1900, territorial_sovereignty_legitimacy__self_determination_reading, theater_ratio, 1900, 0.0).
narrative_ontology:measurement(terr_tr_t1920, territorial_sovereignty_legitimacy__self_determination_reading, theater_ratio, 1920, 0.08).
narrative_ontology:measurement(terr_tr_t1945, territorial_sovereignty_legitimacy__self_determination_reading, theater_ratio, 1945, 0.28).
narrative_ontology:measurement(terr_tr_t1967, territorial_sovereignty_legitimacy__self_determination_reading, theater_ratio, 1967, 0.38).
narrative_ontology:measurement(terr_tr_t1993, territorial_sovereignty_legitimacy__self_determination_reading, theater_ratio, 1993, 0.4).
narrative_ontology:measurement(terr_tr_t2026, territorial_sovereignty_legitimacy__self_determination_reading, theater_ratio, 2026, 0.41).

% Extraction over time
narrative_ontology:measurement(terr_be_t1900, territorial_sovereignty_legitimacy__self_determination_reading, base_extractiveness, 1900, 0.0).
narrative_ontology:measurement(terr_be_t1920, territorial_sovereignty_legitimacy__self_determination_reading, base_extractiveness, 1920, 0.15).
narrative_ontology:measurement(terr_be_t1945, territorial_sovereignty_legitimacy__self_determination_reading, base_extractiveness, 1945, 0.52).
narrative_ontology:measurement(terr_be_t1967, territorial_sovereignty_legitimacy__self_determination_reading, base_extractiveness, 1967, 0.71).
narrative_ontology:measurement(terr_be_t1993, territorial_sovereignty_legitimacy__self_determination_reading, base_extractiveness, 1993, 0.79).
narrative_ontology:measurement(terr_be_t2026, territorial_sovereignty_legitimacy__self_determination_reading, base_extractiveness, 2026, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(terr_su_t1900, territorial_sovereignty_legitimacy__self_determination_reading, suppression_requirement, 1900, 0.0).
narrative_ontology:measurement(terr_su_t1920, territorial_sovereignty_legitimacy__self_determination_reading, suppression_requirement, 1920, 0.22).
narrative_ontology:measurement(terr_su_t1945, territorial_sovereignty_legitimacy__self_determination_reading, suppression_requirement, 1945, 0.58).
narrative_ontology:measurement(terr_su_t1967, territorial_sovereignty_legitimacy__self_determination_reading, suppression_requirement, 1967, 0.74).
narrative_ontology:measurement(terr_su_t1993, territorial_sovereignty_legitimacy__self_determination_reading, suppression_requirement, 1993, 0.77).
narrative_ontology:measurement(terr_su_t2026, territorial_sovereignty_legitimacy__self_determination_reading, suppression_requirement, 2026, 0.79).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(territorial_sovereignty_legitimacy__self_determination_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(territorial_sovereignty_legitimacy__self_determination_reading, 0.12).
narrative_ontology:affects_constraint(territorial_sovereignty_legitimacy__self_determination_reading, territorial_sovereignty_legitimacy__covenant_continuity_reading).
narrative_ontology:affects_constraint(territorial_sovereignty_legitimacy__self_determination_reading, territorial_sovereignty_legitimacy__existential_matrix_reading).
narrative_ontology:affects_constraint(territorial_sovereignty_legitimacy__self_determination_reading, palestinian_right_of_return__self_determination_framing).
narrative_ontology:affects_constraint(territorial_sovereignty_legitimacy__self_determination_reading, settlement_expansion_legitimacy__occupation_constraints).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the contested kernel 'territorial_sovereignty_legitimacy'. The self-determination reading applies modern collective self-determination principle to Arab demographic majority; the covenant_continuity reading applies ancient covenant + Jewish historical claim + international recognition; the existential_matrix reading rejects juridical legitimacy entirely, treating the conflict as existential zero-sum. All three readings derive from the same underlying kernel (the question of what legitimates territorial sovereignty), but they have distinct ε values, beneficiary/victim structures, and enforcement mechanisms. Each reading is a separate constraint story with its own type classification. They are linked via network.affects_constraints because changes in one reading's institutional power ripple through the others (if self-determination reading gains international legal authority, it influences the viability and framing of the covenant and existential readings). The sibling readings are NOT authoring alternatives to choose between; they are simultaneous readings held by different parties. The corpus models the contest, not the resolution.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
