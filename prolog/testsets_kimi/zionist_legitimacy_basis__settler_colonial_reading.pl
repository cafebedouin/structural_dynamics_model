% ============================================================================
% CONSTRAINT STORY: zionist_legitimacy_basis__settler_colonial_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   domain: political_history/settler_colonialism
 *
 * SUMMARY:
 *   Zionism as a European settler-colonial movement established an
 *   ethno-state in Palestine through the systematic displacement of the
 *   indigenous Palestinian population. Under the settler-colonial reading of
 *   the Zionist legitimacy basis, the state's authority derives from colonial
 *   structure rather than self-determination or divine promise, with
 *   indigenous displacement recognized as constitutive. This reading
 *   instantiates one branch of the contested kernel zionist_legitimacy_basis;
 *   sibling readings frame the same historical events as national liberation
 *   or religious restoration. The constraint coordinates the settler
 *   population into a sovereign state while extracting land, sovereignty, and
 *   demographic control from the indigenous population, requiring continuous
 *   active enforcement to maintain the separation.
 *
 * KEY AGENTS:
 *   - zionist_state: Agenda-setter (institutional/arbitrage) â administers legal and military machinery of settler-colonial state formation
 *   - jewish_settler_society: Primary beneficiary (organized/mobile) â receives land, resources, and exclusive citizenship
 *   - palestinian_indigenous_population: Primary target/payer (powerless/trapped) â bears dispossession, occupation, and denial of return
 *   - international_enablers: Secondary beneficiary (institutional/arbitrage) â provides diplomatic and military cover for strategic benefit
 *   - anti_colonial_scholars: Analytical observer (analytical/analytical) â documents colonial structure and contests legitimacy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(zionist_legitimacy_basis__settler_colonial_reading, 0.9).
domain_priors:suppression_score(zionist_legitimacy_basis__settler_colonial_reading, 0.95).
domain_priors:theater_ratio(zionist_legitimacy_basis__settler_colonial_reading, 0.72).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(zionist_legitimacy_basis__settler_colonial_reading, extractiveness, 0.9).
narrative_ontology:constraint_metric(zionist_legitimacy_basis__settler_colonial_reading, suppression_requirement, 0.95).
narrative_ontology:constraint_metric(zionist_legitimacy_basis__settler_colonial_reading, theater_ratio, 0.72).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(zionist_legitimacy_basis__settler_colonial_reading, accessibility_collapse, 0.85).
narrative_ontology:constraint_metric(zionist_legitimacy_basis__settler_colonial_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(zionist_legitimacy_basis__settler_colonial_reading, tangled_rope).
narrative_ontology:human_readable(zionist_legitimacy_basis__settler_colonial_reading, "Zionist Legitimacy Basis (Settler-Colonial Reading)").
narrative_ontology:topic_domain(zionist_legitimacy_basis__settler_colonial_reading, "political_history/settler_colonialism").

domain_priors:requires_active_enforcement(zionist_legitimacy_basis__settler_colonial_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(zionist_legitimacy_basis__settler_colonial_reading, 'f9b627a4-82f9-4795-b268-fc01aab91ad8').
narrative_ontology:cs_kernel_codification('f9b627a4-82f9-4795-b268-fc01aab91ad8', formalized).
narrative_ontology:cs_authority_grounding('f9b627a4-82f9-4795-b268-fc01aab91ad8', extraction).
narrative_ontology:cs_interpretation_layer_present('f9b627a4-82f9-4795-b268-fc01aab91ad8').
narrative_ontology:cs_reading_relation('f9b627a4-82f9-4795-b268-fc01aab91ad8', zionist_legitimacy_basis__national_liberation_reading, coexists_with).
narrative_ontology:cs_reading_relation('f9b627a4-82f9-4795-b268-fc01aab91ad8', zionist_legitimacy_basis__religious_restoration_reading, coexists_with).
narrative_ontology:cs_axiom('f9b627a4-82f9-4795-b268-fc01aab91ad8', foundational, colonial_structure_determines_legitimacy).
narrative_ontology:cs_axiom_status(colonial_structure_determines_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('f9b627a4-82f9-4795-b268-fc01aab91ad8', colonial_structure_determines_legitimacy, empirically_contingent).
narrative_ontology:cs_axiom('f9b627a4-82f9-4795-b268-fc01aab91ad8', foundational, indigenous_displacement_constitutive).
narrative_ontology:cs_axiom_status(indigenous_displacement_constitutive, holdable).
narrative_ontology:cs_axiom_grounding('f9b627a4-82f9-4795-b268-fc01aab91ad8', indigenous_displacement_constitutive, empirically_contingent).
narrative_ontology:cs_reference_frame('f9b627a4-82f9-4795-b268-fc01aab91ad8', settler_sovereignty_framework).
narrative_ontology:cs_drift_state('f9b627a4-82f9-4795-b268-fc01aab91ad8', contemporary, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('f9b627a4-82f9-4795-b268-fc01aab91ad8', '').
narrative_ontology:cs_kernel_id(zionist_legitimacy_basis__settler_colonial_reading, zionist_legitimacy_basis).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(zionist_legitimacy_basis__settler_colonial_reading, jewish_settler_society).
narrative_ontology:constraint_beneficiary(zionist_legitimacy_basis__settler_colonial_reading, international_enablers).
narrative_ontology:constraint_victim(zionist_legitimacy_basis__settler_colonial_reading, palestinian_indigenous_population).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the legal, military, and bureaucratic machinery of settler-colonial state formation: military occupation, land expropriation, citizenship and immigration laws privileging Jewish settlement, and border control. Sets the ideological agenda by framing the state as a democracy and security actor while operationalizing demographic engineering and territorial expansion.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__settler_colonial_reading, zionist_state, agenda_setter,
    institutional, generational, arbitrage, national).

% Receives state-provided land, housing, infrastructure, security services, and exclusive citizenship privileges in exchange for political loyalty and demographic consolidation. Their collective existence as a sovereign society is coordinated through state institutions, but this coordination is predicated on and sustained by the ongoing exclusion and displacement of the indigenous population.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__settler_colonial_reading, jewish_settler_society, beneficiary,
    organized, generational, mobile, national).

% Bears the costs of territorial dispossession, military occupation, apartheid legal regimes, blockade, and the denial of refugee return. Structurally excluded from the state's legitimacy framework, which classifies them as security threats, absentees, or demographic dangers. Exit is blocked by militarized borders, statelessness, refugee status, and indigenous attachment to the land.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__settler_colonial_reading, palestinian_indigenous_population, payer,
    powerless, generational, trapped, regional).

% Western states and international institutions that provide diplomatic protection, military funding, and legal impunity to the settler state, receiving in return strategic regional influence, arms trade relationships, and ideological alignment with Western geopolitical interests.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__settler_colonial_reading, international_enablers, beneficiary,
    institutional, generational, arbitrage, global).

% Academic researchers and solidarity movement analysts who document the settler-colonial structure, contest the Zionist legitimacy basis, and are routinely marginalized or suppressed in mainstream policy and media discourse.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__settler_colonial_reading, anti_colonial_scholars, observer,
    analytical, civilizational, analytical, global).

narrative_ontology:fixing_cost_class(zionist_legitimacy_basis__settler_colonial_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a dispersed European Jewish population into a consolidated territorial state with shared institutions, military defense, and a citizenship framework that provides public goods to the settler population.
% TRANSFER_FUNCTION: Transfers land, water, sovereignty, and demographic majority from the indigenous Palestinian population to the settler state and population, underwritten by international military and diplomatic support.
% ABSENT_VOICES: Palestinian refugees and internally displaced persons are structurally excluded from the political and legal conversation; their claims to return and restitution are legally erased or suspended. Anti-colonial indigenous voices are marginalized in international diplomatic forums.
% DISAPPEARANCE_RATIONALE: If the settler-colonial legitimacy basis vanished overnight, the legal and military architecture maintaining Jewish demographic majority and territorial control would lose coherence; the state would face immediate claims for return, restitution, and equal citizenship, and the regional geopolitical order would fundamentally reconfigure.
% FOUNDING_PROBLEM: Jewish statelessness and persecution in Europe; the need for a territorial solution to antisemitic violence and minority status.
% FOUNDING_PROBLEM_CORROBORATION: Jewish diaspora historians and Israeli critical scholars attest that the European refugee crisis that motivated mass migration has passed; the state now persists beyond its founding rescue function. Palestinian scholars and anti-colonial historians attest the founding problem was resolved at the expense of indigenous sovereignty.
narrative_ontology:disappearance_verdict(zionist_legitimacy_basis__settler_colonial_reading, world_rearranges).
narrative_ontology:founding_problem_status(zionist_legitimacy_basis__settler_colonial_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(zionist_legitimacy_basis__settler_colonial_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(zionist_legitimacy_basis__settler_colonial_reading, 'none', 1).
narrative_ontology:epsilon_provenance(zionist_legitimacy_basis__settler_colonial_reading, 0.9, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness is very high (0.92 at interval end) because the constraint's operation is constitutively tied to indigenous dispossession and demographic replacement, which continues through settlement expansion and military occupation. Suppression is near-total (0.95) because Palestinian alternatives (return, equal citizenship, sovereign statehood) are actively blocked by military force, legal discrimination, and international diplomatic suppression. Theater ratio is high (0.72) because the state invests heavily in performative democratic, security, and legal institutions that obscure the underlying colonial structure. Accessibility collapse is high (0.85) because once the colonial structure is understood, the only alternatives (decolonization, one democratic state) are rendered inaccessible by the existing power structure. Resistance is high (0.80) because Palestinian organized resistance and international solidarity movements actively contest the constraint.
 *
 * PERSPECTIVAL GAP:
 *   The Jewish settler society experiences the constraint as protective coordination (security, identity, statehood), computing toward rope or tangled-rope from that seat. The Palestinian indigenous population experiences the same structure as extraction and enclosure, computing toward snare. The international enabler seat experiences it as strategic coordination with manageable externalities. The engine derives this divergence from the same structural data: identical scope and enforcement, opposite directionality.
 *
 * DIRECTIONALITY LOGIC:
 *   Jewish_settler_society and international_enablers are declared beneficiaries, placing their directionality near the subsidy end; the state apparatus is agenda_setter rather than beneficiary in the receipt sense. Palestinian_indigenous_population is declared victim with trapped exit and powerless status, placing directionality near the full-target end and amplifying effective extraction through scope. The asymmetry is total: the coordination function operates for one group through the same machinery that extracts from the other.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â statelessness and persecution of European Jews â is authoritatively dead: Jewish security in diaspora is substantially achieved elsewhere, and the state persists beyond its rescue function. A founding_problem_status of 'dead' paired with a disappearance_verdict of 'world_rearranges' triggers the mandatrophy mismatch flag: the constraint continues to rearrange the world after its founding problem is solved, indicating it has become a self-sustaining extraction structure rather than a scaffold. This prevents misclassification as mere coordination by showing the coordination has outlived its justification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Does the settler-colonial reading foreclose the national liberation and religious restoration readings, or do they coexist as incommensurable frameworks?',
    'Comparative analysis of whether a single logical framework can hold both colonial structure and self-determination as primary determinants of legitimacy.',
    'If foreclosed, this constraint is analytically dispositive; if coexisting, classification must remain perspectival and the kernel remains genuinely contested.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Structural relationship between competing readings of Zionist legitimacy').

omega_variable(
    founding_problem_obsolescence,
    'Has the founding problem of European Jewish statelessness and persecution been resolved, rendering the constraint a zombie colonial structure?',
    'Comparative demographic and security analysis of Jewish diaspora communities versus Israeli Jewish population; assessment of whether the original refugee crisis persists.',
    'If dead, supports mandatrophy and piton/tangled-rope classification; if live, the constraint may still function as scaffold or rope for its intended beneficiaries.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_obsolescence, empirical, 'Whether the constraint''s founding justification remains operative').

omega_variable(
    enforcement_sustainability,
    'Is the suppression requirement trending toward collapse or hardening as international legitimacy erodes?',
    'Temporal tracking of international institutional rulings (ICJ, UNGA) versus military expenditure and settler population growth.',
    'If suppression hardens while legitimacy erodes, theater_ratio will rise and the constraint may drift toward snare; if enforcement becomes unsustainable, decolonization pressure increases.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(enforcement_sustainability, empirical, 'Trajectory of enforcement capacity versus international legitimacy').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(zionist_legitimacy_basis__settler_colonial_reading, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(zionist_legitimacy_basis_sc_tr_t0, zionist_legitimacy_basis__settler_colonial_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(zionist_legitimacy_basis_sc_tr_t15, zionist_legitimacy_basis__settler_colonial_reading, theater_ratio, 15, 0.35).
narrative_ontology:measurement(zionist_legitimacy_basis_sc_tr_t30, zionist_legitimacy_basis__settler_colonial_reading, theater_ratio, 30, 0.45).
narrative_ontology:measurement(zionist_legitimacy_basis_sc_tr_t45, zionist_legitimacy_basis__settler_colonial_reading, theater_ratio, 45, 0.6).
narrative_ontology:measurement(zionist_legitimacy_basis_sc_tr_t60, zionist_legitimacy_basis__settler_colonial_reading, theater_ratio, 60, 0.68).
narrative_ontology:measurement(zionist_legitimacy_basis_sc_tr_t75, zionist_legitimacy_basis__settler_colonial_reading, theater_ratio, 75, 0.72).

% Extraction over time
narrative_ontology:measurement(zionist_legitimacy_basis_sc_be_t0, zionist_legitimacy_basis__settler_colonial_reading, base_extractiveness, 0, 0.82).
narrative_ontology:measurement(zionist_legitimacy_basis_sc_be_t15, zionist_legitimacy_basis__settler_colonial_reading, base_extractiveness, 15, 0.78).
narrative_ontology:measurement(zionist_legitimacy_basis_sc_be_t30, zionist_legitimacy_basis__settler_colonial_reading, base_extractiveness, 30, 0.85).
narrative_ontology:measurement(zionist_legitimacy_basis_sc_be_t45, zionist_legitimacy_basis__settler_colonial_reading, base_extractiveness, 45, 0.8).
narrative_ontology:measurement(zionist_legitimacy_basis_sc_be_t60, zionist_legitimacy_basis__settler_colonial_reading, base_extractiveness, 60, 0.87).
narrative_ontology:measurement(zionist_legitimacy_basis_sc_be_t75, zionist_legitimacy_basis__settler_colonial_reading, base_extractiveness, 75, 0.92).

% Suppression requirement over time
narrative_ontology:measurement(zionist_legitimacy_basis_sc_su_t0, zionist_legitimacy_basis__settler_colonial_reading, suppression_requirement, 0, 0.8).
narrative_ontology:measurement(zionist_legitimacy_basis_sc_su_t15, zionist_legitimacy_basis__settler_colonial_reading, suppression_requirement, 15, 0.75).
narrative_ontology:measurement(zionist_legitimacy_basis_sc_su_t30, zionist_legitimacy_basis__settler_colonial_reading, suppression_requirement, 30, 0.85).
narrative_ontology:measurement(zionist_legitimacy_basis_sc_su_t45, zionist_legitimacy_basis__settler_colonial_reading, suppression_requirement, 45, 0.78).
narrative_ontology:measurement(zionist_legitimacy_basis_sc_su_t60, zionist_legitimacy_basis__settler_colonial_reading, suppression_requirement, 60, 0.88).
narrative_ontology:measurement(zionist_legitimacy_basis_sc_su_t75, zionist_legitimacy_basis__settler_colonial_reading, suppression_requirement, 75, 0.95).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(zionist_legitimacy_basis__settler_colonial_reading, identity_coordination).
narrative_ontology:affects_constraint(zionist_legitimacy_basis__settler_colonial_reading, zionist_legitimacy_basis__national_liberation_reading).
narrative_ontology:affects_constraint(zionist_legitimacy_basis__settler_colonial_reading, zionist_legitimacy_basis__religious_restoration_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kernel zionist_legitimacy_basis. The settler-colonial reading decomposes the legitimacy basis into a colonial structure with distinct beneficiaries and victims, while sibling readings frame the same historical events through national liberation or religious restoration lenses. Per the epsilon-invariance principle, each reading carries its own epsilon and stakeholder structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
