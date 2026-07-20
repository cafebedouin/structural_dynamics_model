% ============================================================================
% CONSTRAINT STORY: territorial_legitimacy__partition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_territorial_legitimacy__partition_reading, []).

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
 *   constraint_id: territorial_legitimacy__partition_reading
 *   human_readable: Territorial Legitimacy via International Legal Partition and State Recognition (UN Resolution 181, 1948 Borders)
 *   domain: political/international_law/territorial_sovereignty
 *
 * SUMMARY:
 *   This constraint instantiates the partition_reading of the
 *   territorial_legitimacy kernel: legitimacy is derived from international
 *   legal partition under UN Resolution 181 and subsequent recognized
 *   borders. The reading treats both Israeli and Palestinian state claims as
 *   legitimate within assigned boundaries, delegitimizes settlement beyond
 *   1967 lines, and structurally enables a two-state solution. It is a
 *   contested reading within a kernel that also includes
 *   indigenous_continuity_reading and security_necessity_reading.
 *
 * KEY AGENTS:
 *   - un_partition_institutions: Agenda-setter (institutional/global) â administers recognition framework
 *   - israeli_state_recognized: Beneficiary (institutional/national) â receives sovereignty within borders
 *   - palestinian_national_authority: Beneficiary (moderate/national) â receives diplomatic standing via partition
 *   - palestinian_refugees_1948: Primary target (powerless/trapped) â bears dispossession cost
 *   - internally_displaced_communities: Secondary target (powerless/trapped) â property claims frozen by borders
 *   - expansionist_territorial_claimants: Excluded (organized/constrained) â claims delegitimized by framework
 *   - major_power_arbitrators: Observer (institutional/global) â adjudicates between readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(territorial_legitimacy__partition_reading, 0.7).
domain_priors:suppression_score(territorial_legitimacy__partition_reading, 0.82).
domain_priors:theater_ratio(territorial_legitimacy__partition_reading, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(territorial_legitimacy__partition_reading, extractiveness, 0.7).
narrative_ontology:constraint_metric(territorial_legitimacy__partition_reading, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(territorial_legitimacy__partition_reading, theater_ratio, 0.68).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(territorial_legitimacy__partition_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(territorial_legitimacy__partition_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(territorial_legitimacy__partition_reading, tangled_rope).
narrative_ontology:human_readable(territorial_legitimacy__partition_reading, "Territorial Legitimacy via International Legal Partition and State Recognition (UN Resolution 181, 1948 Borders)").
narrative_ontology:topic_domain(territorial_legitimacy__partition_reading, "political/international_law/territorial_sovereignty").

domain_priors:requires_active_enforcement(territorial_legitimacy__partition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(territorial_legitimacy__partition_reading, '528b6992-bf14-45e9-8106-5d4ff80a6038').
narrative_ontology:cs_kernel_codification('528b6992-bf14-45e9-8106-5d4ff80a6038', formalized).
narrative_ontology:cs_authority_grounding('528b6992-bf14-45e9-8106-5d4ff80a6038', lineage).
narrative_ontology:cs_interpretation_layer_present('528b6992-bf14-45e9-8106-5d4ff80a6038').
narrative_ontology:cs_reading_relation('528b6992-bf14-45e9-8106-5d4ff80a6038', territorial_legitimacy__indigenous_continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('528b6992-bf14-45e9-8106-5d4ff80a6038', territorial_legitimacy__security_necessity_reading, coexists_with).
narrative_ontology:cs_axiom('528b6992-bf14-45e9-8106-5d4ff80a6038', foundational, statehood_via_international_mandate).
narrative_ontology:cs_axiom_status(statehood_via_international_mandate, holdable).
narrative_ontology:cs_axiom_grounding('528b6992-bf14-45e9-8106-5d4ff80a6038', statehood_via_international_mandate, conventional).
narrative_ontology:cs_axiom('528b6992-bf14-45e9-8106-5d4ff80a6038', foundational, territorial_integrity_of_assigned_borders).
narrative_ontology:cs_axiom_status(territorial_integrity_of_assigned_borders, holdable).
narrative_ontology:cs_axiom_grounding('528b6992-bf14-45e9-8106-5d4ff80a6038', territorial_integrity_of_assigned_borders, conventional).
narrative_ontology:cs_reference_frame('528b6992-bf14-45e9-8106-5d4ff80a6038', international_legal_partition_framework).
narrative_ontology:cs_drift_state('528b6992-bf14-45e9-8106-5d4ff80a6038', contemporary_post_1967_occupation, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('528b6992-bf14-45e9-8106-5d4ff80a6038', '').
narrative_ontology:cs_kernel_id(territorial_legitimacy__partition_reading, territorial_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(territorial_legitimacy__partition_reading, israeli_state_recognized).
narrative_ontology:constraint_beneficiary(territorial_legitimacy__partition_reading, palestinian_national_authority).
narrative_ontology:constraint_victim(territorial_legitimacy__partition_reading, palestinian_refugees_1948).
narrative_ontology:constraint_victim(territorial_legitimacy__partition_reading, internally_displaced_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the international legal framework of partition through Resolution 181, recognition protocols, and UN membership criteria. Determines which territorial claims achieve statehood status and which are relegated to non-state or occupied status. Maintains the register of legitimate borders and sanctions departures from them through resolutions and international legal findings.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__partition_reading, un_partition_institutions, agenda_setter,
    institutional, civilizational, constrained, global).

% Receives sovereign legitimacy and diplomatic recognition within the partition-assigned borders. Exercises territorial control and state functions within those boundaries. Under this reading, claims beyond the 1967 lines are delegitimized as violations of international law, while sovereignty within recognized borders is protected.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__partition_reading, israeli_state_recognized, beneficiary,
    institutional, generational, constrained, national).

% Claims statehood legitimacy based on the partition framework and 1967 borders. Seeks full international recognition as a sovereign equal within assigned territory. Its diplomatic standing in UN forums depends on the partition reading, which provides the legal vocabulary for its claim.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__partition_reading, palestinian_national_authority, beneficiary,
    moderate, generational, constrained, national).

% Descended from or directly displaced by the 1948 partition. Denied return to properties that fell within the Israeli-assigned border. Statelessness or refugee status persists across generations. The partition framework legally extinguished their territorial claims in favor of state recognition for others.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__partition_reading, palestinian_refugees_1948, payer,
    powerless, biographical, trapped, regional).

% Communities displaced by the 1948 war and subsequent conflicts who remain within the broader territory but outside their original localities. The partition framework freezes their property claims inside boundaries assigned to the other state, converting them into permanent refugees or second-class residents.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__partition_reading, internally_displaced_communities, payer,
    powerless, generational, trapped, regional).

% Assert sovereignty claims beyond the 1967 lines based on historical, religious, or security arguments. Structurally excluded from UN recognition forums that operate on the partition reading; their claims are treated as violations rather than alternatives.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__partition_reading, expansionist_territorial_claimants, excluded,
    organized, biographical, constrained, regional).

% Act as final arbiters of recognition and enforcement. They can validate or withhold recognition, impose sanctions, or support international legal findings. They observe and adjudicate between competing legitimacy claims from outside the immediate territorial dispute.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__partition_reading, major_power_arbitrators, observer,
    institutional, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a multilateral framework for state recognition in contested territory, converting competing national claims into bounded sovereign entities with defined borders, thereby attempting to prevent broader regional war through mutual diplomatic recognition and UN membership.
% TRANSFER_FUNCTION: Transfers territorial sovereignty and international legal standing from pre-existing non-state communities and continuous habitation claims to two defined state entities, while transferring the costs of displacement, exile, and border enforcement to refugee and stateless populations.
% ABSENT_VOICES: Indigenous continuity claimants who view 1948 as catastrophic dispossession rather than legitimate partition, and expansionist claimants who reject the 1967 boundaries, are structurally excluded from UN recognition forums. Their objections are routed through non-state violence or marginal diplomatic channels rather than the legal framework itself.
% DISAPPEARANCE_RATIONALE: If the partition framework vanished overnight, mutual recognition between the two state entities would collapse, diplomatic relations would revert to zero-sum territorial contest, regional alliances would realign around irredentist claims, and the UN's territorial arbitration role in the region would dissolve into contested historical narratives.
% FOUNDING_PROBLEM: Competing national claims over Mandatory Palestine risked escalating into sustained regional warfare and colonial power vacuum; a supervised partition was constructed to create two viable states and terminate the mandate.
% FOUNDING_PROBLEM_CORROBORATION: The UN Special Committee on Palestine (UNSCOP) attested the problem in 1947 from outside the benefiting parties. However, Palestinian representatives and indigenous continuity advocates attest that the 'problem' was framed by colonial powers to favor Zionist statehood and that binational or democratic frameworks were excluded. Post-colonial scholars and major-power archival research corroborate the excluded-alternatives reading from outside the beneficiary set.
narrative_ontology:disappearance_verdict(territorial_legitimacy__partition_reading, world_rearranges).
narrative_ontology:founding_problem_status(territorial_legitimacy__partition_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(territorial_legitimacy__partition_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(territorial_legitimacy__partition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(territorial_legitimacy__partition_reading, 0.7, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(territorial_legitimacy__partition_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(territorial_legitimacy__partition_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(territorial_legitimacy__partition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.70 at interval end) is substantial because the partition framework legally assigned sovereignty to defined state actors while extinguishing the territorial claims of continuous inhabitants and refugees â a transfer that persists across generations. Suppression requirement peaked at 0.82 as enforcement shifted from post-partition military consolidation to prolonged occupation and blockade. Theater ratio (0.68) reflects Goodhart drift: UN resolutions and diplomatic process have become performative substitutes for actual territorial partition, especially after 1993. Resistance (0.80) is high because excluded parties mount persistent violent and diplomatic rejection. Accessibility collapse is moderate (0.42): alternatives (binationalism, single democratic state, indigenous title) are institutionally marginalized but have not disappeared on the ground. The claimed type is tangled_rope because the constraint carries a genuine coordination function (preventing all-out war via mutual recognition) alongside clear asymmetric extraction (refugee dispossession, border enforcement).
 *
 * PERSPECTIVAL GAP:
 *   The UN institutional seat and the recognized state seats experience the constraint as a necessary legal architecture for international order. The refugee and displaced seats experience the same framework as a permanent, legally sanctioned dispossession. The engine computes this divergence from identical structural data through directionality: beneficiaries (states, UN) sit near the low-d end while targets (refugees) sit near the high-d end with trapped exit.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations (israeli_state_recognized, palestinian_national_authority) feed low directionality: they gain sovereignty and diplomatic standing from the framework. Victim declarations (palestinian_refugees_1948, internally_displaced_communities) feed high directionality: they lose property, return rights, and political status. The UN partition institutions are agenda-setters with constrained exit but still benefit from the framework's existence, placing them at low-to-moderate d. Major power arbitrators are analytical observers with no extraction. Expansionist claimants are excluded but constrained, receiving moderate d from their structural position outside the legitimacy frame.
 *
 * MANDATROPHY ANALYSIS:
 *   Without the tangled_rope classification, the partition framework could be misread as pure rope (international coordination) â ignoring the Nakba and refugee permanence â or as pure snare (colonial imposition) â ignoring the genuine diplomatic coordination it provides. The R5 genealogy interview forces the question: what problem was this built to solve? The founding problem (competing claims over Palestine) is contested in status, and the divergence between founding_problem_status=contested and disappearance_verdict=world_rearranges signals that the arrangement persists beyond its contested mandate, but not yet as pure inertia because the coordination function is still invoked.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    partition_vs_colonial_imposition,
    'Is UN Resolution 181 a genuinely neutral coordination mechanism for competing claims, or a colonially-framed extraction of territory from continuous indigenous inhabitants?',
    'Historical archival analysis of UNSCOP deliberations and power dynamics; comparative analysis of partition outcomes versus binational or single-state alternatives.',
    'If colonially framed, the partition reading''s authority_grounding shifts from lineage/conventional toward extraction, and its classification shifts toward snare. If neutral coordination, it remains tangled_rope or rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(partition_vs_colonial_imposition, conceptual, 'Whether the partition framework is neutral coordination or colonial imposition').

omega_variable(
    empirical_contingency_of_two_state_solution,
    'Does the partition reading''s foundational axiom require the empirical existence of two viable states, and if so, does persistent failure of Palestinian statehood override the axiom''s holdability?',
    'Observation of whether international legal institutions formally abandon the two-state framework or continue to assert it despite the empirical reality of prolonged occupation and settlement.',
    'If the axiom is treated as empirically contingent and the condition fails, the engine''s axiom_overriding drift path activates, potentially shifting classification toward piton or mandatrophy. If treated as conventional regardless of outcome, the reading remains stable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(empirical_contingency_of_two_state_solution, empirical, 'Whether two-state empirical failure overrides the partition axiom').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(territorial_legitimacy__partition_reading, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(terr_tr_t0, territorial_legitimacy__partition_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(terr_tr_t15, territorial_legitimacy__partition_reading, theater_ratio, 15, 0.25).
narrative_ontology:measurement(terr_tr_t30, territorial_legitimacy__partition_reading, theater_ratio, 30, 0.42).
narrative_ontology:measurement(terr_tr_t45, territorial_legitimacy__partition_reading, theater_ratio, 45, 0.32).
narrative_ontology:measurement(terr_tr_t60, territorial_legitimacy__partition_reading, theater_ratio, 60, 0.55).
narrative_ontology:measurement(terr_tr_t75, territorial_legitimacy__partition_reading, theater_ratio, 75, 0.68).

% Extraction over time
narrative_ontology:measurement(terr_be_t0, territorial_legitimacy__partition_reading, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(terr_be_t15, territorial_legitimacy__partition_reading, base_extractiveness, 15, 0.61).
narrative_ontology:measurement(terr_be_t30, territorial_legitimacy__partition_reading, base_extractiveness, 30, 0.64).
narrative_ontology:measurement(terr_be_t45, territorial_legitimacy__partition_reading, base_extractiveness, 45, 0.62).
narrative_ontology:measurement(terr_be_t60, territorial_legitimacy__partition_reading, base_extractiveness, 60, 0.67).
narrative_ontology:measurement(terr_be_t75, territorial_legitimacy__partition_reading, base_extractiveness, 75, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(terr_su_t0, territorial_legitimacy__partition_reading, suppression_requirement, 0, 0.72).
narrative_ontology:measurement(terr_su_t15, territorial_legitimacy__partition_reading, suppression_requirement, 15, 0.65).
narrative_ontology:measurement(terr_su_t30, territorial_legitimacy__partition_reading, suppression_requirement, 30, 0.78).
narrative_ontology:measurement(terr_su_t45, territorial_legitimacy__partition_reading, suppression_requirement, 45, 0.48).
narrative_ontology:measurement(terr_su_t60, territorial_legitimacy__partition_reading, suppression_requirement, 60, 0.75).
narrative_ontology:measurement(terr_su_t75, territorial_legitimacy__partition_reading, suppression_requirement, 75, 0.82).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(territorial_legitimacy__partition_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(territorial_legitimacy__partition_reading, territorial_legitimacy__indigenous_continuity_reading).
narrative_ontology:affects_constraint(territorial_legitimacy__partition_reading, territorial_legitimacy__security_necessity_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the territorial_legitimacy kernel, which decomposes into structurally distinct claims: partition via international law (this file), indigenous continuity (separate file), and security necessity (separate file). The partition reading's epsilon reflects the extraction inherent in legally assigning sovereignty through external mandate; sibling readings have different epsilon values and victim/beneficiary structures.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
