% ============================================================================
% CONSTRAINT STORY: jewish_self_determination__indigenous_return_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jewish_self_determination__indigenous_return_reading, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: jewish_self_determination__indigenous_return_reading
 *   human_readable: Jewish Indigeneity and Unbroken Connection Reading
 *   domain: political/nationalism/postcolonial
 *
 * SUMMARY:
 *   This constraint instantiates the indigenous_return_reading of the
 *   jewish_self_determination kernel. It asserts that Jewish people are
 *   indigenous to the Land of Israel/Palestine through an unbroken historical
 *   and cultural connection, thereby framing Zionism as a project of
 *   decolonization rather than colonization. The reading treats indigenous
 *   status as a discoverable historical factâstructurally claiming mountain
 *   statusâwhile operating within a contested political field where it
 *   coordinates Jewish territorial claims against competing Palestinian
 *   indigeneity narratives. The authored metrics reflect high contestation
 *   and active discursive enforcement, producing deliberate divergence from
 *   the mountain claim.
 *
 * KEY AGENTS:
 *   - jewish_claimants_to_ancestral_land: Primary beneficiary (organized/generational/identity_locked) â receives legitimacy and territorial priority from the indigenous framing
 *   - zionist_advocacy_institutions: Agenda setter (institutional/generational/constrained) â produces and enforces the narrative infrastructure
 *   - palestinian_communities: Excluded and structurally targeted (powerless/generational/trapped) â competing claims reframed as subordinate
 *   - critical_postcolonial_scholars: Analytical observer (organized/generational/analytical) â evaluates the discursive structure from outside
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_self_determination__indigenous_return_reading, 0.84).
domain_priors:suppression_score(jewish_self_determination__indigenous_return_reading, 0.68).
domain_priors:theater_ratio(jewish_self_determination__indigenous_return_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_self_determination__indigenous_return_reading, extractiveness, 0.84).
narrative_ontology:constraint_metric(jewish_self_determination__indigenous_return_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(jewish_self_determination__indigenous_return_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_self_determination__indigenous_return_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(jewish_self_determination__indigenous_return_reading, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_self_determination__indigenous_return_reading, mountain).
narrative_ontology:human_readable(jewish_self_determination__indigenous_return_reading, "Jewish Indigeneity and Unbroken Connection Reading").
narrative_ontology:topic_domain(jewish_self_determination__indigenous_return_reading, "political/nationalism/postcolonial").

domain_priors:emerges_naturally(jewish_self_determination__indigenous_return_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_self_determination__indigenous_return_reading, '47768ea0-8514-4245-b8d4-8e1793f9d48e').
narrative_ontology:cs_kernel_codification('47768ea0-8514-4245-b8d4-8e1793f9d48e', distributed).
narrative_ontology:cs_authority_grounding('47768ea0-8514-4245-b8d4-8e1793f9d48e', lineage).
narrative_ontology:cs_interpretation_layer_present('47768ea0-8514-4245-b8d4-8e1793f9d48e').
narrative_ontology:cs_reading_relation('47768ea0-8514-4245-b8d4-8e1793f9d48e', jewish_self_determination__diasporist_reading, coexists_with).
narrative_ontology:cs_reading_relation('47768ea0-8514-4245-b8d4-8e1793f9d48e', jewish_self_determination__liberal_nationalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('47768ea0-8514-4245-b8d4-8e1793f9d48e', jewish_self_determination__religious_covenant_reading, coexists_with).
narrative_ontology:cs_reading_relation('47768ea0-8514-4245-b8d4-8e1793f9d48e', jewish_self_determination__settler_colonial_reading, forecloses).
narrative_ontology:cs_axiom('47768ea0-8514-4245-b8d4-8e1793f9d48e', foundational, unbroken_indigenous_connection_to_land).
narrative_ontology:cs_axiom_status(unbroken_indigenous_connection_to_land, holdable).
narrative_ontology:cs_axiom_grounding('47768ea0-8514-4245-b8d4-8e1793f9d48e', unbroken_indigenous_connection_to_land, empirically_contingent).
narrative_ontology:cs_axiom('47768ea0-8514-4245-b8d4-8e1793f9d48e', foundational, indigenous_right_to_territorial_return).
narrative_ontology:cs_axiom_status(indigenous_right_to_territorial_return, holdable).
narrative_ontology:cs_axiom_grounding('47768ea0-8514-4245-b8d4-8e1793f9d48e', indigenous_right_to_territorial_return, deontological).
narrative_ontology:cs_reference_frame('47768ea0-8514-4245-b8d4-8e1793f9d48e', unbroken_indigenous_connection).
narrative_ontology:cs_drift_state('47768ea0-8514-4245-b8d4-8e1793f9d48e', contemporary_postcolonial_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('47768ea0-8514-4245-b8d4-8e1793f9d48e', '').
narrative_ontology:cs_kernel_id(jewish_self_determination__indigenous_return_reading, jewish_self_determination).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_self_determination__indigenous_return_reading, jewish_claimants_to_ancestral_land).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Their collective identity and territorial claims are vindicated by the unbroken indigenous connection narrative; the constraint positions their return as decolonization and grants prima facie legitimacy to territorial self-determination, binding identity to land across diaspora communities.
narrative_ontology:constraint_stakeholder(jewish_self_determination__indigenous_return_reading, jewish_claimants_to_ancestral_land, beneficiary,
    organized, generational, identity_locked, global).

% Produce, circulate, and defend historical and archaeological narratives establishing unbroken connection; set the discursive agenda in legal, educational, and diplomatic arenas and organize collective mobilization around the indigenous return frame.
narrative_ontology:constraint_stakeholder(jewish_self_determination__indigenous_return_reading, zionist_advocacy_institutions, agenda_setter,
    institutional, generational, constrained, global).

% Their competing territorial and indigenous claims are reframed within this reading as later arrival, co-indigenous with subordinate status, or non-indigenous; they are structurally excluded from the beneficiary position and their claims are subordinated to the Jewish ancestral return frame.
narrative_ontology:constraint_stakeholder(jewish_self_determination__indigenous_return_reading, palestinian_communities, excluded,
    powerless, generational, trapped, regional).

% Analyze the indigenous return reading as a discursive strategy leveraging postcolonial legitimacy categories to recast territorial acquisition; they evaluate structural effects without benefiting from or bearing direct costs of the constraint.
narrative_ontology:constraint_stakeholder(jewish_self_determination__indigenous_return_reading, critical_postcolonial_scholars, observer,
    organized, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a legitimating framework that coordinates Jewish collective territorial claims across diaspora communities by grounding them in an unbroken indigenous connection, converting diverse historical narratives into a unified political entitlement under postcolonial normative standards.
% TRANSFER_FUNCTION: Transfers legitimacy, territorial priority, and postcolonial moral authority to Jewish claimants by framing Zionism as decolonization rather than colonization, while subordinating or reframing competing Palestinian claims to a secondary or later-arriving status.
% ABSENT_VOICES: Palestinian communities whose competing indigenous claims are reframed as subordinate; anti-Zionist Jewish voices who reject territorial sovereignty as central to Jewish fate; and diasporist scholars who argue Jewish flourishing does not require territorial return.
% DISAPPEARANCE_RATIONALE: If the unbroken indigenous connection claim disappeared, the decolonization framing of Zionism would collapse; Jewish territorial claims would lose their primary postcolonial legitimacy anchor and would need to be rejustified through liberal-nationalist or religious-covenant frames, significantly rearranging the discursive and legal architecture of the conflict.
% FOUNDING_PROBLEM: The problem of Jewish statelessness and diaspora vulnerability in the nineteenth and twentieth centuries, compounded by the need to establish territorial sovereignty in a context of rising anti-colonial normative ascendancy and competing national claims.
% FOUNDING_PROBLEM_CORROBORATION: Zionist institutional advocates attest the founding problem remains live, citing ongoing antisemitism and security threats. Palestinian communities and diasporist scholars attest the problem is reframed to justify territorial extraction. Critical postcolonial and international legal scholars outside the beneficiary set acknowledge historical vulnerability but dispute whether the indigenous return frame is the necessary or appropriate resolution. No outside consensus corroborates the indigenous framing as the only valid solution.
narrative_ontology:disappearance_verdict(jewish_self_determination__indigenous_return_reading, world_rearranges).
narrative_ontology:founding_problem_status(jewish_self_determination__indigenous_return_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_self_determination__indigenous_return_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(jewish_self_determination__indigenous_return_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jewish_self_determination__indigenous_return_reading, 0.84, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jewish_self_determination__indigenous_return_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jewish_self_determination__indigenous_return_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(jewish_self_determination__indigenous_return_reading, ExtMetricName, E),
    domain_priors:suppression_score(jewish_self_determination__indigenous_return_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(jewish_self_determination__indigenous_return_reading),
    narrative_ontology:constraint_metric(jewish_self_determination__indigenous_return_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(jewish_self_determination__indigenous_return_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(jewish_self_determination__indigenous_return_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.84) is high because the contested classification of indigeneity functions as a zero-sum legitimating mechanism: granting indigenous status to one party structurally subordinates competing claims. Suppression (0.68) reflects the active discursive enforcement required to maintain the unbroken-connection narrative against historical critique and competing Palestinian claims. Theater_ratio (0.58) is moderate-high: while historical scholarship is genuine, a substantial portion of narrative maintenance is performativeâdirected at securing political legitimacy under postcolonial normative frameworks rather than purely descriptive historical inquiry. Accessibility_collapse (0.72) is high within the reading's framework (alternative claims are reframed as colonial or late-arriving), though alternatives persist externally. Resistance (0.78) is high due to sustained contestation from Palestinian communities, postcolonial scholars, and the settler-colonial reading. The temporal series shows extraction and theater rising over the interval as postcolonial discourse intensified and the indigenous frame became more strategically central to legitimation.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary seat (Jewish claimants) experiences the constraint as a discovered historical truth that vindicates collective identity and territorial aspiration; the excluded Palestinian seat experiences the same structure as an erasure and subordination of their competing claims. The agenda-setting institutions experience it as a necessary coordination mechanism for national self-determination; analytical observers see a contested kernel reading whose mountain claim is maintained by performative enforcement. The engine computes this divergence from the structural dataâclaimed mountain, authored high extraction and resistance.
 *
 * DIRECTIONALITY LOGIC:
 *   Jewish claimants are declared beneficiaries, deriving low directionality and subsidy from the constraint. Palestinian communities are structurally targetedâtheir claims are extracted from and subordinatedâbut are not declared victims in this reading's framework (they are reframed as later arrivals or co-indigenous with subordinate status). A directionality override for the powerless atom raises d to 0.9 to reflect this structural targeting without violating the reading's own victim-null framework. Zionist institutions sit near the beneficiary end as agenda-setters, though their exit is constrained by institutional identity-lock.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents mislabeling by distinguishing the reading's self-presentation (historical fact/mountain) from its operational structure (contested legitimating frame requiring active enforcement). If the founding problem (Jewish statelessness/diaspora vulnerability) were dead but the arrangement persisted purely by inertia, it would read as piton; however, the active enforcement and rising extraction indicate ongoing coordination and extraction rather than atrophy. The FSM pathway captures the false-summit risk: a mountain claim with declared beneficiaries and high extraction is flagged for reclassification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    indigenous_status_natural_or_constructed,
    'Is Jewish indigeneity to the land a discoverable historical fact independent of political framework, or a constructed classification that benefits specific claimants?',
    'Comparative indigenous rights jurisprudence and independent archaeological and historical consensus assessing continuity versus rupture.',
    'If constructed, the constraint is a false summit and FSM triggers reclassification toward tangled_rope or snare; if natural, it remains mountain despite beneficiary presence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(indigenous_status_natural_or_constructed, conceptual, 'Natural law versus political construction of indigeneity claim').

omega_variable(
    committer_reading_boundary,
    'Which structural element differentiates the indigenous_return_reading from its siblingsâempirical continuity, normative framing, or beneficiary structure?',
    'Cross-reading comparison of holdable versus overridden axioms and which premises are shared across the kernel family.',
    'Determines whether the kernel is a family of irreconcilable positions or variations on a shared commitment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_reading_boundary, conceptual, 'Structural boundary between kernel readings').

omega_variable(
    unbroken_connection_empirical_status,
    'Does the historical record support an unbroken Jewish connection to the land sufficient to ground indigenous status under international frameworks?',
    'Independent historical, archaeological, and demographic review assessing continuity, rupture, and demographic change.',
    'If the empirical claim fails, the mountain classification collapses and the constraint reclassifies as rope or tangled_rope depending on enforcement patterns; if sustained, it resists FSM reclassification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(unbroken_connection_empirical_status, empirical, 'Empirical status of unbroken connection claim').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_self_determination__indigenous_return_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jewi_tr_t0, jewish_self_determination__indigenous_return_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(jewi_tr_t20, jewish_self_determination__indigenous_return_reading, theater_ratio, 20, 0.22).
narrative_ontology:measurement(jewi_tr_t40, jewish_self_determination__indigenous_return_reading, theater_ratio, 40, 0.32).
narrative_ontology:measurement(jewi_tr_t60, jewish_self_determination__indigenous_return_reading, theater_ratio, 60, 0.42).
narrative_ontology:measurement(jewi_tr_t80, jewish_self_determination__indigenous_return_reading, theater_ratio, 80, 0.52).
narrative_ontology:measurement(jewi_tr_t100, jewish_self_determination__indigenous_return_reading, theater_ratio, 100, 0.6).

% Extraction over time
narrative_ontology:measurement(jewi_be_t0, jewish_self_determination__indigenous_return_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(jewi_be_t20, jewish_self_determination__indigenous_return_reading, base_extractiveness, 20, 0.48).
narrative_ontology:measurement(jewi_be_t40, jewish_self_determination__indigenous_return_reading, base_extractiveness, 40, 0.6).
narrative_ontology:measurement(jewi_be_t60, jewish_self_determination__indigenous_return_reading, base_extractiveness, 60, 0.7).
narrative_ontology:measurement(jewi_be_t80, jewish_self_determination__indigenous_return_reading, base_extractiveness, 80, 0.78).
narrative_ontology:measurement(jewi_be_t100, jewish_self_determination__indigenous_return_reading, base_extractiveness, 100, 0.84).

% Suppression requirement over time
narrative_ontology:measurement(jewi_su_t0, jewish_self_determination__indigenous_return_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(jewi_su_t20, jewish_self_determination__indigenous_return_reading, suppression_requirement, 20, 0.38).
narrative_ontology:measurement(jewi_su_t40, jewish_self_determination__indigenous_return_reading, suppression_requirement, 40, 0.5).
narrative_ontology:measurement(jewi_su_t60, jewish_self_determination__indigenous_return_reading, suppression_requirement, 60, 0.6).
narrative_ontology:measurement(jewi_su_t80, jewish_self_determination__indigenous_return_reading, suppression_requirement, 80, 0.7).
narrative_ontology:measurement(jewi_su_t100, jewish_self_determination__indigenous_return_reading, suppression_requirement, 100, 0.76).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(jewish_self_determination__indigenous_return_reading, powerless, 0.9).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
