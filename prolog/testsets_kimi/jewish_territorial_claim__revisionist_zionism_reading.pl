% ============================================================================
% CONSTRAINT STORY: jewish_territorial_claim__revisionist_zionism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jewish_territorial_claim__revisionist_zionism_reading, []).

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
    narrative_ontology:stakeholder_gain_flow/2,
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
 *   constraint_id: jewish_territorial_claim__revisionist_zionism_reading
 *   human_readable: Revisionist Zionist Maximalist Territorial Claim (Both Banks)
 *   domain: political/historical
 *
 * SUMMARY:
 *   This constraint instantiates the revisionist_zionism_reading of the
 *   jewish_territorial_claim kernel. It describes the maximalist claim to
 *   sovereignty over both banks of the Jordan River, advanced by Revisionist
 *   Zionist movements from the 1920s onward. The claim explicitly rejects
 *   Arab political consent as a prerequisite for Jewish sovereignty,
 *   substituting what Jabotinsky termed an 'Iron Wall' of overwhelming
 *   military force to compel Arab acceptance. The constraint coordinates
 *   Jewish settlers and institutions around an irredentist territorial
 *   program while extracting land, autonomy, and security from the indigenous
 *   Palestinian Arab population through active military enforcement.
 *
 * KEY AGENTS:
 *   - revisionist_zionist_leadership: Agenda-setter (organized/generational/identity_locked) â formalizes the maximalist program and directs the Iron Wall strategy through political and paramilitary institutions.
 *   - jewish_maximalist_settlers: Beneficiary (moderate/generational/constrained) â receives territorial allocation, armed protection, and sovereign privileges under the claim.
 *   - palestinian_arab_inhabitants: Payer (powerless/generational/trapped) â bears dispossession, political exclusion, and military subjection as the indigenous population of the claimed territory.
 *   - british_mandatory_authority: Observer (institutional/biographical/mobile) â mandatory sovereign balancing between commitments, neither direct beneficiary nor payer of the maximalist claim.
 *   - neighboring_arab_states: Excluded (institutional/generational/constrained) â affected by displacement and territorial loss but structurally excluded from the constraint's internal decision-making.
 *   - international_community: Observer (institutional/generational/analytical) â monitors through League and UN structures without direct cost or benefit from the territorial arrangement.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_territorial_claim__revisionist_zionism_reading, 0.82).
domain_priors:suppression_score(jewish_territorial_claim__revisionist_zionism_reading, 0.88).
domain_priors:theater_ratio(jewish_territorial_claim__revisionist_zionism_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_territorial_claim__revisionist_zionism_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(jewish_territorial_claim__revisionist_zionism_reading, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(jewish_territorial_claim__revisionist_zionism_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_territorial_claim__revisionist_zionism_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(jewish_territorial_claim__revisionist_zionism_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_territorial_claim__revisionist_zionism_reading, tangled_rope).
narrative_ontology:human_readable(jewish_territorial_claim__revisionist_zionism_reading, "Revisionist Zionist Maximalist Territorial Claim (Both Banks)").
narrative_ontology:topic_domain(jewish_territorial_claim__revisionist_zionism_reading, "political/historical").

domain_priors:requires_active_enforcement(jewish_territorial_claim__revisionist_zionism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_territorial_claim__revisionist_zionism_reading, '6834f358-94a0-4c02-b5c9-761887daa00a').
narrative_ontology:cs_kernel_codification('6834f358-94a0-4c02-b5c9-761887daa00a', formalized).
narrative_ontology:cs_authority_grounding('6834f358-94a0-4c02-b5c9-761887daa00a', lineage).
narrative_ontology:cs_interpretation_layer_present('6834f358-94a0-4c02-b5c9-761887daa00a').
narrative_ontology:cs_reading_relation('6834f358-94a0-4c02-b5c9-761887daa00a', jewish_territorial_claim__political_zionism_reading, influences).
narrative_ontology:cs_reading_relation('6834f358-94a0-4c02-b5c9-761887daa00a', jewish_territorial_claim__labor_zionism_reading, coexists_with).
narrative_ontology:cs_reading_relation('6834f358-94a0-4c02-b5c9-761887daa00a', jewish_territorial_claim__cultural_zionism_reading, coexists_with).
narrative_ontology:cs_axiom('6834f358-94a0-4c02-b5c9-761887daa00a', foundational, territorial_maximalism_irreducible).
narrative_ontology:cs_axiom_status(territorial_maximalism_irreducible, holdable).
narrative_ontology:cs_axiom_grounding('6834f358-94a0-4c02-b5c9-761887daa00a', territorial_maximalism_irreducible, deontological).
narrative_ontology:cs_axiom('6834f358-94a0-4c02-b5c9-761887daa00a', foundational, military_supremacy_prerequisite_for_acceptance).
narrative_ontology:cs_axiom_status(military_supremacy_prerequisite_for_acceptance, holdable).
narrative_ontology:cs_axiom_grounding('6834f358-94a0-4c02-b5c9-761887daa00a', military_supremacy_prerequisite_for_acceptance, empirically_contingent).
narrative_ontology:cs_reference_frame('6834f358-94a0-4c02-b5c9-761887daa00a', maximalist_historical_territorial_integrity).
narrative_ontology:cs_drift_state('6834f358-94a0-4c02-b5c9-761887daa00a', post_mandate_partition_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('6834f358-94a0-4c02-b5c9-761887daa00a', '').
narrative_ontology:cs_kernel_id(jewish_territorial_claim__revisionist_zionism_reading, jewish_territorial_claim).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_territorial_claim__revisionist_zionism_reading, jewish_maximalist_settlers).
narrative_ontology:constraint_victim(jewish_territorial_claim__revisionist_zionism_reading, palestinian_arab_inhabitants).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Formalizes and advocates the maximalist territorial program through political parties and paramilitary organizations. Their political identity and institutional trajectory are constituted by the irredentist claim to both banks of the Jordan. They design the Iron Wall strategy and direct settlement expansion and military pressure.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__revisionist_zionism_reading, revisionist_zionist_leadership, agenda_setter,
    organized, generational, identity_locked, regional).

% Receive land allocation, armed protection, and sovereign privileges under the maximalist claim. Their presence on both banks of the Jordan is subsidized and secured by the enforcement apparatus. Exit means abandoning the territorial project and the economic and social networks built within it.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__revisionist_zionism_reading, jewish_maximalist_settlers, beneficiary,
    moderate, generational, constrained, regional).

% Bear the costs of territorial dispossession, political exclusion, and military subjugation. Their villages and agricultural lands fall within the claimed territory, and they are denied sovereign self-determination. As the indigenous population, geographic exit means refugee displacement rather than free movement.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__revisionist_zionism_reading, palestinian_arab_inhabitants, payer,
    powerless, generational, trapped, local).

% Holds formal sovereignty under the League of Nations Mandate. Attempts to balance Zionist settlement with Arab political demands through policy documents and military garrisons. Neither a beneficiary of the maximalist claim nor its primary target, but its administrative and military infrastructure is co-opted by and intermittently resists the constraint.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__revisionist_zionism_reading, british_mandatory_authority, observer,
    institutional, biographical, mobile, global).

% Directly affected by the territorial claim and population displacement but structurally excluded from the constraint's internal decision-making. Their diplomatic protests are overridden by the mandatory power and Zionist military facts on the ground.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__revisionist_zionism_reading, neighboring_arab_states, excluded,
    institutional, generational, constrained, regional).

% Observes through League of Nations and later UN commissions. Issues reports that intermittently critique the maximalist claim but lacks enforcement capacity to alter the constraint's operation. Does not directly bear costs or receive benefits from the territorial arrangement.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__revisionist_zionism_reading, international_community, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jewish_territorial_claim__revisionist_zionism_reading, jewish_maximalist_settlers).
narrative_ontology:fixing_cost_class(jewish_territorial_claim__revisionist_zionism_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates Jewish national sovereignty over the full claimed historical territory when Arab consent is absent, substituting unilateral military force and settlement for negotiated political agreement.
% TRANSFER_FUNCTION: Moves territorial control, settlement opportunity, and political sovereignty from Palestinian Arab inhabitants to Jewish maximalist settlers and institutions, enforced by military superiority.
% ABSENT_VOICES: Palestinian Arab political leadership demanding sovereign equality, anti-colonial international jurists questioning unilateral territorial acquisition by force, and binationalist Jewish dissidents are structurally excluded; the framework presupposes their non-consent and substitutes compulsion for negotiation.
% DISAPPEARANCE_RATIONALE: If the maximalist claim and Iron Wall enforcement vanished overnight, Jewish settlement patterns would face restored Arab political autonomy across both banks, territorial control would revert to demographic majorities, and the revisionist state-building project would lose its coercive foundation â the regional political architecture would reorganize entirely.
% FOUNDING_PROBLEM: The Jewish national project in Palestine confronts an Arab demographic and political majority that opposes Jewish sovereignty over the full territorial scope; without overwhelming military force, the maximalist territorial claim is unattainable.
% FOUNDING_PROBLEM_CORROBORATION: Revisionist Zionist theorists attest the founding problem from within the beneficiary tradition. Palestinian Arab leadership, British mandatory officials, and anti-colonial observers attest that the 'problem' is manufactured settler-colonial expansion rather than an objective coordination deficit; they document Arab opposition as political resistance to colonization, not a natural obstacle to inevitable sovereignty. Corroboration from outside the benefiting parties supports the contested reading.
narrative_ontology:disappearance_verdict(jewish_territorial_claim__revisionist_zionism_reading, world_rearranges).
narrative_ontology:founding_problem_status(jewish_territorial_claim__revisionist_zionism_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_territorial_claim__revisionist_zionism_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(jewish_territorial_claim__revisionist_zionism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jewish_territorial_claim__revisionist_zionism_reading, 0.82, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jewish_territorial_claim__revisionist_zionism_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jewish_territorial_claim__revisionist_zionism_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jewish_territorial_claim__revisionist_zionism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.82) is high because the constraint transfers territorial control and political autonomy unilaterally from Arab inhabitants to Jewish settlers. Suppression (0.88) is higher still because the constraint's persistence depends on actively suppressing Arab political alternatives and military resistance. Theater_ratio (0.45) is moderate: settlement and military display carry symbolic performative weight, but the underlying enforcement is functionally coercive. Accessibility_collapse (0.78) is high for Arab alternatives â partition, autonomy, and binational frameworks collapse under the military supremacy doctrine. Resistance (0.80) is high, documented by the 1936-39 Arab revolt and ongoing opposition. The metrics are authored independently from the tangled_rope claim; if the engine computes a snare classification, that divergence is the intended signal.
 *
 * PERSPECTIVAL GAP:
 *   From the Jewish beneficiary seats, the constraint coordinates national existence on the full ancestral territory against hostile opposition; from the Arab payer seat, it operates as settler-colonial extraction enforced by military violence. The British observer seat experiences the constraint as an unworkable imperial balancing act. The engine computes these divergent per-seat classifications from the same structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Revisionist leadership and settlers are structural beneficiaries (low directionality; the constraint subsidizes their territorial and political project). Palestinian Arabs are structural targets (high directionality near 1.0; the constraint extracts land and sovereignty from them). The British seat sits near symmetric: it bears administrative costs and violence but gains no territorial benefit. Neighboring Arab states are excluded rather than coordinated.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification prevents mislabeling this constraint as pure coordination (rope) by requiring declared victims and active enforcement. Without these structural declarations, one might see Jewish national territorial coordination as benign mutual benefit; the presence of identified payers and suppression mechanisms forces the extraction dimension into the classification. Conversely, it prevents mislabeling as pure snare by acknowledging that a genuine coordination function (Jewish national territorial integrity) is structurally coupled with the extraction, not merely a cover story.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_vs_extraction_cover,
    'Does the Iron Wall constraint solve a genuine collective-action problem for Jewish national coordination, or is the coordination narrative entirely cover for territorial extraction from Palestinian Arabs?',
    'Comparative historical analysis of whether alternative coordination mechanisms (federation, binationalism, territorial compromise) were structurally viable at the time or were actively suppressed by the same actors who advanced the maximalist claim.',
    'If coordination is genuine and irreplaceable, the classification remains tangled_rope; if the coordination story is cover, the constraint reclassifies toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_vs_extraction_cover, conceptual, 'Ambiguity between genuine coordination function and extraction cover story').

omega_variable(
    consent_prerequisite_constructedness,
    'Is the rejection of Arab consent as a prerequisite a response to an empirically inevitable Arab refusal, or a constructed political position that precludes consent-based alternatives?',
    'Archival analysis of negotiations and proposals from 1919 to 1947 to determine whether Arab parties offered viable consensual frameworks that revisionist leadership systematically rejected, versus whether Arab opposition was total and non-negotiable.',
    'If consent was constructively excluded rather than empirically impossible, the constraint''s suppression and extractiveness metrics understate its voluntaristic asymmetry.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consent_prerequisite_constructedness, empirical, 'Whether Arab consent was empirically impossible or politically excluded').

omega_variable(
    british_role_enforcement_ambiguity,
    'Does the British mandatory authority function as an external observer, a passive enabler, or an active co-enforcer of the territorial constraint?',
    'Archival and administrative-history analysis of British military collaboration with Zionist paramilitaries versus British suppression of both Arab revolt and Zionist excesses.',
    'Reclassifying British role from observer to co-enforcer would increase the constraint''s institutional scope and alter the directionality derivation for the mandatory seat.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(british_role_enforcement_ambiguity, empirical, 'Ambiguity in British mandatory authority''s structural relationship to the constraint').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_territorial_claim__revisionist_zionism_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jewi_tr_t0, jewish_territorial_claim__revisionist_zionism_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(jewi_tr_t5, jewish_territorial_claim__revisionist_zionism_reading, theater_ratio, 5, 0.3).
narrative_ontology:measurement(jewi_tr_t10, jewish_territorial_claim__revisionist_zionism_reading, theater_ratio, 10, 0.35).
narrative_ontology:measurement(jewi_tr_t15, jewish_territorial_claim__revisionist_zionism_reading, theater_ratio, 15, 0.42).
narrative_ontology:measurement(jewi_tr_t20, jewish_territorial_claim__revisionist_zionism_reading, theater_ratio, 20, 0.4).
narrative_ontology:measurement(jewi_tr_t25, jewish_territorial_claim__revisionist_zionism_reading, theater_ratio, 25, 0.45).

% Extraction over time
narrative_ontology:measurement(jewi_be_t0, jewish_territorial_claim__revisionist_zionism_reading, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(jewi_be_t5, jewish_territorial_claim__revisionist_zionism_reading, base_extractiveness, 5, 0.66).
narrative_ontology:measurement(jewi_be_t10, jewish_territorial_claim__revisionist_zionism_reading, base_extractiveness, 10, 0.73).
narrative_ontology:measurement(jewi_be_t15, jewish_territorial_claim__revisionist_zionism_reading, base_extractiveness, 15, 0.8).
narrative_ontology:measurement(jewi_be_t20, jewish_territorial_claim__revisionist_zionism_reading, base_extractiveness, 20, 0.82).
narrative_ontology:measurement(jewi_be_t25, jewish_territorial_claim__revisionist_zionism_reading, base_extractiveness, 25, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(jewi_su_t0, jewish_territorial_claim__revisionist_zionism_reading, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(jewi_su_t5, jewish_territorial_claim__revisionist_zionism_reading, suppression_requirement, 5, 0.75).
narrative_ontology:measurement(jewi_su_t10, jewish_territorial_claim__revisionist_zionism_reading, suppression_requirement, 10, 0.82).
narrative_ontology:measurement(jewi_su_t15, jewish_territorial_claim__revisionist_zionism_reading, suppression_requirement, 15, 0.88).
narrative_ontology:measurement(jewi_su_t20, jewish_territorial_claim__revisionist_zionism_reading, suppression_requirement, 20, 0.85).
narrative_ontology:measurement(jewi_su_t25, jewish_territorial_claim__revisionist_zionism_reading, suppression_requirement, 25, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


% DUAL FORMULATION NOTE:
% This constraint is one reading of the jewish_territorial_claim kernel, decomposed per the epsilon-invariance principle from sibling readings (political_zionism_reading, labor_zionism_reading, cultural_zionism_reading) because the epsilon values and structural relationships differ across readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
