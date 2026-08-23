% ============================================================================
% CONSTRAINT STORY: jewish_self_determination__diasporist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jewish_self_determination__diasporist_reading, []).

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
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: jewish_self_determination__diasporist_reading
 *   human_readable: Atrophied Diasporist Alternative to Zionist Hegemony
 *   domain: political/nationalism/postcolonial
 *
 * SUMMARY:
 *   The diasporist reading of the jewish_self_determination kernel contests
 *   the hegemonic equation of Jewish survival with territorial sovereignty.
 *   It identifies the diasporist institutional alternative not as a live
 *   political option but as an atrophied piton: a residue of pre-state Jewish
 *   political autonomy that persists through inertia and theatrical
 *   maintenance by diaspora communal leadership, while Zionist institutions
 *   suppress its revival. The constraint extracts political agency from Jews
 *   coerced into the militarized state framework and endangers diaspora
 *   communities by associating them with state violence. This is one reading
 *   of a contested kernel; sibling readings include liberal nationalist,
 *   indigenous return, religious covenant, and settler colonial framings.
 *
 * KEY AGENTS:
 *   - Diaspora Jewish communities: Diffuse beneficiaries of cultural continuity, politically constrained by Zionist hegemony (organized/constrained/global)
 *   - Jews coerced into Zionism: Primary targets bearing militarization costs and identity fusion (moderate/identity_locked/national)
 *   - Diaspora communal leadership: Agenda-setters administering the atrophied institutional framework without capturing extraction (moderate/constrained/global)
 *   - Host state governments: Analytical observers setting minority-rights frameworks (institutional/analytical/national)
 *   - Anti-Zionist Jewish dissidents: Excluded voices offering non-statist alternatives (moderate/constrained/global)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_self_determination__diasporist_reading, 0.52).
domain_priors:suppression_score(jewish_self_determination__diasporist_reading, 0.61).
domain_priors:theater_ratio(jewish_self_determination__diasporist_reading, 0.74).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_self_determination__diasporist_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(jewish_self_determination__diasporist_reading, suppression_requirement, 0.61).
narrative_ontology:constraint_metric(jewish_self_determination__diasporist_reading, theater_ratio, 0.74).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_self_determination__diasporist_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(jewish_self_determination__diasporist_reading, resistance, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_self_determination__diasporist_reading, piton).
narrative_ontology:human_readable(jewish_self_determination__diasporist_reading, "Atrophied Diasporist Alternative to Zionist Hegemony").
narrative_ontology:topic_domain(jewish_self_determination__diasporist_reading, "political/nationalism/postcolonial").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_self_determination__diasporist_reading, 'de62b212-7eab-43bf-9895-0fbaa7d15c85').
narrative_ontology:cs_kernel_codification('de62b212-7eab-43bf-9895-0fbaa7d15c85', distributed).
narrative_ontology:cs_authority_grounding('de62b212-7eab-43bf-9895-0fbaa7d15c85', distributed).
narrative_ontology:cs_reading_relation('de62b212-7eab-43bf-9895-0fbaa7d15c85', jewish_self_determination__liberal_nationalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('de62b212-7eab-43bf-9895-0fbaa7d15c85', jewish_self_determination__indigenous_return_reading, influences).
narrative_ontology:cs_reading_relation('de62b212-7eab-43bf-9895-0fbaa7d15c85', jewish_self_determination__settler_colonial_reading, coexists_with).
narrative_ontology:cs_reading_relation('de62b212-7eab-43bf-9895-0fbaa7d15c85', jewish_self_determination__religious_covenant_reading, forecloses).
narrative_ontology:cs_axiom('de62b212-7eab-43bf-9895-0fbaa7d15c85', foundational, jewish_flourishing_through_diaspora_pluralism).
narrative_ontology:cs_axiom_status(jewish_flourishing_through_diaspora_pluralism, holdable).
narrative_ontology:cs_axiom_grounding('de62b212-7eab-43bf-9895-0fbaa7d15c85', jewish_flourishing_through_diaspora_pluralism, empirically_contingent).
narrative_ontology:cs_axiom('de62b212-7eab-43bf-9895-0fbaa7d15c85', foundational, territorial_sovereignty_as_deviation).
narrative_ontology:cs_axiom_status(territorial_sovereignty_as_deviation, holdable).
narrative_ontology:cs_axiom_grounding('de62b212-7eab-43bf-9895-0fbaa7d15c85', territorial_sovereignty_as_deviation, instrumental).
narrative_ontology:cs_reference_frame('de62b212-7eab-43bf-9895-0fbaa7d15c85', diaspora_autonomous_political_existence).
narrative_ontology:cs_drift_state('de62b212-7eab-43bf-9895-0fbaa7d15c85', contemporary_post_1967_era, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('de62b212-7eab-43bf-9895-0fbaa7d15c85', '').
narrative_ontology:cs_kernel_id(jewish_self_determination__diasporist_reading, jewish_self_determination).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_self_determination__diasporist_reading, diaspora_jewish_communities).
narrative_ontology:constraint_victim(jewish_self_determination__diasporist_reading, jews_coerced_into_zionism).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain distinct religious, cultural, and linguistic identities across host societies without territorial sovereignty. Depend on communal institutions and host-state minority-rights frameworks for continuity. Their non-Zionist political expression is institutionally marginalized by the hegemonic framing of Jewish collective interests as identical with Israeli state policy, yet they continue to receive diffuse cultural benefits from diaspora institutional life.
narrative_ontology:constraint_stakeholder(jewish_self_determination__diasporist_reading, diaspora_jewish_communities, beneficiary,
    organized, generational, constrained, global).

% Conscripted into military service, taxed for settlement and defense expenditure, and socially pressured to embody the Zionist state project. Their Jewish identity is fused with citizenship, militarized nationalism, and state security discourse. Exit requires emigration and often severance from family networks, or internal ostracism as traitors or self-hating Jews.
narrative_ontology:constraint_stakeholder(jewish_self_determination__diasporist_reading, jews_coerced_into_zionism, payer,
    moderate, biographical, identity_locked, national).

% Administer Jewish communal life through federations, religious courts, cultural bodies, and philanthropic structures. They perform representation of Jewish interests to host governments but have largely accepted the subordination of diaspora political agency to Zionist sovereignty frameworks. They maintain the inherited institutional forms of pre-state Jewish autonomy without sovereign content or independent foreign-policy voice.
narrative_ontology:constraint_stakeholder(jewish_self_determination__diasporist_reading, diaspora_communal_leadership, agenda_setter,
    moderate, generational, constrained, global).

% Set the legal and policy frameworks for minority rights, religious freedom, and anti-discrimination that determine the actual conditions of diaspora Jewish survival. They are not direct parties to the intra-Jewish contest but their immigration, security, and hate-speech policies shape whether diaspora pluralism is structurally viable.
narrative_ontology:constraint_stakeholder(jewish_self_determination__diasporist_reading, host_state_governments, observer,
    institutional, generational, analytical, national).

% Organize explicitly against Zionist hegemony from within Jewish identity, advocating for non-statist, diasporist, or binational alternatives. They are structurally excluded from mainstream communal funding mechanisms, denied representative platforms, and labeled as fringe or dangerous by the dominant institutional apparatus.
narrative_ontology:constraint_stakeholder(jewish_self_determination__diasporist_reading, anti_zionist_jewish_dissidents, excluded,
    moderate, biographical, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jewish_self_determination__diasporist_reading, diffuse).
narrative_ontology:fixing_cost_class(jewish_self_determination__diasporist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Originally coordinated Jewish survival across dispersed host societies through communal autonomy (kehillot), rabbinic courts, mutual aid networks, and lateral solidarity without centralized territorial sovereignty.
% TRANSFER_FUNCTION: Transfers political agency, diplomatic representation, and collective identity-formation capacity from diaspora Jewish communities and coerced Jews into a Zionist territorial-sovereignty framework, while leaving diaspora institutions as theatrical residues that perform Jewish continuity without autonomous political efficacy.
% ABSENT_VOICES: Anti-Zionist Jewish dissidents and non-statist Jewish religious traditions (Haredi anti-Zionist streams, Jewish anarchists, binationalists) are structurally excluded from communal funding, media platforms, and representative bodies; they would argue for a revived diasporist politics but are denied institutional voice and labeled illegitimate.
% DISAPPEARANCE_RATIONALE: If the atrophied diaspora institutional framework vanished entirely, Jewish collective life would reorganize around either full assimilation into host societies or total absorption into the Zionist state project. The current equilibrium, in which diaspora institutions provide a performative non-sovereign alternative, would collapse and force a binary political choice.
% FOUNDING_PROBLEM: Jewish survival in dispersion without sovereign territory, relying on host-state tolerance, communal self-governance, and transnational solidarity networks.
% FOUNDING_PROBLEM_CORROBORATION: Jewish historians of the medieval and early modern periods attest to the viability of diaspora communal autonomy. Postcolonial and minority-rights legal scholars outside the Jewish communal beneficiary structure corroborate that diaspora existence was politically generative, not merely precarious. Zionist historiography contests this reading, claiming the founding problem was never solved by diaspora autonomy. No uncontested external corroborator exists because the question is enmeshed in the Israeli-Palestinian conflict.
narrative_ontology:disappearance_verdict(jewish_self_determination__diasporist_reading, world_rearranges).
narrative_ontology:founding_problem_status(jewish_self_determination__diasporist_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_self_determination__diasporist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(jewish_self_determination__diasporist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jewish_self_determination__diasporist_reading, 0.52, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jewish_self_determination__diasporist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jewish_self_determination__diasporist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jewish_self_determination__diasporist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.52) is moderate because the atrophied framework channels political agency away from diaspora self-determination without delivering robust sovereign protection; it neither fully extracts nor fully coordinates. Suppression (0.61) reflects the active marginalization of anti-Zionist Jewish voices and the institutional foreclosure of non-statist alternatives. Theater ratio (0.74) is high: diaspora institutions perform Jewish political representation and continuity without autonomous efficacy, while Zionist hegemony performs 'Jewish unity' that masks the underlying atrophy. Accessibility collapse (0.48) indicates alternatives are thinkable but institutionally weak. Resistance (0.42) captures growing but marginalized anti-Zionist Jewish organizing.
 *
 * PERSPECTIVAL GAP:
 *   The diaspora communities experience the constraint as diffuse cultural continuity with suppressed political horizons; the coerced Jews experience it as direct militarized extraction and endangerment. The communal leadership experiences it as inherited institutional duty and organizational inertia. The engine computes these seats differently: the diffuse beneficiary seat (diaspora communities) sits near symmetric-to-beneficiary directionality, while the identity-locked target seat (coerced Jews) sits near full-target directionality.
 *
 * DIRECTIONALITY LOGIC:
 *   Diaspora Jewish communities are declared beneficiaries because they receive diffuse cultural continuity from the piton's persistence, but they do not capture concentrated extraction; their directionality is low. Jews coerced into Zionism are the declared victims, identity-locked into the militarized state framework; their directionality is high. The diaspora communal leadership administers the framework without meaningful profit, sitting near the middle. Host states are analytical observers with no stake in the extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The diasporist reading prevents mislabeling by distinguishing the original coordination function (diaspora communal autonomy) from its current atrophied state. Without this genealogy, the framework might classify Zionist hegemony as a snare (if concentrated beneficiaries are assumed) or as a mountain (if Jewish statehood is treated as an inevitable historical endpoint). The piton classification captures that the original diasporist function has decayed; what persists is theatrical maintenance, institutional inertia, and the suppression of revival by an external hegemonic structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    diaspora_viability_vs_host_tolerance,
    'Is diaspora Jewish survival structurally viable without the Zionist state as a backstop, or does it depend on exceptional host-state tolerance that cannot be generalized?',
    'Comparative historical analysis of Jewish diaspora communities under varying host-state regimes; assessment of whether Zionist statehood increased or decreased diaspora security over time.',
    'If diaspora survival is broadly viable, the Zionist monopoly is revealed as suppressing a genuine alternative, raising effective extraction; if not, the diaspora framework was always a piton masking dependency.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(diaspora_viability_vs_host_tolerance, empirical, 'Whether diaspora Jewish existence is structurally viable or exceptional.').

omega_variable(
    coercion_vs_inertia_in_zionist_hegemony,
    'Does Zionist hegemony over Jewish identity persist primarily through active suppression of alternatives or through institutional inertia and identity fusion?',
    'Measure institutional funding flows, platform exclusion, and social sanctions against anti-Zionist Jews versus passive demographic and ideological shifts.',
    'If active suppression dominates, the external constraint on diaspora revival is better classified as tangled_rope or snare; if inertia dominates, the piton classification of the diaspora framework holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coercion_vs_inertia_in_zionist_hegemony, conceptual, 'Active suppression versus institutional inertia as the persistence mechanism.').

omega_variable(
    zionist_beneficiary_concentration,
    'Is there a concentrated beneficiary of the Zionist hegemony over Jewish self-determination, or is the extraction truly diffuse?',
    'Trace resource flows, military manpower extraction, diplomatic capital, and institutional capture to identify specific capturer seats.',
    'If concentrated beneficiaries exist (e.g., Israeli state security apparatus, settlement sector), the broader constraint is a snare rather than a piton; if extraction is diffuse across Jewish populations, the piton classification is supported.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(zionist_beneficiary_concentration, empirical, 'Whether extraction from the diaspora piton is captured or diffuse.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_self_determination__diasporist_reading, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jewi_tr_t0, jewish_self_determination__diasporist_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(jewi_tr_t10, jewish_self_determination__diasporist_reading, theater_ratio, 10, 0.4).
narrative_ontology:measurement(jewi_tr_t20, jewish_self_determination__diasporist_reading, theater_ratio, 20, 0.52).
narrative_ontology:measurement(jewi_tr_t30, jewish_self_determination__diasporist_reading, theater_ratio, 30, 0.61).
narrative_ontology:measurement(jewi_tr_t40, jewish_self_determination__diasporist_reading, theater_ratio, 40, 0.68).
narrative_ontology:measurement(jewi_tr_t50, jewish_self_determination__diasporist_reading, theater_ratio, 50, 0.72).
narrative_ontology:measurement(jewi_tr_t60, jewish_self_determination__diasporist_reading, theater_ratio, 60, 0.75).
narrative_ontology:measurement(jewi_tr_t70, jewish_self_determination__diasporist_reading, theater_ratio, 70, 0.74).

% Extraction over time
narrative_ontology:measurement(jewi_be_t0, jewish_self_determination__diasporist_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(jewi_be_t10, jewish_self_determination__diasporist_reading, base_extractiveness, 10, 0.38).
narrative_ontology:measurement(jewi_be_t20, jewish_self_determination__diasporist_reading, base_extractiveness, 20, 0.44).
narrative_ontology:measurement(jewi_be_t30, jewish_self_determination__diasporist_reading, base_extractiveness, 30, 0.49).
narrative_ontology:measurement(jewi_be_t40, jewish_self_determination__diasporist_reading, base_extractiveness, 40, 0.51).
narrative_ontology:measurement(jewi_be_t50, jewish_self_determination__diasporist_reading, base_extractiveness, 50, 0.52).
narrative_ontology:measurement(jewi_be_t60, jewish_self_determination__diasporist_reading, base_extractiveness, 60, 0.53).
narrative_ontology:measurement(jewi_be_t70, jewish_self_determination__diasporist_reading, base_extractiveness, 70, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(jewi_su_t0, jewish_self_determination__diasporist_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(jewi_su_t10, jewish_self_determination__diasporist_reading, suppression_requirement, 10, 0.55).
narrative_ontology:measurement(jewi_su_t20, jewish_self_determination__diasporist_reading, suppression_requirement, 20, 0.62).
narrative_ontology:measurement(jewi_su_t30, jewish_self_determination__diasporist_reading, suppression_requirement, 30, 0.68).
narrative_ontology:measurement(jewi_su_t40, jewish_self_determination__diasporist_reading, suppression_requirement, 40, 0.7).
narrative_ontology:measurement(jewi_su_t50, jewish_self_determination__diasporist_reading, suppression_requirement, 50, 0.69).
narrative_ontology:measurement(jewi_su_t60, jewish_self_determination__diasporist_reading, suppression_requirement, 60, 0.68).
narrative_ontology:measurement(jewi_su_t70, jewish_self_determination__diasporist_reading, suppression_requirement, 70, 0.67).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_self_determination__diasporist_reading, identity_coordination).

% DUAL FORMULATION NOTE:
% This constraint is one member of the jewish_self_determination kernel family. The kernel decomposes into multiple structurally distinct constraints because the sibling readings (liberal nationalist, indigenous return, religious covenant, settler colonial) assign different epsilon values, beneficiary/victim structures, and types to the same historical-political object. Each reading should be authored as a separate constraint story and linked via affects_constraints when sibling files are generated.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
