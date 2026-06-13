% ============================================================================
% CONSTRAINT STORY: jewish_self_determination__diasporist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: jewish_self_determination__diasporist_reading
 *   human_readable: Diasporist Reading: Jewish Self-Determination Through Minority Rights
 *   domain: political_philosophy/nationalism_studies/postcolonial_theory
 *
 * SUMMARY:
 *   This constraint embodies the diasporist reading of Jewish
 *   self-determination: the claim that Jewish collective survival and
 *   flourishing are best secured through diaspora pluralism and
 *   minority-rights frameworks rather than territorial sovereignty, and that
 *   Zionism represents a dangerous deviation tying Jewish fate to militarized
 *   state power. This is ONE reading of the contested kernel
 *   'jewish_self_determination,' not an evaluation of whether the reading is
 *   true or superior. The constraint describes how this reading operates as a
 *   normative and institutional force—what it demands, who benefits, who
 *   bears costs, and how it persists despite institutional pressure. The
 *   reading is classified as piton because diasporist institutions have
 *   atrophied relative to Zionist hegemony: the reading retains intellectual
 *   coherence and is articulated by intellectuals and scholars, but
 *   institutional capacity, resource access, and community acceptance have
 *   declined, leaving diasporism largely performative within mainstream
 *   Jewish organizational life. The structure that once sustained diaspora
 *   Jewish institutional autonomy (autonomous communal institutions,
 *   legitimacy narratives centered on minority integration) has been
 *   substantially displaced by Zionist institutional framing.
 *
 * KEY AGENTS:
 *   - diaspora_jewish_communities — direct beneficiaries of diasporist legitimacy (organizational level, strategic position)
 *   - coerced_zionist_adherents — victims; bear identity-lock costs from Zionist monopoly (individual and biographical level, high suppression)
 *   - diaspora_jews_endangered_by_state_actions — victims; bear material and reputational costs from Israeli state actions they do not control (individual and global level, immediate time horizon)
 *   - zionist_institutional_apparatus — agenda-setter; controls resource allocation, institutional gatekeeping, and definition of legitimate Jewish discourse (organizational and institutional level)
 *   - diasporist_intellectuals — beneficiaries of legitimacy but victims of marginalization (intellectual and professional level, constrained exit)
 *   - host_states_and_liberal_frameworks — beneficiaries; diasporism vindicates pluralist model (institutional level, analytical exit)
 *   - palestinian_political_movements — excluded; have highest material stakes but no seat in the diasporist-Zionist debate (organizational and regional level, structural exclusion)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_self_determination__diasporist_reading, 0.62).
domain_priors:suppression_score(jewish_self_determination__diasporist_reading, 0.71).
domain_priors:theater_ratio(jewish_self_determination__diasporist_reading, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_self_determination__diasporist_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(jewish_self_determination__diasporist_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(jewish_self_determination__diasporist_reading, theater_ratio, 0.68).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_self_determination__diasporist_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(jewish_self_determination__diasporist_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_self_determination__diasporist_reading, piton).
narrative_ontology:human_readable(jewish_self_determination__diasporist_reading, "Diasporist Reading: Jewish Self-Determination Through Minority Rights").
narrative_ontology:topic_domain(jewish_self_determination__diasporist_reading, "political_philosophy/nationalism_studies/postcolonial_theory").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_self_determination__diasporist_reading, 'ce128cb7-8aea-4771-996a-8cc10a01b3cf').
narrative_ontology:cs_kernel_codification('ce128cb7-8aea-4771-996a-8cc10a01b3cf', distributed).
narrative_ontology:cs_authority_grounding('ce128cb7-8aea-4771-996a-8cc10a01b3cf', distributed).
narrative_ontology:cs_reading_relation('ce128cb7-8aea-4771-996a-8cc10a01b3cf', jewish_self_determination__liberal_nationalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('ce128cb7-8aea-4771-996a-8cc10a01b3cf', jewish_self_determination__indigenous_return_reading, forecloses).
narrative_ontology:cs_reading_relation('ce128cb7-8aea-4771-996a-8cc10a01b3cf', jewish_self_determination__religious_covenant_reading, forecloses).
narrative_ontology:cs_reading_relation('ce128cb7-8aea-4771-996a-8cc10a01b3cf', jewish_self_determination__settler_colonial_reading, influences).
narrative_ontology:cs_axiom('ce128cb7-8aea-4771-996a-8cc10a01b3cf', foundational, diaspora_pluralism_sustainable).
narrative_ontology:cs_axiom_status(diaspora_pluralism_sustainable, holdable).
narrative_ontology:cs_axiom_grounding('ce128cb7-8aea-4771-996a-8cc10a01b3cf', diaspora_pluralism_sustainable, empirically_contingent).
narrative_ontology:cs_axiom('ce128cb7-8aea-4771-996a-8cc10a01b3cf', foundational, territorial_sovereignty_unnecessary_for_jewish_flourishing).
narrative_ontology:cs_axiom_status(territorial_sovereignty_unnecessary_for_jewish_flourishing, holdable).
narrative_ontology:cs_axiom_grounding('ce128cb7-8aea-4771-996a-8cc10a01b3cf', territorial_sovereignty_unnecessary_for_jewish_flourishing, empirically_contingent).
narrative_ontology:cs_axiom('ce128cb7-8aea-4771-996a-8cc10a01b3cf', secondary, zionism_endangers_diaspora_jews).
narrative_ontology:cs_axiom_status(zionism_endangers_diaspora_jews, holdable).
narrative_ontology:cs_axiom_grounding('ce128cb7-8aea-4771-996a-8cc10a01b3cf', zionism_endangers_diaspora_jews, empirically_contingent).
narrative_ontology:cs_reference_frame('ce128cb7-8aea-4771-996a-8cc10a01b3cf', diaspora_autonomous_jewish_institutions).
narrative_ontology:cs_drift_state('ce128cb7-8aea-4771-996a-8cc10a01b3cf', contemporary_zionist_hegemony, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('ce128cb7-8aea-4771-996a-8cc10a01b3cf', '').
narrative_ontology:cs_kernel_id(jewish_self_determination__diasporist_reading, jewish_self_determination).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_self_determination__diasporist_reading, diaspora_jewish_communities).
narrative_ontology:constraint_victim(jewish_self_determination__diasporist_reading, coerced_zionist_adherents).
narrative_ontology:constraint_victim(jewish_self_determination__diasporist_reading, diaspora_jews_endangered_by_state_actions).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_self_determination__diasporist_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(jewish_self_determination__diasporist_reading, 'none', 1).

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
 *   Extractiveness is moderate (0.62 at end interval) because the constraint's operation extracts from coerced adherents and endangered diaspora Jews, but diasporist institutions do not collect concentrated rents—they collect legitimacy and institutional access, both of which have declined. The high theater_ratio (0.68 at interval end) reflects the piton diagnosis: diasporist activity persists (intellectuals write, some institutions maintain diaspora frameworks) but the primary function has atrophied. What remains is largely performative—articulation of an alternative that lacks the institutional power to enforce itself. Suppression is high (0.71) because maintaining Zionist institutional hegemony requires active gatekeeping: institutional funding allocated away from diasporist voices, academic positions closed to diasporist intellectuals, community membership and belonging contingent on accepting Zionist framing. The measurement series spans 1880–2024 to capture the constraint's trajectory from viable intellectual alternative (pre-1920s) through institutional displacement (1948 onwards) to contemporary atrophy (2024). Theater ratio rises sharply after 1948 (the Israeli state's founding) because diasporist activity shifts from institutional governance to intellectual articulation. Suppression requirement rises to maintain the constraint against growing resistance from endangered diaspora Jews and Palestinian movements seeking to challenge Zionist framing.
 *
 * PERSPECTIVAL GAP:
 *   The agenda_setter (Zionist apparatus) and the payer seats (coerced adherents, endangered Jews) should compute different constraint types from the same structural facts. The apparatus perceives institutional discipline; the payer seats perceive coercion. This divergence IS the signal the measurement exists to capture—not an error, but the feature. Do not attempt to reconcile the claim (piton) to the metrics (suppression 0.71, extractiveness 0.62) by tuning either. The high suppression and theater_ratio relative to extractiveness is exactly the piton signature: work is being done to maintain something (theater), and that work is extractive from the payers, but the constraint collects no concentrated benefit (hence piton rather than snare).
 *
 * DIRECTIONALITY LOGIC:
 *   Diaspora_jewish_communities and host_states hold directionality near beneficiary (d low): they gain legitimacy, institutional coherence, and safe minority positioning from diasporism. Coerced_zionist_adherents and endangered_diaspora_jews hold directionality near target (d high): they bear identity-lock costs and material endangerment as the price of suppressing diasporist alternatives. The apparatus holds directionality near symmetric-to-target (d 0.4–0.6): it benefits from institutional control and ideological hegemony but pays through constant suppression labor and reputational vulnerability (diasporist and Palestinian critiques of Zionism gain traction as Israeli actions generate visible costs). Diasporist_intellectuals hold d near symmetric (0.5) because they gain legitimacy and coherence from the constraint but pay through marginalization and institutional exclusion—a genuinely dual position. The exclusion of Palestinian movements creates a structural asymmetry: they would have the highest d (nearest full target) if included, but their exclusion prevents the constraint's operation from computing their position at all.
 *
 * MANDATROPHY ANALYSIS:
 *   The diasporist constraint exhibits the piton pattern: atrophied primary function (institutional autonomy and legitimacy for diaspora Jewish life) maintained largely through performance and institutional gatekeeping. The founding problem (can diaspora Jews flourish under minority-rights frameworks?) has substantial empirical resolution in North American and Western European contexts—pluralist minority protection has demonstrably worked for Jewish communities in those jurisdictions. Yet the constraint persists not because it solves an active problem but because Zionist institutional apparatus has subordinated diasporist alternatives and made Zionist affiliation a prerequisite for institutional access, professional standing, and communal belonging. The mandatrophy is contested because Zionist interpreters deny it: they argue the problem persists (ongoing anti-Semitism, Palestinian political hostility) and therefore Zionist territorial sovereignty remains necessary. Diasporist interpreters argue the problem is substantially solved in pluralist contexts but the arrangement persists as institutional power consolidation. The T17 abductive trigger (mountain_extraction_accumulation) does not apply—this is not a claimed mountain. The S17 pattern match (piton signature: rising theater_ratio, stable-to-declining extractiveness, high suppression) fires strongly: theater_ratio rises from 0.05 (1880) to 0.68 (2024); suppression rises from 0.08 to 0.71; extractiveness rises from 0.15 to 0.62 but plateaus after 1990 (indicating the constraint is no longer expanding its primary function, only maintaining it). This is consistent with piton diagnosis: institutional resources flow toward the constraint's maintenance, not toward growth in what it produces.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_diaspora,
    'Is the contemporary dominance of Zionism in Jewish institutional life a natural or inevitable result of Jewish experience, or a constructed hegemony that suppressed viable diasporist alternatives?',
    'Historical analysis of counterfactual scenarios in which Zionist movement did not consolidate hegemonic control (e.g., post-1920s Jewish communal governance under alternative institutional arrangements; alternative responses to Holocaust that did not center state sovereignty). Testimony from diasporist intellectuals about what conditions would have enabled institutional sustainability.',
    'If dominance is natural law, Zionism is a mountain and diasporism is a piton by structural necessity. If dominance is constructed, Zionism is a snare or tangled_rope with diasporism as a suppressed coordinate. The classification of both constraints depends on this resolution.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_diaspora, conceptual, 'Whether Zionist hegemony follows from historical necessity or from institutional power consolidation.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the suppression of diasporist alternatives structurally imposed (external gatekeeping, institutional exclusion) or internalized (Jewish communities have genuinely come to believe Zionism is the only politically coherent Jewish position)?',
    'Post-suppression trajectories: if Zionist institutional suppression were removed (e.g., in a hypothetical alternative institutional ecology), would diasporist frameworks re-emerge vigorously or remain marginalized? Survey data on Jewish attitudes toward Zionism when diasporist alternatives are actively presented.',
    'If suppression is structural only, removing the apparatus would restore diasporism. If suppression is internalized, diasporist alternatives would require re-education and institutional rebuilding. If both, the constraint carries forward the internalized suppression even if external apparatus is removed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Whether measured suppression is external gatekeeping or internalized belief formation.').

omega_variable(
    identity_fusion_mechanisms,
    'What specific mechanisms bind Jewish identity to Zionist commitment such that exiting Zionism feels like self-erasure? Is it ideological (Zionism as the only legitimate Jewish political response), relational (Zionist institutions control access to Jewish community and belonging), or institutional (professional and educational pathways require Zionist affiliation)?',
    'Narrative analysis from Jews who have exited Zionist frameworks; documentation of institutional pathways that require or reward Zionist commitment; analysis of Jewish educational curricula and their treatment of diasporism.',
    'Different mechanisms require different remedies. Ideological fusion might dissolve with exposure to diasporist intellectual frameworks. Relational fusion requires alternative institutional structures. Institutional fusion requires professional and educational access outside Zionist gatekeeping. Understanding the mechanism is prerequisite to understanding whether identity-locked exit is possible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_fusion_mechanisms, empirical, 'Specific mechanisms binding Jewish identity to Zionist commitment.').

omega_variable(
    colonial_continuity_of_diaspora_framework,
    'Does the diasporist framework itself depend on colonial-era assumptions about host-state capacity and willingness to accommodate Jewish minorities? Is the framework viable in a post-colonial or post-liberal era?',
    'Analysis of diasporist success rates in pluralist contexts (North America, Western Europe) vs. non-pluralist or post-pluralist contexts. Evaluation of whether liberal minority-rights frameworks are eroding or strengthening globally.',
    'If diasporism depends on liberal pluralism and pluralism is declining, diasporism might be atrophying not from Zionist suppression but from structural obsolescence. If pluralism is stable or strengthening, diasporism remains viable and its atrophy is genuinely suppressive.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(colonial_continuity_of_diaspora_framework, conceptual, 'Whether diasporist framework is dependent on liberal-pluralist conditions that may be declining.').

omega_variable(
    kernel_reading_contestation,
    'Is the diasporist reading a genuinely holdable alternative to Zionist readings within Jewish tradition, or does it represent a fundamental rupture with Jewish political self-understanding?',
    'Textual and historical analysis of diaspora Jewish thought (medieval and modern philosophy, legal-interpretive traditions); documentation of periods in which diasporist frameworks were institutionally dominant; evaluation of whether diasporism is continuous with Jewish intellectual history or a modern invention.',
    'If diasporism is continuous with Jewish tradition, it is foreclosed by neither religious nor nationalist readings and remains a viable reading throughout Jewish history. If it is a modern rupture, its atrophy might reflect a genuine incompatibility with Jewish political self-understanding rather than suppression. The relation between this reading and religious_covenant_reading would shift from ''forecloses'' to ''influenced by'' or ''partly continuous with.''',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Whether diasporist reading is continuous with Jewish intellectual history or represents modern rupture.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_self_determination__diasporist_reading, 1880, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jewi_tr_t1880, jewish_self_determination__diasporist_reading, theater_ratio, 1880, 0.05).
narrative_ontology:measurement(jewi_tr_t1920, jewish_self_determination__diasporist_reading, theater_ratio, 1920, 0.12).
narrative_ontology:measurement(jewi_tr_t1948, jewish_self_determination__diasporist_reading, theater_ratio, 1948, 0.28).
narrative_ontology:measurement(jewi_tr_t1967, jewish_self_determination__diasporist_reading, theater_ratio, 1967, 0.45).
narrative_ontology:measurement(jewi_tr_t1990, jewish_self_determination__diasporist_reading, theater_ratio, 1990, 0.62).
narrative_ontology:measurement(jewi_tr_t2010, jewish_self_determination__diasporist_reading, theater_ratio, 2010, 0.67).
narrative_ontology:measurement(jewi_tr_t2024, jewish_self_determination__diasporist_reading, theater_ratio, 2024, 0.68).

% Extraction over time
narrative_ontology:measurement(jewi_be_t1880, jewish_self_determination__diasporist_reading, base_extractiveness, 1880, 0.15).
narrative_ontology:measurement(jewi_be_t1920, jewish_self_determination__diasporist_reading, base_extractiveness, 1920, 0.22).
narrative_ontology:measurement(jewi_be_t1948, jewish_self_determination__diasporist_reading, base_extractiveness, 1948, 0.38).
narrative_ontology:measurement(jewi_be_t1967, jewish_self_determination__diasporist_reading, base_extractiveness, 1967, 0.51).
narrative_ontology:measurement(jewi_be_t1990, jewish_self_determination__diasporist_reading, base_extractiveness, 1990, 0.58).
narrative_ontology:measurement(jewi_be_t2010, jewish_self_determination__diasporist_reading, base_extractiveness, 2010, 0.61).
narrative_ontology:measurement(jewi_be_t2024, jewish_self_determination__diasporist_reading, base_extractiveness, 2024, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(jewi_su_t1880, jewish_self_determination__diasporist_reading, suppression_requirement, 1880, 0.08).
narrative_ontology:measurement(jewi_su_t1920, jewish_self_determination__diasporist_reading, suppression_requirement, 1920, 0.15).
narrative_ontology:measurement(jewi_su_t1948, jewish_self_determination__diasporist_reading, suppression_requirement, 1948, 0.35).
narrative_ontology:measurement(jewi_su_t1967, jewish_self_determination__diasporist_reading, suppression_requirement, 1967, 0.52).
narrative_ontology:measurement(jewi_su_t1990, jewish_self_determination__diasporist_reading, suppression_requirement, 1990, 0.65).
narrative_ontology:measurement(jewi_su_t2010, jewish_self_determination__diasporist_reading, suppression_requirement, 2010, 0.69).
narrative_ontology:measurement(jewi_su_t2024, jewish_self_determination__diasporist_reading, suppression_requirement, 2024, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_self_determination__diasporist_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(jewish_self_determination__diasporist_reading, 0.12).
narrative_ontology:affects_constraint(jewish_self_determination__diasporist_reading, jewish_self_determination__liberal_nationalist_reading).
narrative_ontology:affects_constraint(jewish_self_determination__diasporist_reading, jewish_self_determination__indigenous_return_reading).
narrative_ontology:affects_constraint(jewish_self_determination__diasporist_reading, jewish_self_determination__religious_covenant_reading).
narrative_ontology:affects_constraint(jewish_self_determination__diasporist_reading, jewish_self_determination__settler_colonial_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of a five-way contested kernel (jewish_self_determination). The ε values across all five readings differ substantially: diasporist_reading (moderate extraction from coerced adherents + endangered Jews, suppressed by apparatus) vs. liberal_nationalist_reading (lower extraction, genuine coordination function) vs. indigenous_return_reading (natural law framing, minimal suppression) vs. religious_covenant_reading (natural law framing, theological grounding) vs. settler_colonial_reading (high extraction, colonial structure). Each reading is a separate constraint with distinct beneficiary/victim structures. The readings coexist across different communities and institutional locations; they are not serial historical phases. Network edges track how changes in one reading's legitimacy propagate to others (e.g., if indigenous_return reading gains epistemic credibility, it influences diasporist_reading by creating an alternative nationalist framing; if settler_colonial_reading gains institutional power, it forecloses or severely constrains liberal_nationalist_reading's viability).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(jewish_self_determination__diasporist_reading, moderate, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
