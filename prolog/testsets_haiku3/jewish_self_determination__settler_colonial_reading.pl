% ============================================================================
% CONSTRAINT STORY: jewish_self_determination__settler_colonial_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jewish_self_determination__settler_colonial_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: jewish_self_determination__settler_colonial_reading
 *   human_readable: Zionist Settler-Colonial Dispossession (Settler-Colonial Reading)
 *   domain: political_philosophy/postcolonial_theory
 *
 * SUMMARY:
 *   This constraint instantiates one reading of a deeply contested kernel:
 *   the establishment and maintenance of Jewish statehood and settlements in
 *   Palestinian territory from the late 19th century onward. In the
 *   settler-colonial reading presented here, Zionism is framed as a
 *   European-origin political movement that solved European Jewish
 *   vulnerability by displacing and subordinating an indigenous Palestinian
 *   population through systematic violence, legal exclusion, and demographic
 *   replacement. The constraint's operation involves military occupation,
 *   settlement expansion, land appropriation via law, differential legal
 *   status (Law of Return), and suppression of Palestinian resistance. This
 *   reading contests the liberal nationalist frame (equal national
 *   self-determination claim), the indigenous-return frame (Jewish presence
 *   as restoration not colonization), the religious-covenant frame (divine
 *   territorial claim), and the diasporist frame (diaspora as safer
 *   collective strategy). Each reading instantiates a different constraint
 *   with different ε, beneficiaries, victims, and claimed types. This JSON
 *   represents ONLY the settler-colonial reading as a clean, ε-invariant
 *   constraint. Sibling readings are other constraints, documented
 *   separately, linked via network relationships and reading_relations
 *   axioms.
 *
 * KEY AGENTS:
 *   - European Jewish settlers (arriving late 19th–20th century): institutional beneficiaries of land acquisition, political sovereignty, demographic majority status
 *   - Israeli state institutions (established 1948, ongoing): agenda-setter enforcing settlement, occupation, legal discrimination through military and administrative apparatus
 *   - Palestinian Arabs (indigenous population): primary victims of displacement, occupation, legal subordination, resource appropriation
 *   - Palestinian refugees (displaced 1948, 1967): victims of indefinite exile and legal exclusion via Law of Return asymmetry
 *   - International Zionist movement (diaspora and institutional): secondary agenda-setter recruiting settlers, securing recognition, mobilizing resources
 *   - Palestinian resistance movements: constrained payers bearing enforcement costs through military suppression
 *   - International legal observers: documenting the constraint's operation but excluded from epistemic control
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_self_determination__settler_colonial_reading, 0.89).
domain_priors:suppression_score(jewish_self_determination__settler_colonial_reading, 0.91).
domain_priors:theater_ratio(jewish_self_determination__settler_colonial_reading, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_self_determination__settler_colonial_reading, extractiveness, 0.89).
narrative_ontology:constraint_metric(jewish_self_determination__settler_colonial_reading, suppression_requirement, 0.91).
narrative_ontology:constraint_metric(jewish_self_determination__settler_colonial_reading, theater_ratio, 0.62).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_self_determination__settler_colonial_reading, accessibility_collapse, 0.87).
narrative_ontology:constraint_metric(jewish_self_determination__settler_colonial_reading, resistance, 0.74).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_self_determination__settler_colonial_reading, snare).
narrative_ontology:human_readable(jewish_self_determination__settler_colonial_reading, "Zionist Settler-Colonial Dispossession (Settler-Colonial Reading)").
narrative_ontology:topic_domain(jewish_self_determination__settler_colonial_reading, "political_philosophy/postcolonial_theory").

domain_priors:requires_active_enforcement(jewish_self_determination__settler_colonial_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_self_determination__settler_colonial_reading, '0e4d7c09-6b80-42e1-a8e0-4c4dbfe4d6c3').
narrative_ontology:cs_kernel_codification('0e4d7c09-6b80-42e1-a8e0-4c4dbfe4d6c3', distributed).
narrative_ontology:cs_authority_grounding('0e4d7c09-6b80-42e1-a8e0-4c4dbfe4d6c3', extraction).
narrative_ontology:cs_interpretation_layer_present('0e4d7c09-6b80-42e1-a8e0-4c4dbfe4d6c3').
narrative_ontology:cs_reading_relation('0e4d7c09-6b80-42e1-a8e0-4c4dbfe4d6c3', jewish_self_determination__liberal_nationalist_reading, forecloses).
narrative_ontology:cs_reading_relation('0e4d7c09-6b80-42e1-a8e0-4c4dbfe4d6c3', jewish_self_determination__indigenous_return_reading, forecloses).
narrative_ontology:cs_reading_relation('0e4d7c09-6b80-42e1-a8e0-4c4dbfe4d6c3', jewish_self_determination__religious_covenant_reading, coexists_with).
narrative_ontology:cs_reading_relation('0e4d7c09-6b80-42e1-a8e0-4c4dbfe4d6c3', jewish_self_determination__diasporist_reading, coexists_with).
narrative_ontology:cs_axiom('0e4d7c09-6b80-42e1-a8e0-4c4dbfe4d6c3', foundational, dispossession_constitutive_not_incidental).
narrative_ontology:cs_axiom_status(dispossession_constitutive_not_incidental, holdable).
narrative_ontology:cs_axiom_grounding('0e4d7c09-6b80-42e1-a8e0-4c4dbfe4d6c3', dispossession_constitutive_not_incidental, empirically_contingent).
narrative_ontology:cs_axiom('0e4d7c09-6b80-42e1-a8e0-4c4dbfe4d6c3', foundational, european_jewish_settlement_displaces_indigenous_palestinians).
narrative_ontology:cs_axiom_status(european_jewish_settlement_displaces_indigenous_palestinians, holdable).
narrative_ontology:cs_axiom_grounding('0e4d7c09-6b80-42e1-a8e0-4c4dbfe4d6c3', european_jewish_settlement_displaces_indigenous_palestinians, empirically_contingent).
narrative_ontology:cs_reference_frame('0e4d7c09-6b80-42e1-a8e0-4c4dbfe4d6c3', indigenous_palestinian_territorial_possession_precolonial).
narrative_ontology:cs_drift_state('0e4d7c09-6b80-42e1-a8e0-4c4dbfe4d6c3', contemporary_occupation_and_settlement_expansion, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('0e4d7c09-6b80-42e1-a8e0-4c4dbfe4d6c3', '').
narrative_ontology:cs_kernel_id(jewish_self_determination__settler_colonial_reading, jewish_self_determination).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_self_determination__settler_colonial_reading, european_jewish_settlers).
narrative_ontology:constraint_beneficiary(jewish_self_determination__settler_colonial_reading, israeli_state_institutions).
narrative_ontology:constraint_victim(jewish_self_determination__settler_colonial_reading, palestinian_arabs).
narrative_ontology:constraint_victim(jewish_self_determination__settler_colonial_reading, indigenous_palestinian_population).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(jewish_self_determination__settler_colonial_reading, international_zionist_movement).
narrative_ontology:constraint_victim(jewish_self_determination__settler_colonial_reading, palestinian_refugees).
narrative_ontology:constraint_victim(jewish_self_determination__settler_colonial_reading, palestinian_resistance_movements).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Arrived in Palestine from late 19th century onward, primarily from Europe, establishing agricultural colonies, urban communities, and institutional structures. Acquired land through purchase and confiscation, established exclusive settlements, and built institutions (political, military, educational, cultural) that secured Jewish demographic and institutional control over territory. Benefited from displacement of Palestinian Arabs through access to land, water, agricultural resources, and political sovereignty structures. Their exit option involves maintaining claims to the territory or relocating to alternative settlements; institutional exit (dismantling Israeli institutions and returning to diaspora) is theoretically possible but organizationally and politically resisted.
narrative_ontology:constraint_stakeholder(jewish_self_determination__settler_colonial_reading, european_jewish_settlers, beneficiary,
    institutional, generational, arbitrage, regional).

% The formal state structure that codifies and enforces the settler-colonial arrangement through military occupation, legal systems (Law of Return, settlement authorization, land appropriation), administrative control, and security apparatus. Establishes and maintains differential legal status between Jewish citizens/settlers and Palestinian residents/non-citizens. Administers the confiscation of Palestinian land, water rights, and resources. Justifies these mechanisms as security, national self-determination, and demographic necessity. Controls the coercive apparatus that enforces the arrangement.
narrative_ontology:constraint_stakeholder(jewish_self_determination__settler_colonial_reading, israeli_state_institutions, agenda_setter,
    institutional, generational, arbitrage, regional).

% Indigenous population of Palestine whose land was progressively appropriated beginning in the late 19th century and accelerating after 1948 and 1967. Subject to displacement (nakba), occupation, military law in the West Bank, siege conditions in Gaza, legal exclusion via Law of Return (which privileges Jewish immigration while denying Palestinian refugees' right of return), restricted access to land and water, economic marginalization, and systematic violence. Their options are constrained: internal displacement to remaining Palestinian territories, legal subordination within Israel (Palestinian citizens), or exile. Leaving Palestinian territory means abandonment of land claims and family ties; staying means living under military occupation or legal subordination.
narrative_ontology:constraint_stakeholder(jewish_self_determination__settler_colonial_reading, palestinian_arabs, payer,
    powerless, generational, trapped, regional).

% Palestinians displaced during 1948 nakba and subsequent conflicts, living in refugee camps in Jordan, Lebanon, Syria, and Palestine territories, or scattered globally in diaspora. Legally barred from returning to homes and land via the Law of Return asymmetry (which admits Jewish diaspora but denies Palestinian refugees). Bear the cost of indefinite displacement, loss of property, legal statelessness, and generational trauma. Exit options are restricted: formal resettlement in third countries is limited; return to former homes is structurally prohibited by the constraint itself.
narrative_ontology:constraint_stakeholder(jewish_self_determination__settler_colonial_reading, palestinian_refugees, payer,
    powerless, generational, trapped, global).

% The transnational political and institutional movement that articulated the settler-colonial project, recruited settlers from diaspora communities, secured international diplomatic recognition, mobilized financial resources, and continues to shape state policy. Operates through political parties, cultural organizations, diaspora fundraising networks, and international advocacy. Frames the constraint as legitimate self-determination and historical justice; benefits from the territorial sovereignty and institutional power the constraint establishes.
narrative_ontology:constraint_stakeholder(jewish_self_determination__settler_colonial_reading, international_zionist_movement, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(jewish_self_determination__settler_colonial_reading, international_zionist_movement, agenda_setter).

% Organized Palestinian political and armed factions resisting displacement and occupation. Bear the cost of enforcement through military suppression, legal persecution, detention, and assassination. Their resistance is constrained by asymmetric military power (Israeli state military vs. Palestinian armed groups) and international law enforcement against Palestinian resistance while protecting Israeli state violence. Exit from resistance means accepting the constraint; continued resistance means exposure to state retaliation.
narrative_ontology:constraint_stakeholder(jewish_self_determination__settler_colonial_reading, palestinian_resistance_movements, payer,
    moderate, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(jewish_self_determination__settler_colonial_reading, palestinian_resistance_movements, observer).

% Human rights organizations, international legal bodies (UN mechanisms, International Court of Justice), and legal scholars that document the constraint's operation. Produce evidence of displacement, settlement expansion, legal discrimination, and military occupation. Their analytical position is constrained by geopolitical interests of states and competing interpretive frames that dispute settler-colonial characterization.
narrative_ontology:constraint_stakeholder(jewish_self_determination__settler_colonial_reading, international_legal_observers, observer,
    institutional, generational, analytical, global).

% Proponents of alternative readings of Jewish self-determination (liberal nationalist, indigenous-return, religious-covenant, diasporist frames) who are structurally excluded from this reading's scope. Their interpretations contest the settler-colonial diagnosis but lack enforcement capacity within this constraint's operation. Would argue for competing framings of legitimacy, justice, and historical claims if admitted to the reading's epistemic framework.
narrative_ontology:constraint_stakeholder(jewish_self_determination__settler_colonial_reading, liberal_nationalist_alternative_framers, excluded,
    institutional, generational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jewish_self_determination__settler_colonial_reading, israeli_state_institutions).
narrative_ontology:fixing_cost_class(jewish_self_determination__settler_colonial_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: No coordination function in this reading: the constraint's stated justifications (security, national self-determination, historical justice) are reframed here as cover stories. The actual function is dispossession and demographic control of territory for exclusive Jewish settlement and statehood.
% TRANSFER_FUNCTION: Transfers land, water resources, political sovereignty, legal status, and security from Palestinian Arabs (dispossessed indigenous population) to Jewish settlers and Israeli state institutions. The transfer is enforced through military occupation, legal appropriation (land laws, settlement authorization), demographic replacement, and military/police suppression of Palestinian resistance.
% ABSENT_VOICES: Palestinian voices are structurally present as the constraint's targets but their political agency is systematically constrained by military occupation and legal subordination. Alternative readings of the kernel (liberal nationalist, indigenous-return, religious-covenant, diasporist frames) are excluded from this reading's scope—their proponents would contest the settler-colonial characterization and offer competing legitimacy claims.
% DISAPPEARANCE_RATIONALE: If the constraint vanished overnight—if the Israeli state dissolved or fundamentally restructured, settlers withdrew, occupation ended, and the Law of Return were abolished—the political geography, property relations, security arrangements, and demographic composition of the region would reorganize. Palestinian refugees could return, land would be redistributed, Palestinian self-determination structures would emerge, and the resource extraction that currently flows to settlers would cease. The arrangement is not a natural feature—it is actively maintained and would not persist absent continuous enforcement.
% FOUNDING_PROBLEM: European antisemitism and Jewish marginalization in diaspora communities, particularly the Russian Empire and Central Europe, created pressure for territorial refuge and collective self-determination. Zionist movement framed Palestinian territory as the answer to European Jewish vulnerability, presenting settlement and state-building as liberation from persecution.
% FOUNDING_PROBLEM_CORROBORATION: Zionist historiography and diasporist scholars attest to European Jewish persecution as the founding motivation. Postcolonial theorists and Palestinian historians attest that European Jewish vulnerability, while real, does not justify settler-colonial dispossession of another indigenous population—the founding problem (European antisemitism) cannot legitimately be solved through Palestinian dispossession (a different problem entirely). International human rights documentation attests to the constraint's current operation as occupation and legal discrimination, not as a response to ongoing European antisemitism. The legitimacy of the founding problem's diagnosis is not disputed; the claim that Palestinian dispossession solves it is contested by analysts outside the benefiting parties.
narrative_ontology:disappearance_verdict(jewish_self_determination__settler_colonial_reading, world_rearranges).
narrative_ontology:founding_problem_status(jewish_self_determination__settler_colonial_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_self_determination__settler_colonial_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(jewish_self_determination__settler_colonial_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jewish_self_determination__settler_colonial_reading, 0.89, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jewish_self_determination__settler_colonial_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jewish_self_determination__settler_colonial_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jewish_self_determination__settler_colonial_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high and rising (0.89 endpoint) because the constraint continuously extracts land, water, political authority, and legal status from Palestinians and accrues them to settlers and the Israeli state. The extraction is not declining—settlement expansion continues, resource control tightens, legal subordination persists. Suppression is higher than extractiveness (0.91) because the constraint's persistence depends entirely on active military occupation, police enforcement, legal exclusion, and suppression of Palestinian resistance—there is no voluntary coordination holding this arrangement. Participants do not benefit equally or voluntarily; targets cannot exit without abandoning claims and homeland. Theater is moderate-high (0.62) because significant institutional activity is devoted to legitimating the arrangement (security narratives, national self-determination claims, legal formalisms) while the underlying function is dispossession. The measurement series show extraction and suppression rising over 25 years (settlement expansion accelerating, occupation controls hardening, resistance suppression intensifying), which tracks historical reality from the 1990s onward—the constraint is not stabilizing; it is intensifying. Theater also rises as the gap between stated justifications (security) and actual enforcement (settlement expansion) widens. All three metrics are authored from the settler-colonial reading's perspective: extraction and suppression are high because the reading asserts continuous dispossession; theater rises because the reading documents the gap between security narratives and settlement expansion.
 *
 * PERSPECTIVAL GAP:
 *   The settler-colonial reading and the liberal-nationalist reading compute radically differently from the same territory. From the settler perspective (agenda-setter, beneficiary seats), the constraint may appear as rope (genuine coordination for security and self-determination); from the Palestinian perspective (payer, victim seats), it appears as snare (pure extraction enforced by coercion). The engine computes per-seat types from power, exit options, and beneficiary/victim declarations: Israeli settlers with institutional power and arbitrage exit will compute differently from Palestinians with powerless status and trapped exit. The Israeli state (agenda-setter) experiences the arrangement as legitimate self-determination it administers; Palestinians (payers with no exit) experience it as occupation and dispossession. These are not mere disagreements about values—they are structural differences in how the constraint operates at different seats. The settler-colonial reading makes the stronger claim: it asserts the entire framing (self-determination, security, legitimacy) is cover story, and the true function is dispossession. This is a contestable reading (hence the omega on kernel contention), not a settled empirical fact.
 *
 * DIRECTIONALITY LOGIC:
 *   European Jewish settlers and the Israeli state are structural beneficiaries (d near 0.0): they receive land, water, political sovereignty, legal status, and demographic security. They have institutional power to enforce and arbitrage exit (they could, collectively, relocate or dismantle institutions, though politically this is resisted). Palestinian Arabs are structural targets (d near 1.0): they bear the costs of displacement, legal subordination, occupation, resource appropriation, and suppression. They are trapped (cannot exit without abandoning homeland and family; internal displacement is constrained by occupation and legal barriers; external exit means refugee camps and statelessness). Palestinian refugees are even more trapped (d = 1.0): they cannot return due to Law of Return asymmetry, cannot remain in original homes, and bear indefinite displacement. The directionality derivation flows directly from declared beneficiaries/victims and exit options: those who collect the extraction and control exit sit at low d; those who bear costs and are trapped sit at high d. No overrides are needed—the structural data produces the intended directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint does not face mandatrophy in the settler-colonial reading: the founding problem (European Jewish vulnerability) and the current function (dispossession and territorial sovereignty) have diverged substantially, making this a snare without coordination justification. The constraint is not being maintained because it solved its founding problem; it is being maintained because it produces ongoing extraction for beneficiaries. In the liberal-nationalist reading (sibling), mandatrophy would be contested—whether Jewish self-determination (founding problem) continues to require territorial occupation and settlement expansion (current function) is debated. In the settler-colonial reading, there is no mandate to resolve: the arrangement is pure extraction, justified post-hoc by security and self-determination narratives that have decoupled from the founding problem. The theater ratio rising (0.38 → 0.62) suggests increasing theatrical justification as the extraction function becomes more visible—this is consistent with mandatrophy-adjacent dynamics where the stated purpose diverges from actual operation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    settler_colonial_characterization,
    'Is the constraint structurally and empirically a settler-colonial project, or does it constitute indigenous return and legitimate national self-determination (as alternative readings claim)?',
    'Comparative analysis with other settler-colonial cases (North America, Australia, South Africa); empirical examination of displacement timelines, indigenous Palestinian presence pre-1882, legal mechanisms of land appropriation, demographic replacement patterns, and international legal characterizations. Historical documentation of whether Palestinian Arab presence and claims predate European Jewish settlement.',
    'If confirmed as settler-colonial: constraint type remains snare, epsilon remains high, victims are indigenous population. If alternative reading prevails: constraint would be reclassified as legitimate decolonization/return, epsilon would be reinterpreted, beneficiaries and victims would be reframed. This is the core contested kernel distinction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(settler_colonial_characterization, conceptual, 'Whether Zionism is settler colonialism or indigenous return—the foundational dispute between this reading and siblings.').

omega_variable(
    law_of_return_asymmetry,
    'Does the Law of Return constitute legal discrimination against Palestinian refugees, or does it represent legitimate national self-determination policy?',
    'Legal analysis of differential treatment: Law of Return admits diaspora Jews while systematically denying Palestinian refugees'' right of return; international legal opinion on whether this constitutes discrimination; empirical examination of application and enforcement patterns. Comparison with international legal standards for refugee rights and minority protection.',
    'Confirmation would establish the constraint as using law to enforce dispossession; reframing would argue it reflects legitimate state preference for national majority. Shapes the characterization of suppression mechanism: structural legal discrimination vs. legitimate immigration policy.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(law_of_return_asymmetry, empirical, 'Whether Law of Return operates as legal discrimination or legitimate policy.').

omega_variable(
    suppression_mechanism_internalization,
    'Is Palestinian political subordination primarily structural (military occupation, resource control, legal barriers) or has acceptance of subordination become internalized across generations of occupation?',
    'Post-occupation scenario analysis: if occupation ended and legal barriers were removed, would resistance re-emerge fully, or has generational trauma and institutional incorporation created durable compliance? Comparative analysis with decolonization cases.',
    'If structural dominates: enforcement intensity can be reduced by removing barriers and occupation structures. If internalized component is substantial: the constraint''s effective suppression is higher than institutional measures alone suggest—targets carry the suppression internally. Shapes remediation scenarios.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Structural vs. internalized character of Palestinian suppression.').

omega_variable(
    kernel_reading_contention,
    'Which of the five readings of the jewish_self_determination kernel is structurally true?',
    'This is the committer-frame core uncertainty: different readings produce different ε values, different beneficiary/victim structures, different constraint types. The settler-colonial reading instantiates THIS constraint; sibling readings instantiate different constraints with different structural data. No single resolution mechanism exists—readings are incommensurable axes, each with its own logic and evidence base. This omega documents the irreducible pluralism of the kernel itself.',
    'This reading claims snare type with high epsilon, beneficiary=settlers, victims=Palestinians. Sibling readings would claim rope (liberal nationalist), mountain-like (indigenous return and religious covenant), or rope with inverted beneficiary/victim (diasporist). The engine computes per-reading classifications; contention remains.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_contention, conceptual, 'Which reading of jewish_self_determination captures the constraint''s true structure.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_self_determination__settler_colonial_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jewi_tr_t0, jewish_self_determination__settler_colonial_reading, theater_ratio, 0, 0.38).
narrative_ontology:measurement_basis(jewi_tr_t0, observed).
narrative_ontology:measurement(jewi_tr_t3, jewish_self_determination__settler_colonial_reading, theater_ratio, 3, 0.44).
narrative_ontology:measurement_basis(jewi_tr_t3, observed).
narrative_ontology:measurement(jewi_tr_t6, jewish_self_determination__settler_colonial_reading, theater_ratio, 6, 0.5).
narrative_ontology:measurement_basis(jewi_tr_t6, observed).
narrative_ontology:measurement(jewi_tr_t9, jewish_self_determination__settler_colonial_reading, theater_ratio, 9, 0.55).
narrative_ontology:measurement_basis(jewi_tr_t9, observed).
narrative_ontology:measurement(jewi_tr_t12, jewish_self_determination__settler_colonial_reading, theater_ratio, 12, 0.58).
narrative_ontology:measurement_basis(jewi_tr_t12, observed).
narrative_ontology:measurement(jewi_tr_t16, jewish_self_determination__settler_colonial_reading, theater_ratio, 16, 0.6).
narrative_ontology:measurement_basis(jewi_tr_t16, observed).
narrative_ontology:measurement(jewi_tr_t20, jewish_self_determination__settler_colonial_reading, theater_ratio, 20, 0.61).
narrative_ontology:measurement_basis(jewi_tr_t20, observed).
narrative_ontology:measurement(jewi_tr_t25, jewish_self_determination__settler_colonial_reading, theater_ratio, 25, 0.62).
narrative_ontology:measurement_basis(jewi_tr_t25, observed).

% Extraction over time
narrative_ontology:measurement(jewi_be_t0, jewish_self_determination__settler_colonial_reading, base_extractiveness, 0, 0.72).
narrative_ontology:measurement_basis(jewi_be_t0, observed).
narrative_ontology:measurement(jewi_be_t3, jewish_self_determination__settler_colonial_reading, base_extractiveness, 3, 0.76).
narrative_ontology:measurement_basis(jewi_be_t3, observed).
narrative_ontology:measurement(jewi_be_t6, jewish_self_determination__settler_colonial_reading, base_extractiveness, 6, 0.81).
narrative_ontology:measurement_basis(jewi_be_t6, observed).
narrative_ontology:measurement(jewi_be_t9, jewish_self_determination__settler_colonial_reading, base_extractiveness, 9, 0.85).
narrative_ontology:measurement_basis(jewi_be_t9, observed).
narrative_ontology:measurement(jewi_be_t12, jewish_self_determination__settler_colonial_reading, base_extractiveness, 12, 0.87).
narrative_ontology:measurement_basis(jewi_be_t12, observed).
narrative_ontology:measurement(jewi_be_t16, jewish_self_determination__settler_colonial_reading, base_extractiveness, 16, 0.88).
narrative_ontology:measurement_basis(jewi_be_t16, observed).
narrative_ontology:measurement(jewi_be_t20, jewish_self_determination__settler_colonial_reading, base_extractiveness, 20, 0.89).
narrative_ontology:measurement_basis(jewi_be_t20, observed).
narrative_ontology:measurement(jewi_be_t25, jewish_self_determination__settler_colonial_reading, base_extractiveness, 25, 0.89).
narrative_ontology:measurement_basis(jewi_be_t25, observed).

% Suppression requirement over time
narrative_ontology:measurement(jewi_su_t0, jewish_self_determination__settler_colonial_reading, suppression_requirement, 0, 0.74).
narrative_ontology:measurement_basis(jewi_su_t0, observed).
narrative_ontology:measurement(jewi_su_t3, jewish_self_determination__settler_colonial_reading, suppression_requirement, 3, 0.78).
narrative_ontology:measurement_basis(jewi_su_t3, observed).
narrative_ontology:measurement(jewi_su_t6, jewish_self_determination__settler_colonial_reading, suppression_requirement, 6, 0.82).
narrative_ontology:measurement_basis(jewi_su_t6, observed).
narrative_ontology:measurement(jewi_su_t9, jewish_self_determination__settler_colonial_reading, suppression_requirement, 9, 0.86).
narrative_ontology:measurement_basis(jewi_su_t9, observed).
narrative_ontology:measurement(jewi_su_t12, jewish_self_determination__settler_colonial_reading, suppression_requirement, 12, 0.88).
narrative_ontology:measurement_basis(jewi_su_t12, observed).
narrative_ontology:measurement(jewi_su_t16, jewish_self_determination__settler_colonial_reading, suppression_requirement, 16, 0.89).
narrative_ontology:measurement_basis(jewi_su_t16, observed).
narrative_ontology:measurement(jewi_su_t20, jewish_self_determination__settler_colonial_reading, suppression_requirement, 20, 0.9).
narrative_ontology:measurement_basis(jewi_su_t20, observed).
narrative_ontology:measurement(jewi_su_t25, jewish_self_determination__settler_colonial_reading, suppression_requirement, 25, 0.91).
narrative_ontology:measurement_basis(jewi_su_t25, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_self_determination__settler_colonial_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(jewish_self_determination__settler_colonial_reading, 0.08).
narrative_ontology:affects_constraint(jewish_self_determination__settler_colonial_reading, jewish_self_determination__liberal_nationalist_reading).
narrative_ontology:affects_constraint(jewish_self_determination__settler_colonial_reading, jewish_self_determination__indigenous_return_reading).
narrative_ontology:affects_constraint(jewish_self_determination__settler_colonial_reading, jewish_self_determination__religious_covenant_reading).
narrative_ontology:affects_constraint(jewish_self_determination__settler_colonial_reading, jewish_self_determination__diasporist_reading).
narrative_ontology:affects_constraint(jewish_self_determination__settler_colonial_reading, palestinian_right_of_return).
narrative_ontology:affects_constraint(jewish_self_determination__settler_colonial_reading, israeli_occupation_west_bank).
narrative_ontology:affects_constraint(jewish_self_determination__settler_colonial_reading, law_of_return_demographic_policy).

% DUAL FORMULATION NOTE:
% The jewish_self_determination kernel decomposes into five constraint stories, each instantiating a different reading with different ε, beneficiaries, victims, and claimed types. The settler-colonial reading (this constraint) is upstream of the liberal-nationalist and indigenous-return readings in the sense that it contests their core premises, but the readings are not hierarchical—they are incommensurable epistemic frames held by different communities. The network edges link this reading to its siblings and to downstream constraints (Palestinian right of return, occupation, Law of Return) that are affected structurally by how this kernel's reading is resolved.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(jewish_self_determination__settler_colonial_reading, powerless, 0.98).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
