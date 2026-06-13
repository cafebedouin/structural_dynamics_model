% ============================================================================
% CONSTRAINT STORY: jewish_sovereignty_palestine__post_zionist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jewish_sovereignty_palestine__post_zionist_reading, []).

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
    narrative_ontology:constraint_vindicates/2,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: jewish_sovereignty_palestine__post_zionist_reading
 *   human_readable: Zionist Sovereignty Framework: Post-Zionist Reading
 *   domain: political_philosophy/nationalism_studies/postcolonial_theory
 *
 * SUMMARY:
 *   The post-Zionist reading asserts that the Zionist project successfully
 *   achieved its founding goal — establishing a sovereign Jewish state and
 *   refuge — but that the state's ethnic-national institutional framework,
 *   justified during the emergency of diaspora insecurity, has become an
 *   extraction mechanism that subordinates Palestinian citizens and occupied
 *   populations to Jewish privilege. The reading distinguishes itself from
 *   liberal Zionism (which argues institutions can be reformed while
 *   preserving statehood) and settler colonialism (which reads the entire
 *   project as displacement from inception). The post-Zionist position
 *   argues: (1) the founding coordination problem was real and the state was
 *   a reasonable response, but (2) that founding problem is now effectively
 *   resolved, and (3) the ethnic-national framework no longer serves
 *   coordination — it now sustains extraction and prevents civic equality.
 *   The reading emerges from inside Israeli society, particularly from
 *   Palestinian-Israeli intellectuals and post-1967-war Israeli critics who
 *   documented the structural extraction, not from external observers. The
 *   constraint's extractiveness rises substantially over the interval (0.35
 *   in 1948 to 0.68 in 2024), driven by: occupation without representation,
 *   settlement expansion, law of return asymmetry intensification, and the
 *   theatrical performance of liberal democracy alongside substantive ethnic
 *   privilege. The theater ratio rises because Israeli institutions adopt
 *   human-rights language, minority-protection rhetoric, and equality pledges
 *   while enforcement mechanisms preserve ethnic hierarchy — the constraint's
 *   performative layer expands as resistance to extraction grows.
 *
 * KEY AGENTS:
 *   - jewish_israeli_citizens: primary beneficiary; institutional agenda-setter; control state resources, Law of Return, land access; can exit to diaspora but remain institutionally embedded
 *   - palestinian_citizens_israel: formal citizens but structurally subordinated; identity-locked to place; excluded from ethnic-national institutional privilege
 *   - occupied_palestinian_populations: trapped; under military governance; maximum extraction, minimum voice
 *   - religious_zionist_settlers: organized enforcer of territorial expansion and ethnic privilege; identity-locked to ideological commitment
 *   - post_zionist intellectuals: analytical seat; document the structural gap between founding narrative and extraction operation
 *   - liberal_zionist advocates: defend reform path; argue institutions can change while preserving statehood; organized resistance to post-Zionist conclusion
 *   - international community: excluded from enforcement; advocate for civic equality and Palestinian self-determination from outside
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_sovereignty_palestine__post_zionist_reading, 0.68).
domain_priors:suppression_score(jewish_sovereignty_palestine__post_zionist_reading, 0.62).
domain_priors:theater_ratio(jewish_sovereignty_palestine__post_zionist_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__post_zionist_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__post_zionist_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__post_zionist_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__post_zionist_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__post_zionist_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_sovereignty_palestine__post_zionist_reading, tangled_rope).
narrative_ontology:human_readable(jewish_sovereignty_palestine__post_zionist_reading, "Zionist Sovereignty Framework: Post-Zionist Reading").
narrative_ontology:topic_domain(jewish_sovereignty_palestine__post_zionist_reading, "political_philosophy/nationalism_studies/postcolonial_theory").

domain_priors:requires_active_enforcement(jewish_sovereignty_palestine__post_zionist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_sovereignty_palestine__post_zionist_reading, 'ff215a53-f116-4971-873e-ff126a8c583d').
narrative_ontology:cs_kernel_codification('ff215a53-f116-4971-873e-ff126a8c583d', formalized).
narrative_ontology:cs_authority_grounding('ff215a53-f116-4971-873e-ff126a8c583d', extraction).
narrative_ontology:cs_interpretation_layer_present('ff215a53-f116-4971-873e-ff126a8c583d').
narrative_ontology:cs_reading_relation('ff215a53-f116-4971-873e-ff126a8c583d', jewish_sovereignty_palestine__liberal_nationalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('ff215a53-f116-4971-873e-ff126a8c583d', jewish_sovereignty_palestine__settler_colonial_reading, influences).
narrative_ontology:cs_reading_relation('ff215a53-f116-4971-873e-ff126a8c583d', jewish_sovereignty_palestine__religious_zionist_reading, coexists_with).
narrative_ontology:cs_reading_relation('ff215a53-f116-4971-873e-ff126a8c583d', jewish_sovereignty_palestine__cultural_zionist_reading, coexists_with).
narrative_ontology:cs_axiom('ff215a53-f116-4971-873e-ff126a8c583d', foundational, foundational_problem_resolved).
narrative_ontology:cs_axiom_status(foundational_problem_resolved, holdable).
narrative_ontology:cs_axiom_grounding('ff215a53-f116-4971-873e-ff126a8c583d', foundational_problem_resolved, empirically_contingent).
narrative_ontology:cs_axiom('ff215a53-f116-4971-873e-ff126a8c583d', foundational, ethnic_privilege_irreformable).
narrative_ontology:cs_axiom_status(ethnic_privilege_irreformable, holdable).
narrative_ontology:cs_axiom_grounding('ff215a53-f116-4971-873e-ff126a8c583d', ethnic_privilege_irreformable, deontological).
narrative_ontology:cs_axiom('ff215a53-f116-4971-873e-ff126a8c583d', secondary, state_form_preservable_through_civic_equality).
narrative_ontology:cs_axiom_status(state_form_preservable_through_civic_equality, holdable).
narrative_ontology:cs_axiom_grounding('ff215a53-f116-4971-873e-ff126a8c583d', state_form_preservable_through_civic_equality, instrumental).
narrative_ontology:cs_reference_frame('ff215a53-f116-4971-873e-ff126a8c583d', jewish_security_and_self_determination_achieved).
narrative_ontology:cs_drift_state('ff215a53-f116-4971-873e-ff126a8c583d', contemporary_occupation_and_settlement_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('ff215a53-f116-4971-873e-ff126a8c583d', '').
narrative_ontology:cs_kernel_id(jewish_sovereignty_palestine__post_zionist_reading, jewish_sovereignty_palestine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_sovereignty_palestine__post_zionist_reading, jewish_israeli_citizens).
narrative_ontology:constraint_victim(jewish_sovereignty_palestine__post_zionist_reading, palestinian_citizens_israel).
narrative_ontology:constraint_victim(jewish_sovereignty_palestine__post_zionist_reading, occupied_palestinian_populations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(jewish_sovereignty_palestine__post_zionist_reading, liberal_zionist_advocates).
narrative_ontology:constraint_vindicates(jewish_sovereignty_palestine__post_zionist_reading, jewish_national_self_determination_achieved).
narrative_ontology:constraint_vindicates(jewish_sovereignty_palestine__post_zionist_reading, territorial_state_establishment_completed).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Possess privileged access to land, citizenship pathways, and state resources via Law of Return and Jewish-majority institutional arrangements. Set and enforce state institutions, national symbols, and citizenship criteria favoring Jewish identity. Benefit from demographic and legal asymmetries that embed Jewish privilege in state structures. Can exit to diaspora communities globally; institutional exit (civic participation despite ethnic framework) constrained by state's ethnic-national definition.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__post_zionist_reading, jewish_israeli_citizens, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(jewish_sovereignty_palestine__post_zionist_reading, jewish_israeli_citizens, agenda_setter).

% Formal Israeli citizens but structurally subordinated through state institutions designed around Jewish-majority ethnic nationalism: Law of Return excludes them from return-migration rights, land laws deny equal access, national symbols (flag, anthem, holidays) encode Jewish identity. Cannot exit citizenship (identity locked to place and family roots); cannot fully participate in the ethnic-nationalist state framework; politically organized but institutionally constrained.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__post_zionist_reading, palestinian_citizens_israel, payer,
    moderate, biographical, identity_locked, national).

% Under military administration or blockade without citizenship or meaningful self-governance. Excluded from state institutions entirely. Land access controlled through settlement policy and permit systems that privilege Jewish development. No legal exit; geographic exit restricted by borders and international law. Bear the heaviest extractive costs: displacement, land seizure, military enforcement, denial of civic participation.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__post_zionist_reading, occupied_palestinian_populations, payer,
    powerless, biographical, trapped, national).

% Defend the Jewish state's legitimacy as an exercise of collective self-determination rights; argue institutional reforms can preserve Jewish sovereignty while expanding Palestinian rights. Benefit from the state's existence and legitimacy; constrained by pressure from post-Zionist critics who argue the state's ethnic-national foundation is irreformable.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__post_zionist_reading, liberal_zionist_advocates, beneficiary,
    organized, biographical, constrained, national).

% Analyze and critique the state's ethnic-national framework from within Israeli society and academia. Occupy analytical seats; some face institutional pressure but retain mobility (academic freedom, diaspora connections). Document the structural gap between the state's founding narrative (return to homeland) and its operational extraction (ethnic privilege, Palestinian subordination).
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__post_zionist_reading, post_zionist_intellectuals_activists, observer,
    moderate, biographical, mobile, global).

% Enforce territorial expansion and settlement policy through institutional leverage and mobilization. Ground legitimacy in theological claim to Eretz Yisrael; view the constraint's civic-equality pressure as existential threat. Strongly committed to the ethnic-national framework; identity locked to ideological commitment.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__post_zionist_reading, religious_zionist_settlers, agenda_setter,
    organized, civilizational, identity_locked, national).

% Observers and occasional interveners (UN, ICJ, human-rights bodies) without enforcement power over Israeli state institutions. Would advocate for civic equality, Palestinian self-determination, and de-militarization if given direct authority; excluded from internal state institutional reform.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__post_zionist_reading, international_community, excluded,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jewish_sovereignty_palestine__post_zionist_reading, jewish_israeli_citizens).
narrative_ontology:fixing_cost_class(jewish_sovereignty_palestine__post_zionist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The Zionist project solved a genuine coordination problem: Jewish diaspora communities faced existential insecurity, persecution, and powerlessness; centralized immigration, land acquisition, and institution-building created a territorial refuge and a political entity capable of defending Jewish survival, agency, and self-determination.
% TRANSFER_FUNCTION: The state's ethnic-national institutional framework transfers institutional privilege, land access, and citizenship rights to Jewish Israelis while denying them to Palestinians. Specifically: Law of Return automatically grants citizenship and return rights to Jews worldwide while denying Palestinian refugees the same right; land laws privilege Jewish development and settlement; state symbols, official calendar, and primary language encode Jewish-majority culture; military occupation enables uncompensated land seizure and governance of Palestinian populations without representation.
% ABSENT_VOICES: Palestinian citizens of Israel lack institutional voice in design of ethnic-national institutions; occupied Palestinian populations have no formal participation in the institutions that govern their territory and movement. Both populations argue (and some do in academic, legal, and activist contexts) that civic equality is impossible within an ethnic-national state framework. International human-rights bodies and post-Zionist Israeli critics would argue the state's institutional legitimacy is fundamentally compromised by ethnic privilege.
% DISAPPEARANCE_RATIONALE: Liberal Zionists argue: if post-Zionist pressure for de-Zionization disappeared, institutional reform could preserve Jewish statehood with expanded Palestinian rights — the founding function (refuge, security) remains intact. Post-Zionists and Palestinian critics argue: if the ethnic-national framework disappeared, the state's legitimacy, citizenship structure, and territorial control would be fundamentally reorganized — a genuine civic-equality framework would require Palestinian right of return, equal land access, and constitutional secularization, effectively ending Zionist statehood as institutionally constituted.
% FOUNDING_PROBLEM: Late 19th-century Jewish diaspora faced insecurity, persecution, and lack of sovereign agency; the Zionist project proposed building a Jewish territorial state in Palestine to solve this security and self-determination crisis. The founding problem centered on Jewish collective survival and the right to self-governance in response to antisemitism and powerlessness.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem's historical reality (Jewish persecution, diaspora insecurity, Holocaust genocide) is widely acknowledged across all readings and by international scholarship. The contest centers on: (1) whether territorial statehood in Palestine was the necessary solution, and (2) whether the founding problem remains live. Jewish Israelis and liberal Zionists argue: ongoing antisemitism and security threats mean the problem remains live. Post-Zionist and Palestinian scholars argue: Jewish institutional power is now globally established; state security has been achieved; the founding problem is effectively resolved, and the state persists through extraction rather than through solving a live coordination problem. Corroboration from outside benefiting parties: international historians document diaspora insecurity; scholars debate necessity of territorial statehood; Palestinian and post-Zionist scholars argue occupation and settlement persistence depend on suppressing Palestinian agency and return rights, not on addressing founding Jewish security crisis.
narrative_ontology:disappearance_verdict(jewish_sovereignty_palestine__post_zionist_reading, contested).
narrative_ontology:founding_problem_status(jewish_sovereignty_palestine__post_zionist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_sovereignty_palestine__post_zionist_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(jewish_sovereignty_palestine__post_zionist_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jewish_sovereignty_palestine__post_zionist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jewish_sovereignty_palestine__post_zionist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jewish_sovereignty_palestine__post_zionist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) reflects the systematic transfer of land, citizenship rights, and institutional privilege from Palestinians to Jewish Israelis, with the transfer velocity increasing substantially post-1967. Suppression (0.62) reflects the military and administrative machinery required to maintain ethnic privilege against Palestinian resistance — higher than in 1948 because the founding security threat has declined while the ethnic-privilege extraction continues. Theater_ratio (0.41) reflects the growing gap between liberal-democratic institutional forms (human-rights courts, equality pledges, minority-protection laws) and substantive enforcement that preserves ethnic hierarchy. Accessibility_collapse and resistance show inversion across the interval: in 1948 Palestinian alternatives (diaspora for Palestinian refugees, civic participation for Palestinian citizens) seemed more available; by 2024 alternatives have collapsed structurally (right of return denied, settlement barriers erected, occupation hardened) while organized resistance has grown stronger. The coercion_grid shows divergent level-dynamics: structural-level suppression and stakes have risen (occupation infrastructure, settlement policy), while individual-level resistance and stakes have also risen (Palestinian awareness of structural exclusion, youth mobilization). The measurement series track the constraint's evolution from genuine coordination (refugee absorption, state-building) toward pure extraction (ethnic-national privilege maintenance, Palestinian subordination). The single shared time grid ensures every metric is authored at every examined point; the interval spans 1948–2024 to capture the full lifecycle from founding to contemporary operation.
 *
 * PERSPECTIVAL GAP:
 *   Jewish Israeli institutional actors and the liberal Zionist intellectual coalition perceive the constraint as successful coordination (state security, refuge achieved, reform pathway available); Palestinian citizens and post-Zionist critics perceive it as extraction (ethnic privilege, subordination, reform pathway blocked by state design). The engine computes different per-seat types from this structural divergence: the beneficiary seat (jewish_israeli_citizens, institutional power, arbitrage exit) experiences the constraint as enabling; the victim seats (palestinian_citizens_israel identity-locked, occupied_palestinian_populations trapped) experience it as extractive. The post-Zionist reading explicitly asserts that the coordinating function has atrophied while the extraction mechanism persists and intensifies — the founding problem is resolved but the state persists through ethnic privilege rather than through solving a live coordination problem.
 *
 * DIRECTIONALITY LOGIC:
 *   jewish_israeli_citizens derive directionality near 0.0 (full beneficiary): they control institutional design, collect the benefits of Law of Return and land access asymmetries, and can exit to diaspora if desired. palestinian_citizens_israel derive directionality near 1.0 (full target): they are formally citizens but structurally excluded from the ethnic-national privilege structure, identity-locked to their Palestinian identity and geographic rootedness, with constrained alternatives (civic participation denied, voting power diluted, land access unequal). occupied_palestinian_populations derive directionality even closer to 1.0 (trapped target): they lack citizenship entirely, lack meaningful alternatives, and bear the maximum extraction (military rule, land seizure, resource denial). liberal_zionist_advocates sit near 0.2 (slight beneficiary): they benefit from the state's legitimacy and existence but face pressure from post-Zionist critique and Palestinian resistance. religious_zionist_settlers sit near 0.1 (beneficiary): they benefit from settlement policy and ideological commitment but face domestic and international pressure. The directionality structure maps cleanly to the claimed tangled_rope type: genuine coordination (Jewish refuge, state security) paired with asymmetric extraction (ethnic privilege, Palestinian subordination), requiring active enforcement (military occupation, land control, institutional privilege maintenance).
 *
 * MANDATROPHY ANALYSIS:
 *   The post-Zionist reading identifies a mandate obsolescence: the founding mandate (Jewish security, diaspora refuge, self-determination in response to persecution) remains claimed as the legitimating narrative, but the founding problem it addressed is substantially resolved (Jewish security is no longer existentially threatened; diaspora insecurity has declined; Jewish institutional power is globally established). The state persists and intensifies extraction (ethnic privilege, occupation, settlement expansion) not to solve the founding problem but to maintain Jewish demographic and institutional majority. The theater_ratio rise (0.18 to 0.41 across the interval) signals mandate drift: the state adopts human-rights language and equality rhetoric while enforcement preserves ethnic hierarchy. The post-Zionist reading argues the state has undergone mandatrophy — the mandate (Jewish survival and self-determination in response to diaspora crisis) is no longer operationally live, but the state apparatus persists through institutional inertia and extraction. This maps to the piton trajectory (theater rises, actual function atrophies, enforcement persists by institutional momentum) or to the snare trajectory (theater is a cover story, extraction is the actual function, enforcement masks extraction as coordination). The post-Zionist reading leans toward snare classification: the constraint is read as pure extraction with a coordination cover story, not as degraded coordination with residual theater. The mandatrophy signal is the foundational_problem_status x disappearance_verdict mismatch: founding_problem_status = 'contested' (the Jewish security crisis is declared resolved by post-Zionists, contested by defenders); disappearance_verdict = 'contested' (post-Zionists argue disappearance would enable civic-equality reorganization; Zionists argue disappearance would eliminate Jewish security). The mismatch flags the mandate as incoherent under post-Zionist reading.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    founding_problem_obsolescence,
    'Is the founding problem that Zionism was designed to solve — Jewish diaspora insecurity, antisemitism, and powerlessness — still live or effectively resolved?',
    'Measure Jewish diaspora well-being, antisemitism rates, and Jewish institutional power globally over the interval; compare threat levels in 1948 to contemporary threat levels; assess whether current Israeli state security depends on territorial expansion and ethnic privilege or could be maintained through civic institutions.',
    'If the founding problem is declared effectively resolved, the post-Zionist reading argues the ethnic-national state persists as pure extraction without its coordinating function; if the problem remains live, liberal Zionists argue territorial statehood remains necessary for Jewish security, partially justifying the ethnic framework.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_obsolescence, empirical, 'Whether the foundational coordination crisis Zionism addressed is live or resolved.').

omega_variable(
    ethnic_framework_irreformability,
    'Can the state''s ethnic-national institutions be reformed to accommodate civic equality for Palestinians without fundamentally dissolving the Zionist project, or is ethnic privilege structurally embedded?',
    'Comparative analysis of constitutional reform attempts; examination of whether Law of Return, land laws, and demographic policy can be decoupled from Jewish-majority privilege; empirical testing of whether Palestinians accept reformed institutions or demand de-Zionization.',
    'If the ethnic framework is reformable, liberal Zionists'' institutional-change path is viable; if irreformable, post-Zionist reading gains structural support and the constraint maps as a snare rather than tangled rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(ethnic_framework_irreformability, conceptual, 'Whether ethnic-national institutions can be reformed or are structurally irreformable.').

omega_variable(
    theater_mechanism_in_institutional_consent,
    'To what extent does the rising theater_ratio reflect performative adherence to liberal democratic norms (human-rights language, minority protections, equality pledges) that mask ongoing ethnic privilege and institutional extraction?',
    'Gap analysis between formal rights declarations and enforcement; measurement of Palestinian institutional representation and decision-making power; comparison of rhetoric to resource allocation and land policy implementation.',
    'High theater ratio with low actual equality enforcement would indicate the constraint operates as snare with liberal-democratic window-dressing, suggesting mandatrophy (state legitimacy claims become incoherent when enforcement-to-rhetoric gap widens).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(theater_mechanism_in_institutional_consent, empirical, 'Whether liberal-democratic institutional forms mask ongoing ethnic extraction.').

omega_variable(
    post_zionist_reading_epistemic_location,
    'The post-Zionist reading instantiates a challenger epistemology that emerged inside Israeli intellectual culture. Is this reading''s force grounded in structural facts about the state''s ethnic framework (empirical), or in a normative reframing of what ''self-determination'' and ''equality'' require (preference-dependent)?',
    'Trace the reading''s emergence to institutional conditions (academic freedom, Palestinian integration, international legal norms); compare with sibling readings'' emergence contexts; assess whether the reading survives as other institutional conditions change or whether it is contingent on current critique windows.',
    'If the reading is largely preference-dependent rather than structurally grounded, the committer frame becomes more salient — different reading coalitions will persist indefinitely without convergence. If empirically grounded, enforcement intensification and theater rise may eventually force institutional reckoning.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(post_zionist_reading_epistemic_location, conceptual, 'Whether the post-Zionist reading is structurally grounded or preference-contingent.').

omega_variable(
    settler_colonial_vs_liberation_narrative_overlap,
    'At what structural points does the post-Zionist reading''s analysis overlap with the settler-colonial reading''s analysis, and where do they diverge? Can both readings coexist or do they foreclose each other?',
    'Comparative analysis of the two readings'' axioms and empirical claims; examination of whether acknowledging settler-colonial dynamics requires abandoning the post-Zionist goal of Jewish civic equality within a reformed state, or whether the two projects can remain distinct.',
    'If the readings coexist (as the schema expects: post-Zionist = coexists_with settler-colonial), the post-Zionist path remains institutionally live; if they foreclose each other, the reading''s ability to command intellectual consensus may depend on which framing becomes politically dominant.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(settler_colonial_vs_liberation_narrative_overlap, conceptual, 'Structural and empirical relationship between post-Zionist and settler-colonial readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_sovereignty_palestine__post_zionist_reading, 1948, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jewi_tr_t1948, jewish_sovereignty_palestine__post_zionist_reading, theater_ratio, 1948, 0.18).
narrative_ontology:measurement_basis(jewi_tr_t1948, observed).
narrative_ontology:measurement(jewi_tr_t1967, jewish_sovereignty_palestine__post_zionist_reading, theater_ratio, 1967, 0.25).
narrative_ontology:measurement_basis(jewi_tr_t1967, observed).
narrative_ontology:measurement(jewi_tr_t1987, jewish_sovereignty_palestine__post_zionist_reading, theater_ratio, 1987, 0.32).
narrative_ontology:measurement_basis(jewi_tr_t1987, observed).
narrative_ontology:measurement(jewi_tr_t2005, jewish_sovereignty_palestine__post_zionist_reading, theater_ratio, 2005, 0.38).
narrative_ontology:measurement_basis(jewi_tr_t2005, observed).
narrative_ontology:measurement(jewi_tr_t2015, jewish_sovereignty_palestine__post_zionist_reading, theater_ratio, 2015, 0.4).
narrative_ontology:measurement_basis(jewi_tr_t2015, observed).
narrative_ontology:measurement(jewi_tr_t2024, jewish_sovereignty_palestine__post_zionist_reading, theater_ratio, 2024, 0.41).
narrative_ontology:measurement_basis(jewi_tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(jewi_be_t1948, jewish_sovereignty_palestine__post_zionist_reading, base_extractiveness, 1948, 0.35).
narrative_ontology:measurement_basis(jewi_be_t1948, observed).
narrative_ontology:measurement(jewi_be_t1967, jewish_sovereignty_palestine__post_zionist_reading, base_extractiveness, 1967, 0.52).
narrative_ontology:measurement_basis(jewi_be_t1967, observed).
narrative_ontology:measurement(jewi_be_t1987, jewish_sovereignty_palestine__post_zionist_reading, base_extractiveness, 1987, 0.61).
narrative_ontology:measurement_basis(jewi_be_t1987, observed).
narrative_ontology:measurement(jewi_be_t2005, jewish_sovereignty_palestine__post_zionist_reading, base_extractiveness, 2005, 0.65).
narrative_ontology:measurement_basis(jewi_be_t2005, observed).
narrative_ontology:measurement(jewi_be_t2015, jewish_sovereignty_palestine__post_zionist_reading, base_extractiveness, 2015, 0.67).
narrative_ontology:measurement_basis(jewi_be_t2015, observed).
narrative_ontology:measurement(jewi_be_t2024, jewish_sovereignty_palestine__post_zionist_reading, base_extractiveness, 2024, 0.68).
narrative_ontology:measurement_basis(jewi_be_t2024, observed).

% Suppression requirement over time
narrative_ontology:measurement(jewi_su_t1948, jewish_sovereignty_palestine__post_zionist_reading, suppression_requirement, 1948, 0.38).
narrative_ontology:measurement_basis(jewi_su_t1948, observed).
narrative_ontology:measurement(jewi_su_t1967, jewish_sovereignty_palestine__post_zionist_reading, suppression_requirement, 1967, 0.48).
narrative_ontology:measurement_basis(jewi_su_t1967, observed).
narrative_ontology:measurement(jewi_su_t1987, jewish_sovereignty_palestine__post_zionist_reading, suppression_requirement, 1987, 0.56).
narrative_ontology:measurement_basis(jewi_su_t1987, observed).
narrative_ontology:measurement(jewi_su_t2005, jewish_sovereignty_palestine__post_zionist_reading, suppression_requirement, 2005, 0.6).
narrative_ontology:measurement_basis(jewi_su_t2005, observed).
narrative_ontology:measurement(jewi_su_t2015, jewish_sovereignty_palestine__post_zionist_reading, suppression_requirement, 2015, 0.61).
narrative_ontology:measurement_basis(jewi_su_t2015, observed).
narrative_ontology:measurement(jewi_su_t2024, jewish_sovereignty_palestine__post_zionist_reading, suppression_requirement, 2024, 0.62).
narrative_ontology:measurement_basis(jewi_su_t2024, observed).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=1948, tn=2024
narrative_ontology:measurement(jewi_grid_01, jewish_sovereignty_palestine__post_zionist_reading, accessibility_collapse(class), 1948, 0.71).
narrative_ontology:measurement(jewi_grid_02, jewish_sovereignty_palestine__post_zionist_reading, accessibility_collapse(class), 2024, 0.81).
narrative_ontology:measurement(jewi_grid_03, jewish_sovereignty_palestine__post_zionist_reading, accessibility_collapse(individual), 1948, 0.35).
narrative_ontology:measurement(jewi_grid_04, jewish_sovereignty_palestine__post_zionist_reading, accessibility_collapse(individual), 2024, 0.42).
narrative_ontology:measurement(jewi_grid_05, jewish_sovereignty_palestine__post_zionist_reading, accessibility_collapse(organizational), 1948, 0.48).
narrative_ontology:measurement(jewi_grid_06, jewish_sovereignty_palestine__post_zionist_reading, accessibility_collapse(organizational), 2024, 0.58).
narrative_ontology:measurement(jewi_grid_07, jewish_sovereignty_palestine__post_zionist_reading, accessibility_collapse(structural), 1948, 0.62).
narrative_ontology:measurement(jewi_grid_08, jewish_sovereignty_palestine__post_zionist_reading, accessibility_collapse(structural), 2024, 0.72).
narrative_ontology:measurement(jewi_grid_09, jewish_sovereignty_palestine__post_zionist_reading, resistance(class), 1948, 0.62).
narrative_ontology:measurement(jewi_grid_10, jewish_sovereignty_palestine__post_zionist_reading, resistance(class), 2024, 0.82).
narrative_ontology:measurement(jewi_grid_11, jewish_sovereignty_palestine__post_zionist_reading, resistance(individual), 1948, 0.45).
narrative_ontology:measurement(jewi_grid_12, jewish_sovereignty_palestine__post_zionist_reading, resistance(individual), 2024, 0.68).
narrative_ontology:measurement(jewi_grid_13, jewish_sovereignty_palestine__post_zionist_reading, resistance(organizational), 1948, 0.35).
narrative_ontology:measurement(jewi_grid_14, jewish_sovereignty_palestine__post_zionist_reading, resistance(organizational), 2024, 0.72).
narrative_ontology:measurement(jewi_grid_15, jewish_sovereignty_palestine__post_zionist_reading, resistance(structural), 1948, 0.28).
narrative_ontology:measurement(jewi_grid_16, jewish_sovereignty_palestine__post_zionist_reading, resistance(structural), 2024, 0.58).
narrative_ontology:measurement(jewi_grid_17, jewish_sovereignty_palestine__post_zionist_reading, stakes_inflation(class), 1948, 0.68).
narrative_ontology:measurement(jewi_grid_18, jewish_sovereignty_palestine__post_zionist_reading, stakes_inflation(class), 2024, 0.74).
narrative_ontology:measurement(jewi_grid_19, jewish_sovereignty_palestine__post_zionist_reading, stakes_inflation(individual), 1948, 0.58).
narrative_ontology:measurement(jewi_grid_20, jewish_sovereignty_palestine__post_zionist_reading, stakes_inflation(individual), 2024, 0.65).
narrative_ontology:measurement(jewi_grid_21, jewish_sovereignty_palestine__post_zionist_reading, stakes_inflation(organizational), 1948, 0.42).
narrative_ontology:measurement(jewi_grid_22, jewish_sovereignty_palestine__post_zionist_reading, stakes_inflation(organizational), 2024, 0.48).
narrative_ontology:measurement(jewi_grid_23, jewish_sovereignty_palestine__post_zionist_reading, stakes_inflation(structural), 1948, 0.35).
narrative_ontology:measurement(jewi_grid_24, jewish_sovereignty_palestine__post_zionist_reading, stakes_inflation(structural), 2024, 0.52).
narrative_ontology:measurement(jewi_grid_25, jewish_sovereignty_palestine__post_zionist_reading, suppression(class), 1948, 0.48).
narrative_ontology:measurement(jewi_grid_26, jewish_sovereignty_palestine__post_zionist_reading, suppression(class), 2024, 0.68).
narrative_ontology:measurement(jewi_grid_27, jewish_sovereignty_palestine__post_zionist_reading, suppression(individual), 1948, 0.42).
narrative_ontology:measurement(jewi_grid_28, jewish_sovereignty_palestine__post_zionist_reading, suppression(individual), 2024, 0.55).
narrative_ontology:measurement(jewi_grid_29, jewish_sovereignty_palestine__post_zionist_reading, suppression(organizational), 1948, 0.32).
narrative_ontology:measurement(jewi_grid_30, jewish_sovereignty_palestine__post_zionist_reading, suppression(organizational), 2024, 0.62).
narrative_ontology:measurement(jewi_grid_31, jewish_sovereignty_palestine__post_zionist_reading, suppression(structural), 1948, 0.28).
narrative_ontology:measurement(jewi_grid_32, jewish_sovereignty_palestine__post_zionist_reading, suppression(structural), 2024, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_sovereignty_palestine__post_zionist_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(jewish_sovereignty_palestine__post_zionist_reading, 0.12).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__post_zionist_reading, jewish_sovereignty_palestine__liberal_nationalist_reading).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__post_zionist_reading, jewish_sovereignty_palestine__settler_colonial_reading).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__post_zionist_reading, palestinian_right_of_return).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__post_zionist_reading, israeli_settlement_policy).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__post_zionist_reading, law_of_return_asymmetry).

% DUAL FORMULATION NOTE:
% The jewish_sovereignty_palestine kernel decomposes into five structurally distinct constraint stories, each instantiating a different reading with different beneficiaries, victims, and ε-invariant extraction profiles. The post-Zionist reading (this story) asserts that founding coordination has atrophied while extraction persists; the liberal-nationalist reading asserts coordination can be preserved through institutional reform; the settler-colonial reading asserts the entire project is displacement from inception; the cultural-Zionist reading denies the state form is necessary; the religious-Zionist reading asserts theological legitimacy independent of civic equality. Each reading generates different per-seat type classifications from the same stakeholder set because the readings differ on foundational axioms about statehood legitimacy, ethnic privilege necessity, and civic-equality reformability. The post-Zionist reading influences (not forecloses) the settler-colonial reading: demonstrating extraction and mandate obsolescence makes settler-colonial framing more plausible, but does not logically require it. The post-Zionist reading coexists with liberal-nationalist reading: both defend the state form but disagree on whether ethnic privilege is reformable or fundamental.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(jewish_sovereignty_palestine__post_zionist_reading, organized, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
