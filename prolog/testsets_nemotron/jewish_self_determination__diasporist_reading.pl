% ============================================================================
% CONSTRAINT STORY: jewish_self_determination__diasporist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: jewish_self_determination__diasporist_reading
 *   human_readable: Diasporist Reading of Jewish Self-Determination: Survival Through Pluralism
 *   domain: political_philosophy/nationalism_studies/postcolonial_theory
 *
 * SUMMARY:
 *   The diasporist reading holds that Jewish collective survival and
 *   flourishing are best secured through diaspora pluralism and minority
 *   rights within liberal democratic states, not through territorial
 *   sovereignty in a militarized nation-state. It views Zionism as a
 *   dangerous deviation that ties Jewish fate to state violence, endangers
 *   diaspora Jews by association with Israeli actions, and monopolizes the
 *   definition of 'Jewish interest' to suppress alternatives. This reading
 *   was once a live coordination mechanism (Bundist, Territorialist, cultural
 *   autonomist movements) but has atrophied since 1948 as Zionist
 *   institutions captured communal resources, defined communal boundaries,
 *   and marginalized non-Zionist Jewish politics. The constraint now persists
 *   largely as identity performance — diaspora institutions maintain the
 *   forms of pluralist coordination while substantive collective
 *   problem-solving has migrated to Zionist frameworks.
 *
 * KEY AGENTS:
 *   - diaspora_jewish_communities: Primary beneficiary (moderate/constrained) — maintains distinct identity through pluralist institutions
 *   - jews_coerced_into_zionist_framework: Primary victim (moderate/identity_locked) — pressured to identify with Israeli state policies
 *   - jews_endangered_by_israeli_association: Victim (moderate/constrained) — faces antisemitism redirected from Israeli actions
 *   - zionist_institutional_hegemony: Agenda setter (institutional/arbitrage) — controls communal resources and definition of Jewish interest
 *   - liberal_democratic_states: Secondary beneficiary (institutional/mobile) — gains stable minority integration model
 *   - palestinians: Excluded (powerless/trapped) — displacement enables the territorial sovereignty the diasporist reading rejects
 *   - analytical_observer: Observer (analytical/analytical) — sees full structure of suppression and atrophied coordination
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_self_determination__diasporist_reading, 0.55).
domain_priors:suppression_score(jewish_self_determination__diasporist_reading, 0.68).
domain_priors:theater_ratio(jewish_self_determination__diasporist_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_self_determination__diasporist_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(jewish_self_determination__diasporist_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(jewish_self_determination__diasporist_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_self_determination__diasporist_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(jewish_self_determination__diasporist_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_self_determination__diasporist_reading, piton).
narrative_ontology:human_readable(jewish_self_determination__diasporist_reading, "Diasporist Reading of Jewish Self-Determination: Survival Through Pluralism").
narrative_ontology:topic_domain(jewish_self_determination__diasporist_reading, "political_philosophy/nationalism_studies/postcolonial_theory").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_self_determination__diasporist_reading, '495e88ca-0e70-4838-ac12-132709a3aa09').
narrative_ontology:cs_kernel_codification('495e88ca-0e70-4838-ac12-132709a3aa09', formalized).
narrative_ontology:cs_authority_grounding('495e88ca-0e70-4838-ac12-132709a3aa09', extraction).
narrative_ontology:cs_interpretation_layer_present('495e88ca-0e70-4838-ac12-132709a3aa09').
narrative_ontology:cs_reading_relation('495e88ca-0e70-4838-ac12-132709a3aa09', jewish_self_determination__liberal_nationalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('495e88ca-0e70-4838-ac12-132709a3aa09', jewish_self_determination__indigenous_return_reading, coexists_with).
narrative_ontology:cs_reading_relation('495e88ca-0e70-4838-ac12-132709a3aa09', jewish_self_determination__religious_covenant_reading, coexists_with).
narrative_ontology:cs_reading_relation('495e88ca-0e70-4838-ac12-132709a3aa09', jewish_self_determination__settler_colonial_reading, influences).
narrative_ontology:cs_axiom('495e88ca-0e70-4838-ac12-132709a3aa09', foundational, jewish_survival_requires_pluralism_not_sovereignty).
narrative_ontology:cs_axiom_status(jewish_survival_requires_pluralism_not_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('495e88ca-0e70-4838-ac12-132709a3aa09', jewish_survival_requires_pluralism_not_sovereignty, empirically_contingent).
narrative_ontology:cs_axiom('495e88ca-0e70-4838-ac12-132709a3aa09', foundational, zionism_endangers_diaspora_jews_by_association).
narrative_ontology:cs_axiom_status(zionism_endangers_diaspora_jews_by_association, holdable).
narrative_ontology:cs_axiom_grounding('495e88ca-0e70-4838-ac12-132709a3aa09', zionism_endangers_diaspora_jews_by_association, empirically_contingent).
narrative_ontology:cs_axiom('495e88ca-0e70-4838-ac12-132709a3aa09', secondary, minority_rights_frame_protects_collective_identity).
narrative_ontology:cs_axiom_status(minority_rights_frame_protects_collective_identity, holdable).
narrative_ontology:cs_axiom_grounding('495e88ca-0e70-4838-ac12-132709a3aa09', minority_rights_frame_protects_collective_identity, conventional).
narrative_ontology:cs_reference_frame('495e88ca-0e70-4838-ac12-132709a3aa09', pre_state_diasporist_autonomism).
narrative_ontology:cs_drift_state('495e88ca-0e70-4838-ac12-132709a3aa09', post_1948_zionist_hegemony_consolidation, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('495e88ca-0e70-4838-ac12-132709a3aa09', '').
narrative_ontology:cs_kernel_id(jewish_self_determination__diasporist_reading, jewish_self_determination).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_self_determination__diasporist_reading, diaspora_jewish_communities).
narrative_ontology:constraint_beneficiary(jewish_self_determination__diasporist_reading, liberal_democratic_states).
narrative_ontology:constraint_victim(jewish_self_determination__diasporist_reading, jews_coerced_into_zionist_framework).
narrative_ontology:constraint_victim(jewish_self_determination__diasporist_reading, jews_endangered_by_israeli_association).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(jewish_self_determination__diasporist_reading, diaspora_jewish_communities).
narrative_ontology:constraint_vindicates(jewish_self_determination__diasporist_reading, minority_rights_protect_collective_survival).
narrative_ontology:constraint_vindicates(jewish_self_determination__diasporist_reading, pluralism_over_territorial_sovereignty).
narrative_ontology:constraint_vindicates(jewish_self_determination__diasporist_reading, zionism_as_militarized_deviation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain distinct Jewish identities through cultural, educational, and religious institutions in diaspora. Benefit from minority rights protections and pluralist frameworks. Also pay into communal federations that fund Zionist institutions and face antisemitism redirected from Israeli state actions. Exit from the communal framework means loss of cultural infrastructure and social networks; exit from the Zionist identification means communal ostracism.
narrative_ontology:constraint_stakeholder(jewish_self_determination__diasporist_reading, diaspora_jewish_communities, beneficiary,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(jewish_self_determination__diasporist_reading, diaspora_jewish_communities, payer).

% Jews who reject or question Zionism but face communal pressure to identify with Israel as central to Jewish identity. Experience identity policing in synagogues, campus organizations, federations, and family settings. Their Jewishness is questioned; access to communal resources (Birthright, educational grants, communal employment) is conditional on Zionist affirmation. Exit requires breaking identity fusion — 'who am I if not a Jew who supports Israel?' — making it psychologically prohibitive despite no legal barrier.
narrative_ontology:constraint_stakeholder(jewish_self_determination__diasporist_reading, jews_coerced_into_zionist_framework, payer,
    moderate, biographical, identity_locked, global).

% Diaspora Jews who face antisemitic violence, harassment, or discrimination explicitly linked to Israeli military actions (Gaza wars, settlement expansion, Palestinian dispossession). The association is imposed externally — they cannot individually disavow it because the Zionist framework claims to represent all Jews. Their physical safety is the extraction's collateral damage. Exit is constrained: they can privately dissent but the structural association remains; public dissent invites communal retaliation (see jews_coerced_into_zionist_framework).
narrative_ontology:constraint_stakeholder(jewish_self_determination__diasporist_reading, jews_endangered_by_israeli_association, payer,
    moderate, immediate, constrained, global).

% Network of organizations (Jewish Agency, WZO, major federations, ADL, AIPAC, campus Hillels, Israeli government diaspora affairs) that define Jewish communal boundaries, control resource allocation, set the agenda for 'Jewish interest,' and enforce Zionist consensus. They administer the constraint by marginalizing non-Zionist Jewish politics, funding only Zionist-aligned initiatives, and policing communal discourse. They face no meaningful accountability: no competing institution controls Jewish communal resources, and the Israeli state backs their authority. Exit is arbitrage-grade — they could reform but have no incentive to.
narrative_ontology:constraint_stakeholder(jewish_self_determination__diasporist_reading, zionist_institutional_hegemony, agenda_setter,
    institutional, generational, arbitrage, global).

% States with strong minority rights frameworks (US, Canada, Western Europe) that gain a model of Jewish integration through pluralism rather than territorial nationalism. They benefit from stable, loyal minority communities that participate in democratic life. However, they also face pressure to equate criticism of Israel with antisemitism, constraining their foreign policy and domestic discourse. Exit is mobile: they could adopt different minority rights models or resist the conflation, but political incentives align with the Zionist framework.
narrative_ontology:constraint_stakeholder(jewish_self_determination__diasporist_reading, liberal_democratic_states, beneficiary,
    institutional, generational, mobile, national).

% The indigenous population displaced by the territorial sovereignty the diasporist reading rejects. Their displacement is the condition of possibility for the Zionist framework that suppresses the diasporist alternative. They have no voice in Jewish self-determination discourse — the kernel's readings are all intra-Jewish. Their situation is the structural outside of the kernel: the settler_colonial_reading centers them, but the other readings treat them as background condition or demographic threat. Exit is trapped: they cannot leave the condition of dispossession.
narrative_ontology:constraint_stakeholder(jewish_self_determination__diasporist_reading, palestinians, excluded,
    powerless, generational, trapped, national).

% Sees the full structure: a once-live coordination mechanism (diasporist pluralism) atrophied into performative maintenance (piton) because a rival constraint (Zionist sovereignty) captured the institutional infrastructure of Jewish collective life. The suppression is real but operates through identity policing and resource control rather than state violence (within diaspora). The victims are Jews themselves — coerced into a framework that endangers them physically and politically. The beneficiaries are diffuse: diaspora communities get identity preservation but pay through conformity; liberal states get integration model but pay through policy capture. No concentrated beneficiary captures the extraction — the piton signature.
narrative_ontology:constraint_stakeholder(jewish_self_determination__diasporist_reading, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jewish_self_determination__diasporist_reading, diffuse).
narrative_ontology:fixing_cost_class(jewish_self_determination__diasporist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Jewish collective survival in modernity without territorial sovereignty: maintaining distinct identity, mutual aid, cultural continuity, and political safety through minority rights, pluralist institutions, and integration into liberal democratic states.
% TRANSFER_FUNCTION: Moves communal conformity and resource allocation from diaspora Jews to Zionist institutional hegemony. Jews who would pursue diasporist politics transfer their participation, funding, and silence to the Zionist framework. In return, they receive communal acceptance and access to Jewish institutional life. The extraction is not monetary but identity-political: the price of Jewish communal belonging is Zionist affirmation.
% ABSENT_VOICES: Palestinians are structurally excluded from the kernel 'Jewish self-determination' — all five readings are intra-Jewish. Non-Zionist Jewish traditions (Bundism, Territorialism, cultural autonomism, Satmar anti-Zionism) are marginalized within Jewish discourse, their institutional infrastructure captured or defunded. Jews of color, Mizrahi Jews, and converts often experience the Zionist framework as erasing their specific histories. These voices would object to the kernel's framing if present, but the kernel's codification in Zionist institutions keeps them out.
% DISAPPEARANCE_RATIONALE: If the diasporist reading vanished overnight (i.e., the last institutional memory of non-territorial Jewish self-determination disappeared), Jewish collective life would be wholly captured by the Zionist framework. No internal alternative would remain. Antisemitism would be entirely conflated with anti-Zionism. Diaspora Jews would lose the conceptual vocabulary for Jewish existence outside sovereignty. The world rearranges: the constraint's disappearance eliminates the last structural check on Zionist hegemony's definition of Jewish fate.
% FOUNDING_PROBLEM: Jewish collective survival in modernity: how to maintain distinct identity, physical safety, and cultural continuity without a territorial state, given centuries of persecution, expulsion, and genocide in diaspora.
% FOUNDING_PROBLEM_CORROBORATION: Zionist institutions attest the founding problem is solved by sovereignty (statehood = survival). Anti-Zionist Jewish groups (Jewish Voice for Peace, Satmar, Bundist remnants) and diasporist scholars (Simon Dubnow's autonomism, contemporary diasporist theorists like Daniel Boyarin, Atalia Omer) attest the problem persists and sovereignty creates new existential risks. Liberal democratic states' minority rights frameworks corroborate that pluralist survival is possible — but also show its fragility (rising antisemitism). No consensus exists; the problem's status is genuinely contested across the kernel's readings.
narrative_ontology:disappearance_verdict(jewish_self_determination__diasporist_reading, world_rearranges).
narrative_ontology:founding_problem_status(jewish_self_determination__diasporist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_self_determination__diasporist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(jewish_self_determination__diasporist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jewish_self_determination__diasporist_reading, 0.55, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness (0.55) is moderate: the constraint extracts compliance from Jews who would prefer diasporist politics but face communal ostracism, funding denial, and identity policing. Suppression (0.68) is substantial: the constraint's persistence depends on active exclusion of alternatives from communal institutions, not passive preference. Theater ratio (0.42) is elevated: diaspora institutions perform pluralist coordination (cultural festivals, educational programs) while substantive collective survival strategy has been outsourced to Zionist frameworks. Accessibility collapse (0.58) is moderate: alternatives exist but are institutionally marginalized. Resistance (0.45) is present but fragmented: anti-Zionist Jewish groups (JVP, IfNotNow, Satmar) resist but lack institutional power. The interval (0-80) tracks from pre-1948 (t=0) to present (t=80), showing extraction accumulation and theater growth as Zionist hegemony consolidated.
 *
 * PERSPECTIVAL GAP:
 *   From the diaspora community seat (moderate/constrained), the constraint appears as a fragile coordination mechanism under siege — a rope degrading into piton. From the Zionist institutional seat (institutional/arbitrage), the same structure appears as a resolved question — diasporism is a historical dead end, the constraint is a mountain (Jewish fate = sovereignty). From the coerced Jew seat (moderate/identity_locked), it is a snare: extraction without coordination benefit, exit blocked by identity fusion. The engine computes this divergence from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Diaspora communities are declared beneficiaries: they receive identity preservation and minority rights protections. But they are also partly payers: they fund Zionist institutions through communal federations and face antisemitism redirected from Israeli actions. Jews coerced into the Zionist framework are primary victims: they bear identity policing, communal exclusion, and physical danger from association with Israeli state violence. Jews endangered by association are victims without the identity-lock — they can exit the identification but not the antisemitic projection. Liberal democratic states are incidental beneficiaries: they receive a model of minority integration. Zionist institutional hegemony is the agenda setter with arbitrage-grade exit: it controls the definition of Jewish interest and faces no accountability. Palestinians are excluded: their displacement is the territorial sovereignty's condition of possibility, but they have no voice in Jewish self-determination discourse.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — Jewish collective survival in modernity — was live. The diasporist solution (cultural autonomy, minority rights, pluralism) was a genuine coordination mechanism. That problem is now contested: Zionists argue it is solved by sovereignty; diasporists argue sovereignty creates new existential risks. The arrangement (diasporist institutions) persists despite the founding problem's contested status because Zionist hegemony actively suppresses the alternative, not because the alternative fails. This is mandatrophy: the constraint's coordination function has atrophied, but it is maintained theatrically by identity inertia and communal nostalgia, while the actual collective survival strategy has migrated to a different constraint (Zionist sovereignty). The piton classification captures this: atrophied function, maintained by performance, no concentrated beneficiary capturing the extraction — the extraction is diffuse (communal conformity, identity policing) and the administrator (Zionist institutions) could change it but bears no cost for maintaining the suppression.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is the diasporist reading a live constraint on Jewish collective life, or an atrophied alternative suppressed by Zionist institutional hegemony?',
    'Trace institutional resource flows: if diaspora institutions (bunds, landsmanshaftn, Yiddish cultural networks, non-Zionist educational systems) have been systematically defunded, marginalized, or absorbed by Zionist organizations since 1948, the constraint is a piton — a former coordination mechanism maintained only theatrically. If they operate independently with living membership, it is a rope or tangled rope.',
    'If piton, the reading''s claimed coordination function is largely performative; the constraint persists through identity inertia and communal nostalgia rather than active collective problem-solving. This reclassifies the reading from a viable alternative to a degraded remnant.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, empirical, 'Whether diasporist institutions retain functional coordination capacity or survive only as identity performance').

omega_variable(
    coercion_mechanism_ambiguity,
    'Is the suppression of diasporist alternatives structural (institutional capture, funding monopolies, communal ostracism) or internalized (Jews believing Zionism is the only authentic Jewish politics)?',
    'Post-exit trajectory study: do Jews who reject Zionism face sustained material penalties (career, funding, communal access) or primarily social-psychological pressure? If material penalties persist after ideological exit, suppression is structural. If pressure dissolves upon internal de-identification, it is substantially internalized.',
    'If internalized, the constraint''s effective suppression is higher than structural measures suggest — the target carries the suppression with them. This affects whether the constraint is a snare (structural coercion) or a piton with identity-locked internalization.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coercion_mechanism_ambiguity, conceptual, 'Structural vs. internalized suppression of diasporist alternatives').

omega_variable(
    diaspora_survival_contingency,
    'Does diaspora Jewish survival genuinely depend on host-state tolerance (making it a coordination problem), or does the reading romanticize a condition of structural vulnerability?',
    'Comparative historical analysis: correlate diaspora community longevity with host-state minority rights regimes across centuries. If survival tracks rights protections, the coordination function is real. If communities persist despite rights denial (through internal cohesion, economic niches, migration), the reading overstates host-state dependency.',
    'If survival is less contingent on host tolerance than claimed, the reading''s coordination function is weaker — pushing toward piton (atrophied coordination story covering identity persistence). If highly contingent, the reading identifies a genuine collective-action problem (rope/tangled rope).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(diaspora_survival_contingency, empirical, 'Whether diaspora survival''s contingency on minority rights is structural or overstated').

omega_variable(
    framing_underdetermination,
    'Does the kernel ''Jewish self-determination'' admit the diasporist reading as a coherent framing, or does the kernel''s historical codification in Zionist institutions foreclose it?',
    'Analyze whether major Jewish institutional bodies (Jewish Agency, WZO, major federations) formally recognize non-territorial self-determination as a valid Jewish political project, or treat it as oxymoronic. If the kernel''s authoritative interpreters define self-determination as inherently territorial, the diasporist reading is structurally foreclosed within the kernel''s own framework — an external critique, not an internal reading.',
    'If foreclosed, this constraint is not a reading OF the kernel but a constraint ABOUT the kernel — changing cs_structure.authority_grounding and reading_relations. The engine would compute foreclosure via cs_axiom_contradiction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(framing_underdetermination, conceptual, 'Whether the diasporist reading is an internal kernel reading or an external critique').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_self_determination__diasporist_reading, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jsd_diasporist_tr_t0, jewish_self_determination__diasporist_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(jsd_diasporist_tr_t0, observed).
narrative_ontology:measurement(jsd_diasporist_tr_t20, jewish_self_determination__diasporist_reading, theater_ratio, 20, 0.25).
narrative_ontology:measurement_basis(jsd_diasporist_tr_t20, observed).
narrative_ontology:measurement(jsd_diasporist_tr_t40, jewish_self_determination__diasporist_reading, theater_ratio, 40, 0.32).
narrative_ontology:measurement_basis(jsd_diasporist_tr_t40, observed).
narrative_ontology:measurement(jsd_diasporist_tr_t60, jewish_self_determination__diasporist_reading, theater_ratio, 60, 0.38).
narrative_ontology:measurement_basis(jsd_diasporist_tr_t60, observed).
narrative_ontology:measurement(jsd_diasporist_tr_t80, jewish_self_determination__diasporist_reading, theater_ratio, 80, 0.42).
narrative_ontology:measurement_basis(jsd_diasporist_tr_t80, observed).

% Extraction over time
narrative_ontology:measurement(jsd_diasporist_be_t0, jewish_self_determination__diasporist_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement_basis(jsd_diasporist_be_t0, observed).
narrative_ontology:measurement(jsd_diasporist_be_t20, jewish_self_determination__diasporist_reading, base_extractiveness, 20, 0.42).
narrative_ontology:measurement_basis(jsd_diasporist_be_t20, observed).
narrative_ontology:measurement(jsd_diasporist_be_t40, jewish_self_determination__diasporist_reading, base_extractiveness, 40, 0.48).
narrative_ontology:measurement_basis(jsd_diasporist_be_t40, observed).
narrative_ontology:measurement(jsd_diasporist_be_t60, jewish_self_determination__diasporist_reading, base_extractiveness, 60, 0.52).
narrative_ontology:measurement_basis(jsd_diasporist_be_t60, observed).
narrative_ontology:measurement(jsd_diasporist_be_t80, jewish_self_determination__diasporist_reading, base_extractiveness, 80, 0.55).
narrative_ontology:measurement_basis(jsd_diasporist_be_t80, observed).

% Suppression requirement over time
narrative_ontology:measurement(jsd_diasporist_su_t0, jewish_self_determination__diasporist_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement_basis(jsd_diasporist_su_t0, observed).
narrative_ontology:measurement(jsd_diasporist_su_t20, jewish_self_determination__diasporist_reading, suppression_requirement, 20, 0.45).
narrative_ontology:measurement_basis(jsd_diasporist_su_t20, observed).
narrative_ontology:measurement(jsd_diasporist_su_t40, jewish_self_determination__diasporist_reading, suppression_requirement, 40, 0.55).
narrative_ontology:measurement_basis(jsd_diasporist_su_t40, observed).
narrative_ontology:measurement(jsd_diasporist_su_t60, jewish_self_determination__diasporist_reading, suppression_requirement, 60, 0.62).
narrative_ontology:measurement_basis(jsd_diasporist_su_t60, observed).
narrative_ontology:measurement(jsd_diasporist_su_t80, jewish_self_determination__diasporist_reading, suppression_requirement, 80, 0.68).
narrative_ontology:measurement_basis(jsd_diasporist_su_t80, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_self_determination__diasporist_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(jewish_self_determination__diasporist_reading, 0.08).
narrative_ontology:affects_constraint(jewish_self_determination__diasporist_reading, jewish_self_determination__liberal_nationalist_reading).
narrative_ontology:affects_constraint(jewish_self_determination__diasporist_reading, jewish_self_determination__indigenous_return_reading).
narrative_ontology:affects_constraint(jewish_self_determination__diasporist_reading, jewish_self_determination__religious_covenant_reading).
narrative_ontology:affects_constraint(jewish_self_determination__diasporist_reading, jewish_self_determination__settler_colonial_reading).
narrative_ontology:affects_constraint(jewish_self_determination__diasporist_reading, zionist_institutional_hegemony).
narrative_ontology:affects_constraint(jewish_self_determination__diasporist_reading, diaspora_jewish_institutions).
narrative_ontology:affects_constraint(jewish_self_determination__diasporist_reading, antisemitism_monitoring_industry).

% DUAL FORMULATION NOTE:
% The kernel 'jewish_self_determination' decomposes into five constraint stories, each with distinct epsilon, beneficiary/victim structures, and claimed types. The diasporist reading is the only one claiming piton (atrophied alternative) and identifying Zionist hegemony as the suppressing structure. The liberal_nationalist and indigenous_return readings claim rope/tangled_rope (active coordination with extraction). The religious_covenant reading claims mountain (divine obligation). The settler_colonial reading claims snare (extraction via displacement). All five are linked via affects_constraints. The diasporist reading's network edges point to the readings it structurally influences (by suppressing their alternatives) and the institutional structures that suppress it.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(jewish_self_determination__diasporist_reading, moderate, 0.75).
constraint_indexing:directionality_override(jewish_self_determination__diasporist_reading, institutional, 0.1).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
