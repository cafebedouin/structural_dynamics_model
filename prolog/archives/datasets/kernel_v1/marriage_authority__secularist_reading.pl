% ============================================================================
% CONSTRAINT STORY: marriage_authority__secularist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_authority__secularist_reading, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: marriage_authority__secularist_reading
 *   human_readable: Marriage Authority Concentrated in Democratic Legislature (Secularist Reading)
 *   domain: constitutional_law/legal_pluralism/family_law
 *
 * SUMMARY:
 *   The secularist reading of marriage authority consolidation treats the
 *   concentration of family law power in the democratic legislature as a
 *   necessary step toward universal gender equality and rational legal
 *   coherence. Under this reading, personal law pluralism is an anachronistic
 *   transitional state — a remnant of pre-modern legal fragmentation — that
 *   must be eliminated via Uniform Civil Code to achieve gender parity and
 *   jurisdictional clarity. This reading experiences significant perspectival
 *   conflict: the democratic legislature and gender-rights coalition perceive
 *   the constraint as coordination and progressive reform; minority religious
 *   communities perceive it as extractive subordination of their autonomous
 *   legal regimes. The secularist reading is not presented as a choice among
 *   equally legitimate frameworks but as the inevitable endpoint of
 *   modernization and enlightenment. This positioning naturalizes what is
 *   actually a contestable institutional arrangement, making it a candidate
 *   for false-summit detection when analyzed cross-reading.
 *
 * KEY AGENTS:
 *   - Secular Modernist Coalition: Primary beneficiary (institutional/arbitrage) — consolidates authority over family law, implements gender equality provisions, eliminates legal fragmentation; includes progressive judges, secular reformers, women's rights organizations
 *   - Minority Religious Communities: Primary victim (powerless/trapped) — lose autonomous jurisdiction over marriage, divorce, succession; cannot exit secular code or preserve personal law autonomy within the state
 *   - Democratic Legislature: Institutional beneficiary (institutional/arbitrage) — centralizes marriage authority, resolves inter-community jurisdictional conflicts, enforces uniform gender provisions
 *   - Communal Religious Leadership: Organized victim (organized/constrained) — retain some negotiating capacity through political process but face systematic subordination of communal authority structures
 *   - Gender Rights Advocates: Secondary beneficiary (moderate/constrained) — benefit from elimination of gender-discriminatory personal law provisions but constrained by political process and counter-mobilization
 *   - Judicial Harmonization Coalition: Path-builder (organized/mobile) — advocates incremental case-by-case convergence as alternative to wholesale legislative elimination; sees temporary structure that can be sunsetted
 *   - Constitutional Establishment Clause: Performative maintainer (institutional/arbitrage) — persists through inertia, performs secular neutrality despite diminished functional differentiation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_authority__secularist_reading, 0.58).
domain_priors:suppression_score(marriage_authority__secularist_reading, 0.68).
domain_priors:theater_ratio(marriage_authority__secularist_reading, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_authority__secularist_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(marriage_authority__secularist_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(marriage_authority__secularist_reading, theater_ratio, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_authority__secularist_reading, tangled_rope).
narrative_ontology:human_readable(marriage_authority__secularist_reading, "Marriage Authority Concentrated in Democratic Legislature (Secularist Reading)").
narrative_ontology:topic_domain(marriage_authority__secularist_reading, "constitutional_law/legal_pluralism/family_law").

domain_priors:requires_active_enforcement(marriage_authority__secularist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_authority__secularist_reading, '3a24b536-dba1-4ef3-bb24-5f097c0f8039').
narrative_ontology:cs_kernel_codification('3a24b536-dba1-4ef3-bb24-5f097c0f8039', fixed_text).
narrative_ontology:cs_authority_grounding('3a24b536-dba1-4ef3-bb24-5f097c0f8039', extraction).
narrative_ontology:cs_interpretation_layer_present('3a24b536-dba1-4ef3-bb24-5f097c0f8039').
narrative_ontology:cs_reading_relation('3a24b536-dba1-4ef3-bb24-5f097c0f8039', marriage_authority__communal_autonomy_reading, forecloses).
narrative_ontology:cs_reading_relation('3a24b536-dba1-4ef3-bb24-5f097c0f8039', marriage_authority__gender_rights_reading, influences).
narrative_ontology:cs_reading_relation('3a24b536-dba1-4ef3-bb24-5f097c0f8039', marriage_authority__federalist_millet_reading, coexists_with).
narrative_ontology:cs_reading_relation('3a24b536-dba1-4ef3-bb24-5f097c0f8039', marriage_authority__judicial_harmonization_reading, influences).
narrative_ontology:cs_axiom('3a24b536-dba1-4ef3-bb24-5f097c0f8039', foundational, secular_legislature_necessary_for_gender_equality).
narrative_ontology:cs_axiom_status(secular_legislature_necessary_for_gender_equality, holdable).
narrative_ontology:cs_axiom_grounding('3a24b536-dba1-4ef3-bb24-5f097c0f8039', secular_legislature_necessary_for_gender_equality, empirically_contingent).
narrative_ontology:cs_axiom('3a24b536-dba1-4ef3-bb24-5f097c0f8039', foundational, modernization_as_inevitable_consolidation).
narrative_ontology:cs_axiom_status(modernization_as_inevitable_consolidation, holdable).
narrative_ontology:cs_axiom_grounding('3a24b536-dba1-4ef3-bb24-5f097c0f8039', modernization_as_inevitable_consolidation, empirically_contingent).
narrative_ontology:cs_axiom('3a24b536-dba1-4ef3-bb24-5f097c0f8039', secondary, religious_autonomy_incompatible_with_state_equality).
narrative_ontology:cs_axiom_status(religious_autonomy_incompatible_with_state_equality, holdable).
narrative_ontology:cs_axiom_grounding('3a24b536-dba1-4ef3-bb24-5f097c0f8039', religious_autonomy_incompatible_with_state_equality, empirically_contingent).
narrative_ontology:cs_reference_frame('3a24b536-dba1-4ef3-bb24-5f097c0f8039', secular_democratic_supremacy).
narrative_ontology:cs_drift_state('3a24b536-dba1-4ef3-bb24-5f097c0f8039', contemporary_plural_resistance, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('3a24b536-dba1-4ef3-bb24-5f097c0f8039', '').
narrative_ontology:cs_kernel_id(marriage_authority__secularist_reading, marriage_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_authority__secularist_reading, secular_modernist_coalition).
narrative_ontology:constraint_beneficiary(marriage_authority__secularist_reading, gender_rights_advocates).
narrative_ontology:constraint_victim(marriage_authority__secularist_reading, minority_religious_communities).
narrative_ontology:constraint_victim(marriage_authority__secularist_reading, personal_law_autonomy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MINORITY RELIGIOUS COMMUNITY MEMBER (SNARE) — Cannot exit the secular legislative framework or preserve personal law autonomy within the state. Family law determinations (marriage, divorce, succession) are subordinated to secular code without genuine consent or exit option. Maximum extraction experienced: forced assimilation of intimate legal regimes.
constraint_indexing:constraint_classification(marriage_authority__secularist_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: PROGRESSIVE GENDER RIGHTS ADVOCATE (TANGLED ROPE) — Constrained by the political process and by counter-mobilization from communal autonomy defenders, but benefits from secular legislative authority that can eliminate gender-discriminatory personal law provisions. Mixed position: genuine coordination function (uniform marriage age, spousal equality) exists alongside asymmetric extraction (minority religious communities lose autonomy).
constraint_indexing:constraint_classification(marriage_authority__secularist_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: DEMOCRATIC LEGISLATURE (ROPE) — Experiences consolidation of marriage authority as a coordination function: unified family law reduces jurisdictional conflict, enables uniform rights enforcement, and centralizes enforcement of gender equality. The legislature sees this as solving a collective action problem (fragmentation of family law across communities). Net beneficiary with arbitrage options — can relocate legal authority, interpret precedent, amend codes.
constraint_indexing:constraint_classification(marriage_authority__secularist_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: COMMUNAL RELIGIOUS LEADERSHIP (TANGLED ROPE) — Organized capacity to resist and negotiate (religious councils, bar associations, political blocs) but constrained by legislative supremacy. Benefits from coordination function (reduced inter-community conflict, clearer authority) but bears extraction: loss of autonomous jurisdiction over marriage, divorce, succession within community. Active enforcement of secular code dismantles parallel communal authority structures.
constraint_indexing:constraint_classification(marriage_authority__secularist_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: JUDICIAL HARMONIZATION COALITION (SCAFFOLD) — Courts and legal reformers advocating incremental convergence of personal laws without wholesale code elimination. See the constraint as temporary: gradual judicial interpretation, case-by-case harmonization, and negotiated reform could bridge secular and communal frames without full zero-sum supremacy. Sunset clause implicit: as religious and secular law converge on gender equality through case law, the need for legislative override diminishes.
constraint_indexing:constraint_classification(marriage_authority__secularist_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: CONSTITUTIONAL ESTABLISHMENT CLAUSE (PITON) — The constitutional principle of secular state authority persists through institutional inertia despite diminished functional differentiation. The 'secular authority' is increasingly performative: personal law communities have adapted, married couples often navigate multiple overlapping systems, enforcement gaps reveal the complexity. The establishment clause maintains itself through theater (repeated claims of secular neutrality) rather than through functional supervision of marriage authority.
constraint_indexing:constraint_classification(marriage_authority__secularist_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal frame, the consolidation of marriage authority in democratic legislatures reflects an inevitable modernization logic: complex societies require unified legal codes, plural personal laws create jurisdictional conflict, and gender equality is incompatible with communal discretion. This perspective naturalizes what is actually a contested institutional choice. Engine will flag false summit: identifiable beneficiaries exist (secular coalition, gender advocates); the constraint is not a natural law but a political choice.
constraint_indexing:constraint_classification(marriage_authority__secularist_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_authority__secularist_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(marriage_authority__secularist_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(marriage_authority__secularist_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(marriage_authority__secularist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(marriage_authority__secularist_reading, TR),
    TR >= 0.70.

:- end_tests(marriage_authority__secularist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The secularist reading achieves genuine coordination benefits (uniform rights, jurisdictional clarity, gender equality enforcement) but at the cost of subordinating minority communal autonomy. The extraction is not pure predation — there are real coordination gains. However, the framing as 'inevitable modernization' rather than 'politically contingent choice' conceals that the secularist coalition benefits from the authority consolidation while communities bear the autonomy cost. The value reflects that meaningful coordination exists alongside asymmetric distribution of authority. Suppression (0.68): Moderately high. Minority communities cannot exit the secular framework without violating state law, cannot preserve personal law without legislative permission, and face social cost to any attempted exit (conversion, relocation). Suppression has increased over the interval (0.42→0.68) as legislative authority has been consolidated and enforcement mechanisms strengthened. Theater ratio (0.52): Moderate. The secularist reading performs 'rational legal science' and 'inevitable modernization' as justifications for what is a political choice about authority concentration. The performance is semi-transparent: few actors deny the political dimension entirely, but the naturalization-as-modernism framing obscures the institutional choice. Theater has grown over the interval as the reading has become institutionalized.
 *
 * PERSPECTIVAL GAP:
 *   The secularist reading demonstrates maximum perspectival divergence. The legislature and secular coalition see coordination and progress (Rope); religious communities see extraction and subordination (Snare); the communal leadership sees mixed loss of autonomy alongside some coordination benefit (Tangled Rope); judicial reformers see a temporary problem solvable through incremental harmonization (Scaffold); the constitutional framework performs its own neutrality while being incrementally captured by the secular coalition (Piton); the analytical observer risks naturalizing a political choice as a law of modernization (Mountain/False Summit). The gaps reveal that this is not a natural law of social evolution but a contestable reading of which institutional arrangement should govern marriage authority.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality (d) is determined by the agent's structural position relative to authority consolidation. The secular coalition benefits from centralization (low d, negative χ — experiences the constraint as beneficial coordination). Minority communities bear the subordination cost (high d, high χ — experiences maximum extraction). The legislature gains centralized authority (low d). Religious leadership retains some negotiating power through organization but faces structural subordination (moderate-high d). Gender advocates benefit but are constrained by political process (moderate d). Judicial reformers see the constraint as temporary and reformable, so their experienced extraction is dampened by perceived exit (mobile exit_options). The constitutional framework is captured by the secular coalition (it performs neutrality while enforcing secularist positions), so its directionality is complex — formally neutral (d~0.5) but functionally captured (d→0.2 in effect). Minorities face genuine trapped status: they cannot exit the secular code without violating state law or abandoning legal personality within the state.
 *
 * MANDATROPHY ANALYSIS:
 *   The secularist reading resolves the mandatrophy by acknowledging the genuine coordination function (uniform family law, gender equality enforcement) while recognizing the asymmetric extraction (minority autonomy loss). The constraint is not pure coordination (rope) because it involves subordination of one reading to another through legislative supremacy, not negotiated agreement. It is not pure extraction (snare) because legitimate gender-equality gains exist and some communities benefit from jurisdictional clarity. Tangled Rope classification captures this: genuine coordination coexists with asymmetric extraction, active enforcement is required (legislative machinery, judicial authority), and the distribution reflects political power rather than consent. The mandatrophy is resolved by recognizing that what appears as 'inevitable modernization' is actually a contestable reading that concentrates benefits on the secular coalition and costs on minority communities.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    gender_equality_vs_communal_autonomy_incommensurability,
    'Can gender equality requirements and communal law autonomy be satisfied simultaneously, or are they logically incompatible within a single legal framework?',
    'Comparative case analysis: jurisdictions that have negotiated hybrid frameworks (India''s constitutional pluralism, Indonesia''s Islamic law integration); assessment of whether communal law can evolve internal gender equality norms without external legislative mandate.',
    'If compatible: the snare classification weakens — victims retain structural options for internal reform. Constraint reclassifies toward tangled_rope or even rope. If genuinely incompatible: snare classification strengthens — extraction is not eliminable through communal evolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gender_equality_vs_communal_autonomy_incommensurability, conceptual, 'Whether gender equality and communal autonomy are logically compatible').

omega_variable(
    ucc_inevitability_modernization_or_political_choice,
    'Is consolidation of marriage authority in secular legislature an inevitable modernization process, or a contingent political choice that reflects particular power configurations?',
    'Historical comparison: how have other societies achieved gender equality without eliminating personal law pluralism? (Morocco, Malaysia, Indonesia hybrid models vs full secular consolidation in India/Tunisia). Analysis of whether the inevitability framing naturalizes a specific geopolitical coalition''s interests.',
    'If contingent choice: the mountain perspective is reclassified as false summit. The ''natural law'' reading loses analytical force. If genuinely inevitable: analytical observer perspective is vindicated; the constraint reflects structural reality of modernization.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ucc_inevitability_modernization_or_political_choice, conceptual, 'Whether UCC is inevitable modernization or contingent political choice').

omega_variable(
    conversion_and_exit_capacity_in_personal_law_pluralism,
    'In a pluralist regime, do community members have meaningful capacity to exit their personal law system (through conversion, relocation, or opt-out of jurisdiction), or is the ''option'' purely formal?',
    'Ethnographic and legal analysis: case studies of conversion or jurisdiction-exit attempts; documentation of social costs (community expulsion, family loss, economic penalty) that technically legal exit mechanisms impose. Assessment of whether formal exit options meaningfully reduce experienced extraction.',
    'If exit is substantively available: victims'' exit_options should be upgraded from trapped to constrained or mobile. Perspectives reclassify; overall extractiveness may decrease. If exit is formal-only: the trapped classification is confirmed; the constraint''s suppression is confirmed as structural.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(conversion_and_exit_capacity_in_personal_law_pluralism, empirical, 'Whether exit from personal law jurisdiction has meaningful capacity').

omega_variable(
    kernel_reading_contest_formalized_or_unacknowledged,
    'Do the sibling readings (communal autonomy, gender rights, federalism, judicial harmonization) form a coherent, publicly contested system of alternatives, or are they implicit/unacknowledged positions that different actors advance without explicit framework recognition?',
    'Constitutional discourse analysis: examination of whether the readings appear as explicit alternative positions in judicial opinions, legislative debate, and academic literature. Assessment of the degree to which different actors are conscious of defending readings as competing coherent positions vs advancing particular interests without framing them as such.',
    'If fully formalized: the kernel structure and reading relations are conscious, which affects the possibility of deliberate, principled choice among readings. If unacknowledged: readings function as implicit struggle, and the constraint carries higher theater (deliberate framing disagreement as technical rather than structural).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest_formalized_or_unacknowledged, conceptual, 'Whether competing readings are formalized or implicit in public discourse').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_authority__secularist_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_auth_sec_tr_t0, marriage_authority__secularist_reading, theater_ratio, 0, 0.38).
narrative_ontology:measurement(marr_auth_sec_tr_t15, marriage_authority__secularist_reading, theater_ratio, 15, 0.45).
narrative_ontology:measurement(marr_auth_sec_tr_t30, marriage_authority__secularist_reading, theater_ratio, 30, 0.52).

% Extraction over time
narrative_ontology:measurement(marr_auth_sec_be_t0, marriage_authority__secularist_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(marr_auth_sec_be_t15, marriage_authority__secularist_reading, base_extractiveness, 15, 0.48).
narrative_ontology:measurement(marr_auth_sec_be_t30, marriage_authority__secularist_reading, base_extractiveness, 30, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(marr_auth_sec_su_t0, marriage_authority__secularist_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(marr_auth_sec_su_t15, marriage_authority__secularist_reading, suppression_requirement, 15, 0.62).
narrative_ontology:measurement(marr_auth_sec_su_t30, marriage_authority__secularist_reading, suppression_requirement, 30, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_authority__secularist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(marriage_authority__secularist_reading, marriage_authority__communal_autonomy_reading).
narrative_ontology:affects_constraint(marriage_authority__secularist_reading, marriage_authority__gender_rights_reading).
narrative_ontology:affects_constraint(marriage_authority__secularist_reading, marriage_authority__federalist_millet_reading).
narrative_ontology:affects_constraint(marriage_authority__secularist_reading, marriage_authority__judicial_harmonization_reading).

% DUAL FORMULATION NOTE:
% The marriage_authority kernel is decomposed into five separate constraint stories, one per reading. Each story has its own ε value, its own beneficiary/victim structure, and its own perspectival gap. The secularist reading (this story) treats personal law pluralism as transitional (ε=0.58, Tangled Rope). The communal autonomy reading treats legislative supremacy as extractive subordination (ε≈0.72, Snare). The gender rights reading treats authority distribution as hybrid (ε≈0.45, Tangled Rope with different beneficiaries). The federalist reading treats multiple simultaneous authority levels as legitimate (ε≈0.25, Rope). The judicial harmonization reading treats legislative elimination as unnecessarily zero-sum (ε≈0.40, Scaffold). Each reading instantiates a different constraint because they make incommensurable claims about what legitimates authority consolidation or pluralism. The ε-invariance principle requires separate stories: measuring via the secularist frame gives one ε; measuring via the communal frame gives another; the difference is not noise but structural — different readings measure different constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(marriage_authority__secularist_reading, institutional, 0.28).
constraint_indexing:directionality_override(marriage_authority__secularist_reading, powerless, 0.94).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
