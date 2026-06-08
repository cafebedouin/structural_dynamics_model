% ============================================================================
% CONSTRAINT STORY: parsi_communal_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_parsi_communal_reading, []).

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
 *   constraint_id: parsi_communal_reading
 *   human_readable: Parsi Communal Marriage Authority (1936 Act Codified Tradition)
 *   domain: comparative_law/constitutional_pluralism/religious_governance
 *
 * SUMMARY:
 *   The Parsi Marriage and Divorce Act of 1936 codified religious authority
 *   over marriage within the Zoroastrian Parsi community, establishing Fire
 *   Temple councils and Parsi judges as the sole authorities for solemnizing
 *   marriages within the community. The central constraint is endogamy: to
 *   access community-sanctioned marriage and inheritance rights, Parsi
 *   individuals must marry within the faith. This constraint reveals the
 *   structural mechanism of constitutive pluralism — minority communities
 *   carving out jurisdictional space within nation-states — alongside the
 *   extraction dynamics that minoritarian arrangements can conceal. The Parsi
 *   reading instantiates one interpretation of marriage authority: grounded
 *   in religious community autonomy, enforced through inheritance and social
 *   recognition mechanisms, justified by the demographic survival imperative
 *   of a diaspora minority (global Parsi population ≈ 100,000 and declining).
 *   The constraint's manifestation is a bifurcated marriage system: Parsis
 *   marrying within the faith access community property law, inheritance
 *   rules, and social standing; those marrying outside must use civil courts,
 *   losing communal standing and facing exclusion from Parsi trusts and
 *   religious rites. The measurement trajectory shows rising theater_ratio
 *   (1936–2010, peaking at 0.48) and extractiveness (1936–2010, peaking at
 *   0.41), then both declining modestly (2010–2025), suggesting recent
 *   pressure from civil rights advocacy and courtroom challenges to the
 *   constraint's enforcement. The declining extractiveness from 2010–2025 may
 *   reflect either relaxation of enforcement intensity or reduced
 *   effectiveness of the constraint due to expanding civil marriage
 *   alternatives.
 *
 * KEY AGENTS:
 *   - Parsi Religious Authority (Fire Temple Councils, Parsi Judges): Institutional beneficiary (institutional/arbitrage) — the 1936 Act concentrates marriage solemnization power within their authority structure, legitimizing their role as keepers of communal law
 *   - Parsi Community Continuity Narrative: Abstract beneficiary (non-agent, but vindicates demographic preservation doctrine) — the constraint's operation vindicates the principle that minorities must maintain boundary control to persist
 *   - Interfaith Couples & Converts: Primary victims (identity_locked / constrained) — structurally mobile (civil courts available) but identity-fused into Parsi community through heritage, family, or long-term participation; the endogamy requirement forecloses marriage within the religious framework they identify with
 *   - Parsi Families Seeking Endogamous Marriages: Moderate agents (moderate/constrained) — benefit from coordinated partner search and inheritance stability within community but also bear some extraction if they seek partners outside approved circles
 *   - Civil Court System & Human Rights Coalition: Organized alternative (organized/mobile) — offers exit pathway but constrained by inheritance and community belonging implications
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing community boundary maintenance as universal law rather than seeing the constructed institutional arrangement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(parsi_communal_reading, 0.35).
domain_priors:suppression_score(parsi_communal_reading, 0.48).
domain_priors:theater_ratio(parsi_communal_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(parsi_communal_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(parsi_communal_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(parsi_communal_reading, theater_ratio, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(parsi_communal_reading, tangled_rope).
narrative_ontology:human_readable(parsi_communal_reading, "Parsi Communal Marriage Authority (1936 Act Codified Tradition)").
narrative_ontology:topic_domain(parsi_communal_reading, "comparative_law/constitutional_pluralism/religious_governance").

domain_priors:requires_active_enforcement(parsi_communal_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(parsi_communal_reading, '1a2199bc-6699-45f1-8c90-e8fa303c756a').
narrative_ontology:cs_kernel_codification('1a2199bc-6699-45f1-8c90-e8fa303c756a', formalized).
narrative_ontology:cs_authority_grounding('1a2199bc-6699-45f1-8c90-e8fa303c756a', lineage).
narrative_ontology:cs_interpretation_layer_present('1a2199bc-6699-45f1-8c90-e8fa303c756a').
narrative_ontology:cs_reading_relation('1a2199bc-6699-45f1-8c90-e8fa303c756a', parsi_communal_reading__hindu_codified_reading, coexists_with).
narrative_ontology:cs_reading_relation('1a2199bc-6699-45f1-8c90-e8fa303c756a', parsi_communal_reading__muslim_shariat_reading, coexists_with).
narrative_ontology:cs_reading_relation('1a2199bc-6699-45f1-8c90-e8fa303c756a', parsi_communal_reading__christian_canonical_reading, coexists_with).
narrative_ontology:cs_reading_relation('1a2199bc-6699-45f1-8c90-e8fa303c756a', parsi_communal_reading__secular_civil_reading, influences).
narrative_ontology:cs_axiom('1a2199bc-6699-45f1-8c90-e8fa303c756a', foundational, minority_religious_community_marriage_autonomy).
narrative_ontology:cs_axiom_status(minority_religious_community_marriage_autonomy, holdable).
narrative_ontology:cs_axiom_grounding('1a2199bc-6699-45f1-8c90-e8fa303c756a', minority_religious_community_marriage_autonomy, deontological).
narrative_ontology:cs_axiom('1a2199bc-6699-45f1-8c90-e8fa303c756a', foundational, endogamy_essential_to_demographic_continuity).
narrative_ontology:cs_axiom_status(endogamy_essential_to_demographic_continuity, holdable).
narrative_ontology:cs_axiom_grounding('1a2199bc-6699-45f1-8c90-e8fa303c756a', endogamy_essential_to_demographic_continuity, empirically_contingent).
narrative_ontology:cs_reference_frame('1a2199bc-6699-45f1-8c90-e8fa303c756a', parsi_communal_religious_autonomy_within_nation_state).
narrative_ontology:cs_drift_state('1a2199bc-6699-45f1-8c90-e8fa303c756a', contemporary_human_rights_pressure_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('1a2199bc-6699-45f1-8c90-e8fa303c756a', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(parsi_communal_reading, marriage_authority_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(parsi_communal_reading, parsi_religious_authority).
narrative_ontology:constraint_beneficiary(parsi_communal_reading, parsi_community_continuity).
narrative_ontology:constraint_victim(parsi_communal_reading, individuals_rejected_endogamy_requirement).
narrative_ontology:constraint_victim(parsi_communal_reading, interfaith_couples).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INTERFAITH COUPLE OR CONVERTED PARTNER (SNARE) — Structurally mobile (could relocate, access civil courts) but identity-locked into Parsi identity through heritage, family bonds, or long-term community participation. The endogamy requirement forecloses marriage within the community framework. Exit would require abandoning not just the marriage partner but the entire identity-constituted-through-community. No coordination benefit to offset extraction.
constraint_indexing:constraint_classification(parsi_communal_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(local))).

% PERSPECTIVE 2: PARSI FAMILY SEEKING ENDOGAMOUS MARRIAGE (TANGLED ROPE) — Constrained by community membership and inheritance customs but also benefits from the coordinated endogamy mechanism that preserves community genetic/cultural continuity. The constraint both coordinates family alliance formation within community and extracts from those who reject its boundary conditions. Moderate experienced extraction — constrained but not powerless, benefiting and harmed simultaneously.
constraint_indexing:constraint_classification(parsi_communal_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: PARSI RELIGIOUS AUTHORITY / FIRE TEMPLE HIERARCHY (ROPE) — Institutional actor with arbitrage capacity (can choose to enforce or relax interpretation). Experiences the constraint as pure coordination: the 1936 Act codified their traditional authority to solemnize marriages within the community, which preserves their institutional role. The constraint legitimizes and concentrates religious authority. Net beneficiary — extraction runs toward this actor.
constraint_indexing:constraint_classification(parsi_communal_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: PARSI DEMOGRAPHIC CONTINUITY NARRATIVE (PITON) — The constraint's original function was demographic preservation through endogamy — a real coordination problem in the 19th-20th century when Parsi population faced assimilation pressure. This function has substantially atrophied. Modern enforcement maintains the constraint through theatrical adherence to tradition ('who are we without endogamy?') rather than because endogamy still solves a live coordination problem. Current demographic data shows declining Parsi population DESPITE the constraint — suggesting the constraint no longer achieves its stated purpose. The maintenance is performative: community leadership invokes tradition to justify enforcement even as the constraint's functional mandate disappears.
constraint_indexing:constraint_classification(parsi_communal_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: CIVIL COURT ALTERNATIVE & REFORM COALITION (TANGLED ROPE) — Organized agents (human rights groups, civil marriage advocates, secular Parsis seeking reform) see the 1936 Act as creating an unjust bifurcation: those who want community-sanctioned marriage must accept endogamy; those who reject it lose community standing and inheritance rights. The constraint both coordinates inheritance structures (genuine coordination function) AND extracts through boundary enforcement. Mobile exit (civil courts provide alternative marriage framework) but constrained by inheritance implications and community belonging. The coalition perceives moderate extraction — the coordination is real but asymmetrically beneficial.
constraint_indexing:constraint_classification(parsi_communal_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / UNIVERSAL COMMUNITY AUTONOMY VIEW (MOUNTAIN) — From a civilizational/universal perspective, the right of a minority religious community to define membership boundaries is seen as an irreducible natural right — communities cannot coordinate without boundary maintenance; endogamy rules are inherent to community identity persistence. This perspective naturalizes the endogamy requirement as a universal property of community continuity. However, the structural data reveals a false summit: the constraint shows identifiable beneficiaries (religious authority, community continuity narrative), evidence of active enforcement despite declining function (piton signals), and victims (interfaith couples, converts) — all indicators of a constructed institutional arrangement rather than a natural law.
constraint_indexing:constraint_classification(parsi_communal_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(parsi_communal_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(parsi_communal_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(parsi_communal_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(parsi_communal_reading, TR),
    TR >= 0.70.

:- end_tests(parsi_communal_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.35, baseline): Moderate. The constraint extracts from interfaith couples through denial of community marriage sanction and inheritance access, but does not employ violent coercion — exit exists via civil marriage (reducing trapped-ness and d values). However, for identity-locked individuals, exit carries psychological/identity cost not fully captured in pure material cost models. The moderate value reflects that extraction is real but mediated by alternative pathways. The peak at 0.41 (2010) likely reflects maximum enforcement intensity before recent court challenges and reform advocacy began fragmenting institutional unity. The current 0.35 reflects either enforcement relaxation or reduced fear of enforcement due to civil court precedents. Suppression (0.48, baseline): Moderate-high. Barriers to interfaith marriage within the community framework include legal prohibition (the 1936 Act restricts Fire Temple solemnization to Parsis), social ostracism (rejection from Parsi social and business networks), inheritance barriers (Parsi succession law designed for endogamous families), and knowledge barriers (many Parsi youth raised in the diaspora may not know alternatives to community marriage). The suppression is not total — civil courts exist and can solemnize marriages — but it is substantial for identity-locked individuals. The slight decline (0.54–0.48, 1960–2025) reflects both broadened civil court recognition and community normalization of interfaith relationships among younger Parsis. Theater ratio (0.38, baseline): Moderate. The 1936 Act's codified procedures (registration with Fire Temple judges, ritual requirements) are functional — they do solemnize marriages and structure inheritance — rather than purely performative. However, the enforcement of endogamy itself has become increasingly theatrical: community leaders invoke 'tradition' and 'survival' to justify the requirement, yet demographic data shows Parsi population declining despite enforcement. The rising trajectory (0.25→0.48, 1936–2010) reflects accumulating gap between the constraint's stated function (demographic survival through boundary maintenance) and actual outcome (population decline). The recent decline (0.48→0.38, 2010–2025) may reflect either community acknowledgment of the mandate's atrophy or increased civil marriage alternatives reducing the theater's effective stage.
 *
 * PERSPECTIVAL GAP:
 *   This constraint generates a sharp perspectival divergence between beneficiaries and victims. The religious authority perspective (rope) sees pure coordination: the 1936 Act legitimized their traditional role in a modern legal framework. The interfaith couple perspective (snare) sees pure extraction: they are barred from the marriage framework they identify with. The family perspective (tangled_rope) experiences both: they benefit from coordinated endogamous partner search but also constrain their children's choices. The demographic narrative perspective (piton) experiences degradation: the constraint was designed to maintain population but manifestly does not — enforcement persists through inertia. The reform coalition perspective (tangled_rope) sees extraction through a coordination lens: inheritance law genuinely coordinates family property, but the endogamy requirement asymmetrically benefits those who accept it. The analytical observer perspective (mountain, false summit candidate) risks naturalizing the constraint as inherent to community survival when the structural data reveals it as an institutional arrangement with identifiable beneficiaries and victims. The perspectival gap is maximal between the interfaith victim's snare experience and the religious authority's rope experience — the gap reveals the extraction mechanism: the same institutional structure that coordinates marriage and inheritance for the community coordinators, denies access and belongs to the non-conformers.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) values diverge sharply across positions. Religious authorities exhibit d ≈ 0.1–0.15 (full beneficiaries with arbitrage exit, identity fused with institutional role) — their effective extraction chi is near-zero or slightly negative (they subsidize the system). Interfaith couples with identity_locked exit exhibit d ≈ 0.85–0.95 (nearly full targets, despite civil court exit pathway — the identity lock anchors them within community framework and they bear extraction within that frame). Parsi families with endogamous partners exhibit d ≈ 0.45–0.55 (symmetric: they benefit from coordinated marriage market within community but also bear extraction if they seek partners outside). Civil court users with mobile exit exhibit d ≈ 0.35 (constrained but mobile, moderate directionality — they experience extraction through inheritance loss but have navigated to low-cost exit). The derivation chain follows beneficiary/victim declarations + exit options: victims (interfaith couples, individuals rejected by endogamy rule) feed high d; beneficiaries (religious authority, community continuity narrative) feed low d; identity_locked exit modulates d upward compared to mere constrained, because identity fusion prevents exit exercise even when alternatives exist materially.
 *
 * MANDATROPHY ANALYSIS:
 *   The Parsi constraint exhibits classic mandatrophy: the constraint was designed to solve a live coordination problem (demographic preservation of a diaspora minority in the face of assimilation pressure, early-to-mid 20th century). The mandate was real — Parsi population faced genuine threat. However, measurement data shows the mandate has become atrophied: Parsi population has declined despite (or perhaps because of) the endogamy constraint. The constraint persists through theatrical invocation ('this is our tradition,' 'our survival depends on it') rather than because endogamy solves the stated demographic problem. The piton perspective captures this: enforcement is maintained by inertia and institutional identity (the Fire Temple councils ARE the marriage authority; abolishing the constraint would dissolve their role), not by demonstrated functional success. The mandatrophy is not yet formally resolved — the constraint is still enforced — but the measurement trajectory and the piton classification signal its atrophy. The ethical path forward is to acknowledge the mandate's death and either sunset the constraint or redesign it to achieve demographic continuity through means that do not extract from identity-locked individuals.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_endogamy,
    'Is endogamy a natural law of community boundary maintenance, or a historically constructed institutional arrangement that benefits specific actors within the Parsi community?',
    'Historical analysis comparing Parsi endogamy enforcement pre-1936 vs post-1936 vs contemporary; examination of comparable minority communities (Jewish, Armenian, Zoroastrian diaspora) that maintain identity without legal endogamy codification; assessment of whether current enforcement persists because of functional necessity or institutional inertia',
    'If natural law: mountain classification sustained; community autonomy outweighs individual choice. If constructed: false summit confirmed; the constraint is tangled_rope at analytical level; individual exit rights should be weighed against community preservation claims.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_endogamy, empirical, 'Whether endogamy is essential to community persistence or contingent institutional arrangement').

omega_variable(
    demographic_mandate_obsolescence,
    'Does endogamy enforcement currently solve the demographic preservation problem it was designed to address, or does it represent mandatrophy — institutional persistence despite function atrophy?',
    'Longitudinal demographic data: Parsi population trend 1936–present; correlation analysis between endogamy enforcement intensity and population stability; comparison with outcome under relaxed enforcement regimes (Israel''s Law of Return, contemporary Jewish diaspora identity maintenance without legal endogamy); interviews with Parsi community leaders on perceived necessity vs tradition invocation',
    'If mandate live: constraint''s enforcement is justified by functional necessity. If mandate dead: the constraint qualifies as piton (performative maintenance of degraded function); theater_ratio should increase, and the ethical case for enforcement substantially weakens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(demographic_mandate_obsolescence, empirical, 'Whether the demographic preservation mandate the constraint was designed to solve still exists').

omega_variable(
    reading_kernel_identity_ambiguity,
    'What constitutes the marriage authority kernel for the Parsi reading — is it the 1936 Act as formalized text, or the underlying communal tradition the Act claims to codify?',
    'Textual analysis of the 1936 Act against pre-1936 Parsi customary practice; interview data on Fire Temple interpretation authority (are they bound by the text or by tradition?); case law showing whether courts have allowed reinterpretation of the Act to reflect evolving communal practice; examination of whether Parsi community accepts the Act as immutable or subject to communal deliberation',
    'If kernel is the text: authority_grounding = formalized; interpretation is boundary-constrained. If kernel is tradition: authority_grounding = distributed (Fire Temple councils as carriers of living tradition); text is merely one codification point; substantial interpretive flexibility and potential for reform. Different reading_relations follow: textual reading forecloses reform; traditional reading coexists_with reform advocates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_kernel_identity_ambiguity, conceptual, 'Is the kernel the 1936 Act text or the communal tradition it claims to codify?').

omega_variable(
    interfaith_integration_counterfactual,
    'Would relaxing the endogamy requirement preserve Parsi community identity and continuity, or would it accelerate assimilation and demographic decline?',
    'Comparative case studies: Jewish communities in diaspora with and without formal endogamy rules (contrast Israeli law of return + inclusive marriage with conservative diaspora communities + restrictive marriage rules); outcome measurements on Parsi identity transmission, language maintenance, religious practice continuity in interfaith vs endogamous households; survey of Parsi identity-locked individuals on whether endogamy requirement is necessary to their sense of belonging',
    'If relaxation preserves identity: the extraction mechanism (endogamy enforcement) is not functionally necessary; victims are bearing real costs for questionable benefit; false summit is confirmed. If relaxation accelerates decline: the constraint''s extraction serves a genuine coordination function; the ethical case for enforcement is stronger, but still requires weighing individual liberty against collective survival.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(interfaith_integration_counterfactual, empirical, 'Impact of endogamy relaxation on Parsi identity continuity').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(parsi_communal_reading, 1936, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(parsi_theater_1936_codification, parsi_communal_reading, theater_ratio, 1936, 0.25).
narrative_ontology:measurement(parsi_tr_t1960, parsi_communal_reading, theater_ratio, 1960, 0.32).
narrative_ontology:measurement(parsi_tr_t1990, parsi_communal_reading, theater_ratio, 1990, 0.42).
narrative_ontology:measurement(parsi_tr_t2010, parsi_communal_reading, theater_ratio, 2010, 0.48).
narrative_ontology:measurement(parsi_tr_t2025, parsi_communal_reading, theater_ratio, 2025, 0.38).

% Extraction over time
narrative_ontology:measurement(parsi_extract_1936_codification, parsi_communal_reading, base_extractiveness, 1936, 0.28).
narrative_ontology:measurement(parsi_be_t1960, parsi_communal_reading, base_extractiveness, 1960, 0.32).
narrative_ontology:measurement(parsi_be_t1990, parsi_communal_reading, base_extractiveness, 1990, 0.38).
narrative_ontology:measurement(parsi_be_t2010, parsi_communal_reading, base_extractiveness, 2010, 0.41).
narrative_ontology:measurement(parsi_be_t2025, parsi_communal_reading, base_extractiveness, 2025, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(parsi_suppress_1936_codification, parsi_communal_reading, suppression_requirement, 1936, 0.52).
narrative_ontology:measurement(parsi_su_t1960, parsi_communal_reading, suppression_requirement, 1960, 0.54).
narrative_ontology:measurement(parsi_su_t1990, parsi_communal_reading, suppression_requirement, 1990, 0.51).
narrative_ontology:measurement(parsi_su_t2010, parsi_communal_reading, suppression_requirement, 2010, 0.46).
narrative_ontology:measurement(parsi_su_t2025, parsi_communal_reading, suppression_requirement, 2025, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(parsi_communal_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(parsi_communal_reading, 0.12).
narrative_ontology:affects_constraint(parsi_communal_reading, hindu_codified_reading).
narrative_ontology:affects_constraint(parsi_communal_reading, muslim_shariat_reading).
narrative_ontology:affects_constraint(parsi_communal_reading, christian_canonical_reading).
narrative_ontology:affects_constraint(parsi_communal_reading, secular_civil_reading).
narrative_ontology:affects_constraint(parsi_communal_reading, parsi_inheritance_law_constraint).

% DUAL FORMULATION NOTE:
% The Parsi constraint family decomposes into two linked constraints: (1) parsi_communal_reading — the marriage authority and endogamy rule (this story), with ε=0.35 reflecting moderate extraction from interfaith couples; (2) parsi_inheritance_law_constraint — the property succession system designed for endogamous families, with potentially higher ε reflecting deeper structural asymmetry. The two constraints are linked: endogamy enforcement makes inheritance law coherent, but inheritance law also makes endogamy costly to violate (identity_locked agents face not just social ostracism but property loss). This story focuses on the marriage authority reading; the inheritance constraint is a separate story. Network.affects_constraints links both to the marriage kernel's sibling readings and to the inheritance story to show the family structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
