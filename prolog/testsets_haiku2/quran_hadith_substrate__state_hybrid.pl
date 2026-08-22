% ============================================================================
% CONSTRAINT STORY: quran_hadith_substrate__state_hybrid
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_quran_hadith_substrate__state_hybrid, []).

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
 *   constraint_id: quran_hadith_substrate__state_hybrid
 *   human_readable: State Hybrid Islamic Jurisprudence Application
 *   domain: legal/religious/political
 *
 * SUMMARY:
 *   Modern Islamic states selectively invoke classical Islamic jurisprudence
 *   to claim religious legitimacy while preserving policy autonomy in
 *   economic domains. In this reading, the state is the primary agent of
 *   selection: it applies classical fiqh rulings in family law and criminal
 *   codes (where conservative constituencies expect Islamic application and
 *   international law allows variation), while applying secular or reformist
 *   frameworks in commercial and administrative law (where international
 *   capital and trade relations require predictability and where classical
 *   fiqh constraints would damage state revenue and economic
 *   competitiveness). This is not a coherent jurisprudential position
 *   (neither comprehensive sharia nor principled secular pluralism); it is a
 *   political arrangement where the state instrumentalizes Islamic authority
 *   to maintain legitimacy while reserving unilateral power to decide where
 *   sharia applies. The arrangement produces victims on multiple sides:
 *   traditionalist jurists see their comprehensive vision truncated;
 *   reformist scholars are suppressed when their contextual reasoning
 *   threatens regime stability; conservative constituents experience
 *   cognitive dissonance between sharia-governed family law and
 *   secular-governed commerce; and international observers identify the
 *   incoherence as instrumental rather than principled.
 *
 * KEY AGENTS:
 *   - state_executive_elites: institutional power, arbitrage-grade exit options, primary agenda-setter; benefits from maintaining both legitimacy claim and policy autonomy
 *   - traditionalist_jurists: organized power, constrained exit, payers; their comprehensive fiqh vision is selectively invoked then overruled
 *   - reformist_scholars: moderate power, identity-locked exit, payers; suppressed when their contextual readings threaten regime stability
 *   - conservative_constituents: powerless, trapped, beneficiaries and payers simultaneously; receive classical sharia in family law but bear costs of secular commercial law
 *   - secular_commercial_actors: powerful, mobile exit options, beneficiaries; operate under secular frameworks with high autonomy
 *   - constitutional_court: institutional power, constrained exit, partially coopted agenda-setter; must rationalize selective application via sovereignty doctrine
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quran_hadith_substrate__state_hybrid, 0.38).
domain_priors:suppression_score(quran_hadith_substrate__state_hybrid, 0.52).
domain_priors:theater_ratio(quran_hadith_substrate__state_hybrid, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quran_hadith_substrate__state_hybrid, extractiveness, 0.38).
narrative_ontology:constraint_metric(quran_hadith_substrate__state_hybrid, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(quran_hadith_substrate__state_hybrid, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quran_hadith_substrate__state_hybrid, accessibility_collapse, 0.41).
narrative_ontology:constraint_metric(quran_hadith_substrate__state_hybrid, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quran_hadith_substrate__state_hybrid, tangled_rope).
narrative_ontology:human_readable(quran_hadith_substrate__state_hybrid, "State Hybrid Islamic Jurisprudence Application").
narrative_ontology:topic_domain(quran_hadith_substrate__state_hybrid, "legal/religious/political").

domain_priors:requires_active_enforcement(quran_hadith_substrate__state_hybrid).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quran_hadith_substrate__state_hybrid, '1ed6bdd8-aee0-4a0a-8020-05b52582df2c').
narrative_ontology:cs_kernel_codification('1ed6bdd8-aee0-4a0a-8020-05b52582df2c', fixed_text).
narrative_ontology:cs_authority_grounding('1ed6bdd8-aee0-4a0a-8020-05b52582df2c', extraction).
narrative_ontology:cs_interpretation_layer_present('1ed6bdd8-aee0-4a0a-8020-05b52582df2c').
narrative_ontology:cs_reading_relation('1ed6bdd8-aee0-4a0a-8020-05b52582df2c', quran_hadith_substrate__traditionalist_taqlid, coexists_with).
narrative_ontology:cs_reading_relation('1ed6bdd8-aee0-4a0a-8020-05b52582df2c', quran_hadith_substrate__reformist_ijtihad, influences).
narrative_ontology:cs_axiom('1ed6bdd8-aee0-4a0a-8020-05b52582df2c', foundational, state_authority_overrides_jurisprudential_consistency).
narrative_ontology:cs_axiom_status(state_authority_overrides_jurisprudential_consistency, holdable).
narrative_ontology:cs_axiom_grounding('1ed6bdd8-aee0-4a0a-8020-05b52582df2c', state_authority_overrides_jurisprudential_consistency, conventional).
narrative_ontology:cs_axiom('1ed6bdd8-aee0-4a0a-8020-05b52582df2c', foundational, contextual_application_grounded_in_sovereignty_not_ethics).
narrative_ontology:cs_axiom_status(contextual_application_grounded_in_sovereignty_not_ethics, holdable).
narrative_ontology:cs_axiom_grounding('1ed6bdd8-aee0-4a0a-8020-05b52582df2c', contextual_application_grounded_in_sovereignty_not_ethics, instrumental).
narrative_ontology:cs_reference_frame('1ed6bdd8-aee0-4a0a-8020-05b52582df2c', state_sovereignty_selective_application).
narrative_ontology:cs_drift_state('1ed6bdd8-aee0-4a0a-8020-05b52582df2c', contemporary_international_human_rights_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('1ed6bdd8-aee0-4a0a-8020-05b52582df2c', '').
narrative_ontology:cs_kernel_id(quran_hadith_substrate__state_hybrid, quran_hadith_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quran_hadith_substrate__state_hybrid, state_executive_elites).
narrative_ontology:constraint_beneficiary(quran_hadith_substrate__state_hybrid, secular_commercial_actors).
narrative_ontology:constraint_victim(quran_hadith_substrate__state_hybrid, traditionalist_jurists).
narrative_ontology:constraint_victim(quran_hadith_substrate__state_hybrid, reformist_scholars).
narrative_ontology:constraint_victim(quran_hadith_substrate__state_hybrid, conservative_constituents).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(quran_hadith_substrate__state_hybrid, conservative_constituents).
narrative_ontology:constraint_victim(quran_hadith_substrate__state_hybrid, constitutional_court).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set and enforce differential jurisprudential application: classical sharia in family law and criminal codes (legitimacy signaling to conservative constituents), secular/reformist frameworks in commercial and administrative law (economic flexibility and international compatibility). They justify this as 'contextual application' while reserving the right to shift the boundary—maximizing political cover while preserving policy flexibility. Revenue and state capacity depend on both the legitimacy claim and the economic policy autonomy.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__state_hybrid, state_executive_elites, agenda_setter,
    institutional, generational, arbitrage, national).

% See their comprehensive fiqh vision truncated by state instrumentalization. The constraint selects classical rulings when politically convenient (family law, criminal codes), but abandons classical fiqh principles when they would constrain state economic policy (riba prohibitions, commercial ethics). They cannot force comprehensive application, cannot exit the legal system, and face suppression if they publicly denounce the incoherence. Their moral authority is invoked selectively and then overruled by sovereign decree.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__state_hybrid, traditionalist_jurists, payer,
    organized, generational, constrained, national).

% Advocate contextual ijtihad and contemporary ethical readings of Quran and hadith. The constraint suppresses them by: (a) using classical rulings as the default legitimacy claim (their contemporaneous readings are positioned as 'innovation' or 'Western influence'), (b) restricting their institutional access when their readings threaten state stability, (c) allowing limited operation in commercial law contexts where reformist reasoning aligns with state economic interests, but blocking their application in family/criminal law where classical rulings serve regime legitimacy. Identity as Islamic scholar is fused to engagement with the tradition; exit means abandoning the intellectual community and religious framework.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__state_hybrid, reformist_scholars, payer,
    moderate, biographical, identity_locked, national).

% Receive classical sharia application in family law (marriage, inheritance, child custody rules follow traditional fiqh) and criminal codes (hudud punishments, testimony weight, evidentiary standards follow classical schools). They also bear the cost when state applies secular commercial law that contradicts classical principles (interest-bearing banking, commercial contracts bypassing traditional ethics, gender-mixed commercial spaces), creating cognitive dissonance. They cannot exit the state's jurisdiction and have no institutional voice in redefining the boundaries.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__state_hybrid, conservative_constituents, beneficiary,
    powerless, biographical, trapped, national).
narrative_ontology:stakeholder_secondary_role(quran_hadith_substrate__state_hybrid, conservative_constituents, payer).

% Operate freely under secular commercial and administrative law frameworks. International investors, multinational corporations, and domestic business elites benefit from secular contract law, interest-bearing financial instruments, and administrative procedures that bypass classical fiqh constraints. They have high exit options (relocate capital, diversify jurisdiction) and leverage that exit threat into continued policy accommodation.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__state_hybrid, secular_commercial_actors, beneficiary,
    powerful, biographical, mobile, global).

% Assess the state's human rights compliance, legal consistency, and pluralism framework. They document the selective application of classical rulings in family law (gender-based inequalities, limited divorce rights for women, inheritance asymmetries) and the secular frameworks in commercial law (equal contracting rights, interest-bearing instruments, cross-gender commercial transactions). They produce reports that frame the constraint as incoherent or instrumentalized, but lack enforcement power over sovereign state decisions.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__state_hybrid, international_observer_bodies, observer,
    institutional, generational, analytical, global).

% Theoretically adjudicates conflicts between classical sharia and secular law by invoking state sovereignty doctrine and 'contextual application' reasoning. In practice, they are constrained by executive pressure and must rationalize the selective application. They face suppression when their rulings threaten regime stability (if they declare classical family law unconstitutional, they delegitimize the sovereignty claim; if they declare commercial law violations of sharia, they constrain economic policy). They are partially coopted into legitimizing the constraint.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__state_hybrid, constitutional_court, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(quran_hadith_substrate__state_hybrid, constitutional_court, payer).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(quran_hadith_substrate__state_hybrid, state_executive_elites).
narrative_ontology:fixing_cost_class(quran_hadith_substrate__state_hybrid, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a legal framework that bridges conservative religious constituencies (through classical sharia application in family/criminal law) and international commercial interests (through secular commercial law) within a single state apparatus, avoiding the full commitment to either traditionalist comprehensive sharia or reformist ijtihad. Solves the political problem of maintaining Islamic legitimacy while pursuing modern economic policy.
% TRANSFER_FUNCTION: Moves political legitimacy from traditionalist and reformist jurists to the state executive (the state claims to represent authentic Islamic jurisprudence while reserving unilateral authority to decide which rulings apply where). Moves economic autonomy from conservative constituents and traditionalist jurists to state elites and international commercial actors. Moves intellectual authority from independent scholars to state-controlled religious institutions.
% ABSENT_VOICES: Traditionalist jurists who would demand comprehensive fiqh application in all domains; reformist scholars who would mandate contextual ijtihad everywhere; conservative constituents who would question why sharia is selectively applied; international human-rights observers who would frame the constraint as incoherent instrumentalization.
% DISAPPEARANCE_RATIONALE: If the state abandoned selective jurisprudential application overnight, the state would face a binary choice: commit to comprehensive classical sharia (delegitimizing commercial law, restricting capital flows, alienating secular business actors) or abandon Islamic law frameworks entirely (delegitimizing the regime to conservative constituencies and traditionalist scholars). The current constraint's disappearance would force a choice that the regime's governing coalition currently avoids. Political reorganization would be rapid and severe.
% FOUNDING_PROBLEM: Early postcolonial or neo-modern Islamic states faced the simultaneous need to claim Islamic legitimacy (to consolidate power against secularist competitors and traditionalist challengers), maintain international commercial integration (to access capital and trade), and preserve state autonomy in policy domains where classical fiqh would constrain executive power (taxation, interest-bearing banking, gender-mixed commercial activity). A comprehensive sharia commitment would close external trade; pure secularism would delegitimize the regime.
% FOUNDING_PROBLEM_CORROBORATION: State elites and constitutional courts attest the founding problem remains live, citing ongoing tension between religious constituencies and commercial interests. Traditionalist jurists attest it is a false problem manufactured by state refusal to commit to comprehensive sharia. Reformist scholars attest the founding problem was real but is now solved by more sophisticated ijtihad (contextual reasoning that permits both sharia and modern commerce without incoherence). International observers and human-rights bodies attest the founding problem reflects genuine political pressure but that the state's solution is instrumentalized incoherence rather than principled pluralism. The widest corroboration (outside benefiting parties) comes from scholars documenting how selective application serves regime interests.
narrative_ontology:disappearance_verdict(quran_hadith_substrate__state_hybrid, world_rearranges).
narrative_ontology:founding_problem_status(quran_hadith_substrate__state_hybrid, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quran_hadith_substrate__state_hybrid, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(quran_hadith_substrate__state_hybrid, 'none', 1).
narrative_ontology:epsilon_provenance(quran_hadith_substrate__state_hybrid, 0.38, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(quran_hadith_substrate__state_hybrid_tests).
:- end_tests(quran_hadith_substrate__state_hybrid_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.38) rather than high because the state does provide genuine coordination goods (legal framework that bridges constituencies and facilitates commerce) alongside the extraction. The extraction itself is 'hybrid': it extracts political legitimacy from multiple sources (claims to represent Islamic law while reserving the right to override it), but the magnitude is constrained by the need to maintain both traditionalist and secular constituencies. Suppression is moderate-high (0.52) because maintaining the incoherence requires active enforcement against scholars who would expose it (traditionalist jurists are suppressed if they demand comprehensive application; reformist scholars are suppressed when their readings threaten regime stability). Theater ratio is high (0.58) because a substantial share of the state's jurisprudential activity is performative: invoking classical authorities to legitimize decisions that are driven by political/economic incentives, not by Islamic jurisprudential reasoning. The measurement series shows extractiveness rising from 0.28 to 0.40 over the interval (as regimes mature and become more confident in selective application), theater rising from 0.42 to 0.60 (as the incoherence becomes more visible and more elaborate justifications are needed), and suppression rising from 0.38 to 0.55 (as scholars become more vocal and regimes respond with institutional/legal pressure). The slight decline at endpoint (t=40) may reflect either measurement noise or temporary relaxation when regime confidence is high.
 *
 * PERSPECTIVAL GAP:
 *   From the state executive seat, this is tangled rope: genuine coordination (bridging constituencies and facilitating commerce) plus asymmetric extraction (monopolizing authority to decide where sharia applies). From the traditionalist jurist seat, this is closer to snare: their authority is invoked for legitimacy but overruled in practice, with no exit. From the reformist scholar seat, this is snare when their readings threaten stability (suppressed), but rope when their reasoning aligns with state interests (permitted in commercial law). From the conservative constituent seat, this is rope in family law (genuine benefits, access to classical rules they prefer) but snare in commercial law (secular frameworks imposed without consent). The divergence is structural and seat-based, not observer-relative: the engine computes it from power, exit options, and beneficiary/victim declarations.
 *
 * DIRECTIONALITY LOGIC:
 *   State elites are beneficiaries: they gain political legitimacy, policy autonomy, and institutional authority. Secular commercial actors are beneficiaries: they gain the legal autonomy to operate without classical fiqh constraints. Traditionalist jurists are victims: their comprehensive vision is truncated, their authority is selectively invoked, and they face suppression if they openly challenge the incoherence. Reformist scholars are victims: they are suppressed when their contextual reasoning threatens regime stability (delegitimizes the sovereignty claim that 'the state represents Islamic law'). Conservative constituents are dual-positioned: they benefit from classical sharia in family law (their preferred framework) but are victimized by secular commercial law (which contradicts their ethical expectations). The constraint produces asymmetric directionalities across seats because the state's selectivity benefits some constituencies and harms others depending on which domain they are affected by.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (bridging Islamic legitimacy with modern commerce) is attested as live by state elites but contested by other parties. Traditionalist jurists argue the founding problem is manufactured—they claim comprehensive sharia is both legitimate AND compatible with commerce if the state commits. Reformist scholars argue the founding problem was real but is now solved by sophisticated ijtihad. The disappearance verdict is clearly 'world_rearranges': if selective application vanished, the state would face a binary choice. This mismatch (live founding problem + world_rearranges) suggests the constraint is correctly classified as tangled rope: genuine coordination function (bridging constituencies) AND active enforcement to maintain asymmetric extraction (state authority monopoly). If the constraint were pure rope, the founding problem would be fully solved and disappearance would produce minor rearrangement. If it were pure snare, the founding problem would be dead (solved by the extraction itself) or contested only by victims. The live-and-contested status, paired with world_rearranges, confirms tangled rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coherence_vs_instrumentalization,
    'Is the state''s selective application of classical sharia a coherent jurisprudential position (e.g., a sophisticated form of contextual application), or is it instrumental incoherence driven by political/economic incentives?',
    'Examine the consistency of the state''s jurisprudential reasoning across time and across cases. If the state applies the same jurisprudential principle (e.g., ''maslaha'' or ''public interest'') to justify both selections and exclusions, it is coherent. If the state''s reasoning shifts with political/economic conditions, or if the same principle is invoked to justify contradictory selections, it is instrumental.',
    'If coherent, the constraint reclassifies closer to rope (genuine coordination via a principled legal framework, even if controversial). If instrumental, it confirms tangled rope or snare (extraction masked by incoherent reasoning). The classification is robust to this ambiguity, but the political meaning differs substantially.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(coherence_vs_instrumentalization, empirical, 'Whether selective application is principled jurisprudence or instrumental political reasoning.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the suppression of reformist scholars and traditionalist jurists structural (legal barriers, institutional exclusion, state capacity to punish) or partially internalized (scholars self-censor, internalize the regime''s framing, become identity-fused to the state''s version of Islam)?',
    'Post-regime-change scenarios: if suppression persists after regime change (scholars continue self-censoring, internalize the regime''s framework), it is partially internalized. If suppression immediately dissolves after institutional removal, it is structural. Alternatively, track scholars'' behavior in diaspora: if their output changes substantially when removed from regime pressure, suppression was structural.',
    'If structural, the effective suppression is measurable by institutional capacity. If internalized, the effective suppression is higher than the structural measure suggests—scholars carry the suppression with them even after the constraint is removed. This affects long-term estimates of how quickly alternative jurisprudential frameworks would emerge if the constraint dissolved.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether suppression operates primarily through institutional barriers or through internalized constraints on scholars'' reasoning and self-concept.').

omega_variable(
    kernel_reading_boundary_ambiguity,
    'At what point does selective application of classical sharia transform from STATE_HYBRID (pragmatic state selectivity) into REFORMIST_IJTIHAD (principled contextual reasoning)? Where is the boundary between the readings?',
    'Operationalize the boundary via the axioms: state_hybrid grounds legitimacy in state sovereignty (political authority); reformist_ijtihad grounds it in ethical coherence with Quranic trajectory. When the state begins justifying selections via ethical trajectory or public interest without invoking political sovereignty, the reading migrates toward reformist_ijtihad. When independent scholars explicitly adopt state selectivity as their jurisprudential method (instead of principled ijtihad), the constraint migrates toward state_hybrid.',
    'If the reading migrates, the constraint''s metrics and classification may shift: state_hybrid is tangled rope (coordination + extraction); reformist_ijtihad would be closer to rope (coordination via principled framework, minimal extraction once the principle is established). High migration risk if regime legitimacy degrades and elites begin adopting reformist language to maintain credibility.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_boundary_ambiguity, conceptual, 'Whether the state_hybrid reading remains distinct from reformist_ijtihad or whether selective application eventually reframes itself as principled contextual reasoning.').

omega_variable(
    beneficiary_stability_cross_regime,
    'Are the beneficiary groups (state elites, secular commercial actors) stable across different regime types, or do they shift when the regime changes? Do traditionalist and reformist scholars ever become beneficiaries under some regime configurations?',
    'Comparative institutional analysis: track which groups benefit under different regime types (authoritarian, democratic, theocratic, secular-nationalist). If state_hybrid benefits state elites under all regime types, beneficiaries are stable. If beneficiaries shift (e.g., under democratic regimes, reformist scholars become beneficiaries; under theocratic regimes, traditionalist jurists become beneficiaries), the constraint''s structural relationship to the regime type is contingent.',
    'If beneficiary stability is contingent on regime type, the constraint''s ε value may be regime-dependent. High-authoritarian regimes may show ε=0.40 (state extraction is easier). Democratic regimes may show ε=0.20 (state extraction is constrained by electoral pressure). This affects generalizability of the metrics across different Islamic states.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_stability_cross_regime, empirical, 'Whether beneficiaries are stable across regime types or whether state_hybrid produces different structural relations under different governance arrangements.').

omega_variable(
    reformist_traditionalist_victim_divergence,
    'Are reformist scholars and traditionalist jurists genuinely both victims of this constraint, or is the traditionalist-victim classification an over-read? Do traditionalists actually benefit from the state''s use of classical rulings in family/criminal law, even though their comprehensive vision is truncated?',
    'Examine whether traditionalists'' stated positions distinguish between ''partial application is better than none'' and ''partial application is unjust because it legitimizes state authority over the complete sharia''. If traditionalists voice the former, they are partial beneficiaries (d closer to 0.4–0.6). If they voice the latter, they are victims (d closer to 0.8–1.0). Track institutional behavior: do traditionalist scholars collaborate with state institutions that apply selective sharia, or do they publicly distance themselves?',
    'If traditionalists are genuinely dual-positioned (partial beneficiary + partial victim), the directionality computation must reflect that via directionality_overrides. If they are straightforward victims (the truncation is experienced as delegitimization), the d value is clearer. This affects whether the constraint computes as benefiting or harming the organized conservative constituency.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reformist_traditionalist_victim_divergence, empirical, 'Whether traditionalist jurists are victims of truncation or partial beneficiaries of state-enforced classical-law application, despite their objections to selectivity.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quran_hadith_substrate__state_hybrid, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qura_tr_t0, quran_hadith_substrate__state_hybrid, theater_ratio, 0, 0.42).
narrative_ontology:measurement(qura_tr_t5, quran_hadith_substrate__state_hybrid, theater_ratio, 5, 0.46).
narrative_ontology:measurement(qura_tr_t10, quran_hadith_substrate__state_hybrid, theater_ratio, 10, 0.51).
narrative_ontology:measurement(qura_tr_t20, quran_hadith_substrate__state_hybrid, theater_ratio, 20, 0.58).
narrative_ontology:measurement(qura_tr_t30, quran_hadith_substrate__state_hybrid, theater_ratio, 30, 0.6).
narrative_ontology:measurement(qura_tr_t40, quran_hadith_substrate__state_hybrid, theater_ratio, 40, 0.58).

% Extraction over time
narrative_ontology:measurement(qura_be_t0, quran_hadith_substrate__state_hybrid, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(qura_be_t5, quran_hadith_substrate__state_hybrid, base_extractiveness, 5, 0.32).
narrative_ontology:measurement(qura_be_t10, quran_hadith_substrate__state_hybrid, base_extractiveness, 10, 0.36).
narrative_ontology:measurement(qura_be_t20, quran_hadith_substrate__state_hybrid, base_extractiveness, 20, 0.4).
narrative_ontology:measurement(qura_be_t30, quran_hadith_substrate__state_hybrid, base_extractiveness, 30, 0.38).
narrative_ontology:measurement(qura_be_t40, quran_hadith_substrate__state_hybrid, base_extractiveness, 40, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(qura_su_t0, quran_hadith_substrate__state_hybrid, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(qura_su_t5, quran_hadith_substrate__state_hybrid, suppression_requirement, 5, 0.42).
narrative_ontology:measurement(qura_su_t10, quran_hadith_substrate__state_hybrid, suppression_requirement, 10, 0.47).
narrative_ontology:measurement(qura_su_t20, quran_hadith_substrate__state_hybrid, suppression_requirement, 20, 0.53).
narrative_ontology:measurement(qura_su_t30, quran_hadith_substrate__state_hybrid, suppression_requirement, 30, 0.55).
narrative_ontology:measurement(qura_su_t40, quran_hadith_substrate__state_hybrid, suppression_requirement, 40, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quran_hadith_substrate__state_hybrid, identity_coordination).
narrative_ontology:boltzmann_floor_override(quran_hadith_substrate__state_hybrid, 0.12).
narrative_ontology:affects_constraint(quran_hadith_substrate__state_hybrid, quran_hadith_substrate__traditionalist_taqlid).
narrative_ontology:affects_constraint(quran_hadith_substrate__state_hybrid, quran_hadith_substrate__reformist_ijtihad).

% DUAL FORMULATION NOTE:
% The quran_hadith_substrate kernel is instantiated by three distinct constraint stories: (1) state_hybrid (THIS STORY) — state as jurisprudential selector, grounding legitimacy in political sovereignty; (2) traditionalist_taqlid — classical fiqh schools as binding authority, taqlid as obligation; (3) reformist_ijtihad — contextual ijtihad as jurisprudential method, ethical trajectory as guide. Each reading has distinct ε values (state_hybrid: 0.25–0.45; traditionalist_taqlid: ~0.15 if truly binding, ~0.55 if coercively enforced; reformist_ijtihad: ~0.10 if genuinely pluralistic, ~0.45 if state-suppressed). They are linked via network.affects_constraints: state_hybrid and traditionalist_taqlid coexist as live contested positions; state_hybrid influences reformist_ijtihad (state selectivity creates incentives for reformist scholars to adopt selective reasoning as cover). Do NOT merge these stories into one constraint with measurement-parameter variability; they are structurally distinct readings of the same contested kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(quran_hadith_substrate__state_hybrid, organized, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
