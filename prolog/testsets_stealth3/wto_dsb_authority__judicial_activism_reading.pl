% ============================================================================
% CONSTRAINT STORY: wto_dsb_authority__judicial_activism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_wto_dsb_authority__judicial_activism_reading, []).

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
    narrative_ontology:coordination_type/2,
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
 *   constraint_id: wto_dsb_authority__judicial_activism_reading
 *   human_readable: WTO Dispute Settlement as Illegitimate Judicial Legislation (Judicial-Activism Reading)
 *   domain: legal/international_trade_governance
 *
 * SUMMARY:
 *   The standing arrangement under contest is the WTO's binding
 *   dispute-settlement operation as it has developed since 1995: compulsory
 *   adjudication before panels and the Appellate Body, compliance
 *   obligations, and authorized retaliation against non-compliance. This
 *   story instantiates the judicial-activism reading of the wto_dsb_authority
 *   kernel. On this reading the arrangement's coordination story —
 *   rules-based settlement replacing power-based retaliation — is real, but
 *   it has been wrapped around a growing extractive core: adjudicators create
 *   obligations the membership never negotiated, through interpretive moves
 *   the institution describes as mere clarification, enforced by retaliation
 *   authorization and locked in by the impossibility of treaty exit at
 *   acceptable cost. Epsilon is assessed over that standing arrangement by
 *   this reading's own lights: what is transferred is regulatory autonomy and
 *   legislative authority, moved from member governments and their domestic
 *   institutions to Geneva adjudicators and export interests, without
 *   consent. This is one file of a three-story constraint family; the sibling
 *   readings and their epsilon deltas are documented in kernel_context and
 *   network.dual_formulation_note. KEY AGENTS (by structural relationship):
 *   see commentary.key_agents.
 *
 * KEY AGENTS:
 *   - wto_adjudicative_institution: agenda-setter and gain-recipient (institutional/constrained) — issues the rulings, controls precedent, compounds its own authority with each expansive move
 *   - member_state_governments: primary target (powerful/trapped) — bear the transferred obligations; exit requires abandoning the entire treaty
 *   - domestic_regulatory_bodies: structural target (powerless/trapped) — their measures are adjudicated without a seat of their own
 *   - sovereigntist_member_states: resisting target (powerful/constrained) — block appointments, object on the record, refuse replacement arrangements
 *   - export_oriented_trading_interests: beneficiary (organized/mobile) — collect market-access gains when measures are struck down
 *   - small_open_trading_economies: dual-positioned (moderate/trapped) — shielded by rules-based settlement, bound by doctrine they cannot shape
 *   - domestic_civil_society_groups: excluded (powerless/trapped) — non-commercial values with no standing in the forum
 *   - trade_law_scholarship: analytical observer (analytical/analytical) — maps doctrine against the negotiating record
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(wto_dsb_authority__judicial_activism_reading, 0.7).
domain_priors:suppression_score(wto_dsb_authority__judicial_activism_reading, 0.6).
domain_priors:theater_ratio(wto_dsb_authority__judicial_activism_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(wto_dsb_authority__judicial_activism_reading, extractiveness, 0.7).
narrative_ontology:constraint_metric(wto_dsb_authority__judicial_activism_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(wto_dsb_authority__judicial_activism_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(wto_dsb_authority__judicial_activism_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(wto_dsb_authority__judicial_activism_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(wto_dsb_authority__judicial_activism_reading, tangled_rope).
narrative_ontology:human_readable(wto_dsb_authority__judicial_activism_reading, "WTO Dispute Settlement as Illegitimate Judicial Legislation (Judicial-Activism Reading)").
narrative_ontology:topic_domain(wto_dsb_authority__judicial_activism_reading, "legal/international_trade_governance").

domain_priors:requires_active_enforcement(wto_dsb_authority__judicial_activism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(wto_dsb_authority__judicial_activism_reading, '8f8f094e-17a5-41ea-9ce5-6bd5ed8b8ab3').
narrative_ontology:cs_kernel_codification('8f8f094e-17a5-41ea-9ce5-6bd5ed8b8ab3', fixed_text).
narrative_ontology:cs_authority_grounding('8f8f094e-17a5-41ea-9ce5-6bd5ed8b8ab3', extraction).
narrative_ontology:cs_interpretation_layer_present('8f8f094e-17a5-41ea-9ce5-6bd5ed8b8ab3').
narrative_ontology:cs_reading_relation('8f8f094e-17a5-41ea-9ce5-6bd5ed8b8ab3', wto_dsb_authority__binding_referee_reading, forecloses).
narrative_ontology:cs_reading_relation('8f8f094e-17a5-41ea-9ce5-6bd5ed8b8ab3', wto_dsb_authority__advisory_coordination_reading, forecloses).
narrative_ontology:cs_axiom('8f8f094e-17a5-41ea-9ce5-6bd5ed8b8ab3', foundational, obligation_creation_requires_member_consent).
narrative_ontology:cs_axiom_status(obligation_creation_requires_member_consent, holdable).
narrative_ontology:cs_axiom_grounding('8f8f094e-17a5-41ea-9ce5-6bd5ed8b8ab3', obligation_creation_requires_member_consent, conventional).
narrative_ontology:cs_axiom('8f8f094e-17a5-41ea-9ce5-6bd5ed8b8ab3', secondary, interpretive_gap_filling_constitutes_legislation).
narrative_ontology:cs_axiom_status(interpretive_gap_filling_constitutes_legislation, holdable).
narrative_ontology:cs_axiom_grounding('8f8f094e-17a5-41ea-9ce5-6bd5ed8b8ab3', interpretive_gap_filling_constitutes_legislation, conventional).
narrative_ontology:cs_reference_frame('8f8f094e-17a5-41ea-9ce5-6bd5ed8b8ab3', uruguay_round_bounded_mandate).
narrative_ontology:cs_drift_state('8f8f094e-17a5-41ea-9ce5-6bd5ed8b8ab3', post_2019_appellate_paralysis, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('8f8f094e-17a5-41ea-9ce5-6bd5ed8b8ab3', '').
narrative_ontology:cs_kernel_id(wto_dsb_authority__judicial_activism_reading, wto_dsb_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(wto_dsb_authority__judicial_activism_reading, wto_adjudicative_institution).
narrative_ontology:constraint_beneficiary(wto_dsb_authority__judicial_activism_reading, export_oriented_trading_interests).
narrative_ontology:constraint_beneficiary(wto_dsb_authority__judicial_activism_reading, small_open_trading_economies).
narrative_ontology:constraint_victim(wto_dsb_authority__judicial_activism_reading, member_state_governments).
narrative_ontology:constraint_victim(wto_dsb_authority__judicial_activism_reading, domestic_regulatory_bodies).
narrative_ontology:constraint_victim(wto_dsb_authority__judicial_activism_reading, sovereigntist_member_states).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(wto_dsb_authority__judicial_activism_reading, wto_secretariat_legal_division).
narrative_ontology:constraint_victim(wto_dsb_authority__judicial_activism_reading, small_open_trading_economies).
narrative_ontology:constraint_vindicates(wto_dsb_authority__judicial_activism_reading, dsu_article_3_2_non_expansion_clause).
narrative_ontology:constraint_vindicates(wto_dsb_authority__judicial_activism_reading, vienna_convention_ordinary_meaning_discipline).
narrative_ontology:constraint_vindicates(wto_dsb_authority__judicial_activism_reading, sovereign_consent_theory_of_treaty_obligation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Panels and the Appellate Body hear disputes compulsorily and issue rulings that members treat as compliance obligations backed by authorized retaliation. Each ruling's interpretive moves become precedent the institution itself applies in later cases. Its authority, docket, and doctrinal reach compound with every decision; it cannot be restructured without consensus of all members, including those it rules against. It describes its interpretive moves as clarification of the agreed text.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__judicial_activism_reading, wto_adjudicative_institution, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(wto_dsb_authority__judicial_activism_reading, wto_adjudicative_institution, beneficiary).

% Gave up the right to block adverse rulings when they accepted the DSU; they must defend domestic measures in compulsory proceedings and bring their laws into compliance when rulings go against them. Exit means withdrawing from the WTO treaty entirely — forfeiting tariff bindings and market access everywhere — which no major trading economy has chosen. They contest specific doctrines on the DSB record while continuing to litigate within the system.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__judicial_activism_reading, member_state_governments, payer,
    powerful, generational, trapped, global).

% National agencies and standard-setters whose measures — food safety rules, environmental regulations, trade-remedy calculations — are adjudicated in Geneva. They hold no standing of their own; they appear only through their executive's litigation strategy, which may trade their mandate for concessions elsewhere. When a ruling goes against their measure, they rewrite it under compliance supervision regardless of their own technical judgment.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__judicial_activism_reading, domestic_regulatory_bodies, payer,
    powerless, biographical, trapped, national).

% Members — most prominently the system's largest historical litigant — that read the accumulated jurisprudence as obligation-creation beyond the negotiated text. They object on the DSB record, block the appointment of adjudicators, refuse to join replacement arbitration arrangements, and revive unilateral trade tools while remaining inside the treaty. Their resistance aims at re-bounding the mandate, not at leaving the system.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__judicial_activism_reading, sovereigntist_member_states, payer,
    powerful, generational, constrained, global).

% Export industries and their associations that bring market-access claims and collect the gains when foreign regulatory measures are struck down or narrowed. They can relocate supply chains and restructure entities across jurisdictions to optimize where they litigate and where the resulting market access accrues.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__judicial_activism_reading, export_oriented_trading_interests, beneficiary,
    organized, biographical, mobile, global).

% Mid-sized economies that gain a rules-based shield — they can win rulings against larger powers they could never deter unilaterally. The same jurisprudence ratchets obligations onto them that they lacked the legal capacity to contest or shape, and compliance with expanded doctrines costs them disproportionately relative to their negotiating weight.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__judicial_activism_reading, small_open_trading_economies, beneficiary,
    moderate, biographical, trapped, global).
narrative_ontology:stakeholder_secondary_role(wto_dsb_authority__judicial_activism_reading, small_open_trading_economies, payer).

% Supplies legal support to panels, drafts reasoning, and maintains the doctrinal archive. Its professional standing and staffing case grow with the system's caseload and doctrinal depth; it holds an institutional stake in the continuity and expansion of adjudication.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__judicial_activism_reading, wto_secretariat_legal_division, beneficiary,
    institutional, generational, constrained, global).

% Health, environmental, labor, and development organizations whose regulatory protections are the subject matter of disputes. Amicus participation exists at adjudicators' discretion and carries no decision weight they can rely on; they watch their policy preferences being re-balanced against trade obligations in a forum they cannot address or appeal to.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__judicial_activism_reading, domestic_civil_society_groups, excluded,
    powerless, biographical, trapped, national).

% Academic and practitioner analysts who map the jurisprudence against the negotiating record and the Vienna Convention rules. They document where interpretation ends and obligation-creation begins; their assessments feed member-state objections and DSU reform proposals without themselves holding any enforcement position.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__judicial_activism_reading, trade_law_scholarship, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(wto_dsb_authority__judicial_activism_reading, wto_adjudicative_institution).
narrative_ontology:fixing_cost_class(wto_dsb_authority__judicial_activism_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Converts trade frictions from power-based retaliation into adjudicated procedure: exporters get enforceable market-access expectations, import-regulating governments get a rules-based channel, and disputes that would otherwise escalate through unilateral measures are processed through panel procedure instead.
% TRANSFER_FUNCTION: Moves regulatory decision authority over domestic measures (health, environment, trade remedies) from national institutions to Geneva adjudicators and to export interests seeking market access; compliance costs flow from member treasuries and domestic regulators to sanctioned exporters when retaliation is authorized.
% ABSENT_VOICES: Domestic legislatures and regulatory agencies whose measures are adjudicated have no seat — they are represented only by their executive's litigation position, which may not share their mandate. Civil-society and non-commercial constituencies (health, environment, development) enter only through discretionary amicus channels. The publics whose elected policy choices are overridden are absent from the conversation entirely.
% DISAPPEARANCE_RATIONALE: If the binding dispute-settlement operation vanished overnight, disputes would revert to unilateral retaliation and power-based settlement; export expectations would reprice; mid-sized members would lose the shield that lets them win against larger powers; and the MPIA and bilateral arbitration arrangements would partially reconstitute the function among subscribers — a rearrangement of the trading order, not a return to any prior equilibrium.
% FOUNDING_PROBLEM: The GATT dispute-settlement system was blockable: losing parties vetoed panel reports, and major trading powers imposed unilateral retaliation outside any rules — trade conflicts escalated by power rather than resolved by rule.
% FOUNDING_PROBLEM_CORROBORATION: The Uruguay Round negotiating record and the DSU's own legislative history attest the founding problem (blockable panels, Section 301-era unilateralism); academic trade-law scholarship from outside the benefiting parties (Hudec, Jackson and successors) documents it; member-state DSU review submissions across blocs attest that blocking and unilateralism remain live risks. No corroborator outside the beneficiary set attests that the solution as operated remains within the negotiated mandate — that is precisely what this reading contests.
narrative_ontology:disappearance_verdict(wto_dsb_authority__judicial_activism_reading, world_rearranges).
narrative_ontology:founding_problem_status(wto_dsb_authority__judicial_activism_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(wto_dsb_authority__judicial_activism_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(wto_dsb_authority__judicial_activism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(wto_dsb_authority__judicial_activism_reading, 0.7, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(wto_dsb_authority__judicial_activism_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(wto_dsb_authority__judicial_activism_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(wto_dsb_authority__judicial_activism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (0.70 at interval end) because each expansive interpretive move converts what a member understood as its policy space into a compliance obligation, and the stock of such conversions compounds through precedent — the transfer is cumulative, not per-case. Suppression (0.60) reflects the DSU's exclusive-remedy structure: unilateral retaliation is barred, exit requires withdrawing from the whole treaty, and compliance is enforced through authorized retaliation and reputational machinery. The suppression_requirement series is authored because this story specifically traces enforcement-capacity change: a ratchet building from 1995 through 2020 (compliance surveillance, retaliation authorizations, institutional hardening), then decay after the 2019 Appellate Body paralysis as members construct workarounds — the 2025 decline is enforcement decay, not acceptance. Theater (0.45) captures a growing share of institutional activity that performs neutrality — deference recitals, 'objective assessment' language, compliance-review ritual — while the doctrinal stock keeps expanding. Accessibility collapse is moderate (0.50): the GATT-era alternative of blocking adverse reports is foreclosed, but members retain partial alternatives — absorbing retaliation rather than complying, blocking appointments, negotiating parallel arrangements — and are actively using them. Resistance is high (0.70): appointment blockades, explicit overreach objections on the DSB record, refusal to join the replacement arbitration arrangement, revived unilateral tools. All three tracked series share one time grid (1995, 2000, 2005, 2010, 2015, 2020, 2025) so every metric is authored at every examined point; base_properties values are the interval-end states. The claim (tangled_rope) and the metrics are independent authored facts: the reading asserts a real coordination function with an asymmetric extractive core, and the metrics describe that structure as this reading assesses it.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat and the payer seats compute differently from the same structure. From the adjudicative institution's position, each interpretive move is the ordinary work of applying agreed text to novel facts — the arrangement is a functioning legal system and its authority is legitimacy. From the trapped member-government seat and the seatless domestic-regulator seat, the same moves are obligation-creation without consent — the arrangement operates as extraction. The excluded civil-society seat experiences a third face: non-commercial values being re-balanced by a forum it cannot address. The engine computes these per-seat classifications from the structural data; the divergence between the institutional seat's experience and the payer seats' experience is the perspectival content of this reading, not a defect in it.
 *
 * DIRECTIONALITY LOGIC:
 *   The adjudicative institution sits at the beneficiary end: it collects authority, docket, and precedent-control from every expansive ruling, and no ruling ever runs against its own position. Export-oriented trading interests also sit near the beneficiary end — market-access gains flow to them when measures fall, and their mobility lets them litigate and book gains where they are largest. Member-state governments, domestic regulatory bodies, and sovereigntist member states sit at the target end: they bear the transferred obligations; exit for the first is treaty-withdrawal (prohibitive), for the second structurally unavailable (no standing), and the third has chosen resistance over exit while remaining inside the lock. Small open trading economies are the story's one directionality override (moderate, d=0.42): the structural derivation would read their beneficiary declaration into low d, but in this reading their net position is near-symmetric — the rules-based shield is a genuine benefit, while the same doctrine ratchets obligations onto them that they lack the legal capacity to contest or shape, so they neither purely collect the gains nor purely pay the costs. Civil-society groups are excluded rather than positioned: they bear diffuse costs of overridden policy preferences without any flow they could collect or redirect.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — blockable panels and power-based retaliation — is live, so this is not a mandatrophy case: the constraint is not a function-less residue kept alive by inertia, and it is not drifting toward piton through function-loss; it is a functioning structure under active legitimacy contest. The tangled_rope classification does the work both mislabelings would do. Reading the arrangement as pure rope would erase the extraction: obligations created without consent, enforced by retaliation authorization against members who cannot exit. Reading it as pure snare would erase the coordination: disputes genuinely are resolved by rule rather than power, and mid-sized members genuinely collect protection they could never obtain unilaterally. The classification holds both facts — a real coordination function and an asymmetric extractive core riding the same enforcement machinery — with the mandate boundary itself as the contested line. If the mandate-boundary omega resolved to 'drift is consented interpretation,' the extractive core shrinks and the structure migrates toward rope; if the enforcement-decay omega resolved to 'the machinery is broken and only performance remains,' the structure migrates toward piton. Neither resolution is available on current evidence, and the classification is authored as what the structure presently is.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sibling_reading_structural_delta,
    'This constraint is the judicial_activism_reading of kernel wto_dsb_authority. What would change structurally if the binding_referee_reading were adopted instead, and where exactly do the readings disagree?',
    'DSU review negotiation outcomes and member-state position papers: if the membership codifies adjudicators'' interpretive authority as part of the original consent, the binding_referee structure displaces this one; if it codifies the Article 3.2 non-expansion discipline instead, this reading''s structure is affirmed.',
    'Under binding_referee, epsilon drops sharply (obligations are consented), the victim set dissolves into parties to a consented adjudication, and the classification migrates toward rope; under advisory_coordination, binding enforcement disappears and the measured extraction nearly vanishes. The disagreement is located in a single structural element: whether interpretive output that creates obligations remains inside the consent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_structural_delta, conceptual, 'Committer structure: which reading of the DSB-authority kernel this constraint instantiates, what sibling readings would change, and where the disagreement sits.').

omega_variable(
    mandate_boundary_location,
    'Where exactly does legitimate interpretation under the Vienna Convention rules (which DSU Article 3.2 directs adjudicators to apply) end and illegitimate obligation-creation begin?',
    'Doctrinal analysis of the contested Appellate Body moves against the Uruguay Round negotiating record and member submissions — the ''public body'' reading, zeroing, standard-of-review deference, gap-filling in covered agreements — classifying each as ordinary-meaning application or rule-creation beyond the text.',
    'If most contested doctrine resolves as within-mandate interpretation, the extractive core shrinks and the structure migrates toward rope; if the contested doctrines are systematic rule-creation, the snare reading gains force. The current classification depends on the drift being real and substantial rather than marginal.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandate_boundary_location, empirical, 'The location of the interpretation/legislation boundary, which determines how much of the measured transfer of regulatory authority is drift rather than consented cost.').

omega_variable(
    post_paralysis_enforcement_trajectory,
    'With Appellate Body appointments blocked since 2019 and appeals routed ''into the void,'' is the arrangement''s enforcement machinery decaying, or being reconstituted in smaller form (MPIA, bilateral arbitration, reputational compliance)?',
    'Track MPIA membership growth, compliance rates for rulings delivered under alternative arrangements, and whether blocked-appeal cases produce de facto compliance, over a 5-10 year window.',
    'If enforcement decays, effective extraction falls with it and the structure drifts toward piton (residual authority without machinery); if the MPIA reconstitutes binding settlement among subscribers, the current structure persists in reduced scope and the extractiveness series resumes rising.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(post_paralysis_enforcement_trajectory, empirical, 'Whether the enforcement ratchet is decaying or being reconstituted, which governs the trajectory after 2020.').

omega_variable(
    grievance_vs_principle_ambiguity,
    'Is this reading''s illegitimacy claim a principled mandate-boundary position, or disproportionately a losing-litigant position voiced by powerful members when expansive doctrine runs against them — and does the answer change the classification?',
    'Compare members'' positions across cases won versus lost under the same doctrines: a member that accepts an expansive doctrine when it wins and rejects it when it loses is voicing grievance, not principle. Cross-reference DSB meeting objections with litigation outcomes.',
    'If the reading is grievance-driven, its high epsilon reflects the observer''s litigation position more than a stable property of the arrangement, and the sibling binding_referee reading''s lower epsilon is the better-attested characterization; if held consistently across wins and losses, the high epsilon stands as this reading''s stable assessment of the arrangement.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(grievance_vs_principle_ambiguity, conceptual, 'Whether the reading''s assessment of the arrangement reflects the arrangement itself or the observer''s position within it.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(wto_dsb_authority__judicial_activism_reading, 1995, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(wto_dsb_activism_tr_t1995, wto_dsb_authority__judicial_activism_reading, theater_ratio, 1995, 0.18).
narrative_ontology:measurement(wto_dsb_activism_tr_t2000, wto_dsb_authority__judicial_activism_reading, theater_ratio, 2000, 0.24).
narrative_ontology:measurement(wto_dsb_activism_tr_t2005, wto_dsb_authority__judicial_activism_reading, theater_ratio, 2005, 0.28).
narrative_ontology:measurement(wto_dsb_activism_tr_t2010, wto_dsb_authority__judicial_activism_reading, theater_ratio, 2010, 0.33).
narrative_ontology:measurement(wto_dsb_activism_tr_t2015, wto_dsb_authority__judicial_activism_reading, theater_ratio, 2015, 0.37).
narrative_ontology:measurement(wto_dsb_activism_tr_t2020, wto_dsb_authority__judicial_activism_reading, theater_ratio, 2020, 0.44).
narrative_ontology:measurement(wto_dsb_activism_tr_t2025, wto_dsb_authority__judicial_activism_reading, theater_ratio, 2025, 0.45).

% Extraction over time
narrative_ontology:measurement(wto_dsb_activism_be_t1995, wto_dsb_authority__judicial_activism_reading, base_extractiveness, 1995, 0.3).
narrative_ontology:measurement(wto_dsb_activism_be_t2000, wto_dsb_authority__judicial_activism_reading, base_extractiveness, 2000, 0.44).
narrative_ontology:measurement(wto_dsb_activism_be_t2005, wto_dsb_authority__judicial_activism_reading, base_extractiveness, 2005, 0.54).
narrative_ontology:measurement(wto_dsb_activism_be_t2010, wto_dsb_authority__judicial_activism_reading, base_extractiveness, 2010, 0.62).
narrative_ontology:measurement(wto_dsb_activism_be_t2015, wto_dsb_authority__judicial_activism_reading, base_extractiveness, 2015, 0.68).
narrative_ontology:measurement(wto_dsb_activism_be_t2020, wto_dsb_authority__judicial_activism_reading, base_extractiveness, 2020, 0.72).
narrative_ontology:measurement(wto_dsb_activism_be_t2025, wto_dsb_authority__judicial_activism_reading, base_extractiveness, 2025, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(wto_dsb_activism_su_t1995, wto_dsb_authority__judicial_activism_reading, suppression_requirement, 1995, 0.35).
narrative_ontology:measurement(wto_dsb_activism_su_t2000, wto_dsb_authority__judicial_activism_reading, suppression_requirement, 2000, 0.45).
narrative_ontology:measurement(wto_dsb_activism_su_t2005, wto_dsb_authority__judicial_activism_reading, suppression_requirement, 2005, 0.52).
narrative_ontology:measurement(wto_dsb_activism_su_t2010, wto_dsb_authority__judicial_activism_reading, suppression_requirement, 2010, 0.58).
narrative_ontology:measurement(wto_dsb_activism_su_t2015, wto_dsb_authority__judicial_activism_reading, suppression_requirement, 2015, 0.63).
narrative_ontology:measurement(wto_dsb_activism_su_t2020, wto_dsb_authority__judicial_activism_reading, suppression_requirement, 2020, 0.66).
narrative_ontology:measurement(wto_dsb_activism_su_t2025, wto_dsb_authority__judicial_activism_reading, suppression_requirement, 2025, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(wto_dsb_authority__judicial_activism_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(wto_dsb_authority__judicial_activism_reading, binding_referee_reading).
narrative_ontology:affects_constraint(wto_dsb_authority__judicial_activism_reading, advisory_coordination_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'WTO DSB authority' covers three structurally distinct claims about what members consented to. This file instantiates the judicial_activism_reading (high epsilon: obligations created beyond consent, assessed over the standing arrangement). The binding_referee_reading sibling authors low epsilon for the same referent (obligations consented, interpretive authority included in the grant); the advisory_coordination_reading sibling authors near-zero extraction (no binding effect at all). Same referent, reading-indexed epsilon, three files linked via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(wto_dsb_authority__judicial_activism_reading, moderate, 0.42).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
