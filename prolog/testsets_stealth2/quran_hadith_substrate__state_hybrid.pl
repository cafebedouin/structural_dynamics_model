% ============================================================================
% CONSTRAINT STORY: quran_hadith_substrate__state_hybrid
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
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
 *   human_readable: State-Hybrid Selective Sharia Application
 *   domain: legal/religious-political
 *
 * SUMMARY:
 *   Across much of the Muslim-majority world, states legislate selectively
 *   from the Quran-and-sunna substrate: family and criminal codes draw on
 *   classical fiqh provisions, while commercial, investment, and
 *   administrative law follows secular or reformist drafting. Legitimacy for
 *   the arrangement rests on political sovereignty — the state's claim to
 *   decide which rulings bind — rather than on doctrinal fidelity to any
 *   school. This file authors ONE reading of the quran_hadith_substrate
 *   kernel: the state_hybrid reading, as a clean, epsilon-invariant
 *   constraint. The sibling readings (traditionalist_taqlid,
 *   reformist_ijtihad) are separate stories; the contest among them is routed
 *   to omega variables, not folded into this classification. The epsilon
 *   referent is the standing hybrid arrangement itself, assessed by this
 *   reading's own lights — never the arrangement a sibling reading would
 *   install. Claim and metrics are authored independently: the arrangement is
 *   CLAIMED as tangled_rope (a real coordination function joined to
 *   asymmetric extraction under active enforcement), and the metrics describe
 *   moderately extractive, actively enforced operation consistent with that
 *   structure. KEY AGENTS (by structural relationship): -
 *   state_ruling_elites: Agenda-setting beneficiary (institutional/arbitrage)
 *   — enacts the selective code and collects the legitimacy yield -
 *   official_state_ulama: Captured beneficiary (organized/identity_locked) —
 *   staffs the licensed interpretive offices -
 *   independent_traditionalist_scholars: Payer (organized/constrained) —
 *   comprehensive fiqh claim truncated by statute - reformist_legal_scholars:
 *   Payer (moderate/constrained) — methods borrowed, conclusions suppressed -
 *   family_law_litigants: Payer (powerless/trapped) — bear the frozen
 *   classical provisions - commercial_elites_foreign_investors: Beneficiary
 *   (powerful/arbitrage) — receive the secular commercial layer -
 *   exiled_dissident_clerics: Excluded voice (moderate/mobile) — contests
 *   from outside the jurisdiction - comparative_law_monitoring_bodies:
 *   Analytical observer (institutional/analytical) — documents the divergence
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quran_hadith_substrate__state_hybrid, 0.36).
domain_priors:suppression_score(quran_hadith_substrate__state_hybrid, 0.62).
domain_priors:theater_ratio(quran_hadith_substrate__state_hybrid, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quran_hadith_substrate__state_hybrid, extractiveness, 0.36).
narrative_ontology:constraint_metric(quran_hadith_substrate__state_hybrid, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(quran_hadith_substrate__state_hybrid, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quran_hadith_substrate__state_hybrid, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(quran_hadith_substrate__state_hybrid, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quran_hadith_substrate__state_hybrid, tangled_rope).
narrative_ontology:human_readable(quran_hadith_substrate__state_hybrid, "State-Hybrid Selective Sharia Application").
narrative_ontology:topic_domain(quran_hadith_substrate__state_hybrid, "legal/religious-political").

domain_priors:requires_active_enforcement(quran_hadith_substrate__state_hybrid).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quran_hadith_substrate__state_hybrid, 'bbc1f128-a354-4363-908a-d4ac9e4998e9').
narrative_ontology:cs_kernel_codification('bbc1f128-a354-4363-908a-d4ac9e4998e9', fixed_text).
narrative_ontology:cs_authority_grounding('bbc1f128-a354-4363-908a-d4ac9e4998e9', extraction).
narrative_ontology:cs_interpretation_layer_present('bbc1f128-a354-4363-908a-d4ac9e4998e9').
narrative_ontology:cs_reading_relation('bbc1f128-a354-4363-908a-d4ac9e4998e9', quran_hadith_substrate__traditionalist_taqlid, forecloses).
narrative_ontology:cs_reading_relation('bbc1f128-a354-4363-908a-d4ac9e4998e9', quran_hadith_substrate__reformist_ijtihad, influences).
narrative_ontology:cs_axiom('bbc1f128-a354-4363-908a-d4ac9e4998e9', foundational, legal_validity_flows_from_sovereign_enactment).
narrative_ontology:cs_axiom_status(legal_validity_flows_from_sovereign_enactment, holdable).
narrative_ontology:cs_axiom_grounding('bbc1f128-a354-4363-908a-d4ac9e4998e9', legal_validity_flows_from_sovereign_enactment, conventional).
narrative_ontology:cs_axiom('bbc1f128-a354-4363-908a-d4ac9e4998e9', foundational, sharia_scope_is_domain_partitionable).
narrative_ontology:cs_axiom_status(sharia_scope_is_domain_partitionable, holdable).
narrative_ontology:cs_axiom_grounding('bbc1f128-a354-4363-908a-d4ac9e4998e9', sharia_scope_is_domain_partitionable, instrumental).
narrative_ontology:cs_reference_frame('bbc1f128-a354-4363-908a-d4ac9e4998e9', sovereign_selective_codification).
narrative_ontology:cs_drift_state('bbc1f128-a354-4363-908a-d4ac9e4998e9', contemporary_transnational_media_environment, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('bbc1f128-a354-4363-908a-d4ac9e4998e9', '').
narrative_ontology:cs_kernel_id(quran_hadith_substrate__state_hybrid, quran_hadith_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quran_hadith_substrate__state_hybrid, state_ruling_elites).
narrative_ontology:constraint_beneficiary(quran_hadith_substrate__state_hybrid, official_state_ulama).
narrative_ontology:constraint_beneficiary(quran_hadith_substrate__state_hybrid, commercial_elites_foreign_investors).
narrative_ontology:constraint_victim(quran_hadith_substrate__state_hybrid, independent_traditionalist_scholars).
narrative_ontology:constraint_victim(quran_hadith_substrate__state_hybrid, reformist_legal_scholars).
narrative_ontology:constraint_victim(quran_hadith_substrate__state_hybrid, family_law_litigants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(quran_hadith_substrate__state_hybrid, official_state_ulama).
narrative_ontology:constraint_vindicates(quran_hadith_substrate__state_hybrid, state_sovereignty_over_legal_sources).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enact penal and personal-status codes drawing on classical fiqh provisions while legislating commercial, investment, and administrative law through secular drafting committees. Appoint and dismiss grand muftis, fatwa council members, and supreme court judges. Trade doctrinal fidelity for governing room: the religious legitimacy produced by the sharia-labeled codes accrues to them, and so does the policy freedom preserved in the secular domains. They define the rules they themselves live under.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__state_hybrid, state_ruling_elites, agenda_setter,
    institutional, generational, arbitrage, national).

% Staff state fatwa councils, teach in state-run Islamic faculties, and hold salaried positions with formal rank. In exchange they confine their opinions to questions the state refers and refrain from ruling on the legality of state policy. Careers, reputations, and the standing to speak at all are built inside the state apparatus; leaving means losing the only platform from which their scholarship counts as authoritative.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__state_hybrid, official_state_ulama, beneficiary,
    organized, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(quran_hadith_substrate__state_hybrid, official_state_ulama, payer).

% Maintain madhhab study circles, private fatwa networks, and informal arbitration panels. Their position that classical rulings govern all domains of life is confined by statute to whatever the state has left uncodified. Teaching or publishing outside licensed channels risks closure, fines, or prosecution; some operate from neighboring jurisdictions and commute their influence across borders.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__state_hybrid, independent_traditionalist_scholars, payer,
    organized, generational, constrained, regional).

% Argue for contextual reinterpretation that prioritizes the Quran's ethical trajectory and public interest over literalist application. State drafters adopt their methods piecemeal in commercial codes without acknowledgment, while their calls to extend the same method to family and criminal law are treated as threats to public order. Universities decline their appointments; journals return their submissions; security services monitor their seminars.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__state_hybrid, reformist_legal_scholars, payer,
    moderate, biographical, constrained, national).

% Encounter the classical-rulings layer chiefly in marriage, divorce, custody, inheritance, and hudud-adjacent offenses. There is no civil-code opt-out; outcomes turn on whichever classical provision the codified law froze decades ago. Women litigants bear the largest share of the frozen provisions' costs, and court fees, delays, and appellate distance determine who can contest an unfavorable ruling at all.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__state_hybrid, family_law_litigants, payer,
    powerless, biographical, trapped, national).

% Operate under modernized commercial codes, investment laws, and international arbitration regimes modeled on transnational practice. They obtain contractual predictability that unmodified classical transaction rules would not provide, and they can relocate capital if the secular layer deteriorates — a credible threat that disciplines the state toward keeping the carve-out intact.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__state_hybrid, commercial_elites_foreign_investors, beneficiary,
    powerful, immediate, arbitrage, global).

% Broadcast critiques from satellite channels and platforms based abroad. They would contest the state's monopoly on religious interpretation from both directions — some demanding comprehensive application, others demanding free ijtihad — if admitted to domestic councils. Licensing rules, travel bans, and denaturalization threats keep them outside the conversation.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__state_hybrid, exiled_dissident_clerics, excluded,
    moderate, generational, mobile, continental).

% Document the divergence between constitutional clauses declaring sharia a principal source of legislation and the operative content of commercial statutes. Produce country reports, shadow reviews, and treaty-body assessments. They hold no enforcement power but shape loan conditionality and diplomatic framing, and they see the whole structure from outside any single seat.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__state_hybrid, comparative_law_monitoring_bodies, observer,
    institutional, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(quran_hadith_substrate__state_hybrid, state_ruling_elites).
narrative_ontology:fixing_cost_class(quran_hadith_substrate__state_hybrid, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single state legal order that lets one government serve populations demanding religious law in status and morality domains while running an economy that requires internationally compatible commercial rules. Codification happens once, centrally, instead of leaving plural, uncoordinated religious and mercantile jurisdictions to collide.
% TRANSFER_FUNCTION: Moves interpretive authority over the Quran-and-sunna corpus from independent scholarly lineages to state organs; moves the legitimacy yield to ruling elites; moves contractual predictability to commercial actors; and moves the frozen costs of selected classical provisions onto family-law and criminal-law subjects.
% ABSENT_VOICES: Exiled dissident clerics, unlicensed traditionalist teachers, and women's-rights advocates excluded from codification committees would object from opposite directions. They sit outside the legislative councils — in exile, in informal networks, or appearing only before international treaty bodies.
% DISAPPEARANCE_RATIONALE: Overnight removal would force an immediate and unresolved choice between full secular codification and comprehensive madhhab application. Ruling coalitions would lose their legitimacy formula, commercial actors their predictability guarantee, and both scholarly flanks would rush the vacated interpretive space; courts, schools, and licensing bodies would all reorganize.
% FOUNDING_PROBLEM: Newly centralized Muslim-majority states had to legislate uniformly for religiously constituted societies: inherited fiqh was plural, procedural, and judge-made, poorly suited to codified administration and global commerce, yet abandoning it outright threatened religious legitimacy against both secular-nationalist rivals and Islamist challengers.
% FOUNDING_PROBLEM_CORROBORATION: Comparative-law scholarship outside the benefiting parties documents the same founding tension across dozens of states. Opposition movements on both flanks attest it from below — traditionalists by campaigning to restore comprehensive application, reformists by cataloguing the selectivity — though each proposes an opposite resolution. No attesting source inside the beneficiary set is relied upon.
narrative_ontology:disappearance_verdict(quran_hadith_substrate__state_hybrid, world_rearranges).
narrative_ontology:founding_problem_status(quran_hadith_substrate__state_hybrid, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quran_hadith_substrate__state_hybrid, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(quran_hadith_substrate__state_hybrid, 'none', 1).
narrative_ontology:epsilon_provenance(quran_hadith_substrate__state_hybrid, 0.36, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness is authored at 0.36, inside the manifest's low-to-moderate band: real transfers occur (truncated rights in family law, suppressed voices on both flanks, authority moved from scholarly lineages to the state), but the arrangement also delivers genuine services — unified courts, codified certainty, subsidized adjudication at scale. Suppression is authored at 0.62 as a RAW structural property, deliberately unscaled: the machinery that holds the hybrid together (clergy licensing, censorship, statutory bars on uncodified fiqh in court) is real coercive overhead, and per the framework only extractiveness is scaled by directionality and scope — the commentary and the metric both treat suppression as context-free. Theater ratio 0.48: the sharia-labeled layer performs legitimacy while the operative lawmaking attention sits in commercial codes; nearly half of religious-law activity is ceremonial maintenance. Accessibility collapse 0.50: alternatives persist informally (private fatwa networks, offshore publication, cross-border arbitration) but are foreclosed inside state channels. Resistance 0.55: both flanks actively resist, from restoration campaigns to reformist documentation projects. The temporal series share one grid (t=0..50); the suppression series is intentionally cyclical — crackdown phases after Islamist electoral surges or security shocks, relaxation during economic-opening windows — and the oscillation itself functions as intermittent reinforcement: periodic loosening splits opposition coalitions before they consolidate, then re-tightening resets the field. Extractiveness and theater rise monotonically beneath the cycle, tracking accumulation of legitimacy rents and the migration of substantive lawmaking into the secular layer.
 *
 * PERSPECTIVAL GAP:
 *   From the ruler's seat the arrangement is a governing formula working as designed; from the traditionalist scholar's seat it is enforced truncation of a comprehensive obligation; from the reformist scholar's seat it is appropriation of method without license to conclude; from the commercial actor's seat it is invisible, benign infrastructure. One structure, four incompatible experiences — the engine computes these divergent per-seat classifications from the structural data rather than averaging them. Identity-lock note: the official ulama's lock is institutional — the organization has become their only authorized voice, so exit would dissolve the authority their scholarship depends on; if the licensing frame broke, their position would shift from captured beneficiary toward independent target. Coalition note: family-law litigants are individually powerless, but advocacy organizations periodically aggregate them into organized resistance, briefly raising their effective power atom before the cycle re-tightens.
 *
 * DIRECTIONALITY LOGIC:
 *   Ruling elites sit nearest the beneficiary pole: they administer the selection, collect the legitimacy yield, and hold arbitrage-grade exit because they define the rules they live under. Commercial elites also sit beneficiary-side; their arbitrage exit and the carve-out written for them dampen their effective burden toward subsidy. Official ulama derive as beneficiaries through patronage, but their captured position — independent interpretive authority surrendered as the price of office — places them above a pure beneficiary; no directionality override is authored because the beneficiary-plus-secondary-payer declaration and the identity_locked exit already encode the capture for the derivation chain. Family-law litigants sit nearest the target pole: trapped exit, no opt-out, frozen provisions applied to them without consent. Independent traditionalists and reformists are targets with constrained exit; containing them is the enforcement object itself. Exiled clerics stand largely outside the domestic derivation surface — their exclusion is maintained precisely to keep them off it.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — legislating uniformly for religiously constituted populations while integrating into a global economy — remains live, so mandatrophy_resolved is not declared. The tangled_rope claim keeps both faces visible: reading the arrangement as pure coordination erases the suppressed flanks and the frozen costs borne by family-law litigants; reading it as pure extraction erases the real services delivered (unified courts, codified certainty, dispute resolution at scale). Mandatrophy resolution here guards against two mislabels at once. Should the founding tension dissolve — through generational secularization or comprehensive Islamization — the residual licensing machinery would persist administratively while ceasing to argue doctrine; the theater-trajectory omega tracks exactly that drift, and the R5 mismatch consumer would flag dead-problem-plus-rearranging-world if the founding problem lapsed while the apparatus held.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest_location,
    'This story instantiates the state_hybrid reading of the quran_hadith_substrate kernel; where exactly do the three readings disagree, and what would each sibling change structurally?',
    'Doctrinal analysis locating the disputed element: the criterion of binding legal authority — sovereign enactment (this reading) versus madhhab-consensus obligation (traditionalist_taqlid) versus mandated contextual ijtihad (reformist_ijtihad).',
    'Under traditionalist_taqlid the state loses selection discretion and the payer set shifts to anyone deviating from madhhab rulings; under reformist_ijtihad authority relocates to qualified interpreters and the state''s licensing machinery loses its object. Epsilon and the beneficiary/victim structure change accordingly in each sibling file.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest_location, conceptual, 'Location of the inter-reading disagreement within the quran_hadith_substrate kernel.').

omega_variable(
    cross_regime_epsilon_variance,
    'How widely does the arrangement''s effective extraction vary across regime types — rentier monarchies, competitive-authoritarian republics, post-conflict states?',
    'Cross-national dataset coding regime incentive structures against measured suppression of both flanks and the breadth of the secular-commercial carve-out.',
    'At the high end (legitimacy purchased cheaply, wide suppression) the arrangement approaches purely extractive operation; at the low end (broad participation in codification) it approaches coordination-with-overhead. The authored 0.25-0.45 band spans both, and per-seat classifications will diverge sharply by regime.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cross_regime_epsilon_variance, empirical, 'Regime-type variance in the hybrid''s effective extraction.').

omega_variable(
    citizen_net_position_ambiguity,
    'Are ordinary subjects net beneficiaries of the hybrid (codified certainty, subsidized courts, communal recognition) or net payers (frozen classical provisions in family and criminal law)?',
    'Docket-level outcome analysis comparing results under codified classical provisions versus realistically available alternatives, weighted by which goods affected populations themselves prioritize in survey data.',
    'A net-beneficiary finding pulls the arrangement toward coordination-with-overhead and lowers effective extraction for the mass seat; a net-payer finding adds a mass victim class and pushes effective extraction above the authored band.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(citizen_net_position_ambiguity, preference, 'Whether the mass seat is net-subsidized or net-burdened by the hybrid.').

omega_variable(
    sharia_component_theater_trajectory,
    'Is the classical-rulings layer becoming predominantly performative — legitimacy display decoupled from doctrinal substance — as commercial lawmaking absorbs the state''s real legislative attention?',
    'Longitudinal coding of family-court docket volumes, doctrinal citation depth in judgments, and legislative session time allocated to personal-status versus commercial statutes.',
    'A rising performative share predicts drift toward inertial persistence: the religious layer surviving as ceremony administered by officials who no longer argue doctrine, pushing the arrangement''s religious component toward degraded-inertial operation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sharia_component_theater_trajectory, empirical, 'Whether the sharia layer is drifting from functional to theatrical maintenance.').

omega_variable(
    dual_flank_suppression_sustainability,
    'Can simultaneous containment of the traditionalist and reformist flanks hold indefinitely, or does it generate convergent opposition that eventually breaks the licensing machinery?',
    'Event-history analysis of episodes where the two flanks coordinated despite doctrinal hostility, and of licensing-system breakdowns under succession crises or fiscal shock.',
    'Sustained dual containment supports the tangled-rope reading with stable enforcement requirements; breakdown would force rapid reclassification as the suppression requirement collapses (attrition) or spikes (succession crackdown).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(dual_flank_suppression_sustainability, empirical, 'Durability of the dual-flank containment strategy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quran_hadith_substrate__state_hybrid, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qura_tr_t0, quran_hadith_substrate__state_hybrid, theater_ratio, 0, 0.3).
narrative_ontology:measurement(qura_tr_t10, quran_hadith_substrate__state_hybrid, theater_ratio, 10, 0.34).
narrative_ontology:measurement(qura_tr_t20, quran_hadith_substrate__state_hybrid, theater_ratio, 20, 0.38).
narrative_ontology:measurement(qura_tr_t30, quran_hadith_substrate__state_hybrid, theater_ratio, 30, 0.42).
narrative_ontology:measurement(qura_tr_t40, quran_hadith_substrate__state_hybrid, theater_ratio, 40, 0.45).
narrative_ontology:measurement(qura_tr_t50, quran_hadith_substrate__state_hybrid, theater_ratio, 50, 0.48).

% Extraction over time
narrative_ontology:measurement(qura_be_t0, quran_hadith_substrate__state_hybrid, base_extractiveness, 0, 0.24).
narrative_ontology:measurement(qura_be_t10, quran_hadith_substrate__state_hybrid, base_extractiveness, 10, 0.27).
narrative_ontology:measurement(qura_be_t20, quran_hadith_substrate__state_hybrid, base_extractiveness, 20, 0.3).
narrative_ontology:measurement(qura_be_t30, quran_hadith_substrate__state_hybrid, base_extractiveness, 30, 0.32).
narrative_ontology:measurement(qura_be_t40, quran_hadith_substrate__state_hybrid, base_extractiveness, 40, 0.34).
narrative_ontology:measurement(qura_be_t50, quran_hadith_substrate__state_hybrid, base_extractiveness, 50, 0.36).

% Suppression requirement over time
narrative_ontology:measurement(qura_su_t0, quran_hadith_substrate__state_hybrid, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(qura_su_t10, quran_hadith_substrate__state_hybrid, suppression_requirement, 10, 0.57).
narrative_ontology:measurement(qura_su_t20, quran_hadith_substrate__state_hybrid, suppression_requirement, 20, 0.64).
narrative_ontology:measurement(qura_su_t30, quran_hadith_substrate__state_hybrid, suppression_requirement, 30, 0.52).
narrative_ontology:measurement(qura_su_t40, quran_hadith_substrate__state_hybrid, suppression_requirement, 40, 0.6).
narrative_ontology:measurement(qura_su_t50, quran_hadith_substrate__state_hybrid, suppression_requirement, 50, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quran_hadith_substrate__state_hybrid, enforcement_mechanism).
narrative_ontology:affects_constraint(quran_hadith_substrate__state_hybrid, traditionalist_taqlid).
narrative_ontology:affects_constraint(quran_hadith_substrate__state_hybrid, reformist_ijtihad).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the quran_hadith_substrate kernel. The colloquial label 'Islamic law' covers three structurally distinct arrangements of the same textual substrate, each with its own epsilon, beneficiary/victim structure, and failure modes: traditionalist_taqlid (madhhab consensus as binding criterion), reformist_ijtihad (mandated contextual reinterpretation), and this file's state_hybrid (sovereign selection). The state_hybrid reading is downstream of both: it displaces the traditionalist reading's authority claim while borrowing the reformist reading's methods selectively. Each member links the others via network.affects_constraints; contamination propagates along these edges (e.g., a state crackdown on reformists degrades the reformist reading's operating environment, and a traditionalist restoration movement attacks the hybrid's selection discretion directly).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
