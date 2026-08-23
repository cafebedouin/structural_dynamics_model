% ============================================================================
% CONSTRAINT STORY: paris_article_4_ndc__sovereigntist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_paris_article_4_ndc__sovereigntist_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: paris_article_4_ndc__sovereigntist_reading
 *   human_readable: NDC Voluntary Pledge System (Sovereigntist Reading)
 *   domain: political/international-climate-governance
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the Paris Article 4 kernel: the
 *   sovereigntist reading, under which nationally determined contributions
 *   are voluntary self-determined pledges that preserve national energy
 *   sovereignty and require no punitive enforcement. Per Rule 1 the file
 *   authors only this reading — one epsilon, one beneficiary structure, one
 *   type — with no hedging across sibling readings; the contest is routed to
 *   omega variables and cs_structure. Assumptions stated openly: the interval
 *   maps to calendar years 2015-2025 (adoption through the second pledge
 *   cycle); endpoint measurement values equal the base_properties scalars;
 *   sampling parameters are assumed defaults for this generation run. The
 *   claim and metrics are independent authored facts: the claimed type states
 *   what is structurally true of the arrangement as this reading holds it,
 *   and the metrics describe its observed operation — including the rising
 *   pledge-theater share — without being tuned toward any predicted engine
 *   verdict.
 *
 * KEY AGENTS:
 *   - - unfccc_secretariat: Administrator ([institutional]/[identity_locked]) — runs the COP cycle, transparency reviews, and NDC registry; organizationally fused to the regime's continuation
 *   - - fossil_producing_states: Primary beneficiary ([organized]/[arbitrage]) — preserves export and production pathways at near-zero membership cost; blocks hardening by consensus veto
 *   - - major_emerging_economies: Beneficiary ([powerful]/[mobile]) — secures standing and flexibility while preserving growth space through intensity-based pledges
 *   - - industrialized_ambitious_states: Dual beneficiary/payer ([institutional]/[mobile]) — volunteers stringent targets for universal coverage; absorbs self-chosen transition costs and missed finance obligations
 *   - - climate_vulnerable_states: Dual beneficiary/exposure-bearer ([powerless]/[trapped]) — gained platform and finance channels; territories bear the aggregate shortfall the discretionary design permits
 *   - - future_generations: Absent voice — inherit the accumulated atmospheric stock; no seat in the process
 *   - - ipcc_assessment_body: Analytical observer — measures aggregate pledge adequacy against temperature goals from outside decision authority
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(paris_article_4_ndc__sovereigntist_reading, 0.23).
domain_priors:suppression_score(paris_article_4_ndc__sovereigntist_reading, 0.18).
domain_priors:theater_ratio(paris_article_4_ndc__sovereigntist_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(paris_article_4_ndc__sovereigntist_reading, extractiveness, 0.23).
narrative_ontology:constraint_metric(paris_article_4_ndc__sovereigntist_reading, suppression_requirement, 0.18).
narrative_ontology:constraint_metric(paris_article_4_ndc__sovereigntist_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(paris_article_4_ndc__sovereigntist_reading, accessibility_collapse, 0.25).
narrative_ontology:constraint_metric(paris_article_4_ndc__sovereigntist_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(paris_article_4_ndc__sovereigntist_reading, rope).
narrative_ontology:human_readable(paris_article_4_ndc__sovereigntist_reading, "NDC Voluntary Pledge System (Sovereigntist Reading)").
narrative_ontology:topic_domain(paris_article_4_ndc__sovereigntist_reading, "political/international-climate-governance").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(paris_article_4_ndc__sovereigntist_reading, 'd97e0b38-8b47-479a-b034-5ac3acd62089').
narrative_ontology:cs_kernel_codification('d97e0b38-8b47-479a-b034-5ac3acd62089', fixed_text).
narrative_ontology:cs_authority_grounding('d97e0b38-8b47-479a-b034-5ac3acd62089', distributed).
narrative_ontology:cs_reading_relation('d97e0b38-8b47-479a-b034-5ac3acd62089', paris_article_4_ndc__supranational_reading, coexists_with).
narrative_ontology:cs_reading_relation('d97e0b38-8b47-479a-b034-5ac3acd62089', paris_article_4_ndc__equity_reading, coexists_with).
narrative_ontology:cs_axiom('d97e0b38-8b47-479a-b034-5ac3acd62089', foundational, nationally_determined_ambition_is_sovereign_right).
narrative_ontology:cs_axiom_status(nationally_determined_ambition_is_sovereign_right, holdable).
narrative_ontology:cs_axiom_grounding('d97e0b38-8b47-479a-b034-5ac3acd62089', nationally_determined_ambition_is_sovereign_right, conventional).
narrative_ontology:cs_axiom('d97e0b38-8b47-479a-b034-5ac3acd62089', secondary, facilitative_design_sustains_universal_participation).
narrative_ontology:cs_axiom_status(facilitative_design_sustains_universal_participation, holdable).
narrative_ontology:cs_axiom_grounding('d97e0b38-8b47-479a-b034-5ac3acd62089', facilitative_design_sustains_universal_participation, instrumental).
narrative_ontology:cs_reference_frame('d97e0b38-8b47-479a-b034-5ac3acd62089', nationally_determined_sovereign_voluntarism).
narrative_ontology:cs_drift_state('d97e0b38-8b47-479a-b034-5ac3acd62089', post_first_global_stocktake, gap(repudiation_pressure, minor, true)).
narrative_ontology:cs_created_at('d97e0b38-8b47-479a-b034-5ac3acd62089', '').
narrative_ontology:cs_kernel_id(paris_article_4_ndc__sovereigntist_reading, paris_article_4_ndc).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(paris_article_4_ndc__sovereigntist_reading, fossil_producing_states).
narrative_ontology:constraint_beneficiary(paris_article_4_ndc__sovereigntist_reading, major_emerging_economies).
narrative_ontology:constraint_beneficiary(paris_article_4_ndc__sovereigntist_reading, industrialized_ambitious_states).
narrative_ontology:constraint_beneficiary(paris_article_4_ndc__sovereigntist_reading, climate_vulnerable_states).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(paris_article_4_ndc__sovereigntist_reading, industrialized_ambitious_states).
narrative_ontology:constraint_victim(paris_article_4_ndc__sovereigntist_reading, climate_vulnerable_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Convenes the annual conference cycle, operates the NDC registry, commissions technical expert reviews of transparency reports, and synthesizes submissions for global stocktakes. It administers the process parties designed but sets no rules itself; rule-setting sits with the parties under consensus, which gives every party a de facto veto over hardening. Its staffing, budget justification, and institutional purpose are wholly constituted by running this process, so its organizational survival is inseparable from the regime's continuation regardless of how the regime performs.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__sovereigntist_reading, unfccc_secretariat, agenda_setter,
    institutional, generational, identity_locked, global).

% Export-dependent hydrocarbon economies that pledge modestly or conditionally while expanding production, and that exercise blocking leverage inside consensus decisions on any text that would discipline supply. Membership costs them little and shields their development plans behind self-determined targets; leaving would forfeit a seat at the table they can otherwise dominate from inside, so they stay and shape. Their export markets and financing alternatives span multiple continents, making exit cheap and outside options attractive.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__sovereigntist_reading, fossil_producing_states, beneficiary,
    organized, generational, arbitrage, global).

% Large rapidly industrializing emitters that file intensity-based or peaking-date targets calibrated to economic growth rather than absolute caps. Participation buys them standing and access to technology and finance discussions while preserving unrestricted domestic development space; the five-year revision right means no pledge outlives their willingness to honor it. Their size gives them alternatives to the regime that smaller parties lack.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__sovereigntist_reading, major_emerging_economies, beneficiary,
    powerful, generational, mobile, continental).

% Developed economies that volunteer comparatively stringent targets and absorb the associated domestic transition costs and competitiveness exposure, in exchange for the regime's principal achievement: every significant emitter inside one reporting and review frame, which earlier obligation-based designs never secured. They also carry the finance-transfer expectations that delivery has repeatedly missed, and they fund a disproportionate share of institutional operations. Their exit is available and has been exercised historically by at least one major party, but departure surrenders the leadership position they cultivate.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__sovereigntist_reading, industrialized_ambitious_states, beneficiary,
    institutional, generational, mobile, continental).
narrative_ontology:stakeholder_secondary_role(paris_article_4_ndc__sovereigntist_reading, industrialized_ambitious_states, payer).

% Small island and least-developed parties that gained the platform they fought for: universal recognition of their exposure, loss-and-damage channels, adaptation finance architecture, and a diplomatic stage where coalition skill partially offsets material weakness. At the same time, the design they accepted leaves aggregate ambition to each party's discretion, and the resulting shortfall lands physically on their territories, which cannot relocate out of the atmosphere or out of the ocean's reach. Leaving the process would cost them their principal lever, so they stay and press from inside.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__sovereigntist_reading, climate_vulnerable_states, beneficiary,
    powerless, generational, trapped, regional).
narrative_ontology:stakeholder_secondary_role(paris_article_4_ndc__sovereigntist_reading, climate_vulnerable_states, payer).

% Not yet present to speak in any negotiating room; they inherit the atmospheric stock that current pledge levels accumulate. No mechanism in the process weighs their interests except as invoked rhetorically by existing parties, and the sovereignty frame assigns responsibility for them entirely to each state's domestic politics.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__sovereigntist_reading, future_generations, excluded,
    powerless, civilizational, trapped, universal).
narrative_ontology:stakeholder_non_agent(paris_article_4_ndc__sovereigntist_reading, future_generations).

% Assesses the aggregate adequacy of submitted pledges against stated temperature goals and publishes the comparison on a multi-year cycle. It informs but does not vote; its findings enter the process as context that parties may adopt or set aside. Its assessment role depends on the reporting stream the regime generates.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__sovereigntist_reading, ipcc_assessment_body, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(paris_article_4_ndc__sovereigntist_reading, diffuse).
narrative_ontology:fixing_cost_class(paris_article_4_ndc__sovereigntist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a universal-participation architecture: every significant emitter files a nationally chosen target on a common five-year cycle, reports through a shared transparency framework, and submits aggregate progress to periodic global stocktakes — coordination achieved through invitation, visibility, and recurrence rather than obligation.
% TRANSFER_FUNCTION: Moves little material value: it circulates information (effort disclosure), status (pledge credibility and review attention), and diplomatic focus. Its one large promised material transfer — climate finance from developed to developing parties — remains a pledge-level commitment whose delivery has repeatedly fallen short of the stated benchmark.
% ABSENT_VOICES: Future generations and communities already displaced by climate impacts hold no seat. Within many parties, populations whose governments pledge weakly are represented only through those governments' discretion — the sovereignty frame converts domestic dissent into an internal matter outside the regime's purview. The scientific assessment community attends and speaks but does not vote.
% DISAPPEARANCE_RATIONALE: Procedural arrangements would rearrange immediately: the COP cycle, transparency reporting, finance signaling, and the diplomatic focal point would lapse, and the secretariat's mandate would collapse. Physical emissions trajectories would move far less on any short horizon, because implementation runs on domestic policy and economics more than on pledge mechanics. Seats whose operations depend on the architecture — the secretariat, vulnerable-state diplomacy, finance pipelines — predict rearrangement; seats that treat pledges as ornamental predict rough continuity.
% FOUNDING_PROBLEM: The Kyoto-era failure of binding top-down targets to achieve coverage: major emitters either declined obligations or walked away, leaving most global emissions outside any common framework.
% FOUNDING_PROBLEM_CORROBORATION: IPCC assessment reports and the independent emissions-gap literature corroborate from outside the beneficiary set that the participation problem was real and is architecturally addressed, while aggregate pledged ambition remains incompatible with the stated temperature goals. No corroborating voice exists inside the regime that disputes its founding narrative — parties unanimously attest their own origin story — so the corroboration that matters sits entirely external, in the assessment and scholarship record.
narrative_ontology:disappearance_verdict(paris_article_4_ndc__sovereigntist_reading, contested).
narrative_ontology:founding_problem_status(paris_article_4_ndc__sovereigntist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(paris_article_4_ndc__sovereigntist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(paris_article_4_ndc__sovereigntist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(paris_article_4_ndc__sovereigntist_reading, 0.23, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(paris_article_4_ndc__sovereigntist_reading_tests).
:- end_tests(paris_article_4_ndc__sovereigntist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.23) because nothing in the arrangement is coerced: pledges are chosen, revision is a right, and the withdrawal clause prices exit at roughly three years' notice. Suppression is low (0.18) for the same reason — the regime restrains through visibility, not penalty. Accessibility_collapse is low (0.25) because alternatives remain open by design: minilateral clubs, bilateral deals, proposed carbon clubs, and outright exit have all been exercised or credibly threatened, and the arrangement suppresses none of them. Resistance is low (0.15) because a structure demanding little meets little opposition. Theater (0.38) is the moving number: headline pledge announcements have progressively decoupled from implementation pathways, while the reporting and stocktake machinery performs a real information-coordination function underneath — the rising series tracks announcement inflation, not machinery decay. The suppression_requirement series is authored because the narrative genuinely tracks enforcement-capacity change: the Katowice rulebook (2018), the operational enhanced transparency framework (~2021), and the first global stocktake (2023) constitute a measured, procedural-capacity ratchet — review machinery thickened while remaining strictly facilitative, with no punitive element emerging. All three series share one six-point grid (2015-2025, biennial), so no metric is sampled against a substituted end-state value at earlier times.
 *
 * PERSPECTIVAL GAP:
 *   Seats should compute differently. From the secretariat's chair the arrangement is an indispensable operating architecture whose value compounds with each cycle; from petrostate chairs it is a low-cost shield that legitimizes continued production; from ambitious developed-state chairs it is a stage for leadership purchased at self-chosen cost plus an unpaid finance bill; from vulnerable-state chairs it is simultaneously the best available platform and the instrument that certified a shortfall landing on their territory. Same nominal standing (all sovereign parties), radically different experienced structures — differentiated here by exit quality (arbitrage versus trapped), role position, and exposure, not by power tier alone.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations place fossil_producing_states, major_emerging_economies, industrialized_ambitious_states, and climate_vulnerable_states near the subsidized end, with exit quality modulating: arbitrage-grade exit (petrostates) sits closest to full beneficiary; mobile exit (emerging and developed economies) slightly less so. The dual-role seats derive intermediate d: industrialized states' self-imposed costs and finance burdens pull them up from pure subsidy, and climate_vulnerable_states pull furthest of the four because trapped exit amplifies their exposure-bearing position despite the beneficiary primary role. The secretariat derives near-zero d (administers, does not pay or collect materially). The structural signature of this reading is the ABSENCE of any full-target seat: no party is extracted from against its will, because the design forbids compelled targets. That absence is the reading's defining claim, and it is authored as structure, not as evaluation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — universal participation where binding designs had failed — is architecturally delivered, and the participation function remains live each pledge cycle, so the mandate has not outlived its function outright; hence mandatrophy_resolved is false. But the arrangement carries a directional expectation (the ambition ratchet) without a sunset clause or a declared transition: it behaves as an implicitly transitional structure whose escalation mechanism has underperformed for a decade. Classification guards against both misreadings: reading low epsilon as proof the underlying collective-action problem is solved (it is not — the adequacy gap widens), and reading critics' coercion-framing as evidence of hidden extraction (none is compelled; the costs that exist are self-selected or exposure-derived). The scaffold-adjacent tension — directional intent without declared sunset — is documented here rather than forced into a scaffold typing, because the arrangement makes no sunset claim of its own.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ndc_kernel_reading_contest,
    'This constraint is one reading of kernel paris_article_4_ndc — specifically the sovereigntist_reading. What would each sibling reading change structurally if it governed, and where exactly is the disagreement located?',
    'Comparative classification across the three linked reading-stories: the supranational_reading would raise effective extraction on laggard parties by substituting binding trajectory accountability for voluntary revision; the equity_reading would restructure the beneficiary/victim sets around imposed developed/developing category distinctions rather than self-differentiation. The disagreement is located in three structural elements: the locus of ambition determination (national versus international), the owed enforcement intensity (facilitative versus binding), and the differentiation mechanism (self-declared versus categorical).',
    'If the supranational structure were instantiated, this file''s low-epsilon profile inverts for non-compliant parties and the classification migrates toward enforced-hybrid forms; if the equity structure were instantiated, beneficiary and victim assignments redistribute across development categories and several seats'' directionality reverses. Cross-reading comparison is only interpretable because each file holds a single stable epsilon.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(ndc_kernel_reading_contest, conceptual, 'Committer-frame routing: one reading of a contested treaty kernel; sibling readings instantiate different constraints.').

omega_variable(
    sovereignty_vs_shortfall_attribution,
    'Do climate damages arising from the aggregate ambition shortfall count as costs imposed BY this arrangement (making climate_vulnerable_states victims of it) or as external to it (the sovereigntist attribution, under which each state''s choices are its own and the arrangement merely declines to compel)?',
    'Counterfactual design analysis: compare projected damages under this discretionary design against feasible alternative architectures available at the same adoption moment (binding-top-down variants having failed historically, weighted-science-prescribed allocation variants having been rejected). Attribution turns on whether a materially lower-damage feasible design existed that this arrangement''s specific design choices displaced.',
    'If damages are attributable to the design, climate_vulnerable_states acquire a victim position, effective extraction rises for the trapped seat, and the structure drifts toward hybrid extraction forms requiring enforcement analysis; if external, the reading''s clean low-extraction profile holds. This omega is the precise boundary line between this reading and its siblings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sovereignty_vs_shortfall_attribution, conceptual, 'Whether the arrangement owns the consequences of the discretion it institutionalizes.').

omega_variable(
    enforcement_atrophy_or_hardening,
    'Will facilitative mechanisms continue to atrophy (the expected structural delta) or harden into quasi-binding review through procedural accretion?',
    'Track successive global stocktake outcomes and CMA decisions for punitive drift: consequence-bearing review elements, linkage of support to performance, or border-adjustment spillovers functioning as external enforcement.',
    'Hardening raises the suppression and extraction trajectories and pushes classification toward enforced hybrid territory; continued atrophy confirms the low-constraint profile and raises long-run inertial-drift questions. The observed decade shows procedural thickening without punitive emergence — the omega remains open.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_atrophy_or_hardening, empirical, 'Trajectory of the facilitative machinery: decay, stasis, or accretion toward bindingness.').

omega_variable(
    pledge_implementation_theater_share,
    'What fraction of announced pledge activity carries credible domestic implementation pathways, as distinct from announcement-level diplomatic credit?',
    'Implementation-gap tracking: independent assessment of policy-backing behind each submitted target (sectoral coverage, legislated instruments, budgetary allocation) against announced headline ambition.',
    'A widening implementation gap raises theater_ratio and signals proxy-goal substitution (announcement replacing delivery), dating a possible drift toward performative-maintenance dynamics; a narrowing gap supports the genuine-coordination reading of pledge activity.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(pledge_implementation_theater_share, empirical, 'Empirical decomposition of the rising theater series into announcement inflation versus machinery decay.').

omega_variable(
    climate_finance_delivery_reality,
    'Is the promised developed-to-developing climate finance transfer a real compensating flow or a rhetorical component of the founding bargain?',
    'Audit of delivered finance against the stated benchmark using agreed accounting definitions, separating mobilized private flows from public transfers and loan-heavy from grant-based composition.',
    'If delivery is substantially rhetorical, the founding bargain is asymmetric — flexibility was conceded, compensation was not paid — which strengthens the exposure-bearing position of developing-party seats and feeds the attribution omega; if delivery is real, part of the measured extraction on vulnerable seats is compensated and their net position improves.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(climate_finance_delivery_reality, empirical, 'Whether the bargain''s compensation leg is material or declaratory.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(paris_article_4_ndc__sovereigntist_reading, 2015, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pari_tr_t2015, paris_article_4_ndc__sovereigntist_reading, theater_ratio, 2015, 0.24).
narrative_ontology:measurement(pari_tr_t2017, paris_article_4_ndc__sovereigntist_reading, theater_ratio, 2017, 0.27).
narrative_ontology:measurement(pari_tr_t2019, paris_article_4_ndc__sovereigntist_reading, theater_ratio, 2019, 0.3).
narrative_ontology:measurement(pari_tr_t2021, paris_article_4_ndc__sovereigntist_reading, theater_ratio, 2021, 0.33).
narrative_ontology:measurement(pari_tr_t2023, paris_article_4_ndc__sovereigntist_reading, theater_ratio, 2023, 0.36).
narrative_ontology:measurement(pari_tr_t2025, paris_article_4_ndc__sovereigntist_reading, theater_ratio, 2025, 0.38).

% Extraction over time
narrative_ontology:measurement(pari_be_t2015, paris_article_4_ndc__sovereigntist_reading, base_extractiveness, 2015, 0.15).
narrative_ontology:measurement(pari_be_t2017, paris_article_4_ndc__sovereigntist_reading, base_extractiveness, 2017, 0.17).
narrative_ontology:measurement(pari_be_t2019, paris_article_4_ndc__sovereigntist_reading, base_extractiveness, 2019, 0.19).
narrative_ontology:measurement(pari_be_t2021, paris_article_4_ndc__sovereigntist_reading, base_extractiveness, 2021, 0.21).
narrative_ontology:measurement(pari_be_t2023, paris_article_4_ndc__sovereigntist_reading, base_extractiveness, 2023, 0.22).
narrative_ontology:measurement(pari_be_t2025, paris_article_4_ndc__sovereigntist_reading, base_extractiveness, 2025, 0.23).

% Suppression requirement over time
narrative_ontology:measurement(pari_su_t2015, paris_article_4_ndc__sovereigntist_reading, suppression_requirement, 2015, 0.08).
narrative_ontology:measurement(pari_su_t2017, paris_article_4_ndc__sovereigntist_reading, suppression_requirement, 2017, 0.11).
narrative_ontology:measurement(pari_su_t2019, paris_article_4_ndc__sovereigntist_reading, suppression_requirement, 2019, 0.14).
narrative_ontology:measurement(pari_su_t2021, paris_article_4_ndc__sovereigntist_reading, suppression_requirement, 2021, 0.16).
narrative_ontology:measurement(pari_su_t2023, paris_article_4_ndc__sovereigntist_reading, suppression_requirement, 2023, 0.17).
narrative_ontology:measurement(pari_su_t2025, paris_article_4_ndc__sovereigntist_reading, suppression_requirement, 2025, 0.18).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(paris_article_4_ndc__sovereigntist_reading, information_standard).
narrative_ontology:affects_constraint(paris_article_4_ndc__sovereigntist_reading, paris_article_4_ndc__supranational_reading).
narrative_ontology:affects_constraint(paris_article_4_ndc__sovereigntist_reading, paris_article_4_ndc__equity_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition per the epsilon-invariance principle: the colloquial label 'the Paris NDC regime' conflates three structurally distinct claims that share Article 4 text but differ in epsilon, beneficiary structure, and enforcement logic. This file is the sovereigntist instance (voluntary self-determination, facilitative machinery, low extraction). paris_article_4_ndc__supranational_reading instantiates binding-trajectory accountability (higher epsilon for laggards); paris_article_4_ndc__equity_reading instantiates categorical CBDR differentiation (restructured beneficiary/victim sets). The sovereigntist instance is the operating baseline against which the other two press: both siblings cite the same kernel text, and the contest among them is routed through each file's omega variables rather than averaged into any single classification. Edges are declared bidirectionally across the family; orphan stories within this family would be a code smell.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
