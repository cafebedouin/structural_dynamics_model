% ============================================================================
% CONSTRAINT STORY: ai_risk_governance_priority__bridge_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_risk_governance_priority__bridge_reading, []).

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
 *   constraint_id: ai_risk_governance_priority__bridge_reading
 *   human_readable: Unified Entangled-Framework Mandate in AI Risk Governance (Bridge Reading)
 *   domain: technology governance/research policy/ethics
 *
 * SUMMARY:
 *   This story instantiates the bridge reading of the
 *   ai_risk_governance_priority kernel: the claim that AI risk governance
 *   must treat present harms and existential risks as non-mutually-exclusive,
 *   structurally entangled concerns requiring unified frameworks. Per the
 *   epsilon-invariance discipline, this file authors ONE constraint — the
 *   unified-framework mandate as it actually operates — with one stable
 *   epsilon over one referent: the standing integrated-governance apparatus
 *   (broker-mediated integration layered over two still-segregated research
 *   communities), assessed by this reading's own lights. The sibling readings
 *   (existential_risk_reading, near_term_harms_reading) are other files with
 *   their own victim sets and epsilon values; they are not described, hedged,
 *   or averaged here. KEY AGENTS (by structural relationship):
 *   cross_domain_bridging_institutions — agenda-setter and principal
 *   collector (institutional/arbitrage), runs the integrated agenda and
 *   receives its rewards; ai_governance_funders — beneficiary
 *   (institutional/arbitrage), gains legibility and hedging;
 *   specialist_xrisk_researchers and specialist_near_term_harms_researchers —
 *   payers with secondary benefits (organized/constrained), bear the
 *   translation burden; present_marginalized_populations — payer
 *   (powerless/trapped, immediate horizon); future_humanity — payer
 *   (powerless/trapped, civilizational horizon, universal scope,
 *   proxy-represented only); grassroots_community_advocates — excluded voice;
 *   intergovernmental_risk_assessment_bodies — analytical observer.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_risk_governance_priority__bridge_reading, 0.5).
domain_priors:suppression_score(ai_risk_governance_priority__bridge_reading, 0.4).
domain_priors:theater_ratio(ai_risk_governance_priority__bridge_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_risk_governance_priority__bridge_reading, extractiveness, 0.5).
narrative_ontology:constraint_metric(ai_risk_governance_priority__bridge_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(ai_risk_governance_priority__bridge_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_risk_governance_priority__bridge_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(ai_risk_governance_priority__bridge_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_risk_governance_priority__bridge_reading, tangled_rope).
narrative_ontology:human_readable(ai_risk_governance_priority__bridge_reading, "Unified Entangled-Framework Mandate in AI Risk Governance (Bridge Reading)").
narrative_ontology:topic_domain(ai_risk_governance_priority__bridge_reading, "technology governance/research policy/ethics").

domain_priors:requires_active_enforcement(ai_risk_governance_priority__bridge_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_risk_governance_priority__bridge_reading, 'e21a4db7-28e1-4268-a5ad-46df314e2289').
narrative_ontology:cs_kernel_codification('e21a4db7-28e1-4268-a5ad-46df314e2289', distributed).
narrative_ontology:cs_authority_grounding('e21a4db7-28e1-4268-a5ad-46df314e2289', expertise).
narrative_ontology:cs_interpretation_layer_present('e21a4db7-28e1-4268-a5ad-46df314e2289').
narrative_ontology:cs_reading_relation('e21a4db7-28e1-4268-a5ad-46df314e2289', ai_risk_governance_priority__existential_risk_reading, influences).
narrative_ontology:cs_reading_relation('e21a4db7-28e1-4268-a5ad-46df314e2289', ai_risk_governance_priority__near_term_harms_reading, influences).
narrative_ontology:cs_axiom('e21a4db7-28e1-4268-a5ad-46df314e2289', foundational, risk_classes_structurally_entangled).
narrative_ontology:cs_axiom_status(risk_classes_structurally_entangled, holdable).
narrative_ontology:cs_axiom_grounding('e21a4db7-28e1-4268-a5ad-46df314e2289', risk_classes_structurally_entangled, empirically_contingent).
narrative_ontology:cs_axiom('e21a4db7-28e1-4268-a5ad-46df314e2289', foundational, unified_frameworks_outperform_segregated_mandates).
narrative_ontology:cs_axiom_status(unified_frameworks_outperform_segregated_mandates, holdable).
narrative_ontology:cs_axiom_grounding('e21a4db7-28e1-4268-a5ad-46df314e2289', unified_frameworks_outperform_segregated_mandates, instrumental).
narrative_ontology:cs_reference_frame('e21a4db7-28e1-4268-a5ad-46df314e2289', unified_entangled_priority_framework).
narrative_ontology:cs_drift_state('e21a4db7-28e1-4268-a5ad-46df314e2289', contemporary, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('e21a4db7-28e1-4268-a5ad-46df314e2289', '').
narrative_ontology:cs_kernel_id(ai_risk_governance_priority__bridge_reading, ai_risk_governance_priority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_risk_governance_priority__bridge_reading, cross_domain_bridging_institutions).
narrative_ontology:constraint_beneficiary(ai_risk_governance_priority__bridge_reading, ai_governance_funders).
narrative_ontology:constraint_victim(ai_risk_governance_priority__bridge_reading, specialist_xrisk_researchers).
narrative_ontology:constraint_victim(ai_risk_governance_priority__bridge_reading, specialist_near_term_harms_researchers).
narrative_ontology:constraint_victim(ai_risk_governance_priority__bridge_reading, present_marginalized_populations).
narrative_ontology:constraint_victim(ai_risk_governance_priority__bridge_reading, future_humanity).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(ai_risk_governance_priority__bridge_reading, specialist_xrisk_researchers).
narrative_ontology:constraint_beneficiary(ai_risk_governance_priority__bridge_reading, specialist_near_term_harms_researchers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% A small set of interdisciplinary centers, bridging journals and workshop series, and senior scholars whose output supplies the large majority of cross-links between the catastrophic-risk and fairness literatures. They convene integrated agenda-setting panels, draft unified risk taxonomies, staff funder advisory boards, and define what counts as a well-formed AI risk question. Funding, citation centrality, and definitional authority concentrate in this seat; their translation competence is scarce and portable across whatever framing next attracts money.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__bridge_reading, cross_domain_bridging_institutions, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(ai_risk_governance_priority__bridge_reading, cross_domain_bridging_institutions, beneficiary).

% Public agencies and philanthropic foundations allocating portfolios across AI risk programming. The unified framing hands them a single legible theory of change covering both harm classes, hedges reputational exposure on either flank, and simplifies reporting lines. They can rebalance or drop the framing at portfolio-review speed and bear little of its operating cost.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__bridge_reading, ai_governance_funders, beneficiary,
    institutional, generational, arbitrage, global).

% Alignment and catastrophic-risk researchers. Under the unified mandate they must demonstrate near-term relevance to compete for integrated grants and publish in bridged venues, stretching thin technical agendas across unfamiliar literatures. When integration works they gain access to larger pooled budgets and broader audiences; when it stalls they have ceded agenda-setting leverage to intermediaries while their home venues shrink.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__bridge_reading, specialist_xrisk_researchers, payer,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(ai_risk_governance_priority__bridge_reading, specialist_xrisk_researchers, beneficiary).

% Fairness, accountability, labor-displacement, and surveillance scholars working on demonstrated present-day harms. The unified framing asks them to connect documented injuries to speculative tail scenarios before integrated funding will move, straining their evidentiary norms. They gain cross-community reach and relief from being framed as narrow, while definitional control over what counts as a harm migrates into integrated taxonomies they did not write.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__bridge_reading, specialist_near_term_harms_researchers, payer,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(ai_risk_governance_priority__bridge_reading, specialist_near_term_harms_researchers, beneficiary).

% Communities bearing deployed-system harms now: biased allocation decisions, automated workplace management, pervasive screening. The unified promise is that their harms will not be traded away against distant scenarios; in operation, remedy timelines lengthen as resources route through integrated programs in which they hold no seats, and they cannot exit the systems that injure them.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__bridge_reading, present_marginalized_populations, payer,
    powerless, immediate, trapped, global).

% Persons whose existence and prospects depend on how catastrophic capability-concentration risks are governed. They act only through proxy advocates. Inside the unified frame their protection competes against auditable present-day outcome metrics, and integration that tilts toward measurable near-term deliverables leaves tail-risk work under-resourced. There is no exit and no direct voice; every representation of their interest is mediated.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__bridge_reading, future_humanity, payer,
    powerless, civilizational, trapped, universal).

% Organizers, legal-aid clinics, and worker centers accompanying deployment-affected communities. They would insist that consent, redress, and enforceable accountability be non-negotiable components of any combined framework, but the integrated agenda-setting happens in funder convenings and academic workshops they rarely enter, and consultation deadlines assume resources they do not have.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__bridge_reading, grassroots_community_advocates, excluded,
    powerless, immediate, trapped, regional).

% International scientific-assessment and standards efforts compiling evidence across both risk classes. They solicit input from every camp, test whether integrated taxonomies survive contact with national regulatory contexts, and can endorse or decline the combined framing in their reports, which materially shifts its legitimacy.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__bridge_reading, intergovernmental_risk_assessment_bodies, observer,
    institutional, generational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ai_risk_governance_priority__bridge_reading, cross_domain_bridging_institutions).
narrative_ontology:fixing_cost_class(ai_risk_governance_priority__bridge_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves a real collective-action problem between two governance communities that had duplicated infrastructure, maintained incompatible risk taxonomies, and talked past each other in policy windows that forced a single story. A combined framework gives funders and assessment bodies one coherent map on which present-harm evidence and catastrophic-risk scenarios can be stated in shared terms, and makes visible pathways (surveillance infrastructure built for engagement optimization, allocation systems prefiguring capability concentration) that neither silo tracks alone.
% TRANSFER_FUNCTION: Moves grant funding, citation flow, and agenda-setting authority toward integrated safety-and-ethics programs and the broker institutions that run them; moves engagement and translation costs onto specialists in each camp; and nominally routes protective benefit to both presently harmed populations and future persons, with the actual routing decided inside broker-controlled venues.
% ABSENT_VOICES: Grassroots community advocates and deployment-affected workers are structurally outside the integrated agenda-setting rooms, though they hold the most direct knowledge of present harms. Future persons hold no seat at all except through proxy claims advanced by the same broker institutions that collect the integration's rewards. Both absences flatter the unanimity with which the unified framing presents itself.
% DISAPPEARANCE_RATIONALE: If the unified-framework mandate vanished overnight, the two research communities would re-segregate along their existing venue and funding boundaries, funders would split portfolios into separate ethics and safety streams, the bridging institutions would lose their niche and their revenue, and policy processes would revert to forcing a choice between harm classes at each decision point. The arrangements currently organized around integration would visibly rearrange.
% FOUNDING_PROBLEM: After roughly 2016, AI governance split into two silos: a fairness-and-accountability community tracking demonstrated discrimination, due-process, labor, and surveillance harms, and a safety community tracking alignment failure and catastrophic capability concentration. Cross-citation between them was sparse, funders demanded a single coherent rationale, and policy windows repeatedly forced advocates to choose between 'near-term ethics' and 'long-term safety' framing. The combined-framework mandate was built to end that forced choice.
% FOUNDING_PROBLEM_CORROBORATION: Bibliometric studies documenting sparse cross-field linkage between the two literatures come from science-of-science researchers outside both camps; joint statements and workshop consensus documents issued by figures from both communities attest the siloing; and funders' own strategy reviews and legislative-hearing testimony independently describe fragmentation as an unresolved governance problem. None of these corroborating sources sits inside the benefiting broker set.
narrative_ontology:disappearance_verdict(ai_risk_governance_priority__bridge_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_risk_governance_priority__bridge_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_risk_governance_priority__bridge_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(ai_risk_governance_priority__bridge_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_risk_governance_priority__bridge_reading, 0.5, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_risk_governance_priority__bridge_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_risk_governance_priority__bridge_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ai_risk_governance_priority__bridge_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Claimed type is tangled_rope, authored independently of the metrics: the unified mandate possesses a genuine coordination function (shared taxonomy, pooled infrastructure, visible cross-class pathways) AND asymmetric collection (rewards concentrate in a broker seat while costs spread across specialists and both constituencies), sustained by active enforcement (funder mandates, venue scope statements, integrated review criteria). Metrics are authored as descriptively true: epsilon 0.50 is moderate on both dimensions — present-harm remedy dilutes through integrated routing while tail-risk protection dilutes toward auditable near-term deliverables; suppression 0.40 is soft-power gatekeeping rather than hard coercion, since specialists retain separate venues and funders can exit at portfolio speed; theater_ratio 0.42 reflects a substantial performative layer (bridging workshops, white papers, and citation exchanges that produce little operational integration) atop real function; accessibility_collapse 0.35 is low because alternatives persist (segregated venues, dedicated streams, national unilateral approaches); resistance 0.60 is high because both camps actively contest the mandate. The temporal series run on one shared grid (2016, 2017, 2018, 2020, 2022, 2024) with every tracked metric authored at every point. The suppression_requirement series is included deliberately: the story traces enforcement-capacity intensification, as the unified framing hardened from voluntary aspiration (circa 2016-2018) into gatekeeping embedded in grant calls, venue scope statements, and policy consultation designs. Trajectories are monotonic; no cyclical dynamic is modeled, and none is claimed.
 *
 * PERSPECTIVAL GAP:
 *   Seats should compute divergent types from identical structural data. From the broker seat, the arrangement is a coordination achievement it built and staffs: integration is real, scarce, and under-supplied, and its rewards are compensation for scarce translational competence. From the specialist payer seats, the same structure operates as an engagement levy with diminishing agenda control — a coordination frame whose terms were written elsewhere. From the two constituency seats, the frame is a representation deficit: each is invoked as the reason for unification while holding no seat in the venues where unification's terms are set. The engine derives these per-seat classifications from power, exit, and directional position; this commentary explains why they diverge without adjudicating among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Cross_domain_bridging_institutions derive a directionality near the beneficiary pole: they administer the mandate and collect its rewards, with arbitrage-grade exit (translation competence ports to any successor framing). Ai_governance_funders likewise sit near the beneficiary pole — they receive legibility and hedging while bearing little operating cost. The two specialist groups are payers whose dual position (secondary benefit from pooled budgets and cross-audience reach) moderates but does not reverse their target-side directionality; constrained exit keeps them well short of symmetry. Present_marginalized_populations and future_humanity sit nearest the full-target pole: powerless, trapped, and — in the latter case — locked to proxy representation at universal scope, which the engine's scope handling treats as verification-hardened. Suppression is authored as a raw structural property and is not scaled; only extractiveness is scaled by directionality and scope in the engine's computation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (siloed governance communities forcing a choice between harm classes) remains live, corroborated from outside the benefiting parties; the disappearance verdict is world_rearranges, so the R5 mismatch consumer finds no dead-mandate-plus-persistence flag. The tangled_rope claim is what prevents mislabeling in both directions: reading the mandate as pure rope would launder broker concentration and the representation deficit as mere coordination cost; reading it as pure snare would license demolishing a genuinely scarce integration function that both camps partially depend on. The piton reading is also blocked by the data: the function is not atrophied (cross-field output is growing, not vestigial), and a concentrated collector demonstrably profits, which disqualifies the no-meaningful-beneficiary piton signature. What the classification preserves is the reform target: reduce the broker seat's monopoly on integration without destroying integration — the omega variables name the measurements (broker-set turnover, funding dispersion, representation warrants) that would distinguish those outcomes.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This constraint is one reading (bridge_reading) of the kernel ai_risk_governance_priority; the sibling readings existential_risk_reading and near_term_harms_reading instantiate different constraints with different victim sets, enforcement structures, and epsilon profiles. Where exactly is the disagreement located?',
    'The disagreement is located in whether the two risk classes are separable priorities or structurally entangled: the existential reading treats catastrophic risk as lexically prior, the near-term reading treats demonstrated harm as the only admissible evidentiary ground, and this reading asserts non-mutual-exclusivity with mandatory unification. Each reading is authored as its own constraint file; no averaging across readings occurs here.',
    'A sibling reading would change the victim set (future-humanity-only or present-populations-only), the beneficiary set (single-camp institutions rather than brokers), and epsilon substantially; classifications computed from this file are valid only for the bridge reading''s constraint.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer structure: this story instantiates one reading of a three-reading kernel; sibling readings are separate constraints.').

omega_variable(
    broker_capture_or_division_of_labor,
    'Is the observed concentration of cross-field linkage in a small broker set (roughly 5% of papers supplying 85% of cross-links) an emergent and benign division of translational labor, or consolidating gatekeeping over the integration agenda?',
    'Longitudinal bibliometric analysis of broker-set turnover: if new entrants join the high-linkage set over time and linkage leadership rotates, the concentration is functional specialization; if the set is closed and self-reproducing through co-authorship and panel recruitment, it is consolidation.',
    'If consolidation, the coordination function is increasingly a license for the broker seat and the constraint drifts snare-ward; if rotation, the measured costs are closer to ordinary coordination overhead and the constraint drifts rope-ward.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(broker_capture_or_division_of_labor, empirical, 'Whether broker concentration reflects healthy specialization or closing gatekeeping.').

omega_variable(
    entanglement_evidential_warrant,
    'How strong is the empirical warrant for the structural-entanglement premise itself — that present-harm infrastructures and catastrophic capability-concentration risks are causally coupled rather than merely rhetorically paired?',
    'Systematic case audit of documented pathways where near-term harm infrastructure scaled toward systemic risk (recommendation systems and persuasion at scale, biometric surveillance buildout, compute concentration driven by consumer products), contrasted with cases where the coupling claim was asserted without mechanism.',
    'A strong warrant supports the unified mandate as genuine coordination and locates the measured costs in broker mediation; a weak warrant would mean the entanglement claim functions mainly as a funding umbrella, and the constraint''s coordination half is largely cover.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(entanglement_evidential_warrant, empirical, 'Empirical robustness of the entanglement thesis underlying this reading''s foundational axiom.').

omega_variable(
    proxy_representation_warrant,
    'By what warrant do broker institutions claim to represent present marginalized populations and future humanity inside integrated bodies, given that neither constituency holds seats there?',
    'Trace representation chains: documented consultation with affected communities, accountability mechanisms binding integrated programs to the constituencies they invoke, and independent assessment of whether invoked interests match constituency-stated priorities.',
    'If the proxy claims lack warrant, a substantial share of the constraint''s apparent coordination function is delegated performance — raising the effective theater ratio and weakening the rope-half of any hybrid classification; if warranted, the representation costs are real coordination overhead.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proxy_representation_warrant, conceptual, 'Legitimacy of broker claims to speak for the two victim constituencies.').

omega_variable(
    fragile_dependency_trajectory,
    'Will the constraint''s structural fragility — dependence on a handful of broker actors rather than distributed collaboration — resolve into broadened participation or harden into dependency?',
    'Track funding dispersion across integrated programs, new-institution entry rates into bridging venues, and whether integrated training pipelines reproduce the broker profile or widen it, over the next funding cycles.',
    'Broadening pushes the constraint toward distributed coordination with falling mediation costs; hardening locks extraction into the broker seat and converts the unified mandate into a chokepoint, shifting the computed type at both payer and constituency seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fragile_dependency_trajectory, empirical, 'Direction of drift for the broker-dependency structure over coming funding cycles.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_risk_governance_priority__bridge_reading, 2016, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_r_tr_t2016, ai_risk_governance_priority__bridge_reading, theater_ratio, 2016, 0.2).
narrative_ontology:measurement(ai_r_tr_t2017, ai_risk_governance_priority__bridge_reading, theater_ratio, 2017, 0.24).
narrative_ontology:measurement(ai_r_tr_t2018, ai_risk_governance_priority__bridge_reading, theater_ratio, 2018, 0.28).
narrative_ontology:measurement(ai_r_tr_t2020, ai_risk_governance_priority__bridge_reading, theater_ratio, 2020, 0.34).
narrative_ontology:measurement(ai_r_tr_t2022, ai_risk_governance_priority__bridge_reading, theater_ratio, 2022, 0.39).
narrative_ontology:measurement(ai_r_tr_t2024, ai_risk_governance_priority__bridge_reading, theater_ratio, 2024, 0.42).

% Extraction over time
narrative_ontology:measurement(ai_r_be_t2016, ai_risk_governance_priority__bridge_reading, base_extractiveness, 2016, 0.3).
narrative_ontology:measurement(ai_r_be_t2017, ai_risk_governance_priority__bridge_reading, base_extractiveness, 2017, 0.34).
narrative_ontology:measurement(ai_r_be_t2018, ai_risk_governance_priority__bridge_reading, base_extractiveness, 2018, 0.38).
narrative_ontology:measurement(ai_r_be_t2020, ai_risk_governance_priority__bridge_reading, base_extractiveness, 2020, 0.44).
narrative_ontology:measurement(ai_r_be_t2022, ai_risk_governance_priority__bridge_reading, base_extractiveness, 2022, 0.48).
narrative_ontology:measurement(ai_r_be_t2024, ai_risk_governance_priority__bridge_reading, base_extractiveness, 2024, 0.5).

% Suppression requirement over time
narrative_ontology:measurement(ai_r_su_t2016, ai_risk_governance_priority__bridge_reading, suppression_requirement, 2016, 0.22).
narrative_ontology:measurement(ai_r_su_t2017, ai_risk_governance_priority__bridge_reading, suppression_requirement, 2017, 0.26).
narrative_ontology:measurement(ai_r_su_t2018, ai_risk_governance_priority__bridge_reading, suppression_requirement, 2018, 0.31).
narrative_ontology:measurement(ai_r_su_t2020, ai_risk_governance_priority__bridge_reading, suppression_requirement, 2020, 0.36).
narrative_ontology:measurement(ai_r_su_t2022, ai_risk_governance_priority__bridge_reading, suppression_requirement, 2022, 0.38).
narrative_ontology:measurement(ai_r_su_t2024, ai_risk_governance_priority__bridge_reading, suppression_requirement, 2024, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_risk_governance_priority__bridge_reading, information_standard).
narrative_ontology:affects_constraint(ai_risk_governance_priority__bridge_reading, existential_risk_reading).
narrative_ontology:affects_constraint(ai_risk_governance_priority__bridge_reading, near_term_harms_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial debate over 'AI risk governance priorities' decomposes, per the epsilon-invariance principle, into three structurally distinct constraints sharing one kernel. This file is the bridge reading; existential_risk_reading and near_term_harms_reading are the pole readings. Epsilon differs across the family because the victim sets, enforcement structures, and beneficiary concentrations differ structurally — not because one constraint is measured different ways. The bridge reading sits between the poles and mediates resource flow to both: its institutionalization changes the operating environment of each sibling (funders demand near-term relevance from catastrophic-risk work and tail-risk acknowledgment from harm work) without logically eliminating either, which is why the reading_relations are influences rather than forecloses. Family members link mutually through affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
