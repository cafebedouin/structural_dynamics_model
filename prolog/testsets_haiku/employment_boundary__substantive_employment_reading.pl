% ============================================================================
% CONSTRAINT STORY: employment_boundary__substantive_employment_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_employment_boundary__substantive_employment_reading, []).

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
 *   constraint_id: employment_boundary__substantive_employment_reading
 *   human_readable: Substantive Employment Definition (Economic Dependence & Algorithmic Control)
 *   domain: labor/social_policy/platform_economy
 *
 * SUMMARY:
 *   The substantive employment reading defines an employment relationship by
 *   the presence of economic dependence and algorithmic control, regardless
 *   of contractual form. This reading reclassifies platform workers as
 *   employees entitled to full employment protections (minimum wage,
 *   benefits, job security, anti-retaliation). It is ONE reading of a
 *   contested kernel (employment_boundary); the kernel contest also includes
 *   the formalist_employment_reading (contract and direct supervision define
 *   employment) and the hybrid_security_reading (a third category of
 *   dependent contractor with tailored protections). Under this reading,
 *   platforms become obligated beneficiaries (must provide employment-level
 *   protections) and platform workers move from the payer set (independent
 *   contractors bearing precarity) into the beneficiary set. The measurement
 *   series traces rising extractiveness as the reading gains traction in
 *   high-enforcement jurisdictions and platforms resist through litigation,
 *   automation, and regulatory capture. Theater rises as platforms adopt
 *   compliance theater (algorithmic transparency reports, worker-support
 *   initiatives) while maintaining substantive contractor classification in
 *   low-enforcement jurisdictions. Suppression rises as platforms scale
 *   anti-union messaging and algorithmic discipline. Resistance rises from
 *   workers and labor organizations, particularly at the class and
 *   organizational levels.
 *
 * KEY AGENTS:
 *   - platform_workers (economic dependence, algorithmic discipline, powerless, trapped exit)
 *   - platform_operators (institutional power, arbitrage exit, resisting reclassification through litigation and relocation)
 *   - labor_regulators (institutional authority, analytical seat, adjudicating reclassification)
 *   - worker_advocacy_organizations (organized power, mobile exit, mobilizing through strikes and campaigns)
 *   - business_model_investors (powerful, arbitrage exit, shifting capital to automation or low-enforcement jurisdictions)
 *   - formalist_legal_interpreters (excluded from this reading's forums, holding alternative constitutional reading)
 *   - consumer_users (diffuse cost burden, mobile exit, benefiting from low prices enabled by contractor status)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(employment_boundary__substantive_employment_reading, 0.62).
domain_priors:suppression_score(employment_boundary__substantive_employment_reading, 0.58).
domain_priors:theater_ratio(employment_boundary__substantive_employment_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(employment_boundary__substantive_employment_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(employment_boundary__substantive_employment_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(employment_boundary__substantive_employment_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(employment_boundary__substantive_employment_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(employment_boundary__substantive_employment_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(employment_boundary__substantive_employment_reading, tangled_rope).
narrative_ontology:human_readable(employment_boundary__substantive_employment_reading, "Substantive Employment Definition (Economic Dependence & Algorithmic Control)").
narrative_ontology:topic_domain(employment_boundary__substantive_employment_reading, "labor/social_policy/platform_economy").

domain_priors:requires_active_enforcement(employment_boundary__substantive_employment_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(employment_boundary__substantive_employment_reading, '7ea274a6-d5d5-4805-9481-567a5c989bb9').
narrative_ontology:cs_kernel_codification('7ea274a6-d5d5-4805-9481-567a5c989bb9', fixed_text).
narrative_ontology:cs_authority_grounding('7ea274a6-d5d5-4805-9481-567a5c989bb9', lineage).
narrative_ontology:cs_interpretation_layer_present('7ea274a6-d5d5-4805-9481-567a5c989bb9').
narrative_ontology:cs_reading_relation('7ea274a6-d5d5-4805-9481-567a5c989bb9', employment_boundary__formalist_employment_reading, forecloses).
narrative_ontology:cs_reading_relation('7ea274a6-d5d5-4805-9481-567a5c989bb9', employment_boundary__hybrid_security_reading, influences).
narrative_ontology:cs_axiom('7ea274a6-d5d5-4805-9481-567a5c989bb9', foundational, control_and_dependence_define_employment).
narrative_ontology:cs_axiom_status(control_and_dependence_define_employment, holdable).
narrative_ontology:cs_axiom_grounding('7ea274a6-d5d5-4805-9481-567a5c989bb9', control_and_dependence_define_employment, deontological).
narrative_ontology:cs_axiom('7ea274a6-d5d5-4805-9481-567a5c989bb9', foundational, employment_protection_universalism).
narrative_ontology:cs_axiom_status(employment_protection_universalism, holdable).
narrative_ontology:cs_axiom_grounding('7ea274a6-d5d5-4805-9481-567a5c989bb9', employment_protection_universalism, deontological).
narrative_ontology:cs_reference_frame('7ea274a6-d5d5-4805-9481-567a5c989bb9', labor_law_protection_mandate).
narrative_ontology:cs_drift_state('7ea274a6-d5d5-4805-9481-567a5c989bb9', platform_economy_emergence, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('7ea274a6-d5d5-4805-9481-567a5c989bb9', '').
narrative_ontology:cs_kernel_id(employment_boundary__substantive_employment_reading, employment_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(employment_boundary__substantive_employment_reading, platform_workers_as_employees).
narrative_ontology:constraint_victim(employment_boundary__substantive_employment_reading, platform_operators).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(employment_boundary__substantive_employment_reading, platform_workers).
narrative_ontology:constraint_beneficiary(employment_boundary__substantive_employment_reading, worker_advocacy_organizations).
narrative_ontology:constraint_beneficiary(employment_boundary__substantive_employment_reading, consumer_users).
narrative_ontology:constraint_victim(employment_boundary__substantive_employment_reading, business_model_investors).
narrative_ontology:constraint_vindicates(employment_boundary__substantive_employment_reading, labor_rights_universalism).
narrative_ontology:constraint_vindicates(employment_boundary__substantive_employment_reading, substantive_over_formal_employment_test).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Workers classified as independent contractors by platforms but dependent on algorithmic assignment for income, subject to algorithmic discipline (deactivation, rate cuts, task denial), with no collective bargaining capacity. Under this reading, they are reclassified as employees entitled to minimum wage, benefits, job security, and anti-retaliation protections. They benefit from the constraint's enforcement but bear the cost if platforms respond by reducing work availability or shifting to geographic markets with lower employment standards.
narrative_ontology:constraint_stakeholder(employment_boundary__substantive_employment_reading, platform_workers, beneficiary,
    powerless, biographical, trapped, global).

% Operate labor-distribution platforms (ridesharing, food delivery, task work, etc.) under the formalist employment reading and extract labor cost savings by classifying workers as independent contractors rather than employees. The substantive reading imposes full employment liabilities, forcing cost absorption, benefit provision, and job-security guarantees. They resist reclassification through litigation, lobbying, and regulatory capture, and have arbitrage options: geographical relocation, automation, or business-model restructuring away from on-demand labor.
narrative_ontology:constraint_stakeholder(employment_boundary__substantive_employment_reading, platform_operators, payer,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(employment_boundary__substantive_employment_reading, platform_operators, agenda_setter).

% Enforce labor law and adjudicate employment classification. Under the substantive reading, regulators must determine that economic dependence and algorithmic control constitute an employment relationship regardless of formal contract language. They face litigation from platforms and conflicting policy guidance from different jurisdictions. Their authority to reclassify workers is contested across borders and politically vulnerable to platform lobbying.
narrative_ontology:constraint_stakeholder(employment_boundary__substantive_employment_reading, labor_regulators, agenda_setter,
    institutional, generational, analytical, national).

% Labor unions, worker centers, and labor-rights NGOs advocate for the substantive employment reading and mobilize worker power through strikes, public campaigns, and legislative pressure. They collaterally benefit from expanded union jurisdiction if workers are reclassified as employees. They have alternatives to this specific constraint: they could advocate for hybrid/third-category protections (hybrid_security_reading) or stronger sectoral regulation without reclassification.
narrative_ontology:constraint_stakeholder(employment_boundary__substantive_employment_reading, worker_advocacy_organizations, beneficiary,
    organized, generational, mobile, global).

% Venture capital and private equity investors in platform companies. The substantive employment reading imposes significant cost externalities (benefits, taxes, stability) that compress platform profitability and exit valuations. They have arbitrage: they can shift capital to markets with weaker labor enforcement, to automation strategies, or to business models (on-demand contracted services, subscription models) that avoid triggering the reclassification.
narrative_ontology:constraint_stakeholder(employment_boundary__substantive_employment_reading, business_model_investors, payer,
    powerful, biographical, arbitrage, global).

% Judges, legal scholars, and regulators who interpret the employment kernel through formalist/contract-centric lenses. They would argue that formal contract terms and absence of direct supervision make workers independent contractors, and that social protection for such workers belongs in sectoral regulation or a third category, not in employment law. They are excluded from decision-making where the substantive reading gains ground, but remain active in jurisdictions where the formalist reading holds.
narrative_ontology:constraint_stakeholder(employment_boundary__substantive_employment_reading, formalist_legal_interpreters, excluded,
    institutional, generational, analytical, national).

% End users of platform services (ride, food delivery, task completion). Benefit from lower service costs enabled by lower labor costs under the contractor model. Under the substantive employment reading, service costs would rise as platform operators absorb employment-level labor costs. They experience this as a diffuse cost increase and have exit options (use competing services, use non-platform alternatives).
narrative_ontology:constraint_stakeholder(employment_boundary__substantive_employment_reading, consumer_users, beneficiary,
    organized, immediate, mobile, global).

% Policy advocates, some labor economists, and some jurisdictions that propose a third category of 'dependent contractors' or 'platform workers' with tailored protections (portable benefits, sectoral bargaining, algorithmic transparency) distinct from both employment and independent contracting. They would argue the substantive reading is over-inclusive and the formalist reading is under-inclusive; they advocate for a middle ground. They are excluded from the binary choice this constraint imposes between employment and independent contracting.
narrative_ontology:constraint_stakeholder(employment_boundary__substantive_employment_reading, hybrid_security_reading_advocates, excluded,
    organized, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(employment_boundary__substantive_employment_reading, platform_workers).
narrative_ontology:fixing_cost_class(employment_boundary__substantive_employment_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a baseline for labor protection that applies universally to any work arrangement involving economic dependence and algorithmic control, regardless of contractual form. Solves the coordination problem of preventing regulatory arbitrage where platforms avoid labor standards by relabeling workers as independent contractors while exerting full employment-equivalent control.
% TRANSFER_FUNCTION: Transfers labor-cost liabilities from workers to platform operators: workers gain access to minimum wage guarantees, benefits (health, unemployment, pension), job security, and anti-retaliation protection; platforms must absorb the full employment-level cost of labor provision, reducing their ability to externalize risk and precarity onto workers.
% ABSENT_VOICES: Formalist legal interpreters (contract-centric judges, conservative legal scholars) are structurally excluded from the decision-making forums where this reading gains traction; hybrid-security advocates are excluded from the binary choice (employment vs. independent contractor) this constraint imposes. Both would argue for alternative framings of the employment kernel that this reading forecloses or makes invisible.
% DISAPPEARANCE_RATIONALE: If this constraint disappeared overnight and platforms reverted entirely to the formalist reading, the platform economy would reorganize: workers would lose access to employment protections (minimum wage, benefits, job security); platforms would accelerate automation and geographical relocation to low-enforcement jurisdictions; worker organizing and labor union presence in the platform sector would collapse; public expenditure on social safety nets would rise to absorb precarity otherwise borne by platforms.
% FOUNDING_PROBLEM: The platform economy emerged with a new labor arrangement: workers economically dependent on algorithmic assignment of work, subject to algorithmic discipline and deactivation, paid per-task without benefits or stability, but with no formal employment relationship. This arrangement enabled platforms to capture labor-cost savings by externalizing risk onto workers while exerting control equivalent to employment. The founding problem: how to prevent regulatory arbitrage where control without contract allows labor exploitation.
% FOUNDING_PROBLEM_CORROBORATION: Worker advocacy organizations, labor economists, and labor regulators in high-enforcement jurisdictions (EU, California post-AB5, UK post-Uber v. Aslam) attest the problem remains live and intensifying as platforms scale. Platform operators and business-model investors attest the problem is overstated and argue that flexibility and independence are genuine worker preferences. The divergence in attestation reflects the kernel contest itself: the formalist and substantive readings disagree on whether the problem exists.
narrative_ontology:disappearance_verdict(employment_boundary__substantive_employment_reading, world_rearranges).
narrative_ontology:founding_problem_status(employment_boundary__substantive_employment_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(employment_boundary__substantive_employment_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(employment_boundary__substantive_employment_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(employment_boundary__substantive_employment_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(employment_boundary__substantive_employment_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(employment_boundary__substantive_employment_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint exhibits moderate-to-high extractiveness (0.62 at interval end) because platforms extract labor-cost savings by reclassifying workers out of the employment relationship while maintaining control equivalent to employment. Suppression is substantial (0.58) but lower than extractiveness because the constraint requires active enforcement against platform resistance: platforms litigate reclassification in every major jurisdiction, lobby for legislative carve-outs, and threaten economic exit (automation, relocation). The theater ratio (0.41) reflects rising performative compliance: platforms adopt algorithmic transparency initiatives and worker-support programs while maintaining the contractor model in jurisdictions where enforcement is weak, or automating faster where enforcement is strong. Resistance is high (0.71) because the constraint faces organized labor mobilization, legislative pressure in multiple countries, and appellate litigation that generates vivid worker testimony. The coercion grid shows differentiated level dynamics: individual-level accessibility collapse is high (workers are trapped at 0.68-0.72) but organizational-level collapse is low (platforms retain arbitrage and litigation options at 0.35-0.38). At the class level, accessibility collapse and stakes inflation both rise (0.42→0.51 and 0.52→0.71) as worker organizing and legislative pressure intensify. At the structural level, suppression and resistance both rise (0.32→0.48 and 0.42→0.68) as the employment kernel becomes a site of intense politicization.
 *
 * PERSPECTIVAL GAP:
 *   The substantive employment reading produces radically different classifications across seats. From the platform_operators seat (institutional power, arbitrage exit): the reading is an existential threat — reclassification imposes prohibitive costs and eliminates the labor-cost advantages that enabled the business model. Platforms see this reading as imposing employment liabilities based on a redefinition of contract and control that conflicts with explicit worker agreement to contractor status. From the platform_workers seat (powerless, trapped exit): the reading is a vindication — it recognizes the reality of economic dependence and algorithmic discipline that the formal contract denies. From the labor_regulators seat (institutional authority, analytical): the reading represents a coherent legal theory of substantive employment that aligns with labor-protection principles developed in the 20th century. From the hybrid_security_reading_advocates seat (excluded): the reading is over-inclusive — it applies the employment category to a labor arrangement that may warrant distinct treatment. These divergences are structural, not empirical — they reflect genuine differences in what the employment kernel MEANS and what classifications it generates at different institutional positions.
 *
 * DIRECTIONALITY LOGIC:
 *   Platform workers are direct beneficiaries (gain minimum wage, benefits, job security, anti-retaliation protection; d near 0.0 as beneficiaries). Platform operators are payers (absorb employment-level costs, face reclassification litigation, must restructure labor relations; d near 1.0 as targets). Labor regulators are neither pure beneficiaries nor payers — they bear the cost of enforcement (litigation, administrative capacity, political pressure) while gaining legitimacy and authority from enforcing labor-protection principles; their directionality is moderate (d ~0.45). Worker advocacy organizations are beneficiaries (expanded union jurisdiction, vindication of organizing efforts) but also bear costs through litigation and sustained mobilization; their directionality is moderate (d ~0.35). Business-model investors are payers (profit compression, valuation pressure) with arbitrage options (relocation, automation); their directionality is elevated (d ~0.75). Consumer-users bear diffuse costs (higher service prices) with exit options (use competing services); their directionality is moderate (d ~0.50). The structural asymmetry is clear: platforms bear concentrated, unavoidable costs under this reading; workers gain concentrated benefits; other seats distribute costs and benefits more diffusely. The constraint requires active enforcement because platforms' arbitrage options (automation, geographic relocation) would otherwise eliminate the reclassification.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is live (the platform economy continues to generate labor arrangements designed to avoid employment classification) and the substantive employment reading targets it directly. However, the constraint exhibits characteristics of capture and performance: platforms are adapting their labor strategies faster than regulatory enforcement can respond (automation, relocation to lower-enforcement jurisdictions, use of contractor-friendly legislation to override the reading). At the regulatory level, the constraint shows signs of mandatrophy: the original mandate (prevent labor-cost externalization through contractor misclassification) is live, but the enforcement machinery (regulatory agencies, courts, labor departments) is increasingly outmatched by platform scale and cross-border arbitrage. In high-enforcement jurisdictions (EU, California), the constraint is enforced; in low-enforcement jurisdictions, platforms use the formalist reading. The theater ratio rising over the interval suggests the constraint is becoming increasingly performative: platforms adopt labor-friendly messaging and support programs in high-enforcement jurisdictions while maintaining contractor classification and automation strategies elsewhere. The constraint does NOT yet meet the piton definition (it is not an atrophied function maintained theatrically; the enforcement machinery is active and contested, not inert), but it is trending toward mandatrophy if platform arbitrage continues to outpace enforcement capacity. The resolving fact: whether labor regulators can coordinate enforcement across jurisdictions faster than platforms can arbitrage to low-enforcement jurisdictions.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    algorithmic_control_definition,
    'What degree and type of algorithmic control constitutes employment-equivalent control for purposes of reclassification? Does algorithmic task assignment alone suffice, or must there be algorithmic discipline (deactivation, rate cuts, performance monitoring)?',
    'Regulatory and judicial doctrine establishing a threshold test. Analysis of what control powers platforms actually exercise (algorithmic matching, rate setting, deactivation) versus what control powers employers exercise (direct supervision, performance management, termination). Comparative analysis across jurisdictions of what control attributes are held sufficient by courts that adopt the substantive reading.',
    'A narrow definition (algorithmic task assignment alone is not control) would reduce the victim set and lower the constraint''s extractiveness; a broad definition (any algorithmic coordination constitutes control) would expand the victim set and raise extractiveness. The definition determines how many workers fall within the substantive employment boundary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(algorithmic_control_definition, conceptual, 'The constitutive content of algorithmic control in the employment test.').

omega_variable(
    economic_dependence_degree,
    'What threshold of income dependence qualifies as employment-defining economic dependence? Full-time income dependence, primary income source, substantial income supplementation, or any meaningful income contribution?',
    'Comparative analysis of worker income profiles across platform types (full-time delivery workers, part-time task workers, occasional gig workers). Regulatory and judicial doctrine from high-enforcement jurisdictions. Analysis of what income-dependence threshold is used in existing employment law (secondary earners, students, retirees) and where the boundary is drawn.',
    'A narrow definition (employment requires full-time income dependence) would reduce the victim set and lower extractiveness; a broad definition (any substantial income contribution triggers employment status) would expand the victim set and raise extractiveness. The definition determines whether part-time and occasional gig workers are included in the reclassification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(economic_dependence_degree, conceptual, 'The income-dependence threshold for substantive employment.').

omega_variable(
    formalist_reading_persistence,
    'Will the formalist employment reading persist in low-enforcement jurisdictions and in judicial coalitions committed to contractual freedom and minimal labor regulation, even as the substantive reading gains ground in high-enforcement jurisdictions?',
    'Long-term tracking of employment classification doctrine across jurisdictions. Analysis of whether the substantive reading establishes a coordinated global labor-protection floor, or whether regulatory arbitrage allows platforms to segment their labor strategy by enforcement regime (substantive reading in EU/California, formalist reading in Florida/India/Southeast Asia).',
    'If the formalist reading persists in low-enforcement jurisdictions, the constraint achieves partial enforcement but remains vulnerable to platform arbitrage (automation in high-enforcement zones, relocation to low-enforcement zones, use of global contractor labor pools). If the substantive reading establishes a coordinated enforcement floor, the constraint reaches global scope. The kernel contest remains live across jurisdictions even if the substantive reading wins in some.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(formalist_reading_persistence, empirical, 'The cross-jurisdictional persistence of the formalist reading and platform regulatory arbitrage.').

omega_variable(
    suppression_mechanism_internalization,
    'Is the measured suppression (platform litigation, lobbying, automation threats) structural (external barriers that disappear if the constraint is removed) or internalized (platforms and investors have absorbed a narrative that platforms cannot survive with employment-level labor costs, and this belief persists even if external barriers change)?',
    'Post-reclassification case studies from jurisdictions that mandate the substantive reading: do platforms actually reorganize and survive with employment-level labor costs, or do they exit/automate faster than the narrative predicted? Analysis of platform messaging and investor behavior to distinguish structural barriers (litigation costs, higher labor costs) from internalized narratives (platform models are fundamentally incompatible with employment).',
    'If suppression is structural, removing the constraint or changing the regulatory environment would change platform behavior. If suppression is internalized, platforms would continue to resist and pursue automation/relocation even with changed external conditions. Internalized suppression suggests the constraint''s persistence depends on sustained enforcement against deeply entrenched opposition.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether platform suppression of the substantive reading is structural or internalized.').

omega_variable(
    kernel_reading_contest,
    'Is the employment_boundary kernel a genuine contest between three incommensurable legal-philosophical readings of what employment means (substantive control, formal contract, dependent contractor category), or is one reading more legally and morally defensible and the others are merely positions of interest?',
    'Philosophical and jurisprudential analysis of whether the three readings are equally coherent within labor law''s own internal logic, or whether the labor law tradition has a settled definition of employment that one reading captures and the others distort. Historical analysis of how the employment concept developed and whether contemporary platforms represent genuinely novel labor arrangements or just old precarity under new names.',
    'If the contest is genuinely incommensurable (three coherent readings with no principled way to choose between them), the constraint is constitutively political — there is no fact of the matter about which reading is correct, and enforcement depends on political power. If one reading is more defensible, that reading represents a real discovery about what employment is, and the other readings are cover stories. The classification of this constraint as tangled_rope (genuine coordination function + asymmetric extraction) assumes the readings are incommensurable; if one reading is objectively correct, the constraint should be reclassified as snare (pure extraction with a coordination cover story) from the losing reading''s perspective.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_contest, preference, 'The philosophical status of the employment kernel contest.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(employment_boundary__substantive_employment_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(empl_tr_t0, employment_boundary__substantive_employment_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement_basis(empl_tr_t0, observed).
narrative_ontology:measurement(empl_tr_t5, employment_boundary__substantive_employment_reading, theater_ratio, 5, 0.28).
narrative_ontology:measurement_basis(empl_tr_t5, observed).
narrative_ontology:measurement(empl_tr_t10, employment_boundary__substantive_employment_reading, theater_ratio, 10, 0.33).
narrative_ontology:measurement_basis(empl_tr_t10, observed).
narrative_ontology:measurement(empl_tr_t15, employment_boundary__substantive_employment_reading, theater_ratio, 15, 0.38).
narrative_ontology:measurement_basis(empl_tr_t15, observed).
narrative_ontology:measurement(empl_tr_t20, employment_boundary__substantive_employment_reading, theater_ratio, 20, 0.4).
narrative_ontology:measurement_basis(empl_tr_t20, observed).
narrative_ontology:measurement(empl_tr_t25, employment_boundary__substantive_employment_reading, theater_ratio, 25, 0.41).
narrative_ontology:measurement_basis(empl_tr_t25, projected).

% Extraction over time
narrative_ontology:measurement(empl_be_t0, employment_boundary__substantive_employment_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement_basis(empl_be_t0, observed).
narrative_ontology:measurement(empl_be_t5, employment_boundary__substantive_employment_reading, base_extractiveness, 5, 0.45).
narrative_ontology:measurement_basis(empl_be_t5, observed).
narrative_ontology:measurement(empl_be_t10, employment_boundary__substantive_employment_reading, base_extractiveness, 10, 0.52).
narrative_ontology:measurement_basis(empl_be_t10, observed).
narrative_ontology:measurement(empl_be_t15, employment_boundary__substantive_employment_reading, base_extractiveness, 15, 0.58).
narrative_ontology:measurement_basis(empl_be_t15, observed).
narrative_ontology:measurement(empl_be_t20, employment_boundary__substantive_employment_reading, base_extractiveness, 20, 0.61).
narrative_ontology:measurement_basis(empl_be_t20, observed).
narrative_ontology:measurement(empl_be_t25, employment_boundary__substantive_employment_reading, base_extractiveness, 25, 0.62).
narrative_ontology:measurement_basis(empl_be_t25, projected).

% Suppression requirement over time
narrative_ontology:measurement(empl_su_t0, employment_boundary__substantive_employment_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement_basis(empl_su_t0, observed).
narrative_ontology:measurement(empl_su_t5, employment_boundary__substantive_employment_reading, suppression_requirement, 5, 0.42).
narrative_ontology:measurement_basis(empl_su_t5, observed).
narrative_ontology:measurement(empl_su_t10, employment_boundary__substantive_employment_reading, suppression_requirement, 10, 0.48).
narrative_ontology:measurement_basis(empl_su_t10, observed).
narrative_ontology:measurement(empl_su_t15, employment_boundary__substantive_employment_reading, suppression_requirement, 15, 0.54).
narrative_ontology:measurement_basis(empl_su_t15, observed).
narrative_ontology:measurement(empl_su_t20, employment_boundary__substantive_employment_reading, suppression_requirement, 20, 0.57).
narrative_ontology:measurement_basis(empl_su_t20, observed).
narrative_ontology:measurement(empl_su_t25, employment_boundary__substantive_employment_reading, suppression_requirement, 25, 0.58).
narrative_ontology:measurement_basis(empl_su_t25, projected).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=0, tn=25
narrative_ontology:measurement(empl_grid_01, employment_boundary__substantive_employment_reading, accessibility_collapse(class), 0, 0.42).
narrative_ontology:measurement(empl_grid_02, employment_boundary__substantive_employment_reading, accessibility_collapse(class), 25, 0.51).
narrative_ontology:measurement(empl_grid_03, employment_boundary__substantive_employment_reading, accessibility_collapse(individual), 0, 0.72).
narrative_ontology:measurement(empl_grid_04, employment_boundary__substantive_employment_reading, accessibility_collapse(individual), 25, 0.68).
narrative_ontology:measurement(empl_grid_05, employment_boundary__substantive_employment_reading, accessibility_collapse(organizational), 0, 0.35).
narrative_ontology:measurement(empl_grid_06, employment_boundary__substantive_employment_reading, accessibility_collapse(organizational), 25, 0.38).
narrative_ontology:measurement(empl_grid_07, employment_boundary__substantive_employment_reading, accessibility_collapse(structural), 0, 0.38).
narrative_ontology:measurement(empl_grid_08, employment_boundary__substantive_employment_reading, accessibility_collapse(structural), 25, 0.45).
narrative_ontology:measurement(empl_grid_09, employment_boundary__substantive_employment_reading, resistance(class), 0, 0.65).
narrative_ontology:measurement(empl_grid_10, employment_boundary__substantive_employment_reading, resistance(class), 25, 0.75).
narrative_ontology:measurement(empl_grid_11, employment_boundary__substantive_employment_reading, resistance(individual), 0, 0.48).
narrative_ontology:measurement(empl_grid_12, employment_boundary__substantive_employment_reading, resistance(individual), 25, 0.62).
narrative_ontology:measurement(empl_grid_13, employment_boundary__substantive_employment_reading, resistance(organizational), 0, 0.71).
narrative_ontology:measurement(empl_grid_14, employment_boundary__substantive_employment_reading, resistance(organizational), 25, 0.78).
narrative_ontology:measurement(empl_grid_15, employment_boundary__substantive_employment_reading, resistance(structural), 0, 0.42).
narrative_ontology:measurement(empl_grid_16, employment_boundary__substantive_employment_reading, resistance(structural), 25, 0.68).
narrative_ontology:measurement(empl_grid_17, employment_boundary__substantive_employment_reading, stakes_inflation(class), 0, 0.52).
narrative_ontology:measurement(empl_grid_18, employment_boundary__substantive_employment_reading, stakes_inflation(class), 25, 0.71).
narrative_ontology:measurement(empl_grid_19, employment_boundary__substantive_employment_reading, stakes_inflation(individual), 0, 0.65).
narrative_ontology:measurement(empl_grid_20, employment_boundary__substantive_employment_reading, stakes_inflation(individual), 25, 0.68).
narrative_ontology:measurement(empl_grid_21, employment_boundary__substantive_employment_reading, stakes_inflation(organizational), 0, 0.48).
narrative_ontology:measurement(empl_grid_22, employment_boundary__substantive_employment_reading, stakes_inflation(organizational), 25, 0.62).
narrative_ontology:measurement(empl_grid_23, employment_boundary__substantive_employment_reading, stakes_inflation(structural), 0, 0.41).
narrative_ontology:measurement(empl_grid_24, employment_boundary__substantive_employment_reading, stakes_inflation(structural), 25, 0.58).
narrative_ontology:measurement(empl_grid_25, employment_boundary__substantive_employment_reading, suppression(class), 0, 0.35).
narrative_ontology:measurement(empl_grid_26, employment_boundary__substantive_employment_reading, suppression(class), 25, 0.62).
narrative_ontology:measurement(empl_grid_27, employment_boundary__substantive_employment_reading, suppression(individual), 0, 0.52).
narrative_ontology:measurement(empl_grid_28, employment_boundary__substantive_employment_reading, suppression(individual), 25, 0.55).
narrative_ontology:measurement(empl_grid_29, employment_boundary__substantive_employment_reading, suppression(organizational), 0, 0.28).
narrative_ontology:measurement(empl_grid_30, employment_boundary__substantive_employment_reading, suppression(organizational), 25, 0.38).
narrative_ontology:measurement(empl_grid_31, employment_boundary__substantive_employment_reading, suppression(structural), 0, 0.32).
narrative_ontology:measurement(empl_grid_32, employment_boundary__substantive_employment_reading, suppression(structural), 25, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(employment_boundary__substantive_employment_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(employment_boundary__substantive_employment_reading, 0.18).
narrative_ontology:affects_constraint(employment_boundary__substantive_employment_reading, employment_boundary__formalist_employment_reading).
narrative_ontology:affects_constraint(employment_boundary__substantive_employment_reading, employment_boundary__hybrid_security_reading).
narrative_ontology:affects_constraint(employment_boundary__substantive_employment_reading, platform_regulatory_capture).
narrative_ontology:affects_constraint(employment_boundary__substantive_employment_reading, labor_cost_externalization_mechanism).

% DUAL FORMULATION NOTE:
% The employment_boundary kernel generates three constraint stories corresponding to three readings: the substantive_employment_reading (this story), the formalist_employment_reading, and the hybrid_security_reading. Each story has its own ε, beneficiary/victim structure, and type classification. The readings are linked via network.affects_constraints to enable analysis of how gains and losses flow across the kernel contest. The substantive reading directly influences the formalist reading (makes it inoperative in high-enforcement jurisdictions) and influences the hybrid reading (narrows the logical space where a third category could exist).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(employment_boundary__substantive_employment_reading, institutional, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
