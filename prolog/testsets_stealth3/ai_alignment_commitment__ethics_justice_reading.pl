% ============================================================================
% CONSTRAINT STORY: ai_alignment_commitment__ethics_justice_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-01
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_alignment_commitment__ethics_justice_reading, []).

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
 *   constraint_id: ai_alignment_commitment__ethics_justice_reading
 *   human_readable: Ethics-and-Justice Reading of AI Alignment: Present-Harm Prevention Mandate
 *   domain: technological/political/ethical
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the contested kernel 'what does AI
 *   alignment mean': the ethics-and-justice reading, under which alignment
 *   means preventing reproduction of social bias and present-day harm in
 *   deployed systems. The standing arrangement under contest — and therefore
 *   the epsilon referent — is the institutional allocation regime governed by
 *   this definition: funding lines, compliance rubrics, mandated audits, and
 *   publication norms that direct alignment effort toward demonstrated
 *   present harms. Epsilon is assessed BY THIS READING'S OWN LIGHTS: the
 *   arrangement genuinely delivers protection to currently-harmed communities
 *   while conceding, as the reading's own structural accounting shows, a real
 *   transfer burden on long-horizon safety research — it is not scored
 *   against the reading's endorsed ideal (which would drive epsilon toward
 *   zero by construction). The colloquial label 'AI alignment' decomposes,
 *   per the epsilon-invariance principle, into three structurally distinct
 *   constraints: this reading (present-harm prevention, victim set =
 *   displaced safety research), safety_control_reading (catastrophic-risk
 *   prevention, victim set = present-harm-affected communities under a
 *   reversed priority), and integrated_reading (joint non-exclusive
 *   attention). They are separate stories linked through
 *   network.affects_constraints; this file authors only its own reading and
 *   does not average or hedge across siblings. KEY AGENTS (by structural
 *   relationship): - alignment_funding_agencies: agenda setter
 *   (institutional/arbitrage) — administers the definition -
 *   marginalized_bias_affected_communities: primary intended beneficiary
 *   (organized/trapped) - fairness_audit_industry: beneficiary
 *   (organized/mobile) — seat where redirected resources land -
 *   long_term_safety_researchers: primary payer (moderate/identity_locked) -
 *   frontier_ai_labs: payer/beneficiary (powerful/arbitrage) -
 *   independent_open_source_developers: payer (moderate/constrained) -
 *   non_consulted_end_users: excluded (powerless/trapped) -
 *   alignment_governance_analysts: analytical observer
 *
 * KEY AGENTS:
 *   - - alignment_funding_agencies: agenda setter (institutional/arbitrage) — operationalizes the ethics-justice definition through grant lines, compliance rubrics, and standards bodies
 *   - - marginalized_bias_affected_communities: primary intended beneficiary (organized/trapped) — experiences the present-day harms the definition targets; the biographical time horizon is this reading's signature
 *   - - fairness_audit_industry: beneficiary (organized/mobile) — converts mandates into audit contracts, methods literature, and career capital
 *   - - long_term_safety_researchers: primary payer (moderate/identity_locked) — loses budget share and definitional standing; civilizational horizon collides with the reading's biographical one
 *   - - frontier_ai_labs: payer and beneficiary (powerful/arbitrage) — absorbs compliance costs while banking legal cover and trust returns
 *   - - independent_open_source_developers: payer (moderate/constrained) — carries disproportionate documentation burden with least representation
 *   - - non_consulted_end_users: excluded (powerless/trapped) — experience fairness-driven behavior changes with no seat in standard-setting
 *   - - alignment_governance_analysts: analytical observer (analytical/analytical) — sees the full three-reading contest
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_alignment_commitment__ethics_justice_reading, 0.66).
domain_priors:suppression_score(ai_alignment_commitment__ethics_justice_reading, 0.61).
domain_priors:theater_ratio(ai_alignment_commitment__ethics_justice_reading, 0.34).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_alignment_commitment__ethics_justice_reading, extractiveness, 0.66).
narrative_ontology:constraint_metric(ai_alignment_commitment__ethics_justice_reading, suppression_requirement, 0.61).
narrative_ontology:constraint_metric(ai_alignment_commitment__ethics_justice_reading, theater_ratio, 0.34).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_alignment_commitment__ethics_justice_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(ai_alignment_commitment__ethics_justice_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_alignment_commitment__ethics_justice_reading, tangled_rope).
narrative_ontology:human_readable(ai_alignment_commitment__ethics_justice_reading, "Ethics-and-Justice Reading of AI Alignment: Present-Harm Prevention Mandate").
narrative_ontology:topic_domain(ai_alignment_commitment__ethics_justice_reading, "technological/political/ethical").

domain_priors:requires_active_enforcement(ai_alignment_commitment__ethics_justice_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_alignment_commitment__ethics_justice_reading, 'c1ed2309-6cca-4274-884a-d00734854992').
narrative_ontology:cs_kernel_codification('c1ed2309-6cca-4274-884a-d00734854992', distributed).
narrative_ontology:cs_authority_grounding('c1ed2309-6cca-4274-884a-d00734854992', expertise).
narrative_ontology:cs_interpretation_layer_present('c1ed2309-6cca-4274-884a-d00734854992').
narrative_ontology:cs_reading_relation('c1ed2309-6cca-4274-884a-d00734854992', ai_alignment_commitment__safety_control_reading, coexists_with).
narrative_ontology:cs_reading_relation('c1ed2309-6cca-4274-884a-d00734854992', ai_alignment_commitment__integrated_reading, influences).
narrative_ontology:cs_axiom('c1ed2309-6cca-4274-884a-d00734854992', foundational, demonstrated_present_harms_take_priority).
narrative_ontology:cs_axiom_status(demonstrated_present_harms_take_priority, holdable).
narrative_ontology:cs_axiom_grounding('c1ed2309-6cca-4274-884a-d00734854992', demonstrated_present_harms_take_priority, empirically_contingent).
narrative_ontology:cs_axiom('c1ed2309-6cca-4274-884a-d00734854992', secondary, affected_communities_shape_evaluation_standards).
narrative_ontology:cs_axiom_status(affected_communities_shape_evaluation_standards, holdable).
narrative_ontology:cs_axiom_grounding('c1ed2309-6cca-4274-884a-d00734854992', affected_communities_shape_evaluation_standards, deontological).
narrative_ontology:cs_reference_frame('c1ed2309-6cca-4274-884a-d00734854992', present_harm_centered_alignment_practice).
narrative_ontology:cs_drift_state('c1ed2309-6cca-4274-884a-d00734854992', contemporary_retrenchment_period, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('c1ed2309-6cca-4274-884a-d00734854992', '').
narrative_ontology:cs_kernel_id(ai_alignment_commitment__ethics_justice_reading, ai_alignment_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_alignment_commitment__ethics_justice_reading, marginalized_bias_affected_communities).
narrative_ontology:constraint_beneficiary(ai_alignment_commitment__ethics_justice_reading, fairness_audit_industry).
narrative_ontology:constraint_victim(ai_alignment_commitment__ethics_justice_reading, long_term_safety_researchers).
narrative_ontology:constraint_victim(ai_alignment_commitment__ethics_justice_reading, independent_open_source_developers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(ai_alignment_commitment__ethics_justice_reading, frontier_ai_labs).
narrative_ontology:constraint_victim(ai_alignment_commitment__ethics_justice_reading, frontier_ai_labs).
narrative_ontology:constraint_vindicates(ai_alignment_commitment__ethics_justice_reading, algorithmic_bias_prevalence_claim).
narrative_ontology:constraint_vindicates(ai_alignment_commitment__ethics_justice_reading, distributive_justice_in_deployed_systems_doctrine).
narrative_ontology:constraint_vindicates(ai_alignment_commitment__ethics_justice_reading, demonstrated_over_speculative_risk_heuristic).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Public funders, standards bodies, and lab review boards that operationalize the definition of alignment in grant calls, compliance rubrics, and evaluation requirements. They decide which research portfolios count as alignment work and can re-weight them; their authority depends on the definition they administer remaining coherent.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__ethics_justice_reading, alignment_funding_agencies, agenda_setter,
    institutional, generational, arbitrage, global).

% Communities currently experiencing discriminatory model outputs in credit, hiring, policing, housing, and content moderation. Advocacy coalitions among them supply the documented harm cases that anchor the definition. Individually they cannot exit algorithmically mediated services; their protection is delivered through the constraint's evaluation and remediation machinery, on timescales measured in their own lives.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__ethics_justice_reading, marginalized_bias_affected_communities, beneficiary,
    organized, biographical, trapped, global).

% Audit firms, fairness-methods researchers, consultancies, and tooling vendors whose services are converted into demand by mandated bias evaluations. Mandates become contracts, citations, and career capital; they can move between clients and sectors freely, and their revenue scales with the breadth of the definition.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__ethics_justice_reading, fairness_audit_industry, beneficiary,
    organized, biographical, mobile, global).

% Interpretability, control, and catastrophic-risk researchers whose programs lose budget share and definitional standing as alignment portfolios are redirected toward present-harm work. Their funding proposals are recast as speculative; some reframe their own work in fairness-compatible language to survive review. Leaving the field would mean abandoning the problem they hold to be paramount, and their professional identity is fused with it; migration to dedicated safety-funded organizations preserves the work but not the standing inside mainstream alignment institutions.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__ethics_justice_reading, long_term_safety_researchers, payer,
    moderate, civilizational, identity_locked, global).

% Develop large-scale models and absorb the constraint's direct compliance costs: audit overhead, documentation, release delays, and fairness-evaluation staff. In exchange they acquire legal defensibility, procurement eligibility, and public-trust returns. They can arbitrage jurisdictions and standards, shifting which regime's requirements bind them.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__ethics_justice_reading, frontier_ai_labs, payer,
    powerful, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(ai_alignment_commitment__ethics_justice_reading, frontier_ai_labs, beneficiary).

% Small teams and community projects distributing models outside frontier labs. They carry the same documentation and evaluation expectations with a fraction of the staffing, and have little representation in the standards bodies that write the requirements; their practical exit is shipping without certification and accepting market exclusion.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__ethics_justice_reading, independent_open_source_developers, payer,
    moderate, biographical, constrained, global).

% Users whose systems' behavior is adjusted by fairness interventions — refusal policies, output filtering, score recalibration — decided in standard-setting rooms they have no access to. They experience the adjustments immediately and cannot opt out of deployed infrastructure.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__ethics_justice_reading, non_consulted_end_users, excluded,
    powerless, immediate, trapped, global).

% Researchers and institutes tracking how the alignment concept is defined, funded, and enforced across jurisdictions. They see the full three-reading contest and the resource flows among the seats, publish comparisons, and hold no stake in which reading wins.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__ethics_justice_reading, alignment_governance_analysts, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ai_alignment_commitment__ethics_justice_reading, fairness_audit_industry).
narrative_ontology:fixing_cost_class(ai_alignment_commitment__ethics_justice_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Deployed systems measurably reproduce discriminatory outcomes; this arrangement gives developers, regulators, funders, and affected communities a single shared, actionable target — documented present-day harm — and builds the evaluation, red-teaming, and remediation infrastructure to address it once, centrally, instead of ad hoc.
% TRANSFER_FUNCTION: Moves funding, institutional attention, and career security within the AI research and governance ecosystem away from long-horizon catastrophic-risk programs toward fairness auditing, bias evaluation, and remediation serving currently-harmed communities; moves compliance costs from the governance layer onto developers, disproportionately onto small distributors.
% ABSENT_VOICES: Non-consulted end users would object that fairness-driven behavior changes are made over their heads; underdeployed-beneficiary populations (patients, applicants) denied useful systems by conservative fairness freezes have no seat at all. Both are outside the standard-setting conversation that determines what counts as harm.
% DISAPPEARANCE_RATIONALE: If the ethics-justice definition stopped governing alignment overnight, mandated-audit revenue would evaporate, regulatory rubrics and procurement criteria would be orphaned and rewritten, long-horizon safety programs would regain budget share within funding cycles, and communities currently holding a designated advocate for present harms would lose that standing — the whole portfolio-alignment economy reorganizes around whichever definition succeeds it.
% FOUNDING_PROBLEM: Documented algorithmic discrimination — recidivism-score disparities, facial-recognition error gaps, discriminatory hiring filters — was being neglected by a safety discourse oriented toward hypothetical future catastrophe; the reading was founded to force present, measurable, unequally-distributed harms into the center of what alignment means.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set: the peer-reviewed and journalistic audit literature (recidivism-score, facial-recognition, and hiring-tool studies), regulator fundamental-rights impact assessments, and discrimination-litigation records independently attest that present-day algorithmic harms continue. Notably, the paying cohort itself — long-term safety researchers — acknowledges these harms are real while disputing resource priority, which corroborates the founding problem's liveness from the seat that bears the constraint's costs.
narrative_ontology:disappearance_verdict(ai_alignment_commitment__ethics_justice_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_alignment_commitment__ethics_justice_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_alignment_commitment__ethics_justice_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(ai_alignment_commitment__ethics_justice_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_alignment_commitment__ethics_justice_reading, 0.66, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_alignment_commitment__ethics_justice_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_alignment_commitment__ethics_justice_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ai_alignment_commitment__ethics_justice_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is substantial (0.66 at interval end) because the definition decouples resource allocation from comparative risk assessment: portfolios are weighted by which harms are demonstrated and documentable, and the safety cohort's programs are priced as speculative regardless of their own evidentiary case. Suppression (0.61) is enforcement-through-gatekeeping — funding criteria, review norms, and regulatory rubrics — rather than prohibition; alternatives persist (dedicated safety funders, some lab-internal programs), so accessibility_collapse is modest (0.40). Theater_ratio (0.34) reflects a growing compliance layer: bias audits performed for procurement eligibility and ethics statements without remediation teeth, alongside genuinely consequential audit work — the ratio is real activity diluted, not empty performance. Resistance (0.60) is sustained: the paying cohort contests the framing openly, labs lobby against requirement expansion, and integrated-reading advocates argue both problem classes matter. The measurement series run on ONE shared grid (t=0 through t=10, mapping approximately 2016–2026: ProPublica-era audit scandals through the current retrenchment period), with every tracked metric authored at every point; end-state values equal the base_properties scalars. The trajectories are monotonic ratchets over a micro-cyclical substrate: each documented-harm scandal triggers a mandate wave, compliance infrastructure scales, enforcement hardens (rising suppression_requirement), and periodic retrenchment episodes (funding cuts, deregulatory turns) dent but do not reverse the baseline. Suppression here is a RAW STRUCTURAL property of the constraint and is NOT scaled by directionality or scope; only extractiveness is scaled in the engine's computation. Identity-lock note for the payer cohort: the binding mechanism is professional-mission fusion (career path dependence plus self-concept constituted by the problem's importance) — if that frame broke (e.g., a widely-accepted demonstration that present-harm and catastrophe-risk portfolios are strongly complementary), the cohort's exit option would loosen toward constrained/mobile and the seat's computed extraction would drop.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From alignment_funding_agencies' position the arrangement is justice-delivery infrastructure they built and legitimately administer; from marginalized_bias_affected_communities it is overdue protection; from fairness_audit_industry it is a demand guarantee; from long_term_safety_researchers the same structure operates as defunding and definitional exile — their life-scale problem reclassified as someone else's speculation; from frontier_ai_labs it is a compliance price partially repaid in legal cover; from independent_open_source_developers it is an unfunded mandate. Note the payer coalition problem: the safety cohort is fragmented across organizations and identity-fused rather than mobilized, and small distributors lack coordination infrastructure, so neither payer seat mounts effective collective pressure despite shared exposure — the engine should see resistance concentrated in contestation-of-framing rather than material withdrawal.
 *
 * DIRECTIONALITY LOGIC:
 *   Declarations map to directionality as follows. marginalized_bias_affected_communities: full beneficiary-side relationship (protection flows to them; trapped exit pins them near the subsidized end despite their organized advocacy). fairness_audit_industry: beneficiary-side with mobile exit and arbitrage-grade repositioning — nearest the beneficiary pole. long_term_safety_researchers: near-full-target relationship amplified by identity_locked exit; the constraint extracts their budget share and standing specifically. independent_open_source_developers: target-side, dampened slightly by their constrained-but-nonzero alternatives. frontier_ai_labs: genuinely dual-positioned (declared payer with secondary_role beneficiary) — the derivation should place them mid-range rather than at either pole, and no override is needed because the dual declaration already encodes the mixed relationship. alignment_funding_agencies: agenda-setter seat whose arbitrage exit and definitional control sit near the beneficiary end without collecting the extraction receipts directly. No directionality overrides are authored: the beneficiary/victim declarations plus exit atoms produce the relationships described, and adding overrides would second-guess the derivation without new structural information.
 *
 * MANDATROPHY ANALYSIS:
 *   Classification guards both mislabelings at once. Reading the arrangement as a pure snare would erase the genuine coordination function — documented harms are real, the audit literature is corroborated from outside the beneficiary set, and the founding problem is LIVE, which blocks any dead-mandate (piton) verdict outright. Reading it as a pure rope would launder the asymmetric transfer: the same rubric that protects communities prices an entire research program as speculative and lands the redirected resources in a capturable seat. Tangled_rope holds both facts: coordination (shared actionable harm-target) and extraction (safety-cohort burden through the identical structure), sustained by active enforcement (requires_active_enforcement: true). The receipt surface sharpens this: gains accrue to a named seat, so the constraint is captured-flavored even though its coordination half is real — capture does not demote the coordination function, and the live founding problem does not excuse the capture.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This constraint is one reading of the kernel ai_alignment_commitment. If the safety_control_reading were instead the governing definition, which structural elements of this story invert?',
    'Comparative classification of the sibling constraint stories: instantiate safety_control_reading and integrated_reading as their own files and compare victim sets, epsilon values, and computed types across the family.',
    'Under the safety sibling, the victim set becomes present-harm-affected communities and long_term_safety_researchers move to the beneficiary side; the integrated sibling dissolves the exclusive-definition claim and should compute lower extraction for both cohorts. The disagreement is located in the temporal-priority premise, not in any observable.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Committer structure: this story is the ethics-justice instantiation of a three-reading kernel contest.').

omega_variable(
    substitutability_of_safety_and_fairness_spend,
    'Are fairness-allocation and long-horizon-safety resources genuinely zero-sum substitutes, or complementary portfolios sharing methods (interpretability, evaluation infrastructure, red-teaming)?',
    'Longitudinal lab and funder budget data cross-referenced with output complementarity analysis: do safety-program cuts buy measurable fairness-capacity gains, or do the portfolios draw on shared tooling such that cuts degrade both?',
    'Strong complementarity would mean the measured extraction from the safety seat is overstated — the constraint drifts rope-ward; strict substitution confirms the tangled_rope/snare boundary is the live question.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(substitutability_of_safety_and_fairness_spend, empirical, 'Whether the constraint''s transfer function is real displacement or apparent displacement over shared infrastructure.').

omega_variable(
    harm_measurement_validity,
    'Do mandated bias evaluations measure realized harm, or do they increasingly produce compliance artifacts optimized for rubric passage?',
    'Track mandated-audit findings against downstream outcome data for audited systems; compare metric-choice distributions across mandatory versus voluntary audit contexts.',
    'Artifact-dominance would mean theater_ratio is understated at interval end and Goodhart drift is further along than the scalar suggests; genuine-outcome dominance supports the coordination-function reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(harm_measurement_validity, empirical, 'Validity of the constraint''s central measurement instrument against the harm it nominally targets.').

omega_variable(
    suppression_internalization_in_safety_cohort,
    'Is the suppression borne by long_term_safety_researchers purely structural (funding gates, review norms), or partially internalized (anticipatory self-silencing, reflexive reframing of one''s own work in fairness-compatible language)?',
    'Post-exit suppression trajectory: follow researchers who move to safety-autonomous organizations and foundations; if self-censorship and defensive framing persist after the gatekeeping mechanism is removed, mark the internalized component.',
    'If substantially internalized, the constraint''s effective suppression exceeds the structural measure — targets carry the constraint with them after exit, raising effective extraction on the identity_locked seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization_in_safety_cohort, empirical, 'Structural versus internalized suppression mechanism for the identity-locked payer cohort.').

omega_variable(
    victim_set_boundary_contestation,
    'Which communities count as the ''currently harmed'' set that anchors the definition — and who decides? The boundary shifts with discourse (linguistic minorities added, incarcerated populations contested, non-user bystanders rarely counted).',
    'Trace the composition of harm taxonomies across standards-body revisions, audit-firm methodologies, and advocacy demands over time; identify which additions track documented incidence versus advocacy salience.',
    'Boundary composition changes who sits in the beneficiary derivation and therefore redistributes effective extraction across seats; a boundary drawn by the audit industry rather than affected communities would shift the capture assessment materially.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(victim_set_boundary_contestation, conceptual, 'Contestability of the victim-set boundary that defines this reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_alignment_commitment__ethics_justice_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_a_tr_t0, ai_alignment_commitment__ethics_justice_reading, theater_ratio, 0, 0.16).
narrative_ontology:measurement_basis(ai_a_tr_t0, observed).
narrative_ontology:measurement(ai_a_tr_t2, ai_alignment_commitment__ethics_justice_reading, theater_ratio, 2, 0.2).
narrative_ontology:measurement_basis(ai_a_tr_t2, observed).
narrative_ontology:measurement(ai_a_tr_t4, ai_alignment_commitment__ethics_justice_reading, theater_ratio, 4, 0.24).
narrative_ontology:measurement_basis(ai_a_tr_t4, observed).
narrative_ontology:measurement(ai_a_tr_t6, ai_alignment_commitment__ethics_justice_reading, theater_ratio, 6, 0.27).
narrative_ontology:measurement_basis(ai_a_tr_t6, observed).
narrative_ontology:measurement(ai_a_tr_t8, ai_alignment_commitment__ethics_justice_reading, theater_ratio, 8, 0.31).
narrative_ontology:measurement_basis(ai_a_tr_t8, observed).
narrative_ontology:measurement(ai_a_tr_t10, ai_alignment_commitment__ethics_justice_reading, theater_ratio, 10, 0.34).
narrative_ontology:measurement_basis(ai_a_tr_t10, observed).

% Extraction over time
narrative_ontology:measurement(ai_a_be_t0, ai_alignment_commitment__ethics_justice_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement_basis(ai_a_be_t0, observed).
narrative_ontology:measurement(ai_a_be_t2, ai_alignment_commitment__ethics_justice_reading, base_extractiveness, 2, 0.44).
narrative_ontology:measurement_basis(ai_a_be_t2, observed).
narrative_ontology:measurement(ai_a_be_t4, ai_alignment_commitment__ethics_justice_reading, base_extractiveness, 4, 0.51).
narrative_ontology:measurement_basis(ai_a_be_t4, observed).
narrative_ontology:measurement(ai_a_be_t6, ai_alignment_commitment__ethics_justice_reading, base_extractiveness, 6, 0.57).
narrative_ontology:measurement_basis(ai_a_be_t6, observed).
narrative_ontology:measurement(ai_a_be_t8, ai_alignment_commitment__ethics_justice_reading, base_extractiveness, 8, 0.62).
narrative_ontology:measurement_basis(ai_a_be_t8, observed).
narrative_ontology:measurement(ai_a_be_t10, ai_alignment_commitment__ethics_justice_reading, base_extractiveness, 10, 0.66).
narrative_ontology:measurement_basis(ai_a_be_t10, observed).

% Suppression requirement over time
narrative_ontology:measurement(ai_a_su_t0, ai_alignment_commitment__ethics_justice_reading, suppression_requirement, 0, 0.36).
narrative_ontology:measurement_basis(ai_a_su_t0, observed).
narrative_ontology:measurement(ai_a_su_t2, ai_alignment_commitment__ethics_justice_reading, suppression_requirement, 2, 0.43).
narrative_ontology:measurement_basis(ai_a_su_t2, observed).
narrative_ontology:measurement(ai_a_su_t4, ai_alignment_commitment__ethics_justice_reading, suppression_requirement, 4, 0.49).
narrative_ontology:measurement_basis(ai_a_su_t4, observed).
narrative_ontology:measurement(ai_a_su_t6, ai_alignment_commitment__ethics_justice_reading, suppression_requirement, 6, 0.54).
narrative_ontology:measurement_basis(ai_a_su_t6, observed).
narrative_ontology:measurement(ai_a_su_t8, ai_alignment_commitment__ethics_justice_reading, suppression_requirement, 8, 0.58).
narrative_ontology:measurement_basis(ai_a_su_t8, observed).
narrative_ontology:measurement(ai_a_su_t10, ai_alignment_commitment__ethics_justice_reading, suppression_requirement, 10, 0.61).
narrative_ontology:measurement_basis(ai_a_su_t10, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_alignment_commitment__ethics_justice_reading, resource_allocation).
narrative_ontology:affects_constraint(ai_alignment_commitment__ethics_justice_reading, safety_control_reading).
narrative_ontology:affects_constraint(ai_alignment_commitment__ethics_justice_reading, integrated_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the colloquial label 'AI alignment' per the epsilon-invariance principle. The label covers three structurally distinct claims with different epsilon values, victim sets, and enforcement surfaces: this file (ethics_justice_reading — present demonstrated harm; victim set: displaced long-horizon safety research), safety_control_reading (catastrophic loss of control; victim set: present-harm-affected communities under reversed priority), and integrated_reading (joint non-exclusive attention). The upstream evidence base (peer-reviewed audit literature) feeds this reading's legitimacy; this reading and safety_control_reading jointly feed integrated_reading, which exists partly as a response to their standoff. All family members are linked through network.affects_constraints; no single story hedges epsilon across readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
