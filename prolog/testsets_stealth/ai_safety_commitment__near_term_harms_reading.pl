% ============================================================================
% CONSTRAINT STORY: ai_safety_commitment__near_term_harms_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_safety_commitment__near_term_harms_reading, []).

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
 *   constraint_id: ai_safety_commitment__near_term_harms_reading
 *   human_readable: Near-Term Harms Reading of the AI Safety Commitment
 *   domain: technology governance/AI policy
 *
 * SUMMARY:
 *   Since the mid-2010s, 'AI safety' has been institutionally defined — in
 *   corporate responsible-AI programs, lab-affiliated safety institutes, and
 *   much policy discourse — around documented present-day harms:
 *   discriminatory screening in housing, employment, credit, and benefits;
 *   algorithmic management of gig labor; misinformation amplification and
 *   discriminatory ad delivery. This file instantiates ONE reading of the
 *   contested ai_safety_commitment kernel (the near_term_harms_reading); the
 *   existential_risk_reading and dual_priority_reading are separate
 *   constraints with different victim sets and different epsilon loci, linked
 *   through the network section. The epsilon referent here is the standing
 *   arrangement this reading produced — the voluntary audit-and-disclosure
 *   complex — assessed by the reading's own lights (does it deliver measured
 *   harm reduction?). By that standard it falls short in a structured way:
 *   harms are documented faster than remedied, the remedy path runs through
 *   deployer-controlled processes, and the apparatus doubles as the
 *   deployers' principal instrument for deferring binding regulation. KEY
 *   AGENTS (by structural relationship) are enumerated in key_agents below;
 *   every agent named in beneficiaries or victims appears as a structured
 *   stakeholder.
 *
 * KEY AGENTS:
 *   - large_ai_system_operators: Agenda-setting beneficiary (institutional/arbitrage) — administers the audit-and-disclosure apparatus, controls remediation timelines, and collects regulatory deferral and legitimacy gains
 *   - ai_ethics_audit_industry: Commercial beneficiary (organized/mobile) — sells the audit-shaped remedy; auditors are selected and paid by the deployers they assess
 *   - algorithmically_screened_applicants: Primary target (powerless/trapped) — bears screening discrimination in housing, jobs, credit, and benefits; remedy runs through deployer-controlled processes
 *   - gig_platform_workers: Primary target (moderate/constrained) — bear algorithmic-dispatch, pricing, and deactivation harms; studied more than protected
 *   - marginalized_user_communities: Target with partial voice (moderate/constrained) — bear misinformation and moderation harms; advisory input rarely binds deployment decisions
 *   - civil_rights_regulators: Observer (institutional/analytical) — assess whether voluntary programs deliver measurable harm reduction; can convert guidance into enforceable requirements
 *   - academic_fairness_researchers: Observer (moderate/mobile) — build the metrics and document the harms; data access and funding route through the deployers being studied
 *   - ai_existential_risk_advocates: Excluded (organized/constrained) — locked out of the safety definition this reading draws; contest it from outside the responsible-AI bodies
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_safety_commitment__near_term_harms_reading, 0.68).
domain_priors:suppression_score(ai_safety_commitment__near_term_harms_reading, 0.6).
domain_priors:theater_ratio(ai_safety_commitment__near_term_harms_reading, 0.56).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_safety_commitment__near_term_harms_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(ai_safety_commitment__near_term_harms_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(ai_safety_commitment__near_term_harms_reading, theater_ratio, 0.56).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_safety_commitment__near_term_harms_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(ai_safety_commitment__near_term_harms_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_safety_commitment__near_term_harms_reading, tangled_rope).
narrative_ontology:human_readable(ai_safety_commitment__near_term_harms_reading, "Near-Term Harms Reading of the AI Safety Commitment").
narrative_ontology:topic_domain(ai_safety_commitment__near_term_harms_reading, "technology governance/AI policy").

domain_priors:requires_active_enforcement(ai_safety_commitment__near_term_harms_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_safety_commitment__near_term_harms_reading, '050770ff-0e36-4ad0-ad80-e3d6d9fb5767').
narrative_ontology:cs_kernel_codification('050770ff-0e36-4ad0-ad80-e3d6d9fb5767', distributed).
narrative_ontology:cs_authority_grounding('050770ff-0e36-4ad0-ad80-e3d6d9fb5767', distributed).
narrative_ontology:cs_reading_relation('050770ff-0e36-4ad0-ad80-e3d6d9fb5767', ai_safety_commitment__existential_risk_reading, influences).
narrative_ontology:cs_reading_relation('050770ff-0e36-4ad0-ad80-e3d6d9fb5767', ai_safety_commitment__dual_priority_reading, forecloses).
narrative_ontology:cs_axiom('050770ff-0e36-4ad0-ad80-e3d6d9fb5767', foundational, documented_present_harm_primacy).
narrative_ontology:cs_axiom_status(documented_present_harm_primacy, holdable).
narrative_ontology:cs_axiom_grounding('050770ff-0e36-4ad0-ad80-e3d6d9fb5767', documented_present_harm_primacy, empirically_contingent).
narrative_ontology:cs_axiom('050770ff-0e36-4ad0-ad80-e3d6d9fb5767', foundational, speculative_risk_outside_safety_mandate).
narrative_ontology:cs_axiom_status(speculative_risk_outside_safety_mandate, holdable).
narrative_ontology:cs_axiom_grounding('050770ff-0e36-4ad0-ad80-e3d6d9fb5767', speculative_risk_outside_safety_mandate, instrumental).
narrative_ontology:cs_reference_frame('050770ff-0e36-4ad0-ad80-e3d6d9fb5767', documented_present_harm_baseline).
narrative_ontology:cs_drift_state('050770ff-0e36-4ad0-ad80-e3d6d9fb5767', contemporary_post_conformity_assessment_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('050770ff-0e36-4ad0-ad80-e3d6d9fb5767', '').
narrative_ontology:cs_kernel_id(ai_safety_commitment__near_term_harms_reading, ai_safety_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_safety_commitment__near_term_harms_reading, large_ai_system_operators).
narrative_ontology:constraint_beneficiary(ai_safety_commitment__near_term_harms_reading, ai_ethics_audit_industry).
narrative_ontology:constraint_victim(ai_safety_commitment__near_term_harms_reading, algorithmically_screened_applicants).
narrative_ontology:constraint_victim(ai_safety_commitment__near_term_harms_reading, gig_platform_workers).
narrative_ontology:constraint_victim(ai_safety_commitment__near_term_harms_reading, marginalized_user_communities).
narrative_ontology:constraint_vindicates(ai_safety_commitment__near_term_harms_reading, voluntary_audit_sufficiency_doctrine).
narrative_ontology:constraint_vindicates(ai_safety_commitment__near_term_harms_reading, documentation_before_liability_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operate the deployed systems whose outputs produce the documented harms, and run the responsible-AI programs that define the response: internal fairness audits, model cards, incident review boards, published principles. Fund safety institutes and policy engagement. Publish harm documentation on schedules they control; remediation commitments are typically non-binding. Their commercial exposure to binding liability, deployment limits, or mandated independent assessment is reduced for as long as governance stays in this voluntary channel.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__near_term_harms_reading, large_ai_system_operators, agenda_setter,
    institutional, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(ai_safety_commitment__near_term_harms_reading, large_ai_system_operators, beneficiary).

% Sells fairness audits, algorithmic impact assessments, and ethics consulting to deployers. Revenue depends on audit-shaped governance remaining the dominant response; a shift to statutory liability with government-run or litigated assessment would displace much of this market. Auditors are selected and paid by the deployers whose systems they assess.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__near_term_harms_reading, ai_ethics_audit_industry, beneficiary,
    organized, biographical, mobile, global).

% Encounter automated screening in housing, employment, credit, and benefits: resume filters, tenant scores, fraud flags. Harms arrive as denials with limited explanation. The available remedy path — complaint portals, deployer-commissioned audits, occasional litigation — is slow, evidence-opaque, and individually costly. Opting out of automated screening is generally not possible; declining the transaction means losing the housing, job, or loan.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__near_term_harms_reading, algorithmically_screened_applicants, payer,
    powerless, immediate, trapped, global).

% Work under algorithmic dispatch, pricing, and deactivation systems. Responsible-AI programs study their conditions and publish workforce-impact summaries, while deactivation decisions and pay-setting remain opaque and appeal processes are narrow. Income needs limit refusal of platform terms; multi-homing across platforms is possible but degrades earnings stability.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__near_term_harms_reading, gig_platform_workers, payer,
    moderate, immediate, constrained, global).

% Bear disproportionate exposure to misinformation amplification, discriminatory ad delivery, and content-moderation errors affecting their languages and topics. Community organizations are invited into advisory councils and trust-and-safety consultations; their recommendations inform reports but rarely bind deployment or product decisions. Exit means leaving communication infrastructure others rely on.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__near_term_harms_reading, marginalized_user_communities, payer,
    moderate, biographical, constrained, global).

% Sectoral and civil-rights agencies receive complaints, run inquiries, and in some jurisdictions now require bias audits for specific automated-decision uses. They assess whether voluntary programs deliver measurable harm reduction and can convert guidance into enforceable requirements; their reach currently lags deployment scale.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__near_term_harms_reading, civil_rights_regulators, observer,
    institutional, generational, analytical, national).

% Develop the disparity metrics, benchmark datasets, and audit methods the apparatus runs on, and document harms the deployers did not volunteer. Data access and research funding frequently route through the deployers being studied, which shapes what gets measured and published.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__near_term_harms_reading, academic_fairness_researchers, observer,
    moderate, biographical, mobile, global).

% Argue the safety label should attach to catastrophic and extinction-scale risks from future systems. Under this reading's definition their concern is classified outside AI safety entirely; they publish, fund rival institutes, and contest the definition in policy venues but hold no seat in the responsible-AI bodies that operationalize the term.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__near_term_harms_reading, ai_existential_risk_advocates, excluded,
    organized, generational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ai_safety_commitment__near_term_harms_reading, large_ai_system_operators).
narrative_ontology:fixing_cost_class(ai_safety_commitment__near_term_harms_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves a real information problem: deployed-system harms were previously invisible, unattributable, and incomparable. Shared harm taxonomies, disparity benchmarks, incident reporting, and red-team practice let deployers, researchers, and regulators see the same events and measure interventions against common baselines.
% TRANSFER_FUNCTION: Moves remedial obligation away from deployers and onto the harmed: the cost of demonstrating harm, waiting out audit cycles, and continuing exposure during review falls on screened applicants, platform workers, and targeted communities. Simultaneously it moves money and status toward deployer-controlled safety programs and the audit industry, and — by this reading's boundary — away from speculative-alignment research.
% ABSENT_VOICES: Existential-risk advocates are excluded by the definition itself; data labelers and content moderators — the labor that produces the training data and filtering the apparatus studies — have no seat; screened individuals appear only as anonymized case statistics in the audits conducted about them.
% DISAPPEARANCE_RATIONALE: Incident documentation, benchmark comparability, and the audit market would collapse overnight, and deployers would lose the legitimacy channel that has deferred binding liability — statutory regimes, litigation, and worker organizing would face a much less defended field. Parties dispute whether the rearrangement would help or further expose the harmed populations, but the arrangements built around the definition visibly depend on it.
% FOUNDING_PROBLEM: Between roughly 2014 and 2020 a wave of documented deployments — recidivism scoring, resume-filter tools, facial-recognition misidentification, discriminatory ad targeting — showed algorithmic systems producing measurable discrimination and manipulation at scale, with no shared method to detect, attribute, or remedy the harms.
% FOUNDING_PROBLEM_CORROBORATION: Investigative journalism (the COMPAS and tenant-screening reporting), civil-rights litigation dockets, NIST measurement studies, and affected-community advocacy organizations attest the founding harms and their continuation independently of the deployers who fund most remediation programs; no corroborating source outside the beneficiary set attests that the problem is solved.
narrative_ontology:disappearance_verdict(ai_safety_commitment__near_term_harms_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_safety_commitment__near_term_harms_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_safety_commitment__near_term_harms_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(ai_safety_commitment__near_term_harms_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_safety_commitment__near_term_harms_reading, 0.68, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_safety_commitment__near_term_harms_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_safety_commitment__near_term_harms_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ai_safety_commitment__near_term_harms_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.68: the arrangement's remedy path converts documented harm into process — audits, reports, principles — whose costs land on victims as delay and continued exposure while deployers bank the legitimacy; the epsilon referent is the standing near-term-harms governance arrangement itself, judged by this reading's own standard of measured harm reduction, which it demonstrably misses. Suppression 0.60 is authored as a raw, unscaled structural property: it reflects crowding-out of binding alternatives (liability regimes, deployment limits, independent assessment mandates) and the practical impossibility of exiting scored systems — it is not coercive force scaled by power or scope, and the engine scales only extractiveness. Theater 0.56: a majority of visible responsible-AI activity is reportage and principle publication, but incident databases, benchmark-driven mitigation, and some genuine red-teaming keep the functional share real. Accessibility collapse 0.45: alternatives — civil-rights litigation, municipal ordinances, the EU conformity-assessment track, worker organizing — remain partly reachable, so alternatives do not fully collapse. Resistance 0.62: sibling-reading contest, community advocacy, and labor pressure are sustained and growing. The claimed type tangled_rope is stated from structural belief — genuine coordination function plus asymmetric transfer plus active enforcement — independently of these metric values; the measurement series run on one shared time grid so every tracked metric is authored at every examined time point.
 *
 * PERSPECTIVAL GAP:
 *   From the operator seat the apparatus is governance it built, funds, and can show regulators; from the screened-applicant and worker seats the same apparatus is a queue their harms enter and rarely exit. The audit industry experiences it as a market; excluded existential-risk advocates experience it as a definitional wall; regulators experience it as a moving target that promises measurement while resisting liability. The engine computes these per-seat divergences from the power, horizon, and exit data on the stakeholder surface; nothing in the authored claim adjudicates them.
 *
 * DIRECTIONALITY LOGIC:
 *   Operators sit nearest the beneficiary end: they administer the arrangement, collect the deferral and legitimacy gains, and hold arbitrage-grade exit (redefinition, relocation, rebranding). The audit industry shares the low-directionality side commercially with mobile exit. Screened applicants sit nearest the target end: trapped exit, immediate horizons, full exposure to harms the apparatus documents but does not stop. Gig workers and marginalized communities sit high-target with constrained exit and partial organizational voice — their advocacy capacity moderates but does not reverse their position. Regulators and academics occupy observer positions with analytical or mobile exit; their directionalities carry little extraction weight. No directionality overrides were needed: the beneficiary/victim declarations plus exit options already yield the correct structural relationships for every seat.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — undocumented, unattributable, unremediable algorithmic harms — is live, so no mandatrophy is declared and no sunset applies. The classification guards against two mislabels: treating the whole apparatus as pure extraction would erase the documentation infrastructure that harmed communities and researchers actively use and that made the harms legible at all; treating it as pure coordination would erase the transfer of remedial burden onto the harmed and the deferral function the operator seat collects. The tangled-rope structure holds both halves. The rising theater series marks the drift to watch: if binding enforcement never arrives, the functional share decays toward administered performance and the arrangement slides toward inertial maintenance — the temporal data exist precisely to date that transition if it comes.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Which reading of the ai_safety_commitment kernel governs the institutional ''AI safety'' label — this near_term_harms_reading, the existential_risk_reading, or the dual_priority_reading?',
    'Track statutory mandate language, public-funding body criteria, and professional-society definitions over time: whichever definition gets written into law, grant criteria, and curricula is the one governing the label.',
    'If the existential_risk_reading were adopted, the victim set shifts to future persons, epsilon concentrates on alignment-research governance, and present-day harm remedies lose their claim on the safety budget entirely; dual_priority adoption restores both agendas as co-equal. This file authors only the near-term reading''s constraint; the siblings are separate stories.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Committer structure: this constraint is one reading of a contested kernel; sibling readings instantiate different constraints.').

omega_variable(
    documented_harm_boundary,
    'Where does ''documented present-day harm'' end — do statistically aggregated discrimination effects, probabilistic misinformation harms, and algorithmic-management injuries that overlap ordinary labor law fall inside the reading''s mandate?',
    'Adjudication in measurement standards and litigation: which harm categories NIST-style benchmarks, conformity assessments, and courts treat as demonstrable and attributable to deployed systems.',
    'A expanding boundary pulls this reading''s scope toward the dual_priority_reading''s territory; a contracting boundary reduces the reading to audit minimalism and lowers measured extractiveness as fewer harms qualify for remedy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(documented_harm_boundary, conceptual, 'Boundary instability in the reading''s core category ''documented present-day harm''.').

omega_variable(
    binding_enforcement_trajectory,
    'Can the audit-shaped remedy convert into actual harm reduction as mandatory conformity regimes arrive (bias-audit mandates for hiring tools, EU conformity assessment), or is the deployer-controlled audit structurally incapable of remedying the harms it documents?',
    'Comparative harm-rate data across jurisdictions and enforcement eras: measured disparity trajectories where independent mandatory assessment applies versus voluntary self-audit.',
    'If mandatory enforcement closes the gap, the arrangement''s coordination share grows and its classification drifts toward load-bearing coordination; if harms persist under binding rules too, the extraction component consolidates and the victim seats'' position worsens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(binding_enforcement_trajectory, empirical, 'Whether the enforcement ramp converts deferral into remedy.').

omega_variable(
    theater_function_attribution,
    'What fraction of visible responsible-AI activity produces measured disparity reduction versus legitimacy output — principles published, reports issued, boards convened — with no intervention effect?',
    'Outcome-linked audit studies: pre/post disparity measurement on systems that underwent formal responsible-AI processes, controlling for secular trends.',
    'A high functional share supports reading the arrangement primarily as coordination infrastructure the victims themselves use; a low share confirms the legitimacy-shield reading and predicts continued theater-ratio growth toward inertial maintenance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theater_function_attribution, empirical, 'Attribution of the theater share: performance versus function in responsible-AI activity.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_safety_commitment__near_term_harms_reading, 2016, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ais_near_term_harms_tr_t2016, ai_safety_commitment__near_term_harms_reading, theater_ratio, 2016, 0.3).
narrative_ontology:measurement_basis(ais_near_term_harms_tr_t2016, observed).
narrative_ontology:measurement(ais_near_term_harms_tr_t2018, ai_safety_commitment__near_term_harms_reading, theater_ratio, 2018, 0.38).
narrative_ontology:measurement_basis(ais_near_term_harms_tr_t2018, observed).
narrative_ontology:measurement(ais_near_term_harms_tr_t2020, ai_safety_commitment__near_term_harms_reading, theater_ratio, 2020, 0.45).
narrative_ontology:measurement_basis(ais_near_term_harms_tr_t2020, observed).
narrative_ontology:measurement(ais_near_term_harms_tr_t2022, ai_safety_commitment__near_term_harms_reading, theater_ratio, 2022, 0.5).
narrative_ontology:measurement_basis(ais_near_term_harms_tr_t2022, observed).
narrative_ontology:measurement(ais_near_term_harms_tr_t2023, ai_safety_commitment__near_term_harms_reading, theater_ratio, 2023, 0.53).
narrative_ontology:measurement_basis(ais_near_term_harms_tr_t2023, observed).
narrative_ontology:measurement(ais_near_term_harms_tr_t2024, ai_safety_commitment__near_term_harms_reading, theater_ratio, 2024, 0.56).
narrative_ontology:measurement_basis(ais_near_term_harms_tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(ais_near_term_harms_be_t2016, ai_safety_commitment__near_term_harms_reading, base_extractiveness, 2016, 0.52).
narrative_ontology:measurement_basis(ais_near_term_harms_be_t2016, observed).
narrative_ontology:measurement(ais_near_term_harms_be_t2018, ai_safety_commitment__near_term_harms_reading, base_extractiveness, 2018, 0.57).
narrative_ontology:measurement_basis(ais_near_term_harms_be_t2018, observed).
narrative_ontology:measurement(ais_near_term_harms_be_t2020, ai_safety_commitment__near_term_harms_reading, base_extractiveness, 2020, 0.61).
narrative_ontology:measurement_basis(ais_near_term_harms_be_t2020, observed).
narrative_ontology:measurement(ais_near_term_harms_be_t2022, ai_safety_commitment__near_term_harms_reading, base_extractiveness, 2022, 0.64).
narrative_ontology:measurement_basis(ais_near_term_harms_be_t2022, observed).
narrative_ontology:measurement(ais_near_term_harms_be_t2023, ai_safety_commitment__near_term_harms_reading, base_extractiveness, 2023, 0.66).
narrative_ontology:measurement_basis(ais_near_term_harms_be_t2023, observed).
narrative_ontology:measurement(ais_near_term_harms_be_t2024, ai_safety_commitment__near_term_harms_reading, base_extractiveness, 2024, 0.68).
narrative_ontology:measurement_basis(ais_near_term_harms_be_t2024, observed).

% Suppression requirement over time
narrative_ontology:measurement(ais_near_term_harms_su_t2016, ai_safety_commitment__near_term_harms_reading, suppression_requirement, 2016, 0.35).
narrative_ontology:measurement_basis(ais_near_term_harms_su_t2016, observed).
narrative_ontology:measurement(ais_near_term_harms_su_t2018, ai_safety_commitment__near_term_harms_reading, suppression_requirement, 2018, 0.4).
narrative_ontology:measurement_basis(ais_near_term_harms_su_t2018, observed).
narrative_ontology:measurement(ais_near_term_harms_su_t2020, ai_safety_commitment__near_term_harms_reading, suppression_requirement, 2020, 0.46).
narrative_ontology:measurement_basis(ais_near_term_harms_su_t2020, observed).
narrative_ontology:measurement(ais_near_term_harms_su_t2022, ai_safety_commitment__near_term_harms_reading, suppression_requirement, 2022, 0.52).
narrative_ontology:measurement_basis(ais_near_term_harms_su_t2022, observed).
narrative_ontology:measurement(ais_near_term_harms_su_t2023, ai_safety_commitment__near_term_harms_reading, suppression_requirement, 2023, 0.57).
narrative_ontology:measurement_basis(ais_near_term_harms_su_t2023, observed).
narrative_ontology:measurement(ais_near_term_harms_su_t2024, ai_safety_commitment__near_term_harms_reading, suppression_requirement, 2024, 0.6).
narrative_ontology:measurement_basis(ais_near_term_harms_su_t2024, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_safety_commitment__near_term_harms_reading, information_standard).
narrative_ontology:affects_constraint(ai_safety_commitment__near_term_harms_reading, existential_risk_reading).
narrative_ontology:affects_constraint(ai_safety_commitment__near_term_harms_reading, dual_priority_reading).

% DUAL FORMULATION NOTE:
% Decomposition of the colloquial label 'AI safety' per the epsilon-invariance principle: the label covers at least three structurally distinct claims with different epsilon values, victim sets, and failure modes. This story carries the near-term-harms claim alone (epsilon concentrated on deployed-system governance: transparency, auditing, labor protections; negligible claim on speculative alignment research, which lies outside this reading's scope by definition rather than by measurement). The existential-risk claim (epsilon concentrated on alignment-research governance, victims located in the future) and the dual-priority claim (both agendas asserted non-competing) are separate files linked here. Influence runs in both directions: each reading cites the others' failures to justify its own definition, and institutional adoption of any one definition reallocates the safety budget against the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
