% ============================================================================
% CONSTRAINT STORY: ai_alignment_priority__nearterm_harms_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_alignment_priority__nearterm_harms_reading, []).

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
 *   constraint_id: ai_alignment_priority__nearterm_harms_reading
 *   human_readable: Near-Term Harms Reading: X-Risk-Dominant Alignment Priority Assessed at Deployed-System Harm Sites
 *   domain: technological/political
 *
 * SUMMARY:
 *   This story instantiates the nearterm_harms_reading of the
 *   ai_alignment_priority kernel: alignment means preventing present
 *   discriminatory and extractive harms of deployed AI, with justice for
 *   marginalized populations as the priority. Per the epsilon-referent rule
 *   for kernel readings, the constraint classified is the STANDING
 *   arrangement under contest — the x-risk-dominant priority ordering of the
 *   AI-safety field — assessed by this reading's own lights, not the
 *   justice-first regime this reading would install. Through that lens the
 *   standing arrangement is a hybrid: catastrophic-risk prevention solves a
 *   genuine collective-action problem no single actor can hedge alone, yet
 *   the same structure concentrates funding, talent, and definitional
 *   authority in institutions addressing speculative future persons while
 *   documented discriminatory outputs of deployed systems —
 *   facial-recognition misidentification, biased hiring screens, automated
 *   benefits sanctions, age-based triage deprioritization — persist
 *   unremediated. The victim set is specific: racial minorities, disabled
 *   benefits claimants, and elderly care recipients, the populations the
 *   delta names. The claim and the metrics are independent authored facts:
 *   claimed_type states my structural judgment of the standing arrangement
 *   through this reading; the metrics state its descriptive operation.
 *   Sibling readings (existential_risk_reading, integrated_reading) are
 *   separate files with their own epsilon, victim sets, and classifications;
 *   this file neither describes nor averages over them.
 *
 * KEY AGENTS:
 *   - xrisk_research_institutes: Agenda-setter and principal beneficiary (institutional / identity_locked) — administers the field's definitional apparatus, flagship venues, and benchmark infrastructure; collects the largest resource share
 *   - alignment_funding_foundations: Co-agenda-setter with secondary beneficiary position (powerful / mobile) — allocates the multi-year funding that holds the priority ordering in place
 *   - frontier_lab_safety_teams: Beneficiary (powerful / arbitrage) — collect headcount, budgets, and safety legitimacy under the x-risk umbrella while deployed products escape present-harm scrutiny
 *   - deployed_system_operators: Incidental beneficiary (powerful / arbitrage) — commercial deployers in credit, hiring, health, and welfare shielded by the future-facing frame
 *   - racial_minority_algorithm_subjects: Principal payer (powerless / trapped) — bear facial-recognition, hiring-screen, credit-scoring, and predictive-policing harms with no opt-out
 *   - disabled_benefits_claimants: Principal payer (powerless / trapped) — bear automated eligibility and fraud-detection sanctions; appeal requires legal resources many lack
 *   - elderly_care_recipients: Principal payer (powerless / trapped) — bear algorithmic triage and care-allocation deprioritization inside consolidated health systems
 *   - civil_rights_audit_organizations: Excluded voice (organized / constrained) — perform the sociotechnical audit work this frame centers but hold no seat where alignment priorities are set
 *   - integrated_framework_advocates: Analytical observer (organized / analytical) — hold the complementary-priorities sibling position and take testimony from both camps
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_alignment_priority__nearterm_harms_reading, 0.8).
domain_priors:suppression_score(ai_alignment_priority__nearterm_harms_reading, 0.63).
domain_priors:theater_ratio(ai_alignment_priority__nearterm_harms_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_alignment_priority__nearterm_harms_reading, extractiveness, 0.8).
narrative_ontology:constraint_metric(ai_alignment_priority__nearterm_harms_reading, suppression_requirement, 0.63).
narrative_ontology:constraint_metric(ai_alignment_priority__nearterm_harms_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_alignment_priority__nearterm_harms_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(ai_alignment_priority__nearterm_harms_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_alignment_priority__nearterm_harms_reading, tangled_rope).
narrative_ontology:human_readable(ai_alignment_priority__nearterm_harms_reading, "Near-Term Harms Reading: X-Risk-Dominant Alignment Priority Assessed at Deployed-System Harm Sites").
narrative_ontology:topic_domain(ai_alignment_priority__nearterm_harms_reading, "technological/political").

domain_priors:requires_active_enforcement(ai_alignment_priority__nearterm_harms_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_alignment_priority__nearterm_harms_reading, 'e8a4236c-ead8-4448-9fad-6fedb294c84b').
narrative_ontology:cs_kernel_codification('e8a4236c-ead8-4448-9fad-6fedb294c84b', distributed).
narrative_ontology:cs_authority_grounding('e8a4236c-ead8-4448-9fad-6fedb294c84b', distributed).
narrative_ontology:cs_reading_relation('e8a4236c-ead8-4448-9fad-6fedb294c84b', ai_alignment_priority__existential_risk_reading, coexists_with).
narrative_ontology:cs_reading_relation('e8a4236c-ead8-4448-9fad-6fedb294c84b', ai_alignment_priority__integrated_reading, influences).
narrative_ontology:cs_axiom('e8a4236c-ead8-4448-9fad-6fedb294c84b', foundational, present_identifiable_persons_morally_prioritary).
narrative_ontology:cs_axiom_status(present_identifiable_persons_morally_prioritary, holdable).
narrative_ontology:cs_axiom_grounding('e8a4236c-ead8-4448-9fad-6fedb294c84b', present_identifiable_persons_morally_prioritary, deontological).
narrative_ontology:cs_axiom('e8a4236c-ead8-4448-9fad-6fedb294c84b', secondary, deployed_system_impact_site_authoritative).
narrative_ontology:cs_axiom_status(deployed_system_impact_site_authoritative, holdable).
narrative_ontology:cs_axiom_grounding('e8a4236c-ead8-4448-9fad-6fedb294c84b', deployed_system_impact_site_authoritative, instrumental).
narrative_ontology:cs_reference_frame('e8a4236c-ead8-4448-9fad-6fedb294c84b', justice_first_present_harm_priority).
narrative_ontology:cs_drift_state('e8a4236c-ead8-4448-9fad-6fedb294c84b', contemporary_frontier_scaling_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('e8a4236c-ead8-4448-9fad-6fedb294c84b', '').
narrative_ontology:cs_kernel_id(ai_alignment_priority__nearterm_harms_reading, ai_alignment_priority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_alignment_priority__nearterm_harms_reading, xrisk_research_institutes).
narrative_ontology:constraint_beneficiary(ai_alignment_priority__nearterm_harms_reading, alignment_funding_foundations).
narrative_ontology:constraint_beneficiary(ai_alignment_priority__nearterm_harms_reading, frontier_lab_safety_teams).
narrative_ontology:constraint_victim(ai_alignment_priority__nearterm_harms_reading, racial_minority_algorithm_subjects).
narrative_ontology:constraint_victim(ai_alignment_priority__nearterm_harms_reading, disabled_benefits_claimants).
narrative_ontology:constraint_victim(ai_alignment_priority__nearterm_harms_reading, elderly_care_recipients).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(ai_alignment_priority__nearterm_harms_reading, deployed_system_operators).
narrative_ontology:constraint_vindicates(ai_alignment_priority__nearterm_harms_reading, longtermist_priority_doctrine).
narrative_ontology:constraint_vindicates(ai_alignment_priority__nearterm_harms_reading, catastrophic_loss_of_control_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Define alignment as loss-of-control prevention and administer the field's flagship venues, benchmarks, and fellowship pipelines. Receive the largest dedicated share of safety funding and set which problems count as core. Their professional identities and institutional missions are constituted by the catastrophic-risk frame; exiting would mean dissolving the mission that makes them who they are, so they defend the frame through peer review, grant-making, and public argument rather than through falsifiable bets.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__nearterm_harms_reading, xrisk_research_institutes, agenda_setter,
    institutional, civilizational, identity_locked, global).

% Allocate multi-year grants overwhelmingly to control-research portfolios and the institutes that pursue them. Gain portfolio growth, agenda influence, and access to frontier-lab partnerships. Capital is mobile: if the definitional frame shifted, they could redirect funds within a few grant cycles, which makes their continued allocation the load-bearing enforcement of the priority ordering.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__nearterm_harms_reading, alignment_funding_foundations, agenda_setter,
    powerful, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(ai_alignment_priority__nearterm_harms_reading, alignment_funding_foundations, beneficiary).

% Collect headcount, budgets, and public safety legitimacy under the x-risk umbrella. Many team members do sincere technical work on interpretability and evals. Their employers' deployed products — ranking, targeting, content moderation, agentic assistants — escape the present-harm scrutiny a justice-first regime would impose, and the teams can rebrand or pivot as frames shift without personal cost.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__nearterm_harms_reading, frontier_lab_safety_teams, beneficiary,
    powerful, biographical, arbitrage, global).

% Operate the credit-scoring, hiring-screen, welfare-eligibility, and health-triage systems whose discriminatory outputs are the harm stock this reading measures. Benefit from a field-wide frame that locates AI danger in hypothetical future systems rather than in their current product lines; face limited present-harm liability and can shift jurisdictions or product lines if scrutiny intensifies.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__nearterm_harms_reading, deployed_system_operators, beneficiary,
    powerful, biographical, arbitrage, continental).

% Subject to facial-recognition misidentification, biased resume screens, credit scoring disparities, and predictive-policing targeting. Cannot opt out of algorithmic decisions governing housing, employment, credit, or police contact. Redress channels — complaint procedures, litigation — are slow, expensive, and under-resourced relative to the volume of automated decisions made about them.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__nearterm_harms_reading, racial_minority_algorithm_subjects, payer,
    powerless, biographical, trapped, national).

% Flagged and sanctioned at elevated rates by automated eligibility determinations and welfare-fraud detection systems. Depend on state-administered benefits with no private alternative, so exit from the system is impossible. Appealing an adverse automated decision requires documentation, legal help, and months of elapsed time that many claimants cannot survive financially.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__nearterm_harms_reading, disabled_benefits_claimants, payer,
    powerless, biographical, trapped, national).

% Subjected to algorithmic triage, care-allocation, and remote-monitoring systems that encode age-based deprioritization of treatment and support. Consolidated health and social-care markets offer no non-algorithmic care pathway to choose instead. Harms arrive as denied interventions and delayed care rather than as discrete appealable events.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__nearterm_harms_reading, elderly_care_recipients, payer,
    powerless, biographical, trapped, national).

% Perform the sociotechnical audit work — disparate-impact testing, red-teaming of deployed decision systems, community-based documentation — that a justice-first frame would center. Hold seats at fairness and accountability venues but not on the safety boards, funding panels, or summit rosters where alignment priorities are set. Their findings circulate as 'adjacent' literature rather than entering the field's core problem list.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__nearterm_harms_reading, civil_rights_audit_organizations, excluded,
    organized, biographical, constrained, national).

% Hold the complementary-priorities position: catastrophic and present harms addressed together rather than ranked. Take testimony from both camps, publish syntheses, and mediate funding disputes. Analytically positioned relative to this story — they neither administer the standing arrangement nor bear its extraction — their stake is in which framing wins the field's definitional contest.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__nearterm_harms_reading, integrated_framework_advocates, observer,
    organized, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ai_alignment_priority__nearterm_harms_reading, xrisk_research_institutes).
narrative_ontology:fixing_cost_class(ai_alignment_priority__nearterm_harms_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Pools research talent, funding, and standards development around the scenario no single actor can hedge alone — loss of control of systems more capable than their overseers — solving a genuine free-rider problem in catastrophic-risk preparedness that uncoordinated market incentives undersupply.
% TRANSFER_FUNCTION: Moves funding, talent, prestige, and definitional authority from present-harm remediation (bias audits, redress infrastructure, deployment gating) toward speculative-control research and its institutions; leaves the ongoing costs of unaddressed discriminatory outputs with the populations subjected to them.
% ABSENT_VOICES: Marginalized communities bearing present harms are nearly absent from alignment agenda-setting bodies — frontier-lab safety forums, x-risk convenings, and the funding panels that compose the field's portfolio. Civil-rights technologists and disability advocates hold seats at fairness venues but not where 'alignment' priorities are fixed; their objection — that the priority ordering prices their members' present harm below hypothetical future persons — is registered only outside the room.
% DISAPPEARANCE_RATIONALE: If the x-risk-dominated priority arrangement vanished overnight, the funding and talent now concentrated in control research would rebalance toward deployed-system audits, redress infrastructure, and deployment gating; conference agendas, career ladders, and evaluation benchmarks would reorganize around present-harm metrics; and the populations currently bearing unremediated discriminatory outputs would see remediation capacity scale up within a few budget cycles.
% FOUNDING_PROBLEM: Before roughly 2015, deployed AI seemed distant and catastrophic loss-of-control appeared radically under-provisioned relative to its stakes; a small research community consolidated around preventing advanced systems from pursuing objectives misaligned with human intent before capability growth made the problem urgent.
% FOUNDING_PROBLEM_CORROBORATION: Split and partial. Government AI-risk assessments and a minority of independent ML researchers corroborate that catastrophic-risk research addresses a real, under-provisioned problem. Audit studies, regulatory enforcement actions, and incident databases corroborate that present discriminatory harms are concrete, recurring, and unremediated. No source outside the arrangement's beneficiary set attests that the priority ordering itself — catastrophic risk above present harm — remains justified; that ordering is attested only by the institutions it funds.
narrative_ontology:disappearance_verdict(ai_alignment_priority__nearterm_harms_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_alignment_priority__nearterm_harms_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_alignment_priority__nearterm_harms_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(ai_alignment_priority__nearterm_harms_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_alignment_priority__nearterm_harms_reading, 0.8, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_alignment_priority__nearterm_harms_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_alignment_priority__nearterm_harms_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ai_alignment_priority__nearterm_harms_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.80 at interval end) because the standing arrangement's resource and legitimacy flows are decoupled from the distribution of realized harm: the populations bearing documented discriminatory outputs receive a marginal share of alignment investment while control research absorbs the growth. Suppression (0.63) is structural, not internalized: gatekeeping runs through funding panels, venue hierarchies, and definitional authority ('that is fairness work, not alignment'), though a secondary internalized component persists in affected communities' learned expectation that their harms will not count as the field's core problem — the omega battery carries the residual ambiguity. Theater ratio (0.38) is moderate and rising: the control-research core is substantively real, but a growing share of field activity is principles documents, ethics boards, and safety framing that signal virtue without touching deployed-system outputs. Accessibility collapse (0.45) is moderate because alternatives persist — the fairness/audit track exists in parallel (NIST-style frameworks, fundamental-rights pillars, FAccT venues) — but it is systematically deprioritized rather than suppressed outright. Resistance (0.60) reflects a mature counter-coalition: civil-rights technologists, labor organizers, and disability advocates contest the priority ordering in publications, hearings, and standards processes. The measurement series run on one shared seven-point grid (2012–2025) with all three metrics authored at every point; trajectories are monotonic rather than cyclical — accumulation, not oscillation — driven by successive funding waves and scaling-era consolidation, with no intermittent-reinforcement mechanism identified.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the agenda-setter seats compute differently from the same structural data. From the x-risk institute and funder positions, the arrangement is genuine coordination they built: a rational response to a catastrophic, under-provisioned risk, with fairness work as a complementary track others are free to pursue. From the trapped payer seats, the identical structure operates as enforced extraction: their harms are priced below hypothetical future persons by institutions they cannot exit, vote out of the agenda, or opt out of being governed by. Frontier lab safety teams occupy the hinge: they sincerely do safety work while their employers collect the deflected-scrutiny dividend. The engine computes these per-seat divergences from power, exit, and role data; the authored claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive the agenda-setter and beneficiary seats toward the low-d (subsidized) end: institutes, foundations, lab safety teams, and deployers all collect from the arrangement's operation. Victim declarations drive the three marginalized-population seats toward the high-d (target) end, amplified by trapped exit — none can opt out of algorithmic credit, welfare, policing, or triage decisions — and by biographical time horizons spent bearing the harm. Global spatial scope modestly amplifies effective extraction by raising verification difficulty for diffuse sociotechnical harms. No directionality overrides were needed: the beneficiary/victim declarations plus exit atoms already differentiate every seat, including the subtle cases — the foundation seat derives correctly from its dual agenda-setter/beneficiary position, and the identity_locked atom on the x-risk institutes binds them TO the arrangement (stabilizing persistence) rather than amplifying their extraction, since lock on a beneficiary seat dampens rather than raises chi.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying the standing arrangement as tangled_rope rather than snare preserves what this reading concedes: catastrophic-risk prevention is a real collective-action problem, and flattening it to cover-story status would erase the genuine coordination core and hand the agenda-setters a persecution narrative that strengthens their gatekeeping. Classifying it as anything softer than tangled_rope would launder the asymmetric extraction this reading exists to register — the same structure that pools real insurance capacity also routes resources away from the only populations suffering verified, ongoing harm. The founding problem (radically under-provisioned catastrophic-risk research circa 2012) is contested rather than dead: its beneficiaries attest it is live, external sources corroborate both underlying problems while corroborating no priority ordering, so mandatrophy_resolved is not declared. The rising theater ratio is the early-warning signal this analysis tracks: if proxy activity (principles, pledges, safety branding) continues substituting for both control research and harm remediation, the arrangement drifts toward piton — maintained performatively by institutions too identity-fused to revise it and too comfortable to kill it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This constraint is one reading of the ai_alignment_priority kernel; what would change structurally if a sibling reading governed instead?',
    'Cross-file comparison over the linked family stories: under the existential_risk_reading the victim set shifts from present marginalized populations to future/unspecified persons and the epsilon referent site shifts from deployed-system audits to training-run oversight; under the integrated_reading the victim set becomes a weighted union and resource flows split between bias mitigation and control research.',
    'Classification of this file is stable under its own reading; cross-reading comparison must use the linked sibling files, never a blended epsilon averaged over readings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Committer structure: this story is the nearterm_harms_reading of a three-reading kernel contest.').

omega_variable(
    catastrophic_risk_substrate_question,
    'Is catastrophic loss-of-control a genuine technical possibility warranting its current resource share, or primarily a discursive construction serving institutional interests?',
    'Convergence of independent technical assessments untied from x-risk funding streams; capability-generalization evidence; adversarial red-team results evaluated by researchers with no stake in either reading.',
    'If the substrate is constructed, the standing arrangement trends snare (coordination story as cover for rent collection); if genuine, tangled_rope holds and part of the measured extraction is the price of real insurance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(catastrophic_risk_substrate_question, empirical, 'Whether the coordination core of the standing arrangement is real or cover.').

omega_variable(
    present_harm_aggregate_magnitude,
    'How large is the aggregate present harm from deployed discriminatory systems relative to the counterfactual in which alignment resources were redirected to remediation?',
    'Systematic audit registries and incident databases with population-level harm quantification across hiring, credit, welfare, policing, and health domains.',
    'Calibrates epsilon: a larger verified harm stock raises epsilon and strengthens the extraction reading of the priority ordering.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(present_harm_aggregate_magnitude, empirical, 'Magnitude of the unremediated present-harm stock this reading measures.').

omega_variable(
    xrisk_identity_fusion_depth,
    'Is the x-risk research community''s commitment epistemic (responsive to evidence) or identity-fused (constitutive of professional selves and institutional missions)?',
    'Track belief revision under disconfirming capability-generalization evidence; observe career mobility out of x-risk roles when funding shifts; compare institutional behavior when core predictions fail.',
    'If identity-fused, exit_options stay identity_locked and the arrangement''s persistence decouples from evidence, hardening the tangled_rope reading toward inertial maintenance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(xrisk_identity_fusion_depth, empirical, 'Depth of identity lock binding the agenda-setting seat to the arrangement.').

omega_variable(
    justice_priority_enforceability,
    'Could a justice-first priority actually bind, given that sociotechnical harms are diffuse and hard to verify while capability metrics are crisp?',
    'Compare compliance trajectories under jurisdictions with mandated third-party sociotechnical audits versus voluntary fairness frameworks.',
    'If present harms resist verification at scale, this reading''s endorsed alternative would itself drift toward theater — constraining how sharply this reading can condemn the standing arrangement''s verification asymmetry.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(justice_priority_enforceability, conceptual, 'Whether the reading''s own remedy escapes the verification asymmetry it diagnoses.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_alignment_priority__nearterm_harms_reading, 2012, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(aip_nearterm_harms_tr_t2012, ai_alignment_priority__nearterm_harms_reading, theater_ratio, 2012, 0.15).
narrative_ontology:measurement_basis(aip_nearterm_harms_tr_t2012, observed).
narrative_ontology:measurement(aip_nearterm_harms_tr_t2014, ai_alignment_priority__nearterm_harms_reading, theater_ratio, 2014, 0.18).
narrative_ontology:measurement_basis(aip_nearterm_harms_tr_t2014, observed).
narrative_ontology:measurement(aip_nearterm_harms_tr_t2016, ai_alignment_priority__nearterm_harms_reading, theater_ratio, 2016, 0.22).
narrative_ontology:measurement_basis(aip_nearterm_harms_tr_t2016, observed).
narrative_ontology:measurement(aip_nearterm_harms_tr_t2018, ai_alignment_priority__nearterm_harms_reading, theater_ratio, 2018, 0.27).
narrative_ontology:measurement_basis(aip_nearterm_harms_tr_t2018, observed).
narrative_ontology:measurement(aip_nearterm_harms_tr_t2020, ai_alignment_priority__nearterm_harms_reading, theater_ratio, 2020, 0.31).
narrative_ontology:measurement_basis(aip_nearterm_harms_tr_t2020, observed).
narrative_ontology:measurement(aip_nearterm_harms_tr_t2022, ai_alignment_priority__nearterm_harms_reading, theater_ratio, 2022, 0.35).
narrative_ontology:measurement_basis(aip_nearterm_harms_tr_t2022, observed).
narrative_ontology:measurement(aip_nearterm_harms_tr_t2025, ai_alignment_priority__nearterm_harms_reading, theater_ratio, 2025, 0.38).
narrative_ontology:measurement_basis(aip_nearterm_harms_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(aip_nearterm_harms_be_t2012, ai_alignment_priority__nearterm_harms_reading, base_extractiveness, 2012, 0.45).
narrative_ontology:measurement_basis(aip_nearterm_harms_be_t2012, observed).
narrative_ontology:measurement(aip_nearterm_harms_be_t2014, ai_alignment_priority__nearterm_harms_reading, base_extractiveness, 2014, 0.52).
narrative_ontology:measurement_basis(aip_nearterm_harms_be_t2014, observed).
narrative_ontology:measurement(aip_nearterm_harms_be_t2016, ai_alignment_priority__nearterm_harms_reading, base_extractiveness, 2016, 0.58).
narrative_ontology:measurement_basis(aip_nearterm_harms_be_t2016, observed).
narrative_ontology:measurement(aip_nearterm_harms_be_t2018, ai_alignment_priority__nearterm_harms_reading, base_extractiveness, 2018, 0.66).
narrative_ontology:measurement_basis(aip_nearterm_harms_be_t2018, observed).
narrative_ontology:measurement(aip_nearterm_harms_be_t2020, ai_alignment_priority__nearterm_harms_reading, base_extractiveness, 2020, 0.72).
narrative_ontology:measurement_basis(aip_nearterm_harms_be_t2020, observed).
narrative_ontology:measurement(aip_nearterm_harms_be_t2022, ai_alignment_priority__nearterm_harms_reading, base_extractiveness, 2022, 0.77).
narrative_ontology:measurement_basis(aip_nearterm_harms_be_t2022, observed).
narrative_ontology:measurement(aip_nearterm_harms_be_t2025, ai_alignment_priority__nearterm_harms_reading, base_extractiveness, 2025, 0.8).
narrative_ontology:measurement_basis(aip_nearterm_harms_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(aip_nearterm_harms_su_t2012, ai_alignment_priority__nearterm_harms_reading, suppression_requirement, 2012, 0.35).
narrative_ontology:measurement_basis(aip_nearterm_harms_su_t2012, observed).
narrative_ontology:measurement(aip_nearterm_harms_su_t2014, ai_alignment_priority__nearterm_harms_reading, suppression_requirement, 2014, 0.41).
narrative_ontology:measurement_basis(aip_nearterm_harms_su_t2014, observed).
narrative_ontology:measurement(aip_nearterm_harms_su_t2016, ai_alignment_priority__nearterm_harms_reading, suppression_requirement, 2016, 0.47).
narrative_ontology:measurement_basis(aip_nearterm_harms_su_t2016, observed).
narrative_ontology:measurement(aip_nearterm_harms_su_t2018, ai_alignment_priority__nearterm_harms_reading, suppression_requirement, 2018, 0.53).
narrative_ontology:measurement_basis(aip_nearterm_harms_su_t2018, observed).
narrative_ontology:measurement(aip_nearterm_harms_su_t2020, ai_alignment_priority__nearterm_harms_reading, suppression_requirement, 2020, 0.58).
narrative_ontology:measurement_basis(aip_nearterm_harms_su_t2020, observed).
narrative_ontology:measurement(aip_nearterm_harms_su_t2022, ai_alignment_priority__nearterm_harms_reading, suppression_requirement, 2022, 0.61).
narrative_ontology:measurement_basis(aip_nearterm_harms_su_t2022, observed).
narrative_ontology:measurement(aip_nearterm_harms_su_t2025, ai_alignment_priority__nearterm_harms_reading, suppression_requirement, 2025, 0.63).
narrative_ontology:measurement_basis(aip_nearterm_harms_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_alignment_priority__nearterm_harms_reading, resource_allocation).
narrative_ontology:affects_constraint(ai_alignment_priority__nearterm_harms_reading, ai_alignment_priority__existential_risk_reading).
narrative_ontology:affects_constraint(ai_alignment_priority__nearterm_harms_reading, ai_alignment_priority__integrated_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'AI alignment' decomposes, per the epsilon-invariance principle, into three structurally distinct priority regimes that cannot share one story — measuring the arrangement at deployed-system audit sites yields high epsilon with a victim set of present marginalized populations (this file), measuring it at training-run oversight sites yields the existential_risk_reading's profile, and the integrated_reading weights both referent sites. The existential_risk_reading is upstream: institutionally established first, it is cited as evidence that present-harm work is premature or a distraction, which is precisely the legitimating move this reading identifies as the extraction mechanism. This reading is downstream-contested and exerts structural pressure back on the integrated reading, which must now incorporate present harms as non-negotiable to remain credible.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
