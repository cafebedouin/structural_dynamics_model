% ============================================================================
% CONSTRAINT STORY: ai_risk_governance_priority__near_term_harms_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_risk_governance_priority__near_term_harms_reading, []).

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
 *   constraint_id: ai_risk_governance_priority__near_term_harms_reading
 *   human_readable: Near-Term Harms Reading of AI Risk Governance Priority
 *   domain: technology_governance/economic/political
 *
 * SUMMARY:
 *   This story instantiates the near_term_harms_reading of the contested
 *   kernel ai_risk_governance_priority: the claim that AI risk governance
 *   must prioritize mitigating demonstrated present harms — algorithmic
 *   discrimination, misinformation, labor displacement, surveillance —
 *   affecting marginalized populations now. Per the kernel-reading epsilon
 *   referent rule, epsilon's referent is the standing arrangement under
 *   contest — the current AI-governance priority allocation, in which
 *   catastrophic-risk framing commands the funding, staffing, and agenda
 *   space while present-deployment harms remain lightly regulated — assessed
 *   by THIS reading's own lights, which price that arrangement as heavily
 *   costly to the populations bearing unmitigated harms and favorable to the
 *   developers the diverted attention shelters. The reading's endorsed
 *   alternative (fairness audits, bias mitigation, present-system regulatory
 *   frameworks) is the counterfactual, never the measured arrangement, and
 *   contributes nothing to the authored epsilon. The reading's differential
 *   assessment across harm classes (high cost attributed to present
 *   deployment harms, negligible to speculative superintelligence) lives in
 *   the commentary and omegas; the story itself carries ONE stable epsilon
 *   per the epsilon-invariance principle. Family decomposition: the
 *   colloquial label 'AI risk governance priorities' decomposes into three
 *   structurally distinct constraint stories linked via network edges — this
 *   member, existential_risk_reading (disjoint victim set:
 *   humanity-at-large/future persons; high epsilon attributed to unmitigated
 *   catastrophic risk instead), and bridge_reading (refuses the
 *   decomposition; authors intermediate epsilon over both). The claim/metric
 *   gap is deliberate and unreconciled: the constraint is CLAIMED as
 *   tangled_rope from the authoring seat, and the authored metrics
 *   independently describe enforced, substantially costly, actively
 *   maintained operation — the engine measures the divergence.
 *
 * KEY AGENTS:
 *   - frontier_model_developers: Primary beneficiary (institutional/arbitrage) — collects deferred present-harm compliance and co-sets the priority agenda
 *   - xrisk_research_funding_institutions: Secondary beneficiary (powerful/mobile) — captures the research-funding stream the current priority ordering sustains
 *   - marginalized_groups_facing_algorithmic_discrimination: Primary target (powerless/trapped) — bears automated discrimination across credit, housing, policing, and benefits
 *   - automation_displaced_workers: Primary target (moderate/constrained) — absorbs income loss ahead of any transition support
 *   - global_south_populations: Primary target (powerless/trapped) — supplies low-wage data and moderation labor and absorbs exported surveillance
 *   - ai_regulatory_and_standards_bodies: Agenda setter (institutional/constrained) — administers which risks are evaluated and which obligations bind
 *   - present_harms_civil_society_advocates: Excluded voice (moderate/constrained) — documents present harms with little seat in priority-setting venues
 *   - ai_end_users: Near-symmetric party (moderate/mobile) — gains from deployment while absorbing diffuse misinformation and privacy costs
 *   - independent_algorithmic_auditors: Analytical observer (analytical/national) — produces the evidence base from outside the benefiting parties
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_risk_governance_priority__near_term_harms_reading, 0.71).
domain_priors:suppression_score(ai_risk_governance_priority__near_term_harms_reading, 0.62).
domain_priors:theater_ratio(ai_risk_governance_priority__near_term_harms_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_risk_governance_priority__near_term_harms_reading, extractiveness, 0.71).
narrative_ontology:constraint_metric(ai_risk_governance_priority__near_term_harms_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(ai_risk_governance_priority__near_term_harms_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_risk_governance_priority__near_term_harms_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(ai_risk_governance_priority__near_term_harms_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_risk_governance_priority__near_term_harms_reading, tangled_rope).
narrative_ontology:human_readable(ai_risk_governance_priority__near_term_harms_reading, "Near-Term Harms Reading of AI Risk Governance Priority").
narrative_ontology:topic_domain(ai_risk_governance_priority__near_term_harms_reading, "technology_governance/economic/political").

domain_priors:requires_active_enforcement(ai_risk_governance_priority__near_term_harms_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_risk_governance_priority__near_term_harms_reading, '33a07d9d-cc3f-4e6c-a493-b954e7504ce3').
narrative_ontology:cs_kernel_codification('33a07d9d-cc3f-4e6c-a493-b954e7504ce3', distributed).
narrative_ontology:cs_authority_grounding('33a07d9d-cc3f-4e6c-a493-b954e7504ce3', distributed).
narrative_ontology:cs_reading_relation('33a07d9d-cc3f-4e6c-a493-b954e7504ce3', ai_risk_governance_priority__existential_risk_reading, coexists_with).
narrative_ontology:cs_reading_relation('33a07d9d-cc3f-4e6c-a493-b954e7504ce3', ai_risk_governance_priority__bridge_reading, influences).
narrative_ontology:cs_axiom('33a07d9d-cc3f-4e6c-a493-b954e7504ce3', foundational, present_persons_hold_first_claim_on_protection).
narrative_ontology:cs_axiom_status(present_persons_hold_first_claim_on_protection, holdable).
narrative_ontology:cs_axiom_grounding('33a07d9d-cc3f-4e6c-a493-b954e7504ce3', present_persons_hold_first_claim_on_protection, deontological).
narrative_ontology:cs_axiom('33a07d9d-cc3f-4e6c-a493-b954e7504ce3', secondary, binding_rules_require_demonstrated_harm_evidence).
narrative_ontology:cs_axiom_status(binding_rules_require_demonstrated_harm_evidence, holdable).
narrative_ontology:cs_axiom_grounding('33a07d9d-cc3f-4e6c-a493-b954e7504ce3', binding_rules_require_demonstrated_harm_evidence, empirically_contingent).
narrative_ontology:cs_reference_frame('33a07d9d-cc3f-4e6c-a493-b954e7504ce3', demonstrated_harm_priority_framework).
narrative_ontology:cs_drift_state('33a07d9d-cc3f-4e6c-a493-b954e7504ce3', contemporary_summit_era_governance, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('33a07d9d-cc3f-4e6c-a493-b954e7504ce3', '').
narrative_ontology:cs_kernel_id(ai_risk_governance_priority__near_term_harms_reading, ai_risk_governance_priority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_risk_governance_priority__near_term_harms_reading, frontier_model_developers).
narrative_ontology:constraint_beneficiary(ai_risk_governance_priority__near_term_harms_reading, xrisk_research_funding_institutions).
narrative_ontology:constraint_beneficiary(ai_risk_governance_priority__near_term_harms_reading, ai_end_users).
narrative_ontology:constraint_victim(ai_risk_governance_priority__near_term_harms_reading, marginalized_groups_facing_algorithmic_discrimination).
narrative_ontology:constraint_victim(ai_risk_governance_priority__near_term_harms_reading, automation_displaced_workers).
narrative_ontology:constraint_victim(ai_risk_governance_priority__near_term_harms_reading, global_south_populations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(ai_risk_governance_priority__near_term_harms_reading, ai_end_users).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Build and deploy large general-purpose models worldwide. Fund and staff the safety-research and policy ecosystem centered on catastrophic-risk scenarios, participate in frontier-model safety institutes and summits, and shape which risks reach governance agendas. Because regulatory attention concentrates on speculative frontier scenarios, binding obligations on present deployment practices — discrimination testing, content provenance, data-worker conditions — remain light, so deployment revenue continues while compliance costs stay deferred. Operations, lobbying, and even regulatory engagement can move across jurisdictions.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__near_term_harms_reading, frontier_model_developers, beneficiary,
    institutional, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(ai_risk_governance_priority__near_term_harms_reading, frontier_model_developers, agenda_setter).

% Philanthropic funders, university centers, and fellowship programs financing research and talent pipelines oriented to catastrophic and superintelligence scenarios. Grant portfolios, hiring, and convening power all presuppose that frame staying central; staff careers and organizational missions are built around it. Redirecting portfolios toward present-deployment harms would strand accumulated expertise and prestige, so portfolio change is slow even as evidence accumulates elsewhere.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__near_term_harms_reading, xrisk_research_funding_institutions, beneficiary,
    powerful, generational, mobile, global).

% Live under automated decisions in credit scoring, tenant screening, hiring filters, predictive policing, and benefits administration that reproduce historical disparities at scale. Cannot opt out: these systems gate housing, employment, mobility, and services. Local organizing capacity exists but rarely reaches the venues where research agendas and regulatory priorities are set.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__near_term_harms_reading, marginalized_groups_facing_algorithmic_discrimination, payer,
    powerless, immediate, trapped, global).

% Writers, illustrators, translators, translators, customer-service staff, and warehouse and driving workers whose tasks are absorbed by deployed models. Income falls faster than retraining or transition support arrives. Sector-level organizing — creative-industry strikes, platform-worker campaigns — has won some protections but covers a minority of exposed occupations; moving to unaffected work often means downward mobility.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__near_term_harms_reading, automation_displaced_workers, payer,
    moderate, biographical, constrained, global).

% Perform outsourced data annotation and content moderation at low wages under hazardous conditions, host data-center and e-waste burdens, and are subjected to exported surveillance and experimentation tools under thin local regulation. Hold the least representation in governance forums, which concentrate in North American and European capitals, and have the fewest realistic exits from annotation-labor markets that are frequently the available work.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__near_term_harms_reading, global_south_populations, payer,
    powerless, immediate, trapped, global).

% Nonprofit researchers and campaign organizations documenting discriminatory deployments, moderating-work conditions, and displacement effects, and pressing for audits, liability rules, and worker protections. Publish findings and testify publicly but hold few seats in frontier-safety summits, safety-institute advisory boards, and closed consultation processes where priority frameworks are drafted; several operate on grants sized far below the safety-research funding streams.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__near_term_harms_reading, present_harms_civil_society_advocates, excluded,
    moderate, biographical, constrained, continental).

% National AI safety institutes, standards agencies, and multilateral processes deciding which risks get evaluated, which obligations bind, and which harms remain voluntary-disclosure matters. Staffed and advised heavily from the catastrophic-risk research community; present-deployment harm files exist but rank below frontier-evaluation work in resourcing and leadership attention.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__near_term_harms_reading, ai_regulatory_and_standards_bodies, agenda_setter,
    institutional, generational, constrained, continental).

% Academic labs and commercial audit firms measuring bias, misinformation spread, and displacement effects in deployed systems. Produce the evidence base the present-harms case rests on; findings circulate in journals and press coverage but convert into binding rules only sporadically.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__near_term_harms_reading, independent_algorithmic_auditors, observer,
    analytical, biographical, analytical, national).

% Consumers and businesses using deployed AI assistants, generators, and recommendation systems. Gain productivity, access, and convenience from fast deployment under light present-harm obligations; also absorb diffuse costs as misinformation exposure, fraud tooling, and privacy erosion spread. Can switch products individually but have little organized voice on deployment standards.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__near_term_harms_reading, ai_end_users, beneficiary,
    moderate, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(ai_risk_governance_priority__near_term_harms_reading, ai_end_users, payer).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ai_risk_governance_priority__near_term_harms_reading, frontier_model_developers).
narrative_ontology:fixing_cost_class(ai_risk_governance_priority__near_term_harms_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves a real collective-action problem: competing developers racing on general-purpose capabilities generate cross-border safety externalities no single actor fully internalizes. Shared model evaluations, incident reporting, red-team disclosure norms, and safety-institute coordination let rivals pool information about dangerous capabilities without surrendering competitive advantage on everything.
% TRANSFER_FUNCTION: Moves governance attention, research funding, and regulatory drafting capacity toward speculative frontier and catastrophic scenarios and away from demonstrated present harms; correspondingly moves the unpriced costs of deployment — discriminatory decisions, displacement income losses, surveillance exposure, low-wage hazardous data work — onto marginalized populations and displaced workers, while deferring compliance costs for developers.
% ABSENT_VOICES: Global South annotation and content-moderation workers, communities living under predictive policing and benefits algorithms, and displaced creative and service workers are largely absent from the rooms where priority frameworks are drafted — frontier summits, safety-institute boards, closed regulatory consultations. Their objections arrive secondhand through civil-society testimony, if at all; the apparent unanimity of expert consensus on priority ordering reflects who was convened, not agreement among those bearing the harms.
% DISAPPEARANCE_RATIONALE: If the present allocation vanished overnight, funding and regulatory capacity would reflow toward audits, liability rules, worker-transition programs, and content-provenance mandates; developer compliance costs would rise immediately; catastrophic-risk research would shrink toward grant scale; and deployment practices carrying discrimination and displacement exposure would change within product cycles — the surrounding governance economy reorganizes around it.
% FOUNDING_PROBLEM: Frontier capability growth began outrunning existing oversight tools faster than deployment-harm regulation matured; governance institutions were assembled to give states a handle on loss-of-control and strategic-race dangers from increasingly capable general-purpose models.
% FOUNDING_PROBLEM_CORROBORATION: Split, and partly self-serving: catastrophic-risk researchers and the institutes they staff attest the founding problem is live, but they sit inside the arrangement's benefiting funding stream. Independent machine-learning academics dispute loss-of-control imminence and point to demonstrated present harms as the evidenced problem; civil-society auditors and affected-worker organizations corroborate the shifted-function reading from outside the benefiting parties. No source wholly outside every benefiting party attests the original framing's continued primacy — that absence is itself signal.
narrative_ontology:disappearance_verdict(ai_risk_governance_priority__near_term_harms_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_risk_governance_priority__near_term_harms_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_risk_governance_priority__near_term_harms_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(ai_risk_governance_priority__near_term_harms_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_risk_governance_priority__near_term_harms_reading, 0.71, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_risk_governance_priority__near_term_harms_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_risk_governance_priority__near_term_harms_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ai_risk_governance_priority__near_term_harms_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.71: present harms are demonstrated at scale (documented discrimination in lending, hiring, and policing; measurable displacement income loss; hazardous low-wage data work) while the mitigation share of governance capacity stays small — priced by this reading's lights against the standing allocation. Suppression 0.62 is authored as the RAW structural property it is — unscaled by power or scope; only extractiveness is context-scaled in the engine's computation. Roughly two-thirds of the measured suppression is structural (agenda control over summits and advisory boards, funding asymmetry between safety-research and accountability-research streams, preemption politics against sub-national AI regulation); roughly one-third is internalized (ethics-researcher self-censorship amid career risk, affected-community disengagement after repeated non-response) — the split is carried into the suppression_internalized_share omega. Theater 0.48: ethics boards dissolved after inconvenient findings, voluntary pledge regimes, audits without consequence pathways — nearly half of present-harms governance activity is performative, but real audit infrastructure and the EU AI Act's present-harms chapters keep it below half. Accessibility_collapse 0.45: alternatives persist (litigation routes, state-level statutes, alternative framings survive publicly) but collapse partially under resource asymmetry. Resistance 0.60: creative-industry strikes, content-moderator unionization wins, discrimination lawsuits, sustained academic critique. The measurement series run on ONE shared time grid (every tracked metric authored at all eight points) so no metric row borrows an end-state value; trajectories are monotone rather than cyclical, with a 2022-2023 inflection where deployment scale-up and summit-era institutionalization accelerated all three series together. Coalition check: the three payer groups hold latent coalition power — witnessed wins (strike-won protections, moderator wage increases) prove it — but geography, language, and sector fragmentation keep the coalition from agenda power, which is why powerless-class victims remain individually tractable targets.
 *
 * PERSPECTIVAL GAP:
 *   Seats compute differently from the same structure. From the frontier-developer seat the arrangement is coordination it finances and staffs — shared evals, incident channels, safety institutes are goods it helped build, and the priority ordering looks like prudence. From the trapped payer seats the same structure operates as maintained denial: the harms are documented, the remedies known, the agenda held elsewhere. The regulator seat sits between — administering a frame its own staffing pipeline reproduces. Same-level actor dynamics: frontier_model_developers and xrisk_research_funding_institutions occupy adjacent institutional/powerful bands yet derive different directionalities through exit structure (arbitrage-grade jurisdictional mobility versus mobile-but-prestige-bound portfolios); the two trapped victim groups share near-full-target directionality while receiving different transfers (decision harm versus labor extraction). Identity-lock dynamics concentrate in the funding institutions: the organization has become its function — portfolios, hiring, and convening power presuppose the catastrophic-risk frame — so institutional identity, not just interest, maintains the allocation; if that frame broke, reallocation would be swift, which is why maintenance effort concentrates there. Advocates show milder professional path-dependence rather than full identity fusion.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared structure drives the derivation without overrides. frontier_model_developers: declared beneficiary with arbitrage-grade exit — derivation places them nearest the beneficiary end (subsidy-side d), correctly capturing that deferred compliance dominates their relationship to the arrangement. xrisk_research_funding_institutions: beneficiary with mobile exit — low d, slightly above the developers because portfolio mobility is real but prestige lock-in dampens it. ai_end_users: beneficiary with secondary payer — derivation lands them near symmetric, matching the diffuse benefit/cost mix. The three victim groups: trapped or constrained exit with full-cost declarations push them toward the full-target end; global_south_populations sit furthest out because global scope amplifies effective extraction on verification-resistant harm channels (outsourced moderation conditions, exported surveillance). ai_regulatory_and_standards_bodies: agenda-setters who administer without collecting — derivation yields a mid-low d, reflecting administrative capture pressure without direct receipt. present_harms_civil_society_advocates: no formal beneficiary/victim declaration, but structural proximity to the harmed populations places their derived d in the upper-middle range. No directionality_overrides were needed: every deviation a hand-tuned override would patch is already produced by the declared exit and role data.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope claim guards both misclassification directions. Reading the arrangement as pure snare erases the genuine coordination goods it delivers — shared capability evaluations, incident-reporting channels, and safety-institute coordination solve real collective-action problems no competitor internalizes alone; even this reading, which disputes the allocation, concedes the coordination function is real. Reading it as pure rope ignores the enforced asymmetry the same structure runs: no plausible seat makes the trapped victim groups net beneficiaries, and the arrangement persists through active agenda control rather than participant preference. Mandatrophy is NOT declared: founding_problem_status is contested, not dead — the founding problem (loss-of-control and race-dynamics risk) retains disputants on both sides, so the pathology is allocation skew under a live-but-disputed mandate, not obsolescence. The piton signature fails twice: theater_ratio 0.48 stays below the performative-dominance range, and a profit-collecting concentrated beneficiary exists (frontier_model_developers), which is capture-shaped rather than inertia-shaped. Receipt-surface cross-check: gain_flow names the developer seat affirmatively (deferred present-harm compliance is worth more than the entire accountability-research grant stream, so gains demonstrably land there, making 'diffuse' false), and fixing_cost is prohibitive for any single agenda-setter because overcoming coordinated incumbent resistance — lobbying footprint, arbitrage threats, staffing dependence — exceeds what any one body bears; coalition-level fixes would be cheaper, but no such coalition currently holds agenda power. The mismatch consumer reads founding_problem_status=contested x disappearance_verdict=world_rearranges: no dead-mandate flag fires, consistent with a captured-allocation rather than a zombie-institution diagnosis.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_membership,
    'This constraint is the near_term_harms_reading of kernel ai_risk_governance_priority — what structurally changes if a sibling reading prevails?',
    'Track which reading captures binding instruments (statutes, institute mandates, summit communiques): if catastrophic-risk language dominates binding text, existential_risk_reading prevails; if unified dual-track frameworks are enacted, bridge_reading prevails.',
    'If existential_risk_reading prevails, the victim set swaps: present marginalized populations leave the protected set and hypothetical future persons enter it, and the developer-seat beneficiary declaration inverts. If bridge_reading prevails, this reading''s exclusive-priority claim dissolves into an entanglement framework and its standalone classification lapses.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_membership, conceptual, 'Committer-frame record: this file is one reading of the AI-governance-priority kernel; sibling prevalence would restructure the victim and beneficiary sets.').

omega_variable(
    evidentiary_standard_disagreement,
    'Is the kernel contest located precisely in the evidentiary standard that qualifies a harm for governance priority — demonstrated-and-present versus forecast-catastrophic?',
    'An adjudicated evidentiary rubric for governance priority (what demonstration threshold binds regulation), or decisive empirical settlement of catastrophic-risk probability estimates accepted across the readings.',
    'If forecast harms qualify for binding priority, this reading''s exclusivity fails and effective extraction redistributes toward speculative scenarios; if demonstration remains the requirement, speculative-scenario spending registers as diversion and this reading''s high epsilon holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(evidentiary_standard_disagreement, conceptual, 'Locates the inter-reading disagreement in the harm-evidentiary standard rather than in values alone.').

omega_variable(
    diversion_zero_sum_ambiguity,
    'Is underinvestment in present-harm mitigation actually caused by diversion toward catastrophic-risk framing, or would underinvestment persist regardless because attention and budgets are not zero-sum?',
    'Compare present-harms regulatory output and budget shares across jurisdictions and periods with differing catastrophic-risk salience (pre/post-2023, across national contexts), controlling for total governance spend.',
    'If diversion is real, the developer-seat receipt attribution firms up and effective extraction rises; if spending is additive neglect, the arrangement looks closer to inertial under-provision than capture and epsilon falls.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(diversion_zero_sum_ambiguity, empirical, 'Whether the catastrophic-risk framing diverts finite governance capacity or coexists with independent neglect.').

omega_variable(
    suppression_internalized_share,
    'Of the measured suppression, how much is structural (agenda control, funding gates, preemption politics) versus internalized (researcher self-censorship, affected-community resignation)?',
    'Post-barrier trajectory: if present-harms advocacy volume and community engagement expand quickly where structural barriers are lifted (new funding lines, open consultation seats), the residual gap marks the internalized share.',
    'If a large share is internalized, lifting formal exclusion will under-deliver participation and the arrangement''s suppressive force will persist beyond its enforcement machinery; classification consequences follow the structural remainder.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_internalized_share, empirical, 'Structural versus internalized components of the measured suppression.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_risk_governance_priority__near_term_harms_reading, 2019, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(near_term_harms_tr_t2019, ai_risk_governance_priority__near_term_harms_reading, theater_ratio, 2019, 0.28).
narrative_ontology:measurement(near_term_harms_tr_t2020, ai_risk_governance_priority__near_term_harms_reading, theater_ratio, 2020, 0.31).
narrative_ontology:measurement(near_term_harms_tr_t2021, ai_risk_governance_priority__near_term_harms_reading, theater_ratio, 2021, 0.33).
narrative_ontology:measurement(near_term_harms_tr_t2022, ai_risk_governance_priority__near_term_harms_reading, theater_ratio, 2022, 0.36).
narrative_ontology:measurement(near_term_harms_tr_t2023, ai_risk_governance_priority__near_term_harms_reading, theater_ratio, 2023, 0.41).
narrative_ontology:measurement(near_term_harms_tr_t2024, ai_risk_governance_priority__near_term_harms_reading, theater_ratio, 2024, 0.44).
narrative_ontology:measurement(near_term_harms_tr_t2025, ai_risk_governance_priority__near_term_harms_reading, theater_ratio, 2025, 0.46).
narrative_ontology:measurement(near_term_harms_tr_t2026, ai_risk_governance_priority__near_term_harms_reading, theater_ratio, 2026, 0.48).

% Extraction over time
narrative_ontology:measurement(near_term_harms_be_t2019, ai_risk_governance_priority__near_term_harms_reading, base_extractiveness, 2019, 0.54).
narrative_ontology:measurement(near_term_harms_be_t2020, ai_risk_governance_priority__near_term_harms_reading, base_extractiveness, 2020, 0.56).
narrative_ontology:measurement(near_term_harms_be_t2021, ai_risk_governance_priority__near_term_harms_reading, base_extractiveness, 2021, 0.58).
narrative_ontology:measurement(near_term_harms_be_t2022, ai_risk_governance_priority__near_term_harms_reading, base_extractiveness, 2022, 0.62).
narrative_ontology:measurement(near_term_harms_be_t2023, ai_risk_governance_priority__near_term_harms_reading, base_extractiveness, 2023, 0.66).
narrative_ontology:measurement(near_term_harms_be_t2024, ai_risk_governance_priority__near_term_harms_reading, base_extractiveness, 2024, 0.68).
narrative_ontology:measurement(near_term_harms_be_t2025, ai_risk_governance_priority__near_term_harms_reading, base_extractiveness, 2025, 0.7).
narrative_ontology:measurement(near_term_harms_be_t2026, ai_risk_governance_priority__near_term_harms_reading, base_extractiveness, 2026, 0.71).

% Suppression requirement over time
narrative_ontology:measurement(near_term_harms_su_t2019, ai_risk_governance_priority__near_term_harms_reading, suppression_requirement, 2019, 0.36).
narrative_ontology:measurement(near_term_harms_su_t2020, ai_risk_governance_priority__near_term_harms_reading, suppression_requirement, 2020, 0.39).
narrative_ontology:measurement(near_term_harms_su_t2021, ai_risk_governance_priority__near_term_harms_reading, suppression_requirement, 2021, 0.42).
narrative_ontology:measurement(near_term_harms_su_t2022, ai_risk_governance_priority__near_term_harms_reading, suppression_requirement, 2022, 0.47).
narrative_ontology:measurement(near_term_harms_su_t2023, ai_risk_governance_priority__near_term_harms_reading, suppression_requirement, 2023, 0.54).
narrative_ontology:measurement(near_term_harms_su_t2024, ai_risk_governance_priority__near_term_harms_reading, suppression_requirement, 2024, 0.58).
narrative_ontology:measurement(near_term_harms_su_t2025, ai_risk_governance_priority__near_term_harms_reading, suppression_requirement, 2025, 0.6).
narrative_ontology:measurement(near_term_harms_su_t2026, ai_risk_governance_priority__near_term_harms_reading, suppression_requirement, 2026, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_risk_governance_priority__near_term_harms_reading, resource_allocation).
narrative_ontology:affects_constraint(ai_risk_governance_priority__near_term_harms_reading, ai_risk_governance_priority__existential_risk_reading).
narrative_ontology:affects_constraint(ai_risk_governance_priority__near_term_harms_reading, ai_risk_governance_priority__bridge_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition per the epsilon-invariance principle: the colloquial label 'AI risk governance priorities' conflates three structurally distinct priority claims, each with its own epsilon, victim set, and classification. This member (near_term_harms_reading) authors high epsilon for the standing allocation assessed as diverting capacity from demonstrated present harms; the existential_risk_reading member authors high epsilon for unmitigated catastrophic risk instead, with a disjoint victim set (future humanity versus present marginalized populations); bridge_reading declines the decomposition and authors intermediate epsilon over both harm classes. Upstream/downstream structure: present-harm evidence accumulation (audit results, strike settlements, discrimination litigation outcomes) exerts downstream pressure on how the other two readings weight their cases, so edges run from this story to both siblings. Each file keeps exactly one stable epsilon; the inter-reading contest lives in the omega variables, never inside any single story's classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
