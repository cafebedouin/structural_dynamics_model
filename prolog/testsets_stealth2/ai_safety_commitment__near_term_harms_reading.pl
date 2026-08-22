% ============================================================================
% CONSTRAINT STORY: ai_safety_commitment__near_term_harms_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
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
 *   constraint_id: ai_safety_commitment__near_term_harms_reading
 *   human_readable: Near-Term Harms Reading of the AI Safety Commitment
 *   domain: technological/governance
 *
 * SUMMARY:
 *   This story instantiates the near-term-harms reading of the AI-safety
 *   commitment: the claim that safety work means preventing documented,
 *   present-day harms from deployed systems — discriminatory decisions,
 *   algorithmic labor exploitation, synthetic misinformation. Across
 *   2018-2025 (interval units 0-7) this definition became the institutionally
 *   dominant one: corporate responsible-AI programs, a commercial audit
 *   industry, dedicated conference tracks, foundation grant lines, and
 *   regulatory drafts all encode it. The arrangement has a genuine
 *   coordination core — shared harm taxonomies, benchmarks, and incident
 *   reporting that no single victim or lab could build alone — and an
 *   asymmetric underside: affected populations supply the documented
 *   incidents the apparatus runs on while remedies skew symbolic, deploying
 *   companies convert accountability pressure into voluntary-compliance
 *   legitimacy, and speculative-alignment work sits wholly outside the
 *   frame's extraction surface. The claim and the metrics are independent
 *   authored facts: claimed_type states the structure I believe true (hybrid
 *   coordination/extraction requiring active boundary defense); the metrics
 *   describe observed operation without being tuned to any predicted engine
 *   verdict.
 *
 * KEY AGENTS:
 *   - tech_companies: Primary beneficiary and co-agenda-setter (institutional/arbitrage) — converts accountability pressure into voluntary-compliance legitimacy
 *   - marginalized_algorithmic_decision_subjects: Primary target (powerless/trapped) — bears discrimination harms; supplies the documented incidents the apparatus runs on
 *   - gig_platform_workers: Primary target (moderate/constrained) — bears algorithmic-management exploitation
 *   - misinformation_affected_publics: Primary target (powerless/trapped) — bears synthetic-media information harms
 *   - civil_society_advocacy_orgs: Agenda-setter and incidental beneficiary (organized/identity_locked) — champions the definition, draws funding through it
 *   - philanthropic_ai_funders: Agenda-setter (powerful/mobile) — allocates grant money along the definition's boundaries
 *   - ai_ethics_researchers: Dual-positioned payer/beneficiary (moderate/constrained) — staffs the apparatus; bears censorship and career risk
 *   - responsible_ai_audit_industry: Secondary beneficiary (organized/mobile) — collects compliance revenue
 *   - existential_risk_researchers: Excluded voice (organized/identity_locked) — would contest the definitional boundary
 *   - standards_and_regulatory_bodies: Analytical observer (institutional/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_safety_commitment__near_term_harms_reading, 0.62).
domain_priors:suppression_score(ai_safety_commitment__near_term_harms_reading, 0.58).
domain_priors:theater_ratio(ai_safety_commitment__near_term_harms_reading, 0.47).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_safety_commitment__near_term_harms_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(ai_safety_commitment__near_term_harms_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(ai_safety_commitment__near_term_harms_reading, theater_ratio, 0.47).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_safety_commitment__near_term_harms_reading, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(ai_safety_commitment__near_term_harms_reading, resistance, 0.57).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_safety_commitment__near_term_harms_reading, tangled_rope).
narrative_ontology:human_readable(ai_safety_commitment__near_term_harms_reading, "Near-Term Harms Reading of the AI Safety Commitment").
narrative_ontology:topic_domain(ai_safety_commitment__near_term_harms_reading, "technological/governance").

domain_priors:requires_active_enforcement(ai_safety_commitment__near_term_harms_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_safety_commitment__near_term_harms_reading, 'f7b4865d-7399-4e79-ba6a-32163217e4ab').
narrative_ontology:cs_kernel_codification('f7b4865d-7399-4e79-ba6a-32163217e4ab', distributed).
narrative_ontology:cs_authority_grounding('f7b4865d-7399-4e79-ba6a-32163217e4ab', expertise).
narrative_ontology:cs_interpretation_layer_present('f7b4865d-7399-4e79-ba6a-32163217e4ab').
narrative_ontology:cs_reading_relation('f7b4865d-7399-4e79-ba6a-32163217e4ab', ai_safety_commitment__existential_risk_reading, coexists_with).
narrative_ontology:cs_reading_relation('f7b4865d-7399-4e79-ba6a-32163217e4ab', ai_safety_commitment__dual_priority_reading, influences).
narrative_ontology:cs_axiom('f7b4865d-7399-4e79-ba6a-32163217e4ab', foundational, present_harmed_persons_hold_prior_claim).
narrative_ontology:cs_axiom_status(present_harmed_persons_hold_prior_claim, holdable).
narrative_ontology:cs_axiom_grounding('f7b4865d-7399-4e79-ba6a-32163217e4ab', present_harmed_persons_hold_prior_claim, deontological).
narrative_ontology:cs_axiom('f7b4865d-7399-4e79-ba6a-32163217e4ab', secondary, evidence_threshold_gates_safety_scope).
narrative_ontology:cs_axiom_status(evidence_threshold_gates_safety_scope, holdable).
narrative_ontology:cs_axiom_grounding('f7b4865d-7399-4e79-ba6a-32163217e4ab', evidence_threshold_gates_safety_scope, empirically_contingent).
narrative_ontology:cs_reference_frame('f7b4865d-7399-4e79-ba6a-32163217e4ab', documented_harm_prevention_standard).
narrative_ontology:cs_drift_state('f7b4865d-7399-4e79-ba6a-32163217e4ab', contemporary_post_frontier_scaling_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('f7b4865d-7399-4e79-ba6a-32163217e4ab', '').
narrative_ontology:cs_kernel_id(ai_safety_commitment__near_term_harms_reading, ai_safety_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_safety_commitment__near_term_harms_reading, tech_companies).
narrative_ontology:constraint_beneficiary(ai_safety_commitment__near_term_harms_reading, responsible_ai_audit_industry).
narrative_ontology:constraint_beneficiary(ai_safety_commitment__near_term_harms_reading, civil_society_advocacy_orgs).
narrative_ontology:constraint_victim(ai_safety_commitment__near_term_harms_reading, marginalized_algorithmic_decision_subjects).
narrative_ontology:constraint_victim(ai_safety_commitment__near_term_harms_reading, gig_platform_workers).
narrative_ontology:constraint_victim(ai_safety_commitment__near_term_harms_reading, misinformation_affected_publics).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(ai_safety_commitment__near_term_harms_reading, ai_ethics_researchers).
narrative_ontology:constraint_victim(ai_safety_commitment__near_term_harms_reading, ai_ethics_researchers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operate the deployed systems whose outputs produce the documented harms. Run internal responsible-AI teams implementing the prevailing definition of safety work: bias evaluations, content-policy enforcement, model documentation, red-team exercises. Publish safety reports and join voluntary pledges. Gain reputational standing and a defensible position against mandatory rules; bear audit and governance costs that are small relative to avoided liability and preserved deployment freedom. Can reframe, rebrand, or relocate governance programs across jurisdictions at will.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__near_term_harms_reading, tech_companies, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(ai_safety_commitment__near_term_harms_reading, tech_companies, agenda_setter).

% Firms and consultancies selling bias audits, conformity assessments, and governance tooling to deployers. Revenue scales with the volume of compliance activity the definition generates. Portable across clients and jurisdictions.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__near_term_harms_reading, responsible_ai_audit_industry, beneficiary,
    organized, biographical, mobile, global).

% Nonprofits and research groups that campaigned to place documented harms at the center of what AI safety means. Receive foundation grants earmarked for responsible-AI and algorithmic-accountability work; staff careers and organizational missions are bound up with the frame. Push companies and regulators for stronger transparency, auditing, and labor protections, and defend the definition against dilution.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__near_term_harms_reading, civil_society_advocacy_orgs, agenda_setter,
    organized, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(ai_safety_commitment__near_term_harms_reading, civil_society_advocacy_orgs, beneficiary).

% Foundations and donor collaboratives deciding which safety research and advocacy gets funded. Their grant lines encode the definition: portfolios weighted toward harm measurement, auditing, and governance, with little allocated outside it. Can redirect portfolios on board cycles.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__near_term_harms_reading, philanthropic_ai_funders, agenda_setter,
    powerful, generational, mobile, global).

% People subject to automated decisions in lending, housing, hiring, benefits, and policing who bear discriminatory errors. Cannot opt out of the systems that rank them; learn of harms only after denial or adverse action. Their documented cases supply the evidence base the safety apparatus studies, while remedies arrive slowly or not at all.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__near_term_harms_reading, marginalized_algorithmic_decision_subjects, payer,
    powerless, biographical, trapped, national).

% Workers whose tasks, pay, and continued access to work are governed by algorithmic management systems. Bear wage opacity, unilateral rating penalties, and abrupt account deactivation. Need the income and have limited ability to multi-home or leave platforms; organize where law permits.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__near_term_harms_reading, gig_platform_workers, payer,
    moderate, immediate, constrained, global).

% Communities exposed to synthetic media, coordinated manipulation, and recommendation-amplified falsehoods. Bear persuasion, fraud, and degraded shared information they did not choose and cannot exit; their attention is the medium the harms travel through.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__near_term_harms_reading, misinformation_affected_publics, payer,
    powerless, biographical, trapped, continental).

% Researchers staffing harm-measurement, auditing, and governance work inside companies and universities. Draw salaries, datasets, and publication venues from the apparatus; some have faced reassignment or dismissal after publishing findings unfavorable to their employers. Mobility exists but carries reputational and reference risk.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__near_term_harms_reading, ai_ethics_researchers, payer,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(ai_safety_commitment__near_term_harms_reading, ai_ethics_researchers, beneficiary).

% Researchers focused on catastrophic and extinction-scale outcomes from advanced systems. Their agenda sits outside the prevailing definition's scope; they compete for the same funding, venues, and policy attention and argue the boundary is arbitrary. Mission-committed; few professional exits into the near-term frame.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__near_term_harms_reading, existential_risk_researchers, excluded,
    organized, civilizational, identity_locked, global).

% Standards institutes and agencies translating the definition into frameworks, audit criteria, and draft rules. Take input from all other seats, commission studies, and can convert voluntary practice into binding obligation.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__near_term_harms_reading, standards_and_regulatory_bodies, observer,
    institutional, generational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ai_safety_commitment__near_term_harms_reading, tech_companies).
narrative_ontology:fixing_cost_class(ai_safety_commitment__near_term_harms_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Documented AI harms are dispersed across millions of deployments and technically opaque to individual victims. A shared definition of safety as present-harm prevention lets labs, auditors, regulators, and advocacy groups pool harm taxonomies, benchmarks, incident reporting, and evaluation methods, so mitigation effort accumulates instead of restarting per case.
% TRANSFER_FUNCTION: Moves funding, careers, conference space, and regulatory bandwidth toward harm measurement, auditing, and content-governance work, and away from structural-labor and catastrophic-risk framings. Moves reputational legitimacy to deploying companies that adopt the apparatus voluntarily, and moves the burden of proof for remedy onto affected populations, whose documented cases feed the apparatus without guaranteed correction.
% ABSENT_VOICES: Existential-risk researchers sit outside the room the definition drew; they would contest the boundary that classes their agenda as not-safety. Labor scholars would object that bias framing individualizes what are labor-relations and market-power problems. Affected community members appear as data sources and consultation subjects far more often than as decision-makers with veto power.
% DISAPPEARANCE_RATIONALE: If the definition lost its hold overnight, the compliance industry would lose its mandate, corporate governance programs would lose their legitimating script, conference tracks and grant lines would re-sort around whichever rival definition captured the vacuum, and regulatory drafts keyed to harm documentation would stall or be rewritten. The institutional ecology organized by the definition would visibly reorganize.
% FOUNDING_PROBLEM: Deployed systems were producing documented, measurable harms — discriminatory decisions, exploitative algorithmic management, industrial-scale misinformation — that neither existing civil-rights enforcement nor the field's early catastrophic-risk discourse treated as the safety agenda. Advocates defined AI safety around these harms to force recognition, measurement, and remedy onto the field's agenda.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set: publicly maintained AI-incident databases run by academic consortia, investigative journalism archives, court filings in algorithmic-discrimination litigation, and legislative inquiry transcripts all attest that documented harms continue at scale. Deploying companies — the principal beneficiaries — attest instead that the problem is substantially managed through voluntary programs; that divergence between beneficiary attestation and the external record is itself signal.
narrative_ontology:disappearance_verdict(ai_safety_commitment__near_term_harms_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_safety_commitment__near_term_harms_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_safety_commitment__near_term_harms_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(ai_safety_commitment__near_term_harms_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_safety_commitment__near_term_harms_reading, 0.62, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness 0.62: the referent is the standing arrangement — the institutionalized definition and its compliance apparatus — assessed by this reading's own lights. The harms the reading names remain documented and widespread while the apparatus's remedy output skews symbolic, and the burden falls hardest on those least able to refuse the systems governing them. It is not higher because the apparatus does deliver real measurement infrastructure and some deployment changes. Suppression 0.58 is predominantly structural (funding gates, venue norms, employment consequences for dissenting researchers) with a smaller internalized component (anticipatory self-censorship) — roughly 70/30, informing the suppression-mechanism omega. Theater 0.47: a large share of activity is artifact production (principles pages, model cards, audit reports) whose link to outcome change for affected groups is weak. Accessibility collapse 0.38: alternatives remain workable — structural-labor framing, litigation, and the catastrophic-risk framing all persist; the definition narrows the option space without closing it. Resistance 0.57: sustained contest from the x-risk camp, worker organizing, affected-community activism, and intra-field dissent keeps the boundary actively defended. All three temporal series run on ONE shared grid (annual points 0-7); every tracked metric is authored at every point. Trajectories rise monotonically: enforcement machinery matured (governance headcount, review gates, conformity-assessment demand), capture deepened as voluntary frameworks substituted for binding rules in major jurisdictions, and boundary defense intensified as rival framings gained ground. No oscillation is modeled: incident-pledge-relaxation cycling exists at a shorter wavelength than the annual grid resolves and is noted here rather than fabricated into the series. Coalition check: the payer seats are individually weak but coalition-capable — worker organizations, affected-community litigants, and dissident researchers have repeatedly aligned (data-labor campaigns, whistleblower testimony); combined payer power is the main internal threat to the arrangement's stability, which is why suppression rises alongside extraction.
 *
 * PERSPECTIVAL GAP:
 *   From the tech_companies seat the arrangement is legitimate self-governance it built and funds — a manageable coordination cost that buys license to operate. From the payer seats the same apparatus documents their harms without remediating them — extraction wearing a coordination veneer. From the advocacy seats the definition is a hard-won victory perpetually at risk of dilution — fragile coordination needing constant defense. Inter-institutional dynamics: companies (institutional/arbitrage), funders (powerful/mobile), and regulators (institutional/analytical) occupy the same nominal institutional tier but different exit positions, so identical formal pressure lands as negotiable cost for the first, portfolio choice for the second, and mandate question for the third. Same-level lateral dynamics: civil-society advocacy orgs and existential-risk research orgs hold similar organized power with opposed frames; what differentiates them is identity lock — the advocacy orgs' missions are constituted by this definition, the x-risk orgs' by its negation — so neither can cheaply adopt the other's frame. The engine computes these divergent per-seat classifications from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries derive low directionality: tech_companies are net collectors (avoided binding regulation and reputational subsidy exceed compliance spend), the audit industry collects revenue proportional to compliance volume, and advocacy orgs draw funding and relevance through the frame while paying co-optation costs. Payers derive high directionality: marginalized decision subjects and misinformation-exposed publics are trapped (no exit from ranked or saturated environments), pushing them toward the full-target end; gig workers and ethics researchers are constrained (income and career dependence limit exit) sitting slightly inside. Funders sit near symmetric — they set the agenda but bear little of the arrangement's cost or benefit directly. No directionality overrides are needed: the beneficiary/victim declarations plus exit options reproduce the true structural relationships without correction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — the recognition gap for documented harms — remains live, so mandatrophy_resolved stays false and no sunset applies. The tangled-rope classification guards both error directions: a pure-snare label would erase the real mitigation the apparatus delivers (benchmarks, incident reporting, some deployment changes); a pure-rope label would launder the capture dynamic (symbolic remedy, avoided regulation, exclusion of structural framings). The R5 mismatch consumer reads founding_problem_status=live against disappearance_verdict=world_rearranges — no dead-problem/zombie flag fires. The rising theater_ratio is the series to watch: if audit activity continues decoupling from remediation, the computed type should drift toward snare, and the audit_remediation_ratio omega is the instrument that would date that transition.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This constraint is the near_term_harms_reading of the ai_safety_commitment kernel alone; how would the victim set, beneficiary structure, and extraction profile differ if the existential_risk_reading or dual_priority_reading sibling were instantiated instead?',
    'Author and compile the sibling stories; compare computed per-seat classifications and effective-extraction profiles across the constraint family.',
    'Under the existential-risk sibling the victim set shifts toward speculative humanity-scale harms, deployer compliance burdens invert, and this reading''s high-extraction dimensions (transparency, auditing, labor protection) drop to low extraction; under the dual-priority sibling extraction spreads across both profiles simultaneously.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Committer structure: one reading of a contested kernel; sibling readings would restructure victims and extraction.').

omega_variable(
    accountability_lever_vs_containment,
    'Does institutionalizing the near-term definition operate as an accountability lever that precedes binding regulation, or as a containment strategy that substitutes voluntary compliance for it?',
    'Cross-jurisdiction comparison where the definition fed binding obligations (mandatory bias audits, conformity assessment) versus jurisdictions where voluntary frameworks dominated; track documented-harm rates and deployment changes under each regime.',
    'A lever finding supports the coordination half of the tangled-rope reading and a rising coordination value; a containment finding shifts weight toward snare as theater share grows and validates the beneficiary declaration against the companies'' own framing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(accountability_lever_vs_containment, empirical, 'Whether the definition''s institutional success disciplines deployers or launders legitimacy.').

omega_variable(
    audit_remediation_ratio,
    'What fraction of audit, documentation, and red-teaming activity produces material remediation for affected populations rather than compliance artifacts?',
    'Longitudinal linkage of audit findings to deployment changes and to outcome deltas for the affected groups being measured.',
    'High remediation lowers theater_ratio and strengthens the rope side; low remediation raises theater_ratio, pushing the computed type toward snare and dating a tangled-rope-to-snare transition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(audit_remediation_ratio, empirical, 'Substance-versus-artifact ratio inside the compliance apparatus.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the marginalization of rival framings (structural-labor accounts, catastrophic-risk accounts) inside AI-safety institutions structural — funding gates, venue norms, employment risk — or internalized — anticipatory self-censorship and mission fusion?',
    'Post-exit trajectory of researchers who left safety roles; funding-decision and reviewer audits comparing framed versus reframed proposals.',
    'An internalized component means effective suppression exceeds the structural measure and persists after barrier removal; a purely structural component means reforming gates and venues would release suppressed framings quickly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression of rival framings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_safety_commitment__near_term_harms_reading, 0, 7).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(near_term_harms_tr_t0, ai_safety_commitment__near_term_harms_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(near_term_harms_tr_t1, ai_safety_commitment__near_term_harms_reading, theater_ratio, 1, 0.31).
narrative_ontology:measurement(near_term_harms_tr_t2, ai_safety_commitment__near_term_harms_reading, theater_ratio, 2, 0.34).
narrative_ontology:measurement(near_term_harms_tr_t3, ai_safety_commitment__near_term_harms_reading, theater_ratio, 3, 0.37).
narrative_ontology:measurement(near_term_harms_tr_t4, ai_safety_commitment__near_term_harms_reading, theater_ratio, 4, 0.4).
narrative_ontology:measurement(near_term_harms_tr_t5, ai_safety_commitment__near_term_harms_reading, theater_ratio, 5, 0.43).
narrative_ontology:measurement(near_term_harms_tr_t6, ai_safety_commitment__near_term_harms_reading, theater_ratio, 6, 0.45).
narrative_ontology:measurement(near_term_harms_tr_t7, ai_safety_commitment__near_term_harms_reading, theater_ratio, 7, 0.47).

% Extraction over time
narrative_ontology:measurement(near_term_harms_be_t0, ai_safety_commitment__near_term_harms_reading, base_extractiveness, 0, 0.44).
narrative_ontology:measurement(near_term_harms_be_t1, ai_safety_commitment__near_term_harms_reading, base_extractiveness, 1, 0.47).
narrative_ontology:measurement(near_term_harms_be_t2, ai_safety_commitment__near_term_harms_reading, base_extractiveness, 2, 0.51).
narrative_ontology:measurement(near_term_harms_be_t3, ai_safety_commitment__near_term_harms_reading, base_extractiveness, 3, 0.54).
narrative_ontology:measurement(near_term_harms_be_t4, ai_safety_commitment__near_term_harms_reading, base_extractiveness, 4, 0.57).
narrative_ontology:measurement(near_term_harms_be_t5, ai_safety_commitment__near_term_harms_reading, base_extractiveness, 5, 0.59).
narrative_ontology:measurement(near_term_harms_be_t6, ai_safety_commitment__near_term_harms_reading, base_extractiveness, 6, 0.61).
narrative_ontology:measurement(near_term_harms_be_t7, ai_safety_commitment__near_term_harms_reading, base_extractiveness, 7, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(near_term_harms_su_t0, ai_safety_commitment__near_term_harms_reading, suppression_requirement, 0, 0.36).
narrative_ontology:measurement(near_term_harms_su_t1, ai_safety_commitment__near_term_harms_reading, suppression_requirement, 1, 0.39).
narrative_ontology:measurement(near_term_harms_su_t2, ai_safety_commitment__near_term_harms_reading, suppression_requirement, 2, 0.42).
narrative_ontology:measurement(near_term_harms_su_t3, ai_safety_commitment__near_term_harms_reading, suppression_requirement, 3, 0.45).
narrative_ontology:measurement(near_term_harms_su_t4, ai_safety_commitment__near_term_harms_reading, suppression_requirement, 4, 0.49).
narrative_ontology:measurement(near_term_harms_su_t5, ai_safety_commitment__near_term_harms_reading, suppression_requirement, 5, 0.52).
narrative_ontology:measurement(near_term_harms_su_t6, ai_safety_commitment__near_term_harms_reading, suppression_requirement, 6, 0.55).
narrative_ontology:measurement(near_term_harms_su_t7, ai_safety_commitment__near_term_harms_reading, suppression_requirement, 7, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_safety_commitment__near_term_harms_reading, information_standard).
narrative_ontology:affects_constraint(ai_safety_commitment__near_term_harms_reading, existential_risk_reading).
narrative_ontology:affects_constraint(ai_safety_commitment__near_term_harms_reading, dual_priority_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'AI safety' decomposes into at least three structurally distinct commitments. This file is the near-term-harms member; existential_risk_reading and dual_priority_reading are siblings. Epsilon differs sharply across members: this reading concentrates extraction on transparency, auditing, and labor-protection dimensions while leaving speculative-alignment work unextracted; the existential-risk reading inverts that profile. Upstream/downstream structure runs through funding and venue competition: whichever reading holds institutional dominance conditions the others' operating environment, which is why this (currently dominant) reading declares influence toward the dual-priority sibling and coexistence with the existential-risk sibling.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
