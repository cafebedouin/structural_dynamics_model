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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   human_readable: Alignment-as-Present-Harm-Prevention (Near-Term Justice Reading)
 *   domain: AI governance / algorithmic fairness / technology ethics
 *
 * SUMMARY:
 *   This constraint captures one reading of the contested 'alignment' kernel:
 *   the claim that alignment work should be defined and prioritized around
 *   preventing present, documented, discriminatory and extractive harms from
 *   AI systems already deployed against marginalized populations — rather
 *   than around preventing catastrophic loss of control over future advanced
 *   systems. Under this reading, a substantial research, audit, and
 *   regulatory apparatus has formed around disparate-impact detection and
 *   mitigation. The coordination function is real: without this apparatus,
 *   ongoing discriminatory deployment harms would go largely undetected and
 *   unaddressed. But the apparatus also concentrates funding, prestige, and
 *   market position in ethics-research labs and audit consultancies whose
 *   economic and reputational interests are tied to the present-harms framing
 *   remaining institutionally dominant, while the harmed populations who
 *   justify the framing's priority claim capture comparatively little of the
 *   resource flow once an audit or paper is published. This is a Tangled
 *   Rope: genuine coordination (audits do catch real discriminatory patterns)
 *   coexists with asymmetric extraction (funding and prestige accrue
 *   disproportionately to the auditing apparatus relative to the audited
 *   populations), sustained by active enforcement in the form of civil-rights
 *   and consumer-protection regulatory mandates that privilege this framing's
 *   evidentiary standards.
 *
 * KEY AGENTS:
 *   - ai_ethics_research_labs: agenda_setter/beneficiary — sets audit methodology, draws funding and prestige
 *   - algorithmic_audit_consultancies: beneficiary — sells compliance services contingent on this framing's dominance
 *   - present_vulnerable_populations: beneficiary/payer — the named justification for the framing, exposed between audit cycles
 *   - disabled_applicants_screened_by_automated_systems, racialized_communities_over_policed_by_predictive_tools, elderly_users_denied_automated_benefits_determinations, gig_workers_subject_to_algorithmic_management: payer — the specific documented victim classes
 *   - frontier_ai_labs: excluded — marginalized within this reading's priority-setting venues
 *   - regulatory_agencies: observer/agenda_setter — structurally privileges this framing via statutory mandate
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_alignment_priority__nearterm_harms_reading, 0.68).
domain_priors:suppression_score(ai_alignment_priority__nearterm_harms_reading, 0.52).
domain_priors:theater_ratio(ai_alignment_priority__nearterm_harms_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_alignment_priority__nearterm_harms_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(ai_alignment_priority__nearterm_harms_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(ai_alignment_priority__nearterm_harms_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_alignment_priority__nearterm_harms_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(ai_alignment_priority__nearterm_harms_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_alignment_priority__nearterm_harms_reading, tangled_rope).
narrative_ontology:human_readable(ai_alignment_priority__nearterm_harms_reading, "Alignment-as-Present-Harm-Prevention (Near-Term Justice Reading)").
narrative_ontology:topic_domain(ai_alignment_priority__nearterm_harms_reading, "AI governance / algorithmic fairness / technology ethics").

domain_priors:requires_active_enforcement(ai_alignment_priority__nearterm_harms_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_alignment_priority__nearterm_harms_reading, 'b6e980c8-5202-42ca-9bee-aa655691f151').
narrative_ontology:cs_kernel_codification('b6e980c8-5202-42ca-9bee-aa655691f151', distributed).
narrative_ontology:cs_authority_grounding('b6e980c8-5202-42ca-9bee-aa655691f151', distributed).
narrative_ontology:cs_reading_relation('b6e980c8-5202-42ca-9bee-aa655691f151', ai_alignment_priority__existential_risk_reading, coexists_with).
narrative_ontology:cs_reading_relation('b6e980c8-5202-42ca-9bee-aa655691f151', ai_alignment_priority__integrated_reading, influences).
narrative_ontology:cs_axiom('b6e980c8-5202-42ca-9bee-aa655691f151', foundational, present_documented_harm_takes_moral_priority).
narrative_ontology:cs_axiom_status(present_documented_harm_takes_moral_priority, holdable).
narrative_ontology:cs_axiom_grounding('b6e980c8-5202-42ca-9bee-aa655691f151', present_documented_harm_takes_moral_priority, deontological).
narrative_ontology:cs_axiom('b6e980c8-5202-42ca-9bee-aa655691f151', foundational, sociotechnical_audit_is_the_correct_alignment_methodology).
narrative_ontology:cs_axiom_status(sociotechnical_audit_is_the_correct_alignment_methodology, holdable).
narrative_ontology:cs_axiom_grounding('b6e980c8-5202-42ca-9bee-aa655691f151', sociotechnical_audit_is_the_correct_alignment_methodology, conventional).
narrative_ontology:cs_reference_frame('b6e980c8-5202-42ca-9bee-aa655691f151', civil_rights_grounded_algorithmic_accountability).
narrative_ontology:cs_drift_state('b6e980c8-5202-42ca-9bee-aa655691f151', post_generative_ai_scaling_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('b6e980c8-5202-42ca-9bee-aa655691f151', '').
narrative_ontology:cs_kernel_id(ai_alignment_priority__nearterm_harms_reading, ai_alignment_priority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_alignment_priority__nearterm_harms_reading, ai_ethics_research_labs).
narrative_ontology:constraint_beneficiary(ai_alignment_priority__nearterm_harms_reading, algorithmic_audit_consultancies).
narrative_ontology:constraint_beneficiary(ai_alignment_priority__nearterm_harms_reading, present_vulnerable_populations).
narrative_ontology:constraint_victim(ai_alignment_priority__nearterm_harms_reading, disabled_applicants_screened_by_automated_systems).
narrative_ontology:constraint_victim(ai_alignment_priority__nearterm_harms_reading, racialized_communities_over_policed_by_predictive_tools).
narrative_ontology:constraint_victim(ai_alignment_priority__nearterm_harms_reading, elderly_users_denied_automated_benefits_determinations).
narrative_ontology:constraint_victim(ai_alignment_priority__nearterm_harms_reading, gig_workers_subject_to_algorithmic_management).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(ai_alignment_priority__nearterm_harms_reading, present_vulnerable_populations).
narrative_ontology:constraint_vindicates(ai_alignment_priority__nearterm_harms_reading, disparate_impact_doctrine).
narrative_ontology:constraint_vindicates(ai_alignment_priority__nearterm_harms_reading, sociotechnical_systems_framing).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets the methodology for what counts as an alignment failure — dataset audits, disparate-impact testing, fairness metrics — and administers grant funding, publication venues, and conference agendas around this framing. Draws funding, prestige, and hiring pipelines from being the recognized authority on present-harm alignment work.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__nearterm_harms_reading, ai_ethics_research_labs, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(ai_alignment_priority__nearterm_harms_reading, ai_ethics_research_labs, beneficiary).

% Sell bias-audit and compliance services to firms deploying automated decision systems. Their business model depends on the near-term harms framing remaining the operative definition of alignment; existential-risk framing generates no billable audit work for them.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__nearterm_harms_reading, algorithmic_audit_consultancies, beneficiary,
    organized, biographical, mobile, national).

% Are the named subjects the reading exists to protect — people currently screened, scored, and sorted by deployed systems. Benefit when audits catch and correct discriminatory patterns, but remain exposed between audit cycles and have no direct control over whether the audit happens, how it is scoped, or whether findings are acted on.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__nearterm_harms_reading, present_vulnerable_populations, beneficiary,
    powerless, immediate, trapped, national).
narrative_ontology:stakeholder_secondary_role(ai_alignment_priority__nearterm_harms_reading, present_vulnerable_populations, payer).

% Face hiring, benefits, or credit screening tools whose disparate impact is the direct object of audit. They cannot opt out of automated screening in most contexts and cannot see or contest the models scoring them; their harm is the evidentiary basis for the reading's priority claim, but they have no seat in setting audit scope or remedy design.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__nearterm_harms_reading, disabled_applicants_screened_by_automated_systems, payer,
    powerless, immediate, trapped, national).

% Are subject to predictive policing and risk-scoring tools that concentrate enforcement attention on them. Documented disparate impact drives audit and mitigation resource flows, but remedy typically arrives as retrained models rather than removal of the tool or redress for accumulated harm.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__nearterm_harms_reading, racialized_communities_over_policed_by_predictive_tools, payer,
    powerless, generational, trapped, national).

% Have benefits eligibility determined by automated systems that systematically misclassify age-related documentation gaps as fraud risk. Their appeals are slow and under-resourced relative to the speed of automated denial; they are cited in fairness literature as an affected class but rarely funded as direct claimants.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__nearterm_harms_reading, elderly_users_denied_automated_benefits_determinations, payer,
    powerless, immediate, trapped, national).

% Have pay, scheduling, and deactivation decided by opaque platform algorithms. Fall within the near-term harms priority as a matter of framing, but platform labor is rarely the funded audit target compared with higher-visibility domains like hiring and credit; their exit option is leaving gig work entirely, at direct income cost.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__nearterm_harms_reading, gig_workers_subject_to_algorithmic_management, payer,
    powerless, biographical, constrained, global).

% Develop the most capable general-purpose systems and argue that near-term fairness audits do not address catastrophic misalignment risk from advanced systems. Under this reading their concerns are treated as a distraction from present, documentable harm and receive limited standing in the priority-setting conversation that this reading dominates.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__nearterm_harms_reading, frontier_ai_labs, excluded,
    institutional, civilizational, arbitrage, global).

% Draft and enforce disparate-impact and algorithmic accountability rules, drawing heavily on the near-term harms research community for evidentiary standards and remedy templates. Their statutory mandate (civil rights, consumer protection) structurally privileges the present-harms framing over existential-risk framing, which has no comparable enforcement hook.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__nearterm_harms_reading, regulatory_agencies, observer,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(ai_alignment_priority__nearterm_harms_reading, regulatory_agencies, agenda_setter).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ai_alignment_priority__nearterm_harms_reading, diffuse).
narrative_ontology:fixing_cost_class(ai_alignment_priority__nearterm_harms_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a research, audit, and regulatory apparatus that can detect, document, and pressure deployment-stage correction of discriminatory or extractive patterns in AI systems already affecting people — a genuine problem since disparate impact is measurable, ongoing, and otherwise invisible to those without technical access to the models.
% TRANSFER_FUNCTION: Moves funding, research attention, regulatory enforcement capacity, and remediation resources toward auditing and mitigating deployed-system harms to named marginalized groups, and moves prestige and market position toward the labs and consultancies that supply the audit methodology — while moving comparatively little toward the maintenance or compensation of the harmed populations themselves once an audit closes.
% ABSENT_VOICES: Frontier AI labs and existential-risk researchers argue the framing under-weights catastrophic tail risk and would object to resource allocation being dominated by present-harm audits; they are present in the broader alignment discourse but structurally marginalized within this reading's priority-setting venues. Harmed individuals (gig workers, benefits claimants) are rarely present in the audit-design process itself, only as data subjects.
% DISAPPEARANCE_RATIONALE: If this reading's priority-setting apparatus vanished overnight, disparate-impact audits, bias-mitigation funding streams, and civil-rights-grounded algorithmic accountability enforcement would lose their organizing framework; regulatory agencies would need a new evidentiary basis, audit consultancies would lose their primary market, and documented present-harm patterns would go substantially unmonitored in the near term.
% FOUNDING_PROBLEM: Deployed automated decision systems were producing measurable, documented discriminatory outcomes in hiring, lending, policing, and benefits administration, with no established methodology to detect, attribute, or remediate the harm, and no priority framework compelling attention to it amid growing hype around advanced AI capabilities.
% FOUNDING_PROBLEM_CORROBORATION: Independent empirical audits (e.g. academic algorithmic-fairness studies, investigative journalism on hiring and benefits systems, litigation discovery in disparate-impact cases) corroborate that discriminatory deployment harms are ongoing and measurable — this corroboration comes from outside the ethics-research and audit-consultancy beneficiary set. However, whether the CURRENT institutional apparatus is the right or sufficient response to that live problem is contested by both harmed communities (who report remedy capture by audit firms rather than direct benefit) and existential-risk researchers (who dispute the priority allocation, not the underlying harm).
narrative_ontology:disappearance_verdict(ai_alignment_priority__nearterm_harms_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_alignment_priority__nearterm_harms_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_alignment_priority__nearterm_harms_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(ai_alignment_priority__nearterm_harms_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_alignment_priority__nearterm_harms_reading, 0.68, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness rises across the interval (0.42 -> 0.68) as the audit-and-remediation apparatus institutionalizes: funding streams, consultancy markets, and academic career paths increasingly depend on the framing's continued dominance, while the marginal harm-reduction benefit to affected populations per dollar spent appears to plateau. Theater ratio also rises (0.20 -> 0.40) as compliance-oriented bias audits proliferate faster than measurable outcome improvements for the named victim classes — a pattern consistent with audit activity partially substituting for structural remedy. Suppression is moderate and rising (0.30 -> 0.52): dissenting framings (existential-risk prioritization) face increasing exclusion from funding panels and policy venues as the present-harms apparatus consolidates institutional gatekeeping power. Accessibility collapse is deliberately kept low-moderate (0.35): unlike a mountain, alternative framings (existential-risk, integrated) remain articulable and are actively defended by other institutional actors — this is a live contest, not a foreclosed one.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat (ai_ethics_research_labs), the arrangement looks like hard-won, still-underfunded justice work correcting real and ongoing algorithmic discrimination. From the payer seats (the specific victim classes), the same structure can look like their documented suffering being converted into papers, audits, and compliance products that rarely translate into direct redress, faster appeals, or removal of the harmful system. The engine computing these as different per-seat classifications from the same structural data is the point — neither seat is wrong about its own position.
 *
 * DIRECTIONALITY LOGIC:
 *   Ai_ethics_research_labs and algorithmic_audit_consultancies sit near the beneficiary end: their funding, prestige, and market position derive directly from the framing's institutional dominance, and their exit options (arbitrage/mobile) let them pivot methodology without bearing the downside of framing failure. The named victim classes sit near the full-target end: trapped exit options, immediate time horizons, and no control over audit scope or remedy design, despite being the evidentiary and moral justification for the entire apparatus. Present_vulnerable_populations is coded dual (beneficiary/payer) because they are the class the framing is FOR, but the resource flow analysis shows the apparatus built around them captures more value than it delivers to them directly.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — undetected, unremediated discriminatory deployment harm — remains live and independently corroborated (R5 status: live), which is what prevents this from being mislabeled pure extraction dressed as justice. Genuine present harms exist and genuine audits do catch and sometimes correct them; that coordination function is real and would be lost if the apparatus vanished (disappearance_verdict: world_rearranges). What keeps this from being classified as pure coordination (Rope) is the asymmetric capture: the rising extractiveness and theater trajectories show resources increasingly flowing to the apparatus that studies and audits harm rather than to the harmed populations themselves, sustained by active regulatory and institutional enforcement of this framing's evidentiary primacy over competing readings.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    framing_resource_competition,
    'Is the near-term-harms framing''s institutional dominance a proportionate response to a genuinely more tractable and better-evidenced harm, or does it partly reflect that present-harms audits are more fundable, more publishable, and more legible to existing civil-rights enforcement infrastructure than existential-risk work — i.e., is the priority ordering epistemic or institutional?',
    'Comparative analysis of funding allocation, publication venues, and regulatory enforcement actions across the two framings relative to independent expert estimates of tractable harm-reduction per dollar in each domain.',
    'If institutional legibility rather than harm-tractability drives the framing''s dominance, the extraction component (resources flowing to the apparatus over the affected populations) is better evidenced as structural capture rather than justified prioritization.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(framing_resource_competition, conceptual, 'Whether framing dominance reflects epistemic merit or institutional fundability.').

omega_variable(
    remedy_capture_vs_direct_benefit,
    'What share of resources flowing through the present-harms audit apparatus reaches the named victim classes as direct remedy (compensation, system removal, appeal-process improvement) versus remaining within the research/consultancy/regulatory apparatus as papers, audit fees, and enforcement infrastructure?',
    'Tracing itemized budget flows from major algorithmic-fairness grants and audit engagements to documented direct-benefit outcomes for affected populations (compensation paid, systems decommissioned, appeal success rates).',
    'A low direct-benefit share would strengthen the tangled_rope classification (asymmetric extraction riding on real coordination); a high direct-benefit share would push the classification toward rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(remedy_capture_vs_direct_benefit, empirical, 'Whether audit-apparatus resources convert to direct victim benefit or apparatus self-maintenance.').

omega_variable(
    kernel_reading_exclusivity,
    'Is the near-term-harms reading''s marginalization of existential-risk concerns a necessary consequence of finite institutional attention (a genuine zero-sum priority contest) or an artifact of this reading''s own framing choices that the integrated_reading demonstrates is avoidable?',
    'Track whether institutions that adopt the integrated_reading''s dual-priority framing show measurably reduced present-harm audit output, which would support zero-sum framing; if audit output holds steady, exclusivity is a framing artifact rather than a resource constraint.',
    'Determines whether this reading''s exclusion of existential-risk voices from its priority-setting venues is structurally necessary or a contestable framing choice — directly informs the reading_relations classification against the integrated_reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_exclusivity, conceptual, 'Whether this reading''s priority exclusivity is structurally forced or a framing choice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_alignment_priority__nearterm_harms_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_a_tr_t0, ai_alignment_priority__nearterm_harms_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(ai_a_tr_t4, ai_alignment_priority__nearterm_harms_reading, theater_ratio, 4, 0.24).
narrative_ontology:measurement(ai_a_tr_t8, ai_alignment_priority__nearterm_harms_reading, theater_ratio, 8, 0.29).
narrative_ontology:measurement(ai_a_tr_t12, ai_alignment_priority__nearterm_harms_reading, theater_ratio, 12, 0.33).
narrative_ontology:measurement(ai_a_tr_t16, ai_alignment_priority__nearterm_harms_reading, theater_ratio, 16, 0.36).
narrative_ontology:measurement(ai_a_tr_t20, ai_alignment_priority__nearterm_harms_reading, theater_ratio, 20, 0.38).
narrative_ontology:measurement(ai_a_tr_t24, ai_alignment_priority__nearterm_harms_reading, theater_ratio, 24, 0.4).

% Extraction over time
narrative_ontology:measurement(ai_a_be_t0, ai_alignment_priority__nearterm_harms_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(ai_a_be_t4, ai_alignment_priority__nearterm_harms_reading, base_extractiveness, 4, 0.5).
narrative_ontology:measurement(ai_a_be_t8, ai_alignment_priority__nearterm_harms_reading, base_extractiveness, 8, 0.56).
narrative_ontology:measurement(ai_a_be_t12, ai_alignment_priority__nearterm_harms_reading, base_extractiveness, 12, 0.6).
narrative_ontology:measurement(ai_a_be_t16, ai_alignment_priority__nearterm_harms_reading, base_extractiveness, 16, 0.63).
narrative_ontology:measurement(ai_a_be_t20, ai_alignment_priority__nearterm_harms_reading, base_extractiveness, 20, 0.66).
narrative_ontology:measurement(ai_a_be_t24, ai_alignment_priority__nearterm_harms_reading, base_extractiveness, 24, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(ai_a_su_t0, ai_alignment_priority__nearterm_harms_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(ai_a_su_t4, ai_alignment_priority__nearterm_harms_reading, suppression_requirement, 4, 0.35).
narrative_ontology:measurement(ai_a_su_t8, ai_alignment_priority__nearterm_harms_reading, suppression_requirement, 8, 0.4).
narrative_ontology:measurement(ai_a_su_t12, ai_alignment_priority__nearterm_harms_reading, suppression_requirement, 12, 0.44).
narrative_ontology:measurement(ai_a_su_t16, ai_alignment_priority__nearterm_harms_reading, suppression_requirement, 16, 0.47).
narrative_ontology:measurement(ai_a_su_t20, ai_alignment_priority__nearterm_harms_reading, suppression_requirement, 20, 0.5).
narrative_ontology:measurement(ai_a_su_t24, ai_alignment_priority__nearterm_harms_reading, suppression_requirement, 24, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_alignment_priority__nearterm_harms_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(ai_alignment_priority__nearterm_harms_reading, 0.12).
narrative_ontology:affects_constraint(ai_alignment_priority__nearterm_harms_reading, ai_alignment_priority__existential_risk_reading).
narrative_ontology:affects_constraint(ai_alignment_priority__nearterm_harms_reading, ai_alignment_priority__integrated_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three siblings decomposing the natural-language 'AI alignment priority' claim per the ε-invariance principle: nearterm_harms_reading (this file, ε=0.68, tangled_rope), existential_risk_reading (separate file, distinct ε and victim/beneficiary structure), and integrated_reading (separate file, attempts synthesis). Each reading names a different standing arrangement, different victims, and a different ε — they are not the same constraint viewed from different angles but three structurally distinct constraints sharing a contested kernel (ai_alignment_priority). Linked bidirectionally: the existential_risk_reading and integrated_reading files should list this constraint_id in their own affects_constraints arrays.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
