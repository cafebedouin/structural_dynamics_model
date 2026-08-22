% ============================================================================
% CONSTRAINT STORY: ai_safety_commitment__near_term_harms_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
 *   human_readable: AI Safety as Near-Term Documented Harms Prevention
 *   domain: technological/regulatory/social
 *
 * SUMMARY:
 *   The near-term harms reading of AI safety centers present-day documented
 *   injuries from deployed algorithmic systems—discriminatory hiring filters,
 *   predatory lending algorithms, opaque content moderation, worker
 *   surveillance and deactivation—as the primary safety problem. This reading
 *   names specific victims (marginalized populations, gig workers, content
 *   moderators, affected communities), establishes that harms are measurable
 *   and attributable to system design choices, and frames safety as
 *   prevention of these injuries through transparency, auditing, and
 *   remediation. The constraint operates at the intersection of tech company
 *   interests (self-regulation as alternative to mandated structural change),
 *   researcher incentives (legitimacy and funding from harm quantification),
 *   and regulatory desire (documented harms provide a legible mandate for
 *   oversight). The claim is tangled_rope: genuine coordination function
 *   (alignment on measurable harm metrics, shared audit frameworks,
 *   information exchange) co-present with asymmetric extraction (companies
 *   retain operational control while gaining legitimacy, marginalized
 *   populations gain voice without decision power). Extraction increases over
 *   the interval (0.54→0.68) as companies professionalize harm-reduction
 *   theater: formal ethics boards, commissioned audits, harm disclosure—the
 *   machinery of coordination without structural remedy. Theater ratio rises
 *   (0.28→0.42) as the ratio of audit performance to actual remediation
 *   grows—audits proliferate, remediation lags.
 *
 * KEY AGENTS:
 *   - technology_companies: institutional power, arbitrage exits — agenda-setters controlling which harms are measured and how remediation is framed
 *   - marginalized_populations: powerless, trapped exits — the primary victim set bearing documented injuries from algorithmic systems
 *   - gig_workers and content_moderators: powerless to moderate, constrained exits — bearing labor exploitation and psychological harm within algorithmic management
 *   - ai_safety_researchers (near-term focus): organized power, mobile exits — beneficiaries of the legitimacy granted by near-term harms framing, gatekeepers of harm quantification
 *   - regulatory_bodies: institutional power, analytical exits — observers who can impose mandates but face technical capture and coordination costs
 *   - existential_risk_advocates: excluded from harm governance — their frame competes for the same funding and narrative authority
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_safety_commitment__near_term_harms_reading, 0.68).
domain_priors:suppression_score(ai_safety_commitment__near_term_harms_reading, 0.71).
domain_priors:theater_ratio(ai_safety_commitment__near_term_harms_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_safety_commitment__near_term_harms_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(ai_safety_commitment__near_term_harms_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(ai_safety_commitment__near_term_harms_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_safety_commitment__near_term_harms_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(ai_safety_commitment__near_term_harms_reading, resistance, 0.73).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_safety_commitment__near_term_harms_reading, tangled_rope).
narrative_ontology:human_readable(ai_safety_commitment__near_term_harms_reading, "AI Safety as Near-Term Documented Harms Prevention").
narrative_ontology:topic_domain(ai_safety_commitment__near_term_harms_reading, "technological/regulatory/social").

domain_priors:requires_active_enforcement(ai_safety_commitment__near_term_harms_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_safety_commitment__near_term_harms_reading, 'ad2a9d76-cd43-40a2-98df-a6bff726187a').
narrative_ontology:cs_kernel_codification('ad2a9d76-cd43-40a2-98df-a6bff726187a', distributed).
narrative_ontology:cs_authority_grounding('ad2a9d76-cd43-40a2-98df-a6bff726187a', extraction).
narrative_ontology:cs_reading_relation('ad2a9d76-cd43-40a2-98df-a6bff726187a', ai_safety_commitment__existential_risk_reading, coexists_with).
narrative_ontology:cs_reading_relation('ad2a9d76-cd43-40a2-98df-a6bff726187a', ai_safety_commitment__dual_priority_reading, influences).
narrative_ontology:cs_axiom('ad2a9d76-cd43-40a2-98df-a6bff726187a', foundational, documented_present_harms_are_primary_safety_problem).
narrative_ontology:cs_axiom_status(documented_present_harms_are_primary_safety_problem, holdable).
narrative_ontology:cs_axiom_grounding('ad2a9d76-cd43-40a2-98df-a6bff726187a', documented_present_harms_are_primary_safety_problem, empirically_contingent).
narrative_ontology:cs_axiom('ad2a9d76-cd43-40a2-98df-a6bff726187a', foundational, algorithmic_systems_in_deployment_measurably_injure_real_populations).
narrative_ontology:cs_axiom_status(algorithmic_systems_in_deployment_measurably_injure_real_populations, holdable).
narrative_ontology:cs_axiom_grounding('ad2a9d76-cd43-40a2-98df-a6bff726187a', algorithmic_systems_in_deployment_measurably_injure_real_populations, empirically_contingent).
narrative_ontology:cs_reference_frame('ad2a9d76-cd43-40a2-98df-a6bff726187a', transparent_algorithmic_governance_with_harm_prevention).
narrative_ontology:cs_drift_state('ad2a9d76-cd43-40a2-98df-a6bff726187a', contemporary_implementation_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('ad2a9d76-cd43-40a2-98df-a6bff726187a', '').
narrative_ontology:cs_kernel_id(ai_safety_commitment__near_term_harms_reading, ai_safety_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_safety_commitment__near_term_harms_reading, technology_companies).
narrative_ontology:constraint_victim(ai_safety_commitment__near_term_harms_reading, marginalized_populations).
narrative_ontology:constraint_victim(ai_safety_commitment__near_term_harms_reading, gig_workers).
narrative_ontology:constraint_victim(ai_safety_commitment__near_term_harms_reading, content_moderators).
narrative_ontology:constraint_victim(ai_safety_commitment__near_term_harms_reading, affected_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(ai_safety_commitment__near_term_harms_reading, ai_safety_researchers_near_term_focus).
narrative_ontology:constraint_beneficiary(ai_safety_commitment__near_term_harms_reading, tech_company_investors).
narrative_ontology:constraint_vindicates(ai_safety_commitment__near_term_harms_reading, algorithmic_discrimination_is_real).
narrative_ontology:constraint_vindicates(ai_safety_commitment__near_term_harms_reading, deployed_systems_cause_measurable_harm).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Deploy large-scale algorithmic systems for hiring, lending, content moderation, and recommendation. Control the definition of 'safety' within product development and govern which harms are measured and reported. Frame near-term harm prevention as an internal quality issue (product liability, customer satisfaction) rather than an structural externality. Set the cadence of audits, choose which researchers conduct them, and determine remediation scope. Benefit from framing safety as a technical solvability problem rather than a power asymmetry problem.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__near_term_harms_reading, technology_companies, agenda_setter,
    institutional, biographical, arbitrage, global).

% Experience algorithmic discrimination in hiring, lending, content recommendation, and predictive policing. Have no choice about exposure to the systems (required to apply for jobs, access services, participate in society). Cannot opt out or switch providers. Bear the cumulative harms: denied credit, filtered out of job pools, misrepresented in content feeds, targeted for enforcement. Have minimal voice in how harm is defined or measured; expertise on their own experience is not solicited as part of 'safety' protocols.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__near_term_harms_reading, marginalized_populations, payer,
    powerless, biographical, trapped, global).

% Subject to algorithmic management systems (task assignment, performance rating, deactivation) operated by platform operators. Deactivation is algorithmic and opaque; appeals are limited. 'Safety' protocols rarely include worker protections against algorithmic termination, wage theft, or exposure to unsafe tasks. Economic dependence on the platform makes resistance costly. Labor organizing is explicitly suppressed by platform policy framed as 'independent contractor' status.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__near_term_harms_reading, gig_workers, payer,
    moderate, immediate, constrained, national).

% Employed (often as contractors) to view and classify extreme content at scale. Experience psychological trauma, PTSD, and vicarious injury. 'Safety' frameworks focus on system-level content detection and user experience; moderator mental health and workplace safety are systematically underweighted. Contractor status limits employment protections. Turnover is high; accumulating harms are distributed across a precarious workforce.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__near_term_harms_reading, content_moderators, payer,
    powerless, immediate, constrained, global).

% Bearers of categorical harms from algorithmic systems: Black communities harmed by discriminatory lending and hiring algorithms, immigrant communities tracked by deportation prediction systems, religious minorities targeted by radicalization feeds, low-income communities subject to surveillance-driven policing. Have mounting evidence of pattern harms but limited standing to force remediation. Can organize advocacy and file legal complaints, but tech companies control technical disclosure and harm quantification.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__near_term_harms_reading, affected_communities, payer,
    organized, generational, mobile, national).

% Conduct research on bias, discrimination, and present-day harms from deployed systems. Benefit from institutional funding, publication venues, and professional legitimacy when near-term harms are designated as 'AI safety.' Control the metrics and methods by which harms are quantified. Their legitimacy is enhanced when companies acknowledge near-term harms and commission audits (even when audits are controlled and limited). Compete with existential-risk researchers for funding and narrative authority.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__near_term_harms_reading, ai_safety_researchers_near_term_focus, beneficiary,
    organized, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(ai_safety_commitment__near_term_harms_reading, ai_safety_researchers_near_term_focus, observer).

% Benefit from the constraint's operation: near-term harm prevention, when framed as a technical and internal corporate issue, keeps regulatory liability limited and does not require structural changes to business models. Companies can adopt harm-reduction practices without altering fundamental incentives or transferring power. Benefit from the narrative that 'AI safety research' is happening and harms are being mitigated, which sustains investor confidence.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__near_term_harms_reading, tech_company_investors, beneficiary,
    powerful, biographical, arbitrage, global).

% Have authority to mandate algorithmic audits, transparency, and harm reporting but face technical capture and coordination challenges. Often lack in-house expertise to verify company claims. Can be lobbied by tech companies to adopt frameworks that define 'safety' narrowly (technical bias mitigation rather than structural power reallocation). May use near-term harms framework to justify light-touch regulation rather than structural remediation.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__near_term_harms_reading, regulatory_bodies, observer,
    institutional, generational, analytical, national).

% Advocate that AI safety primarily means preventing extinction-level outcomes from misaligned superintelligent systems. Compete with near-term harms advocates for funding and narrative authority. Are structurally excluded from harm quantification conversations; their frame suggests present-day harms are negligible relative to existential risk. Their exclusion from vulnerability mapping is part of what allows the near-term harms reading to proceed uncontested in applied governance contexts.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__near_term_harms_reading, existential_risk_advocates, excluded,
    organized, civilizational, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ai_safety_commitment__near_term_harms_reading, technology_companies).
narrative_ontology:fixing_cost_class(ai_safety_commitment__near_term_harms_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aligns tech companies, researchers, and regulators on a shared definition of 'AI safety' focused on measurable harms from deployed systems—bias, discrimination, labor exploitation, misinformation. Creates common metrics for audit and disclosure, enabling information sharing without mandating structural power reallocation. Coordinates behavior around documented harm prevention rather than speculative risk.
% TRANSFER_FUNCTION: Moves regulatory legitimacy and investor confidence from tech companies to the researchers and advocates who define and measure near-term harms. Companies that adopt transparency and auditing frameworks gain public credibility while maintaining operational control. Transfer of labor and attention: harm documentation and remediation work flows to companies, researchers, and affected communities, but remediation authority remains with companies.
% ABSENT_VOICES: Existential-risk researchers and AI capabilities developers are structurally excluded—their frame competes for funding and narrative authority. Communities most harmed by algorithmic systems have voice in documenting harms but minimal voice in designing remediation or governing technical choices. Employees and contractors building these systems (who see power asymmetries firsthand) are rarely included in safety governance.
% DISAPPEARANCE_RATIONALE: If the near-term harms framing of AI safety disappeared and were replaced by existential-risk framing alone, tech companies would redirect funding and researcher attention away from present-day harm audits and toward speculative alignment research. Marginalized populations would lose the institutional framing that names their documented injuries as 'safety issues.' Regulatory momentum for transparency and algorithmic auditing would dissipate. The resource flows and research incentive structures would reorganize entirely; harm-reduction communities would lose standing.
% FOUNDING_PROBLEM: Deployed algorithmic systems in hiring, lending, content moderation, and social services cause measurable, documented harms to marginalized populations: discriminatory filtering, wage theft, misrepresentation, and psychological injury. Early AI deployments revealed severe bias in facial recognition, hiring algorithms, and predictive policing—harms borne by real people, measurable in real time.
% FOUNDING_PROBLEM_CORROBORATION: Documented by independent researchers (Buolamwini, Gebru on facial recognition; Obermeyer et al. on algorithmic bias in healthcare), civil rights organizations (ACLU, AI Now Institute), affected communities testifying before regulatory bodies, and investigative journalism (ProPublica on COMPAS, risk algorithms). Tech companies acknowledge some harms in response to pressure but frame them as technical problems solvable through bias audits rather than structural power reallocation.
narrative_ontology:disappearance_verdict(ai_safety_commitment__near_term_harms_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_safety_commitment__near_term_harms_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_safety_commitment__near_term_harms_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(ai_safety_commitment__near_term_harms_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_safety_commitment__near_term_harms_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is high (0.68) because the constraint delivers regulatory legitimacy, investor confidence, and researcher funding to tech companies while leaving operational power intact—harms are named and measured but structural incentives remain unchanged. Suppression is correspondingly high (0.71) because the constraint's persistence depends on actively excluding existential-risk frames from harm governance (discouraging competing funding) and limiting remediation scope to technical bias mitigation (suppressing structural critique). Theater is moderate (0.42) reflecting the genuine harm quantification work alongside performative auditing: real researchers document real harms; companies respond with disclosure and limited remediation; the system persists because neither speed nor depth of remediation matches the rate of new system deployment and harm discovery. Accessibility collapse is low (0.48)—alternatives are visible and articulated (existential-risk framing, structural power redistribution, mandatory tech worker participation in governance, community co-design)—but are suppressed through narrative competition and resource allocation rather than being logically foreclosed. Resistance is high (0.73): marginalized communities and civil rights organizations actively contest the framing, researchers compete for resources under different frames, tech workers organize against algorithmic management.
 *
 * PERSPECTIVAL GAP:
 *   From the technology company seat, the constraint is genuine and valuable coordination: 'we have aligned on shared metrics, we conduct audits, we remediate known harms, and we do this collaboratively with researchers and regulators.' From the marginalized population seat, the constraint is a façade of coordination that preserves power asymmetry: 'we are named and documented as harmed, but the systems that harm us are not redesigned, the incentives that produce harm remain, and our remedy is limited to disclosure of harms we did not consent to bear.' From the researcher seat (near-term focus), the constraint is a legitimate governance framework and professional home. From the existential-risk seat, the constraint is a diversion of resources and attention from the primary safety problem. The engine should compute these seats as experiencing fundamentally different constraint types because their exit options, power, and relationship to the beneficiary group differ sharply.
 *
 * DIRECTIONALITY LOGIC:
 *   Technology companies: d ≈ 0.25 (beneficiary with modest costs—reputational risk from auditing, but operational control retained, regulatory capture maintained). Marginalized populations: d ≈ 0.95 (nearly full target—trapped, powerless, bearing documented injury, dependent on external advocacy). Gig workers: d ≈ 0.88 (high target—trapped by economic dependence, algorithmic deactivation is within scope of labor harms but outside most 'AI safety' definitions). Content moderators: d ≈ 0.92 (high target—psychological injury, contractor status, no seat at safety governance tables). Safety researchers (near-term): d ≈ 0.15 (minor beneficiary—legitimacy and funding flow to this frame, but no capture of the constraint's extraction). Regulatory bodies: d ≈ 0.50 (symmetric—mandate to protect public, but technical and institutional constraints limit their power to enforce structural change). Existential-risk advocates: d ≈ 0.80 (high target of suppression—competing frame, excluded from harm governance, see funding redirected away).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem—present-day harms from deployed systems—is live and actively documented. However, mandatrophy is nascent: the constraint was built to solve 'prevent documented harms through transparency and auditing' but the actual outcome is 'document harms and maintain power asymmetry.' Remediation speed has not kept pace with deployment speed; the gap is growing (theater_ratio rising). The constraint persists because (1) companies benefit from the legitimacy it grants while retaining operational control, (2) researchers have career investment in the frame, (3) regulatory bodies lack mandate or capacity for structural remedy, and (4) affected communities lack power to force change. Mandatrophy is prevented from full bloom by ongoing harm documentation and civil rights pressure, but the trajectory is clear: as auditing becomes routine and remediation becomes theatrical, the founding problem will be declared 'solved' while harms persist. Mandate obsolescence is actively being produced by the constraint's own operation—the machinery of auditing (theater) is designed to appear responsive while limiting actual power reallocation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    remediation_speed_vs_deployment_speed,
    'Does the rate at which documented harms are remediated keep pace with the rate at which new systems are deployed and new harms discovered?',
    'Longitudinal tracking of harm reports vs. remediation timelines; assessment of whether system deployment in new domains precedes harm documentation in those domains.',
    'If deployment outpaces remediation, the constraint is increasingly theatrical—it names harms but cannot prevent them at scale, and mandatrophy accumulates. If remediation tracks deployment, the constraint is functional. If remediation leads deployment, the constraint is preventive.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(remediation_speed_vs_deployment_speed, empirical, 'Whether harm documentation enables preventive action or merely documents persistent injuries.').

omega_variable(
    structural_remedy_vs_technical_mitigation,
    'Are the harms documented by the near-term-safety frame amenable to technical bias mitigation (auditing, algorithm tuning, disclosure), or do they require structural changes to power and incentives (community co-design, worker governance, mandatory remedy)?',
    'Comparative study of harm persistence across companies with high-sophistication auditing vs. communities that have negotiated structural governance changes.',
    'If technical mitigation suffices, the constraint is addressing the founding problem. If harms persist despite sophisticated auditing, the constraint is a Tangled Rope of coordination theater and preserved extraction. This determines whether mandatrophy is imminent or whether the founding problem is genuinely live.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(structural_remedy_vs_technical_mitigation, conceptual, 'Whether the constraint addresses root causes or symptom management.').

omega_variable(
    kernel_reading_foreclosure,
    'Does the near-term-harms reading logically foreclose the existential-risk reading, or do they coexist as competing frames held by different parties?',
    'Examine whether a single coherent framework can hold both—e.g., ''we must prevent near-term harms AND prepare for existential risk.'' If frameworks can hold both (most likely outcome), then coexistence is the answer. If one premise directly denies a core claim of the other, foreclosure may apply.',
    'If coexistence, the readings are in resource competition but not logical contradiction—kernel contest continues. If foreclosure, one reading''s core premise has been invalidated by events or evidence, reshaping the kernel. Affects cs_structure.reading_relations classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure, conceptual, 'Whether competing safety readings can coexist in a single coherent framework.').

omega_variable(
    beneficiary_capture_scope,
    'Which stakeholders are the actual captors of the constraint''s extraction—do tech companies monopolize the legitimacy gains, or do researchers, regulators, and civil rights organizations also capture portions?',
    'Track resource flows: funding allocated to near-term harm research, regulatory capacity expansion, advocacy organization funding. Assess who retains decision authority over remediation.',
    'If capture is primarily by tech companies (high probability given institutional power and technical control), the constraint is a snare-flavored Tangled Rope. If capture is distributed, remediation authority may be more credible and mandatrophy delayed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_capture_scope, empirical, 'Distribution of extraction capture across seats.').

omega_variable(
    suppression_of_structural_critique,
    'Is the measured suppression (0.71) primarily structural (external barriers to systemic change) or performative (the machinery of auditing itself as suppression narrative)?',
    'Test: if companies committed to community co-governance and power-sharing in safety decisions, would structural barriers dissolve, or would they claim technical infeasibility? If infeasibility claims persist despite dismantled formal barriers, suppression is internalized in technical discourse.',
    'If structural, removing formal barriers (regulatory mandate, funding change) enables remedy. If performative/internalized, the constraint persists by making systemic change unthinkable despite its feasibility—signature of a Snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_of_structural_critique, empirical, 'Locus of suppression: external barriers vs. internalized technical inevitability.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_safety_commitment__near_term_harms_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_s_tr_t0, ai_safety_commitment__near_term_harms_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement_basis(ai_s_tr_t0, observed).
narrative_ontology:measurement(ai_s_tr_t4, ai_safety_commitment__near_term_harms_reading, theater_ratio, 4, 0.33).
narrative_ontology:measurement_basis(ai_s_tr_t4, observed).
narrative_ontology:measurement(ai_s_tr_t8, ai_safety_commitment__near_term_harms_reading, theater_ratio, 8, 0.36).
narrative_ontology:measurement_basis(ai_s_tr_t8, observed).
narrative_ontology:measurement(ai_s_tr_t12, ai_safety_commitment__near_term_harms_reading, theater_ratio, 12, 0.4).
narrative_ontology:measurement_basis(ai_s_tr_t12, observed).
narrative_ontology:measurement(ai_s_tr_t16, ai_safety_commitment__near_term_harms_reading, theater_ratio, 16, 0.41).
narrative_ontology:measurement_basis(ai_s_tr_t16, observed).
narrative_ontology:measurement(ai_s_tr_t20, ai_safety_commitment__near_term_harms_reading, theater_ratio, 20, 0.42).
narrative_ontology:measurement_basis(ai_s_tr_t20, observed).

% Extraction over time
narrative_ontology:measurement(ai_s_be_t0, ai_safety_commitment__near_term_harms_reading, base_extractiveness, 0, 0.54).
narrative_ontology:measurement_basis(ai_s_be_t0, observed).
narrative_ontology:measurement(ai_s_be_t4, ai_safety_commitment__near_term_harms_reading, base_extractiveness, 4, 0.58).
narrative_ontology:measurement_basis(ai_s_be_t4, observed).
narrative_ontology:measurement(ai_s_be_t8, ai_safety_commitment__near_term_harms_reading, base_extractiveness, 8, 0.62).
narrative_ontology:measurement_basis(ai_s_be_t8, observed).
narrative_ontology:measurement(ai_s_be_t12, ai_safety_commitment__near_term_harms_reading, base_extractiveness, 12, 0.65).
narrative_ontology:measurement_basis(ai_s_be_t12, observed).
narrative_ontology:measurement(ai_s_be_t16, ai_safety_commitment__near_term_harms_reading, base_extractiveness, 16, 0.67).
narrative_ontology:measurement_basis(ai_s_be_t16, observed).
narrative_ontology:measurement(ai_s_be_t20, ai_safety_commitment__near_term_harms_reading, base_extractiveness, 20, 0.68).
narrative_ontology:measurement_basis(ai_s_be_t20, observed).

% Suppression requirement over time
narrative_ontology:measurement(ai_s_su_t0, ai_safety_commitment__near_term_harms_reading, suppression_requirement, 0, 0.62).
narrative_ontology:measurement_basis(ai_s_su_t0, observed).
narrative_ontology:measurement(ai_s_su_t4, ai_safety_commitment__near_term_harms_reading, suppression_requirement, 4, 0.65).
narrative_ontology:measurement_basis(ai_s_su_t4, observed).
narrative_ontology:measurement(ai_s_su_t8, ai_safety_commitment__near_term_harms_reading, suppression_requirement, 8, 0.68).
narrative_ontology:measurement_basis(ai_s_su_t8, observed).
narrative_ontology:measurement(ai_s_su_t12, ai_safety_commitment__near_term_harms_reading, suppression_requirement, 12, 0.7).
narrative_ontology:measurement_basis(ai_s_su_t12, observed).
narrative_ontology:measurement(ai_s_su_t16, ai_safety_commitment__near_term_harms_reading, suppression_requirement, 16, 0.7).
narrative_ontology:measurement_basis(ai_s_su_t16, observed).
narrative_ontology:measurement(ai_s_su_t20, ai_safety_commitment__near_term_harms_reading, suppression_requirement, 20, 0.71).
narrative_ontology:measurement_basis(ai_s_su_t20, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_safety_commitment__near_term_harms_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(ai_safety_commitment__near_term_harms_reading, 0.12).
narrative_ontology:affects_constraint(ai_safety_commitment__near_term_harms_reading, ai_safety_commitment__existential_risk_reading).
narrative_ontology:affects_constraint(ai_safety_commitment__near_term_harms_reading, ai_safety_commitment__dual_priority_reading).

% DUAL FORMULATION NOTE:
% The ai_safety_commitment kernel has three structurally distinct readings: near_term_harms (this story), existential_risk, and dual_priority. Each reading instantiates a different constraint with different ε values, victim sets, and enforcement structures. The near_term reading focuses on present-day measurable injuries and achieves high extractiveness through tech company control of remediation. The existential_risk reading focuses on speculative tail risks and treats present harms as lower-priority. The dual_priority reading attempts to hold both but creates resource competition. These are not the same constraint viewed from different seats—they have fundamentally different referents, different beneficiary/victim structures, and different ε values. Linked via network.affects_constraints to enable comparative analysis of how reading competition shapes governance.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ai_safety_commitment__near_term_harms_reading, analytical, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
