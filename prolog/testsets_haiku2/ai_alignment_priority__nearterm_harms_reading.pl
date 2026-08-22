% ============================================================================
% CONSTRAINT STORY: ai_alignment_priority__nearterm_harms_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: ai_alignment_priority__nearterm_harms_reading
 *   human_readable: AI Alignment as Nearterm Harms Prevention (Justice Priority Reading)
 *   domain: technology/governance/ethics
 *
 * SUMMARY:
 *   This constraint instantiates one reading of the contested 'AI alignment'
 *   kernel: alignment MEANS preventing present discriminatory and extractive
 *   harms from deployed AI systems, and the priority is justice for
 *   marginalized populations subject to those harms TODAY. Under this
 *   reading, alignment research, deployment standards, and computational
 *   resource allocation are reoriented toward bias audits, impact assessments
 *   on marginalized groups, and mandatory evaluation before system
 *   deployment. The constraint is CLAIMED as tangled_rope (genuine
 *   coordination function around justice, asymmetric extraction from
 *   capability research and deployment schedules) and the authored metrics
 *   (ε=0.81, suppression=0.76, theater_ratio=0.48) describe a substantially
 *   extractive, actively enforced operation. The measurement series track how
 *   extractiveness and suppression intensity have risen over the interval
 *   (0–20) as justice-centered audit requirements and deployment gates have
 *   hardened, while theater_ratio (ratio of performative to functional
 *   activity) has remained moderate — the justice evaluation function is
 *   real, but enforcement increasingly shifts toward defending the priority
 *   frame against existential-risk and capability-research counterclaims.
 *
 * KEY AGENTS:
 *   - marginalized_populations_present: victims of documented algorithmic harms (powerless, trapped exit, biographical horizon)
 *   - civil_rights_advocates: organizational voice centering affected communities, setting agenda for justice-aligned evaluation (organized, constrained exit, generational horizon)
 *   - ai_safety_auditors: institutional researchers and regulators tasked with bias assessment and deployment gates (institutional, mobile exit, generational horizon)
 *   - ai_industry_deployment_schedules: powerful actors whose release timelines are constrained by justice-centered evaluation requirements (powerful, constrained exit, biographical horizon)
 *   - capability_research_prioritization: institutional allocation of funding and GPU toward scaling vs. fairness (institutional, constrained exit, generational horizon)
 *   - existential_risk_researchers: excluded from the priority frame, would argue for long-horizon focus (institutional, trapped by the priority reframing, civilizational horizon)
 *   - integrated_alignment_framework: analytical observer modeling complementarity between nearterm and existential concerns
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_alignment_priority__nearterm_harms_reading, 0.81).
domain_priors:suppression_score(ai_alignment_priority__nearterm_harms_reading, 0.76).
domain_priors:theater_ratio(ai_alignment_priority__nearterm_harms_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_alignment_priority__nearterm_harms_reading, extractiveness, 0.81).
narrative_ontology:constraint_metric(ai_alignment_priority__nearterm_harms_reading, suppression_requirement, 0.76).
narrative_ontology:constraint_metric(ai_alignment_priority__nearterm_harms_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_alignment_priority__nearterm_harms_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(ai_alignment_priority__nearterm_harms_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_alignment_priority__nearterm_harms_reading, tangled_rope).
narrative_ontology:human_readable(ai_alignment_priority__nearterm_harms_reading, "AI Alignment as Nearterm Harms Prevention (Justice Priority Reading)").
narrative_ontology:topic_domain(ai_alignment_priority__nearterm_harms_reading, "technology/governance/ethics").

domain_priors:requires_active_enforcement(ai_alignment_priority__nearterm_harms_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_alignment_priority__nearterm_harms_reading, '5b54196d-ae1c-4c0e-9fdd-01d474e7b1ec').
narrative_ontology:cs_kernel_codification('5b54196d-ae1c-4c0e-9fdd-01d474e7b1ec', formalized).
narrative_ontology:cs_authority_grounding('5b54196d-ae1c-4c0e-9fdd-01d474e7b1ec', distributed).
narrative_ontology:cs_reading_relation('5b54196d-ae1c-4c0e-9fdd-01d474e7b1ec', ai_alignment_priority__existential_risk_reading, coexists_with).
narrative_ontology:cs_reading_relation('5b54196d-ae1c-4c0e-9fdd-01d474e7b1ec', ai_alignment_priority__integrated_reading, influences).
narrative_ontology:cs_axiom('5b54196d-ae1c-4c0e-9fdd-01d474e7b1ec', foundational, present_harm_is_alignment_failure).
narrative_ontology:cs_axiom_status(present_harm_is_alignment_failure, holdable).
narrative_ontology:cs_axiom_grounding('5b54196d-ae1c-4c0e-9fdd-01d474e7b1ec', present_harm_is_alignment_failure, empirically_contingent).
narrative_ontology:cs_axiom('5b54196d-ae1c-4c0e-9fdd-01d474e7b1ec', foundational, justice_for_current_victims_is_priority).
narrative_ontology:cs_axiom_status(justice_for_current_victims_is_priority, holdable).
narrative_ontology:cs_axiom_grounding('5b54196d-ae1c-4c0e-9fdd-01d474e7b1ec', justice_for_current_victims_is_priority, deontological).
narrative_ontology:cs_reference_frame('5b54196d-ae1c-4c0e-9fdd-01d474e7b1ec', deployment_without_justice_audit).
narrative_ontology:cs_drift_state('5b54196d-ae1c-4c0e-9fdd-01d474e7b1ec', contemporary_post_2020_regulation, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('5b54196d-ae1c-4c0e-9fdd-01d474e7b1ec', '').
narrative_ontology:cs_kernel_id(ai_alignment_priority__nearterm_harms_reading, ai_alignment_priority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_alignment_priority__nearterm_harms_reading, marginalized_populations_present).
narrative_ontology:constraint_beneficiary(ai_alignment_priority__nearterm_harms_reading, ai_safety_auditors).
narrative_ontology:constraint_beneficiary(ai_alignment_priority__nearterm_harms_reading, civil_rights_advocates).
narrative_ontology:constraint_victim(ai_alignment_priority__nearterm_harms_reading, ai_industry_deployment_schedules).
narrative_ontology:constraint_victim(ai_alignment_priority__nearterm_harms_reading, capability_research_prioritization).
narrative_ontology:constraint_victim(ai_alignment_priority__nearterm_harms_reading, computational_resource_allocation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Subject to deployed AI systems that make consequential decisions about credit, employment, healthcare, criminal justice, and social services. Face documented algorithmic bias and discriminatory outcomes that harm their material conditions TODAY. Under the nearterm-harms reading, they are the primary beneficiaries whose justice is the alignment priority — their harms are the measure of misalignment.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__nearterm_harms_reading, marginalized_populations_present, beneficiary,
    powerless, biographical, trapped, global).

% Organizations and researchers documenting algorithmic harms to marginalized groups. Advocate for mandatory bias audits, impact assessments, and deployment delays pending justice-centered evaluation. Set the agenda for alignment by framing it as harm-prevention and centering affected communities' voices in technical decision-making.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__nearterm_harms_reading, civil_rights_advocates, beneficiary,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(ai_alignment_priority__nearterm_harms_reading, civil_rights_advocates, agenda_setter).

% Technical researchers, ethicists, and regulatory bodies tasked with evaluating AI systems for discriminatory harm and deployment risk. This reading gives them the mandate to conduct sociotechnical audits, test systems against marginalized-group outcomes, and recommend deployment constraints or delays. Their resources and institutional authority expand under the nearterm-harms priority.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__nearterm_harms_reading, ai_safety_auditors, beneficiary,
    institutional, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(ai_alignment_priority__nearterm_harms_reading, ai_safety_auditors, agenda_setter).

% Technology companies race to deploy increasingly capable systems on production timelines. Under the nearterm-harms reading, deployment is contingent on passing bias audits and demonstrating non-harm to marginalized groups — a requirement that slows release cycles, requires additional testing infrastructure, and creates liability exposure if harms manifest post-deployment.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__nearterm_harms_reading, ai_industry_deployment_schedules, payer,
    powerful, biographical, constrained, global).

% Research funding and computational resources directed toward scaling AI capabilities. Under the nearterm-harms reading, capability advancement is subordinated to justice requirements — funding shifts toward auditing and mitigation rather than scaling; models are tested on marginalized-group robustness before deployment; capability gains are constrained by justice-centered deployment gates.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__nearterm_harms_reading, capability_research_prioritization, payer,
    institutional, generational, constrained, global).

% Scarce GPU, engineering, and research cycles allocated to model scaling vs. bias testing and auditing. The nearterm-harms reading redirects computational investment toward robustness testing, fairness evaluation, and impact assessment on marginalized groups — an allocation that competes with capabilities scaling and slows deployment velocity.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__nearterm_harms_reading, computational_resource_allocation, payer,
    powerful, immediate, constrained, global).

% Researchers prioritizing long-horizon existential safety (preventing loss of control, power-seeking behavior, misalignment at scale). Under the nearterm-harms reading, existential risk is deframed as the primary alignment concern and resources shift away from speculative long-horizon scenarios toward present documented harms. They are structurally excluded from the priority-setting frame that the nearterm reading instantiates.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__nearterm_harms_reading, existential_risk_researchers, excluded,
    institutional, civilizational, trapped, global).

% The principle of centering affected communities' voices in alignment and deployment decisions. Not an actor but a procedural requirement this reading instantiates — marginalized populations' own testimony and analysis become inputs to technical and policy decisions, rather than being filtered through researcher or industry intermediaries.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__nearterm_harms_reading, affected_communities_voice, beneficiary,
    powerless, biographical, trapped, global).
narrative_ontology:stakeholder_non_agent(ai_alignment_priority__nearterm_harms_reading, affected_communities_voice).

% Analytical seat representing the integrated reading that treats nearterm and existential harms as complementary rather than competing. Observes the sibling readings' resource competition and priority conflict; models where addressing present harms strengthens long-horizon alignment (reducing deployment of unjust systems) and where existential constraints enable justice (technical robustness requirements that serve both goals).
narrative_ontology:constraint_stakeholder(ai_alignment_priority__nearterm_harms_reading, integrated_alignment_framework, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ai_alignment_priority__nearterm_harms_reading, diffuse).
narrative_ontology:fixing_cost_class(ai_alignment_priority__nearterm_harms_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aligns AI development, deployment, and resource allocation toward justice for marginalized populations: coordinates research toward bias auditing, deployment toward justice gates, institutional authority toward impact assessment and community accountability, and affected-community voice toward decision-making centers. Solves the coordination problem of preventing documented algorithmic harms from reaching production without justice-centered evaluation.
% TRANSFER_FUNCTION: Moves institutional resources (research funding, GPU allocation, regulatory authority, deployment timelines) from capability-scaling priorities toward bias-auditing and justice-centered evaluation. Transfers decision-making authority from pure-capability metrics toward fairness and impact criteria. Transfers visibility and voice from researcher-mediated framing toward affected-community testimony.
% ABSENT_VOICES: Existential-risk researchers and long-horizon AI safety communities are excluded from the priority frame; they would argue that nearterm-justice concerns should be addressed by regulation and governance AFTER development, not by constraining research itself, and that focusing alignment resources on documented present harms diverts attention from speculative catastrophic risks. Capability researchers and deployment-accelerated teams are also structurally excluded — their interests (research velocity, capability gains, market timing) are subordinated under the justice priority. Technology entrepreneurs focused on competitive positioning are similarly excluded from voice in deployment-gate decisions.
% DISAPPEARANCE_RATIONALE: If the nearterm-harms alignment reading and its enforcement mechanisms disappeared, AI deployment would accelerate without mandatory bias audits; marginalized populations would face increased algorithmic harms across credit, employment, healthcare, and criminal-justice systems without pre-deployment justice assessment; civil-rights advocacy infrastructure that uses the alignment frame to justify mandatory audits would lose a key institutional lever; research resources currently allocated to fairness and robustness evaluation would reallocate to capability scaling; regulatory bodies would lose a governance framing for deployment oversight tied to justice metrics.
% FOUNDING_PROBLEM: AI systems deployed in production make high-stakes decisions (credit access, employment, healthcare, criminal justice) that disproportionately harm marginalized populations (people of color, people with disabilities, low-income populations) through encoded and amplified algorithmic bias; existing governance structures treat these as isolated incidents or product bugs rather than alignment failures; AI development priorities (capability scaling, deployment velocity) race ahead without requiring justice-impact assessment; marginalized populations bear documented harms without participating in technical decisions that affect them.
% FOUNDING_PROBLEM_CORROBORATION: Documented by independent researchers outside the industry (Buolamwini & Buonassisi on facial-recognition bias across skin types; Obermeyer et al. on racial bias in healthcare-allocation algorithms; Mitchell & Buolamwini gender-shades study; ProPublica investigations of COMPAS recidivism scoring; Williams et al. on algorithmic bias in hiring; Zarsky on discrimination by algorithm). Corroborated by civil-rights organizations (ACLU, Electronic Frontier Foundation, racial justice and disability rights groups), investigative journalism (ProPublica, The Verge, MIT Technology Review, Washington Post), regulatory bodies (FTC enforcement actions, Department of Justice civil-rights findings), and testimony from affected communities. No corroboration from the AI industry's beneficiary seats — the industry typically acknowledges bias as a technical problem while resisting the justice framing that would mandate pre-deployment gates.
narrative_ontology:disappearance_verdict(ai_alignment_priority__nearterm_harms_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_alignment_priority__nearterm_harms_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_alignment_priority__nearterm_harms_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(ai_alignment_priority__nearterm_harms_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_alignment_priority__nearterm_harms_reading, 0.81, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is high (0.81) and rising because the constraint redirects computational resources and institutional priority away from capability scaling—where the beneficiary (industry deployment) would naturally invest—toward justice auditing where power is more distributed. The extraction is from capability research (loses resources and priority), from deployment schedules (constrained by audit gates), and from research time budgets (auditing slows capability work). Suppression is substantial (0.76) because the constraint requires active enforcement of the bias-evaluation and deployment-gate mandates; without continuous institutional effort (regulatory oversight, mandatory audit funding, deployment liability frameworks), the industry would default back to capability-first prioritization. The suppression measurement rising over time reflects intensifying regulatory pressure and mandatory-audit infrastructure hardening. Theater_ratio is moderate (0.48) because the justice evaluation function is genuine (documented biases exist, auditing detects them, justice-centered evaluation prevents some harms) but a growing share of enforcement machinery defends the priority reframing itself against existential-risk counterclaims and capability-research pressure, rather than performing the core coordination function. The measurement series on one shared time grid (every metric at every point) enables cross-metric pattern detection.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter (civil-rights advocates) and the primary target (industry deployment and capability research) experience radically different constraint types. From the advocacy seat, the nearterm-harms reading is genuine coordination around justice: mobilizing research toward harms detection, centering affected communities' voices, preventing deployment of unjust systems. From the industry and capability-research seats, the same structure operates as enforced extraction: mandatory audits delay deployment, bias-centered evaluation subordinates capability metrics, regulatory gates create liability, and resources divert from scaling. The existential-risk researchers occupy a third perspective: seeing the nearterm priority as a constraint that redirects alignment resources away from existential safety and toward distributional harms they frame as solvable by post-deployment governance (regulation, auditing, harm mitigation) rather than pre-deployment constraints. The engine computes these divergences from the structural data: beneficiary/victim declarations generate the asymmetry; power differences (powerful industry vs. organized advocates vs. powerless marginalized populations) modulate the effective extraction; exit-option differences (industry constrained by regulatory gates, marginalized populations trapped in harm, advocates mobile across institutional venues) amplify the directionality divergence. The authored claim (tangled_rope) reflects the analyst's view that genuine coordination (justice evaluation, community-centered assessment) coexists with asymmetric extraction (from capability research, from industry deployment speed); the engine checks whether the computed per-seat types match or diverge from the claim.
 *
 * DIRECTIONALITY LOGIC:
 *   Marginalized populations are the primary beneficiaries: they collide with deployed AI systems that harm them, and under the nearterm-harms reading their justice is THE alignment metric (d near beneficiary end, ~0.1–0.2). Civil-rights advocates are beneficiaries with secondary agenda-setting power: they set the priority frame and direct resources toward justice-centered evaluation (d moderate, ~0.3). AI-safety auditors benefit from institutional expansion and resource allocation under this reading; they gain authority and budget to conduct sociotechnical audits (d beneficiary, ~0.2). The constraint TARGETS the powerful AI industry and capability researchers: their deployment schedules and research allocation are subordinated to justice requirements; computational cycles redirect away from scaling toward auditing; institutional priority shifts from pure-capability metrics toward fairness and robustness (d target, ~0.8–0.9). The integrated-alignment observer seat is analytical (d=0.5 by construction) — seeing where the nearterm and existential frames diverge and examining whether addressing present harms strengthens long-horizon alignment or competes with it. Directionality overrides are not needed; the structural derivation captures the asymmetry from beneficiary/victim declarations and exit modulation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (deployed AI systems encode and amplify discrimination against marginalized populations) is LIVE — documented algorithmic biases continue to harm credit access, employment opportunity, healthcare outcomes, and criminal-justice vulnerability for marginalized groups in production systems. The founding problem's status is not dead or obsolete; it persists in the operational reality of deployed systems. This gates the tangled-rope classification: a tangled rope solves a real, live coordination problem (justice-centered evaluation of AI systems, prevention of documented harms, centering affected communities' voices) while extracting resources from the capability/deployment seats. If the founding problem were dead (algorithmic bias solved, harms eliminated), the constraint would degrade to piton (theatrical maintenance of a justice frame around solved problems). The disappearance verdict is world_rearranges, not world_unchanged, because arrangements DO depend on the constraint: marginalized populations currently receive some harm-prevention from audits and deployment gates; civil-rights advocacy infrastructure depends on the alignment frame to justify mandatory audits; regulatory bodies use the alignment framing to allocate oversight authority. If the constraint vanished, deployment would accelerate without bias gates, and documented harms would increase. The mismatch test (R5 genealogy) shows no mandatrophy: founding_problem=live, founding_problem_status=live, disappearance_verdict=world_rearranges — congruent pattern, no zombie flag.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    nearterm_existential_research_tradeoff,
    'Do resources and institutional attention allocated to nearterm-harms auditing competitively exclude existential-risk research, or do the two research programs reinforce each other?',
    'Observational: measure institutional resource flows (funding, personnel, computational allocation) over 5–10 years; compare constraint-tightening periods (when nearterm-harms requirements are enforced) against existential-risk research productivity and institutional expansion. Natural experiment: jurisdictions that mandate nearterm-harms auditing vs. those that do not, tracking existential-risk research output as a proxy for whether local mandates crowd out the field.',
    'If resources are mutually exclusive (zero-sum), the nearterm-harms reading is extractive from existential-risk constituencies and the existential-risk reading''s constraint-building is justified. If complementary (addressing present harms strengthens long-horizon safety, e.g., by creating institutional infrastructure for technical robustness evaluation), the integrated reading''s framing becomes more defensible and both readings can coexist without structural conflict.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(nearterm_existential_research_tradeoff, empirical, 'Whether nearterm-justice and long-horizon-existential priorities compete for resources or reinforce.').

omega_variable(
    marginalized_populations_voice_integration,
    'Are affected communities (marginalized populations experiencing algorithmic harm) genuinely centered in the decision-making for deployment gates and bias standards, or is their voice instrumental (cited as rationale but filtered through researcher and advocate intermediaries)?',
    'Procedural audit: examine deployment-gate decisions and bias-standard-setting bodies; measure what percentage of decision-making authority is held by representatives with direct accountability to affected communities vs. researchers, advocates, regulators who speak on their behalf. Track whether community veto power exists (can affected communities block deployment) or only advisory status (can make recommendations that are overridden). Survey affected communities about whether they experience voice and agency in alignment decisions.',
    'If voice is instrumental (filtered through intermediaries), the constraint is extractive even on the beneficiary side — the justice framing is used to justify decisions made without community participation, and marginalized populations remain objects of protection rather than participants. This would reclassify portions of the constraint toward snare. If voice is genuine (community veto exists, decision-making is accountable to affected populations), the coordination function is more robust and the beneficiary relationship more symmetric.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(marginalized_populations_voice_integration, empirical, 'Whether affected-community voice is structural or performative in deployment decisions.').

omega_variable(
    audit_capability_bias_gap,
    'Can audit methodologies for algorithmic bias detect all consequential harms to marginalized populations, or are there categories of harm that testing and auditing cannot reliably identify before deployment?',
    'Technical research: stress-test audit methodologies against documented harms from deployed systems (ProPublica COMPAS cases, Buolamwini gender-shades, etc.); identify which harms the audits would have caught and which would have escaped. Theoretical analysis: determine whether certain classes of harm (emergent properties at scale, harms that manifest through interaction with social context rather than system behavior alone, second-order harms mediated through human interpretation of system outputs) are structurally auditable or require post-deployment monitoring.',
    'If audits are incomplete (cannot detect some harms), the deployment-gate regime provides only partial protection and the constraint''s effectiveness is lower than claimed. This would reduce both the functional extraction (the harm-prevention benefit) and the justification for suppressing capability research (the gate regime cannot guarantee justice, only reduce risk). If audits are comprehensive, the constraint''s coordination function is stronger and the resource extraction from capability research is better justified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(audit_capability_bias_gap, empirical, 'Whether pre-deployment audit methodologies can detect all consequential algorithmic harms.').

omega_variable(
    reading_boundary_ambiguity,
    'Is ''present discriminatory harm'' (the referent for this reading''s alignment concern) a stable category, or does it expand as audit methodologies improve and stakeholder awareness grows, such that the reading''s focal concern shifts over time?',
    'Historical trace: examine how the category ''algorithmic bias'' and its associated harm-universe have been defined and redefined over the interval (2015–present). Track whether early audit practices were focused on legally-protected categories (race, gender, disability) and whether the category has expanded to include intersectional harms, disparities in aggregate, second-order social harms, etc. Project forward: what would ''present harm'' encompass in 5 and 10 years if the expansion trajectory continues.',
    'If the category is stable, the reading''s beneficiary set and extraction targets are stable and the constraint is classifiable. If the category expands indefinitely, the reading''s scope is unbounded and may eventually encompass all AI development as ''harm-producing unless proved otherwise,'' which would shift the constraint toward snare (everything is framed as harm unless constrained). The expansion also increases suppression: if the harm-universe grows, more systems require audits, more research is constrained, and the institutional extraction rises.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_boundary_ambiguity, conceptual, 'Whether ''present discriminatory harm'' is a stable or expanding category that shifts the reading''s referent over time.').

omega_variable(
    reading_foreclosure_conditions,
    'Under what empirical or normative conditions would this reading (nearterm-harms priority) logically FORECLOSE the existential-risk reading? Under what conditions can they coexist?',
    'Logical analysis: map the core premises of each reading and identify contradictory pairs. The nearterm reading asserts: (1) present documented harms are high-certainty; (2) justice for current victims is a priority; (3) deployment should be constrained until harms are mitigated. The existential reading asserts: (1) catastrophic loss-of-control risks are high-severity (even if low-probability); (2) long-horizon existential safety is a priority; (3) research toward alignment should focus on speculative long-horizon scenarios. Identify: do these premises contradict (foreclose), or can a unified framework hold both? When does ''both matter'' become unstable?',
    'If foreclosure is genuine (one reading''s premises logically exclude the other''s), then this is a true kernel conflict where the sibling readings cannot coexist in any single framework — one framework chooses nearterm, another chooses existential. If coexistence is possible (a framework can hold both as complementary), then the conflict is not logical but distributional (both matter, but how much resource and priority each receives is a separate question). This affects the reading_relations claim in cs_structure.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_foreclosure_conditions, conceptual, 'Whether the nearterm and existential readings logically foreclose each other or can coexist in principle.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_alignment_priority__nearterm_harms_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_a_tr_t0, ai_alignment_priority__nearterm_harms_reading, theater_ratio, 0, 0.32).
narrative_ontology:measurement(ai_a_tr_t4, ai_alignment_priority__nearterm_harms_reading, theater_ratio, 4, 0.36).
narrative_ontology:measurement(ai_a_tr_t8, ai_alignment_priority__nearterm_harms_reading, theater_ratio, 8, 0.41).
narrative_ontology:measurement(ai_a_tr_t12, ai_alignment_priority__nearterm_harms_reading, theater_ratio, 12, 0.45).
narrative_ontology:measurement(ai_a_tr_t16, ai_alignment_priority__nearterm_harms_reading, theater_ratio, 16, 0.47).
narrative_ontology:measurement(ai_a_tr_t20, ai_alignment_priority__nearterm_harms_reading, theater_ratio, 20, 0.48).

% Extraction over time
narrative_ontology:measurement(ai_a_be_t0, ai_alignment_priority__nearterm_harms_reading, base_extractiveness, 0, 0.64).
narrative_ontology:measurement(ai_a_be_t4, ai_alignment_priority__nearterm_harms_reading, base_extractiveness, 4, 0.69).
narrative_ontology:measurement(ai_a_be_t8, ai_alignment_priority__nearterm_harms_reading, base_extractiveness, 8, 0.74).
narrative_ontology:measurement(ai_a_be_t12, ai_alignment_priority__nearterm_harms_reading, base_extractiveness, 12, 0.78).
narrative_ontology:measurement(ai_a_be_t16, ai_alignment_priority__nearterm_harms_reading, base_extractiveness, 16, 0.8).
narrative_ontology:measurement(ai_a_be_t20, ai_alignment_priority__nearterm_harms_reading, base_extractiveness, 20, 0.81).

% Suppression requirement over time
narrative_ontology:measurement(ai_a_su_t0, ai_alignment_priority__nearterm_harms_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(ai_a_su_t4, ai_alignment_priority__nearterm_harms_reading, suppression_requirement, 4, 0.63).
narrative_ontology:measurement(ai_a_su_t8, ai_alignment_priority__nearterm_harms_reading, suppression_requirement, 8, 0.68).
narrative_ontology:measurement(ai_a_su_t12, ai_alignment_priority__nearterm_harms_reading, suppression_requirement, 12, 0.72).
narrative_ontology:measurement(ai_a_su_t16, ai_alignment_priority__nearterm_harms_reading, suppression_requirement, 16, 0.75).
narrative_ontology:measurement(ai_a_su_t20, ai_alignment_priority__nearterm_harms_reading, suppression_requirement, 20, 0.76).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_alignment_priority__nearterm_harms_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(ai_alignment_priority__nearterm_harms_reading, 0.18).
narrative_ontology:affects_constraint(ai_alignment_priority__nearterm_harms_reading, ai_alignment_priority__existential_risk_reading).
narrative_ontology:affects_constraint(ai_alignment_priority__nearterm_harms_reading, ai_alignment_priority__integrated_reading).

% DUAL FORMULATION NOTE:
% The 'AI alignment' kernel decomposes into three structurally distinct constraint stories corresponding to three competing readings. This story (nearterm-harms reading) differs from its siblings by (1) centering present documented harms to marginalized populations as the measure of alignment; (2) specifying victims as capability research and deployment schedules (constrained by justice audits); (3) high ε on the redirection of computational resources away from scaling. The existential-risk reading centers catastrophic loss-of-control as the measure of alignment, specifies no current marginalized victims (treats long-horizon risk as the prioritized concern), and treats existential-safety research as the beneficiary. The integrated reading claims both nearterm and existential concerns matter and are complementary rather than competing for resources. The three stories are linked by network.affects_constraints because each reading's institutional success (allocation of resources, research direction, deployment standards) directly constrains the others' operationalization. They share the same kernel (AI alignment as a commitment) but diverge on what alignment means, which victims matter, and how to prioritize. The ε-invariance principle requires separate constraint stories because measuring alignment by 'nearterm harm prevention' produces structurally different extractiveness (0.81, high) than measuring by 'existential safety' (different victims, different beneficiaries, different computational allocation patterns). Each reading gets its own constraint file, its own ε, its own stakeholders, its own six_questions answers; the committer frame (which reading is being instantiated) is documented in omegas and cs_structure rather than folded into the core constraint metrics.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ai_alignment_priority__nearterm_harms_reading, institutional, 0.82).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
