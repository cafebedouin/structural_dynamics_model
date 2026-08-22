% ============================================================================
% CONSTRAINT STORY: ai_alignment_commitment__integrated_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_alignment_commitment__integrated_reading, []).

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
 *   constraint_id: ai_alignment_commitment__integrated_reading
 *   human_readable: Integrated AI Alignment Commitment (Control + Justice Unified)
 *   domain: ai_governance/technology_ethics/risk_assessment
 *
 * SUMMARY:
 *   The integrated AI alignment reading asserts that effective alignment
 *   requires simultaneous, non-subordinated attention to both catastrophic
 *   control problems (loss of control over advanced systems) and justice
 *   problems (reproduction and amplification of present-day social harms).
 *   This reading rejects the false dichotomy where safety and justice are
 *   treated as competing or separable mandates. It constitutes a distinct
 *   constraint because it imposes a unified governance requirement on
 *   research and development that differs structurally from either siloed
 *   frame: siloed safety treats justice as downstream; siloed justice treats
 *   control as abstract; integration requires both as primary from inception.
 *   The victim set includes siloed researchers (identity-locked into single
 *   frames) and marginalized populations (whose justice concerns are now
 *   instrumentalized into control architectures). This is ONE READING of the
 *   contested kernel 'ai_alignment_commitment'; sibling readings
 *   (safety_control_reading, ethics_justice_reading) share the kernel (what
 *   constitutes alignment) but instantiate different constraints with
 *   different victim sets and different extractiveness profiles.
 *
 * KEY AGENTS:
 *   - unified_alignment_research_programs: institutional agenda-setter, collects legitimacy and research resources from coordination mandate
 *   - siloed_safety_researchers: moderate power, identity-locked exit, payer (forced to justify or absorb justice-adjacent work)
 *   - siloed_ethics_researchers: moderate power, identity-locked exit, payer (forced to adopt control timescales and rigor standards)
 *   - marginalized_populations_present: powerless, trapped exit, nominally beneficiary but structurally subordinate to control timelines and research priorities
 *   - future_humanity: powerless, civilizational time horizon, nominally beneficiary but unrepresentable in present institutional structures
 *   - safety_control_reading_proponents: excluded by the integrated frame's rejection of false dichotomy; their position is treated as dichotomous thinking
 *   - ethics_justice_reading_proponents: excluded by the integrated frame's subordination of justice urgency to control coordination; present harms are deferred by control-first timelines
 *   - funding_bodies and academic_institutions: agenda-setters that enforce the integrated frame through resource allocation and career incentives
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_alignment_commitment__integrated_reading, 0.68).
domain_priors:suppression_score(ai_alignment_commitment__integrated_reading, 0.71).
domain_priors:theater_ratio(ai_alignment_commitment__integrated_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_alignment_commitment__integrated_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(ai_alignment_commitment__integrated_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(ai_alignment_commitment__integrated_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_alignment_commitment__integrated_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(ai_alignment_commitment__integrated_reading, resistance, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_alignment_commitment__integrated_reading, tangled_rope).
narrative_ontology:human_readable(ai_alignment_commitment__integrated_reading, "Integrated AI Alignment Commitment (Control + Justice Unified)").
narrative_ontology:topic_domain(ai_alignment_commitment__integrated_reading, "ai_governance/technology_ethics/risk_assessment").

domain_priors:requires_active_enforcement(ai_alignment_commitment__integrated_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_alignment_commitment__integrated_reading, '13d041ad-9eee-4a04-840e-3fa0ac951ac6').
narrative_ontology:cs_kernel_codification('13d041ad-9eee-4a04-840e-3fa0ac951ac6', distributed).
narrative_ontology:cs_authority_grounding('13d041ad-9eee-4a04-840e-3fa0ac951ac6', distributed).
narrative_ontology:cs_reading_relation('13d041ad-9eee-4a04-840e-3fa0ac951ac6', ai_alignment_commitment__safety_control_reading, coexists_with).
narrative_ontology:cs_reading_relation('13d041ad-9eee-4a04-840e-3fa0ac951ac6', ai_alignment_commitment__ethics_justice_reading, coexists_with).
narrative_ontology:cs_axiom('13d041ad-9eee-4a04-840e-3fa0ac951ac6', foundational, control_and_justice_are_structurally_complementary).
narrative_ontology:cs_axiom_status(control_and_justice_are_structurally_complementary, holdable).
narrative_ontology:cs_axiom_grounding('13d041ad-9eee-4a04-840e-3fa0ac951ac6', control_and_justice_are_structurally_complementary, deontological).
narrative_ontology:cs_axiom('13d041ad-9eee-4a04-840e-3fa0ac951ac6', foundational, false_dichotomy_rejection_is_methodologically_sound).
narrative_ontology:cs_axiom_status(false_dichotomy_rejection_is_methodologically_sound, holdable).
narrative_ontology:cs_axiom_grounding('13d041ad-9eee-4a04-840e-3fa0ac951ac6', false_dichotomy_rejection_is_methodologically_sound, instrumental).
narrative_ontology:cs_axiom('13d041ad-9eee-4a04-840e-3fa0ac951ac6', secondary, unified_institutional_enforcement_enables_integration).
narrative_ontology:cs_axiom_status(unified_institutional_enforcement_enables_integration, holdable).
narrative_ontology:cs_axiom_grounding('13d041ad-9eee-4a04-840e-3fa0ac951ac6', unified_institutional_enforcement_enables_integration, empirically_contingent).
narrative_ontology:cs_reference_frame('13d041ad-9eee-4a04-840e-3fa0ac951ac6', siloed_research_bifurcation).
narrative_ontology:cs_drift_state('13d041ad-9eee-4a04-840e-3fa0ac951ac6', contemporary_integrated_mandate_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('13d041ad-9eee-4a04-840e-3fa0ac951ac6', '').
narrative_ontology:cs_kernel_id(ai_alignment_commitment__integrated_reading, ai_alignment_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_alignment_commitment__integrated_reading, unified_alignment_research_programs).
narrative_ontology:constraint_beneficiary(ai_alignment_commitment__integrated_reading, marginalized_populations_present).
narrative_ontology:constraint_beneficiary(ai_alignment_commitment__integrated_reading, future_humanity).
narrative_ontology:constraint_victim(ai_alignment_commitment__integrated_reading, siloed_safety_researchers).
narrative_ontology:constraint_victim(ai_alignment_commitment__integrated_reading, siloed_ethics_researchers).
narrative_ontology:constraint_victim(ai_alignment_commitment__integrated_reading, resource_constrained_justice_advocates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(ai_alignment_commitment__integrated_reading, marginalized_populations_present).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Research teams and institutions that have organized around integrated control-and-justice frameworks gain legitimacy, funding coordination, and institutional primacy. They set research agendas, define what counts as alignment-relevant work, and determine how resources are allocated between control and justice research. They are not constrained by the integration requirement; they define it.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__integrated_reading, unified_alignment_research_programs, beneficiary,
    institutional, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(ai_alignment_commitment__integrated_reading, unified_alignment_research_programs, agenda_setter).

% Technical researchers trained in AI safety (adversarial robustness, interpretability, scalable oversight) face institutional pressure to integrate justice considerations into their research frames, funding proposals, and publication outlets. Journals and conferences now expect justice-adjacent discussion even for narrowly technical safety papers. Funding agencies score proposals lower if justice implications are not addressed. Career advancement increasingly requires demonstrating engagement with justice dimensions. Exit from safety research means abandoning established expertise and credentials.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__integrated_reading, siloed_safety_researchers, payer,
    moderate, biographical, identity_locked, global).

% Applied ethics and fairness researchers face institutional pressure to adopt technical rigor standards, engage with control-oriented timescales, and demonstrate how their work contributes to long-horizon catastrophic-risk prevention. Their empirical base (social science, ethnography, participatory design) is treated as insufficiently rigorous without control-relevance framing. Funding for justice research increasingly requires dual-track outputs (control-relevant applications). Career legitimacy depends on being legible to control-focused institutional structures.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__integrated_reading, siloed_ethics_researchers, payer,
    moderate, biographical, identity_locked, global).

% Communities currently harmed by biased, discriminatory, and manipulative AI systems theoretically gain from the integrated frame's centering of justice concerns. Their harms are now treated as alignment-relevant rather than downstream. However, they pay costs when research participation is required without commensurate benefit-sharing, when their concerns are instrumentalized for control research purposes, and when urgent present-harm mitigation is deferred by control timelines (e.g., deployment is accelerated for control testing, compounding exposure to bias). Their voice in determining research priorities and institutional resource allocation is minimal.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__integrated_reading, marginalized_populations_present, beneficiary,
    powerless, biographical, trapped, global).
narrative_ontology:stakeholder_secondary_role(ai_alignment_commitment__integrated_reading, marginalized_populations_present, payer).

% Present and future humans at risk from catastrophic AI failure gain from integrated alignment work that does not bifurcate safety and justice. A fragmented field risks leaving injustice-shaped failure modes (e.g., an unjust system that does exactly what it was designed to do) unaddressed in the control frame. However, future humans cannot advocate, negotiate, or contest how their interests are represented. Their interests are mediated entirely by present institutional actors.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__integrated_reading, future_humanity, beneficiary,
    powerless, civilizational, trapped, universal).

% Researchers and institutions (e.g., technical AI safety labs) that prioritize catastrophic-loss-of-control as the primary or sole alignment problem resist integration with justice concerns. They argue that diluting safety focus with justice mandates undermines technical rigor, introduces incommensurable value dimensions, and delays urgent risk mitigation. Their position — that control and justice are separable problems requiring separate institutional structures — is treated by the integrated frame as false dichotomy thinking and is systematically excluded from integrated governance structures. Their exclusion is structural, not individual (it applies to anyone holding the position), so exit requires changing their core research commitments.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__integrated_reading, safety_control_reading_proponents, excluded,
    institutional, generational, constrained, global).

% Researchers and advocates (e.g., critical algorithm studies, participatory justice movements, racial justice technologists) that prioritize prevention of present-day AI harms as the primary alignment problem argue that integration subordinates urgent justice work to abstract future catastrophe scenarios. They see integration as instrumentalization: justice becomes a value-add to control research rather than an autonomous research domain. Their position — that justice deserves independent institutional priority — conflicts with the integrated frame's claim that both must be simultaneous. They are excluded from integrated governance by the frame's rejection of false dichotomy.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__integrated_reading, ethics_justice_reading_proponents, excluded,
    moderate, biographical, constrained, global).

% Foundations, government research agencies, and corporate innovation budgets adopt the integrated frame as a governance requirement. They allocate funding through the lens of control-and-justice complementarity, require funded research to address both dimensions, and use funding decisions to shape research priorities and institutional structures. They coordinate resource flows to enforce integration across the research ecosystem.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__integrated_reading, funding_bodies, agenda_setter,
    institutional, generational, mobile, global).

% Universities, research institutes, and academic departments embed the integrated frame in hiring criteria, promotion standards, degree requirements, and research expectations. They enforce interdisciplinary coordination through committee structures, co-advising requirements, and publication venue expectations. They reshape entire career paths around the unified frame.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__integrated_reading, academic_institutions, agenda_setter,
    institutional, generational, mobile, global).

% Governments and regulatory bodies adopt the integrated frame as a requirement for AI development, incorporating both safety and justice into licensing conditions, accountability frameworks, and regulatory mandates. They use the frame to justify consolidation of previously separate regulatory domains (safety regulators + fairness regulators) under unified governance.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__integrated_reading, policy_makers, agenda_setter,
    institutional, generational, mobile, national).

% Examines the constraint from outside the immediate institutional contest: whether the integrated frame represents genuine progress in alignment thinking or false synthesis that instrumentalizes justice in service to control objectives while extracting researcher autonomy and subordinating present-harm prevention.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__integrated_reading, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ai_alignment_commitment__integrated_reading, unified_alignment_research_programs).
narrative_ontology:fixing_cost_class(ai_alignment_commitment__integrated_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Eliminates siloed research blind spots by requiring control and justice researchers to consider each other's concerns from the start, preventing scenarios where safety advances are deployed into unjust systems or justice improvements ignore catastrophic risk factors. Enforces institutional coordination across previously separate research domains.
% TRANSFER_FUNCTION: Moves institutional authority, research priority-setting, and career advancement opportunities from individual siloed researchers to unified research programs and cross-disciplinary teams. Redistributes legitimacy and funding toward programs that can demonstrate integration across control and justice dimensions. Extracts identity-commitment from siloed researchers, who must now justify single-domain focus or reframe their work.
% ABSENT_VOICES: Researchers who believe control and justice are fundamentally separable problems and should maintain institutional independence; control-focused technologists who see justice as a constraint on progress; justice-focused advocates who see control framing as abstract and present-harm deferral; siloed researchers whose expertise has been devalued by integration requirements; future-humanity representatives who are not themselves researchers or policy makers.
% DISAPPEARANCE_RATIONALE: If the integrated commitment vanished, research and policy would revert to separate institutional tracks: safety research would deprioritize justice concerns as out-of-scope, justice research would work without pressure to address control scenarios, funding bodies would allocate by domain separately. Institutional structures would reorganize into separate safety agencies and fairness agencies rather than unified governance. Career incentives would decouple from the requirement to integrate across domains.
% FOUNDING_PROBLEM: Early AI alignment discourse bifurcated into control-focused and justice-focused research communities with minimal structural engagement, producing blind spots: control approaches ignored how unjust systems amplify harm at scale; justice approaches overlooked how control failure could corrupt any system. Siloed research prevented recognition that alignment requires both dimensions simultaneously non-hierarchically.
% FOUNDING_PROBLEM_CORROBORATION: Institutional analysts and interdisciplinary researchers document the original siloing and blind spots of purely control-focused or purely justice-focused approaches. However, control-focused researchers argue integration dilutes safety focus and timescale urgency, while justice-focused researchers argue integration subordinates present harm to abstract futures. No external validator has independently certified that integration solves the founding problem rather than trading one set of blind spots for another.
narrative_ontology:disappearance_verdict(ai_alignment_commitment__integrated_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_alignment_commitment__integrated_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_alignment_commitment__integrated_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(ai_alignment_commitment__integrated_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_alignment_commitment__integrated_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_alignment_commitment__integrated_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_alignment_commitment__integrated_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ai_alignment_commitment__integrated_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.48 to 0.68 over the interval because the unified commitment initially provides genuine coordination benefit (breaking siloes, preventing blind spots) but accumulates extraction as institutional power consolidates around the frame: funding bodies use integration as a requirement to reshape research priorities; academic institutions use it to enforce hiring and curriculum changes; policy makers use it to justify regulatory consolidation. Siloed researchers face identity-locked exit because decades of career investment are suddenly incomplete — their expertise is now treated as parochial unless reframed. Marginalized populations gain nominal beneficiary status but pay costs when research attention is subordinated to control timelines (study participation for control research is required; compensation for justice research is deferred). Theater rises from 0.25 to 0.42 because institutional enforcement of integration becomes increasingly performative: programs adopt dual-track language to satisfy requirements while maintaining internal priority hierarchies; integration is performed in public-facing research statements while internal resource allocation remains control-dominant. Suppression requirement rises from 0.52 to 0.71 as the integrated frame must actively suppress competing frames (excluding siloed researchers, marginalizing control-skeptics) to maintain its mandate as primary. The measurement series is authored on a single shared time grid: every metric is valued at t=0,5,10,15,20,25.
 *
 * PERSPECTIVAL GAP:
 *   The unified research program sees the constraint as net-beneficial coordination that eliminates harmful siloing. Siloed safety researchers see it as institutional coercion that dilutes technical focus. Siloed justice researchers see it as instrumentalization that subordinates present harm. Marginalized populations see promise in justice centering but face subordination when that centering is subordinated to control timelines. Future humanity (unrepresentable) cannot object but stands to lose if integration is false synthesis rather than true unification. The engine computes these divergent seat classifications from the structural asymmetries: unified programs (agenda-setter, mobile exit) sit at beneficiary end; siloed researchers (moderate power, identity-locked exit) sit at target end; populations present and future sit nominally as beneficiaries but structurally as payers (their interests are instrumental to control, not primary).
 *
 * DIRECTIONALITY LOGIC:
 *   Unified alignment programs (institutional power, mobile exit) are structural beneficiaries: they collect legitimacy, funding, and institutional authority from the mandate to integrate. They are not forced into the frame; they lead its instantiation. Siloed safety researchers (moderate power, identity-locked exit) are structural targets: their accumulated expertise is treated as incomplete, their career identities are now at risk, their exit from the field (the only clean exit available) means abandoning decades of credentialing. Justice researchers face similar targeting but with the additional asymmetry that their work is now instrumentalized — they retain voice within the integrated structure but only insofar as their output serves control objectives. Marginalized populations are coded as beneficiaries (the frame centers justice) but sit at the payer end of directionality: their participation is extracted for research purposes, their timescale urgency (present harm) is subordinated to control timescale (catastrophic future), and their capacity to set priorities within the unified structure is minimal (powerless + global scope = near-zero institutional influence). Future humanity cannot exit or advocate; their directionality is extreme target (civilizational time horizon, universal scope, trapped exit, no voice).
 *
 * MANDATROPHY ANALYSIS:
 *   The integrated constraint avoids simple mandatrophy (a dead mandate persisting theatrically) because the founding problem (siloed alignment discourse) is genuinely solved — control and justice researchers DO coordinate more, DO consider each other's concerns, DO avoid the worst blind spots. The mandate is not phantom. However, the constraint exhibits what might be called 'instrumental mandatrophy': the founding problem is solved, but the institutional solution has accrued extraction (career coercion, research subordination, timeline subordination) that outweighs the coordination benefit. The theater ratio rising from 0.25 to 0.42 suggests that institutional enforcement increasingly focuses on policing the frame (ensuring justice talk is present) rather than enabling the actual coordination benefit. The suppression rising to 0.71 signals that the frame requires active effort to maintain — competing approaches (pure safety, pure justice, separable-problem frames) are not eliminated; they must be suppressed. This is tangled_rope classification rather than rope (pure coordination): the coordination function is real, but active enforcement of extraction (identity-locking researchers, subordinating timelines, centralizing priority-setting) is required to sustain it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    integration_or_subordination,
    'Is the measured integration of control and justice a genuine non-hierarchical union, or a subordination of justice concerns to control timelines and priorities?',
    'Audit research funding and publication patterns: does funding for justice-primary research match control-primary research? Do institutional reward structures (promotion, prestige) equally credit justice-primary contributions? Do policy outputs reflect equal weight for justice and control constraints, or do control constraints override justice when they conflict?',
    'If genuinely integrated (equal institutional weight, non-hierarchical), the constraint is tangled_rope with significant but balanced extraction. If subordinative, it is snare — the justice framing is cover for control prioritization, and the victim set is primarily justice researchers and present-harm communities.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(integration_or_subordination, empirical, 'Whether integration achieves non-hierarchical union or subordinates justice to control.').

omega_variable(
    timescale_compatibility,
    'Are control timescales (urgency increasing toward future catastrophic scenarios) and justice timescales (urgency in present harm prevention) structurally compatible within a unified mandate, or does integration require one to subordinate to the other?',
    'Case analysis of decisions where control urgency and justice urgency conflict (e.g., deployment timeline pressure vs. fairness validation deadline, resource allocation between control and bias research): which timescale wins in institutional priority-setting?',
    'If compatible (integration enables synchronized response), the constraint better fits rope. If incompatible (one always dominates), the unified frame is false synthesis and the constraint is snare masquerading as tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(timescale_compatibility, conceptual, 'Whether control and justice timescales can coexist non-hierarchically or whether unified framing requires systematic subordination of one.').

omega_variable(
    identity_lock_mechanism,
    'Why do siloed researchers face identity-locked exit rather than constrained exit? Is it because their credentialed expertise genuinely becomes unmarketable outside the integrated frame, or because institutional coercion (career penalties for siloed work) makes exit feel impossible even when alternative paths technically exist?',
    'Longitudinal tracking of researcher careers after institutional adoption of integrated mandates: do siloed researchers who change fields or institutions experience economic or prestige penalties relative to comparable researchers in integrated frames? Do ''siloed'' research programs persist in non-integrated institutions, or does the unified frame propagate globally such that no alternative institutional space exists?',
    'If genuinely identity-locked (credentials unmarketable), the constraint exhibits high suppression through structural obsolescence — the frame itself eliminates alternatives. If policy-locked (institutional penalties for non-integration), suppression is enforced coercion, which is cleaner to resist and potentially more unstable.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_mechanism, empirical, 'Whether exit-locking is structural or policy-enforced, and how stable the constraint is under researcher migration.').

omega_variable(
    future_humanity_representation,
    'Who legitimately represents the interests of future humanity in present institutional structures that enforce the integrated frame? Are future-oriented control researchers actually serving future interests, or are they instrumentalizing those interests to justify control prioritization?',
    'Normative analysis of representation legitimacy (philosophy of future-oriented ethics) combined with institutional audit: whose voices are cited as representatives of future interests? Do non-control stakeholders (justice advocates, affected communities) have equal standing to define what future humanity needs?',
    'If control researchers legitimately represent future interests, their extraction of institutional authority is justified by future beneficiary stakes. If representation is self-asserted and uncontested, future humanity is a phantom victim legitimizing present extraction, and the constraint is more extractive than measured.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(future_humanity_representation, preference, 'The legitimacy of unelected representation of unborn agents'' interests in present governance.').

omega_variable(
    false_dichotomy_or_real_tradeoff,
    'Is the rejection of the safety/justice dichotomy a recognition of genuine complementarity (control and justice enable each other), or a false synthesis that obscures real tradeoffs (some control architectures are unjust, some justice requirements compromise control resilience)?',
    'Technical and ethical analysis of specific AI systems: are there verifiable cases where control-optimized architectures conflict with justice-optimized architectures? Where do tradeoffs occur? Where do synergies occur?',
    'If genuine complementarity, the integrated frame reduces extractiveness (truly eliminates blind spots). If false synthesis obscuring tradeoffs, the frame is ideological cover and extractiveness is higher than measured (it is hiding rather than solving conflicts).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_dichotomy_or_real_tradeoff, conceptual, 'Whether integration is true complementarity or false synthesis of genuinely conflicting objectives.').

omega_variable(
    kernel_reading_divergence,
    'Does this reading (integrated) succeed in escaping the limitations of its sibling readings (safety-only, justice-only), or does it inherit the blind spots of both while adding the extraction cost of unified institutional enforcement?',
    'Comparative research outcome analysis: do integrated research programs produce outputs (technical safety advances, justice improvements) equivalent to high-performing siloed programs in each domain? Do they avoid the blind spots each sibling reading exhibits?',
    'If integration produces better outcomes across both dimensions, it is a net positive constraint (higher classification as rope/tangled_rope justified by outputs). If it produces mediocre outcomes in both domains while extracting researcher attention, it is primarily extractive (snare).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_divergence, empirical, 'Whether the integrated reading achieves its founding promise or represents a compromise that underperforms both component approaches.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_alignment_commitment__integrated_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_a_tr_t0, ai_alignment_commitment__integrated_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(ai_a_tr_t0, observed).
narrative_ontology:measurement(ai_a_tr_t5, ai_alignment_commitment__integrated_reading, theater_ratio, 5, 0.3).
narrative_ontology:measurement_basis(ai_a_tr_t5, observed).
narrative_ontology:measurement(ai_a_tr_t10, ai_alignment_commitment__integrated_reading, theater_ratio, 10, 0.35).
narrative_ontology:measurement_basis(ai_a_tr_t10, observed).
narrative_ontology:measurement(ai_a_tr_t15, ai_alignment_commitment__integrated_reading, theater_ratio, 15, 0.4).
narrative_ontology:measurement_basis(ai_a_tr_t15, observed).
narrative_ontology:measurement(ai_a_tr_t20, ai_alignment_commitment__integrated_reading, theater_ratio, 20, 0.41).
narrative_ontology:measurement_basis(ai_a_tr_t20, projected).
narrative_ontology:measurement(ai_a_tr_t25, ai_alignment_commitment__integrated_reading, theater_ratio, 25, 0.42).
narrative_ontology:measurement_basis(ai_a_tr_t25, projected).

% Extraction over time
narrative_ontology:measurement(ai_a_be_t0, ai_alignment_commitment__integrated_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(ai_a_be_t0, observed).
narrative_ontology:measurement(ai_a_be_t5, ai_alignment_commitment__integrated_reading, base_extractiveness, 5, 0.54).
narrative_ontology:measurement_basis(ai_a_be_t5, observed).
narrative_ontology:measurement(ai_a_be_t10, ai_alignment_commitment__integrated_reading, base_extractiveness, 10, 0.61).
narrative_ontology:measurement_basis(ai_a_be_t10, observed).
narrative_ontology:measurement(ai_a_be_t15, ai_alignment_commitment__integrated_reading, base_extractiveness, 15, 0.66).
narrative_ontology:measurement_basis(ai_a_be_t15, observed).
narrative_ontology:measurement(ai_a_be_t20, ai_alignment_commitment__integrated_reading, base_extractiveness, 20, 0.67).
narrative_ontology:measurement_basis(ai_a_be_t20, projected).
narrative_ontology:measurement(ai_a_be_t25, ai_alignment_commitment__integrated_reading, base_extractiveness, 25, 0.68).
narrative_ontology:measurement_basis(ai_a_be_t25, projected).

% Suppression requirement over time
narrative_ontology:measurement(ai_a_su_t0, ai_alignment_commitment__integrated_reading, suppression_requirement, 0, 0.52).
narrative_ontology:measurement_basis(ai_a_su_t0, observed).
narrative_ontology:measurement(ai_a_su_t5, ai_alignment_commitment__integrated_reading, suppression_requirement, 5, 0.58).
narrative_ontology:measurement_basis(ai_a_su_t5, observed).
narrative_ontology:measurement(ai_a_su_t10, ai_alignment_commitment__integrated_reading, suppression_requirement, 10, 0.65).
narrative_ontology:measurement_basis(ai_a_su_t10, observed).
narrative_ontology:measurement(ai_a_su_t15, ai_alignment_commitment__integrated_reading, suppression_requirement, 15, 0.7).
narrative_ontology:measurement_basis(ai_a_su_t15, observed).
narrative_ontology:measurement(ai_a_su_t20, ai_alignment_commitment__integrated_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement_basis(ai_a_su_t20, projected).
narrative_ontology:measurement(ai_a_su_t25, ai_alignment_commitment__integrated_reading, suppression_requirement, 25, 0.71).
narrative_ontology:measurement_basis(ai_a_su_t25, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_alignment_commitment__integrated_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(ai_alignment_commitment__integrated_reading, 0.18).
narrative_ontology:affects_constraint(ai_alignment_commitment__integrated_reading, ai_alignment_commitment__safety_control_reading).
narrative_ontology:affects_constraint(ai_alignment_commitment__integrated_reading, ai_alignment_commitment__ethics_justice_reading).

% DUAL FORMULATION NOTE:
% The ai_alignment_commitment kernel decomposes into three structurally distinct constraints, one per reading. All share the same referent (what constitutes AI alignment) but differ in what victim set is centered, what extractiveness mechanism operates, and what institutional enforcement structure is required. The integrated reading is downstream of both sibling readings in that it claims to synthesize them; however, it structurally influences both siblings by establishing the unified frame as institutional common sense, against which both safety-only and justice-only positions must now defend themselves as false dichotomies. This is not a simple hierarchy — each reading has institutional power and continues to attract researchers and funding — but a contest where the integrated reading has accrued institutional legitimacy that constrains the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ai_alignment_commitment__integrated_reading, powerless, 0.82).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
