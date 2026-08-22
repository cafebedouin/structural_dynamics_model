% ============================================================================
% CONSTRAINT STORY: ai_alignment_commitment__ethics_justice_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: ai_alignment_commitment__ethics_justice_reading
 *   human_readable: AI Alignment as Present-Harm Prevention (Ethics-Justice Reading)
 *   domain: ai_governance/technology_ethics
 *
 * SUMMARY:
 *   The AI alignment commitment is a contested kernel: 'alignment' can mean
 *   preventing reproduction of social bias (this reading), preventing
 *   catastrophic loss of control (safety-control reading), or simultaneously
 *   attending to both (integrated reading). This constraint story
 *   instantiates the ethics-justice reading only: alignment means ensuring AI
 *   systems do not amplify or reproduce historical bias against marginalized
 *   populations, do not displace workers without remediation, and do not
 *   enable discriminatory surveillance. The reading emerges from documented
 *   harms in lending, employment, criminal justice, and content moderation.
 *   It redefines whose research counts as 'alignment work' (ethics
 *   researchers gain prestige), what resources flow where (fairness auditing
 *   over control theory), and who bears voice in policy (marginalized
 *   community advocates gain standing). The constraint is presented as
 *   coordination for justice; simultaneously, it extracts from long-horizon
 *   safety research and speeds advocates by redefining the scope of what
 *   counts as alignment-relevant work. The reading is live in policy bodies
 *   (EU AI Act, US fairness frameworks) and powerful in ethics-adjacent
 *   research communities, but contested by control-focused researchers and
 *   capability developers.
 *
 * KEY AGENTS:
 *   - Marginalized communities vulnerable to algorithmic bias: primary beneficiaries, trapped exit, bearers of immediate documented harms.
 *   - AI ethics researchers and advocates: agenda-setters who define the boundary between alignment and other concerns; benefit from prestige and funding redirection.
 *   - Long-horizon safety researchers: payers, excluded from the definitional boundary; lose prestige and resources when alignment redefines to present-harm focus.
 *   - AI capability developers: payers (fairness costs), dual-positioned beneficiaries (legal defense via fairness).
 *   - Policy and regulatory bodies: observers; their acceptance of the reading determines whether fairness becomes a compliance obligation.
 *   - Data subjects and workers: beneficiaries who gain framework that centers their harms as foundational to what systems should do.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_alignment_commitment__ethics_justice_reading, 0.68).
domain_priors:suppression_score(ai_alignment_commitment__ethics_justice_reading, 0.72).
domain_priors:theater_ratio(ai_alignment_commitment__ethics_justice_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_alignment_commitment__ethics_justice_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(ai_alignment_commitment__ethics_justice_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(ai_alignment_commitment__ethics_justice_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_alignment_commitment__ethics_justice_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(ai_alignment_commitment__ethics_justice_reading, resistance, 0.73).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_alignment_commitment__ethics_justice_reading, tangled_rope).
narrative_ontology:human_readable(ai_alignment_commitment__ethics_justice_reading, "AI Alignment as Present-Harm Prevention (Ethics-Justice Reading)").
narrative_ontology:topic_domain(ai_alignment_commitment__ethics_justice_reading, "ai_governance/technology_ethics").

domain_priors:requires_active_enforcement(ai_alignment_commitment__ethics_justice_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_alignment_commitment__ethics_justice_reading, 'a45e2776-543e-4640-b4b0-a98185278913').
narrative_ontology:cs_kernel_codification('a45e2776-543e-4640-b4b0-a98185278913', fixed_text).
narrative_ontology:cs_authority_grounding('a45e2776-543e-4640-b4b0-a98185278913', extraction).
narrative_ontology:cs_interpretation_layer_present('a45e2776-543e-4640-b4b0-a98185278913').
narrative_ontology:cs_reading_relation('a45e2776-543e-4640-b4b0-a98185278913', ai_alignment_commitment__safety_control_reading, coexists_with).
narrative_ontology:cs_reading_relation('a45e2776-543e-4640-b4b0-a98185278913', ai_alignment_commitment__integrated_reading, influences).
narrative_ontology:cs_axiom('a45e2776-543e-4640-b4b0-a98185278913', foundational, algorithmic_harm_is_immediate).
narrative_ontology:cs_axiom_status(algorithmic_harm_is_immediate, holdable).
narrative_ontology:cs_axiom_grounding('a45e2776-543e-4640-b4b0-a98185278913', algorithmic_harm_is_immediate, empirically_contingent).
narrative_ontology:cs_axiom('a45e2776-543e-4640-b4b0-a98185278913', foundational, justice_concerns_are_alignment_concerns).
narrative_ontology:cs_axiom_status(justice_concerns_are_alignment_concerns, holdable).
narrative_ontology:cs_axiom_grounding('a45e2776-543e-4640-b4b0-a98185278913', justice_concerns_are_alignment_concerns, deontological).
narrative_ontology:cs_reference_frame('a45e2776-543e-4640-b4b0-a98185278913', alignment_as_present_justice).
narrative_ontology:cs_drift_state('a45e2776-543e-4640-b4b0-a98185278913', contemporary_policy_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('a45e2776-543e-4640-b4b0-a98185278913', '2026-06-11T14:32:15Z').
narrative_ontology:cs_kernel_id(ai_alignment_commitment__ethics_justice_reading, ai_alignment_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_alignment_commitment__ethics_justice_reading, marginalized_communities_vulnerable_to_bias).
narrative_ontology:constraint_beneficiary(ai_alignment_commitment__ethics_justice_reading, workers_displaced_by_automation).
narrative_ontology:constraint_beneficiary(ai_alignment_commitment__ethics_justice_reading, data_subjects_in_surveillance_systems).
narrative_ontology:constraint_victim(ai_alignment_commitment__ethics_justice_reading, long_term_safety_research_funders).
narrative_ontology:constraint_victim(ai_alignment_commitment__ethics_justice_reading, control_problem_researchers).
narrative_ontology:constraint_victim(ai_alignment_commitment__ethics_justice_reading, ai_development_speed_advocates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(ai_alignment_commitment__ethics_justice_reading, ai_capability_developers).
narrative_ontology:constraint_victim(ai_alignment_commitment__ethics_justice_reading, ai_capability_developers).
narrative_ontology:constraint_vindicates(ai_alignment_commitment__ethics_justice_reading, algorithmic_harm_is_immediate).
narrative_ontology:constraint_vindicates(ai_alignment_commitment__ethics_justice_reading, justice_concerns_are_alignment_concerns).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Subjected to biased algorithmic decisions in lending, employment screening, criminal risk assessment, and hiring across jurisdictions. They bear the immediate, measurable harms of reproduced historical bias when systems are trained on biased data or deployed without fairness testing. Their exit from algorithmic systems is not optional — financial systems, government services, and employment markets all embed these systems. They benefit when alignment is defined as prevention of demonstrated present-day bias.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__ethics_justice_reading, marginalized_communities_vulnerable_to_bias, beneficiary,
    powerless, biographical, trapped, global).

% Experience labor market disruption from automation systems that may or may not be aligned with long-term safety constraints. They benefit when alignment prioritizes fairness in automation deployment, worker retraining, and preservation of labor market participation. Their alternative is accepting displacement without remediation or organizing for policy intervention.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__ethics_justice_reading, workers_displaced_by_automation, beneficiary,
    organized, biographical, constrained, global).

% Subjected to algorithmic surveillance and decision-making (credit scoring, insurance pricing, content moderation, law enforcement) whose bias directly harms them. They cannot opt out without abandoning social participation. They benefit when alignment frameworks prioritize prevention of demonstrated bias in these systems now.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__ethics_justice_reading, data_subjects_in_surveillance_systems, beneficiary,
    powerless, biographical, identity_locked, global).

% Fund and direct research into alignment as long-horizon catastrophic risk reduction (superintelligence control, goal misalignment, instrumental convergence). They argue that diverting resources and attention to present-day fairness concerns dilutes focus on existential risk. Their resources could flow to other research directions; they remain in the constraint's framing only while they accept the ethics-justice reading's priority ordering.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__ethics_justice_reading, long_term_safety_research_funders, payer,
    institutional, generational, mobile, global).

% Develop technical approaches to AI control, formal verification, and alignment as superintelligence safety. They experience the ethics-justice reading as redefining their research area's scope and urgency metrics: alignment becomes a present-harm problem rather than a control-at-scale problem. Their research programs and career trajectories are indexed to the long-horizon safety framing; reindexing to present harms redistributes funding and prestige within the research community.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__ethics_justice_reading, control_problem_researchers, payer,
    powerful, generational, mobile, global).

% Industry and policy figures who argue that faster AI deployment and capability advancement are net beneficial despite present-day alignment risks. They bear a cost when alignment is defined to include demonstrated fairness in deployed systems: bias testing, fairness audits, and diversity in training data all slow deployment cycles. Their interest lies in speed-first framings where alignment is controlled research only, not a present-deployment constraint.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__ethics_justice_reading, ai_development_speed_advocates, payer,
    powerful, biographical, mobile, global).

% Scholars and practitioners who have established AI fairness, interpretability, and ethics as a recognized subfield. They set research agendas, write policy recommendations, and define what counts as alignment work. This reading amplifies their authority: alignment becomes definitionally tied to the present-harm prevention work their field already does. They administer the boundary between 'alignment' and 'other concerns' and stand to gain research prestige, funding, and policy influence from the scope redefinition.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__ethics_justice_reading, ai_ethics_researchers_and_advocates, agenda_setter,
    institutional, generational, mobile, global).

% Build and deploy AI systems in production. They benefit from faster time-to-market and looser fairness constraints (speeds feature development). They pay when alignment is defined to include bias prevention: bias auditing, fairness testing, and demographic parity evaluation all add cost and latency. Yet they also benefit insofar as demonstrable fairness provides legal defense and consumer trust — so they are dual-positioned: extraction on speed grounds, benefit on reputational/legal grounds.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__ethics_justice_reading, ai_capability_developers, payer,
    powerful, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(ai_alignment_commitment__ethics_justice_reading, ai_capability_developers, beneficiary).

% Regulate AI systems, write obligations around fairness and bias prevention. They observe the definitional contest over alignment and take testimony from other seats. Their decisions about what counts as alignment-relevant determine whether firms must invest in fairness testing as a compliance matter (present-harm reading supported) or whether existential safety research takes priority in policy (long-horizon reading supported).
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__ethics_justice_reading, policy_and_regulatory_bodies, observer,
    institutional, generational, analytical, national).

% Research programs focused on long-horizon control problems, formal verification of advanced systems, and alignment as a foundational theory question are structurally excluded from the ethics-justice reading's scope of what 'alignment' means. If they wanted voice in the definitional contest, they would argue that safety is distinct from justice and that misaligning the research agendas wastes limited resources on both sides. They remain outside because the reading's definitional boundary is maintained by the agenda-setters.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__ethics_justice_reading, displaced_alignment_research_agendas, excluded,
    powerless, generational, trapped, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ai_alignment_commitment__ethics_justice_reading, ai_ethics_researchers_and_advocates).
narrative_ontology:fixing_cost_class(ai_alignment_commitment__ethics_justice_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes shared commitment that AI systems should not reproduce or amplify documented social biases and present-day harms to marginalized populations, with uniform standards for fairness testing, bias auditing, and demographic representation in training data.
% TRANSFER_FUNCTION: Redirects research funding, engineering labor, and policy attention from long-horizon catastrophic-risk problems to present-deployment fairness work; reshapes whose expertise counts as 'alignment expertise' (ethics researchers gain prestige relative to control theorists); redistributes development cost toward bias detection and fairness measurement.
% ABSENT_VOICES: Researchers focused on long-horizon control problems and foundational alignment theory (formal verification, goal misalignment, scalable oversight) are excluded from the definitional boundary-setting work; they would argue that alignment should remain orthogonal to justice, that resources diverted to present-fairness threaten existential risk work, and that the two domains require distinct expertise communities. They are kept out by the reading's scope definition itself.
% DISAPPEARANCE_RATIONALE: If this reading and its enforcement disappeared, the alignment research agenda would reindex back to long-horizon control problems; fairness work would continue but would no longer carry the prestige and funding flow that 'alignment commitment' status confers; deployment of biased systems would accelerate absent regulatory enforcement; marginalized communities would lose a framework that centers their harms as fundamental to what systems should do.
% FOUNDING_PROBLEM: AI systems reproduce and amplify the historical biases embedded in their training data and the social biases of their designers, causing immediate, measurable, documented harm to marginalized people in lending, employment, criminal justice, and surveillance contexts.
% FOUNDING_PROBLEM_CORROBORATION: Marginalized communities experiencing algorithmic bias attest the problem is live and urgent; independent researchers (Buolamwini, Gebru, Noble, among others) have documented systemic bias in commercial systems; civil rights organizations and policy bodies have entered regulatory proceedings against biased AI. The problem's status is contested only by those whose interests lie in speed or whose research agenda is decoupled from fairness — long-horizon safety researchers do not deny the bias problem exists, they dispute whether it should be called 'alignment.'
narrative_ontology:disappearance_verdict(ai_alignment_commitment__ethics_justice_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_alignment_commitment__ethics_justice_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_alignment_commitment__ethics_justice_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(ai_alignment_commitment__ethics_justice_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_alignment_commitment__ethics_justice_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is high (0.68) because the reading redistributes research authority, funding, and prestige away from long-horizon safety toward present-fairness; it redefines whose expertise counts and what problems matter. This is extraction from one research community because it is funded through the same budget pools, institutional prestige systems, and policy attention. Suppression is high (0.72) because the reading enforces an exclusionary boundary: long-horizon research is structurally kept out of 'alignment' definitional space via scope restriction, not via refutation. Theater is moderate (0.41) because some fairness work is performative (bias audits that don't change deployment) while much is functional (real fairness testing that prevents harms). The measurement trajectory shows extraction and suppression rising through t=12 as the reading gains institutional acceptance, then plateauing (t=16 to t=20) as the boundary stabilizes. Accessibility collapse is low (0.48) because long-horizon researchers and speed advocates retain institutional homes and continue their work even without 'alignment' label — the alternatives (control-focused institutes, capability development labs) remain accessible. Resistance is high (0.73) because long-horizon researchers actively push back against the definitional redirection, capability developers contest fairness mandates, and the boundary is actively defended rather than passively accepted.
 *
 * PERSPECTIVAL GAP:
 *   The ethics-researcher agenda-setter seat experiences this as genuine coordination for justice — they see marginalized communities as the primary stakeholders and alignment as correctly reframed around present harm. From this seat, long-horizon researchers are missing the point and extracting resources from urgent work. The long-horizon researcher seat experiences this as definitional capture — their foundational work on control problems is excluded from 'alignment' definitional space, funding is redirected, and prestige flows away from their work. From this seat, the ethics reading is capturing the alignment label for a different problem. The capability developer seat experiences this as cost imposition — fairness testing adds latency and expense. The marginalized community seat experiences this as finally having their documented harms centered in a framework that was previously orthogonal to them. The engine computes each seat's type from the structural data: the agenda-setter should compute as a rope (coordination + prestige flow); the excluded long-horizon seat should compute as a snare victim (exclusion + extraction); the beneficiary seats should compute as ropers (they benefit from the coordination).
 *
 * DIRECTIONALITY LOGIC:
 *   Ethics researchers and advocates (institutional power, mobile exit) have low d near 0.2–0.3: they are agenda-setters who define the boundary, so they stand to gain authority and resources. Long-horizon safety researchers (powerful, mobile exit) have high d near 0.65–0.75: the reading extracts prestige and funding by redefining alignment scope. Marginalized communities (powerless, trapped exit) have low d near 0.15: they are structural beneficiaries. Capability developers (powerful, constrained exit because they operate in regulated jurisdictions) have d near 0.55–0.65: they pay fairness costs but benefit from legal defense and consumer trust via fairness. The directionality derivation from beneficiary/victim declarations feeds the engine's effective extraction computation: marginalized communities as beneficiaries move their d downward (subsidy effect), safety researchers as victims move their d upward (extraction effect).
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's founding problem (AI systems reproduce social bias, causing documented harms) is live and actively attested by affected communities and independent researchers. The founding problem's status is not dead — the problem persists in deployed systems. However, there is a mandatrophy risk: if the definition of 'alignment' drifts such that long-horizon safety concerns are treated as cosmetic (solved by attending to fairness), then the founding problem of alignment work itself (ensuring systems do what we actually intend them to do at scale) becomes orphaned. The constraint's structure (present-harm focus) may not solve this orphaned problem; it may instead cannibalize resources that would address it. The constraint is not classified as piton because it has clear beneficiaries (marginalized communities) and an active agenda-setter (ethics researchers); piton requires diffuse payees and no concentrated beneficiary. But mandatrophy monitoring is warranted: if the reading gains enough institutional acceptance that long-horizon research becomes unfunded orphans, and if present fairness work proves insufficient to prevent harms (because harms arise from failure modes the fairness frame doesn't address), then the constraint becomes a false summit — falsely claiming to solve alignment while leaving substantive alignment problems unsolved.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint one reading of the ai_alignment_commitment kernel, or a genuinely separable problem?',
    'Examine whether the definitional contest (alignment-as-present-harm vs. alignment-as-control-risk) arises from the SAME commitment system (how AI should be developed) or from fundamentally distinct systems. If the contest lies in interpretation of a shared commitment, it is a kernel reading; if the problems are structurally independent, they are separate constraints.',
    'If this is genuinely a reading of a shared kernel, the engine''s kernel_reading classification should apply; per-seat computation accounts for structural symmetry with sibling readings (same reference frame, same authority structure, divergent axioms). If the reading is incoherent — if present-harm prevention and control-at-scale are actually orthogonal — then classifying both as readings of one kernel is a false decomposition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether the definitional contest sits within a single kernel or across distinct problem domains.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression (0.72) structural (regulatory barriers, resource exclusion, definitional power-moves) or internalized (researchers convinced the present-harm frame is correct and self-suppress long-horizon work)?',
    'Post-exclusion trajectory: if long-horizon control research thrives in independent institutional settings (non-AI-ethics-aligned university labs, non-policy-embedded research groups) without the definitional suppression of alignment authority, the suppression is structural. If the research persists but self-identifies as ''not alignment work,'' the suppression is partially internalized.',
    'Structural suppression is enforcement of a definitional boundary; internalized suppression is researchers accepting the reading''s normative framing of what matters. The distinction affects whether the constraint persists by active maintenance (high effective suppression) or by conversion (lower effective suppression, higher theater).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Whether measured suppression is structural or internalized in the research community.').

omega_variable(
    axiom_empirical_grounding,
    'Does the foundational axiom ''algorithmic harm is immediate'' rest on correctly measured empirical evidence, or does it amplify isolated cases into systemic claims?',
    'Systematic review of bias in deployed AI systems: does documented bias occur universally, in the majority of systems, in high-stakes contexts, or primarily in research demonstrations? Measurement precision: are harms quantified against a baseline or asserted relative to expectations?',
    'If bias is empirically systemic and reliably quantified, the axiom is grounded and the constraint is justified. If bias is real but more scattered, or real but small relative to other harms, the axiom overstates and the constraint''s extraction (0.68) may rest on a false empirical premise. If bias is demonstrated only in academic settings and not in deployed systems, the axiom''s grounding collapses.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(axiom_empirical_grounding, empirical, 'Whether the present-harm axiom is empirically justified as a priority.').

omega_variable(
    dual_positioning_of_capability_developers,
    'Do AI capability developers genuinely benefit from a fairness-first alignment definition (reputation, legal defense, consumer trust) enough to offset the cost of fairness testing and deployment latency?',
    'Market observation: do firms that implement robust fairness testing gain competitive advantage or legal shelter? Do regulatory regimes that require fairness testing increase industry profit margins or decrease them? Do consumers pay price premiums for ''fair AI''?',
    'If developers benefit enough (positive ROI on fairness investment), they are net beneficiaries and the extracted cost is smaller than authored (0.68 extractiveness drops). If fairness is net-cost with no benefit, the extraction is higher and less contested. The dual-positioning claim rests on this empirical question.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dual_positioning_of_capability_developers, empirical, 'Whether capability developers benefit from the fairness-first framing or purely pay.').

omega_variable(
    reading_foreclosure_vs_coexistence,
    'Does the ethics-justice reading''s core axiom (algorithmic harm to marginalized people is alignment-relevant) logically foreclose the safety-control-reading''s axiom (control of superintelligence is alignment-relevant), or can both coexist in different institutional seats?',
    'Test whether a single party (researcher, firm, policy body) could coherently hold both readings: does one party''s adoption of present-harm-first prevent another party from pursuing long-horizon-control-first in the same institutional context? If parties segment (some pursue fairness, others pursue control) without logical contradiction, it is coexistence. If forcing both frames into one institutional decision-making structure creates a logical collision, it is foreclosure.',
    'Coexistence supports the ''different readings'' framing; foreclosure (if it obtains) would mean this reading structurally eliminates the sibling — moving the relation from coexists_with to forecloses, which would indicate the kernel contest is not symmetric or integrable.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_foreclosure_vs_coexistence, conceptual, 'Whether the readings can coexist or one logically forecloses the other.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_alignment_commitment__ethics_justice_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_a_tr_t0, ai_alignment_commitment__ethics_justice_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(ai_a_tr_t0, projected).
narrative_ontology:measurement(ai_a_tr_t4, ai_alignment_commitment__ethics_justice_reading, theater_ratio, 4, 0.31).
narrative_ontology:measurement_basis(ai_a_tr_t4, observed).
narrative_ontology:measurement(ai_a_tr_t8, ai_alignment_commitment__ethics_justice_reading, theater_ratio, 8, 0.36).
narrative_ontology:measurement_basis(ai_a_tr_t8, observed).
narrative_ontology:measurement(ai_a_tr_t12, ai_alignment_commitment__ethics_justice_reading, theater_ratio, 12, 0.4).
narrative_ontology:measurement_basis(ai_a_tr_t12, observed).
narrative_ontology:measurement(ai_a_tr_t16, ai_alignment_commitment__ethics_justice_reading, theater_ratio, 16, 0.41).
narrative_ontology:measurement_basis(ai_a_tr_t16, observed).
narrative_ontology:measurement(ai_a_tr_t20, ai_alignment_commitment__ethics_justice_reading, theater_ratio, 20, 0.41).
narrative_ontology:measurement_basis(ai_a_tr_t20, projected).

% Extraction over time
narrative_ontology:measurement(ai_a_be_t0, ai_alignment_commitment__ethics_justice_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(ai_a_be_t0, projected).
narrative_ontology:measurement(ai_a_be_t4, ai_alignment_commitment__ethics_justice_reading, base_extractiveness, 4, 0.54).
narrative_ontology:measurement_basis(ai_a_be_t4, observed).
narrative_ontology:measurement(ai_a_be_t8, ai_alignment_commitment__ethics_justice_reading, base_extractiveness, 8, 0.61).
narrative_ontology:measurement_basis(ai_a_be_t8, observed).
narrative_ontology:measurement(ai_a_be_t12, ai_alignment_commitment__ethics_justice_reading, base_extractiveness, 12, 0.66).
narrative_ontology:measurement_basis(ai_a_be_t12, observed).
narrative_ontology:measurement(ai_a_be_t16, ai_alignment_commitment__ethics_justice_reading, base_extractiveness, 16, 0.67).
narrative_ontology:measurement_basis(ai_a_be_t16, observed).
narrative_ontology:measurement(ai_a_be_t20, ai_alignment_commitment__ethics_justice_reading, base_extractiveness, 20, 0.68).
narrative_ontology:measurement_basis(ai_a_be_t20, projected).

% Suppression requirement over time
narrative_ontology:measurement(ai_a_su_t0, ai_alignment_commitment__ethics_justice_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement_basis(ai_a_su_t0, projected).
narrative_ontology:measurement(ai_a_su_t4, ai_alignment_commitment__ethics_justice_reading, suppression_requirement, 4, 0.63).
narrative_ontology:measurement_basis(ai_a_su_t4, observed).
narrative_ontology:measurement(ai_a_su_t8, ai_alignment_commitment__ethics_justice_reading, suppression_requirement, 8, 0.68).
narrative_ontology:measurement_basis(ai_a_su_t8, observed).
narrative_ontology:measurement(ai_a_su_t12, ai_alignment_commitment__ethics_justice_reading, suppression_requirement, 12, 0.71).
narrative_ontology:measurement_basis(ai_a_su_t12, observed).
narrative_ontology:measurement(ai_a_su_t16, ai_alignment_commitment__ethics_justice_reading, suppression_requirement, 16, 0.72).
narrative_ontology:measurement_basis(ai_a_su_t16, observed).
narrative_ontology:measurement(ai_a_su_t20, ai_alignment_commitment__ethics_justice_reading, suppression_requirement, 20, 0.72).
narrative_ontology:measurement_basis(ai_a_su_t20, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_alignment_commitment__ethics_justice_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(ai_alignment_commitment__ethics_justice_reading, 0.12).
narrative_ontology:affects_constraint(ai_alignment_commitment__ethics_justice_reading, ai_alignment_commitment__safety_control_reading).
narrative_ontology:affects_constraint(ai_alignment_commitment__ethics_justice_reading, ai_alignment_commitment__integrated_reading).

% DUAL FORMULATION NOTE:
% The ai_alignment_commitment kernel decomposes into three structurally distinct constraint stories: (1) ethics_justice_reading: alignment = present-harm prevention (this story, ε=0.68, extractive from long-horizon research), (2) safety_control_reading: alignment = control-at-scale safety (ε differs, benefits long-horizon research), (3) integrated_reading: alignment = attending to both simultaneously (ε differs again, attempts to bridge). These are not three perspectives on one constraint; they are three structurally distinct constraints arising from divergent specifications of what 'alignment' means. ε-invariance principle applies: if the ε values differ materially (as they do — present-harm has higher extraction from control research; control-first has higher accessibility collapse for fairness work), they are separate constraints. They are linked via network.affects_constraints because the legitimacy of each reading depends on the kernel contest — if one reading 'wins' institutional adoption, it shifts the operating environment for the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ai_alignment_commitment__ethics_justice_reading, organized, 0.52).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
