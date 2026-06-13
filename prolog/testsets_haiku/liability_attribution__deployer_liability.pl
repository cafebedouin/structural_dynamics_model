% ============================================================================
% CONSTRAINT STORY: liability_attribution__deployer_liability
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_liability_attribution__deployer_liability, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: liability_attribution__deployer_liability
 *   human_readable: Deployer Primary Liability for Foundation Model Harms
 *   domain: legal/technological/regulatory
 *
 * SUMMARY:
 *   Foundation model deployment creates a novel causation problem: a single
 *   upstream artifact (the model) trained on fixed data and released with
 *   imperfect information about its limitations is deployed across diverse
 *   downstream contexts by parties unfamiliar with its internals. Liability
 *   for harms must be assigned somewhere. The deployer-liability reading
 *   places primary responsibility on the integrating organization—the party
 *   with deployment context control and decision authority—arguing they are
 *   the natural duty-bearer. This externalizes liability from model creators
 *   (who control training data, architecture, release decisions) to deployers
 *   (who choose which model to use and how to apply it). The constraint is
 *   classified as tangled rope: it solves a genuine coordination problem
 *   (clarifying responsibility and incentivizing deployment safeguards) but
 *   does so asymmetrically—creators benefit from the liability shield while
 *   deployers absorb the exposure. The claim/metric gap is deliberate:
 *   deployer liability reads as rope (coordination framing); the metrics
 *   describe substantially extractive operation requiring active suppression
 *   of alternative liability regimes.
 *
 * KEY AGENTS:
 *   - foundation_model_providers: agenda-setters and primary beneficiaries—architect the liability framing and escape downstream exposure
 *   - developers_creating_applications: beneficiaries—sit between model provider (who disclaims) and deployer (who is liable); retain technical knowledge but escape primary liability
 *   - deployers_of_systems: victims—bear legal exposure for harms they cannot fully control or evaluate; constrained by the need to deploy to remain competitive
 *   - harmed_end_users: secondary victims—experience harms but structurally absent from liability allocation; their only recourse is through deployers
 *   - regulators_and_courts: agenda-setters—maintain and enforce the deployer-liable regime; choice between this reading and sibling readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(liability_attribution__deployer_liability, 0.68).
domain_priors:suppression_score(liability_attribution__deployer_liability, 0.62).
domain_priors:theater_ratio(liability_attribution__deployer_liability, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(liability_attribution__deployer_liability, extractiveness, 0.68).
narrative_ontology:constraint_metric(liability_attribution__deployer_liability, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(liability_attribution__deployer_liability, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(liability_attribution__deployer_liability, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(liability_attribution__deployer_liability, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(liability_attribution__deployer_liability, tangled_rope).
narrative_ontology:human_readable(liability_attribution__deployer_liability, "Deployer Primary Liability for Foundation Model Harms").
narrative_ontology:topic_domain(liability_attribution__deployer_liability, "legal/technological/regulatory").

domain_priors:requires_active_enforcement(liability_attribution__deployer_liability).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(liability_attribution__deployer_liability, '0e6fd8ff-ecfc-4bc4-965c-ba66cfcb1593').
narrative_ontology:cs_kernel_codification('0e6fd8ff-ecfc-4bc4-965c-ba66cfcb1593', distributed).
narrative_ontology:cs_authority_grounding('0e6fd8ff-ecfc-4bc4-965c-ba66cfcb1593', distributed).
narrative_ontology:cs_reading_relation('0e6fd8ff-ecfc-4bc4-965c-ba66cfcb1593', liability_attribution__developer_liability, coexists_with).
narrative_ontology:cs_reading_relation('0e6fd8ff-ecfc-4bc4-965c-ba66cfcb1593', liability_attribution__shared_liability, influences).
narrative_ontology:cs_axiom('0e6fd8ff-ecfc-4bc4-965c-ba66cfcb1593', foundational, deployment_control_determines_liability).
narrative_ontology:cs_axiom_status(deployment_control_determines_liability, holdable).
narrative_ontology:cs_axiom_grounding('0e6fd8ff-ecfc-4bc4-965c-ba66cfcb1593', deployment_control_determines_liability, instrumental).
narrative_ontology:cs_axiom('0e6fd8ff-ecfc-4bc4-965c-ba66cfcb1593', secondary, creator_context_insensitivity).
narrative_ontology:cs_axiom_status(creator_context_insensitivity, holdable).
narrative_ontology:cs_axiom_grounding('0e6fd8ff-ecfc-4bc4-965c-ba66cfcb1593', creator_context_insensitivity, empirically_contingent).
narrative_ontology:cs_reference_frame('0e6fd8ff-ecfc-4bc4-965c-ba66cfcb1593', deployment_decision_authority_determines_legal_duty).
narrative_ontology:cs_drift_state('0e6fd8ff-ecfc-4bc4-965c-ba66cfcb1593', post_harm_evidence_accumulation, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('0e6fd8ff-ecfc-4bc4-965c-ba66cfcb1593', '').
narrative_ontology:cs_kernel_id(liability_attribution__deployer_liability, liability_attribution).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(liability_attribution__deployer_liability, foundation_model_providers).
narrative_ontology:constraint_beneficiary(liability_attribution__deployer_liability, developers_creating_applications).
narrative_ontology:constraint_victim(liability_attribution__deployer_liability, deployers_of_systems).
narrative_ontology:constraint_victim(liability_attribution__deployer_liability, harmed_end_users).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Creates and releases foundation models as products or APIs. Positions deployers as the liable party, arguing that deployment context control and application-specific choices mean deployers bear responsibility for downstream harms. This framing externalizes liability from model creators. Collects licensing and usage revenue without bearing legal exposure for model failures in deployed systems.
narrative_ontology:constraint_stakeholder(liability_attribution__deployer_liability, foundation_model_providers, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(liability_attribution__deployer_liability, foundation_model_providers, beneficiary).

% Builds applications and services using foundation models. Under deployer liability, they sit between the model provider (who disclaims responsibility) and the deployer (who bears legal exposure). They benefit by escaping primary liability; their application-design choices shape harm risk but the legal burden is on the deployer. They retain technical knowledge of their system's capabilities and failure modes but face no direct liability for application-specific failures.
narrative_ontology:constraint_stakeholder(liability_attribution__deployer_liability, developers_creating_applications, beneficiary,
    powerful, generational, mobile, global).

% Organizations that integrate foundation models into live systems (enterprises, government agencies, service providers). Bear primary legal liability for harms caused by the models they deploy, even when model creators' design choices or opacity about capabilities/limitations significantly contributed. They control deployment context but not the underlying model; suppression comes from the asymmetry: they are held accountable for risks they cannot fully evaluate or control.
narrative_ontology:constraint_stakeholder(liability_attribution__deployer_liability, deployers_of_systems, payer,
    organized, biographical, constrained, global).

% Individuals who experience harms from deployed systems (discriminatory decisions, privacy violations, misinformation, safety failures). They are the immediate injury-bearers but structurally absent from liability allocation. They can sue deployers but lack standing against model creators whose design choices or training data contamination contributed to harm. Their only recourse is through deployers, who may lack resources or technical knowledge to assign root causes.
narrative_ontology:constraint_stakeholder(liability_attribution__deployer_liability, harmed_end_users, payer,
    powerless, immediate, trapped, global).

% Interpret and enforce liability regimes. Under deployer liability doctrine, they assign primary responsibility to the party with deployment control and decision authority, regardless of whether that party authored the underlying model or understood its failure modes. This is a procedural and evidentiary choice that systematically favors creators over integrators.
narrative_ontology:constraint_stakeholder(liability_attribution__deployer_liability, regulators_and_courts, agenda_setter,
    institutional, generational, analytical, national).

% Third-party firms and researchers that conduct safety and capability evaluations of foundation models. They would have strong incentive to certify models as safe if liability were shared, but under deployer liability they become witnesses to deployer due diligence rather than gatekeepers of safety. Their exclusion from the liability regime reduces accountability pressure on model creators.
narrative_ontology:constraint_stakeholder(liability_attribution__deployer_liability, model_auditors_and_evaluators, excluded,
    moderate, biographical, constrained, global).

% Views this reading as one of three competing doctrines (deployer, developer, shared liability). Notes that liability attribution determines who bears the cost of opacity, due diligence, and remediation. This reading externalizes those costs from creators to integrators; sibling readings distribute them differently.
narrative_ontology:constraint_stakeholder(liability_attribution__deployer_liability, observer_comparative_jurisprudence, observer,
    analytical, generational, analytical, global).
narrative_ontology:stakeholder_non_agent(liability_attribution__deployer_liability, observer_comparative_jurisprudence).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(liability_attribution__deployer_liability, foundation_model_providers).
narrative_ontology:fixing_cost_class(liability_attribution__deployer_liability, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allocates responsibility for harm mitigation in multi-party software systems by assigning primary liability to the party with deployment context control and operational authority. This creates a clear duty-bearer and incentivizes deployment-level safeguards (monitoring, vetting, access controls, user consent).
% TRANSFER_FUNCTION: Transfers legal liability exposure (potential damages, injunctive relief, compliance costs) from model creators to deployers, creating financial and reputational risk that flows asymmetrically: deployers absorb liability regardless of whether they designed, trained, or understood the model; creators escape downstream responsibility despite controlling training data, architecture, and release decisions.
% ABSENT_VOICES: Model auditors and safety researchers are structurally excluded from the liability regime—they would demand transparency and certification if liability were distributed along the causal chain. Harmed end-users can sue deployers but have no direct claim against creators, and deployers often lack the technical knowledge or access to assign root causes to model design versus deployment choices. Alternative liability framings (developer-liable, shared-liable) would amplify these excluded voices.
% DISAPPEARANCE_RATIONALE: If deployer primary liability doctrine vanished and were replaced with developer or shared liability, model creators would immediately face higher legal exposure, prompting investment in safety, auditing, and licensing restrictions; deployers would face lower liability but fewer tools to claim they exercised due diligence; the economics of model release and deployment would reorganize around different incentive centers.
% FOUNDING_PROBLEM: Early foundation model deployment created a novel causation structure: a single model (created once, trained on fixed data, released with known or unknown limitations) is deployed in diverse contexts by parties unfamiliar with its internals. The question of which party should bear liability for harms became urgent when models began causing measurable injury. Deployer liability was proposed to clarify responsibility: the integrator chooses the model, chooses the application, controls the threshold—they decide whether to deploy, so they bear the cost of their decision.
% FOUNDING_PROBLEM_CORROBORATION: Model providers and major tech companies attested to deployer liability in early policy documents and liability disclaimers (2023-2024). Harmed users, civil rights advocates, and a subset of AI safety researchers contest it, arguing that model creators' design and release decisions are the structural cause and should carry corresponding liability. Litigation in multiple jurisdictions (EU AI Act implementation, US state torts, UK Online Safety Bill) is producing conflicting rulings—some favor deployer liability, others impose shared or creator liability. No consensus among independent researchers exists; regulatory bodies remain divided.
narrative_ontology:disappearance_verdict(liability_attribution__deployer_liability, world_rearranges).
narrative_ontology:founding_problem_status(liability_attribution__deployer_liability, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(liability_attribution__deployer_liability, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(liability_attribution__deployer_liability, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(liability_attribution__deployer_liability_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(liability_attribution__deployer_liability, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(liability_attribution__deployer_liability_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness measures 0.68 at interval end, reflecting that deployer liability transfers legal and financial risk from creators to integrators without corresponding transfer of control over the underlying model. The constraint rises from 0.52 to 0.68 over 36 time units because: (1) litigation accumulates evidence that deployers are held liable even for model-design failures they could not have anticipated or prevented, (2) model capabilities expand faster than deployer evaluation capabilities, widening the information gap, (3) harm incidents increase, raising the cost of the liability exposure. Theater ratio climbs from 0.28 to 0.41 and plateaus, reflecting growing performative elements: deployers implement compliance theaters (audits, documentation, monitoring) that mimic due diligence but cannot actually prevent model failures rooted in training data or architecture. Suppression requirement rises from 0.48 to 0.62 and stabilizes, indicating the regime requires increasing active enforcement to suppress alternative liability regimes (developer-liable, shared-liable) that would reallocate the burden. Accessibility collapse (0.58) reflects that deployers have limited alternatives once they commit to AI integration; resistance (0.72) reflects active pushback from harmed users, civil rights advocates, and some regulators who contest the deployer-liable reading. The shared time grid ensures every metric is authored at every examined point; measurements report observed drift through actual litigation history and policy evolution (2023-2026 cutoff).
 *
 * PERSPECTIVAL GAP:
 *   The model-provider and deployer seats compute radically different constraint types. From the provider's position: deployer liability is coordination—it clarifies responsibility, creates incentives for deployment safeguards, and solves a genuine causation problem in multi-party systems (rope or weak tangled-rope). From the deployer's position: it is extraction via information asymmetry—they are held liable for risks they cannot evaluate or prevent, and alternatives are suppressed (snare or strong tangled-rope). From the harmed user's position: it is a substitution that deflects their claim away from the party with design control. The engine computes this divergence from power, exit, and beneficiary/victim declarations; the authored claimed_type (tangled_rope) represents the structural truth—it IS coordination plus asymmetric extraction, simultaneously.
 *
 * DIRECTIONALITY LOGIC:
 *   Model providers sit at directionality ~0.05–0.15 (full beneficiaries): they control the model, escape liability, collect licensing revenue, and have highest exit options (can pivot to other markets or tighten licensing). Deployers sit at directionality ~0.8–0.9 (full targets): they bear primary liability despite limited control over the model, constrained by competitive pressure to deploy, and face biographical or longer time horizons of exposure. Developers sit ~0.3–0.4 (mixed): they benefit from the liability shield but also face some design-choice liability and depend on deployers' continued willingness to use their applications. Harmed users sit ~0.85 (strong targets): they experience harm but are structurally absent from liability allocation and can only sue deployers (creating a filter). The directionality profile is asymmetric and is the source of the tangled-rope classification: coordination function (clear duty-bearer) coupled with extraction (asymmetric risk transfer).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (who bears liability in multi-party AI systems?) is contestable. Deployer liability proposes one answer by assigning primary responsibility to the integrating party. But the founding problem's status is contested: some argue deployment context control justifies liability (status: live), others argue that model-creation decisions are the true causal root and deployer liability is a cover story (status: dead, constraint is a zombie). The mandatrophy test asks: if the founding problem disappeared (we developed a perfect liability-allocation mechanism that traced all harms to their true causal origins), would deployer liability persist? Answer: no, it would be replaced by a more granular scheme. But the constraint persists despite the founding problem being contested precisely because it benefits creators; the liability shield extracts value by being maintained even as its rationale erodes. This is a mandatrophy candidate: deployment context provides a plausible-sounding basis for liability that externalizes costs from creators to deployers, whether or not the basis is true.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Which liability reading—deployer, developer, or shared—correctly maps causal responsibility to legal duty?',
    'Multi-jurisdiction longitudinal study of harm outcomes, deployer due-diligence costs, and model-creator investment in safety under each regime. Comparison of litigated cases assigning root cause to model design versus deployment choice.',
    'If deployer liability produces systemic underinvestment in model safety (creators have no incentive to improve transparency or safety), reclassify as pure extraction (snare). If it produces net-positive deployment-level safeguards that outweigh creator underinvestment, remains tangled rope. If costs are genuinely distributed along causal chains, shared liability is structurally correct.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'The fundamental question of which reading correctly allocates causal responsibility and legal duty in multi-party AI systems.').

omega_variable(
    deployer_information_asymmetry,
    'Can deployers realistically evaluate model capabilities and limitations to exercise due diligence, or is the burden placed on a party structurally unable to discharge it?',
    'Audit of deployer vetting practices in production systems; measurement of what information model providers disclose versus what deployers need to assess safety; interviews with deployers about their evaluation processes and confidence levels.',
    'If deployers cannot realistically evaluate risks they are held liable for, the constraint is purely extractive (snare) with suppression operating through information asymmetry. If deployers can develop adequate due diligence practices, tangled rope with real but manageable burden. The information gap determines whether liability is actionable or theatrical.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deployer_information_asymmetry, empirical, 'Whether deployers have the information and capability to discharge the liability burden placed on them.').

omega_variable(
    opacity_as_deployer_cost,
    'Does deployer primary liability systematically increase opacity incentives for model providers? Do creators have structural reason to withhold capability and limitation information if deployers bear the liability for failures?',
    'Compare transparency practices (model cards, capability documentation, limitation disclosure, audit access) across regimes favoring different liability allocations. Survey model providers about how liability rules affect disclosure decisions.',
    'If deployer liability incentivizes creator opacity, then the constraint directly subsidizes information asymmetry and increases effective harm risk. This would shift the constraint toward snare (extraction via forced ignorance). If transparency remains high despite deployer liability (creators release comprehensive documentation), the coordination function is preserved.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(opacity_as_deployer_cost, empirical, 'Whether deployer liability creates perverse incentives for model creator opacity.').

omega_variable(
    sibling_reading_structural_incompatibility,
    'Can deployer liability coexist with developer liability within the same legal framework, or do they foreclose each other?',
    'Examine mixed-regime jurisdictions (where some liability is deployer-based and some is developer-based) to see if they generate consistent incentives or create contradictory duties. Analyze whether a court could rationally hold both deployer and developer liable for the same harm without circularity.',
    'If truly coexisting (both parties liable for the same harm, incentives align), the readings are genuinely competitive but not foreclosing—both survive in a shared liability world. If framework-incompatible (holding both liable creates logical contradiction or cascade liability), then developer liability and deployer liability foreclose each other, and shared liability is the actual intermediate resolution. This determines the reading_relations classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_structural_incompatibility, conceptual, 'Whether this reading''s core premise logically forecloses its sibling readings or can coexist with them.').

omega_variable(
    suppression_mechanism_internalization,
    'Is suppression of alternative liability regimes structural (legal barriers, institutional inertia) or internalized (deployers accept the burden as legitimate)?',
    'Survey deployers about whether they accept deployer liability as fair and necessary, versus whether they experience it as forced imposition. Analyze litigation patterns: do deployers mount legal challenges to the doctrine, or do they grudgingly comply? Post-remedy trajectory: if a deployer can exit to a developer-liable regime, do they flee?',
    'If internalized, the constraint persists by normalizing the burden; if structural, it persists by lack of alternatives. Internalized suppression is higher effective extraction (target carries burden internally after exit). This determines whether the theater_ratio accurately captures the performative portion, or whether it underestimates the psychological cost.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether suppression of alternative liability regimes is structural or internalized in deployer decision-making.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(liability_attribution__deployer_liability, 0, 36).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(liab_tr_t0, liability_attribution__deployer_liability, theater_ratio, 0, 0.28).
narrative_ontology:measurement_basis(liab_tr_t0, observed).
narrative_ontology:measurement(liab_tr_t6, liability_attribution__deployer_liability, theater_ratio, 6, 0.32).
narrative_ontology:measurement_basis(liab_tr_t6, observed).
narrative_ontology:measurement(liab_tr_t12, liability_attribution__deployer_liability, theater_ratio, 12, 0.36).
narrative_ontology:measurement_basis(liab_tr_t12, observed).
narrative_ontology:measurement(liab_tr_t18, liability_attribution__deployer_liability, theater_ratio, 18, 0.39).
narrative_ontology:measurement_basis(liab_tr_t18, observed).
narrative_ontology:measurement(liab_tr_t24, liability_attribution__deployer_liability, theater_ratio, 24, 0.41).
narrative_ontology:measurement_basis(liab_tr_t24, observed).
narrative_ontology:measurement(liab_tr_t30, liability_attribution__deployer_liability, theater_ratio, 30, 0.41).
narrative_ontology:measurement_basis(liab_tr_t30, observed).
narrative_ontology:measurement(liab_tr_t36, liability_attribution__deployer_liability, theater_ratio, 36, 0.41).
narrative_ontology:measurement_basis(liab_tr_t36, observed).

% Extraction over time
narrative_ontology:measurement(liab_be_t0, liability_attribution__deployer_liability, base_extractiveness, 0, 0.52).
narrative_ontology:measurement_basis(liab_be_t0, observed).
narrative_ontology:measurement(liab_be_t6, liability_attribution__deployer_liability, base_extractiveness, 6, 0.58).
narrative_ontology:measurement_basis(liab_be_t6, observed).
narrative_ontology:measurement(liab_be_t12, liability_attribution__deployer_liability, base_extractiveness, 12, 0.62).
narrative_ontology:measurement_basis(liab_be_t12, observed).
narrative_ontology:measurement(liab_be_t18, liability_attribution__deployer_liability, base_extractiveness, 18, 0.65).
narrative_ontology:measurement_basis(liab_be_t18, observed).
narrative_ontology:measurement(liab_be_t24, liability_attribution__deployer_liability, base_extractiveness, 24, 0.67).
narrative_ontology:measurement_basis(liab_be_t24, observed).
narrative_ontology:measurement(liab_be_t30, liability_attribution__deployer_liability, base_extractiveness, 30, 0.68).
narrative_ontology:measurement_basis(liab_be_t30, observed).
narrative_ontology:measurement(liab_be_t36, liability_attribution__deployer_liability, base_extractiveness, 36, 0.68).
narrative_ontology:measurement_basis(liab_be_t36, observed).

% Suppression requirement over time
narrative_ontology:measurement(liab_su_t0, liability_attribution__deployer_liability, suppression_requirement, 0, 0.48).
narrative_ontology:measurement_basis(liab_su_t0, observed).
narrative_ontology:measurement(liab_su_t6, liability_attribution__deployer_liability, suppression_requirement, 6, 0.52).
narrative_ontology:measurement_basis(liab_su_t6, observed).
narrative_ontology:measurement(liab_su_t12, liability_attribution__deployer_liability, suppression_requirement, 12, 0.56).
narrative_ontology:measurement_basis(liab_su_t12, observed).
narrative_ontology:measurement(liab_su_t18, liability_attribution__deployer_liability, suppression_requirement, 18, 0.6).
narrative_ontology:measurement_basis(liab_su_t18, observed).
narrative_ontology:measurement(liab_su_t24, liability_attribution__deployer_liability, suppression_requirement, 24, 0.62).
narrative_ontology:measurement_basis(liab_su_t24, observed).
narrative_ontology:measurement(liab_su_t30, liability_attribution__deployer_liability, suppression_requirement, 30, 0.62).
narrative_ontology:measurement_basis(liab_su_t30, observed).
narrative_ontology:measurement(liab_su_t36, liability_attribution__deployer_liability, suppression_requirement, 36, 0.62).
narrative_ontology:measurement_basis(liab_su_t36, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(liability_attribution__deployer_liability, enforcement_mechanism).
narrative_ontology:affects_constraint(liability_attribution__deployer_liability, liability_attribution__developer_liability).
narrative_ontology:affects_constraint(liability_attribution__deployer_liability, liability_attribution__shared_liability).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kernel 'liability_attribution'. Sibling readings (developer_liability, shared_liability) instantiate different ε values and beneficiary/victim structures by reassigning which party bears primary responsibility. The three readings form a constraint family; all members link to each other via network.affects_constraints. The decomposition reflects ε-invariance: each reading produces a different constraint because the assignment of liability changes who pays, who benefits, and what suppression mechanisms operate. A single constraint cannot coherently claim all three readings—the core claim (who bears liability) is different in each, making them separate constraints with distinct structural data.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(liability_attribution__deployer_liability, analytical, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
