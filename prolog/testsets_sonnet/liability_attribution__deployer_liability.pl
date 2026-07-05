% ============================================================================
% CONSTRAINT STORY: liability_attribution__deployer_liability
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:coordination_type/2,
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
 *   constraint_id: liability_attribution__deployer_liability
 *   human_readable: Deployer-Primary Liability Reading of AI Harm Attribution
 *   domain: technology_governance/legal_theory/regulatory_design
 *
 * SUMMARY:
 *   This story instantiates one reading of a contested liability-attribution
 *   kernel: given an AI system causing downstream harm, which party in the
 *   value chain bears primary legal liability? Under the deployer-liability
 *   reading, the party with deployment context control — the organization
 *   that configured, fine-tuned, and put the model into a live decision
 *   context — is held primarily responsible, on the theory that it made the
 *   proximate decision to deploy and controls the operational context in
 *   which harm occurred. This is a clean, ε-invariant claim about one
 *   specific liability rule; sibling readings (developer_liability,
 *   shared_liability) are separate constraints with their own
 *   beneficiary/victim structures and are not blended into this one. As the
 *   kernel context predicts, this reading pulls deployers into the victim
 *   set, lets developers (foundation model providers) externalize deployment
 *   risk through licensing terms, converts model opacity into the deployer's
 *   due-diligence burden rather than the provider's disclosure obligation,
 *   and shields foundation model providers from downstream harm claims.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(liability_attribution__deployer_liability, 0.62).
domain_priors:suppression_score(liability_attribution__deployer_liability, 0.58).
domain_priors:theater_ratio(liability_attribution__deployer_liability, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(liability_attribution__deployer_liability, extractiveness, 0.62).
narrative_ontology:constraint_metric(liability_attribution__deployer_liability, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(liability_attribution__deployer_liability, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(liability_attribution__deployer_liability, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(liability_attribution__deployer_liability, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(liability_attribution__deployer_liability, tangled_rope).
narrative_ontology:human_readable(liability_attribution__deployer_liability, "Deployer-Primary Liability Reading of AI Harm Attribution").
narrative_ontology:topic_domain(liability_attribution__deployer_liability, "technology_governance/legal_theory/regulatory_design").

domain_priors:requires_active_enforcement(liability_attribution__deployer_liability).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(liability_attribution__deployer_liability, '862563f3-a282-4f16-9321-a269afed69be').
narrative_ontology:cs_kernel_codification('862563f3-a282-4f16-9321-a269afed69be', distributed).
narrative_ontology:cs_authority_grounding('862563f3-a282-4f16-9321-a269afed69be', distributed).
narrative_ontology:cs_reading_relation('862563f3-a282-4f16-9321-a269afed69be', liability_attribution__developer_liability, coexists_with).
narrative_ontology:cs_reading_relation('862563f3-a282-4f16-9321-a269afed69be', liability_attribution__shared_liability, influences).
narrative_ontology:cs_axiom('862563f3-a282-4f16-9321-a269afed69be', foundational, proximate_operational_control_grounds_liability).
narrative_ontology:cs_axiom_status(proximate_operational_control_grounds_liability, holdable).
narrative_ontology:cs_axiom_grounding('862563f3-a282-4f16-9321-a269afed69be', proximate_operational_control_grounds_liability, conventional).
narrative_ontology:cs_axiom('862563f3-a282-4f16-9321-a269afed69be', secondary, opacity_is_a_due_diligence_burden_not_a_disclosure_obligation).
narrative_ontology:cs_axiom_status(opacity_is_a_due_diligence_burden_not_a_disclosure_obligation, holdable).
narrative_ontology:cs_axiom_grounding('862563f3-a282-4f16-9321-a269afed69be', opacity_is_a_due_diligence_burden_not_a_disclosure_obligation, instrumental).
narrative_ontology:cs_reference_frame('862563f3-a282-4f16-9321-a269afed69be', traditional_product_liability_proximate_cause_doctrine).
narrative_ontology:cs_drift_state('862563f3-a282-4f16-9321-a269afed69be', post_foundation_model_proliferation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('862563f3-a282-4f16-9321-a269afed69be', '').
narrative_ontology:cs_kernel_id(liability_attribution__deployer_liability, liability_attribution).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(liability_attribution__deployer_liability, foundation_model_providers).
narrative_ontology:constraint_beneficiary(liability_attribution__deployer_liability, model_licensing_intermediaries).
narrative_ontology:constraint_victim(liability_attribution__deployer_liability, deploying_organizations).
narrative_ontology:constraint_victim(liability_attribution__deployer_liability, downstream_end_users).
narrative_ontology:constraint_victim(liability_attribution__deployer_liability, small_business_deployers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Trains and licenses the underlying model, retains the deepest knowledge of training data, capability limits, and failure modes, but under this reading bears no primary liability once the model is deployed by a licensee. Sets license terms that push audit and monitoring obligations downstream and can walk away from any single deployment dispute without losing market position.
narrative_ontology:constraint_stakeholder(liability_attribution__deployer_liability, foundation_model_providers, beneficiary,
    institutional, generational, arbitrage, global).

% Packages foundation models into deployable products and drafts the license and terms-of-service language that assigns downstream duty of care to the deploying organization. Benefits from selling deployment-ready tooling while contractually disclaiming responsibility for how it is configured or applied.
narrative_ontology:constraint_stakeholder(liability_attribution__deployer_liability, model_licensing_intermediaries, beneficiary,
    organized, biographical, mobile, continental).
narrative_ontology:stakeholder_secondary_role(liability_attribution__deployer_liability, model_licensing_intermediaries, agenda_setter).

% Configures, fine-tunes, and puts the model into a live decision-making context (hiring, lending, medical triage, content moderation) without access to training data, evaluation methodology, or internal safety testing. Under this reading, bears primary liability for harms because it controls the deployment context and made the decision to deploy, even though it cannot fully audit what it licensed. Exit means not deploying at all, forgoing the capability.
narrative_ontology:constraint_stakeholder(liability_attribution__deployer_liability, deploying_organizations, payer,
    moderate, biographical, constrained, national).

% Licenses the same foundation-model-derived product as larger organizations but lacks legal staff, compliance budget, or technical capacity to perform meaningful due diligence on an opaque model. Faces the identical liability standard as a well-resourced enterprise deployer, effectively absorbing risk it has no realistic means to assess or mitigate.
narrative_ontology:constraint_stakeholder(liability_attribution__deployer_liability, small_business_deployers, payer,
    powerless, immediate, trapped, local).

% Experiences the actual harm — a denied loan, a wrongful flag, a bad medical recommendation — generated by the deployed model. Has no contractual relationship with the foundation model provider and must pursue the deployer, who is the only visible, locally reachable party, regardless of whether the deployer or the underlying model architecture caused the failure.
narrative_ontology:constraint_stakeholder(liability_attribution__deployer_liability, downstream_end_users, payer,
    powerless, immediate, trapped, local).

% Adjudicates liability claims and writes the rules that assign deployment-context control as the operative legal test. Favors this reading partly because deployers are locally identifiable and jurisdictionally reachable, while foundation model providers may be offshore, diffuse, or shielded by complex corporate structure.
narrative_ontology:constraint_stakeholder(liability_attribution__deployer_liability, regulators_and_courts, agenda_setter,
    institutional, generational, analytical, national).

% Would testify that many failure modes originate in training data, architecture choices, or alignment gaps invisible to any deployer no matter how diligent, and that deployer-primary liability incentivizes providers to withhold exactly the information deployers would need to do genuine due diligence. Rarely has standing or a forum to raise this in individual liability disputes.
narrative_ontology:constraint_stakeholder(liability_attribution__deployer_liability, ai_safety_researchers, excluded,
    moderate, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(liability_attribution__deployer_liability, foundation_model_providers).
narrative_ontology:fixing_cost_class(liability_attribution__deployer_liability, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Assigns a single, locally reachable, jurisdictionally accountable party — the deployer — as the liability target, giving courts and regulators a tractable enforcement point instead of chasing a diffuse, often offshore model supply chain.
% TRANSFER_FUNCTION: Moves the cost of harm investigation, litigation exposure, and compliance burden from foundation model providers (who retain the deepest technical knowledge and the greatest resources) onto deploying organizations and, ultimately, onto downstream end users who bear uncompensated residual harm when deployer due diligence proves insufficient against an opaque model.
% ABSENT_VOICES: AI safety researchers who understand training-time failure modes are rarely parties to liability disputes; foundation model providers' internal evaluation teams, who could speak to known limitations, are shielded by trade secrecy and are not compelled to testify under this liability standard.
% DISAPPEARANCE_RATIONALE: If deployer-primary liability were abandoned tomorrow, deployers would lose their strongest argument for demanding audit rights and evaluation transparency from model providers as a defense; foundation model providers would face direct exposure and would likely change licensing terms, disclosure practices, and possibly pricing to reflect new retained risk.
% FOUNDING_PROBLEM: As foundation models proliferated through licensing chains, courts needed a workable rule for assigning liability when harm occurred at the point of use rather than at the point of model creation, and deployment context (what data was used, what decision was automated, what oversight existed) was seen as the proximate, knowable cause.
% FOUNDING_PROBLEM_CORROBORATION: Foundation model providers and their trade associations attest the rule correctly reflects who controls the proximate cause of harm. Independent legal scholars, consumer protection advocates, and small-business trade groups outside the provider ecosystem attest the rule externalizes information asymmetry onto parties least able to bear it, citing cases where deployers could not have detected the defect through any feasible audit.
narrative_ontology:disappearance_verdict(liability_attribution__deployer_liability, world_rearranges).
narrative_ontology:founding_problem_status(liability_attribution__deployer_liability, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(liability_attribution__deployer_liability, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(liability_attribution__deployer_liability, 'none', 1).
narrative_ontology:epsilon_provenance(liability_attribution__deployer_liability, 0.62, 'claude-sonnet-5', 'none', direct).

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
 *   Extraction is moderate-to-substantial (0.62) because the rule transfers real cost — litigation exposure, insurance premiums, compliance overhead — from the best-resourced, best-informed party (the model provider) to parties (deployers, especially small ones) who often cannot obtain the information needed to actually perform the due diligence the rule demands of them. Suppression (0.58) reflects that alternative liability allocations (shared liability, developer liability) are foreclosed by statute or case law in jurisdictions adopting this rule, and deployers have limited ability to contract around it given standard-form licensing terms. Theater ratio is modest (0.28) — the due-diligence obligations imposed on deployers are not purely performative; some deployers do meaningfully invest in testing and monitoring — but a growing share of 'compliance' activity is defensive documentation aimed at litigation posture rather than actually reducing harm, which is why theater rises gently over the measured interval.
 *
 * PERSPECTIVAL GAP:
 *   From the foundation model provider's seat, this rule looks like sound allocation of responsibility to the party 'closest' to the harm and best positioned to prevent it through context-specific safeguards — a coordination-shaped story. From the deploying organization's seat, especially a small business deployer, the same rule computes as extraction: it is being asked to bear liability for defects it structurally cannot detect, using a licensed product it did not design, under contractual terms it did not negotiate. The engine's per-seat computation is expected to diverge exactly along these lines, which is the point of authoring both seats rather than reconciling them.
 *
 * DIRECTIONALITY LOGIC:
 *   Foundation model providers and the intermediaries that package their models sit near the beneficiary end: they retain the deepest technical knowledge, set license terms that allocate risk downstream, and have global mobility/exit if any single jurisdiction's liability regime becomes unfavorable. Deploying organizations sit near the target end: they are held primarily liable for outcomes shaped by decisions (training data curation, architecture, alignment) they neither made nor can fully inspect. Small business deployers sit further toward full-target than large enterprise deployers because they lack the resources to negotiate audit rights or purchase compliance tooling — the same nominal liability standard extracts more from them in practice. Downstream end users are structurally powerless victims with no privity to any upstream party; the deployer is simply the only reachable defendant.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — courts needing a tractable, locally enforceable liability target when AI harm chains span opaque, often offshore model supply chains — remains partly live (enforcement tractability is real), which is why founding_problem_status is marked contested rather than dead. But the rule's persistence in its current form, without corresponding disclosure obligations on providers, increasingly serves provider interests more than it serves the original goal of accountability-that-actually-reduces-harm. This is not classified as pure snare because a genuine coordination function exists (someone must be legally answerable, and deployers do have real operational control over configuration and monitoring); it is tangled_rope because that genuine function coexists with asymmetric extraction that requires active enforcement (case law, statute, contractual disclaimers) to sustain against the objection that deployers cannot audit what they cannot see.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    control_versus_information_asymmetry,
    'Does ''deployment context control'' actually track the capacity to prevent harm, or does it merely track who is locally reachable and jurisdictionally convenient to sue, independent of who could have prevented the harm?',
    'Case-by-case analysis of whether deployers who received full model documentation, evaluation results, and known-failure-mode disclosures from providers could have prevented specific harms through feasible due diligence, versus cases where the defect was undetectable regardless of deployer diligence.',
    'If control tracks true preventive capacity, deployer-primary liability is closer to a genuine coordination rule; if it tracks reachability rather than capacity, the rule is closer to pure cost externalization dressed as accountability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(control_versus_information_asymmetry, empirical, 'Whether deployment control is a meaningful proxy for preventability or a convenience proxy for enforceability.').

omega_variable(
    kernel_reading_selection_pressure,
    'Is the deployer_liability reading gaining ground because it best fits the causal structure of AI harm, or because it is the reading most favorable to the best-resourced, most politically organized parties in the value chain (foundation model providers)?',
    'Comparative analysis of which reading (deployer, developer, shared) has been adopted in jurisdictions with strong versus weak technology-sector lobbying presence, and tracking amicus brief authorship in the relevant caselaw.',
    'If adoption correlates with lobbying strength rather than causal analysis, this reading''s persistence is better explained as regulatory capture of the liability-attribution kernel than as considered legal reasoning — which would support the shared_liability reading as the less captured alternative.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection_pressure, conceptual, 'Whether reading selection across jurisdictions tracks causal fit or political economy.').

omega_variable(
    small_deployer_disproportionate_burden,
    'Should the liability standard for a well-resourced enterprise deployer and an under-resourced small business deployer be identical, given that only the former can realistically perform meaningful due diligence on an opaque foundation model?',
    'Track litigation and settlement outcomes segmented by deployer resource tier over the next several years; if small deployers settle or lose disproportionately regardless of actual fault, that is evidence the uniform standard is functioning as disproportionate extraction.',
    'Resolution favoring tiered standards would reduce this reading''s extraction score for small deployers specifically and might shift the story toward a scaffold (transitional rule pending tiered reform) rather than a settled tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(small_deployer_disproportionate_burden, preference, 'Whether uniform deployer liability standards are equitable across deployer resource tiers.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(liability_attribution__deployer_liability, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(liab_tr_t0, liability_attribution__deployer_liability, theater_ratio, 0, 0.18).
narrative_ontology:measurement(liab_tr_t4, liability_attribution__deployer_liability, theater_ratio, 4, 0.2).
narrative_ontology:measurement(liab_tr_t8, liability_attribution__deployer_liability, theater_ratio, 8, 0.22).
narrative_ontology:measurement(liab_tr_t12, liability_attribution__deployer_liability, theater_ratio, 12, 0.24).
narrative_ontology:measurement(liab_tr_t16, liability_attribution__deployer_liability, theater_ratio, 16, 0.26).
narrative_ontology:measurement(liab_tr_t20, liability_attribution__deployer_liability, theater_ratio, 20, 0.27).
narrative_ontology:measurement(liab_tr_t24, liability_attribution__deployer_liability, theater_ratio, 24, 0.28).

% Extraction over time
narrative_ontology:measurement(liab_be_t0, liability_attribution__deployer_liability, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(liab_be_t4, liability_attribution__deployer_liability, base_extractiveness, 4, 0.48).
narrative_ontology:measurement(liab_be_t8, liability_attribution__deployer_liability, base_extractiveness, 8, 0.53).
narrative_ontology:measurement(liab_be_t12, liability_attribution__deployer_liability, base_extractiveness, 12, 0.56).
narrative_ontology:measurement(liab_be_t16, liability_attribution__deployer_liability, base_extractiveness, 16, 0.59).
narrative_ontology:measurement(liab_be_t20, liability_attribution__deployer_liability, base_extractiveness, 20, 0.61).
narrative_ontology:measurement(liab_be_t24, liability_attribution__deployer_liability, base_extractiveness, 24, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(liab_su_t0, liability_attribution__deployer_liability, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(liab_su_t4, liability_attribution__deployer_liability, suppression_requirement, 4, 0.46).
narrative_ontology:measurement(liab_su_t8, liability_attribution__deployer_liability, suppression_requirement, 8, 0.5).
narrative_ontology:measurement(liab_su_t12, liability_attribution__deployer_liability, suppression_requirement, 12, 0.53).
narrative_ontology:measurement(liab_su_t16, liability_attribution__deployer_liability, suppression_requirement, 16, 0.55).
narrative_ontology:measurement(liab_su_t20, liability_attribution__deployer_liability, suppression_requirement, 20, 0.57).
narrative_ontology:measurement(liab_su_t24, liability_attribution__deployer_liability, suppression_requirement, 24, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(liability_attribution__deployer_liability, enforcement_mechanism).
narrative_ontology:affects_constraint(liability_attribution__deployer_liability, liability_attribution__developer_liability).
narrative_ontology:affects_constraint(liability_attribution__deployer_liability, liability_attribution__shared_liability).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the liability_attribution kernel. deployer_liability places deployers in the victim set and shields foundation model providers; developer_liability inverts this, placing model creators in the victim set; shared_liability apportions liability jointly by causal contribution. Each reading has independently authored ε, beneficiaries, and victims — they are not measurement variants of one constraint but three structurally distinct legal rules competing to govern the same underlying harm-attribution problem.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(liability_attribution__deployer_liability, moderate, 0.68).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
