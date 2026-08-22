% ============================================================================
% CONSTRAINT STORY: liability_attribution__deployer_liability
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
    narrative_ontology:constraint_stakeholder/7,
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
 *   As foundation models proliferated into downstream deployments, courts,
 *   regulators, and industry lobbying converged on a liability rule that
 *   places primary legal responsibility on deployers — the parties who
 *   select, configure, and put AI systems into specific use contexts — rather
 *   than on the developers who trained the underlying models. This story
 *   instantiates the deployer-liability reading of a contested
 *   liability-attribution kernel: it treats the standing arrangement
 *   (deployer-primary liability as currently being codified and litigated) as
 *   the referent for extraction, assessed from within this reading's own
 *   premises. The sibling readings — developer-primary liability and
 *   shared/proportional liability — are separate constraints with their own ε
 *   values and victim sets; they are not represented here, per the
 *   ε-invariance and kernel-reading discipline. Under the deployer-liability
 *   reading, foundation model providers are structurally shielded because the
 *   standard locates the duty of care at the point of context control rather
 *   than the point of capability creation, and deployers — particularly
 *   smaller, less-resourced ones — absorb a due-diligence burden they often
 *   lack the informational access to actually discharge.
 *
 * KEY AGENTS:
 *   - foundation_model_providers: primary beneficiary (institutional/arbitrage) — shielded from downstream liability by the standard
 *   - downstream_deployers: primary target (moderate/constrained) — assigned primary liability despite limited model visibility
 *   - small_and_midsize_deployers: secondary target (powerless/trapped) — bear disproportionate relative exposure
 *   - end_users_harmed_by_deployed_systems: ultimate victims (powerless/trapped) — recovery depends on thinner deployer balance sheets
 *   - insurance_underwriters_for_deployers: secondary beneficiary (organized/mobile) — profits from the new risk-transfer market this rule creates
 *   - regulators_and_courts: agenda-setter (institutional/analytical) — codifies and could revise the standard
 *   - ai_safety_researchers_and_auditors: excluded voice (moderate/constrained) — has no standing to compel disclosure that would resolve the due-diligence question
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
narrative_ontology:constraint_metric(liability_attribution__deployer_liability, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(liability_attribution__deployer_liability, tangled_rope).
narrative_ontology:human_readable(liability_attribution__deployer_liability, "Deployer-Primary Liability Reading of AI Harm Attribution").
narrative_ontology:topic_domain(liability_attribution__deployer_liability, "technology_governance/legal_theory/regulatory_design").

domain_priors:requires_active_enforcement(liability_attribution__deployer_liability).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(liability_attribution__deployer_liability, 'a439492d-18e1-4b84-a940-b0df09e31feb').
narrative_ontology:cs_kernel_codification('a439492d-18e1-4b84-a940-b0df09e31feb', distributed).
narrative_ontology:cs_authority_grounding('a439492d-18e1-4b84-a940-b0df09e31feb', distributed).
narrative_ontology:cs_reading_relation('a439492d-18e1-4b84-a940-b0df09e31feb', liability_attribution__developer_liability, coexists_with).
narrative_ontology:cs_reading_relation('a439492d-18e1-4b84-a940-b0df09e31feb', liability_attribution__shared_liability, influences).
narrative_ontology:cs_axiom('a439492d-18e1-4b84-a940-b0df09e31feb', foundational, context_control_grounds_duty_of_care).
narrative_ontology:cs_axiom_status(context_control_grounds_duty_of_care, holdable).
narrative_ontology:cs_axiom_grounding('a439492d-18e1-4b84-a940-b0df09e31feb', context_control_grounds_duty_of_care, conventional).
narrative_ontology:cs_axiom('a439492d-18e1-4b84-a940-b0df09e31feb', secondary, capability_creation_is_too_remote_for_proximate_liability).
narrative_ontology:cs_axiom_status(capability_creation_is_too_remote_for_proximate_liability, holdable).
narrative_ontology:cs_axiom_grounding('a439492d-18e1-4b84-a940-b0df09e31feb', capability_creation_is_too_remote_for_proximate_liability, instrumental).
narrative_ontology:cs_reference_frame('a439492d-18e1-4b84-a940-b0df09e31feb', deployment_context_as_proximate_cause).
narrative_ontology:cs_drift_state('a439492d-18e1-4b84-a940-b0df09e31feb', post_frontier_model_opacity_escalation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('a439492d-18e1-4b84-a940-b0df09e31feb', '').
narrative_ontology:cs_kernel_id(liability_attribution__deployer_liability, liability_attribution).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(liability_attribution__deployer_liability, foundation_model_providers).
narrative_ontology:constraint_beneficiary(liability_attribution__deployer_liability, insurance_underwriters_for_deployers).
narrative_ontology:constraint_victim(liability_attribution__deployer_liability, downstream_deployers).
narrative_ontology:constraint_victim(liability_attribution__deployer_liability, small_and_midsize_deployers).
narrative_ontology:constraint_victim(liability_attribution__deployer_liability, end_users_harmed_by_deployed_systems).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Trains and releases the underlying model, retaining exclusive knowledge of training data, architecture, and known failure modes. Under this reading, liability attaches at the point of deployment decision, not the point of capability creation, so the provider's exposure to downstream harm claims is sharply reduced regardless of how foreseeable the harm was from the model's known behavior. Distributes the model widely via API or license and captures revenue from that distribution without bearing the corresponding tail risk.
narrative_ontology:constraint_stakeholder(liability_attribution__deployer_liability, foundation_model_providers, beneficiary,
    institutional, civilizational, arbitrage, global).

% Integrates a foundation model into a product or service and configures, prompts, fine-tunes, or otherwise controls how it is used in a specific context. Under this reading, bears primary liability for resulting harms because it controlled the deployment context and made the decision to deploy — even where the harmful behavior originates in model weaknesses the deployer had no practical way to audit or fully understand. Cannot obtain the model's full training data, safety evaluation internals, or red-team results, yet is assigned the due-diligence burden of having caught the flaw before deployment.
narrative_ontology:constraint_stakeholder(liability_attribution__deployer_liability, downstream_deployers, payer,
    moderate, biographical, constrained, national).

% Smaller companies and public agencies that adopt foundation models off-the-shelf, without the technical staff or contractual leverage to demand transparency from the provider or to negotiate indemnification. They face the same primary-liability exposure as large enterprise deployers but without the resources to conduct meaningful due diligence or to self-insure against catastrophic-tail outcomes, making the assigned liability effectively unbounded relative to their capacity to bear it.
narrative_ontology:constraint_stakeholder(liability_attribution__deployer_liability, small_and_midsize_deployers, payer,
    powerless, biographical, trapped, national).

% Individuals harmed by a deployed AI system — denied a loan, misdiagnosed, wrongly flagged — who must identify and sue the deployer rather than the model's creator. Their recovery depends on the deployer's solvency and insurance, which is often thinner than the foundation model provider's balance sheet, and on litigating a due-diligence standard that does not require the provider to disclose the information that would prove the underlying defect.
narrative_ontology:constraint_stakeholder(liability_attribution__deployer_liability, end_users_harmed_by_deployed_systems, payer,
    powerless, immediate, trapped, local).

% Builds a new market selling AI-deployment liability coverage to deployers who now face concentrated primary liability. Prices policies based on deployer due-diligence practices it can audit contractually, and profits from the risk transfer this liability reading creates, without itself bearing exposure to the underlying model defects.
narrative_ontology:constraint_stakeholder(liability_attribution__deployer_liability, insurance_underwriters_for_deployers, beneficiary,
    organized, generational, mobile, national).

% Adjudicates and codifies the deployer-primary liability standard, drawing on doctrines of proximate cause and control (the party closest to the point of harm and with decision authority over context is assigned the duty of care). Can revise the standard toward shared or developer-primary liability if the due-diligence burden proves structurally impossible for deployers to discharge.
narrative_ontology:constraint_stakeholder(liability_attribution__deployer_liability, regulators_and_courts, agenda_setter,
    institutional, generational, analytical, national).

% Would testify that many failure modes are latent in the foundation model itself and are not discoverable by deployer-side testing no matter how diligent, given the providers' refusal to disclose training data provenance or internal evaluation results. Rarely party to the liability litigation itself and has no standing to compel the disclosure that would settle the due-diligence question.
narrative_ontology:constraint_stakeholder(liability_attribution__deployer_liability, ai_safety_researchers_and_auditors, excluded,
    moderate, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(liability_attribution__deployer_liability, foundation_model_providers).
narrative_ontology:fixing_cost_class(liability_attribution__deployer_liability, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Assigns a single, locatable, judgment-proof-adjacent party (the deployer) as the point of legal accountability for AI-caused harm, so injured parties and regulators have a clear address for claims instead of an unresolvable multi-party causal inquiry into a modelâ€™s training pipeline.
% TRANSFER_FUNCTION: Moves liability exposure, insurance cost, and due-diligence burden from foundation model providers to deployers, and moves recovery risk from end users (who face thinner, less certain deployer balance sheets) away from providers with the deepest pockets and most complete information about the underlying defect.
% ABSENT_VOICES: AI safety researchers and independent auditors who understand which failure modes are latent in the model itself are not parties to the liability standard-setting process and have no mechanism to compel the training-data and evaluation disclosure that would let deployers actually discharge the due-diligence burden assigned to them.
% DISAPPEARANCE_RATIONALE: If deployer-primary liability were abandoned overnight in favor of developer-primary or shared liability, foundation model providers would face direct exposure to downstream harm claims, insurance markets currently pricing deployer risk would collapse and reprice around providers, and deployers — especially small and midsize ones — would face dramatically reduced litigation exposure and could adopt AI systems with less defensive over-caution.
% FOUNDING_PROBLEM: As foundation models became embedded in countless downstream products, courts and regulators needed a liability rule that did not require litigating the internals of a black-box model in every harm case; assigning liability to the party with visible deployment context and decision authority offered a tractable, administrable standard.
% FOUNDING_PROBLEM_CORROBORATION: Foundation model providers and their industry associations attest the rule is sound because deployers make the proximate decisions that determine real-world harm. Independent AI safety researchers, plaintiffs' counsel in early AI-harm litigation, and several legal scholars outside the foundation-model industry attest the rule shifts liability toward the party least able to detect or prevent the underlying defect, and that the administrability the rule buys comes at the cost of accuracy in assigning responsibility to the party that actually controls the risk.
narrative_ontology:disappearance_verdict(liability_attribution__deployer_liability, world_rearranges).
narrative_ontology:founding_problem_status(liability_attribution__deployer_liability, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(liability_attribution__deployer_liability, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
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
 *   Extractiveness is substantial (0.62 at interval end) but not extreme: the rule genuinely solves a real coordination problem (a locatable point of accountability for AI harm) while also asymmetrically transferring risk away from the party with the deepest information advantage. Suppression (0.58) reflects the structural difficulty deployers face in contesting the standard — they cannot compel the disclosure that would let them prove the harm originated in undiscoverable model defects rather than deployment negligence. Theater ratio is modest (0.28) and rising, reflecting an increasing gap between the stated due-diligence standard and deployers' actual practical ability to meet it, as foundation models grow more opaque and complex over the measured interval.
 *
 * PERSPECTIVAL GAP:
 *   From the foundation model provider's seat, the standard is efficient allocation to the party with genuine contextual control — a coordination story. From the small deployer's seat, the same standard is liability assigned without the information needed to discharge it — an extraction story riding on the coordination framing. The engine computes these divergent seat classifications from the same structural data; this story does not resolve the divergence, it documents it.
 *
 * DIRECTIONALITY LOGIC:
 *   Foundation model providers sit near the full-beneficiary end: they retain proprietary information advantage, capture distribution revenue, and are shielded from the liability their design choices help produce. Downstream deployers and especially small/midsize deployers sit near the full-target end: they are assigned primary liability for harms rooted in decisions and information they do not control. End users are targets once removed — their practical recovery is capped by deployer solvency rather than provider solvency. Insurance underwriters are a secondary beneficiary class created by the liability allocation itself, profiting from a risk transfer they did not cause but that this reading manufactures.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — needing an administrable liability rule instead of litigating opaque model internals in every case — remains partially live (courts still need a workable standard), but the specific allocation to deployers increasingly serves an administrative-convenience function disconnected from actual causal control, since deployers frequently cannot obtain the technical information needed to have prevented the harm. Classifying this as tangled_rope rather than snare preserves the genuine coordination value (a locatable defendant, predictable liability rules enabling insurance markets and adoption) while still registering the asymmetric extraction (providers externalize risk onto parties who cannot fully mitigate it) that a pure-coordination 'rope' label would obscure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    administrability_vs_accuracy_tradeoff,
    'Does the deployer-primary standard''s administrability (a single locatable defendant) justify its accuracy cost (assigning liability to a party that frequently lacks the information to have prevented the harm)?',
    'Comparative litigation outcome studies across jurisdictions applying deployer-primary vs. developer-primary vs. shared liability standards, tracking settlement rates, case duration, and correlation between liability assignment and actual causal contribution as established at trial.',
    'If administrability gains are large and accuracy losses are modest, the tangled_rope classification''s coordination component strengthens; if accuracy losses dominate, the constraint drifts toward snare as the coordination story becomes primarily cover for cost-shifting.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(administrability_vs_accuracy_tradeoff, empirical, 'Whether the deployer-primary rule''s convenience benefits outweigh its misallocation costs.').

omega_variable(
    which_reading_becomes_dominant_law,
    'Which of the three kernel readings (deployer_liability, developer_liability, shared_liability) will actually consolidate into binding law or dominant industry practice, and does that consolidation reflect genuine assessment of causal control or the relative lobbying power of foundation model providers versus deployers?',
    'Track regulatory and appellate court convergence across major jurisdictions (EU AI Act enforcement, US state tort law, sectoral regulators) over the next five to ten years; compare against lobbying expenditure and testimony records from foundation model providers versus deployer trade associations.',
    'If deployer_liability consolidates primarily due to provider lobbying rather than principled causal analysis, this strengthens the case that the current reading functions as regulatory capture dressed as administrable doctrine; if it consolidates because courts independently find deployment context genuinely dispositive, the coordination function is more genuine than the extraction critique suggests.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(which_reading_becomes_dominant_law, conceptual, 'Which sibling reading of the liability_attribution kernel will become dominant, and why.').

omega_variable(
    disclosure_feasibility_ambiguity,
    'Is it technically and economically feasible for foundation model providers to disclose enough training-data and evaluation information for deployers to conduct meaningful due diligence, without destroying the providers'' competitive position or exposing proprietary methods?',
    'Technical feasibility studies on structured disclosure regimes (e.g., model cards with enforceable minimum content, red-team result escrow, third-party audit access) and their effect on deployer due-diligence outcomes in pilot jurisdictions.',
    'If meaningful disclosure is feasible without destroying provider incentives, the deployer-liability reading''s due-diligence burden becomes more defensible; if truly infeasible, the burden is structurally undischargeable and the reading functions as liability-shifting regardless of deployer effort.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(disclosure_feasibility_ambiguity, empirical, 'Whether deployers could in principle meet the due-diligence standard if providers disclosed more.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(liability_attribution__deployer_liability, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(liab_tr_t0, liability_attribution__deployer_liability, theater_ratio, 0, 0.12).
narrative_ontology:measurement(liab_tr_t4, liability_attribution__deployer_liability, theater_ratio, 4, 0.15).
narrative_ontology:measurement(liab_tr_t8, liability_attribution__deployer_liability, theater_ratio, 8, 0.18).
narrative_ontology:measurement(liab_tr_t12, liability_attribution__deployer_liability, theater_ratio, 12, 0.21).
narrative_ontology:measurement(liab_tr_t16, liability_attribution__deployer_liability, theater_ratio, 16, 0.24).
narrative_ontology:measurement(liab_tr_t20, liability_attribution__deployer_liability, theater_ratio, 20, 0.26).
narrative_ontology:measurement(liab_tr_t24, liability_attribution__deployer_liability, theater_ratio, 24, 0.28).

% Extraction over time
narrative_ontology:measurement(liab_be_t0, liability_attribution__deployer_liability, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(liab_be_t4, liability_attribution__deployer_liability, base_extractiveness, 4, 0.46).
narrative_ontology:measurement(liab_be_t8, liability_attribution__deployer_liability, base_extractiveness, 8, 0.51).
narrative_ontology:measurement(liab_be_t12, liability_attribution__deployer_liability, base_extractiveness, 12, 0.55).
narrative_ontology:measurement(liab_be_t16, liability_attribution__deployer_liability, base_extractiveness, 16, 0.58).
narrative_ontology:measurement(liab_be_t20, liability_attribution__deployer_liability, base_extractiveness, 20, 0.6).
narrative_ontology:measurement(liab_be_t24, liability_attribution__deployer_liability, base_extractiveness, 24, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(liab_su_t0, liability_attribution__deployer_liability, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(liab_su_t4, liability_attribution__deployer_liability, suppression_requirement, 4, 0.41).
narrative_ontology:measurement(liab_su_t8, liability_attribution__deployer_liability, suppression_requirement, 8, 0.46).
narrative_ontology:measurement(liab_su_t12, liability_attribution__deployer_liability, suppression_requirement, 12, 0.5).
narrative_ontology:measurement(liab_su_t16, liability_attribution__deployer_liability, suppression_requirement, 16, 0.53).
narrative_ontology:measurement(liab_su_t20, liability_attribution__deployer_liability, suppression_requirement, 20, 0.56).
narrative_ontology:measurement(liab_su_t24, liability_attribution__deployer_liability, suppression_requirement, 24, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(liability_attribution__deployer_liability, enforcement_mechanism).
narrative_ontology:affects_constraint(liability_attribution__deployer_liability, liability_attribution__developer_liability).
narrative_ontology:affects_constraint(liability_attribution__deployer_liability, liability_attribution__shared_liability).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the liability_attribution kernel, decomposed per the ε-invariance principle: deployer_liability (this story, deployers as primary victim class), developer_liability (developers as primary victim class, foundation model providers face direct exposure), and shared_liability (proportional distribution by causal contribution, diffusing both victim and beneficiary concentration). Each reading has its own ε, beneficiary/victim structure, and classification because each reading describes a structurally different standing arrangement, not merely a different opinion about the same arrangement. All three link to each other via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
