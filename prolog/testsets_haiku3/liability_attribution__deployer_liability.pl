% ============================================================================
% CONSTRAINT STORY: liability_attribution__deployer_liability
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
 *   constraint_id: liability_attribution__deployer_liability
 *   human_readable: Deployer Primary Liability Regime for AI Systems
 *   domain: legal/technological/governance
 *
 * SUMMARY:
 *   Deployer-primary liability is ONE reading of a contested kernel about who
 *   bears responsibility for harms caused by deployed AI systems. This
 *   reading assigns primary liability to the organization that deploys and
 *   operates the system, on the grounds that they control the deployment
 *   context and make the decision to put the system in production. The
 *   REFERENT is the standing liability allocation under this reading—the
 *   regime as described—assessed by this reading's own lights
 *   (deployment-context causation, operational decision authority). The
 *   reading's endorsed alternative (e.g., provider liability or shared
 *   liability) is NOT the referent; ε measures the extractiveness of the
 *   deployer-liability regime itself. Sibling readings (developer_liability,
 *   shared_liability) are separate constraints with their own ε values,
 *   beneficiary/victim structures, and deployed rationales.
 *
 * KEY AGENTS:
 *   - system_deployers (institutional power, decision authority, constrained exit) — primary liable parties under this reading
 *   - foundation_model_providers (institutional power, arbitrage exit, externalize risk) — shielded from downstream harm claims
 *   - developer_communities (moderate power, mobile exit) — similarly shielded via deployer due-diligence burden
 *   - harmed_end_users (powerless, trapped exit) — recourse against deployer; remedy subject to deployer's due-diligence defense
 *   - regulatory_authorities (institutional power, agenda-setter) — define deployer responsibility standard and enforcement
 *   - insurance_and_verification_markets (powerful, arbitrage exit) — profit from deployer liability via audit fees, insurance premiums
 *   - excluded_parties (civil society, researchers, transparency advocates) — structurally absent from regime design
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(liability_attribution__deployer_liability, 0.62).
domain_priors:suppression_score(liability_attribution__deployer_liability, 0.71).
domain_priors:theater_ratio(liability_attribution__deployer_liability, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(liability_attribution__deployer_liability, extractiveness, 0.62).
narrative_ontology:constraint_metric(liability_attribution__deployer_liability, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(liability_attribution__deployer_liability, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(liability_attribution__deployer_liability, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(liability_attribution__deployer_liability, resistance, 0.69).

% --- Constraint claim ---
narrative_ontology:constraint_claim(liability_attribution__deployer_liability, tangled_rope).
narrative_ontology:human_readable(liability_attribution__deployer_liability, "Deployer Primary Liability Regime for AI Systems").
narrative_ontology:topic_domain(liability_attribution__deployer_liability, "legal/technological/governance").

domain_priors:requires_active_enforcement(liability_attribution__deployer_liability).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(liability_attribution__deployer_liability, '4e51fb3f-ed89-4c46-9adc-8c53d47b7e70').
narrative_ontology:cs_kernel_codification('4e51fb3f-ed89-4c46-9adc-8c53d47b7e70', formalized).
narrative_ontology:cs_authority_grounding('4e51fb3f-ed89-4c46-9adc-8c53d47b7e70', extraction).
narrative_ontology:cs_interpretation_layer_present('4e51fb3f-ed89-4c46-9adc-8c53d47b7e70').
narrative_ontology:cs_reading_relation('4e51fb3f-ed89-4c46-9adc-8c53d47b7e70', liability_attribution__developer_liability, coexists_with).
narrative_ontology:cs_reading_relation('4e51fb3f-ed89-4c46-9adc-8c53d47b7e70', liability_attribution__shared_liability, coexists_with).
narrative_ontology:cs_axiom('4e51fb3f-ed89-4c46-9adc-8c53d47b7e70', foundational, deployment_decision_authority_entails_liability).
narrative_ontology:cs_axiom_status(deployment_decision_authority_entails_liability, holdable).
narrative_ontology:cs_axiom_grounding('4e51fb3f-ed89-4c46-9adc-8c53d47b7e70', deployment_decision_authority_entails_liability, deontological).
narrative_ontology:cs_axiom('4e51fb3f-ed89-4c46-9adc-8c53d47b7e70', foundational, operational_context_control_is_sole_liability_basis).
narrative_ontology:cs_axiom_status(operational_context_control_is_sole_liability_basis, holdable).
narrative_ontology:cs_axiom_grounding('4e51fb3f-ed89-4c46-9adc-8c53d47b7e70', operational_context_control_is_sole_liability_basis, deontological).
narrative_ontology:cs_axiom('4e51fb3f-ed89-4c46-9adc-8c53d47b7e70', secondary, provider_opacity_is_inherent_to_capability).
narrative_ontology:cs_axiom_status(provider_opacity_is_inherent_to_capability, holdable).
narrative_ontology:cs_axiom_grounding('4e51fb3f-ed89-4c46-9adc-8c53d47b7e70', provider_opacity_is_inherent_to_capability, empirically_contingent).
narrative_ontology:cs_reference_frame('4e51fb3f-ed89-4c46-9adc-8c53d47b7e70', deployer_operational_authority_framework).
narrative_ontology:cs_drift_state('4e51fb3f-ed89-4c46-9adc-8c53d47b7e70', contemporary_liability_contestation, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('4e51fb3f-ed89-4c46-9adc-8c53d47b7e70', '').
narrative_ontology:cs_kernel_id(liability_attribution__deployer_liability, liability_attribution).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(liability_attribution__deployer_liability, foundation_model_providers).
narrative_ontology:constraint_beneficiary(liability_attribution__deployer_liability, developer_communities).
narrative_ontology:constraint_victim(liability_attribution__deployer_liability, system_deployers).
narrative_ontology:constraint_victim(liability_attribution__deployer_liability, harmed_end_users).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(liability_attribution__deployer_liability, insurance_and_verification_markets).
narrative_ontology:constraint_victim(liability_attribution__deployer_liability, insurance_and_verification_markets).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Organizations that integrate and operate AI systems in production environments (hospitals, banks, autonomous vehicle fleets, content platforms). Under deployer-liability regimes, they bear primary legal and financial responsibility for harms caused by the deployed system, regardless of whether the harm originated from model design flaws, training data issues, or emergent behaviors they did not and could not predict. Their burden includes comprehensive auditing, monitoring, documentation of due diligence, and insurance or self-insurance against downstream liability. They cannot exit deployment without abandoning their operational capacity; their exit options collapse around the decision to deploy at all.
narrative_ontology:constraint_stakeholder(liability_attribution__deployer_liability, system_deployers, payer,
    institutional, biographical, constrained, global).

% Companies that train and release large language models, vision models, and multi-modal systems. Under deployer liability, they are substantially shielded from downstream harm claims: the deployer, not the model provider, bears the duty to audit, constrain, and monitor the model. Model providers externalize deployment risk and maintain pricing power despite uncertainty about their products' failure modes. They can exit through model retirement or fine-tuning guidance without absorbing liability cascades.
narrative_ontology:constraint_stakeholder(liability_attribution__deployer_liability, foundation_model_providers, beneficiary,
    institutional, generational, arbitrage, global).

% Open-source developers, researchers, and tool-builders who create applications and systems layered on top of foundation models. Under deployer liability, they are similarly shielded: if an application they publish causes harm in deployment, the deployer's due diligence obligation extends to vetting and constraining the application before use. Developers retain the benefit of broad distribution and adoption without assuming liability for all downstream deployment contexts.
narrative_ontology:constraint_stakeholder(liability_attribution__deployer_liability, developer_communities, beneficiary,
    moderate, biographical, mobile, global).

% Individuals and communities who suffer direct harm from deployed AI systems (discriminatory loan denials, medical misdiagnosis, privacy violations, autonomous vehicle injuries, defamatory content generation). Under deployer liability, their recourse is against the deployer organization, not the model provider or developer. However, if the deployer can establish they exercised due diligence (conducted audits, implemented controls), the end user's claim may be denied or limited, leaving harmed parties without remedy despite suffering the concrete injury.
narrative_ontology:constraint_stakeholder(liability_attribution__deployer_liability, harmed_end_users, payer,
    powerless, immediate, trapped, global).

% Governments, sectoral regulators (FDA, SEC, financial authorities), and cross-border standard-setters that establish the liability regime. They define the scope of deployer responsibility, the due diligence standard, the safe-harbor conditions, and enforcement mechanisms. They face pressure from deployer organizations to limit liability and from civil society to ensure harmed parties have recourse.
narrative_ontology:constraint_stakeholder(liability_attribution__deployer_liability, regulatory_authorities, agenda_setter,
    institutional, generational, analytical, national).

% Insurance carriers, auditing firms, and certification bodies that emerge to manage deployer liability: they conduct compliance audits, issue risk assessments, price insurance coverage, and certify due diligence. They collect insurance premiums and audit fees from deployers and profit from the opacity: the harder it is to predict AI failure modes, the higher the risk premiums they can charge and the more audits and certifications they can sell.
narrative_ontology:constraint_stakeholder(liability_attribution__deployer_liability, insurance_and_verification_markets, beneficiary,
    powerful, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(liability_attribution__deployer_liability, insurance_and_verification_markets, payer).

% Civil society organizations advocating for victim compensation, academic researchers demanding model transparency, and competing deployment frameworks emphasizing provider accountability. They argue that deployers lack the technical knowledge to audit opaque models and that liability should follow causal responsibility (the provider/developer who created the capability). They are structurally excluded from the regime design because liability rules are written by deployers, model providers, and regulators without systematic civil-society participation in the foundational premise-setting.
narrative_ontology:constraint_stakeholder(liability_attribution__deployer_liability, excluded_parties, excluded,
    moderate, biographical, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(liability_attribution__deployer_liability, foundation_model_providers).
narrative_ontology:fixing_cost_class(liability_attribution__deployer_liability, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a clear chain of responsibility for AI system safety: deployers have decision authority over deployment contexts, so deployers bear the duty to audit, constrain, and monitor their systems. This creates a single accountable party and avoids the diffuse-responsibility problem where model providers deny responsibility because they did not control deployment, and deployers deny responsibility because they did not design the model.
% TRANSFER_FUNCTION: Transfers liability risk from upstream providers (model creators, foundation model companies) to downstream deployers (operational organizations). Deployers absorb the cost of due diligence, insurance, and potential liability settlements; providers externalize deployment risk and maintain pricing power despite model opacity.
% ABSENT_VOICES: Harmed end users and their advocates are structurally excluded from regime-design decisions; they do not participate in defining the deployer liability standard or the safe-harbor conditions. Civil-society accountability movements and transparency advocates are sidelined. Academic researchers demanding model auditability are not parties to the negotiation.
% DISAPPEARANCE_RATIONALE: If deployer-primary liability vanished and was replaced by developer/provider liability or shared liability, the allocation of insurance costs, due-diligence burdens, and compensation pools would reorganize. Model providers would face liability pressure to increase transparency and auditability; deployers would have stronger claims for model provider indemnification; harmed parties would have alternative defendants with deeper pockets. The entire insurance, audit, and certification market built around deployer liability would shrink or reorient.
% FOUNDING_PROBLEM: In the early deployment phase of large AI models, responsibility for harms was ambiguous: model providers claimed they provided only tools and could not be held liable for all possible downstream uses; deployers claimed they lacked the technical knowledge to audit opaque models; harmed parties had no clear defendant. The deployer-liability regime was proposed as a solution: place responsibility clearly on the party with decision authority and deployment context (the deployer), and make deployers' due diligence obligations the mechanism for risk management.
% FOUNDING_PROBLEM_CORROBORATION: Model providers and large technology companies attest that deployer liability creates clear incentives for safety and allows them to continue innovation without rear-facing liability. Deployers and their legal representatives initially supported it as a way to establish insurance and safe-harbor mechanisms, though increasingly contest it as the due-diligence burden grows. Civil-society organizations and some academic researchers attest that the founding problem was NEVER that responsibility was ambiguous to deployers—deployers made conscious choices to deploy opaque systems—but that providers lacked accountability. Harmed-party advocates attest the regime leaves them without meaningful recourse when deployers claim due diligence.
narrative_ontology:disappearance_verdict(liability_attribution__deployer_liability, world_rearranges).
narrative_ontology:founding_problem_status(liability_attribution__deployer_liability, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(liability_attribution__deployer_liability, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(liability_attribution__deployer_liability, 'none', 1).
narrative_ontology:epsilon_provenance(liability_attribution__deployer_liability, 0.62, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is moderate-high (0.62 at interval end) because the regime transfers upstream risk to deployers who lack technical knowledge to audit opaque models, creating a systematic asymmetry: deployers bear liability for failures they did not cause and cannot fully predict, while providers retain pricing power and innovation freedom. Suppression is high (0.71) because deployers are constrained to accept the regime (they cannot deploy without assuming liability) and excluded parties are absent from foundational regime-setting. Theater is moderate (0.48) because the regime includes genuine safety-motivation (deployers do conduct audits and monitoring) but a substantial portion of enforcement energy defends the risk-transfer mechanism itself rather than end-user protection. The measurement series shows extractiveness rising from 0.48 to 0.62 over the interval (as the regime matures and deployers internalize their liability burden) then slight decline at t=20 (as political pressure mounts and alternative regime proposals gain traction). Suppression plateaus around 0.70–0.72 (the regime's enforcement infrastructure stabilizes). Theater rises and plateaus, indicating the regime's protective functions become routinized while the underlying asymmetry persists.
 *
 * PERSPECTIVAL GAP:
 *   From the provider/developer seat: the regime is coordination—it clarifies responsibility, enables innovation, and allows them to focus on capability creation without rear-facing liability. From the deployer seat: the regime is extraction—it assigns liability for harms they did not cause, imposes due-diligence burdens they cannot fully discharge (auditing opaque models), and transfers risk upstream providers created. From the end-user seat: the regime is neither coordination nor extraction but abdication—the only path to remedy requires proving the deployer failed their due-diligence obligation, a burden end users cannot meet. The engine computes these divergences from the structural data (beneficiary/victim declarations, exit options, power asymmetry) without adjudicating which seat's perception is correct.
 *
 * DIRECTIONALITY LOGIC:
 *   Foundation model providers and developers sit near the beneficiary end (d ≈ 0.1–0.2): they externalize deployment risk, maintain pricing power, and face negligible liability exposure despite creating the core capability. System deployers sit near the target end (d ≈ 0.8–0.9): they bear primary liability, internalize risk their decision authority does not fully control, and absorb costs (audits, insurance, liability settlements) without corresponding coordination benefit. Harmed end users sit at the target end (d ≈ 1.0): they suffer the harm and have limited recourse (their only defendant is the deployer, who can claim due-diligence defense). Insurance and verification markets occupy a dual position (d ≈ 0.5–0.6): they benefit from the regime (collect fees) but also bear some cost (must maintain credibility, face regulatory pressure). Regulatory authorities and excluded parties sit outside the beneficiary/payer axis (analytical power).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem ('responsibility was ambiguous') is increasingly contested: deployers now argue the problem was never ambiguity but provider opacity; providers argue the problem was never responsibility but risk-aversion by deployers. The regime persists because (1) deployers have internalized the liability burden and built business models around it (exit cost is now high), (2) providers have accrued substantial power and influence over regulatory design, and (3) harmed parties remain excluded from the regime's foundational design. The classification as tangled_rope (not snare) depends on the genuine coordination function: deployers do achieve clarity about responsibility and can implement monitoring and control. But the extraction component is substantial: the regime transfers upstream risk to downstream parties who lack control over the model creation that caused the risk. Mandatrophy is live but not yet resolved—the regime's function (clarity) has partially atrophied relative to its original justification, and political pressure to revise it toward shared or provider liability is mounting.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    deployer_knowledge_asymmetry,
    'Can deployers realistically conduct adequate due diligence on opaque foundation models, or does the due-diligence standard impose an impossible burden?',
    'Empirical study: measure the gap between audit depth deployers can achieve versus the technical complexity of model failure modes; survey deployer organizations about due-diligence cost, feasibility, and perceived adequacy.',
    'If deployers cannot achieve adequate due diligence despite good-faith effort, the regime is fundamentally extractive (imposing unachievable liability); if they can, the coordination function is genuine. This determines whether the classification should shift toward snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(deployer_knowledge_asymmetry, empirical, 'Whether the deployer due-diligence standard is technically achievable given model opacity.').

omega_variable(
    provider_deliberate_opacity,
    'Do foundation model providers deliberately maintain model opacity to externalizes deployment risk, or is opacity an unavoidable side effect of scaling?',
    'Comparative analysis: study providers'' investment in interpretability, auditability, and transparency; interview provider executives about the trade-off between capability scaling and auditability; examine whether providers offer transparent-but-weaker models as an option.',
    'If providers deliberately choose opacity to externalize liability, the regime is enforced extraction by the provider set against the deployer set; if opacity is inherent to scaling, the regime may be a genuine coordination problem without remedy. This affects whether the reading forecloses the developer_liability reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(provider_deliberate_opacity, empirical, 'Whether model opacity is a deliberate choice or inherent to scaling.').

omega_variable(
    end_user_remedy_closure,
    'Under deployer-primary liability with deployer due-diligence defense, what fraction of harmed end users have actual legal recourse?',
    'Litigation analysis: sample deployed-AI harms; determine for each harm whether the deployer''s due-diligence defense likely succeeds; calculate the fraction where harmed parties have meaningful recourse versus are left without remedy.',
    'If the deployer due-diligence defense forecloses most end-user claims, the regime is a liability-exoneration structure dressed as responsibility allocation, and the extraction vector shifts from deployers-to-providers to deployers-to-end-users. This may justify reclassification to snare or suggest mandatrophy (the coordination function of ''clear responsibility'' is undermined by deployer-defense closure).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(end_user_remedy_closure, empirical, 'Whether end users retain meaningful legal recourse despite deployer-primary liability.').

omega_variable(
    sibling_reading_foreclosure,
    'Does this reading''s core premise (deployers are the accountable decision-makers) logically foreclose the developer_liability reading (providers are accountable capability creators), or do they coexist as distinct framings?',
    'Conceptual analysis: a single organization (e.g., the EU) could enact shared liability, thereby rejecting pure deployer-primary liability without logically foreclosing it. Conversely, a reading that asserts ''only deployers make decisions about deployment'' does foreclose any reading that asserts ''only providers make decisions about capability creation'' within a single jurisdiction. Examine whether sibling readings claim logical necessity or merely political preference.',
    'If readings foreclose each other, the kernel will resolve toward one dominant reading; if they coexist, the kernel persists as a live multi-reading regime. Classification of the reading_relations (forecloses vs. coexists_with) depends on this resolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure, conceptual, 'Whether this reading''s premises logically foreclose or coexist with sibling readings.').

omega_variable(
    regulatory_capture_dynamics,
    'Is the deployer-liability regime maintained because it is genuinely optimal, or because model providers have captured the regulatory design process?',
    'Governance analysis: study regulatory participation (who sits on standards bodies, advises regulators, funds policy research); compare the regime that would emerge from pure deployer advocacy versus provider advocacy versus end-user advocacy; examine whether regulatory reversals have occurred when political balance shifted.',
    'Evidence of capture would suggest the regime persists not because it solves the coordination problem but because providers extract political power over the design. This would strengthen the extraction classification and suggest the regime is more snare than tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_capture_dynamics, empirical, 'Whether the deployer-liability regime reflects genuine coordination necessity or regulatory capture.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(liability_attribution__deployer_liability, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(liab_tr_t0, liability_attribution__deployer_liability, theater_ratio, 0, 0.35).
narrative_ontology:measurement(liab_tr_t3, liability_attribution__deployer_liability, theater_ratio, 3, 0.4).
narrative_ontology:measurement(liab_tr_t6, liability_attribution__deployer_liability, theater_ratio, 6, 0.44).
narrative_ontology:measurement(liab_tr_t10, liability_attribution__deployer_liability, theater_ratio, 10, 0.48).
narrative_ontology:measurement(liab_tr_t15, liability_attribution__deployer_liability, theater_ratio, 15, 0.49).
narrative_ontology:measurement(liab_tr_t20, liability_attribution__deployer_liability, theater_ratio, 20, 0.48).

% Extraction over time
narrative_ontology:measurement(liab_be_t0, liability_attribution__deployer_liability, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(liab_be_t3, liability_attribution__deployer_liability, base_extractiveness, 3, 0.54).
narrative_ontology:measurement(liab_be_t6, liability_attribution__deployer_liability, base_extractiveness, 6, 0.58).
narrative_ontology:measurement(liab_be_t10, liability_attribution__deployer_liability, base_extractiveness, 10, 0.62).
narrative_ontology:measurement(liab_be_t15, liability_attribution__deployer_liability, base_extractiveness, 15, 0.64).
narrative_ontology:measurement(liab_be_t20, liability_attribution__deployer_liability, base_extractiveness, 20, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(liab_su_t0, liability_attribution__deployer_liability, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(liab_su_t3, liability_attribution__deployer_liability, suppression_requirement, 3, 0.62).
narrative_ontology:measurement(liab_su_t6, liability_attribution__deployer_liability, suppression_requirement, 6, 0.68).
narrative_ontology:measurement(liab_su_t10, liability_attribution__deployer_liability, suppression_requirement, 10, 0.72).
narrative_ontology:measurement(liab_su_t15, liability_attribution__deployer_liability, suppression_requirement, 15, 0.7).
narrative_ontology:measurement(liab_su_t20, liability_attribution__deployer_liability, suppression_requirement, 20, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(liability_attribution__deployer_liability, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(liability_attribution__deployer_liability, 0.12).
narrative_ontology:affects_constraint(liability_attribution__deployer_liability, liability_attribution__developer_liability).
narrative_ontology:affects_constraint(liability_attribution__deployer_liability, liability_attribution__shared_liability).
narrative_ontology:affects_constraint(liability_attribution__deployer_liability, ai_safety_due_diligence_standard).
narrative_ontology:affects_constraint(liability_attribution__deployer_liability, ai_model_transparency_requirement).

% DUAL FORMULATION NOTE:
% The liability_attribution kernel instantiates three structurally distinct readings: deployer_liability (this constraint), developer_liability (sibling), and shared_liability (sibling). Each reading has different ε, different beneficiary/victim allocations, and different policy implications. They coexist in contemporary governance; no single reading has achieved exclusive dominance. The three constraints are linked via network.affects_constraints to enable comparison of how different liability allocations change incentives for transparency, due diligence, and provider accountability.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(liability_attribution__deployer_liability, institutional, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
