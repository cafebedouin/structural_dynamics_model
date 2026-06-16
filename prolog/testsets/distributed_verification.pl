% ============================================================================
% CONSTRAINT STORY: distributed_verification
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_distributed_verification, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: distributed_verification
 *   human_readable: Distributed Cryptographic Verification Authority
 *   domain: technology_governance/information_epistemology
 *
 * SUMMARY:
 *   The distributed verification reading instantiates a constraint where
 *   visual evidentiary authority migrates from post-hoc forensic analysis to
 *   point-of-capture cryptographic authentication. Blockchain provenance,
 *   hardware signing, and multi-source consensus replace expert testimony.
 *   This reading treats the shift as a genuine coordination response to the
 *   deepfake threat that simultaneously creates structural extraction through
 *   access barriers and protocol rents. The claim is tangled_rope
 *   (coordination + asymmetric extraction); the metrics describe rising
 *   extraction and suppression as the infrastructure matures and evidentiary
 *   standards harden around authenticated-only content.
 *
 * KEY AGENTS:
 *   - blockchain_infrastructure_providers: agenda_setter (institutional/arbitrage) — design protocols, collect fees, set verification standards
 *   - unconnected_populations: payer (powerless/trapped) — structurally excluded from authenticated capture, evidentiary standing diminished
 *   - legacy_forensic_practitioners: payer (moderate/constrained) — professional authority depreciated by paradigm shift
 *   - early_adopter_institutions: beneficiary (institutional/constrained) — reputational and evidentiary advantage from early integration
 *   - digital_rights_advocates: observer (organized/analytical) — document disparate impact, contest legitimacy structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(distributed_verification, 0.68).
domain_priors:suppression_score(distributed_verification, 0.71).
domain_priors:theater_ratio(distributed_verification, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(distributed_verification, extractiveness, 0.68).
narrative_ontology:constraint_metric(distributed_verification, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(distributed_verification, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(distributed_verification, accessibility_collapse, 0.63).
narrative_ontology:constraint_metric(distributed_verification, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(distributed_verification, tangled_rope).
narrative_ontology:human_readable(distributed_verification, "Distributed Cryptographic Verification Authority").
narrative_ontology:topic_domain(distributed_verification, "technology_governance/information_epistemology").

domain_priors:requires_active_enforcement(distributed_verification).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(distributed_verification, '71f146b8-75a4-424a-baee-c9948b654d63').
narrative_ontology:cs_kernel_codification('71f146b8-75a4-424a-baee-c9948b654d63', distributed).
narrative_ontology:cs_authority_grounding('71f146b8-75a4-424a-baee-c9948b654d63', distributed).
narrative_ontology:cs_reading_relation('71f146b8-75a4-424a-baee-c9948b654d63', visual_evidentiary_authority__indexical_realism, coexists_with).
narrative_ontology:cs_reading_relation('71f146b8-75a4-424a-baee-c9948b654d63', visual_evidentiary_authority__epistemic_collapse, influences).
narrative_ontology:cs_reading_relation('71f146b8-75a4-424a-baee-c9948b654d63', visual_evidentiary_authority__post_evidentiary, influences).
narrative_ontology:cs_axiom('71f146b8-75a4-424a-baee-c9948b654d63', foundational, cryptographic_provenance_as_truth_warrant).
narrative_ontology:cs_axiom_status(cryptographic_provenance_as_truth_warrant, holdable).
narrative_ontology:cs_axiom_grounding('71f146b8-75a4-424a-baee-c9948b654d63', cryptographic_provenance_as_truth_warrant, conventional).
narrative_ontology:cs_axiom('71f146b8-75a4-424a-baee-c9948b654d63', foundational, point_of_capture_authentication_necessity).
narrative_ontology:cs_axiom_status(point_of_capture_authentication_necessity, holdable).
narrative_ontology:cs_axiom_grounding('71f146b8-75a4-424a-baee-c9948b654d63', point_of_capture_authentication_necessity, empirically_contingent).
narrative_ontology:cs_reference_frame('71f146b8-75a4-424a-baee-c9948b654d63', expert_mediated_forensic_authority).
narrative_ontology:cs_drift_state('71f146b8-75a4-424a-baee-c9948b654d63', post_generative_ai_era, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('71f146b8-75a4-424a-baee-c9948b654d63', '').
narrative_ontology:cs_kernel_id(distributed_verification, visual_evidentiary_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(distributed_verification, blockchain_infrastructure_providers).
narrative_ontology:constraint_beneficiary(distributed_verification, cryptographic_authentication_vendors).
narrative_ontology:constraint_beneficiary(distributed_verification, early_adopter_institutions).
narrative_ontology:constraint_victim(distributed_verification, unconnected_populations).
narrative_ontology:constraint_victim(distributed_verification, legacy_forensic_practitioners).
narrative_ontology:constraint_victim(distributed_verification, low_resource_claimants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(distributed_verification, device_manufacturers).
narrative_ontology:constraint_beneficiary(distributed_verification, platform_content_moderators).
narrative_ontology:constraint_victim(distributed_verification, device_manufacturers).
narrative_ontology:constraint_vindicates(distributed_verification, cryptographic_immutability_doctrine).
narrative_ontology:constraint_vindicates(distributed_verification, consensus_as_truth_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design and operate the distributed ledger systems that timestamp and authenticate visual media at point of capture. Set protocol standards for what counts as verified. Collect transaction fees for every authentication event and sell enterprise verification services. Frame the shift as democratizing truth against centralized gatekeepers.
narrative_ontology:constraint_stakeholder(distributed_verification, blockchain_infrastructure_providers, agenda_setter,
    institutional, generational, arbitrage, global).

% Sell hardware signing modules, software SDKs, and integration services to device manufacturers and content platforms. Benefit from the mandatory authentication requirement without operating the underlying infrastructure. Market position depends on the verification paradigm becoming standard.
narrative_ontology:constraint_stakeholder(distributed_verification, cryptographic_authentication_vendors, beneficiary,
    powerful, biographical, mobile, global).

% News organizations, legal systems, and corporate entities that integrated cryptographic verification early. Gain evidentiary advantage and reputational premium from authenticated content. Their early investment becomes a moat as the paradigm spreads; late adopters face catch-up costs.
narrative_ontology:constraint_stakeholder(distributed_verification, early_adopter_institutions, beneficiary,
    institutional, biographical, constrained, national).

% Lack access to authentication-capable devices, reliable network connectivity, or institutional enrollment in verification networks. Their visual documentation of events carries diminished evidentiary weight regardless of content accuracy. The shift from post-hoc analysis to point-of-capture authentication structurally excludes their testimony.
narrative_ontology:constraint_stakeholder(distributed_verification, unconnected_populations, payer,
    powerless, immediate, trapped, regional).

% Forensic analysts, investigative journalists, and expert witnesses whose professional authority derived from post-hoc artifact analysis. Their skills depreciate as evidentiary standards shift to cryptographic provenance. Must retrain as infrastructure auditors or accept diminished professional standing.
narrative_ontology:constraint_stakeholder(distributed_verification, legacy_forensic_practitioners, payer,
    moderate, biographical, constrained, national).

% Individuals documenting abuse, environmental damage, or rights violations without access to authenticated capture infrastructure. Their evidence is systematically discounted in legal and institutional contexts that privilege cryptographic verification. The coordination function that protects against deepfakes simultaneously excludes their claims.
narrative_ontology:constraint_stakeholder(distributed_verification, low_resource_claimants, payer,
    powerless, immediate, trapped, local).

% Must integrate authentication hardware and pay licensing fees to participate in verification networks. Benefit from product differentiation in authenticated-capable devices but bear implementation costs and ongoing protocol compliance burdens. Market pressure forces adoption even where margins are thin.
narrative_ontology:constraint_stakeholder(distributed_verification, device_manufacturers, payer,
    powerful, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(distributed_verification, device_manufacturers, beneficiary).

% Social media platforms and content distribution networks that use cryptographic verification to automate authenticity decisions. Reduce moderation labor costs and liability exposure by deferring to protocol-level authentication. Gain efficiency but also inherit the exclusions the protocol embeds.
narrative_ontology:constraint_stakeholder(distributed_verification, platform_content_moderators, beneficiary,
    organized, biographical, mobile, global).

% Governments and intelligence services that operate outside Western-dominated verification networks. Structurally excluded from setting authentication standards; their visual documentation is pre-discredited regardless of accuracy. Would contest the legitimacy structure if admitted but are kept out by protocol governance.
narrative_ontology:constraint_stakeholder(distributed_verification, adversarial_state_actors, excluded,
    institutional, generational, arbitrage, national).

% Document the evidentiary exclusion of unconnected populations and contest the equation of cryptographic provenance with truth. Argue that the coordination function is separable from the access barriers. Produce reports showing disparate impact but lack enforcement authority.
narrative_ontology:constraint_stakeholder(distributed_verification, digital_rights_advocates, observer,
    organized, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the deepfake verification problem: cryptographic signing at point of capture creates tamper-evident provenance chains, enabling automated authenticity decisions at scale where post-hoc forensic analysis cannot keep pace with synthetic media volume.
% TRANSFER_FUNCTION: Moves evidentiary authority from post-hoc expert analysis (accessible to anyone with documentation) to point-of-capture authentication (accessible only to those enrolled in verification networks). Transfers transaction fees and licensing revenue from claimants and device makers to infrastructure providers.
% ABSENT_VOICES: Unconnected populations and adversarial state actors are structurally excluded from protocol governance. They would argue for verification methods that do not require real-time network access or enrollment in Western-controlled infrastructure, but their exclusion is embedded in the technical architecture.
% DISAPPEARANCE_RATIONALE: If cryptographic verification infrastructure vanished overnight, evidentiary standards would revert to post-hoc forensic analysis, unconnected populations would regain evidentiary standing, infrastructure providers would lose a primary revenue stream, and the deepfake problem would return to manual expert review at institutional bottleneck scale.
% FOUNDING_PROBLEM: Generative AI made synthetic visual media indistinguishable from authentic documentation at scale, overwhelming the capacity of forensic experts to verify authenticity post-hoc and threatening to collapse evidentiary standards across legal, journalistic, and institutional contexts.
% FOUNDING_PROBLEM_CORROBORATION: The synthetic media threat is corroborated by independent computer vision researchers, digital forensics labs, and legal scholars outside the benefiting infrastructure providers. The problem's liveness is not contested; what is contested is whether cryptographic authentication at point of capture is the only viable solution or whether it solves the problem by excluding a class of claimants.
narrative_ontology:disappearance_verdict(distributed_verification, world_rearranges).
narrative_ontology:founding_problem_status(distributed_verification, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(distributed_verification, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-16',
    'unspecified', 'agent/example_platform_commission.json',
    'claude-sonnet-4-5-20250929', 'unspecified').
narrative_ontology:story_seed(distributed_verification, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(distributed_verification_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(distributed_verification, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(distributed_verification_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is substantial (0.68 at interval end) because the coordination function is bundled with access barriers that exclude unconnected populations and extract protocol rents from device manufacturers. Suppression is high (0.71) because evidentiary standards increasingly privilege cryptographic provenance over content accuracy, and institutions defer to protocol-level authentication. Theater ratio is moderate (0.42): the deepfake protection function is real, but a growing share of verification activity enforces network enrollment and protocol compliance rather than truth-seeking. Accessibility collapse is moderate (0.63): post-hoc forensic methods remain technically possible but are institutionally deprecated. Resistance is substantial (0.58): excluded populations and legacy practitioners contest the legitimacy shift, and digital rights advocates document disparate impact.
 *
 * PERSPECTIVAL GAP:
 *   From the infrastructure provider seat, the constraint is coordination infrastructure solving a genuine epistemic crisis. From the unconnected population seat, the same structure operates as evidentiary exclusion enforced by access barriers. From the legacy practitioner seat, it is professional displacement dressed as technical necessity. The engine computes these divergences from the structural data; the tangled_rope claim does not adjudicate between them but asserts both functions coexist.
 *
 * DIRECTIONALITY LOGIC:
 *   Infrastructure providers are structural beneficiaries (collect fees, set standards, control protocol governance — d near beneficiary end). Unconnected populations and low-resource claimants are full targets (structurally excluded from evidentiary standing, trapped exit — d near 1.0). Legacy forensic practitioners are targets with constrained exit (professional depreciation, must retrain — d ~0.7). Early adopter institutions are beneficiaries (evidentiary advantage, constrained exit — d ~0.3). Device manufacturers are mixed (implementation costs but product differentiation — d ~0.5).
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (protect against deepfakes) remains live, but the coordination function is bundled with extraction (protocol rents, access barriers) that could be architecturally separated. A verification system that authenticated content without requiring real-time network enrollment or proprietary hardware would solve the deepfake problem without excluding unconnected populations. The bundling is a design choice, not a technical necessity, which is why the constraint computes as tangled_rope rather than pure rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_extraction_separability,
    'Is cryptographic authentication at point of capture structurally necessary for deepfake protection, or can the coordination function be separated from real-time network enrollment and proprietary hardware requirements?',
    'Development and institutional adoption of offline-capable, open-protocol verification methods that achieve comparable tamper-evidence without requiring network access at capture time. Natural experiment from jurisdictions that mandate protocol interoperability.',
    'If separable, the access barriers are pure extraction riding on genuine coordination; if inseparable, part of the measured extraction is the inherent cost of the coordination itself. Separability would support regulatory intervention requiring open protocols; inseparability would vindicate the infrastructure providers'' architectural choices.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_extraction_separability, empirical, 'Whether deepfake protection requires the specific access barriers this implementation embeds.').

omega_variable(
    evidentiary_exclusion_vs_fraud_prevention,
    'Does the constraint''s exclusion of unconnected populations represent an acceptable tradeoff (preventing fraud at the cost of some legitimate claims) or a structural injustice (privileging authenticated testimony over accurate testimony)?',
    'Longitudinal study of legal and institutional outcomes: do authenticated-only evidentiary standards systematically exclude true claims from unconnected populations at rates that exceed false-positive prevention? Comparative analysis across jurisdictions with different verification requirements.',
    'If exclusion rates are low and fraud prevention is high, the constraint operates as claimed coordination. If exclusion of true claims is systematic and concentrated on powerless populations, the constraint operates as structural suppression of disfavored testimony regardless of accuracy.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(evidentiary_exclusion_vs_fraud_prevention, preference, 'Whether evidentiary exclusion is a necessary cost or an unjust outcome of the verification paradigm.').

omega_variable(
    protocol_governance_capture,
    'Are the blockchain infrastructure providers genuinely neutral protocol operators, or do they extract rents by controlling authentication standards in ways that favor their commercial interests over verification accuracy?',
    'Audit of protocol governance decisions: do standard changes correlate with revenue optimization for infrastructure providers, or with verification accuracy improvements? Analysis of rejected protocol proposals from non-commercial actors.',
    'If governance is captured, the constraint''s extraction is higher than the base metric suggests because protocol evolution serves rent-seeking rather than coordination. If governance is neutral, the extraction is limited to transaction fees and the coordination function is preserved.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(protocol_governance_capture, empirical, 'Whether protocol governance serves verification accuracy or infrastructure provider revenue.').

omega_variable(
    reading_under_determination,
    'Is this constraint one reading of a contested kernel (visual evidentiary authority), or is it the only structurally coherent response to the deepfake threat?',
    'Viability demonstration of sibling readings: if indexical_realism (artifact analysis) or alternative architectures (offline verification, open protocols) achieve comparable deepfake protection without the access barriers, the kernel is genuinely contested. If all alternatives fail at scale, this reading is the only viable implementation.',
    'If the kernel is contested, the constraint''s claimed_type (tangled_rope) is one framing among several and the extraction is a design choice. If this reading is the only viable response, the extraction is the unavoidable cost of solving the founding problem and the constraint should compute closer to rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_under_determination, conceptual, 'Whether alternative readings of the visual evidentiary authority kernel are structurally viable.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(distributed_verification, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dist_tr_t0, distributed_verification, theater_ratio, 0, 0.22).
narrative_ontology:measurement(dist_tr_t5, distributed_verification, theater_ratio, 5, 0.27).
narrative_ontology:measurement(dist_tr_t10, distributed_verification, theater_ratio, 10, 0.32).
narrative_ontology:measurement(dist_tr_t15, distributed_verification, theater_ratio, 15, 0.36).
narrative_ontology:measurement(dist_tr_t20, distributed_verification, theater_ratio, 20, 0.39).
narrative_ontology:measurement(dist_tr_t25, distributed_verification, theater_ratio, 25, 0.42).

% Extraction over time
narrative_ontology:measurement(dist_be_t0, distributed_verification, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(dist_be_t5, distributed_verification, base_extractiveness, 5, 0.53).
narrative_ontology:measurement(dist_be_t10, distributed_verification, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(dist_be_t15, distributed_verification, base_extractiveness, 15, 0.62).
narrative_ontology:measurement(dist_be_t20, distributed_verification, base_extractiveness, 20, 0.66).
narrative_ontology:measurement(dist_be_t25, distributed_verification, base_extractiveness, 25, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(dist_su_t0, distributed_verification, suppression_requirement, 0, 0.52).
narrative_ontology:measurement(dist_su_t5, distributed_verification, suppression_requirement, 5, 0.57).
narrative_ontology:measurement(dist_su_t10, distributed_verification, suppression_requirement, 10, 0.62).
narrative_ontology:measurement(dist_su_t15, distributed_verification, suppression_requirement, 15, 0.66).
narrative_ontology:measurement(dist_su_t20, distributed_verification, suppression_requirement, 20, 0.69).
narrative_ontology:measurement(dist_su_t25, distributed_verification, suppression_requirement, 25, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(distributed_verification, global_infrastructure).
narrative_ontology:boltzmann_floor_override(distributed_verification, 0.18).
narrative_ontology:affects_constraint(distributed_verification, indexical_realism).
narrative_ontology:affects_constraint(distributed_verification, epistemic_collapse).
narrative_ontology:affects_constraint(distributed_verification, post_evidentiary).

% DUAL FORMULATION NOTE:
% This constraint is one of four readings of the visual_evidentiary_authority kernel. The distributed_verification reading treats cryptographic authentication as genuine coordination with embedded extraction. The indexical_realism reading treats the same infrastructure as theater layered over unchanged forensic authority. The epistemic_collapse reading treats all verification methods as failed. The post_evidentiary reading treats visual documentation as having lost evidentiary status entirely. All four readings must link via network.affects_constraints because they are alternative framings of the same contested kernel, not independent constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(distributed_verification, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
