% ============================================================================
% CONSTRAINT STORY: post_evidentiary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_post_evidentiary, []).

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
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: post_evidentiary
 *   human_readable: Post-Evidentiary Consensus Regime
 *   domain: technology_governance/information_epistemology
 *
 * SUMMARY:
 *   The post-evidentiary reading holds that visual media never possessed the
 *   indexical authority claimed for it, that social consensus always grounded
 *   truth claims, and that the generative AI crisis merely makes this
 *   explicit. Under this reading, institutions adapt by openly acknowledging
 *   that truth determination is a social negotiation process mediated by
 *   consensus brokers. The constraint coordinates expectations about how
 *   claims will be evaluated while extracting from those who lack the network
 *   position to participate in consensus formation. The claim/metric
 *   divergence is deliberate: the constraint is claimed as tangled_rope
 *   (genuine coordination function with asymmetric extraction) while the
 *   metrics track rising extraction and suppression as the regime matures and
 *   consensus brokerage consolidates.
 *
 * KEY AGENTS:
 *   - consensus_brokers: Agenda setters (institutional/arbitrage) — operate the platforms where evidence is negotiated into narrative; extract by positioning as necessary intermediaries
 *   - platform_verification_services: Beneficiaries (powerful/mobile) — provide technical services that facilitate consensus formation; benefit from shift from proof to negotiation
 *   - institutional_narrative_authorities: Beneficiaries (institutional/constrained) — legacy institutions whose existing practice is vindicated by explicit acknowledgment of consensus primacy
 *   - marginalized_claimants: Payers (powerless/trapped) — hold evidence but lack network position to achieve consensus; structural disadvantage made explicit
 *   - non_networked_witnesses: Payers (moderate/constrained) — document events but lack institutional backing; claims systematically discounted in negotiation
 *   - legacy_forensic_practitioners: Payers (organized/identity_locked) — expertise devalued by acknowledgment that their function was legitimating consensus, not discovering truth
 *   - distributed_verification_advocates: Excluded (organized/mobile) — propose cryptographic alternatives that would disintermediate consensus brokers
 *   - epistemology_researchers: Observers (analytical/analytical) — study how truth claims are validated across epistemic regimes
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(post_evidentiary, 0.68).
domain_priors:suppression_score(post_evidentiary, 0.72).
domain_priors:theater_ratio(post_evidentiary, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(post_evidentiary, extractiveness, 0.68).
narrative_ontology:constraint_metric(post_evidentiary, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(post_evidentiary, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(post_evidentiary, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(post_evidentiary, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(post_evidentiary, tangled_rope).
narrative_ontology:human_readable(post_evidentiary, "Post-Evidentiary Consensus Regime").
narrative_ontology:topic_domain(post_evidentiary, "technology_governance/information_epistemology").

domain_priors:requires_active_enforcement(post_evidentiary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(post_evidentiary, '12b59c25-e9fd-467e-bd8c-86ac189cbb07').
narrative_ontology:cs_kernel_codification('12b59c25-e9fd-467e-bd8c-86ac189cbb07', distributed).
narrative_ontology:cs_authority_grounding('12b59c25-e9fd-467e-bd8c-86ac189cbb07', distributed).
narrative_ontology:cs_reading_relation('12b59c25-e9fd-467e-bd8c-86ac189cbb07', visual_evidentiary_authority__indexical_realism, forecloses).
narrative_ontology:cs_reading_relation('12b59c25-e9fd-467e-bd8c-86ac189cbb07', visual_evidentiary_authority__epistemic_collapse, coexists_with).
narrative_ontology:cs_reading_relation('12b59c25-e9fd-467e-bd8c-86ac189cbb07', visual_evidentiary_authority__distributed_verification, influences).
narrative_ontology:cs_axiom('12b59c25-e9fd-467e-bd8c-86ac189cbb07', foundational, consensus_primacy_over_indexicality).
narrative_ontology:cs_axiom_status(consensus_primacy_over_indexicality, holdable).
narrative_ontology:cs_axiom_grounding('12b59c25-e9fd-467e-bd8c-86ac189cbb07', consensus_primacy_over_indexicality, empirically_contingent).
narrative_ontology:cs_axiom('12b59c25-e9fd-467e-bd8c-86ac189cbb07', secondary, explicit_acknowledgment_as_adaptation).
narrative_ontology:cs_axiom_status(explicit_acknowledgment_as_adaptation, holdable).
narrative_ontology:cs_axiom_grounding('12b59c25-e9fd-467e-bd8c-86ac189cbb07', explicit_acknowledgment_as_adaptation, instrumental).
narrative_ontology:cs_reference_frame('12b59c25-e9fd-467e-bd8c-86ac189cbb07', indexical_evidentiary_regime).
narrative_ontology:cs_drift_state('12b59c25-e9fd-467e-bd8c-86ac189cbb07', post_generative_ai_proliferation, gap(axiom_overriding, severe, true)).
narrative_ontology:cs_created_at('12b59c25-e9fd-467e-bd8c-86ac189cbb07', '').
narrative_ontology:cs_kernel_id(post_evidentiary, visual_evidentiary_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(post_evidentiary, consensus_brokers).
narrative_ontology:constraint_beneficiary(post_evidentiary, platform_verification_services).
narrative_ontology:constraint_beneficiary(post_evidentiary, institutional_narrative_authorities).
narrative_ontology:constraint_victim(post_evidentiary, marginalized_claimants).
narrative_ontology:constraint_victim(post_evidentiary, non_networked_witnesses).
narrative_ontology:constraint_victim(post_evidentiary, legacy_forensic_practitioners).
narrative_ontology:constraint_vindicates(post_evidentiary, social_construction_of_truth).
narrative_ontology:constraint_vindicates(post_evidentiary, consensus_primacy_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operate the platforms and protocols where visual evidence is negotiated into accepted narrative. Set the rules for what counts as corroboration, which voices weight more heavily, and when consensus is declared reached. Extract value by positioning themselves as necessary intermediaries between raw claims and social acceptance.
narrative_ontology:constraint_stakeholder(post_evidentiary, consensus_brokers, agenda_setter,
    institutional, generational, arbitrage, global).

% Provide technical services that assess visual media authenticity and provenance. Benefit from the regime's explicit acknowledgment that verification is a social service rather than a technical proof — their business model shifts from 'detecting fakes' to 'facilitating consensus formation,' which is a larger and more defensible market.
narrative_ontology:constraint_stakeholder(post_evidentiary, platform_verification_services, beneficiary,
    powerful, biographical, mobile, global).

% Legacy institutions whose authority was already grounded in narrative control rather than evidentiary proof. The post-evidentiary regime vindicates their existing practice by making explicit what was always implicit: that institutional credibility, not indexical proof, determines what counts as true.
narrative_ontology:constraint_stakeholder(post_evidentiary, institutional_narrative_authorities, beneficiary,
    institutional, generational, constrained, national).

% Hold visual evidence of harms but lack the network position to achieve consensus around their claims. Under the indexical regime they could appeal to 'the camera doesn't lie'; under the post-evidentiary regime their claims are explicitly subject to consensus formation processes they cannot influence. The shift from proof to negotiation makes their structural disadvantage explicit and harder to contest.
narrative_ontology:constraint_stakeholder(post_evidentiary, marginalized_claimants, payer,
    powerless, immediate, trapped, local).

% Individuals who document events but are not embedded in the social networks where consensus forms. Their visual evidence enters a negotiation process where their lack of institutional backing or network centrality means their claims are systematically discounted. They pay the cost of the regime's honesty about consensus primacy.
narrative_ontology:constraint_stakeholder(post_evidentiary, non_networked_witnesses, payer,
    moderate, biographical, constrained, regional).

% Professionals whose expertise was grounded in the indexical authority of visual media. The post-evidentiary regime devalues their technical skills by making explicit that their real function was always to provide institutional legitimacy to consensus, not to discover objective truth. Their professional identity is constituted through the indexical frame; exit means abandoning the expertise that defines them.
narrative_ontology:constraint_stakeholder(post_evidentiary, legacy_forensic_practitioners, payer,
    organized, biographical, identity_locked, national).

% Technologists and activists who argue for cryptographic provenance and decentralized verification as an alternative to both indexical realism and consensus brokerage. They are structurally excluded from the post-evidentiary regime because their approach would route around the consensus brokers who set the rules.
narrative_ontology:constraint_stakeholder(post_evidentiary, distributed_verification_advocates, excluded,
    organized, generational, mobile, global).

% Study how truth claims are validated across different epistemic regimes. They observe that the post-evidentiary reading makes explicit what social epistemology has long argued: that consensus, not correspondence, grounds collective truth. They analyze the distributional consequences of this acknowledgment without being positioned within the regime itself.
narrative_ontology:constraint_stakeholder(post_evidentiary, epistemology_researchers, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable framework for collective truth determination in an environment where visual media can no longer serve as indexical proof. Coordinates expectations about how claims will be evaluated and what counts as sufficient corroboration.
% TRANSFER_FUNCTION: Moves epistemic authority from those who hold visual evidence to those who can mobilize consensus around interpretations of that evidence. Transfers the costs of uncertainty from institutions to individuals who lack network position.
% ABSENT_VOICES: Distributed verification advocates who would argue for cryptographic provenance systems are excluded because their approach would disintermediate the consensus brokers. Marginalized claimants whose harms are documented but cannot achieve consensus are structurally silenced by the regime's explicit acknowledgment that evidence alone is insufficient.
% DISAPPEARANCE_RATIONALE: If the post-evidentiary regime vanished, institutions would face a legitimacy crisis as they could neither return to indexical realism (the technology has moved past that) nor operate without an explicit framework for truth determination. Legal systems, journalism, and historical documentation would need to reconstruct their evidentiary standards. The consensus brokerage industry would collapse and marginalized claimants would face different but not necessarily better barriers to having their claims heard.
% FOUNDING_PROBLEM: The proliferation of generative AI and deepfake technology made it impossible to maintain the fiction that visual media provides indexical proof of events. Institutions needed a new framework for truth determination that could function without relying on the evidentiary authority of images and video.
% FOUNDING_PROBLEM_CORROBORATION: The technological impossibility of reliable visual authentication is attested by cryptographers, computer vision researchers, and digital forensics practitioners across multiple institutions. The need for a new epistemic framework is acknowledged even by those who contest this particular solution — including distributed verification advocates and defenders of indexical realism who agree the problem is real even as they propose different responses.
narrative_ontology:disappearance_verdict(post_evidentiary, world_rearranges).
narrative_ontology:founding_problem_status(post_evidentiary, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(post_evidentiary, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-16',
    'unspecified', 'agent/example_platform_commission.json',
    'claude-sonnet-4-5-20250929', 'unspecified').
narrative_ontology:story_seed(post_evidentiary, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(post_evidentiary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(post_evidentiary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(post_evidentiary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is substantial (0.68 at interval end) because the regime transfers epistemic authority from evidence-holders to consensus brokers, creating a new intermediary class that captures value from the negotiation process. Suppression is high (0.72) because the regime requires active enforcement to prevent alternative verification systems (cryptographic provenance, distributed consensus) from routing around the brokerage layer. Theater ratio is moderate (0.41) because the coordination function is real — institutions genuinely need a framework for truth determination — but a growing share of activity is performative legitimation of predetermined consensus rather than genuine negotiation. Accessibility collapse is moderate (0.48) because alternative epistemic frameworks remain conceptually available even as they are structurally suppressed. Resistance is high (0.71) because marginalized claimants, forensic practitioners, and distributed verification advocates all contest the regime, though from different positions.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter and beneficiary seats should compute as coordination with modest extraction (the regime solves a real problem and they benefit from the solution). The payer seats should compute as substantially extractive (the regime makes their structural disadvantage explicit and harder to contest). The identity-locked forensic practitioners should show the highest effective extraction (the regime devalues their expertise while their exit options are constrained by professional identity). The engine computes these divergences from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Consensus brokers are structural beneficiaries (set the rules, extract intermediation rents, arbitrage exit options). Platform verification services and institutional narrative authorities benefit from the regime's vindication of their existing practice. Marginalized claimants and non-networked witnesses are targets (bear the cost of explicit consensus primacy, trapped or constrained exit). Legacy forensic practitioners are identity-locked targets (professional identity constituted through the indexical frame the regime explicitly rejects). Distributed verification advocates are excluded rather than coordinated — their structural position is outside the regime by design.
 *
 * MANDATROPHY ANALYSIS:
 *   The post-evidentiary regime risks mandatrophy if the coordination function (providing a stable framework for truth determination) becomes secondary to the extraction function (consensus brokerage as rent collection). The rising theater ratio and suppression requirement suggest this drift is underway. The regime would be mandatrophic if consensus brokers persist in extracting intermediation rents after distributed verification technologies make their coordination function obsolete. The omega variables document the empirical uncertainties that determine whether this is genuine adaptation or captured transition.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    consensus_vs_capture,
    'Is the post-evidentiary regime''s consensus formation process a genuine coordination mechanism or a cover story for epistemic capture by network-positioned actors?',
    'Longitudinal analysis of whose claims achieve consensus and whose do not, controlling for evidentiary strength. If consensus systematically favors institutionally-backed claims regardless of evidence quality, the regime is extractive capture. If consensus tracks evidence quality across institutional position, the coordination function is real.',
    'If the regime is extractive capture, it should be classified as snare rather than tangled_rope, and the coordination function is theater. If genuine coordination, the extraction is the price of solving a real epistemic problem.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consensus_vs_capture, empirical, 'Whether consensus formation coordinates or captures epistemic authority.').

omega_variable(
    distributed_verification_viability,
    'Are cryptographic provenance and distributed verification systems technically viable alternatives to consensus brokerage, or do they face insurmountable coordination problems?',
    'Deployment of distributed verification systems at scale and measurement of adoption rates, technical reliability, and resistance to gaming. If they achieve widespread adoption without consensus brokerage, the post-evidentiary regime''s suppression of alternatives is extractive. If they fail to coordinate at scale, the regime''s brokerage function is necessary.',
    'If distributed verification is viable, the post-evidentiary regime''s suppression of alternatives is pure extraction and the regime should be classified as snare. If distributed verification fails at scale, the regime''s coordination function is genuine and the extraction is the cost of centralized coordination.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(distributed_verification_viability, empirical, 'Whether decentralized alternatives can coordinate without brokerage.').

omega_variable(
    indexical_authority_recovery,
    'Is the loss of indexical authority for visual media permanent, or could technical advances in authentication restore it?',
    'Development of authentication technologies that can reliably distinguish generated from captured media at scale, or formal proofs that such authentication is computationally infeasible. If authentication is restored, the post-evidentiary regime''s premise is falsified. If authentication is proven impossible, the regime''s premise is vindicated.',
    'If indexical authority can be restored, the post-evidentiary regime is a premature surrender to technical limitations and should be classified as extractive capture of a temporary crisis. If indexical authority is permanently lost, the regime is a necessary adaptation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(indexical_authority_recovery, empirical, 'Whether visual media''s evidentiary authority can be technically restored.').

omega_variable(
    marginalized_claimant_outcomes,
    'Under the post-evidentiary regime, do marginalized claimants have better or worse outcomes than under the indexical regime''s fiction of objective proof?',
    'Comparative analysis of claim success rates for marginalized vs. institutionally-backed claimants before and after the regime''s adoption. If marginalized claimants'' success rates decline relative to institutional claimants, the regime increases extraction from the powerless. If success rates equalize, the regime''s honesty about consensus primacy may reduce institutional advantage.',
    'If outcomes worsen for marginalized claimants, the regime''s extraction is concentrated on the powerless and classification should weight toward snare. If outcomes improve, the regime''s coordination function may outweigh its extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(marginalized_claimant_outcomes, empirical, 'Whether explicit consensus primacy helps or harms the powerless.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(post_evidentiary, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(post_tr_t0, post_evidentiary, theater_ratio, 0, 0.22).
narrative_ontology:measurement(post_tr_t5, post_evidentiary, theater_ratio, 5, 0.27).
narrative_ontology:measurement(post_tr_t10, post_evidentiary, theater_ratio, 10, 0.32).
narrative_ontology:measurement(post_tr_t15, post_evidentiary, theater_ratio, 15, 0.36).
narrative_ontology:measurement(post_tr_t20, post_evidentiary, theater_ratio, 20, 0.39).
narrative_ontology:measurement(post_tr_t25, post_evidentiary, theater_ratio, 25, 0.41).

% Extraction over time
narrative_ontology:measurement(post_be_t0, post_evidentiary, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(post_be_t5, post_evidentiary, base_extractiveness, 5, 0.49).
narrative_ontology:measurement(post_be_t10, post_evidentiary, base_extractiveness, 10, 0.56).
narrative_ontology:measurement(post_be_t15, post_evidentiary, base_extractiveness, 15, 0.62).
narrative_ontology:measurement(post_be_t20, post_evidentiary, base_extractiveness, 20, 0.66).
narrative_ontology:measurement(post_be_t25, post_evidentiary, base_extractiveness, 25, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(post_su_t0, post_evidentiary, suppression_requirement, 0, 0.51).
narrative_ontology:measurement(post_su_t5, post_evidentiary, suppression_requirement, 5, 0.57).
narrative_ontology:measurement(post_su_t10, post_evidentiary, suppression_requirement, 10, 0.63).
narrative_ontology:measurement(post_su_t15, post_evidentiary, suppression_requirement, 15, 0.67).
narrative_ontology:measurement(post_su_t20, post_evidentiary, suppression_requirement, 20, 0.7).
narrative_ontology:measurement(post_su_t25, post_evidentiary, suppression_requirement, 25, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(post_evidentiary, information_standard).
narrative_ontology:affects_constraint(post_evidentiary, indexical_realism).
narrative_ontology:affects_constraint(post_evidentiary, epistemic_collapse).
narrative_ontology:affects_constraint(post_evidentiary, distributed_verification).

% DUAL FORMULATION NOTE:
% The visual_evidentiary_authority kernel decomposes into four readings with different ε values and victim sets. The post_evidentiary reading (this constraint) has moderate-high extraction (0.68) concentrated on marginalized claimants and non-networked witnesses. The indexical_realism reading has lower extraction but higher suppression (requires active denial of generative AI capabilities). The epistemic_collapse reading has the highest extraction (no stable framework means pure power determines truth). The distributed_verification reading has the lowest extraction but faces coordination challenges. All four readings are linked via network.affects_constraints because they compete for institutional adoption and each reading's success changes the operating environment for the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(post_evidentiary, organized, 0.78).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
