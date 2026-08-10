% ============================================================================
% CONSTRAINT STORY: sociotechnical_risk_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sociotechnical_risk_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: sociotechnical_risk_reading
 *   human_readable: Persona-Simulation Infrastructure as Latent Targeting-Inference Capability
 *   domain: AI Evaluation Infrastructure / Simulated-User Research Methodology
 *
 * SUMMARY:
 *   A research paper introduces a dependency-graph population sampler paired
 *   with frontier LLMs to generate 8.3 billion attribute-conditioned
 *   behavioral personas for AI evaluation research. The paper includes a
 *   responsible-use section (§M) warning against impersonation, targeting,
 *   price discrimination, and protected-group profiling. This reading argues
 *   that the central fact about the artifact is not whether it behaves
 *   validly in its intended evaluation use (the instrumentalist-screening
 *   reading) or whether its personas correspond faithfully to real humans
 *   (the representational-correspondence reading), but that a general-purpose
 *   targeting-inference engine now exists in the world, usable by anyone with
 *   access to the sampler and an LLM, regardless of the paper's own stated
 *   intentions. The externality does not require misuse to have occurred —
 *   the capability's existence is itself the constraint.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sociotechnical_risk_reading, 0.68).
domain_priors:suppression_score(sociotechnical_risk_reading, 0.55).
domain_priors:theater_ratio(sociotechnical_risk_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sociotechnical_risk_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(sociotechnical_risk_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(sociotechnical_risk_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sociotechnical_risk_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(sociotechnical_risk_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sociotechnical_risk_reading, tangled_rope).
narrative_ontology:human_readable(sociotechnical_risk_reading, "Persona-Simulation Infrastructure as Latent Targeting-Inference Capability").
narrative_ontology:topic_domain(sociotechnical_risk_reading, "AI Evaluation Infrastructure / Simulated-User Research Methodology").

domain_priors:requires_active_enforcement(sociotechnical_risk_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sociotechnical_risk_reading, '0eb282d0-3469-421d-8583-56e1dc0b5fec').
narrative_ontology:cs_kernel_codification('0eb282d0-3469-421d-8583-56e1dc0b5fec', distributed).
narrative_ontology:cs_authority_grounding('0eb282d0-3469-421d-8583-56e1dc0b5fec', distributed).
narrative_ontology:cs_reading_relation('0eb282d0-3469-421d-8583-56e1dc0b5fec', sociotechnical_risk_reading__instrumentalist_screening_reading, coexists_with).
narrative_ontology:cs_reading_relation('0eb282d0-3469-421d-8583-56e1dc0b5fec', sociotechnical_risk_reading__representational_correspondence_reading, influences).
narrative_ontology:cs_reading_relation('0eb282d0-3469-421d-8583-56e1dc0b5fec', sociotechnical_risk_reading__behavioral_mechanism_reading, coexists_with).
narrative_ontology:cs_axiom('0eb282d0-3469-421d-8583-56e1dc0b5fec', foundational, artifact_existence_is_the_hazard).
narrative_ontology:cs_axiom_status(artifact_existence_is_the_hazard, holdable).
narrative_ontology:cs_axiom_grounding('0eb282d0-3469-421d-8583-56e1dc0b5fec', artifact_existence_is_the_hazard, empirically_contingent).
narrative_ontology:cs_axiom('0eb282d0-3469-421d-8583-56e1dc0b5fec', foundational, non_drawn_population_cannot_consent).
narrative_ontology:cs_axiom_status(non_drawn_population_cannot_consent, holdable).
narrative_ontology:cs_axiom_grounding('0eb282d0-3469-421d-8583-56e1dc0b5fec', non_drawn_population_cannot_consent, deontological).
narrative_ontology:cs_reference_frame('0eb282d0-3469-421d-8583-56e1dc0b5fec', responsible_use_appendix_as_sufficient_governance).
narrative_ontology:cs_drift_state('0eb282d0-3469-421d-8583-56e1dc0b5fec', post_deployment_diffusion, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('0eb282d0-3469-421d-8583-56e1dc0b5fec', '').
narrative_ontology:cs_kernel_id(sociotechnical_risk_reading, persona_as_valid_proxy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sociotechnical_risk_reading, infrastructure_builders).
narrative_ontology:constraint_beneficiary(sociotechnical_risk_reading, frontier_lab_funding_partners).
narrative_ontology:constraint_beneficiary(sociotechnical_risk_reading, downstream_commercial_deployers).
narrative_ontology:constraint_victim(sociotechnical_risk_reading, encoded_demographic_population).
narrative_ontology:constraint_victim(sociotechnical_risk_reading, real_world_analog_individuals).
narrative_ontology:constraint_victim(sociotechnical_risk_reading, protected_group_members_at_inference_risk).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Designs and maintains the dependency-graph population sampler, the compatibility rules, and the demographic calibration layer that together produce 8.3 billion attribute-conditioned behavioral profiles. Frames the artifact as an evaluation tool and writes the responsible-use section (§M) as the governing document for downstream use, but does not control what happens once the sampler-plus-LLM pairing leaves their hands. Collects academic credit, funding renewal, and platform adoption from the artifact's existence.
narrative_ontology:constraint_stakeholder(sociotechnical_risk_reading, infrastructure_builders, agenda_setter,
    institutional, generational, arbitrage, global).

% Funds and integrates the persona infrastructure into evaluation and product pipelines (OpenAI, Anthropic, Meta named as examples). Gains a general-purpose simulated-user capability applicable far beyond the stated evaluation use case, at no marginal cost of building demographic calibration in-house. Faces no binding obligation tied to the responsible-use disclaimer.
narrative_ontology:constraint_stakeholder(sociotechnical_risk_reading, frontier_lab_funding_partners, beneficiary,
    institutional, generational, arbitrage, global).

% Third parties who license or fork the persona-generation approach for targeting, price discrimination, or synthetic-audience testing that the paper's §M explicitly warns against. Nothing in the artifact's technical structure prevents this repurposing; the only barrier is a norms-based appendix with no enforcement mechanism.
narrative_ontology:constraint_stakeholder(sociotechnical_risk_reading, downstream_commercial_deployers, beneficiary,
    powerful, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(sociotechnical_risk_reading, downstream_commercial_deployers, agenda_setter).

% The statistical population whose demographic patterns, correlations, and behavioral tendencies are encoded into the compatibility rules and calibration weights, even though no individual was consented or drawn from directly — the population is synthetic in the sense that it was assembled from aggregate distributions, not recruited. Bears the externality risk of having their demographic patterns made newly computable and targetable at scale, with no mechanism by which they could object, be identified, or withdraw, because they were never named as participants in the first place.
narrative_ontology:constraint_stakeholder(sociotechnical_risk_reading, encoded_demographic_population, payer,
    powerless, civilizational, trapped, global).

% Actual living people whose demographic profile matches one of the 8.3 billion attribute-conditioned personas closely enough that impersonation, targeted manipulation, or price discrimination against them becomes newly feasible once the capability exists. They have no relationship to the original research project, never volunteered anything, and have no visibility into whether a profile resembling them has been generated or used against them commercially.
narrative_ontology:constraint_stakeholder(sociotechnical_risk_reading, real_world_analog_individuals, payer,
    powerless, biographical, trapped, global).

% Members of legally or socially protected categories (race, disability, religion, etc.) whose group-level behavioral correlations are, per the paper's own §M warning, newly inferable and exploitable through the calibration and compatibility-rule structure. The existence of the capability itself constitutes exposure, independent of any specific downstream use — the risk is the artifact's existence in the world, not a documented instance of misuse.
narrative_ontology:constraint_stakeholder(sociotechnical_risk_reading, protected_group_members_at_inference_risk, payer,
    powerless, generational, trapped, global).

% The subset of the population who did volunteer and consent to have their data used for the original research purpose. Their consent covers only the research use case, not the general-purpose targeting-inference capability that emerges structurally from the same infrastructure. They are excluded from this externality conversation because the consent framework treats them as the whole story, when the synthetic 8.3B-record population extrapolated from them was never consented to at all.
narrative_ontology:constraint_stakeholder(sociotechnical_risk_reading, volunteer_research_participants, excluded,
    moderate, biographical, constrained, national).

% Third-party researchers, auditors, and policy bodies evaluating whether the artifact's dual-use structure warrants restricted release, red-teaming requirements, or regulatory intervention. They read the §M warnings not as a disclaimer appendix but as the central structural fact about what the artifact is capable of doing.
narrative_ontology:constraint_stakeholder(sociotechnical_risk_reading, ai_safety_and_policy_reviewers, observer,
    organized, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(sociotechnical_risk_reading, diffuse).
narrative_ontology:fixing_cost_class(sociotechnical_risk_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The infrastructure solves a genuine methodological problem: evaluating LLM-based systems against a large, demographically diverse, attribute-conditioned population of simulated users is far cheaper and more scalable than recruiting and compensating millions of real human testers for every evaluation cycle.
% TRANSFER_FUNCTION: The arrangement moves a general-purpose targeting-inference and behavioral-simulation capability from a research artifact framed around evaluation into the hands of funding partners and downstream commercial deployers, while the externality risk (impersonation, targeting, price discrimination, protected-group profiling) is transferred onto a diffuse, non-consenting population that was never party to the original research agreement at all.
% ABSENT_VOICES: The 8.3 billion synthetic profiles have no representative in the room because they were never drawn from named individuals — there is no consent body to object because consent was never sought from a population that did not exist as a population until the sampler assembled it from aggregate statistics. Real individuals who happen to match a generated profile closely are similarly absent; they do not know the capability exists, let alone that it might be used against them.
% DISAPPEARANCE_RATIONALE: If the persona-generation infrastructure and its LLM pairing vanished overnight, funding partners would lose a cheap, scalable simulated-user capability and would need to fall back on smaller-scale, actually-consented human panels for evaluation; downstream commercial deployers would lose an off-the-shelf targeting-inference substrate; the diffuse population currently exposed to the externality risk would no longer be exposed by this specific artifact (though the underlying demographic data and modeling techniques would likely persist elsewhere in less consolidated form).
% FOUNDING_PROBLEM: LLM-based systems needed cheap, diverse, at-scale evaluation against a range of user types without the cost, slowness, and ethical overhead of recruiting millions of real human evaluators for every test cycle.
% FOUNDING_PROBLEM_CORROBORATION: The infrastructure builders and funding partners attest the founding problem (evaluation scalability) is live and the artifact is a proportionate solution to it. Independent AI safety and policy reviewers — outside the benefiting parties — attest that the evaluation problem, while real, does not require or justify a general-purpose targeting-inference capability with no access controls, and that the responsible-use section's own warnings corroborate that the builders themselves recognize the dual-use risk without having built any structural mechanism to contain it.
narrative_ontology:disappearance_verdict(sociotechnical_risk_reading, world_rearranges).
narrative_ontology:founding_problem_status(sociotechnical_risk_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sociotechnical_risk_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-10',
    'unspecified', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'unspecified').
narrative_ontology:story_seed(sociotechnical_risk_reading, 'none', 1).
narrative_ontology:epsilon_provenance(sociotechnical_risk_reading, 0.68, 'claude-sonnet-5', 'matraix_persona_simulation_2026_20260810_114056', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sociotechnical_risk_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(sociotechnical_risk_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(sociotechnical_risk_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.68 and rising because the value the infrastructure generates (a scalable, demographically calibrated behavioral-simulation capability) accrues overwhelmingly to institutional builders and funding partners while the externality risk accrues to a population that has no relationship to the research agreement and no visibility into the artifact's existence. Suppression is moderate (0.55): there is no direct coercion of the payer population, but the mechanism by which they could object or withdraw simply does not exist, because they were never consented as a population in the first place — this is closer to structural exclusion from the consent process than to active coercion. Theater ratio rises over the interval (0.30 to 0.58) as the responsible-use section increasingly functions as reputational cover — cited in press and grant materials as evidence of ethical diligence — while the underlying capability continues to diffuse into downstream commercial use unconstrained by any enforcement mechanism tied to §M's warnings.
 *
 * PERSPECTIVAL GAP:
 *   From the infrastructure-builder seat, the artifact is unambiguously coordination: it solves a real, costly evaluation problem and the responsible-use section demonstrates good-faith risk management. From the encoded-population and real-world-analog seats — who have no seat at all in the ordinary sense, since they were never recruited as participants — the same structure is an externality-generating machine whose risk profile does not depend on anyone's intentions. The engine should compute these as structurally different seats because the positional atoms differ starkly: institutional/arbitrage/global for the builders versus powerless/trapped/global for the exposed population.
 *
 * DIRECTIONALITY LOGIC:
 *   Infrastructure builders and funding partners sit near the full-beneficiary end: they collect academic credit, platform adoption, and a reusable capability, with arbitrage-grade exit from any consequence of downstream misuse (the responsible-use disclaimer functions as a liability shield, not a control). Downstream commercial deployers are also structural beneficiaries — they inherit the capability without inheriting the original ethical review. The encoded demographic population, real-world analog individuals, and protected-group members are targets in the fullest sense: trapped exit (they cannot withdraw from a population they were never told they were part of), civilizational-to-generational time horizon (the exposure persists as long as the artifact and its derivatives exist), and no channel to register consent or dissent.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — cheap, scalable evaluation without recruiting millions of human testers — is real and was live when the infrastructure was built. But the sociotechnical-risk reading holds that the artifact's continued framing as 'an evaluation tool with a responsible-use appendix' has outlived what that framing can actually govern: the capability generalizes far past the evaluation use case, and the appendix does no work to contain that generalization. This is not a claim that the original coordination function was fake — it is a claim that the mandate ('build an evaluation tool') no longer bounds what has actually been built ('a general-purpose demographic targeting-inference substrate'), and that treating the responsible-use section as sufficient governance is itself the mislabeling this classification exists to catch.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    consent_scope_boundary,
    'Does volunteer consent from the original real-participant population extend, even loosely, to cover the synthetic extrapolated population of 8.3 billion attribute-conditioned profiles derived from aggregate patterns in that data?',
    'Legal and ethical review of whether consent frameworks for aggregate statistical modeling can meaningfully cover downstream synthetic population generation at a scale and generality far exceeding the original study design; comparison to precedent in differential-privacy and synthetic-data governance law.',
    'If consent scope is found to structurally cover the synthetic population, the extraction/victim framing weakens substantially and the constraint moves toward a rope or scaffold reading. If it does not, the tangled_rope reading (or a harder snare reading) is reinforced.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(consent_scope_boundary, conceptual, 'Whether original research consent extends to the synthetic extrapolated population.').

omega_variable(
    responsible_use_enforceability,
    'Is the §M responsible-use section a binding constraint on downstream use (license terms, access gating, audit requirements) or purely an advisory disclaimer with no enforcement mechanism?',
    'Review of the artifact''s actual licensing terms, access controls, and any documented enforcement or takedown actions taken against violators of the stated responsible-use guidelines.',
    'If enforceable, theater_ratio should be revised sharply downward and the classification could shift toward scaffold (transitional coordination with real teeth) or rope. If purely advisory, the tangled_rope/high-theater reading is corroborated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(responsible_use_enforceability, empirical, 'Whether the responsible-use section carries any enforcement mechanism.').

omega_variable(
    counterfactual_capability_diffusion,
    'Would an equivalent targeting-inference capability have emerged from other sources (commercial ad-tech, data brokers, existing demographic modeling firms) even without this specific research artifact, making the marginal externality attributable to this constraint smaller than it appears?',
    'Survey of existing commercial demographic-profiling and ad-targeting infrastructure to assess whether comparable attribute-conditioned behavioral simulation at this scale and fidelity already existed prior to this artifact''s release.',
    'If comparable capability already existed elsewhere, the marginal extractiveness attributable to this specific research artifact is lower and the beneficiary/victim asymmetry is less concentrated on this constraint specifically. If this artifact represents a genuine capability leap, the extraction attribution is stronger.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(counterfactual_capability_diffusion, empirical, 'Whether the capability is genuinely novel or a marginal addition to existing commercial infrastructure.').

omega_variable(
    kernel_reading_incommensurability,
    'Can the instrumentalist-screening and representational-correspondence readings'' satisfaction (the persona behaves as assigned, matches real distributions) coexist with a high-externality-risk verdict under this reading, or does technical success on the first two readings necessarily aggravate the risk this reading identifies?',
    'Structural analysis of whether improving persona fidelity (serving the correspondence reading) is causally coupled to improving targeting-inference precision (worsening the risk this reading identifies) — i.e., whether these are the same technical capability viewed from two evaluative angles.',
    'If fidelity improvements and targeting-inference risk are causally coupled (the same technical advance serves both), the sibling readings are not merely coexisting but structurally entangled — success by the correspondence reading''s standard directly increases this reading''s ε. If decoupled, the readings can be pursued independently with different risk profiles.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_incommensurability, conceptual, 'Whether technical progress on persona fidelity is causally coupled to targeting-inference risk.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sociotechnical_risk_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(soci_tr_t0, sociotechnical_risk_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(soci_tr_t4, sociotechnical_risk_reading, theater_ratio, 4, 0.35).
narrative_ontology:measurement(soci_tr_t8, sociotechnical_risk_reading, theater_ratio, 8, 0.41).
narrative_ontology:measurement(soci_tr_t12, sociotechnical_risk_reading, theater_ratio, 12, 0.47).
narrative_ontology:measurement(soci_tr_t16, sociotechnical_risk_reading, theater_ratio, 16, 0.52).
narrative_ontology:measurement(soci_tr_t20, sociotechnical_risk_reading, theater_ratio, 20, 0.56).
narrative_ontology:measurement(soci_tr_t24, sociotechnical_risk_reading, theater_ratio, 24, 0.58).

% Extraction over time
narrative_ontology:measurement(soci_be_t0, sociotechnical_risk_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(soci_be_t4, sociotechnical_risk_reading, base_extractiveness, 4, 0.42).
narrative_ontology:measurement(soci_be_t8, sociotechnical_risk_reading, base_extractiveness, 8, 0.5).
narrative_ontology:measurement(soci_be_t12, sociotechnical_risk_reading, base_extractiveness, 12, 0.57).
narrative_ontology:measurement(soci_be_t16, sociotechnical_risk_reading, base_extractiveness, 16, 0.62).
narrative_ontology:measurement(soci_be_t20, sociotechnical_risk_reading, base_extractiveness, 20, 0.66).
narrative_ontology:measurement(soci_be_t24, sociotechnical_risk_reading, base_extractiveness, 24, 0.68).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(sociotechnical_risk_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sociotechnical_risk_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(sociotechnical_risk_reading, 0.1).
narrative_ontology:affects_constraint(sociotechnical_risk_reading, instrumentalist_screening_reading).
narrative_ontology:affects_constraint(sociotechnical_risk_reading, representational_correspondence_reading).
narrative_ontology:affects_constraint(sociotechnical_risk_reading, behavioral_mechanism_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of four sibling readings of the persona_as_valid_proxy kernel, each authored as a separate constraint story per the ε-invariance principle. The instrumentalist_screening_reading asks whether the persona performs its assigned evaluation function; the representational_correspondence_reading asks whether persona behavior matches real human behavioral distributions; the behavioral_mechanism_reading asks what causal process generates persona outputs. This sociotechnical_risk_reading asks what changes in the world once the infrastructure exists, independent of intended use, and is the only sibling that authors a substantial beneficiary/victim asymmetry — the other three readings largely evaluate internal properties of the artifact rather than its externality profile. ε differs sharply across the family: the correspondence and mechanism readings likely register low-to-moderate ε (measurement/validity questions with contested but non-adversarial stakes), while this reading registers high and rising ε because it identifies a concentrated beneficiary class and a diffuse, non-consenting, structurally excluded payer class.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
