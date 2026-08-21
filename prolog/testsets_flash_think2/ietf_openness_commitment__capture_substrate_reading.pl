% ============================================================================
% CONSTRAINT STORY: ietf_openness_commitment__capture_substrate_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ietf_openness_commitment__capture_substrate_reading, []).

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
 *   constraint_id: ietf_openness_commitment__capture_substrate_reading
 *   human_readable: IETF Openness Commitment: Capture Substrate Reading
 *   domain: technology_governance/internet_standards/institutional_economics
 *
 * SUMMARY:
 *   This constraint is the 'capture substrate' reading of the
 *   `ietf_openness_commitment` kernel. It describes how the IETF's open
 *   standards process, while ostensibly neutral, becomes a substrate for
 *   large platform operators to encode proprietary advantages, leading to
 *   gatekeeping. Sibling readings include the 'commons stewardship' reading
 *   (focus on genuine interoperability) and the 'legitimacy erosion' reading
 *   (focus on the rough consensus mechanism's vulnerability). The claimed
 *   type is 'tangled_rope' because the process genuinely coordinates internet
 *   interoperability (beneficiaries) but simultaneously enables asymmetric
 *   extraction through resource-driven influence (victims).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ietf_openness_commitment__capture_substrate_reading, 0.68).
domain_priors:suppression_score(ietf_openness_commitment__capture_substrate_reading, 0.65).
domain_priors:theater_ratio(ietf_openness_commitment__capture_substrate_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ietf_openness_commitment__capture_substrate_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(ietf_openness_commitment__capture_substrate_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(ietf_openness_commitment__capture_substrate_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ietf_openness_commitment__capture_substrate_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(ietf_openness_commitment__capture_substrate_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ietf_openness_commitment__capture_substrate_reading, tangled_rope).
narrative_ontology:human_readable(ietf_openness_commitment__capture_substrate_reading, "IETF Openness Commitment: Capture Substrate Reading").
narrative_ontology:topic_domain(ietf_openness_commitment__capture_substrate_reading, "technology_governance/internet_standards/institutional_economics").

domain_priors:requires_active_enforcement(ietf_openness_commitment__capture_substrate_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ietf_openness_commitment__capture_substrate_reading, '84ef7036-7ce5-4297-88b4-a45d5b20a94f').
narrative_ontology:cs_kernel_codification('84ef7036-7ce5-4297-88b4-a45d5b20a94f', formalized).
narrative_ontology:cs_authority_grounding('84ef7036-7ce5-4297-88b4-a45d5b20a94f', practice).
narrative_ontology:cs_interpretation_layer_present('84ef7036-7ce5-4297-88b4-a45d5b20a94f').
narrative_ontology:cs_reading_relation('84ef7036-7ce5-4297-88b4-a45d5b20a94f', ietf_openness_commitment__commons_stewardship_reading, coexists_with).
narrative_ontology:cs_reading_relation('84ef7036-7ce5-4297-88b4-a45d5b20a94f', ietf_openness_commitment__legitimacy_erosion_reading, influences).
narrative_ontology:cs_axiom('84ef7036-7ce5-4297-88b4-a45d5b20a94f', foundational, standards_as_competitive_advantage).
narrative_ontology:cs_axiom_status(standards_as_competitive_advantage, holdable).
narrative_ontology:cs_axiom_grounding('84ef7036-7ce5-4297-88b4-a45d5b20a94f', standards_as_competitive_advantage, instrumental).
narrative_ontology:cs_axiom('84ef7036-7ce5-4297-88b4-a45d5b20a94f', foundational, resource_asymmetry_shapes_consensus).
narrative_ontology:cs_axiom_status(resource_asymmetry_shapes_consensus, holdable).
narrative_ontology:cs_axiom_grounding('84ef7036-7ce5-4297-88b4-a45d5b20a94f', resource_asymmetry_shapes_consensus, empirically_contingent).
narrative_ontology:cs_reference_frame('84ef7036-7ce5-4297-88b4-a45d5b20a94f', neutral_technical_meritocracy).
narrative_ontology:cs_drift_state('84ef7036-7ce5-4297-88b4-a45d5b20a94f', contemporary_internet_governance, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('84ef7036-7ce5-4297-88b4-a45d5b20a94f', '').
narrative_ontology:cs_kernel_id(ietf_openness_commitment__capture_substrate_reading, ietf_openness_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ietf_openness_commitment__capture_substrate_reading, large_platform_operators).
narrative_ontology:constraint_victim(ietf_openness_commitment__capture_substrate_reading, small_implementers).
narrative_ontology:constraint_victim(ietf_openness_commitment__capture_substrate_reading, end_users).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These entities possess significant resources to fund participation in IETF working groups, influence technical direction, and promote their proprietary extensions as de facto standards. They benefit by encoding their market advantages into ostensibly open specifications, creating gatekeeping effects.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__capture_substrate_reading, large_platform_operators, agenda_setter,
    institutional, generational, arbitrage, global).

% These developers and companies must conform to standards that increasingly reflect the interests of large operators. They bear the costs of adapting to complex specifications that may be difficult to implement without the resources of larger players, or face market exclusion.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__capture_substrate_reading, small_implementers, payer,
    moderate, biographical, constrained, global).

% Experience reduced choice, vendor lock-in, and potentially higher costs or poorer service quality when internet standards are shaped to favor specific platform ecosystems rather than universal interoperability. Their diffuse interests are rarely directly represented.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__capture_substrate_reading, end_users, payer,
    powerless, immediate, constrained, global).

% The formal bodies where technical standards are debated and written. While operating under principles of 'rough consensus and running code,' their composition and the resources available to participants can be heavily influenced by large corporate interests, leading to subtle forms of capture.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__capture_substrate_reading, ietf_working_groups, agenda_setter,
    organized, biographical, constrained, global).

% Analyze the IETF process, its outputs, and its economic and social impacts. They often identify patterns of influence and capture, but their findings may not directly translate into policy changes within the standards body.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__capture_substrate_reading, academic_researchers, observer,
    analytical, generational, analytical, global).

% Actively promote truly open, unencumbered standards and resist corporate influence. Despite their efforts, they often find themselves marginalized in discussions where resource-backed proposals dominate, or their concerns are addressed superficially.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__capture_substrate_reading, open_source_advocates, excluded,
    organized, biographical, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ietf_openness_commitment__capture_substrate_reading, large_platform_operators).
narrative_ontology:fixing_cost_class(ietf_openness_commitment__capture_substrate_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To provide universally interoperable, technically sound specifications for the internet's core protocols and applications, ensuring global connectivity and preventing fragmentation.
% TRANSFER_FUNCTION: Transfers de facto control over internet infrastructure development and market advantage from a broad, diverse community of implementers and users to well-resourced corporate actors, by allowing resource advantage to shape the 'rough consensus' and embed proprietary interests into open standards.
% ABSENT_VOICES: Small implementers, independent researchers, and end-users, whose diffuse interests are outmatched by the concentrated resources and lobbying power of large platform operators. Truly independent technical experts lacking corporate backing may also be effectively marginalized.
% DISAPPEARANCE_RATIONALE: If the IETF's standards process vanished overnight, the internet's technical evolution would likely fragment into purely proprietary silos, leading to severe interoperability issues, increased vendor lock-in, and a breakdown of the global 'network of networks' principle. Even a captured process provides a common substrate.
% FOUNDING_PROBLEM: The original problem was the need for a neutral, open, and consensus-driven process to develop technical standards for the internet to ensure global interoperability, prevent fragmentation, and foster innovation from all participants.
% FOUNDING_PROBLEM_CORROBORATION: Large platform operators and some IETF participants argue the problem of managing global internet infrastructure complexity is still live, justifying the current process. Critics (academic researchers, open source advocates) attest that while the original problem of fragmentation is largely mitigated, the process has been co-opted, and the arrangement now primarily serves incumbent interests, as evidenced by economic analysis and historical patterns of standards adoption.
narrative_ontology:disappearance_verdict(ietf_openness_commitment__capture_substrate_reading, world_rearranges).
narrative_ontology:founding_problem_status(ietf_openness_commitment__capture_substrate_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ietf_openness_commitment__capture_substrate_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(ietf_openness_commitment__capture_substrate_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ietf_openness_commitment__capture_substrate_reading, 0.68, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ietf_openness_commitment__capture_substrate_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ietf_openness_commitment__capture_substrate_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ietf_openness_commitment__capture_substrate_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-to-high because the process, while open, allows well-resourced actors to shape outcomes that benefit them disproportionately, leading to de facto proprietary control over key internet functions. Suppression is also moderate-to-high, as the sheer complexity and resource requirements of participation effectively suppress the voices and proposals of smaller, less-resourced entities. The theater ratio is moderate, reflecting the performative maintenance of 'openness' and 'rough consensus' even as outcomes are increasingly skewed. The temporal measurements show a gradual increase in extractiveness, suppression, and theatricality over 30 years, reflecting the growing influence of large corporate actors as the internet matured.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of large platform operators, the IETF process is a legitimate, open forum for technical coordination. From the perspective of small implementers and end-users, it functions as a mechanism for embedding proprietary advantage and creating gatekeeping, despite its stated commitment to openness. The engine's classification will highlight this divergence between the claimed 'rope' and the computed 'tangled_rope' or 'snare' from victim seats.
 *
 * DIRECTIONALITY LOGIC:
 *   Large platform operators are the primary beneficiaries and agenda-setters, leveraging their resources to shape standards. Small implementers and end-users are the victims, bearing the costs of adapting to standards that favor incumbents or experiencing reduced choice. IETF working groups are also agenda-setters, but their 'constrained' exit options and 'organized' power reflect their vulnerability to influence. Open source advocates are 'excluded' as their proposals often fail to gain traction against well-funded alternatives.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    resource_advantage_impact,
    'To what extent does resource advantage (funding, personnel, lobbying) directly translate into influence over IETF standards outcomes, beyond technical merit?',
    'Empirical studies correlating participant resources with proposal success rates, analysis of ''rough consensus'' formation dynamics, and case studies of controversial standards where resource-backed proposals prevailed over technically superior but less-resourced alternatives.',
    'If resource advantage is a strong predictor of outcome, it would confirm the extractive nature of the constraint, potentially reclassifying it closer to a Snare. If technical merit consistently prevails, it would support the Rope framing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(resource_advantage_impact, empirical, 'Quantifying the influence of corporate resources on IETF standards decisions.').

omega_variable(
    true_openness_definition,
    'What constitutes ''openness'' in an internet standard: merely public availability, or also equitable access to participation, implementation, and non-discriminatory use?',
    'Conceptual analysis and community consensus on a multi-dimensional definition of ''openness'' that includes process, implementation, and market impact, rather than just formal publication.',
    'A narrow definition of openness (e.g., ''publicly available'') allows for greater extraction under the guise of open standards. A broader definition would highlight the current constraint''s extractive elements more clearly, potentially shifting its classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(true_openness_definition, conceptual, 'Defining the scope of ''openness'' in internet standards.').

omega_variable(
    capture_mechanism_ambiguity,
    'Is the capture of the IETF process explicit (e.g., direct lobbying, quid pro quo) or implicit (e.g., ''rough consensus'' naturally favoring well-resourced proposals, ''running code'' being proprietary implementations)?',
    'Detailed ethnographic studies of working group dynamics, analysis of email list archives, and interviews with long-term participants to identify subtle and overt mechanisms of influence.',
    'Explicit capture would strengthen the Snare classification. Implicit capture, while still extractive, might suggest a more ''Tangled Rope'' dynamic where the coordination function is genuinely present but subtly distorted.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capture_mechanism_ambiguity, empirical, 'Distinguishing explicit vs. implicit capture mechanisms in the IETF.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ietf_openness_commitment__capture_substrate_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ietf_tr_t0, ietf_openness_commitment__capture_substrate_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(ietf_tr_t6, ietf_openness_commitment__capture_substrate_reading, theater_ratio, 6, 0.25).
narrative_ontology:measurement(ietf_tr_t12, ietf_openness_commitment__capture_substrate_reading, theater_ratio, 12, 0.3).
narrative_ontology:measurement(ietf_tr_t18, ietf_openness_commitment__capture_substrate_reading, theater_ratio, 18, 0.37).
narrative_ontology:measurement(ietf_tr_t24, ietf_openness_commitment__capture_substrate_reading, theater_ratio, 24, 0.42).
narrative_ontology:measurement(ietf_tr_t30, ietf_openness_commitment__capture_substrate_reading, theater_ratio, 30, 0.45).

% Extraction over time
narrative_ontology:measurement(ietf_be_t0, ietf_openness_commitment__capture_substrate_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(ietf_be_t6, ietf_openness_commitment__capture_substrate_reading, base_extractiveness, 6, 0.52).
narrative_ontology:measurement(ietf_be_t12, ietf_openness_commitment__capture_substrate_reading, base_extractiveness, 12, 0.58).
narrative_ontology:measurement(ietf_be_t18, ietf_openness_commitment__capture_substrate_reading, base_extractiveness, 18, 0.63).
narrative_ontology:measurement(ietf_be_t24, ietf_openness_commitment__capture_substrate_reading, base_extractiveness, 24, 0.66).
narrative_ontology:measurement(ietf_be_t30, ietf_openness_commitment__capture_substrate_reading, base_extractiveness, 30, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(ietf_su_t0, ietf_openness_commitment__capture_substrate_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(ietf_su_t6, ietf_openness_commitment__capture_substrate_reading, suppression_requirement, 6, 0.46).
narrative_ontology:measurement(ietf_su_t12, ietf_openness_commitment__capture_substrate_reading, suppression_requirement, 12, 0.53).
narrative_ontology:measurement(ietf_su_t18, ietf_openness_commitment__capture_substrate_reading, suppression_requirement, 18, 0.59).
narrative_ontology:measurement(ietf_su_t24, ietf_openness_commitment__capture_substrate_reading, suppression_requirement, 24, 0.63).
narrative_ontology:measurement(ietf_su_t30, ietf_openness_commitment__capture_substrate_reading, suppression_requirement, 30, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ietf_openness_commitment__capture_substrate_reading, information_standard).
narrative_ontology:affects_constraint(ietf_openness_commitment__capture_substrate_reading, ietf_openness_commitment__commons_stewardship_reading).
narrative_ontology:affects_constraint(ietf_openness_commitment__capture_substrate_reading, ietf_openness_commitment__legitimacy_erosion_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'ietf_openness_commitment' kernel. It describes the process as a substrate for capture, distinct from the 'commons stewardship' reading (focus on genuine interoperability) and the 'legitimacy erosion' reading (focus on the rough consensus mechanism's vulnerability).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
