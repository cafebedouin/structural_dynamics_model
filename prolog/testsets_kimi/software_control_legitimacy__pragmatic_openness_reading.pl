% ============================================================================
% CONSTRAINT STORY: software_control_legitimacy__pragmatic_openness_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_software_control_legitimacy__pragmatic_openness_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
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
 *   constraint_id: software_control_legitimacy__pragmatic_openness_reading
 *   human_readable: Pragmatic Openness Reading of Software Control Legitimacy
 *   domain: software engineering / political economy of technology / intellectual property
 *
 * SUMMARY:
 *   This constraint instantiates the pragmatic_openness_reading of the
 *   contested software_control_legitimacy kernel. It treats software control
 *   as a development methodology choice rather than an ethical imperative or
 *   property right, asserting that open source produces better software
 *   through peer review and collaboration while accepting proprietary models
 *   as legitimate alternatives. The reading coordinates decentralized
 *   development without suppressing proprietary activity, producing a
 *   low-extraction rope structure. Sibling readings include the
 *   freedom_imperative_reading (proprietary ethically illegitimate),
 *   property_rights_reading (creator control), and commons_reading
 *   (negotiated collective management).
 *
 * KEY AGENTS:
 *   - pragmatic_oss_advocates: Agenda-setter (organized/global) â maintains discourse frameworks and license certification standards that operationalize the reading
 *   - software_developers: Beneficiary (moderate/global) â coordinates via peer review and reuses open components across project boundaries
 *   - software_users: Beneficiary (organized/global) â consumes quality-optimized software across both open and proprietary channels
 *   - proprietary_vendors: Beneficiary (powerful/global) â retains legitimacy as a tolerated alternative rather than a moral outcast
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(software_control_legitimacy__pragmatic_openness_reading, 0.12).
domain_priors:suppression_score(software_control_legitimacy__pragmatic_openness_reading, 0.08).
domain_priors:theater_ratio(software_control_legitimacy__pragmatic_openness_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(software_control_legitimacy__pragmatic_openness_reading, extractiveness, 0.12).
narrative_ontology:constraint_metric(software_control_legitimacy__pragmatic_openness_reading, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(software_control_legitimacy__pragmatic_openness_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(software_control_legitimacy__pragmatic_openness_reading, accessibility_collapse, 0.25).
narrative_ontology:constraint_metric(software_control_legitimacy__pragmatic_openness_reading, resistance, 0.12).

% --- Constraint claim ---
narrative_ontology:constraint_claim(software_control_legitimacy__pragmatic_openness_reading, rope).
narrative_ontology:human_readable(software_control_legitimacy__pragmatic_openness_reading, "Pragmatic Openness Reading of Software Control Legitimacy").
narrative_ontology:topic_domain(software_control_legitimacy__pragmatic_openness_reading, "software engineering / political economy of technology / intellectual property").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(software_control_legitimacy__pragmatic_openness_reading, '3eb76a8f-97d0-4316-986f-b10df984c26b').
narrative_ontology:cs_kernel_codification('3eb76a8f-97d0-4316-986f-b10df984c26b', distributed).
narrative_ontology:cs_authority_grounding('3eb76a8f-97d0-4316-986f-b10df984c26b', expertise).
narrative_ontology:cs_interpretation_layer_present('3eb76a8f-97d0-4316-986f-b10df984c26b').
narrative_ontology:cs_reading_relation('3eb76a8f-97d0-4316-986f-b10df984c26b', software_control_legitimacy__freedom_imperative_reading, forecloses).
narrative_ontology:cs_reading_relation('3eb76a8f-97d0-4316-986f-b10df984c26b', software_control_legitimacy__property_rights_reading, coexists_with).
narrative_ontology:cs_reading_relation('3eb76a8f-97d0-4316-986f-b10df984c26b', software_control_legitimacy__commons_reading, coexists_with).
narrative_ontology:cs_axiom('3eb76a8f-97d0-4316-986f-b10df984c26b', foundational, open_source_pragmatically_superior).
narrative_ontology:cs_axiom_status(open_source_pragmatically_superior, holdable).
narrative_ontology:cs_axiom_grounding('3eb76a8f-97d0-4316-986f-b10df984c26b', open_source_pragmatically_superior, empirically_contingent).
narrative_ontology:cs_axiom('3eb76a8f-97d0-4316-986f-b10df984c26b', foundational, proprietary_legitimate_alternative).
narrative_ontology:cs_axiom_status(proprietary_legitimate_alternative, holdable).
narrative_ontology:cs_axiom_grounding('3eb76a8f-97d0-4316-986f-b10df984c26b', proprietary_legitimate_alternative, instrumental).
narrative_ontology:cs_reference_frame('3eb76a8f-97d0-4316-986f-b10df984c26b', pragmatic_quality_optimization).
narrative_ontology:cs_drift_state('3eb76a8f-97d0-4316-986f-b10df984c26b', contemporary_commercial_oss_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('3eb76a8f-97d0-4316-986f-b10df984c26b', '').
narrative_ontology:cs_kernel_id(software_control_legitimacy__pragmatic_openness_reading, software_control_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(software_control_legitimacy__pragmatic_openness_reading, software_developers).
narrative_ontology:constraint_beneficiary(software_control_legitimacy__pragmatic_openness_reading, software_users).
narrative_ontology:constraint_beneficiary(software_control_legitimacy__pragmatic_openness_reading, proprietary_vendors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Promote open source as a pragmatic methodology for producing higher-quality software through peer review and transparent collaboration. They set discourse norms by maintaining definitions like the Open Source Definition and certifying licenses, while explicitly acknowledging proprietary software as a legitimate alternative for contexts where openness is impractical.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__pragmatic_openness_reading, pragmatic_oss_advocates, agenda_setter,
    organized, generational, mobile, global).

% Choose between open source and proprietary development models based on project context and quality goals. They receive access to peer-reviewed codebases, reusable components, and collaborative improvement norms that reduce duplication of effort and raise baseline quality.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__pragmatic_openness_reading, software_developers, beneficiary,
    moderate, biographical, mobile, global).

% Receive higher-quality software produced under open source peer review norms, while retaining access to proprietary alternatives for specialized or capital-intensive applications where open source offerings remain immature.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__pragmatic_openness_reading, software_users, beneficiary,
    organized, biographical, constrained, global).

% Retain legitimacy and market access because the pragmatic reading frames proprietary development as an acceptable alternative rather than an ethical violation. They participate in the ecosystem by contributing to or adopting open source where it complements commercial strategy.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__pragmatic_openness_reading, proprietary_vendors, beneficiary,
    powerful, generational, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(software_control_legitimacy__pragmatic_openness_reading, diffuse).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables large-scale, decentralized software development by establishing peer review and transparent collaboration as quality-optimizing norms, while preserving ideological peace by treating proprietary development as a legitimate alternative.
% TRANSFER_FUNCTION: Moves developer attention and labor toward open source projects and moves quality benefits to users; does not forcibly transfer resources away from proprietary actors.
% ABSENT_VOICES: Freedom-imperative advocates who view proprietary software as inherently unethical are sidelined in this framing, as are property-rights absolutists who reject any limitation on creator control. Their exclusion is discursive rather than structural.
% DISAPPEARANCE_RATIONALE: If the pragmatic openness reading vanished, the ideological truce between open and proprietary models would collapse. Developers would face polarized pressure to adopt either strict free-software ethics or pure commercial property frames, fragmenting collaborative norms and reducing the quality-focused cross-pollination that currently sustains the ecosystem.
% FOUNDING_PROBLEM: How to organize software development at scale without centralized command, while avoiding perpetual ideological war between advocates of user freedom and proprietary control.
% FOUNDING_PROBLEM_CORROBORATION: Empirical software engineering studies outside the open source advocacy community corroborate the defect-reduction benefits of peer review. Major proprietary software firms independently attest that decentralized development models produce viable competitors, confirming the practical need for coexistence.
narrative_ontology:disappearance_verdict(software_control_legitimacy__pragmatic_openness_reading, world_rearranges).
narrative_ontology:founding_problem_status(software_control_legitimacy__pragmatic_openness_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(software_control_legitimacy__pragmatic_openness_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(software_control_legitimacy__pragmatic_openness_reading, 'none', 1).
narrative_ontology:epsilon_provenance(software_control_legitimacy__pragmatic_openness_reading, 0.12, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(software_control_legitimacy__pragmatic_openness_reading_tests).
:- end_tests(software_control_legitimacy__pragmatic_openness_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.12) because the constraint coordinates through voluntary peer review and collaboration rather than rent collection. Suppression is minimal (0.08) because proprietary alternatives are explicitly treated as legitimate. Theater ratio is low (0.10) because the peer review function is substantive and not primarily performative. Accessibility collapse is modest (0.25) because the reading does not foreclose competing ideological framings; resistance is low (0.12) because the lack of a victim set minimizes organized opposition. The measurement series use a single shared time grid to prevent temporal misalignment.
 *
 * PERSPECTIVAL GAP:
 *   All seated agents experience low effective extraction. The agenda-setter and beneficiary seats are aligned: pragmatic advocates, developers, users, and proprietary vendors all gain from the ideological truce the reading sustains. There is no concentrated payer seat, so seat divergence is minimal and the engine should compute a uniform rope classification across positions.
 *
 * DIRECTIONALITY LOGIC:
 *   All declared agents sit near the beneficiary end of the directionality spectrum. Pragmatic OSS advocates set the agenda but also gain from the ecosystem's expansion. Developers and users gain quality and access. Proprietary vendors gain legitimacy. No agent is structurally targeted for extraction. The engine should derive directionality near 0.0 for all power atoms, yielding negligible effective extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâorganizing decentralized software development without ideological warâremains live, as evidenced by ongoing debates between free software and open source factions. The constraint has not atrophied into a piton because its coordination function is still actively producing value (peer-reviewed infrastructure, reusable components) and no administrator maintains it merely out of inertia.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    oss_quality_empirical_basis,
    'Does the claim that open source produces better software rest on empirically contingent quality advantages, or is it a normative commitment that would persist even if proprietary quality matched or exceeded OSS?',
    'Large-scale randomized controlled trials or comprehensive meta-analyses comparing defect rates, security vulnerabilities, and long-term maintainability between matched OSS and proprietary projects.',
    'If empirically contingent and challenged, the reading''s coordination function weakens and it may drift toward pure ideology or collapse; if normative, it is more stable but less distinguishable from the freedom_imperative_reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(oss_quality_empirical_basis, empirical, 'Empirical basis of open source quality superiority claim').

omega_variable(
    framing_under_determination_commons_market,
    'Is the pragmatic openness reading grounded in a commons-governance framing or a market-competition framing?',
    'Discourse analysis of foundational texts and advocate speeches to determine whether collective-governance norms or market-efficiency norms predominate.',
    'A commons grounding would align the reading closer to the commons_reading and shift its coordination type; a market-efficiency grounding aligns it with property_rights_reading influences.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(framing_under_determination_commons_market, conceptual, 'Alternative framing as commons governance versus market competition').

omega_variable(
    kernel_reading_structural_delta,
    'What would change structurally if the freedom_imperative_reading were adopted instead of the pragmatic_openness_reading?',
    'Comparative analysis of victim set and extraction profile across the two sibling readings.',
    'Adoption of the freedom_imperative_reading would transform the constraint from a rope into a snare or tangled rope by introducing proprietary_vendors as victims and raising suppression of proprietary alternatives.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_structural_delta, conceptual, 'Structural delta between pragmatic openness and freedom imperative readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(software_control_legitimacy__pragmatic_openness_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pragmatic_openness_tr_t0, software_control_legitimacy__pragmatic_openness_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(pragmatic_openness_tr_t5, software_control_legitimacy__pragmatic_openness_reading, theater_ratio, 5, 0.06).
narrative_ontology:measurement(pragmatic_openness_tr_t10, software_control_legitimacy__pragmatic_openness_reading, theater_ratio, 10, 0.06).
narrative_ontology:measurement(pragmatic_openness_tr_t15, software_control_legitimacy__pragmatic_openness_reading, theater_ratio, 15, 0.07).
narrative_ontology:measurement(pragmatic_openness_tr_t20, software_control_legitimacy__pragmatic_openness_reading, theater_ratio, 20, 0.08).
narrative_ontology:measurement(pragmatic_openness_tr_t25, software_control_legitimacy__pragmatic_openness_reading, theater_ratio, 25, 0.09).
narrative_ontology:measurement(pragmatic_openness_tr_t30, software_control_legitimacy__pragmatic_openness_reading, theater_ratio, 30, 0.1).

% Extraction over time
narrative_ontology:measurement(pragmatic_openness_be_t0, software_control_legitimacy__pragmatic_openness_reading, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(pragmatic_openness_be_t5, software_control_legitimacy__pragmatic_openness_reading, base_extractiveness, 5, 0.08).
narrative_ontology:measurement(pragmatic_openness_be_t10, software_control_legitimacy__pragmatic_openness_reading, base_extractiveness, 10, 0.09).
narrative_ontology:measurement(pragmatic_openness_be_t15, software_control_legitimacy__pragmatic_openness_reading, base_extractiveness, 15, 0.09).
narrative_ontology:measurement(pragmatic_openness_be_t20, software_control_legitimacy__pragmatic_openness_reading, base_extractiveness, 20, 0.1).
narrative_ontology:measurement(pragmatic_openness_be_t25, software_control_legitimacy__pragmatic_openness_reading, base_extractiveness, 25, 0.11).
narrative_ontology:measurement(pragmatic_openness_be_t30, software_control_legitimacy__pragmatic_openness_reading, base_extractiveness, 30, 0.12).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(software_control_legitimacy__pragmatic_openness_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(software_control_legitimacy__pragmatic_openness_reading, software_control_legitimacy__freedom_imperative_reading).
narrative_ontology:affects_constraint(software_control_legitimacy__pragmatic_openness_reading, software_control_legitimacy__property_rights_reading).
narrative_ontology:affects_constraint(software_control_legitimacy__pragmatic_openness_reading, software_control_legitimacy__commons_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the software_control_legitimacy kernel, decomposed from the natural-language concept of 'software control' into four structurally distinct claims per the epsilon-invariance principle. This reading isolates the pragmatic methodological claim; siblings isolate ethical freedom, property rights, and commons governance claims.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
