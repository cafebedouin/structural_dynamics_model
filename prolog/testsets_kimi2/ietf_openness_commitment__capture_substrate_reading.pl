% ============================================================================
% CONSTRAINT STORY: ietf_openness_commitment__capture_substrate_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:suppression_profile/2,
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
 *   domain: technology_governance/internet_standards
 *
 * SUMMARY:
 *   This constraint is the capture_substrate_reading of the
 *   ietf_openness_commitment kernel. It models the IETF standards process not
 *   as a neutral commons but as a coordination substrate where resource
 *   advantage translates into encoded gatekeeping. Large platform operators
 *   dominate working group participation and draft authorship, shaping
 *   ostensibly open standards to accommodate proprietary extensions. Small
 *   implementers and end users bear the costs of interoperability
 *   fragmentation disguised as technical evolution. Sibling readings include
 *   commons_stewardship_reading (functioning public infrastructure) and
 *   legitimacy_erosion_reading (procedural vulnerability without capture).
 *
 * KEY AGENTS:
 *   - large_platform_operators: Primary beneficiary (institutional/arbitrage) â captures process via full-time engineering and draft control.
 *   - small_implementers: Primary target (moderate/constrained) â lacks resources to shape standards and bears fragmentation costs.
 *   - end_users: Secondary target (organized/constrained) â locked into ecosystems via network effects.
 *   - ietf_administration: Agenda setter (institutional/constrained) â maintains process that structurally favors resourced participants.
 *   - public_interest_advocates: Excluded voice (organized/constrained) â economically prevented from sustained participation.
 *   - independent_standards_researchers: Analytical observer (analytical/analytical) â documents capture dynamics.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ietf_openness_commitment__capture_substrate_reading, 0.55).
domain_priors:suppression_score(ietf_openness_commitment__capture_substrate_reading, 0.45).
domain_priors:theater_ratio(ietf_openness_commitment__capture_substrate_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ietf_openness_commitment__capture_substrate_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(ietf_openness_commitment__capture_substrate_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(ietf_openness_commitment__capture_substrate_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ietf_openness_commitment__capture_substrate_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(ietf_openness_commitment__capture_substrate_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ietf_openness_commitment__capture_substrate_reading, tangled_rope).
narrative_ontology:human_readable(ietf_openness_commitment__capture_substrate_reading, "IETF Openness Commitment: Capture Substrate Reading").
narrative_ontology:topic_domain(ietf_openness_commitment__capture_substrate_reading, "technology_governance/internet_standards").

domain_priors:requires_active_enforcement(ietf_openness_commitment__capture_substrate_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ietf_openness_commitment__capture_substrate_reading, 'b521acc4-c4ad-4519-8014-4fefd19d4cfb').
narrative_ontology:cs_kernel_codification('b521acc4-c4ad-4519-8014-4fefd19d4cfb', formalized).
narrative_ontology:cs_authority_grounding('b521acc4-c4ad-4519-8014-4fefd19d4cfb', expertise).
narrative_ontology:cs_interpretation_layer_present('b521acc4-c4ad-4519-8014-4fefd19d4cfb').
narrative_ontology:cs_reading_relation('b521acc4-c4ad-4519-8014-4fefd19d4cfb', ietf_openness_commitment__commons_stewardship_reading, coexists_with).
narrative_ontology:cs_reading_relation('b521acc4-c4ad-4519-8014-4fefd19d4cfb', ietf_openness_commitment__legitimacy_erosion_reading, influences).
narrative_ontology:cs_axiom('b521acc4-c4ad-4519-8014-4fefd19d4cfb', foundational, resource_concentration_structurally_distorts_consensus).
narrative_ontology:cs_axiom_status(resource_concentration_structurally_distorts_consensus, holdable).
narrative_ontology:cs_axiom_grounding('b521acc4-c4ad-4519-8014-4fefd19d4cfb', resource_concentration_structurally_distorts_consensus, empirically_contingent).
narrative_ontology:cs_axiom('b521acc4-c4ad-4519-8014-4fefd19d4cfb', foundational, open_process_formality_conceals_power_asymmetry).
narrative_ontology:cs_axiom_status(open_process_formality_conceals_power_asymmetry, holdable).
narrative_ontology:cs_axiom_grounding('b521acc4-c4ad-4519-8014-4fefd19d4cfb', open_process_formality_conceals_power_asymmetry, empirically_contingent).
narrative_ontology:cs_reference_frame('b521acc4-c4ad-4519-8014-4fefd19d4cfb', decentralized_meritocratic_coordination).
narrative_ontology:cs_drift_state('b521acc4-c4ad-4519-8014-4fefd19d4cfb', platform_consolidation_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('b521acc4-c4ad-4519-8014-4fefd19d4cfb', '').
narrative_ontology:cs_kernel_id(ietf_openness_commitment__capture_substrate_reading, ietf_openness_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ietf_openness_commitment__capture_substrate_reading, large_platform_operators).
narrative_ontology:constraint_victim(ietf_openness_commitment__capture_substrate_reading, small_implementers).
narrative_ontology:constraint_victim(ietf_openness_commitment__capture_substrate_reading, end_users).
narrative_ontology:constraint_vindicates(ietf_openness_commitment__capture_substrate_reading, open_process_neutrality_claim).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Dominate IETF working groups by dedicating full-time engineers to draft authorship and meeting attendance. Shape standards to encode proprietary extensions as optional features that effectively require their infrastructure scale to implement efficiently. Benefit from standards complexity that raises barriers for smaller competitors.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__capture_substrate_reading, large_platform_operators, beneficiary,
    institutional, generational, arbitrage, global).

% Must implement complex standards written with large-scale infrastructure assumptions. Lack resources to attend meetings or author drafts. Bear interoperability costs when platforms deploy standards-plus-extensions that fragment the open specification.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__capture_substrate_reading, small_implementers, payer,
    moderate, biographical, constrained, global).

% Locked into platform ecosystems by network effects. Experience degraded interoperability when platforms use proprietary extensions marketed as standards compliance. Indirectly pay via reduced choice and higher switching costs.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__capture_substrate_reading, end_users, payer,
    organized, biographical, constrained, global).

% Maintains rough consensus process, open mailing lists, and RFC publication. Formally committed to neutrality and openness. Structurally dependent on corporate participation for engineering labor and travel sponsorship, which constrains ability to resist capture.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__capture_substrate_reading, ietf_administration, agenda_setter,
    institutional, civilizational, constrained, global).

% Represent user privacy, accessibility, and anti-monopoly interests but lack corporate travel budgets and dedicated engineering time to sustain working group participation. Their absence from drafts and consensus calls is economic, not formal.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__capture_substrate_reading, public_interest_advocates, excluded,
    organized, generational, constrained, global).

% Study capture dynamics in standards bodies. Document how resource advantages translate into draft authorship, chair appointments, and consensus outcomes. Do not participate as implementers or beneficiaries.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__capture_substrate_reading, independent_standards_researchers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ietf_openness_commitment__capture_substrate_reading, large_platform_operators).
narrative_ontology:fixing_cost_class(ietf_openness_commitment__capture_substrate_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a common technical grammar and coordination substrate for internet interoperability, reducing fragmentation and enabling heterogeneous systems to communicate without bilateral negotiation.
% TRANSFER_FUNCTION: Moves de facto standard-setting authority from a decentralized public-interest coordination process to large platform operators who can resource participation; transfers implementation burden and interoperability costs to small implementers and end users.
% ABSENT_VOICES: Public interest advocates, individual non-corporate researchers, and small-nation regulators are structurally absent due to travel and time costs; their exclusion is economic rather than formal.
% DISAPPEARANCE_RATIONALE: If the IETF openness process disappeared overnight, internet infrastructure would lose a key coordination substrate; large platforms would lose a legitimacy mechanism for proprietary gatekeeping, and small implementers would face immediate protocol fragmentation risks.
% FOUNDING_PROBLEM: Preventing protocol fragmentation and ensuring interoperable data exchange across independently administered networks in the early internet.
% FOUNDING_PROBLEM_CORROBORATION: Internet historians and early RFC authors attest to the fragmentation problem. Digital rights advocates and competition economists from outside the corporate beneficiary set attest that the founding problem is largely solved and the arrangement now functions as legitimacy for capture.
narrative_ontology:disappearance_verdict(ietf_openness_commitment__capture_substrate_reading, world_rearranges).
narrative_ontology:founding_problem_status(ietf_openness_commitment__capture_substrate_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ietf_openness_commitment__capture_substrate_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(ietf_openness_commitment__capture_substrate_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ietf_openness_commitment__capture_substrate_reading, 0.55, 'kimi-k2.6', 'none', direct).

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
 *   The constraint is classified as tangled_rope because it possesses both a genuine coordination function (interoperability substrate for the internet) and asymmetric extraction (resource advantage encodes gatekeeping). Extractiveness is moderate (0.55) because the extraction is structural rather than overt: it operates through complexity, meeting fatigue, and draft authorship concentration rather than explicit fee or exclusion. Suppression (0.45) is moderate because the constraint's persistence depends on procedural norms that are actively maintained (working group culture, consensus definitions) and that suppress alternative coordination modes by network-effect lock-in. Theater ratio (0.42) reflects the growing gap between the performative openness of the process and the substantive capture of its outputs. Accessibility collapse (0.50) captures the partial closure of alternatives: other standards bodies exist but lack the IETF's network-effect legitimacy. Resistance (0.35) is moderate because opposition is fragmented across small implementers and civil society without concentrated power.
 *
 * PERSPECTIVAL GAP:
 *   The large platform operator seat experiences the constraint as legitimate, necessary technical coordination that rewards investment and scale. The small implementer seat experiences the same standards as barrier-raising and fragmentation-inducing. The IETF administration seat experiences the process as neutral meritocracy while structurally producing captured outcomes. The engine computes this divergence from the same structural data; the claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Large platform operators are declared beneficiaries with arbitrage-grade exit (they can fork, sponsor, or route around the process), yielding a low directionality value. Small implementers and end users are declared victims with constrained exit, yielding high directionality. The IETF administration sits near symmetric: it does not collect extraction directly but derives institutional legitimacy from the process, and its exit is constrained by mission dependency.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification prevents two errors: (1) labeling the constraint a rope would ignore the structural extraction from small implementers and the resource-asymmetry in participation; (2) labeling it a snare would ignore the genuine interoperability coordination the IETF still provides. The measurement series shows extraction and theater accumulating over time, consistent with a coordination mechanism gradually captured rather than designed for extraction from the outset.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    corporate_draft_dominance,
    'What proportion of RFCs and working group drafts are authored or heavily influenced by large platform operators versus independent or academic contributors?',
    'Bibliometric analysis of draft authorship and affiliation data across IETF working groups.',
    'High corporate dominance would validate the capture_substrate reading''s victim/beneficiary structure; low dominance would support the commons_stewardship reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(corporate_draft_dominance, empirical, 'Empirical measure of corporate capture of standards authorship.').

omega_variable(
    participation_barrier_nature,
    'Is the resource barrier to IETF participation a natural coordination cost or an extractive gatekeeping mechanism?',
    'Comparative analysis of participation costs and outcomes across standards bodies with varying subsidy models.',
    'If barriers are natural cost, extraction is lower; if gatekeeping, extraction is higher and the reading shifts toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(participation_barrier_nature, conceptual, 'Ambiguity about whether participation costs are inherent or extractive.').

omega_variable(
    kernel_reading_ambiguity,
    'Does the IETF process function primarily as captured substrate, functioning commons, or procedurally vulnerable institution?',
    'Triangulation across the three sibling readings using outcome metrics including interoperability, market concentration, and procedural fairness.',
    'Determines which reading of the kernel is structurally dominant; affects classification and beneficiary mapping.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Uncertainty about which reading of the IETF kernel is structurally dominant.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ietf_openness_commitment__capture_substrate_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ietf_capture_substrate_tr_t0, ietf_openness_commitment__capture_substrate_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(ietf_capture_substrate_tr_t8, ietf_openness_commitment__capture_substrate_reading, theater_ratio, 8, 0.19).
narrative_ontology:measurement(ietf_capture_substrate_tr_t16, ietf_openness_commitment__capture_substrate_reading, theater_ratio, 16, 0.26).
narrative_ontology:measurement(ietf_capture_substrate_tr_t24, ietf_openness_commitment__capture_substrate_reading, theater_ratio, 24, 0.33).
narrative_ontology:measurement(ietf_capture_substrate_tr_t32, ietf_openness_commitment__capture_substrate_reading, theater_ratio, 32, 0.38).
narrative_ontology:measurement(ietf_capture_substrate_tr_t40, ietf_openness_commitment__capture_substrate_reading, theater_ratio, 40, 0.42).

% Extraction over time
narrative_ontology:measurement(ietf_capture_substrate_be_t0, ietf_openness_commitment__capture_substrate_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(ietf_capture_substrate_be_t8, ietf_openness_commitment__capture_substrate_reading, base_extractiveness, 8, 0.31).
narrative_ontology:measurement(ietf_capture_substrate_be_t16, ietf_openness_commitment__capture_substrate_reading, base_extractiveness, 16, 0.39).
narrative_ontology:measurement(ietf_capture_substrate_be_t24, ietf_openness_commitment__capture_substrate_reading, base_extractiveness, 24, 0.47).
narrative_ontology:measurement(ietf_capture_substrate_be_t32, ietf_openness_commitment__capture_substrate_reading, base_extractiveness, 32, 0.52).
narrative_ontology:measurement(ietf_capture_substrate_be_t40, ietf_openness_commitment__capture_substrate_reading, base_extractiveness, 40, 0.55).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(ietf_openness_commitment__capture_substrate_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ietf_openness_commitment__capture_substrate_reading, global_infrastructure).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
