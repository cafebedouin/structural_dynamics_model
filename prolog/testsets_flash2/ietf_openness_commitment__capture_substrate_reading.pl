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
 *   human_readable: IETF Openness Commitment (Capture Substrate Reading)
 *   domain: technology_governance/internet_standards/institutional_economics
 *
 * SUMMARY:
 *   This constraint describes the IETF's commitment to openness, viewed
 *   through the lens of 'capture as substrate.' While the IETF maintains a
 *   formal commitment to open standards development, this reading argues that
 *   the process has become a substrate for large platform operators to encode
 *   their resource advantages into 'open' standards. This leads to a form of
 *   gatekeeping where proprietary extensions are subtly integrated or
 *   favored, extracting value from smaller implementers and end-users who
 *   rely on true interoperability. The constraint is claimed as a Tangled
 *   Rope, reflecting its dual function of genuine coordination and asymmetric
 *   extraction.
 *
 * KEY AGENTS:
 *   - large_platform_operators: Primary beneficiary (institutional/arbitrage) — shapes standards to its advantage.
 *   - small_implementers: Primary payer (moderate/constrained) — bears costs of adapting to biased standards.
 *   - end_users: Secondary payer (powerless/trapped) — suffers from reduced competition and vendor lock-in.
 *   - ietf_working_groups: Agenda-setter (organized/constrained) — administers the process, susceptible to influence.
 *   - internet_society: Observer (institutional/analytical) — provides oversight but limited direct intervention.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ietf_openness_commitment__capture_substrate_reading, 0.65).
domain_priors:suppression_score(ietf_openness_commitment__capture_substrate_reading, 0.45).
domain_priors:theater_ratio(ietf_openness_commitment__capture_substrate_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ietf_openness_commitment__capture_substrate_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(ietf_openness_commitment__capture_substrate_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(ietf_openness_commitment__capture_substrate_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ietf_openness_commitment__capture_substrate_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(ietf_openness_commitment__capture_substrate_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ietf_openness_commitment__capture_substrate_reading, tangled_rope).
narrative_ontology:human_readable(ietf_openness_commitment__capture_substrate_reading, "IETF Openness Commitment (Capture Substrate Reading)").
narrative_ontology:topic_domain(ietf_openness_commitment__capture_substrate_reading, "technology_governance/internet_standards/institutional_economics").

domain_priors:requires_active_enforcement(ietf_openness_commitment__capture_substrate_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ietf_openness_commitment__capture_substrate_reading, '368c4739-c73f-4922-9722-69705c45b0c7').
narrative_ontology:cs_kernel_codification('368c4739-c73f-4922-9722-69705c45b0c7', formalized).
narrative_ontology:cs_authority_grounding('368c4739-c73f-4922-9722-69705c45b0c7', practice).
narrative_ontology:cs_interpretation_layer_present('368c4739-c73f-4922-9722-69705c45b0c7').
narrative_ontology:cs_reading_relation('368c4739-c73f-4922-9722-69705c45b0c7', ietf_openness_commitment__commons_stewardship_reading, coexists_with).
narrative_ontology:cs_reading_relation('368c4739-c73f-4922-9722-69705c45b0c7', ietf_openness_commitment__legitimacy_erosion_reading, coexists_with).
narrative_ontology:cs_axiom('368c4739-c73f-4922-9722-69705c45b0c7', foundational, standards_as_competitive_leverage).
narrative_ontology:cs_axiom_status(standards_as_competitive_leverage, holdable).
narrative_ontology:cs_axiom_grounding('368c4739-c73f-4922-9722-69705c45b0c7', standards_as_competitive_leverage, instrumental).
narrative_ontology:cs_axiom('368c4739-c73f-4922-9722-69705c45b0c7', foundational, resource_asymmetry_shapes_consensus).
narrative_ontology:cs_axiom_status(resource_asymmetry_shapes_consensus, holdable).
narrative_ontology:cs_axiom_grounding('368c4739-c73f-4922-9722-69705c45b0c7', resource_asymmetry_shapes_consensus, empirically_contingent).
narrative_ontology:cs_reference_frame('368c4739-c73f-4922-9722-69705c45b0c7', open_process_as_neutral_substrate).
narrative_ontology:cs_drift_state('368c4739-c73f-4922-9722-69705c45b0c7', contemporary_platform_economy, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('368c4739-c73f-4922-9722-69705c45b0c7', '').
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

% These entities leverage their significant resources and market power to influence the IETF standards process, ensuring that 'open' standards are compatible with or even dependent on their proprietary extensions. They benefit from the network effects of 'open' standards while maintaining a competitive advantage through their control over key implementations and related services. They can exit by simply not adopting standards that don't align with their interests, or by forking standards.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__capture_substrate_reading, large_platform_operators, beneficiary,
    institutional, generational, arbitrage, global).

% These developers and companies rely on truly open and interoperable standards to compete. They bear the cost of adapting to standards that are subtly biased towards large operators' proprietary systems, or face exclusion from markets dominated by those operators. Their exit options are limited, as they depend on the established standards for market access.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__capture_substrate_reading, small_implementers, payer,
    moderate, biographical, constrained, global).

% End users ultimately pay the cost of reduced competition and vendor lock-in when standards are subtly captured. They experience less choice, higher prices, and reduced interoperability, often without understanding the underlying technical and governance mechanisms. Their exit options are severely limited, often requiring switching entire ecosystems.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__capture_substrate_reading, end_users, payer,
    powerless, biographical, trapped, global).

% These groups are responsible for developing and refining Internet standards. While ostensibly open, their processes can be influenced by well-resourced participants who can dedicate more time and expertise to shaping outcomes. They enforce the 'rough consensus' model, which can be swayed by persistent, well-articulated arguments from powerful actors. Their exit is to disband or fork, which is rare and costly.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__capture_substrate_reading, ietf_working_groups, agenda_setter,
    organized, biographical, constrained, global).

% As the organizational home of the IETF, the Internet Society provides administrative and financial support and champions the open development of the Internet. From this reading, they observe the process and its outcomes, but their ability to directly intervene in the technical content of standards is limited. Their exit is to withdraw support, which would be a major institutional crisis.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__capture_substrate_reading, internet_society, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a neutral forum for diverse stakeholders to coordinate on technical specifications for the Internet, ensuring global interoperability and preventing fragmentation.
% TRANSFER_FUNCTION: Transfers influence over the direction of Internet standards from a broad, diverse community to well-resourced, organized actors, leading to standards that subtly favor their existing platforms and business models. This translates into economic value for large operators and costs for smaller players and users.
% ABSENT_VOICES: Many small implementers, academic researchers, and individual users lack the resources to consistently participate in complex, long-running standards discussions. Their perspectives, which would advocate for truly neutral and universally beneficial standards, are often underrepresented or drowned out by well-funded corporate interests.
% DISAPPEARANCE_RATIONALE: If the IETF's commitment to openness (even as a substrate for capture) vanished, the Internet's interoperability would rapidly degrade. Large operators would likely push purely proprietary solutions, leading to a fragmented 'splinternet' where different platforms are incompatible, forcing users and small implementers into specific walled gardens.
% FOUNDING_PROBLEM: The early Internet needed a mechanism for diverse, independent parties to agree on common technical protocols to ensure global interoperability and prevent vendor lock-in.
% FOUNDING_PROBLEM_CORROBORATION: The IETF and large operators claim the problem is still live, emphasizing the ongoing need for coordination. Small implementers and some academic observers argue that while coordination is still needed, the 'openness' aspect of the founding problem has been subtly undermined, leading to a new form of vendor lock-in. Independent analyses of standard adoption patterns and market concentration support this shifted-function reading.
narrative_ontology:disappearance_verdict(ietf_openness_commitment__capture_substrate_reading, world_rearranges).
narrative_ontology:founding_problem_status(ietf_openness_commitment__capture_substrate_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ietf_openness_commitment__capture_substrate_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(ietf_openness_commitment__capture_substrate_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ietf_openness_commitment__capture_substrate_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is moderate-high (0.65) because the 'open' standards, while technically accessible, often require significant resources to implement in a way that truly competes with the large operators' ecosystems. Suppression (0.45) is present but subtle: it's not outright censorship, but rather the suppression of alternative technical directions or the marginalization of voices that lack the resources to participate consistently. Theater ratio (0.30) reflects that while the IETF's open process is genuinely functional, a portion of its activity serves to legitimize outcomes that benefit powerful actors, masking the underlying capture. Accessibility collapse (0.40) is moderate, as alternatives are not completely foreclosed but are made significantly harder to pursue. Resistance (0.55) is also moderate, manifesting as ongoing debates, alternative proposals, and occasional public critiques from smaller players and academics.
 *
 * PERSPECTIVAL GAP:
 *   Large platform operators perceive the IETF process as a successful Rope, enabling essential coordination for the Internet's growth. Small implementers and end-users, however, experience it as a Tangled Rope, where the coordination function is intertwined with subtle, resource-driven extraction. The engine's per-seat classification will reflect this divergence, with beneficiaries seeing a Rope and payers experiencing a Snare or Tangled Rope.
 *
 * DIRECTIONALITY LOGIC:
 *   Large platform operators are beneficiaries (d near 0.0) because they leverage the 'open' process to encode their advantages, effectively receiving a subsidy in market control. Small implementers and end-users are targets (d near 1.0) as they bear the costs of this encoded gatekeeping. IETF working groups, while administering the process, are also constrained by the dynamics of participation and 'rough consensus,' placing them closer to symmetric (d near 0.5) but with a bias towards those who can participate most effectively.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling the IETF's openness commitment as a pure Rope (as its proponents claim) or a pure Snare (as some critics might assert). By identifying it as a Tangled Rope, the analysis acknowledges its genuine coordination function while highlighting the asymmetric extraction that has accumulated over time. This allows for a nuanced understanding of how a valuable coordination mechanism can become a substrate for subtle capture, rather than dismissing its original mandate entirely.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    resource_advantage_quantification,
    'To what extent does resource advantage (e.g., number of engineers, legal teams, lobbying budget) directly translate into influence over IETF standards outcomes?',
    'Empirical study correlating participant resource levels with successful proposal adoption, or analysis of ''rough consensus'' formation in contentious areas.',
    'Strong correlation would further solidify the ''capture substrate'' reading, potentially reclassifying the constraint closer to a Snare for certain seats. Weak correlation would support a more ''commons stewardship'' reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(resource_advantage_quantification, empirical, 'Quantifying the link between resources and influence in standards development.').

omega_variable(
    proprietary_extension_detection,
    'How many ''open'' standards developed through the IETF process have subsequently become de facto proprietary due to essential, non-standardized extensions controlled by a single dominant vendor?',
    'Technical audit of widely adopted ''open'' standards and their real-world implementations, identifying critical dependencies on proprietary components.',
    'A high number of such instances would strongly support the ''capture substrate'' reading, indicating that the ''openness'' is largely performative in practice, increasing the effective extractiveness.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(proprietary_extension_detection, empirical, 'Measuring the extent of proprietary lock-in within ''open'' standards.').

omega_variable(
    legitimacy_of_rough_consensus,
    'Is ''rough consensus'' a genuinely equitable decision-making mechanism, or does it inherently favor well-resourced, persistent voices?',
    'Sociological and political science analysis of IETF decision-making processes, including power dynamics and participation biases.',
    'If found to be inherently biased, the legitimacy_erosion_reading would gain significant weight, and the IETF''s governance model itself would be reclassified as more extractive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_of_rough_consensus, conceptual, 'Assessing the fairness and equity of the IETF''s ''rough consensus'' model.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ietf_openness_commitment__capture_substrate_reading, 1995, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ietf_tr_t1995, ietf_openness_commitment__capture_substrate_reading, theater_ratio, 1995, 0.1).
narrative_ontology:measurement(ietf_tr_t2000, ietf_openness_commitment__capture_substrate_reading, theater_ratio, 2000, 0.15).
narrative_ontology:measurement(ietf_tr_t2005, ietf_openness_commitment__capture_substrate_reading, theater_ratio, 2005, 0.2).
narrative_ontology:measurement(ietf_tr_t2010, ietf_openness_commitment__capture_substrate_reading, theater_ratio, 2010, 0.25).
narrative_ontology:measurement(ietf_tr_t2015, ietf_openness_commitment__capture_substrate_reading, theater_ratio, 2015, 0.28).
narrative_ontology:measurement(ietf_tr_t2020, ietf_openness_commitment__capture_substrate_reading, theater_ratio, 2020, 0.29).
narrative_ontology:measurement(ietf_tr_t2025, ietf_openness_commitment__capture_substrate_reading, theater_ratio, 2025, 0.3).

% Extraction over time
narrative_ontology:measurement(ietf_be_t1995, ietf_openness_commitment__capture_substrate_reading, base_extractiveness, 1995, 0.3).
narrative_ontology:measurement(ietf_be_t2000, ietf_openness_commitment__capture_substrate_reading, base_extractiveness, 2000, 0.4).
narrative_ontology:measurement(ietf_be_t2005, ietf_openness_commitment__capture_substrate_reading, base_extractiveness, 2005, 0.5).
narrative_ontology:measurement(ietf_be_t2010, ietf_openness_commitment__capture_substrate_reading, base_extractiveness, 2010, 0.58).
narrative_ontology:measurement(ietf_be_t2015, ietf_openness_commitment__capture_substrate_reading, base_extractiveness, 2015, 0.62).
narrative_ontology:measurement(ietf_be_t2020, ietf_openness_commitment__capture_substrate_reading, base_extractiveness, 2020, 0.64).
narrative_ontology:measurement(ietf_be_t2025, ietf_openness_commitment__capture_substrate_reading, base_extractiveness, 2025, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(ietf_su_t1995, ietf_openness_commitment__capture_substrate_reading, suppression_requirement, 1995, 0.2).
narrative_ontology:measurement(ietf_su_t2000, ietf_openness_commitment__capture_substrate_reading, suppression_requirement, 2000, 0.28).
narrative_ontology:measurement(ietf_su_t2005, ietf_openness_commitment__capture_substrate_reading, suppression_requirement, 2005, 0.35).
narrative_ontology:measurement(ietf_su_t2010, ietf_openness_commitment__capture_substrate_reading, suppression_requirement, 2010, 0.4).
narrative_ontology:measurement(ietf_su_t2015, ietf_openness_commitment__capture_substrate_reading, suppression_requirement, 2015, 0.43).
narrative_ontology:measurement(ietf_su_t2020, ietf_openness_commitment__capture_substrate_reading, suppression_requirement, 2020, 0.44).
narrative_ontology:measurement(ietf_su_t2025, ietf_openness_commitment__capture_substrate_reading, suppression_requirement, 2025, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ietf_openness_commitment__capture_substrate_reading, information_standard).
narrative_ontology:affects_constraint(ietf_openness_commitment__capture_substrate_reading, ietf_openness_commitment__commons_stewardship_reading).
narrative_ontology:affects_constraint(ietf_openness_commitment__capture_substrate_reading, ietf_openness_commitment__legitimacy_erosion_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'IETF openness commitment' kernel. It focuses on how resource advantage translates into encoded gatekeeping within the standards process. Sibling readings explore the commons stewardship function and the erosion of the rough consensus mechanism's legitimacy.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
