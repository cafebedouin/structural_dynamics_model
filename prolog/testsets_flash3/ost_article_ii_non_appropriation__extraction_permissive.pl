% ============================================================================
% CONSTRAINT STORY: ost_article_ii_non_appropriation__extraction_permissive
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ost_article_ii_non_appropriation__extraction_permissive, []).

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
 *   constraint_id: ost_article_ii_non_appropriation__extraction_permissive
 *   human_readable: Outer Space Treaty Article II: Non-Appropriation (Extraction-Permissive Reading)
 *   domain: international_space_law/commons_governance
 *
 * SUMMARY:
 *   This constraint represents the 'extraction-permissive' reading of Outer
 *   Space Treaty (OST) Article II, which states that outer space is not
 *   subject to national appropriation. This reading interprets the article as
 *   barring sovereign territorial claims but not private ownership of
 *   extracted resources. This interpretation has gained traction as space
 *   resource extraction becomes technologically feasible, leading to a de
 *   facto enclosure of resources by technologically advanced actors. The
 *   constraint is classified as a Tangled Rope because it provides a minimal
 *   coordination function (preventing overt territorial claims) while
 *   enabling significant asymmetric extraction.
 *
 * KEY AGENTS:
 *   - space_resource_companies: Primary beneficiary/agenda-setter (powerful/mobile) — drives the permissive interpretation and benefits from it.
 *   - technologically_advanced_states: Beneficiary (institutional/arbitrage) — supports national companies and benefits from their access to resources.
 *   - developing_nations: Primary victim (powerless/trapped) — excluded from resource access and benefit-sharing.
 *   - future_generations: Victim (powerless/trapped) — bears long-term costs of resource depletion.
 *   - international_legal_scholars: Observer (analytical/analytical) — analyzes the legal ambiguities and consequences.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ost_article_ii_non_appropriation__extraction_permissive, 0.85).
domain_priors:suppression_score(ost_article_ii_non_appropriation__extraction_permissive, 0.7).
domain_priors:theater_ratio(ost_article_ii_non_appropriation__extraction_permissive, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__extraction_permissive, extractiveness, 0.85).
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__extraction_permissive, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__extraction_permissive, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__extraction_permissive, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__extraction_permissive, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ost_article_ii_non_appropriation__extraction_permissive, tangled_rope).
narrative_ontology:human_readable(ost_article_ii_non_appropriation__extraction_permissive, "Outer Space Treaty Article II: Non-Appropriation (Extraction-Permissive Reading)").
narrative_ontology:topic_domain(ost_article_ii_non_appropriation__extraction_permissive, "international_space_law/commons_governance").

domain_priors:requires_active_enforcement(ost_article_ii_non_appropriation__extraction_permissive).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ost_article_ii_non_appropriation__extraction_permissive, '1293cccd-0b47-46e6-bb29-eb2f4c26c141').
narrative_ontology:cs_kernel_codification('1293cccd-0b47-46e6-bb29-eb2f4c26c141', fixed_text).
narrative_ontology:cs_authority_grounding('1293cccd-0b47-46e6-bb29-eb2f4c26c141', lineage).
narrative_ontology:cs_interpretation_layer_present('1293cccd-0b47-46e6-bb29-eb2f4c26c141').
narrative_ontology:cs_reading_relation('1293cccd-0b47-46e6-bb29-eb2f4c26c141', ost_article_ii_non_appropriation__commons_conservation, coexists_with).
narrative_ontology:cs_reading_relation('1293cccd-0b47-46e6-bb29-eb2f4c26c141', ost_article_ii_non_appropriation__international_regime, coexists_with).
narrative_ontology:cs_axiom('1293cccd-0b47-46e6-bb29-eb2f4c26c141', foundational, private_property_rights_in_extracted_resources).
narrative_ontology:cs_axiom_status(private_property_rights_in_extracted_resources, holdable).
narrative_ontology:cs_axiom_grounding('1293cccd-0b47-46e6-bb29-eb2f4c26c141', private_property_rights_in_extracted_resources, conventional).
narrative_ontology:cs_axiom('1293cccd-0b47-46e6-bb29-eb2f4c26c141', foundational, non_appropriation_limited_to_sovereign_claims).
narrative_ontology:cs_axiom_status(non_appropriation_limited_to_sovereign_claims, holdable).
narrative_ontology:cs_axiom_grounding('1293cccd-0b47-46e6-bb29-eb2f4c26c141', non_appropriation_limited_to_sovereign_claims, conventional).
narrative_ontology:cs_reference_frame('1293cccd-0b47-46e6-bb29-eb2f4c26c141', early_space_law_minimalist_interpretation).
narrative_ontology:cs_drift_state('1293cccd-0b47-46e6-bb29-eb2f4c26c141', contemporary_space_resource_boom, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('1293cccd-0b47-46e6-bb29-eb2f4c26c141', '').
narrative_ontology:cs_kernel_id(ost_article_ii_non_appropriation__extraction_permissive, ost_article_ii_non_appropriation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ost_article_ii_non_appropriation__extraction_permissive, space_resource_companies).
narrative_ontology:constraint_beneficiary(ost_article_ii_non_appropriation__extraction_permissive, technologically_advanced_states).
narrative_ontology:constraint_victim(ost_article_ii_non_appropriation__extraction_permissive, developing_nations).
narrative_ontology:constraint_victim(ost_article_ii_non_appropriation__extraction_permissive, future_generations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Actively developing technologies for asteroid mining and lunar resource extraction. They interpret Article II as permitting private ownership of extracted resources, provided no sovereign claim is made. They benefit directly from the lack of a clear prohibition on private appropriation.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__extraction_permissive, space_resource_companies, agenda_setter,
    powerful, biographical, mobile, global).

% Support their national space resource companies through legal frameworks and diplomatic efforts. They benefit from their companies' ability to access and exploit space resources without an international sharing or compensation mechanism. Their legal interpretations align with the extraction-permissive reading.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__extraction_permissive, technologically_advanced_states, beneficiary,
    institutional, generational, arbitrage, global).

% Lack the technological and financial capacity to access space resources. They bear the cost of exclusion from a common heritage, with no mechanism for benefit-sharing or compensation, effectively losing access to resources that are theoretically common property.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__extraction_permissive, developing_nations, payer,
    powerless, generational, trapped, global).

% Bear the long-term cost of resource depletion and potential environmental damage in space, without having a voice in current policy or a share in the benefits of extraction. Their interests are not represented in the current legal interpretation.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__extraction_permissive, future_generations, payer,
    powerless, civilizational, trapped, universal).

% Analyze the legal implications of Article II and its various interpretations. They highlight the ambiguities and the potential for conflict arising from the extraction-permissive reading, but have no direct power to enforce or alter the constraint.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__extraction_permissive, international_legal_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a minimal framework for states to avoid direct territorial conflict in space by prohibiting sovereign claims, allowing for some level of predictable activity without formal annexation.
% TRANSFER_FUNCTION: Facilitates the transfer of valuable space resources from the 'common heritage of mankind' to private entities and their sponsoring states, without a corresponding transfer of benefits or compensation to other nations.
% ABSENT_VOICES: Developing nations and future generations are largely excluded from the current interpretive discourse, as their lack of technological capability or temporal presence prevents them from asserting their claims to space resources. They would advocate for a more equitable and conservation-oriented regime.
% DISAPPEARANCE_RATIONALE: If this interpretation vanished, it would create immediate legal uncertainty for space resource companies and their state sponsors, potentially halting extraction efforts until a new, more explicit international regime is established. The current 'gold rush' mentality would be replaced by a scramble for new legal frameworks.
% FOUNDING_PROBLEM: The original problem was to prevent a new 'scramble for Africa' in space, ensuring that space remained a realm for peaceful exploration and use, free from national appropriation and military conflict.
% FOUNDING_PROBLEM_CORROBORATION: The problem of preventing conflict and ensuring peaceful use of space remains live, attested by all states. However, the interpretation of 'non-appropriation' as permitting private extraction is contested, with many states and legal scholars arguing it undermines the original intent and creates new forms of conflict over resources.
narrative_ontology:disappearance_verdict(ost_article_ii_non_appropriation__extraction_permissive, world_rearranges).
narrative_ontology:founding_problem_status(ost_article_ii_non_appropriation__extraction_permissive, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ost_article_ii_non_appropriation__extraction_permissive, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(ost_article_ii_non_appropriation__extraction_permissive, 'none', 1).
narrative_ontology:epsilon_provenance(ost_article_ii_non_appropriation__extraction_permissive, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ost_article_ii_non_appropriation__extraction_permissive_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ost_article_ii_non_appropriation__extraction_permissive, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ost_article_ii_non_appropriation__extraction_permissive_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.85) is high because this reading allows for the unilateral capture of resources that are nominally the 'common heritage of mankind,' concentrating wealth and opportunity. Suppression (0.7) is significant because states lacking space capabilities are effectively prevented from accessing these resources, and their attempts to establish a more equitable regime (e.g., through the Moon Agreement) have been actively resisted. The theater ratio (0.2) is low, as the prohibition on sovereign claims is genuinely upheld, but it serves as a cover for private appropriation. The rising extractiveness over time reflects the increasing technological feasibility of space resource extraction and the corresponding hardening of this permissive interpretation.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of space resource companies and advanced states, this interpretation is a reasonable way to enable economic activity in space while respecting the OST's core principle. From the perspective of developing nations and those advocating for a 'common heritage' approach, it represents a new form of colonial enclosure, leveraging technological advantage to privatize common resources.
 *
 * DIRECTIONALITY LOGIC:
 *   Space resource companies and technologically advanced states are clear beneficiaries, as this reading enables their activities and secures their gains (low directionality). Developing nations and future generations are targets, bearing the costs of exclusion and potential resource depletion (high directionality).
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's original mandate was to prevent a 'scramble for space' and ensure peaceful use. This reading, while preventing overt military appropriation, has allowed for an economic 'scramble' for resources. The classification as Tangled Rope highlights that the coordination function (preventing sovereign claims) is intertwined with an extractive function (permitting private resource capture), preventing mislabeling it as pure coordination or pure extraction. The contest over the founding problem status ('contested') further underscores this tension.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    private_vs_sovereign_appropriation,
    'Does the OST''s prohibition on ''national appropriation'' implicitly extend to private appropriation by entities under national jurisdiction, or is it strictly limited to sovereign claims?',
    'An advisory opinion from the International Court of Justice or a new multilateral treaty explicitly clarifying the scope of ''appropriation'' in the context of private resource extraction.',
    'If private appropriation is deemed implicitly prohibited, this reading would be foreclosed, leading to a reclassification towards a Snare or even a Mountain (if the prohibition is seen as inherent). If explicitly permitted, the current Tangled Rope classification would be reinforced, but with increased pressure for benefit-sharing mechanisms.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(private_vs_sovereign_appropriation, conceptual, 'Ambiguity regarding the scope of ''appropriation'' in Article II of the Outer Space Treaty.').

omega_variable(
    common_heritage_principle_applicability,
    'To what extent does the ''common heritage of mankind'' principle, articulated in other international agreements, apply to space resources under the OST framework?',
    'Negotiation and ratification of a new international agreement (e.g., a revised Moon Agreement or a new ''Space Resources Treaty'') that explicitly defines the common heritage principle''s application to space resources.',
    'If the common heritage principle is strongly affirmed and applied, it would necessitate a benefit-sharing regime, significantly reducing the extractiveness of this reading and potentially reclassifying it towards a Rope or Scaffold. If rejected, the current extractive dynamics would intensify.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(common_heritage_principle_applicability, preference, 'The normative status and practical implications of the ''common heritage of mankind'' principle for space resources.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ost_article_ii_non_appropriation__extraction_permissive, 1967, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ost__tr_t1967, ost_article_ii_non_appropriation__extraction_permissive, theater_ratio, 1967, 0.1).
narrative_ontology:measurement(ost__tr_t1980, ost_article_ii_non_appropriation__extraction_permissive, theater_ratio, 1980, 0.12).
narrative_ontology:measurement(ost__tr_t1995, ost_article_ii_non_appropriation__extraction_permissive, theater_ratio, 1995, 0.15).
narrative_ontology:measurement(ost__tr_t2010, ost_article_ii_non_appropriation__extraction_permissive, theater_ratio, 2010, 0.18).
narrative_ontology:measurement(ost__tr_t2024, ost_article_ii_non_appropriation__extraction_permissive, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(ost__be_t1967, ost_article_ii_non_appropriation__extraction_permissive, base_extractiveness, 1967, 0.3).
narrative_ontology:measurement(ost__be_t1980, ost_article_ii_non_appropriation__extraction_permissive, base_extractiveness, 1980, 0.45).
narrative_ontology:measurement(ost__be_t1995, ost_article_ii_non_appropriation__extraction_permissive, base_extractiveness, 1995, 0.6).
narrative_ontology:measurement(ost__be_t2010, ost_article_ii_non_appropriation__extraction_permissive, base_extractiveness, 2010, 0.75).
narrative_ontology:measurement(ost__be_t2024, ost_article_ii_non_appropriation__extraction_permissive, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(ost__su_t1967, ost_article_ii_non_appropriation__extraction_permissive, suppression_requirement, 1967, 0.2).
narrative_ontology:measurement(ost__su_t1980, ost_article_ii_non_appropriation__extraction_permissive, suppression_requirement, 1980, 0.35).
narrative_ontology:measurement(ost__su_t1995, ost_article_ii_non_appropriation__extraction_permissive, suppression_requirement, 1995, 0.5).
narrative_ontology:measurement(ost__su_t2010, ost_article_ii_non_appropriation__extraction_permissive, suppression_requirement, 2010, 0.6).
narrative_ontology:measurement(ost__su_t2024, ost_article_ii_non_appropriation__extraction_permissive, suppression_requirement, 2024, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ost_article_ii_non_appropriation__extraction_permissive, enforcement_mechanism).
narrative_ontology:affects_constraint(ost_article_ii_non_appropriation__extraction_permissive, ost_article_ii_non_appropriation__commons_conservation).
narrative_ontology:affects_constraint(ost_article_ii_non_appropriation__extraction_permissive, ost_article_ii_non_appropriation__international_regime).
narrative_ontology:affects_constraint(ost_article_ii_non_appropriation__extraction_permissive, space_resource_governance_regime).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'OST Article II Non-Appropriation' kernel. This 'extraction-permissive' reading directly influences the 'space_resource_governance_regime' by shaping the de facto legal environment for resource extraction.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
