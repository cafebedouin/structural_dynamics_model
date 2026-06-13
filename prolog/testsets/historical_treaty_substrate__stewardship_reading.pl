% ============================================================================
% CONSTRAINT STORY: historical_treaty_substrate__stewardship_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_historical_treaty_substrate__stewardship_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: historical_treaty_substrate__stewardship_reading
 *   human_readable: Historical Treaty Substrate (Stewardship Reading)
 *   domain: legal_anthropology/indigenous_law/comparative_constitutional_theory
 *
 * SUMMARY:
 *   This constraint represents the 'stewardship reading' of historical
 *   treaties between Indigenous nations and settler states. In this reading,
 *   treaties are understood as relational pacts for shared territorial
 *   stewardship, affirming Indigenous sovereignty and establishing mutual
 *   obligations for coexistence and co-management, rather than ceding land.
 *   This reading emphasizes ongoing relationships and responsibilities over
 *   one-time transactions. The metrics reflect a relatively low
 *   extractiveness and suppression, as this reading posits a more equitable
 *   and consensual framework, though it acknowledges the historical and
 *   ongoing struggle to uphold these principles.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(historical_treaty_substrate__stewardship_reading, 0.25).
domain_priors:suppression_score(historical_treaty_substrate__stewardship_reading, 0.15).
domain_priors:theater_ratio(historical_treaty_substrate__stewardship_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(historical_treaty_substrate__stewardship_reading, extractiveness, 0.25).
narrative_ontology:constraint_metric(historical_treaty_substrate__stewardship_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(historical_treaty_substrate__stewardship_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(historical_treaty_substrate__stewardship_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(historical_treaty_substrate__stewardship_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(historical_treaty_substrate__stewardship_reading, rope).
narrative_ontology:human_readable(historical_treaty_substrate__stewardship_reading, "Historical Treaty Substrate (Stewardship Reading)").
narrative_ontology:topic_domain(historical_treaty_substrate__stewardship_reading, "legal_anthropology/indigenous_law/comparative_constitutional_theory").

domain_priors:requires_active_enforcement(historical_treaty_substrate__stewardship_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(historical_treaty_substrate__stewardship_reading, 'fd965628-4e6e-4373-afb3-5f02ba4180ff').
narrative_ontology:cs_kernel_codification('fd965628-4e6e-4373-afb3-5f02ba4180ff', fixed_text).
narrative_ontology:cs_authority_grounding('fd965628-4e6e-4373-afb3-5f02ba4180ff', lineage).
narrative_ontology:cs_interpretation_layer_present('fd965628-4e6e-4373-afb3-5f02ba4180ff').
narrative_ontology:cs_reading_relation('fd965628-4e6e-4373-afb3-5f02ba4180ff', historical_treaty_substrate__extinguishment_reading, forecloses).
narrative_ontology:cs_reading_relation('fd965628-4e6e-4373-afb3-5f02ba4180ff', historical_treaty_substrate__nation_to_nation_reading, coexists_with).
narrative_ontology:cs_axiom('fd965628-4e6e-4373-afb3-5f02ba4180ff', foundational, indigenous_sovereignty_uncoded).
narrative_ontology:cs_axiom_status(indigenous_sovereignty_uncoded, holdable).
narrative_ontology:cs_axiom_grounding('fd965628-4e6e-4373-afb3-5f02ba4180ff', indigenous_sovereignty_uncoded, deontological).
narrative_ontology:cs_axiom('fd965628-4e6e-4373-afb3-5f02ba4180ff', foundational, territorial_stewardship_mutual_obligation).
narrative_ontology:cs_axiom_status(territorial_stewardship_mutual_obligation, holdable).
narrative_ontology:cs_axiom_grounding('fd965628-4e6e-4373-afb3-5f02ba4180ff', territorial_stewardship_mutual_obligation, conventional).
narrative_ontology:cs_reference_frame('fd965628-4e6e-4373-afb3-5f02ba4180ff', original_relational_pact).
narrative_ontology:cs_drift_state('fd965628-4e6e-4373-afb3-5f02ba4180ff', contemporary_reconciliation_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('fd965628-4e6e-4373-afb3-5f02ba4180ff', '').
narrative_ontology:cs_kernel_id(historical_treaty_substrate__stewardship_reading, historical_treaty_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(historical_treaty_substrate__stewardship_reading, indigenous_nations).
narrative_ontology:constraint_beneficiary(historical_treaty_substrate__stewardship_reading, settler_state_citizens).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(historical_treaty_substrate__stewardship_reading, resource_extraction_industries).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Retain inherent jurisdiction over traditional territories and participate in co-management of resources. Their sovereignty is affirmed, and their traditional stewardship practices are recognized as foundational to the treaty relationship. They benefit from mutual obligations for coexistence and shared governance.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__stewardship_reading, indigenous_nations, beneficiary,
    organized, generational, identity_locked, regional).

% Is obligated to seek consent for resource development, engage in shared governance, and uphold mutual obligations for coexistence. They benefit from stable, legitimate access to resources and peaceful relations, but bear the cost of shared decision-making and limits on unilateral action. Their role is to administer the pact, not to unilaterally extract.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__stewardship_reading, settler_state_government, agenda_setter,
    institutional, generational, constrained, national).

% Benefit from stable, peaceful coexistence and sustainable resource management through shared stewardship. They are educated about the relational nature of treaties and participate in a society that respects Indigenous sovereignty and co-governance principles.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__stewardship_reading, settler_state_citizens, beneficiary,
    moderate, biographical, mobile, national).

% Must operate under co-management agreements, obtain consent from Indigenous nations, and adhere to stricter environmental and social standards. They bear increased costs and face delays due to shared decision-making, but gain long-term stability and legitimacy for their operations.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__stewardship_reading, resource_extraction_industries, payer,
    powerful, immediate, constrained, regional).

% Analyze and articulate the principles of Indigenous legal traditions and their application to treaty interpretation, advocating for a relational and stewardship-based understanding of historical agreements. They provide intellectual grounding for this reading.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__stewardship_reading, legal_scholars_indigenous_law, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a framework for shared territorial stewardship and peaceful coexistence between Indigenous nations and the settler state, ensuring sustainable resource management and mutual respect for distinct jurisdictions.
% TRANSFER_FUNCTION: Transfers decision-making authority from unilateral settler state control to joint governance structures, and transfers obligations for consent and shared management to the settler state, while affirming Indigenous nations' inherent jurisdiction.
% ABSENT_VOICES: Those who adhere strictly to a doctrine of absolute Crown sovereignty or who believe in the unilateral right to extract resources without Indigenous consent are structurally excluded from the co-governance framework this reading establishes. They would argue for a more extractive, less constrained approach to territorial management.
% DISAPPEARANCE_RATIONALE: If this reading of treaties vanished, the legal and political landscape would fundamentally shift. Indigenous nations would lose a key framework for asserting their inherent jurisdiction and co-management rights, likely leading to increased conflict over land and resources. The settler state would lose a legitimate basis for its presence and resource access, destabilizing governance and economic activity. The entire relationship would revert to a more extractive, colonial model.
% FOUNDING_PROBLEM: The original problem was how to establish a lasting relationship of peace, friendship, and mutual respect between distinct peoples sharing a territory, ensuring the well-being of all and the sustainable use of the land.
% FOUNDING_PROBLEM_CORROBORATION: Indigenous elders and legal scholars consistently attest that the original intent of treaties was relational and about shared stewardship, not land cession. This is corroborated by oral histories, traditional legal principles, and comparative analysis of treaty-making practices globally, from outside the settler state's self-serving narratives.
narrative_ontology:disappearance_verdict(historical_treaty_substrate__stewardship_reading, world_rearranges).
narrative_ontology:founding_problem_status(historical_treaty_substrate__stewardship_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(historical_treaty_substrate__stewardship_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(historical_treaty_substrate__stewardship_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(historical_treaty_substrate__stewardship_reading_tests).
:- end_tests(historical_treaty_substrate__stewardship_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.25) is low because this reading fundamentally limits unilateral resource extraction by the settler state, requiring consent and shared governance. Suppression (0.15) is also low, as the framework is built on mutual respect and negotiation, not coercion. Theater ratio (0.1) is minimal, as the core function is genuine coordination and shared responsibility, though some performative elements may exist in the settler state's engagement. The historical measurements show some fluctuation, reflecting periods where this reading was more or less acknowledged or enforced, but generally trending towards a more balanced relationship.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of Indigenous nations, this reading aligns with their traditional understandings of treaties as ongoing relationships. From the settler state's perspective, adopting this reading requires a significant shift from historical claims of absolute sovereignty, leading to a different experience of the constraint as one of obligation and shared power. Resource industries would experience it as a constraint on their profit motives, while citizens might see it as a path to reconciliation and sustainability.
 *
 * DIRECTIONALITY LOGIC:
 *   Indigenous nations are primary beneficiaries, retaining jurisdiction and gaining co-management rights. Settler state citizens also benefit from stable coexistence and sustainable practices. The settler state government is an agenda-setter with significant obligations, benefiting from legitimacy but constrained in unilateral action. Resource extraction industries are payers, facing increased costs and shared decision-making. This reading aims for a more balanced directionality, moving away from the settler state as a full beneficiary and Indigenous nations as full targets.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    stewardship_vs_extinguishment_ambiguity,
    'Is the historical treaty substrate fundamentally a pact for shared stewardship, or a mechanism for land extinguishment?',
    'Comprehensive historical and legal analysis incorporating Indigenous oral histories, legal traditions, and comparative treaty law, alongside settler state archives, to determine the dominant intent and understanding at the time of treaty-making and its evolution.',
    'If resolved as primarily extinguishment, this constraint would reclassify towards a Snare, with significantly higher extractiveness and suppression. If resolved as stewardship, it reinforces the Rope classification and its lower extractiveness.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(stewardship_vs_extinguishment_ambiguity, conceptual, 'Ambiguity between stewardship and extinguishment interpretations of historical treaties.').

omega_variable(
    implementation_gap_stewardship_reading,
    'To what extent is the ''stewardship reading'' genuinely implemented in practice versus remaining an aspirational legal theory?',
    'Empirical assessment of co-management agreements, consent processes for resource development, and judicial decisions on Indigenous land rights. Measurement of actual power-sharing in governance structures.',
    'If the implementation gap is wide, the effective extractiveness and suppression experienced by Indigenous nations would be higher than this reading suggests, potentially pushing the classification towards a Tangled Rope or even Snare in practice, despite the theoretical framing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(implementation_gap_stewardship_reading, empirical, 'Gap between the theoretical ''stewardship reading'' and its practical implementation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(historical_treaty_substrate__stewardship_reading, 1700, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hist_tr_t1700, historical_treaty_substrate__stewardship_reading, theater_ratio, 1700, 0.05).
narrative_ontology:measurement(hist_tr_t1800, historical_treaty_substrate__stewardship_reading, theater_ratio, 1800, 0.08).
narrative_ontology:measurement(hist_tr_t1900, historical_treaty_substrate__stewardship_reading, theater_ratio, 1900, 0.12).
narrative_ontology:measurement(hist_tr_t2000, historical_treaty_substrate__stewardship_reading, theater_ratio, 2000, 0.1).
narrative_ontology:measurement(hist_tr_t2024, historical_treaty_substrate__stewardship_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(hist_be_t1700, historical_treaty_substrate__stewardship_reading, base_extractiveness, 1700, 0.2).
narrative_ontology:measurement(hist_be_t1800, historical_treaty_substrate__stewardship_reading, base_extractiveness, 1800, 0.25).
narrative_ontology:measurement(hist_be_t1900, historical_treaty_substrate__stewardship_reading, base_extractiveness, 1900, 0.3).
narrative_ontology:measurement(hist_be_t2000, historical_treaty_substrate__stewardship_reading, base_extractiveness, 2000, 0.28).
narrative_ontology:measurement(hist_be_t2024, historical_treaty_substrate__stewardship_reading, base_extractiveness, 2024, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(hist_su_t1700, historical_treaty_substrate__stewardship_reading, suppression_requirement, 1700, 0.1).
narrative_ontology:measurement(hist_su_t1800, historical_treaty_substrate__stewardship_reading, suppression_requirement, 1800, 0.15).
narrative_ontology:measurement(hist_su_t1900, historical_treaty_substrate__stewardship_reading, suppression_requirement, 1900, 0.2).
narrative_ontology:measurement(hist_su_t2000, historical_treaty_substrate__stewardship_reading, suppression_requirement, 2000, 0.18).
narrative_ontology:measurement(hist_su_t2024, historical_treaty_substrate__stewardship_reading, suppression_requirement, 2024, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(historical_treaty_substrate__stewardship_reading, identity_coordination).
narrative_ontology:affects_constraint(historical_treaty_substrate__stewardship_reading, resource_governance_frameworks).
narrative_ontology:affects_constraint(historical_treaty_substrate__stewardship_reading, indigenous_rights_litigation).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'historical_treaty_substrate' kernel. The other readings are 'extinguishment_reading' and 'nation_to_nation_reading', each representing a distinct structural interpretation of historical treaties.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
