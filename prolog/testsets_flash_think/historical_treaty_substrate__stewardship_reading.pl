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
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: historical_treaty_substrate__stewardship_reading
 *   human_readable: Historical Treaties as Shared Territorial Stewardship
 *   domain: legal_anthropology/indigenous_law/comparative_constitutional_theory
 *
 * SUMMARY:
 *   This constraint instantiates the 'stewardship reading' of historical
 *   treaties, interpreting them as relational pacts for shared territorial
 *   stewardship, affirming inherent Indigenous sovereignty and mutual
 *   obligations for coexistence, rather than as instruments of land cession.
 *   This reading emphasizes ongoing consent and shared governance over
 *   resources. It is a contested interpretation, often advanced by Indigenous
 *   nations and their allies, and actively resisted by those who benefit from
 *   more extractive readings.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(historical_treaty_substrate__stewardship_reading, 0.25).
domain_priors:suppression_score(historical_treaty_substrate__stewardship_reading, 0.15).
domain_priors:theater_ratio(historical_treaty_substrate__stewardship_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(historical_treaty_substrate__stewardship_reading, extractiveness, 0.25).
narrative_ontology:constraint_metric(historical_treaty_substrate__stewardship_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(historical_treaty_substrate__stewardship_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(historical_treaty_substrate__stewardship_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(historical_treaty_substrate__stewardship_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(historical_treaty_substrate__stewardship_reading, rope).
narrative_ontology:human_readable(historical_treaty_substrate__stewardship_reading, "Historical Treaties as Shared Territorial Stewardship").
narrative_ontology:topic_domain(historical_treaty_substrate__stewardship_reading, "legal_anthropology/indigenous_law/comparative_constitutional_theory").

domain_priors:requires_active_enforcement(historical_treaty_substrate__stewardship_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(historical_treaty_substrate__stewardship_reading, '786e4a1b-08e4-4002-89eb-0f5931e148c3').
narrative_ontology:cs_kernel_codification('786e4a1b-08e4-4002-89eb-0f5931e148c3', fixed_text).
narrative_ontology:cs_authority_grounding('786e4a1b-08e4-4002-89eb-0f5931e148c3', lineage).
narrative_ontology:cs_interpretation_layer_present('786e4a1b-08e4-4002-89eb-0f5931e148c3').
narrative_ontology:cs_reading_relation('786e4a1b-08e4-4002-89eb-0f5931e148c3', historical_treaty_substrate__extinguishment_reading, forecloses).
narrative_ontology:cs_reading_relation('786e4a1b-08e4-4002-89eb-0f5931e148c3', historical_treaty_substrate__nation_to_nation_reading, coexists_with).
narrative_ontology:cs_axiom('786e4a1b-08e4-4002-89eb-0f5931e148c3', foundational, inherent_indigenous_sovereignty).
narrative_ontology:cs_axiom_status(inherent_indigenous_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('786e4a1b-08e4-4002-89eb-0f5931e148c3', inherent_indigenous_sovereignty, deontological).
narrative_ontology:cs_axiom('786e4a1b-08e4-4002-89eb-0f5931e148c3', foundational, territorial_stewardship_as_mutual_obligation).
narrative_ontology:cs_axiom_status(territorial_stewardship_as_mutual_obligation, holdable).
narrative_ontology:cs_axiom_grounding('786e4a1b-08e4-4002-89eb-0f5931e148c3', territorial_stewardship_as_mutual_obligation, deontological).
narrative_ontology:cs_reference_frame('786e4a1b-08e4-4002-89eb-0f5931e148c3', pre_colonial_relational_governance).
narrative_ontology:cs_drift_state('786e4a1b-08e4-4002-89eb-0f5931e148c3', contemporary_post_colonial_era, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('786e4a1b-08e4-4002-89eb-0f5931e148c3', '').
narrative_ontology:cs_kernel_id(historical_treaty_substrate__stewardship_reading, historical_treaty_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(historical_treaty_substrate__stewardship_reading, indigenous_nations).
narrative_ontology:constraint_beneficiary(historical_treaty_substrate__stewardship_reading, settler_state).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(historical_treaty_substrate__stewardship_reading, environmental_advocates).
narrative_ontology:constraint_victim(historical_treaty_substrate__stewardship_reading, resource_extraction_industries).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Retain inherent jurisdiction over their traditional territories, participate in shared governance, and benefit from sustainable resource management. Their ability to fully realize this reading is constrained by ongoing power imbalances.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__stewardship_reading, indigenous_nations, beneficiary,
    organized, generational, constrained, regional).

% Is obligated to seek consent, engage in shared governance, and uphold mutual obligations for coexistence. Benefits from enhanced legitimacy, reduced conflict, and improved environmental outcomes when adhering to this reading.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__stewardship_reading, settler_state, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(historical_treaty_substrate__stewardship_reading, settler_state, beneficiary).

% Must seek free, prior, and informed consent from Indigenous nations for projects on traditional territories and adhere to jointly developed stewardship plans. Bears the cost of shared decision-making and potentially reduced access to resources compared to unilateral extraction.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__stewardship_reading, resource_extraction_industries, payer,
    powerful, biographical, constrained, regional).

% Benefit from the ecological protection and sustainable practices inherent in Indigenous stewardship principles, which are central to this reading of treaties. They often align with Indigenous nations in advocating for this approach.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__stewardship_reading, environmental_advocates, beneficiary,
    organized, generational, mobile, global).

% Advocate for treaties as completed property transactions where Indigenous sovereignty was ceded. Their core premise is directly contradicted by this stewardship reading, leading to their exclusion from its framework, though they actively resist its implementation.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__stewardship_reading, extinguishment_proponents, excluded,
    powerful, biographical, mobile, national).

% Analyze the legal, historical, and practical implications of this reading within broader frameworks of international law, Indigenous legal traditions, and constitutional theory. They provide critical commentary and theoretical grounding.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__stewardship_reading, comparative_legal_scholars, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To coordinate shared governance, sustainable resource management, and peaceful coexistence between Indigenous nations and the settler state, based on mutual respect and non-cession of inherent Indigenous sovereignty.
% TRANSFER_FUNCTION: Transfers unilateral decision-making authority and resource control from the settler state to a model of shared governance with Indigenous nations, reallocating benefits and responsibilities for territorial stewardship.
% ABSENT_VOICES: Proponents of the extinguishment reading, who view treaties as completed land cessions, are structurally excluded from the framework of mutual obligation and shared stewardship that defines this reading. They would object to any interpretation that affirms ongoing Indigenous jurisdiction.
% DISAPPEARANCE_RATIONALE: If this reading vanished, the legal and political landscape regarding Indigenous rights and land use would revert to more extractive, unilateral settler state control, leading to increased conflict, environmental degradation, and a crisis of legitimacy for the settler state's claims to jurisdiction.
% FOUNDING_PROBLEM: Historical treaties were often negotiated under duress, with fundamental misunderstandings or deliberate misrepresentations regarding Indigenous sovereignty and land use, leading to ongoing conflicts over jurisdiction, resources, and the nature of Indigenous-settler relations.
% FOUNDING_PROBLEM_CORROBORATION: Indigenous legal traditions, numerous court decisions (e.g., in Canada and Australia), reports from international human rights bodies, and independent legal scholars consistently corroborate the ongoing nature of these conflicts and the need for a relational, stewardship-based approach to treaty interpretation.
narrative_ontology:disappearance_verdict(historical_treaty_substrate__stewardship_reading, world_rearranges).
narrative_ontology:founding_problem_status(historical_treaty_substrate__stewardship_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(historical_treaty_substrate__stewardship_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(historical_treaty_substrate__stewardship_reading, 'none', 1).
narrative_ontology:epsilon_provenance(historical_treaty_substrate__stewardship_reading, 0.25, 'gemini-2.5-flash', 'none', direct).

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
 *   The claimed type is 'rope' because this reading fundamentally posits a coordination mechanism for shared governance and mutual benefit. However, its 'actual operation' (as reflected in metrics) is heavily influenced by its contested status. Extractiveness (0.25) and suppression (0.15) are low, reflecting the *ideal* of this reading, where it aims to reduce unilateral extraction and coercion. However, resistance is high (0.7) because this reading challenges established power structures and is actively opposed by proponents of other interpretations. Theater ratio (0.45) is also significant, as this reading is often invoked performatively by settler states without full, consistent implementation, leading to a gap between rhetoric and practice. Accessibility collapse is low (0.3) because this reading actively opens alternatives to unilateral state action.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of Indigenous nations and environmental advocates, this reading represents a path toward justice and sustainability. From the perspective of resource extraction industries, it represents increased costs and reduced certainty. From the perspective of extinguishment proponents, it is an illegitimate reinterpretation of settled history. The engine's classification will highlight the tension between the claimed 'rope' and the high resistance and theater ratio, indicating a coordination mechanism that is heavily contested in practice.
 *
 * DIRECTIONALITY LOGIC:
 *   Indigenous nations are primary beneficiaries, as this reading affirms their inherent jurisdiction and right to shared governance. The settler state is also a beneficiary, gaining legitimacy and stability through adherence to mutual obligations. Resource extraction industries are payers, as they must now operate under shared consent and stewardship principles. Proponents of extinguishment are excluded, as their core premise is incompatible with this reading.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    historical_text_ambiguity,
    'To what extent do the original historical treaty texts unambiguously support or contradict the ''stewardship'' interpretation versus ''cession''?',
    'Comprehensive linguistic and historical analysis of original treaty documents, including Indigenous oral histories and legal traditions, alongside settler state records.',
    'Strong textual support for stewardship would strengthen its legal and moral authority, potentially reducing resistance. Ambiguity or contradiction would highlight the interpretive nature of the reading and the ongoing contest over historical narratives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(historical_text_ambiguity, empirical, 'Ambiguity of historical treaty texts regarding sovereignty and stewardship.').

omega_variable(
    implementation_sincerity_gap,
    'Is the settler state''s engagement with the ''stewardship reading'' a genuine commitment to shared governance, or a performative act to manage dissent while maintaining de facto control?',
    'Longitudinal analysis of policy outcomes, resource allocation, and actual shifts in decision-making power in specific treaty territories over time, assessed by independent Indigenous-led monitoring bodies.',
    'If performative, the effective extractiveness and suppression of the overall system remain high, despite the rhetoric of stewardship, pushing the classification towards a ''tangled_rope'' or ''snare'' in practice. If genuine, the ''rope'' classification is reinforced.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(implementation_sincerity_gap, empirical, 'Sincerity of settler state''s commitment to shared stewardship.').

omega_variable(
    coexistence_vs_foreclosure_nation_to_nation,
    'Does the ''stewardship_reading'' truly coexist with the ''nation_to_nation_reading'', or does its emphasis on relational pacts implicitly foreclose the full implications of international sovereign equality?',
    'Conceptual analysis of the legal and political implications of both readings when applied to specific cases of Indigenous self-determination and international relations, particularly regarding external sovereignty.',
    'If it implicitly forecloses, the ''stewardship_reading'' might be seen as a more palatable, but ultimately limiting, interpretation for settler states, potentially reducing its transformative power compared to a full ''nation_to_nation'' approach.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(coexistence_vs_foreclosure_nation_to_nation, conceptual, 'Conceptual relationship between stewardship and nation-to-nation readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(historical_treaty_substrate__stewardship_reading, 1800, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hist_tr_t1800, historical_treaty_substrate__stewardship_reading, theater_ratio, 1800, 0.3).
narrative_ontology:measurement(hist_tr_t1850, historical_treaty_substrate__stewardship_reading, theater_ratio, 1850, 0.35).
narrative_ontology:measurement(hist_tr_t1900, historical_treaty_substrate__stewardship_reading, theater_ratio, 1900, 0.4).
narrative_ontology:measurement(hist_tr_t1950, historical_treaty_substrate__stewardship_reading, theater_ratio, 1950, 0.42).
narrative_ontology:measurement(hist_tr_t2000, historical_treaty_substrate__stewardship_reading, theater_ratio, 2000, 0.44).
narrative_ontology:measurement(hist_tr_t2024, historical_treaty_substrate__stewardship_reading, theater_ratio, 2024, 0.45).

% Extraction over time
narrative_ontology:measurement(hist_be_t1800, historical_treaty_substrate__stewardship_reading, base_extractiveness, 1800, 0.35).
narrative_ontology:measurement(hist_be_t1850, historical_treaty_substrate__stewardship_reading, base_extractiveness, 1850, 0.3).
narrative_ontology:measurement(hist_be_t1900, historical_treaty_substrate__stewardship_reading, base_extractiveness, 1900, 0.28).
narrative_ontology:measurement(hist_be_t1950, historical_treaty_substrate__stewardship_reading, base_extractiveness, 1950, 0.27).
narrative_ontology:measurement(hist_be_t2000, historical_treaty_substrate__stewardship_reading, base_extractiveness, 2000, 0.26).
narrative_ontology:measurement(hist_be_t2024, historical_treaty_substrate__stewardship_reading, base_extractiveness, 2024, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(hist_su_t1800, historical_treaty_substrate__stewardship_reading, suppression_requirement, 1800, 0.25).
narrative_ontology:measurement(hist_su_t1850, historical_treaty_substrate__stewardship_reading, suppression_requirement, 1850, 0.2).
narrative_ontology:measurement(hist_su_t1900, historical_treaty_substrate__stewardship_reading, suppression_requirement, 1900, 0.18).
narrative_ontology:measurement(hist_su_t1950, historical_treaty_substrate__stewardship_reading, suppression_requirement, 1950, 0.17).
narrative_ontology:measurement(hist_su_t2000, historical_treaty_substrate__stewardship_reading, suppression_requirement, 2000, 0.16).
narrative_ontology:measurement(hist_su_t2024, historical_treaty_substrate__stewardship_reading, suppression_requirement, 2024, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(historical_treaty_substrate__stewardship_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(historical_treaty_substrate__stewardship_reading, historical_treaty_substrate__extinguishment_reading).
narrative_ontology:affects_constraint(historical_treaty_substrate__stewardship_reading, historical_treaty_substrate__nation_to_nation_reading).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
