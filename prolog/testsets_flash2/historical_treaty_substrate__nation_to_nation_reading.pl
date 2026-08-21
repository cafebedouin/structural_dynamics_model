% ============================================================================
% CONSTRAINT STORY: historical_treaty_substrate__nation_to_nation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_historical_treaty_substrate__nation_to_nation_reading, []).

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
 *   constraint_id: historical_treaty_substrate__nation_to_nation_reading
 *   human_readable: Historical Treaty Substrate: Nation-to-Nation Reading
 *   domain: legal_anthropology/indigenous_law/comparative_constitutional_theory
 *
 * SUMMARY:
 *   This constraint represents the 'nation-to-nation' reading of historical
 *   treaties, where they are understood as international agreements between
 *   sovereign equals, requiring ongoing consent and subject to modern treaty
 *   law principles. This reading contrasts sharply with colonial
 *   interpretations that view treaties as land surrender. The classification
 *   as 'rope' reflects the ideal of genuine coordination and mutual benefit,
 *   though the metrics acknowledge the historical and ongoing struggle to
 *   fully realize this ideal against extractive forces. This is one reading
 *   of the 'historical_treaty_substrate' kernel.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(historical_treaty_substrate__nation_to_nation_reading, 0.25).
domain_priors:suppression_score(historical_treaty_substrate__nation_to_nation_reading, 0.4).
domain_priors:theater_ratio(historical_treaty_substrate__nation_to_nation_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(historical_treaty_substrate__nation_to_nation_reading, extractiveness, 0.25).
narrative_ontology:constraint_metric(historical_treaty_substrate__nation_to_nation_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(historical_treaty_substrate__nation_to_nation_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(historical_treaty_substrate__nation_to_nation_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(historical_treaty_substrate__nation_to_nation_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(historical_treaty_substrate__nation_to_nation_reading, rope).
narrative_ontology:human_readable(historical_treaty_substrate__nation_to_nation_reading, "Historical Treaty Substrate: Nation-to-Nation Reading").
narrative_ontology:topic_domain(historical_treaty_substrate__nation_to_nation_reading, "legal_anthropology/indigenous_law/comparative_constitutional_theory").

domain_priors:requires_active_enforcement(historical_treaty_substrate__nation_to_nation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(historical_treaty_substrate__nation_to_nation_reading, 'f924c61f-0d4a-4bdb-9d05-3badcd9b8bf1').
narrative_ontology:cs_kernel_codification('f924c61f-0d4a-4bdb-9d05-3badcd9b8bf1', fixed_text).
narrative_ontology:cs_authority_grounding('f924c61f-0d4a-4bdb-9d05-3badcd9b8bf1', lineage).
narrative_ontology:cs_interpretation_layer_present('f924c61f-0d4a-4bdb-9d05-3badcd9b8bf1').
narrative_ontology:cs_reading_relation('f924c61f-0d4a-4bdb-9d05-3badcd9b8bf1', historical_treaty_substrate__extinguishment_reading, coexists_with).
narrative_ontology:cs_reading_relation('f924c61f-0d4a-4bdb-9d05-3badcd9b8bf1', historical_treaty_substrate__stewardship_reading, coexists_with).
narrative_ontology:cs_axiom('f924c61f-0d4a-4bdb-9d05-3badcd9b8bf1', foundational, indigenous_sovereignty_pre_exists_settlement).
narrative_ontology:cs_axiom_status(indigenous_sovereignty_pre_exists_settlement, holdable).
narrative_ontology:cs_axiom_grounding('f924c61f-0d4a-4bdb-9d05-3badcd9b8bf1', indigenous_sovereignty_pre_exists_settlement, deontological).
narrative_ontology:cs_axiom('f924c61f-0d4a-4bdb-9d05-3badcd9b8bf1', foundational, treaties_are_living_agreements).
narrative_ontology:cs_axiom_status(treaties_are_living_agreements, holdable).
narrative_ontology:cs_axiom_grounding('f924c61f-0d4a-4bdb-9d05-3badcd9b8bf1', treaties_are_living_agreements, conventional).
narrative_ontology:cs_reference_frame('f924c61f-0d4a-4bdb-9d05-3badcd9b8bf1', international_law_of_treaties).
narrative_ontology:cs_drift_state('f924c61f-0d4a-4bdb-9d05-3badcd9b8bf1', contemporary_legal_precedent, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('f924c61f-0d4a-4bdb-9d05-3badcd9b8bf1', '').
narrative_ontology:cs_kernel_id(historical_treaty_substrate__nation_to_nation_reading, historical_treaty_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(historical_treaty_substrate__nation_to_nation_reading, indigenous_nations).
narrative_ontology:constraint_beneficiary(historical_treaty_substrate__nation_to_nation_reading, settler_state_international_reputation).
narrative_ontology:constraint_victim(historical_treaty_substrate__nation_to_nation_reading, settler_state_unilateral_resource_extraction_interests).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(historical_treaty_substrate__nation_to_nation_reading, settler_state_citizens).
narrative_ontology:constraint_victim(historical_treaty_substrate__nation_to_nation_reading, settler_state_citizens).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% As co-equal sovereigns, Indigenous nations benefit from the recognition of their inherent rights and the requirement for their ongoing consent on matters affecting their traditional territories. Their identity is deeply tied to their land and treaty relationships, making 'exit' from the treaty relationship a non-option.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__nation_to_nation_reading, indigenous_nations, beneficiary,
    organized, generational, identity_locked, regional).

% The settler state, in this reading, is bound by international law and principles of ongoing consent. It benefits from enhanced international reputation and stability through respectful treaty relations, but is constrained in its ability to unilaterally exploit resources or assert absolute sovereignty over treaty lands.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__nation_to_nation_reading, settler_state_government, agenda_setter,
    institutional, generational, constrained, national).

% These interests (e.g., mining, forestry, oil and gas companies) bear the costs of requiring Indigenous consent, consultation, and revenue sharing. Their ability to operate without impediment is directly curtailed by this reading of treaty obligations.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__nation_to_nation_reading, settler_state_unilateral_resource_extraction_interests, payer,
    powerful, immediate, constrained, local).

% These bodies (e.g., UN, ICJ) observe and interpret international law, providing a framework that supports the nation-to-nation reading of treaties. They do not directly enforce but exert significant normative pressure.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__nation_to_nation_reading, international_legal_bodies, observer,
    institutional, civilizational, analytical, global).

% Citizens may bear indirect costs through resource development delays or increased regulatory burdens, but also benefit from a more just and stable society, and a state that upholds international human rights norms.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__nation_to_nation_reading, settler_state_citizens, payer,
    moderate, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(historical_treaty_substrate__nation_to_nation_reading, settler_state_citizens, beneficiary).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a framework for ongoing, respectful relations and shared governance between Indigenous nations and the settler state, ensuring mutual consent and adherence to international legal norms for territorial and resource management.
% TRANSFER_FUNCTION: Transfers decision-making power and resource benefits from unilateral settler state control to a shared governance model with Indigenous nations, in accordance with treaty principles and international law.
% ABSENT_VOICES: Colonial-era legal doctrines that asserted terra nullius or absolute Crown sovereignty are structurally excluded from this reading; their proponents would argue for a more limited interpretation of Indigenous rights.
% DISAPPEARANCE_RATIONALE: If this reading of treaties vanished, Indigenous nations would lose a key legal and political tool for asserting their rights, leading to increased conflict, legal challenges, and a breakdown of reconciliation efforts. The settler state would face severe international condemnation and internal instability.
% FOUNDING_PROBLEM: The historical problem was the need to establish peaceful coexistence and define relationships between Indigenous peoples and incoming European powers, often through formal agreements.
% FOUNDING_PROBLEM_CORROBORATION: Indigenous legal scholars, international human rights organizations, and a growing segment of the settler state's judiciary and public attest that the problem of establishing equitable nation-to-nation relations is still live and unresolved, requiring ongoing adherence to treaty principles.
narrative_ontology:disappearance_verdict(historical_treaty_substrate__nation_to_nation_reading, world_rearranges).
narrative_ontology:founding_problem_status(historical_treaty_substrate__nation_to_nation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(historical_treaty_substrate__nation_to_nation_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(historical_treaty_substrate__nation_to_nation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(historical_treaty_substrate__nation_to_nation_reading, 0.25, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(historical_treaty_substrate__nation_to_nation_reading_tests).
:- end_tests(historical_treaty_substrate__nation_to_nation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.25) is relatively low because this reading emphasizes mutual benefit and consent, reducing the unilateral extraction seen in other readings. Suppression (0.40) is moderate, reflecting the ongoing need for legal and political advocacy to uphold this interpretation against historical and contemporary pressures. Theater ratio (0.10) is low, as this reading seeks genuine implementation rather than performative gestures. The decreasing extractiveness and suppression over time reflect the growing legal recognition and advocacy for this interpretation since the 1970s.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of Indigenous nations, this reading is a vital mechanism for justice and self-determination. From the perspective of settler state resource interests, it represents an impediment to economic development. The settler state government navigates between these, balancing international obligations and domestic economic pressures.
 *
 * DIRECTIONALITY LOGIC:
 *   Indigenous nations are primary beneficiaries (d near 0.0) as this reading affirms their sovereignty and rights. The settler state's unilateral resource extraction interests are victims (d near 1.0) as their operations are constrained. The settler state government itself is an agenda-setter that benefits from international legitimacy but also bears costs of compliance, placing its d closer to symmetric. International legal bodies are observers, and settler state citizens are diffuse beneficiaries/payers.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legal_recognition_vs_political_will,
    'To what extent does the legal recognition of the nation-to-nation reading translate into actual political will and policy implementation by the settler state?',
    'Analysis of legislative changes, resource revenue sharing agreements, and the outcomes of consent processes over a 10-year period.',
    'If political will lags legal recognition, the effective extractiveness and suppression remain higher than the legal framework suggests, indicating a ''tangled rope'' or ''snare'' in practice despite the ''rope'' claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legal_recognition_vs_political_will, empirical, 'Gap between legal theory and political practice.').

omega_variable(
    international_law_enforceability,
    'How effectively can international legal bodies enforce the principles of modern treaty law on sovereign states that resist full implementation?',
    'Case studies of international legal interventions and their impact on state behavior regarding Indigenous treaty rights.',
    'If international law proves weakly enforceable, the ''nation-to-nation'' reading''s protective function is diminished, increasing the effective extractiveness for Indigenous nations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(international_law_enforceability, empirical, 'The practical enforceability of international treaty law.').

omega_variable(
    extinguishment_vs_nation_to_nation_framing,
    'Is the ''extinguishment_reading'' logically foreclosed by the ''nation_to_nation_reading'' within a single coherent legal framework, or do they merely coexist as competing interpretations?',
    'Analysis of a hypothetical supreme court ruling that explicitly rejects the legal basis of extinguishment in favor of nation-to-nation principles, and its subsequent impact on lower court decisions.',
    'If ''extinguishment'' is foreclosed, the ''nation-to-nation'' reading gains significant structural power, reducing suppression and extractiveness. If they merely coexist, the struggle for interpretation continues, maintaining higher suppression.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(extinguishment_vs_nation_to_nation_framing, conceptual, 'Whether the core premises of the extinguishment and nation-to-nation readings are mutually exclusive.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(historical_treaty_substrate__nation_to_nation_reading, 1970, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hist_tr_t1970, historical_treaty_substrate__nation_to_nation_reading, theater_ratio, 1970, 0.25).
narrative_ontology:measurement(hist_tr_t1985, historical_treaty_substrate__nation_to_nation_reading, theater_ratio, 1985, 0.2).
narrative_ontology:measurement(hist_tr_t2000, historical_treaty_substrate__nation_to_nation_reading, theater_ratio, 2000, 0.15).
narrative_ontology:measurement(hist_tr_t2010, historical_treaty_substrate__nation_to_nation_reading, theater_ratio, 2010, 0.12).
narrative_ontology:measurement(hist_tr_t2024, historical_treaty_substrate__nation_to_nation_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(hist_be_t1970, historical_treaty_substrate__nation_to_nation_reading, base_extractiveness, 1970, 0.45).
narrative_ontology:measurement(hist_be_t1985, historical_treaty_substrate__nation_to_nation_reading, base_extractiveness, 1985, 0.35).
narrative_ontology:measurement(hist_be_t2000, historical_treaty_substrate__nation_to_nation_reading, base_extractiveness, 2000, 0.3).
narrative_ontology:measurement(hist_be_t2010, historical_treaty_substrate__nation_to_nation_reading, base_extractiveness, 2010, 0.28).
narrative_ontology:measurement(hist_be_t2024, historical_treaty_substrate__nation_to_nation_reading, base_extractiveness, 2024, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(hist_su_t1970, historical_treaty_substrate__nation_to_nation_reading, suppression_requirement, 1970, 0.7).
narrative_ontology:measurement(hist_su_t1985, historical_treaty_substrate__nation_to_nation_reading, suppression_requirement, 1985, 0.6).
narrative_ontology:measurement(hist_su_t2000, historical_treaty_substrate__nation_to_nation_reading, suppression_requirement, 2000, 0.5).
narrative_ontology:measurement(hist_su_t2010, historical_treaty_substrate__nation_to_nation_reading, suppression_requirement, 2010, 0.45).
narrative_ontology:measurement(hist_su_t2024, historical_treaty_substrate__nation_to_nation_reading, suppression_requirement, 2024, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(historical_treaty_substrate__nation_to_nation_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(historical_treaty_substrate__nation_to_nation_reading, historical_treaty_substrate__extinguishment_reading).
narrative_ontology:affects_constraint(historical_treaty_substrate__nation_to_nation_reading, historical_treaty_substrate__stewardship_reading).
narrative_ontology:affects_constraint(historical_treaty_substrate__nation_to_nation_reading, indigenous_self_determination_framework).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'historical_treaty_substrate' kernel. It represents the 'nation-to-nation' interpretation, emphasizing treaties as agreements between sovereign equals. It influences and coexists with the 'extinguishment' and 'stewardship' readings, which offer different interpretations of treaty purpose and effect.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
