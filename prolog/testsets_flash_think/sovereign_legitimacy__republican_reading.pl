% ============================================================================
% CONSTRAINT STORY: sovereign_legitimacy__republican_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sovereign_legitimacy__republican_reading, []).

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
 *   constraint_id: sovereign_legitimacy__republican_reading
 *   human_readable: Republican Principle of Popular Sovereignty
 *   domain: Political Philosophy / Constitutional Theory / Legitimacy Studies
 *
 * SUMMARY:
 *   This constraint instantiates the republican reading of legitimate
 *   authority, asserting that power flows upward from the people through
 *   delegated consent, grounded in popular sovereignty and social contract
 *   theory. It is a foundational principle for modern democratic states,
 *   providing a framework for governance and accountability. However, its
 *   implementation often involves inherent tensions, particularly regarding
 *   the definition of 'the people' and the potential for majoritarian tyranny
 *   over minority or excluded groups.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sovereign_legitimacy__republican_reading, 0.45).
domain_priors:suppression_score(sovereign_legitimacy__republican_reading, 0.35).
domain_priors:theater_ratio(sovereign_legitimacy__republican_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sovereign_legitimacy__republican_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(sovereign_legitimacy__republican_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(sovereign_legitimacy__republican_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sovereign_legitimacy__republican_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(sovereign_legitimacy__republican_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sovereign_legitimacy__republican_reading, tangled_rope).
narrative_ontology:human_readable(sovereign_legitimacy__republican_reading, "Republican Principle of Popular Sovereignty").
narrative_ontology:topic_domain(sovereign_legitimacy__republican_reading, "Political Philosophy / Constitutional Theory / Legitimacy Studies").

domain_priors:requires_active_enforcement(sovereign_legitimacy__republican_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sovereign_legitimacy__republican_reading, '38804451-ce39-4c99-973d-67883c3026ed').
narrative_ontology:cs_kernel_codification('38804451-ce39-4c99-973d-67883c3026ed', formalized).
narrative_ontology:cs_authority_grounding('38804451-ce39-4c99-973d-67883c3026ed', practice).
narrative_ontology:cs_interpretation_layer_present('38804451-ce39-4c99-973d-67883c3026ed').
narrative_ontology:cs_reading_relation('38804451-ce39-4c99-973d-67883c3026ed', sovereign_legitimacy__monarchical_reading, forecloses).
narrative_ontology:cs_reading_relation('38804451-ce39-4c99-973d-67883c3026ed', sovereign_legitimacy__constitutional_hybrid_reading, coexists_with).
narrative_ontology:cs_axiom('38804451-ce39-4c99-973d-67883c3026ed', foundational, popular_sovereignty_is_foundational).
narrative_ontology:cs_axiom_status(popular_sovereignty_is_foundational, holdable).
narrative_ontology:cs_axiom_grounding('38804451-ce39-4c99-973d-67883c3026ed', popular_sovereignty_is_foundational, deontological).
narrative_ontology:cs_axiom('38804451-ce39-4c99-973d-67883c3026ed', foundational, consent_of_governed_is_legitimacy_source).
narrative_ontology:cs_axiom_status(consent_of_governed_is_legitimacy_source, holdable).
narrative_ontology:cs_axiom_grounding('38804451-ce39-4c99-973d-67883c3026ed', consent_of_governed_is_legitimacy_source, conventional).
narrative_ontology:cs_reference_frame('38804451-ce39-4c99-973d-67883c3026ed', enlightenment_social_contract).
narrative_ontology:cs_drift_state('38804451-ce39-4c99-973d-67883c3026ed', contemporary_democratic_backsliding, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('38804451-ce39-4c99-973d-67883c3026ed', '').
narrative_ontology:cs_kernel_id(sovereign_legitimacy__republican_reading, sovereign_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sovereign_legitimacy__republican_reading, citizenry_with_voting_rights).
narrative_ontology:constraint_beneficiary(sovereign_legitimacy__republican_reading, elected_representatives).
narrative_ontology:constraint_victim(sovereign_legitimacy__republican_reading, excluded_factions).
narrative_ontology:constraint_victim(sovereign_legitimacy__republican_reading, non_citizens_without_franchise).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The primary source of legitimate authority, delegating consent through elections and participation. Benefits from self-governance and accountability, but can be subject to majoritarian decisions.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__republican_reading, citizenry_with_voting_rights, beneficiary,
    organized, biographical, constrained, national).

% Exercise delegated authority on behalf of the people, enacting laws and governing. They benefit from the legitimacy conferred by popular consent but are accountable to the electorate.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__republican_reading, elected_representatives, agenda_setter,
    institutional, biographical, constrained, national).

% Groups whose interests or identities are marginalized by the majority, experiencing the constraint as a form of majoritarian tyranny or systemic disadvantage despite the rhetoric of popular sovereignty. Their consent is not effectively represented.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__republican_reading, excluded_factions, payer,
    powerless, immediate, trapped, national).

% Individuals residing within the state's jurisdiction but denied voting rights or full political participation, thus subject to laws without direct consent. Their situation highlights the limits of 'the people' in popular sovereignty.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__republican_reading, non_citizens_without_franchise, payer,
    powerless, generational, identity_locked, national).

% Those who believe in alternative forms of legitimate authority (e.g., inherited monarchy) and are structurally excluded from the republican framework's definition of legitimate power. Their views are not considered valid within the dominant paradigm.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__republican_reading, monarchical_advocates, excluded,
    powerless, generational, constrained, national).

% Academics and scholars who analyze the theoretical underpinnings, historical evolution, and practical challenges of popular sovereignty and delegated consent. They observe and critique the constraint's operation.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__republican_reading, constitutional_theorists, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(sovereign_legitimacy__republican_reading, elected_representatives).
narrative_ontology:fixing_cost_class(sovereign_legitimacy__republican_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To establish a stable and legitimate system of collective self-governance where authority is derived from the consent of the governed, enabling a diverse populace to act as a unified political body.
% TRANSFER_FUNCTION: Transfers political authority and decision-making power from individual citizens to a body of elected representatives, who are then entrusted with governing on their behalf.
% ABSENT_VOICES: Monarchical advocates, those advocating for non-democratic forms of governance, and those permanently excluded from the franchise (e.g., non-citizens, certain felons) would object to the foundational premises or their application, arguing for alternative sources of legitimacy or broader inclusion.
% DISAPPEARANCE_RATIONALE: If the principle of legitimate authority flowing upward from the people through delegated consent vanished, the entire edifice of modern democratic states would lose its foundational justification, leading to widespread political instability, challenges to existing governments, and a scramble for new sources of legitimacy.
% FOUNDING_PROBLEM: To establish a stable, just, and legitimate government that avoids tyranny and ensures the consent of the governed, following the decline of monarchical and divine right theories of authority.
% FOUNDING_PROBLEM_CORROBORATION: Political scientists, historians, and international human rights organizations corroborate the ongoing relevance of these challenges, citing democratic backsliding, struggles for self-determination, and debates over inclusion and representation globally. Independent analyses confirm the persistent need to validate and defend the mechanisms of popular consent.
narrative_ontology:disappearance_verdict(sovereign_legitimacy__republican_reading, world_rearranges).
narrative_ontology:founding_problem_status(sovereign_legitimacy__republican_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sovereign_legitimacy__republican_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(sovereign_legitimacy__republican_reading, 'none', 1).
narrative_ontology:epsilon_provenance(sovereign_legitimacy__republican_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sovereign_legitimacy__republican_reading_tests).
:- end_tests(sovereign_legitimacy__republican_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Tangled Rope because it genuinely coordinates collective self-governance (benefiting citizens with voting rights) but also contains mechanisms for asymmetric extraction (from excluded factions and non-citizens). Extractiveness (0.45) reflects the ongoing costs borne by those marginalized by majoritarian decisions or denied full participation. Suppression (0.35) is present in the active exclusion of alternative legitimacy claims and the maintenance of franchise boundaries. Theater ratio is low (0.15) as the mechanisms of consent and representation are generally functional, though their authenticity can be debated.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the citizenry with voting rights, this constraint often appears as a Rope, a pure coordination mechanism for self-governance. However, from the perspective of excluded factions or non-citizens, it can operate as a Snare, extracting compliance and resources without genuine consent or representation. The engine's per-seat classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The citizenry with voting rights and their elected representatives are the primary beneficiaries, gaining legitimate authority and the ability to self-govern. Excluded factions and non-citizens are the victims, subject to laws and decisions without full participation or effective recourse, leading to higher directionality values for these groups.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate to ensure legitimate governance through consent remains live, preventing it from becoming a Piton. However, the persistent challenges of defining 'the people' and mitigating majoritarian tyranny mean it cannot be a pure Rope. The ongoing contestation over its scope and application prevents mandatrophy, but also highlights its extractive potential.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    majoritarian_tyranny_inherence,
    'Is the extraction from excluded groups an inherent, unavoidable feature of popular sovereignty (a structural cost), or a contingent failure of its implementation (a remediable defect)?',
    'Comparative analysis of republican systems with different constitutional protections for minorities and different definitions of citizenship: if systems with stronger protections show significantly lower extraction from minorities, it suggests remediability.',
    'If inherent, the constraint''s base extractiveness is a necessary cost of coordination; if contingent, it represents remediable rent-seeking or structural injustice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(majoritarian_tyranny_inherence, conceptual, 'Whether majoritarian extraction is an inherent or contingent feature of republican legitimacy.').

omega_variable(
    consent_mechanism_authenticity,
    'How authentic and uncoerced is the ''delegated consent'' given factors like voter apathy, campaign finance influence, and media manipulation?',
    'Empirical studies on voter turnout, public trust in institutions, and the impact of money in politics, combined with philosophical analysis of what constitutes genuine consent in complex societies.',
    'If consent is substantially compromised, the coordination function is weakened, and the constraint''s effective extractiveness increases, potentially shifting its classification closer to a Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consent_mechanism_authenticity, empirical, 'The degree to which delegated consent is genuinely free and informed.').

omega_variable(
    kernel_framing_ambiguity,
    'Is this constraint''s framing as ''republican_reading'' the most appropriate, or would an alternative framing (e.g., ''constitutional_hybrid_reading'') better capture its structural dynamics?',
    'Analysis of the dominant legal and political discourse in a given state: if a state''s foundational documents and judicial interpretations consistently emphasize dual sources of authority (e.g., inherited crown + delegated parliament), the ''constitutional_hybrid_reading'' might be more apt.',
    'Adopting a different kernel reading would alter the declared axioms, reading relations, and potentially the base metrics, leading to a different classification and analysis of legitimacy.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_framing_ambiguity, conceptual, 'Ambiguity in the primary framing of legitimate authority.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sovereign_legitimacy__republican_reading, 1789, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sove_tr_t1789, sovereign_legitimacy__republican_reading, theater_ratio, 1789, 0.1).
narrative_ontology:measurement(sove_tr_t1850, sovereign_legitimacy__republican_reading, theater_ratio, 1850, 0.12).
narrative_ontology:measurement(sove_tr_t1900, sovereign_legitimacy__republican_reading, theater_ratio, 1900, 0.15).
narrative_ontology:measurement(sove_tr_t1950, sovereign_legitimacy__republican_reading, theater_ratio, 1950, 0.13).
narrative_ontology:measurement(sove_tr_t2000, sovereign_legitimacy__republican_reading, theater_ratio, 2000, 0.14).
narrative_ontology:measurement(sove_tr_t2024, sovereign_legitimacy__republican_reading, theater_ratio, 2024, 0.15).

% Extraction over time
narrative_ontology:measurement(sove_be_t1789, sovereign_legitimacy__republican_reading, base_extractiveness, 1789, 0.3).
narrative_ontology:measurement(sove_be_t1850, sovereign_legitimacy__republican_reading, base_extractiveness, 1850, 0.38).
narrative_ontology:measurement(sove_be_t1900, sovereign_legitimacy__republican_reading, base_extractiveness, 1900, 0.42).
narrative_ontology:measurement(sove_be_t1950, sovereign_legitimacy__republican_reading, base_extractiveness, 1950, 0.4).
narrative_ontology:measurement(sove_be_t2000, sovereign_legitimacy__republican_reading, base_extractiveness, 2000, 0.43).
narrative_ontology:measurement(sove_be_t2024, sovereign_legitimacy__republican_reading, base_extractiveness, 2024, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(sove_su_t1789, sovereign_legitimacy__republican_reading, suppression_requirement, 1789, 0.25).
narrative_ontology:measurement(sove_su_t1850, sovereign_legitimacy__republican_reading, suppression_requirement, 1850, 0.3).
narrative_ontology:measurement(sove_su_t1900, sovereign_legitimacy__republican_reading, suppression_requirement, 1900, 0.32).
narrative_ontology:measurement(sove_su_t1950, sovereign_legitimacy__republican_reading, suppression_requirement, 1950, 0.3).
narrative_ontology:measurement(sove_su_t2000, sovereign_legitimacy__republican_reading, suppression_requirement, 2000, 0.33).
narrative_ontology:measurement(sove_su_t2024, sovereign_legitimacy__republican_reading, suppression_requirement, 2024, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sovereign_legitimacy__republican_reading, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
