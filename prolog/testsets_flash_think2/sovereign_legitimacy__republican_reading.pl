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
 *   human_readable: Republican Reading of Popular Sovereignty and Delegated Consent
 *   domain: political_philosophy/constitutional_theory/legitimacy_studies
 *
 * SUMMARY:
 *   This constraint represents the republican reading of legitimate
 *   authority, where power flows upward from the people through delegated
 *   consent, grounded in popular sovereignty and social contract theory. It
 *   posits that government derives its just powers from the consent of the
 *   governed, typically expressed through elections and constitutional
 *   adherence. While aiming for broad coordination and self-governance, this
 *   reading acknowledges inherent vulnerabilities to majoritarian tyranny and
 *   the exclusion of certain groups from the mechanisms of consent, leading
 *   to an extractive component.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sovereign_legitimacy__republican_reading, 0.6).
domain_priors:suppression_score(sovereign_legitimacy__republican_reading, 0.7).
domain_priors:theater_ratio(sovereign_legitimacy__republican_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sovereign_legitimacy__republican_reading, extractiveness, 0.6).
narrative_ontology:constraint_metric(sovereign_legitimacy__republican_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(sovereign_legitimacy__republican_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sovereign_legitimacy__republican_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(sovereign_legitimacy__republican_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sovereign_legitimacy__republican_reading, tangled_rope).
narrative_ontology:human_readable(sovereign_legitimacy__republican_reading, "Republican Reading of Popular Sovereignty and Delegated Consent").
narrative_ontology:topic_domain(sovereign_legitimacy__republican_reading, "political_philosophy/constitutional_theory/legitimacy_studies").

domain_priors:requires_active_enforcement(sovereign_legitimacy__republican_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sovereign_legitimacy__republican_reading, '4b019b61-8893-44eb-ba66-eb6e8a1e938e').
narrative_ontology:cs_kernel_codification('4b019b61-8893-44eb-ba66-eb6e8a1e938e', formalized).
narrative_ontology:cs_authority_grounding('4b019b61-8893-44eb-ba66-eb6e8a1e938e', lineage).
narrative_ontology:cs_interpretation_layer_present('4b019b61-8893-44eb-ba66-eb6e8a1e938e').
narrative_ontology:cs_reading_relation('4b019b61-8893-44eb-ba66-eb6e8a1e938e', sovereign_legitimacy__monarchical_reading, forecloses).
narrative_ontology:cs_reading_relation('4b019b61-8893-44eb-ba66-eb6e8a1e938e', sovereign_legitimacy__constitutional_hybrid_reading, coexists_with).
narrative_ontology:cs_axiom('4b019b61-8893-44eb-ba66-eb6e8a1e938e', foundational, popular_sovereignty_is_foundational).
narrative_ontology:cs_axiom_status(popular_sovereignty_is_foundational, holdable).
narrative_ontology:cs_axiom_grounding('4b019b61-8893-44eb-ba66-eb6e8a1e938e', popular_sovereignty_is_foundational, deontological).
narrative_ontology:cs_axiom('4b019b61-8893-44eb-ba66-eb6e8a1e938e', foundational, delegated_consent_legitimizes_rule).
narrative_ontology:cs_axiom_status(delegated_consent_legitimizes_rule, holdable).
narrative_ontology:cs_axiom_grounding('4b019b61-8893-44eb-ba66-eb6e8a1e938e', delegated_consent_legitimizes_rule, conventional).
narrative_ontology:cs_reference_frame('4b019b61-8893-44eb-ba66-eb6e8a1e938e', enlightenment_social_contract).
narrative_ontology:cs_drift_state('4b019b61-8893-44eb-ba66-eb6e8a1e938e', contemporary_democratic_backsliding, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('4b019b61-8893-44eb-ba66-eb6e8a1e938e', '').
narrative_ontology:cs_kernel_id(sovereign_legitimacy__republican_reading, sovereign_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sovereign_legitimacy__republican_reading, citizenry_with_franchise).
narrative_ontology:constraint_beneficiary(sovereign_legitimacy__republican_reading, elected_representatives).
narrative_ontology:constraint_victim(sovereign_legitimacy__republican_reading, excluded_citizens).
narrative_ontology:constraint_victim(sovereign_legitimacy__republican_reading, minority_factions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Participates in elections, delegates authority, and is theoretically the ultimate source of legitimacy. Benefits from self-governance and representation but can be subject to majoritarian decisions that may not align with individual or minority interests.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__republican_reading, citizenry_with_franchise, beneficiary,
    organized, biographical, constrained, national).

% Exercise delegated authority, enact laws, and administer the state. They are accountable to the citizenry through electoral cycles and constitutional mechanisms, but also wield significant power in shaping policy and governance.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__republican_reading, elected_representatives, agenda_setter,
    institutional, immediate, constrained, national).

% Those denied voting rights or meaningful participation in the consent mechanisms (e.g., non-citizens, historical disenfranchised groups). They bear the costs of governance and are subject to laws without direct representation or a voice in the delegation of authority.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__republican_reading, excluded_citizens, payer,
    powerless, biographical, trapped, national).

% Groups whose interests are consistently outvoted or marginalized by the majority. While nominally part of the citizenry, their effective consent is often overridden, leading to a sense of extraction and vulnerability to majoritarian tyranny.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__republican_reading, minority_factions, payer,
    moderate, biographical, constrained, national).

% Advocates for alternative forms of authority (e.g., inherited monarchy or divine right) whose claims are structurally excluded and delegitimized by the republican framework. They are outside the conversation of legitimate governance.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__republican_reading, monarchical_claimants, excluded,
    powerful, generational, identity_locked, national).

% Analyze the theoretical underpinnings, historical evolution, and practical application of popular sovereignty and delegated consent. They critique its successes and failures, and propose reforms, but do not directly participate in its operation.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__republican_reading, constitutional_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(sovereign_legitimacy__republican_reading, elected_representatives).
narrative_ontology:fixing_cost_class(sovereign_legitimacy__republican_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a stable and legitimate framework for collective decision-making and governance by channeling popular will through representative institutions, preventing both arbitrary rule and chaotic direct democracy.
% TRANSFER_FUNCTION: Transfers political authority and the right to govern from the collective 'people' to elected representatives, in exchange for accountability, public service, and the promise of self-governance.
% ABSENT_VOICES: Monarchical claimants and those advocating for non-democratic forms of authority are structurally excluded. Also, future generations who cannot directly consent to current constitutional arrangements, and non-human entities affected by governance decisions.
% DISAPPEARANCE_RATIONALE: If the principle of popular sovereignty and delegated consent vanished, the entire edifice of modern republican governance would collapse. Authority would become arbitrary or revert to other forms (e.g., inherited, divine right, or pure force), leading to widespread political instability and a complete reordering of state structures.
% FOUNDING_PROBLEM: To establish a stable and just form of government that derives its authority from the governed, avoiding both arbitrary rule by a monarch or elite, and the potential for mob rule or instability of pure direct democracy.
% FOUNDING_PROBLEM_CORROBORATION: Political theorists, historians, and constitutional scholars widely corroborate the historical problem of legitimate governance and the republican solution. Ongoing debates about democratic deficits, voter suppression, and representation attest to the problem's continued relevance, even if the solution is imperfect.
narrative_ontology:disappearance_verdict(sovereign_legitimacy__republican_reading, world_rearranges).
narrative_ontology:founding_problem_status(sovereign_legitimacy__republican_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sovereign_legitimacy__republican_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(sovereign_legitimacy__republican_reading, 'none', 1).
narrative_ontology:epsilon_provenance(sovereign_legitimacy__republican_reading, 0.6, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sovereign_legitimacy__republican_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(sovereign_legitimacy__republican_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(sovereign_legitimacy__republican_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The claimed type is 'tangled_rope' because while the ideal of popular sovereignty aims for broad coordination (beneficiaries: citizenry, elected representatives), the practical implementation often involves asymmetric extraction from those excluded from the franchise or marginalized by majoritarian rule (victims: excluded citizens, minority factions). Extractiveness (0.6) is substantial due to historical and ongoing disenfranchisement and the vulnerability of minorities. Suppression (0.7) is high because maintaining the system requires actively suppressing alternative forms of authority and managing dissent from excluded groups. Theater ratio (0.25) is moderate, reflecting that while elections and constitutional processes are functional, they can also become performative, masking deeper structural inequalities or a lack of genuine consent for all.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the citizenry with franchise, the constraint is largely a 'rope' or even a 'mountain' of self-evident truth, providing legitimate governance. From the perspective of excluded citizens or minority factions, it operates more like a 'snare' or 'tangled_rope,' extracting compliance and resources without genuine consent or representation. The engine's computation of per-seat classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The citizenry with franchise and elected representatives are beneficiaries, as they participate in and benefit from the system of self-governance. Excluded citizens and minority factions are victims, bearing the costs of governance without full participation or protection from majoritarian decisions. Monarchical claimants are structurally excluded, their very existence challenging the foundational premise of the constraint.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    scope_of_the_people,
    'Who constitutes ''the people'' from whom legitimate authority flows, and is this definition fixed or historically contingent?',
    'Historical analysis of franchise expansion/contraction, and contemporary legal/philosophical debates on citizenship and political inclusion.',
    'If ''the people'' is a historically contingent and exclusionary construct, the constraint''s effective extraction from those excluded is higher than currently measured, potentially reclassifying it closer to a snare for those seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scope_of_the_people, conceptual, 'Ambiguity in the definition of the sovereign ''people''.').

omega_variable(
    consent_authenticity,
    'To what extent is ''delegated consent'' a genuine expression of popular will, versus a manufactured or coerced outcome of political processes?',
    'Empirical studies of voter suppression, gerrymandering, campaign finance, and media influence on electoral outcomes. Analysis of the gap between public opinion and policy.',
    'If consent is substantially manufactured or coerced, the constraint''s coordination function is diminished, and its extractiveness and suppression are higher, pushing it further towards a snare or more extractive tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consent_authenticity, empirical, 'Authenticity of delegated consent.').

omega_variable(
    majoritarian_tyranny_threshold,
    'At what point does majoritarian rule, even if procedurally legitimate, become ''tyranny of the majority'' for minority factions, undermining the constraint''s claim to justice?',
    'Normative philosophical debate, legal precedent on minority rights, and empirical studies of the impact of majoritarian policies on vulnerable groups.',
    'A lower threshold for ''tyranny'' would increase the perceived extraction for minority seats, potentially shifting their classification towards snare, even if the overall system remains a tangled_rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(majoritarian_tyranny_threshold, preference, 'Defining the boundary of legitimate majoritarian rule.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sovereign_legitimacy__republican_reading, 1789, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sove_tr_t1789, sovereign_legitimacy__republican_reading, theater_ratio, 1789, 0.1).
narrative_ontology:measurement(sove_tr_t1850, sovereign_legitimacy__republican_reading, theater_ratio, 1850, 0.15).
narrative_ontology:measurement(sove_tr_t1900, sovereign_legitimacy__republican_reading, theater_ratio, 1900, 0.2).
narrative_ontology:measurement(sove_tr_t1950, sovereign_legitimacy__republican_reading, theater_ratio, 1950, 0.22).
narrative_ontology:measurement(sove_tr_t2000, sovereign_legitimacy__republican_reading, theater_ratio, 2000, 0.24).
narrative_ontology:measurement(sove_tr_t2024, sovereign_legitimacy__republican_reading, theater_ratio, 2024, 0.25).

% Extraction over time
narrative_ontology:measurement(sove_be_t1789, sovereign_legitimacy__republican_reading, base_extractiveness, 1789, 0.4).
narrative_ontology:measurement(sove_be_t1850, sovereign_legitimacy__republican_reading, base_extractiveness, 1850, 0.45).
narrative_ontology:measurement(sove_be_t1900, sovereign_legitimacy__republican_reading, base_extractiveness, 1900, 0.5).
narrative_ontology:measurement(sove_be_t1950, sovereign_legitimacy__republican_reading, base_extractiveness, 1950, 0.55).
narrative_ontology:measurement(sove_be_t2000, sovereign_legitimacy__republican_reading, base_extractiveness, 2000, 0.58).
narrative_ontology:measurement(sove_be_t2024, sovereign_legitimacy__republican_reading, base_extractiveness, 2024, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(sove_su_t1789, sovereign_legitimacy__republican_reading, suppression_requirement, 1789, 0.5).
narrative_ontology:measurement(sove_su_t1850, sovereign_legitimacy__republican_reading, suppression_requirement, 1850, 0.58).
narrative_ontology:measurement(sove_su_t1900, sovereign_legitimacy__republican_reading, suppression_requirement, 1900, 0.65).
narrative_ontology:measurement(sove_su_t1950, sovereign_legitimacy__republican_reading, suppression_requirement, 1950, 0.68).
narrative_ontology:measurement(sove_su_t2000, sovereign_legitimacy__republican_reading, suppression_requirement, 2000, 0.69).
narrative_ontology:measurement(sove_su_t2024, sovereign_legitimacy__republican_reading, suppression_requirement, 2024, 0.7).


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
