% ============================================================================
% CONSTRAINT STORY: territorial_legitimacy_dual__zionist_refuge_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_territorial_legitimacy_dual__zionist_refuge_reading, []).

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
 *   constraint_id: territorial_legitimacy_dual__zionist_refuge_reading
 *   human_readable: Zionist Refuge Reading of Israeli Territorial Legitimacy
 *   domain: political_theory/international_relations/territorial_sovereignty
 *
 * SUMMARY:
 *   This constraint represents the 'Zionist Refuge' reading of Israeli
 *   territorial legitimacy, which grounds the state's right to exist and
 *   control territory in historical Jewish persecution, divine promise, and
 *   UN partition acceptance. This reading frames 1948 legitimacy as
 *   uncontested, 1967 boundaries as negotiable based on security, and
 *   Palestinian displacement as a consequence of Arab rejection of partition.
 *   Security concerns are paramount in justifying territorial control. This
 *   is one reading of the 'territorial_legitimacy_dual' kernel; other
 *   readings exist and are modeled as separate constraints.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(territorial_legitimacy_dual__zionist_refuge_reading, 0.65).
domain_priors:suppression_score(territorial_legitimacy_dual__zionist_refuge_reading, 0.78).
domain_priors:theater_ratio(territorial_legitimacy_dual__zionist_refuge_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(territorial_legitimacy_dual__zionist_refuge_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(territorial_legitimacy_dual__zionist_refuge_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(territorial_legitimacy_dual__zionist_refuge_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(territorial_legitimacy_dual__zionist_refuge_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(territorial_legitimacy_dual__zionist_refuge_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(territorial_legitimacy_dual__zionist_refuge_reading, tangled_rope).
narrative_ontology:human_readable(territorial_legitimacy_dual__zionist_refuge_reading, "Zionist Refuge Reading of Israeli Territorial Legitimacy").
narrative_ontology:topic_domain(territorial_legitimacy_dual__zionist_refuge_reading, "political_theory/international_relations/territorial_sovereignty").

domain_priors:requires_active_enforcement(territorial_legitimacy_dual__zionist_refuge_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(territorial_legitimacy_dual__zionist_refuge_reading, 'ad25735f-9e6c-4287-a937-1af1354627c8').
narrative_ontology:cs_kernel_codification('ad25735f-9e6c-4287-a937-1af1354627c8', formalized).
narrative_ontology:cs_authority_grounding('ad25735f-9e6c-4287-a937-1af1354627c8', lineage).
narrative_ontology:cs_interpretation_layer_present('ad25735f-9e6c-4287-a937-1af1354627c8').
narrative_ontology:cs_reading_relation('ad25735f-9e6c-4287-a937-1af1354627c8', territorial_legitimacy_dual__palestinian_autochthony_reading, coexists_with).
narrative_ontology:cs_reading_relation('ad25735f-9e6c-4287-a937-1af1354627c8', territorial_legitimacy_dual__two_state_coexistence_reading, coexists_with).
narrative_ontology:cs_axiom('ad25735f-9e6c-4287-a937-1af1354627c8', foundational, jewish_self_determination_in_ancestral_homeland).
narrative_ontology:cs_axiom_status(jewish_self_determination_in_ancestral_homeland, holdable).
narrative_ontology:cs_axiom_grounding('ad25735f-9e6c-4287-a937-1af1354627c8', jewish_self_determination_in_ancestral_homeland, deontological).
narrative_ontology:cs_axiom('ad25735f-9e6c-4287-a937-1af1354627c8', foundational, security_imperative_justifies_territorial_control).
narrative_ontology:cs_axiom_status(security_imperative_justifies_territorial_control, holdable).
narrative_ontology:cs_axiom_grounding('ad25735f-9e6c-4287-a937-1af1354627c8', security_imperative_justifies_territorial_control, instrumental).
narrative_ontology:cs_reference_frame('ad25735f-9e6c-4287-a937-1af1354627c8', zionist_founding_principles).
narrative_ontology:cs_drift_state('ad25735f-9e6c-4287-a937-1af1354627c8', contemporary, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('ad25735f-9e6c-4287-a937-1af1354627c8', '').
narrative_ontology:cs_kernel_id(territorial_legitimacy_dual__zionist_refuge_reading, territorial_legitimacy_dual).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(territorial_legitimacy_dual__zionist_refuge_reading, state_of_israel).
narrative_ontology:constraint_beneficiary(territorial_legitimacy_dual__zionist_refuge_reading, israeli_citizens).
narrative_ontology:constraint_victim(territorial_legitimacy_dual__zionist_refuge_reading, palestinian_refugees).
narrative_ontology:constraint_victim(territorial_legitimacy_dual__zionist_refuge_reading, palestinian_residents_occupied_territories).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Asserts its right to exist and defend its borders based on historical Jewish connection to the land, divine promise, and UN Resolution 181. Actively enforces control over territory, including areas beyond 1948 borders, citing security imperatives. Benefits from the continued recognition of its sovereignty and the ability to define its own security needs.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__zionist_refuge_reading, state_of_israel, agenda_setter,
    institutional, generational, constrained, national).

% Benefit from the security and national identity provided by the state. Their lives are structured around the existence and defense of Israel within its asserted borders. They bear the costs of ongoing conflict but perceive the state's existence as a necessary refuge.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__zionist_refuge_reading, israeli_citizens, beneficiary,
    organized, biographical, constrained, national).

% Displaced from their homes in 1948 and subsequent conflicts, they are denied the right of return to what is now Israel. Their existence as refugees is a direct consequence of the territorial claims and historical narrative asserted by this reading. They bear the cost of statelessness and loss of property.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__zionist_refuge_reading, palestinian_refugees, payer,
    powerless, generational, trapped, regional).

% Live under Israeli military occupation or administrative control, with restricted movement, land access, and political rights. Their daily lives are heavily constrained by security measures and settlement expansion justified by this reading's framework. Their identity is deeply tied to their land and resistance.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__zionist_refuge_reading, palestinian_residents_occupied_territories, payer,
    powerless, biographical, identity_locked, local).

% Observes the conflict, often attempting to mediate or enforce international law. Its positions are often divided, with some states supporting Israel's security claims and others advocating for Palestinian rights. It bears the diffuse costs of regional instability and humanitarian crises.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__zionist_refuge_reading, international_community, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a secure national homeland for the Jewish people, coordinating their historical, religious, and cultural identity with a territorial state, and offering refuge from persecution.
% TRANSFER_FUNCTION: Transfers territorial control, resources, and security guarantees to the State of Israel and its citizens, from Palestinian populations who are displaced or live under occupation.
% ABSENT_VOICES: Palestinian voices advocating for the right of return, self-determination, and an end to occupation are systematically marginalized or excluded from the core discourse of this reading, which frames their displacement as a consequence of historical conflict and Arab rejection of partition.
% DISAPPEARANCE_RATIONALE: If this reading of legitimacy vanished, the foundational justification for Israel's current territorial control and security policies would collapse. This would necessitate a radical re-evaluation of borders, refugee status, and the rights of all inhabitants, leading to a profound rearrangement of the political and social landscape.
% FOUNDING_PROBLEM: The historical persecution of Jewish people, culminating in the Holocaust, and the absence of a secure national homeland.
% FOUNDING_PROBLEM_CORROBORATION: The State of Israel and its citizens attest that the founding problem of Jewish insecurity and the need for a refuge remains live, citing ongoing threats. International Jewish organizations and some allied states corroborate this, emphasizing the historical context and the need for a safe haven. Palestinian and Arab states, however, contest this framing, arguing that the solution to one historical injustice should not create another.
narrative_ontology:disappearance_verdict(territorial_legitimacy_dual__zionist_refuge_reading, world_rearranges).
narrative_ontology:founding_problem_status(territorial_legitimacy_dual__zionist_refuge_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(territorial_legitimacy_dual__zionist_refuge_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(territorial_legitimacy_dual__zionist_refuge_reading, 'none', 1).
narrative_ontology:epsilon_provenance(territorial_legitimacy_dual__zionist_refuge_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(territorial_legitimacy_dual__zionist_refuge_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(territorial_legitimacy_dual__zionist_refuge_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(territorial_legitimacy_dual__zionist_refuge_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.65) due to the significant territorial and resource control asserted over land also claimed by Palestinians, leading to displacement and restricted rights. Suppression is also high (0.78) as this reading requires active military and administrative enforcement to maintain its territorial claims and manage Palestinian resistance. Theater ratio is moderate (0.20): while security concerns are genuine, a portion of the enforcement activity serves to maintain and expand territorial control beyond immediate defensive needs, rather than purely for coordination. The metrics reflect the ongoing, active nature of this constraint.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the State of Israel and its citizens, this constraint is a necessary 'Rope' or even 'Mountain' for survival and self-determination. From the perspective of Palestinians, it operates as a 'Snare' or 'Tangled Rope' of dispossession and occupation. The engine's classification will reflect this divergence based on the structural data provided for each seat.
 *
 * DIRECTIONALITY LOGIC:
 *   The State of Israel and its citizens are primary beneficiaries, gaining security, national identity, and territorial control. Palestinian refugees and residents of occupied territories are clear targets/victims, bearing the costs of displacement, loss of land, and restricted freedoms. The international community acts as an observer, with varied positions and diffuse costs.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    historical_narrative_contestation,
    'To what extent is the historical narrative of ''divine promise'' and ''UN partition acceptance'' universally accepted as the sole basis for legitimacy, versus being contested by alternative historical accounts?',
    'Analysis of international legal consensus, historical scholarship from diverse perspectives, and public opinion surveys across affected populations.',
    'If the narrative is widely contested, the ''naturalness'' claim of this reading weakens, increasing its perceived extractiveness and suppression. If widely accepted, its legitimacy as a coordination mechanism is strengthened.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(historical_narrative_contestation, conceptual, 'Ambiguity regarding the universal acceptance of the historical narrative grounding this reading''s legitimacy.').

omega_variable(
    security_vs_expansion_motivation,
    'What proportion of territorial control and enforcement actions are genuinely driven by defensive security needs, versus being motivated by ideological expansion or resource acquisition?',
    'Independent military and geopolitical analysis, declassified government documents, and long-term trends in settlement expansion relative to security threats.',
    'If a significant portion is found to be expansionist, the ''coordination'' function of this reading diminishes, and its ''extraction'' and ''suppression'' metrics would be re-evaluated upwards, potentially shifting its classification towards a Snare. If purely defensive, its coordination function is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(security_vs_expansion_motivation, empirical, 'Distinguishing between security-driven and expansion-driven territorial control.').

omega_variable(
    mandatrophy_of_refuge_claim,
    'Has the ''refuge from persecution'' mandate, central to this reading, atrophied or been superseded by other motivations for territorial control?',
    'Analysis of policy statements, public discourse, and resource allocation over time, comparing emphasis on refuge versus other justifications (e.g., historical right, security, economic development).',
    'If the refuge mandate has atrophied, the constraint''s justification shifts, potentially revealing a ''Piton'' or ''Snare'' where a ''Rope'' once stood, as the original coordination problem is no longer the primary driver.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandatrophy_of_refuge_claim, empirical, 'Whether the original mandate of providing refuge remains the primary driver of territorial control.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(territorial_legitimacy_dual__zionist_refuge_reading, 1948, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(terr_tr_t1948, territorial_legitimacy_dual__zionist_refuge_reading, theater_ratio, 1948, 0.1).
narrative_ontology:measurement(terr_tr_t1967, territorial_legitimacy_dual__zionist_refuge_reading, theater_ratio, 1967, 0.15).
narrative_ontology:measurement(terr_tr_t1993, territorial_legitimacy_dual__zionist_refuge_reading, theater_ratio, 1993, 0.18).
narrative_ontology:measurement(terr_tr_t2000, territorial_legitimacy_dual__zionist_refuge_reading, theater_ratio, 2000, 0.19).
narrative_ontology:measurement(terr_tr_t2010, territorial_legitimacy_dual__zionist_refuge_reading, theater_ratio, 2010, 0.2).
narrative_ontology:measurement(terr_tr_t2024, territorial_legitimacy_dual__zionist_refuge_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(terr_be_t1948, territorial_legitimacy_dual__zionist_refuge_reading, base_extractiveness, 1948, 0.5).
narrative_ontology:measurement(terr_be_t1967, territorial_legitimacy_dual__zionist_refuge_reading, base_extractiveness, 1967, 0.6).
narrative_ontology:measurement(terr_be_t1993, territorial_legitimacy_dual__zionist_refuge_reading, base_extractiveness, 1993, 0.62).
narrative_ontology:measurement(terr_be_t2000, territorial_legitimacy_dual__zionist_refuge_reading, base_extractiveness, 2000, 0.63).
narrative_ontology:measurement(terr_be_t2010, territorial_legitimacy_dual__zionist_refuge_reading, base_extractiveness, 2010, 0.64).
narrative_ontology:measurement(terr_be_t2024, territorial_legitimacy_dual__zionist_refuge_reading, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(terr_su_t1948, territorial_legitimacy_dual__zionist_refuge_reading, suppression_requirement, 1948, 0.65).
narrative_ontology:measurement(terr_su_t1967, territorial_legitimacy_dual__zionist_refuge_reading, suppression_requirement, 1967, 0.75).
narrative_ontology:measurement(terr_su_t1993, territorial_legitimacy_dual__zionist_refuge_reading, suppression_requirement, 1993, 0.76).
narrative_ontology:measurement(terr_su_t2000, territorial_legitimacy_dual__zionist_refuge_reading, suppression_requirement, 2000, 0.77).
narrative_ontology:measurement(terr_su_t2010, territorial_legitimacy_dual__zionist_refuge_reading, suppression_requirement, 2010, 0.78).
narrative_ontology:measurement(terr_su_t2024, territorial_legitimacy_dual__zionist_refuge_reading, suppression_requirement, 2024, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
