% ============================================================================
% CONSTRAINT STORY: jewish_self_determination__settler_colonial_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-01-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jewish_self_determination__settler_colonial_reading, []).

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
 *   constraint_id: jewish_self_determination__settler_colonial_reading
 *   human_readable: Zionism as European Settler-Colonial Project
 *   domain: political/nationalism/postcolonial
 *
 * SUMMARY:
 *   This constraint instantiates the settler_colonial_reading of the
 *   contested kernel jewish_self_determination. It treats Zionism not as a
 *   normal national self-determination project but as a European
 *   settler-colonial structure whose persistence requires the ongoing
 *   dispossession, legal exclusion, and demographic displacement of the
 *   indigenous Palestinian population. The arrangement is presented
 *   internationally as security and refugee protection; the settler-colonial
 *   reading evaluates it as a snare in which coordination claims serve to
 *   mask and sustain extraction.
 *
 * KEY AGENTS:
 *   - israeli_state: Primary agenda-setter (institutional/constrained) â administers the legal-military architecture of settler colonialism, collects territorial expansion and demographic control
 *   - jewish_settler_society: Primary beneficiary (organized/identity_locked) â accrues land, housing, mobility rights, and legal privileges through the exclusion of the indigenous population
 *   - palestinian_population: Primary target (powerless/trapped) â bears dispossession, occupation, refugee status, and legal exclusion; structurally blocked from exit or reversal
 *   - international_community: Analytical observer (institutional/analytical) â sustains the structure diplomatically and economically while intermittently condemning its surface manifestations
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_self_determination__settler_colonial_reading, 0.88).
domain_priors:suppression_score(jewish_self_determination__settler_colonial_reading, 0.9).
domain_priors:theater_ratio(jewish_self_determination__settler_colonial_reading, 0.75).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_self_determination__settler_colonial_reading, extractiveness, 0.88).
narrative_ontology:constraint_metric(jewish_self_determination__settler_colonial_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(jewish_self_determination__settler_colonial_reading, theater_ratio, 0.75).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_self_determination__settler_colonial_reading, accessibility_collapse, 0.82).
narrative_ontology:constraint_metric(jewish_self_determination__settler_colonial_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_self_determination__settler_colonial_reading, snare).
narrative_ontology:human_readable(jewish_self_determination__settler_colonial_reading, "Zionism as European Settler-Colonial Project").
narrative_ontology:topic_domain(jewish_self_determination__settler_colonial_reading, "political/nationalism/postcolonial").

domain_priors:requires_active_enforcement(jewish_self_determination__settler_colonial_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_self_determination__settler_colonial_reading, 'f28373ff-5f82-43bb-a038-113f4e804342').
narrative_ontology:cs_kernel_codification('f28373ff-5f82-43bb-a038-113f4e804342', formalized).
narrative_ontology:cs_authority_grounding('f28373ff-5f82-43bb-a038-113f4e804342', extraction).
narrative_ontology:cs_interpretation_layer_present('f28373ff-5f82-43bb-a038-113f4e804342').
narrative_ontology:cs_reading_relation('f28373ff-5f82-43bb-a038-113f4e804342', jewish_self_determination__diasporist_reading, coexists_with).
narrative_ontology:cs_reading_relation('f28373ff-5f82-43bb-a038-113f4e804342', jewish_self_determination__indigenous_return_reading, forecloses).
narrative_ontology:cs_reading_relation('f28373ff-5f82-43bb-a038-113f4e804342', jewish_self_determination__liberal_nationalist_reading, influences).
narrative_ontology:cs_reading_relation('f28373ff-5f82-43bb-a038-113f4e804342', jewish_self_determination__religious_covenant_reading, coexists_with).
narrative_ontology:cs_axiom('f28373ff-5f82-43bb-a038-113f4e804342', foundational, colonial_displacement_as_foundational).
narrative_ontology:cs_axiom_status(colonial_displacement_as_foundational, holdable).
narrative_ontology:cs_axiom_grounding('f28373ff-5f82-43bb-a038-113f4e804342', colonial_displacement_as_foundational, empirically_contingent).
narrative_ontology:cs_axiom('f28373ff-5f82-43bb-a038-113f4e804342', foundational, indigenous_sovereignty_precedence).
narrative_ontology:cs_axiom_status(indigenous_sovereignty_precedence, holdable).
narrative_ontology:cs_axiom_grounding('f28373ff-5f82-43bb-a038-113f4e804342', indigenous_sovereignty_precedence, deontological).
narrative_ontology:cs_reference_frame('f28373ff-5f82-43bb-a038-113f4e804342', settler_sovereignty_supremacy).
narrative_ontology:cs_drift_state('f28373ff-5f82-43bb-a038-113f4e804342', contemporary_one_state_reality, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('f28373ff-5f82-43bb-a038-113f4e804342', '').
narrative_ontology:cs_kernel_id(jewish_self_determination__settler_colonial_reading, jewish_self_determination).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_self_determination__settler_colonial_reading, israeli_state).
narrative_ontology:constraint_beneficiary(jewish_self_determination__settler_colonial_reading, jewish_settler_society).
narrative_ontology:constraint_victim(jewish_self_determination__settler_colonial_reading, palestinian_population).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the legal and military architecture of settler colonialism: the Law of Return, military occupation, settlement administration, and differential citizenship regimes. Collects territorial expansion and demographic control as sovereign prerogatives. Exit from this structure would require dismantling the ethnic supremacy at the core of state identity.
narrative_ontology:constraint_stakeholder(jewish_self_determination__settler_colonial_reading, israeli_state, agenda_setter,
    institutional, generational, constrained, national).

% Benefits from subsidized settlement housing, preferential land access, full citizenship rights, and freedom of movement denied to Palestinians. Daily life is organized around the spatial and legal segregation of the indigenous population. Exit options are technically mobile but identity-locked by ideological fusion with the settler project.
narrative_ontology:constraint_stakeholder(jewish_self_determination__settler_colonial_reading, jewish_settler_society, beneficiary,
    organized, biographical, identity_locked, national).

% Bears the costs of displacement, military occupation, resource deprivation, and legal exclusion. Includes refugees denied return, Palestinian citizens of Israel under discriminatory laws, and occupied Palestinians under military administration. Exit is structurally blocked by international border regimes, internal checkpoints, and the denial of residency and construction permits.
narrative_ontology:constraint_stakeholder(jewish_self_determination__settler_colonial_reading, palestinian_population, payer,
    powerless, generational, trapped, national).

% Observes through UN bodies, international courts, and human rights organizations. Periodically issues condemnations and legal findings but generally sustains the structural arrangement through diplomatic recognition, military aid to the settler state, and refusal to enforce international law against settlement expansion.
narrative_ontology:constraint_stakeholder(jewish_self_determination__settler_colonial_reading, international_community, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The arrangement claims to coordinate Jewish collective survival, refugee resettlement, and national self-determination through territorial concentration in a single sovereign state.
% TRANSFER_FUNCTION: Moves land, water, sovereignty, and demographic control from the indigenous Palestinian population to the settler population and the Israeli state, enforced through differential legal status and military administration.
% ABSENT_VOICES: Palestinian refugees and the internally displaced are excluded from territorial and political negotiation; anti-Zionist Jewish voices and diasporist frameworks are marginalized within institutional Jewish communal discourse and excluded from state policy.
% DISAPPEARANCE_RATIONALE: If the settler-colonial structure vanished overnight, land and resource distribution would revert or require radical renegotiation, the legal hierarchy of citizenship and residency would collapse, and the demographic and territorial composition of the state would shift fundamentally.
% FOUNDING_PROBLEM: European Jewish populations faced antisemitic persecution, statelessness, and exclusion from citizenship rights in European states; Zionism proposed territorial concentration in Palestine as the solution.
% FOUNDING_PROBLEM_CORROBORATION: Zionist historiography and Israeli state institutions assert the problem remains live. Palestinian historians, postcolonial scholars, and diasporist Jewish historians attest that the European persecution crisis that motivated mass emigration has passed, that Jewish populations now hold secure citizenship globally, and that the territorial state persists not from ongoing refugee necessity but from colonial structural inertia. Corroboration from outside the benefiting parties includes Palestinian oral history archives, UNRWA documentation of ongoing dispossession, and the academic field of settler-colonial studies.
narrative_ontology:disappearance_verdict(jewish_self_determination__settler_colonial_reading, world_rearranges).
narrative_ontology:founding_problem_status(jewish_self_determination__settler_colonial_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_self_determination__settler_colonial_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(jewish_self_determination__settler_colonial_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jewish_self_determination__settler_colonial_reading, 0.88, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jewish_self_determination__settler_colonial_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jewish_self_determination__settler_colonial_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jewish_self_determination__settler_colonial_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very high (0.88) because the constraint systematically transfers land, water, and sovereignty from the indigenous population to the settler society and state. Suppression is higher (0.90) because the structure depends on military occupation, legal apartheid, and the denial of refugee return. Theater ratio is substantial (0.75): the claims of democracy, security, and peace process function as performative cover for territorial expansion. Accessibility collapse is high (0.82) because Palestinian statehood and return remain theoretically recognized but are practically blocked by enforcement; resistance is substantial (0.70) because Palestinian organizing, armed struggle, and international solidarity persist despite suppression.
 *
 * PERSPECTIVAL GAP:
 *   From the Israeli state and settler society seats, the arrangement appears as security, nation-building, and self-determination. From the Palestinian seat, the identical structure operates as dispossession, fragmentation, and elimination. The engine computes this divergence from the structural data: identical constraint, opposed directionality, different effective extraction for each seat.
 *
 * DIRECTIONALITY LOGIC:
 *   The Israeli state and Jewish settler society are declared beneficiaries: the state secures territorial sovereignty and demographic dominance, while the settler population receives material allocations of land and rights denied to Palestinians. These declarations push their directionality toward the beneficiary end. The Palestinian population is the declared victim group, with trapped exit options, pushing directionality toward the full-target end. The international community sits at analytical remove.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â persecution and statelessness of European Jews â was historically real but, on this reading, has been substantially addressed by other means (diaspora integration, post-war political rights). The persistence of the territorial state and its expansion is therefore classified as snare: the coordination story (refuge, self-determination) serves as cover for ongoing extraction. If the founding problem were still live and the arrangement genuinely transitional, it might read as scaffold; if it extracted without any historical coordination rationale, it would be a clearer snare. The mandatrophy check prevents misclassifying historical origin as current function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    colonial_vs_national_framing,
    'Is the Israeli state a normal nation-state with exceptional security policies, or a settler-colonial regime whose persistence requires ongoing indigenous dispossession?',
    'Comparative legal and historical analysis: if the state''s territorial and demographic policies are structurally reversible without collapse, it is a nation-state; if sovereignty itself is predicated on the elimination of indigenous political presence, it is settler-colonial.',
    'Resolution would reclassify the constraint from snare to tangled_rope (if coordination and extraction are separable) or scaffold (if transitional), or confirm snare if extraction is constitutive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(colonial_vs_national_framing, conceptual, 'Whether the constraint is fundamentally colonial or nationalist in structure.').

omega_variable(
    enforcement_vs_internalized_suppression,
    'Is Palestinian subordination maintained primarily by external military and legal enforcement, or by internalized political demobilization and fragmentation?',
    'Comparative analysis of resistance patterns during periods of enforcement relaxation versus intensification; trajectory of Palestinian political organization across refugee camps, diaspora, and occupied territories.',
    'If primarily internalized, the constraint''s effective suppression exceeds the structural measure and the classification tilts toward deeper entrapment; if primarily external, the constraint is vulnerable to enforcement disruption.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_vs_internalized_suppression, empirical, 'Structural versus internalized suppression mechanism for the indigenous population.').

omega_variable(
    international_complicity_sustainability,
    'Does the constraint''s persistence depend on active international sponsorship and diplomatic protection, or could the settler-colonial structure self-fund indefinitely?',
    'Modeling of military, economic, and diplomatic flows with and without US/EU support; assessment of domestic Israeli economic capacity to sustain occupation and settlement infrastructure.',
    'If dependent on international support, the constraint is externally coupled and potentially fragile; if self-sustaining, it is a robust snare with deep institutional roots.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(international_complicity_sustainability, empirical, 'Whether the extraction mechanism requires external international sponsorship.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_self_determination__settler_colonial_reading, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jewi_tr_t0, jewish_self_determination__settler_colonial_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(jewi_tr_t15, jewish_self_determination__settler_colonial_reading, theater_ratio, 15, 0.32).
narrative_ontology:measurement(jewi_tr_t30, jewish_self_determination__settler_colonial_reading, theater_ratio, 30, 0.48).
narrative_ontology:measurement(jewi_tr_t45, jewish_self_determination__settler_colonial_reading, theater_ratio, 45, 0.6).
narrative_ontology:measurement(jewi_tr_t60, jewish_self_determination__settler_colonial_reading, theater_ratio, 60, 0.68).
narrative_ontology:measurement(jewi_tr_t75, jewish_self_determination__settler_colonial_reading, theater_ratio, 75, 0.75).

% Extraction over time
narrative_ontology:measurement(jewi_be_t0, jewish_self_determination__settler_colonial_reading, base_extractiveness, 0, 0.75).
narrative_ontology:measurement(jewi_be_t15, jewish_self_determination__settler_colonial_reading, base_extractiveness, 15, 0.7).
narrative_ontology:measurement(jewi_be_t30, jewish_self_determination__settler_colonial_reading, base_extractiveness, 30, 0.73).
narrative_ontology:measurement(jewi_be_t45, jewish_self_determination__settler_colonial_reading, base_extractiveness, 45, 0.78).
narrative_ontology:measurement(jewi_be_t60, jewish_self_determination__settler_colonial_reading, base_extractiveness, 60, 0.84).
narrative_ontology:measurement(jewi_be_t75, jewish_self_determination__settler_colonial_reading, base_extractiveness, 75, 0.88).

% Suppression requirement over time
narrative_ontology:measurement(jewi_su_t0, jewish_self_determination__settler_colonial_reading, suppression_requirement, 0, 0.8).
narrative_ontology:measurement(jewi_su_t15, jewish_self_determination__settler_colonial_reading, suppression_requirement, 15, 0.65).
narrative_ontology:measurement(jewi_su_t30, jewish_self_determination__settler_colonial_reading, suppression_requirement, 30, 0.72).
narrative_ontology:measurement(jewi_su_t45, jewish_self_determination__settler_colonial_reading, suppression_requirement, 45, 0.78).
narrative_ontology:measurement(jewi_su_t60, jewish_self_determination__settler_colonial_reading, suppression_requirement, 60, 0.85).
narrative_ontology:measurement(jewi_su_t75, jewish_self_determination__settler_colonial_reading, suppression_requirement, 75, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(jewish_self_determination__settler_colonial_reading, jewish_self_determination__liberal_nationalist_reading).
narrative_ontology:affects_constraint(jewish_self_determination__settler_colonial_reading, jewish_self_determination__indigenous_return_reading).
narrative_ontology:affects_constraint(jewish_self_determination__settler_colonial_reading, jewish_self_determination__religious_covenant_reading).
narrative_ontology:affects_constraint(jewish_self_determination__settler_colonial_reading, jewish_self_determination__diasporist_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kernel jewish_self_determination. The label 'Jewish self-determination' conflates multiple structurally distinct claims: liberal-nationalist (equal national claim), indigenous-return (decolonization), religious-covenant (divine obligation), diasporist (pluralist minority rights), and settler-colonial (European dispossession). Each reading instantiates a different constraint with different epsilon, beneficiaries, and victims. They are linked as a constraint family via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
