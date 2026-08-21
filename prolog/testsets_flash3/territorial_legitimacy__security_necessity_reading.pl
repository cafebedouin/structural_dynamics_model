% ============================================================================
% CONSTRAINT STORY: territorial_legitimacy__security_necessity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_territorial_legitimacy__security_necessity_reading, []).

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
 *   constraint_id: territorial_legitimacy__security_necessity_reading
 *   human_readable: Territorial Legitimacy via Security Necessity (1967 borders + strategic depth)
 *   domain: political_theory/international_law/territorial_sovereignty
 *
 * SUMMARY:
 *   This constraint instantiates the 'security necessity' reading of
 *   territorial legitimacy, which justifies Israeli control over territories
 *   beyond the 1967 borders (e.g., West Bank, Golan Heights) as essential for
 *   national security and strategic depth. Palestinian sovereignty is viewed
 *   as conditional on demilitarization, and settlements are considered
 *   legitimate security presences. This reading operates as a Snare,
 *   characterized by high extraction of land and rights from the Palestinian
 *   population, maintained by active enforcement and suppression of
 *   alternatives.
 *
 * KEY AGENTS:
 *   - State of Israel: Agenda-setter, primary beneficiary of territorial control.
 *   - Palestinian Population: Primary payer, bears the costs of occupation and territorial restrictions.
 *   - Palestinian Authority: Payer, exercises limited, conditional sovereignty.
 *   - Israeli Settlers: Beneficiary, their presence is justified by the security doctrine.
 *   - International Law Bodies: Observer, challenge the legitimacy of the reading but lack direct enforcement.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(territorial_legitimacy__security_necessity_reading, 0.85).
domain_priors:suppression_score(territorial_legitimacy__security_necessity_reading, 0.92).
domain_priors:theater_ratio(territorial_legitimacy__security_necessity_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(territorial_legitimacy__security_necessity_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(territorial_legitimacy__security_necessity_reading, suppression_requirement, 0.92).
narrative_ontology:constraint_metric(territorial_legitimacy__security_necessity_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(territorial_legitimacy__security_necessity_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(territorial_legitimacy__security_necessity_reading, resistance, 0.88).

% --- Constraint claim ---
narrative_ontology:constraint_claim(territorial_legitimacy__security_necessity_reading, snare).
narrative_ontology:human_readable(territorial_legitimacy__security_necessity_reading, "Territorial Legitimacy via Security Necessity (1967 borders + strategic depth)").
narrative_ontology:topic_domain(territorial_legitimacy__security_necessity_reading, "political_theory/international_law/territorial_sovereignty").

domain_priors:requires_active_enforcement(territorial_legitimacy__security_necessity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(territorial_legitimacy__security_necessity_reading, '5314ec94-48ed-4e5f-9a45-c90b1ccb090e').
narrative_ontology:cs_kernel_codification('5314ec94-48ed-4e5f-9a45-c90b1ccb090e', formalized).
narrative_ontology:cs_authority_grounding('5314ec94-48ed-4e5f-9a45-c90b1ccb090e', extraction).
narrative_ontology:cs_interpretation_layer_present('5314ec94-48ed-4e5f-9a45-c90b1ccb090e').
narrative_ontology:cs_reading_relation('5314ec94-48ed-4e5f-9a45-c90b1ccb090e', territorial_legitimacy__partition_reading, coexists_with).
narrative_ontology:cs_reading_relation('5314ec94-48ed-4e5f-9a45-c90b1ccb090e', territorial_legitimacy__indigenous_continuity_reading, coexists_with).
narrative_ontology:cs_axiom('5314ec94-48ed-4e5f-9a45-c90b1ccb090e', foundational, defensible_borders_are_existential).
narrative_ontology:cs_axiom_status(defensible_borders_are_existential, holdable).
narrative_ontology:cs_axiom_grounding('5314ec94-48ed-4e5f-9a45-c90b1ccb090e', defensible_borders_are_existential, empirically_contingent).
narrative_ontology:cs_axiom('5314ec94-48ed-4e5f-9a45-c90b1ccb090e', foundational, territory_acquired_in_defensive_war_is_legitimate).
narrative_ontology:cs_axiom_status(territory_acquired_in_defensive_war_is_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('5314ec94-48ed-4e5f-9a45-c90b1ccb090e', territory_acquired_in_defensive_war_is_legitimate, conventional).
narrative_ontology:cs_reference_frame('5314ec94-48ed-4e5f-9a45-c90b1ccb090e', post_1967_defensive_control).
narrative_ontology:cs_drift_state('5314ec94-48ed-4e5f-9a45-c90b1ccb090e', contemporary_international_law_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('5314ec94-48ed-4e5f-9a45-c90b1ccb090e', '').
narrative_ontology:cs_kernel_id(territorial_legitimacy__security_necessity_reading, territorial_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(territorial_legitimacy__security_necessity_reading, state_of_israel).
narrative_ontology:constraint_beneficiary(territorial_legitimacy__security_necessity_reading, israeli_settlers).
narrative_ontology:constraint_victim(territorial_legitimacy__security_necessity_reading, palestinian_population).
narrative_ontology:constraint_victim(territorial_legitimacy__security_necessity_reading, palestinian_authority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Asserts control over territories beyond 1967 borders (West Bank, Golan Heights) as essential for defensive strategic depth, citing historical and ongoing security threats. Administers these territories, including settlement expansion, under this security doctrine. Benefits from the territorial buffer and resource control.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__security_necessity_reading, state_of_israel, agenda_setter,
    institutional, generational, constrained, national).

% Lives under military occupation and administrative control in the West Bank and Gaza, with severe restrictions on movement, land use, and self-governance. Bears the direct costs of territorial control justified by security necessity, experiencing displacement, resource deprivation, and limited political agency.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__security_necessity_reading, palestinian_population, payer,
    powerless, generational, trapped, local).

% Exercises limited self-governance in fragmented enclaves, with its authority and territorial contiguity constrained by Israeli security control. Its sovereignty is conditional on demilitarization and cooperation with Israeli security interests, limiting its ability to represent its population's full aspirations.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__security_necessity_reading, palestinian_authority, payer,
    moderate, biographical, constrained, regional).

% Resides in settlements in the West Bank and Golan Heights, benefiting from state protection, infrastructure, and often subsidized living. Their presence is justified by the security necessity reading as a forward defensive presence, reinforcing the territorial claims.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__security_necessity_reading, israeli_settlers, beneficiary,
    organized, biographical, mobile, local).

% Monitor and adjudicate territorial disputes based on principles of international law, including the illegality of acquiring territory by force and the status of settlements. Their pronouncements challenge the security necessity reading but lack direct enforcement power.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__security_necessity_reading, international_law_bodies, observer,
    institutional, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the defense of the State of Israel by establishing strategic depth and control over key topographical features, aiming to prevent attacks and ensure national security.
% TRANSFER_FUNCTION: Transfers territorial control, resource access, and sovereign rights from the Palestinian population to the State of Israel, justified by the imperative of national security.
% ABSENT_VOICES: The pre-1948 indigenous Palestinian population, whose historical claims and continuous presence are largely excluded from the security necessity framing, would articulate a right to self-determination and return based on continuous habitation and anti-colonial principles.
% DISAPPEARANCE_RATIONALE: If the security necessity doctrine and its enforcement vanished, the territorial status quo would immediately collapse. Palestinian claims to sovereignty over the West Bank and Gaza would be asserted, settlements would lose their legal and military backing, and the entire regional security architecture would be fundamentally reconfigured.
% FOUNDING_PROBLEM: The existential threat to the State of Israel from hostile neighbors and non-state actors, necessitating defensible borders and strategic depth to ensure survival.
% FOUNDING_PROBLEM_CORROBORATION: The State of Israel and its allies attest that the founding problem of existential security threats remains live, citing ongoing conflicts and regional instability. Critics, including international law bodies and Palestinian representatives, argue that while security is a legitimate concern, the territorial control exceeds defensive needs and serves expansionist aims, making the 'live' status contested.
narrative_ontology:disappearance_verdict(territorial_legitimacy__security_necessity_reading, world_rearranges).
narrative_ontology:founding_problem_status(territorial_legitimacy__security_necessity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(territorial_legitimacy__security_necessity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(territorial_legitimacy__security_necessity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(territorial_legitimacy__security_necessity_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(territorial_legitimacy__security_necessity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(territorial_legitimacy__security_necessity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(territorial_legitimacy__security_necessity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.85) is high due to the extensive control over Palestinian land, resources, and movement, which goes beyond immediate defensive needs. Suppression (0.92) is very high, reflecting the military occupation, administrative restrictions, and active enforcement required to maintain this territorial arrangement against significant resistance. The theater ratio (0.4) indicates that while genuine security concerns exist, a substantial portion of the justification and enforcement serves to maintain territorial control and settlement expansion rather than purely defensive functions. The claimed type is 'snare' because the coordination story (national security) serves as cover for asymmetric extraction and suppression of the victim population.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the State of Israel and Israeli settlers, this constraint is a necessary 'rope' or even a 'mountain' for national survival, ensuring security and preventing existential threats. From the perspective of the Palestinian population and international law bodies, it is a 'snare' that systematically extracts land and rights under the guise of security, maintained through coercion and the suppression of self-determination.
 *
 * DIRECTIONALITY LOGIC:
 *   The State of Israel and Israeli settlers are clear beneficiaries, gaining territorial control, resources, and security. The Palestinian population and Palestinian Authority are clear targets, bearing the costs of occupation, restricted movement, and diminished sovereignty. International law bodies act as analytical observers, assessing the constraint against external legal frameworks.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (national security) is still 'live' for the State of Israel, but its application to territorial control beyond 1967 borders is 'contested' by other parties. The high extractiveness and suppression, coupled with rising resistance, suggest that the constraint functions more as a snare than a legitimate security measure, indicating a potential drift from its original defensive mandate towards territorial expansion and control. The classification as a snare prevents mislabeling this as a legitimate security coordination mechanism when its primary effect is asymmetric extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    security_vs_expansion_motivation,
    'To what extent is the territorial control genuinely driven by defensive security necessity versus territorial expansion or resource acquisition?',
    'Analysis of military and intelligence assessments compared with settlement growth patterns, resource control (e.g., water), and infrastructure development in the occupied territories. Independent expert reports on strategic military needs vs. civilian expansion.',
    'If primarily expansionist, the ''security necessity'' justification is largely theatrical, increasing the effective extractiveness and suppression, solidifying the ''snare'' classification. If genuinely defensive, the constraint might lean more towards a ''tangled rope'' with a legitimate, albeit extractive, coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(security_vs_expansion_motivation, empirical, 'Distinguishing genuine security needs from other motivations for territorial control.').

omega_variable(
    proportionality_of_control,
    'Is the level of territorial control and suppression proportional to the actual security threats faced by the State of Israel?',
    'Comparative analysis with other conflict zones and international legal standards for occupation, assessing whether less restrictive measures could achieve comparable security outcomes. Expert military and legal opinions on proportionality.',
    'If disproportionate, the ''snare'' classification is reinforced, as the excess control constitutes unjustified extraction. If proportional, it would suggest a more complex ''tangled rope'' where security coordination is genuinely costly but necessary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proportionality_of_control, empirical, 'Assessing if the means of control are proportional to the stated security ends.').

omega_variable(
    kernel_reading_divergence,
    'This constraint is one reading of the ''territorial_legitimacy'' kernel. How would the classification change under the ''partition_reading'' or ''indigenous_continuity_reading''?',
    'Analyzing the structural deltas: the ''partition_reading'' would likely classify the post-1967 control as a ''snare'' due to violation of international law, while the ''indigenous_continuity_reading'' would classify it as a ''snare'' or ''tangled rope'' from 1948, with higher extractiveness and suppression due to the longer historical baseline of dispossession.',
    'The ''security_necessity_reading'' minimizes the historical extraction and frames current control as defensive. Other readings would likely increase the perceived extractiveness and suppression, potentially shifting the claimed type or the engine''s computed type for the same underlying facts.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_divergence, conceptual, 'Impact of alternative kernel readings on constraint classification.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(territorial_legitimacy__security_necessity_reading, 1967, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(terr_tr_t1967, territorial_legitimacy__security_necessity_reading, theater_ratio, 1967, 0.1).
narrative_ontology:measurement(terr_tr_t1980, territorial_legitimacy__security_necessity_reading, theater_ratio, 1980, 0.15).
narrative_ontology:measurement(terr_tr_t1993, territorial_legitimacy__security_necessity_reading, theater_ratio, 1993, 0.25).
narrative_ontology:measurement(terr_tr_t2005, territorial_legitimacy__security_necessity_reading, theater_ratio, 2005, 0.3).
narrative_ontology:measurement(terr_tr_t2015, territorial_legitimacy__security_necessity_reading, theater_ratio, 2015, 0.35).
narrative_ontology:measurement(terr_tr_t2024, territorial_legitimacy__security_necessity_reading, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(terr_be_t1967, territorial_legitimacy__security_necessity_reading, base_extractiveness, 1967, 0.7).
narrative_ontology:measurement(terr_be_t1980, territorial_legitimacy__security_necessity_reading, base_extractiveness, 1980, 0.75).
narrative_ontology:measurement(terr_be_t1993, territorial_legitimacy__security_necessity_reading, base_extractiveness, 1993, 0.78).
narrative_ontology:measurement(terr_be_t2005, territorial_legitimacy__security_necessity_reading, base_extractiveness, 2005, 0.82).
narrative_ontology:measurement(terr_be_t2015, territorial_legitimacy__security_necessity_reading, base_extractiveness, 2015, 0.84).
narrative_ontology:measurement(terr_be_t2024, territorial_legitimacy__security_necessity_reading, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(terr_su_t1967, territorial_legitimacy__security_necessity_reading, suppression_requirement, 1967, 0.8).
narrative_ontology:measurement(terr_su_t1980, territorial_legitimacy__security_necessity_reading, suppression_requirement, 1980, 0.85).
narrative_ontology:measurement(terr_su_t1993, territorial_legitimacy__security_necessity_reading, suppression_requirement, 1993, 0.88).
narrative_ontology:measurement(terr_su_t2005, territorial_legitimacy__security_necessity_reading, suppression_requirement, 2005, 0.9).
narrative_ontology:measurement(terr_su_t2015, territorial_legitimacy__security_necessity_reading, suppression_requirement, 2015, 0.91).
narrative_ontology:measurement(terr_su_t2024, territorial_legitimacy__security_necessity_reading, suppression_requirement, 2024, 0.92).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(territorial_legitimacy__security_necessity_reading, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'territorial_legitimacy' kernel, alongside 'partition_reading' and 'indigenous_continuity_reading'. Each reading offers a distinct structural interpretation of the same underlying territorial dispute.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
