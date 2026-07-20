% ============================================================================
% CONSTRAINT STORY: jati_practice_norm__colonial_census_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jati_practice_norm__colonial_census_reading, []).

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
    narrative_ontology:stakeholder_gain_flow/2,
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
 *   constraint_id: jati_practice_norm__colonial_census_reading
 *   human_readable: Colonial Census Reification of Jati Categories
 *   domain: social_anthropology/colonial_governance
 *
 * SUMMARY:
 *   Under British colonial rule in South Asia, jatiâpreviously fluid,
 *   locally negotiated categories of occupation, marriage, and statusâwere
 *   stabilized into fixed enumerations through census operations, gazetteers,
 *   and administrative law. This reading treats the resulting constraint as a
 *   colonial administrative artifact: it provided genuine governance
 *   legibility while simultaneously extracting autonomy from local
 *   communities and reifying hierarchy. The constraint is claimed as
 *   tangled_rope because the same structure that coordinated a complex
 *   population for governance also asymmetrically transferred power from
 *   local practitioners to the colonial state and certain collaborating
 *   elites.
 *
 * KEY AGENTS:
 *   - colonial_state: Agenda-setter and primary beneficiary (institutional/national/arbitrage) â designs and enforces census categories, capturing legibility and control.
 *   - dominant_caste_elites: Secondary beneficiary (powerful/national/constrained) â capture fixed categories to consolidate privilege.
 *   - subaltern_jati_groups: Primary payer (powerless/national/identity_locked) â bear the cost of rigidified, state-assigned identity.
 *   - local_community_practitioners: Secondary payer (moderate/local/identity_locked) â lose authority over fluid boundary management.
 *   - postcolonial_scholars: Observer (analytical/global/analytical) â trace the long-term effects of colonial categorization.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jati_practice_norm__colonial_census_reading, 0.62).
domain_priors:suppression_score(jati_practice_norm__colonial_census_reading, 0.58).
domain_priors:theater_ratio(jati_practice_norm__colonial_census_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jati_practice_norm__colonial_census_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(jati_practice_norm__colonial_census_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(jati_practice_norm__colonial_census_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jati_practice_norm__colonial_census_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(jati_practice_norm__colonial_census_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jati_practice_norm__colonial_census_reading, tangled_rope).
narrative_ontology:human_readable(jati_practice_norm__colonial_census_reading, "Colonial Census Reification of Jati Categories").
narrative_ontology:topic_domain(jati_practice_norm__colonial_census_reading, "social_anthropology/colonial_governance").

domain_priors:requires_active_enforcement(jati_practice_norm__colonial_census_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jati_practice_norm__colonial_census_reading, 'fa630daa-aec7-44b0-b3a3-6e94426e6714').
narrative_ontology:cs_kernel_codification('fa630daa-aec7-44b0-b3a3-6e94426e6714', formalized).
narrative_ontology:cs_authority_grounding('fa630daa-aec7-44b0-b3a3-6e94426e6714', extraction).
narrative_ontology:cs_interpretation_layer_present('fa630daa-aec7-44b0-b3a3-6e94426e6714').
narrative_ontology:cs_reading_relation('fa630daa-aec7-44b0-b3a3-6e94426e6714', jati_practice_norm__orthodox_textual_reading, coexists_with).
narrative_ontology:cs_reading_relation('fa630daa-aec7-44b0-b3a3-6e94426e6714', jati_practice_norm__localized_practice_reading, influences).
narrative_ontology:cs_axiom('fa630daa-aec7-44b0-b3a3-6e94426e6714', foundational, jati_as_administrative_enumeration).
narrative_ontology:cs_axiom_status(jati_as_administrative_enumeration, holdable).
narrative_ontology:cs_axiom_grounding('fa630daa-aec7-44b0-b3a3-6e94426e6714', jati_as_administrative_enumeration, conventional).
narrative_ontology:cs_axiom('fa630daa-aec7-44b0-b3a3-6e94426e6714', foundational, state_monopoly_on_social_classification).
narrative_ontology:cs_axiom_status(state_monopoly_on_social_classification, holdable).
narrative_ontology:cs_axiom_grounding('fa630daa-aec7-44b0-b3a3-6e94426e6714', state_monopoly_on_social_classification, conventional).
narrative_ontology:cs_reference_frame('fa630daa-aec7-44b0-b3a3-6e94426e6714', colonial_legibility_framework).
narrative_ontology:cs_drift_state('fa630daa-aec7-44b0-b3a3-6e94426e6714', postcolonial_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('fa630daa-aec7-44b0-b3a3-6e94426e6714', '').
narrative_ontology:cs_kernel_id(jati_practice_norm__colonial_census_reading, jati_practice_norm).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jati_practice_norm__colonial_census_reading, colonial_state).
narrative_ontology:constraint_beneficiary(jati_practice_norm__colonial_census_reading, dominant_caste_elites).
narrative_ontology:constraint_victim(jati_practice_norm__colonial_census_reading, subaltern_jati_groups).
narrative_ontology:constraint_victim(jati_practice_norm__colonial_census_reading, local_community_practitioners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Designs and imposes census categories, gazetteers, and legal classifications across the territory. Benefits from governance legibility, streamlined taxation, recruitment, and social control. Could revise categories but maintains fixed schedules to preserve administrative stability.
narrative_ontology:constraint_stakeholder(jati_practice_norm__colonial_census_reading, colonial_state, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(jati_practice_norm__colonial_census_reading, colonial_state, beneficiary).

% Collaborate with colonial enumerators to secure favorable recorded status. Leverage fixed census categories to consolidate land, ritual, and political privileges that were previously more contested at the local level.
narrative_ontology:constraint_stakeholder(jati_practice_norm__colonial_census_reading, dominant_caste_elites, beneficiary,
    powerful, generational, constrained, national).

% Assigned fixed census identities that override local self-understanding. Recorded status determines access to public space, legal recourse, and economic opportunity. The colonial category fuses with community identity, making exit from the classification impossible.
narrative_ontology:constraint_stakeholder(jati_practice_norm__colonial_census_reading, subaltern_jati_groups, payer,
    powerless, generational, identity_locked, national).

% Previously managed fluid boundaries through village councils, marriage negotiations, and occupational networks. Colonial census overrides local authority; their expertise and role in social classification becomes irrelevant or illegitimate under the new regime.
narrative_ontology:constraint_stakeholder(jati_practice_norm__colonial_census_reading, local_community_practitioners, payer,
    moderate, biographical, identity_locked, local).

% Document and analyze the colonial transformation of jati from fluid practice to rigid administrative category. Operate outside the constraint's direct operation, tracing its long-term effects on identity and politics.
narrative_ontology:constraint_stakeholder(jati_practice_norm__colonial_census_reading, postcolonial_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jati_practice_norm__colonial_census_reading, colonial_state).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides governance legibility for a large, diverse population by replacing locally fluid social boundaries with fixed, enumerable administrative units suitable for census, taxation, and legal classification.
% TRANSFER_FUNCTION: Moves authority over social classification from local communities and practitioners to the colonial state apparatus; moves recognition and consolidated privilege toward groups that successfully capture fixed census categories.
% ABSENT_VOICES: Local community practitioners and subaltern groups whose self-understanding did not match census schedules were excluded from the classification design process; their alternative taxonomies had no seat at the enumerators' table.
% DISAPPEARANCE_RATIONALE: If the fixed census categories vanished overnight, the colonial administrative architecture would lose its basic units of social knowledge; taxation, recruitment, and representative structures would require entirely different infrastructural scaffolding, and local communities would revert to fluid negotiation.
% FOUNDING_PROBLEM: The colonial state needed to know, count, and govern a vast population with highly localized and fluid social boundaries; existing practice-based knowledge was illegible to centralized bureaucracy.
% FOUNDING_PROBLEM_CORROBORATION: Colonial officials and census commissioners attested the administrative need in memoirs and reports. Postcolonial scholars and subaltern historians attest the problem was manufactured by the colonial project itself to serve extractive control; no party entirely outside the benefiting colonial apparatus unambiguously corroborates the problem as natural and independent of colonial construction.
narrative_ontology:disappearance_verdict(jati_practice_norm__colonial_census_reading, world_rearranges).
narrative_ontology:founding_problem_status(jati_practice_norm__colonial_census_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jati_practice_norm__colonial_census_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(jati_practice_norm__colonial_census_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jati_practice_norm__colonial_census_reading, 0.62, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jati_practice_norm__colonial_census_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jati_practice_norm__colonial_census_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jati_practice_norm__colonial_census_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) is moderate-to-high because the constraint transfers substantial autonomy over social identity from communities to the state, while theater_ratio (0.25) remains relatively low because the census apparatus performed genuine administrative work. Suppression (0.58) reflects the active enforcement required to maintain fixed categories against local fluidity, rising over the interval as the census hardened into legal and political infrastructure. Accessibility_collapse (0.65) captures how alternatives to colonial categories became administratively invisible, while resistance (0.45) registers petitions, misreporting, and evasion that never coalesced into systemic reversal.
 *
 * PERSPECTIVAL GAP:
 *   The colonial_state seat experiences the constraint as necessary coordinationâwithout fixed categories, the territory is ungovernable. The subaltern_jati_groups and local_community_practitioners seats experience the same structure as externally imposed identity lock. The engine should compute strong seat divergence: the agenda_setter/beneficiary seats derive low directionality (subsidized by the constraint's legibility gains), while the payer seats derive high directionality (targeted by its identity-fixing extraction).
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations (colonial_state, dominant_caste_elites) feed low directionality because the constraint subsidizes their control and privilege. Victim declarations (subaltern_jati_groups, local_community_practitioners) feed high directionality because the constraint extracts autonomy and local authority. Exit modulation amplifies this: colonial_state has arbitrage-grade exit (could redesign categories), while subaltern groups are identity_locked (the census category becomes constitutive of their social existence). No override is needed because the structural derivation chain already captures the asymmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint prevents mislabeling in both directions. Against a pure snare reading, it preserves the genuine coordination function: colonial governance of a vast, heterogeneous population was not a fiction, and fixed categories did solve real information problems. Against a pure rope reading, it insists that the coordination was not symmetricâlocal communities paid for the state's legibility with lost autonomy. The tangled_rope classification captures that both claims are structurally true of the same arrangement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    colonial_intent_vs_effect,
    'Did the colonial administration intentionally reify jati categories for divide-and-rule, or did rigidification emerge as an unintended consequence of bureaucratic legibility demands?',
    'Archival analysis of census commissioners'' internal instructions, ethnographic surveys, and administrative debates to distinguish instrumental design from emergent bureaucratic path-dependence.',
    'If intentional, extraction shifts snare-ward; if emergent, the coordination function is more salient and the structure remains tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(colonial_intent_vs_effect, empirical, 'Ambiguity between instrumental colonial design and emergent bureaucratic effect.').

omega_variable(
    category_permanence_ambiguity,
    'To what extent do contemporary jati identities remain dependent on colonial census categories versus pre-colonial or local practice?',
    'Ethnographic comparison across regions with differential colonial census penetration, combined with oral-history projects tracing identity narratives before and after colonial enumeration.',
    'If colonial categories permanently constitutive, effective extraction is higher and more irreversible; if removable administrative layer, the constraint is more scaffold-like.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(category_permanence_ambiguity, conceptual, 'Whether colonial census reification is a removable layer or permanently constitutive of modern identity.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jati_practice_norm__colonial_census_reading, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jati_practice_norm__colonial_census_reading_tr_t0, jati_practice_norm__colonial_census_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(jati_practice_norm__colonial_census_reading_tr_t10, jati_practice_norm__colonial_census_reading, theater_ratio, 10, 0.18).
narrative_ontology:measurement(jati_practice_norm__colonial_census_reading_tr_t20, jati_practice_norm__colonial_census_reading, theater_ratio, 20, 0.2).
narrative_ontology:measurement(jati_practice_norm__colonial_census_reading_tr_t30, jati_practice_norm__colonial_census_reading, theater_ratio, 30, 0.22).
narrative_ontology:measurement(jati_practice_norm__colonial_census_reading_tr_t40, jati_practice_norm__colonial_census_reading, theater_ratio, 40, 0.24).
narrative_ontology:measurement(jati_practice_norm__colonial_census_reading_tr_t50, jati_practice_norm__colonial_census_reading, theater_ratio, 50, 0.25).
narrative_ontology:measurement(jati_practice_norm__colonial_census_reading_tr_t60, jati_practice_norm__colonial_census_reading, theater_ratio, 60, 0.26).
narrative_ontology:measurement(jati_practice_norm__colonial_census_reading_tr_t70, jati_practice_norm__colonial_census_reading, theater_ratio, 70, 0.27).
narrative_ontology:measurement(jati_practice_norm__colonial_census_reading_tr_t80, jati_practice_norm__colonial_census_reading, theater_ratio, 80, 0.28).

% Extraction over time
narrative_ontology:measurement(jati_practice_norm__colonial_census_reading_be_t0, jati_practice_norm__colonial_census_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(jati_practice_norm__colonial_census_reading_be_t10, jati_practice_norm__colonial_census_reading, base_extractiveness, 10, 0.45).
narrative_ontology:measurement(jati_practice_norm__colonial_census_reading_be_t20, jati_practice_norm__colonial_census_reading, base_extractiveness, 20, 0.5).
narrative_ontology:measurement(jati_practice_norm__colonial_census_reading_be_t30, jati_practice_norm__colonial_census_reading, base_extractiveness, 30, 0.55).
narrative_ontology:measurement(jati_practice_norm__colonial_census_reading_be_t40, jati_practice_norm__colonial_census_reading, base_extractiveness, 40, 0.6).
narrative_ontology:measurement(jati_practice_norm__colonial_census_reading_be_t50, jati_practice_norm__colonial_census_reading, base_extractiveness, 50, 0.62).
narrative_ontology:measurement(jati_practice_norm__colonial_census_reading_be_t60, jati_practice_norm__colonial_census_reading, base_extractiveness, 60, 0.65).
narrative_ontology:measurement(jati_practice_norm__colonial_census_reading_be_t70, jati_practice_norm__colonial_census_reading, base_extractiveness, 70, 0.66).
narrative_ontology:measurement(jati_practice_norm__colonial_census_reading_be_t80, jati_practice_norm__colonial_census_reading, base_extractiveness, 80, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(jati_practice_norm__colonial_census_reading_su_t0, jati_practice_norm__colonial_census_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(jati_practice_norm__colonial_census_reading_su_t10, jati_practice_norm__colonial_census_reading, suppression_requirement, 10, 0.5).
narrative_ontology:measurement(jati_practice_norm__colonial_census_reading_su_t20, jati_practice_norm__colonial_census_reading, suppression_requirement, 20, 0.55).
narrative_ontology:measurement(jati_practice_norm__colonial_census_reading_su_t30, jati_practice_norm__colonial_census_reading, suppression_requirement, 30, 0.58).
narrative_ontology:measurement(jati_practice_norm__colonial_census_reading_su_t40, jati_practice_norm__colonial_census_reading, suppression_requirement, 40, 0.6).
narrative_ontology:measurement(jati_practice_norm__colonial_census_reading_su_t50, jati_practice_norm__colonial_census_reading, suppression_requirement, 50, 0.62).
narrative_ontology:measurement(jati_practice_norm__colonial_census_reading_su_t60, jati_practice_norm__colonial_census_reading, suppression_requirement, 60, 0.65).
narrative_ontology:measurement(jati_practice_norm__colonial_census_reading_su_t70, jati_practice_norm__colonial_census_reading, suppression_requirement, 70, 0.68).
narrative_ontology:measurement(jati_practice_norm__colonial_census_reading_su_t80, jati_practice_norm__colonial_census_reading, suppression_requirement, 80, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jati_practice_norm__colonial_census_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(jati_practice_norm__colonial_census_reading, orthodox_textual_reading).
narrative_ontology:affects_constraint(jati_practice_norm__colonial_census_reading, localized_practice_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the jati_practice_norm kernel, decomposed per the epsilon-invariance principle alongside orthodox_textual_reading and localized_practice_reading. Each reading instantiates a structurally distinct claim with different epsilon values, beneficiary/victim structures, and coordination functions. The colonial census reading is downstream of administrative enforcement; it influences the localized practice reading by structurally constraining local renegotiation, and coexists with the orthodox textual reading as an alternative knowledge project within the same colonial epistemic field.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
