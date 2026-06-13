% ============================================================================
% CONSTRAINT STORY: legitimacy_of_imposed_practice__exogenous_override_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legitimacy_of_imposed_practice__exogenous_override_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: legitimacy_of_imposed_practice__exogenous_override_reading
 *   human_readable: Exogenous Override: State Decree Authority Displaces Prior Practice
 *   domain: political/cultural/institutional
 *
 * SUMMARY:
 *   This constraint instantiates the EXOGENOUS OVERRIDE READING of the
 *   contested kernel 'legitimacy_of_imposed_practice.' The reading claims
 *   that state decree authority is structurally sufficient to displace prior
 *   practice; compliance follows from legal mandate regardless of whether the
 *   target population internalizes the new practice or continues private
 *   non-compliance. This is ONE reading among three sibling readings
 *   (endogenous_climb_reading, hybrid_scaffolding_reading) that contest the
 *   same kernel with different structural premises. The exogenous override
 *   reading asserts that coercive state authority, backed by enforcement
 *   machinery, can impose new social practices even absent internalization.
 *   The measurement series tracks the constraint's operation over 40 years:
 *   extraction rises steeply in the first 15 years as enforcement machinery
 *   hardens, then plateaus as the constraint reaches equilibrium—high
 *   suppression but persistent private non-compliance. Theater ratio rises
 *   throughout, indicating enforcement activity increasingly devoted to
 *   maintaining the fiction of displacement rather than to actual behavior
 *   change.
 *
 * KEY AGENTS:
 *   - state_modernization_apparatus: institutional agenda-setter (issues decree, benefits from unified framework)
 *   - rural_populations: powerless, trapped payers (bear adjustment costs, sustain underground compliance)
 *   - tradition_practitioners: identity-locked payers (social role eroded; exit means identity death)
 *   - urban_administrative_centers: institutional beneficiaries (gain from standardization)
 *   - enforcement_apparatus: institutional agenda-setter and beneficiary (expands scope/budget with enforcement)
 *   - competing_legitimacy_claims: excluded moderates (would challenge the reading but are kept out)
 *   - historical_analyst: analytical observer (measures displacement rate and internalization gap)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimacy_of_imposed_practice__exogenous_override_reading, 0.68).
domain_priors:suppression_score(legitimacy_of_imposed_practice__exogenous_override_reading, 0.79).
domain_priors:theater_ratio(legitimacy_of_imposed_practice__exogenous_override_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__exogenous_override_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__exogenous_override_reading, suppression_requirement, 0.79).
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__exogenous_override_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__exogenous_override_reading, accessibility_collapse, 0.61).
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__exogenous_override_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimacy_of_imposed_practice__exogenous_override_reading, tangled_rope).
narrative_ontology:human_readable(legitimacy_of_imposed_practice__exogenous_override_reading, "Exogenous Override: State Decree Authority Displaces Prior Practice").
narrative_ontology:topic_domain(legitimacy_of_imposed_practice__exogenous_override_reading, "political/cultural/institutional").

domain_priors:requires_active_enforcement(legitimacy_of_imposed_practice__exogenous_override_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimacy_of_imposed_practice__exogenous_override_reading, '6df9c079-ae91-480c-bdd9-397d8dacd05e').
narrative_ontology:cs_kernel_codification('6df9c079-ae91-480c-bdd9-397d8dacd05e', formalized).
narrative_ontology:cs_authority_grounding('6df9c079-ae91-480c-bdd9-397d8dacd05e', extraction).
narrative_ontology:cs_interpretation_layer_present('6df9c079-ae91-480c-bdd9-397d8dacd05e').
narrative_ontology:cs_reading_relation('6df9c079-ae91-480c-bdd9-397d8dacd05e', legitimacy_of_imposed_practice__endogenous_climb_reading, coexists_with).
narrative_ontology:cs_reading_relation('6df9c079-ae91-480c-bdd9-397d8dacd05e', legitimacy_of_imposed_practice__hybrid_scaffolding_reading, influences).
narrative_ontology:cs_axiom('6df9c079-ae91-480c-bdd9-397d8dacd05e', foundational, decree_authority_sufficient_for_displacement).
narrative_ontology:cs_axiom_status(decree_authority_sufficient_for_displacement, holdable).
narrative_ontology:cs_axiom_grounding('6df9c079-ae91-480c-bdd9-397d8dacd05e', decree_authority_sufficient_for_displacement, empirically_contingent).
narrative_ontology:cs_axiom('6df9c079-ae91-480c-bdd9-397d8dacd05e', foundational, internalization_not_required_for_compliance).
narrative_ontology:cs_axiom_status(internalization_not_required_for_compliance, holdable).
narrative_ontology:cs_axiom_grounding('6df9c079-ae91-480c-bdd9-397d8dacd05e', internalization_not_required_for_compliance, empirically_contingent).
narrative_ontology:cs_reference_frame('6df9c079-ae91-480c-bdd9-397d8dacd05e', state_monopoly_over_social_ordering).
narrative_ontology:cs_drift_state('6df9c079-ae91-480c-bdd9-397d8dacd05e', contemporary_persistence_with_underground_continuation, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('6df9c079-ae91-480c-bdd9-397d8dacd05e', '').
narrative_ontology:cs_kernel_id(legitimacy_of_imposed_practice__exogenous_override_reading, legitimacy_of_imposed_practice).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimacy_of_imposed_practice__exogenous_override_reading, state_modernization_apparatus).
narrative_ontology:constraint_victim(legitimacy_of_imposed_practice__exogenous_override_reading, rural_populations).
narrative_ontology:constraint_victim(legitimacy_of_imposed_practice__exogenous_override_reading, tradition_practitioners).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimacy_of_imposed_practice__exogenous_override_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(legitimacy_of_imposed_practice__exogenous_override_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legitimacy_of_imposed_practice__exogenous_override_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(legitimacy_of_imposed_practice__exogenous_override_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(legitimacy_of_imposed_practice__exogenous_override_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderately high (0.68 final) because the constraint transfers authority and legitimacy from customary to state channels, and the payers (rural populations, tradition practitioners) sustain real adjustment costs without consultation. The extraction would be higher if internalization were complete, but the persistent gap between surface compliance and private practice moderates it—the constraint's effectiveness is systematically limited. Suppression is very high (0.79 final) because enforcement machinery must continuously prevent reversion and detect non-compliance; without suppression, the prior practice would rapidly reassert. Theater ratio rises from 0.18 to 0.42 because, after the initial compliance shock, enforcement activity increasingly consists of performing compliance rather than achieving behavioral displacement—inspections that verify calendar use in schools while rural communities plant by lunar calendar, dress code enforcement that catches ceremonial practice, naming records that mask kinship naming in private use. The measurements trace a typical authoritarian displacement curve: rapid extraction growth as enforcement machinery hardens, then plateau as the constraint reaches equilibrium—high suppression sustains compliance at the surface, but the theater ratio indicates the compliance is theater rather than internalization.
 *
 * PERSPECTIVAL GAP:
 *   The state modernization apparatus and enforcement apparatus perceive the constraint as successful coordination: a unified framework they built and maintain. Rural populations and tradition practitioners perceive it as extractive enforcement: they pay adjustment costs, sustain private non-compliance, and experience identity erosion without exit. The historical analyst perceives a third structure: successful surface displacement coupled with persistent underground continuation—the constraint works for administrative purposes (tax collection, military conscription, commerce) but fails for internalization purposes (why people WANT to follow the practice). The engine computes these divergent directionalities from the structural data: agenda-setters get low d (benefit collectors); rural and tradition payers get high d (targets); the analyst seat is symmetric. Where the computed type diverges from the claimed tangled_rope (coordination + asymmetric extraction), that divergence reveals the reading's structural tension: is this coordination with victims, or pure extraction dressed as modernization?
 *
 * DIRECTIONALITY LOGIC:
 *   State modernization apparatus: low d (full beneficiary; controls decree, collects legitimacy, bears minimal adjustment cost). Urban administrative centers: low d (beneficiary; gain from standardization without direct adjustment burden). Enforcement apparatus: low-to-moderate d (benefits from expanded budget and scope, but institutionally bound to continued enforcement—if the practice displaced completely, suppression need would fall and their budget might contract; their interest is in CONTINUED non-compliance as justification for continued enforcement). Rural populations: high d (full target; bear adjustment costs, trapped without exit, no consultation, coerced compliance). Tradition practitioners: moderate-to-high d (target; identity-locked, can exit only via identity death). Competing legitimacy claims: high d (excluded target; their authority is directly threatened by the decree's claim to override prior practice, but they are kept out of the conversation). The directionality_overrides are not needed here because the structural derivation from beneficiary/victim + exit + power captures the asymmetry cleanly.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is NOT mandatrophy—the mandate is very much alive. The founding problem (administrative friction from regional heterogeneity) remains contested but live: state authorities continue to push for compliance, and rural populations continue to resist. The constraint's classification as tangled_rope rather than snare hinges on whether a genuine coordination function exists beneath the extraction. The reading claims it does: unified calendar/dress/naming does solve real coordination problems for commerce and administration. The sibiling readings contest this: endogenous_climb argues coordination requires internalization (decree alone is insufficient); hybrid_scaffolding argues pure decree fails but ideological messaging can generate partial adoption. The measurement series show the constraint sustaining high suppression without rising extraction after t=30, which is consistent with equilibrium under coercion—not with natural internalization (which would show falling suppression as behavior became internalized) or with complete displacement (which would show falling theater ratio as real compliance replaced performance). The exogenous override reading predicts exactly this: suppression must stay high indefinitely to prevent reversion, theater must stay high because the compliance is maintained by enforcement not internalization, and extraction plateaus because the extraction is structural transfer of authority, not accumulating rents.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    internalization_vs_surface_compliance,
    'Does the constraint achieve behavioral displacement (people actually want to follow the new practice) or only surface compliance (people obey because of enforcement)? Do private communities continue the prior practice, or is it genuinely abandoned?',
    'Long-term ethnographic observation in rural communities with varying enforcement intensity; comparison of public behavior (school, market, official records) with private behavior (home, ritual, seasonal); generational tracking of whether children spontaneously adopt the new practice or require continued enforcement to sustain it.',
    'If internalization is substantial, the constraint is pure coordination with successful displacement—endogenous_climb reading gains credibility. If surface compliance persists with underground continuation, the exogenous override reading''s claim holds: decree is sufficient for administrative coordination but insufficient for behavioral displacement. This determines long-term stability: internalized practices survive enforcement erosion; surface-only compliance collapses.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(internalization_vs_surface_compliance, empirical, 'Whether displacement is real behavioral change or sustained only by enforcement.').

omega_variable(
    competing_authority_suppression,
    'Is the constraint sustained by genuine state authority that overrides prior practice, or is it sustained by suppression of competing legitimacy claims that would challenge the state''s right to override?',
    'Comparative analysis: remove enforcement (or observe rare zones where enforcement is weak); does the prior practice reassert through competing institutions (religious, customary, land-based)? Are competing authorities prevented from public operation, or do they simply choose not to contest?',
    'If competing authorities are genuinely suppressed and would reassert if suppression lifted, the constraint is maintained by coercion of legitimacy claims themselves—extraction is higher than the beneficiary framing suggests. If competing authorities are simply sidelined but could function if permitted, the choice to exclude them is structural to the constraint''s design.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(competing_authority_suppression, empirical, 'Whether state authority overrides prior practice or suppresses the institutions that claim prior authority.').

omega_variable(
    adjustment_cost_internalization_asymmetry,
    'Do rural populations eventually internalize the adjustment costs (accept them as reasonable or necessary) or do they permanently experience them as imposed burden?',
    'Generational surveys of rural populations'' own assessment of the practice change; whether second/third generation members report the new practice as ''natural'' or continue to frame it as ''the state''s imposition''; comparison with populations that adopted practices endogenously.',
    'If costs are internalized, the constraint transitions from tangled_rope (coordination + asymmetric extraction) toward rope (pure coordination)—the endogenous_climb reading gains support. If costs remain permanent burden, the constraint sustains as tangled_rope or snare-variant indefinitely, dependent on continuous suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(adjustment_cost_internalization_asymmetry, empirical, 'Whether adjustment costs are eventually accepted or permanently experienced as extraction.').

omega_variable(
    kernel_reading_foreclosure,
    'Is the exogenous override reading logically coherent with endogenous_climb, or do their core premises directly contradict in a way that makes both simultaneously true impossible?',
    'Formal analysis of the two premises: (1) decree authority is sufficient (exogenous override) vs. (2) internalization is necessary (endogenous_climb). Can a system be in a state where decree-driven compliance persists indefinitely alongside ongoing non-internalization? Or does the exogenous override reading''s claim require that internalization is NOT necessary—foreclosing the endogenous_climb''s core premise?',
    'If the premises are truly contradictory, the readings coexist_with is incorrect and should be forecloses. If both can be true (decree is sufficient for administrative compliance but insufficient for internalization), then coexists_with is the right relation and the measurement series showing plateaued suppression-dependent compliance supports both readings simultaneously.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure, conceptual, 'Whether the exogenous override and endogenous climb readings can logically coexist or whether one forecloses the other.').

omega_variable(
    state_authority_source,
    'Does the state''s authority to override prior practice derive from legal/administrative power alone, or does it depend on cultural legitimacy that itself derives from prior acceptance of state authority?',
    'Historical analysis of the state''s legitimacy sources: is the decree issued by an authority the population already recognized before the decree, or is the decree the STATE''S FIRST CLAIM to authority over this domain? Does the state''s authority to issue the decree rest on something independent of the prior practice, or is it parasitic on acceptance of state authority in OTHER domains?',
    'If state authority is independent, the exogenous override reading''s premise holds at face value: decree is structurally sufficient. If state authority depends on prior acceptance in other domains, the reading requires implicit endogenous acceptance of state authority, which partially undermines the claim that decree alone is sufficient.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(state_authority_source, conceptual, 'Whether state authority is truly exogenous or depends on prior endogenous acceptance of state legitimacy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimacy_of_imposed_practice__exogenous_override_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legi_tr_t0, legitimacy_of_imposed_practice__exogenous_override_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(legi_tr_t5, legitimacy_of_imposed_practice__exogenous_override_reading, theater_ratio, 5, 0.23).
narrative_ontology:measurement(legi_tr_t10, legitimacy_of_imposed_practice__exogenous_override_reading, theater_ratio, 10, 0.29).
narrative_ontology:measurement(legi_tr_t15, legitimacy_of_imposed_practice__exogenous_override_reading, theater_ratio, 15, 0.34).
narrative_ontology:measurement(legi_tr_t20, legitimacy_of_imposed_practice__exogenous_override_reading, theater_ratio, 20, 0.38).
narrative_ontology:measurement(legi_tr_t25, legitimacy_of_imposed_practice__exogenous_override_reading, theater_ratio, 25, 0.4).
narrative_ontology:measurement(legi_tr_t30, legitimacy_of_imposed_practice__exogenous_override_reading, theater_ratio, 30, 0.41).
narrative_ontology:measurement(legi_tr_t40, legitimacy_of_imposed_practice__exogenous_override_reading, theater_ratio, 40, 0.42).

% Extraction over time
narrative_ontology:measurement(legi_be_t0, legitimacy_of_imposed_practice__exogenous_override_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(legi_be_t5, legitimacy_of_imposed_practice__exogenous_override_reading, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(legi_be_t10, legitimacy_of_imposed_practice__exogenous_override_reading, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(legi_be_t15, legitimacy_of_imposed_practice__exogenous_override_reading, base_extractiveness, 15, 0.62).
narrative_ontology:measurement(legi_be_t20, legitimacy_of_imposed_practice__exogenous_override_reading, base_extractiveness, 20, 0.65).
narrative_ontology:measurement(legi_be_t25, legitimacy_of_imposed_practice__exogenous_override_reading, base_extractiveness, 25, 0.67).
narrative_ontology:measurement(legi_be_t30, legitimacy_of_imposed_practice__exogenous_override_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement(legi_be_t40, legitimacy_of_imposed_practice__exogenous_override_reading, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(legi_su_t0, legitimacy_of_imposed_practice__exogenous_override_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(legi_su_t5, legitimacy_of_imposed_practice__exogenous_override_reading, suppression_requirement, 5, 0.65).
narrative_ontology:measurement(legi_su_t10, legitimacy_of_imposed_practice__exogenous_override_reading, suppression_requirement, 10, 0.71).
narrative_ontology:measurement(legi_su_t15, legitimacy_of_imposed_practice__exogenous_override_reading, suppression_requirement, 15, 0.75).
narrative_ontology:measurement(legi_su_t20, legitimacy_of_imposed_practice__exogenous_override_reading, suppression_requirement, 20, 0.77).
narrative_ontology:measurement(legi_su_t25, legitimacy_of_imposed_practice__exogenous_override_reading, suppression_requirement, 25, 0.78).
narrative_ontology:measurement(legi_su_t30, legitimacy_of_imposed_practice__exogenous_override_reading, suppression_requirement, 30, 0.79).
narrative_ontology:measurement(legi_su_t40, legitimacy_of_imposed_practice__exogenous_override_reading, suppression_requirement, 40, 0.79).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimacy_of_imposed_practice__exogenous_override_reading, global_infrastructure).
narrative_ontology:boltzmann_floor_override(legitimacy_of_imposed_practice__exogenous_override_reading, 0.25).
narrative_ontology:affects_constraint(legitimacy_of_imposed_practice__exogenous_override_reading, legitimacy_of_imposed_practice__endogenous_climb_reading).
narrative_ontology:affects_constraint(legitimacy_of_imposed_practice__exogenous_override_reading, legitimacy_of_imposed_practice__hybrid_scaffolding_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the kernel 'legitimacy_of_imposed_practice' — the claim that state decree authority can displace prior practice. The kernel is contested across three distinct readings, each with different structural premises about how displacement occurs (exogenous override vs. endogenous adoption vs. scaffolded hybrid). All three readings are linked via network.affects_constraints to show the family relationship. The ε values differ substantially: exogenous override shows moderately high extractiveness (0.68) because displacement is incomplete and theater-dependent; endogenous_climb shows lower extractiveness (the reading predicts complete displacement requires internalization, so partial displacement signals reading failure); hybrid_scaffolding shows intermediate extractiveness (partial displacement via ideological messaging). Each reading has its own beneficiary/victim structure reflecting its premise about HOW displacement occurs.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
