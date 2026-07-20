% ============================================================================
% CONSTRAINT STORY: border_normative_status__freedom_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_border_normative_status__freedom_primary, []).

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
 *   constraint_id: border_normative_status__freedom_primary
 *   human_readable: Freedom of Movement as Fundamental Right (Freedom-Primary Reading)
 *   domain: political/international_law/migration
 *
 * SUMMARY:
 *   This constraint story instantiates the freedom-primary reading of the
 *   border_normative_status kernel: the claim that freedom of movement is a
 *   fundamental human right and that territorial borders are impermissible
 *   restrictions requiring extraordinary justification. Under this reading,
 *   the traditional victim of border regimesâthe excluded migrantâis
 *   reclassified as a rights-holder who cannot legitimately be victimized by
 *   exclusion. Conversely, domestic workers in destination countries who face
 *   labor-market displacement enter the victim set. The constraint is the
 *   international human rights norm itself, administered by treaty bodies and
 *   enforced through legal challenge against state exclusion. It coordinates
 *   global mobility and refugee protection while extracting from settled
 *   labor populations and sovereign state autonomy.
 *
 * KEY AGENTS:
 *   - Migrants and asylum seekers: Primary beneficiaries (powerless/constrained) â gain a standing right to move and reduced burden of justification.
 *   - Displaced domestic workers: Primary targets (moderate/constrained) â bear labor-market competition and wage compression.
 *   - Human rights treaty bodies: Agenda-setters (institutional/analytical) â interpret and enforce the norm against states.
 *   - Destination states: Payers (institutional/constrained) â lose default exclusion authority and face legal challenge.
 *   - Sovereigntist advocates: Excluded voices (organized/constrained) â argue for state border authority but are marginalized in human rights discourse.
 *   - Migration economists: Observers (analytical/analytical) â provide empirical evidence on labor-market effects without normative commitment.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(border_normative_status__freedom_primary, 0.58).
domain_priors:suppression_score(border_normative_status__freedom_primary, 0.62).
domain_priors:theater_ratio(border_normative_status__freedom_primary, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(border_normative_status__freedom_primary, extractiveness, 0.58).
narrative_ontology:constraint_metric(border_normative_status__freedom_primary, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(border_normative_status__freedom_primary, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(border_normative_status__freedom_primary, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(border_normative_status__freedom_primary, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(border_normative_status__freedom_primary, tangled_rope).
narrative_ontology:human_readable(border_normative_status__freedom_primary, "Freedom of Movement as Fundamental Right (Freedom-Primary Reading)").
narrative_ontology:topic_domain(border_normative_status__freedom_primary, "political/international_law/migration").

domain_priors:requires_active_enforcement(border_normative_status__freedom_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(border_normative_status__freedom_primary, 'a3278332-f31b-4c4b-a401-2d4a9899f475').
narrative_ontology:cs_kernel_codification('a3278332-f31b-4c4b-a401-2d4a9899f475', formalized).
narrative_ontology:cs_authority_grounding('a3278332-f31b-4c4b-a401-2d4a9899f475', lineage).
narrative_ontology:cs_interpretation_layer_present('a3278332-f31b-4c4b-a401-2d4a9899f475').
narrative_ontology:cs_reading_relation('a3278332-f31b-4c4b-a401-2d4a9899f475', border_normative_status__sovereignty_primary, forecloses).
narrative_ontology:cs_reading_relation('a3278332-f31b-4c4b-a401-2d4a9899f475', border_normative_status__qualified_sovereignty, influences).
narrative_ontology:cs_axiom('a3278332-f31b-4c4b-a401-2d4a9899f475', foundational, freedom_of_movement_fundamental).
narrative_ontology:cs_axiom_status(freedom_of_movement_fundamental, holdable).
narrative_ontology:cs_axiom_grounding('a3278332-f31b-4c4b-a401-2d4a9899f475', freedom_of_movement_fundamental, deontological).
narrative_ontology:cs_axiom('a3278332-f31b-4c4b-a401-2d4a9899f475', foundational, extraordinary_justification_for_exclusion).
narrative_ontology:cs_axiom_status(extraordinary_justification_for_exclusion, holdable).
narrative_ontology:cs_axiom_grounding('a3278332-f31b-4c4b-a401-2d4a9899f475', extraordinary_justification_for_exclusion, deontological).
narrative_ontology:cs_reference_frame('a3278332-f31b-4c4b-a401-2d4a9899f475', universal_mobility_default).
narrative_ontology:cs_drift_state('a3278332-f31b-4c4b-a401-2d4a9899f475', contemporary_border_regime_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('a3278332-f31b-4c4b-a401-2d4a9899f475', '').
narrative_ontology:cs_kernel_id(border_normative_status__freedom_primary, border_normative_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(border_normative_status__freedom_primary, migrants_and_asylum_seekers).
narrative_ontology:constraint_victim(border_normative_status__freedom_primary, displaced_domestic_workers).
narrative_ontology:constraint_victim(border_normative_status__freedom_primary, destination_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Seek entry or residence in destination countries on the basis of a claimed fundamental right to move; the norm shifts the burden of justification to the state, but they remain subject to border enforcement, detention, and procedural delay while claims are processed.
narrative_ontology:constraint_stakeholder(border_normative_status__freedom_primary, migrants_and_asylum_seekers, beneficiary,
    powerless, immediate, constrained, global).

% Low- and medium-skilled workers in destination-country labor markets who face wage compression and employment displacement when migration volumes rise; they bear the diffuse economic cost of the mobility right but are not party to the human rights discourse that authorizes it.
narrative_ontology:constraint_stakeholder(border_normative_status__freedom_primary, displaced_domestic_workers, payer,
    moderate, biographical, constrained, national).

% Interpret and enforce international human rights instruments that codify freedom of movement; they issue rulings, general comments, and oversight reports that constrain state exclusion prerogatives and define what counts as extraordinary justification.
narrative_ontology:constraint_stakeholder(border_normative_status__freedom_primary, human_rights_treaty_bodies, agenda_setter,
    institutional, generational, analytical, global).

% Sovereign states that lose default authority to exclude non-citizens; they must provide extraordinary justification for every border restriction and face legal challenge before international tribunals when they restrict movement.
narrative_ontology:constraint_stakeholder(border_normative_status__freedom_primary, destination_states, payer,
    institutional, generational, constrained, national).

% Political movements and legal scholars who argue that collective self-determination requires unqualified state border authority; their arguments are treated as illegitimate or retrograde in human-rights-framed discourse and are structurally absent from treaty-body deliberations.
narrative_ontology:constraint_stakeholder(border_normative_status__freedom_primary, sovereigntist_advocates, excluded,
    organized, generational, constrained, national).

% Study the labor-market and fiscal effects of migration regimes; they provide evidence on wage effects and fiscal transfers that is cited by both treaty bodies and restriction advocates without being structurally committed to either normative frame.
narrative_ontology:constraint_stakeholder(border_normative_status__freedom_primary, migration_economists, observer,
    analytical, biographical, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents arbitrary territorial imprisonment by accident of birth; coordinates a global regime where individuals may cross borders to escape persecution, reunite families, and seek opportunity without needing a state's prior permission.
% TRANSFER_FUNCTION: Transfers the burden of justification from the individual seeker to the excluding state; shifts labor-market risk and wage compression from mobile capital and migrants to settled domestic workers in receiving societies; transfers regulatory authority from state border agencies to international human rights tribunals.
% ABSENT_VOICES: Sovereigntist legal scholars and domestic labor unions advocating restrictive immigration are structurally absent from human-rights treaty discourse; their claims are pre-categorized as discrimination or economic nostalgia rather than as rights-based arguments.
% DISAPPEARANCE_RATIONALE: If the freedom-primary norm vanished overnight, states would revert to presumptive exclusion authority, international refugee and family-reunification pipelines would collapse, and labor markets in wealthy countries would reconfigure around closed rather than open borders.
% FOUNDING_PROBLEM: The twentieth-century catastrophes of statelessness, forced displacement, and arbitrary territorial imprisonmentâcrystallized by the Holocaust and the post-WWII refugee crisisâdemonstrated that unlimited state exclusion power is lethal and that individuals need a standing right to move and seek safety.
% FOUNDING_PROBLEM_CORROBORATION: Historians of the Holocaust and refugee law scholars outside the immediate human-rights institutional beneficiary set corroborate the founding problem's reality. However, labor economists and political scientists attesting from outside the beneficiary set argue the norm has drifted from emergency protection to a general mobility entitlement that no longer tracks the original catastrophe, corroborating the dead/drifted reading.
narrative_ontology:disappearance_verdict(border_normative_status__freedom_primary, world_rearranges).
narrative_ontology:founding_problem_status(border_normative_status__freedom_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(border_normative_status__freedom_primary, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(border_normative_status__freedom_primary, 'none', 1).
narrative_ontology:epsilon_provenance(border_normative_status__freedom_primary, 0.58, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(border_normative_status__freedom_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(border_normative_status__freedom_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(border_normative_status__freedom_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.58) because the norm imposes real costs on domestic workers and constrains state self-governance, but it also delivers genuine coordination (refugee protection, family reunification). Suppression (0.62) reflects the active suppression of sovereigntist alternatives in international legal discourse. Theater ratio (0.42) captures the growing performative gap between human rights rhetoric and actual border enforcement. Resistance is high (0.75) because state and populist resistance to the norm is vigorous and growing. Accessibility collapse (0.55) indicates that within the human rights framework, alternatives to open mobility are treated as illegitimate, though outside the framework they remain vibrant.
 *
 * PERSPECTIVAL GAP:
 *   The treaty-body seat and the migrant seat compute the constraint as protective coordination; the domestic-worker seat and the destination-state seat compute it as extraction. The engine derives this divergence from the structural data: identical border rules produce opposite directionality depending on whether the agent gains mobility rights or loses labor-market security and sovereign control.
 *
 * DIRECTIONALITY LOGIC:
 *   Migrants and asylum seekers are structural beneficiaries (d near 0.0) because the constraint subsidizes their mobility claims. Human rights treaty bodies sit near the beneficiary end as agenda-setters administering the norm. Displaced domestic workers and destination states are structural targets (d near 1.0) because they bear the labor-market and sovereignty costs without collecting offsetting benefits. Sovereigntist advocates are excluded entirelyâtheir exclusion is constitutive of the norm's operation.
 *
 * MANDATROPHY ANALYSIS:
 *   The norm's founding problemâstatelessness and arbitrary exclusion in the mid-twentieth centuryâis arguably solved or transformed: most contemporary mobility is economic, not refugee-flight. Yet the arrangement persists and has expanded. The mismatch between founding_problem_status (contested) and disappearance_verdict (world_rearranges) flags potential mandatrophy: the norm may be a zombie scaffold or tangled rope whose coordination function has atrophied while its extractive effects on domestic labor have grown.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_right,
    'Is freedom of movement a natural-law constraint that would persist without institutional enforcement, or a constructed norm that depends on treaty-body maintenance?',
    'Historical comparison of border regimes before and after the human rights framework; observation of whether the norm persists in the absence of treaty-body enforcement.',
    'If natural-law, classification trends toward mountain (though beneficiary presence would trigger FSM); if constructed and extractive, classification stays tangled_rope or snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_right, conceptual, 'Whether the right to free movement is discovered or constructed.').

omega_variable(
    labor_market_extraction_mechanism,
    'Does the freedom-primary norm extract from domestic workers through labor-market displacement, or is the economic cost a side effect of a non-extractive coordination mechanism?',
    'Comparative labor-market analysis of sectors with high migrant inflows versus closed-border counterfactuals; natural experiments from sudden border openings or closures.',
    'If the cost to domestic workers is systematic and large, the norm operates as tangled_rope or snare; if negligible or incidental, it is closer to rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(labor_market_extraction_mechanism, empirical, 'Whether labor market harm is constitutive or incidental.').

omega_variable(
    domestic_worker_voice_exclusion,
    'Are displaced domestic workers structurally excluded from the human rights discourse that authorizes mobility, or do they have effective standing to challenge the norm?',
    'Trace participatory rights and amicus standing before human rights bodies for labor unions and domestic-worker advocates; observe whether their claims are treated as rights-based or as mere policy preferences.',
    'If systematically excluded, the constraint shows higher suppression; if included, the coordination function is more symmetric.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(domestic_worker_voice_exclusion, empirical, 'Whether domestic worker voices are structurally excluded.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(border_normative_status__freedom_primary, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bnf_freedom_tr_t0, border_normative_status__freedom_primary, theater_ratio, 0, 0.1).
narrative_ontology:measurement(bnf_freedom_tr_t15, border_normative_status__freedom_primary, theater_ratio, 15, 0.2).
narrative_ontology:measurement(bnf_freedom_tr_t30, border_normative_status__freedom_primary, theater_ratio, 30, 0.28).
narrative_ontology:measurement(bnf_freedom_tr_t45, border_normative_status__freedom_primary, theater_ratio, 45, 0.35).
narrative_ontology:measurement(bnf_freedom_tr_t60, border_normative_status__freedom_primary, theater_ratio, 60, 0.4).
narrative_ontology:measurement(bnf_freedom_tr_t75, border_normative_status__freedom_primary, theater_ratio, 75, 0.42).

% Extraction over time
narrative_ontology:measurement(bnf_freedom_be_t0, border_normative_status__freedom_primary, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(bnf_freedom_be_t15, border_normative_status__freedom_primary, base_extractiveness, 15, 0.32).
narrative_ontology:measurement(bnf_freedom_be_t30, border_normative_status__freedom_primary, base_extractiveness, 30, 0.4).
narrative_ontology:measurement(bnf_freedom_be_t45, border_normative_status__freedom_primary, base_extractiveness, 45, 0.48).
narrative_ontology:measurement(bnf_freedom_be_t60, border_normative_status__freedom_primary, base_extractiveness, 60, 0.52).
narrative_ontology:measurement(bnf_freedom_be_t75, border_normative_status__freedom_primary, base_extractiveness, 75, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(bnf_freedom_su_t0, border_normative_status__freedom_primary, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(bnf_freedom_su_t15, border_normative_status__freedom_primary, suppression_requirement, 15, 0.4).
narrative_ontology:measurement(bnf_freedom_su_t30, border_normative_status__freedom_primary, suppression_requirement, 30, 0.5).
narrative_ontology:measurement(bnf_freedom_su_t45, border_normative_status__freedom_primary, suppression_requirement, 45, 0.55).
narrative_ontology:measurement(bnf_freedom_su_t60, border_normative_status__freedom_primary, suppression_requirement, 60, 0.6).
narrative_ontology:measurement(bnf_freedom_su_t75, border_normative_status__freedom_primary, suppression_requirement, 75, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(border_normative_status__freedom_primary, border_normative_status__qualified_sovereignty).
narrative_ontology:affects_constraint(border_normative_status__freedom_primary, border_normative_status__sovereignty_primary).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the border_normative_status kernel; sibling readings instantiate structurally distinct constraints from the same contested kernel, linked by mutual influence in international legal discourse.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
