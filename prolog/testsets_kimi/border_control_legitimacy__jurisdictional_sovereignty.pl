% ============================================================================
% CONSTRAINT STORY: border_control_legitimacy__jurisdictional_sovereignty
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_border_control_legitimacy__jurisdictional_sovereignty, []).

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
 *   constraint_id: border_control_legitimacy__jurisdictional_sovereignty
 *   human_readable: Jurisdictional Sovereignty with Proportionate Border Enforcement
 *   domain: political/international_law/migration
 *
 * SUMMARY:
 *   This constraint instantiates the jurisdictional_sovereignty reading of
 *   the contested border_control_legitimacy kernel. Under this reading, state
 *   sovereignty is understood as jurisdictional authorityâthe power to
 *   regulate rights and obligations within territoryâwithout necessarily
 *   entailing an absolute authority to close borders. The constraint
 *   coordinates territorial governance by requiring a balance between
 *   protection obligations, labor needs, and public consent. However, it also
 *   extracts asymmetrically: excluded migrants bear the direct costs of
 *   border enforcement, while displaced citizens bear the concentrated
 *   domestic costs of enforcement infrastructure and labor-market imbalances.
 *   The arrangement requires active enforcement constrained by
 *   proportionality and necessity tests, yet faces a structural legitimacy
 *   crisis when enforcement violates human rights or when admission levels
 *   undermine public consent. The constraint is claimed as a necessary
 *   coordination mechanism for territorial political order, but the metrics
 *   describe a tangled structure where genuine coordination coexists with
 *   identifiable victimization.
 *
 * KEY AGENTS:
 *   - state_apparatus: Primary agenda-setter (institutional/constrained) â administers border enforcement and jurisdictional authority
 *   - citizen_electorate: Primary beneficiary (organized/constrained) â provides public consent and receives protection/labor balance
 *   - excluded_migrants: Primary transnational target (powerless/trapped) â bears direct exclusion costs
 *   - displaced_citizens: Secondary domestic target (moderate/constrained) â bears enforcement and labor-dislocation costs
 *   - human_rights_monitoring_bodies: Analytical observer (institutional/analytical) â assesses proportionality compliance
 *   - labor_dependent_employers: Excluded voice (powerful/constrained) â marginalized when public consent favors restriction
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(border_control_legitimacy__jurisdictional_sovereignty, 0.62).
domain_priors:suppression_score(border_control_legitimacy__jurisdictional_sovereignty, 0.55).
domain_priors:theater_ratio(border_control_legitimacy__jurisdictional_sovereignty, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(border_control_legitimacy__jurisdictional_sovereignty, extractiveness, 0.62).
narrative_ontology:constraint_metric(border_control_legitimacy__jurisdictional_sovereignty, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(border_control_legitimacy__jurisdictional_sovereignty, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(border_control_legitimacy__jurisdictional_sovereignty, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(border_control_legitimacy__jurisdictional_sovereignty, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(border_control_legitimacy__jurisdictional_sovereignty, tangled_rope).
narrative_ontology:human_readable(border_control_legitimacy__jurisdictional_sovereignty, "Jurisdictional Sovereignty with Proportionate Border Enforcement").
narrative_ontology:topic_domain(border_control_legitimacy__jurisdictional_sovereignty, "political/international_law/migration").

domain_priors:requires_active_enforcement(border_control_legitimacy__jurisdictional_sovereignty).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(border_control_legitimacy__jurisdictional_sovereignty, '1e16a0a2-994a-4af2-b085-dcc42e011e91').
narrative_ontology:cs_kernel_codification('1e16a0a2-994a-4af2-b085-dcc42e011e91', formalized).
narrative_ontology:cs_authority_grounding('1e16a0a2-994a-4af2-b085-dcc42e011e91', lineage).
narrative_ontology:cs_interpretation_layer_present('1e16a0a2-994a-4af2-b085-dcc42e011e91').
narrative_ontology:cs_reading_relation('1e16a0a2-994a-4af2-b085-dcc42e011e91', border_control_legitimacy__sovereignty_primary, forecloses).
narrative_ontology:cs_reading_relation('1e16a0a2-994a-4af2-b085-dcc42e011e91', border_control_legitimacy__freedom_of_movement_primary, coexists_with).
narrative_ontology:cs_axiom('1e16a0a2-994a-4af2-b085-dcc42e011e91', foundational, jurisdictional_authority_without_closure).
narrative_ontology:cs_axiom_status(jurisdictional_authority_without_closure, holdable).
narrative_ontology:cs_axiom_grounding('1e16a0a2-994a-4af2-b085-dcc42e011e91', jurisdictional_authority_without_closure, conventional).
narrative_ontology:cs_axiom('1e16a0a2-994a-4af2-b085-dcc42e011e91', foundational, proportionality_as_legitimacy_condition).
narrative_ontology:cs_axiom_status(proportionality_as_legitimacy_condition, holdable).
narrative_ontology:cs_axiom_grounding('1e16a0a2-994a-4af2-b085-dcc42e011e91', proportionality_as_legitimacy_condition, deontological).
narrative_ontology:cs_reference_frame('1e16a0a2-994a-4af2-b085-dcc42e011e91', westphalian_jurisdictional_order).
narrative_ontology:cs_drift_state('1e16a0a2-994a-4af2-b085-dcc42e011e91', contemporary_migration_crisis_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('1e16a0a2-994a-4af2-b085-dcc42e011e91', '').
narrative_ontology:cs_kernel_id(border_control_legitimacy__jurisdictional_sovereignty, border_control_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(border_control_legitimacy__jurisdictional_sovereignty, citizen_electorate).
narrative_ontology:constraint_victim(border_control_legitimacy__jurisdictional_sovereignty, excluded_migrants).
narrative_ontology:constraint_victim(border_control_legitimacy__jurisdictional_sovereignty, displaced_citizens).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers territorial jurisdiction and border enforcement apparatus. Claims authority from international legal lineage and constitutional mandate. Subject to proportionality and necessity tests in principle, but retains agenda-setting power over how those tests are interpreted, resourced, and enforced in practice.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__jurisdictional_sovereignty, state_apparatus, agenda_setter,
    institutional, generational, constrained, national).

% Provides public consent for the jurisdictional order. Receives protection obligations, labor market regulation, and the diffuse goods of territorial membership. Bears indirect costs when enforcement violates rights or when labor shortages emerge from over-restriction.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__jurisdictional_sovereignty, citizen_electorate, beneficiary,
    organized, biographical, constrained, national).

% Subject to border closure and enforcement despite the reading's claim that sovereignty does not necessarily include closure authority. Barred from territorial rights and obligations. Bear the direct cost of exclusion, including detention, deportation, and denial of mobility.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__jurisdictional_sovereignty, excluded_migrants, payer,
    powerless, immediate, trapped, global).

% Citizens who bear concentrated domestic costs of the enforcement-labor balance: communities hosting detention infrastructure, workers facing labor market disruption from either excessive restriction or unplanned admission, and civil society actors whose rights are eroded by militarized border practices.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__jurisdictional_sovereignty, displaced_citizens, payer,
    moderate, biographical, constrained, national).

% Assess whether border enforcement complies with proportionality, necessity, and basic human rights standards. Publish findings that can delegitimize enforcement practices but lack direct enforcement power over sovereign states.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__jurisdictional_sovereignty, human_rights_monitoring_bodies, observer,
    institutional, civilizational, analytical, global).

% Require migrant labor for economic operations. Structurally excluded from the legitimacy calculus when public consent tilts toward restriction, despite the reading's formal requirement to balance labor needs against protection and consent.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__jurisdictional_sovereignty, labor_dependent_employers, excluded,
    powerful, biographical, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a territorial framework for regulating rights and obligations among a population, balancing state protection obligations against economic labor needs and maintaining public consent for the political order.
% TRANSFER_FUNCTION: Moves the authority to regulate membership and territorial presence from the global plane to the state, and moves the concentrated costs of enforcement and exclusion from the state apparatus to excluded migrants and displaced citizen communities.
% ABSENT_VOICES: Excluded migrants are physically absent from the polity and thus from consent mechanisms; labor-dependent employers and humanitarian actors are often marginalized when public consent tilts toward restriction; future generations who will inherit the demographic and fiscal consequences of labor imbalances are unrepresented.
% DISAPPEARANCE_RATIONALE: If the constraint vanished, the distinction between jurisdictional authority and border closure would dissolve into either absolute state discretion or free movement, fundamentally reorganizing the international state system, labor markets, and the legal status of millions of migrants.
% FOUNDING_PROBLEM: The need to establish a stable, territorially bounded political authority capable of guaranteeing rights and obligations to a specific population while managing cross-border mobility.
% FOUNDING_PROBLEM_CORROBORATION: International legal scholars and constitutional historians attest to the problem's historical reality, but human rights monitoring bodies and migration researchers contest that the current arrangement solves it rather than deferring its costs to the powerless. No fully external corroboration exists: even critics operate within the Westphalian framework they seek to reform.
narrative_ontology:disappearance_verdict(border_control_legitimacy__jurisdictional_sovereignty, world_rearranges).
narrative_ontology:founding_problem_status(border_control_legitimacy__jurisdictional_sovereignty, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(border_control_legitimacy__jurisdictional_sovereignty, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(border_control_legitimacy__jurisdictional_sovereignty, 'none', 1).
narrative_ontology:epsilon_provenance(border_control_legitimacy__jurisdictional_sovereignty, 0.62, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(border_control_legitimacy__jurisdictional_sovereignty_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(border_control_legitimacy__jurisdictional_sovereignty, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(border_control_legitimacy__jurisdictional_sovereignty_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) reflects substantial but bounded extraction: the constraint deliberately limits itself via proportionality tests and human rights norms, yet still systematically excludes and displaces. Suppression (0.55) is moderate because enforcement is active but legally constrained; it does not reach snare levels because alternatives (legal challenge, some mobility) are not fully collapsed. Theater ratio (0.40) captures the performative dimension of border enforcementâsovereignty signaling that exceeds functional security needsâwithout reducing the entire constraint to theater. Accessibility collapse (0.45) is moderate: open-border and closed-border alternatives remain thinkable and institutionally contested. Resistance (0.58) is substantial because the constraint faces continuous legal challenge, migrant contestation, and democratic backlash. The measurement series share one time grid so every metric is authored at every examined time point.
 *
 * PERSPECTIVAL GAP:
 *   From the state_apparatus seat, the constraint appears as a necessary and legally bounded coordination mechanism. From the excluded_migrants seat, it appears as enforced exclusion dressed in legal formality. From the displaced_citizens seat, it appears as a political order that externalizes enforcement costs onto their communities while denying them effective voice. The engine computes these divergences from the structural data rather than adjudicating them.
 *
 * DIRECTIONALITY LOGIC:
 *   The state_apparatus sets the agenda and claims legitimacy through lineage authority, sitting near the beneficiary end of directionality for its own institutional continuity, though it does not directly collect material rents. The citizen_electorate receives the coordination benefits of territorial order and the diffuse gains of labor-market regulation, placing them at the beneficiary end. Excluded_migrants are full targets: they are structurally trapped by border enforcement and bear the highest extraction. Displaced_citizens are intermediate targets: they possess formal political rights but are powerless to alter the enforcement-labor balance that harms them.
 *
 * MANDATROPHY ANALYSIS:
 *   The Tangled Rope classification prevents mislabeling this arrangement as pure coordination (Rope) because dual victim sets are structurally acknowledged and active enforcement is required. It prevents mislabeling as pure extraction (Snare) because the coordination functionâterritorial jurisdictional orderâis genuine and not merely cover. The proportionality and necessity tests, even if partially theatrical, are not reducible to extraction: their presence distinguishes this from a closed snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_boundary,
    'How would the classification of this constraint change if the sovereignty_primary or freedom_of_movement_primary reading of the border_control_legitimacy kernel were adopted instead?',
    'Cross-reading comparison: sovereignty_primary would eliminate proportionality constraints and consolidate extraction on excluded migrants (snare-ward drift); freedom_of_movement_primary would constrain border regulation authority and shift citizen_electorate toward payer status (rope-ward or scaffold-ward drift).',
    'Determines whether the dual-victim structure and contested legitimacy are artifacts of this reading or structurally necessary to the kernel.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_boundary, conceptual, 'Sibling reading structural delta for jurisdictional sovereignty').

omega_variable(
    proportionality_enforcement_gap,
    'Do proportionality and necessity tests actually constrain enforcement, or do they function as post-hoc legitimation theater?',
    'Empirical audit of judicial review outcomes in border enforcement cases: high rates of deferential review suggest theater; high rates of annulment suggest genuine constraint.',
    'If theater, the constraint''s extraction is higher than modeled and its coordination function weaker; if genuine, the tangled_rope classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proportionality_enforcement_gap, empirical, 'Whether legal proportionality tests are functional or performative').

omega_variable(
    public_consent_crisis_threshold,
    'At what threshold does admission undermine public consent sufficiently to trigger a legitimacy crisis, and is this threshold structurally knowable?',
    'Comparative analysis of electoral backlash, policy reversals, and social cohesion metrics across jurisdictions with varying admission levels.',
    'If the threshold is indeterminate, the constraint carries an irreducible legitimacy instability that amplifies extraction through cyclical enforcement tightening.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(public_consent_crisis_threshold, preference, 'Indeterminacy of the public consent threshold').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(border_control_legitimacy__jurisdictional_sovereignty, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bcl_js_tr_t0, border_control_legitimacy__jurisdictional_sovereignty, theater_ratio, 0, 0.2).
narrative_ontology:measurement(bcl_js_tr_t6, border_control_legitimacy__jurisdictional_sovereignty, theater_ratio, 6, 0.28).
narrative_ontology:measurement(bcl_js_tr_t12, border_control_legitimacy__jurisdictional_sovereignty, theater_ratio, 12, 0.35).
narrative_ontology:measurement(bcl_js_tr_t18, border_control_legitimacy__jurisdictional_sovereignty, theater_ratio, 18, 0.48).
narrative_ontology:measurement(bcl_js_tr_t24, border_control_legitimacy__jurisdictional_sovereignty, theater_ratio, 24, 0.45).
narrative_ontology:measurement(bcl_js_tr_t30, border_control_legitimacy__jurisdictional_sovereignty, theater_ratio, 30, 0.4).

% Extraction over time
narrative_ontology:measurement(bcl_js_be_t0, border_control_legitimacy__jurisdictional_sovereignty, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(bcl_js_be_t6, border_control_legitimacy__jurisdictional_sovereignty, base_extractiveness, 6, 0.52).
narrative_ontology:measurement(bcl_js_be_t12, border_control_legitimacy__jurisdictional_sovereignty, base_extractiveness, 12, 0.6).
narrative_ontology:measurement(bcl_js_be_t18, border_control_legitimacy__jurisdictional_sovereignty, base_extractiveness, 18, 0.68).
narrative_ontology:measurement(bcl_js_be_t24, border_control_legitimacy__jurisdictional_sovereignty, base_extractiveness, 24, 0.65).
narrative_ontology:measurement(bcl_js_be_t30, border_control_legitimacy__jurisdictional_sovereignty, base_extractiveness, 30, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(bcl_js_su_t0, border_control_legitimacy__jurisdictional_sovereignty, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(bcl_js_su_t6, border_control_legitimacy__jurisdictional_sovereignty, suppression_requirement, 6, 0.45).
narrative_ontology:measurement(bcl_js_su_t12, border_control_legitimacy__jurisdictional_sovereignty, suppression_requirement, 12, 0.55).
narrative_ontology:measurement(bcl_js_su_t18, border_control_legitimacy__jurisdictional_sovereignty, suppression_requirement, 18, 0.68).
narrative_ontology:measurement(bcl_js_su_t24, border_control_legitimacy__jurisdictional_sovereignty, suppression_requirement, 24, 0.62).
narrative_ontology:measurement(bcl_js_su_t30, border_control_legitimacy__jurisdictional_sovereignty, suppression_requirement, 30, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(border_control_legitimacy__jurisdictional_sovereignty, enforcement_mechanism).
narrative_ontology:affects_constraint(border_control_legitimacy__jurisdictional_sovereignty, border_control_legitimacy__sovereignty_primary).
narrative_ontology:affects_constraint(border_control_legitimacy__jurisdictional_sovereignty, border_control_legitimacy__freedom_of_movement_primary).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the border_control_legitimacy kernel, decomposed per the Îµ-invariance principle. The jurisdictional_sovereignty reading is structurally distinct from the sovereignty_primary reading (which treats closure as constitutive) and the freedom_of_movement_primary reading (which treats movement as fundamental). Each reading carries its own Îµ, stakeholders, and classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
