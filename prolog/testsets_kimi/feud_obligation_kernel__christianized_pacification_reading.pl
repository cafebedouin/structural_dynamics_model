% ============================================================================
% CONSTRAINT STORY: feud_obligation_kernel__christianized_pacification_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_feud_obligation_kernel__christianized_pacification_reading, []).

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
 *   constraint_id: feud_obligation_kernel__christianized_pacification_reading
 *   human_readable: Christianized Pacification Prohibition on Blood Feud
 *   domain: legal_anthropology/medieval_history/comparative_political_systems
 *
 * SUMMARY:
 *   This constraint instantiates the christianized_pacification_reading of
 *   the feud_obligation_kernel. It treats the blood-feud prohibition as the
 *   active constraint: divine law prohibits vengeance, legitimate violence
 *   resides with God alone, and ecclesiastical and royal institutions hold
 *   delegated monopoly. All feud participants are placed in the victim set
 *   due to spiritual peril, while the Church gains interpretive monopoly and
 *   expanded jurisdictional reach. Sibling readings include the
 *   stateless_coordination_reading (feud as self-enforcing justice) and the
 *   extraction_cycle_reading (feud as material depletion).
 *
 * KEY AGENTS:
 *   - ecclesiastical_hierarchy (agenda_setter/beneficiary, institutional/arbitrage) â claims interpretive monopoly and collects jurisdictional rents
 *   - royal_authority (beneficiary/agenda_setter, institutional/arbitrage) â receives delegated violence authority and suppresses noble autonomy
 *   - noble_kin_groups (payer, powerful/identity_locked) â bear spiritual and social costs of suppressed vengeance obligations
 *   - traditional_jurists (excluded, moderate/trapped) â custodians of pre-Christian law silenced by the new framework
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(feud_obligation_kernel__christianized_pacification_reading, 0.72).
domain_priors:suppression_score(feud_obligation_kernel__christianized_pacification_reading, 0.85).
domain_priors:theater_ratio(feud_obligation_kernel__christianized_pacification_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(feud_obligation_kernel__christianized_pacification_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(feud_obligation_kernel__christianized_pacification_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(feud_obligation_kernel__christianized_pacification_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(feud_obligation_kernel__christianized_pacification_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(feud_obligation_kernel__christianized_pacification_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(feud_obligation_kernel__christianized_pacification_reading, tangled_rope).
narrative_ontology:human_readable(feud_obligation_kernel__christianized_pacification_reading, "Christianized Pacification Prohibition on Blood Feud").
narrative_ontology:topic_domain(feud_obligation_kernel__christianized_pacification_reading, "legal_anthropology/medieval_history/comparative_political_systems").

domain_priors:requires_active_enforcement(feud_obligation_kernel__christianized_pacification_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(feud_obligation_kernel__christianized_pacification_reading, '16b12eb7-6519-413c-8baa-0c9526fdfb0d').
narrative_ontology:cs_kernel_codification('16b12eb7-6519-413c-8baa-0c9526fdfb0d', fixed_text).
narrative_ontology:cs_authority_grounding('16b12eb7-6519-413c-8baa-0c9526fdfb0d', lineage).
narrative_ontology:cs_interpretation_layer_present('16b12eb7-6519-413c-8baa-0c9526fdfb0d').
narrative_ontology:cs_reading_relation('16b12eb7-6519-413c-8baa-0c9526fdfb0d', feud_obligation_kernel__stateless_coordination_reading, forecloses).
narrative_ontology:cs_reading_relation('16b12eb7-6519-413c-8baa-0c9526fdfb0d', feud_obligation_kernel__extraction_cycle_reading, influences).
narrative_ontology:cs_axiom('16b12eb7-6519-413c-8baa-0c9526fdfb0d', foundational, divine_monopoly_on_legitimate_violence).
narrative_ontology:cs_axiom_status(divine_monopoly_on_legitimate_violence, holdable).
narrative_ontology:cs_axiom_grounding('16b12eb7-6519-413c-8baa-0c9526fdfb0d', divine_monopoly_on_legitimate_violence, theological).
narrative_ontology:cs_axiom('16b12eb7-6519-413c-8baa-0c9526fdfb0d', foundational, blood_feud_as_mortal_sin).
narrative_ontology:cs_axiom_status(blood_feud_as_mortal_sin, holdable).
narrative_ontology:cs_axiom_grounding('16b12eb7-6519-413c-8baa-0c9526fdfb0d', blood_feud_as_mortal_sin, theological).
narrative_ontology:cs_reference_frame('16b12eb7-6519-413c-8baa-0c9526fdfb0d', divine_peace_order).
narrative_ontology:cs_drift_state('16b12eb7-6519-413c-8baa-0c9526fdfb0d', high_medieval_territorial_consolidation, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('16b12eb7-6519-413c-8baa-0c9526fdfb0d', '').
narrative_ontology:cs_kernel_id(feud_obligation_kernel__christianized_pacification_reading, feud_obligation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(feud_obligation_kernel__christianized_pacification_reading, ecclesiastical_hierarchy).
narrative_ontology:constraint_beneficiary(feud_obligation_kernel__christianized_pacification_reading, royal_authority).
narrative_ontology:constraint_victim(feud_obligation_kernel__christianized_pacification_reading, noble_kin_groups).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers penitential discipline, canon law, and ecclesiastical courts; claims sole interpretive authority over divine law regarding legitimate violence. Expands jurisdictional reach by subordinating kin-based vengeance to church-mediated penance and delegated royal justice, collecting tithes, court fees, and obedience in the process.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__christianized_pacification_reading, ecclesiastical_hierarchy, agenda_setter,
    institutional, civilizational, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(feud_obligation_kernel__christianized_pacification_reading, ecclesiastical_hierarchy, beneficiary).

% Receives delegated authority over legitimate violence from ecclesiastical sanction; uses the prohibition on blood feud to suppress local noble autonomy and consolidate territorial justice under royal courts and crown-appointed judges.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__christianized_pacification_reading, royal_authority, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(feud_obligation_kernel__christianized_pacification_reading, royal_authority, agenda_setter).

% Bound by customary obligation to avenge slain kin and defend collective honor. Under the Christian prohibition, every feud incurs spiritual peril, mandatory penance, and threat of excommunication. Honor and kin solidarity are constitutive of their social identity, making renunciation of vengeance experienced as existential loss even when the feud itself is materially costly.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__christianized_pacification_reading, noble_kin_groups, payer,
    powerful, generational, identity_locked, regional).

% Custodians of pre-Christian and customary law who historically recognized blood feud as lawful reciprocity and regulated its limits. Excluded from the Christian penitential framework, their objections are classified as heresy or barbarism and are not admitted to canonical debate.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__christianized_pacification_reading, traditional_jurists, excluded,
    moderate, generational, trapped, regional).

narrative_ontology:fixing_cost_class(feud_obligation_kernel__christianized_pacification_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Centralizes legitimate violence under a unified divine and royal legal order, replacing decentralized kin-based self-help with a cross-regional moral prohibition enforced by penitential discipline and ecclesiastical courts.
% TRANSFER_FUNCTION: Transfers the authority to judge and execute violent redress from regional kin groups to ecclesiastical and royal institutions; transfers material and jurisdictional resourcesâtithes, court fees, obedience, and interpretive controlâto the Church and crown.
% ABSENT_VOICES: Pre-Christian jurists, traditional kin elders, and pagan legal specialists who recognized blood feud as regulated reciprocity rather than sinful anarchy. They are outside the Christian penitential framework and their voices are treated as heresy or barbarism.
% DISAPPEARANCE_RATIONALE: If the divine prohibition and its penitential enforcement vanished overnight, noble kin groups would revert to customary blood-feud obligations; ecclesiastical jurisdiction over domestic violence would collapse, and royal claims to a monopoly on legitimate violence would weaken against resurgent local kin solidarity.
% FOUNDING_PROBLEM: Chronic blood-feuding depletes the faithful, destabilizes Christian realms, and usurps God's sole authority over life and death; a unified Christian moral order requires subordinating kin vengeance to divine justice mediated by Church and crown.
% FOUNDING_PROBLEM_CORROBORATION: Church fathers, canon lawyers, and penitential handbooks attest the problem from within the beneficiary set. Independent corroboration from outside the benefiting parties is sparse: Frankish and Anglo-Saxon chroniclers document feuding but often narrate it as customary justice rather than existential disorder. No non-ecclesiastical source attests the problem in the Church's specific theological terms.
narrative_ontology:disappearance_verdict(feud_obligation_kernel__christianized_pacification_reading, world_rearranges).
narrative_ontology:founding_problem_status(feud_obligation_kernel__christianized_pacification_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(feud_obligation_kernel__christianized_pacification_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(feud_obligation_kernel__christianized_pacification_reading, 'none', 1).
narrative_ontology:epsilon_provenance(feud_obligation_kernel__christianized_pacification_reading, 0.72, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(feud_obligation_kernel__christianized_pacification_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(feud_obligation_kernel__christianized_pacification_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(feud_obligation_kernel__christianized_pacification_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.72) is high because the constraint strips kin groups of autonomous justice and redirects jurisdictional and material flows to Church and crown. Suppression (0.85) is very high because persistence depends on penitential discipline, excommunication, and active exclusion of customary law. Theater ratio (0.48) is moderate-high: the spiritual claims are sincerely held by many agents, but a substantial share of enforcement activity performs jurisdictional expansion rather than genuine pacification. Accessibility collapse (0.75) is high because once the divine-law framing is accepted, blood feud collapses as a spiritually viable alternative. Resistance (0.70) is high because noble kin groups continue feuding clandestinely and resist ecclesiastical jurisdiction.
 *
 * PERSPECTIVAL GAP:
 *   The ecclesiastical seat computes the constraint as necessary spiritual salvation and public order; the noble kin seat computes it as alien imposition destroying honor and kin solidarity. The engine derives this divergence from the same structural data: the Church holds agenda-setting power and arbitrage-grade exit, while kin groups are identity-locked victims.
 *
 * DIRECTIONALITY LOGIC:
 *   The ecclesiastical_hierarchy and royal_authority are structural beneficiaries (low d, near subsidy) because the constraint channels authority and resources to them. Noble_kin_groups are structural victims (high d, near full target) because the constraint extracts honor, autonomy, and customary legal capacity from them; their identity_locked exit amplifies effective extraction. Traditional_jurists are excluded entirely, with no directional flow.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling the constraint as pure extraction (snare) by acknowledging the genuine coordination function: suppressing blood feud does reduce cycles of violence and creates a cross-kin moral order under written law. However, it is tangled_rope rather than rope because that coordination is inseparable from the Church's asymmetric extraction of interpretive monopoly and jurisdictional reach. If the coordination could be separated from the monopolyâe.g., if divine pacification were enforced without ecclesiastical court fees and tithesâthe extraction component would drop and the constraint might recertify as scaffold or rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'Does the constraint''s high extractiveness derive from the genuine spiritual necessity of centralizing violence under divine law, or from the Church''s institutional expansion and jurisdictional rent-seeking?',
    'Compare jurisdictions where Christian pacification succeeded without granting the Church exclusive judicial monopoly versus those where Church courts gained binding jurisdiction; examine whether royal authority independently corroborates the spiritual peril or merely leverages it for territorial consolidation.',
    'If the spiritual claim is sincere and separable from jurisdictional expansion, the constraint trends toward rope or scaffold; if inseparable, it remains tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Whether the reading''s spiritual claims are separable from institutional extraction').

omega_variable(
    penitential_suppression_mechanism,
    'Is the suppression of feud obligations achieved primarily through internalized guilt and spiritual fear, or through external ecclesiastical enforcement such as excommunication and interdict?',
    'Analyze penitential records, chronicle evidence, and feud continuation rates: if feuding declines primarily where bishops wield coercive sanctions, suppression is structural; if it declines through sermons and internalized sin even where enforcement is weak, suppression is partially internalized.',
    'Internalized suppression increases effective extraction beyond the structural measure because the target carries the constraint even after external enforcement weakens, potentially shifting the seat classification for noble kin groups.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(penitential_suppression_mechanism, empirical, 'Structural vs internalized suppression mechanism in penitential discipline').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(feud_obligation_kernel__christianized_pacification_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(feud_tr_t0, feud_obligation_kernel__christianized_pacification_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(feud_tr_t20, feud_obligation_kernel__christianized_pacification_reading, theater_ratio, 20, 0.3).
narrative_ontology:measurement(feud_tr_t40, feud_obligation_kernel__christianized_pacification_reading, theater_ratio, 40, 0.38).
narrative_ontology:measurement(feud_tr_t60, feud_obligation_kernel__christianized_pacification_reading, theater_ratio, 60, 0.42).
narrative_ontology:measurement(feud_tr_t80, feud_obligation_kernel__christianized_pacification_reading, theater_ratio, 80, 0.45).
narrative_ontology:measurement(feud_tr_t100, feud_obligation_kernel__christianized_pacification_reading, theater_ratio, 100, 0.48).

% Extraction over time
narrative_ontology:measurement(feud_be_t0, feud_obligation_kernel__christianized_pacification_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(feud_be_t20, feud_obligation_kernel__christianized_pacification_reading, base_extractiveness, 20, 0.45).
narrative_ontology:measurement(feud_be_t40, feud_obligation_kernel__christianized_pacification_reading, base_extractiveness, 40, 0.58).
narrative_ontology:measurement(feud_be_t60, feud_obligation_kernel__christianized_pacification_reading, base_extractiveness, 60, 0.65).
narrative_ontology:measurement(feud_be_t80, feud_obligation_kernel__christianized_pacification_reading, base_extractiveness, 80, 0.7).
narrative_ontology:measurement(feud_be_t100, feud_obligation_kernel__christianized_pacification_reading, base_extractiveness, 100, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(feud_su_t0, feud_obligation_kernel__christianized_pacification_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(feud_su_t20, feud_obligation_kernel__christianized_pacification_reading, suppression_requirement, 20, 0.55).
narrative_ontology:measurement(feud_su_t40, feud_obligation_kernel__christianized_pacification_reading, suppression_requirement, 40, 0.68).
narrative_ontology:measurement(feud_su_t60, feud_obligation_kernel__christianized_pacification_reading, suppression_requirement, 60, 0.75).
narrative_ontology:measurement(feud_su_t80, feud_obligation_kernel__christianized_pacification_reading, suppression_requirement, 80, 0.82).
narrative_ontology:measurement(feud_su_t100, feud_obligation_kernel__christianized_pacification_reading, suppression_requirement, 100, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(feud_obligation_kernel__christianized_pacification_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(feud_obligation_kernel__christianized_pacification_reading, feud_obligation_kernel__stateless_coordination_reading).
narrative_ontology:affects_constraint(feud_obligation_kernel__christianized_pacification_reading, feud_obligation_kernel__extraction_cycle_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the feud_obligation_kernel. The christianized_pacification_reading treats the blood-feud prohibition as the active constraint; the stateless_coordination_reading treats the feud obligation itself as the constraint; the extraction_cycle_reading treats the feud obligation as a destructive extraction mechanism. Each reading carries a distinct epsilon and stakeholder geometry, linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
