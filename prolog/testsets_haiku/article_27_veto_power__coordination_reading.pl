% ============================================================================
% CONSTRAINT STORY: article_27_veto_power__coordination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article_27_veto_power__coordination_reading, []).

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
 *   constraint_id: article_27_veto_power__coordination_reading
 *   human_readable: P5 Veto Power (Coordination Reading): Prevention of Compelled Great-Power War
 *   domain: international_relations/institutional_design
 *
 * SUMMARY:
 *   The UN Security Council's permanent-member veto power (Article 27, UN
 *   Charter) is a mechanism by which each of the five nuclear-armed permanent
 *   members can unilaterally block any Council resolution. This constraint
 *   story instantiates the COORDINATION READING of this contested kernel: the
 *   veto is understood as solving the collective-action problem of
 *   great-power consent to international law. Without the veto, each nuclear
 *   state would face strategic uncertainty that its peers could manufacture a
 *   Council mandate to compel it into military confrontation it rejects. The
 *   veto removes this risk by guaranteeing that no resolution can pass
 *   without all P5 consent. In this reading, all signatories benefit: smaller
 *   states get protection from P5-initiated compulsion; P5 members get
 *   certainty that they cannot be bound without consent. The constraint is
 *   classified as ROPE because it solves a genuine coordination problem (the
 *   consent failure) with minimal coercive overhead—participation in the UN
 *   and deference to Council decisions is voluntary, and the veto enforces a
 *   simple, transparent rule (unanimity). The measured extractiveness is low
 *   (0.18 at interval end) because the veto's primary function is
 *   coordination, not extraction. Suppression is minimal (0.12) because the
 *   constraint works by making compulsion impossible, not by silencing
 *   dissent. Theater is very low (0.08) because the veto's operation is
 *   straightforward and not defended by elaborate performative justification.
 *
 * KEY AGENTS:
 *   - Permanent_five_members (nuclear-armed great powers): Hold veto power; benefit from guaranteed refusal capacity
 *   - Non-permanent_council_members (elected rotating states): Benefit from P5 veto constraint on each other; bear cost of blocked humanitarian action
 *   - General_UN_membership (all other states): Benefit from prevention of Council-mandated compulsion; bear cost of P5 veto shields on their own aggression
 *   - International_legal_scholars (observer seat): Analyze whether veto solves coordination or entrenches oligopoly
 *   - Rival_institutional_designs (excluded): Would offer majoritarian or proportional voting but are barred by Article 27 immutability
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_27_veto_power__coordination_reading, 0.18).
domain_priors:suppression_score(article_27_veto_power__coordination_reading, 0.12).
domain_priors:theater_ratio(article_27_veto_power__coordination_reading, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_27_veto_power__coordination_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(article_27_veto_power__coordination_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(article_27_veto_power__coordination_reading, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_27_veto_power__coordination_reading, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(article_27_veto_power__coordination_reading, resistance, 0.22).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_27_veto_power__coordination_reading, rope).
narrative_ontology:human_readable(article_27_veto_power__coordination_reading, "P5 Veto Power (Coordination Reading): Prevention of Compelled Great-Power War").
narrative_ontology:topic_domain(article_27_veto_power__coordination_reading, "international_relations/institutional_design").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_27_veto_power__coordination_reading, '74b9d8fa-4f5e-47b5-9ac6-5c7d3f6f91ff').
narrative_ontology:cs_kernel_codification('74b9d8fa-4f5e-47b5-9ac6-5c7d3f6f91ff', fixed_text).
narrative_ontology:cs_authority_grounding('74b9d8fa-4f5e-47b5-9ac6-5c7d3f6f91ff', lineage).
narrative_ontology:cs_interpretation_layer_present('74b9d8fa-4f5e-47b5-9ac6-5c7d3f6f91ff').
narrative_ontology:cs_reading_relation('74b9d8fa-4f5e-47b5-9ac6-5c7d3f6f91ff', article_27_veto_power__oligopoly_reading, coexists_with).
narrative_ontology:cs_reading_relation('74b9d8fa-4f5e-47b5-9ac6-5c7d3f6f91ff', article_27_veto_power__sovereignty_reading, influences).
narrative_ontology:cs_axiom('74b9d8fa-4f5e-47b5-9ac6-5c7d3f6f91ff', foundational, unanimity_prevents_compulsion).
narrative_ontology:cs_axiom_status(unanimity_prevents_compulsion, holdable).
narrative_ontology:cs_axiom_grounding('74b9d8fa-4f5e-47b5-9ac6-5c7d3f6f91ff', unanimity_prevents_compulsion, empirically_contingent).
narrative_ontology:cs_axiom('74b9d8fa-4f5e-47b5-9ac6-5c7d3f6f91ff', secondary, great_power_consent_enables_institutional_stability).
narrative_ontology:cs_axiom_status(great_power_consent_enables_institutional_stability, holdable).
narrative_ontology:cs_axiom_grounding('74b9d8fa-4f5e-47b5-9ac6-5c7d3f6f91ff', great_power_consent_enables_institutional_stability, instrumental).
narrative_ontology:cs_reference_frame('74b9d8fa-4f5e-47b5-9ac6-5c7d3f6f91ff', unanimous_consent_protection).
narrative_ontology:cs_drift_state('74b9d8fa-4f5e-47b5-9ac6-5c7d3f6f91ff', contemporary_humanitarian_crisis_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('74b9d8fa-4f5e-47b5-9ac6-5c7d3f6f91ff', '').
narrative_ontology:cs_kernel_id(article_27_veto_power__coordination_reading, article_27_veto_power).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_27_veto_power__coordination_reading, international_system_stability).
narrative_ontology:constraint_beneficiary(article_27_veto_power__coordination_reading, nuclear_armed_great_powers).
narrative_ontology:constraint_beneficiary(article_27_veto_power__coordination_reading, non_aligned_states).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_27_veto_power__coordination_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(article_27_veto_power__coordination_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article_27_veto_power__coordination_reading_tests).
:- end_tests(article_27_veto_power__coordination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness series (0.12 → 0.29 over the interval) models gradual increase in veto usage to block humanitarian action and Council accountability, rather than increase in compulsion risk. The measurement uses observed data through t=60 (post-WWII era through recent years) and projects forward to t=75 (competitive multipolar scenario ~2030). Even in the high-projection scenario, extractiveness stays below 0.30 because the coordination reading holds that the veto's PRIMARY function (preventing compulsion) remains valuable—the increase reflects secondary uses (blocking justice/accountability) not the core function. Theater rises modestly (0.05 → 0.11) as the veto's humanitarian-blocking use becomes more salient; the ratio stays well below 0.5 because the veto remains functionally necessary, not theatrically maintained. Suppression is low and stable because the veto operates through structural impossibility (no resolution can pass), not through coercion of dissent. All three measurements are authored on one shared time grid (0, 15, 30, 45, 60, 75) so the metrics can be compared at each time point.
 *
 * PERSPECTIVAL GAP:
 *   From the P5 perspective, the veto is the institution's core legitimacy: it makes the UN worth joining because it guarantees they cannot be bound without consent. From smaller states' perspective, the veto is a valuable mutual constraint on great powers (preventing any one P5 from dominating) but also a frustration when those same powers use vetoes to block accountability for their own actions. From the analytical perspective, these two views reflect the veto's dual role: as coordination mechanism (preventing compulsion) and as power-preserving mechanism (blocking institutional evolution that would redistribute veto power). The engine's per-seat classification will compute different perceived types at different power atoms: institutional-seat analysis may show Rope (coordination function dominates); powerless-seat analysis may show contamination toward Tangled Rope (cost of blocked humanitarian action visible). The claim/metric independence rule holds: I claim Rope (coordination reading) and author metrics consistent with low extractiveness (the veto's PRIMARY function is coordination, not extraction), but I also author rising theater (secondary uses are increasingly salient), allowing the engine and the interpreter to see the constraints on that reading.
 *
 * DIRECTIONALITY LOGIC:
 *   The permanent five members appear as beneficiaries (d ≈ 0.15 for institutional power-atom analysis: they benefit from refusal capacity, have high exit options analytically, and bear minimal cost in the coordination reading). Non-permanent members are secondary beneficiaries with secondary costs (d ≈ 0.45: they benefit from P5 mutual constraint but also bear the cost of P5 veto shields). Smaller states are asymmetrically positioned (d ≈ 0.52: they benefit from being protected from P5-initiated compulsion but bear the cost that P5 members can veto humanitarian action; the asymmetry means they are slight net targets, though in the coordination reading all parties benefit overall). The international legal scholars sit in the observer/analytical seat (d = 0.5, neither collecting nor paying). No directionality overrides are needed: the structural derivation from beneficiary declarations produces the correct d values because the beneficiary set is symmetric (all states benefit from the unanimity gate) even though the distribution of power is asymmetric (P5 hold the veto while others do not). The absence of a clear victim class—unlike in Snare or Tangled Rope—is itself the signal that this is Rope.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem of the veto—ensuring great powers consent to international law—was live in 1945 and arguably remains live in 2026 (disputed in omega_1). However, the FOUNDING_PROBLEM_STATUS field in six_questions is marked 'contested' precisely because there is scholarly and political disagreement: reformists argue the problem is 'dead' (nuclear deterrence, not veto protection, prevents compelled war) and the veto now serves only to entrench P5 power; institutionalists argue it remains 'live' (without the veto, strategic uncertainty about Council mandates would be real, and great powers would either withdraw or demand amendment). The measurement series (extractiveness rising from 0.12 to 0.29) models a scenario where the founding problem's salience decreases relative to the veto's secondary uses (humanitarian blocking). If the founding_problem_status shifts from 'live' to 'dead' and disappearance_verdict remains 'world_rearranges', the conjunction would trigger a mandatrophy flag: the constraint persists (veto remains in Charter) despite its founding justification eroding. This is exactly the condition to audit: is the veto a zombie—a rule that solved a real problem now solved elsewhere, kept in place by institutional inertia and P5 interest? The omega variables name this ambiguity without resolving it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_vs_oligopoly_boundary,
    'Does the veto primarily solve a genuine collective-action problem (coordination reading) or primarily entrench geopolitical oligopoly (oligopoly reading)?',
    'Historical counterfactual: would great powers have joined a unanimity-gated international organization if they lacked veto protection? Comparative institutional analysis: do alternative unanimity mechanisms (e.g., regional security councils with veto gates) persist because they solve coordination problems or because they entrench incumbent power?',
    'If the veto solves coordination, ε is low (~0.15–0.25), the constraint is Rope, and the beneficiaries are all signatories. If the veto primarily entrenches oligopoly, ε is high (~0.60–0.75), the constraint is Snare, and beneficiaries are the P5 alone. The constraint''s classification hinges on this boundary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_vs_oligopoly_boundary, conceptual, 'Whether the veto is a coordination mechanism or an oligopoly entrenchment. This is the core structural dispute between the coordination and oligopoly readings.').

omega_variable(
    founding_problem_persistence,
    'Does the founding problem that the veto was built to solve—preventing great-power compulsion via Council mandate—still pose a real strategic risk, or has nuclear deterrence decoupled from the veto''s protection?',
    'Strategic analysis of great-power military decision-making: would any P5 member''s military planning change if the veto were removed and Council majorities could issue military mandates? Does institutional behavior (threat of withdrawal, reform demands, bloc formation around alternative security arrangements) indicate persistent perceived risk?',
    'If the founding problem persists, the veto''s coordination function remains valuable and ε stays low. If it has been superseded by nuclear deterrence and other enforcement mechanisms, the veto becomes vestigial—its persistence shifts from coordination value to oligopoly entrenchment, and ε rises sharply.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(founding_problem_persistence, empirical, 'Whether the veto''s founding problem is live or dead, and whether the veto''s continued operation reflects coordination necessity or institutional inertia.').

omega_variable(
    veto_measurement_observable,
    'Does this constraint measure the veto''s role as coordination mechanism, or the veto''s role as blocking mechanism for justice/humanitarian action? These are structurally different extractiveness profiles.',
    'Define the constraint''s scope: does it cover the veto''s use to prevent compulsion (coordination function) or its use to block humanitarian action against the vetoing state (oligopoly/extraction function)? If the same institutional rule serves both functions, two separate constraints may be needed per DP-001 ε-invariance.',
    'If this constraint measures only the coordination function (preventing compulsion), ε is low and the constraint is Rope. If it measures the veto''s use to shield vetoing states from accountability (blocking humanitarian action), ε is high and the constraint is Snare. These are NOT two measurements of the same constraint—they are two different constraints with two different ε values.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(veto_measurement_observable, conceptual, 'Whether this constraint is defined narrowly (veto as coordination against compulsion) or broadly (veto as blocking mechanism for all purposes). The ε-invariance principle requires separate stories if the same rule carries two structurally different functions.').

omega_variable(
    beneficiary_identity_alignment,
    'In the coordination reading, is the ''international system stability'' a genuine beneficiary (an outcome all parties want), or is it a rhetorical cover for P5 oligopoly protection?',
    'Behavioral observation: do non-permanent council members and smaller states consistently support the veto when it prevents their own humanitarian crises from reaching Council action? Do they advocate for its abolition or for reform? Do they join reform coalitions when the political cost is low? Their revealed preferences about whether ''system stability'' (the veto) or ''justice'' (unfettered Council action) matters more indicates whether the veto''s beneficiaries align with the coordination framing or split.',
    'If non-P5 states consistently choose veto preservation when reform is cost-free, the coordination reading is vindicated: all parties benefit from the unanimity gate. If they consistently advocate reform and only tolerate the veto because they cannot change it, the oligopoly reading is more accurate: the veto entrenchment benefits only P5, while smaller states bear the cost of blocked humanitarian action.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_identity_alignment, empirical, 'Whether smaller states'' revealed preferences align with the coordination reading (veto benefits everyone) or the oligopoly reading (veto benefits P5 at the cost of smaller-state justice).').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_27_veto_power__coordination_reading, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t0, article_27_veto_power__coordination_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(arti_tr_t15, article_27_veto_power__coordination_reading, theater_ratio, 15, 0.06).
narrative_ontology:measurement(arti_tr_t30, article_27_veto_power__coordination_reading, theater_ratio, 30, 0.07).
narrative_ontology:measurement(arti_tr_t45, article_27_veto_power__coordination_reading, theater_ratio, 45, 0.08).
narrative_ontology:measurement(arti_tr_t60, article_27_veto_power__coordination_reading, theater_ratio, 60, 0.09).
narrative_ontology:measurement(arti_tr_t75, article_27_veto_power__coordination_reading, theater_ratio, 75, 0.11).

% Extraction over time
narrative_ontology:measurement(arti_be_t0, article_27_veto_power__coordination_reading, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(arti_be_t15, article_27_veto_power__coordination_reading, base_extractiveness, 15, 0.15).
narrative_ontology:measurement(arti_be_t30, article_27_veto_power__coordination_reading, base_extractiveness, 30, 0.18).
narrative_ontology:measurement(arti_be_t45, article_27_veto_power__coordination_reading, base_extractiveness, 45, 0.21).
narrative_ontology:measurement(arti_be_t60, article_27_veto_power__coordination_reading, base_extractiveness, 60, 0.25).
narrative_ontology:measurement(arti_be_t75, article_27_veto_power__coordination_reading, base_extractiveness, 75, 0.29).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t0, article_27_veto_power__coordination_reading, suppression_requirement, 0, 0.08).
narrative_ontology:measurement(arti_su_t15, article_27_veto_power__coordination_reading, suppression_requirement, 15, 0.09).
narrative_ontology:measurement(arti_su_t30, article_27_veto_power__coordination_reading, suppression_requirement, 30, 0.1).
narrative_ontology:measurement(arti_su_t45, article_27_veto_power__coordination_reading, suppression_requirement, 45, 0.11).
narrative_ontology:measurement(arti_su_t60, article_27_veto_power__coordination_reading, suppression_requirement, 60, 0.12).
narrative_ontology:measurement(arti_su_t75, article_27_veto_power__coordination_reading, suppression_requirement, 75, 0.14).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_27_veto_power__coordination_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(article_27_veto_power__coordination_reading, 0.1).
narrative_ontology:affects_constraint(article_27_veto_power__coordination_reading, article_27_veto_power__oligopoly_reading).
narrative_ontology:affects_constraint(article_27_veto_power__coordination_reading, article_27_veto_power__sovereignty_reading).
narrative_ontology:affects_constraint(article_27_veto_power__coordination_reading, security_council_action_paralysis__humanitarian_crisis).
narrative_ontology:affects_constraint(article_27_veto_power__coordination_reading, un_charter_amendment_immutability).

% DUAL FORMULATION NOTE:
% The veto power is one kernel with three structurally distinct readings. This constraint (coordination_reading) decomposes from its siblings because the three readings differ in their core structural claims: coordination_reading asserts the veto solves the consent problem (low ε, all states benefit); oligopoly_reading asserts the veto entrenches P5 power (high ε, only P5 benefit); sovereignty_reading asserts the veto instantiates the Westphalian principle (medium ε, P5 benefit structurally even if principle applies to all). These are not observer-relative measurements of the same constraint—they are three different constraints with three different ε values, claimed by three different scholarly and political parties as the 'true' reading of Article 27. The family is linked because each reading logically depends on the others NOT being true: if oligopoly_reading is accurate, coordination_reading is a cover story; if sovereignty_reading's principle grounds the veto, it also grounds reform arguments for expanded permanent membership. See network.affects_constraints for cross-family edges.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
