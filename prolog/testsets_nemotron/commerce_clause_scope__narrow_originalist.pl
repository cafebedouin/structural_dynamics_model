% ============================================================================
% CONSTRAINT STORY: commerce_clause_scope__narrow_originalist
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_commerce_clause_scope__narrow_originalist, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: commerce_clause_scope__narrow_originalist
 *   human_readable: Commerce Clause — Narrow Originalist Reading
 *   domain: constitutional/federalism/commerce_power
 *
 * SUMMARY:
 *   This constraint story instantiates the narrow originalist reading of the
 *   Commerce Clause scope kernel. The reading holds that 'commerce among the
 *   several states' refers exclusively to trade crossing state lines,
 *   'regulate' means to make regular or facilitate rather than restrict, and
 *   federal power extends only to removing state-imposed barriers to
 *   interstate trade and ensuring uniform commercial rules. Under this
 *   reading, federal environmental, labor, and civil rights laws that
 *   regulate non-commercial or purely intrastate activity exceed the
 *   enumerated power. The constraint presents itself as a mountain — a fixed
 *   textual and historical limit on federal authority — but declares
 *   beneficiaries (state governments, local businesses, advocates of
 *   regulatory decentralization) which triggers False Summit Mountain
 *   evaluation. The claimed_type is mountain (the reading's own
 *   self-understanding as a natural limit); the metrics describe low
 *   extractiveness and suppression but non-zero resistance from competing
 *   readings.
 *
 * KEY AGENTS:
 *   - state_governments: Primary beneficiary (institutional/generational) — retains regulatory autonomy over intrastate economic activity
 *   - local_businesses: Beneficiary (organized/biographical) — insulated from federal regulatory compliance costs
 *   - decentralized_regulatory_experimentation: Beneficiary (analytical/civilizational) — normative commitment to state-level policy innovation
 *   - national_regulatory_uniformity: Victim (analytical/generational) — coherence of federal regulatory schemes across states
 *   - civil_rights_enforcement_in_recalcitrant_states: Victim (powerless/biographical) — populations in states that resist federal civil rights mandates
 *   - federal_judiciary: Agenda setter (institutional/generational) — adjudicates the boundary; its composition determines which reading prevails
 *   - congress: Payer (institutional/biographical) — loses legislative capacity under this reading; bears political cost of constrained power
 *   - originalist_legal_academy: Observer (analytical/civilizational) — develops and maintains the interpretive methodology
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(commerce_clause_scope__narrow_originalist, 0.12).
domain_priors:suppression_score(commerce_clause_scope__narrow_originalist, 0.15).
domain_priors:theater_ratio(commerce_clause_scope__narrow_originalist, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(commerce_clause_scope__narrow_originalist, extractiveness, 0.12).
narrative_ontology:constraint_metric(commerce_clause_scope__narrow_originalist, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(commerce_clause_scope__narrow_originalist, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(commerce_clause_scope__narrow_originalist, accessibility_collapse, 0.22).
narrative_ontology:constraint_metric(commerce_clause_scope__narrow_originalist, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(commerce_clause_scope__narrow_originalist, mountain).
narrative_ontology:human_readable(commerce_clause_scope__narrow_originalist, "Commerce Clause — Narrow Originalist Reading").
narrative_ontology:topic_domain(commerce_clause_scope__narrow_originalist, "constitutional/federalism/commerce_power").

domain_priors:emerges_naturally(commerce_clause_scope__narrow_originalist).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(commerce_clause_scope__narrow_originalist, 'f6494c25-4711-442d-a70b-a8e9b2917ed7').
narrative_ontology:cs_kernel_codification('f6494c25-4711-442d-a70b-a8e9b2917ed7', fixed_text).
narrative_ontology:cs_authority_grounding('f6494c25-4711-442d-a70b-a8e9b2917ed7', lineage).
narrative_ontology:cs_interpretation_layer_present('f6494c25-4711-442d-a70b-a8e9b2917ed7').
narrative_ontology:cs_reading_relation('f6494c25-4711-442d-a70b-a8e9b2917ed7', commerce_clause_scope__broad_effects_test, forecloses).
narrative_ontology:cs_reading_relation('f6494c25-4711-442d-a70b-a8e9b2917ed7', commerce_clause_scope__intermediate_channels, coexists_with).
narrative_ontology:cs_axiom('f6494c25-4711-442d-a70b-a8e9b2917ed7', foundational, commerce_means_cross_border_trade_only).
narrative_ontology:cs_axiom_status(commerce_means_cross_border_trade_only, holdable).
narrative_ontology:cs_axiom_grounding('f6494c25-4711-442d-a70b-a8e9b2917ed7', commerce_means_cross_border_trade_only, empirically_contingent).
narrative_ontology:cs_axiom('f6494c25-4711-442d-a70b-a8e9b2917ed7', foundational, regulate_means_make_regular_not_prohibit).
narrative_ontology:cs_axiom_status(regulate_means_make_regular_not_prohibit, holdable).
narrative_ontology:cs_axiom_grounding('f6494c25-4711-442d-a70b-a8e9b2917ed7', regulate_means_make_regular_not_prohibit, empirically_contingent).
narrative_ontology:cs_axiom('f6494c25-4711-442d-a70b-a8e9b2917ed7', secondary, federal_power_limited_to_removing_state_barriers).
narrative_ontology:cs_axiom_status(federal_power_limited_to_removing_state_barriers, holdable).
narrative_ontology:cs_axiom_grounding('f6494c25-4711-442d-a70b-a8e9b2917ed7', federal_power_limited_to_removing_state_barriers, deontological).
narrative_ontology:cs_reference_frame('f6494c25-4711-442d-a70b-a8e9b2917ed7', founding_era_original_public_meaning).
narrative_ontology:cs_drift_state('f6494c25-4711-442d-a70b-a8e9b2917ed7', contemporary_originalist_revival, gap(revival_pressure, substantial, false)).
narrative_ontology:cs_created_at('f6494c25-4711-442d-a70b-a8e9b2917ed7', '2026-08-04T14:23:12Z').
narrative_ontology:cs_kernel_id(commerce_clause_scope__narrow_originalist, commerce_clause_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(commerce_clause_scope__narrow_originalist, state_governments).
narrative_ontology:constraint_beneficiary(commerce_clause_scope__narrow_originalist, local_businesses).
narrative_ontology:constraint_beneficiary(commerce_clause_scope__narrow_originalist, decentralized_regulatory_experimentation).
narrative_ontology:constraint_victim(commerce_clause_scope__narrow_originalist, national_regulatory_uniformity).
narrative_ontology:constraint_victim(commerce_clause_scope__narrow_originalist, civil_rights_enforcement_in_recalcitrant_states).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(commerce_clause_scope__narrow_originalist, local_businesses).
narrative_ontology:constraint_victim(commerce_clause_scope__narrow_originalist, congress).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Retain plenary regulatory authority over intrastate economic activity — labor, environment, consumer protection, land use — without federal preemption under the Commerce Clause. Can experiment with divergent policy approaches. Exit from the constraint would mean accepting federal regulatory supremacy; they have high exit leverage through Senate representation and state sovereignty doctrines.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__narrow_originalist, state_governments, beneficiary,
    institutional, generational, arbitrage, national).

% Avoid compliance costs of federal regulatory regimes (minimum wage, overtime, environmental standards, civil rights mandates) that would apply under broader readings. However, they also lose the benefits of a uniform national market and may face competitive disadvantage against businesses in states with laxer regulation. Exit is constrained — they cannot easily relocate to avoid state-level regulation, but they benefit from the absence of federal floor.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__narrow_originalist, local_businesses, beneficiary,
    organized, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(commerce_clause_scope__narrow_originalist, local_businesses, payer).

% A normative commitment to state-level policy innovation as a discovery mechanism for effective governance (the 'laboratories of democracy' thesis). This is not an actor that collects rents but a vindicated proposition that the constraint's operation supports. It benefits from the constraint's limitation on federal power but bears no costs and has no exit.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__narrow_originalist, decentralized_regulatory_experimentation, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(commerce_clause_scope__narrow_originalist, decentralized_regulatory_experimentation).

% The coherence and effectiveness of federal regulatory schemes (environmental protection, financial regulation, workplace safety, civil rights) that require consistent national standards. Under the narrow reading, these schemes develop gaps where states opt out or refuse cooperation, creating regulatory arbitrage and enforcement voids. This is a structural victim — a systemic good that is degraded by the constraint — not a human actor.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__narrow_originalist, national_regulatory_uniformity, payer,
    analytical, generational, analytical, universal).
narrative_ontology:stakeholder_non_agent(commerce_clause_scope__narrow_originalist, national_regulatory_uniformity).

% Populations in states that resist or refuse to enforce federal civil rights protections (voting rights, anti-discrimination, fair housing, disability access). When federal authority is constrained to only cross-border commerce, these populations lose the federal floor that overrides state-level hostility. They are trapped — they cannot exit the state's jurisdiction, and state-level democratic processes have historically failed them. The constraint structurally enables their subordination.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__narrow_originalist, civil_rights_enforcement_in_recalcitrant_states, payer,
    powerless, biographical, trapped, national).

% Adjudicates the boundary of federal commerce power through judicial review. The Court's composition determines which reading prevails. The constraint gives the judiciary significant power to invalidate federal legislation, but also exposes it to legitimacy challenges when its decisions track partisan lines. It can 'exit' by shifting doctrine (as in 1937 or 1995) but at high institutional cost.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__narrow_originalist, federal_judiciary, agenda_setter,
    institutional, generational, arbitrage, national).

% Loses legislative capacity to address national problems through commerce power. Must rely on other powers (spending, taxing, treaty, Section 5 of 14th Amendment) which are narrower or politically costlier. Exit is constrained — constitutional amendment is practically impossible; the only exit is appointing judges who adopt a different reading, which takes decades.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__narrow_originalist, congress, payer,
    institutional, biographical, constrained, national).

% Develops, refines, and defends the originalist methodology that produces this reading. Provides the intellectual infrastructure (historical research, linguistic analysis, theoretical frameworks) that makes the reading credible to the judiciary and the public. Neither collects nor pays; observes and influences the constraint's interpretive trajectory.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__narrow_originalist, originalist_legal_academy, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the federal-state boundary by providing a fixed, textually grounded limit on federal legislative power, enabling states to govern intrastate affairs without fear of displacement and giving the judiciary a neutral principle for invalidating federal overreach.
% TRANSFER_FUNCTION: Transfers regulatory authority from the federal government to state governments over intrastate economic activity. The 'gain' is state autonomy; the 'cost' is foregone national regulatory capacity. No direct monetary transfer, but a structural reallocation of governing power.
% ABSENT_VOICES: Marginalized populations in states with histories of rights suppression (Black voters in the Jim Crow South, LGBTQ+ individuals in states without anti-discrimination laws, workers in states without labor protections) are structurally excluded from the constitutional conversation that defines the commerce power. They would argue for a broader reading that enables federal protection, but the originalist methodology treats their interests as policy preferences outside the constitutional text.
% DISAPPEARANCE_RATIONALE: If the narrow originalist constraint vanished overnight (i.e., the Court adopted the broad effects test), Congress would immediately regain authority to regulate intrastate activity with substantial aggregate effects on interstate commerce. Federal environmental, labor, and civil rights statutes would apply uniformly. States would lose regulatory autonomy in those domains. The national regulatory landscape would fundamentally reorganize.
% FOUNDING_PROBLEM: The Founders feared that a broad federal commerce power would become a general police power, displacing state regulation of domestic economic life and consolidating authority in a distant national legislature unaccountable to local conditions. The narrow reading was built to prevent this consolidation by limiting 'commerce' to cross-border trade and 'regulate' to facilitation.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by the Federalist Papers (Madison in Federalist 45: 'The powers delegated... to the federal government are few and defined. Those which are to remain in the State governments are numerous and indefinite') and by the ratification-era understanding that the Constitution created a government of enumerated powers. However, the Anti-Federalists (Brutus, Federal Farmer) contested whether the commerce clause as written would actually remain narrow, predicting it would become a vehicle for federal expansion. Modern originalist scholars (Barnett, Lawson) corroborate the narrow reading's historical grounding; living constitutionalist scholars (Amar, Balkin) and New Deal-era jurists (Stone, Douglas) corroborate that the founding problem has been superseded by economic integration and the need for national solutions.
narrative_ontology:disappearance_verdict(commerce_clause_scope__narrow_originalist, world_rearranges).
narrative_ontology:founding_problem_status(commerce_clause_scope__narrow_originalist, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(commerce_clause_scope__narrow_originalist, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(commerce_clause_scope__narrow_originalist, 'none', 1).
narrative_ontology:epsilon_provenance(commerce_clause_scope__narrow_originalist, 0.12, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(commerce_clause_scope__narrow_originalist_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(commerce_clause_scope__narrow_originalist, ExtMetricName, E),
    domain_priors:suppression_score(commerce_clause_scope__narrow_originalist, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(commerce_clause_scope__narrow_originalist),
    narrative_ontology:constraint_metric(commerce_clause_scope__narrow_originalist, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(commerce_clause_scope__narrow_originalist, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(commerce_clause_scope__narrow_originalist_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.12) because the reading primarily restricts federal power rather than extracting resources from regulated parties — the 'extraction' is the opportunity cost of foregone federal regulation. Suppression is low (0.15) because the constraint operates through judicial invalidation of federal statutes, not direct coercion of individuals. Theater ratio is minimal (0.05) because the reading's proponents genuinely believe it reflects constitutional text and history, not performative maintenance. Accessibility collapse is low (0.22) because alternative readings (broad effects test, intermediate channels) remain live and intellectually respectable — the constraint does not foreclose interpretive alternatives. Resistance is high (0.78) because the reading has been contested since the Founding and faced sustained pressure from the New Deal era onward; the constraint must be actively defended by a committed judicial coalition. The measurement series shows extractiveness spiking during the Lochner era (1895) when the Court used narrow commerce readings to strike down federal labor laws, then dropping after 1937, then modestly rising with the Rehnquist/Roberts Court federalism revival.
 *
 * PERSPECTIVAL GAP:
 *   From the state government seat (beneficiary, institutional power, generational horizon), the constraint appears as a mountain — a fixed constitutional barrier protecting state sovereignty. From the civil rights enforcement seat (victim, powerless, biographical), the same constraint operates as a snare — a structural barrier that prevents federal protection against state-level discrimination. From the federal judiciary seat (agenda setter, institutional), the constraint is a tangled rope — a coordination mechanism for federalism that requires active judicial enforcement and produces asymmetric effects. The engine computes these divergent seat classifications from the structural data; the claimed_type (mountain) represents only the reading's own self-understanding.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (state governments, local businesses, decentralized experimentation advocates) receive structural protection from federal regulatory reach — directionality d near 0.0 (full beneficiary). Victims (national regulatory uniformity, civil rights enforcement in recalcitrant states) bear the cost of foregone federal capacity — directionality d near 1.0 (full target). The federal judiciary as agenda setter sits at d ~ 0.5 (symmetric) — it administers the constraint but its institutional legitimacy depends on the constraint's perceived neutrality. Congress as payer sits at d ~ 0.8 (target) — it loses legislative authority but retains political accountability. The narrow reading's directionality profile is sharply bimodal: strong beneficiaries, strong victims, little middle ground.
 *
 * MANDATROPHY ANALYSIS:
 *   The reading's founding problem — preventing federal regulatory overreach into state domestic affairs — remains live (contested status) because the tension between national governance needs and state autonomy persists. However, the specific historical fears (federal displacement of state commercial regulation, consolidation of power in a distant legislature) have been substantially transformed by modern interstate economic integration. The reading persists not because the founding problem is unchanged, but because it has been adopted as a methodological commitment by a durable judicial coalition. This is not classic mandatrophy (where the function atrophies but the form persists theatrically); rather, the function has been redefined from 'preventing overreach' to 'enforcing original meaning' — a different telos that sustains the constraint.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_federalism,
    'Does the narrow originalist reading reflect a genuine structural feature of the constitutional design (mountain), or is it a constructed constraint that benefits identifiable political actors who favor decentralized power?',
    'Historical analysis of Founding-era understandings of ''commerce'' and ''regulate'' compared against the practical governance needs that emerged post-ratification; counterfactual assessment of whether a narrow reading was practically sustainable or always required judicial construction to maintain.',
    'If the reading is a genuine natural law of the constitutional structure, it persists regardless of political preference and the mountain classification holds. If it is a constructed constraint maintained by beneficiaries (states'' rights advocates, business interests opposing federal regulation), the False Summit Mountain signature should trigger reclassification to tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_federalism, conceptual, 'Whether the narrow reading is a structural feature of the Constitution or a constructed preference for decentralized power').

omega_variable(
    kernel_reading_committer_structure,
    'This constraint is one reading (narrow_originalist) of the contested kernel commerce_clause_scope. What are the structural consequences of the sibling readings (broad_effects_test, intermediate_channels) that this reading forecloses, coexists with, or influences?',
    'Comparative analysis of the three readings'' beneficiary/victim structures, extractiveness profiles, and institutional implications. The engine computes foreclosure from cs_structure axioms + drift_state; this omega records the committer-frame mapping.',
    'Routes the kernel/reading committer structure through the omega infrastructure rather than inventing schema fields. Enables cross-reading contamination analysis when sibling stories are compiled.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'Committer-frame mapping: this reading of commerce_clause_scope kernel; siblings are broad_effects_test and intermediate_channels').

omega_variable(
    civil_rights_enforcement_gap,
    'Does the narrow reading''s victim set (civil rights enforcement in recalcitrant states) represent a genuine structural extraction from vulnerable populations, or is it a policy disagreement about federal power that the reading''s proponents would characterize as state autonomy rather than victimhood?',
    'Empirical assessment of whether recalcitrant states systematically under-enforce civil rights protections when federal authority is constrained, and whether the resulting harm is structural (inescapable by the affected populations) or political (contestable through state-level democratic processes).',
    'If the gap produces inescapable harm to identifiable populations with no state-level exit, the victim declaration is structurally grounded and the constraint has extractive force. If the harm is politically contestable at the state level, the victim declaration may reflect policy preference rather than structural extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(civil_rights_enforcement_gap, empirical, 'Whether the civil rights enforcement gap constitutes structural victimhood or policy disagreement').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(commerce_clause_scope__narrow_originalist, 1789, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ccsn_tr_t1789, commerce_clause_scope__narrow_originalist, theater_ratio, 1789, 0.02).
narrative_ontology:measurement(ccsn_tr_t1824, commerce_clause_scope__narrow_originalist, theater_ratio, 1824, 0.03).
narrative_ontology:measurement(ccsn_tr_t1895, commerce_clause_scope__narrow_originalist, theater_ratio, 1895, 0.08).
narrative_ontology:measurement(ccsn_tr_t1937, commerce_clause_scope__narrow_originalist, theater_ratio, 1937, 0.04).
narrative_ontology:measurement(ccsn_tr_t1995, commerce_clause_scope__narrow_originalist, theater_ratio, 1995, 0.05).
narrative_ontology:measurement(ccsn_tr_t2024, commerce_clause_scope__narrow_originalist, theater_ratio, 2024, 0.05).

% Extraction over time
narrative_ontology:measurement(ccsn_be_t1789, commerce_clause_scope__narrow_originalist, base_extractiveness, 1789, 0.05).
narrative_ontology:measurement(ccsn_be_t1824, commerce_clause_scope__narrow_originalist, base_extractiveness, 1824, 0.08).
narrative_ontology:measurement(ccsn_be_t1895, commerce_clause_scope__narrow_originalist, base_extractiveness, 1895, 0.18).
narrative_ontology:measurement(ccsn_be_t1937, commerce_clause_scope__narrow_originalist, base_extractiveness, 1937, 0.09).
narrative_ontology:measurement(ccsn_be_t1995, commerce_clause_scope__narrow_originalist, base_extractiveness, 1995, 0.1).
narrative_ontology:measurement(ccsn_be_t2024, commerce_clause_scope__narrow_originalist, base_extractiveness, 2024, 0.12).

% Suppression requirement over time
narrative_ontology:measurement(ccsn_su_t1789, commerce_clause_scope__narrow_originalist, suppression_requirement, 1789, 0.1).
narrative_ontology:measurement(ccsn_su_t1824, commerce_clause_scope__narrow_originalist, suppression_requirement, 1824, 0.12).
narrative_ontology:measurement(ccsn_su_t1895, commerce_clause_scope__narrow_originalist, suppression_requirement, 1895, 0.25).
narrative_ontology:measurement(ccsn_su_t1937, commerce_clause_scope__narrow_originalist, suppression_requirement, 1937, 0.15).
narrative_ontology:measurement(ccsn_su_t1995, commerce_clause_scope__narrow_originalist, suppression_requirement, 1995, 0.14).
narrative_ontology:measurement(ccsn_su_t2024, commerce_clause_scope__narrow_originalist, suppression_requirement, 2024, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(commerce_clause_scope__narrow_originalist, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(commerce_clause_scope__narrow_originalist, 0.08).
narrative_ontology:affects_constraint(commerce_clause_scope__narrow_originalist, commerce_clause_scope__broad_effects_test).
narrative_ontology:affects_constraint(commerce_clause_scope__narrow_originalist, commerce_clause_scope__intermediate_channels).
narrative_ontology:affects_constraint(commerce_clause_scope__narrow_originalist, federal_preemption_doctrine).
narrative_ontology:affects_constraint(commerce_clause_scope__narrow_originalist, state_police_powers_reserved).
narrative_ontology:affects_constraint(commerce_clause_scope__narrow_originalist, tenth_amendment_anticommandeering).

% DUAL FORMULATION NOTE:
% This story is one of three in the commerce_clause_scope constraint family. The narrow_originalist reading claims mountain status with low extractiveness; the broad_effects_test reading claims rope/tangled_rope with higher extractiveness (federal regulatory capacity); the intermediate_channels reading claims tangled_rope with moderate extractiveness and active enforcement requirements. The three readings share the same kernel (Commerce Clause text) but instantiate structurally distinct constraints with different beneficiary/victim sets and extraction profiles. They are linked via affects_constraints to enable cross-reading contamination analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(commerce_clause_scope__narrow_originalist, institutional, 0.15).
constraint_indexing:directionality_override(commerce_clause_scope__narrow_originalist, powerless, 0.92).
constraint_indexing:directionality_override(commerce_clause_scope__narrow_originalist, organized, 0.2).
constraint_indexing:directionality_override(commerce_clause_scope__narrow_originalist, analytical, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
