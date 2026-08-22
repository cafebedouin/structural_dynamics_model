% ============================================================================
% CONSTRAINT STORY: feud_obligation_kernel__extraction_cycle_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_feud_obligation_kernel__extraction_cycle_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: feud_obligation_kernel__extraction_cycle_reading
 *   human_readable: Blood-Feud Obligation as Productive-Capacity Extraction Cycle
 *   domain: legal_anthropology/political_economy
 *
 * SUMMARY:
 *   This story instantiates the extraction-cycle reading of the contested
 *   blood-feud kernel: it treats kin-based vengeance and compensation
 *   obligations not as a genuine coordination substitute for absent state
 *   capacity, but as a self-perpetuating structure that steadily consumes the
 *   productive base (labor, livestock, land, lives) of the households it
 *   binds, while a consolidating royal authority captures the resulting
 *   instability as the justification for extending its own monopoly on
 *   legitimate violence and, with it, its tax and jurisdictional reach. The
 *   suppression_requirement series rises across the interval to reflect the
 *   crown's growing enforcement apparatus (peace bonds, royal courts,
 *   outlawry statutes) needed to displace kin-based self-help — not the
 *   feud's own coercive force, which this reading treats as internal to the
 *   victim-side dynamic captured in extractiveness and resistance.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(feud_obligation_kernel__extraction_cycle_reading, 0.71).
domain_priors:suppression_score(feud_obligation_kernel__extraction_cycle_reading, 0.62).
domain_priors:theater_ratio(feud_obligation_kernel__extraction_cycle_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(feud_obligation_kernel__extraction_cycle_reading, extractiveness, 0.71).
narrative_ontology:constraint_metric(feud_obligation_kernel__extraction_cycle_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(feud_obligation_kernel__extraction_cycle_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(feud_obligation_kernel__extraction_cycle_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(feud_obligation_kernel__extraction_cycle_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(feud_obligation_kernel__extraction_cycle_reading, tangled_rope).
narrative_ontology:human_readable(feud_obligation_kernel__extraction_cycle_reading, "Blood-Feud Obligation as Productive-Capacity Extraction Cycle").
narrative_ontology:topic_domain(feud_obligation_kernel__extraction_cycle_reading, "legal_anthropology/political_economy").

domain_priors:requires_active_enforcement(feud_obligation_kernel__extraction_cycle_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(feud_obligation_kernel__extraction_cycle_reading, 'd34cc091-907f-44d9-b6f0-9ed7b5ce85c8').
narrative_ontology:cs_kernel_codification('d34cc091-907f-44d9-b6f0-9ed7b5ce85c8', distributed).
narrative_ontology:cs_authority_grounding('d34cc091-907f-44d9-b6f0-9ed7b5ce85c8', practice).
narrative_ontology:cs_interpretation_layer_present('d34cc091-907f-44d9-b6f0-9ed7b5ce85c8').
narrative_ontology:cs_reading_relation('d34cc091-907f-44d9-b6f0-9ed7b5ce85c8', feud_obligation_kernel__stateless_coordination_reading, coexists_with).
narrative_ontology:cs_reading_relation('d34cc091-907f-44d9-b6f0-9ed7b5ce85c8', feud_obligation_kernel__christianized_pacification_reading, influences).
narrative_ontology:cs_axiom('d34cc091-907f-44d9-b6f0-9ed7b5ce85c8', foundational, feud_persistence_is_net_productive_loss).
narrative_ontology:cs_axiom_status(feud_persistence_is_net_productive_loss, holdable).
narrative_ontology:cs_axiom_grounding('d34cc091-907f-44d9-b6f0-9ed7b5ce85c8', feud_persistence_is_net_productive_loss, empirically_contingent).
narrative_ontology:cs_axiom('d34cc091-907f-44d9-b6f0-9ed7b5ce85c8', secondary, centralized_violence_monopoly_reduces_aggregate_extraction).
narrative_ontology:cs_axiom_status(centralized_violence_monopoly_reduces_aggregate_extraction, holdable).
narrative_ontology:cs_axiom_grounding('d34cc091-907f-44d9-b6f0-9ed7b5ce85c8', centralized_violence_monopoly_reduces_aggregate_extraction, instrumental).
narrative_ontology:cs_reference_frame('d34cc091-907f-44d9-b6f0-9ed7b5ce85c8', kin_reciprocity_self_help_order).
narrative_ontology:cs_drift_state('d34cc091-907f-44d9-b6f0-9ed7b5ce85c8', post_royal_law_code_promulgation, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('d34cc091-907f-44d9-b6f0-9ed7b5ce85c8', '').
narrative_ontology:cs_kernel_id(feud_obligation_kernel__extraction_cycle_reading, feud_obligation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(feud_obligation_kernel__extraction_cycle_reading, consolidating_royal_authority).
narrative_ontology:constraint_beneficiary(feud_obligation_kernel__extraction_cycle_reading, wergild_intermediary_elites).
narrative_ontology:constraint_victim(feud_obligation_kernel__extraction_cycle_reading, feuding_kin_groups).
narrative_ontology:constraint_victim(feud_obligation_kernel__extraction_cycle_reading, agricultural_dependents).
narrative_ontology:constraint_victim(feud_obligation_kernel__extraction_cycle_reading, unmarried_male_kin_avengers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bound by kin-honor obligation to avenge killings or extract wergild compensation; each cycle of retaliation removes able-bodied men from farming and herding, destroys livestock and property in raids, and locks the lineage into multi-generational reciprocal claims. Exiting the obligation risks social death — loss of standing, marriageability, and protection — so the cost of continuing is borne even as it visibly depletes the household's productive base.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__extraction_cycle_reading, feuding_kin_groups, payer,
    moderate, generational, trapped, regional).

% The specific men called upon to carry out vengeance killings or accept wergild terms on the family's behalf; they bear direct mortality risk and lose years of productive labor to raids, hiding, or exile after a killing. They have essentially no individual exit — refusal disgraces the entire kin line and can trigger internal sanction as severe as external feud.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__extraction_cycle_reading, unmarried_male_kin_avengers, payer,
    powerless, biographical, trapped, local).

% Tenants, laborers, and smallholders attached to feuding households who do not choose the feud but absorb its costs: burned fields, seized livestock, disrupted planting and harvest cycles, and the diversion of household resources into weapons, compensation payments, and armed retainers instead of productive investment.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__extraction_cycle_reading, agricultural_dependents, payer,
    powerless, biographical, trapped, local).

% A rising crown or comparable central authority that campaigns to outlaw or heavily circumscribe self-help vengeance, replacing it with royal courts, royal peace guarantees, and fines payable to the crown rather than only to the injured kin group. By suppressing kin-based enforcement it establishes itself as the sole legitimate violence-monopolist, which in turn legitimizes its capacity to tax, conscript, and administer territory the feuds had kept fragmented and ungovernable.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__extraction_cycle_reading, consolidating_royal_authority, agenda_setter,
    institutional, civilizational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(feud_obligation_kernel__extraction_cycle_reading, consolidating_royal_authority, beneficiary).

% Local lords, arbitrators, and negotiators who broker compensation settlements between feuding lineages, extracting fees, land concessions, or political loyalty as the price of ending a given cycle; they profit from the feud's persistence as much as from its resolution, since repeat mediation is a durable revenue stream.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__extraction_cycle_reading, wergild_intermediary_elites, beneficiary,
    powerful, generational, mobile, regional).

% Administer the emerging law codes that convert feud liability into fixed schedules of compensation payable partly to the crown; they enforce peace bonds and prosecute unlicensed vengeance, gradually substituting centralized adjudication for kin reciprocity.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__extraction_cycle_reading, royal_courts_and_officials, agenda_setter,
    institutional, generational, analytical, national).

% Reconstruct agricultural output, population, and settlement-pattern data across feud-prevalent versus pacified regions to assess whether feuding measurably suppressed territorial consolidation and productive investment over successive generations.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__extraction_cycle_reading, economic_historians, observer,
    analytical, civilizational, analytical, continental).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: At a minimal level the feud cycle does coordinate expectations — it signals which lineages are dangerous to cross and provides a (costly) mechanism for redress absent a court system — but this reading holds that whatever coordination benefit existed is structurally dominated by the extraction the cycle imposes on the productive base of every participating household and by the rent-taking of intermediary elites who profit from unresolved cycles.
% TRANSFER_FUNCTION: The arrangement moves labor-years, livestock, land, and lives out of feuding households and into destroyed capital, compensation payments to mediating elites, and eventually into the tax and jurisdictional base of a consolidating royal authority that displaces kin-based enforcement with its own monopoly on legitimate violence.
% ABSENT_VOICES: The women of feuding lineages, whose marriage prospects, labor, and household security absorb the feud's costs without a formal role in initiating or ending it, are almost entirely outside the negotiation; likewise the rural producers tied to feuding households have no voice in whether a cycle is pursued or settled.
% DISAPPEARANCE_RATIONALE: If feud obligation vanished overnight, kin groups would redirect labor and capital from raiding, retaliation, and compensation payments into sustained agricultural investment and settlement expansion; mediating elites would lose a durable revenue stream; and the crown's central claim to being the sole guarantor of peace — the legitimating basis for its tax and jurisdictional reach — would lose its primary contrast case.
% FOUNDING_PROBLEM: In the absence of a capable central enforcement authority, kin groups needed some mechanism to deter killing, theft, and injury and to obtain redress when norms were violated.
% FOUNDING_PROBLEM_CORROBORATION: Royal chroniclers and later legal historians attest that by the period this reading examines, central courts and peace-bonds already existed as functioning alternatives in many regions, making the founding deterrence problem substantially solved rather than live; this corroboration comes from crown administrative records and independent economic-historical reconstruction of the period, not from the feuding kin groups themselves, who continued to assert the obligation's necessity.
narrative_ontology:disappearance_verdict(feud_obligation_kernel__extraction_cycle_reading, world_rearranges).
narrative_ontology:founding_problem_status(feud_obligation_kernel__extraction_cycle_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(feud_obligation_kernel__extraction_cycle_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(feud_obligation_kernel__extraction_cycle_reading, 'none', 1).
narrative_ontology:epsilon_provenance(feud_obligation_kernel__extraction_cycle_reading, 0.71, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(feud_obligation_kernel__extraction_cycle_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(feud_obligation_kernel__extraction_cycle_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(feud_obligation_kernel__extraction_cycle_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high-moderate (0.71 at interval end) reflecting sustained depletion of labor, capital, and lives across generations of feuding lineages, rising modestly over the interval as feud-cycle lengths and compensation demands escalate under this reading's account. Suppression is authored as the royal/administrative apparatus's growing capacity to criminalize and displace kin vengeance — deliberately distinct from, and rising alongside, the extraction it targets. Theater ratio stays low (0.1 to 0.2) because both the feud cycle and the emerging royal court system perform substantive functions (redress and adjudication respectively), not primarily performative ones, in this reading. Accessibility collapse is moderate (0.45): some households did find alternate paths (migration, monastic sanctuary, early royal arbitration), so alternatives were not fully foreclosed even before formal pacification. Resistance is moderate (0.58): honor codes were not universally embraced even by participants, and evidence exists of kin groups seeking settlement rather than continued vengeance where possible.
 *
 * DIRECTIONALITY LOGIC:
 *   Feuding kin groups, and especially the specific young men called to avenge or defend, sit at the target end of directionality: trapped exit options, direct mortality and labor-loss exposure, no meaningful alternative absent full lineage disgrace. Agricultural dependents share victim status without even nominal participation in the decision to feud. Consolidating royal authority sits at the beneficiary end: it does not bear feud costs directly and gains legitimacy and revenue capacity precisely by suppressing the practice. Wergild-intermediary elites occupy an intermediate beneficiary position — their exit options are comparatively mobile, and their income depends on the feud cycle's persistence rather than its resolution, which is why they are named as beneficiaries rather than neutral mediators.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as tangled_rope (rather than pure snare) preserves the fact that the feud arrangement did solve a genuine problem — some redress mechanism where none existed — while insisting that, under this reading, the mechanism's continued operation past the point where royal courts became available constitutes extraction riding on that original coordination function. Labeling it pure snare would erase the founding_problem's initial legitimacy; labeling it pure rope would erase the documented multi-generational depletion this reading centers. The founding_problem_status is authored 'contested' rather than 'dead' precisely because the stateless_coordination_reading (a sibling, not this story) would dispute that royal courts were adequate substitutes in every region at every time.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    feud_versus_state_capacity_causality,
    'Did blood-feud persistence cause the absence of territorial consolidation, or did the absence of state capacity (for independent reasons — geography, population density, external threat) cause feud persistence, with consolidation failure a shared downstream effect of both?',
    'Comparative regional economic-historical analysis: compare consolidation trajectories in similar-geography regions with differing feud intensity, controlling for external military pressure and population density.',
    'If feud persistence is a downstream symptom rather than an independent causal driver of consolidation failure, this reading''s extraction framing overstates the feud''s causal weight relative to prior state-capacity conditions, though the depletion of productive capacity documented here would still hold as a real cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(feud_versus_state_capacity_causality, empirical, 'Whether feuding caused fragmentation or merely accompanied it.').

omega_variable(
    royal_beneficiary_versus_incidental_gain,
    'Did the consolidating crown actively cultivate feud suppression as an extraction-legitimizing strategy, or did centralization occur for independent military/fiscal reasons with feud suppression as an incidental byproduct rather than a designed mechanism?',
    'Examine royal charters, law-code preambles, and court records for explicit framing of peace-guarantee provisions as revenue or legitimacy instruments versus purely public-order justifications.',
    'If suppression was incidental rather than strategic, the beneficiary classification for consolidating_royal_authority still holds structurally (it did benefit) but the requires_active_enforcement framing as deliberate extraction-legitimizing strategy would need softening toward opportunistic capture.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(royal_beneficiary_versus_incidental_gain, conceptual, 'Whether royal benefit from feud suppression was designed or incidental.').

omega_variable(
    kernel_reading_selection_ambiguity,
    'Is the extraction-cycle framing the correct primary lens for this kernel, given that the stateless_coordination_reading and christianized_pacification_reading each capture real, independently documented aspects of the same historical practice?',
    'This is inherent to the kernel''s contested nature and is not resolvable by additional data within a single reading — it is resolved (per framework design) by maintaining all three readings as separate linked constraint stories rather than collapsing them into one adjudicated verdict.',
    'No single reading should be treated as the ''true'' classification of blood-feud obligation; each is a structurally distinct constraint with its own ε, beneficiaries, and victims, linked via network.affects_constraints, and analytical users must select the reading appropriate to their question rather than averaging across them.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_selection_ambiguity, conceptual, 'The kernel itself supports multiple non-commensurable readings by design.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(feud_obligation_kernel__extraction_cycle_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(feud_tr_t0, feud_obligation_kernel__extraction_cycle_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(feud_tr_t20, feud_obligation_kernel__extraction_cycle_reading, theater_ratio, 20, 0.12).
narrative_ontology:measurement(feud_tr_t40, feud_obligation_kernel__extraction_cycle_reading, theater_ratio, 40, 0.15).
narrative_ontology:measurement(feud_tr_t60, feud_obligation_kernel__extraction_cycle_reading, theater_ratio, 60, 0.17).
narrative_ontology:measurement(feud_tr_t80, feud_obligation_kernel__extraction_cycle_reading, theater_ratio, 80, 0.19).
narrative_ontology:measurement(feud_tr_t100, feud_obligation_kernel__extraction_cycle_reading, theater_ratio, 100, 0.2).

% Extraction over time
narrative_ontology:measurement(feud_be_t0, feud_obligation_kernel__extraction_cycle_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(feud_be_t20, feud_obligation_kernel__extraction_cycle_reading, base_extractiveness, 20, 0.6).
narrative_ontology:measurement(feud_be_t40, feud_obligation_kernel__extraction_cycle_reading, base_extractiveness, 40, 0.65).
narrative_ontology:measurement(feud_be_t60, feud_obligation_kernel__extraction_cycle_reading, base_extractiveness, 60, 0.68).
narrative_ontology:measurement(feud_be_t80, feud_obligation_kernel__extraction_cycle_reading, base_extractiveness, 80, 0.7).
narrative_ontology:measurement(feud_be_t100, feud_obligation_kernel__extraction_cycle_reading, base_extractiveness, 100, 0.71).

% Suppression requirement over time
narrative_ontology:measurement(feud_su_t0, feud_obligation_kernel__extraction_cycle_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(feud_su_t20, feud_obligation_kernel__extraction_cycle_reading, suppression_requirement, 20, 0.38).
narrative_ontology:measurement(feud_su_t40, feud_obligation_kernel__extraction_cycle_reading, suppression_requirement, 40, 0.47).
narrative_ontology:measurement(feud_su_t60, feud_obligation_kernel__extraction_cycle_reading, suppression_requirement, 60, 0.53).
narrative_ontology:measurement(feud_su_t80, feud_obligation_kernel__extraction_cycle_reading, suppression_requirement, 80, 0.58).
narrative_ontology:measurement(feud_su_t100, feud_obligation_kernel__extraction_cycle_reading, suppression_requirement, 100, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(feud_obligation_kernel__extraction_cycle_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(feud_obligation_kernel__extraction_cycle_reading, 0.1).
narrative_ontology:affects_constraint(feud_obligation_kernel__extraction_cycle_reading, feud_obligation_kernel__stateless_coordination_reading).
narrative_ontology:affects_constraint(feud_obligation_kernel__extraction_cycle_reading, feud_obligation_kernel__christianized_pacification_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of feud_obligation_kernel. The stateless_coordination_reading treats the same underlying practice as near-Rope (genuine deterrence/justice function, minimal net extraction). The christianized_pacification_reading treats it as illegitimate under a doctrinal authority claim rather than an economic-extraction claim. All three share the kernel_id but author independent ε values, beneficiary/victim sets, and claimed_type per the ε-invariance principle; they are not to be averaged or reconciled into a single verdict.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
