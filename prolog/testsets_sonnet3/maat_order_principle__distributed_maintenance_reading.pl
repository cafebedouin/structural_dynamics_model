% ============================================================================
% CONSTRAINT STORY: maat_order_principle__distributed_maintenance_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_maat_order_principle__distributed_maintenance_reading, []).

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: maat_order_principle__distributed_maintenance_reading
 *   human_readable: Ma'at as Distributed Cosmic-Order Maintenance (Every Station Sustains Balance)
 *   domain: ancient_history/political_philosophy/religious_studies
 *
 * SUMMARY:
 *   This story authors the distributed-maintenance reading of Ma'at: the
 *   claim, drawn from tomb autobiographies and wisdom literature
 *   (Instructions of Ptahhotep, Amenemope), that cosmic order is sustained
 *   not solely by Pharaoh's unique relationship to the divine but by every
 *   actor's proper conduct within their assigned station — scribe,
 *   magistrate, artisan, farmer, servant. This is one of three sibling
 *   readings of the same Ma'at kernel. The divine_mandate_reading holds that
 *   order flows exclusively from Pharaoh's embodiment of Ma'at and cannot be
 *   violated by him by definition — a very different authority structure. The
 *   reciprocity_reading holds that Ma'at is a mutual bargain requiring
 *   Pharaoh to actively deliver justice and resource distribution in exchange
 *   for legitimacy. This reading is structurally distinct from both: it
 *   multiplies the number of legitimate interpreters of what 'proper conduct'
 *   means (scribes, magistrates, elders each hold real interpretive standing,
 *   not merely delegated authority), and it grounds legitimacy in
 *   demonstrated maintenance behavior rather than inherent status or a
 *   bargained exchange. That multiplication of interpretive seats is exactly
 *   what produces this reading's comparatively low extraction relative to the
 *   divine_mandate_reading: no single actor's unaccountable status is the
 *   sole load-bearing claim.
 *
 * KEY AGENTS:
 *   - pharaoh: apex station, but judged by the same conduct-in-station standard as everyone else in this reading
 *   - temple_scribal_administration: co-authors what 'proper conduct' means; gains real interpretive standing
 *   - local_magistrates and village_elders: exercise independently legitimated authority at regional and local scale
 *   - unlanded_laborers and foreign_captives: assigned stations they cannot contest, benefiting least from the reading's promised universality
 *   - modern_egyptologists: reconstruct this reading from administrative self-description, with the corroboration gap that implies
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(maat_order_principle__distributed_maintenance_reading, 0.28).
domain_priors:suppression_score(maat_order_principle__distributed_maintenance_reading, 0.32).
domain_priors:theater_ratio(maat_order_principle__distributed_maintenance_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(maat_order_principle__distributed_maintenance_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(maat_order_principle__distributed_maintenance_reading, suppression_requirement, 0.32).
narrative_ontology:constraint_metric(maat_order_principle__distributed_maintenance_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(maat_order_principle__distributed_maintenance_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(maat_order_principle__distributed_maintenance_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(maat_order_principle__distributed_maintenance_reading, rope).
narrative_ontology:human_readable(maat_order_principle__distributed_maintenance_reading, "Ma'at as Distributed Cosmic-Order Maintenance (Every Station Sustains Balance)").
narrative_ontology:topic_domain(maat_order_principle__distributed_maintenance_reading, "ancient_history/political_philosophy/religious_studies").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(maat_order_principle__distributed_maintenance_reading, '61503758-ff2a-49c8-8fb9-76e3b67b9f23').
narrative_ontology:cs_kernel_codification('61503758-ff2a-49c8-8fb9-76e3b67b9f23', distributed).
narrative_ontology:cs_authority_grounding('61503758-ff2a-49c8-8fb9-76e3b67b9f23', practice).
narrative_ontology:cs_interpretation_layer_present('61503758-ff2a-49c8-8fb9-76e3b67b9f23').
narrative_ontology:cs_reading_relation('61503758-ff2a-49c8-8fb9-76e3b67b9f23', maat_order_principle__divine_mandate_reading, forecloses).
narrative_ontology:cs_reading_relation('61503758-ff2a-49c8-8fb9-76e3b67b9f23', maat_order_principle__reciprocity_reading, coexists_with).
narrative_ontology:cs_axiom('61503758-ff2a-49c8-8fb9-76e3b67b9f23', foundational, authority_grounded_in_demonstrated_conduct_not_status).
narrative_ontology:cs_axiom_status(authority_grounded_in_demonstrated_conduct_not_status, holdable).
narrative_ontology:cs_axiom_grounding('61503758-ff2a-49c8-8fb9-76e3b67b9f23', authority_grounded_in_demonstrated_conduct_not_status, conventional).
narrative_ontology:cs_axiom('61503758-ff2a-49c8-8fb9-76e3b67b9f23', foundational, multiple_stations_hold_independent_interpretive_standing).
narrative_ontology:cs_axiom_status(multiple_stations_hold_independent_interpretive_standing, holdable).
narrative_ontology:cs_axiom_grounding('61503758-ff2a-49c8-8fb9-76e3b67b9f23', multiple_stations_hold_independent_interpretive_standing, conventional).
narrative_ontology:cs_reference_frame('61503758-ff2a-49c8-8fb9-76e3b67b9f23', old_kingdom_administrative_ideal_biography_tradition).
narrative_ontology:cs_drift_state('61503758-ff2a-49c8-8fb9-76e3b67b9f23', late_period_ptolemaic_syncretism, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('61503758-ff2a-49c8-8fb9-76e3b67b9f23', '').
narrative_ontology:cs_kernel_id(maat_order_principle__distributed_maintenance_reading, maat_order_principle).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(maat_order_principle__distributed_maintenance_reading, temple_scribal_administration).
narrative_ontology:constraint_beneficiary(maat_order_principle__distributed_maintenance_reading, local_magistrates).
narrative_ontology:constraint_beneficiary(maat_order_principle__distributed_maintenance_reading, village_elders).
narrative_ontology:constraint_beneficiary(maat_order_principle__distributed_maintenance_reading, artisan_guild_heads).
narrative_ontology:constraint_beneficiary(maat_order_principle__distributed_maintenance_reading, household_patriarchs).
narrative_ontology:constraint_victim(maat_order_principle__distributed_maintenance_reading, unlanded_laborers).
narrative_ontology:constraint_victim(maat_order_principle__distributed_maintenance_reading, foreign_captives_and_servants).
narrative_ontology:constraint_victim(maat_order_principle__distributed_maintenance_reading, women_outside_household_authority).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(maat_order_principle__distributed_maintenance_reading, pharaoh).
narrative_ontology:constraint_beneficiary(maat_order_principle__distributed_maintenance_reading, free_commoners).
narrative_ontology:constraint_victim(maat_order_principle__distributed_maintenance_reading, free_commoners).
narrative_ontology:constraint_vindicates(maat_order_principle__distributed_maintenance_reading, cosmic_order_requires_universal_participation).
narrative_ontology:constraint_vindicates(maat_order_principle__distributed_maintenance_reading, proper_conduct_in_station_sustains_maat).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Occupies the apex station and is understood, in this reading, as one maintainer among many rather than as the sole source of order — his conduct is judged by the same standard of 'proper action in station' applied to everyone else, which makes his legitimacy partially contingent on the visible functioning of order at every lower level, not solely on his own rites.
narrative_ontology:constraint_stakeholder(maat_order_principle__distributed_maintenance_reading, pharaoh, agenda_setter,
    institutional, civilizational, analytical, national).
narrative_ontology:stakeholder_secondary_role(maat_order_principle__distributed_maintenance_reading, pharaoh, beneficiary).

% Records, interprets, and adjudicates what proper conduct in each station looks like — effectively co-authoring Ma'at alongside the throne. Because this reading grounds authority in demonstrated maintenance rather than inherent royal status, the scribal class gains real interpretive standing and material support (land grants, temple income) for performing that function.
narrative_ontology:constraint_stakeholder(maat_order_principle__distributed_maintenance_reading, temple_scribal_administration, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(maat_order_principle__distributed_maintenance_reading, temple_scribal_administration, beneficiary).

% Adjudicate disputes and enforce customary norms at the nome or town level, deriving legitimacy from visibly discharging their station's duties rather than from royal appointment alone. They benefit from the distributed-responsibility frame because it validates their judgments as independently authoritative, not merely delegated.
narrative_ontology:constraint_stakeholder(maat_order_principle__distributed_maintenance_reading, local_magistrates, agenda_setter,
    powerful, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(maat_order_principle__distributed_maintenance_reading, local_magistrates, beneficiary).

% Settle small disputes and model proper conduct within the village, participating in cosmic maintenance at the smallest civic scale. Their authority is real but bounded; they gain modest local standing from being seen as order-keepers.
narrative_ontology:constraint_stakeholder(maat_order_principle__distributed_maintenance_reading, village_elders, agenda_setter,
    moderate, biographical, constrained, local).

% Maintain craft standards and internal guild discipline as their 'station's' contribution to order, which legitimizes their control over apprentices and pricing within the trade.
narrative_ontology:constraint_stakeholder(maat_order_principle__distributed_maintenance_reading, artisan_guild_heads, beneficiary,
    moderate, biographical, constrained, regional).

% Maintain order within the household as their station's cosmic duty, which legitimizes authority over dependents, wives, and children as a sacred obligation rather than a mere social convention.
narrative_ontology:constraint_stakeholder(maat_order_principle__distributed_maintenance_reading, household_patriarchs, beneficiary,
    moderate, biographical, constrained, local).

% Are told their labor, tax compliance, and deference to station-appropriate roles are themselves cosmic maintenance, which secures a claim to protection and predictable order but also naturalizes their subordinate position as a sacred duty rather than a negotiable arrangement. Leaving one's station is framed as a cosmic transgression, not a social choice.
narrative_ontology:constraint_stakeholder(maat_order_principle__distributed_maintenance_reading, free_commoners, payer,
    powerless, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(maat_order_principle__distributed_maintenance_reading, free_commoners, beneficiary).

% Perform corvée and agricultural labor understood as their 'station's' contribution to order; the framework offers them no alternative station to move to and no mechanism to contest labor demands, since refusing labor is framed as disrupting cosmic balance rather than as a grievance against a particular authority.
narrative_ontology:constraint_stakeholder(maat_order_principle__distributed_maintenance_reading, unlanded_laborers, payer,
    powerless, immediate, trapped, local).

% Are assigned a station at the very bottom of the order — servitude — and told that fulfilling it maintains cosmic balance. The distributed-responsibility frame gives them no interpretive standing to contest their assignment, since 'proper conduct in station' presumes the station itself is legitimate.
narrative_ontology:constraint_stakeholder(maat_order_principle__distributed_maintenance_reading, foreign_captives_and_servants, payer,
    powerless, immediate, trapped, local).

% Hold formally recognized legal capacities in some domains (property, contracts) but are folded into household 'stations' defined by male heads for the purposes of cosmic order; their independent maintenance of Ma'at is rarely recognized as a distinct contribution, limiting the reading's promised universality in practice.
narrative_ontology:constraint_stakeholder(maat_order_principle__distributed_maintenance_reading, women_outside_household_authority, payer,
    powerless, biographical, trapped, local).

% Reconstruct this reading from tomb autobiographies, wisdom literature, and administrative texts that repeatedly emphasize officials' and commoners' personal accountability for maintaining order in their sphere, distinct from texts emphasizing the king's unique cosmic role.
narrative_ontology:constraint_stakeholder(maat_order_principle__distributed_maintenance_reading, modern_egyptologists, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Distributes the cognitive and moral labor of sustaining social order across every social station, so that order does not depend solely on the competence or virtue of a single ruler — each actor's proper conduct within their own domain is treated as a load-bearing contribution to the overall system's stability.
% TRANSFER_FUNCTION: Moves interpretive authority and a share of legitimacy away from the throne alone and toward scribes, magistrates, elders, and household heads at each station; in the same motion it moves compliance burden onto powerless actors (laborers, captives, women outside household headship) by framing their subordination as cosmic duty rather than negotiable social arrangement.
% ABSENT_VOICES: Unlanded laborers, foreign captives, and women without independent household standing are assigned stations by others and have no textual tradition of their own contesting how 'proper conduct' in their station is defined — the wisdom literature and tomb autobiographies that document this reading were authored by the literate administrative class describing its own virtue.
% DISAPPEARANCE_RATIONALE: If the distributed-maintenance framing vanished and only a hard divine-mandate-through-Pharaoh model remained, the administrative and priestly class's independent interpretive authority would collapse toward pure delegation from the throne — a real institutional rearrangement for scribes and magistrates. For unlanded laborers and captives, whose labor obligations would persist under any Ma'at reading, the practical rearrangement is smaller: the ideological register justifying their labor would shift, but the underlying corvée and servitude arrangements have their own independent enforcement basis.
% FOUNDING_PROBLEM: Early Egyptian state formation needed a legitimating account of order that could survive the reality that the king could not personally administer, adjudicate, or produce order everywhere at once — some account was needed for why village-level, household-level, and administrative-level conduct mattered cosmically, not just legally.
% FOUNDING_PROBLEM_CORROBORATION: Modern Egyptological analysis of tomb autobiographies (the 'ideal biography' genre) and administrative correspondence, produced by scholars outside the ancient beneficiary class, corroborates that this distributed framing served contemporaneous administrative self-legitimation; no surviving voice from the laborer, captive, or subordinate-women strata corroborates or contests the framing directly, since those groups left no comparable textual record — a gap the reading's own universality claim cannot resolve from inside its surviving sources.
narrative_ontology:disappearance_verdict(maat_order_principle__distributed_maintenance_reading, contested).
narrative_ontology:founding_problem_status(maat_order_principle__distributed_maintenance_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(maat_order_principle__distributed_maintenance_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(maat_order_principle__distributed_maintenance_reading, 'none', 1).
narrative_ontology:epsilon_provenance(maat_order_principle__distributed_maintenance_reading, 0.28, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(maat_order_principle__distributed_maintenance_reading_tests).
:- end_tests(maat_order_principle__distributed_maintenance_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low relative to the sibling readings (0.28 vs. what a divine_mandate_reading would show) because authority here is distributed across many interpretive seats rather than concentrated in one unaccountable station — no single actor's status is immune to judgment by the same conduct standard applied to all. Suppression is moderate (0.32): the framework does constrain exit from one's assigned station (a laborer cannot simply reinterpret their own station's duties), but the constraint is diffuse rather than backed by a centralized enforcement apparatus, since the whole point of this reading is that order is self-enforcing through universal internalized conduct rather than top-down coercion. Theater ratio rises modestly over the interval (0.30 to 0.40) reflecting the long Egyptian administrative record: tomb-autobiography 'ideal biography' conventions became increasingly formulaic and performative over centuries, describing virtuous station-conduct in near-identical boilerplate language regardless of the official's actual administrative record — a Goodhart-style drift where the textual performance of order-maintenance detaches somewhat from any verifiable underlying conduct.
 *
 * DIRECTIONALITY LOGIC:
 *   The administrative and priestly classes (scribes, magistrates, elders, guild heads, household patriarchs) are the reading's chief structural beneficiaries: distributing responsibility for cosmic maintenance simultaneously distributes legitimating authority to them, letting them claim independent standing rather than mere delegation. Powerless and trapped agents — unlanded laborers, foreign captives, women outside household headship — are assigned 'payer' status because the same universalizing logic that elevates the administrative class's authority is used to naturalize their subordination as sacred duty, foreclosing the possibility of framing their position as a negotiable social or economic arrangement. Pharaoh is dual-positioned: elevated by being one 'station' among the cosmic order and thus not the sole author of its stability, but also modestly constrained because the reading in principle allows his conduct to be judged by the same station-appropriate standard as everyone else's, unlike the divine_mandate_reading where his conduct is definitionally beyond judgment.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading resists mislabeling as pure extraction because it authentically distributes interpretive load and legitimacy across many stations rather than concentrating both in a single unaccountable actor — that structural multiplication of accountable seats is a genuine coordination achievement relative to the divine_mandate_reading. It equally resists being read as pure coordination because the same universalizing 'everyone maintains order in their station' language is the mechanism that naturalizes the subordination of laborers, captives, and women outside household headship, foreclosing any station-reassignment claim those groups might otherwise make. The founding-problem status is authored 'dead' because the acute state-formation legitimation problem (how to justify order without omnipresent royal administration) no longer applies to any living political arrangement — but the underlying compliance-inducing function this reading served for its beneficiary administrative class is well corroborated by scholarship outside that class, even though no voice from the laborer or captive strata survives to corroborate or contest it directly.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    distributed_authority_vs_administrative_self_interest,
    'Is the distributed-maintenance framing a genuine philosophical commitment to universal moral agency in sustaining order, or is it primarily a self-legitimating narrative produced by the literate scribal and administrative class to secure its own interpretive authority and material support?',
    'Comparative analysis of tomb-autobiography formulas across social strata (were similar ''ideal biography'' claims available to non-elite burials?) and cross-reference with administrative land-grant records correlating self-described virtuous conduct with material reward.',
    'If primarily self-legitimating, the reading''s low authored extraction understates the concentration of benefit within the literate administrative class specifically, even though authority is nominally distributed across many stations rather than one throne.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(distributed_authority_vs_administrative_self_interest, conceptual, 'Whether distributed responsibility is genuine universal moral framework or scribal-class self-legitimation.').

omega_variable(
    universality_claim_vs_excluded_strata,
    'Does the reading''s claim of universal participation (''all actors from Pharaoh to commoner'') genuinely extend interpretive standing to laborers, captives, and women outside household headship, or does it use universalizing language while structurally denying those groups any voice in defining their own station''s proper conduct?',
    'Search for any surviving textual tradition, however fragmentary, in which a non-elite or subordinate voice articulates or contests their assigned station''s duties in Ma''at terms, as distinct from elite descriptions of subordinate stations.',
    'If no such voice exists, the reading''s universality is asserted rather than practiced, and the true operative constraint for excluded strata is closer to a tangled_rope or snare dressed in universalist language, even while it authentically functions as low-extraction coordination for the literate administrative class.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(universality_claim_vs_excluded_strata, empirical, 'Whether excluded strata have any genealogical voice in the universality claim, or only elite-authored descriptions of their stations.').

omega_variable(
    framing_choice_kernel_vs_authority_layer,
    'Should this constraint be framed as the kernel-level Ma''at concept itself (the underlying idea of cosmic order), or as the authority-distribution layer built atop a more primitive shared cosmological commitment that all three sibling readings take for granted?',
    'Compare the reading against Old Kingdom versus Middle Kingdom textual evidence: if the distributed-maintenance emphasis intensifies specifically during periods of weakened central authority (First Intermediate Period, late Middle Kingdom), that supports reading it as an authority-layer response to crisis rather than a stable original kernel commitment.',
    'If it is a crisis-period authority-layer construction, its comparatively low extraction may be partly an artifact of periods when central enforcement capacity was itself weak, rather than a stable structural feature of the reading across all periods it was invoked.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(framing_choice_kernel_vs_authority_layer, conceptual, 'Whether this reading is a stable kernel-level commitment or a period-specific authority-distribution response layered atop it.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(maat_order_principle__distributed_maintenance_reading, 0, 400).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(maat_tr_t0, maat_order_principle__distributed_maintenance_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(maat_tr_t80, maat_order_principle__distributed_maintenance_reading, theater_ratio, 80, 0.33).
narrative_ontology:measurement(maat_tr_t160, maat_order_principle__distributed_maintenance_reading, theater_ratio, 160, 0.35).
narrative_ontology:measurement(maat_tr_t240, maat_order_principle__distributed_maintenance_reading, theater_ratio, 240, 0.37).
narrative_ontology:measurement(maat_tr_t320, maat_order_principle__distributed_maintenance_reading, theater_ratio, 320, 0.39).
narrative_ontology:measurement(maat_tr_t400, maat_order_principle__distributed_maintenance_reading, theater_ratio, 400, 0.4).

% Extraction over time
narrative_ontology:measurement(maat_be_t0, maat_order_principle__distributed_maintenance_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(maat_be_t80, maat_order_principle__distributed_maintenance_reading, base_extractiveness, 80, 0.24).
narrative_ontology:measurement(maat_be_t160, maat_order_principle__distributed_maintenance_reading, base_extractiveness, 160, 0.26).
narrative_ontology:measurement(maat_be_t240, maat_order_principle__distributed_maintenance_reading, base_extractiveness, 240, 0.27).
narrative_ontology:measurement(maat_be_t320, maat_order_principle__distributed_maintenance_reading, base_extractiveness, 320, 0.28).
narrative_ontology:measurement(maat_be_t400, maat_order_principle__distributed_maintenance_reading, base_extractiveness, 400, 0.28).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(maat_order_principle__distributed_maintenance_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(maat_order_principle__distributed_maintenance_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(maat_order_principle__distributed_maintenance_reading, 0.1).
narrative_ontology:affects_constraint(maat_order_principle__distributed_maintenance_reading, maat_order_principle__divine_mandate_reading).
narrative_ontology:affects_constraint(maat_order_principle__distributed_maintenance_reading, maat_order_principle__reciprocity_reading).

% DUAL FORMULATION NOTE:
% Three sibling readings of the maat_order_principle kernel: distributed_maintenance_reading (this story — lowest extraction, distributed accountability, multiple interpretive seats), divine_mandate_reading (authority concentrated exclusively in Pharaoh as cosmic embodiment, definitionally unaccountable, highest expected extraction), reciprocity_reading (bargained mutual obligation between Pharaoh and subjects, extraction contingent on whether Pharaoh delivers the reciprocal justice/resource-distribution side). Each reading carries its own ε and stakeholder structure per the ε-invariance principle; they are linked here rather than merged because the 'BGS-style' observable-dependence test fails to hold a single stable ε across framings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
