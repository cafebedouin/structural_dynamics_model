% ============================================================================
% CONSTRAINT STORY: supermajority_threshold__minoritarian_veto_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_supermajority_threshold__minoritarian_veto_reading, []).

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
    narrative_ontology:stakeholder_gain_flow/2,
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
 *   constraint_id: supermajority_threshold__minoritarian_veto_reading
 *   human_readable: Supermajority Amendment Threshold as Entrenched Minoritarian Veto
 *   domain: constitutional/political economy
 *
 * SUMMARY:
 *   This is the minoritarian-veto reading of the supermajority threshold
 *   kernel: the same textual amendment rule read as an entrenchment mechanism
 *   rather than as a consensus safeguard. Under this reading, the threshold's
 *   ratification math is calibrated not to current consensus-formation
 *   dynamics but to a historically contingent apportionment settlement that
 *   persistently overweights a shrinking or geographically concentrated bloc.
 *   That bloc uses its blocking position to convert what began as a
 *   structural bargain into a permanent veto over constitutional correction,
 *   regardless of how large or durable the contrary majority becomes. The
 *   coordination story (protecting against transient passions) is real as
 *   rhetoric but this reading holds it functions as cover for a standing
 *   extraction: numerical majorities are permanently taxed in political
 *   capital and organizing effort for reforms that never clear ratification,
 *   while the blocking minority pays nothing to maintain its veto beyond
 *   ordinary coalition maintenance.
 *
 * KEY AGENTS:
 *   - malapportioned_rural_jurisdictions: primary beneficiary (organized/arbitrage) — holds disproportionate blocking weight
 *   - incumbent_property_interests: beneficiary (powerful/arbitrage) — funds coalition maintenance to preserve entrenchment
 *   - originalist_judicial_establishment: beneficiary/agenda_setter (institutional/arbitrage) — interpretive authority insulated by amendment difficulty
 *   - contemporary_reform_majorities: primary victim (moderate/trapped) — repeatedly defeated despite numerical support
 *   - underrepresented_urban_populations: victim (powerless/trapped) — doubly underweighted
 *   - disenfranchised_constituencies_seeking_redress: victim (powerless/trapped) — structural remedy blocked at ratification stage
 *   - constitutional_law_scholars: analytical observer (analytical) — documents the apportionment-to-blocking-power correlation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(supermajority_threshold__minoritarian_veto_reading, 0.71).
domain_priors:suppression_score(supermajority_threshold__minoritarian_veto_reading, 0.68).
domain_priors:theater_ratio(supermajority_threshold__minoritarian_veto_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(supermajority_threshold__minoritarian_veto_reading, extractiveness, 0.71).
narrative_ontology:constraint_metric(supermajority_threshold__minoritarian_veto_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(supermajority_threshold__minoritarian_veto_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(supermajority_threshold__minoritarian_veto_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(supermajority_threshold__minoritarian_veto_reading, resistance, 0.74).

% --- Constraint claim ---
narrative_ontology:constraint_claim(supermajority_threshold__minoritarian_veto_reading, snare).
narrative_ontology:human_readable(supermajority_threshold__minoritarian_veto_reading, "Supermajority Amendment Threshold as Entrenched Minoritarian Veto").
narrative_ontology:topic_domain(supermajority_threshold__minoritarian_veto_reading, "constitutional/political economy").

domain_priors:requires_active_enforcement(supermajority_threshold__minoritarian_veto_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(supermajority_threshold__minoritarian_veto_reading, '52b95e98-18d4-44b5-a2dd-7aa568ded9da').
narrative_ontology:cs_kernel_codification('52b95e98-18d4-44b5-a2dd-7aa568ded9da', fixed_text).
narrative_ontology:cs_authority_grounding('52b95e98-18d4-44b5-a2dd-7aa568ded9da', extraction).
narrative_ontology:cs_interpretation_layer_present('52b95e98-18d4-44b5-a2dd-7aa568ded9da').
narrative_ontology:cs_reading_relation('52b95e98-18d4-44b5-a2dd-7aa568ded9da', supermajority_threshold__consensus_safeguard_reading, coexists_with).
narrative_ontology:cs_reading_relation('52b95e98-18d4-44b5-a2dd-7aa568ded9da', supermajority_threshold__adaptive_gradient_reading, influences).
narrative_ontology:cs_axiom('52b95e98-18d4-44b5-a2dd-7aa568ded9da', foundational, apportionment_derived_power_is_illegitimate_absent_current_consent).
narrative_ontology:cs_axiom_status(apportionment_derived_power_is_illegitimate_absent_current_consent, holdable).
narrative_ontology:cs_axiom_grounding('52b95e98-18d4-44b5-a2dd-7aa568ded9da', apportionment_derived_power_is_illegitimate_absent_current_consent, deontological).
narrative_ontology:cs_axiom('52b95e98-18d4-44b5-a2dd-7aa568ded9da', secondary, ratification_failure_pattern_tracks_blocking_power_not_passion).
narrative_ontology:cs_axiom_status(ratification_failure_pattern_tracks_blocking_power_not_passion, holdable).
narrative_ontology:cs_axiom_grounding('52b95e98-18d4-44b5-a2dd-7aa568ded9da', ratification_failure_pattern_tracks_blocking_power_not_passion, empirically_contingent).
narrative_ontology:cs_reference_frame('52b95e98-18d4-44b5-a2dd-7aa568ded9da', founding_era_apportionment_bargain).
narrative_ontology:cs_drift_state('52b95e98-18d4-44b5-a2dd-7aa568ded9da', contemporary_demographic_divergence, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('52b95e98-18d4-44b5-a2dd-7aa568ded9da', '').
narrative_ontology:cs_kernel_id(supermajority_threshold__minoritarian_veto_reading, supermajority_threshold).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(supermajority_threshold__minoritarian_veto_reading, malapportioned_rural_jurisdictions).
narrative_ontology:constraint_beneficiary(supermajority_threshold__minoritarian_veto_reading, incumbent_property_interests).
narrative_ontology:constraint_beneficiary(supermajority_threshold__minoritarian_veto_reading, originalist_judicial_establishment).
narrative_ontology:constraint_beneficiary(supermajority_threshold__minoritarian_veto_reading, status_quo_political_factions).
narrative_ontology:constraint_victim(supermajority_threshold__minoritarian_veto_reading, contemporary_reform_majorities).
narrative_ontology:constraint_victim(supermajority_threshold__minoritarian_veto_reading, underrepresented_urban_populations).
narrative_ontology:constraint_victim(supermajority_threshold__minoritarian_veto_reading, disenfranchised_constituencies_seeking_redress).
narrative_ontology:constraint_victim(supermajority_threshold__minoritarian_veto_reading, future_generations_bound_by_frozen_settlement).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold blocking-minority weight in the amendment process wildly disproportionate to population share, a legacy of historical apportionment bargains. Can veto any amendment package by withholding ratification from a small number of jurisdictions, and use this leverage to extract concessions or simply freeze reform indefinitely. Face no penalty for exercising the veto repeatedly.
narrative_ontology:constraint_stakeholder(supermajority_threshold__minoritarian_veto_reading, malapportioned_rural_jurisdictions, beneficiary,
    organized, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(supermajority_threshold__minoritarian_veto_reading, malapportioned_rural_jurisdictions, agenda_setter).

% Benefit from the current constitutional settlement's protection of existing property and contract arrangements. Fund and organize the coalition-maintenance work that keeps the blocking minority intact across election cycles, because the amendment threshold makes their advantage durable against ordinary legislative or electoral reversal.
narrative_ontology:constraint_stakeholder(supermajority_threshold__minoritarian_veto_reading, incumbent_property_interests, beneficiary,
    powerful, generational, arbitrage, national).

% Interprets the frozen text and gains authority precisely because the text cannot be easily changed to override contested rulings. Has professional and institutional incentive to characterize the high threshold as principled restraint rather than as the mechanism that insulates its own interpretive power from correction.
narrative_ontology:constraint_stakeholder(supermajority_threshold__minoritarian_veto_reading, originalist_judicial_establishment, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(supermajority_threshold__minoritarian_veto_reading, originalist_judicial_establishment, agenda_setter).

% Currently hold enough of the blocking share to prevent amendments that would dilute their structural advantages (districting rules, chamber composition, veto points). Campaign to preserve the threshold itself as a top strategic priority, since losing it would expose their other advantages to majoritarian correction.
narrative_ontology:constraint_stakeholder(supermajority_threshold__minoritarian_veto_reading, status_quo_political_factions, beneficiary,
    organized, biographical, mobile, national).

% Repeatedly assemble numerical majorities — sometimes overwhelming ones in polling and popular vote — favoring specific amendments, only to see the proposals die in the ratification stage against a blocking minority representing a much smaller population share. Have no lawful path to change the constitution that does not run through the same threshold that defeats them.
narrative_ontology:constraint_stakeholder(supermajority_threshold__minoritarian_veto_reading, contemporary_reform_majorities, payer,
    moderate, biographical, trapped, national).

% Live under an apportionment structure that gives their preferences systematically less weight in the amendment calculus than an equivalent number of residents in over-represented jurisdictions. Bear the compounding effect: underrepresented in ordinary legislation and doubly underrepresented in the supermajority ratification math.
narrative_ontology:constraint_stakeholder(supermajority_threshold__minoritarian_veto_reading, underrepresented_urban_populations, payer,
    powerless, biographical, trapped, regional).

% Seek constitutional remedy for a specific historical injury (voting rights, discriminatory structural provisions) that ordinary legislation cannot fully cure. Find that the same threshold that would need to be cleared to entrench their remedy permanently is set exactly high enough that the coalition that benefits from the injury's persistence can block the cure indefinitely.
narrative_ontology:constraint_stakeholder(supermajority_threshold__minoritarian_veto_reading, disenfranchised_constituencies_seeking_redress, payer,
    powerless, generational, trapped, national).

% Inherit a governing document calibrated to bargains struck among factions long dead, with no meaningful capacity to renegotiate its core terms because the same entrenched blocking coalition (or its structural heirs) persists across generations. Cannot exit the jurisdiction of the constitution without abandoning citizenship entirely.
narrative_ontology:constraint_stakeholder(supermajority_threshold__minoritarian_veto_reading, future_generations_bound_by_frozen_settlement, payer,
    powerless, civilizational, trapped, national).

% Study ratification patterns, apportionment history, and amendment failure rates. Document the gap between formal amendability and functional amendability, and increasingly characterize supermajority ratification thresholds calibrated to historically contingent apportionment as structurally anti-majoritarian rather than neutral safeguards.
narrative_ontology:constraint_stakeholder(supermajority_threshold__minoritarian_veto_reading, constitutional_law_scholars, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(supermajority_threshold__minoritarian_veto_reading, diffuse).
narrative_ontology:fixing_cost_class(supermajority_threshold__minoritarian_veto_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: In principle, the threshold coordinates by requiring broad buy-in before altering foundational rules, preventing narrow transient majorities from rewriting the basic structure repeatedly. That coordination story is the cover; this reading holds the actual operative function is different.
% TRANSFER_FUNCTION: Moves political power and the capacity for structural self-correction from numerical majorities (especially underrepresented urban and historically disenfranchised populations) to a blocking minority whose weight derives from historical apportionment bargains rather than current population share or current consent.
% ABSENT_VOICES: The populations whose apportionment weight was fixed at founding-era bargains had no voice in setting the threshold that now binds their descendants; contemporary reform coalitions that clear ordinary majoritarian and even large supermajority thresholds in public opinion still have no forum in which their numbers translate into ratification power.
% DISAPPEARANCE_RATIONALE: If the supermajority threshold vanished and amendments could pass by simple national majority, the blocking coalition's veto leverage would disappear overnight; previously stalled amendments with durable majority support would proceed, apportionment-based advantages would lose their entrenchment function, and the coalition currently organized around defending the threshold would have to compete on ordinary majoritarian terms like everyone else.
% FOUNDING_PROBLEM: The stated founding problem was preventing transient majoritarian passion from destabilizing foundational governance arrangements — protecting minority rights and long-run stability against short-term factional swings.
% FOUNDING_PROBLEM_CORROBORATION: Independent political scientists and comparative constitutional scholars outside the beneficiary coalition attest that empirically, the threshold's blocking function tracks apportionment-derived minority power rather than any measurable 'transient passion' problem — ratification failure rates correlate with population-weight mismatch, not with amendment volatility or public opinion instability. The beneficiary coalition itself is the primary voice asserting the founding problem remains live; this reading treats that self-assertion as insufficient corroboration.
narrative_ontology:disappearance_verdict(supermajority_threshold__minoritarian_veto_reading, world_rearranges).
narrative_ontology:founding_problem_status(supermajority_threshold__minoritarian_veto_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(supermajority_threshold__minoritarian_veto_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(supermajority_threshold__minoritarian_veto_reading, 'none', 1).
narrative_ontology:epsilon_provenance(supermajority_threshold__minoritarian_veto_reading, 0.71, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(supermajority_threshold__minoritarian_veto_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(supermajority_threshold__minoritarian_veto_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(supermajority_threshold__minoritarian_veto_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.71) and suppression (0.68) are authored high because the operative mechanism — apportionment-derived blocking power — transfers a persistent structural advantage to a coalition whose position does not track current population or consent, and that advantage is actively defended (constitutional litigation, coalition-maintenance campaigns) rather than passively inherited. Theater ratio (0.42) reflects that a meaningful share of public defense of the threshold invokes the 'protects against transient passion' rationale even as the empirical ratification-failure pattern this reading emphasizes tracks apportionment weight, not passion metrics — the rhetoric increasingly outruns the function it claims. Accessibility collapse (0.58) is moderate rather than extreme: formal amendment paths remain nominally open, so alternatives haven't fully vanished, but repeated failure has taught reform coalitions that the path is not realistically traversable. Resistance (0.74) is high because reform majorities persistently organize, litigate, and campaign against the threshold itself, which is precisely what a durable entrenchment mechanism should generate.
 *
 * PERSPECTIVAL GAP:
 *   From the blocking coalition's seat, the threshold reads as the constitution simply operating as designed — a stability feature. From the reform-majority seat operating under this reading, the identical rule reads as an unaccountable veto that has never once, across the measured interval, yielded to a demonstrated and durable majority preference. The engine computes these as structurally different experiences of the same rule from different structural positions, not as a difference of opinion about a shared experience.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (malapportioned jurisdictions, incumbent property interests, the originalist judiciary, status-quo factions) sit near the full-beneficiary end: the threshold subsidizes their capacity to block change and their exit options are effectively arbitrage-grade because they can shift strategy across venues (judicial, legislative, ratification-blocking) without losing their veto. Victims (reform majorities, underrepresented urban populations, disenfranchised constituencies, future generations) sit near the full-target end: they are trapped — no exit exists from the jurisdiction of the constitution short of secession or generational timescales, and their numerical strength does not translate into structural leverage under this reading.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading treats the founding problem (preventing transient majoritarian passion) as contested rather than resolved-live or resolved-dead: the beneficiary coalition insists it remains live, but corroborating evidence from outside that coalition ties the actual blocking pattern to apportionment weight rather than passion volatility. The classification as snare (under this reading) exists specifically to prevent the coordination story from permanently immunizing an apportionment-driven veto from scrutiny — the point of separating this reading from the consensus_safeguard_reading is that the same textual rule cannot be allowed to hide behind its most flattering justification when the empirical operation, viewed through this reading's lens, tracks something structurally different.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    apportionment_weight_vs_transient_passion_correlation,
    'Does ratification failure under the supermajority threshold correlate more strongly with apportionment-derived blocking weight, or with genuine indicators of transient majoritarian passion (opinion volatility, short-lived issue salience)?',
    'Comparative empirical analysis of failed amendment attempts across the interval: cross-reference ratification failure against (a) population-weighted vs. unit-weighted vote margins and (b) longitudinal persistence of public support for the proposal (a proxy for ''passion'' vs. durable consensus).',
    'If failure correlates with apportionment weight and not with passion volatility, this reading''s snare classification is empirically supported over the consensus_safeguard_reading. If failure correlates with genuine passion volatility, the consensus_safeguard_reading is empirically favored and this reading''s beneficiary/victim framing would need revision.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(apportionment_weight_vs_transient_passion_correlation, empirical, 'Whether the threshold''s blocking function tracks apportionment power or genuine consensus-instability.').

omega_variable(
    reading_selection_grounds,
    'Is the choice to read the threshold as minoritarian-veto (rather than consensus-safeguard or adaptive-gradient) itself a contestable framing decision, or does the apportionment-weight correlation settle it?',
    'Track whether independent constitutional scholarship converges on one reading as the empirically dominant characterization over multiple amendment cycles, versus remaining permanently contested across ideological lines.',
    'If scholarly consensus converges on the entrenchment reading, this constraint''s classification gains confidence independent of any single observer''s priors. If it remains split along predictable partisan/ideological lines, the reading itself is evidence of contested kernel structure rather than resolved fact.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_selection_grounds, conceptual, 'Whether this reading''s selection is empirically forced or remains a genuinely contested framing choice.').

omega_variable(
    future_generations_consent_problem,
    'Can a threshold calibrated to a historical apportionment bargain bind future generations legitimately at all, independent of whether the current blocking coalition is ''entrenched'' in a pejorative sense?',
    'Normative/philosophical analysis of intergenerational consent theory as applied to constitutional design; no empirical resolution available.',
    'If intergenerational binding is illegitimate as a category, this reading''s victim classification for ''future_generations_bound_by_frozen_settlement'' holds regardless of any recalibration; if intergenerational binding is treated as a normal and legitimate feature of constitutionalism, this victim category weakens substantially.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(future_generations_consent_problem, preference, 'Whether intergenerational binding by a fixed threshold is legitimate in principle.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(supermajority_threshold__minoritarian_veto_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(supe_tr_t0, supermajority_threshold__minoritarian_veto_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(supe_tr_t20, supermajority_threshold__minoritarian_veto_reading, theater_ratio, 20, 0.26).
narrative_ontology:measurement(supe_tr_t40, supermajority_threshold__minoritarian_veto_reading, theater_ratio, 40, 0.31).
narrative_ontology:measurement(supe_tr_t60, supermajority_threshold__minoritarian_veto_reading, theater_ratio, 60, 0.35).
narrative_ontology:measurement(supe_tr_t80, supermajority_threshold__minoritarian_veto_reading, theater_ratio, 80, 0.39).
narrative_ontology:measurement(supe_tr_t100, supermajority_threshold__minoritarian_veto_reading, theater_ratio, 100, 0.42).

% Extraction over time
narrative_ontology:measurement(supe_be_t0, supermajority_threshold__minoritarian_veto_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(supe_be_t20, supermajority_threshold__minoritarian_veto_reading, base_extractiveness, 20, 0.51).
narrative_ontology:measurement(supe_be_t40, supermajority_threshold__minoritarian_veto_reading, base_extractiveness, 40, 0.58).
narrative_ontology:measurement(supe_be_t60, supermajority_threshold__minoritarian_veto_reading, base_extractiveness, 60, 0.63).
narrative_ontology:measurement(supe_be_t80, supermajority_threshold__minoritarian_veto_reading, base_extractiveness, 80, 0.68).
narrative_ontology:measurement(supe_be_t100, supermajority_threshold__minoritarian_veto_reading, base_extractiveness, 100, 0.71).

% Suppression requirement over time
narrative_ontology:measurement(supe_su_t0, supermajority_threshold__minoritarian_veto_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(supe_su_t20, supermajority_threshold__minoritarian_veto_reading, suppression_requirement, 20, 0.55).
narrative_ontology:measurement(supe_su_t40, supermajority_threshold__minoritarian_veto_reading, suppression_requirement, 40, 0.59).
narrative_ontology:measurement(supe_su_t60, supermajority_threshold__minoritarian_veto_reading, suppression_requirement, 60, 0.62).
narrative_ontology:measurement(supe_su_t80, supermajority_threshold__minoritarian_veto_reading, suppression_requirement, 80, 0.65).
narrative_ontology:measurement(supe_su_t100, supermajority_threshold__minoritarian_veto_reading, suppression_requirement, 100, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(supermajority_threshold__minoritarian_veto_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(supermajority_threshold__minoritarian_veto_reading, 0.1).
narrative_ontology:affects_constraint(supermajority_threshold__minoritarian_veto_reading, supermajority_threshold__consensus_safeguard_reading).
narrative_ontology:affects_constraint(supermajority_threshold__minoritarian_veto_reading, supermajority_threshold__adaptive_gradient_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the supermajority_threshold kernel (same textual amendment rule, three structurally distinct constraints). consensus_safeguard_reading claims the identical rule as a rope (genuine consensus filter, minimal extraction). adaptive_gradient_reading claims it as an empirically-contingent scaffold/rope requiring evidence-based calibration. This reading (minoritarian_veto_reading) claims it as a snare with concentrated beneficiaries (apportionment-advantaged blocking coalitions) and concentrated victims (structurally underrepresented reform majorities). Each reading has its own ε, its own beneficiary/victim declarations, and its own claimed_type; none is derived from or averaged with the others. Link all three via affects_constraints so contamination/coupling analysis can trace how a purity finding in one reading bears on public legitimacy of the shared kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
