% ============================================================================
% CONSTRAINT STORY: stone_land_use_rule__behavioral_competence
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_stone_land_use_rule__behavioral_competence, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: stone_land_use_rule__behavioral_competence
 *   human_readable: Tsunami Stone Line as Living Land-Use Prohibition (Behavioral Competence Reading)
 *   domain: disaster_anthropology/institutional_memory/land_use_governance
 *
 * SUMMARY:
 *   After a great wave destroyed the settlement and killed a large share of
 *   its inhabitants, survivors cut stones at the highest reach of the water
 *   and carved an instruction: do not build below this point. For
 *   seventy-eight years the village has kept its houses, storehouses, wells,
 *   and paths above that line, not because a government enforces it but
 *   because daily spatial practice does: children are taught the carvings'
 *   meaning, the annual observance walks the line, households clear the
 *   markers, and newcomers scouting the flat shorefront are corrected before
 *   foundations are dug. This file instantiates the behavioral_competence
 *   reading of the stone_land_use_rule kernel: the inscription as a live
 *   land-use prohibition with sustained behavioral force. The sibling reading
 *   (stone_land_use_rule__commemorative_husk) treats the same stones as
 *   memorial artifacts whose warning has lost behavioral force; that is a
 *   separate constraint story with its own epsilon, linked via
 *   network.affects_constraints. Per the epsilon-referent rule,
 *   extractiveness here is authored over the standing arrangement under
 *   contest, the prohibition as actually practiced, assessed by this
 *   reading's lights: the costs it imposes (steeper sites, uphill hauling,
 *   forgone shorefront value) are real but bounded, and no seat collects them
 *   as rent. KEY AGENTS (by structural relationship): - village_households:
 *   primary participant-beneficiaries (organized/constrained) — bear the
 *   hill-climb cost and receive the protection - descendant_villagers:
 *   intergenerational beneficiaries (powerless/trapped) — inherit both
 *   protection and transmission duty - memory_keepers: agenda-setters
 *   (organized/identity_locked) — administer the stones, the teachings, and
 *   line arbitration - lowland_plot_seekers: marginal cost-bearers
 *   (moderate/mobile) — denied cheap shorefront plots, protected anyway -
 *   disaster_researchers: analytical observers — assess whether the practice
 *   still binds - prefectural_planning_office: institutional observers — map
 *   hazards, defer to the line
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(stone_land_use_rule__behavioral_competence, 0.16).
domain_priors:suppression_score(stone_land_use_rule__behavioral_competence, 0.34).
domain_priors:theater_ratio(stone_land_use_rule__behavioral_competence, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(stone_land_use_rule__behavioral_competence, extractiveness, 0.16).
narrative_ontology:constraint_metric(stone_land_use_rule__behavioral_competence, suppression_requirement, 0.34).
narrative_ontology:constraint_metric(stone_land_use_rule__behavioral_competence, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(stone_land_use_rule__behavioral_competence, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(stone_land_use_rule__behavioral_competence, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(stone_land_use_rule__behavioral_competence, rope).
narrative_ontology:human_readable(stone_land_use_rule__behavioral_competence, "Tsunami Stone Line as Living Land-Use Prohibition (Behavioral Competence Reading)").
narrative_ontology:topic_domain(stone_land_use_rule__behavioral_competence, "disaster_anthropology/institutional_memory/land_use_governance").

domain_priors:requires_active_enforcement(stone_land_use_rule__behavioral_competence).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(stone_land_use_rule__behavioral_competence, '90f6b732-cb8e-41c8-8934-f7a5a978276f').
narrative_ontology:cs_kernel_codification('90f6b732-cb8e-41c8-8934-f7a5a978276f', fixed_text).
narrative_ontology:cs_authority_grounding('90f6b732-cb8e-41c8-8934-f7a5a978276f', practice).
narrative_ontology:cs_interpretation_layer_present('90f6b732-cb8e-41c8-8934-f7a5a978276f').
narrative_ontology:cs_reading_relation('90f6b732-cb8e-41c8-8934-f7a5a978276f', stone_land_use_rule__commemorative_husk, forecloses).
narrative_ontology:cs_axiom('90f6b732-cb8e-41c8-8934-f7a5a978276f', foundational, carved_instruction_binds_land_use).
narrative_ontology:cs_axiom_status(carved_instruction_binds_land_use, holdable).
narrative_ontology:cs_axiom_grounding('90f6b732-cb8e-41c8-8934-f7a5a978276f', carved_instruction_binds_land_use, conventional).
narrative_ontology:cs_axiom('90f6b732-cb8e-41c8-8934-f7a5a978276f', secondary, stoneline_marks_observed_runup_limit).
narrative_ontology:cs_axiom_status(stoneline_marks_observed_runup_limit, holdable).
narrative_ontology:cs_axiom_grounding('90f6b732-cb8e-41c8-8934-f7a5a978276f', stoneline_marks_observed_runup_limit, empirically_contingent).
narrative_ontology:cs_reference_frame('90f6b732-cb8e-41c8-8934-f7a5a978276f', binding_runup_prohibition).
narrative_ontology:cs_drift_state('90f6b732-cb8e-41c8-8934-f7a5a978276f', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('90f6b732-cb8e-41c8-8934-f7a5a978276f', '').
narrative_ontology:cs_kernel_id(stone_land_use_rule__behavioral_competence, stone_land_use_rule).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(stone_land_use_rule__behavioral_competence, village_households).
narrative_ontology:constraint_beneficiary(stone_land_use_rule__behavioral_competence, descendant_villagers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(stone_land_use_rule__behavioral_competence, lowland_plot_seekers).
narrative_ontology:constraint_victim(stone_land_use_rule__behavioral_competence, village_households).
narrative_ontology:constraint_victim(stone_land_use_rule__behavioral_competence, lowland_plot_seekers).
narrative_ontology:constraint_vindicates(stone_land_use_rule__behavioral_competence, intergenerational_customary_warning_efficacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Live in the settlement above the carved line. Each household builds and rebuilds on sloping ground, hauls timber and tile uphill, and accepts smaller or steeper plots than the flat shore offers. Households take turns clearing vegetation from the marker stones, walk the line in the annual observance, and correct newcomers who scout the shorefront for house sites. Leaving the valley would mean abandoning terraces, kin networks, and burial grounds, so exit is possible but costly.
narrative_ontology:constraint_stakeholder(stone_land_use_rule__behavioral_competence, village_households, beneficiary,
    organized, generational, constrained, local).
narrative_ontology:stakeholder_secondary_role(stone_land_use_rule__behavioral_competence, village_households, payer).

% Not yet present but already addressed by the arrangement: the stones, the teachings, and the maintained line exist so that people who do not yet exist will not build below the runup limit. They will inherit both the protection and the obligation to transmit it onward; they cannot opt out of the hazard, and they cannot opt out of the inheritance.
narrative_ontology:constraint_stakeholder(stone_land_use_rule__behavioral_competence, descendant_villagers, beneficiary,
    powerless, generational, trapped, local).

% Elders and shrine custodians who maintain the stones, recite the founding account at the annual observance, teach children what the carvings mean, and arbitrate disputes about where the line runs when erosion or roadwork blurs it. Their standing in the village rests on this office; stepping back from it would cost them the role that organizes their late-life identity and their social authority.
narrative_ontology:constraint_stakeholder(stone_land_use_rule__behavioral_competence, memory_keepers, agenda_setter,
    organized, biographical, identity_locked, local).

% Younger couples and in-migrants who would prefer the flat, cheap, road-adjacent land below the line. The custom denies them those plots; they build higher at added cost or look for land in neighboring valleys. Because they can migrate out of the village entirely, their threat of exit is real, and some have taken it, which pressures the village to keep the custom's burdens explainable to outsiders.
narrative_ontology:constraint_stakeholder(stone_land_use_rule__behavioral_competence, lowland_plot_seekers, payer,
    moderate, immediate, mobile, regional).
narrative_ontology:stakeholder_secondary_role(stone_land_use_rule__behavioral_competence, lowland_plot_seekers, beneficiary).

% Field researchers and hazard historians who survey the stones, interview the keepers, and compare the customary line against sediment cores and modeled runup. They publish assessments of whether the practice still binds building decisions, and their findings shape how outside authorities treat the custom.
narrative_ontology:constraint_stakeholder(stone_land_use_rule__behavioral_competence, disaster_researchers, observer,
    analytical, biographical, analytical, global).

% Government planners who draw official hazard maps and building regulations. The customary line predates their mapped zones and in places exceeds them; they consult the keepers when revising maps and generally avoid publicly contradicting the stone line, treating it as locally authoritative knowledge.
narrative_ontology:constraint_stakeholder(stone_land_use_rule__behavioral_competence, prefectural_planning_office, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(stone_land_use_rule__behavioral_competence, diffuse).
narrative_ontology:fixing_cost_class(stone_land_use_rule__behavioral_competence, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Transmits hard-won hazard knowledge across generations and coordinates settlement placement out of the tsunami runup zone, solving the problem that each generation's cheapest, most convenient land lies exactly where the next great wave will kill whoever builds on it.
% TRANSFER_FUNCTION: Moves construction effort and land value uphill: converts cheap shorefront convenience into distributed survival probability, and moves maintenance labor, teaching time, and ritual attention from each generation to the next.
% ABSENT_VOICES: Those who died in the waves before the stones were cut cannot speak, and their absence is the arrangement's entire authority. Would-be shorefront developers and skeptical newcomers see idle flat land where the village sees a kill zone; they are present only at the margins, corrected rather than convened. Outsiders who read the stones as mere memorial hold the sibling reading and are heard in print, not in the village.
% DISAPPEARANCE_RATIONALE: If the prohibition vanished overnight, nothing dramatic happens at first, which is exactly the danger: within a generation or two, cheap flat land would fill with houses, barns, and roads down to the shore, and the next great wave would find the settlement rebuilt inside the runup zone. The village's entire spatial footprint depends on the line holding.
% FOUNDING_PROBLEM: A catastrophic wave destroyed the settlement and killed a large share of its inhabitants; survivors erected stones at the observed limit of the water with an instruction to descendants never to build below that point.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set: tsunami deposit stratigraphy in the valley soils records multiple pre-stone inundations reaching approximately the carved elevation; regional seismic history documents the recurring subduction source; modern hazard assessment independently maps a runup zone close to the line. None of that evidence was produced by any party inside the arrangement.
narrative_ontology:disappearance_verdict(stone_land_use_rule__behavioral_competence, world_rearranges).
narrative_ontology:founding_problem_status(stone_land_use_rule__behavioral_competence, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(stone_land_use_rule__behavioral_competence, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(stone_land_use_rule__behavioral_competence, 'none', 1).
narrative_ontology:epsilon_provenance(stone_land_use_rule__behavioral_competence, 0.16, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(stone_land_use_rule__behavioral_competence_tests).
:- end_tests(stone_land_use_rule__behavioral_competence_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.16) because the arrangement imposes bounded real costs — steeper sites, uphill material hauling, forgone shorefront value — with a slow upward creep as upland plots fill and scarcity raises the price of compliance; no rent is collected by any seat. Suppression (0.34) reflects genuine but apparatus-free coercive overhead: communal sanction, drilled children, correction of deviants, and real social cost for boundary-testing, with no barred exits and no enforcement bureaucracy. Theater ratio is low (0.10): the ritual elements exist but track the line's actual governance of placement, and the measurement series shows theater rising during complacency phases and dropping after renewal events. Accessibility collapse (0.55) is moderate: once the hazard is understood, building low loses rational appeal, but the collapse reflects the underlying hazard rather than suppression by the constraint — physical alternatives persist, which is why this is not a mountain profile. Resistance is low (0.15): occasional boundary-testing, no organized opposition, and a 78-year compliance record. Coordination type is identity_coordination: compliance functions as a membership norm — to be a proper villager is to respect the line — and the dominant failure mode is decay of the practice complex, not failure of the encoding or of an allocation market; the type default floor (0.08) is used, and measured epsilon sits modestly above it, which is diagnostically appropriate rather than tuned. The temporal series run on one shared nine-point grid (T=0..78) so every tracked metric is authored at every examined time point. The series are deliberately cyclical: enforcement intensity (suppression_requirement) decays between hazard events and renews after them, with renewal events near year 30 and year 62 producing spikes at the following grid points (T=40, T=70); theater moves inversely, peaking in late-complacency phases (T=30, T=60). The oscillation is partly functional — each renewal re-functionalizes the practice — but the complacency phase is where husk-drift risk concentrates, and the intermittent-reinforcement character of event-driven renewal is documented rather than treated as noise.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the village_households seat the arrangement is insurance the community collectively owns: they pay the hill-climb cost and receive the protection, and the same structure reads as a rope. From the lowland_plot_seekers seat the identical structure reads as a denial of cheap land justified by stories they did not witness and obligations they did not incur — a materially costlier experience of the same line, moderated by their real exit option. From the memory_keepers seat the arrangement is a life's office: their identity, standing, and daily schedule are constituted by maintaining it, so exit is not a menu item. The descendant seat cannot consent at all yet inherits the obligation — a trapped beneficiary the engine should read as structurally unable to renegotiate. The engine computes these per-seat classifications from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries are declared as village_households and descendant_villagers, driving both seats toward the beneficiary end of directionality. The memory_keepers administer the arrangement but collect no material rent — only status — so their derived directionality sits mid-low. The one override corrects a known derivation blind spot: lowland_plot_seekers appear in no victim declaration because they are not victims — the arrangement protects them and they are net beneficiaries — yet they visibly bear the marginal cost of the prohibition, so beneficiary-side derivation alone would read them as near-full beneficiaries and understate their cost position; the override sets their d to 0.45, reflecting cost borne without extraction suffered. The override targets the moderate power atom, which in this story only lowland_plot_seekers occupy, so no other seat is affected. Observers carry analytical exit and do not feed the extraction arithmetic.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — descendants rebuilding inside the kill zone — remains live: the sea will send another wave, and the sediment record shows it has done so repeatedly. Founding_problem_status is therefore live, disappearance_verdict is world_rearranges, and the mismatch consumer finds no dead-mandate-plus-dependents flag. The classification discipline guards against two symmetrical errors. The first is the husk error: reading the ritual and commemorative elements as proof that the constraint is mostly performance (piton) when the practice still governs where foundations go — theater_ratio is authored low precisely because the ritual tracks function here. The second is the snare error: reading the real costs imposed on plot seekers as extraction, when no seat collects those costs as gain; they are converted into diffuse survival probability. Mandatrophy resolution here is not that the mandate expired but that it never needed resolving: the arrangement's function and its persistence mechanism are the same activity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_contest_behavioral_force,
    'Does the stone line still bind land-use decisions (this reading, behavioral_competence) or has it decayed to commemoration without behavioral force (sibling reading, commemorative_husk)?',
    'Plot-level behavioral audit: construction dates and footprints relative to the carved line across the last century; violation and correction rates; whether proposed lowland builds are abandoned when keepers object, and whether corrections stick without keeper involvement.',
    'If audits show systematic lowland building tolerated, this reading collapses into the commemorative_husk reading, theater_ratio rises sharply, and classification drifts toward piton; sustained correction confirms the live-prohibition reading and the rope classification stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_contest_behavioral_force, empirical, 'Kernel contest: live prohibition versus commemorative husk — the single structural element on which the sibling readings diverge.').

omega_variable(
    runup_bound_adequacy,
    'Does the carved line mark the maximum credible runup, or only the largest wave historically observed when the stones were cut?',
    'Paleotsunami sediment coring and worst-case subduction rupture modeling compared against stone elevations and the mapped modern runup zone.',
    'If the line sits below worst-case runup, the arrangement underprotects and part of its low measured extraction reflects inadequate rather than efficient restraint; if it matches or exceeds the credible maximum, the rope reading is secure and the low epsilon reflects genuine net benefit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(runup_bound_adequacy, empirical, 'Whether the constraint encodes a sufficient hazard bound or a historically lucky one.').

omega_variable(
    cost_burden_distribution,
    'Is the hill-climb cost distributed evenly across households, or concentrated on those least able to afford upland construction?',
    'Household expenditure and plot-value records correlated with residence elevation; interviews on forgone shorefront access and who actually occupies the cheapest compliant plots.',
    'If costs concentrate on poorer households while wealthier members hold the sheltered upland, a tangled_rope shading appears at the payer seats despite the rope-level aggregate; if the burden spreads evenly, the rope classification holds at every seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cost_burden_distribution, empirical, 'Distributional equity of the compliance cost across the beneficiary population.').

omega_variable(
    suppression_internalization_split,
    'Is compliance maintained by active communal sanction (structural enforcement) or by internalized conviction that persists without enforcement?',
    'Observe correction behavior when keepers are absent or incapacitated: if boundary-testing goes uncorrected, enforcement is structural; if households self-police regardless of who is watching, conviction carries the arrangement.',
    'Structural enforcement predicts rapid decay if transmission lapses — the husk trajectory; internalized conviction predicts durable compliance with lower true suppression than the scalar suggests, changing how the suppression measurement should be read.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization_split, empirical, 'Structural versus internalized enforcement mechanism behind the measured suppression.').

omega_variable(
    kernel_framing_underdetermination,
    'Is the kernel the carved inscription itself (fixed_text), or the surrounding practice complex — procession, teaching, maintenance, arbitration — that interprets and enforces it, with the stones as mnemonic anchors (implicit)?',
    'Erase-test reasoning: if the carvings were destroyed but the practice continued with rebuilt markers, the kernel was the practice; if practice lapsed once the text was gone, the text was the kernel. Comparative evidence from villages that lost their stones versus villages that lost their keeper lineages.',
    'Under the practice-kernel framing, kernel_codification shifts from fixed_text to implicit and adjudication disperses across the practitioner body, altering the commitment-system pattern classification; under the text-kernel framing, the current declaration stands.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_underdetermination, conceptual, 'Framing under-determination in the commitment-system structure: text as kernel versus practice as kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(stone_land_use_rule__behavioral_competence, 0, 78).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ston_tr_t0, stone_land_use_rule__behavioral_competence, theater_ratio, 0, 0.06).
narrative_ontology:measurement_basis(ston_tr_t0, observed).
narrative_ontology:measurement(ston_tr_t10, stone_land_use_rule__behavioral_competence, theater_ratio, 10, 0.09).
narrative_ontology:measurement_basis(ston_tr_t10, observed).
narrative_ontology:measurement(ston_tr_t20, stone_land_use_rule__behavioral_competence, theater_ratio, 20, 0.14).
narrative_ontology:measurement_basis(ston_tr_t20, observed).
narrative_ontology:measurement(ston_tr_t30, stone_land_use_rule__behavioral_competence, theater_ratio, 30, 0.18).
narrative_ontology:measurement_basis(ston_tr_t30, observed).
narrative_ontology:measurement(ston_tr_t40, stone_land_use_rule__behavioral_competence, theater_ratio, 40, 0.1).
narrative_ontology:measurement_basis(ston_tr_t40, observed).
narrative_ontology:measurement(ston_tr_t50, stone_land_use_rule__behavioral_competence, theater_ratio, 50, 0.13).
narrative_ontology:measurement_basis(ston_tr_t50, observed).
narrative_ontology:measurement(ston_tr_t60, stone_land_use_rule__behavioral_competence, theater_ratio, 60, 0.17).
narrative_ontology:measurement_basis(ston_tr_t60, observed).
narrative_ontology:measurement(ston_tr_t70, stone_land_use_rule__behavioral_competence, theater_ratio, 70, 0.11).
narrative_ontology:measurement_basis(ston_tr_t70, observed).
narrative_ontology:measurement(ston_tr_t78, stone_land_use_rule__behavioral_competence, theater_ratio, 78, 0.1).
narrative_ontology:measurement_basis(ston_tr_t78, observed).

% Extraction over time
narrative_ontology:measurement(ston_be_t0, stone_land_use_rule__behavioral_competence, base_extractiveness, 0, 0.1).
narrative_ontology:measurement_basis(ston_be_t0, observed).
narrative_ontology:measurement(ston_be_t10, stone_land_use_rule__behavioral_competence, base_extractiveness, 10, 0.11).
narrative_ontology:measurement_basis(ston_be_t10, observed).
narrative_ontology:measurement(ston_be_t20, stone_land_use_rule__behavioral_competence, base_extractiveness, 20, 0.12).
narrative_ontology:measurement_basis(ston_be_t20, observed).
narrative_ontology:measurement(ston_be_t30, stone_land_use_rule__behavioral_competence, base_extractiveness, 30, 0.13).
narrative_ontology:measurement_basis(ston_be_t30, observed).
narrative_ontology:measurement(ston_be_t40, stone_land_use_rule__behavioral_competence, base_extractiveness, 40, 0.13).
narrative_ontology:measurement_basis(ston_be_t40, observed).
narrative_ontology:measurement(ston_be_t50, stone_land_use_rule__behavioral_competence, base_extractiveness, 50, 0.14).
narrative_ontology:measurement_basis(ston_be_t50, observed).
narrative_ontology:measurement(ston_be_t60, stone_land_use_rule__behavioral_competence, base_extractiveness, 60, 0.15).
narrative_ontology:measurement_basis(ston_be_t60, observed).
narrative_ontology:measurement(ston_be_t70, stone_land_use_rule__behavioral_competence, base_extractiveness, 70, 0.16).
narrative_ontology:measurement_basis(ston_be_t70, observed).
narrative_ontology:measurement(ston_be_t78, stone_land_use_rule__behavioral_competence, base_extractiveness, 78, 0.16).
narrative_ontology:measurement_basis(ston_be_t78, observed).

% Suppression requirement over time
narrative_ontology:measurement(ston_su_t0, stone_land_use_rule__behavioral_competence, suppression_requirement, 0, 0.32).
narrative_ontology:measurement_basis(ston_su_t0, observed).
narrative_ontology:measurement(ston_su_t10, stone_land_use_rule__behavioral_competence, suppression_requirement, 10, 0.26).
narrative_ontology:measurement_basis(ston_su_t10, observed).
narrative_ontology:measurement(ston_su_t20, stone_land_use_rule__behavioral_competence, suppression_requirement, 20, 0.21).
narrative_ontology:measurement_basis(ston_su_t20, observed).
narrative_ontology:measurement(ston_su_t30, stone_land_use_rule__behavioral_competence, suppression_requirement, 30, 0.19).
narrative_ontology:measurement_basis(ston_su_t30, observed).
narrative_ontology:measurement(ston_su_t40, stone_land_use_rule__behavioral_competence, suppression_requirement, 40, 0.4).
narrative_ontology:measurement_basis(ston_su_t40, observed).
narrative_ontology:measurement(ston_su_t50, stone_land_use_rule__behavioral_competence, suppression_requirement, 50, 0.31).
narrative_ontology:measurement_basis(ston_su_t50, observed).
narrative_ontology:measurement(ston_su_t60, stone_land_use_rule__behavioral_competence, suppression_requirement, 60, 0.24).
narrative_ontology:measurement_basis(ston_su_t60, observed).
narrative_ontology:measurement(ston_su_t70, stone_land_use_rule__behavioral_competence, suppression_requirement, 70, 0.36).
narrative_ontology:measurement_basis(ston_su_t70, observed).
narrative_ontology:measurement(ston_su_t78, stone_land_use_rule__behavioral_competence, suppression_requirement, 78, 0.34).
narrative_ontology:measurement_basis(ston_su_t78, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(stone_land_use_rule__behavioral_competence, identity_coordination).
narrative_ontology:affects_constraint(stone_land_use_rule__behavioral_competence, stone_land_use_rule__commemorative_husk).

% DUAL FORMULATION NOTE:
% Constraint family from one kernel: stone_land_use_rule decomposes into behavioral_competence (this file — the inscription as binding land-use rule; low epsilon, low theater, practice-enforced, sustained 78-year compliance) and stone_land_use_rule__commemorative_husk (the same stones as memorial artifacts; high theater, negligible behavioral force). The readings disagree on a single structural element — retained behavioral force — so their epsilon values diverge sharply; each file authors its own epsilon over its own referent and links the other via affects_constraints. The upstream/downstream asymmetry runs from this file to the sibling: the demonstrated behavioral record is what makes the 'decayed to symbol' claim an empirical assertion about change rather than a description of origin.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(stone_land_use_rule__behavioral_competence, moderate, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
