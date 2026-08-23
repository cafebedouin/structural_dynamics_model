% ============================================================================
% CONSTRAINT STORY: aneyoshi_land_use_prohibition__behavioral_competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_aneyoshi_land_use_prohibition__behavioral_competence_reading, []).

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
 *   constraint_id: aneyoshi_land_use_prohibition__behavioral_competence_reading
 *   human_readable: Aneyoshi High-Water Stone as Live Land-Use Prohibition (Behavioral Competence Reading)
 *   domain: disaster anthropology/commitment systems/temporal institutional analysis
 *
 * SUMMARY:
 *   On a hillside above the small hamlet of Aneyoshi (Miyako, Iwate
 *   Prefecture) stands a stone tablet carved in 1933 by survivors of the
 *   Showa Sanriku tsunami. The inscription records how far the water reached
 *   in the 1896 and 1933 waves and instructs descendants not to build their
 *   homes below that mark. Across the following 78 years the hamlet kept the
 *   instruction: houses clustered on the terraces above the carved datum,
 *   newcomers learned the rule as ordinary household sense, and the stone was
 *   tended and retold rather than merely visited. When the 2011 Tohoku
 *   tsunami ran up the valley, the water climbed to approximately the height
 *   of the stone; every house stood above it and the hamlet survived the wave
 *   without loss of life, while neighboring settlements that had expanded
 *   onto the flats suffered catastrophic casualties. Under this reading, the
 *   stone operated throughout the interval as a working land-use rule — a
 *   spatially encoded hazard limit governing settlement siting — sustained by
 *   upbringing, consensus, and a transmitter office rather than by statute,
 *   police, or fines. Its costs are reciprocal and small; its protection is
 *   existential. KEY AGENTS (by structural relationship): -
 *   aneyoshi_hamlet_households: joint bearer of the siting cost and recipient
 *   of the survival dividend ([organized]/[constrained]) -
 *   aneyoshi_lineage_transmitters: administrator of the rule — tends the
 *   stone, transmits the prohibition, holds the office of memory
 *   ([organized]/[identity_locked]) - miyako_city_government: incidental
 *   collector — gathers avoided-casualty savings and reputational standing
 *   without running the rule ([institutional]/[mobile]) -
 *   nearshore_rebuild_advocates: absent counterparty — regional constituency
 *   whose preference for water's-edge rebuilding defines what the prohibition
 *   guards against ([organized]/[trapped]) - disaster_anthropology_observers:
 *   analytical observer — documents compliance outcomes across the interval
 *   ([analytical]/[analytical])
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(aneyoshi_land_use_prohibition__behavioral_competence_reading, 0.11).
domain_priors:suppression_score(aneyoshi_land_use_prohibition__behavioral_competence_reading, 0.1).
domain_priors:theater_ratio(aneyoshi_land_use_prohibition__behavioral_competence_reading, 0.06).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(aneyoshi_land_use_prohibition__behavioral_competence_reading, extractiveness, 0.11).
narrative_ontology:constraint_metric(aneyoshi_land_use_prohibition__behavioral_competence_reading, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(aneyoshi_land_use_prohibition__behavioral_competence_reading, theater_ratio, 0.06).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(aneyoshi_land_use_prohibition__behavioral_competence_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(aneyoshi_land_use_prohibition__behavioral_competence_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(aneyoshi_land_use_prohibition__behavioral_competence_reading, rope).
narrative_ontology:human_readable(aneyoshi_land_use_prohibition__behavioral_competence_reading, "Aneyoshi High-Water Stone as Live Land-Use Prohibition (Behavioral Competence Reading)").
narrative_ontology:topic_domain(aneyoshi_land_use_prohibition__behavioral_competence_reading, "disaster anthropology/commitment systems/temporal institutional analysis").

domain_priors:requires_active_enforcement(aneyoshi_land_use_prohibition__behavioral_competence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(aneyoshi_land_use_prohibition__behavioral_competence_reading, '83506d91-44e0-485f-8b55-6b1c660e8172').
narrative_ontology:cs_kernel_codification('83506d91-44e0-485f-8b55-6b1c660e8172', fixed_text).
narrative_ontology:cs_authority_grounding('83506d91-44e0-485f-8b55-6b1c660e8172', lineage).
narrative_ontology:cs_interpretation_layer_present('83506d91-44e0-485f-8b55-6b1c660e8172').
narrative_ontology:cs_reading_relation('83506d91-44e0-485f-8b55-6b1c660e8172', aneyoshi_land_use_prohibition__commemorative_husk_reading, forecloses).
narrative_ontology:cs_axiom('83506d91-44e0-485f-8b55-6b1c660e8172', foundational, carved_runup_record_is_binding_land_use_rule).
narrative_ontology:cs_axiom_status(carved_runup_record_is_binding_land_use_rule, holdable).
narrative_ontology:cs_axiom_grounding('83506d91-44e0-485f-8b55-6b1c660e8172', carved_runup_record_is_binding_land_use_rule, empirically_contingent).
narrative_ontology:cs_axiom('83506d91-44e0-485f-8b55-6b1c660e8172', foundational, intergenerational_transmission_sustains_force).
narrative_ontology:cs_axiom_status(intergenerational_transmission_sustains_force, holdable).
narrative_ontology:cs_axiom_grounding('83506d91-44e0-485f-8b55-6b1c660e8172', intergenerational_transmission_sustains_force, empirically_contingent).
narrative_ontology:cs_reference_frame('83506d91-44e0-485f-8b55-6b1c660e8172', operative_hazard_inscription).
narrative_ontology:cs_drift_state('83506d91-44e0-485f-8b55-6b1c660e8172', post_2011_field_surveys, gap(stable, minor, true)).
narrative_ontology:cs_created_at('83506d91-44e0-485f-8b55-6b1c660e8172', '').
narrative_ontology:cs_kernel_id(aneyoshi_land_use_prohibition__behavioral_competence_reading, aneyoshi_land_use_prohibition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(aneyoshi_land_use_prohibition__behavioral_competence_reading, aneyoshi_hamlet_households).
narrative_ontology:constraint_beneficiary(aneyoshi_land_use_prohibition__behavioral_competence_reading, miyako_city_government).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(aneyoshi_land_use_prohibition__behavioral_competence_reading, aneyoshi_lineage_transmitters).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Live in scattered houses on the terraces above the carved high-water stone; farm small plots and walk down to the shore to fish; teach children the stone's instruction as ordinary household rule. They forgo building sites closer to the water, which on this steep coast have little alternative value, and in return no house in the hamlet stood in the 2011 inundation. Compliance is kept by upbringing and neighborly expectation rather than by patrols or fines; leaving means relocating away from ancestral land and livelihood, which few have done.
narrative_ontology:constraint_stakeholder(aneyoshi_land_use_prohibition__behavioral_competence_reading, aneyoshi_hamlet_households, beneficiary,
    organized, generational, constrained, local).

% Older residents who tend the stone, recite the 1896 and 1933 flood accounts at gatherings, and quietly correct any talk of building downslope. They inherited the duty from parents who survived the earlier waves, and their standing in the hamlet rests on faithful keeping of the practice. Setting the practice aside would forfeit their role in the community rather than merely change a preference; there is no material reward attached to keeping it, only the office itself.
narrative_ontology:constraint_stakeholder(aneyoshi_land_use_prohibition__behavioral_competence_reading, aneyoshi_lineage_transmitters, agenda_setter,
    organized, generational, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(aneyoshi_land_use_prohibition__behavioral_competence_reading, aneyoshi_lineage_transmitters, beneficiary).

% The municipal office responsible for the area: maintains hazard maps, funded post-2011 rebuilding on high ground, and cites the hamlet as a success case in its resilience reporting. It did not erect the stone and does not police the line day to day, but it enjoys smaller disaster bills and a reputation for prudence whenever the hamlet comes through a wave intact. Its own hazard zoning is a separate instrument it controls and can revise at will.
narrative_ontology:constraint_stakeholder(aneyoshi_land_use_prohibition__behavioral_competence_reading, miyako_city_government, beneficiary,
    institutional, generational, mobile, regional).

% Fisher and shopkeeper households in neighboring port towns who after the 2011 wave pressed to rebuild at the water's edge so that boats, nets, and storefronts stay within walking distance. They hold no seat in this hamlet's consensus — the hamlet never hosted a faction pressing to relax the line — yet they are the population whose contrary preference defines what the prohibition guards against. Several nearby communities that built back at sea level lost hundreds of residents in 2011; their livelihoods tie them to the shore regardless.
narrative_ontology:constraint_stakeholder(aneyoshi_land_use_prohibition__behavioral_competence_reading, nearshore_rebuild_advocates, excluded,
    organized, immediate, trapped, regional).

% Researchers and survey teams who document the stones, interview survivors and keepers, compare survival outcomes across municipalities, and publish the record internationally. They take no part in keeping or enforcing the practice and bear none of its costs.
narrative_ontology:constraint_stakeholder(aneyoshi_land_use_prohibition__behavioral_competence_reading, disaster_anthropology_observers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(aneyoshi_land_use_prohibition__behavioral_competence_reading, diffuse).
narrative_ontology:fixing_cost_class(aneyoshi_land_use_prohibition__behavioral_competence_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves intergenerational hazard-memory coordination: individual households systematically underweight catastrophes that recur beyond a single lifetime, so the arrangement converts the 1896 and 1933 run-up observations into a permanent spatial datum that keeps every succeeding settlement cohort above the verified inundation limit without requiring each generation to re-derive the risk from its own dead.
% TRANSFER_FUNCTION: Moves almost nothing outward: it relocates siting decisions from shore-proximate plots to the terraces above the carved datum, imposing a small uniform siting cost on every household and returning that cost to the same households as near-total elimination of tsunami mortality exposure. No money, goods, labor, or status flows to any third party.
% ABSENT_VOICES: Water's-edge rebuilders: fisher and shopkeeper households in neighboring port towns who would object that the prohibition trades livelihood proximity for safety margin; they are absent because the hamlet's consensus consolidated before any such faction could form inside it, and they sit outside this hamlet's conversation regionally. Also absent: the 1896 and 1933 dead, whose drowned settlements are the datum's original evidence and who speak only through the carving and the keepers' accounts.
% DISAPPEARANCE_RATIONALE: If the prohibition vanished overnight, siting would drift downslope within a generation or two — exactly the trajectory visible in neighboring municipalities where equivalent warnings stopped being taught and settlements expanded onto the flats. The next large tsunami would find houses on the killing ground, repeating 1896 and 1933. Settlement pattern, livelihood geography, and the transmitter office all depend on the rule's continuing behavioral force.
% FOUNDING_PROBLEM: After the 1896 Meiji Sanriku tsunami annihilated the coastal settlements and the 1933 Showa tsunami repeated the lesson, survivors faced a problem no individual lifetime could solve: the water's true reach was known only to people who would soon die, and their descendants would face the same waves without the memory. The stone was built to carry the observation — and the instruction it implies — past the lifespan of the witnesses.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set: the geological and seismological record independently attests recurring Sanriku tsunamis (869 Jogan, 1611 Keicho, 1896, 1933, 2011) with run-ups exceeding any living memory; post-2011 engineering surveys confirmed the 2011 inundation climbed to approximately the carved datum; and comparative casualty statistics from municipalities that relaxed or never enforced equivalent warnings attest what the founding problem costs when left unsolved. None of these attestations originates inside the hamlet.
narrative_ontology:disappearance_verdict(aneyoshi_land_use_prohibition__behavioral_competence_reading, world_rearranges).
narrative_ontology:founding_problem_status(aneyoshi_land_use_prohibition__behavioral_competence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(aneyoshi_land_use_prohibition__behavioral_competence_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(aneyoshi_land_use_prohibition__behavioral_competence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(aneyoshi_land_use_prohibition__behavioral_competence_reading, 0.11, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(aneyoshi_land_use_prohibition__behavioral_competence_reading_tests).
:- end_tests(aneyoshi_land_use_prohibition__behavioral_competence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored very low (0.11 at interval end): the arrangement's costs are reciprocal — each household forgoes shore-proximate plots that on this steep terrain have little alternative value — and the protection returned is existential, so little net value flows anywhere. Suppression (0.10) is authored as the raw structural property it is, deliberately unscaled by power or scope: compliance runs on upbringing, consensus, and the transmitter office, with no patrols, statutes, or sanction machinery; defiance remains physically available, which caps suppression well below coercive arrangements. Theater ratio (0.06) is low because maintenance activity is the regulatory function itself — tending the stone and retelling the flood accounts is how the rule operates; ceremony accreted mildly across the decades without displacing function. Accessibility collapse (0.55) is moderate: grasping the hazard collapses the attractiveness of low-lying siting almost entirely, but nothing mechanistically forbids building below the line, so alternatives persist in principle. Resistance (0.10) is low: no recorded faction inside the hamlet sought to relax the line at any point in the 78 years. Claim/metric independence: claimed_type rope is asserted from structure — a genuine coordination solution to an intergenerational collective-action problem whose participants are net beneficiaries, with no suppressed alternatives and no victim class — while the metrics are asserted independently from descriptive operation; the engine computes per-seat types from the structural data without reference to the claim. Coordination typing: the stone's dominant function is information-standard-like — a durable measurement record (observed maximum run-up) deployed as a siting protocol of minimal complexity and minimal inherent cost — hence boltzmann.coordination_type information_standard at its default floor; no override is justified. Measurement design: all three tracked series share one time grid (0, 13, 26, 39, 52, 65, 78). suppression_requirement is authored because this story specifically tracks enforcement-capacity dynamics, and its trajectory is a sawtooth, not a ramp: enforcement demand starts low after the 1933 catastrophe (fresh witness memory), climbs as memories die through the high-growth decades (peak 0.21 at t=65, when the last direct witnesses were gone), is briefly relieved by the 1960 Chilean tsunami's revalidation of Sanriku warnings (dip at t=39), and drops sharply at t=78 when the 2011 wave revalidated the entire practice. The oscillation is driven by exogenous hazard events resetting memory decay — it serves the coordination function, not intermittent reinforcement in service of extraction. base_properties values are measured at interval end (t=78), immediately after the 2011 revalidation.
 *
 * PERSPECTIVAL GAP:
 *   The payer and administrator seats overlap in membership — transmitters are themselves hamlet households — yet they compute differently. From the household seat the arrangement is inherited common sense: the felt cost is near zero because shore-proximate plots were never seriously desired on this terrain, and the survival outcome reads as the way things are done. From the transmitter seat the same arrangement is an office constituted by the practice itself — a duty inherited from ancestors who purchased the datum with their lives, whose lapse would feel like betrayal rather than policy revision; the identity lock is institutional and relational at once, fused with the keeper role rather than with any material stake, and the classification would shift toward mobile exit if a successor generation redefined keeping the stone as heritage volunteering rather than binding office. From the municipal seat it is free risk reduction collected without administration, backed by fully mobile exit into revised zoning instruments. The excluded regional constituency experiences the counterfactual directly: communities that relaxed equivalent constraints rebuilt at the water's edge for livelihood reasons and paid catastrophically in 2011. The engine derives these divergent per-seat classifications from the power, exit, and role data; nothing in the authored claim adjudicates among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive the derivation: aneyoshi_hamlet_households and miyako_city_government are declared beneficiaries, placing both seats near the subsidized end of directionality; with no victim declarations there are no amplified targets. Effective pressure on members reduces to the small reciprocal siting cost each bears, which the transfer function returns to them as protection — a circular flow, not a capture. The transmitter office adds maintenance labor rather than rents: keepers spend effort on transmission and receive standing, not material skew, and their dual beneficiary listing reflects residence inside the protected zone rather than receipt of any flow. No directionality_overrides are used: the derivation chain from beneficiary declarations plus exit atoms reproduces the qualitative picture without correction, and adding overrides would assert nothing the structural data does not already yield.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification disciplines two misreadings. Reading the stone as mountain would assert naturality: but the arrangement is constructed and dies without transmission — neighboring towns with equivalent stones that stopped teaching the rule lost its protective effect — so emerges_naturally is false and this is maintained practice, not natural law. Reading it as pure extraction would require a victim class: none exists; costs are reciprocal and no seat captures any flow (the receipt surface is authored as diffuse after checking every seat). No mandatrophy declaration is made because the founding problem is live — Sanriku recurrence intervals exceed institutional lifetimes, and paleotsunami records show events larger than the 1933 calibration — so the arrangement's mandate has not outlived its function. The same analysis guards against a premature husk verdict: theater stays low across the interval precisely because maintenance remained functional, which is the structural difference this reading asserts.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'Which reading of the aneyoshi_land_use_prohibition kernel does the 78-year record support — the inscription as continuously enforced land-use rule (this file) or as commemorative husk whose prohibitive force decayed to symbol?',
    'Micro-historical settlement data: siting deliberations and plot-choice testimony from households across the interval, any recorded attempts to waive or litigate the line, and comparative enforcement traces across neighboring stone-marked hamlets.',
    'Adopting the commemorative_husk_reading would invert this story''s metric profile (high theater_ratio, near-zero behavioral force), re-author epsilon over a different operative arrangement, and flip the computed type toward an inertial profile, severing the live-regulation network edge this file declares.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Indexical uncertainty over which reading of the kernel the evidence supports; the sibling reading is a different constraint with a different epsilon, not a measurement parameter on this one.').

omega_variable(
    counterfactual_compliance_attribution,
    'Was the 78-year compliance causally produced by the prohibition''s operative force, or would identical siting have occurred without the stone due to terrain, poverty, and settlement inertia?',
    'Matched comparison of hamlets with and without run-up stones, controlling for slope, land value, and demography; oral histories of plot-choice reasoning asking whether the carved datum entered the decision at all.',
    'Causal attribution sustains the coordination-function credit behind the rope classification; a null attribution leaves behavior unchanged but relocates the operative mechanism outside the arrangement, drifting the effective classification toward an inertial profile.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(counterfactual_compliance_attribution, empirical, 'Whether compliance history attributes causally to the rule or to confounding geographic and economic factors.').

omega_variable(
    suppression_mechanism_social_vs_internalized,
    'Is the compliance mechanism purely structural-social (consensus, upbringing sanctions, keeper correction) or partly internalized (residents treat the line as natural fact requiring no enforcement)?',
    'Post-exit trajectory: households that relocate outside the hamlet either continue honoring equivalent high-water lines on their own or revert to market-priced siting; depth interviews on whether the rule survives contact with communities that lack it.',
    'An internalized component raises durable suppression above the structural measure while reducing dependence on the transmitter office; a purely structural mechanism makes the whole classification sensitive to transmitter continuity across generations.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_social_vs_internalized, empirical, 'Structural versus internalized share of the compliance mechanism, resolvable by exit-trajectory evidence.').

omega_variable(
    runup_calibration_adequacy,
    'Does the 1933-calibrated prohibition line remain adequate for events rarer than the Showa Sanriku tsunami, given that the 2011 run-up climbed to approximately the stone itself?',
    'Paleotsunami deposit mapping (869 Jogan and 1611 Keicho sand sheets) against current settlement elevations, plus re-survey of the carved datum against reconstructed 2011 inundation.',
    'If the founding calibration undershoots rare events, the protective margin narrows, the true cost of compliant siting rises (building higher costs more), and pressure on epsilon grows; the coordination function would need upward recalibration to preserve its guarantee.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(runup_calibration_adequacy, empirical, 'Whether the stone''s founding datum still bounds the relevant hazard distribution or undershoots tail events already attested geologically.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(aneyoshi_land_use_prohibition__behavioral_competence_reading, 0, 78).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(aneyoshi_bcr_tr_t0, aneyoshi_land_use_prohibition__behavioral_competence_reading, theater_ratio, 0, 0.03).
narrative_ontology:measurement(aneyoshi_bcr_tr_t13, aneyoshi_land_use_prohibition__behavioral_competence_reading, theater_ratio, 13, 0.04).
narrative_ontology:measurement(aneyoshi_bcr_tr_t26, aneyoshi_land_use_prohibition__behavioral_competence_reading, theater_ratio, 26, 0.04).
narrative_ontology:measurement(aneyoshi_bcr_tr_t39, aneyoshi_land_use_prohibition__behavioral_competence_reading, theater_ratio, 39, 0.05).
narrative_ontology:measurement(aneyoshi_bcr_tr_t52, aneyoshi_land_use_prohibition__behavioral_competence_reading, theater_ratio, 52, 0.05).
narrative_ontology:measurement(aneyoshi_bcr_tr_t65, aneyoshi_land_use_prohibition__behavioral_competence_reading, theater_ratio, 65, 0.06).
narrative_ontology:measurement(aneyoshi_bcr_tr_t78, aneyoshi_land_use_prohibition__behavioral_competence_reading, theater_ratio, 78, 0.06).

% Extraction over time
narrative_ontology:measurement(aneyoshi_bcr_be_t0, aneyoshi_land_use_prohibition__behavioral_competence_reading, base_extractiveness, 0, 0.07).
narrative_ontology:measurement(aneyoshi_bcr_be_t13, aneyoshi_land_use_prohibition__behavioral_competence_reading, base_extractiveness, 13, 0.07).
narrative_ontology:measurement(aneyoshi_bcr_be_t26, aneyoshi_land_use_prohibition__behavioral_competence_reading, base_extractiveness, 26, 0.08).
narrative_ontology:measurement(aneyoshi_bcr_be_t39, aneyoshi_land_use_prohibition__behavioral_competence_reading, base_extractiveness, 39, 0.09).
narrative_ontology:measurement(aneyoshi_bcr_be_t52, aneyoshi_land_use_prohibition__behavioral_competence_reading, base_extractiveness, 52, 0.09).
narrative_ontology:measurement(aneyoshi_bcr_be_t65, aneyoshi_land_use_prohibition__behavioral_competence_reading, base_extractiveness, 65, 0.1).
narrative_ontology:measurement(aneyoshi_bcr_be_t78, aneyoshi_land_use_prohibition__behavioral_competence_reading, base_extractiveness, 78, 0.11).

% Suppression requirement over time
narrative_ontology:measurement(aneyoshi_bcr_su_t0, aneyoshi_land_use_prohibition__behavioral_competence_reading, suppression_requirement, 0, 0.08).
narrative_ontology:measurement(aneyoshi_bcr_su_t13, aneyoshi_land_use_prohibition__behavioral_competence_reading, suppression_requirement, 13, 0.11).
narrative_ontology:measurement(aneyoshi_bcr_su_t26, aneyoshi_land_use_prohibition__behavioral_competence_reading, suppression_requirement, 26, 0.14).
narrative_ontology:measurement(aneyoshi_bcr_su_t39, aneyoshi_land_use_prohibition__behavioral_competence_reading, suppression_requirement, 39, 0.13).
narrative_ontology:measurement(aneyoshi_bcr_su_t52, aneyoshi_land_use_prohibition__behavioral_competence_reading, suppression_requirement, 52, 0.17).
narrative_ontology:measurement(aneyoshi_bcr_su_t65, aneyoshi_land_use_prohibition__behavioral_competence_reading, suppression_requirement, 65, 0.21).
narrative_ontology:measurement(aneyoshi_bcr_su_t78, aneyoshi_land_use_prohibition__behavioral_competence_reading, suppression_requirement, 78, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(aneyoshi_land_use_prohibition__behavioral_competence_reading, information_standard).
narrative_ontology:affects_constraint(aneyoshi_land_use_prohibition__behavioral_competence_reading, commemorative_husk_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the colloquial label 'the Aneyoshi stone'. Per epsilon-invariance, the label conflates two structurally distinct claims: an inscription as live land-use regulation (this file, behavioral_competence_reading: very low epsilon, low theater, continuous behavioral force, rope-shaped) and an inscription as memorial whose prohibitions decayed to symbol (commemorative_husk_reading: high theater_ratio, near-zero behavioral force, inertial profile). They are separate stories with separate epsilon, beneficiaries, and metrics, linked via network.affects_constraints. This reading is upstream: its compliance record is the evidentiary basis the sibling reading reinterprets, and the two readings' core premises are contrary over the same span, which is why the reading_relations edge is forecloses rather than coexists_with.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
