% ============================================================================
% CONSTRAINT STORY: aneyoshi_stone_commitment__behavioral_competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_aneyoshi_stone_commitment__behavioral_competence_reading, []).

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
 *   constraint_id: aneyoshi_stone_commitment__behavioral_competence_reading
 *   human_readable: Aneyoshi Tsunami Warning Stone as Live Land-Use Rule (Behavioral Competence Reading)
 *   domain: disaster_anthropology/commitment_systems/temporal_institutional_analysis
 *
 * SUMMARY:
 *   On the Sanriku coast of Iwate Prefecture, inscribed stones mark the reach
 *   of past tsunamis and warn against building below them. In the hamlet of
 *   Aneyoshi the directive held: after the 1933 tsunami the community rebuilt
 *   on the high terrace, kept the stones, transmitted the rule through keeper
 *   duties and annual observance, and when the 2011 Tohoku tsunami ran up the
 *   cove, the water stopped at the old line while neighboring settlements
 *   that had built seaward were destroyed. This story instantiates ONE
 *   reading of the aneyoshi_stone_commitment kernel: the stone as a live
 *   land-use rule that retained operational force in building-location
 *   decisions across 78 years, with the 2011 survival causally linked to
 *   compliance. The sibling reading (commemorative_husk_reading) treats the
 *   same stones as a memorial artifact whose behavioral force decayed; it is
 *   a separate constraint story with its own epsilon, its own metrics, and
 *   its own classification, linked through the network edge. Per the
 *   epsilon-invariance principle the two readings are decomposed rather than
 *   averaged: this reading authors very low extraction over the standing
 *   arrangement (the directive as actually obeyed), and the disagreement
 *   between readings is routed to omega variables, not hedged inside the
 *   metrics. KEY AGENTS (by structural relationship): -
 *   aneyoshi_resident_households: primary beneficiary (organized/constrained)
 *   — comply with the siting line, receive the survival margin -
 *   aneyoshi_descendant_households: inheriting beneficiary
 *   (powerless/constrained) — born into elevated siting they never chose -
 *   village_elders_and_stone_keepers: agenda setter
 *   (organized/identity_locked) — maintain stones, transmit the directive,
 *   correct proposed violations - shore_dependent_fishers: cost-bearing
 *   beneficiary (moderate/constrained) — pay the daily carrying-grade cost of
 *   uphill siting - postwar_development_interests: excluded outsider
 *   (moderate/mobile) — wanted seaward lots, built in neighboring coves
 *   instead - municipal_government_of_iwaizumi: institutional observer
 *   (institutional/analytical) — historically hands-off, folded the line into
 *   hazard mapping after 2011 - disaster_researchers: analytical observer
 *   (analytical/analytical) — attribute the survival, contest the readings
 *
 * KEY AGENTS:
 *   - aneyoshi_resident_households: primary beneficiary (organized/constrained) — rebuilt above the line after 1933, kept dwellings on the high terrace, received the 2011 survival margin
 *   - aneyoshi_descendant_households: inheriting beneficiary (powerless/constrained) — raised inside a siting decision made by dead ancestors, bound by instruction before consent was possible
 *   - village_elders_and_stone_keepers: agenda setter (organized/identity_locked) — cleared the stones, led observances, intervened against seaward building proposals; their standing is constituted by the duty
 *   - shore_dependent_fishers: cost-bearing beneficiary (moderate/constrained) — hauled gear, fuel, ice, and catch up the extra grade every working day as the price of the cove's survival record
 *   - postwar_development_interests: excluded outsider (moderate/mobile) — guesthouse promoters and lot buyers with no seaward footprint permitted here; they built along the neighboring strips
 *   - municipal_government_of_iwaizumi: institutional observer (institutional/analytical) — administered the district without enforcing or opposing the line, adopted it into official hazard mapping after 2011
 *   - disaster_researchers: analytical observer (analytical/analytical) — surveyed stones, reconstructed inundation heights, compared mortality across compliant and non-compliant settlements
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(aneyoshi_stone_commitment__behavioral_competence_reading, 0.08).
domain_priors:suppression_score(aneyoshi_stone_commitment__behavioral_competence_reading, 0.12).
domain_priors:theater_ratio(aneyoshi_stone_commitment__behavioral_competence_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(aneyoshi_stone_commitment__behavioral_competence_reading, extractiveness, 0.08).
narrative_ontology:constraint_metric(aneyoshi_stone_commitment__behavioral_competence_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(aneyoshi_stone_commitment__behavioral_competence_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(aneyoshi_stone_commitment__behavioral_competence_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(aneyoshi_stone_commitment__behavioral_competence_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(aneyoshi_stone_commitment__behavioral_competence_reading, rope).
narrative_ontology:human_readable(aneyoshi_stone_commitment__behavioral_competence_reading, "Aneyoshi Tsunami Warning Stone as Live Land-Use Rule (Behavioral Competence Reading)").
narrative_ontology:topic_domain(aneyoshi_stone_commitment__behavioral_competence_reading, "disaster_anthropology/commitment_systems/temporal_institutional_analysis").

domain_priors:requires_active_enforcement(aneyoshi_stone_commitment__behavioral_competence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(aneyoshi_stone_commitment__behavioral_competence_reading, '86c21e1e-912a-42db-b5a6-65bdc60ff104').
narrative_ontology:cs_kernel_codification('86c21e1e-912a-42db-b5a6-65bdc60ff104', fixed_text).
narrative_ontology:cs_authority_grounding('86c21e1e-912a-42db-b5a6-65bdc60ff104', lineage).
narrative_ontology:cs_interpretation_layer_present('86c21e1e-912a-42db-b5a6-65bdc60ff104').
narrative_ontology:cs_reading_relation('86c21e1e-912a-42db-b5a6-65bdc60ff104', aneyoshi_stone_commitment__commemorative_husk_reading, forecloses).
narrative_ontology:cs_axiom('86c21e1e-912a-42db-b5a6-65bdc60ff104', foundational, stone_directive_operationally_binding).
narrative_ontology:cs_axiom_status(stone_directive_operationally_binding, holdable).
narrative_ontology:cs_axiom_grounding('86c21e1e-912a-42db-b5a6-65bdc60ff104', stone_directive_operationally_binding, empirically_contingent).
narrative_ontology:cs_axiom('86c21e1e-912a-42db-b5a6-65bdc60ff104', secondary, intergenerational_transmission_sustains_regulatory_force).
narrative_ontology:cs_axiom_status(intergenerational_transmission_sustains_regulatory_force, holdable).
narrative_ontology:cs_axiom_grounding('86c21e1e-912a-42db-b5a6-65bdc60ff104', intergenerational_transmission_sustains_regulatory_force, empirically_contingent).
narrative_ontology:cs_reference_frame('86c21e1e-912a-42db-b5a6-65bdc60ff104', inherited_binding_land_use_law).
narrative_ontology:cs_drift_state('86c21e1e-912a-42db-b5a6-65bdc60ff104', post_2011_inundation_survey, gap(stable, minor, true)).
narrative_ontology:cs_created_at('86c21e1e-912a-42db-b5a6-65bdc60ff104', '').
narrative_ontology:cs_kernel_id(aneyoshi_stone_commitment__behavioral_competence_reading, aneyoshi_stone_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(aneyoshi_stone_commitment__behavioral_competence_reading, aneyoshi_resident_households).
narrative_ontology:constraint_beneficiary(aneyoshi_stone_commitment__behavioral_competence_reading, aneyoshi_descendant_households).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(aneyoshi_stone_commitment__behavioral_competence_reading, shore_dependent_fishers).
narrative_ontology:constraint_victim(aneyoshi_stone_commitment__behavioral_competence_reading, aneyoshi_resident_households).
narrative_ontology:constraint_victim(aneyoshi_stone_commitment__behavioral_competence_reading, shore_dependent_fishers).
narrative_ontology:constraint_vindicates(aneyoshi_stone_commitment__behavioral_competence_reading, elevated_siting_survival_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Live in houses sited above the inscribed line on the hillside terrace above the cove. After the 1933 tsunami they rebuilt high rather than returning to the shore flats. Daily life runs downhill to the boats and terraced plots and back up. Leaving would mean giving up the cove's fishing grounds and the family graves; staying means keeping the house above the old watermarks and bearing the extra grade that costs.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_commitment__behavioral_competence_reading, aneyoshi_resident_households, beneficiary,
    organized, generational, constrained, local).
narrative_ontology:stakeholder_secondary_role(aneyoshi_stone_commitment__behavioral_competence_reading, aneyoshi_resident_households, payer).

% Children and grandchildren raised in the elevated hamlet. They inherit both the siting decision and its explanation; none of them chose the location. As adults some leave for city work; those who remain build only on the upper ground, and the reason reaches them as family instruction long before it reaches them as geology.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_commitment__behavioral_competence_reading, aneyoshi_descendant_households, beneficiary,
    powerless, generational, constrained, local).

% Keep the stones clear of moss, lead the annual observances, retell the 1896 and 1933 accounts, and step in when a household proposes building below the line. Their standing in the village rests on holding this duty, which their parents and grandparents held before them; laying it down would mean stepping out of the role that constitutes their place. The audience they address shrinks nearly every year as the young leave.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_commitment__behavioral_competence_reading, village_elders_and_stone_keepers, agenda_setter,
    organized, generational, identity_locked, local).

% Work the cove's boats and racks. The line adds slope to every working day — gear, fuel, ice, and catch all travel the extra grade. They treat the siting rule as the price of the cove's survival record and are the first to feel any proposal to relax it, since relaxed siting would put their own workplaces and stores nearest the water.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_commitment__behavioral_competence_reading, shore_dependent_fishers, payer,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(aneyoshi_stone_commitment__behavioral_competence_reading, shore_dependent_fishers, beneficiary).

% Contractors, guesthouse promoters, and seaward-lot buyers who looked at the cove during the growth decades. The village norm left them no seaward footprint here; they built along the national-route strips and in neighboring coves instead, where no inscribed line stood in the way. They were never part of the village deliberations that reaffirmed the elevation.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_commitment__behavioral_competence_reading, postwar_development_interests, excluded,
    moderate, biographical, mobile, regional).

% The town office administering the district that contains the hamlet. Historically it neither enforced nor opposed the line, treating it as a village affair. After 2011 it incorporated the stone elevations into official hazard mapping and relocation guidance, converting a village norm into referenced planning data.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_commitment__behavioral_competence_reading, municipal_government_of_iwaizumi, observer,
    institutional, generational, analytical, regional).

% Field teams in disaster anthropology and coastal engineering who survey the stones, reconstruct inundation heights, and compare fatality records across settlements that complied and did not. They produce the attribution analyses on which every reading of the stones ultimately draws.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_commitment__behavioral_competence_reading, disaster_researchers, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(aneyoshi_stone_commitment__behavioral_competence_reading, diffuse).
narrative_ontology:fixing_cost_class(aneyoshi_stone_commitment__behavioral_competence_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Encodes a maximum safe building elevation, learned from repeated catastrophes, into durable inscribed markers plus a transmission practice, so that each generation sites dwellings above the known inundation reach without needing personal memory of an event that recurs on intervals longer than a lifetime.
% TRANSFER_FUNCTION: Moves no money or goods. It transfers a siting obligation forward across generations and reallocates building locations uphill; the daily cost lands on shore-dependent households as extra carrying grade, while the avoided-loss benefit accrues to every current and future household in the hamlet.
% ABSENT_VOICES: Postwar development interests and prospective seaward builders were never seated in the village deliberations that reaffirmed the line; they would have argued the shorefront lots were usable and built their case on growth economics. Out-migrated youth are a second absence: their departure thinned the enforcement web without their objection ever being registered inside the village. Both absences are recorded as commentary-grade signal, not correction-grade input.
% DISAPPEARANCE_RATIONALE: If the directive and its transmission practice vanished overnight in 1933, postwar rebuilding would have crept seaward as catastrophe memory faded — the trajectory neighboring coves actually followed — and the 2011 runup would likely have found Aneyoshi's houses inside the inundation zone. The arrangement of households on the terrace, the keeper roles, the observance calendar, and the survival outcome itself all depend on the directive's continued force; the world this story describes rearranges without it.
% FOUNDING_PROBLEM: The 1896 Meiji Sanriku tsunami killed most residents of the Sanriku coastal hamlets, and the 1933 Shōwa Sanriku tsunami did so again. Survivors faced a problem no individual memory could solve: the water returns on intervals longer than a human lifetime, so each generation must be warned of a danger it has never seen. The stones were the answer — write the line into the landscape and bind the descendants to it.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set: paleotsunami sediment studies documenting pre-1896 sand sheets on the Sanriku coast, Japan Meteorological Agency and university inundation reconstructions after 2011, municipal hazard mapping, and comparative fatality records from neighboring settlements that built below comparable elevations. None of these sources is an Aneyoshi household, and all attest both the founding problem and its persistence.
narrative_ontology:disappearance_verdict(aneyoshi_stone_commitment__behavioral_competence_reading, world_rearranges).
narrative_ontology:founding_problem_status(aneyoshi_stone_commitment__behavioral_competence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(aneyoshi_stone_commitment__behavioral_competence_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(aneyoshi_stone_commitment__behavioral_competence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(aneyoshi_stone_commitment__behavioral_competence_reading, 0.08, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(aneyoshi_stone_commitment__behavioral_competence_reading_tests).
:- end_tests(aneyoshi_stone_commitment__behavioral_competence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is very low (0.08): the directive imposes a siting cost — extra carrying grade, foregone shorefront lots — and collects no rents for anyone; no seat pockets the compliance of others. Suppression is low (0.12): there is no coercive apparatus, noncompliance remained physically and legally available (neighboring coves exercised it), and what pressure exists is normative and increasingly internalized. Theater ratio is 0.25 at interval end and honestly rising: as new construction thinned and the population aged, a growing share of activity around the stones became ceremonial (moss-clearing rites, memorial observance) relative to active siting enforcement — but the majority of the arrangement's activity remained functional, which is precisely what separates this reading from the sibling's. Accessibility collapse is 0.40: the seaward alternative never closed — it stayed open and was taken elsewhere — though understanding the hazard steadily erodes its attractiveness without eliminating it. Resistance is 0.15: grumbling over carrying costs, development pressure in the growth decades, youth out-migration, but no organized opposition to the line. The claimed type (rope) is authored from the structure I believe true — a genuine collective-action solution with participants as net beneficiaries and minimal coercive overhead — independently of these metric values; the engine computes per-seat classifications from the structural data and any divergence from the claim is the measurement the corpus exists to take. The temporal series run on one shared grid (1933, 1948, 1964, 1979, 1995, 2011) with every tracked metric authored at every point. The suppression_requirement series is included deliberately: the story traces enforcement-capacity change, and the declining trajectory models the migration of enforcement from active keeper intervention toward internalized duty as the transmission web thinned with depopulation — enforcement decay, not enforcement ratchet.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently from identical stones. From the keeper seat the arrangement is a sacred trust whose maintenance is constitutive of standing — exit is unthinkable not because barriers block it but because stepping out of the role dissolves the self that holds it. From the fisher seat the same arrangement is a daily tax paid in slope, tolerable only because the survival record is visible. From the descendant seat it is an inherited safety never chosen — a benefit received before the capacity to refuse existed, which is why the beneficiary sits at the powerless power atom. From the researcher seat it is a natural experiment in intergenerational commitment. The engine derives these divergences from power, exit, and role declarations; the authored claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive the derivation: aneyoshi_resident_households and aneyoshi_descendant_households sit near the beneficiary end (low d, damped or inverted effective extraction). Shore-dependent fishers are declared as payers in the stakeholder surface with a secondary beneficiary role but are deliberately NOT listed in base_properties.victims: they bear the arrangement's cost yet are net beneficiaries of it, and the cost they bear is the inherent price of the coordination (carrying grade), not extraction routed to anyone else. That absence matters structurally — the asymmetric-extraction signature a tangled-rope classification requires (someone coordinated AND someone paying through the same structure for another's gain) is simply not present, which is why no victims array is authored. The keepers administer without collecting: their return is standing, not diverted value. Local spatial scope keeps verification cheap in a face-to-face community of a few dozen households, further damping effective extraction. No directionality overrides are used: the beneficiary/victim-plus-exit derivation already places every seat correctly, and the fisher mid-position is carried by the dual role declaration rather than an override.
 *
 * MANDATROPHY ANALYSIS:
 *   No mandatrophy under this reading. The founding problem — recurring tsunamis on intervals longer than individual memory — remains live on the Sanriku coast, the mandate (keep dwellings above the line) remains aligned with the function, and there is no sunset because the hazard has none. The R5 mismatch consumer reads founding_problem_status=live against disappearance_verdict=world_rearranges: a matched pair, no zombie flag. The mandatrophy story in this kernel belongs to the sibling: under the commemorative_husk_reading the founding problem is equally live but the arrangement's function has died, producing exactly the dead-problem-plus-persistent-arrangement mismatch that flags capture or husk. Authoring the two readings as separate files is what keeps this one clean — folding them together would average a live mandate with a dead one and fabricate a middle classification that describes neither.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_partition,
    'Does the settlement record support this reading (the stone retained operational force in siting decisions across the interval) or the sibling commemorative_husk_reading (observance continued while siting drifted seaward regardless)?',
    'Comparative settlement archaeology and cadastral records: plot post-1933 dwelling foundations in the cove against the inscribed elevation; interview surviving households on whether the stone or its keepers entered actual siting decisions.',
    'If the husk reading is correct, this story''s epsilon and type are wrong: the arrangement recomputes as an inertial remnant with high theater and no operative coordination function, and the 2011 survival requires a different explanation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_partition, empirical, 'Partition test between the two declared readings of the aneyoshi_stone_commitment kernel.').

omega_variable(
    survival_causation_vs_site_selection,
    'Was the 2011 survival of the hamlet caused by compliance with the stone directive, or is the hamlet''s elevation an artifact of site selection for unrelated reasons (harbor geography, arable terrace, road access) that would have held the settlement high even with no stone?',
    'Counterfactual inundation modeling run against reconstructed settlement footprints under a no-directive assumption, cross-checked with oral histories on why each rebuilt house went where it did.',
    'Causal linkage is this reading''s load-bearing empirical claim. If selection explains the elevation, the stone''s regulatory force weakens sharply and the reading slides toward the sibling''s.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(survival_causation_vs_site_selection, empirical, 'Whether compliance, rather than fortunate geography, produced the 2011 outcome.').

omega_variable(
    sparse_event_sampling,
    'How many test events does the 78-year interval actually contain? The 1960 Chile-origin tsunami partially stressed the Sanriku coast, but 2011 may be the only event whose runup meaningfully probed the inscribed line within the interval.',
    'Paleotsunami deposit coring and historical document recovery to extend the event record across the interval and establish which events reached which elevations.',
    'With one severe test, ''retained operational force'' rests on a single observation; additional reaching events would tighten or break the causal attribution and move confidence on the reading''s central axiom.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sparse_event_sampling, empirical, 'Event-sample adequacy behind the 78-year efficacy claim.').

omega_variable(
    enforcement_internalization_split,
    'Is the compliance sustained by structural enforcement (keeper intervention, communal sanction, siting correction) or by internalized duty (households that no longer need external pressure because the rule has become self-concept)?',
    'Post-depopulation compliance trajectory: if compliance holds undiminished as the keeper network thins and corrective interventions approach zero, the residual is internalized; if violations appear as enforcement capacity fades, the structural share dominates.',
    'If internalized, the measured suppression requirement overstates what the arrangement now needs and the decline series understates durability; if structural, the shrinking elder cohort is a live failure risk the flat extractiveness series conceals.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_internalization_split, empirical, 'Structural versus internalized share of the enforcement picture.').

omega_variable(
    kernel_boundary_text_vs_practice,
    'Is the kernel the inscribed text alone, or the text-plus-transmission-practice composite? If the kernel is text only, the sibling husk reading gains ground (stone surfaces persist while the practice around them decays); if the kernel is the composite, this reading''s stability claim covers the thing that actually persisted.',
    'Conceptual adjudication in the corpus: fix the kernel boundary policy for commitment-system stories generally, then re-derive both readings under the fixed boundary.',
    'Under a text-only kernel, this story''s low theater and stable drift profile describe the wrong object and migrate to the practice layer; under the composite kernel, the current classification stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_boundary_text_vs_practice, conceptual, 'Framing under-determination in what counts as the kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(aneyoshi_stone_commitment__behavioral_competence_reading, 1933, 2011).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(aneyoshi_behavioral_tr_t1933, aneyoshi_stone_commitment__behavioral_competence_reading, theater_ratio, 1933, 0.08).
narrative_ontology:measurement_basis(aneyoshi_behavioral_tr_t1933, observed).
narrative_ontology:measurement(aneyoshi_behavioral_tr_t1948, aneyoshi_stone_commitment__behavioral_competence_reading, theater_ratio, 1948, 0.11).
narrative_ontology:measurement_basis(aneyoshi_behavioral_tr_t1948, observed).
narrative_ontology:measurement(aneyoshi_behavioral_tr_t1964, aneyoshi_stone_commitment__behavioral_competence_reading, theater_ratio, 1964, 0.14).
narrative_ontology:measurement_basis(aneyoshi_behavioral_tr_t1964, observed).
narrative_ontology:measurement(aneyoshi_behavioral_tr_t1979, aneyoshi_stone_commitment__behavioral_competence_reading, theater_ratio, 1979, 0.17).
narrative_ontology:measurement_basis(aneyoshi_behavioral_tr_t1979, observed).
narrative_ontology:measurement(aneyoshi_behavioral_tr_t1995, aneyoshi_stone_commitment__behavioral_competence_reading, theater_ratio, 1995, 0.21).
narrative_ontology:measurement_basis(aneyoshi_behavioral_tr_t1995, observed).
narrative_ontology:measurement(aneyoshi_behavioral_tr_t2011, aneyoshi_stone_commitment__behavioral_competence_reading, theater_ratio, 2011, 0.25).
narrative_ontology:measurement_basis(aneyoshi_behavioral_tr_t2011, observed).

% Extraction over time
narrative_ontology:measurement(aneyoshi_behavioral_be_t1933, aneyoshi_stone_commitment__behavioral_competence_reading, base_extractiveness, 1933, 0.06).
narrative_ontology:measurement_basis(aneyoshi_behavioral_be_t1933, observed).
narrative_ontology:measurement(aneyoshi_behavioral_be_t1948, aneyoshi_stone_commitment__behavioral_competence_reading, base_extractiveness, 1948, 0.06).
narrative_ontology:measurement_basis(aneyoshi_behavioral_be_t1948, observed).
narrative_ontology:measurement(aneyoshi_behavioral_be_t1964, aneyoshi_stone_commitment__behavioral_competence_reading, base_extractiveness, 1964, 0.07).
narrative_ontology:measurement_basis(aneyoshi_behavioral_be_t1964, observed).
narrative_ontology:measurement(aneyoshi_behavioral_be_t1979, aneyoshi_stone_commitment__behavioral_competence_reading, base_extractiveness, 1979, 0.07).
narrative_ontology:measurement_basis(aneyoshi_behavioral_be_t1979, observed).
narrative_ontology:measurement(aneyoshi_behavioral_be_t1995, aneyoshi_stone_commitment__behavioral_competence_reading, base_extractiveness, 1995, 0.08).
narrative_ontology:measurement_basis(aneyoshi_behavioral_be_t1995, observed).
narrative_ontology:measurement(aneyoshi_behavioral_be_t2011, aneyoshi_stone_commitment__behavioral_competence_reading, base_extractiveness, 2011, 0.08).
narrative_ontology:measurement_basis(aneyoshi_behavioral_be_t2011, observed).

% Suppression requirement over time
narrative_ontology:measurement(aneyoshi_behavioral_su_t1933, aneyoshi_stone_commitment__behavioral_competence_reading, suppression_requirement, 1933, 0.34).
narrative_ontology:measurement_basis(aneyoshi_behavioral_su_t1933, observed).
narrative_ontology:measurement(aneyoshi_behavioral_su_t1948, aneyoshi_stone_commitment__behavioral_competence_reading, suppression_requirement, 1948, 0.29).
narrative_ontology:measurement_basis(aneyoshi_behavioral_su_t1948, observed).
narrative_ontology:measurement(aneyoshi_behavioral_su_t1964, aneyoshi_stone_commitment__behavioral_competence_reading, suppression_requirement, 1964, 0.25).
narrative_ontology:measurement_basis(aneyoshi_behavioral_su_t1964, observed).
narrative_ontology:measurement(aneyoshi_behavioral_su_t1979, aneyoshi_stone_commitment__behavioral_competence_reading, suppression_requirement, 1979, 0.2).
narrative_ontology:measurement_basis(aneyoshi_behavioral_su_t1979, observed).
narrative_ontology:measurement(aneyoshi_behavioral_su_t1995, aneyoshi_stone_commitment__behavioral_competence_reading, suppression_requirement, 1995, 0.16).
narrative_ontology:measurement_basis(aneyoshi_behavioral_su_t1995, observed).
narrative_ontology:measurement(aneyoshi_behavioral_su_t2011, aneyoshi_stone_commitment__behavioral_competence_reading, suppression_requirement, 2011, 0.12).
narrative_ontology:measurement_basis(aneyoshi_behavioral_su_t2011, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(aneyoshi_stone_commitment__behavioral_competence_reading, information_standard).
narrative_ontology:affects_constraint(aneyoshi_stone_commitment__behavioral_competence_reading, commemorative_husk_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the aneyoshi_stone_commitment kernel. The colloquial label 'the Aneyoshi stone' conflates two structurally distinct claims: (1) the stone as live land-use rule (this story — very low epsilon, low theater, stable drift, rope-shaped structure) and (2) the stone as commemorative husk (sibling story — high theater, inertial persistence, no behavioral constraint). The epsilon values differ widely because the referent differs: this reading assesses the directive-as-obeyed; the sibling assesses the directive-as-displayed. The behavioral reading is the upstream, higher-confidence claim (its 2011 outcome is documented), and the husk reading is its negation on the single operative axis; both cite the same physical stones, which is exactly why they must be separate files linked by network edges rather than one story with a measurement parameter.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
