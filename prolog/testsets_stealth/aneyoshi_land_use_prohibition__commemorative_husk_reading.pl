% ============================================================================
% CONSTRAINT STORY: aneyoshi_land_use_prohibition__commemorative_husk_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_aneyoshi_land_use_prohibition__commemorative_husk_reading, []).

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
 *   constraint_id: aneyoshi_land_use_prohibition__commemorative_husk_reading
 *   human_readable: Aneyoshi Tsunami-Stone Prohibition — Commemorative Husk Reading
 *   domain: disaster anthropology / commitment systems / temporal institutional analysis
 *
 * SUMMARY:
 *   On the Sanriku coast, stones erected after the 1896 and 1933 tsunamis
 *   mark the reached height of the water and instruct descendants to keep
 *   dwellings above the line. This story instantiates the
 *   commemorative_husk_reading of that injunction: the stones stand, tended
 *   and venerated, but the prohibition they carry no longer binds conduct —
 *   enforcement lapsed with the generation that remembered 1933, the
 *   regulatory problem was taken over by post-2011 statutory instruments, and
 *   the stone's regime was formally reclassified as cultural property. The
 *   standing arrangement under contest, assessed by this reading's lights, is
 *   a memorial economy grafted onto a regulatory vacuum: development proceeds
 *   toward and below the line, gains accrue now, and the catastrophe risk is
 *   carried by residents who do not yet exist. Epsilon's referent is that
 *   standing arrangement — never the live-rule arrangement the sibling
 *   reading would endorse. Decomposition note: one kernel, two readings, two
 *   files; the sibling authors low epsilon over the same referent because it
 *   assesses enforcement as continuing, this file authors high epsilon
 *   because it assesses enforcement as lapsed and the assurance as false. The
 *   files are linked through network.affects_constraints. KEY AGENTS (by
 *   structural relationship): - coastal_development_interests: primary
 *   beneficiary (powerful/arbitrage) — converts the prohibition's
 *   non-bindingness into land margin - future_residents_below_line: primary
 *   target (powerless/trapped) — inherit the transferred catastrophe risk -
 *   below_line_landowners: dual-positioned (moderate/constrained) — gain on
 *   developability, exposed in person - miyako_city_planning_authority:
 *   agenda setter (institutional/constrained) — administers the husk, could
 *   restore the rule, bears none of the deferred risk -
 *   heritage_tourism_operators: secondary beneficiary (organized/mobile) —
 *   monetize the memorial frame - survivor_family_associations: excluded
 *   voice (organized/generational) — keep the injunction orally alive,
 *   outside the planning table - disaster_research_community: analytical
 *   observer (institutional/global) — documents the enforcement record and
 *   recurrence
 *
 * KEY AGENTS:
 *   - coastal_development_interests: primary beneficiary (powerful/arbitrage) — converts the prohibition's non-bindingness into land margin
 *   - future_residents_below_line: primary target (powerless/trapped) — inherit the transferred catastrophe risk
 *   - below_line_landowners: dual-positioned (moderate/constrained) — gain on developability, exposed in person
 *   - miyako_city_planning_authority: agenda setter (institutional/constrained) — administers the husk, could restore the rule, bears none of the deferred risk
 *   - heritage_tourism_operators: secondary beneficiary (organized/mobile) — monetize the memorial frame
 *   - survivor_family_associations: excluded voice (organized/generational) — keep the injunction orally alive, outside the planning table
 *   - disaster_research_community: analytical observer (institutional/global) — documents the enforcement record and recurrence
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(aneyoshi_land_use_prohibition__commemorative_husk_reading, 0.76).
domain_priors:suppression_score(aneyoshi_land_use_prohibition__commemorative_husk_reading, 0.28).
domain_priors:theater_ratio(aneyoshi_land_use_prohibition__commemorative_husk_reading, 0.85).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(aneyoshi_land_use_prohibition__commemorative_husk_reading, extractiveness, 0.76).
narrative_ontology:constraint_metric(aneyoshi_land_use_prohibition__commemorative_husk_reading, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(aneyoshi_land_use_prohibition__commemorative_husk_reading, theater_ratio, 0.85).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(aneyoshi_land_use_prohibition__commemorative_husk_reading, accessibility_collapse, 0.32).
narrative_ontology:constraint_metric(aneyoshi_land_use_prohibition__commemorative_husk_reading, resistance, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(aneyoshi_land_use_prohibition__commemorative_husk_reading, piton).
narrative_ontology:human_readable(aneyoshi_land_use_prohibition__commemorative_husk_reading, "Aneyoshi Tsunami-Stone Prohibition — Commemorative Husk Reading").
narrative_ontology:topic_domain(aneyoshi_land_use_prohibition__commemorative_husk_reading, "disaster anthropology / commitment systems / temporal institutional analysis").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(aneyoshi_land_use_prohibition__commemorative_husk_reading, '6accbe5a-ea81-4d5a-ab33-7cfed1239a4c').
narrative_ontology:cs_kernel_codification('6accbe5a-ea81-4d5a-ab33-7cfed1239a4c', fixed_text).
narrative_ontology:cs_authority_grounding('6accbe5a-ea81-4d5a-ab33-7cfed1239a4c', lineage).
narrative_ontology:cs_interpretation_layer_present('6accbe5a-ea81-4d5a-ab33-7cfed1239a4c').
narrative_ontology:cs_reading_relation('6accbe5a-ea81-4d5a-ab33-7cfed1239a4c', aneyoshi_land_use_prohibition__behavioral_competence_reading, influences).
narrative_ontology:cs_axiom('6accbe5a-ea81-4d5a-ab33-7cfed1239a4c', foundational, behavioral_force_requires_living_enforcement).
narrative_ontology:cs_axiom_status(behavioral_force_requires_living_enforcement, holdable).
narrative_ontology:cs_axiom_grounding('6accbe5a-ea81-4d5a-ab33-7cfed1239a4c', behavioral_force_requires_living_enforcement, empirically_contingent).
narrative_ontology:cs_axiom('6accbe5a-ea81-4d5a-ab33-7cfed1239a4c', secondary, commemoration_does_not_bind_conduct).
narrative_ontology:cs_axiom_status(commemoration_does_not_bind_conduct, holdable).
narrative_ontology:cs_axiom_grounding('6accbe5a-ea81-4d5a-ab33-7cfed1239a4c', commemoration_does_not_bind_conduct, conventional).
narrative_ontology:cs_reference_frame('6accbe5a-ea81-4d5a-ab33-7cfed1239a4c', commemorative_monument_baseline).
narrative_ontology:cs_drift_state('6accbe5a-ea81-4d5a-ab33-7cfed1239a4c', post_2011_reconstruction_era, gap(repudiation_pressure, minor, true)).
narrative_ontology:cs_created_at('6accbe5a-ea81-4d5a-ab33-7cfed1239a4c', '').
narrative_ontology:cs_kernel_id(aneyoshi_land_use_prohibition__commemorative_husk_reading, aneyoshi_land_use_prohibition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(aneyoshi_land_use_prohibition__commemorative_husk_reading, coastal_development_interests).
narrative_ontology:constraint_beneficiary(aneyoshi_land_use_prohibition__commemorative_husk_reading, below_line_landowners).
narrative_ontology:constraint_beneficiary(aneyoshi_land_use_prohibition__commemorative_husk_reading, heritage_tourism_operators).
narrative_ontology:constraint_victim(aneyoshi_land_use_prohibition__commemorative_husk_reading, future_residents_below_line).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(aneyoshi_land_use_prohibition__commemorative_husk_reading, below_line_landowners).
narrative_ontology:constraint_vindicates(aneyoshi_land_use_prohibition__commemorative_husk_reading, commemorative_compliance_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Acquire, assemble, and market parcels at and below the historic inundation line along the Sanriku coast. Cite the stones' status as registered cultural property and the post-disaster reconstruction program when setback objections are raised. Realize margins at sale or build-out; the corporate form and the length of the development cycle place the next inundation beyond their books. Capital can move to other coasts if this one turns hostile.
narrative_ontology:constraint_stakeholder(aneyoshi_land_use_prohibition__commemorative_husk_reading, coastal_development_interests, beneficiary,
    powerful, immediate, arbitrage, regional).

% Hold plots whose market value depends on their remaining buildable despite sitting below the marked line. Gain when land is sold, leased, or developed; live with the same water their title profits from. Selling out means parting with ground held since before the war; staying means keeping home and asset in the flood path together.
narrative_ontology:constraint_stakeholder(aneyoshi_land_use_prohibition__commemorative_husk_reading, below_line_landowners, beneficiary,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(aneyoshi_land_use_prohibition__commemorative_husk_reading, below_line_landowners, payer).

% Run guided visits, signage, school programs, and souvenir trade around the stones as 'the village that listened to its ancestors.' Revenue depends on the stones remaining revered and visited. A stone treated as a live regulation would complicate the story they sell; a stone torn out or forgotten would end it.
narrative_ontology:constraint_stakeholder(aneyoshi_land_use_prohibition__commemorative_husk_reading, heritage_tourism_operators, beneficiary,
    organized, biographical, mobile, regional).

% Will occupy houses sited by decisions taken before they existed. They inherit assurance — a tended stone, heritage plaques, the absence of any warning sign — rather than a prohibition. When the sea returns they take the full force of it; leaving afterward is bereavement, not relocation. They have no vote in any of the meetings that site them.
narrative_ontology:constraint_stakeholder(aneyoshi_land_use_prohibition__commemorative_husk_reading, future_residents_below_line, payer,
    powerless, generational, trapped, local).

% Administers the heritage designation, funds the annual rite, and maintains the stones as cultural property. Holds the legal power to re-impose a binding height setback through zoning but spends its political capital on reconstruction grants and tourism promotion instead. Bears the development lobby's costs for any restriction and bears none of the deferred inundation risk directly.
narrative_ontology:constraint_stakeholder(aneyoshi_land_use_prohibition__commemorative_husk_reading, miyako_city_planning_authority, agenda_setter,
    institutional, biographical, constrained, local).

% Descendants of the 1896, 1933, and 2011 households who keep the oral injunction alive — keep your house above the stone — and petition for a binding height ordinance. Present at every ceremony, absent from every planning committee. Their leverage peaked in the reconstruction years and has declined with each budget cycle since.
narrative_ontology:constraint_stakeholder(aneyoshi_land_use_prohibition__commemorative_husk_reading, survivor_family_associations, excluded,
    organized, generational, constrained, local).

% Surveys the stones, archives the enforcement record, models recurrence intervals, and publishes on how disaster memory decays into heritage. Findings circulate in journals and conferences; they hold no seat in municipal permitting. Their work is cited by both sides of the reading contest.
narrative_ontology:constraint_stakeholder(aneyoshi_land_use_prohibition__commemorative_husk_reading, disaster_research_community, observer,
    institutional, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(aneyoshi_land_use_prohibition__commemorative_husk_reading, coastal_development_interests).
narrative_ontology:fixing_cost_class(aneyoshi_land_use_prohibition__commemorative_husk_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates commemorative practice: synchronizes the annual rite, school memory-curricula, and heritage visitation around the stones, and maintains village identity as the community that heeded its ancestors. Coordinates no siting behavior: nothing in the current arrangement constrains where dwellings go.
% TRANSFER_FUNCTION: Moves developable-land value and near-term revenue — sales, taxes, tourism — to present interests (developers, below-line landowners, the municipal base), financed by uncompensated catastrophe risk shifted onto future residents below the line; secondarily moves assurance from the stone's inherited authority to prospective occupants who read veneration as protection.
% ABSENT_VOICES: Future residents below the line have no seat — they are the counterparty to every decision and cannot appear. The 1896 and 1933 dead speak only through a carved text the planning process treats as heritage. Dissenting municipal engineers who proposed retaining a regulatory setback after 2011 were outvoted and are no longer in the room.
% DISAPPEARANCE_RATIONALE: If the husk vanished overnight — stone removed, rites ended, designation lapsed — the heritage economy loses its anchor site, village identity practices lose their object, and below-line parcels lose even the symbolic brake: pricing and siting decisions shift immediately. Nothing protective ceases, because nothing protective operates; what rearranges is the memorial economy and the last soft friction on development.
% FOUNDING_PROBLEM: After the 1896 Meiji Sanriku tsunami killed roughly twenty-two thousand people and the 1933 Showa Sanriku tsunami struck the same coast, villages cut stones marking the reached height of the water with injunctions to keep dwellings above the line; Aneyoshi's tablets instruct descendants not to build below the marked point.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: Japan's post-2011 statutory tsunami-siting provisions and Iwate Prefecture hazard-mapping ordinances attest the regulatory problem was taken up by other instruments; the cultural-property registration record attests the stone's formal reclassification as heritage; the disaster-studies literature and post-2011 field documentation record the lapse of enforcement practice. No beneficiary-side source is relied upon for the dead-status finding.
narrative_ontology:disappearance_verdict(aneyoshi_land_use_prohibition__commemorative_husk_reading, world_rearranges).
narrative_ontology:founding_problem_status(aneyoshi_land_use_prohibition__commemorative_husk_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(aneyoshi_land_use_prohibition__commemorative_husk_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(aneyoshi_land_use_prohibition__commemorative_husk_reading, 'none', 1).
narrative_ontology:epsilon_provenance(aneyoshi_land_use_prohibition__commemorative_husk_reading, 0.76, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(aneyoshi_land_use_prohibition__commemorative_husk_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(aneyoshi_land_use_prohibition__commemorative_husk_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(aneyoshi_land_use_prohibition__commemorative_husk_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Claimed type is piton on decay structure alone: the arrangement is a former coordination rule whose protective function has atrophied until what remains is mostly performance, administered by an authority that could restore the rule but bears none of the deferred risk, while the concentrated gains of non-bindingness accrue to development interests — a captured husk, which is why gain_flow names a seat and fixing is prohibitive for the seat that could act. Metrics are authored independently as descriptive: extractiveness 0.76 reflects intertemporal risk transfer (present gains, deferred catastrophic costs) amplified by the husk's manufacture of false assurance; suppression 0.28 is low and mostly discursive — no machinery compels anyone, the frame simply keeps the re-regulation question feeling like desecration (the structural/internalized split is carried by omega); theater_ratio 0.85 because nearly all current activity — rites, plaques, curricula, signage — is commemorative; accessibility_collapse 0.32 because alternatives (voluntary siting, statutory zoning, seawalls, insurance) remain fully available once the husk is understood; resistance 0.42 reflecting advocates and researchers who contest the husk and lose. Suppression is authored as raw structure, unscaled; the engine scales only extractiveness, by directionality and scope. The measurement series share one eight-point grid (1933-2026); the 2011 spike-and-relapse is a single observable crisis cycle embedded in monotonic decay — each failed revival further entrenches the memorial frame, so the oscillation itself reinforces the husk rather than noise-testing it.
 *
 * PERSPECTIVAL GAP:
 *   Seats compute differently from identical structural facts. From the developer seat the arrangement is an amenity landscape: heritage charm plus regulatory freedom, costs invisible over a project horizon. From the landowner seat it is a double entry: appreciation on the asset, exposure in the household. From the future-resident seat it is a pure trap — every term set by others, the bill arrives as water. From the municipal seat it is cultural administration, a budget line and a calendar. From the research seat the whole arc is visible: enforcement, decay, capture. The engine derives these divergent per-seat classifications from power, horizon, and exit as authored; the divergence is the finding, not a defect.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive d toward zero: coastal_development_interests (arbitrage exit) sits nearest the subsidy end; heritage_tourism_operators close behind; below_line_landowners derive mid-range d from their dual declaration (beneficiary of developability, bearer of exposure). The agenda-setting authority derives a mildly beneficiary d — it collects tax base and ceremonial legitimacy while externalizing the deferred risk. The victim declaration drives future_residents_below_line to the full-target end, amplified by trapped exit and by the local scope at which their exposure goes unverified. Excluded advocates and analytical observers fall outside the beneficiary/victim derivation; their oppositional and analytical positions are recorded on the stakeholder surface rather than forced into d.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — keep dwellings above the inundation line — was superseded as a regulatory matter by post-2011 statutory instruments, and the stone's regime was formally reclassified as cultural property: mandate dead, arrangement persisting. The dead-status x world_rearranges mismatch is the intended zombie flag for this reading; the capture half is filled by gain_flow naming development interests and fixing_cost prohibitive for the authority that could act. The classification prevents two opposite mislabels: reading the stone as a live rope (the ancestor-wisdom story) erases the risk transfer to future residents; reading it as a snare overstates coercion that does not exist — nothing compels, the frame persuades. Piton-with-captured-gains is the precise structure: atrophied function, theatrical maintenance, concentrated beneficiary of deadness, disenfranchised deferred victims.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'This constraint is one reading of the aneyoshi_land_use_prohibition kernel — the commemorative_husk_reading, under which the carved injunction has decayed to symbol without behavioral force. Would instantiating the sibling behavioral_competence_reading (the prohibition operationally enforced across 78 years) change the classification?',
    'Archival and ethnographic resolution: municipal permitting records, village meeting minutes, and oral-history interviews establishing whether below-line construction proposals were actually refused, by whom, and under what sanction, decade by decade from 1933 to the present.',
    'If the sibling reading prevails, epsilon collapses toward coordination-cost levels and the type computes toward rope; if this reading prevails, the high-extraction risk-transfer classification stands. The disagreement is located in a single structural element: whether enforcement practice persisted to the present.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, empirical, 'Reading contest over whether the prohibition retains behavioral force.').

omega_variable(
    recurrence_horizon_weighting,
    'Does the extractiveness measure weight the hazard''s bimodal recurrence correctly — frequent destructive Sanriku-class tsunamis on decadal scales versus rare Jogan-class events on multi-century scales?',
    'Paleotsunami deposit studies and instrumental-record synthesis producing a defensible exceedance curve for the below-line zone.',
    'If decadal-class recurrence dominates, epsilon is understated and the arrangement approaches snare-grade extraction; if only mega-events are counted, epsilon is overstated and the husk reads as negligent neglect rather than extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(recurrence_horizon_weighting, empirical, 'Recurrence-interval weighting behind the deferred-risk valuation.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the residual suppression that keeps the re-regulation question off the agenda structural (heritage-law protections, municipal procedure) or internalized (commemorative framing that makes asking the question feel like desecration)?',
    'Comparative probe across Sanriku villages lacking a revered stone: if the re-regulation question re-emerges where the memorial is absent, the suppression rode the frame; if it stays suppressed everywhere, the mechanism is procedural.',
    'If internalized, the husk''s suppressive force travels with the population even if the stones are removed — effective suppression exceeds the structural measure; if structural, revoking the heritage designation would reopen the regulatory question quickly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression sustaining the husk.').

omega_variable(
    false_assurance_attribution,
    'How much of the future victims'' below-line exposure is produced by the husk''s assurance signal (a venerated stone read as protection) versus ordinary hazard discounting that would occur anyway?',
    'Risk-perception surveys and transaction-data comparison between buyers inside and outside the stone''s assurance radius.',
    'If the assurance signal is load-bearing, the husk actively manufactures victimhood and the extraction attribution strengthens; if not, the husk is passive residue and the piton reading hardens.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(false_assurance_attribution, conceptual, 'Causal share of the husk''s assurance signal in victim exposure.').

omega_variable(
    husk_terminal_or_waypoint,
    'Is the commemorative husk a stable endpoint or a waypoint — toward re-regulation (revival succeeds) or toward complete forgetting (even the memorial dissolves)?',
    'Track ceremonial attendance, heritage funding lines, and below-line permit volume across the next two decades.',
    'If waypoint-to-revival, the piton classification is transient and the story should be re-read as contested rope; if waypoint-to-forgetting, extraction continues to accumulate without even the theatrical brake.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(husk_terminal_or_waypoint, conceptual, 'Trajectory of the husk state: endpoint, revival, or dissolution.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(aneyoshi_land_use_prohibition__commemorative_husk_reading, 1933, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(aneyoshi_husk_tr_t1933, aneyoshi_land_use_prohibition__commemorative_husk_reading, theater_ratio, 1933, 0.05).
narrative_ontology:measurement(aneyoshi_husk_tr_t1950, aneyoshi_land_use_prohibition__commemorative_husk_reading, theater_ratio, 1950, 0.08).
narrative_ontology:measurement(aneyoshi_husk_tr_t1970, aneyoshi_land_use_prohibition__commemorative_husk_reading, theater_ratio, 1970, 0.2).
narrative_ontology:measurement(aneyoshi_husk_tr_t1990, aneyoshi_land_use_prohibition__commemorative_husk_reading, theater_ratio, 1990, 0.4).
narrative_ontology:measurement(aneyoshi_husk_tr_t2005, aneyoshi_land_use_prohibition__commemorative_husk_reading, theater_ratio, 2005, 0.6).
narrative_ontology:measurement(aneyoshi_husk_tr_t2011, aneyoshi_land_use_prohibition__commemorative_husk_reading, theater_ratio, 2011, 0.45).
narrative_ontology:measurement(aneyoshi_husk_tr_t2017, aneyoshi_land_use_prohibition__commemorative_husk_reading, theater_ratio, 2017, 0.72).
narrative_ontology:measurement(aneyoshi_husk_tr_t2026, aneyoshi_land_use_prohibition__commemorative_husk_reading, theater_ratio, 2026, 0.85).

% Extraction over time
narrative_ontology:measurement(aneyoshi_husk_be_t1933, aneyoshi_land_use_prohibition__commemorative_husk_reading, base_extractiveness, 1933, 0.1).
narrative_ontology:measurement(aneyoshi_husk_be_t1950, aneyoshi_land_use_prohibition__commemorative_husk_reading, base_extractiveness, 1950, 0.12).
narrative_ontology:measurement(aneyoshi_husk_be_t1970, aneyoshi_land_use_prohibition__commemorative_husk_reading, base_extractiveness, 1970, 0.25).
narrative_ontology:measurement(aneyoshi_husk_be_t1990, aneyoshi_land_use_prohibition__commemorative_husk_reading, base_extractiveness, 1990, 0.45).
narrative_ontology:measurement(aneyoshi_husk_be_t2005, aneyoshi_land_use_prohibition__commemorative_husk_reading, base_extractiveness, 2005, 0.58).
narrative_ontology:measurement(aneyoshi_husk_be_t2011, aneyoshi_land_use_prohibition__commemorative_husk_reading, base_extractiveness, 2011, 0.5).
narrative_ontology:measurement(aneyoshi_husk_be_t2017, aneyoshi_land_use_prohibition__commemorative_husk_reading, base_extractiveness, 2017, 0.68).
narrative_ontology:measurement(aneyoshi_husk_be_t2026, aneyoshi_land_use_prohibition__commemorative_husk_reading, base_extractiveness, 2026, 0.76).

% Suppression requirement over time
narrative_ontology:measurement(aneyoshi_husk_su_t1933, aneyoshi_land_use_prohibition__commemorative_husk_reading, suppression_requirement, 1933, 0.55).
narrative_ontology:measurement(aneyoshi_husk_su_t1950, aneyoshi_land_use_prohibition__commemorative_husk_reading, suppression_requirement, 1950, 0.48).
narrative_ontology:measurement(aneyoshi_husk_su_t1970, aneyoshi_land_use_prohibition__commemorative_husk_reading, suppression_requirement, 1970, 0.34).
narrative_ontology:measurement(aneyoshi_husk_su_t1990, aneyoshi_land_use_prohibition__commemorative_husk_reading, suppression_requirement, 1990, 0.2).
narrative_ontology:measurement(aneyoshi_husk_su_t2005, aneyoshi_land_use_prohibition__commemorative_husk_reading, suppression_requirement, 2005, 0.12).
narrative_ontology:measurement(aneyoshi_husk_su_t2011, aneyoshi_land_use_prohibition__commemorative_husk_reading, suppression_requirement, 2011, 0.45).
narrative_ontology:measurement(aneyoshi_husk_su_t2017, aneyoshi_land_use_prohibition__commemorative_husk_reading, suppression_requirement, 2017, 0.33).
narrative_ontology:measurement(aneyoshi_husk_su_t2026, aneyoshi_land_use_prohibition__commemorative_husk_reading, suppression_requirement, 2026, 0.28).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(aneyoshi_land_use_prohibition__commemorative_husk_reading, identity_coordination).
narrative_ontology:affects_constraint(aneyoshi_land_use_prohibition__commemorative_husk_reading, aneyoshi_land_use_prohibition__behavioral_competence_reading).
narrative_ontology:affects_constraint(aneyoshi_land_use_prohibition__commemorative_husk_reading, post_2011_tsunami_zoning_code).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'the Aneyoshi tsunami stone rule' decomposes into two structurally distinct claims per the epsilon-invariance principle. The behavioral_competence_reading (sibling file) authors the stone as a live land-use rule with 78 years of operational enforcement — low epsilon, rope-flavored. This file authors the commemorative_husk_reading — the enforcement lapsed, the regime was reclassified as heritage, and the standing arrangement transfers catastrophe risk to future residents — high epsilon, piton with captured gains. Same referent (the standing arrangement around the stone line), different readings' lights, different epsilon. The edge to post_2011_tsunami_zoning_code records the causal dependency that killed the mandate: statutory successor instruments absorbed the regulatory function, which is what left the stone free to become pure memorial.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
