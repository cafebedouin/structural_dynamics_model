% ============================================================================
% CONSTRAINT STORY: aneyoshi_land_use_prohibition__commemorative_husk_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:suppression_profile/2,
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
 *   human_readable: Aneyoshi Tsunami Stone Land-Use Line — Commemorative Husk Reading
 *   domain: disaster anthropology / commitment systems / temporal institutional analysis
 *
 * SUMMARY:
 *   This story instantiates the commemorative_husk reading of the Aneyoshi
 *   tsunami stone kernel: the physical marker persists as a heritage object
 *   and tourist-education artifact, but the land-use prohibition it inscribes
 *   has, on this reading, decayed entirely into symbol. No zoning instrument,
 *   disclosure law, or enforcement mechanism currently ties building
 *   permission below the marked line to the stone's warning. The stone is
 *   real, the memory is real, but the behavioral constraint it once
 *   represented has no operative teeth left. This is a distinct constraint
 *   from the sibling behavioral_competence_reading, which holds that the
 *   prohibition has been operationally enforced across 78 years — that
 *   reading would author a low, stable extractiveness (near-mountain-like
 *   fidelity of practice to warning) and near-zero victim set. Here, by
 *   contrast, the prohibition's decay into pure commemoration is exactly what
 *   allows development interests to treat the marked ground as ordinary
 *   buildable land, producing rising extraction as post-hazard memory fades
 *   and construction below the line increases across decades.
 *
 * KEY AGENTS:
 *   - coastal_developers: build below the line at scale, treating the marker as non-binding heritage rather than active restriction
 *   - local_tourism_authority: monetizes the stone's symbolic and educational value while bearing no responsibility for land-use outcomes
 *   - municipal_land_registry_office: administers permits with no statutory link to the inscribed warning, benefiting from the appearance of heritage stewardship at zero enforcement cost
 *   - below_line_future_residents and renters: inherit undisclosed geophysical risk with no institutional mechanism transmitting the original warning to them
 *   - aneyoshi_descendant_lineage_holders: hold the marker's original generational authority but have no institutional standing to enforce it
 *   - disaster_risk_researchers: analytical observers documenting the gap between hazard reality and institutional memory
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(aneyoshi_land_use_prohibition__commemorative_husk_reading, 0.71).
domain_priors:suppression_score(aneyoshi_land_use_prohibition__commemorative_husk_reading, 0.15).
domain_priors:theater_ratio(aneyoshi_land_use_prohibition__commemorative_husk_reading, 0.82).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(aneyoshi_land_use_prohibition__commemorative_husk_reading, extractiveness, 0.71).
narrative_ontology:constraint_metric(aneyoshi_land_use_prohibition__commemorative_husk_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(aneyoshi_land_use_prohibition__commemorative_husk_reading, theater_ratio, 0.82).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(aneyoshi_land_use_prohibition__commemorative_husk_reading, accessibility_collapse, 0.28).
narrative_ontology:constraint_metric(aneyoshi_land_use_prohibition__commemorative_husk_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(aneyoshi_land_use_prohibition__commemorative_husk_reading, piton).
narrative_ontology:human_readable(aneyoshi_land_use_prohibition__commemorative_husk_reading, "Aneyoshi Tsunami Stone Land-Use Line — Commemorative Husk Reading").
narrative_ontology:topic_domain(aneyoshi_land_use_prohibition__commemorative_husk_reading, "disaster anthropology / commitment systems / temporal institutional analysis").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(aneyoshi_land_use_prohibition__commemorative_husk_reading, '6d75dbaa-851d-43af-bee6-3935826d3709').
narrative_ontology:cs_kernel_codification('6d75dbaa-851d-43af-bee6-3935826d3709', fixed_text).
narrative_ontology:cs_authority_grounding('6d75dbaa-851d-43af-bee6-3935826d3709', practice).
narrative_ontology:cs_reading_relation('6d75dbaa-851d-43af-bee6-3935826d3709', aneyoshi_land_use_prohibition__behavioral_competence_reading, coexists_with).
narrative_ontology:cs_axiom('6d75dbaa-851d-43af-bee6-3935826d3709', foundational, commemoration_discharges_the_founding_obligation).
narrative_ontology:cs_axiom_status(commemoration_discharges_the_founding_obligation, holdable).
narrative_ontology:cs_axiom_grounding('6d75dbaa-851d-43af-bee6-3935826d3709', commemoration_discharges_the_founding_obligation, conventional).
narrative_ontology:cs_axiom('6d75dbaa-851d-43af-bee6-3935826d3709', foundational, unenforced_inscription_carries_no_present_land_use_authority).
narrative_ontology:cs_axiom_status(unenforced_inscription_carries_no_present_land_use_authority, holdable).
narrative_ontology:cs_axiom_grounding('6d75dbaa-851d-43af-bee6-3935826d3709', unenforced_inscription_carries_no_present_land_use_authority, empirically_contingent).
narrative_ontology:cs_reference_frame('6d75dbaa-851d-43af-bee6-3935826d3709', id_1933_survivor_inscribed_generational_warning).
narrative_ontology:cs_drift_state('6d75dbaa-851d-43af-bee6-3935826d3709', contemporary_coastal_development_era, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('6d75dbaa-851d-43af-bee6-3935826d3709', '').
narrative_ontology:cs_kernel_id(aneyoshi_land_use_prohibition__commemorative_husk_reading, aneyoshi_land_use_prohibition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(aneyoshi_land_use_prohibition__commemorative_husk_reading, coastal_developers).
narrative_ontology:constraint_beneficiary(aneyoshi_land_use_prohibition__commemorative_husk_reading, local_tourism_authority).
narrative_ontology:constraint_beneficiary(aneyoshi_land_use_prohibition__commemorative_husk_reading, municipal_land_registry_office).
narrative_ontology:constraint_victim(aneyoshi_land_use_prohibition__commemorative_husk_reading, below_line_future_residents).
narrative_ontology:constraint_victim(aneyoshi_land_use_prohibition__commemorative_husk_reading, below_line_renters).
narrative_ontology:constraint_victim(aneyoshi_land_use_prohibition__commemorative_husk_reading, aneyoshi_descendant_lineage_holders).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Build housing, guesthouses, and commercial structures below the stone's marked line because no zoning ordinance, permit condition, or building code makes the inscription's warning legally binding. They point to the stone's absence from any enforceable land registry restriction as license to develop the cheaper, flatter, more accessible low ground.
narrative_ontology:constraint_stakeholder(aneyoshi_land_use_prohibition__commemorative_husk_reading, coastal_developers, beneficiary,
    organized, biographical, arbitrage, regional).

% Maintains the stone as a heritage site and disaster-memory attraction — signage, guided tours, inclusion in tsunami-education itineraries — while having no statutory authority over, and no active interest in enforcing, the adjacent land's actual use. Collects cultural and reputational capital from the stone's symbolism without bearing responsibility for the line's behavioral force.
narrative_ontology:constraint_stakeholder(aneyoshi_land_use_prohibition__commemorative_husk_reading, local_tourism_authority, beneficiary,
    institutional, generational, mobile, regional).
narrative_ontology:stakeholder_secondary_role(aneyoshi_land_use_prohibition__commemorative_husk_reading, local_tourism_authority, agenda_setter).

% Administers zoning and building permits for the settlement. Could translate the stone's inscribed line into a binding elevation restriction but has not done so; permits are granted below the line on ordinary planning criteria as though the marker did not exist. Bears no direct cost from this omission and faces political and fiscal pressure toward permissive development.
narrative_ontology:constraint_stakeholder(aneyoshi_land_use_prohibition__commemorative_husk_reading, municipal_land_registry_office, agenda_setter,
    institutional, generational, constrained, regional).

% Buy or rent homes on land the stone marks as previously destroyed, without being told by any enforceable instrument that the ground has a known tsunami-inundation history. They inherit the geophysical risk the marker records without inheriting any of the knowledge or legal protection the marker was built to transmit. Their exit is blocked by housing affordability and by the fact the risk is invisible in every document that governs their tenancy.
narrative_ontology:constraint_stakeholder(aneyoshi_land_use_prohibition__commemorative_husk_reading, below_line_future_residents, payer,
    powerless, generational, trapped, local).

% Rent housing stock built below the marked line, typically with even less bargaining power and disclosure than owner-occupants. They are the most exposed and least informed party in the arrangement, with no lease clause, disclosure requirement, or insurance premium differential reflecting the documented risk.
narrative_ontology:constraint_stakeholder(aneyoshi_land_use_prohibition__commemorative_husk_reading, below_line_renters, payer,
    powerless, immediate, trapped, local).

% Descendants of the 1933 tsunami survivors who erected the stone specifically to bind their own descendants' building decisions across generations. They hold the stone's original intergenerational warrant but have no legal standing to compel present-day zoning to honor it; their genealogical authority over the marker's meaning is culturally acknowledged but institutionally inert.
narrative_ontology:constraint_stakeholder(aneyoshi_land_use_prohibition__commemorative_husk_reading, aneyoshi_descendant_lineage_holders, excluded,
    powerless, civilizational, identity_locked, local).

% Study the Aneyoshi stone as a case of eroded disaster memory and document the gap between the marker's historical function and present land-use practice. They can publish findings and advise policy but hold no enforcement power over local zoning decisions.
narrative_ontology:constraint_stakeholder(aneyoshi_land_use_prohibition__commemorative_husk_reading, disaster_risk_researchers, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(aneyoshi_land_use_prohibition__commemorative_husk_reading, diffuse).
narrative_ontology:fixing_cost_class(aneyoshi_land_use_prohibition__commemorative_husk_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None currently operative at the behavioral level: the arrangement no longer coordinates anyone's building decisions. Its residual function is purely commemorative — coordinating collective memory and tourist narrative, not land use.
% TRANSFER_FUNCTION: Moves geophysical risk from the parties who could act on the stone's warning (planners, developers) onto the parties who cannot see it in any binding document (future residents and renters below the line), while moving cultural and reputational capital toward the tourism authority and legitimacy toward the land registry office for 'preserving heritage' without cost.
% ABSENT_VOICES: The lineage holders who erected the stone as a binding generational instruction are structurally absent from zoning decisions — their authorial intent is quoted in tourist brochures but carries no vote on a building permit. Below-line residents and renters are absent from the conversation before they arrive, since no disclosure mechanism brings the stone's warning into their transaction.
% DISAPPEARANCE_RATIONALE: If the stone itself were removed tomorrow, current land-use practice would not change at all: no active permit condition, insurance clause, or zoning line depends on it. The tourism authority would lose an attraction and the historical record would lose a physical artifact, but no building decision currently made below the line is actually gated by the stone's presence — confirming that, under this reading, the prohibition has no behavioral force left to lose.
% FOUNDING_PROBLEM: In 1933 (and reinforced after 1896), survivors of catastrophic tsunami inundation erected the stone to physically and permanently mark the maximum observed flood line, instructing descendants never to build homes below it — a direct, low-tech intergenerational transmission of hazard knowledge meant to survive the death of living witnesses.
% FOUNDING_PROBLEM_CORROBORATION: Disaster risk researchers and national tsunami-hazard agencies, entirely outside the beneficiary set (developers, tourism authority, registry office), corroborate from seismological and inundation-mapping evidence that the geophysical hazard the stone marks remains fully live — the risk has not diminished, only the institutional transmission of the warning has failed. No party benefiting from present development activity offers corroboration that the hazard itself has lessened; their silence on hazard status, versus their active promotion of the stone's symbolic value, is itself part of the evidentiary picture.
narrative_ontology:disappearance_verdict(aneyoshi_land_use_prohibition__commemorative_husk_reading, world_unchanged).
narrative_ontology:founding_problem_status(aneyoshi_land_use_prohibition__commemorative_husk_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(aneyoshi_land_use_prohibition__commemorative_husk_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(aneyoshi_land_use_prohibition__commemorative_husk_reading, 'none', 1).
narrative_ontology:epsilon_provenance(aneyoshi_land_use_prohibition__commemorative_husk_reading, 0.71, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness rises across the interval (0.05 to 0.71) modeling the decay curve: immediately post-1933 the warning was a live social fact enforced by direct survivor testimony and community memory, but as living witnesses died and land-use pressure grew, the gap between marked hazard and actual construction widened. Theater ratio rises even faster (0.10 to 0.82) because commemorative activity — plaques, tours, ceremony — has intensified even as the underlying behavioral function collapsed; this is the piton signature: performative maintenance substituting for functional force. Suppression is low (0.15) because nothing coercive holds this arrangement together — no one is forced to ignore the stone, they simply are not bound by it in any operative instrument. Accessibility collapse is low-moderate (0.28) because alternative arrangements (binding zoning, mandatory disclosure) remain entirely available and cheap to implement; nothing about the current situation forecloses them, which is precisely what makes the persistence of the husk-state a policy failure rather than a structural necessity. Resistance is moderate (0.35): descendant lineage holders and some researchers actively contest the drift, but they lack institutional leverage.
 *
 * DIRECTIONALITY LOGIC:
 *   Coastal developers and the tourism authority sit near the beneficiary end: development interests gain buildable land at market rates unadjusted for known hazard, and the tourism authority gains a heritage asset without any offsetting land-use obligation. The land registry office is a structural agenda-setter that benefits from inaction (zero enforcement cost, credit for heritage preservation) without bearing the downstream liability. Below-line future residents and renters sit at the full-target end: trapped exit options (housing affordability, opaque risk disclosure), maximal directionality toward bearing the constraint's decayed-warning cost. Descendant lineage holders are victims of a different kind — their intergenerational authorial intent is extracted from (used commercially and symbolically) while being denied institutional force.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading is a clean case of mandatrophy: the founding problem (transmit tsunami-line knowledge to prevent future death) is corroborated as still fully live by outside seismological authority, while the arrangement that was built to solve it has been captured by parties who benefit from its non-enforcement. Classifying this as piton rather than snare is deliberate — no single concentrated beneficiary profits enormously; the extraction is diffuse across an entire regional development pattern, and no party is hurt enough in the present to force a fix (the cost of the failure is deferred to the next catastrophic wave, borne by people not yet resident). The land registry office is the agenda_setter who could reintroduce a binding line at negligible administrative cost, but currently bears none of the cost of not doing so — the textbook piton asymmetry.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    prohibition_behavioral_status_ambiguity,
    'Is the Aneyoshi land-use line currently a dead symbol (this reading) or a live, informally-enforced norm still shaping actual construction decisions (the behavioral_competence sibling reading)?',
    'A parcel-by-parcel survey of construction dates and elevations relative to the marked line, cross-referenced against permit records and informal community accounts of whether builders below the line encountered social or informal resistance, would settle which reading better fits the empirical record for any given period.',
    'If the behavioral_competence reading is empirically correct, this commemorative_husk reading describes a different, non-existent constraint — the two are not compatible descriptions of one arrangement but rival claims about which arrangement actually obtains, and only one can be the operative constraint at any given time.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(prohibition_behavioral_status_ambiguity, empirical, 'Whether the prohibition retains informal behavioral force or has fully decayed to symbol, as measured by actual construction patterns.').

omega_variable(
    development_interest_capture_of_husk_status,
    'Did the prohibition decay into a husk through genuine collective forgetting, or was the husk status actively cultivated/exploited by development interests who benefit from the marker having no binding force?',
    'Historical review of zoning board minutes, developer lobbying records, and public statements to determine whether any party actively resisted efforts to codify the stone''s line into binding regulation.',
    'Genuine forgetting supports a piton classification (no concentrated beneficiary drove the decay); active cultivation of ambiguity by developers would shift the classification toward snare, since it would establish a beneficiary who profits specifically from maintaining the constraint''s toothlessness.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(development_interest_capture_of_husk_status, conceptual, 'Whether the husk status is passive institutional drift or an actively maintained ambiguity benefiting development interests.').

omega_variable(
    future_catastrophe_realization_timing,
    'Given that Sanriku-coast tsunami recurrence intervals are on the order of decades to a century, does the absence of a realized catastrophe within the measured interval understate or fairly represent the constraint''s true extractive cost?',
    'Actuarial and seismological hazard-return-period modeling could estimate the expected value of harm to below-line residents, independent of whether an event has occurred within any particular observation window.',
    'If the true extractive cost is dominated by low-probability high-severity outcomes not yet realized, the measured extractiveness trajectory (rising steadily to 0.71) may substantially understate the constraint''s actual expected harm to below-line residents, since realized damage has been near-zero throughout the interval by chance rather than by design.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(future_catastrophe_realization_timing, empirical, 'Whether the absence of a realized tsunami within the interval affects how the extraction measure should be interpreted.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(aneyoshi_land_use_prohibition__commemorative_husk_reading, 0, 90).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(aney_tr_t0, aneyoshi_land_use_prohibition__commemorative_husk_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(aney_tr_t15, aneyoshi_land_use_prohibition__commemorative_husk_reading, theater_ratio, 15, 0.18).
narrative_ontology:measurement(aney_tr_t30, aneyoshi_land_use_prohibition__commemorative_husk_reading, theater_ratio, 30, 0.32).
narrative_ontology:measurement(aney_tr_t45, aneyoshi_land_use_prohibition__commemorative_husk_reading, theater_ratio, 45, 0.5).
narrative_ontology:measurement(aney_tr_t60, aneyoshi_land_use_prohibition__commemorative_husk_reading, theater_ratio, 60, 0.65).
narrative_ontology:measurement(aney_tr_t75, aneyoshi_land_use_prohibition__commemorative_husk_reading, theater_ratio, 75, 0.76).
narrative_ontology:measurement(aney_tr_t90, aneyoshi_land_use_prohibition__commemorative_husk_reading, theater_ratio, 90, 0.82).

% Extraction over time
narrative_ontology:measurement(aney_be_t0, aneyoshi_land_use_prohibition__commemorative_husk_reading, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(aney_be_t15, aneyoshi_land_use_prohibition__commemorative_husk_reading, base_extractiveness, 15, 0.12).
narrative_ontology:measurement(aney_be_t30, aneyoshi_land_use_prohibition__commemorative_husk_reading, base_extractiveness, 30, 0.28).
narrative_ontology:measurement(aney_be_t45, aneyoshi_land_use_prohibition__commemorative_husk_reading, base_extractiveness, 45, 0.44).
narrative_ontology:measurement(aney_be_t60, aneyoshi_land_use_prohibition__commemorative_husk_reading, base_extractiveness, 60, 0.58).
narrative_ontology:measurement(aney_be_t75, aneyoshi_land_use_prohibition__commemorative_husk_reading, base_extractiveness, 75, 0.66).
narrative_ontology:measurement(aney_be_t90, aneyoshi_land_use_prohibition__commemorative_husk_reading, base_extractiveness, 90, 0.71).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(aneyoshi_land_use_prohibition__commemorative_husk_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(aneyoshi_land_use_prohibition__commemorative_husk_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(aneyoshi_land_use_prohibition__commemorative_husk_reading, 0.08).
narrative_ontology:affects_constraint(aneyoshi_land_use_prohibition__commemorative_husk_reading, aneyoshi_land_use_prohibition__behavioral_competence_reading).

% DUAL FORMULATION NOTE:
% This constraint and aneyoshi_land_use_prohibition__behavioral_competence_reading are the two readings of a single contested kernel: the Aneyoshi tsunami stone and its inscribed 1933 warning. They share the same physical artifact and founding history but diverge on whether the prohibition retains behavioral force in the present. This reading authors high extractiveness (0.71) and a substantial victim class (below-line residents and renters); the sibling reading would author low, stable extractiveness and effectively no victim class, since on that reading the line is still operationally honored. The two files are not measurement-parameter variants of one constraint — they are rival structural claims about which arrangement actually exists on the ground, linked here for contamination/network analysis, not for averaging.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
