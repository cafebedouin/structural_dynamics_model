% ============================================================================
% CONSTRAINT STORY: stone_land_use_rule__commemorative_husk
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_stone_land_use_rule__commemorative_husk, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: stone_land_use_rule__commemorative_husk
 *   human_readable: Tsunami Stone as Commemorative Husk â Decayed Land-Use Rule
 *   domain: disaster_anthropology/institutional_memory/land_use_governance
 *
 * SUMMARY:
 *   In coastal Japan, ancestral tsunami stones inscribed with warnings such
 *   as 'Do not build below this mark' once functioned as operative land-use
 *   prohibitions. In the commemorative_husk reading, the stone has decayed
 *   into a memorial artifact: it is cleaned, repainted, and feted in annual
 *   ceremonies, yet contemporary building permits are issued without
 *   reference to its elevation line. Waterfront development has crept past
 *   the marker, driven by economic pressure and the convenience of coastal
 *   access. The stone now operates as institutional theaterâpresented as
 *   heritage, absent as ruleâwhile extraction accumulates in the form of
 *   unregulated settlement in hazard zones. This reading is one branch of the
 *   stone_land_use_rule kernel; the sibling behavioral_competence reading
 *   treats the same physical stone as a live prohibition with daily spatial
 *   enforcement.
 *
 * KEY AGENTS:
 *   - municipal_heritage_board: agenda_setter (institutional/constrained) â administers the stone as heritage
 *   - coastal_residents: payer (powerless/trapped) â bear disaster risk under false assurance
 *   - waterfront_developers: beneficiary (powerful/mobile) â capture value from unregulated coast
 *   - zoning_authority: excluded (institutional/constrained) â permits without reference to the stone
 *   - disaster_risk_analysts: observer (analytical) â document the gap between hazard maps and practice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(stone_land_use_rule__commemorative_husk, 0.76).
domain_priors:suppression_score(stone_land_use_rule__commemorative_husk, 0.38).
domain_priors:theater_ratio(stone_land_use_rule__commemorative_husk, 0.88).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(stone_land_use_rule__commemorative_husk, extractiveness, 0.76).
narrative_ontology:constraint_metric(stone_land_use_rule__commemorative_husk, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(stone_land_use_rule__commemorative_husk, theater_ratio, 0.88).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(stone_land_use_rule__commemorative_husk, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(stone_land_use_rule__commemorative_husk, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(stone_land_use_rule__commemorative_husk, piton).
narrative_ontology:human_readable(stone_land_use_rule__commemorative_husk, "Tsunami Stone as Commemorative Husk â Decayed Land-Use Rule").
narrative_ontology:topic_domain(stone_land_use_rule__commemorative_husk, "disaster_anthropology/institutional_memory/land_use_governance").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(stone_land_use_rule__commemorative_husk, 'fd0c33ba-6ab6-4705-b472-0270ed787207').
narrative_ontology:cs_kernel_codification('fd0c33ba-6ab6-4705-b472-0270ed787207', fixed_text).
narrative_ontology:cs_authority_grounding('fd0c33ba-6ab6-4705-b472-0270ed787207', lineage).
narrative_ontology:cs_interpretation_layer_present('fd0c33ba-6ab6-4705-b472-0270ed787207').
narrative_ontology:cs_reading_relation('fd0c33ba-6ab6-4705-b472-0270ed787207', stone_land_use_rule__behavioral_competence, coexists_with).
narrative_ontology:cs_axiom('fd0c33ba-6ab6-4705-b472-0270ed787207', foundational, memorial_function_supersedes_regulatory_force).
narrative_ontology:cs_axiom_status(memorial_function_supersedes_regulatory_force, holdable).
narrative_ontology:cs_axiom_grounding('fd0c33ba-6ab6-4705-b472-0270ed787207', memorial_function_supersedes_regulatory_force, conventional).
narrative_ontology:cs_axiom('fd0c33ba-6ab6-4705-b472-0270ed787207', secondary, ancestral_warnings_require_modern_governance_successors).
narrative_ontology:cs_axiom_status(ancestral_warnings_require_modern_governance_successors, holdable).
narrative_ontology:cs_axiom_grounding('fd0c33ba-6ab6-4705-b472-0270ed787207', ancestral_warnings_require_modern_governance_successors, conventional).
narrative_ontology:cs_reference_frame('fd0c33ba-6ab6-4705-b472-0270ed787207', commemorative_memorial_marker).
narrative_ontology:cs_drift_state('fd0c33ba-6ab6-4705-b472-0270ed787207', contemporary_development_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('fd0c33ba-6ab6-4705-b472-0270ed787207', '').
narrative_ontology:cs_kernel_id(stone_land_use_rule__commemorative_husk, stone_land_use_rule).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(stone_land_use_rule__commemorative_husk, waterfront_developers).
narrative_ontology:constraint_victim(stone_land_use_rule__commemorative_husk, coastal_residents).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintains the inscribed stone as a registered tangible cultural property. Funds annual cleaning, inscription repainting, and memorial ceremonies. Issues heritage interpretive materials that describe the disaster history but do not assert current land-use restrictions. Lacks statutory authority to approve or deny building permits.
narrative_ontology:constraint_stakeholder(stone_land_use_rule__commemorative_husk, municipal_heritage_board, agenda_setter,
    institutional, generational, constrained, regional).

% Occupy housing and work in buildings constructed below the stone's elevation mark. They face tsunami exposure without modern structural mitigation. Many inherited or purchased properties unaware that the stone once marked a prohibition line.
narrative_ontology:constraint_stakeholder(stone_land_use_rule__commemorative_husk, coastal_residents, payer,
    powerless, biographical, trapped, local).

% Acquire coastal parcels and construct residential and commercial buildings seaward of the stone's inscribed line. Their permitting proceeds through zoning channels that do not reference the ancestral marker. They sell or lease properties at premiums for waterfront proximity.
narrative_ontology:constraint_stakeholder(stone_land_use_rule__commemorative_husk, waterfront_developers, beneficiary,
    powerful, biographical, mobile, regional).

% Operates under national building codes and local urban plans that contain no reference to the stone. Processes development applications based on setback and elevation rules that differ from the stone's placement. Does not coordinate with heritage administrators.
narrative_ontology:constraint_stakeholder(stone_land_use_rule__commemorative_husk, zoning_authority, excluded,
    institutional, biographical, constrained, regional).

% Conduct probabilistic tsunami hazard assessments and publish maps showing the stone's location within high-risk inundation zones. Their findings appear in academic and policy journals but are not incorporated into the statutory zoning maps that govern permitting.
narrative_ontology:constraint_stakeholder(stone_land_use_rule__commemorative_husk, disaster_risk_analysts, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(stone_land_use_rule__commemorative_husk, waterfront_developers).
narrative_ontology:fixing_cost_class(stone_land_use_rule__commemorative_husk, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves intergenerational memory of disaster and maintains communal identity through physical marking of sacred space. Coordinates collective grief, historical continuity, and heritage tourism.
% TRANSFER_FUNCTION: Moves physical risk from governed settlement patterns to individual residents; moves land-value premiums from protected-status absence to waterfront developers; moves political legitimacy from active hazard regulation to symbolic heritage maintenance.
% ABSENT_VOICES: Future disaster victims are structurally absent from present planning decisions. Disaster risk analysts are physically present in the policy ecosystem but their maps are excluded from the zoning authority's statutory instruments. The ancestral voice inscribed on the stone is present as text but absent from enforceable governance.
% DISAPPEARANCE_RATIONALE: If the stone and its nominal rule vanished overnight, the heritage board would lose a key cultural asset and tourism draw, and the political cover for absent zoning would weaken. Pressure for modern sea-walls, managed retreat, or updated hazard zoning would likely rise. The world rearranges because the husk's disappearance removes a substitute for governance.
% FOUNDING_PROBLEM: Coastal communities needed a durable, legible warning to prevent resettlement in tsunami-inundation zones after catastrophic events, before the era of formal zoning or sea-wall technology.
% FOUNDING_PROBLEM_CORROBORATION: Disaster historians and geomorphologists outside the heritage administration attest that the stones were originally emplaced as active warnings with behavioral force. The heritage board acknowledges the historical warning function but asserts it is now superseded; no independent corroboration exists that the founding problem is currently solved by the stone.
narrative_ontology:disappearance_verdict(stone_land_use_rule__commemorative_husk, world_rearranges).
narrative_ontology:founding_problem_status(stone_land_use_rule__commemorative_husk, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(stone_land_use_rule__commemorative_husk, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(stone_land_use_rule__commemorative_husk, 'none', 1).
narrative_ontology:epsilon_provenance(stone_land_use_rule__commemorative_husk, 0.76, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(stone_land_use_rule__commemorative_husk_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(stone_land_use_rule__commemorative_husk, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(stone_land_use_rule__commemorative_husk_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Theater_ratio is very high (0.88) because the stone's maintenance is almost entirely performative: ceremonies, heritage registration, and educational tourism substitute for behavioral force. Extractiveness is high (0.76) because the symbolic presence of the stone satisfies political demand for 'already-addressed' disaster memory, preempting updated sea-walls, retreat zoning, or structural mitigation; the cost of that substitution is borne by residents in hazard exposure. Suppression is moderate-low (0.38): there is no active coercion to settle near the stone, but the memorial status suppresses political demand for real regulation by providing a visible substitute. Resistance is low (0.15) because challenging a beloved memorial is culturally difficult, even when its practical function has vanished. Accessibility_collapse is moderate (0.42): modern zoning and hazard science are technically available alternatives, but they are politically eclipsed by the satisfaction of heritage maintenance. The measurement grid is shared across all three tracked metrics to prevent temporal misalignment.
 *
 * PERSPECTIVAL GAP:
 *   The heritage board experiences the constraint as stable cultural stewardship with low extraction. Coastal residents experience it as ambient background assurance that masks growing risk. Waterfront developers experience it as an irrelevant historical feature that happens to coincide with weak hazard zoning. The engine computes these divergent seats from the same structural data: the board's institutional power and constrained exit give it low directionality, while residents' powerlessness and trapped exit amplify their effective extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   The municipal_heritage_board is the agenda-setter but not a concentrated capturer of extraction; its benefit is bureaucratic legitimacy and budget allocation, not land-value capture. Waterfront_developers are declared beneficiaries because they capture land-value premiums that the absent rule would have prevented; however, they do not maintain the constraint and would develop identically if the stone were removed, making their benefit a free-riding drift rather than captured rent. Coastal_residents are victims because they occupy the risk zone under the false assurance that the stone's presence implies some official protection. The zoning_authority is excluded from the kernel's interpretive community, permitting the divergence between heritage narrative and land-use practice.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâpreventing settlement in tsunami zonesâhas been dead for decades, superseded by modern governance that never arrived. The arrangement persists not because anyone actively needs it, but because removing a memorial is politically costly and residents are not organized enough to demand real mitigation. The classification as piton captures this inertial persistence; were a concentrated party capturing the extraction, it would compute as snare. The authored metrics and structural data are left independent so the engine can measure that boundary.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    structural_vs_internalized_suppression,
    'Is the public''s settlement in the hazard zone driven by structural permitting absence, or by internalized belief that the stone''s presence implies safety?',
    'Post-disaster surveys of survivor risk-perception: if residents cite the stone as a reason for settling, suppression is partially internalized.',
    'If internalized, effective suppression exceeds the structural measure; the constraint operates as cognitive capture despite zero behavioral force.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(structural_vs_internalized_suppression, empirical, 'Whether risk settlement is structurally or cognitively mediated.').

omega_variable(
    piton_vs_snare_beneficiary,
    'Do waterfront developers meaningfully profit from the stone''s symbolic maintenance, or would they develop identically in its absence?',
    'Comparative analysis of permitting rates in jurisdictions with and without equivalent memorial stones; developer testimony on regulatory expectations.',
    'If developers actively leverage the stone''s presence to deflect zoning pressure, the constraint is a snare, not a piton. If development would proceed identically without the stone, the extraction is genuinely inertial.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(piton_vs_snare_beneficiary, empirical, 'Whether beneficiary concentration converts piton inertia to snare capture.').

omega_variable(
    reading_framing_underdetermination,
    'Does the commemorative_husk reading foreclose the behavioral_competence reading, or do they merely describe different empirical communities?',
    'Ethnographic survey of stone-adjacent communities: is the stone''s rule-like force uniformly absent, or locally variable?',
    'If locally variable, the kernel decomposes into jurisdiction-specific constraints rather than two universal readings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_framing_underdetermination, conceptual, 'Empirical segregation of the two kernel readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(stone_land_use_rule__commemorative_husk, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ston_tr_t0, stone_land_use_rule__commemorative_husk, theater_ratio, 0, 0.4).
narrative_ontology:measurement(ston_tr_t10, stone_land_use_rule__commemorative_husk, theater_ratio, 10, 0.55).
narrative_ontology:measurement(ston_tr_t20, stone_land_use_rule__commemorative_husk, theater_ratio, 20, 0.68).
narrative_ontology:measurement(ston_tr_t30, stone_land_use_rule__commemorative_husk, theater_ratio, 30, 0.78).
narrative_ontology:measurement(ston_tr_t40, stone_land_use_rule__commemorative_husk, theater_ratio, 40, 0.85).
narrative_ontology:measurement(ston_tr_t50, stone_land_use_rule__commemorative_husk, theater_ratio, 50, 0.88).

% Extraction over time
narrative_ontology:measurement(ston_be_t0, stone_land_use_rule__commemorative_husk, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(ston_be_t10, stone_land_use_rule__commemorative_husk, base_extractiveness, 10, 0.42).
narrative_ontology:measurement(ston_be_t20, stone_land_use_rule__commemorative_husk, base_extractiveness, 20, 0.53).
narrative_ontology:measurement(ston_be_t30, stone_land_use_rule__commemorative_husk, base_extractiveness, 30, 0.62).
narrative_ontology:measurement(ston_be_t40, stone_land_use_rule__commemorative_husk, base_extractiveness, 40, 0.7).
narrative_ontology:measurement(ston_be_t50, stone_land_use_rule__commemorative_husk, base_extractiveness, 50, 0.76).

% Suppression requirement over time
narrative_ontology:measurement(ston_su_t0, stone_land_use_rule__commemorative_husk, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(ston_su_t10, stone_land_use_rule__commemorative_husk, suppression_requirement, 10, 0.48).
narrative_ontology:measurement(ston_su_t20, stone_land_use_rule__commemorative_husk, suppression_requirement, 20, 0.42).
narrative_ontology:measurement(ston_su_t30, stone_land_use_rule__commemorative_husk, suppression_requirement, 30, 0.38).
narrative_ontology:measurement(ston_su_t40, stone_land_use_rule__commemorative_husk, suppression_requirement, 40, 0.35).
narrative_ontology:measurement(ston_su_t50, stone_land_use_rule__commemorative_husk, suppression_requirement, 50, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(stone_land_use_rule__commemorative_husk, identity_coordination).

% DUAL FORMULATION NOTE:
% This constraint is the commemorative_husk reading of kernel stone_land_use_rule. Its sibling reading behavioral_competence treats the same physical stone as an operative prohibition with daily spatial enforcement. The Îµ-invariance principle requires separate stories because the structural claims differ: one asserts behavioral force, the other asserts zero land-use constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
