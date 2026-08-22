% ============================================================================
% CONSTRAINT STORY: aneyoshi_stone_commitment__commemorative_husk_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_aneyoshi_stone_commitment__commemorative_husk_reading, []).

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
 *   constraint_id: aneyoshi_stone_commitment__commemorative_husk_reading
 *   human_readable: Aneyoshi Tsunami Stone — Commemorative Husk Reading
 *   domain: disaster_anthropology/commitment_systems
 *
 * SUMMARY:
 *   In 1933 (following a prior tsunami), villagers near Aneyoshi erected a
 *   stone marker inscribed with an instruction not to build homes below its
 *   position. This story instantiates the COMMEMORATIVE HUSK reading of that
 *   stone's commitment: over the 78 years to the 2011 Tohoku tsunami, the
 *   inscription persisted as physical text and (increasingly) as a heritage
 *   curiosity, but ceased to function as an actual constraint on where people
 *   built. Land-use decisions were made independently of the stone's
 *   directive — driven by fishing-livelihood proximity, land price, and the
 *   absence of any institutional mechanism (zoning, permitting, insurance)
 *   that referenced the marker. On this reading, the households who survived
 *   in 2011 by living above the line did so incidentally, not because the
 *   stone was governing anyone's settlement choices; the stone's continued
 *   visibility mainly served commemorative and now touristic functions. This
 *   is one of two readings of the same kernel (aneyoshi_stone_commitment);
 *   the sibling behavioral_competence_reading holds that the stone retained
 *   live operational force across the same 78 years and that the 2011
 *   survival differential is direct evidence of that force. The two readings
 *   share the artifact and the interval but author sharply different epsilon,
 *   beneficiary structure, and disappearance verdicts because they disagree
 *   about what actually drove settlement behavior.
 *
 * KEY AGENTS:
 *   - coastal_households_below_the_marker: primary payer (powerless/trapped) — settled without reference to the stone's warning, bore the resulting exposure
 *   - local_tourism_and_heritage_officials: primary beneficiary/agenda_setter (institutional/arbitrage) — administers the stone's symbolic afterlife, could convert it to a binding rule but bears the cost of doing so
 *   - post_disaster_narrative_curators: secondary beneficiary (organized/mobile) — extracts narrative and academic capital from the artifact's visibility
 *   - municipal_land_use_authority: excluded institutional actor — never incorporated the marker into binding land-use instruments
 *   - disaster_historians: analytical observer — documents the decay pattern
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(aneyoshi_stone_commitment__commemorative_husk_reading, 0.68).
domain_priors:suppression_score(aneyoshi_stone_commitment__commemorative_husk_reading, 0.15).
domain_priors:theater_ratio(aneyoshi_stone_commitment__commemorative_husk_reading, 0.82).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(aneyoshi_stone_commitment__commemorative_husk_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(aneyoshi_stone_commitment__commemorative_husk_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(aneyoshi_stone_commitment__commemorative_husk_reading, theater_ratio, 0.82).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(aneyoshi_stone_commitment__commemorative_husk_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(aneyoshi_stone_commitment__commemorative_husk_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(aneyoshi_stone_commitment__commemorative_husk_reading, piton).
narrative_ontology:human_readable(aneyoshi_stone_commitment__commemorative_husk_reading, "Aneyoshi Tsunami Stone — Commemorative Husk Reading").
narrative_ontology:topic_domain(aneyoshi_stone_commitment__commemorative_husk_reading, "disaster_anthropology/commitment_systems").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(aneyoshi_stone_commitment__commemorative_husk_reading, '5e741d25-b71a-4d2e-88aa-bee9f7c05fb7').
narrative_ontology:cs_kernel_codification('5e741d25-b71a-4d2e-88aa-bee9f7c05fb7', fixed_text).
narrative_ontology:cs_authority_grounding('5e741d25-b71a-4d2e-88aa-bee9f7c05fb7', practice).
narrative_ontology:cs_reading_relation('5e741d25-b71a-4d2e-88aa-bee9f7c05fb7', aneyoshi_stone_commitment__behavioral_competence_reading, coexists_with).
narrative_ontology:cs_axiom('5e741d25-b71a-4d2e-88aa-bee9f7c05fb7', foundational, inscribed_directive_without_institutional_uptake_decays_to_symbol).
narrative_ontology:cs_axiom_status(inscribed_directive_without_institutional_uptake_decays_to_symbol, holdable).
narrative_ontology:cs_axiom_grounding('5e741d25-b71a-4d2e-88aa-bee9f7c05fb7', inscribed_directive_without_institutional_uptake_decays_to_symbol, empirically_contingent).
narrative_ontology:cs_axiom('5e741d25-b71a-4d2e-88aa-bee9f7c05fb7', secondary, survival_outcomes_require_independent_causal_evidence_beyond_artifact_presence).
narrative_ontology:cs_axiom_status(survival_outcomes_require_independent_causal_evidence_beyond_artifact_presence, holdable).
narrative_ontology:cs_axiom_grounding('5e741d25-b71a-4d2e-88aa-bee9f7c05fb7', survival_outcomes_require_independent_causal_evidence_beyond_artifact_presence, empirically_contingent).
narrative_ontology:cs_reference_frame('5e741d25-b71a-4d2e-88aa-bee9f7c05fb7', founding_inscription_as_binding_settlement_directive).
narrative_ontology:cs_drift_state('5e741d25-b71a-4d2e-88aa-bee9f7c05fb7', pre_2011_contemporary, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('5e741d25-b71a-4d2e-88aa-bee9f7c05fb7', '').
narrative_ontology:cs_kernel_id(aneyoshi_stone_commitment__commemorative_husk_reading, aneyoshi_stone_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(aneyoshi_stone_commitment__commemorative_husk_reading, local_tourism_and_heritage_officials).
narrative_ontology:constraint_beneficiary(aneyoshi_stone_commitment__commemorative_husk_reading, post_disaster_narrative_curators).
narrative_ontology:constraint_victim(aneyoshi_stone_commitment__commemorative_husk_reading, coastal_households_below_the_marker).
narrative_ontology:constraint_victim(aneyoshi_stone_commitment__commemorative_husk_reading, future_residents_relying_on_folk_memory).
narrative_ontology:constraint_vindicates(aneyoshi_stone_commitment__commemorative_husk_reading, disaster_memory_fades_without_institutional_reinforcement).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Built or bought homes below the stone's marked line over the decades because the stone had become a roadside curiosity rather than an operative rule; land was cheaper, closer to the harbor and fishing livelihoods, and no institution enforced the boundary it names. In the 2011 tsunami, households below the line suffered catastrophic losses while the handful still above it did not — but that survival differential is, on this reading, an accident of who happened to still live uphill, not evidence the stone was governing anyone's choices.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_commitment__commemorative_husk_reading, coastal_households_below_the_marker, payer,
    powerless, generational, trapped, local).

% Maintain the stone as a heritage site and disaster-memory attraction after 2011, funding plaques, tour stops, and commemorative ceremonies. They administer the stone's public meaning and could, if they chose, convert it into an enforced setback line — but the cost of doing so (survey, compensation, rezoning fights with existing landowners) is borne by them, not by anyone currently benefiting from the symbolic status quo, so the conversion never happens.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_commitment__commemorative_husk_reading, local_tourism_and_heritage_officials, beneficiary,
    institutional, biographical, arbitrage, regional).
narrative_ontology:stakeholder_secondary_role(aneyoshi_stone_commitment__commemorative_husk_reading, local_tourism_and_heritage_officials, agenda_setter).

% Researchers, journalists, and disaster-preparedness NGOs who use the stone as a teaching example — sometimes of ancestral wisdom, sometimes (on this reading) of the specific failure mode where a warning becomes ornamental. They collect academic and civic capital from the stone's continued visibility regardless of whether it constrains anyone's actual building decisions.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_commitment__commemorative_husk_reading, post_disaster_narrative_curators, beneficiary,
    organized, generational, mobile, national).

% People who will settle the coastline in coming decades and encounter the stone as an inscribed landmark rather than a zoning instrument. They inherit a monument whose text says 'do not build below this point' but whose institutional apparatus enforces nothing, leaving them to independently rediscover or ignore the warning exactly as prior generations did before 2011.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_commitment__commemorative_husk_reading, future_residents_relying_on_folk_memory, payer,
    powerless, civilizational, trapped, local).

% The body that actually zones and permits construction never treated the stone's inscription as a binding input to its decisions; on this reading it was never in the room where the stone's warning could have been operationalized, and its absence is precisely the mechanism by which the commitment decayed into symbolism.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_commitment__commemorative_husk_reading, municipal_land_use_authority, excluded,
    institutional, biographical, constrained, regional).

% Study the stone post-2011 as a case study in commitment decay: a warning that persisted as text and ritual while losing all behavioral force over the intervening decades, distinguishing genuine institutional transmission from mere physical durability of the artifact.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_commitment__commemorative_husk_reading, disaster_historians, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None operative at present: the stone no longer coordinates land-use decisions among residents, builders, or the municipal authority. Whatever coordination function it once had (if any) has decayed into a shared cultural reference point with no behavioral output.
% TRANSFER_FUNCTION: Moves attention, tourism revenue, and narrative capital toward heritage officials and disaster-memory curators; moves risk exposure toward households who settle below the marker without the stone's warning translating into any zoning or insurance consequence for them.
% ABSENT_VOICES: The municipal land-use authority is structurally excluded from any process that would give the stone's inscription regulatory teeth; households below the marker who died or lost everything in 2011 cannot testify to whether the stone would have changed their decision had it carried institutional force, and that silence is filled by folklore rather than record.
% DISAPPEARANCE_RATIONALE: On this reading, if the stone were removed or destroyed tomorrow, no permit process, insurance rate, or building code would change, because none of those systems currently reference it. Its disappearance would be a cultural and touristic loss, not a regulatory one — which is itself the evidence for the husk reading: a live behavioral constraint would leave an institutional gap when removed, and this reading holds that no such gap exists.
% FOUNDING_PROBLEM: The stone was erected after an earlier tsunami to mark the flood line and instruct future generations, in durable and unambiguous language, not to build homes below that point.
% FOUNDING_PROBLEM_CORROBORATION: Independent post-2011 surveys and journalistic accounts (outside both the heritage-official and tourism-curator beneficiary groups) document that most nearby households were built below the inscribed line in the decades preceding the disaster, indicating the founding directive had ceased to function as an operative constraint on settlement long before it was tested; no municipal record shows the stone's line was ever incorporated into zoning maps or building permits.
narrative_ontology:disappearance_verdict(aneyoshi_stone_commitment__commemorative_husk_reading, world_unchanged).
narrative_ontology:founding_problem_status(aneyoshi_stone_commitment__commemorative_husk_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(aneyoshi_stone_commitment__commemorative_husk_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(aneyoshi_stone_commitment__commemorative_husk_reading, 'none', 1).
narrative_ontology:epsilon_provenance(aneyoshi_stone_commitment__commemorative_husk_reading, 0.68, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(aneyoshi_stone_commitment__commemorative_husk_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(aneyoshi_stone_commitment__commemorative_husk_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(aneyoshi_stone_commitment__commemorative_husk_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (0.68) because, on this reading, the arrangement extracts real costs — settlement risk borne by households, and lives lost in 2011 — while producing no offsetting behavioral coordination; the theater_ratio is authored very high (0.82) because almost all of the stone's ongoing 'function' by 2011 is commemorative and touristic rather than regulatory. Suppression is low (0.15): nothing coercive prevents people from building below the line — the failure is one of institutional absence, not active suppression. Accessibility_collapse is moderate (0.35) and resistance moderate (0.4) because alternatives (actually zoning around the marker) were never foreclosed by force, merely never adopted; this is a piton-shaped absence, not a snare.
 *
 * PERSPECTIVAL GAP:
 *   From the heritage-official seat, the stone's ongoing prominence looks like successful cultural preservation — a monument doing exactly what monuments do. From the payer seat (households who built below the line), the same object is a warning that arrived too late to matter because no institution ever operationalized it. The engine should register this as a piton from the payer/observer seats and something closer to a functioning rope from the beneficiary seat, which is exactly the seat divergence a commitment-decay case is meant to expose.
 *
 * DIRECTIONALITY LOGIC:
 *   Heritage and tourism officials sit near the beneficiary end: they collect visibility, funding, and narrative capital from the stone's persistence with no cost to themselves from its non-enforcement. Narrative curators similarly benefit without bearing settlement risk. Households below the marker and future residents sit near the target end: they bear the uncompensated risk of a warning that exists in stone but nowhere in the zoning apparatus. The municipal authority is neither beneficiary nor victim in the conventional sense — its exclusion from the interpretive loop is the mechanism, not a byproduct, of the husk's persistence.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading resolves a mandatrophy question in a specific direction: the founding problem (preventing settlement below the flood line) is DEAD as an operative constraint well before 2011, yet the artifact and its commemorative apparatus persist and even intensify afterward. Classifying this as piton rather than snare or mountain prevents two errors: treating the stone as a live natural-law-like safeguard (which would falsely credit it for 2011 survival), and treating its current heritage function as predatory extraction (there is no concentrated victim-facing coercion, just diffuse cost from institutional absence). The commemorative_husk reading's core claim is that decay, not enforcement failure or capture, explains the gap between inscription and outcome.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    settlement_causation_ambiguity,
    'Did households actually make land-use decisions independently of the stone''s directive, or did folk memory of the stone (even without institutional enforcement) partially influence some settlement choices in ways not captured by formal records?',
    'Oral history interviews with surviving elderly residents and their descendants about whether the stone was discussed as a reason for or against building location, cross-referenced against land-registry dates and locations.',
    'If oral evidence shows meaningful folk-memory-driven avoidance of the marked zone, the husk reading overstates decay and the behavioral_competence reading''s account of partial informal transmission gains support — the two readings might then describe different degrees of the same decayed-but-not-zero mechanism rather than fully disjoint claims.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(settlement_causation_ambiguity, empirical, 'Whether informal, non-institutional transmission of the stone''s warning partially survived despite the absence of formal enforcement.').

omega_variable(
    kernel_framing_choice,
    'Is the appropriate unit of analysis the stone-as-institutional-rule (which the husk reading finds dead) or the stone-as-cultural-artifact-with-diffuse-influence (which might retain some residual behavioral effect even without formal enforcement)?',
    'This is the same ambiguity that generates the kernel''s two sibling readings; resolving it would require either strong ethnographic evidence of decision-level causation or an operator ruling on which framing the corpus treats as authoritative for this artifact.',
    'Choosing the institutional-rule framing supports classifying this as piton (dead founding problem, theatrical persistence); choosing the diffuse-cultural-influence framing would push toward a rope or scaffold reading closer to the sibling''s account, with much lower extractiveness.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_choice, conceptual, 'Alternative framings of what counts as ''the commitment'' — formal institutional rule vs. diffuse cultural transmission — produce different classifications for the same artifact and interval.').

omega_variable(
    survivor_attribution_bias,
    'Is the 2011 survival of uphill residents genuinely attributable to chance/other factors (as this reading claims), or does survivorship bias in which households happened to remain above the line obscure a real, if weak, deterrent effect of the stone?',
    'Statistical comparison of building density and dates above vs. below the marker across the full 78-year period, controlling for land price and fishing-access proximity, to see if settlement patterns show any measurable avoidance gradient correlated with the stone''s position.',
    'A detectable avoidance gradient would weaken the husk reading''s central claim of zero behavioral effect; its absence would strengthen it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(survivor_attribution_bias, empirical, 'Whether settlement-pattern data supports or undermines the claim that the stone had no measurable deterrent effect on building location.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(aneyoshi_stone_commitment__commemorative_husk_reading, 1933, 2011).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(aney_tr_t1933, aneyoshi_stone_commitment__commemorative_husk_reading, theater_ratio, 1933, 0.2).
narrative_ontology:measurement(aney_tr_t1948, aneyoshi_stone_commitment__commemorative_husk_reading, theater_ratio, 1948, 0.35).
narrative_ontology:measurement(aney_tr_t1963, aneyoshi_stone_commitment__commemorative_husk_reading, theater_ratio, 1963, 0.5).
narrative_ontology:measurement(aney_tr_t1978, aneyoshi_stone_commitment__commemorative_husk_reading, theater_ratio, 1978, 0.62).
narrative_ontology:measurement(aney_tr_t1993, aneyoshi_stone_commitment__commemorative_husk_reading, theater_ratio, 1993, 0.72).
narrative_ontology:measurement(aney_tr_t2005, aneyoshi_stone_commitment__commemorative_husk_reading, theater_ratio, 2005, 0.79).
narrative_ontology:measurement(aney_tr_t2011, aneyoshi_stone_commitment__commemorative_husk_reading, theater_ratio, 2011, 0.82).

% Extraction over time
narrative_ontology:measurement(aney_be_t1933, aneyoshi_stone_commitment__commemorative_husk_reading, base_extractiveness, 1933, 0.1).
narrative_ontology:measurement(aney_be_t1948, aneyoshi_stone_commitment__commemorative_husk_reading, base_extractiveness, 1948, 0.22).
narrative_ontology:measurement(aney_be_t1963, aneyoshi_stone_commitment__commemorative_husk_reading, base_extractiveness, 1963, 0.35).
narrative_ontology:measurement(aney_be_t1978, aneyoshi_stone_commitment__commemorative_husk_reading, base_extractiveness, 1978, 0.48).
narrative_ontology:measurement(aney_be_t1993, aneyoshi_stone_commitment__commemorative_husk_reading, base_extractiveness, 1993, 0.58).
narrative_ontology:measurement(aney_be_t2005, aneyoshi_stone_commitment__commemorative_husk_reading, base_extractiveness, 2005, 0.64).
narrative_ontology:measurement(aney_be_t2011, aneyoshi_stone_commitment__commemorative_husk_reading, base_extractiveness, 2011, 0.68).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(aneyoshi_stone_commitment__commemorative_husk_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(aneyoshi_stone_commitment__commemorative_husk_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(aneyoshi_stone_commitment__commemorative_husk_reading, 0.08).
narrative_ontology:affects_constraint(aneyoshi_stone_commitment__commemorative_husk_reading, aneyoshi_stone_commitment__behavioral_competence_reading).

% DUAL FORMULATION NOTE:
% This story and aneyoshi_stone_commitment__behavioral_competence_reading are the two readings of a single kernel (aneyoshi_stone_commitment): the same physical artifact and 78-year interval, read for opposite conclusions about whether the inscribed directive retained operative force. This reading (commemorative_husk) authors high extractiveness, high theater_ratio, and a dead founding-problem status with a piton claimed_type; the sibling authors low extractiveness, low theater_ratio, and a live founding-problem status with a rope or mountain-adjacent claimed_type. They are linked via affects_constraints rather than merged because averaging their epsilon values would misrepresent both readings as a single, muddled measurement rather than two structurally distinct claims about the same historical object.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
