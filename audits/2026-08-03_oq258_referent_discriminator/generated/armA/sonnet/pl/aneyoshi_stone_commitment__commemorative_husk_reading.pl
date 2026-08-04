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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   human_readable: Aneyoshi Tsunami Stone as Commemorative Husk (Decayed Directive Reading)
 *   domain: disaster_anthropology/commitment_systems
 *
 * SUMMARY:
 *   In 1933 survivors of a devastating tsunami in Aneyoshi, a small hamlet on
 *   Japan's Sanriku coast, erected a stone marker inscribed with an
 *   instruction: do not build homes below this point. This story instantiates
 *   the commemorative_husk_reading of the contested Aneyoshi stone kernel:
 *   the claim that across the 78 years between the marker's erection and the
 *   2011 Tohoku tsunami, the stone's directive decayed from an operative
 *   land-use constraint into symbolic heritage observance. Under this
 *   reading, households in later generations selected building sites for
 *   reasons independent of the stone's marked line (economic convenience,
 *   land price, family inheritance patterns, road access), the survival of
 *   some structures above the line in 2011 was substantially coincidental or
 *   multiply-caused rather than a demonstration of the stone's continued
 *   behavioral force, and the stone itself became primarily a commemorative
 *   and educational artifact — visited by schoolchildren, photographed by
 *   disaster-tourism visitors, cited in retrospective narratives — without
 *   functioning as a live constraint on where anyone actually built. The
 *   sibling behavioral_competence_reading (a separate constraint story)
 *   claims the opposite: that the directive retained operational force
 *   throughout the interval and that 2011 survival is direct evidence of a
 *   successful 78-year folk-memory institution. This story does not
 *   adjudicate between the readings; it authors the commemorative_husk claim
 *   as a clean, internally consistent constraint with its own epsilon.
 *
 * KEY AGENTS:
 *   - coastal_households_below_the_stone_line: bear the tsunami-exposure risk their own building decisions created, largely independent of the stone
 *   - local_tourism_and_heritage_administrators: benefit from the stone's status as a commemorative site and disaster-tourism draw
 *   - disaster_memory_narrative_industry: benefits from the compelling 'stone that saved lives' story regardless of its accuracy
 *   - future_residents_relying_on_folk_memory: inherit a false sense of security if they believe the stone continues to function as an operative constraint
 *   - surviving_1933_tsunami_generation: originally erected the stone with genuine operative intent, now deceased or aged out of building decisions
 *   - disaster_anthropology_researchers: analytical observers assessing which reading the historical record supports
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
narrative_ontology:human_readable(aneyoshi_stone_commitment__commemorative_husk_reading, "Aneyoshi Tsunami Stone as Commemorative Husk (Decayed Directive Reading)").
narrative_ontology:topic_domain(aneyoshi_stone_commitment__commemorative_husk_reading, "disaster_anthropology/commitment_systems").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(aneyoshi_stone_commitment__commemorative_husk_reading, '86f74228-12d7-47ef-8509-2d4223512695').
narrative_ontology:cs_kernel_codification('86f74228-12d7-47ef-8509-2d4223512695', implicit).
narrative_ontology:cs_authority_grounding('86f74228-12d7-47ef-8509-2d4223512695', diffuse_epistemic).
narrative_ontology:cs_reading_relation('86f74228-12d7-47ef-8509-2d4223512695', aneyoshi_stone_commitment__behavioral_competence_reading, coexists_with).
narrative_ontology:cs_axiom('86f74228-12d7-47ef-8509-2d4223512695', foundational, folk_memory_decays_without_active_reinforcement).
narrative_ontology:cs_axiom_status(folk_memory_decays_without_active_reinforcement, holdable).
narrative_ontology:cs_axiom_grounding('86f74228-12d7-47ef-8509-2d4223512695', folk_memory_decays_without_active_reinforcement, empirically_contingent).
narrative_ontology:cs_axiom('86f74228-12d7-47ef-8509-2d4223512695', secondary, symbolic_persistence_is_not_evidence_of_behavioral_force).
narrative_ontology:cs_axiom_status(symbolic_persistence_is_not_evidence_of_behavioral_force, holdable).
narrative_ontology:cs_axiom_grounding('86f74228-12d7-47ef-8509-2d4223512695', symbolic_persistence_is_not_evidence_of_behavioral_force, empirically_contingent).
narrative_ontology:cs_reference_frame('86f74228-12d7-47ef-8509-2d4223512695', founding_generation_operative_directive).
narrative_ontology:cs_drift_state('86f74228-12d7-47ef-8509-2d4223512695', year_2011_tsunami, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('86f74228-12d7-47ef-8509-2d4223512695', '').
narrative_ontology:cs_kernel_id(aneyoshi_stone_commitment__commemorative_husk_reading, aneyoshi_stone_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(aneyoshi_stone_commitment__commemorative_husk_reading, local_tourism_and_heritage_administrators).
narrative_ontology:constraint_beneficiary(aneyoshi_stone_commitment__commemorative_husk_reading, disaster_memory_narrative_industry).
narrative_ontology:constraint_victim(aneyoshi_stone_commitment__commemorative_husk_reading, coastal_households_below_the_stone_line).
narrative_ontology:constraint_victim(aneyoshi_stone_commitment__commemorative_husk_reading, future_residents_relying_on_folk_memory).
narrative_ontology:constraint_vindicates(aneyoshi_stone_commitment__commemorative_husk_reading, folk_memory_devices_require_active_institutional_maintenance).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Households across multiple generations built and lived below the elevation the stone marks, for reasons this reading attributes to land price, road access, and inherited family plots rather than deliberate rejection of the stone's warning. They bear the tsunami risk their siting choices created; the stone's presence did not constrain where they actually built, and in 2011 many below the line did not survive.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_commitment__commemorative_husk_reading, coastal_households_below_the_stone_line, payer,
    powerless, generational, trapped, local).

% Residents in later decades who may believe a folk-memory device like the stone continues to actively protect the community, without recognizing that under this reading its behavioral function has lapsed. If they treat the stone's continued physical presence as evidence of ongoing protective institution, they inherit a false sense of security this reading identifies as the core danger of mistaking a husk for a live mechanism.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_commitment__commemorative_husk_reading, future_residents_relying_on_folk_memory, payer,
    powerless, generational, trapped, local).

% Administer the stone as a heritage and disaster-tourism site, organizing school visits, plaques, and commemorative programming. They benefit from the compelling narrative of the stone's efficacy regardless of whether the behavioral mechanism it claims credit for was actually operative across the 78-year interval; their institutional and reputational interest favors the more dramatic behavioral_competence account, but under this reading their actual function is custodial and commemorative, not causally protective.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_commitment__commemorative_husk_reading, local_tourism_and_heritage_administrators, beneficiary,
    moderate, biographical, constrained, regional).

% Journalists, documentarians, disaster-preparedness educators, and museum curators who circulate the 'stone that saved lives' story as a compelling and pedagogically useful narrative. They collect attention, funding, and moral authority from the story's circulation; their interest is in the story's persuasive power, not in resolving whether the underlying mechanism was actually operative.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_commitment__commemorative_husk_reading, disaster_memory_narrative_industry, beneficiary,
    organized, generational, mobile, national).

% Erected the stone in 1933 with explicit, genuine operative intent: a direct instruction to future builders not to build below the marked line. This generation set the original agenda in good faith; by the time of the 2011 tsunami they were deceased or long removed from active building decisions, and under this reading had no mechanism to ensure their instruction was transmitted with continued behavioral force to later generations.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_commitment__commemorative_husk_reading, surviving_1933_tsunami_generation, agenda_setter,
    powerless, biographical, trapped, local).

% Study the Aneyoshi case as an instance of folk-memory disaster-preparedness institutions, examining land records, oral histories, and settlement patterns to assess whether the stone's directive retained operative force or decayed into ritual. Under this reading, their role is to document the mismatch between the popular narrative and the actual causal record of building decisions.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_commitment__commemorative_husk_reading, disaster_anthropology_researchers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(aneyoshi_stone_commitment__commemorative_husk_reading, disaster_memory_narrative_industry).
narrative_ontology:fixing_cost_class(aneyoshi_stone_commitment__commemorative_husk_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Under this reading, the stone once solved a real coordination problem in 1933 — transmitting hard-won tsunami-risk knowledge to future builders who had not personally experienced the disaster — but by the time of the 2011 tsunami this reading holds that the coordination function had lapsed: the knowledge-transmission mechanism the stone represented no longer reliably shaped building-site decisions.
% TRANSFER_FUNCTION: Moves reputational, narrative, and institutional value from the historical fact of the 1933 disaster and the stone's origin toward present-day tourism administrators and the disaster-memory narrative industry, who collect the credibility of a 'proven folk-memory success story' without the underlying behavioral mechanism this reading claims actually operating. It also transfers false security to coastal households and future residents who may over-rely on the stone's symbolic presence.
% ABSENT_VOICES: The 1933 generation who erected the stone with genuine operative intent are not present to attest whether their transmission mechanism succeeded or failed across the intervening decades; their voice can only be reconstructed through oral history and land records, and this reading's claim that the mechanism decayed is necessarily an inference made in their absence.
% DISAPPEARANCE_RATIONALE: If the physical stone were removed under this reading, proponents of the commemorative_husk account would predict little to no change in actual building-location decisions, since the reading holds the stone was not operatively constraining those decisions anyway — only the commemorative and tourism function would visibly change. Proponents of the sibling behavioral_competence_reading would predict a real increase in below-line construction. The verdict is genuinely contested between the two readings of the same kernel, which is exactly why they are authored as separate constraint stories.
% FOUNDING_PROBLEM: The 1933 tsunami killed most residents who had built below a certain elevation; survivors sought a durable, generation-spanning mechanism to prevent their descendants from repeating the fatal siting choice without requiring each generation to personally witness a tsunami.
% FOUNDING_PROBLEM_CORROBORATION: Disaster anthropology researchers, examining land registry timing and settlement patterns independent of the tourism and heritage administration's own promotional materials, are the corroborating source for this reading's claim that the transmission mechanism lapsed; the tourism and heritage administrators and the narrative industry — the constraint's principal beneficiaries under this reading — are not treated as corroborating sources, since their institutional interest favors the opposite (behavioral_competence) account.
narrative_ontology:disappearance_verdict(aneyoshi_stone_commitment__commemorative_husk_reading, contested).
narrative_ontology:founding_problem_status(aneyoshi_stone_commitment__commemorative_husk_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(aneyoshi_stone_commitment__commemorative_husk_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-04',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
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
 *   Extractiveness is authored high (0.68 at 2011) under this reading because the stone's continued civic and narrative prominence extracts credibility and moral authority (as a proof-of-concept for folk memory disaster preparedness) without the underlying behavioral mechanism it claims credit for. The theater_ratio is authored very high (0.82) because under this reading almost all of the stone's contemporary function is performative — ceremony, tourism, pedagogical citation — decoupled from any actual land-use gatekeeping. Suppression is authored low (0.15): nothing coercively prevents people from citing or disputing the stone's efficacy; the decay is attributed to ordinary generational attrition of folk knowledge, not active suppression of alternatives. Accessibility_collapse is moderate-low (0.35) since alternative explanations for 2011 survival (terrain, road placement, chance) remain visible and contestable, not fully foreclosed. Resistance is moderate (0.4): disaster anthropologists and skeptical residents do push back against the popular narrative, but the emotionally compelling 'stone saved lives' story has substantial cultural momentum resisting scrutiny.
 *
 * PERSPECTIVAL GAP:
 *   From the position of tourism/heritage administrators and the narrative industry, the stone is a living monument whose story functions well regardless of causal accuracy — for them, the constraint's classification as coordination succeeds under this reading's own metrics. From the position of a disaster anthropologist auditing actual siting decisions, or a resident whose family built below the line for economic reasons unrelated to the stone, the same object computes as an inert artifact wrapped in a coordination myth. The engine should register this seat divergence directly from the differing power/exit structural data, not from any narrative reconciliation.
 *
 * DIRECTIONALITY LOGIC:
 *   Tourism and heritage administrators, and the broader disaster-memory narrative industry, are authored as beneficiaries: they collect cultural capital, funding, and narrative value from the stone's story regardless of whether the underlying behavioral mechanism this reading claims actually held. Coastal households below the line and future residents who might over-rely on folk memory devices (believing 'the stone will warn us' when in fact no one is actively enforcing anything) are authored as victims under this reading — they bear the risk that a decayed, ritual-only constraint creates when mistaken for a live one. This is the core asymmetry the commemorative_husk reading identifies: the symbolic afterlife of a constraint can extract narrative and institutional value while its behavioral protective function is gone.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading is precisely a mandatrophy case: the founding problem (preventing repeat tsunami casualties through sited building avoidance) may be dead as an operative mechanism even though the stone-as-institution persists and is actively maintained, ceremonially and administratively. Classifying this as piton rather than a functioning rope prevents the corpus from crediting a commemorative artifact with a coordination function it no longer performs — the mandatrophy analysis is exactly the question of whether continued maintenance tracks a live function or only its museum-quality husk.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    attribution_of_2011_survival,
    'Was the survival of houses above the stone''s marked line in 2011 caused by residents'' operative deference to the stone''s inscribed warning, or by unrelated factors (later infrastructure, road placement, incidental terrain choice, generic risk-aversion not traceable to the stone)?',
    'Oral history interviews with pre-2011 residents asking directly whether the stone was cited in their own or their parents'' building-location decisions, cross-checked against land registry timing (were plots below the line simply unavailable, unattractive, or actively avoided because of the stone) and against comparable villages without a stone marker to establish a counterfactual base rate.',
    'If attribution to the stone is weak, this commemorative_husk_reading is confirmed as the structurally correct account and the sibling behavioral_competence_reading is the false summit. If attribution is strong, the readings should be understood as describing different eras of the same kernel (operative 1933-1980s, decayed 1980s-2011) rather than as flatly competing claims about the whole interval.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(attribution_of_2011_survival, empirical, 'Whether 2011 survival is causally traceable to the stone''s directive or is coincidental/multiply-caused.').

omega_variable(
    decay_timing_ambiguity,
    'At what point, if any, did the stone transition from an operative land-use constraint to a purely symbolic artifact? Was there a single generation in which builders stopped consulting it, or did the constraint decay gradually and unevenly across households?',
    'Generational survey of building permits, deed records, and family building narratives across the 78-year interval, segmented by decade, to locate any discontinuity in whether the stone''s line was treated as a hard boundary.',
    'A sharp discontinuity would support periodizing the kernel into two successive constraints rather than two competing readings of one; a gradual, uneven decay supports treating commemorative_husk as the terminal state of a single continuously-drifting constraint.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(decay_timing_ambiguity, conceptual, 'Whether decay was a discrete transition or continuous drift, and what this implies for periodization versus reading-contest framing.').

omega_variable(
    husk_still_deters_naturalization,
    'Even if the stone no longer determines specific building-siting decisions, does its continued physical presence and ritual observance (school visits, memorial ceremonies) still perform a diffuse risk-awareness function that this reading''s high-epsilon framing undercounts?',
    'Survey of current coastal-zone risk perception among residents who have visited or been taught about the stone versus those who have not, controlling for other tsunami-education exposure (drills, seawalls, sirens).',
    'If diffuse awareness effects are measurable and nontrivial, the commemorative_husk_reading''s ε may be somewhat overstated as pure extraction/theater — some genuine low-grade coordination function may survive inside the symbolic shell.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(husk_still_deters_naturalization, empirical, 'Whether symbolic observance retains a residual diffuse coordination function despite loss of behavioral force.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(aneyoshi_stone_commitment__commemorative_husk_reading, 1933, 2011).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(aney_tr_t1933, aneyoshi_stone_commitment__commemorative_husk_reading, theater_ratio, 1933, 0.05).
narrative_ontology:measurement_basis(aney_tr_t1933, observed).
narrative_ontology:measurement(aney_tr_t1960, aneyoshi_stone_commitment__commemorative_husk_reading, theater_ratio, 1960, 0.25).
narrative_ontology:measurement_basis(aney_tr_t1960, projected).
narrative_ontology:measurement(aney_tr_t1980, aneyoshi_stone_commitment__commemorative_husk_reading, theater_ratio, 1980, 0.5).
narrative_ontology:measurement_basis(aney_tr_t1980, projected).
narrative_ontology:measurement(aney_tr_t1995, aneyoshi_stone_commitment__commemorative_husk_reading, theater_ratio, 1995, 0.68).
narrative_ontology:measurement_basis(aney_tr_t1995, projected).
narrative_ontology:measurement(aney_tr_t2005, aneyoshi_stone_commitment__commemorative_husk_reading, theater_ratio, 2005, 0.78).
narrative_ontology:measurement_basis(aney_tr_t2005, projected).
narrative_ontology:measurement(aney_tr_t2011, aneyoshi_stone_commitment__commemorative_husk_reading, theater_ratio, 2011, 0.82).
narrative_ontology:measurement_basis(aney_tr_t2011, observed).

% Extraction over time
narrative_ontology:measurement(aney_be_t1933, aneyoshi_stone_commitment__commemorative_husk_reading, base_extractiveness, 1933, 0.1).
narrative_ontology:measurement_basis(aney_be_t1933, observed).
narrative_ontology:measurement(aney_be_t1960, aneyoshi_stone_commitment__commemorative_husk_reading, base_extractiveness, 1960, 0.25).
narrative_ontology:measurement_basis(aney_be_t1960, projected).
narrative_ontology:measurement(aney_be_t1980, aneyoshi_stone_commitment__commemorative_husk_reading, base_extractiveness, 1980, 0.4).
narrative_ontology:measurement_basis(aney_be_t1980, projected).
narrative_ontology:measurement(aney_be_t1995, aneyoshi_stone_commitment__commemorative_husk_reading, base_extractiveness, 1995, 0.52).
narrative_ontology:measurement_basis(aney_be_t1995, projected).
narrative_ontology:measurement(aney_be_t2005, aneyoshi_stone_commitment__commemorative_husk_reading, base_extractiveness, 2005, 0.62).
narrative_ontology:measurement_basis(aney_be_t2005, projected).
narrative_ontology:measurement(aney_be_t2011, aneyoshi_stone_commitment__commemorative_husk_reading, base_extractiveness, 2011, 0.68).
narrative_ontology:measurement_basis(aney_be_t2011, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(aneyoshi_stone_commitment__commemorative_husk_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(aneyoshi_stone_commitment__commemorative_husk_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(aneyoshi_stone_commitment__commemorative_husk_reading, 0.08).
narrative_ontology:affects_constraint(aneyoshi_stone_commitment__commemorative_husk_reading, aneyoshi_stone_commitment__behavioral_competence_reading).

% DUAL FORMULATION NOTE:
% This story and aneyoshi_stone_commitment__behavioral_competence_reading are the two declared readings of the aneyoshi_stone_commitment kernel. They share the same physical artifact and historical interval but diverge on the core empirical question of whether the stone's directive retained behavioral force. Because the readings disagree about the operative mechanism itself (not merely its evaluation), they are authored as separate constraint stories per the epsilon-invariance principle, each with its own extractiveness, beneficiary/victim structure, and claimed_type, linked via this network edge rather than merged into one story with a measurement parameter.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
