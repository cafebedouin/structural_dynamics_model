% ============================================================================
% CONSTRAINT STORY: tsunami_stone_commitment__commemorative_husk_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_tsunami_stone_commitment__commemorative_husk_reading, []).

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
 *   constraint_id: tsunami_stone_commitment__commemorative_husk_reading
 *   human_readable: Tsunami Stone as Commemorative Husk (Decayed Warning Marker)
 *   domain: disaster_anthropology/institutional_memory
 *
 * SUMMARY:
 *   Along tsunami-prone coastlines, stone markers inscribed after historical
 *   inundations warn descendants not to build below a given line. This story
 *   authors the reading under which the inscription's behavioral force
 *   decayed well before the 2011 tsunami -- the stone survived as a
 *   commemorative and touristic artifact while the settlement practice it
 *   once encoded eroded, with development permitted below the marked line for
 *   economic reasons. Under this reading, whatever safety compliance appeared
 *   to exist near the stone was coincidental (proximity to high ground for
 *   other reasons, slow redevelopment cycles) rather than the marker's
 *   warning being actively followed. This is a distinct constraint from the
 *   sibling behavioral_competence_reading, which holds the opposite: that
 *   active intergenerational transmission kept the warning behaviorally live.
 *   The two readings share the same physical kernel -- the inscribed stone --
 *   but diverge sharply on epsilon: this reading's arrangement is highly
 *   extractive (0.78) because non-enforcement transfers uncompensated risk
 *   onto future residents, while the sibling reading would author near-zero
 *   extraction (the arrangement was functioning coordination, not
 *   extraction). A third sibling, catastrophe_validation_axis, treats the
 *   2011 event itself as the evidentiary fulcrum between the two readings
 *   rather than authoring a settlement claim -- that is likewise a separate
 *   constraint, not this one.
 *
 * KEY AGENTS:
 *   - coastal_development_actors: primary beneficiary (organized/mobile) -- profits from land use the marker would have foreclosed
 *   - tourism_boards: secondary beneficiary (organized/mobile) -- extracts narrative value while behavioral value lapses
 *   - local_governments_seeking_growth: agenda_setter (institutional/constrained) -- could re-encode the warning as zoning law but does not
 *   - future_coastal_residents: primary target (powerless/trapped) -- inherits uncompensated tsunami exposure
 *   - new_settlement_households: secondary target (powerless/trapped) -- recent arrivals lacking transmitted knowledge
 *   - disaster_anthropologists: analytical observer -- documents the marker/behavior gap independently of the 2011 event
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tsunami_stone_commitment__commemorative_husk_reading, 0.78).
domain_priors:suppression_score(tsunami_stone_commitment__commemorative_husk_reading, 0.42).
domain_priors:theater_ratio(tsunami_stone_commitment__commemorative_husk_reading, 0.81).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tsunami_stone_commitment__commemorative_husk_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(tsunami_stone_commitment__commemorative_husk_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(tsunami_stone_commitment__commemorative_husk_reading, theater_ratio, 0.81).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(tsunami_stone_commitment__commemorative_husk_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(tsunami_stone_commitment__commemorative_husk_reading, resistance, 0.28).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tsunami_stone_commitment__commemorative_husk_reading, piton).
narrative_ontology:human_readable(tsunami_stone_commitment__commemorative_husk_reading, "Tsunami Stone as Commemorative Husk (Decayed Warning Marker)").
narrative_ontology:topic_domain(tsunami_stone_commitment__commemorative_husk_reading, "disaster_anthropology/institutional_memory").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(tsunami_stone_commitment__commemorative_husk_reading, 'ef8661ea-49bf-43a3-8e75-36a88ec56baa').
narrative_ontology:cs_kernel_codification('ef8661ea-49bf-43a3-8e75-36a88ec56baa', fixed_text).
narrative_ontology:cs_authority_grounding('ef8661ea-49bf-43a3-8e75-36a88ec56baa', practice).
narrative_ontology:cs_reading_relation('ef8661ea-49bf-43a3-8e75-36a88ec56baa', tsunami_stone_commitment__behavioral_competence_reading, coexists_with).
narrative_ontology:cs_axiom('ef8661ea-49bf-43a3-8e75-36a88ec56baa', foundational, transmission_decay_precedes_economic_override).
narrative_ontology:cs_axiom_status(transmission_decay_precedes_economic_override, holdable).
narrative_ontology:cs_axiom_grounding('ef8661ea-49bf-43a3-8e75-36a88ec56baa', transmission_decay_precedes_economic_override, empirically_contingent).
narrative_ontology:cs_axiom('ef8661ea-49bf-43a3-8e75-36a88ec56baa', secondary, symbolic_persistence_without_behavioral_content_constitutes_extraction).
narrative_ontology:cs_axiom_status(symbolic_persistence_without_behavioral_content_constitutes_extraction, holdable).
narrative_ontology:cs_axiom_grounding('ef8661ea-49bf-43a3-8e75-36a88ec56baa', symbolic_persistence_without_behavioral_content_constitutes_extraction, conventional).
narrative_ontology:cs_reference_frame('ef8661ea-49bf-43a3-8e75-36a88ec56baa', post_inscription_settlement_avoidance_norm).
narrative_ontology:cs_drift_state('ef8661ea-49bf-43a3-8e75-36a88ec56baa', pre_2011_development_expansion, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('ef8661ea-49bf-43a3-8e75-36a88ec56baa', '').
narrative_ontology:cs_kernel_id(tsunami_stone_commitment__commemorative_husk_reading, tsunami_stone_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(tsunami_stone_commitment__commemorative_husk_reading, coastal_development_actors).
narrative_ontology:constraint_beneficiary(tsunami_stone_commitment__commemorative_husk_reading, tourism_boards).
narrative_ontology:constraint_beneficiary(tsunami_stone_commitment__commemorative_husk_reading, local_governments_seeking_growth).
narrative_ontology:constraint_victim(tsunami_stone_commitment__commemorative_husk_reading, future_coastal_residents).
narrative_ontology:constraint_victim(tsunami_stone_commitment__commemorative_husk_reading, new_settlement_households).
narrative_ontology:constraint_vindicates(tsunami_stone_commitment__commemorative_husk_reading, traditional_knowledge_transmission_narrative).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Developers and land-use planners build housing, hotels, and infrastructure below the stone markers' inundation lines. The stone's continued presence as a picturesque, historic object provides cultural cover -- 'the ancestors knew and we honor them' -- while its actual behavioral content (do not build below this line) goes unenforced. They profit from land that would be unusable if the marker's warning were institutionalized as zoning law.
narrative_ontology:constraint_stakeholder(tsunami_stone_commitment__commemorative_husk_reading, coastal_development_actors, beneficiary,
    organized, biographical, mobile, regional).

% Promote the stones as heritage sites and evidence of ancestral wisdom, drawing visitors to villages that have otherwise abandoned the practical settlement pattern the stones once encoded. The narrative value of the stone is fully extracted while its regulatory value is not.
narrative_ontology:constraint_stakeholder(tsunami_stone_commitment__commemorative_husk_reading, tourism_boards, beneficiary,
    organized, biographical, mobile, regional).

% Control zoning and building permits near the coast. Face tax-revenue and population-growth incentives to permit construction below historical high-water marks. Could re-encode the stone's warning as binding law but do not, because doing so would foreclose valuable low-lying development and because the stone already discharges the community's sense of having 'remembered.'
narrative_ontology:constraint_stakeholder(tsunami_stone_commitment__commemorative_husk_reading, local_governments_seeking_growth, agenda_setter,
    institutional, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(tsunami_stone_commitment__commemorative_husk_reading, local_governments_seeking_growth, beneficiary).

% Households who will buy or rent property below the marker line, often without knowing the stone's original inundation record or believing it is a folkloric curiosity rather than a hazard line. They inherit the exposure the marker was built to prevent, with no voice in the permitting decisions that created it.
narrative_ontology:constraint_stakeholder(tsunami_stone_commitment__commemorative_husk_reading, future_coastal_residents, payer,
    powerless, generational, trapped, local).

% Recent arrivals to coastal towns, often economic migrants or younger residents priced out of higher ground, who settle in newly permitted low-lying developments. They lack the multi-generational residency that would have transmitted the stone's original warning as lived knowledge rather than monument.
narrative_ontology:constraint_stakeholder(tsunami_stone_commitment__commemorative_husk_reading, new_settlement_households, payer,
    powerless, biographical, trapped, local).

% Study the gap between the stone's inscribed content and actual settlement behavior across multiple tsunami-prone regions, documenting cases where markers survived as symbols while the behavioral practice they encoded eroded well before the 2011 disaster.
narrative_ontology:constraint_stakeholder(tsunami_stone_commitment__commemorative_husk_reading, disaster_anthropologists, observer,
    analytical, civilizational, analytical, global).

% The generation who could still recite the stone's warning as a living rule -- 'do not build below this point' -- are mostly deceased or elderly with no institutional channel to formalize their knowledge into enforceable policy before development permits were issued.
narrative_ontology:constraint_stakeholder(tsunami_stone_commitment__commemorative_husk_reading, elderly_original_transmitters, excluded,
    powerless, biographical, trapped, local).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(tsunami_stone_commitment__commemorative_husk_reading, coastal_development_actors).
narrative_ontology:fixing_cost_class(tsunami_stone_commitment__commemorative_husk_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: In its original form, the inscription coordinated settlement location across generations at zero ongoing enforcement cost -- a durable, low-maintenance transmission device for a low-frequency, high-consequence hazard. Under the husk reading, this coordination function has already lapsed; what remains is a memorial function (marking loss, honoring ancestors), not a settlement-coordination function.
% TRANSFER_FUNCTION: Moves exposure to tsunami risk from the present generation of decision-makers (who profit from low-lying development and who experience no cost from the mismatch between marker and settlement) onto future and incoming residents (who bear the flood risk without having consented to or even understood the transfer).
% ABSENT_VOICES: Future coastal residents and new settlement households are not present at the zoning tables where permits are issued; the elderly original transmitters who held the living version of the warning had no institutional mechanism to convert their knowledge into binding law before it faded from active practice.
% DISAPPEARANCE_RATIONALE: If the stone vanished entirely, tourism boards and heritage narratives would lose a cultural artifact, but development patterns and permitting decisions would likely be unchanged -- the behavioral coordination function is, on this reading, already gone. The contest is precisely whether removing the stone would matter: proponents of the sibling behavioral-competence reading would say yes (removing it erodes remaining transmission); this reading holds the transmission is already severed, so removal mostly affects commemoration, not safety.
% FOUNDING_PROBLEM: The stone was erected to encode, in a form that would outlast any single generation's living memory, the empirical lesson of a specific historical tsunami's inundation line -- so that descendants would not resettle land the ancestors had learned, at great cost, was unsafe.
% FOUNDING_PROBLEM_CORROBORATION: Disaster anthropologists studying multiple tsunami-stone communities documented, prior to and independent of the 2011 event, that settlement patterns near many markers had already drifted below the inscribed line decades earlier -- corroboration from researchers outside both the development-beneficiary group and the local government permitting apparatus. No corroborating source from within the benefiting parties (developers, tourism boards, permitting authorities) has been identified; they treat the stone's continued physical presence as if it were still doing protective work.
narrative_ontology:disappearance_verdict(tsunami_stone_commitment__commemorative_husk_reading, contested).
narrative_ontology:founding_problem_status(tsunami_stone_commitment__commemorative_husk_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(tsunami_stone_commitment__commemorative_husk_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(tsunami_stone_commitment__commemorative_husk_reading, 'none', 1).
narrative_ontology:epsilon_provenance(tsunami_stone_commitment__commemorative_husk_reading, 0.78, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(tsunami_stone_commitment__commemorative_husk_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(tsunami_stone_commitment__commemorative_husk_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(tsunami_stone_commitment__commemorative_husk_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (0.78) and rising across the interval because the story is specifically about a warning system's protective function being hollowed out while its symbolic function is actively harvested -- the theater_ratio series rises in lockstep (0.10 to 0.81) on the same shared time grid, modeling the marker's function shifting from protective to performative over roughly a century of coastal development pressure. Suppression is moderate (0.42), not high: nothing coercively prevents residents from heeding the stone if they understood it, the failure is one of transmission decay and permitting incentive, not active suppression of the warning. Accessibility collapse is low-moderate (0.35) because the marker itself, and any documentary record of the original inundation, remains physically accessible -- the collapse is in social transmission and institutional encoding, not in the availability of the information. Resistance is low (0.28): under this reading there is little organized pushback because most parties experience the stone as settled, historical, and non-binding.
 *
 * DIRECTIONALITY LOGIC:
 *   Coastal development actors and tourism boards sit near the full-beneficiary end: they extract cultural/economic value from the stone's continued presence while bearing none of the deferred hazard cost. Local governments are agenda-setters who also benefit (tax base, growth) but carry some institutional exposure if disaster strikes on their watch -- hence agenda_setter with secondary beneficiary rather than pure beneficiary. Future coastal residents and new settlement households sit at the full-target end: trapped by economic necessity into settling exposed land, generationally distant from the original warning, bearing a risk transfer they never negotiated.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem_status is authored dead specifically under this reading -- the coordination function (settlement avoidance) that justified the marker's low-enforcement design has lapsed, yet the marker persists and is actively re-purposed as heritage marketing. This is the mandatrophy signature: an arrangement whose original mandate (protective transmission) is dead is nonetheless treated by beneficiaries as if it still discharges that mandate ('the ancestors' wisdom protects us'), which is precisely the zombie-mandate pattern the founding_problem_status x disappearance_verdict mismatch check is built to catch. The contested disappearance_verdict is deliberate: this reading holds the protective function is already gone, so removal would mostly cost commemoration -- but the story does not resolve whether some residual, weak transmission still occurs, which is exactly the terrain the sibling behavioral_competence_reading disputes.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    compliance_coincidence_vs_transmission,
    'Was the pre-2011 settlement pattern near tsunami stones actually shaped by active, transmitted deference to the inscription''s warning, or did it merely coincide with unrelated factors (terrain cost, land tenure patterns, slow redevelopment cycles) that happened to keep housing above the marked line until economic incentives changed?',
    'Comparative settlement-history research across multiple marker sites: track land-use permit records, oral-history interviews with residents about whether the stone was cited as a reason for avoiding low ground, and land-value gradients over time. Convergent avoidance citing the stone across independent communities would support the behavioral_competence_reading; divergent, economically-explicable patterns would support this reading.',
    'If transmission was genuinely active until recently, this story''s high epsilon and dead founding_problem_status are overstated -- the correct reading would be closer to the sibling''s, with epsilon much lower and founding_problem_status live or only recently contested rather than dead.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(compliance_coincidence_vs_transmission, empirical, 'Whether historical settlement avoidance near the stones reflects active transmitted compliance or coincidental non-development.').

omega_variable(
    development_pressure_as_confound,
    'Is the recent (pre-2011) uptick in below-line construction better explained by the warning''s decay, or by an exogenous rise in coastal land value and tourism demand that would have overridden even a behaviorally live warning?',
    'Compare development timing and intensity against regional land-price indices and tourism-investment cycles; if construction below the line tracks price spikes rather than any documented loss of the warning''s salience, the extraction is better modeled as market pressure overwhelming a still-partially-live norm rather than pure decay.',
    'Would shift some of the authored extractiveness from ''norm decayed to husk'' toward ''norm was overridden by economic force despite partial persistence,'' which changes whether the correct classification is piton (inertial, no concentrated beneficiary) or tangled_rope (a still-partially-functioning coordination captured by developers).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(development_pressure_as_confound, conceptual, 'Whether decay or economic override best explains the observed non-compliance pattern.').

omega_variable(
    cs_framing_alternative_reading,
    'Is the correct kernel framing the physical stone-as-artifact (this story''s framing), or is it more accurately the oral tradition surrounding the stone -- of which the physical inscription is only one transmission channel among several (place names, ritual practice, festival calendars)? If the oral tradition persisted independently of the stone''s physical legibility, the husk reading of the stone alone may misattribute decay to the whole warning system.',
    'Ethnographic survey of whether other transmission channels (place-name warnings, annual commemorations, elder testimony) carried independent behavioral force after the stone itself became illegible or ignored, in communities near the studied markers.',
    'If oral/ritual transmission channels independently carried the warning, this story''s dead founding_problem_status applies only to the stone-as-artifact, not to the warning system as a whole -- and a fourth constraint (oral_tradition_reading) may be warranted rather than treating the stone as the sole kernel-bearing object.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(cs_framing_alternative_reading, conceptual, 'Whether framing the kernel as the physical stone alone, versus the broader oral/ritual transmission complex, changes the classification.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tsunami_stone_commitment__commemorative_husk_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tsun_tr_t0, tsunami_stone_commitment__commemorative_husk_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement_basis(tsun_tr_t0, observed).
narrative_ontology:measurement(tsun_tr_t20, tsunami_stone_commitment__commemorative_husk_reading, theater_ratio, 20, 0.25).
narrative_ontology:measurement_basis(tsun_tr_t20, observed).
narrative_ontology:measurement(tsun_tr_t40, tsunami_stone_commitment__commemorative_husk_reading, theater_ratio, 40, 0.42).
narrative_ontology:measurement_basis(tsun_tr_t40, observed).
narrative_ontology:measurement(tsun_tr_t60, tsunami_stone_commitment__commemorative_husk_reading, theater_ratio, 60, 0.58).
narrative_ontology:measurement_basis(tsun_tr_t60, observed).
narrative_ontology:measurement(tsun_tr_t80, tsunami_stone_commitment__commemorative_husk_reading, theater_ratio, 80, 0.72).
narrative_ontology:measurement_basis(tsun_tr_t80, observed).
narrative_ontology:measurement(tsun_tr_t100, tsunami_stone_commitment__commemorative_husk_reading, theater_ratio, 100, 0.81).
narrative_ontology:measurement_basis(tsun_tr_t100, observed).

% Extraction over time
narrative_ontology:measurement(tsun_be_t0, tsunami_stone_commitment__commemorative_husk_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement_basis(tsun_be_t0, observed).
narrative_ontology:measurement(tsun_be_t20, tsunami_stone_commitment__commemorative_husk_reading, base_extractiveness, 20, 0.28).
narrative_ontology:measurement_basis(tsun_be_t20, observed).
narrative_ontology:measurement(tsun_be_t40, tsunami_stone_commitment__commemorative_husk_reading, base_extractiveness, 40, 0.45).
narrative_ontology:measurement_basis(tsun_be_t40, observed).
narrative_ontology:measurement(tsun_be_t60, tsunami_stone_commitment__commemorative_husk_reading, base_extractiveness, 60, 0.6).
narrative_ontology:measurement_basis(tsun_be_t60, observed).
narrative_ontology:measurement(tsun_be_t80, tsunami_stone_commitment__commemorative_husk_reading, base_extractiveness, 80, 0.71).
narrative_ontology:measurement_basis(tsun_be_t80, observed).
narrative_ontology:measurement(tsun_be_t100, tsunami_stone_commitment__commemorative_husk_reading, base_extractiveness, 100, 0.78).
narrative_ontology:measurement_basis(tsun_be_t100, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(tsunami_stone_commitment__commemorative_husk_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(tsunami_stone_commitment__commemorative_husk_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(tsunami_stone_commitment__commemorative_husk_reading, 0.08).
narrative_ontology:affects_constraint(tsunami_stone_commitment__commemorative_husk_reading, tsunami_stone_commitment__behavioral_competence_reading).
narrative_ontology:affects_constraint(tsunami_stone_commitment__commemorative_husk_reading, tsunami_stone_commitment__catastrophe_validation_axis).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the tsunami_stone_commitment kernel. behavioral_competence_reading claims the opposite historical fact (active living transmission, near-zero extraction, rope/mountain-adjacent profile) about the same physical marker. catastrophe_validation_axis treats the 2011 tsunami as the decisive empirical test between the two settlement-behavior readings rather than taking a position itself. All three share the stone as kernel and must remain linked via affects_constraints; none averages or hedges across the others' epsilon values -- each is a clean, ε-invariant constraint in its own right per Rule 1.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
