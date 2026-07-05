% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_preservation__survival_competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_preservation__survival_competence_reading, []).

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
 *   constraint_id: catastrophe_memory_preservation__survival_competence_reading
 *   human_readable: Catastrophe-Anniversary Ritual as Operational Threat-Recognition Transfer
 *   domain: religious/cultural/collective_memory
 *
 * SUMMARY:
 *   A community that survived a catastrophe (flood, famine, massacre, plague)
 *   institutes an annual or generational ritual that binds mourning to an
 *   explicit rehearsal of precursor-recognition and response drill — the
 *   claim is that this transmits operational survival competence, not merely
 *   commemorates loss. Under the survival-competence reading, the ritual's
 *   costly, mandatory, affect-laden form is precisely what makes
 *   threat-recognition memorable and executable under future stress, rather
 *   than being incidental grief-work. This reading treats the entanglement of
 *   drill with mourning as functionally necessary: stripped-down
 *   informational transfer (a pamphlet, a museum plaque) would not produce
 *   the same embodied recognition speed when a real precursor sign appears
 *   generations later.
 *
 * KEY AGENTS:
 *   - community_continuity_institution: administers and enforces the ritual form, collects legitimacy from being memory's keeper
 *   - present_generation_participants: bear the ritual's psychological and time costs without having chosen the original catastrophe
 *   - future_generation_survivors: hypothetical beneficiaries who cannot consent or verify the transfer in advance
 *   - ritual_specialists: professionalized interpreters with career stakes in the ritual's continued authority
 *   - civil_defense_researchers: analytical observers assessing whether the transmitted content still matches the actual hazard signature
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_preservation__survival_competence_reading, 0.71).
domain_priors:suppression_score(catastrophe_memory_preservation__survival_competence_reading, 0.62).
domain_priors:theater_ratio(catastrophe_memory_preservation__survival_competence_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_preservation__survival_competence_reading, extractiveness, 0.71).
narrative_ontology:constraint_metric(catastrophe_memory_preservation__survival_competence_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(catastrophe_memory_preservation__survival_competence_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_preservation__survival_competence_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(catastrophe_memory_preservation__survival_competence_reading, resistance, 0.44).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_preservation__survival_competence_reading, tangled_rope).
narrative_ontology:human_readable(catastrophe_memory_preservation__survival_competence_reading, "Catastrophe-Anniversary Ritual as Operational Threat-Recognition Transfer").
narrative_ontology:topic_domain(catastrophe_memory_preservation__survival_competence_reading, "religious/cultural/collective_memory").

domain_priors:requires_active_enforcement(catastrophe_memory_preservation__survival_competence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_preservation__survival_competence_reading, '0fe5d85b-1116-465c-883a-2da644095a0c').
narrative_ontology:cs_kernel_codification('0fe5d85b-1116-465c-883a-2da644095a0c', implicit).
narrative_ontology:cs_authority_grounding('0fe5d85b-1116-465c-883a-2da644095a0c', practice).
narrative_ontology:cs_interpretation_layer_present('0fe5d85b-1116-465c-883a-2da644095a0c').
narrative_ontology:cs_reading_relation('0fe5d85b-1116-465c-883a-2da644095a0c', catastrophe_memory_preservation__mourning_practice_reading, coexists_with).
narrative_ontology:cs_reading_relation('0fe5d85b-1116-465c-883a-2da644095a0c', catastrophe_memory_preservation__hybrid_atrophy_reading, influences).
narrative_ontology:cs_axiom('0fe5d85b-1116-465c-883a-2da644095a0c', foundational, costly_affective_form_is_functionally_necessary_for_transfer).
narrative_ontology:cs_axiom_status(costly_affective_form_is_functionally_necessary_for_transfer, holdable).
narrative_ontology:cs_axiom_grounding('0fe5d85b-1116-465c-883a-2da644095a0c', costly_affective_form_is_functionally_necessary_for_transfer, instrumental).
narrative_ontology:cs_axiom('0fe5d85b-1116-465c-883a-2da644095a0c', foundational, operational_content_remains_intact_across_generations).
narrative_ontology:cs_axiom_status(operational_content_remains_intact_across_generations, holdable).
narrative_ontology:cs_axiom_grounding('0fe5d85b-1116-465c-883a-2da644095a0c', operational_content_remains_intact_across_generations, empirically_contingent).
narrative_ontology:cs_reference_frame('0fe5d85b-1116-465c-883a-2da644095a0c', original_catastrophe_precursor_signature).
narrative_ontology:cs_drift_state('0fe5d85b-1116-465c-883a-2da644095a0c', contemporary_third_generation_observance, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('0fe5d85b-1116-465c-883a-2da644095a0c', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_preservation__survival_competence_reading, catastrophe_memory_preservation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_preservation__survival_competence_reading, future_generation_survivors).
narrative_ontology:constraint_beneficiary(catastrophe_memory_preservation__survival_competence_reading, community_continuity_institution).
narrative_ontology:constraint_victim(catastrophe_memory_preservation__survival_competence_reading, present_generation_participants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(catastrophe_memory_preservation__survival_competence_reading, ritual_specialists).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets the calendar and liturgical form of the catastrophe-anniversary ritual: who must fast, recite, reenact, or bear witness, and in what sequence. Frames non-participation as forgetting the danger itself, not merely as a social lapse. Retains discretion over which threat-recognition content is preserved and which is allowed to lapse, and collects legitimacy and continued authority from being the keeper of the warning.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__survival_competence_reading, community_continuity_institution, agenda_setter,
    institutional, civilizational, arbitrage, national).

% Must annually re-enact grief and danger-recognition drills tied to a catastrophe most did not personally experience. Bear the psychological cost of ritualized re-exposure to threat cues, the time cost of mandatory participation, and social sanction for partial or skeptical engagement. Exit means visible defection from communal memory and risks being read as betrayal of the dead, not as a policy disagreement.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__survival_competence_reading, present_generation_participants, payer,
    moderate, biographical, constrained, regional).

% Do not yet exist as participating agents; benefit only if the transmitted pattern-recognition (early warning signs, appropriate response sequence) proves operationally accurate when a comparable threat recurs. They cannot consent to the cost borne on their behalf and cannot verify in advance that what is being preserved is genuinely operational rather than merely symbolic.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__survival_competence_reading, future_generation_survivors, beneficiary,
    powerless, generational, analytical, national).

% Clergy, elders, or trained memory-keepers who administer the ritual's precise sequence. Derive status, income, and social standing from being the certified interpreters of the danger-pattern. Have professional incentive to preserve the ritual's authority even where its content has drifted from operational accuracy.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__survival_competence_reading, ritual_specialists, beneficiary,
    organized, generational, mobile, regional).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_preservation__survival_competence_reading, ritual_specialists, agenda_setter).

% Would argue that the ritual's current form no longer transmits usable threat-recognition information and instead transmits diffuse anxiety and obligation; their objection is rarely solicited because raising it during the ritual itself is treated as disrespect to catastrophe victims rather than as a claim about operational efficacy.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__survival_competence_reading, skeptical_descendants, excluded,
    powerless, biographical, trapped, local).

% Study whether the ritual's content (evacuation cues, early-warning signs, response sequences) still matches the actual hazard profile, or whether it has been symbolically preserved past the point of practical accuracy. Their findings could validate or undercut the ritual's operational claim but are not binding on the institution that administers it.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__survival_competence_reading, civil_defense_researchers, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_memory_preservation__survival_competence_reading, diffuse).
narrative_ontology:fixing_cost_class(catastrophe_memory_preservation__survival_competence_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Transmits, across generations who share no direct experiential memory of the catastrophe, an operational pattern for recognizing precursor signs and executing an appropriate response sequence — solving the genuine problem that survival-relevant knowledge decays if left to informal, unstructured transmission.
% TRANSFER_FUNCTION: Moves psychological, temporal, and social cost from the present generation of participants to the benefit of a hypothetical future generation that may face a comparable threat; also moves interpretive authority and status to the ritual specialists who administer the transmission.
% ABSENT_VOICES: Skeptical descendants who suspect the ritual now transmits obligation and anxiety rather than usable operational content are structurally excluded from raising this during the ritual itself, since doing so is coded as disrespecting the catastrophe's victims rather than as a claim about present efficacy.
% DISAPPEARANCE_RATIONALE: The community-continuity institution and ritual specialists would say the world rearranges catastrophically — the next generation loses the pattern-recognition that prevents recurrence. Civil defense researchers and skeptical descendants would say the world is largely unchanged in operational terms, because the ritual's actual predictive content has already drifted from the original hazard signature; only the social and institutional structure built atop it would visibly collapse.
% FOUNDING_PROBLEM: A specific catastrophe occurred whose precursor signs were not recognized in time; survivors sought to ensure the next generation would recognize those signs and respond correctly rather than repeat the same failure.
% FOUNDING_PROBLEM_CORROBORATION: Ritual specialists and the continuity institution attest the founding problem remains live and that the ritual's operational content is intact. Civil defense researchers, examining the ritual's actual content against the current hazard profile from outside the administering institution, report the transmitted cues have drifted toward symbolic gesture and no longer reliably map onto the precursor signs that preceded the original catastrophe; this is the only corroboration offered from outside the benefiting parties, and it partially contradicts the institution's own claim.
narrative_ontology:disappearance_verdict(catastrophe_memory_preservation__survival_competence_reading, contested).
narrative_ontology:founding_problem_status(catastrophe_memory_preservation__survival_competence_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_preservation__survival_competence_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(catastrophe_memory_preservation__survival_competence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_preservation__survival_competence_reading, 0.71, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_preservation__survival_competence_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(catastrophe_memory_preservation__survival_competence_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(catastrophe_memory_preservation__survival_competence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (0.71 by interval end) because the survival-competence reading commits to the ritual's costly, mandatory, affect-fused form as functionally load-bearing — the cost IS the mechanism, not incidental overhead, and that cost falls disproportionately on present participants who did not choose the founding catastrophe. Suppression (0.62) reflects the social sanction attached to visible non-participation, which is real but softer than legal coercion — defection is punished by communal judgment, not by force. Theater ratio is kept comparatively low (0.28) relative to the sibling hybrid_atrophy_reading precisely because this reading's core claim is that the operational content has NOT decayed into pure performance; a rising theater_ratio trend is authored anyway to register honest uncertainty about long-run drift even within this reading's own commitments.
 *
 * PERSPECTIVAL GAP:
 *   From the institution's seat, mandatory costly participation is the coordination mechanism itself — you cannot get operational, stress-tested pattern recognition transmitted generations forward through low-cost signaling alone. From the present-participant seat, the same structure is experienced as extraction: a cost imposed by the dead and the not-yet-born, enforced by social sanction, with no personal opportunity to verify the transfer will ever be used or was ever accurate. The engine should register genuine seat divergence here rather than a single verdict, because both readings of the lived experience are structurally coherent from their respective positions.
 *
 * DIRECTIONALITY LOGIC:
 *   The community-continuity institution and ritual specialists sit near the beneficiary end: they administer the transfer and derive status/authority from being its stewards, and under this reading their stewardship is doing genuine coordination work. Present-generation participants sit near the target end: constrained exit, real biographical cost, no personal stake in the original catastrophe, and no ability to verify the transfer's eventual payoff. Future-generation survivors are the ultimate beneficiary but hold zero power to shape or consent to the arrangement — an extreme case of benefit without voice. Skeptical descendants are excluded rather than merely dissenting: their doubt about operational content is structurally unwelcome inside the ritual frame itself.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem/disappearance_verdict pairing is authored as contested rather than resolved: the institution and specialists attest the founding problem (precursor-sign blindness) is still live, while the only outside corroboration (civil defense researchers) reports partial drift in the transmitted content. This mismatch is exactly the signal the R5 genealogy interview is designed to surface — it prevents the survival-competence reading from being taken at face value while also preventing a premature reclassification to pure mourning-practice or atrophy without independent evidence.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    operational_content_verifiability,
    'Does the ritual''s transmitted content (specific cues, sequences, warnings) actually match the precursor signature of the original catastrophe, or has it drifted into symbolic gesture that only feels operational?',
    'Independent comparison of the ritual''s specific transmitted content against the documented precursor signs of the original catastrophic event, conducted by researchers outside the administering institution, ideally validated against any subsequent near-miss or recurrence event.',
    'If content has drifted substantially, this reading collapses toward the hybrid_atrophy_reading or even the mourning_practice_reading — the high extractiveness would then be unmoored from any genuine coordination payoff, pushing the classification toward snare or piton rather than tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(operational_content_verifiability, empirical, 'Whether the ritual''s specific content still tracks the original hazard signature.').

omega_variable(
    consent_of_future_beneficiaries,
    'Can a transfer to non-existent future beneficiaries who cannot consent or verify ever be evaluated as pure coordination rather than partially as imposed extraction on the present generation?',
    'No empirical resolution exists; this is a framing question about how intergenerational transfer without consent should be weighted against intergenerational transfer''s genuine necessity for any long-horizon survival knowledge.',
    'Resolving toward ''no genuine consent possible, therefore always partially extractive'' pushes even a functionally accurate ritual toward tangled_rope or snare; resolving toward ''necessity substitutes for consent in genuinely irreplaceable knowledge transfer'' supports a purer rope reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(consent_of_future_beneficiaries, preference, 'Whether unconsented intergenerational benefit transfer can ever be classified as non-extractive.').

omega_variable(
    kernel_reading_selection_basis,
    'What in the specific ritual''s observable structure justifies selecting the survival_competence_reading over the mourning_practice_reading or hybrid_atrophy_reading as the operative framing for THIS community''s practice?',
    'Compare this ritual against the other two sibling readings using the same community''s own historical record: does the ritual''s content include verifiable operational specificity (named precursor signs, drilled response sequences) beyond generic commemorative elements (recitation of names, symbolic reenactment without instructional content)?',
    'If the ritual''s content is found to be predominantly symbolic/commemorative with no drilled operational component, the correct reading for this community is mourning_practice_reading, not survival_competence_reading, and the high extractiveness authored here would be misattributed to a coordination function that does not exist.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection_basis, conceptual, 'Whether this specific ritual''s observable content actually supports the survival-competence framing chosen for this story.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_preservation__survival_competence_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_preservation__survival_competence_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(cata_tr_t10, catastrophe_memory_preservation__survival_competence_reading, theater_ratio, 10, 0.13).
narrative_ontology:measurement(cata_tr_t20, catastrophe_memory_preservation__survival_competence_reading, theater_ratio, 20, 0.17).
narrative_ontology:measurement(cata_tr_t30, catastrophe_memory_preservation__survival_competence_reading, theater_ratio, 30, 0.2).
narrative_ontology:measurement(cata_tr_t40, catastrophe_memory_preservation__survival_competence_reading, theater_ratio, 40, 0.23).
narrative_ontology:measurement(cata_tr_t50, catastrophe_memory_preservation__survival_competence_reading, theater_ratio, 50, 0.26).
narrative_ontology:measurement(cata_tr_t60, catastrophe_memory_preservation__survival_competence_reading, theater_ratio, 60, 0.28).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_preservation__survival_competence_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(cata_be_t10, catastrophe_memory_preservation__survival_competence_reading, base_extractiveness, 10, 0.5).
narrative_ontology:measurement(cata_be_t20, catastrophe_memory_preservation__survival_competence_reading, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(cata_be_t30, catastrophe_memory_preservation__survival_competence_reading, base_extractiveness, 30, 0.63).
narrative_ontology:measurement(cata_be_t40, catastrophe_memory_preservation__survival_competence_reading, base_extractiveness, 40, 0.67).
narrative_ontology:measurement(cata_be_t50, catastrophe_memory_preservation__survival_competence_reading, base_extractiveness, 50, 0.7).
narrative_ontology:measurement(cata_be_t60, catastrophe_memory_preservation__survival_competence_reading, base_extractiveness, 60, 0.71).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_memory_preservation__survival_competence_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(cata_su_t10, catastrophe_memory_preservation__survival_competence_reading, suppression_requirement, 10, 0.46).
narrative_ontology:measurement(cata_su_t20, catastrophe_memory_preservation__survival_competence_reading, suppression_requirement, 20, 0.51).
narrative_ontology:measurement(cata_su_t30, catastrophe_memory_preservation__survival_competence_reading, suppression_requirement, 30, 0.55).
narrative_ontology:measurement(cata_su_t40, catastrophe_memory_preservation__survival_competence_reading, suppression_requirement, 40, 0.58).
narrative_ontology:measurement(cata_su_t50, catastrophe_memory_preservation__survival_competence_reading, suppression_requirement, 50, 0.61).
narrative_ontology:measurement(cata_su_t60, catastrophe_memory_preservation__survival_competence_reading, suppression_requirement, 60, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_preservation__survival_competence_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(catastrophe_memory_preservation__survival_competence_reading, 0.1).
narrative_ontology:affects_constraint(catastrophe_memory_preservation__survival_competence_reading, catastrophe_memory_preservation__mourning_practice_reading).
narrative_ontology:affects_constraint(catastrophe_memory_preservation__survival_competence_reading, catastrophe_memory_preservation__hybrid_atrophy_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the catastrophe_memory_preservation kernel. survival_competence_reading (this file) claims high extractiveness justified by genuine operational transfer, tangled_rope. mourning_practice_reading claims the same ritual transmits only symbolic/identity continuity with no operational component, which would remove the coordination justification for the extraction authored here. hybrid_atrophy_reading claims the operational function is real but historically decayed, situating this ritual on a decay trajectory this story does not itself assert. All three should be evaluated as siblings sharing one ritual-practice referent but instantiating structurally distinct claims about its content and function, per the ε-invariance principle — the empirical omega operational_content_verifiability is the adjudicating fact between them.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
