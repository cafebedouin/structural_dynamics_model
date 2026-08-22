% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_kernel__survival_competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_kernel__survival_competence_reading, []).

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
 *   constraint_id: catastrophe_memory_kernel__survival_competence_reading
 *   human_readable: Ritual as Encoded Persecution-Survival Competence
 *   domain: religious_studies/collective_memory/ritual_practice
 *
 * SUMMARY:
 *   This story instantiates the survival_competence_reading of the
 *   catastrophe_memory_kernel: mourning practices and calendrical ritual
 *   observance are read as an encoding-and-transmission system for
 *   operational persecution-survival competence — concealment behavior,
 *   mutual aid, rapid resource mobilization — passed generationally without
 *   dependence on centralized, seizable institutions. The coordination
 *   function is real (competence transmission that would otherwise depend on
 *   fragile oral or written records survives ritual repetition), but it is
 *   bundled with boundary-maintenance enforcement that falls
 *   disproportionately on members seeking lower-visibility integration and on
 *   mixed-heritage families. Extraction is moderate, not high: this is not a
 *   story of concentrated rent capture but of diffuse, ongoing conformity
 *   costs paid to sustain a competence-transmission function that the
 *   community as a whole (including its future members) draws on.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_kernel__survival_competence_reading, 0.42).
domain_priors:suppression_score(catastrophe_memory_kernel__survival_competence_reading, 0.38).
domain_priors:theater_ratio(catastrophe_memory_kernel__survival_competence_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_kernel__survival_competence_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__survival_competence_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__survival_competence_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_kernel__survival_competence_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__survival_competence_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_kernel__survival_competence_reading, tangled_rope).
narrative_ontology:human_readable(catastrophe_memory_kernel__survival_competence_reading, "Ritual as Encoded Persecution-Survival Competence").
narrative_ontology:topic_domain(catastrophe_memory_kernel__survival_competence_reading, "religious_studies/collective_memory/ritual_practice").

domain_priors:requires_active_enforcement(catastrophe_memory_kernel__survival_competence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_kernel__survival_competence_reading, '683db910-eee8-4b91-9f47-c7bb38212ebc').
narrative_ontology:cs_kernel_codification('683db910-eee8-4b91-9f47-c7bb38212ebc', implicit).
narrative_ontology:cs_authority_grounding('683db910-eee8-4b91-9f47-c7bb38212ebc', practice).
narrative_ontology:cs_interpretation_layer_present('683db910-eee8-4b91-9f47-c7bb38212ebc').
narrative_ontology:cs_reading_relation('683db910-eee8-4b91-9f47-c7bb38212ebc', catastrophe_memory_kernel__boundary_maintenance_reading, coexists_with).
narrative_ontology:cs_reading_relation('683db910-eee8-4b91-9f47-c7bb38212ebc', catastrophe_memory_kernel__symbol_continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('683db910-eee8-4b91-9f47-c7bb38212ebc', catastrophe_memory_kernel__trauma_encoding_reading, influences).
narrative_ontology:cs_axiom('683db910-eee8-4b91-9f47-c7bb38212ebc', foundational, ritual_repetition_transmits_operational_skill).
narrative_ontology:cs_axiom_status(ritual_repetition_transmits_operational_skill, holdable).
narrative_ontology:cs_axiom_grounding('683db910-eee8-4b91-9f47-c7bb38212ebc', ritual_repetition_transmits_operational_skill, empirically_contingent).
narrative_ontology:cs_axiom('683db910-eee8-4b91-9f47-c7bb38212ebc', secondary, competence_value_justifies_conformity_cost).
narrative_ontology:cs_axiom_status(competence_value_justifies_conformity_cost, holdable).
narrative_ontology:cs_axiom_grounding('683db910-eee8-4b91-9f47-c7bb38212ebc', competence_value_justifies_conformity_cost, instrumental).
narrative_ontology:cs_reference_frame('683db910-eee8-4b91-9f47-c7bb38212ebc', active_persecution_era_rehearsal).
narrative_ontology:cs_drift_state('683db910-eee8-4b91-9f47-c7bb38212ebc', contemporary_diaspora_safety, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('683db910-eee8-4b91-9f47-c7bb38212ebc', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_kernel__survival_competence_reading, catastrophe_memory_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_kernel__survival_competence_reading, threatened_community_as_whole).
narrative_ontology:constraint_beneficiary(catastrophe_memory_kernel__survival_competence_reading, future_generations_facing_recurrence).
narrative_ontology:constraint_victim(catastrophe_memory_kernel__survival_competence_reading, assimilation_seeking_members).
narrative_ontology:constraint_victim(catastrophe_memory_kernel__survival_competence_reading, exogamous_and_mixed_families).
narrative_ontology:constraint_vindicates(catastrophe_memory_kernel__survival_competence_reading, ritual_rehearsal_transmits_operational_competence).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Collectively rehearses catastrophe-response patterns (fasting, vigilance calendars, concealment practices, communal mutual-aid drills embedded in mourning liturgy) through recurring ritual observance. Gains a distributed, transmissible repertoire of persecution-response behavior that does not depend on any single elder's memory surviving. Bears the ongoing cost of maintaining the practices even in periods of relative safety.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__survival_competence_reading, threatened_community_as_whole, beneficiary,
    organized, generational, constrained, regional).

% Have not yet faced the persecution event the ritual encodes for, but inherit the encoded competence automatically through participation in the calendar of observance. Cannot consent to or evaluate the transmission in advance; if the competence is needed, they receive it pre-loaded; if it is never needed, they still paid the participation cost.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__survival_competence_reading, future_generations_facing_recurrence, beneficiary,
    powerless, civilizational, trapped, regional).

% Want to reduce visible difference from the surrounding majority to lower persecution risk directly, rather than through ritual rehearsal. Continued observance requirements mark them as members subject to communal enforcement (social pressure, exclusion from mutual-aid networks, family rupture) if they attempt to opt out or intermarry. Exit costs them the very safety-net the ritual is claimed to build.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__survival_competence_reading, assimilation_seeking_members, payer,
    moderate, biographical, constrained, local).

% Occupy a boundary position where the ritual calendar's maintenance function directly costs them: children of mixed unions face contested status, exclusion from full participation, or coercive re-integration demands. Their situation is used to argue for stricter enforcement of the competence-transmission practices as a defense against dilution.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__survival_competence_reading, exogamous_and_mixed_families, payer,
    powerless, biographical, trapped, local).

% Administer the liturgical calendar, decide which practices count as required observance, and adjudicate boundary disputes. Their authority and social standing are constituted by their role as transmitters of the competence; they cannot easily separate their own position from the maintenance of the practice.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__survival_competence_reading, ritual_elders_and_officiants, agenda_setter,
    institutional, generational, identity_locked, regional).

% Historically the source of the threat the ritual encodes against. Not party to the community's internal negotiation over the ritual's cost, but their historical and potential future behavior is the entire justification for the practice's persistence.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__survival_competence_reading, external_persecuting_authorities, excluded,
    powerful, generational, analytical, regional).

% Study the correlation between ritual mourning-practice content and documented persecution-survival outcomes across multiple diaspora and minority traditions, without a stake in either the community's cohesion or its assimilation.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__survival_competence_reading, comparative_religious_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_memory_kernel__survival_competence_reading, diffuse).
narrative_ontology:fixing_cost_class(catastrophe_memory_kernel__survival_competence_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Ritual observance functions as a distributed training and rehearsal system: mourning practices, fasts, and calendrical vigilance behaviors encode operational responses to persecution (concealment, mutual aid, resource pooling, rapid mobilization) that can be transmitted to people who never experienced the original catastrophe, without requiring formal instruction or institutional archives that could themselves be targeted.
% TRANSFER_FUNCTION: Moves operational competence and mutual-aid capacity from those who survived past catastrophes to those who have not yet faced one, at the cost of continued conformity paid disproportionately by members who would prefer to reduce visible difference, and by families whose boundary-crossing unions are treated as dilution of the transmission chain.
% ABSENT_VOICES: Assimilation-seeking members and mixed-family members experience the enforcement costs of boundary maintenance but are structurally discouraged from voicing preference for a lower-visibility survival strategy, since doing so is read as abandoning the collective competence-transmission project rather than as a legitimate alternative risk calculus.
% DISAPPEARANCE_RATIONALE: The community's own elders and many members would say the world rearranges catastrophically — competence is lost and the group is more vulnerable to a repeat event with no rehearsed response. Assimilation-seeking members and outside historians are more likely to say the practical survival value is now largely symbolic under contemporary conditions, and its disappearance would mainly relieve boundary-enforcement costs without materially increasing persecution risk. The dispute is real and not resolvable from either seat alone.
% FOUNDING_PROBLEM: A community facing recurrent, unpredictable persecution needed a way to pass down concrete survival behaviors (concealment, mutual aid, resource mobilization) across generations without relying on centralized records or institutions that could be destroyed or seized by persecutors.
% FOUNDING_PROBLEM_CORROBORATION: Comparative religious historians studying multiple diaspora traditions attest that ritual-encoded behavioral repertoires do correlate with documented differential survival outcomes in some historical episodes, supporting that the founding problem was genuinely live at the practice's origin. The same historians are divided on whether the problem remains live under contemporary conditions in every community that still enforces the full practice, and assimilation-seeking members from inside the community attest that in their lived experience the operative function today is boundary enforcement, not rehearsed competence.
narrative_ontology:disappearance_verdict(catastrophe_memory_kernel__survival_competence_reading, contested).
narrative_ontology:founding_problem_status(catastrophe_memory_kernel__survival_competence_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_kernel__survival_competence_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(catastrophe_memory_kernel__survival_competence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_kernel__survival_competence_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_kernel__survival_competence_reading_tests).
:- end_tests(catastrophe_memory_kernel__survival_competence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness starts low (0.22) reflecting a period closer to active persecution memory, where the coordination function is undisputed, and rises over the interval (to 0.42) as the persecution threat recedes into more distant memory and the same practices increasingly operate as boundary enforcement without a correspondingly present threat to justify the cost — extraction accumulating as the founding problem's liveness becomes contested. Theater ratio rises in parallel (0.10 to 0.28) as more of the observance becomes performative continuity rather than functional rehearsal, though it stays below the piton threshold because a documented, non-trivial competence-transmission function persists. Suppression rises modestly (0.20 to 0.38): enforcement against exit and intermarriage hardens somewhat but never approaches the levels seen in constraints whose sole function is boundary policing.
 *
 * PERSPECTIVAL GAP:
 *   From the ritual_elders_and_officiants seat and the threatened_community_as_whole seat, the arrangement reads as continuous, functioning coordination — the same rehearsal that has worked before. From the assimilation_seeking_members and exogamous_and_mixed_families seats, the same calendar of observance reads as an enforcement mechanism whose survival justification no longer matches present conditions. The engine computes these as different per-seat classifications from the same structural data; the divergence is expected and is the finding, not an error to reconcile.
 *
 * DIRECTIONALITY LOGIC:
 *   threatened_community_as_whole and future_generations_facing_recurrence are declared beneficiaries: they receive competence transmission without paying an identifiable price beyond ordinary participation. assimilation_seeking_members and exogamous_and_mixed_families are declared victims: the same observance requirements that transmit competence to the willing impose direct, differentiated costs (exclusion, family rupture, social sanction) on those who would prefer an alternative risk-reduction strategy. ritual_elders_and_officiants are the agenda_setter, identity_locked because their social position is constituted by administering the practice, not merely benefiting from it externally.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification prevents this constraint from being flattened into either a pure Rope ("it's just heritage transmission, no one is harmed") or a pure Snare ("it's just boundary policing dressed as tradition"). Both the coordination function (competence transmission with plausible historical survival value) and the extraction (differentiated costs on assimilation-seeking and mixed-heritage members) are structurally real and load-bearing on the same mechanism — active enforcement of observance is what produces both the transmission and the boundary cost. Declaring only beneficiaries would mislabel the boundary-enforcement cost as a side effect; declaring only victims would erase the documented historical function the practice may still serve.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    competence_versus_symbolism_boundary,
    'At what point does a ritual practice that once encoded genuine operational survival competence (concealment routes, mutual-aid mobilization, resource caching) shift to encoding only symbolic continuity, such that continued enforcement of the full observance calendar no longer transmits usable competence but only group-boundary signaling?',
    'Comparative ethnographic and historical analysis: track whether specific ritual behaviors correspond to documented, testable survival-relevant skills (e.g., mutual-aid networks that actually mobilize during contemporary crises) versus behaviors that persist purely as identity markers with no operational content.',
    'If the competence content has substantially evaporated while enforcement persists, this reading''s claimed coordination function is weaker than authored and the constraint drifts toward the boundary_maintenance_reading''s territory — a signal that would push classification toward snare rather than tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(competence_versus_symbolism_boundary, empirical, 'Whether operational survival competence persists in current practice or has been replaced by pure boundary signaling.').

omega_variable(
    kernel_reading_indeterminacy,
    'Is the catastrophe_memory_kernel best understood as having one dominant function (competence transmission, boundary maintenance, symbolic continuity, or trauma encoding) with the others as side effects, or do all four operate simultaneously as genuinely distinct, co-present functions of the same ritual practice with no single dominant reading?',
    'This is inherently a framing question rather than one resolvable by additional data: different disciplinary lenses (functionalist anthropology, trauma studies, semiotics, historical sociology) will produce different dominant-function attributions from the same ethnographic record.',
    'If no single dominant function exists, treating any one reading (including this one) as THE constraint rather than as one lens among several risks overstating the coordination-function share and understating that boundary/trauma functions may be doing more structural work than this reading credits.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_indeterminacy, conceptual, 'Whether the kernel has a genuinely dominant function or is irreducibly multi-functional across the four sibling readings.').

omega_variable(
    consent_of_future_beneficiaries,
    'Can future_generations_facing_recurrence be treated as genuine beneficiaries when they inherit the competence-transmission cost (continued observance) before any threat materializes and without the ability to consent to or evaluate the bargain?',
    'Compare cohorts where the persecution threat did versus did not recur after a period of enforced observance; assess whether ex post the inherited competence proved valuable relative to the enforcement costs paid.',
    'If recurrence is rare relative to the sustained cost of observance across many non-recurrence generations, the beneficiary framing for future generations is weaker than authored, and the true balance of extraction shifts further toward the payer seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consent_of_future_beneficiaries, preference, 'Whether treating future generations as beneficiaries is defensible given they cannot consent to the transmission bargain in advance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_kernel__survival_competence_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_kernel__survival_competence_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(cata_tr_t8, catastrophe_memory_kernel__survival_competence_reading, theater_ratio, 8, 0.14).
narrative_ontology:measurement(cata_tr_t16, catastrophe_memory_kernel__survival_competence_reading, theater_ratio, 16, 0.19).
narrative_ontology:measurement(cata_tr_t24, catastrophe_memory_kernel__survival_competence_reading, theater_ratio, 24, 0.22).
narrative_ontology:measurement(cata_tr_t32, catastrophe_memory_kernel__survival_competence_reading, theater_ratio, 32, 0.25).
narrative_ontology:measurement(cata_tr_t40, catastrophe_memory_kernel__survival_competence_reading, theater_ratio, 40, 0.28).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_kernel__survival_competence_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(cata_be_t8, catastrophe_memory_kernel__survival_competence_reading, base_extractiveness, 8, 0.27).
narrative_ontology:measurement(cata_be_t16, catastrophe_memory_kernel__survival_competence_reading, base_extractiveness, 16, 0.33).
narrative_ontology:measurement(cata_be_t24, catastrophe_memory_kernel__survival_competence_reading, base_extractiveness, 24, 0.37).
narrative_ontology:measurement(cata_be_t32, catastrophe_memory_kernel__survival_competence_reading, base_extractiveness, 32, 0.4).
narrative_ontology:measurement(cata_be_t40, catastrophe_memory_kernel__survival_competence_reading, base_extractiveness, 40, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_memory_kernel__survival_competence_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(cata_su_t8, catastrophe_memory_kernel__survival_competence_reading, suppression_requirement, 8, 0.25).
narrative_ontology:measurement(cata_su_t16, catastrophe_memory_kernel__survival_competence_reading, suppression_requirement, 16, 0.3).
narrative_ontology:measurement(cata_su_t24, catastrophe_memory_kernel__survival_competence_reading, suppression_requirement, 24, 0.33).
narrative_ontology:measurement(cata_su_t32, catastrophe_memory_kernel__survival_competence_reading, suppression_requirement, 32, 0.36).
narrative_ontology:measurement(cata_su_t40, catastrophe_memory_kernel__survival_competence_reading, suppression_requirement, 40, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_kernel__survival_competence_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(catastrophe_memory_kernel__survival_competence_reading, 0.1).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__survival_competence_reading, catastrophe_memory_kernel__boundary_maintenance_reading).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__survival_competence_reading, catastrophe_memory_kernel__symbol_continuity_reading).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__survival_competence_reading, catastrophe_memory_kernel__trauma_encoding_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of four sibling readings of the catastrophe_memory_kernel, each authored as a separate ε-invariant constraint per the decomposition principle. survival_competence_reading (this story) authors moderate extraction centered on operational-competence transmission with boundary-enforcement costs as the extraction mechanism. boundary_maintenance_reading authors the same ritual calendar with boundary enforcement as the PRIMARY function rather than a side cost — expect higher extraction and a more concentrated victim set. symbol_continuity_reading authors the practice as low-extraction identity/continuity preservation, closer to a Rope. trauma_encoding_reading authors a distinct victim set (descendants inheriting unprocessed fear/warning-signal content) and a different beneficiary/harm calculus than the competence-transmission framing used here. The four stories share a kernel but are not averaged, blended, or cross-referenced for consistency of ε — each stands on its own structural data.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
