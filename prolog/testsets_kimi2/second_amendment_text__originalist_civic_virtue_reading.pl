% ============================================================================
% CONSTRAINT STORY: second_amendment_text__originalist_civic_virtue_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_second_amendment_text__originalist_civic_virtue_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: second_amendment_text__originalist_civic_virtue_reading
 *   human_readable: Originalist Civic Virtue Reading of the Second Amendment
 *   domain: constitutional_law/political_theory/firearms_policy
 *
 * SUMMARY:
 *   This constraint story instantiates the originalist civic virtue reading
 *   of the Second Amendment: the right to keep and bear arms protects the
 *   citizen-soldier capacity of a universal militia understood as the
 *   politically organized armed citizenry. The kernel label 'Second
 *   Amendment' conflates three structurally distinct readings; this reading
 *   is distinguished by its beneficiary (the political community qua
 *   citizenry), its absence of a specific victim set, and its tethering of
 *   the right to civic republican function rather than personal self-defense
 *   or state regulatory authority. The story treats the standing arrangement
 *   as a coordination device for republican collective defense whose
 *   practical foundation has eroded while its legal form persists.
 *
 * KEY AGENTS:
 *   - political_community: Primary beneficiary (organized/constrained) â receives the protected capacity for collective armed defense and republican civic virtue.
 *   - federal_judiciary: Agenda-setter (institutional/analytical) â interprets and enforces the reading, maintaining its legal force despite drift in actual militia practice.
 *   - gun_regulation_advocates: Excluded voice (organized/constrained) â structurally excluded from the reading's normative framework because their preferred policies would dissolve the citizen-soldier capacity.
 *   - civic_republican_scholars: Analytical observer (analytical/analytical) â documents the historical structure and argues for the reading's theoretical coherence without bearing direct costs.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_text__originalist_civic_virtue_reading, 0.35).
domain_priors:suppression_score(second_amendment_text__originalist_civic_virtue_reading, 0.35).
domain_priors:theater_ratio(second_amendment_text__originalist_civic_virtue_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_text__originalist_civic_virtue_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(second_amendment_text__originalist_civic_virtue_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(second_amendment_text__originalist_civic_virtue_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_text__originalist_civic_virtue_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(second_amendment_text__originalist_civic_virtue_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_text__originalist_civic_virtue_reading, rope).
narrative_ontology:human_readable(second_amendment_text__originalist_civic_virtue_reading, "Originalist Civic Virtue Reading of the Second Amendment").
narrative_ontology:topic_domain(second_amendment_text__originalist_civic_virtue_reading, "constitutional_law/political_theory/firearms_policy").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_text__originalist_civic_virtue_reading, 'af20784b-41d3-42e3-9a99-b6ff59967040').
narrative_ontology:cs_kernel_codification('af20784b-41d3-42e3-9a99-b6ff59967040', fixed_text).
narrative_ontology:cs_authority_grounding('af20784b-41d3-42e3-9a99-b6ff59967040', lineage).
narrative_ontology:cs_interpretation_layer_present('af20784b-41d3-42e3-9a99-b6ff59967040').
narrative_ontology:cs_reading_relation('af20784b-41d3-42e3-9a99-b6ff59967040', second_amendment_text__individual_right_reading, forecloses).
narrative_ontology:cs_reading_relation('af20784b-41d3-42e3-9a99-b6ff59967040', second_amendment_text__collective_security_reading, coexists_with).
narrative_ontology:cs_axiom('af20784b-41d3-42e3-9a99-b6ff59967040', foundational, militia_clause_operative_defines_right).
narrative_ontology:cs_axiom_status(militia_clause_operative_defines_right, holdable).
narrative_ontology:cs_axiom_grounding('af20784b-41d3-42e3-9a99-b6ff59967040', militia_clause_operative_defines_right, empirically_contingent).
narrative_ontology:cs_axiom('af20784b-41d3-42e3-9a99-b6ff59967040', foundational, republican_liberty_requires_armed_citizenry).
narrative_ontology:cs_axiom_status(republican_liberty_requires_armed_citizenry, holdable).
narrative_ontology:cs_axiom_grounding('af20784b-41d3-42e3-9a99-b6ff59967040', republican_liberty_requires_armed_citizenry, instrumental).
narrative_ontology:cs_reference_frame('af20784b-41d3-42e3-9a99-b6ff59967040', founding_era_civic_republican_order).
narrative_ontology:cs_drift_state('af20784b-41d3-42e3-9a99-b6ff59967040', contemporary_professional_military_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('af20784b-41d3-42e3-9a99-b6ff59967040', '').
narrative_ontology:cs_kernel_id(second_amendment_text__originalist_civic_virtue_reading, second_amendment_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_text__originalist_civic_virtue_reading, political_community).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Comprises the citizenry whose collective capacity for armed self-defense and civic virtue the reading protects. Benefits from the constitutional guarantee that the federal government cannot disarm the militia of the whole, preserving a republican distribution of coercive power. Exit from this arrangement would require constitutional amendment or renunciation of citizenship.
narrative_ontology:constraint_stakeholder(second_amendment_text__originalist_civic_virtue_reading, political_community, beneficiary,
    organized, generational, constrained, national).

% Interprets and enforces the Second Amendment through case law and precedent. Under this reading, it must assess whether challenged regulations infringe the citizen-soldier capacity of the political community. Its interpretations bind lower courts and legislatures, maintaining the constraint's legal force even as actual militia practice has disappeared.
narrative_ontology:constraint_stakeholder(second_amendment_text__originalist_civic_virtue_reading, federal_judiciary, agenda_setter,
    institutional, generational, analytical, national).

% Seek comprehensive firearms regulation and view widespread civilian armament as a public safety threat. Under the civic virtue reading, their preferred policies are constitutionally foreclosed because disarming the citizenry would destroy the militia capacity the amendment protects. They participate in public discourse but are structurally excluded from the reading's normative framework.
narrative_ontology:constraint_stakeholder(second_amendment_text__originalist_civic_virtue_reading, gun_regulation_advocates, excluded,
    organized, biographical, constrained, national).

% Analyze and articulate the historical and theoretical foundations of the civic virtue reading. They document the founding-era understanding of militia as universal armed citizenry and argue for its continuing relevance to republican theory, without directly collecting benefits or bearing the constraint's costs.
narrative_ontology:constraint_stakeholder(second_amendment_text__originalist_civic_virtue_reading, civic_republican_scholars, observer,
    analytical, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Ensures that the political community retains a distributed capacity for armed collective defense without relying entirely on a professional standing army, solving the republican problem of civic military virtue and the practical problem of mobilizing defense from a broad citizen base rather than a narrow caste.
% TRANSFER_FUNCTION: Transfers the legal guarantee of non-disarmament from the federal government to the citizenry, preventing the state from monopolizing lethal force and preserving a republican balance of coercive power between rulers and ruled.
% ABSENT_VOICES: Gun regulation advocates who view widespread civilian armament as a public safety threat, and collective-security proponents who would subordinate all arms policy to state regulatory authority, are structurally excluded from the reading's normative framework; their arguments are treated as constitutionally out of bounds because they would dissolve the citizen-soldier capacity.
% DISAPPEARANCE_RATIONALE: If the civic virtue reading vanished, the legal framework would shift toward either the individual self-defense reading (expanding personal rights beyond militia context) or the collective security reading (permitting broader state regulation). The political community would lose the specific constitutional guarantee of citizen-soldier capacity, and the republican justification for widespread armament would no longer anchor Second Amendment jurisprudence.
% FOUNDING_PROBLEM: The founding generation needed to ensure national and state defense without creating a standing army that threatened republican liberty, while guaranteeing that the federal government could not disarm the citizenry and thereby destroy the militia system that was seen as the proper bulwark of a free state.
% FOUNDING_PROBLEM_CORROBORATION: Civic republican historians and legal scholars attest to the founding problem from analytical seats outside direct beneficiary interest; however, modern military historians and gun-regulation advocates attest that the problem is obsolete due to the professionalization of warfare, and no party entirely outside the interpretive dispute corroborates the continued liveness of the founding problem without contest.
narrative_ontology:disappearance_verdict(second_amendment_text__originalist_civic_virtue_reading, world_rearranges).
narrative_ontology:founding_problem_status(second_amendment_text__originalist_civic_virtue_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_text__originalist_civic_virtue_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(second_amendment_text__originalist_civic_virtue_reading, 'none', 1).
narrative_ontology:epsilon_provenance(second_amendment_text__originalist_civic_virtue_reading, 0.35, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_text__originalist_civic_virtue_reading_tests).
:- end_tests(second_amendment_text__originalist_civic_virtue_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.35 because, although the constraint originates as genuine coordination, the disappearance of the universal militia means the legal form now imposes costs on modern governance without delivering its founding coordination function. Theater ratio rises to 0.55 because an increasing share of the reading's maintenance consists in performative historical argumentation rather than actual militia organization. Suppression is moderate at 0.35: the reading suppresses gun control measures that would disarm the citizenry, but it does not require active coercion against widespread alternatives (standing army, professional police) that already exist. Accessibility collapse is moderate at 0.45 because, while the reading renders disarmament constitutionally illegitimate, the practical alternative of professional defense is omnipresent. Resistance is 0.55 because the reading faces sustained opposition from gun control advocates and modern security-state proponents.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary seat (political community) experiences the constraint as a preserved republican liberty and coordination surplus. The excluded seat (gun regulation advocates) experiences the same constraint as an obsolete barrier to public safety policy that channels social costs into widespread armament. The analytical seat (civic republican scholars) sees the coordination function as theoretically live but practically atrophied. The engine computes this divergence from the structural data: low directionality for the beneficiary, high effective exclusion for the excluded voice, and symmetrically moderate for the agenda-setter.
 *
 * DIRECTIONALITY LOGIC:
 *   The political community is the declared beneficiary, sitting near the full-beneficiary end of the directionality spectrum: the constraint subsidizes their capacity and limits government power over them. There is no declared victim set, so no agent sits near the full-target end. The federal judiciary, as agenda-setter, sits near symmetric: it neither collects the benefit nor pays the cost, but administers the arrangement. Gun regulation advocates are excluded rather than targeted, meaning their exclusion is structural but not captured in the extraction arithmetic.
 *
 * MANDATROPHY ANALYSIS:
 *   The rope classification prevents two common mislabelings. First, it prevents treating the constraint as a snare: because there is no concentrated extraction and no identifiable victim set, the arrangement does not fit the pure-extraction profile, despite the fact that gun violence produces diffuse social costs. Second, it prevents treating the constraint as a mountain: constitutional text is humanly enacted and interpretively maintained, not an irreducible physical limit. The rising theater ratio and extractiveness over time signal that the coordination function has partially atrophied, but the absence of a sunset clause and the presence of a concentrated beneficiary community keep the classification from collapsing into piton.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This constraint instantiates the originalist_civic_virtue_reading of second_amendment_text. Sibling readings (individual_right_reading, collective_security_reading) assign different beneficiaries and different directionality to the same textual kernel. Does the civic virtue reading''s structural dependence on a non-existent universal militia make it a rope whose coordination function has atrophied, or a scaffold whose transition was never completed?',
    'Historical analysis of militia participation rates and functional military role from 1791 to present; comparative study of whether republican liberty correlates with armed citizenry in modern states.',
    'If the militia system is irrecoverably dead, the reading''s coordination function is hollow and the constraint likely computes as piton or theater-heavy rope; if the civic function is revivable, the coordination claim remains structurally viable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Structural viability of civic virtue reading given militia obsolescence').

omega_variable(
    text_vs_principle_framing,
    'Is the constraint better framed as a fixed_text commitment to the semantic range of the word ''Militia'' in 1791, or as an implicit commitment to a civic republican principle that happens to be encoded in that word?',
    'Examining whether the reading survives counterfactual evidence that ratifiers used ''militia'' loosely; assessing whether the authority structure treats textual deviation as fatal or absorbable into principle.',
    'A text-bound framing yields a brittle constraint with high theater as practice drifts; a principle-bound framing yields a more adaptable but less determinate constraint with different Boltzmann and directionality profiles.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(text_vs_principle_framing, conceptual, 'Alternative framing under-determination for kernel commitment').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_text__originalist_civic_virtue_reading, 0, 233).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(seco_tr_t0, second_amendment_text__originalist_civic_virtue_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(seco_tr_t40, second_amendment_text__originalist_civic_virtue_reading, theater_ratio, 40, 0.1).
narrative_ontology:measurement(seco_tr_t80, second_amendment_text__originalist_civic_virtue_reading, theater_ratio, 80, 0.18).
narrative_ontology:measurement(seco_tr_t120, second_amendment_text__originalist_civic_virtue_reading, theater_ratio, 120, 0.28).
narrative_ontology:measurement(seco_tr_t160, second_amendment_text__originalist_civic_virtue_reading, theater_ratio, 160, 0.38).
narrative_ontology:measurement(seco_tr_t200, second_amendment_text__originalist_civic_virtue_reading, theater_ratio, 200, 0.48).
narrative_ontology:measurement(seco_tr_t233, second_amendment_text__originalist_civic_virtue_reading, theater_ratio, 233, 0.55).

% Extraction over time
narrative_ontology:measurement(seco_be_t0, second_amendment_text__originalist_civic_virtue_reading, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(seco_be_t40, second_amendment_text__originalist_civic_virtue_reading, base_extractiveness, 40, 0.08).
narrative_ontology:measurement(seco_be_t80, second_amendment_text__originalist_civic_virtue_reading, base_extractiveness, 80, 0.12).
narrative_ontology:measurement(seco_be_t120, second_amendment_text__originalist_civic_virtue_reading, base_extractiveness, 120, 0.18).
narrative_ontology:measurement(seco_be_t160, second_amendment_text__originalist_civic_virtue_reading, base_extractiveness, 160, 0.24).
narrative_ontology:measurement(seco_be_t200, second_amendment_text__originalist_civic_virtue_reading, base_extractiveness, 200, 0.3).
narrative_ontology:measurement(seco_be_t233, second_amendment_text__originalist_civic_virtue_reading, base_extractiveness, 233, 0.35).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(second_amendment_text__originalist_civic_virtue_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(second_amendment_text__originalist_civic_virtue_reading, collective_security_reading).
narrative_ontology:affects_constraint(second_amendment_text__originalist_civic_virtue_reading, individual_right_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the second_amendment_text kernel, decomposed per the epsilon-invariance principle because the colloquial label 'Second Amendment' conflates three structurally distinct readings with different beneficiary sets, victim sets, and coordination/extraction profiles.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
