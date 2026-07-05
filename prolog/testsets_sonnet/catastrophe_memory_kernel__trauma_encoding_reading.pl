% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_kernel__trauma_encoding_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_kernel__trauma_encoding_reading, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: catastrophe_memory_kernel__trauma_encoding_reading
 *   human_readable: Catastrophe-Memory Ritual as Intergenerational Trauma-Encoding Warning System
 *   domain: religious_studies/collective_memory/ritual_practice
 *
 * SUMMARY:
 *   This story instantiates one reading of the catastrophe_memory_kernel:
 *   that mourning-practice functions primarily as a trauma-transmission
 *   mechanism whose payoff is collective threat-vigilance and whose cost is a
 *   psychological burden imposed on generations that did not experience the
 *   original catastrophe. As the acute threat that founded the practice
 *   recedes into history while modern institutional alternatives for threat
 *   monitoring emerge (diaspora networks, human-rights bodies, state
 *   protections), the ratio of transmitted burden to marginal vigilance
 *   benefit has risen — extraction climbs from 0.35 to 0.61 across the
 *   measured interval, and the enforcement needed to keep affective
 *   re-enactment mandatory for children rises alongside it. Sibling readings
 *   (symbol_continuity, survival_competence, boundary_maintenance) describe
 *   the same ritual calendar through different structural lenses and are not
 *   part of this constraint's classification — see network links.
 *
 * KEY AGENTS:
 *   - elder_ritual_authorities: agenda-setter and beneficiary (institutional/identity_locked) — administers the liturgical calendar and derives standing from it
 *   - descendant_generations: primary payer (powerless/identity_locked) — inherits affective burden before independent threat-assessment capacity exists
 *   - children_socialized_into_mourning_practice: most acute payer (powerless/trapped) — inducted pre-consent
 *   - communal_threat_vigilance_function: non-agent beneficiary — the collective disposition the ritual sustains
 *   - trauma_and_memory_researchers: analytical observer documenting both benefit and cost
 *   - reform_minded_community_members: excluded voice — argument for decoupling education from re-enactment has no forum
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_kernel__trauma_encoding_reading, 0.61).
domain_priors:suppression_score(catastrophe_memory_kernel__trauma_encoding_reading, 0.52).
domain_priors:theater_ratio(catastrophe_memory_kernel__trauma_encoding_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_kernel__trauma_encoding_reading, extractiveness, 0.61).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__trauma_encoding_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__trauma_encoding_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_kernel__trauma_encoding_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__trauma_encoding_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_kernel__trauma_encoding_reading, tangled_rope).
narrative_ontology:human_readable(catastrophe_memory_kernel__trauma_encoding_reading, "Catastrophe-Memory Ritual as Intergenerational Trauma-Encoding Warning System").
narrative_ontology:topic_domain(catastrophe_memory_kernel__trauma_encoding_reading, "religious_studies/collective_memory/ritual_practice").

domain_priors:requires_active_enforcement(catastrophe_memory_kernel__trauma_encoding_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_kernel__trauma_encoding_reading, 'e85775bd-e75c-4e46-b24e-9985b43a8e90').
narrative_ontology:cs_kernel_codification('e85775bd-e75c-4e46-b24e-9985b43a8e90', distributed).
narrative_ontology:cs_authority_grounding('e85775bd-e75c-4e46-b24e-9985b43a8e90', practice).
narrative_ontology:cs_interpretation_layer_present('e85775bd-e75c-4e46-b24e-9985b43a8e90').
narrative_ontology:cs_reading_relation('e85775bd-e75c-4e46-b24e-9985b43a8e90', catastrophe_memory_kernel__symbol_continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('e85775bd-e75c-4e46-b24e-9985b43a8e90', catastrophe_memory_kernel__survival_competence_reading, influences).
narrative_ontology:cs_reading_relation('e85775bd-e75c-4e46-b24e-9985b43a8e90', catastrophe_memory_kernel__boundary_maintenance_reading, coexists_with).
narrative_ontology:cs_axiom('e85775bd-e75c-4e46-b24e-9985b43a8e90', foundational, affective_transmission_is_necessary_for_vigilance).
narrative_ontology:cs_axiom_status(affective_transmission_is_necessary_for_vigilance, holdable).
narrative_ontology:cs_axiom_grounding('e85775bd-e75c-4e46-b24e-9985b43a8e90', affective_transmission_is_necessary_for_vigilance, empirically_contingent).
narrative_ontology:cs_axiom('e85775bd-e75c-4e46-b24e-9985b43a8e90', secondary, descendant_consent_is_subordinate_to_collective_survival_need).
narrative_ontology:cs_axiom_status(descendant_consent_is_subordinate_to_collective_survival_need, holdable).
narrative_ontology:cs_axiom_grounding('e85775bd-e75c-4e46-b24e-9985b43a8e90', descendant_consent_is_subordinate_to_collective_survival_need, instrumental).
narrative_ontology:cs_reference_frame('e85775bd-e75c-4e46-b24e-9985b43a8e90', founding_generation_survivor_witness).
narrative_ontology:cs_drift_state('e85775bd-e75c-4e46-b24e-9985b43a8e90', contemporary_diaspora_institutional_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('e85775bd-e75c-4e46-b24e-9985b43a8e90', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_kernel__trauma_encoding_reading, catastrophe_memory_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_kernel__trauma_encoding_reading, communal_threat_vigilance_function).
narrative_ontology:constraint_beneficiary(catastrophe_memory_kernel__trauma_encoding_reading, elder_ritual_authorities).
narrative_ontology:constraint_victim(catastrophe_memory_kernel__trauma_encoding_reading, descendant_generations).
narrative_ontology:constraint_victim(catastrophe_memory_kernel__trauma_encoding_reading, children_socialized_into_mourning_practice).
narrative_ontology:constraint_vindicates(catastrophe_memory_kernel__trauma_encoding_reading, collective_threat_memory_has_survival_value).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design, sequence, and enforce the mourning-liturgy calendar: which catastrophes are named, in what order, with what affective intensity. Their communal standing and interpretive authority derive from being the custodians of the memory. They cannot exit the role without dissolving the authority structure that makes them who they are within the community.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__trauma_encoding_reading, elder_ritual_authorities, agenda_setter,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_kernel__trauma_encoding_reading, elder_ritual_authorities, beneficiary).

% The collective's early-warning posture toward recurrence of persecution is sharpened by ritually rehearsed catastrophe-memory; the community as a whole is more alert to precursor signs of danger because the trauma has been kept affectively live rather than allowed to fade into abstract history.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__trauma_encoding_reading, communal_threat_vigilance_function, beneficiary,
    institutional, civilizational, analytical, national).
narrative_ontology:stakeholder_non_agent(catastrophe_memory_kernel__trauma_encoding_reading, communal_threat_vigilance_function).

% Are inducted into the mourning practice before they have any independent basis for assessing present-day threat levels; they inherit the emotional weight, the fasting, the recitation of atrocity, and the hypervigilance as felt reality, not as historical report. Leaving the practice risks being read as severing themselves from the community and from the dead who are being mourned.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__trauma_encoding_reading, descendant_generations, payer,
    powerless, biographical, identity_locked, national).

% Are the specific age-cohort undergoing first exposure to the catastrophe narratives and mourning rites at a developmental stage where they cannot yet distinguish inherited threat-signal from present danger; the practice is transmitted to them before consent is a meaningful category.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__trauma_encoding_reading, children_socialized_into_mourning_practice, payer,
    powerless, biographical, trapped, local).

% Study epigenetic and psychosocial transmission of catastrophe-memory across generations, documenting both the vigilance benefits and the anxiety, hypervigilance disorders, and identity-foreclosure costs the practice imposes on those born into it.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__trauma_encoding_reading, trauma_and_memory_researchers, observer,
    analytical, generational, analytical, global).

% Would argue for lighter-touch memorial forms that preserve historical knowledge without full affective re-enactment of trauma in children, but raising this inside the liturgical calendar reads as disrespect to victims of the original catastrophe, so the argument rarely reaches a decision-making forum.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__trauma_encoding_reading, reform_minded_community_members, excluded,
    moderate, biographical, constrained, local).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_memory_kernel__trauma_encoding_reading, diffuse).
narrative_ontology:fixing_cost_class(catastrophe_memory_kernel__trauma_encoding_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The ritual keeps a real historical threat pattern affectively salient across generations so the community does not become complacent about warning signs that preceded past catastrophes.
% TRANSFER_FUNCTION: Moves psychological burden — anxiety, hypervigilance, grief inherited at second hand — from the generation that experienced the catastrophe to descendants who did not, in exchange for a collectively held early-warning disposition.
% ABSENT_VOICES: Reform-minded community members and clinicians who treat intergenerational anxiety in the descendant cohort would argue for decoupling historical education from affective re-enactment in children, but this argument is structurally excluded from the liturgical calendar-setting process because raising it is read as insufficient reverence for the dead.
% DISAPPEARANCE_RATIONALE: Elder ritual authorities and much of the older generation would say the world rearranges catastrophically — vigilance erodes and the community becomes vulnerable to recurrence. Descendant-generation critics and researchers argue the world would mostly stay the same in terms of actual threat detection (which now runs through secular institutions, media, and diaspora networks) while removing a significant source of transmitted anxiety; the two camps do not agree on which claim is true.
% FOUNDING_PROBLEM: A catastrophic persecution event left survivors needing both to process grief and to ensure their community would recognize and respond faster to the next warning signs, rather than being caught unprepared again.
% FOUNDING_PROBLEM_CORROBORATION: Historians of the originating community and clinical researchers outside the ritual authority structure attest that the acute early-warning function has been substantially superseded by modern institutional monitoring (diaspora networks, human-rights organizations, state protections), while elder authorities and the liturgical tradition itself maintain the founding threat remains live and unresolved.
narrative_ontology:disappearance_verdict(catastrophe_memory_kernel__trauma_encoding_reading, contested).
narrative_ontology:founding_problem_status(catastrophe_memory_kernel__trauma_encoding_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_kernel__trauma_encoding_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(catastrophe_memory_kernel__trauma_encoding_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_kernel__trauma_encoding_reading, 0.61, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_kernel__trauma_encoding_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(catastrophe_memory_kernel__trauma_encoding_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(catastrophe_memory_kernel__trauma_encoding_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored moderate-to-high and rising because the trauma-transmission function imposes a real, measurable psychological cost (anxiety, hypervigilance, identity-foreclosure documented in the trauma-memory research literature) on a population — descendants and especially children — who did not choose the exposure and cannot easily decline it without being read as betraying the dead. Suppression is moderate: it is less about coercive enforcement machinery and more about the identity-lock that makes exit legible only as communal betrayal, which the metric captures as real structural suppression even though no formal punishment apparatus exists. Theater ratio is modest but rising, reflecting a growing share of ritual observance that performs vigilance for cohesion's sake even where the underlying threat model has weakened.
 *
 * PERSPECTIVAL GAP:
 *   From the elder-authority seat, the ritual is coordination that has always worked and continues to protect the community. From the descendant-payer seat, especially the child cohort, the same structure computes as extraction: a cost imposed without consent for a benefit whose marginal value to their own safety is increasingly uncertain relative to modern institutional alternatives. The engine's per-seat computation is expected to diverge along exactly this line; the claimed_type (tangled_rope) is chosen to hold both readings as structurally real rather than picking one.
 *
 * DIRECTIONALITY LOGIC:
 *   Elder ritual authorities sit near the beneficiary end: they administer the calendar, their authority is constituted by their custodianship of the memory, and they are shielded from most of the burden by having lived close to (or descended proximately from) the original event with corroborating personal or family narrative. The communal_threat_vigilance_function is a non-agent beneficiary — a collective disposition, not a rent-collecting actor, which is why it is marked agent:false and excluded from directionality math. Descendant generations and especially children carry the highest d — they receive the affective cost with the least prior basis for evaluating it and the least exit latitude, since the exit itself would be read as a further identity rupture.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — inadequate collective threat-detection before a specific catastrophe — is contested as still-live: elder authorities maintain the threat model is unchanged, while researchers and reform-minded members outside that authority structure argue institutional alternatives have substantially absorbed the early-warning function. This is exactly the divergence the tangled_rope classification exists to hold open rather than resolve by fiat: the coordination function (vigilance transmission) is real and was real at founding, and the extraction (trauma imposed on non-consenting descendants) is also real and independently verifiable, and neither fact cancels the other.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    vigilance_benefit_vs_burden_magnitude,
    'Does the marginal threat-vigilance benefit conferred by ritual trauma-transmission still exceed the psychological burden imposed on descendants, given that modern institutional threat-monitoring (diaspora networks, human rights bodies, state protection) did not exist at founding but exists now?',
    'Longitudinal clinical and epidemiological study comparing threat-detection accuracy and psychological outcomes between communities that maintain high-intensity affective re-enactment versus communities that shifted to lower-intensity historical education, controlling for actual exposure to recurrent threat.',
    'If institutional alternatives have substantially substituted for ritual-transmitted vigilance, the coordination function has weakened while the extraction (transmitted trauma) has not, pushing the classification toward snare; if the vigilance function remains irreplaceable, the tangled_rope classification with genuine dual function is better supported.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(vigilance_benefit_vs_burden_magnitude, empirical, 'Whether the trauma-encoding function''s coordination value still justifies its burden given modern institutional substitutes.').

omega_variable(
    consent_and_developmental_timing,
    'Is the induction of children into full affective re-enactment (as opposed to historical/educational transmission without re-enactment) a necessary feature of effective threat-vigilance transmission, or a separable intensity choice made by ritual authorities?',
    'Comparative study across communities/denominations within the same broad tradition that vary re-enactment intensity for children, holding narrative content constant, and measuring both vigilance outcomes and psychological cost in adulthood.',
    'If separable, the specific extraction from children is unnecessary to the coordination function and the practice as currently administered is more extractive than the coordination story requires; if inseparable, the burden on children is closer to an unavoidable cost of the coordination function itself.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(consent_and_developmental_timing, conceptual, 'Whether child-directed affective intensity is severable from the vigilance-transmission function.').

omega_variable(
    kernel_reading_boundary_ambiguity,
    'Is trauma-encoding a structurally distinct function from symbol-continuity, survival-competence, and boundary-maintenance, or are these four readings inseparable aspects of a single indivisible ritual act such that decomposing them into separate constraints is itself an analytical artifact?',
    'Cross-reading structural audit: for each candidate decomposition, test whether removing one function (e.g. affective trauma re-enactment) while preserving the others (symbol continuity, boundary maintenance, competence transmission) is empirically observed in any community''s ritual reform, which would evidence real separability.',
    'If the functions cannot be separated even in principle, single-reading ε-invariant treatment may understate a joint extraction/coordination bundle; the ε-invariance principle''s decomposition move still applies per reading, but the interpretive stakes of the decomposition itself should be flagged rather than assumed settled.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_boundary_ambiguity, conceptual, 'Whether the four kernel readings name genuinely separable functions or an artifact of analytical decomposition.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_kernel__trauma_encoding_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_kernel__trauma_encoding_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(cata_tr_t20, catastrophe_memory_kernel__trauma_encoding_reading, theater_ratio, 20, 0.14).
narrative_ontology:measurement(cata_tr_t40, catastrophe_memory_kernel__trauma_encoding_reading, theater_ratio, 40, 0.18).
narrative_ontology:measurement(cata_tr_t60, catastrophe_memory_kernel__trauma_encoding_reading, theater_ratio, 60, 0.22).
narrative_ontology:measurement(cata_tr_t80, catastrophe_memory_kernel__trauma_encoding_reading, theater_ratio, 80, 0.25).
narrative_ontology:measurement(cata_tr_t100, catastrophe_memory_kernel__trauma_encoding_reading, theater_ratio, 100, 0.28).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_kernel__trauma_encoding_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(cata_be_t20, catastrophe_memory_kernel__trauma_encoding_reading, base_extractiveness, 20, 0.42).
narrative_ontology:measurement(cata_be_t40, catastrophe_memory_kernel__trauma_encoding_reading, base_extractiveness, 40, 0.5).
narrative_ontology:measurement(cata_be_t60, catastrophe_memory_kernel__trauma_encoding_reading, base_extractiveness, 60, 0.55).
narrative_ontology:measurement(cata_be_t80, catastrophe_memory_kernel__trauma_encoding_reading, base_extractiveness, 80, 0.58).
narrative_ontology:measurement(cata_be_t100, catastrophe_memory_kernel__trauma_encoding_reading, base_extractiveness, 100, 0.61).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_memory_kernel__trauma_encoding_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(cata_su_t20, catastrophe_memory_kernel__trauma_encoding_reading, suppression_requirement, 20, 0.36).
narrative_ontology:measurement(cata_su_t40, catastrophe_memory_kernel__trauma_encoding_reading, suppression_requirement, 40, 0.42).
narrative_ontology:measurement(cata_su_t60, catastrophe_memory_kernel__trauma_encoding_reading, suppression_requirement, 60, 0.46).
narrative_ontology:measurement(cata_su_t80, catastrophe_memory_kernel__trauma_encoding_reading, suppression_requirement, 80, 0.49).
narrative_ontology:measurement(cata_su_t100, catastrophe_memory_kernel__trauma_encoding_reading, suppression_requirement, 100, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_kernel__trauma_encoding_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(catastrophe_memory_kernel__trauma_encoding_reading, 0.1).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__trauma_encoding_reading, catastrophe_memory_kernel__symbol_continuity_reading).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__trauma_encoding_reading, catastrophe_memory_kernel__survival_competence_reading).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__trauma_encoding_reading, catastrophe_memory_kernel__boundary_maintenance_reading).

% DUAL FORMULATION NOTE:
% These four constraints are sibling readings of a single kernel (catastrophe_memory_kernel): the same liturgical mourning-practice, read through four distinct structural lenses — symbol continuity, survival competence, boundary maintenance, and trauma encoding (this story). Each reading has its own ε, its own beneficiary/victim structure, and its own claimed_type; none is a measurement-basis variant of another. This reading (trauma_encoding) is the most extractive of the four because it isolates the cost imposed specifically on non-consenting descendants, a cost the other readings either do not track or track as incidental to their own coordination story.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
