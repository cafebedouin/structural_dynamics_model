% ============================================================================
% CONSTRAINT STORY: plural_marriage_mandate__endogenous_reinterpretation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_plural_marriage_mandate__endogenous_reinterpretation_reading, []).

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
 *   constraint_id: plural_marriage_mandate__endogenous_reinterpretation_reading
 *   human_readable: 1890 Manifesto: Endogenous Prophetic Reinterpretation
 *   domain: religious/institutional_history/political_theology
 *
 * SUMMARY:
 *   This constraint story instantiates the
 *   'endogenous_reinterpretation_reading' of the plural marriage mandate
 *   kernel. It describes the 1890 Manifesto as a legitimate prophetic
 *   reinterpretation, where God revealed the temporal suspension of plural
 *   marriage to preserve the church's salvific mission. This reading frames
 *   the change as a divinely guided adaptation necessary for the church's
 *   survival and continued growth, rather than a capitulation to external
 *   pressure or a purely pragmatic institutional move. The claimed type is
 *   'rope' from the church's perspective, reflecting coordination around a
 *   new prophetic directive, but the metrics reflect the substantial
 *   extraction and suppression experienced by those who did not accept the
 *   reinterpretation.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(plural_marriage_mandate__endogenous_reinterpretation_reading, 0.65).
domain_priors:suppression_score(plural_marriage_mandate__endogenous_reinterpretation_reading, 0.8).
domain_priors:theater_ratio(plural_marriage_mandate__endogenous_reinterpretation_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(plural_marriage_mandate__endogenous_reinterpretation_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(plural_marriage_mandate__endogenous_reinterpretation_reading, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(plural_marriage_mandate__endogenous_reinterpretation_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(plural_marriage_mandate__endogenous_reinterpretation_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(plural_marriage_mandate__endogenous_reinterpretation_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(plural_marriage_mandate__endogenous_reinterpretation_reading, rope).
narrative_ontology:human_readable(plural_marriage_mandate__endogenous_reinterpretation_reading, "1890 Manifesto: Endogenous Prophetic Reinterpretation").
narrative_ontology:topic_domain(plural_marriage_mandate__endogenous_reinterpretation_reading, "religious/institutional_history/political_theology").

domain_priors:requires_active_enforcement(plural_marriage_mandate__endogenous_reinterpretation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(plural_marriage_mandate__endogenous_reinterpretation_reading, '822a2807-ec4c-4d22-b0ae-5a5ae309c130').
narrative_ontology:cs_kernel_codification('822a2807-ec4c-4d22-b0ae-5a5ae309c130', fixed_text).
narrative_ontology:cs_authority_grounding('822a2807-ec4c-4d22-b0ae-5a5ae309c130', lineage).
narrative_ontology:cs_interpretation_layer_present('822a2807-ec4c-4d22-b0ae-5a5ae309c130').
narrative_ontology:cs_reading_relation('822a2807-ec4c-4d22-b0ae-5a5ae309c130', plural_marriage_mandate__exogenous_override_reading, coexists_with).
narrative_ontology:cs_reading_relation('822a2807-ec4c-4d22-b0ae-5a5ae309c130', plural_marriage_mandate__institutional_pragmatism_reading, coexists_with).
narrative_ontology:cs_axiom('822a2807-ec4c-4d22-b0ae-5a5ae309c130', foundational, prophetic_revelation_supremacy).
narrative_ontology:cs_axiom_status(prophetic_revelation_supremacy, holdable).
narrative_ontology:cs_axiom_grounding('822a2807-ec4c-4d22-b0ae-5a5ae309c130', prophetic_revelation_supremacy, theological).
narrative_ontology:cs_axiom('822a2807-ec4c-4d22-b0ae-5a5ae309c130', foundational, salvific_mission_preservation).
narrative_ontology:cs_axiom_status(salvific_mission_preservation, holdable).
narrative_ontology:cs_axiom_grounding('822a2807-ec4c-4d22-b0ae-5a5ae309c130', salvific_mission_preservation, deontological).
narrative_ontology:cs_reference_frame('822a2807-ec4c-4d22-b0ae-5a5ae309c130', divine_law_adaptable_to_circumstance).
narrative_ontology:cs_drift_state('822a2807-ec4c-4d22-b0ae-5a5ae309c130', contemporary_church_practice, gap(stable, minor, true)).
narrative_ontology:cs_created_at('822a2807-ec4c-4d22-b0ae-5a5ae309c130', '').
narrative_ontology:cs_kernel_id(plural_marriage_mandate__endogenous_reinterpretation_reading, plural_marriage_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(plural_marriage_mandate__endogenous_reinterpretation_reading, the_church_of_jesus_christ_of_latter_day_saints).
narrative_ontology:constraint_beneficiary(plural_marriage_mandate__endogenous_reinterpretation_reading, mainstream_members).
narrative_ontology:constraint_victim(plural_marriage_mandate__endogenous_reinterpretation_reading, fundamentalist_polygamists).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The institutional body that issued and enforces the 1890 Manifesto, preserving its legal standing, property, and global missionary efforts by suspending plural marriage. It frames this as a divinely guided adaptation.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__endogenous_reinterpretation_reading, the_church_of_jesus_christ_of_latter_day_saints, agenda_setter,
    institutional, generational, arbitrage, global).

% The specific leaders (President, Apostles) who are believed to receive and interpret revelation, guiding the church's doctrine and practice. Their identity and authority are fused with their role as prophets, making adherence to the reinterpretation central to their function.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__endogenous_reinterpretation_reading, general_authorities, agenda_setter,
    institutional, biographical, identity_locked, global).

% Adhere to the reinterpretation, gaining access to temple ordinances, full church fellowship, and avoiding legal conflict. Their social ties, belief system, and desire for salvation constrain their exit options from the church.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__endogenous_reinterpretation_reading, mainstream_members, beneficiary,
    organized, biographical, constrained, global).

% Excommunicated or disfellowshipped for continuing plural marriage, losing access to mainstream church blessings and community. Their identity is often deeply tied to the original practice, making exit from their belief system or community extremely difficult.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__endogenous_reinterpretation_reading, fundamentalist_polygamists, payer,
    powerless, biographical, trapped, local).

% The external power that exerted pressure leading to the Manifesto, now observing the church's compliance with anti-polygamy laws. Its actions created the context for the reinterpretation.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__endogenous_reinterpretation_reading, federal_government, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(plural_marriage_mandate__endogenous_reinterpretation_reading, the_church_of_jesus_christ_of_latter_day_saints).
narrative_ontology:fixing_cost_class(plural_marriage_mandate__endogenous_reinterpretation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To coordinate the church's practice with federal law and societal norms, while preserving its theological claims of divine guidance and its salvific mission, ensuring institutional survival and global expansion.
% TRANSFER_FUNCTION: Transfers the burden of legal non-compliance and social ostracization from the church institution and its mainstream members to individual fundamentalists who continue the practice, through excommunication and disfellowshipment.
% ABSENT_VOICES: Early church leaders who established plural marriage as a divine command, and those who maintained it after the Manifesto, are now institutionally silenced or marginalized. They would argue for the eternal nature of the original command.
% DISAPPEARANCE_RATIONALE: If the reinterpretation and its enforcement vanished overnight, the church would face immediate and severe legal challenges, loss of tax-exempt status, internal schism, and public condemnation, fundamentally reorganizing its structure and mission.
% FOUNDING_PROBLEM: The existential threat to the church's legal status, property, and leadership due to federal anti-polygamy laws, which jeopardized its ability to function as a religious institution.
% FOUNDING_PROBLEM_CORROBORATION: Historical records, federal court decisions, and contemporary church statements corroborate the existential threat posed by anti-polygamy laws. Independent historians and sociologists also attest to the severe political and legal pressure faced by the church at the time.
narrative_ontology:disappearance_verdict(plural_marriage_mandate__endogenous_reinterpretation_reading, world_rearranges).
narrative_ontology:founding_problem_status(plural_marriage_mandate__endogenous_reinterpretation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(plural_marriage_mandate__endogenous_reinterpretation_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(plural_marriage_mandate__endogenous_reinterpretation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(plural_marriage_mandate__endogenous_reinterpretation_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(plural_marriage_mandate__endogenous_reinterpretation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(plural_marriage_mandate__endogenous_reinterpretation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(plural_marriage_mandate__endogenous_reinterpretation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base extractiveness (0.65) is moderately high, reflecting the severe costs borne by fundamentalist polygamists (excommunication, loss of community) balanced against the institutional benefits (legal standing, global mission). Suppression (0.80) is high due to the active enforcement mechanisms (ecclesiastical courts, disfellowshipment) used to ensure compliance with the new directive. Theater ratio (0.20) is low, as the reinterpretation is presented as a genuine theological event, though some performative aspects of reiterating its divine origin exist. Accessibility collapse (0.75) is high for those who wish to continue plural marriage within the mainstream church, as no such path exists. Resistance (0.55) is moderate, primarily from fundamentalist groups who continue the practice outside the mainstream church. The temporal measurements show an initial period of adjustment, followed by a hardening of enforcement and stabilization of extractiveness as the new norm became entrenched.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the church and its mainstream members, the Manifesto represents a necessary and divinely sanctioned adaptation, a 'rope' that coordinated the community around a new path. From the perspective of fundamentalist polygamists, it is a 'snare' that extracted their core identity and practice through coercive institutional power, leading to their marginalization and excommunication. The engine's computation of per-seat classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The Church of Jesus Christ of Latter-day Saints and its General Authorities are the primary beneficiaries and agenda-setters, gaining legal and social legitimacy, and preserving their institutional structure. Mainstream members are also beneficiaries, gaining access to temple ordinances and full fellowship. Fundamentalist polygamists are the primary targets and payers, bearing the costs of excommunication and social ostracization. The federal government acts as an external observer whose coercive power initially shaped the context for the reinterpretation.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mislabeling by acknowledging the genuine coordination problem (institutional survival) while also recognizing the extractive consequences for a segment of the population. It avoids framing the reinterpretation as pure extraction by emphasizing the theological justification and the perceived necessity for the church's salvific mission, which is a core coordination function for its adherents. The 'live' status of the founding problem (institutional survival) further supports the ongoing coordination function, even as extraction persists.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    divine_will_vs_pragmatic_necessity,
    'Was the 1890 Manifesto primarily a genuine divine revelation, or a pragmatic adaptation to existential external pressure from the federal government?',
    'Analysis of internal church records, prophetic statements, and external historical accounts for consistency and causal sequencing. Examination of alternative theological justifications considered at the time.',
    'If primarily pragmatic, the ''rope'' classification would shift towards ''tangled_rope'' or ''snare'' for the institution, as the coordination story would be seen as cover for survival-driven extraction. If genuinely divine, the ''rope'' classification is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(divine_will_vs_pragmatic_necessity, conceptual, 'Ambiguity of divine vs. pragmatic motivation for the reinterpretation.').

omega_variable(
    doctrinal_integrity_vs_institutional_survival,
    'Does the reinterpretation of plural marriage compromise core, eternal doctrine for institutional survival, or is it a legitimate evolution of doctrine consistent with prior theological principles?',
    'Comparative theological analysis of historical and contemporary church doctrine, examining the concept of ''eternal'' vs. ''temporal'' commands within the church''s theological framework. Analysis of how the reinterpretation is taught and understood by different generations of members.',
    'If it compromises core doctrine, the constraint''s legitimacy is weakened, potentially shifting its classification towards ''snare'' for those who perceive a betrayal of foundational beliefs. If consistent, the ''rope'' classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(doctrinal_integrity_vs_institutional_survival, conceptual, 'Tension between long-term doctrinal integrity and short-term institutional survival.').

omega_variable(
    internalized_vs_structural_suppression,
    'Is the suppression of plural marriage within the mainstream church primarily due to internalized belief in the new revelation, or active institutional enforcement (e.g., excommunication, social pressure)?',
    'Sociological studies of member attitudes and behaviors, examining the role of personal conviction versus fear of institutional sanction. Analysis of excommunication rates and reasons over time.',
    'If primarily internalized, the effective suppression is lower, as members self-regulate. If primarily structural, the effective suppression is higher, indicating ongoing coercive power. This would modulate the ''suppression'' metric''s impact on per-seat classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(internalized_vs_structural_suppression, empirical, 'Ambiguity of suppression mechanism: internalized belief vs. institutional enforcement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(plural_marriage_mandate__endogenous_reinterpretation_reading, 1890, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(plur_tr_t1890, plural_marriage_mandate__endogenous_reinterpretation_reading, theater_ratio, 1890, 0.1).
narrative_ontology:measurement(plur_tr_t1920, plural_marriage_mandate__endogenous_reinterpretation_reading, theater_ratio, 1920, 0.15).
narrative_ontology:measurement(plur_tr_t1950, plural_marriage_mandate__endogenous_reinterpretation_reading, theater_ratio, 1950, 0.18).
narrative_ontology:measurement(plur_tr_t1980, plural_marriage_mandate__endogenous_reinterpretation_reading, theater_ratio, 1980, 0.2).
narrative_ontology:measurement(plur_tr_t2020, plural_marriage_mandate__endogenous_reinterpretation_reading, theater_ratio, 2020, 0.2).

% Extraction over time
narrative_ontology:measurement(plur_be_t1890, plural_marriage_mandate__endogenous_reinterpretation_reading, base_extractiveness, 1890, 0.5).
narrative_ontology:measurement(plur_be_t1920, plural_marriage_mandate__endogenous_reinterpretation_reading, base_extractiveness, 1920, 0.6).
narrative_ontology:measurement(plur_be_t1950, plural_marriage_mandate__endogenous_reinterpretation_reading, base_extractiveness, 1950, 0.65).
narrative_ontology:measurement(plur_be_t1980, plural_marriage_mandate__endogenous_reinterpretation_reading, base_extractiveness, 1980, 0.68).
narrative_ontology:measurement(plur_be_t2020, plural_marriage_mandate__endogenous_reinterpretation_reading, base_extractiveness, 2020, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(plur_su_t1890, plural_marriage_mandate__endogenous_reinterpretation_reading, suppression_requirement, 1890, 0.7).
narrative_ontology:measurement(plur_su_t1920, plural_marriage_mandate__endogenous_reinterpretation_reading, suppression_requirement, 1920, 0.75).
narrative_ontology:measurement(plur_su_t1950, plural_marriage_mandate__endogenous_reinterpretation_reading, suppression_requirement, 1950, 0.8).
narrative_ontology:measurement(plur_su_t1980, plural_marriage_mandate__endogenous_reinterpretation_reading, suppression_requirement, 1980, 0.82).
narrative_ontology:measurement(plur_su_t2020, plural_marriage_mandate__endogenous_reinterpretation_reading, suppression_requirement, 2020, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(plural_marriage_mandate__endogenous_reinterpretation_reading, identity_coordination).
narrative_ontology:affects_constraint(plural_marriage_mandate__endogenous_reinterpretation_reading, plural_marriage_mandate__exogenous_override_reading).
narrative_ontology:affects_constraint(plural_marriage_mandate__endogenous_reinterpretation_reading, plural_marriage_mandate__institutional_pragmatism_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'plural_marriage_mandate' kernel, focusing on the endogenous prophetic reinterpretation. Sibling readings explore federal coercion and institutional pragmatism.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
