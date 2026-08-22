% ============================================================================
% CONSTRAINT STORY: eternal_marriage_covenant__prophetic_override_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-22
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_eternal_marriage_covenant__prophetic_override_reading, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: eternal_marriage_covenant__prophetic_override_reading
 *   human_readable: Continuing Revelation Override of Eternal Marriage Covenant
 *   domain: religious_law/political_theology
 *
 * SUMMARY:
 *   The eternal marriage covenant (D&C 132) establishes plural marriage as a
 *   requirement for exaltation. The prophetic override reading holds that
 *   continuing revelation doctrine authorizes the living prophet to suspend
 *   or supersede prior revelation when circumstances require — specifically,
 *   when federal pressure threatens the church's institutional survival. The
 *   1890 Manifesto (Official Declaration 1) and 1904 Second Manifesto
 *   instantiate this reading: the practice is suspended, the doctrine is
 *   retained, and prophetic authority is vindicated as adaptive rather than
 *   immutable. This reading differs from the immutable commandment reading
 *   (which treats D&C 132 as irrevocable) and the temporal accommodation
 *   reading (which treats the Manifesto as a mere pause pending future
 *   restoration). Here, the override is doctrinally authoritative: the new
 *   revelation genuinely supersedes the prior one.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(eternal_marriage_covenant__prophetic_override_reading, 0.68).
domain_priors:suppression_score(eternal_marriage_covenant__prophetic_override_reading, 0.42).
domain_priors:theater_ratio(eternal_marriage_covenant__prophetic_override_reading, 0.31).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(eternal_marriage_covenant__prophetic_override_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(eternal_marriage_covenant__prophetic_override_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(eternal_marriage_covenant__prophetic_override_reading, theater_ratio, 0.31).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(eternal_marriage_covenant__prophetic_override_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(eternal_marriage_covenant__prophetic_override_reading, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(eternal_marriage_covenant__prophetic_override_reading, tangled_rope).
narrative_ontology:human_readable(eternal_marriage_covenant__prophetic_override_reading, "Continuing Revelation Override of Eternal Marriage Covenant").
narrative_ontology:topic_domain(eternal_marriage_covenant__prophetic_override_reading, "religious_law/political_theology").

domain_priors:requires_active_enforcement(eternal_marriage_covenant__prophetic_override_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(eternal_marriage_covenant__prophetic_override_reading, '8e9ed716-8f49-4d02-9798-43248f8d9874').
narrative_ontology:cs_kernel_codification('8e9ed716-8f49-4d02-9798-43248f8d9874', formalized).
narrative_ontology:cs_authority_grounding('8e9ed716-8f49-4d02-9798-43248f8d9874', lineage).
narrative_ontology:cs_interpretation_layer_present('8e9ed716-8f49-4d02-9798-43248f8d9874').
narrative_ontology:cs_reading_relation('8e9ed716-8f49-4d02-9798-43248f8d9874', eternal_marriage_covenant__immutable_commandment_reading, forecloses).
narrative_ontology:cs_reading_relation('8e9ed716-8f49-4d02-9798-43248f8d9874', eternal_marriage_covenant__temporal_accommodation_reading, influences).
narrative_ontology:cs_axiom('8e9ed716-8f49-4d02-9798-43248f8d9874', foundational, living_prophet_can_supersede_prior_revelation).
narrative_ontology:cs_axiom_status(living_prophet_can_supersede_prior_revelation, holdable).
narrative_ontology:cs_axiom_grounding('8e9ed716-8f49-4d02-9798-43248f8d9874', living_prophet_can_supersede_prior_revelation, deontological).
narrative_ontology:cs_axiom('8e9ed716-8f49-4d02-9798-43248f8d9874', secondary, institutional_survival_justifies_doctrinal_adaptation).
narrative_ontology:cs_axiom_status(institutional_survival_justifies_doctrinal_adaptation, holdable).
narrative_ontology:cs_axiom_grounding('8e9ed716-8f49-4d02-9798-43248f8d9874', institutional_survival_justifies_doctrinal_adaptation, instrumental).
narrative_ontology:cs_reference_frame('8e9ed716-8f49-4d02-9798-43248f8d9874', prophetic_continuity_framework).
narrative_ontology:cs_drift_state('8e9ed716-8f49-4d02-9798-43248f8d9874', post_manifesto_era, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('8e9ed716-8f49-4d02-9798-43248f8d9874', '').
narrative_ontology:cs_kernel_id(eternal_marriage_covenant__prophetic_override_reading, eternal_marriage_covenant).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(eternal_marriage_covenant__prophetic_override_reading, institutional_leadership).
narrative_ontology:constraint_beneficiary(eternal_marriage_covenant__prophetic_override_reading, mainstream_membership).
narrative_ontology:constraint_victim(eternal_marriage_covenant__prophetic_override_reading, fundamentalist_dissidents).
narrative_ontology:constraint_victim(eternal_marriage_covenant__prophetic_override_reading, polygamous_families).
narrative_ontology:constraint_vindicates(eternal_marriage_covenant__prophetic_override_reading, continuing_revelation_doctrine).
narrative_ontology:constraint_vindicates(eternal_marriage_covenant__prophetic_override_reading, living_prophet_authority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds prophetic authority to receive and declare new revelation. Uses continuing revelation doctrine to suspend polygamous practice when federal pressure threatens institutional survival (property confiscation, disincorporation, imprisonment of leadership). Retains doctrinal authority while adapting practice; collects institutional legitimacy and physical survival.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__prophetic_override_reading, institutional_leadership, agenda_setter,
    institutional, generational, arbitrage, global).

% Avoids federal prosecution, property loss, and social marginalization by complying with the 1890 Manifesto and subsequent revelations. Gains religious legitimacy within mainstream society and state recognition. Exit requires leaving the faith community and its social world.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__prophetic_override_reading, mainstream_membership, beneficiary,
    organized, biographical, constrained, national).

% View the Manifesto as a capitulation violating eternal covenant. Bear costs of schism: excommunication, loss of temple access, social ostracism from mainstream community, legal persecution continuing into 20th century. Identity fused to the eternal-marriage covenant; exit means abandoning the theological self.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__prophetic_override_reading, fundamentalist_dissidents, payer,
    powerless, biographical, identity_locked, local).

% Families formed under the prior revelation face legal dissolution, prosecution of husbands, bastardization of children, loss of inheritance rights. Their marital structure is retroactively delegitimized by the override. No viable exit: they cannot undo existing sealings, and continuing the practice invites state violence.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__prophetic_override_reading, polygamous_families, payer,
    powerless, biographical, trapped, local).
narrative_ontology:stakeholder_secondary_role(eternal_marriage_covenant__prophetic_override_reading, polygamous_families, excluded).

% Applied legal, military, and economic pressure (Edmunds Act, Edmunds-Tucker Act, disincorporation, property seizure) to force abandonment of polygamy. Their pressure is the external constraint that activates the prophetic override mechanism. They would object to any reading that treats the surrender as purely internal doctrinal development.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__prophetic_override_reading, federal_authorities, excluded,
    institutional, generational, analytical, national).

% Analyze the constraint as a case study in religious accommodation to state power, revelation mechanics, and authority maintenance. No material stake in the outcome; sees the full structural field across all seats.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__prophetic_override_reading, theological_observers, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves institutional continuity and member safety by allowing authorized adaptation of practice when external force makes the prior arrangement existentially threatening. Solves the coordination problem: how does a covenant community survive when the state criminalizes its defining practice?
% TRANSFER_FUNCTION: Transfers the cost of federal pressure from the institutional center (which would face destruction) onto fundamentalist dissidents and polygamous families (who bear schism, prosecution, and delegitimization). The institution keeps its legal existence and mainstream legitimacy; the dissidents pay the price of fidelity to the prior revelation.
% ABSENT_VOICES: Polygamous women and children in existing families — their experience of the Manifesto's aftermath (abandonment, poverty, legal bastardization) was not represented in the revelation or the sustaining conference vote. Early fundamentalist leaders who attempted to preserve the practice were excommunicated without theological engagement; their witness is structurally excluded.
% DISAPPEARANCE_RATIONALE: If the continuing revelation override mechanism disappeared overnight, the institutional leadership would have no authorized path to suspend the polygamy mandate without renouncing prophetic authority itself. The church would face the 1890 choice again: institutional destruction or schism. The arrangement's existence is what allows the covenant community to persist as a legal entity.
% FOUNDING_PROBLEM: The church faced existential threat from federal anti-polygamy legislation: disincorporation, seizure of all church property (temples, meetinghouses, assets), imprisonment of the entire leadership cadre, and criminalization of the membership. The founding problem was institutional survival under state warfare.
% FOUNDING_PROBLEM_CORROBORATION: Contemporary federal records (Edmunds-Tucker Act enforcement, Supreme Court rulings, presidential proclamations) corroborate the existential threat. The church's own 1890 Manifesto and Woodruff's later testimony acknowledge the survival motive. Fundamentalist dissidents corroborate the threat was real but argue the surrender was faithless — the problem's reality is not contested, only the legitimacy of the response.
narrative_ontology:disappearance_verdict(eternal_marriage_covenant__prophetic_override_reading, world_rearranges).
narrative_ontology:founding_problem_status(eternal_marriage_covenant__prophetic_override_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(eternal_marriage_covenant__prophetic_override_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(eternal_marriage_covenant__prophetic_override_reading, 'none', 1).
narrative_ontology:epsilon_provenance(eternal_marriage_covenant__prophetic_override_reading, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(eternal_marriage_covenant__prophetic_override_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(eternal_marriage_covenant__prophetic_override_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(eternal_marriage_covenant__prophetic_override_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.68 at interval end) reflects the asymmetric transfer: the institution preserves itself by offloading the cost of compliance onto dissident families. Suppression (0.42) is moderate — the override reduces overt coercion of mainstream members (they no longer face prosecution) but intensifies structural suppression of dissidents (excommunication, delegitimization). Theater ratio (0.31) captures the performative retention of the doctrine while the practice is abandoned — the eternal principle is ritually affirmed while its operational content is hollowed out. Accessibility collapse (0.55) and resistance (0.48) reflect the partial but real alternatives: fundamentalist schisms demonstrate the constraint is not total, but exit is identity-locked for those who remain.
 *
 * PERSPECTIVAL GAP:
 *   From the institutional seat, the override is genuine coordination: revelation adapts to preserve the people. From the fundamentalist seat, the same mechanism is extraction: the institution saves itself by sacrificing the covenant's most demanding adherents. The engine computes this divergence from the structural data — the claimed type (tangled_rope) names the hybrid structure without resolving the perspectival conflict.
 *
 * DIRECTIONALITY LOGIC:
 *   Institutional leadership sits at the beneficiary end (d ≈ 0.15): they gain survival, legitimacy, and continued authority. Mainstream membership is near-symmetric (d ≈ 0.45): genuine coordination benefit (safety, legitimacy) with diffuse cost (doctrinal tension). Fundamentalist dissidents and polygamous families are at the target end (d ≈ 0.85–0.95): they bear concentrated costs, have identity-locked or trapped exit, and their resistance is structurally suppressed. Federal authorities are excluded — their pressure is the external condition, not a party to the covenant.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (federal existential threat) is dead — the church achieved legal recognition, property restitution, and mainstream integration by 1904. Yet the override mechanism persists as the doctrinal basis for all subsequent prophetic adaptations (priesthood restriction lift, correlation movement, policy changes). The mandate has outlived its founding condition; the constraint now operates as a permanent institutional flexibility mechanism. Mandatrophy is unresolved: the arrangement continues to serve coordination (institutional adaptivity) while extracting from those who treat prior revelations as binding.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    revelation_authenticity,
    'Was the 1890 Manifesto received as genuine revelation, or was it a strategic capitulation retroactively sacralized?',
    'Contemporary diary records, Woodruff''s private correspondence, sustaining conference dynamics, and the revelation''s textual form (absence of ''thus saith the Lord'' language).',
    'If strategic capitulation, the override mechanism is a Snare (coordination story as cover for extraction). If genuine revelation, it is a Tangled Rope (real coordination function with asymmetric costs). The classification hinges on the authenticity of the revelatory claim.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(revelation_authenticity, conceptual, 'Epistemic status of the overriding revelation itself.').

omega_variable(
    doctrine_practice_separability,
    'Can the eternal marriage doctrine be meaningfully retained while its defining practice is permanently suspended?',
    'Theological analysis of whether ''eternal principle'' without practice is a coherent category, or whether the doctrine collapses into symbolism. Track fundamentalist schism persistence as evidence of inseparability.',
    'If inseparable, the theater ratio is understated — the retained doctrine is pure performance. If separable, the coordination function (doctrinal continuity) is genuine and the extraction is the price of that continuity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(doctrine_practice_separability, conceptual, 'Whether the doctrine/practice distinction sustains real coordination or masks extraction.').

omega_variable(
    federal_pressure_as_constraint,
    'Is federal pressure an external condition the church navigates, or a constitutive element of the revelation''s content?',
    'Compare the Manifesto''s language to the specific legal threats (Edmunds-Tucker disincorporation provisions, property seizure timelines). If the revelation''s terms map precisely to the legal demands, federal pressure is constitutive.',
    'If constitutive, the ''revelation'' is the state''s will mediated through prophetic authority — the constraint is a Snare with the state as hidden agenda-setter. If external, the church''s adaptive response is genuine agency.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(federal_pressure_as_constraint, empirical, 'Whether the overriding revelation encodes state demands as divine will.').

omega_variable(
    committer_kernel_framing,
    'This reading instantiates the continuing revelation premise as a structural override mechanism. How would classification change under the immutable commandment reading (which denies override legitimacy) or temporal accommodation reading (which treats override as suspension only)?',
    'Generate sibling constraint stories for each reading; compare their extractiveness, suppression, and classification. The kernel''s constraint family structure reveals framing-dependent variance.',
    'If sibling readings produce substantially different classifications, the kernel itself is the site of contestation — the ''eternal marriage covenant'' is not a single constraint but a family. This story''s classification is reading-indexed, not kernel-indexed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_kernel_framing, conceptual, 'Kernel-reading framing under-determination: classification depends on which reading instantiates the constraint.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(eternal_marriage_covenant__prophetic_override_reading, 1852, 1904).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(eter_tr_t1852, eternal_marriage_covenant__prophetic_override_reading, theater_ratio, 1852, 0.05).
narrative_ontology:measurement(eter_tr_t1862, eternal_marriage_covenant__prophetic_override_reading, theater_ratio, 1862, 0.08).
narrative_ontology:measurement(eter_tr_t1872, eternal_marriage_covenant__prophetic_override_reading, theater_ratio, 1872, 0.12).
narrative_ontology:measurement(eter_tr_t1882, eternal_marriage_covenant__prophetic_override_reading, theater_ratio, 1882, 0.21).
narrative_ontology:measurement(eter_tr_t1890, eternal_marriage_covenant__prophetic_override_reading, theater_ratio, 1890, 0.28).
narrative_ontology:measurement(eter_tr_t1904, eternal_marriage_covenant__prophetic_override_reading, theater_ratio, 1904, 0.31).

% Extraction over time
narrative_ontology:measurement(eter_be_t1852, eternal_marriage_covenant__prophetic_override_reading, base_extractiveness, 1852, 0.15).
narrative_ontology:measurement(eter_be_t1862, eternal_marriage_covenant__prophetic_override_reading, base_extractiveness, 1862, 0.22).
narrative_ontology:measurement(eter_be_t1872, eternal_marriage_covenant__prophetic_override_reading, base_extractiveness, 1872, 0.35).
narrative_ontology:measurement(eter_be_t1882, eternal_marriage_covenant__prophetic_override_reading, base_extractiveness, 1882, 0.52).
narrative_ontology:measurement(eter_be_t1890, eternal_marriage_covenant__prophetic_override_reading, base_extractiveness, 1890, 0.65).
narrative_ontology:measurement(eter_be_t1904, eternal_marriage_covenant__prophetic_override_reading, base_extractiveness, 1904, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(eter_su_t1852, eternal_marriage_covenant__prophetic_override_reading, suppression_requirement, 1852, 0.1).
narrative_ontology:measurement(eter_su_t1862, eternal_marriage_covenant__prophetic_override_reading, suppression_requirement, 1862, 0.15).
narrative_ontology:measurement(eter_su_t1872, eternal_marriage_covenant__prophetic_override_reading, suppression_requirement, 1872, 0.25).
narrative_ontology:measurement(eter_su_t1882, eternal_marriage_covenant__prophetic_override_reading, suppression_requirement, 1882, 0.45).
narrative_ontology:measurement(eter_su_t1890, eternal_marriage_covenant__prophetic_override_reading, suppression_requirement, 1890, 0.38).
narrative_ontology:measurement(eter_su_t1904, eternal_marriage_covenant__prophetic_override_reading, suppression_requirement, 1904, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(eternal_marriage_covenant__prophetic_override_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(eternal_marriage_covenant__prophetic_override_reading, 0.1).
narrative_ontology:affects_constraint(eternal_marriage_covenant__prophetic_override_reading, eternal_marriage_covenant__immutable_commandment_reading).
narrative_ontology:affects_constraint(eternal_marriage_covenant__prophetic_override_reading, eternal_marriage_covenant__temporal_accommodation_reading).
narrative_ontology:affects_constraint(eternal_marriage_covenant__prophetic_override_reading, priesthood_restriction__continuing_revelation_override).
narrative_ontology:affects_constraint(eternal_marriage_covenant__prophetic_override_reading, correlation_movement__institutional_standardization).

% DUAL FORMULATION NOTE:
% This story is one of three readings of the eternal_marriage_covenant kernel. The immutable_commandment_reading treats D&C 132 as irrevocable (mountain-claimed, high extraction for fundamentalists). The temporal_accommodation_reading treats the Manifesto as provisional suspension (scaffold-claimed, sunset clause implied). This prophetic_override_reading treats the override as doctrinally authoritative and permanent (tangled_rope). The three differ in their ε referents: the immutable reading assesses the standing covenant; the override reading assesses the adaptive mechanism; the accommodation reading assesses the suspension arrangement.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(eternal_marriage_covenant__prophetic_override_reading, institutional, 0.15).
constraint_indexing:directionality_override(eternal_marriage_covenant__prophetic_override_reading, powerless, 0.9).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
