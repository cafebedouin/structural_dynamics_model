% ============================================================================
% CONSTRAINT STORY: eternal_marriage_covenant__immutable_commandment_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_eternal_marriage_covenant__immutable_commandment_reading, []).

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
 *   constraint_id: eternal_marriage_covenant__immutable_commandment_reading
 *   human_readable: D&C 132 as Eternal, Immutable Law of Plural Marriage Required for Exaltation
 *   domain: religious_law/political_theology/commitment_system
 *
 * SUMMARY:
 *   This story instantiates the immutable_commandment_reading of the
 *   eternal_marriage_covenant kernel: D&C 132 is read as revealing plural
 *   marriage as eternal, unchangeable law, obedience to which is required for
 *   the highest degree of exaltation, with no internal mechanism by which a
 *   later prophet could legitimately suspend or override it without either
 *   apostatizing from the original revelation or admitting the original claim
 *   was wrong. Under this reading, federal anti-polygamy legislation creates
 *   a genuine martyrdom bind: compliance with federal law is read as
 *   disobedience to God, and disobedience to federal law brings prosecution,
 *   disenfranchisement, and loss of property. This reading treats the 1890
 *   Manifesto (addressed in the sibling temporal_accommodation_reading) and
 *   the doctrine of continuing revelation as override authority (addressed in
 *   the sibling prophetic_override_reading) as NOT part of this reading's own
 *   internal logic — from inside the immutable_commandment_reading, any later
 *   suspension is either a failure of nerve or evidence the revelation was
 *   never truly binding, which is exactly why this reading forecloses the
 *   prophetic_override_reading's core premise. Ex is authored on the standing
 *   arrangement of the practice itself (1852-1890 operative period) as this
 *   reading's own lights see it: a costly but binding requirement, not an
 *   optional or already-suspended one.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(eternal_marriage_covenant__immutable_commandment_reading, 0.68).
domain_priors:suppression_score(eternal_marriage_covenant__immutable_commandment_reading, 0.79).
domain_priors:theater_ratio(eternal_marriage_covenant__immutable_commandment_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(eternal_marriage_covenant__immutable_commandment_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(eternal_marriage_covenant__immutable_commandment_reading, suppression_requirement, 0.79).
narrative_ontology:constraint_metric(eternal_marriage_covenant__immutable_commandment_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(eternal_marriage_covenant__immutable_commandment_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(eternal_marriage_covenant__immutable_commandment_reading, resistance, 0.74).

% --- Constraint claim ---
narrative_ontology:constraint_claim(eternal_marriage_covenant__immutable_commandment_reading, tangled_rope).
narrative_ontology:human_readable(eternal_marriage_covenant__immutable_commandment_reading, "D&C 132 as Eternal, Immutable Law of Plural Marriage Required for Exaltation").
narrative_ontology:topic_domain(eternal_marriage_covenant__immutable_commandment_reading, "religious_law/political_theology/commitment_system").

domain_priors:requires_active_enforcement(eternal_marriage_covenant__immutable_commandment_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(eternal_marriage_covenant__immutable_commandment_reading, '60c1ae45-eaaa-4b2c-8f28-073d1b852ff4').
narrative_ontology:cs_kernel_codification('60c1ae45-eaaa-4b2c-8f28-073d1b852ff4', fixed_text).
narrative_ontology:cs_authority_grounding('60c1ae45-eaaa-4b2c-8f28-073d1b852ff4', lineage).
narrative_ontology:cs_interpretation_layer_present('60c1ae45-eaaa-4b2c-8f28-073d1b852ff4').
narrative_ontology:cs_reading_relation('60c1ae45-eaaa-4b2c-8f28-073d1b852ff4', eternal_marriage_covenant__prophetic_override_reading, forecloses).
narrative_ontology:cs_reading_relation('60c1ae45-eaaa-4b2c-8f28-073d1b852ff4', eternal_marriage_covenant__temporal_accommodation_reading, influences).
narrative_ontology:cs_axiom('60c1ae45-eaaa-4b2c-8f28-073d1b852ff4', foundational, revealed_law_admits_no_legitimate_supersession).
narrative_ontology:cs_axiom_status(revealed_law_admits_no_legitimate_supersession, holdable).
narrative_ontology:cs_axiom_grounding('60c1ae45-eaaa-4b2c-8f28-073d1b852ff4', revealed_law_admits_no_legitimate_supersession, deontological).
narrative_ontology:cs_axiom('60c1ae45-eaaa-4b2c-8f28-073d1b852ff4', foundational, plural_marriage_required_for_highest_exaltation).
narrative_ontology:cs_axiom_status(plural_marriage_required_for_highest_exaltation, overridden).
narrative_ontology:cs_axiom_grounding('60c1ae45-eaaa-4b2c-8f28-073d1b852ff4', plural_marriage_required_for_highest_exaltation, theological).
narrative_ontology:cs_reference_frame('60c1ae45-eaaa-4b2c-8f28-073d1b852ff4', restored_patriarchal_order_1852_declaration).
narrative_ontology:cs_drift_state('60c1ae45-eaaa-4b2c-8f28-073d1b852ff4', post_1890_manifesto_and_1904_second_manifesto, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('60c1ae45-eaaa-4b2c-8f28-073d1b852ff4', '').
narrative_ontology:cs_kernel_id(eternal_marriage_covenant__immutable_commandment_reading, eternal_marriage_covenant).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(eternal_marriage_covenant__immutable_commandment_reading, church_hierarchy_1852_1890).
narrative_ontology:constraint_beneficiary(eternal_marriage_covenant__immutable_commandment_reading, polygamous_male_patriarchs).
narrative_ontology:constraint_victim(eternal_marriage_covenant__immutable_commandment_reading, plural_wives).
narrative_ontology:constraint_victim(eternal_marriage_covenant__immutable_commandment_reading, children_of_plural_marriages).
narrative_ontology:constraint_victim(eternal_marriage_covenant__immutable_commandment_reading, monogamous_church_members_pressured_to_enter).
narrative_ontology:constraint_victim(eternal_marriage_covenant__immutable_commandment_reading, women_who_refuse_or_resist).
narrative_ontology:constraint_vindicates(eternal_marriage_covenant__immutable_commandment_reading, joseph_smith_prophetic_authority).
narrative_ontology:constraint_vindicates(eternal_marriage_covenant__immutable_commandment_reading, celestial_marriage_doctrine).
narrative_ontology:constraint_vindicates(eternal_marriage_covenant__immutable_commandment_reading, priesthood_sealing_power).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Publicly announces and defends plural marriage as revealed, binding doctrine in 1852; teaches that refusal to practice or accept it when called forecloses the highest degree of exaltation. Administers who is sealed to whom, controls temple access, and frames federal prosecution as persecution of true religion. Bears reputational and legal risk but retains doctrinal authority and communal loyalty throughout.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__immutable_commandment_reading, church_hierarchy_1852_1890, agenda_setter,
    institutional, civilizational, arbitrage, national).

% Enter plural marriages sanctioned by the doctrine, gaining social status, expanded household labor and reproductive capacity, and a guaranteed path to the highest celestial glory as taught. Face federal prosecution risk under anti-bigamy statutes but can relocate, go into hiding, or serve limited sentences while retaining standing within the community.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__immutable_commandment_reading, polygamous_male_patriarchs, beneficiary,
    powerful, generational, mobile, regional).

% Enter marriages under doctrinal teaching that refusal jeopardizes eternal salvation and family standing; bear disproportionate domestic labor, economic precarity when resources are divided among multiple households, and social stigma or legal vulnerability. Exit means leaving the faith community entirely, losing family, social network, and often economic support — a cost most cannot bear.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__immutable_commandment_reading, plural_wives, payer,
    powerless, biographical, trapped, regional).

% Born into households structured around plural marriage without having consented to the arrangement; may face legal illegitimacy questions, contested inheritance, and social stigma outside the community. Have no exit option as minors and inherit the doctrinal and legal consequences of their parents' compliance.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__immutable_commandment_reading, children_of_plural_marriages, payer,
    powerless, biographical, trapped, regional).

% Taught that celestial marriage in its fullest form is required for exaltation, they face social and doctrinal pressure to enter plural arrangements even when personally reluctant. Remaining monogamous by choice risks being read as insufficient devotion; leaving the church risks losing family and community ties built over a lifetime.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__immutable_commandment_reading, monogamous_church_members_pressured_to_enter, payer,
    moderate, biographical, constrained, regional).

% Object to being sealed into plural arrangements or resist a husband taking additional wives; face teachings that frame resistance as spiritual failure or rebellion against revealed law, with social ostracism and marital/family consequences for open refusal. Formal exit requires abandoning the faith community that constitutes their entire social world.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__immutable_commandment_reading, women_who_refuse_or_resist, payer,
    powerless, biographical, trapped, regional).

% Enacts anti-bigamy and anti-polygamy statutes (Morrill Act, Edmunds Act, Edmunds-Tucker Act) and prosecutes practitioners, but has no standing within the church's own doctrinal framework to contest the revelation's validity — its objection registers as external persecution within the reading's own terms, not as a legitimate voice in the doctrinal conversation.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__immutable_commandment_reading, federal_government, excluded,
    institutional, generational, analytical, national).

% Break with the church over the doctrine, arguing it is neither eternal nor immutable and pointing to internal inconsistency or coercion; their testimony is discounted within the immutable-commandment reading as evidence of apostasy rather than as evidence about the doctrine's structure.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__immutable_commandment_reading, dissenting_members_and_apostates, excluded,
    moderate, biographical, mobile, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(eternal_marriage_covenant__immutable_commandment_reading, church_hierarchy_1852_1890).
narrative_ontology:fixing_cost_class(eternal_marriage_covenant__immutable_commandment_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Binds a persecuted, geographically isolated religious community around a shared, costly, doctrinally-total commitment that signals in-group loyalty, coordinates marriage and inheritance practice across a rapidly growing frontier population, and provides a theological rationale for large families able to settle and hold contested territory.
% TRANSFER_FUNCTION: Moves domestic labor, reproductive capacity, and social deference from women (plural wives, resistant women, and pressured monogamous women) to men who hold priesthood authority and enter plural marriages; moves doctrinal legitimacy and institutional authority to the church hierarchy that administers and defends the practice as immutable revealed law.
% ABSENT_VOICES: Plural wives who married under duress or doctrinal pressure rarely left first-person accounts contesting the arrangement publicly, given the costs of open dissent within a tightly bound community; federal officials and outside observers who documented harm were dismissed within the community's own framework as hostile outsiders rather than credible witnesses.
% DISAPPEARANCE_RATIONALE: If this specific reading — that D&C 132 is eternal, immutable, and required for exaltation with no legitimate revision path — had never taken hold, marriage practice within the church could have followed a more discretionary or optional theological model from the outset. Its actual disappearance (functionally, via the 1890 Manifesto) did rearrange the world: plural marriage ended as institutional practice, federal statehood became possible, and the doctrine itself was reclassified by the institution as suspended rather than binding, which is precisely the sibling reading this reading forecloses.
% FOUNDING_PROBLEM: The founding problem this reading claims to solve is securing the highest possible eternal exaltation for the Saints by revealing and requiring what it presents as a restored, ancient patriarchal marriage order, while also addressing practical frontier problems of population growth, care for widows/converts, and community cohesion under persecution.
% FOUNDING_PROBLEM_CORROBORATION: The institution's own successor authority (the church after 1890, and definitively after 1904's Second Manifesto) attests that the practice's operative requirement ended and that violators face excommunication — an institutional attestation from within the same tradition, not merely outside critics, that the 'required for exaltation' claim as an operative practice no longer holds, even where the doctrine is described as theoretically unrescinded. Outside historians and federal court findings corroborate that coercive and harmful dynamics accompanied the practice; no corroboration from outside the tradition's own successor institution supports the claim that the founding problem remains live today.
narrative_ontology:disappearance_verdict(eternal_marriage_covenant__immutable_commandment_reading, world_rearranges).
narrative_ontology:founding_problem_status(eternal_marriage_covenant__immutable_commandment_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(eternal_marriage_covenant__immutable_commandment_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(eternal_marriage_covenant__immutable_commandment_reading, 'none', 1).
narrative_ontology:epsilon_provenance(eternal_marriage_covenant__immutable_commandment_reading, 0.68, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(eternal_marriage_covenant__immutable_commandment_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(eternal_marriage_covenant__immutable_commandment_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(eternal_marriage_covenant__immutable_commandment_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises across the measured interval (0.42 to 0.70) as the doctrine moves from private teaching (1830s-40s) to public, mandatory-framed practice (1852 onward) and federal pressure intensifies enforcement costs onto the community, which the hierarchy passes down as intensified doctrinal demands on wives and pressured monogamous members. It declines slightly at t=40 as the practice nears its actual historical end, reflecting waning enforceability even within this reading's own terms. Suppression tracks federal legal escalation (Morrill, Edmunds, Edmunds-Tucker Acts) alongside internal doctrinal pressure — both external state coercion and internal excommunication threat operate simultaneously, which this reading does not distinguish as separable mechanisms. Theater ratio stays comparatively low and rises only modestly: this is not primarily a performative constraint, the enforcement and extraction are substantively real for the payer stakeholders.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda_setter seat (church hierarchy), the arrangement is revealed, binding, salvific coordination under righteous persecution. From the payer seats (plural wives, children, resisting women), the same structure is experienced as required extraction of domestic and reproductive labor under threat of eternal consequence, with no legitimate internal path to refuse. The engine should compute these divergently from the same structural data; this reading does not adjudicate between them.
 *
 * DIRECTIONALITY LOGIC:
 *   Church hierarchy and polygamous male patriarchs sit near the beneficiary end: they set doctrine, administer sealings, and receive social status and (for men) expanded household resources, with meaningful exit or mitigation options (relocation, limited sentences, communal reputational shelter). Plural wives, resistant women, and children of plural marriages sit at the target end: trapped exit options, powerless structural position, and direct bearing of domestic, reproductive, and reputational costs framed as spiritually mandatory. Monogamous members under pressure to enter occupy an intermediate position — moderate power, constrained rather than fully trapped exit, but real doctrinal coercion.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (securing exaltation via restored patriarchal order, addressing frontier community needs) is read by this reading's own hierarchy in the 1852-1890 period as fully live and urgent — that is what makes this reading internally coherent as a tangled_rope rather than a pure snare: there IS a genuine, sincerely-held coordination function (community cohesion, care structures, doctrinal completeness) bundled with real asymmetric extraction (from women and children who bear costs without proportionate say). The mandatrophy question becomes visible only from OUTSIDE this reading, in the founding_problem_status field: the institution's own later self (1890, 1904) declares the problem's operative form dead, which this reading cannot accommodate without contradiction — hence its forecloses relationship to prophetic_override_reading and temporal_accommodation_reading.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    immutability_vs_institutional_self_supersession,
    'If the church''s own later authority (1890 Manifesto, 1904 Second Manifesto) treats the practice as suspended and disciplines violators, does that constitute proof the original ''immutable'' claim was never structurally true, or is it evidence of a legitimate but unaddressed revision mechanism this reading simply does not recognize?',
    'Compare the internal doctrinal reasoning offered at the time of the Manifesto (was it framed as continuing revelation, or as external capitulation under legal duress?) against the immutable_commandment_reading''s own stated criteria for what would count as legitimate revision.',
    'If no legitimate revision mechanism is acknowledged even retrospectively within this reading''s tradition, the reading is internally coherent but empirically falsified by its own institution''s later conduct — supporting classification as tangled_rope that could not survive contact with its own founding problem going dead. If a revision mechanism is retroactively recognized, this reading effectively collapses into prophetic_override_reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(immutability_vs_institutional_self_supersession, conceptual, 'Whether the 1890/1904 suspensions falsify or merely complicate the immutable-commandment premise.').

omega_variable(
    coercion_versus_sincere_belief_in_wives_compliance,
    'To what extent did plural wives'' entry into and continuation in these marriages reflect internalized sincere belief in the doctrine''s salvific necessity versus externally coerced compliance under social, economic, and familial pressure?',
    'First-person accounts, exit narratives, divorce/separation records, and comparative rates of participation among women with greater versus lesser economic independence or family support outside the community.',
    'If predominantly sincere belief, effective suppression is lower than the structural exit-option data alone suggests (internalized rather than externally imposed). If predominantly coerced compliance masked by doctrinal framing, effective suppression is higher, and the extraction is closer to a snare dressed in coordination language.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coercion_versus_sincere_belief_in_wives_compliance, empirical, 'Structural versus internalized suppression mechanism among plural wives.').

omega_variable(
    false_summit_of_natural_patriarchal_order,
    'This reading presents plural marriage as revealing a restored, natural, eternal patriarchal order rather than a historically contingent 19th-century institutional arrangement. Is the ''eternal law'' framing a genuine metaphysical claim, or a naturalizing cover story that benefits identifiable parties (male patriarchs, hierarchy) by placing a contestable social arrangement beyond the reach of ordinary revision?',
    'Examine whether comparable claims of eternal necessity attach to other institutional arrangements that were later revised without theological catastrophe, and whether the beneficiary structure (concentrated male and hierarchical benefit) tracks the arrangement''s stated theological rationale or diverges from it.',
    'If the naturalizing claim functions primarily to insulate a beneficiary-concentrated arrangement from revision, the mountain-like ''eternal, immutable'' framing is a false summit and the constraint''s true structural type is tangled_rope or snare regardless of the doctrinal language used to describe it — consistent with this story''s claimed_type.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_summit_of_natural_patriarchal_order, conceptual, 'Whether ''eternal and immutable'' functions as genuine metaphysical claim or naturalizing cover for concentrated benefit.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(eternal_marriage_covenant__immutable_commandment_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(eter_tr_t0, eternal_marriage_covenant__immutable_commandment_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(eter_tr_t8, eternal_marriage_covenant__immutable_commandment_reading, theater_ratio, 8, 0.14).
narrative_ontology:measurement(eter_tr_t16, eternal_marriage_covenant__immutable_commandment_reading, theater_ratio, 16, 0.18).
narrative_ontology:measurement(eter_tr_t24, eternal_marriage_covenant__immutable_commandment_reading, theater_ratio, 24, 0.2).
narrative_ontology:measurement(eter_tr_t32, eternal_marriage_covenant__immutable_commandment_reading, theater_ratio, 32, 0.22).
narrative_ontology:measurement(eter_tr_t40, eternal_marriage_covenant__immutable_commandment_reading, theater_ratio, 40, 0.22).

% Extraction over time
narrative_ontology:measurement(eter_be_t0, eternal_marriage_covenant__immutable_commandment_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(eter_be_t8, eternal_marriage_covenant__immutable_commandment_reading, base_extractiveness, 8, 0.55).
narrative_ontology:measurement(eter_be_t16, eternal_marriage_covenant__immutable_commandment_reading, base_extractiveness, 16, 0.63).
narrative_ontology:measurement(eter_be_t24, eternal_marriage_covenant__immutable_commandment_reading, base_extractiveness, 24, 0.68).
narrative_ontology:measurement(eter_be_t32, eternal_marriage_covenant__immutable_commandment_reading, base_extractiveness, 32, 0.7).
narrative_ontology:measurement(eter_be_t40, eternal_marriage_covenant__immutable_commandment_reading, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(eter_su_t0, eternal_marriage_covenant__immutable_commandment_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(eter_su_t8, eternal_marriage_covenant__immutable_commandment_reading, suppression_requirement, 8, 0.6).
narrative_ontology:measurement(eter_su_t16, eternal_marriage_covenant__immutable_commandment_reading, suppression_requirement, 16, 0.7).
narrative_ontology:measurement(eter_su_t24, eternal_marriage_covenant__immutable_commandment_reading, suppression_requirement, 24, 0.78).
narrative_ontology:measurement(eter_su_t32, eternal_marriage_covenant__immutable_commandment_reading, suppression_requirement, 32, 0.83).
narrative_ontology:measurement(eter_su_t40, eternal_marriage_covenant__immutable_commandment_reading, suppression_requirement, 40, 0.79).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(eternal_marriage_covenant__immutable_commandment_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(eternal_marriage_covenant__immutable_commandment_reading, 0.08).
narrative_ontology:affects_constraint(eternal_marriage_covenant__immutable_commandment_reading, prophetic_override_reading).
narrative_ontology:affects_constraint(eternal_marriage_covenant__immutable_commandment_reading, temporal_accommodation_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the eternal_marriage_covenant kernel. immutable_commandment_reading (this story) treats D&C 132 as binding, eternal, and non-revisable — ε=0.68, tangled_rope, with a martyrdom bind against federal law and no internal exit for payer stakeholders. prophetic_override_reading treats the same textual kernel as subject to legitimate supersession by continuing revelation, which should register lower suppression and a different beneficiary/victim configuration once the override actually occurs. temporal_accommodation_reading treats the Manifesto as suspending operative practice while preserving doctrinal truth-value, occupying a structurally distinct position (likely scaffold-shaped, given the declared sunset-like suspension) from both siblings. All three share the same underlying text and community but diverge sharply in ε and structural type because each reading answers differently the question this reading forecloses: whether legitimate revision is possible at all.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
