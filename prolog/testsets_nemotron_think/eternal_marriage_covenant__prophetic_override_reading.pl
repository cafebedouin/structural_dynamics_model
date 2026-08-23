% ============================================================================
% CONSTRAINT STORY: eternal_marriage_covenant__prophetic_override_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
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
 *   human_readable: Prophetic Override of Eternal Marriage Covenant
 *   domain: religious_law/political_theology/commitment_system
 *
 * SUMMARY:
 *   The prophetic override reading of the eternal marriage covenant holds
 *   that the living prophet, exercising the doctrine of continuing
 *   revelation, can supersede prior revelation (D&C 132) when circumstances —
 *   specifically federal pressure threatening institutional survival —
 *   require it. This reading was instantiated in the 1890 Manifesto and
 *   subsequent revelations ending plural marriage. The constraint coordinates
 *   the community around a new obedience (monogamy, loyalty to the state)
 *   while extracting the cost from those whose identity and salvation were
 *   bound to the old covenant. The engine will compute per-seat
 *   classifications from the structural data below; the claimed type
 *   (tangled_rope) reflects the author's judgment that genuine coordination
 *   (institutional survival) and asymmetric extraction (onto polygamous
 *   families and immutable-commandment adherents) coexist under active
 *   enforcement.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(eternal_marriage_covenant__prophetic_override_reading, 0.65).
domain_priors:suppression_score(eternal_marriage_covenant__prophetic_override_reading, 0.75).
domain_priors:theater_ratio(eternal_marriage_covenant__prophetic_override_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(eternal_marriage_covenant__prophetic_override_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(eternal_marriage_covenant__prophetic_override_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(eternal_marriage_covenant__prophetic_override_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(eternal_marriage_covenant__prophetic_override_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(eternal_marriage_covenant__prophetic_override_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(eternal_marriage_covenant__prophetic_override_reading, tangled_rope).
narrative_ontology:human_readable(eternal_marriage_covenant__prophetic_override_reading, "Prophetic Override of Eternal Marriage Covenant").
narrative_ontology:topic_domain(eternal_marriage_covenant__prophetic_override_reading, "religious_law/political_theology/commitment_system").

domain_priors:requires_active_enforcement(eternal_marriage_covenant__prophetic_override_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(eternal_marriage_covenant__prophetic_override_reading, 'bb7300a8-94da-4890-a49f-49e62247b217').
narrative_ontology:cs_kernel_codification('bb7300a8-94da-4890-a49f-49e62247b217', fixed_text).
narrative_ontology:cs_authority_grounding('bb7300a8-94da-4890-a49f-49e62247b217', lineage).
narrative_ontology:cs_interpretation_layer_present('bb7300a8-94da-4890-a49f-49e62247b217').
narrative_ontology:cs_reading_relation('bb7300a8-94da-4890-a49f-49e62247b217', eternal_marriage_covenant__immutable_commandment_reading, forecloses).
narrative_ontology:cs_reading_relation('bb7300a8-94da-4890-a49f-49e62247b217', eternal_marriage_covenant__temporal_accommodation_reading, forecloses).
narrative_ontology:cs_axiom('bb7300a8-94da-4890-a49f-49e62247b217', foundational, continuing_revelation_supersedes_prior_covenant).
narrative_ontology:cs_axiom_status(continuing_revelation_supersedes_prior_covenant, holdable).
narrative_ontology:cs_axiom_grounding('bb7300a8-94da-4890-a49f-49e62247b217', continuing_revelation_supersedes_prior_covenant, theological).
narrative_ontology:cs_axiom('bb7300a8-94da-4890-a49f-49e62247b217', secondary, church_survival_justifies_doctrinal_change).
narrative_ontology:cs_axiom_status(church_survival_justifies_doctrinal_change, holdable).
narrative_ontology:cs_axiom_grounding('bb7300a8-94da-4890-a49f-49e62247b217', church_survival_justifies_doctrinal_change, instrumental).
narrative_ontology:cs_reference_frame('bb7300a8-94da-4890-a49f-49e62247b217', living_prophetic_authority_framework).
narrative_ontology:cs_drift_state('bb7300a8-94da-4890-a49f-49e62247b217', post_manifesto_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('bb7300a8-94da-4890-a49f-49e62247b217', '').
narrative_ontology:cs_kernel_id(eternal_marriage_covenant__prophetic_override_reading, eternal_marriage_covenant).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(eternal_marriage_covenant__prophetic_override_reading, church_institution).
narrative_ontology:constraint_beneficiary(eternal_marriage_covenant__prophetic_override_reading, living_prophet).
narrative_ontology:constraint_victim(eternal_marriage_covenant__prophetic_override_reading, polygamous_families).
narrative_ontology:constraint_victim(eternal_marriage_covenant__prophetic_override_reading, immutable_commandment_adherents).
narrative_ontology:constraint_vindicates(eternal_marriage_covenant__prophetic_override_reading, continuing_revelation_doctrine).
narrative_ontology:constraint_vindicates(eternal_marriage_covenant__prophetic_override_reading, church_survival_imperative).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receives new revelation superseding prior eternal covenant; legitimizes change as divine will; maintains institutional authority and avoids federal destruction. Exercises coercive power to enforce compliance (excommunication, temple recommend denial).
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__prophetic_override_reading, living_prophet, agenda_setter,
    institutional, generational, arbitrage, global).

% Survives federal anti-polygamy prosecution, property seizure, and disincorporation threats by adapting doctrine. Gains legal legitimacy, statehood for Utah, and continued growth. Bears reputational cost among dissidents but secures institutional continuity.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__prophetic_override_reading, church_institution, beneficiary,
    institutional, civilizational, constrained, global).

% Forced to abandon plural marriages, hide families, or face excommunication. Their religious identity and salvation narrative are bound to the superseded covenant. Exit means loss of community, eternal family sealing, and social world.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__prophetic_override_reading, polygamous_families, payer,
    powerless, biographical, identity_locked, local).

% Believe D&C 132 is eternal and unchangeable. View the Manifesto and subsequent revelations as apostasy. Face pressure to conform or leave; many form fundamentalist schisms. Bear psychological and communal costs of either submission or schism.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__prophetic_override_reading, immutable_commandment_adherents, payer,
    moderate, biographical, constrained, regional).

% External pressure source: anti-polygamy laws (Edmunds Act, Edmunds-Tucker Act) threaten church corporate existence. Their enforcement creates the survival constraint that activates prophetic override. Not a party to the covenant but structurally determinative.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__prophetic_override_reading, federal_authorities, observer,
    institutional, generational, analytical, national).

% Analyzes the constraint from outside the commitment system. Sees the pattern: external threat triggers revelatory adaptation; doctrine changes to preserve institution. No personal stake in salvation or institutional survival.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__prophetic_override_reading, scholar_of_mormon_theology, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables the church to adapt its defining covenant under existential external threat, preserving institutional continuity and avoiding violent confrontation with the state. Solves the coordination problem of collective obedience to a new divine directive that replaces the old one.
% TRANSFER_FUNCTION: Transfers the cost of adaptation (abandoning plural marriage, accepting doctrinal mutation) from the institution onto the polygamous families and immutable-commandment adherents. The institution gains survival and legitimacy; the adherents lose their covenantal framework and face exile or schism.
% ABSENT_VOICES: The voices of those who held the eternal covenant as literally immutable — the polygamous families exiled to Mexico/Canada, the fundamentalist schisms that emerged — were structurally excluded from the revelatory process. They had no seat in the quorum that received the new revelation; their objection was treated as apostasy.
% DISAPPEARANCE_RATIONALE: Without the prophetic override mechanism, the church would have faced federal disincorporation, asset seizure, and likely fragmentation. The constraint is the hinge that allowed the institution to survive; its removal would have rewritten the history of Mormonism and the American West.
% FOUNDING_PROBLEM: The church faced existential destruction by the U.S. federal government (Edmunds-Tucker Act, disincorporation, seizure of temples) unless it abandoned the practice of plural marriage, which was understood as a requirement of the eternal marriage covenant (D&C 132). The constraint was built to resolve the contradiction between divine command and state power.
% FOUNDING_PROBLEM_CORROBORATION: The church's own Manifesto (Official Declaration 1) and subsequent statements by Presidents Woodruff, Snow, and Smith attest the survival imperative. Non-Mormon historians (e.g., Sarah Barringer Gordon, Kathleen Flake) corroborate the federal pressure as the proximate cause. Fundamentalist groups and some scholars (e.g., D. Michael Quinn) argue the founding problem was not survival but capitulation, and that the constraint persists as a mechanism for ongoing doctrinal plasticity.
narrative_ontology:disappearance_verdict(eternal_marriage_covenant__prophetic_override_reading, world_rearranges).
narrative_ontology:founding_problem_status(eternal_marriage_covenant__prophetic_override_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(eternal_marriage_covenant__prophetic_override_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(eternal_marriage_covenant__prophetic_override_reading, 'none', 1).
narrative_ontology:epsilon_provenance(eternal_marriage_covenant__prophetic_override_reading, 0.65, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness (0.65) is substantial because the constraint transfers the burden of adaptation onto a discrete group (polygamous families, fundamentalist sympathizers) while the institution captures the survival benefit. Suppression (0.75) is high because dissent is met with excommunication, loss of temple access, and social ostracism — alternatives are actively closed. Theater ratio (0.4) is moderate: the revelatory form is genuine within the tradition, but the timing and content correlate tightly with federal pressure, suggesting performative maintenance of prophetic authority. Accessibility collapse (0.8) is high because once the prophet speaks, the covenantal framework shifts for all members; there is no internal exit that preserves the old covenant. Resistance (0.6) reflects persistent fundamentalist schisms and internal dissent.
 *
 * PERSPECTIVAL GAP:
 *   From the prophet/institution seat, the constraint appears as a rope (genuine coordination solving an existential collective-action problem). From the polygamous_families seat, it appears as a snare (pure extraction: their covenantal world is dismantled for institutional survival). The engine will compute this divergence from the declared roles, power, and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   The living_prophet and church_institution are structural beneficiaries (d near 0): they receive institutional survival, continued authority, and legal legitimacy. Polygamous_families and immutable_commandment_adherents are targets (d near 1): they bear the full cost of doctrinal mutation with identity-locked or constrained exit. Federal_authorities are observers (d ~ 0.5): they are the external condition that activates the constraint but do not participate in its internal economy. The scholar is analytical (d = 0.5).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (federal existential threat) is historically past, but the mechanism (continuing revelation as override) remains active and has been used for subsequent changes (priesthood ban, LGBT policies). The constraint has not atrophied; it has been repurposed. Mandatrophy is not resolved — the override mechanism persists as a standing capacity for doctrinal adaptation under pressure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    revelation_vs_strategic_adaptation,
    'Is the prophetic override a genuine divine revelation or a strategic adaptation to federal pressure?',
    'Comparative analysis of revelatory timing with federal legislative/judicial milestones; internal diaries of church leaders; theological criteria for authentic revelation within the tradition.',
    'If strategic, the constraint is a snare disguised as coordination; if genuine, the coordination function is theologically grounded and extraction is a side effect of divine economy.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(revelation_vs_strategic_adaptation, conceptual, 'Ontological status of the revelatory event: divine vs. pragmatic.').

omega_variable(
    doctrine_vs_practice_boundary,
    'Does the prophetic override change the doctrine (eternal nature of plural marriage) or only the practice?',
    'Textual analysis of Official Declaration 1, subsequent statements by church presidents, and temple ceremony changes. Correlation with fundamentalist schism justifications.',
    'If doctrine changed, the temporal_accommodation_reading is false and this reading forecloses it. If only practice suspended, this reading overstates the override.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(doctrine_vs_practice_boundary, empirical, 'Scope of the override: doctrinal mutation vs. practical suspension.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (excommunication, legal pressure) or internalized (belief that dissent equals damnation)?',
    'Post-exit trajectory study: do former fundamentalists who leave the mainstream church continue to experience suppression? Comparative analysis of shunning intensity vs. internalized fear.',
    'If internalized, effective suppression is higher than structural measures suggest; the constraint operates through identity fusion, not just institutional power.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for identity-locked payers.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(eternal_marriage_covenant__prophetic_override_reading, 0, 134).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(emc_por_tr_t0, eternal_marriage_covenant__prophetic_override_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(emc_por_tr_t10, eternal_marriage_covenant__prophetic_override_reading, theater_ratio, 10, 0.35).
narrative_ontology:measurement(emc_por_tr_t30, eternal_marriage_covenant__prophetic_override_reading, theater_ratio, 30, 0.38).
narrative_ontology:measurement(emc_por_tr_t60, eternal_marriage_covenant__prophetic_override_reading, theater_ratio, 60, 0.4).
narrative_ontology:measurement(emc_por_tr_t100, eternal_marriage_covenant__prophetic_override_reading, theater_ratio, 100, 0.4).
narrative_ontology:measurement(emc_por_tr_t134, eternal_marriage_covenant__prophetic_override_reading, theater_ratio, 134, 0.4).

% Extraction over time
narrative_ontology:measurement(emc_por_be_t0, eternal_marriage_covenant__prophetic_override_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(emc_por_be_t10, eternal_marriage_covenant__prophetic_override_reading, base_extractiveness, 10, 0.55).
narrative_ontology:measurement(emc_por_be_t30, eternal_marriage_covenant__prophetic_override_reading, base_extractiveness, 30, 0.6).
narrative_ontology:measurement(emc_por_be_t60, eternal_marriage_covenant__prophetic_override_reading, base_extractiveness, 60, 0.62).
narrative_ontology:measurement(emc_por_be_t100, eternal_marriage_covenant__prophetic_override_reading, base_extractiveness, 100, 0.64).
narrative_ontology:measurement(emc_por_be_t134, eternal_marriage_covenant__prophetic_override_reading, base_extractiveness, 134, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(emc_por_su_t0, eternal_marriage_covenant__prophetic_override_reading, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(emc_por_su_t10, eternal_marriage_covenant__prophetic_override_reading, suppression_requirement, 10, 0.72).
narrative_ontology:measurement(emc_por_su_t30, eternal_marriage_covenant__prophetic_override_reading, suppression_requirement, 30, 0.74).
narrative_ontology:measurement(emc_por_su_t60, eternal_marriage_covenant__prophetic_override_reading, suppression_requirement, 60, 0.75).
narrative_ontology:measurement(emc_por_su_t100, eternal_marriage_covenant__prophetic_override_reading, suppression_requirement, 100, 0.75).
narrative_ontology:measurement(emc_por_su_t134, eternal_marriage_covenant__prophetic_override_reading, suppression_requirement, 134, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(eternal_marriage_covenant__prophetic_override_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(eternal_marriage_covenant__prophetic_override_reading, 0.08).
narrative_ontology:affects_constraint(eternal_marriage_covenant__prophetic_override_reading, eternal_marriage_covenant__immutable_commandment_reading).
narrative_ontology:affects_constraint(eternal_marriage_covenant__prophetic_override_reading, eternal_marriage_covenant__temporal_accommodation_reading).

% DUAL FORMULATION NOTE:
% This reading and the temporal_accommodation_reading both invoke continuing revelation but differ on whether doctrine or only practice is overridden. The immutable_commandment_reading denies continuing revelation can touch this covenant. All three share the kernel D&C 132 but instantiate different constraints with different ε, beneficiaries, and victims.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(eternal_marriage_covenant__prophetic_override_reading, institutional, 0.1).
constraint_indexing:directionality_override(eternal_marriage_covenant__prophetic_override_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
