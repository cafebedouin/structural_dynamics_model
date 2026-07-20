% ============================================================================
% CONSTRAINT STORY: animal_moral_status__property_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_animal_moral_status__property_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: animal_moral_status__property_reading
 *   human_readable: Animal Moral Status: Property Reading
 *   domain: applied_ethics/animal_studies/legal_philosophy
 *
 * SUMMARY:
 *   This constraint instantiates the property reading of the
 *   animal_moral_status kernel: the legal and normative framework that
 *   classifies animals as property or resources lacking independent moral
 *   standing, whose interests are definitionally subordinate to human
 *   interests. It is presented as a fixed, almost natural baseline of legal
 *   systems, yet it carries identifiable beneficiaries (property owners and
 *   users) and excludes both animals and advocates from standing. The reading
 *   treats animal use as unconstrained except by norms of waste or
 *   inefficiency, and it historically grounds authority in legal lineage
 *   rather than empirical or moral argument.
 *
 * KEY AGENTS:
 *   - property_owners_users: Primary beneficiary (moderate/mobile) â human agents who hold title to or use animals and operate free of moral standing constraints.
 *   - legal_institutions: Agenda setter (institutional/constrained) â courts, legislatures, and common law tradition that codify and maintain the property classification.
 *   - animal_advocates: Excluded voice (organized/constrained) â welfare and rights advocates who argue for independent moral standing but lack formal weight in the property framework.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(animal_moral_status__property_reading, 0.05).
domain_priors:suppression_score(animal_moral_status__property_reading, 0.1).
domain_priors:theater_ratio(animal_moral_status__property_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(animal_moral_status__property_reading, extractiveness, 0.05).
narrative_ontology:constraint_metric(animal_moral_status__property_reading, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(animal_moral_status__property_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(animal_moral_status__property_reading, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(animal_moral_status__property_reading, resistance, 0.12).

% --- Constraint claim ---
narrative_ontology:constraint_claim(animal_moral_status__property_reading, mountain).
narrative_ontology:human_readable(animal_moral_status__property_reading, "Animal Moral Status: Property Reading").
narrative_ontology:topic_domain(animal_moral_status__property_reading, "applied_ethics/animal_studies/legal_philosophy").

domain_priors:emerges_naturally(animal_moral_status__property_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(animal_moral_status__property_reading, '4da0a530-3ae5-4a3b-8ea2-bd9903d39de9').
narrative_ontology:cs_kernel_codification('4da0a530-3ae5-4a3b-8ea2-bd9903d39de9', fixed_text).
narrative_ontology:cs_authority_grounding('4da0a530-3ae5-4a3b-8ea2-bd9903d39de9', lineage).
narrative_ontology:cs_interpretation_layer_present('4da0a530-3ae5-4a3b-8ea2-bd9903d39de9').
narrative_ontology:cs_reading_relation('4da0a530-3ae5-4a3b-8ea2-bd9903d39de9', animal_moral_status__abolitionist_reading, forecloses).
narrative_ontology:cs_reading_relation('4da0a530-3ae5-4a3b-8ea2-bd9903d39de9', animal_moral_status__welfare_reading, coexists_with).
narrative_ontology:cs_axiom('4da0a530-3ae5-4a3b-8ea2-bd9903d39de9', foundational, animals_lack_independent_moral_standing).
narrative_ontology:cs_axiom_status(animals_lack_independent_moral_standing, holdable).
narrative_ontology:cs_axiom_grounding('4da0a530-3ae5-4a3b-8ea2-bd9903d39de9', animals_lack_independent_moral_standing, conventional).
narrative_ontology:cs_reference_frame('4da0a530-3ae5-4a3b-8ea2-bd9903d39de9', anthropocentric_property_baseline).
narrative_ontology:cs_drift_state('4da0a530-3ae5-4a3b-8ea2-bd9903d39de9', contemporary_animal_ethics_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('4da0a530-3ae5-4a3b-8ea2-bd9903d39de9', '').
narrative_ontology:cs_kernel_id(animal_moral_status__property_reading, animal_moral_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(animal_moral_status__property_reading, property_owners_users).
narrative_ontology:constraint_vindicates(animal_moral_status__property_reading, anthropocentric_legal_supremacy).
narrative_ontology:constraint_vindicates(animal_moral_status__property_reading, property_rights_continuity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold legal title to animals or use them as resources in agriculture, research, companionship, or industry. Their control is constrained chiefly by efficiency and waste norms, not by independent moral claims of the animals. They benefit from clear title, transferability, and the absence of legal standing for animal interests.
narrative_ontology:constraint_stakeholder(animal_moral_status__property_reading, property_owners_users, beneficiary,
    moderate, biographical, mobile, national).

% Courts, legislatures, and administrative bodies that codify animals as property through statutes, precedent, and regulatory categories. They maintain the classification by interpreting chattel and bailment doctrines, absorbing challenges into narrow welfare exceptions without revising the property kernel.
narrative_ontology:constraint_stakeholder(animal_moral_status__property_reading, legal_institutions, agenda_setter,
    institutional, generational, constrained, national).

% Welfare and rights advocates who argue that sentience generates independent moral standing. They are structurally excluded from the property framework: they lack standing to sue on behalf of animals in most jurisdictions, and their normative claims are treated as external to the legal baseline.
narrative_ontology:constraint_stakeholder(animal_moral_status__property_reading, animal_advocates, excluded,
    organized, biographical, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves human-human conflicts over the control, use, and transfer of animals by assigning exclusive, legally enforceable property rights, eliminating ambiguity about ownership and recourse.
% TRANSFER_FUNCTION: Allocates exclusive control, derivative income, and disposition authority over animal bodies and labor to human owners; transfers any burden of conflicting interests onto the owner's discretionary welfare choices.
% ABSENT_VOICES: Abolitionist and deep-ecology advocates, who would argue that sentience or intrinsic value precludes property status, are excluded from legal standing; animals themselves are not party to the framework.
% DISAPPEARANCE_RATIONALE: Agricultural, pharmaceutical, companion-animal, and food economies rest on clear title and chattel status. Removing the property classification would force renegotiation of trillions in asset value, liability regimes, and contractual infrastructure; human legal and economic arrangements would reorganize around a different status framework.
% FOUNDING_PROBLEM: Preventing persistent human-human conflict over the capture, husbandry, and use of animals by establishing predictable dominion and exchange rules without requiring case-by-case moral adjudication.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians and property-law scholars attest the framework's role in settling inter-human disputes. Animal ethics scholars, comparative law experts outside the beneficiary set, and abolitionist jurists argue the founding problem has been superseded by evidence of animal sentience and cognitive complexity, corroborating the contested status from a non-beneficiary seat.
narrative_ontology:disappearance_verdict(animal_moral_status__property_reading, world_rearranges).
narrative_ontology:founding_problem_status(animal_moral_status__property_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(animal_moral_status__property_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(animal_moral_status__property_reading, 'none', 1).
narrative_ontology:epsilon_provenance(animal_moral_status__property_reading, 0.05, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(animal_moral_status__property_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(animal_moral_status__property_reading, ExtMetricName, E),
    domain_priors:suppression_score(animal_moral_status__property_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(animal_moral_status__property_reading),
    narrative_ontology:constraint_metric(animal_moral_status__property_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(animal_moral_status__property_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(animal_moral_status__property_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.05 because, within this reading, the constraint does not extract from its beneficiaries (owners); rather, it removes constraints on them. Animals are explicitly not placed in the victim set per the reading's structural delta, so the metric reflects the near-zero extraction that the framework formally encodes. Suppression is low (0.10) because the framework's persistence relies more on the totalizing accessibility collapse of alternatives (0.92) than on active coercion; resistance is low (0.12) because the framework is so deeply naturalized that opposition is marginalized. Theater ratio is low but creeping (0.05 to 0.15) as legal institutions perform maintenance of the property boundary against encroaching welfare statutes and personhood litigation.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (legal institutions) experiences the constraint as a stable, lineage-backed interpretive tradition requiring only administrative maintenance. The beneficiary seat (property owners) experiences it as freedom of action. An excluded seat (animal advocates) would experience it as totalizing exclusion. The engine should compute high divergence between these seats because the structural data (beneficiary vs. excluded, divergent power/exit) are asymmetrical.
 *
 * DIRECTIONALITY LOGIC:
 *   Property owners/users are declared beneficiaries, placing their directionality near the subsidy end (low d); they collect freedom of use and legal protection of title. Legal institutions are agenda setters but not beneficiaries in the rent-collecting sense; their d is moderate, reflecting administrative investment in the framework's stability. Animal advocates are excluded; their structural relationship is not captured in the victim array (per the reading's delta), but their exclusion still registers as high d should they be modeled. No victim group is declared, so the engine will not produce a full-target seat from this story alone.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâresolving human-human conflict over animal controlâmay be live in a narrow legal-positivist sense but is contested by seats that regard sentience as morally transformative. The property reading resists mandatrophy classification because it denies that its mandate has outlived its function; it treats the classification as constitutive of legal order itself. However, the drift state (authority erosion) and the creeping theater ratio suggest the framework is increasingly maintained performatively against empirical and moral challenges, a pattern the engine should monitor for piton-like inertia if extraction ever decouples from coordination entirely.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    naturalized_property_ambiguity,
    'Is the classification of animals as property an irreducible structural feature of any functional legal system, or a historically contingent construction that benefits existing human owners?',
    'Comparative legal analysis of non-Western property traditions and historical legal archaeology tracing the chattel concept''s emergence; if contingent, the mountain claim falsifies.',
    'If contingent, the FSM trigger reclassifies toward tangled_rope or snare; if irreducible, the mountain claim holds despite declared beneficiaries.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(naturalized_property_ambiguity, conceptual, 'Contingency of the property classification').

omega_variable(
    sentience_boundary_relevance,
    'Does scientific consensus on animal sentience create empirical pressure that structurally contradicts the property reading''s conventional grounding?',
    'Tracking of jurisprudence in jurisdictions that incorporate sentience findings into property law; if sentience systematically overrides property claims in adjudication, the axiom is overridden in practice.',
    'Would shift the drift_state magnitude from substantial to severe and activate axiom_overriding direction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sentience_boundary_relevance, empirical, 'Empirical challenge to conventional property axiom').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(animal_moral_status__property_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anim_tr_t0, animal_moral_status__property_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(anim_tr_t25, animal_moral_status__property_reading, theater_ratio, 25, 0.1).
narrative_ontology:measurement(anim_tr_t50, animal_moral_status__property_reading, theater_ratio, 50, 0.15).

% Extraction over time
narrative_ontology:measurement(anim_be_t0, animal_moral_status__property_reading, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(anim_be_t25, animal_moral_status__property_reading, base_extractiveness, 25, 0.05).
narrative_ontology:measurement(anim_be_t50, animal_moral_status__property_reading, base_extractiveness, 50, 0.06).

% Suppression requirement over time
narrative_ontology:measurement(anim_su_t0, animal_moral_status__property_reading, suppression_requirement, 0, 0.08).
narrative_ontology:measurement(anim_su_t25, animal_moral_status__property_reading, suppression_requirement, 25, 0.1).
narrative_ontology:measurement(anim_su_t50, animal_moral_status__property_reading, suppression_requirement, 50, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(animal_moral_status__property_reading, animal_moral_status__welfare_reading).
narrative_ontology:affects_constraint(animal_moral_status__property_reading, animal_moral_status__abolitionist_reading).

% DUAL FORMULATION NOTE:
% This constraint is one member of the animal_moral_status kernel family. The property reading, welfare reading, and abolitionist reading instantiate structurally distinct constraints from the same contested kernel, differing primarily on the question of whether animals possess independent moral standing and what legal consequences follow.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
