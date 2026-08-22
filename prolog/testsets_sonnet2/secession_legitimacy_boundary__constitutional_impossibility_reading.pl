% ============================================================================
% CONSTRAINT STORY: secession_legitimacy_boundary__constitutional_impossibility_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-14
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_secession_legitimacy_boundary__constitutional_impossibility_reading, []).

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
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: secession_legitimacy_boundary__constitutional_impossibility_reading
 *   human_readable: Constitutional Impossibility Reading of the Secession Legitimacy Boundary
 *   domain: political/federalism/constitutional_law
 *
 * SUMMARY:
 *   A federation confronts a provincial secession movement backed by a
 *   referendum. This story authors the constitutional-impossibility reading:
 *   unilateral secession is void; the only legitimate exit channel is a
 *   negotiated constitutional amendment requiring broad inter-provincial
 *   consent. From this reading's own lights, the federal government is not
 *   extracting anything from the province — it is maintaining the single
 *   legal channel that all provinces, including the one seeking exit, are
 *   equally bound by. The metrics reflect low but non-zero extraction: the
 *   enforcement of the impossibility doctrine does impose real costs (denial
 *   of legal standing, litigation, in extreme cases coercive measures) even
 *   though this reading denies those costs constitute extraction proper.
 *   Rising suppression over the interval reflects hardening federal
 *   enforcement posture and court precedent accretion as separatist
 *   mobilization persists, not an admission that the doctrine is extractive.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(secession_legitimacy_boundary__constitutional_impossibility_reading, 0.28).
domain_priors:suppression_score(secession_legitimacy_boundary__constitutional_impossibility_reading, 0.45).
domain_priors:theater_ratio(secession_legitimacy_boundary__constitutional_impossibility_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(secession_legitimacy_boundary__constitutional_impossibility_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__constitutional_impossibility_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__constitutional_impossibility_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(secession_legitimacy_boundary__constitutional_impossibility_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__constitutional_impossibility_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(secession_legitimacy_boundary__constitutional_impossibility_reading, rope).
narrative_ontology:human_readable(secession_legitimacy_boundary__constitutional_impossibility_reading, "Constitutional Impossibility Reading of the Secession Legitimacy Boundary").
narrative_ontology:topic_domain(secession_legitimacy_boundary__constitutional_impossibility_reading, "political/federalism/constitutional_law").

domain_priors:requires_active_enforcement(secession_legitimacy_boundary__constitutional_impossibility_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(secession_legitimacy_boundary__constitutional_impossibility_reading, 'a992f531-e2be-4459-94fd-6075e1269831').
narrative_ontology:cs_kernel_codification('a992f531-e2be-4459-94fd-6075e1269831', fixed_text).
narrative_ontology:cs_authority_grounding('a992f531-e2be-4459-94fd-6075e1269831', lineage).
narrative_ontology:cs_interpretation_layer_present('a992f531-e2be-4459-94fd-6075e1269831').
narrative_ontology:cs_reading_relation('a992f531-e2be-4459-94fd-6075e1269831', secession_legitimacy_boundary__popular_sovereignty_reading, forecloses).
narrative_ontology:cs_reading_relation('a992f531-e2be-4459-94fd-6075e1269831', secession_legitimacy_boundary__grievance_threshold_reading, coexists_with).
narrative_ontology:cs_reading_relation('a992f531-e2be-4459-94fd-6075e1269831', secession_legitimacy_boundary__treaty_primacy_reading, influences).
narrative_ontology:cs_axiom('a992f531-e2be-4459-94fd-6075e1269831', foundational, constitutional_text_exhausts_legitimate_exit_channels).
narrative_ontology:cs_axiom_status(constitutional_text_exhausts_legitimate_exit_channels, holdable).
narrative_ontology:cs_axiom_grounding('a992f531-e2be-4459-94fd-6075e1269831', constitutional_text_exhausts_legitimate_exit_channels, conventional).
narrative_ontology:cs_axiom('a992f531-e2be-4459-94fd-6075e1269831', foundational, referendum_mandate_confers_no_independent_legal_standing).
narrative_ontology:cs_axiom_status(referendum_mandate_confers_no_independent_legal_standing, holdable).
narrative_ontology:cs_axiom_grounding('a992f531-e2be-4459-94fd-6075e1269831', referendum_mandate_confers_no_independent_legal_standing, conventional).
narrative_ontology:cs_axiom('a992f531-e2be-4459-94fd-6075e1269831', secondary, federal_negotiation_duty_upon_clear_mandate).
narrative_ontology:cs_axiom_status(federal_negotiation_duty_upon_clear_mandate, holdable).
narrative_ontology:cs_axiom_grounding('a992f531-e2be-4459-94fd-6075e1269831', federal_negotiation_duty_upon_clear_mandate, instrumental).
narrative_ontology:cs_reference_frame('a992f531-e2be-4459-94fd-6075e1269831', federal_constitutional_supremacy_founding_settlement).
narrative_ontology:cs_drift_state('a992f531-e2be-4459-94fd-6075e1269831', contemporary_referendum_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('a992f531-e2be-4459-94fd-6075e1269831', '').
narrative_ontology:cs_kernel_id(secession_legitimacy_boundary__constitutional_impossibility_reading, secession_legitimacy_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(secession_legitimacy_boundary__constitutional_impossibility_reading, federal_government).
narrative_ontology:constraint_beneficiary(secession_legitimacy_boundary__constitutional_impossibility_reading, constitutional_order_stability_interest).
narrative_ontology:constraint_beneficiary(secession_legitimacy_boundary__constitutional_impossibility_reading, non_seceding_provinces).
narrative_ontology:constraint_vindicates(secession_legitimacy_boundary__constitutional_impossibility_reading, constitutional_supremacy_doctrine).
narrative_ontology:constraint_vindicates(secession_legitimacy_boundary__constitutional_impossibility_reading, peaceful_amendment_channel_exclusivity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers and enforces the constitutional order, including the rule that unilateral secession is void ab initio and that only a negotiated constitutional amendment process can alter the federation's territorial composition. Litigates against unilateral declarations, can invoke courts and, in extremis, coercive federal power to preserve the union. Frames this not as extraction but as maintenance of a rule every province is also bound by.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__constitutional_impossibility_reading, federal_government, agenda_setter,
    institutional, generational, analytical, national).

% Holds or seeks a provincial mandate (often via referendum) to leave the federation. Under this reading, their unilateral declaration has no legal standing regardless of the referendum margin; their only legitimate path is to petition for a negotiated constitutional amendment, which requires supermajority consent from other provinces that have no incentive to grant it. From their own reading (not this one) this operates as a structural veto, but that claim is not the referent of this story.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__constitutional_impossibility_reading, separatist_provincial_movement, excluded,
    organized, biographical, constrained, regional).

% Benefit from the stability, shared fiscal transfers, and territorial integrity the impossibility rule protects. Retain effective veto power over any amendment path, since consent thresholds require their agreement. Their exit from the federation is not blocked by anything but their own preference — they are not targets of this constraint at all.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__constitutional_impossibility_reading, non_seceding_provinces, beneficiary,
    organized, generational, mobile, national).

% Adjudicates disputes over the constitutionality of secession attempts, historically ruling that unilateral secession has no basis in constitutional or international law absent a negotiated process, while also affirming (in some rulings) a reciprocal federal duty to negotiate in good faith if a clear expression of provincial will exists. Sits inside the same constitutional order it interprets.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__constitutional_impossibility_reading, constitutional_court, agenda_setter,
    institutional, civilizational, analytical, national).
narrative_ontology:stakeholder_secondary_role(secession_legitimacy_boundary__constitutional_impossibility_reading, constitutional_court, observer).

% Populations within the would-be seceding province whose local economies are tied to resource extraction or federal transfer programs. Under this reading they have no independent standing in the secession question at all — their interests are treated as folded into the provincial referendum outcome, not separately weighed, and they cannot exit either the province or the federation on their own initiative.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__constitutional_impossibility_reading, resource_dependent_hinterland_communities, excluded,
    powerless, biographical, trapped, local).

% Analyze whether the impossibility doctrine is a coherent reading of constitutional text and history or a post-hoc consolidation of federal power. Some corroborate the doctrine as textually grounded; others argue it forecloses a genuine right to self-determination that international law recognizes in other contexts.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__constitutional_impossibility_reading, constitutional_law_scholars, observer,
    analytical, civilizational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, predictable, non-violent channel for any territorial reconfiguration of the federation, preventing ad hoc unilateral declarations from destabilizing currency, debt allocation, defense arrangements, and citizenship for the entire population, not just the seceding region.
% TRANSFER_FUNCTION: Under this reading nothing is extracted from the would-be seceding province by the federal government — the rule simply withholds legal recognition from a declared exit that has not passed through the amendment process. What moves is legal validity itself: unilateral declarations are denied standing, and negotiating leverage is allocated to whichever body (federal government, other provinces) must consent to the amendment.
% ABSENT_VOICES: The separatist movement's own reading (that federal action itself crosses an injustice threshold) is not heard here — this story authors the federal-order reading only. Resource-dependent hinterland communities inside the seceding province, and any indigenous treaty holders whose land the province's boundary encloses, have no independent voice in the amendment process this reading describes.
% DISAPPEARANCE_RATIONALE: If the impossibility rule vanished, unilateral declarations would have to be litigated or resolved through raw political and possibly military contest rather than a settled legal channel; currency, debt, and defense arrangements for the remaining federation would become immediately contestable at the moment of any provincial declaration, and every prior negotiated-exit precedent would lose its legal anchor.
% FOUNDING_PROBLEM: Federations historically faced the problem that a province declaring independence unilaterally could trigger civil conflict, contested debt and currency arrangements, and international non-recognition chaos; the constitutional impossibility doctrine was built to route any territorial change through a channel that preserves order and requires broad consent.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional courts outside the federal executive (in rulings from multiple federations) have independently affirmed the doctrine while also imposing a reciprocal duty on the federal government to negotiate if a clear provincial mandate exists — an external judicial corroboration that the founding problem (avoiding unilateral rupture and its downstream chaos) remains live, though those same courts note the doctrine does not resolve whether federal negotiating good faith is itself ever tested.
narrative_ontology:disappearance_verdict(secession_legitimacy_boundary__constitutional_impossibility_reading, world_rearranges).
narrative_ontology:founding_problem_status(secession_legitimacy_boundary__constitutional_impossibility_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(secession_legitimacy_boundary__constitutional_impossibility_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(secession_legitimacy_boundary__constitutional_impossibility_reading, 'none', 1).
narrative_ontology:epsilon_provenance(secession_legitimacy_boundary__constitutional_impossibility_reading, 0.28, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(secession_legitimacy_boundary__constitutional_impossibility_reading_tests).
:- end_tests(secession_legitimacy_boundary__constitutional_impossibility_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low (0.28 at interval end) because, by this reading's own terms, no rents are being collected from the province — the federal government gains no transfer, it withholds recognition, which this reading treats as principled non-recognition rather than extraction. Suppression is moderate and rising (0.45) because active enforcement — litigation, non-recognition of referenda, potential use of federal police powers — is real and intensifying as separatist mobilization continues; suppression here is the coercive backbone required to make the impossibility doctrine hold, independent of whether extraction is present. Accessibility collapse is moderately high (0.62): once a court affirms the doctrine, unilateral paths are foreclosed as a matter of law, though the negotiated-amendment path remains formally open (hence not mountain-level collapse). Resistance is substantial (0.55) — the separatist movement and its referendum mandate constitute genuine organized resistance to the doctrine's application.
 *
 * DIRECTIONALITY LOGIC:
 *   Under this reading, the federal government and non-seceding provinces are the structural beneficiaries: they retain the stability, fiscal transfers, and effective veto power the doctrine protects, at low d. The separatist movement bears the practical cost of non-recognition but is authored here without victim status — its exclusion from the beneficiaries/victims arrays reflects that this reading treats the extraction claim itself as invalid; the movement is captured instead as an excluded stakeholder whose objection is heard in the six_questions layer, not as a payer role, which would import the rival reading's premise into this story.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (avoiding unilateral rupture and its cascading currency/debt/defense chaos) is authored as live, corroborated by constitutional courts external to the federal executive. This blocks a mandatrophy reading in which the doctrine is pure inertia — courts outside the benefiting federal executive still find the problem operative. However, the corroborating courts themselves flag an unresolved reciprocal duty (federal good-faith negotiation) that this reading does not test, leaving room for the doctrine to harden into something closer to a one-way veto if that duty is never enforced against the federal government.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    impossibility_doctrine_natural_vs_constructed,
    'Is the constitutional impossibility doctrine a genuine logical entailment of federal constitutional structure, or a constructed legal consolidation that happens to also serve the federal government''s and non-seceding provinces'' interest in territorial stability?',
    'Comparative constitutional analysis across federations with and without explicit secession clauses; historical analysis of whether the doctrine predates or postdates episodes where it served federal interests.',
    'If genuinely entailed by federal structure, the low extraction score is well-grounded. If substantially constructed to serve federal/non-seceding-province interests, this reading may itself function closer to a false-summit mountain dressed as settled constitutional logic — beneficiaries are already declared, which is why this story includes the required FSM-adjacent omega even though claimed_type here is rope, not mountain.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(impossibility_doctrine_natural_vs_constructed, conceptual, 'Whether the impossibility doctrine is natural constitutional logic or constructed federal-interest consolidation.').

omega_variable(
    reciprocal_negotiation_duty_enforceability,
    'Is the federal government''s reciprocal duty to negotiate in good faith upon a clear provincial mandate actually enforceable, or is it judicial rhetoric with no remedy if breached?',
    'Track whether any federation has actually compelled federal negotiation against federal preference following a court''s affirmation of this duty; absence of any enforced instance after repeated triggering events would indicate the duty is rhetorical.',
    'If unenforceable, the impossibility doctrine functions as a one-way veto for the federal government despite this reading''s claim of mutual constraint, which would push the computed classification for the separatist-facing seat toward extractive despite the authored low ε.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reciprocal_negotiation_duty_enforceability, empirical, 'Whether the doctrine''s reciprocal negotiation duty has any enforceable teeth.').

omega_variable(
    kernel_reading_selection_ambiguity,
    'Given that all four kernel readings (constitutional_impossibility, popular_sovereignty, grievance_threshold, treaty_primacy) are simultaneously held by different real-world parties to the same dispute, is there a principled way to determine which reading a neutral external observer (e.g., an international tribunal) would apply, or is reading selection itself a political act with no neutral resolution?',
    'Survey international law precedent (e.g., ICJ advisory opinions on unilateral declarations of independence) for whether a dominant reading has been adopted trans-nationally, versus remaining jurisdiction-specific.',
    'If international law converges on one reading, that reading''s ε and classification would carry more claim to being the ''real'' structural fact; if it remains genuinely plural, all four readings persist as equally valid constraint instantiations with no meta-reading available.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection_ambiguity, conceptual, 'Whether kernel-reading selection across the secession legitimacy boundary is resolvable or irreducibly political.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(secession_legitimacy_boundary__constitutional_impossibility_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sece_tr_t0, secession_legitimacy_boundary__constitutional_impossibility_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(sece_tr_t8, secession_legitimacy_boundary__constitutional_impossibility_reading, theater_ratio, 8, 0.1).
narrative_ontology:measurement(sece_tr_t16, secession_legitimacy_boundary__constitutional_impossibility_reading, theater_ratio, 16, 0.11).
narrative_ontology:measurement(sece_tr_t24, secession_legitimacy_boundary__constitutional_impossibility_reading, theater_ratio, 24, 0.13).
narrative_ontology:measurement(sece_tr_t32, secession_legitimacy_boundary__constitutional_impossibility_reading, theater_ratio, 32, 0.14).
narrative_ontology:measurement(sece_tr_t40, secession_legitimacy_boundary__constitutional_impossibility_reading, theater_ratio, 40, 0.15).

% Extraction over time
narrative_ontology:measurement(sece_be_t0, secession_legitimacy_boundary__constitutional_impossibility_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(sece_be_t8, secession_legitimacy_boundary__constitutional_impossibility_reading, base_extractiveness, 8, 0.2).
narrative_ontology:measurement(sece_be_t16, secession_legitimacy_boundary__constitutional_impossibility_reading, base_extractiveness, 16, 0.22).
narrative_ontology:measurement(sece_be_t24, secession_legitimacy_boundary__constitutional_impossibility_reading, base_extractiveness, 24, 0.25).
narrative_ontology:measurement(sece_be_t32, secession_legitimacy_boundary__constitutional_impossibility_reading, base_extractiveness, 32, 0.27).
narrative_ontology:measurement(sece_be_t40, secession_legitimacy_boundary__constitutional_impossibility_reading, base_extractiveness, 40, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(sece_su_t0, secession_legitimacy_boundary__constitutional_impossibility_reading, suppression_requirement, 0, 0.32).
narrative_ontology:measurement(sece_su_t8, secession_legitimacy_boundary__constitutional_impossibility_reading, suppression_requirement, 8, 0.35).
narrative_ontology:measurement(sece_su_t16, secession_legitimacy_boundary__constitutional_impossibility_reading, suppression_requirement, 16, 0.38).
narrative_ontology:measurement(sece_su_t24, secession_legitimacy_boundary__constitutional_impossibility_reading, suppression_requirement, 24, 0.41).
narrative_ontology:measurement(sece_su_t32, secession_legitimacy_boundary__constitutional_impossibility_reading, suppression_requirement, 32, 0.43).
narrative_ontology:measurement(sece_su_t40, secession_legitimacy_boundary__constitutional_impossibility_reading, suppression_requirement, 40, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(secession_legitimacy_boundary__constitutional_impossibility_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__constitutional_impossibility_reading, popular_sovereignty_reading).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__constitutional_impossibility_reading, grievance_threshold_reading).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__constitutional_impossibility_reading, treaty_primacy_reading).

% DUAL FORMULATION NOTE:
% This story is one of four linked constraints decomposing the natural-language concept 'the legitimacy of secession' per the ε-invariance principle. Each reading (constitutional_impossibility, popular_sovereignty, grievance_threshold, treaty_primacy) authors a structurally distinct claim with its own ε, beneficiary/victim structure, and classification, sharing the same kernel_id (secession_legitimacy_boundary) but never merged into one story. This reading authors the lowest ε of the four because it denies the extraction premise the other three readings assert to varying degrees; popular_sovereignty_reading and grievance_threshold_reading are expected to author substantially higher ε against the federal government as agenda_setter, and treaty_primacy_reading introduces an entirely separate victim set (treaty-holding indigenous nations) not named in this story at all.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
