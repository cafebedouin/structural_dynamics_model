% ============================================================================
% CONSTRAINT STORY: udhr_article_3__positive_entitlement_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_udhr_article_3__positive_entitlement_reading, []).

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
 *   constraint_id: udhr_article_3__positive_entitlement_reading
 *   human_readable: UDHR Article 3 — Positive Entitlement Reading (State-Provided Material Security)
 *   domain: constitutional_law/human_rights/political_philosophy
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the contested UDHR Article 3
 *   kernel — the positive-entitlement reading, under which 'life, liberty and
 *   security of person' is read to obligate affirmative state provision of
 *   welfare, healthcare, and housing, and to license restrictions on property
 *   and expression where those conflict with the dignity/security interests
 *   of vulnerable groups. This is not a claim about Article 3 generally; the
 *   negative-liberty reading (state forbearance from deprivation) and the
 *   procedural-hybrid reading (due-process floor without resolving the
 *   substantive contest) are separate constraints with their own epsilon
 *   values, authored as sibling stories and linked via
 *   network.affects_constraints. Under this reading alone, epsilon is
 *   substantial and rising: constitutionalized welfare guarantees create
 *   durable transfer obligations enforced by courts and administered by
 *   bureaucracies whose scope has expanded steadily since 1948.
 *
 * KEY AGENTS:
 *   - low_income_households: primary beneficiary (powerless/trapped) — receives transfers and services as constitutional entitlement
 *   - welfare_administering_bureaucracies: agenda-setter (institutional/analytical) — defines and enforces the scope of the entitlement
 *   - high_net_worth_taxpayers: primary payer (powerful/mobile) — funds the redistribution, retains partial exit via jurisdictional arbitrage
 *   - private_property_holders and dissenting_speech_actors: secondary payers (moderate/constrained) — bear regulatory and expressive costs specific to this reading's structural delta
 *   - constitutional_courts: analytical observer — adjudicates the boundary against sibling readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(udhr_article_3__positive_entitlement_reading, 0.68).
domain_priors:suppression_score(udhr_article_3__positive_entitlement_reading, 0.57).
domain_priors:theater_ratio(udhr_article_3__positive_entitlement_reading, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(udhr_article_3__positive_entitlement_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(udhr_article_3__positive_entitlement_reading, suppression_requirement, 0.57).
narrative_ontology:constraint_metric(udhr_article_3__positive_entitlement_reading, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(udhr_article_3__positive_entitlement_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(udhr_article_3__positive_entitlement_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(udhr_article_3__positive_entitlement_reading, tangled_rope).
narrative_ontology:human_readable(udhr_article_3__positive_entitlement_reading, "UDHR Article 3 — Positive Entitlement Reading (State-Provided Material Security)").
narrative_ontology:topic_domain(udhr_article_3__positive_entitlement_reading, "constitutional_law/human_rights/political_philosophy").

domain_priors:requires_active_enforcement(udhr_article_3__positive_entitlement_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(udhr_article_3__positive_entitlement_reading, '479a14b4-3423-4e2d-93da-9f582d0d2866').
narrative_ontology:cs_kernel_codification('479a14b4-3423-4e2d-93da-9f582d0d2866', fixed_text).
narrative_ontology:cs_authority_grounding('479a14b4-3423-4e2d-93da-9f582d0d2866', practice).
narrative_ontology:cs_interpretation_layer_present('479a14b4-3423-4e2d-93da-9f582d0d2866').
narrative_ontology:cs_reading_relation('479a14b4-3423-4e2d-93da-9f582d0d2866', udhr_article_3__negative_liberty_reading, coexists_with).
narrative_ontology:cs_reading_relation('479a14b4-3423-4e2d-93da-9f582d0d2866', udhr_article_3__procedural_hybrid_reading, influences).
narrative_ontology:cs_axiom('479a14b4-3423-4e2d-93da-9f582d0d2866', foundational, material_deprivation_is_a_security_threat).
narrative_ontology:cs_axiom_status(material_deprivation_is_a_security_threat, holdable).
narrative_ontology:cs_axiom_grounding('479a14b4-3423-4e2d-93da-9f582d0d2866', material_deprivation_is_a_security_threat, empirically_contingent).
narrative_ontology:cs_axiom('479a14b4-3423-4e2d-93da-9f582d0d2866', foundational, state_obligation_extends_to_affirmative_provision).
narrative_ontology:cs_axiom_status(state_obligation_extends_to_affirmative_provision, holdable).
narrative_ontology:cs_axiom_grounding('479a14b4-3423-4e2d-93da-9f582d0d2866', state_obligation_extends_to_affirmative_provision, deontological).
narrative_ontology:cs_axiom('479a14b4-3423-4e2d-93da-9f582d0d2866', secondary, group_dignity_interests_can_justify_expressive_restriction).
narrative_ontology:cs_axiom_status(group_dignity_interests_can_justify_expressive_restriction, holdable).
narrative_ontology:cs_axiom_grounding('479a14b4-3423-4e2d-93da-9f582d0d2866', group_dignity_interests_can_justify_expressive_restriction, instrumental).
narrative_ontology:cs_reference_frame('479a14b4-3423-4e2d-93da-9f582d0d2866', post_1948_dignity_and_security_synthesis).
narrative_ontology:cs_drift_state('479a14b4-3423-4e2d-93da-9f582d0d2866', contemporary_welfare_constitutionalism, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('479a14b4-3423-4e2d-93da-9f582d0d2866', '').
narrative_ontology:cs_kernel_id(udhr_article_3__positive_entitlement_reading, udhr_article_3).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(udhr_article_3__positive_entitlement_reading, low_income_households).
narrative_ontology:constraint_beneficiary(udhr_article_3__positive_entitlement_reading, chronically_ill_patients).
narrative_ontology:constraint_beneficiary(udhr_article_3__positive_entitlement_reading, unhoused_populations).
narrative_ontology:constraint_beneficiary(udhr_article_3__positive_entitlement_reading, welfare_administering_bureaucracies).
narrative_ontology:constraint_victim(udhr_article_3__positive_entitlement_reading, high_net_worth_taxpayers).
narrative_ontology:constraint_victim(udhr_article_3__positive_entitlement_reading, private_property_holders).
narrative_ontology:constraint_victim(udhr_article_3__positive_entitlement_reading, dissenting_speech_actors).
narrative_ontology:constraint_victim(udhr_article_3__positive_entitlement_reading, small_business_owners).
narrative_ontology:constraint_vindicates(udhr_article_3__positive_entitlement_reading, substantive_equality_doctrine).
narrative_ontology:constraint_vindicates(udhr_article_3__positive_entitlement_reading, social_rights_are_human_rights_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receive welfare transfers, subsidized healthcare, and housing assistance premised on Article 3's positive-entitlement reading. Depend entirely on continued state administration; have no private-market alternative to fall back on if benefits are withdrawn or reduced.
narrative_ontology:constraint_stakeholder(udhr_article_3__positive_entitlement_reading, low_income_households, beneficiary,
    powerless, biographical, trapped, national).

% Rely on state-guaranteed healthcare access as a life-and-security entitlement. Exit from the arrangement is not meaningfully available — private alternatives are priced beyond reach, which is precisely the market failure the entitlement reading is invoked to correct.
narrative_ontology:constraint_stakeholder(udhr_article_3__positive_entitlement_reading, chronically_ill_patients, beneficiary,
    powerless, biographical, trapped, national).

% The most direct claimants under the housing component of the reading. Their situation is the strongest evidentiary case for the coordination function of the arrangement — without state provision, no alternative housing mechanism exists for this group.
narrative_ontology:constraint_stakeholder(udhr_article_3__positive_entitlement_reading, unhoused_populations, beneficiary,
    powerless, immediate, trapped, local).

% Design, interpret, and enforce the entitlement obligations — set eligibility criteria, administer transfers, and adjudicate what 'material conditions necessary for life and security' means in practice. Their institutional survival and scope of authority grow with the entitlement reading's expansion.
narrative_ontology:constraint_stakeholder(udhr_article_3__positive_entitlement_reading, welfare_administering_bureaucracies, agenda_setter,
    institutional, generational, analytical, national).

% Fund the redistribution through progressive taxation used to finance welfare, healthcare, and housing guarantees. Retain some exit through jurisdictional arbitrage (relocating capital or residency) but bear direct, quantifiable transfer costs while remaining resident.
narrative_ontology:constraint_stakeholder(udhr_article_3__positive_entitlement_reading, high_net_worth_taxpayers, payer,
    powerful, biographical, mobile, national).

% Subject to rent controls, mandated housing set-asides, or expropriation-adjacent land-use rules justified by the housing component of the entitlement reading. Exit requires divesting property or relocating capital, both costly and slow relative to the pace of regulatory change.
narrative_ontology:constraint_stakeholder(udhr_article_3__positive_entitlement_reading, private_property_holders, payer,
    moderate, biographical, constrained, national).

% Face restrictions on speech framed as necessary to protect the dignity and security interests the entitlement reading extends to vulnerable groups (hate speech law, incitement doctrine expanded to cover group-vilification). Their exit option is self-censorship or litigation; the constraint's expressive-rights victim status is a direct structural delta from the negative-liberty reading.
narrative_ontology:constraint_stakeholder(udhr_article_3__positive_entitlement_reading, dissenting_speech_actors, payer,
    moderate, biographical, constrained, national).

% Bear compliance costs (minimum-provision mandates, employer-side healthcare or housing contribution requirements) that scale poorly relative to firm size compared to larger taxpayers who can absorb or arbitrage the same obligations.
narrative_ontology:constraint_stakeholder(udhr_article_3__positive_entitlement_reading, small_business_owners, payer,
    moderate, biographical, constrained, regional).

% Adjudicate the boundary between the entitlement reading and competing constitutional guarantees (property, expression). Their rulings determine how far the positive-entitlement reading can be pressed against the negative-liberty and procedural-hybrid readings without judicial correction.
narrative_ontology:constraint_stakeholder(udhr_article_3__positive_entitlement_reading, constitutional_courts, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(udhr_article_3__positive_entitlement_reading, diffuse).
narrative_ontology:fixing_cost_class(udhr_article_3__positive_entitlement_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the genuine collective-action problem of catastrophic, uninsurable individual risk (illness, homelessness, destitution) that private markets systematically underprovide for the poorest, by pooling resources through compulsory state mechanisms.
% TRANSFER_FUNCTION: Moves tax revenue, in-kind services, and regulatory burden from property holders, high earners, and regulated economic actors to low-income, unhoused, and chronically ill populations, administered and enlarged by welfare bureaucracies along the way.
% ABSENT_VOICES: Small landlords, marginal taxpayers just above assistance thresholds, and unpopular speakers restricted under group-dignity provisions are rarely represented in the drafting or adjudicating bodies that expand the entitlement reading; their objections surface mainly in litigation after the fact, not in the deliberative process that sets scope.
% DISAPPEARANCE_RATIONALE: If the positive-entitlement reading were abandoned overnight, welfare, healthcare, and housing guarantees currently treated as constitutional floors would revert to ordinary legislative discretion — vulnerable populations would lose judicially enforceable claims, administering bureaucracies would lose their constitutional mandate, and property/expression restrictions justified by the reading would face immediate constitutional challenge.
% FOUNDING_PROBLEM: Post-1948 drafters sought to prevent the material deprivation and social collapse that had enabled fascism and mass atrocity, treating freedom from want as inseparable from freedom from fear — the entitlement reading operationalizes that founding concern into enforceable state obligations.
% FOUNDING_PROBLEM_CORROBORATION: Welfare-state scholars and international human rights bodies (outside the beneficiary populations themselves) corroborate that severe material deprivation remains a live threat to security in many jurisdictions. Property-rights and civil-libertarian scholars, also outside the direct beneficiary group, corroborate that the original drafting compromise deliberately left the positive/negative distinction unresolved — meaning the entitlement reading's claim to sole fidelity to the founding problem is itself contested by informed outside observers, not merely by payers.
narrative_ontology:disappearance_verdict(udhr_article_3__positive_entitlement_reading, world_rearranges).
narrative_ontology:founding_problem_status(udhr_article_3__positive_entitlement_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(udhr_article_3__positive_entitlement_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(udhr_article_3__positive_entitlement_reading, 'none', 1).
narrative_ontology:epsilon_provenance(udhr_article_3__positive_entitlement_reading, 0.68, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(udhr_article_3__positive_entitlement_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(udhr_article_3__positive_entitlement_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(udhr_article_3__positive_entitlement_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (0.68) and rising over the interval because the entitlement reading has expanded from a founding-era minimal floor into an increasingly detailed set of enforceable transfer and regulatory obligations — courts have progressively read more specific material guarantees into the same text. Suppression (0.57) reflects the real coercive apparatus required to compel taxation, enforce rent and land-use restrictions, and penalize speech classified as dignity-harming; this is a raw structural property of enforcement machinery, not scaled by scope or power. Theater ratio (0.32) is moderate — much of the apparatus performs genuine redistribution, but a growing share of administrative activity (eligibility litigation, compliance documentation, symbolic rights-recognition without material follow-through) is process rather than delivery. Accessibility collapse is authored moderate (0.4), not mountain-level, because the negative-liberty and procedural-hybrid alternatives remain live, contested, and adopted by other jurisdictions — the entitlement reading has not foreclosed its rivals. Resistance is high (0.72): property holders, fiscal conservatives, and free-expression advocates actively litigate and legislate against the reading's expansion, which is inconsistent with a mountain and consistent with the tangled-rope claim.
 *
 * DIRECTIONALITY LOGIC:
 *   Low-income households, chronically ill patients, and unhoused populations are structural beneficiaries — the constraint subsidizes them directly and they have no meaningful exit (trapped), which the derivation chain correctly reads as low d. High-net-worth taxpayers are targets but retain partial exit (mobile via jurisdictional arbitrage), which the engine should read as high but not maximal d. Private property holders, small business owners, and dissenting speech actors are targets with constrained exit — their d should sit nearer the full-target end than the mobile taxpayer group, reflecting that relocating a business or self-censoring is costlier than relocating capital.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled-rope classification prevents two mislabeling errors: treating the entitlement reading as pure coordination (ignoring that it now imposes concentrated, non-consensual costs on identifiable payer groups) and treating it as pure extraction (ignoring that it genuinely solves an uninsurable risk-pooling problem for populations with no market alternative). Both a coordination function (beneficiaries with no substitute) and asymmetric extraction (payers who did not choose the arrangement and cannot exit cheaply) are present simultaneously, which is exactly the tangled-rope gate.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indeterminacy,
    'Does the drafting history and travaux préparatoires of Article 3 support reading ''security of person'' as encompassing material/economic security, or was the term deliberately left to the separate ICESCR framework precisely because delegates could not agree on a positive obligation?',
    'Close textual and historical analysis of the 1948 drafting committee records, particularly the split between the eventual ICCPR (civil/political) and ICESCR (economic/social) covenants, which some historians read as evidence the entitlement reading was deliberately deferred rather than embedded in Article 3 itself.',
    'If the historical record supports deliberate deferral, the positive-entitlement reading''s claim to fidelity with the founding problem weakens relative to the procedural-hybrid reading, though this would not resolve the reading''s contemporary constitutional legitimacy in jurisdictions that have independently adopted it via domestic constitutional interpretation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_indeterminacy, conceptual, 'Whether the entitlement reading is textually/historically grounded in Article 3 or is a later interpretive expansion.').

omega_variable(
    sibling_reading_resource_competition,
    'Does adoption of the positive-entitlement reading in a jurisdiction structurally crowd out or delegitimize the negative-liberty and procedural-hybrid readings within the same constitutional order, or can all three coexist as complementary layers?',
    'Comparative constitutional analysis of jurisdictions that have adopted strong positive-entitlement doctrines (e.g., South Africa, India) versus those retaining primarily negative-liberty frameworks (e.g., United States) to determine whether the readings are mutually reinforcing or mutually displacing in practice.',
    'If mutually displacing, this reading''s expansion actively forecloses political and judicial space for the negative-liberty reading rather than merely coexisting with it, which would argue for a forecloses relation rather than coexists_with in some jurisdictional contexts even though the cross-jurisdictional global pattern remains coexistence.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sibling_reading_resource_competition, empirical, 'Whether the entitlement reading''s expansion structurally displaces sibling readings within a given constitutional order.').

omega_variable(
    beneficiary_capture_of_administering_bureaucracy,
    'Is welfare_administering_bureaucracies a neutral coordination mechanism, or has the administrative apparatus itself become a self-interested beneficiary whose institutional growth outpaces the material needs it was created to address?',
    'Track administrative overhead ratio (spending on eligibility determination, compliance, and bureaucratic process) relative to direct material transfer over the measurement interval; a rising overhead share independent of caseload growth would indicate bureaucratic self-interest distinct from beneficiary need.',
    'If overhead is rising disproportionately, part of the measured extractiveness reflects bureaucratic rent rather than genuine transfer to vulnerable populations, which would argue for treating welfare_administering_bureaucracies as a second-order beneficiary class distinct from the populations it serves.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_capture_of_administering_bureaucracy, empirical, 'Whether administrative apparatus has become a self-interested beneficiary independent of the populations it serves.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(udhr_article_3__positive_entitlement_reading, 1948, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(udhr_tr_t1948, udhr_article_3__positive_entitlement_reading, theater_ratio, 1948, 0.1).
narrative_ontology:measurement(udhr_tr_t1966, udhr_article_3__positive_entitlement_reading, theater_ratio, 1966, 0.15).
narrative_ontology:measurement(udhr_tr_t1985, udhr_article_3__positive_entitlement_reading, theater_ratio, 1985, 0.2).
narrative_ontology:measurement(udhr_tr_t2000, udhr_article_3__positive_entitlement_reading, theater_ratio, 2000, 0.25).
narrative_ontology:measurement(udhr_tr_t2015, udhr_article_3__positive_entitlement_reading, theater_ratio, 2015, 0.29).
narrative_ontology:measurement(udhr_tr_t2025, udhr_article_3__positive_entitlement_reading, theater_ratio, 2025, 0.32).

% Extraction over time
narrative_ontology:measurement(udhr_be_t1948, udhr_article_3__positive_entitlement_reading, base_extractiveness, 1948, 0.2).
narrative_ontology:measurement(udhr_be_t1966, udhr_article_3__positive_entitlement_reading, base_extractiveness, 1966, 0.32).
narrative_ontology:measurement(udhr_be_t1985, udhr_article_3__positive_entitlement_reading, base_extractiveness, 1985, 0.45).
narrative_ontology:measurement(udhr_be_t2000, udhr_article_3__positive_entitlement_reading, base_extractiveness, 2000, 0.55).
narrative_ontology:measurement(udhr_be_t2015, udhr_article_3__positive_entitlement_reading, base_extractiveness, 2015, 0.63).
narrative_ontology:measurement(udhr_be_t2025, udhr_article_3__positive_entitlement_reading, base_extractiveness, 2025, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(udhr_su_t1948, udhr_article_3__positive_entitlement_reading, suppression_requirement, 1948, 0.25).
narrative_ontology:measurement(udhr_su_t1966, udhr_article_3__positive_entitlement_reading, suppression_requirement, 1966, 0.33).
narrative_ontology:measurement(udhr_su_t1985, udhr_article_3__positive_entitlement_reading, suppression_requirement, 1985, 0.4).
narrative_ontology:measurement(udhr_su_t2000, udhr_article_3__positive_entitlement_reading, suppression_requirement, 2000, 0.47).
narrative_ontology:measurement(udhr_su_t2015, udhr_article_3__positive_entitlement_reading, suppression_requirement, 2015, 0.53).
narrative_ontology:measurement(udhr_su_t2025, udhr_article_3__positive_entitlement_reading, suppression_requirement, 2025, 0.57).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(udhr_article_3__positive_entitlement_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(udhr_article_3__positive_entitlement_reading, 0.15).
narrative_ontology:affects_constraint(udhr_article_3__positive_entitlement_reading, udhr_article_3__negative_liberty_reading).
narrative_ontology:affects_constraint(udhr_article_3__positive_entitlement_reading, udhr_article_3__procedural_hybrid_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling constraints decomposing the single natural-language label 'UDHR Article 3' per the ε-invariance principle. negative_liberty_reading treats the same text as a narrow procedural bar on state deprivation (low epsilon, near-mountain in stable liberal democracies). procedural_hybrid_reading treats it as a due-process floor (habeas corpus, torture prohibition) without resolving the substantive contest (moderate epsilon, closer to rope). This positive_entitlement_reading treats it as an affirmative welfare-provision mandate (high, rising epsilon, tangled_rope). The three stories share no epsilon value and must not be averaged or reconciled — each is generated and evaluated independently, linked here for contamination and family-tracing analysis only.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
