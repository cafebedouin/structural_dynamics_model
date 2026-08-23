% ============================================================================
% CONSTRAINT STORY: second_amendment_scope__individual_right_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_second_amendment_scope__individual_right_reading, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: second_amendment_scope__individual_right_reading
 *   human_readable: Second Amendment Individual Right Reading (Heller/Bruen Line)
 *   domain: constitutional_law/rights_jurisprudence
 *
 * SUMMARY:
 *   This constraint story models the individual right reading of the Second
 *   Amendment as crystallized in District of Columbia v. Heller (2008) and
 *   expanded in New York State Rifle & Pistol Association v. Bruen (2022).
 *   The reading holds that the Second Amendment protects an individual right
 *   to possess firearms for self-defense unconnected to militia service,
 *   triggering strict scrutiny (Heller) and a historical tradition test
 *   (Bruen) for any regulation. The claimed type is tangled_rope: the
 *   constraint coordinates a genuine function (constitutional baseline
 *   against disarmament) while extracting regulatory authority from states
 *   and safety from the public. The engine will compute per-seat
 *   classifications from the structural data authored here.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_scope__individual_right_reading, 0.65).
domain_priors:suppression_score(second_amendment_scope__individual_right_reading, 0.58).
domain_priors:theater_ratio(second_amendment_scope__individual_right_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_scope__individual_right_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(second_amendment_scope__individual_right_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(second_amendment_scope__individual_right_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_scope__individual_right_reading, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(second_amendment_scope__individual_right_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_scope__individual_right_reading, tangled_rope).
narrative_ontology:human_readable(second_amendment_scope__individual_right_reading, "Second Amendment Individual Right Reading (Heller/Bruen Line)").
narrative_ontology:topic_domain(second_amendment_scope__individual_right_reading, "constitutional_law/rights_jurisprudence").

domain_priors:requires_active_enforcement(second_amendment_scope__individual_right_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_scope__individual_right_reading, '79a60d10-55cc-4f9b-ae5d-dd84c552c797').
narrative_ontology:cs_kernel_codification('79a60d10-55cc-4f9b-ae5d-dd84c552c797', fixed_text).
narrative_ontology:cs_authority_grounding('79a60d10-55cc-4f9b-ae5d-dd84c552c797', lineage).
narrative_ontology:cs_interpretation_layer_present('79a60d10-55cc-4f9b-ae5d-dd84c552c797').
narrative_ontology:cs_reading_relation('79a60d10-55cc-4f9b-ae5d-dd84c552c797', second_amendment_scope__collective_right_reading, coexists_with).
narrative_ontology:cs_reading_relation('79a60d10-55cc-4f9b-ae5d-dd84c552c797', second_amendment_scope__civic_right_reading, coexists_with).
narrative_ontology:cs_axiom('79a60d10-55cc-4f9b-ae5d-dd84c552c797', foundational, pre_existing_natural_right_to_arms).
narrative_ontology:cs_axiom_status(pre_existing_natural_right_to_arms, holdable).
narrative_ontology:cs_axiom_grounding('79a60d10-55cc-4f9b-ae5d-dd84c552c797', pre_existing_natural_right_to_arms, deontological).
narrative_ontology:cs_axiom('79a60d10-55cc-4f9b-ae5d-dd84c552c797', secondary, strict_scrutiny_for_fundamental_rights).
narrative_ontology:cs_axiom_status(strict_scrutiny_for_fundamental_rights, holdable).
narrative_ontology:cs_axiom_grounding('79a60d10-55cc-4f9b-ae5d-dd84c552c797', strict_scrutiny_for_fundamental_rights, instrumental).
narrative_ontology:cs_reference_frame('79a60d10-55cc-4f9b-ae5d-dd84c552c797', founding_era_individual_right_understanding).
narrative_ontology:cs_drift_state('79a60d10-55cc-4f9b-ae5d-dd84c552c797', post_bruen_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('79a60d10-55cc-4f9b-ae5d-dd84c552c797', '').
narrative_ontology:cs_kernel_id(second_amendment_scope__individual_right_reading, second_amendment_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_scope__individual_right_reading, individual_citizens).
narrative_ontology:constraint_beneficiary(second_amendment_scope__individual_right_reading, gun_rights_organizations).
narrative_ontology:constraint_victim(second_amendment_scope__individual_right_reading, state_regulatory_authority).
narrative_ontology:constraint_victim(second_amendment_scope__individual_right_reading, public_safety_interests).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(second_amendment_scope__individual_right_reading, individual_citizens).
narrative_ontology:constraint_vindicates(second_amendment_scope__individual_right_reading, pre_existing_natural_right_to_arms).
narrative_ontology:constraint_vindicates(second_amendment_scope__individual_right_reading, originalist_textualism).
narrative_ontology:constraint_vindicates(second_amendment_scope__individual_right_reading, strict_scrutiny_for_fundamental_rights).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Gain constitutionally protected access to firearms for self-defense unconnected to militia service. Bear indirect costs through gun violence externalities and political polarization. Exit from the constraint's effects is constrained by national scope and identity attachment to gun ownership.
narrative_ontology:constraint_stakeholder(second_amendment_scope__individual_right_reading, individual_citizens, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(second_amendment_scope__individual_right_reading, individual_citizens, payer).

% Organize litigation, lobbying, and cultural advocacy to expand and defend the individual right reading. Collect membership dues, political influence, and cultural capital from the constraint's enforcement. Can shift strategy across jurisdictions but are structurally committed to this reading.
narrative_ontology:constraint_stakeholder(second_amendment_scope__individual_right_reading, gun_rights_organizations, beneficiary,
    organized, generational, mobile, national).

% Lose regulatory flexibility to enact gun safety measures (waiting periods, assault weapon bans, licensing regimes). Must justify any regulation under strict scrutiny or historical tradition test. Bear political costs of both regulating and failing to regulate. Cannot exit the constitutional framework.
narrative_ontology:constraint_stakeholder(second_amendment_scope__individual_right_reading, state_regulatory_authority, payer,
    institutional, generational, constrained, national).

% Bear the measurable costs of gun homicide, suicide, accidental shootings, and domestic violence lethality that correlate with firearm density. Have no institutional voice in constitutional interpretation; exit is impossible for victims of gun violence. The constraint's operation extracts safety from this group to subsidize the individual right.
narrative_ontology:constraint_stakeholder(second_amendment_scope__individual_right_reading, public_safety_interests, payer,
    moderate, biographical, trapped, national).
narrative_ontology:stakeholder_non_agent(second_amendment_scope__individual_right_reading, public_safety_interests).

% Adjudicate the scope of the right through strict scrutiny and historical tradition tests (Heller, McDonald, Bruen, Rahimi). Set the enforcement trajectory by selecting which regulations survive. Their institutional legitimacy is partially bound to the constraint's coherence.
narrative_ontology:constraint_stakeholder(second_amendment_scope__individual_right_reading, federal_courts, agenda_setter,
    institutional, generational, analytical, national).

% Advocate for regulations that the individual right reading structurally forecloses. Their preferred policy solutions (universal background checks, assault weapon bans) are ruled off the table by the constraint's current interpretation. Remain in the political conversation but are excluded from the constitutional conversation.
narrative_ontology:constraint_stakeholder(second_amendment_scope__individual_right_reading, gun_control_advocates, excluded,
    organized, biographical, constrained, national).

% Produce the historical, textual, and doctrinal arguments that feed judicial decision-making. Split between originalist, living constitutionalist, and critical approaches. Their work shapes the constraint's intellectual legitimacy but they hold no enforcement power.
narrative_ontology:constraint_stakeholder(second_amendment_scope__individual_right_reading, legal_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a constitutional baseline for individual self-defense that preempts complete disarmament by the state, solving the coordination problem of credible commitment to individual protection against tyranny and crime.
% TRANSFER_FUNCTION: Transfers regulatory authority over firearms from state legislatures to individual right-holders, enforced by courts striking down regulations that fail strict scrutiny or the historical tradition test. The transfer moves political power and risk-allocation from collective governance to individual exercise.
% ABSENT_VOICES: Victims of gun violence and their communities are structurally absent from the constitutional interpretation process; the Court's historical tradition test looks to founding-era laws (enacted by white male property holders) rather than contemporary impacted populations. Future generations who will live with the constraint's mortality externalities are also absent.
% DISAPPEARANCE_RATIONALE: If the individual right reading vanished overnight, states would immediately enact comprehensive gun safety regimes (licensing, waiting periods, bans on certain weapons/classes), the political economy of gun manufacturing would shift, and firearm homicide/suicide rates would likely decline based on international and intranational evidence. The constitutional conversation would restructure around collective or civic right frameworks.
% FOUNDING_PROBLEM: The founding generation feared a standing army and disarmed populace would enable tyranny; the Second Amendment was designed to ensure the people could constitute a militia as a check on federal power, with individual arms possession as the necessary predicate.
% FOUNDING_PROBLEM_CORROBORATION: Originalist scholars (e.g., Scalia in Heller, Amar, Volokh) attest the founding problem was individual arms for militia readiness; living constitutionalist scholars (e.g., Stevens dissent, Waldman, Cornell) and historians (e.g., Rakove, Cornell) attest the founding problem was collective militia preservation against federal overreach, not individual self-defense. No consensus exists outside the benefiting parties (gun rights advocates).
narrative_ontology:disappearance_verdict(second_amendment_scope__individual_right_reading, world_rearranges).
narrative_ontology:founding_problem_status(second_amendment_scope__individual_right_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_scope__individual_right_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(second_amendment_scope__individual_right_reading, 'none', 1).
narrative_ontology:epsilon_provenance(second_amendment_scope__individual_right_reading, 0.65, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_scope__individual_right_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(second_amendment_scope__individual_right_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(second_amendment_scope__individual_right_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.65) reflects the broad coverage (all individuals, all firearm types in common use) and the high bar for regulation (strict scrutiny/historical tradition). Suppression (0.58) captures judicial invalidation of democratically enacted safety laws. Theater ratio (0.28) acknowledges performative originalism — the historical tradition test is applied selectively (e.g., Bruen's treatment of colonial vs. Reconstruction-era laws). Accessibility collapse (0.52) is moderate: states retain some regulatory space (background checks, prohibitions on felons/mentally ill, sensitive places) but the constraint's logic pushes toward further collapse. Resistance (0.72) is high: state non-compliance, circuit splits, scholarly critique, and public opinion majorities for stronger regulation.
 *
 * PERSPECTIVAL GAP:
 *   From the individual_citizen seat, the constraint appears as a protective mountain (right against tyranny). From state_regulatory_authority, it appears as a snare (extraction of police power). From public_safety_interests, it appears as a snare with trapped exit. From federal_courts, it appears as a rope (coordination of constitutional doctrine) with rising theater. The engine computes these divergences from the single structural dataset — the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Individual citizens are beneficiaries (d ~0.2) but also payers through externalized violence costs. Gun rights organizations are pure beneficiaries (d ~0.1) with mobile exit. State regulatory authority is the primary payer (d ~0.9) — institutionally trapped, bears full enforcement cost. Public safety interests are trapped payers (d ~1.0) with zero exit. Courts are agenda_setters with analytical exit (d ~0.5). Gun control advocates are excluded — their exclusion is the constraint's enforcement mechanism. The directionality derivation from beneficiary/victim declarations plus exit options produces this gradient.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (tyranny prevention via armed populace) is contested as live vs. dead. If dead, the constraint persists as pure extraction (piton/snare). If live, it remains tangled_rope. The mandated function (militia readiness) has atrophied — no state uses unorganized militia for defense — but the constraint expanded rather than sunset. This is mandatrophy: the arrangement outlived its function and metastasized into a broader individual right. The founding_problem_status=contested and disappearance_verdict=world_rearranges together flag this as a capture/zombie candidate.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'Is this constraint one reading of the second_amendment_scope kernel, and does the kernel structure require that sibling readings be modeled as separate constraints rather than measurement variations?',
    'Apply the ε-invariance test: if collective_right_reading and civic_right_reading yield structurally different ε, beneficiary/victim sets, and claimed_types, they are separate constraints. The BGS decomposition precedent applies.',
    'If confirmed, the three readings form a constraint family linked by network.affects_constraints. The individual_right_reading''s high ε is reading-indexed, not topic-indexed — the collective reading would author near-zero ε for the same referent.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Commitment to kernel/reading decomposition per DP-001 ε-invariance principle.').

omega_variable(
    historical_tradition_test_coherence,
    'Does the Bruen historical tradition test function as a genuine coordination mechanism (identifying legitimate regulations) or as an extraction mechanism (delegitimizing modern regulations by anchoring to founding-era exclusions)?',
    'Empirical survey of post-Bruen lower court decisions: if regulations analogous to founding-era laws are upheld while novel regulations are struck down, the test coordinates; if even founding-era analogues are rejected or the test is applied asymmetrically, it extracts.',
    'If extraction mechanism, the constraint''s theater_ratio is understated and suppression is higher — the test is cover for judicial veto. If coordination mechanism, the constraint''s claimed tangled_rope classification is descriptively accurate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(historical_tradition_test_coherence, empirical, 'Whether the historical tradition test coordinates or extracts.').

omega_variable(
    public_safety_externalities_measurement,
    'What is the causal magnitude of the individual right reading''s contribution to firearm homicide, suicide, and domestic violence lethality rates, controlling for other factors?',
    'Natural experiments from state-level variation post-Bruen, synthetic control studies comparing similar states with different regulatory regimes, and international comparison with comparable nations lacking individual right constitutional protections.',
    'If causal magnitude is large, public_safety_interests extraction is severe and the constraint trends toward snare. If small, the extraction is modest and tangled_rope holds. This directly affects the victim set''s weight in χ computation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(public_safety_externalities_measurement, empirical, 'Magnitude of safety externalities extracted from public_safety_interests.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of gun control advocacy structural (judicial invalidation, preemption) or internalized (chilling effect on legislators, Overton window shift)?',
    'Track legislative proposal rates and enactment rates pre/post-Heller/Bruen in states with different political compositions. If proposal rates drop without judicial action, internalized suppression is present.',
    'If internalized, effective suppression exceeds the structural measure — the constraint extracts compliance before enforcement. This would increase the constraint''s classification severity for the state_regulatory_authority and gun_control_advocates seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression of regulatory alternatives.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_scope__individual_right_reading, 0, 230).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sa_indiv_tr_t0, second_amendment_scope__individual_right_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(sa_indiv_tr_t50, second_amendment_scope__individual_right_reading, theater_ratio, 50, 0.12).
narrative_ontology:measurement(sa_indiv_tr_t100, second_amendment_scope__individual_right_reading, theater_ratio, 100, 0.15).
narrative_ontology:measurement(sa_indiv_tr_t150, second_amendment_scope__individual_right_reading, theater_ratio, 150, 0.2).
narrative_ontology:measurement(sa_indiv_tr_t200, second_amendment_scope__individual_right_reading, theater_ratio, 200, 0.25).
narrative_ontology:measurement(sa_indiv_tr_t230, second_amendment_scope__individual_right_reading, theater_ratio, 230, 0.28).

% Extraction over time
narrative_ontology:measurement(sa_indiv_be_t0, second_amendment_scope__individual_right_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(sa_indiv_be_t50, second_amendment_scope__individual_right_reading, base_extractiveness, 50, 0.18).
narrative_ontology:measurement(sa_indiv_be_t100, second_amendment_scope__individual_right_reading, base_extractiveness, 100, 0.22).
narrative_ontology:measurement(sa_indiv_be_t150, second_amendment_scope__individual_right_reading, base_extractiveness, 150, 0.35).
narrative_ontology:measurement(sa_indiv_be_t200, second_amendment_scope__individual_right_reading, base_extractiveness, 200, 0.52).
narrative_ontology:measurement(sa_indiv_be_t230, second_amendment_scope__individual_right_reading, base_extractiveness, 230, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(sa_indiv_su_t0, second_amendment_scope__individual_right_reading, suppression_requirement, 0, 0.1).
narrative_ontology:measurement(sa_indiv_su_t50, second_amendment_scope__individual_right_reading, suppression_requirement, 50, 0.15).
narrative_ontology:measurement(sa_indiv_su_t100, second_amendment_scope__individual_right_reading, suppression_requirement, 100, 0.25).
narrative_ontology:measurement(sa_indiv_su_t150, second_amendment_scope__individual_right_reading, suppression_requirement, 150, 0.35).
narrative_ontology:measurement(sa_indiv_su_t200, second_amendment_scope__individual_right_reading, suppression_requirement, 200, 0.48).
narrative_ontology:measurement(sa_indiv_su_t230, second_amendment_scope__individual_right_reading, suppression_requirement, 230, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_scope__individual_right_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(second_amendment_scope__individual_right_reading, 0.12).
narrative_ontology:affects_constraint(second_amendment_scope__individual_right_reading, second_amendment_scope__collective_right_reading).
narrative_ontology:affects_constraint(second_amendment_scope__individual_right_reading, second_amendment_scope__civic_right_reading).
narrative_ontology:affects_constraint(second_amendment_scope__individual_right_reading, commerce_clause_firearms_regulation).
narrative_ontology:affects_constraint(second_amendment_scope__individual_right_reading, fourteenth_amendment_incorporation).

% DUAL FORMULATION NOTE:
% This constraint decomposes the 'Second Amendment scope' natural-language concept into three structurally distinct readings per the ε-invariance principle. The individual_right_reading has substantially higher extractiveness (0.65) than the collective_right_reading would (near 0.05, as it empowers states) and the civic_right_reading (moderate ~0.35, as it conditions rights on service). All three share the fixed_text kernel_codification but diverge on authority_grounding and axioms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(second_amendment_scope__individual_right_reading, institutional, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
