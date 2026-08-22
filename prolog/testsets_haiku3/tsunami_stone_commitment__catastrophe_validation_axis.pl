% ============================================================================
% CONSTRAINT STORY: tsunami_stone_commitment__catastrophe_validation_axis
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_tsunami_stone_commitment__catastrophe_validation_axis, []).

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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: tsunami_stone_commitment__catastrophe_validation_axis
 *   human_readable: Tsunami Stone as Empirical Validation Mechanism
 *   domain: disaster_anthropology/commitment_systems
 *
 * SUMMARY:
 *   Japanese coastal communities, particularly in Anjo and surrounding
 *   settlements, maintain stone inscriptions warning of tsunami hazards. The
 *   stones date to unknown antiquity (some evidence suggests post-1700
 *   placement following documented tsunami deaths; others suggest earlier
 *   origins). The constraint in this reading is the STONE AND ITS EMPIRICAL
 *   ROLE AS VALIDATION MECHANISM: the inscriptions provide a binary test of
 *   whether intergenerational transmission of disaster-response commitment
 *   succeeded or failed. The 2011 Tohoku tsunami is the decisive natural
 *   experiment. Populations that respected the stones' guidance (moved to
 *   high ground) survived; populations unfamiliar with or dismissive of the
 *   stones experienced higher mortality. This reading instantiates the stones
 *   as a natural constraint (the bedrock datum) whose function is to
 *   adjudicate competing claims about whether human behavioral commitment was
 *   maintained. This reading does NOT claim the stones cause anything — it
 *   claims the stones' existence and the population's response to them
 *   constitute a testable commitment-system hypothesis. The catastrophe
 *   provided the test.
 *
 * KEY AGENTS:
 *   - Anjo tsunami stones and proximate variants — the physical commitment objects
 *   - Coastal populations 1700–2011 — the supposed transmission carriers
 *   - Intergenerational covenant thesis — the proposition the stones are hypothesized to test
 *   - Behavioral competence reading adherents — scholars interpreting stones as live behavioral mechanism
 *   - Commemorative husk reading adherents — scholars interpreting stones as degraded symbolic artifacts
 *   - 2011 tsunami event — the adjudication mechanism (natural experiment)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tsunami_stone_commitment__catastrophe_validation_axis, 0.0).
domain_priors:suppression_score(tsunami_stone_commitment__catastrophe_validation_axis, 0.0).
domain_priors:theater_ratio(tsunami_stone_commitment__catastrophe_validation_axis, 0.0).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tsunami_stone_commitment__catastrophe_validation_axis, extractiveness, 0.0).
narrative_ontology:constraint_metric(tsunami_stone_commitment__catastrophe_validation_axis, suppression_requirement, 0.0).
narrative_ontology:constraint_metric(tsunami_stone_commitment__catastrophe_validation_axis, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(tsunami_stone_commitment__catastrophe_validation_axis, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(tsunami_stone_commitment__catastrophe_validation_axis, resistance, 0.02).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tsunami_stone_commitment__catastrophe_validation_axis, mountain).
narrative_ontology:human_readable(tsunami_stone_commitment__catastrophe_validation_axis, "Tsunami Stone as Empirical Validation Mechanism").
narrative_ontology:topic_domain(tsunami_stone_commitment__catastrophe_validation_axis, "disaster_anthropology/commitment_systems").

domain_priors:emerges_naturally(tsunami_stone_commitment__catastrophe_validation_axis).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(tsunami_stone_commitment__catastrophe_validation_axis, 'fc3d6aa6-39fc-47dc-a602-0731e83f2605').
narrative_ontology:cs_kernel_codification('fc3d6aa6-39fc-47dc-a602-0731e83f2605', distributed).
narrative_ontology:cs_authority_grounding('fc3d6aa6-39fc-47dc-a602-0731e83f2605', practice).
narrative_ontology:cs_interpretation_layer_present('fc3d6aa6-39fc-47dc-a602-0731e83f2605').
narrative_ontology:cs_reading_relation('fc3d6aa6-39fc-47dc-a602-0731e83f2605', tsunami_stone_commitment__behavioral_competence_reading, influences).
narrative_ontology:cs_reading_relation('fc3d6aa6-39fc-47dc-a602-0731e83f2605', tsunami_stone_commitment__commemorative_husk_reading, influences).
narrative_ontology:cs_axiom('fc3d6aa6-39fc-47dc-a602-0731e83f2605', foundational, natural_catastrophe_is_sufficient_empirical_test).
narrative_ontology:cs_axiom_status(natural_catastrophe_is_sufficient_empirical_test, holdable).
narrative_ontology:cs_axiom_grounding('fc3d6aa6-39fc-47dc-a602-0731e83f2605', natural_catastrophe_is_sufficient_empirical_test, empirically_contingent).
narrative_ontology:cs_axiom('fc3d6aa6-39fc-47dc-a602-0731e83f2605', foundational, stones_are_commitment_apparatus_not_causal_agents).
narrative_ontology:cs_axiom_status(stones_are_commitment_apparatus_not_causal_agents, holdable).
narrative_ontology:cs_axiom_grounding('fc3d6aa6-39fc-47dc-a602-0731e83f2605', stones_are_commitment_apparatus_not_causal_agents, conventional).
narrative_ontology:cs_reference_frame('fc3d6aa6-39fc-47dc-a602-0731e83f2605', natural_law_validation_frame).
narrative_ontology:cs_drift_state('fc3d6aa6-39fc-47dc-a602-0731e83f2605', contemporary_post_2011, gap(codification_collapse, minor, true)).
narrative_ontology:cs_created_at('fc3d6aa6-39fc-47dc-a602-0731e83f2605', '').
narrative_ontology:cs_kernel_id(tsunami_stone_commitment__catastrophe_validation_axis, tsunami_stone_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(tsunami_stone_commitment__catastrophe_validation_axis, intergenerational_covenant_thesis).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(tsunami_stone_commitment__catastrophe_validation_axis, anjo_residents_2011).
narrative_ontology:constraint_beneficiary(tsunami_stone_commitment__catastrophe_validation_axis, jishin_tsunami_oral_tradition_carriers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The proposition that stone inscriptions bearing warnings about tsunami hazards were maintained as active behavioral commitments across centuries, with the 2011 tsunami providing definitive empirical confirmation that the population protected by the stones survived at elevated rates compared to unprotected cohorts — a binary test of whether commitment-system transmission persisted or failed.
narrative_ontology:constraint_stakeholder(tsunami_stone_commitment__catastrophe_validation_axis, intergenerational_covenant_thesis, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(tsunami_stone_commitment__catastrophe_validation_axis, intergenerational_covenant_thesis).

% Population in Anjo and adjacent coastal settlements where tsunami stones bore legible warnings. The 2011 event provided the test condition: populations respecting stone-inscribed guidance relocated to higher ground; those unfamiliar with or dismissive of the stones remained in vulnerable zones. Survival rates became the empirical datum.
narrative_ontology:constraint_stakeholder(tsunami_stone_commitment__catastrophe_validation_axis, anjo_residents_2011, beneficiary,
    powerless, immediate, trapped, local).

% Scholars and institutional actors who interpret tsunami stones as evidence of live behavioral transmission — that the commitment to heed warnings was maintained through intergenerational practice, not just symbolic commemoration. The 2011 tsunami provided empirical adjudication: if the stones predicted survival patterns, the reading's core premise is vindicated; if not, it requires revision.
narrative_ontology:constraint_stakeholder(tsunami_stone_commitment__catastrophe_validation_axis, behavioral_competence_reading_adherents, observer,
    organized, generational, analytical, global).

% Scholars who interpret tsunami stones as degraded symbolic artifacts — that formal compliance decayed over centuries to performative commemoration, with actual behavioral force attenuated to near-zero. The 2011 tsunami provided empirical adjudication: if survival patterns failed to show differential protection aligned with stone proximity/legibility, the reading's core premise is vindicated; if patterns do show alignment, the reading requires revision.
narrative_ontology:constraint_stakeholder(tsunami_stone_commitment__catastrophe_validation_axis, commemorative_husk_reading_adherents, observer,
    organized, generational, analytical, global).

% Community members, teachers, shrine keepers, and elderly residents who maintained oral transmission of tsunami-hazard knowledge and integrated stone inscriptions into local lore, educational curricula, and disaster-preparedness narrative. Their practice either constituted the 'active behavioral force' the competence reading posits, or was largely ceremonial performance — the distinction empirically testable through 2011 outcomes.
narrative_ontology:constraint_stakeholder(tsunami_stone_commitment__catastrophe_validation_axis, jishin_tsunami_oral_tradition_carriers, beneficiary,
    moderate, generational, constrained, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Inscribed stone warnings solved an intergenerational coordination problem: how to transmit catastrophic-hazard knowledge across generations when direct experience is rare and memory is short, using a durable physical medium (stone) to encode guidance that bypasses individual credibility and institutional decay.
% TRANSFER_FUNCTION: The constraint transfers behavioral capital — the obligation to respond to written warnings by relocating to high ground during seismic events — from literate, experienced generations to subsequent cohorts who have never witnessed a major tsunami. The transfer mechanism is institutional (shrine maintenance, oral tradition, educational curricula) and empirically testable (did the warning-respecting population survive at higher rates?).
% ABSENT_VOICES: Coastal residents displaced by development, tourism, and industrialization — populations relocated away from traditional warning-stone sites and oral-tradition carriers. Post-WWII cohorts with minimal exposure to pre-modern disaster narratives or to elders maintaining the tradition. Non-Japanese transnational residents in the 2011 inundation zone who lacked cultural embedded knowledge of the stones.
% DISAPPEARANCE_RATIONALE: If the constraint (the stone inscriptions and their intergenerational transmission mechanism) had never been established or had completely decayed before 2011, the 2011 tsunami would have struck populations distributed across the coastal plain with uniform vulnerability. The observed non-uniform mortality — lower among populations with proximate, legible warning stones — is evidence the constraint shaped behavioral response. Its disappearance would have meant no differential survival signal; the presence of the signal constitutes empirical proof the constraint operated.
% FOUNDING_PROBLEM: A coastal population living in a region of extreme earthquake and tsunami hazard, but with generational intervals between major events long enough that direct experiential learning (disaster → adaptation → teaching) cannot reliably transmit knowledge. Stone inscriptions encoded permanent, durable, non-degrading warnings legible across centuries.
% FOUNDING_PROBLEM_CORROBORATION: Geophysical evidence confirms large tsunamis strike the Anjo coast at ~100–150 year intervals; archaeological evidence shows settlements existed there for millennia; the 2011 tsunami itself PROVIDED the corroboration from outside the tradition-maintenance community — populations respecting the stones' guidance survived at elevated rates compared to those unfamiliar with them, vindicating the founding-problem diagnosis. Independent sources: geomorphological studies of prehistoric tsunami deposits, historical records of pre-1700 tsunamis, and post-2011 epidemiological analysis of mortality by proximity to stone sites.
narrative_ontology:disappearance_verdict(tsunami_stone_commitment__catastrophe_validation_axis, world_rearranges).
narrative_ontology:founding_problem_status(tsunami_stone_commitment__catastrophe_validation_axis, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(tsunami_stone_commitment__catastrophe_validation_axis, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(tsunami_stone_commitment__catastrophe_validation_axis, 'none', 1).
narrative_ontology:epsilon_provenance(tsunami_stone_commitment__catastrophe_validation_axis, 0.0, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(tsunami_stone_commitment__catastrophe_validation_axis_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(tsunami_stone_commitment__catastrophe_validation_axis, ExtMetricName, E),
    domain_priors:suppression_score(tsunami_stone_commitment__catastrophe_validation_axis, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(tsunami_stone_commitment__catastrophe_validation_axis),
    narrative_ontology:constraint_metric(tsunami_stone_commitment__catastrophe_validation_axis, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(tsunami_stone_commitment__catastrophe_validation_axis, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(tsunami_stone_commitment__catastrophe_validation_axis_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is zero because the stones extract nothing from anyone; they are inscribed warnings, not coercive mechanisms. Suppression is zero for the same reason. Theater_ratio is minimal and rises slightly over time, not because the stones' real function is theater, but because institutional maintenance of the tradition (shrine upkeep, oral storytelling, school curricula) became increasingly decoupled from direct disaster-response behavior in periods without major tsunamis. The rising theater_ratio does not indicate the stones themselves are theater — it indicates the institutional carriers of the transmission may have partially decoupled from the original function. Accessibility_collapse is very high (0.95): once a coastal resident understands a stone inscription warns of tsunami hazard, no realistic alternative interpretation exists. The physical evidence (stone placement at high elevation, inscription legibility, historical record of preserved settlements) makes the alternative frame nearly impossible to sustain. Resistance is near-zero: no organized party actively resists the stones or their message; any disagreement is interpretive, not behavioral.
 *
 * PERSPECTIVAL GAP:
 *   This reading is from the ANALYTICAL/MEASUREMENT seat. The behavioral_competence_reading and commemorative_husk_reading are from INTERPRETIVE seats (scholars defending narratives about what the commitment was). This reading does not defend either narrative — it claims the stones provide empirical arbitration between them. The gap is thus not between conflicting parties but between the measurement apparatus (this reading) and the competing hypotheses (the sibling readings).
 *
 * DIRECTIONALITY LOGIC:
 *   There is no extraction directionality in this reading because there are no beneficiary/victim seats. The 'beneficiary' declared is the intergenerational_covenant_thesis itself (the proposition), which collects nothing and cannot be extracted from. This is the only declaration appropriate under this reading because the reading's content is that the stones serve as a commitment-test apparatus, not as a coordination mechanism with parties. From the volcano's standpoint (if mountains had standpoints), it extracts nothing and benefits no one; it is a constraint of physics. Similarly, from the reading's standpoint, the stones are a physical constraint that tests a hypothesis about human behavior, not a mechanism that extracts from humans.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy — the drift of a commitment from its founding purpose to degraded instrumental or symbolic function — is precisely what THIS READING claims to MEASURE, not what it exhibits. The rising theater_ratio (institutional maintenance becoming decoupled from disaster-response behavior) is the phenomenon THIS READING uses to distinguish the behavioral_competence_reading (which denies theater_ratio rise implies functional decay) from the commemorative_husk_reading (which affirms it). The 2011 tsunami resolves the mandatrophy question empirically: if the population that integrated the stones into their behavior survived at higher rates, mandatrophy did NOT occur (the commitment remained live). If mortality was uniform, mandatrophy DID occur (the commitment had decayed).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    pre_1700_stone_attribution_ambiguity,
    'Were the tsunami stones placed before 1700 (indicating much longer transmission history and stronger prima facie evidence for behavioral competence) or after 1700 (indicating placement following a documented disaster and a shorter transmission interval)?',
    'Archaeological dating (cosmogenic isotope analysis, soil stratigraphy, lichen growth); comparison with historical records of pre-1700 tsunami damage and settlement patterns; archival evidence of stone installation dates.',
    'Pre-1700 placement supports the behavioral_competence_reading (centuries of successful transmission). Post-1700 placement suggests a more recent intervention and shorter opportunity for behavioral decay, complicating the commemorative_husk_reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pre_1700_stone_attribution_ambiguity, empirical, 'Antiquity and transmission duration of the stone-placement practice.').

omega_variable(
    id_2011_differential_survival_attribution,
    'Were the observed differences in mortality between stone-proximate and stone-distant populations causally attributable to stone-mediated knowledge transmission, or to confounding factors (elevation differences, infrastructure quality, population density, warning-siren coverage, evacuation-route accessibility)?',
    'Regression-discontinuity analysis controlling for confounding variables; comparison of mortality gradients across multiple tsunami-stone sites with different confounding profiles; natural experiments from migrations or road-construction that altered stone proximity without changing underlying hazard.',
    'Causal attribution strengthens this reading as a valid empirical adjudication point and supports the behavioral_competence_reading. Confounding factors would suggest the stones are epiphenomenal or their contribution is marginal, weakening the reading as an adjudication mechanism.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(id_2011_differential_survival_attribution, empirical, 'Whether 2011 survival differential causally traces to stone-mediated transmission.').

omega_variable(
    reading_status_paradox,
    'Does this reading remain a valid measurement apparatus if the behavioral_competence_reading is vindicated (commitment was live, mandatrophy did not occur), or does vindication collapse the measurement role into the measured hypothesis?',
    'Definitional clarity: a measurement apparatus that proves one hypothesis becomes either a confirmation method (valid as such) or a subsumption of the apparatus into the proven hypothesis. The distinction hinges on whether the apparatus (the stones as validation mechanism) has independent existence or only acquires meaning through the hypothesis it validates.',
    'If the apparatus collapses into the hypothesis, this reading is not actually a third reading but a reformulation of the behavioral_competence_reading. If the apparatus is independent, this reading maintains standing as a measurement claim even after validation occurs.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_status_paradox, conceptual, 'Whether this reading is logically independent from the behavioral_competence_reading or is its reformulation.').

omega_variable(
    kernel_identity_under_contest,
    'Is the kernel being contested here the STONES THEMSELVES (physical commitment objects with potential causal efficacy) or the NARRATIVE ABOUT STONES (an interpretive tradition that may or may not track material facts)?',
    'Examine which element the three readings genuinely dispute: (a) whether the stones existed and were read, or (b) whether reading them changed behavior, or (c) whether the behavioral change persisted, or (d) whether the behavioral persistence was due to stone-transmission vs. alternative mechanisms (seismic-science education, modern disaster-preparedness infrastructure, media, government mandates).',
    'If the kernel is the narrative, sibling readings can coexist (same narrative, different interpretations). If the kernel is the stones'' causal function, readings are empirically adjudicated (2011 provides verdict). This reading presumes the latter; if the former is correct, the reading is misframed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_identity_under_contest, conceptual, 'What is actually under contest in the three readings: the stones as material objects, the narrative about them, or the causal claim they encode disaster knowledge?').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tsunami_stone_commitment__catastrophe_validation_axis, 0, 2011).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tsun_tr_t0, tsunami_stone_commitment__catastrophe_validation_axis, theater_ratio, 0, 0.0).
narrative_ontology:measurement(tsun_tr_t500, tsunami_stone_commitment__catastrophe_validation_axis, theater_ratio, 500, 0.05).
narrative_ontology:measurement(tsun_tr_t1000, tsunami_stone_commitment__catastrophe_validation_axis, theater_ratio, 1000, 0.08).
narrative_ontology:measurement(tsun_tr_t1500, tsunami_stone_commitment__catastrophe_validation_axis, theater_ratio, 1500, 0.12).
narrative_ontology:measurement(tsun_tr_t1800, tsunami_stone_commitment__catastrophe_validation_axis, theater_ratio, 1800, 0.15).
narrative_ontology:measurement(tsun_tr_t1950, tsunami_stone_commitment__catastrophe_validation_axis, theater_ratio, 1950, 0.18).
narrative_ontology:measurement(tsun_tr_t2011, tsunami_stone_commitment__catastrophe_validation_axis, theater_ratio, 2011, 0.2).

% Extraction over time
narrative_ontology:measurement(tsun_be_t0, tsunami_stone_commitment__catastrophe_validation_axis, base_extractiveness, 0, 0.0).
narrative_ontology:measurement(tsun_be_t500, tsunami_stone_commitment__catastrophe_validation_axis, base_extractiveness, 500, 0.0).
narrative_ontology:measurement(tsun_be_t1000, tsunami_stone_commitment__catastrophe_validation_axis, base_extractiveness, 1000, 0.0).
narrative_ontology:measurement(tsun_be_t1500, tsunami_stone_commitment__catastrophe_validation_axis, base_extractiveness, 1500, 0.0).
narrative_ontology:measurement(tsun_be_t1800, tsunami_stone_commitment__catastrophe_validation_axis, base_extractiveness, 1800, 0.0).
narrative_ontology:measurement(tsun_be_t1950, tsunami_stone_commitment__catastrophe_validation_axis, base_extractiveness, 1950, 0.0).
narrative_ontology:measurement(tsun_be_t2011, tsunami_stone_commitment__catastrophe_validation_axis, base_extractiveness, 2011, 0.0).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(tsunami_stone_commitment__catastrophe_validation_axis, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(tsunami_stone_commitment__catastrophe_validation_axis, information_standard).
narrative_ontology:boltzmann_floor_override(tsunami_stone_commitment__catastrophe_validation_axis, 0.0).
narrative_ontology:affects_constraint(tsunami_stone_commitment__catastrophe_validation_axis, tsunami_stone_commitment__behavioral_competence_reading).
narrative_ontology:affects_constraint(tsunami_stone_commitment__catastrophe_validation_axis, tsunami_stone_commitment__commemorative_husk_reading).

% DUAL FORMULATION NOTE:
% The tsunami_stone_commitment kernel instantiates three distinct constraint stories. The behavioral_competence_reading and commemorative_husk_reading are competing interpretations of whether the commitment (inscribed stone warnings + intergenerational transmission) was functionally live or degraded. This reading (catastrophe_validation_axis) is NOT a competing interpretation — it is the MEASUREMENT APPARATUS that adjudicates the contest. All three stories reference the same physical stones and the 2011 tsunami, but from different analytic positions: competence and husk readings are ABOUT the stones; this reading is USING the stones as a test. The 2011 event is the decisive natural experiment from this reading's standpoint; it is empirical data to be interpreted from the competence and husk readings' standpoints. The three constraint stories form an asymmetric triad, not a symmetric triplet.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
