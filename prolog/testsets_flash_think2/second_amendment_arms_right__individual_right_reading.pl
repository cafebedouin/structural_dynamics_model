% ============================================================================
% CONSTRAINT STORY: second_amendment_arms_right__individual_right_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_second_amendment_arms_right__individual_right_reading, []).

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
 *   constraint_id: second_amendment_arms_right__individual_right_reading
 *   human_readable: Second Amendment: Individual Right to Keep and Bear Arms
 *   domain: constitutional_law/political_philosophy/legal_interpretation
 *
 * SUMMARY:
 *   This constraint represents the 'individual right' reading of the Second
 *   Amendment, asserting that the right to keep and bear arms is a
 *   pre-existing individual liberty protected against federal infringement.
 *   This reading has gained significant legal traction, particularly after
 *   Supreme Court decisions in *Heller* (2008) and *Bruen* (2022), which
 *   affirmed and expanded its scope. The constraint is claimed as a
 *   'mountain' by its proponents, reflecting its perceived status as a
 *   fundamental, immutable right. The metrics, however, reflect its active
 *   enforcement and the substantial extraction it imposes on governmental
 *   regulatory capacity.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_arms_right__individual_right_reading, 0.85).
domain_priors:suppression_score(second_amendment_arms_right__individual_right_reading, 0.78).
domain_priors:theater_ratio(second_amendment_arms_right__individual_right_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_arms_right__individual_right_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(second_amendment_arms_right__individual_right_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(second_amendment_arms_right__individual_right_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_arms_right__individual_right_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(second_amendment_arms_right__individual_right_reading, resistance, 0.9).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_arms_right__individual_right_reading, mountain).
narrative_ontology:human_readable(second_amendment_arms_right__individual_right_reading, "Second Amendment: Individual Right to Keep and Bear Arms").
narrative_ontology:topic_domain(second_amendment_arms_right__individual_right_reading, "constitutional_law/political_philosophy/legal_interpretation").

domain_priors:requires_active_enforcement(second_amendment_arms_right__individual_right_reading).
domain_priors:emerges_naturally(second_amendment_arms_right__individual_right_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_arms_right__individual_right_reading, '35f4e755-2da9-4d35-8627-03cb1466dfc0').
narrative_ontology:cs_kernel_codification('35f4e755-2da9-4d35-8627-03cb1466dfc0', fixed_text).
narrative_ontology:cs_authority_grounding('35f4e755-2da9-4d35-8627-03cb1466dfc0', lineage).
narrative_ontology:cs_interpretation_layer_present('35f4e755-2da9-4d35-8627-03cb1466dfc0').
narrative_ontology:cs_reading_relation('35f4e755-2da9-4d35-8627-03cb1466dfc0', second_amendment_arms_right__collective_right_reading, forecloses).
narrative_ontology:cs_reading_relation('35f4e755-2da9-4d35-8627-03cb1466dfc0', second_amendment_arms_right__civic_republican_reading, coexists_with).
narrative_ontology:cs_axiom('35f4e755-2da9-4d35-8627-03cb1466dfc0', foundational, individual_liberty_pre_exists_government).
narrative_ontology:cs_axiom_status(individual_liberty_pre_exists_government, holdable).
narrative_ontology:cs_axiom_grounding('35f4e755-2da9-4d35-8627-03cb1466dfc0', individual_liberty_pre_exists_government, deontological).
narrative_ontology:cs_axiom('35f4e755-2da9-4d35-8627-03cb1466dfc0', foundational, federal_infringement_prohibited).
narrative_ontology:cs_axiom_status(federal_infringement_prohibited, holdable).
narrative_ontology:cs_axiom_grounding('35f4e755-2da9-4d35-8627-03cb1466dfc0', federal_infringement_prohibited, conventional).
narrative_ontology:cs_reference_frame('35f4e755-2da9-4d35-8627-03cb1466dfc0', original_individual_right).
narrative_ontology:cs_drift_state('35f4e755-2da9-4d35-8627-03cb1466dfc0', post_bruen_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('35f4e755-2da9-4d35-8627-03cb1466dfc0', '').
narrative_ontology:cs_kernel_id(second_amendment_arms_right__individual_right_reading, second_amendment_arms_right).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_arms_right__individual_right_reading, individual_gun_owners).
narrative_ontology:constraint_beneficiary(second_amendment_arms_right__individual_right_reading, firearms_industry).
narrative_ontology:constraint_victim(second_amendment_arms_right__individual_right_reading, federal_government).
narrative_ontology:constraint_victim(second_amendment_arms_right__individual_right_reading, state_governments).
narrative_ontology:constraint_victim(second_amendment_arms_right__individual_right_reading, gun_control_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from the protection of their right to own firearms for self-defense and other lawful purposes, free from significant government infringement. Their ability to exercise this right is enhanced by this reading.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__individual_right_reading, individual_gun_owners, beneficiary,
    organized, biographical, constrained, national).

% Benefits from a legal environment that protects the sale and ownership of firearms, ensuring a market for their products. They actively lobby and litigate to defend this reading.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__individual_right_reading, firearms_industry, beneficiary,
    institutional, generational, arbitrage, national).

% Bears the cost of constrained regulatory power over firearms. Its ability to enact and enforce gun control measures is significantly limited by this interpretation, leading to legislative and enforcement challenges.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__individual_right_reading, federal_government, payer,
    institutional, civilizational, constrained, national).

% Similar to the federal government, state and local authorities face significant legal hurdles in regulating firearms, often having their laws challenged and overturned based on this reading.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__individual_right_reading, state_governments, payer,
    institutional, generational, constrained, national).

% Bear the cost of reduced government capacity to implement desired gun control policies. Their efforts to reduce gun violence through legislation are often thwarted by this constitutional interpretation.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__individual_right_reading, gun_control_advocates, payer,
    organized, biographical, constrained, national).

% As the ultimate interpreter of the Constitution, the Supreme Court sets the authoritative reading of the Second Amendment, shaping the legal landscape for all other parties. Its decisions directly enforce this constraint.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__individual_right_reading, supreme_court, agenda_setter,
    institutional, civilizational, analytical, universal).

% Scholars who argue for a collective or militia-centric interpretation of the Second Amendment are largely excluded from the dominant legal framework established by this individual-right reading, though their arguments persist in academic discourse.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__individual_right_reading, collective_right_scholars, excluded,
    analytical, biographical, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a fundamental individual liberty regarding the ownership of firearms, coordinating expectations between citizens and government about the limits of state power over arms.
% TRANSFER_FUNCTION: Transfers significant regulatory authority over firearms from federal and state governments to individual citizens, and transfers the primary responsibility for self-defense to individuals.
% ABSENT_VOICES: Scholars and advocates for collective or civic republican interpretations of the Second Amendment are structurally excluded from the core premise of this reading, which prioritizes the individual right above all else.
% DISAPPEARANCE_RATIONALE: If this individual-right reading vanished, federal and state governments would gain substantially more power to regulate firearms, leading to a complete restructuring of gun laws and a fundamental shift in the relationship between citizens and the state regarding arms ownership.
% FOUNDING_PROBLEM: To protect the individual's pre-existing natural right to self-defense and to ensure the capacity of citizens to resist potential tyranny, based on a fundamental liberty that predates government.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars, historical texts from the founding era, and contemporary advocacy groups (e.g., Second Amendment Foundation, Cato Institute) corroborate the historical and philosophical basis for an individual right, asserting its ongoing relevance for liberty and self-defense, even as its scope remains debated by others.
narrative_ontology:disappearance_verdict(second_amendment_arms_right__individual_right_reading, world_rearranges).
narrative_ontology:founding_problem_status(second_amendment_arms_right__individual_right_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_arms_right__individual_right_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(second_amendment_arms_right__individual_right_reading, 'none', 1).
narrative_ontology:epsilon_provenance(second_amendment_arms_right__individual_right_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_arms_right__individual_right_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(second_amendment_arms_right__individual_right_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(second_amendment_arms_right__individual_right_reading, ExtMetricName, E),
    domain_priors:suppression_score(second_amendment_arms_right__individual_right_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(second_amendment_arms_right__individual_right_reading),
    narrative_ontology:constraint_metric(second_amendment_arms_right__individual_right_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(second_amendment_arms_right__individual_right_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(second_amendment_arms_right__individual_right_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (0.85) and suppression (0.78) reflect the significant legal and practical barriers this reading places on government efforts to regulate firearms. The 'resistance' metric is high (0.90) due to the ongoing political and legal battles over gun control, where this reading is a central point of contention. The low 'theater_ratio' (0.10) indicates that the constraint is actively and genuinely enforced, not merely maintained for show. The 'accessibility_collapse' (0.70) signifies that many regulatory alternatives are effectively foreclosed by this interpretation. The temporal measurements show a clear increase in extractiveness and suppression, particularly around key Supreme Court decisions, reflecting the strengthening of this reading over time.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of individual gun owners, this constraint is a fundamental protection of liberty, a 'mountain' that ensures their rights. From the perspective of governments and gun control advocates, it is a 'snare' or 'tangled rope' that actively extracts regulatory power and imposes societal costs by limiting public safety measures. The engine's classification will highlight this divergence from the claimed 'mountain' type due to the high extractiveness and active enforcement.
 *
 * DIRECTIONALITY LOGIC:
 *   Individual gun owners and the firearms industry are clear beneficiaries, as the constraint protects and expands their interests. Federal and state governments, along with gun control advocates, are targets, as their ability to regulate firearms is curtailed. The Supreme Court acts as the agenda-setter, defining and enforcing the boundaries of this right. Scholars of alternative readings are excluded from the operational framework of this interpretation.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_right_vs_constitutional_construct,
    'Is the right to keep and bear arms truly a pre-existing natural right, or is it a right primarily constructed and codified by the Constitution?',
    'Philosophical and historical analysis of natural rights theory and the intent of the Founders, alongside contemporary legal interpretation. This is a conceptual debate with no definitive empirical resolution.',
    'If primarily a natural right, its ''mountain'' classification is strengthened. If primarily a constitutional construct, its ''emerges_naturally'' claim is weakened, potentially reclassifying it as a ''tangled_rope'' or ''snare'' depending on its extractive properties.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_right_vs_constitutional_construct, conceptual, 'Ambiguity regarding the foundational source of the right.').

omega_variable(
    scope_of_arms_covered,
    'What types of arms are covered by the individual right, particularly in relation to modern weaponry and historical context?',
    'Ongoing judicial interpretation, historical analysis of ''arms'' at the time of the Second Amendment''s ratification, and empirical assessment of modern weapon capabilities in relation to self-defense and militia purposes.',
    'A narrow interpretation of ''arms'' would reduce the constraint''s extractiveness on government regulation, potentially shifting its classification towards a ''rope'' or ''scaffold''. A broad interpretation would maintain or increase its extractiveness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scope_of_arms_covered, empirical, 'Ambiguity regarding the specific objects protected by the right.').

omega_variable(
    individual_vs_collective_right_ambiguity,
    'Is the Second Amendment primarily an individual right, a collective right tied to militia service, or a civic republican right?',
    'This is the core interpretive contest. Resolution would require a definitive, universally accepted historical and legal consensus, which is unlikely. Judicial decisions have largely settled this in favor of the individual right, but academic and political debate continues.',
    'If a collective or civic republican reading were to become dominant, the beneficiaries and victims would shift dramatically, and the constraint''s extractiveness on government regulation would likely decrease, leading to a reclassification away from its current ''mountain'' claim.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(individual_vs_collective_right_ambiguity, conceptual, 'The fundamental interpretive contest over the Second Amendment''s primary purpose.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_arms_right__individual_right_reading, 1980, 2030).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(seco_tr_t1980, second_amendment_arms_right__individual_right_reading, theater_ratio, 1980, 0.15).
narrative_ontology:measurement(seco_tr_t1990, second_amendment_arms_right__individual_right_reading, theater_ratio, 1990, 0.12).
narrative_ontology:measurement(seco_tr_t2000, second_amendment_arms_right__individual_right_reading, theater_ratio, 2000, 0.1).
narrative_ontology:measurement(seco_tr_t2008, second_amendment_arms_right__individual_right_reading, theater_ratio, 2008, 0.08).
narrative_ontology:measurement(seco_tr_t2015, second_amendment_arms_right__individual_right_reading, theater_ratio, 2015, 0.09).
narrative_ontology:measurement(seco_tr_t2022, second_amendment_arms_right__individual_right_reading, theater_ratio, 2022, 0.1).
narrative_ontology:measurement(seco_tr_t2030, second_amendment_arms_right__individual_right_reading, theater_ratio, 2030, 0.1).

% Extraction over time
narrative_ontology:measurement(seco_be_t1980, second_amendment_arms_right__individual_right_reading, base_extractiveness, 1980, 0.6).
narrative_ontology:measurement(seco_be_t1990, second_amendment_arms_right__individual_right_reading, base_extractiveness, 1990, 0.65).
narrative_ontology:measurement(seco_be_t2000, second_amendment_arms_right__individual_right_reading, base_extractiveness, 2000, 0.7).
narrative_ontology:measurement(seco_be_t2008, second_amendment_arms_right__individual_right_reading, base_extractiveness, 2008, 0.78).
narrative_ontology:measurement(seco_be_t2015, second_amendment_arms_right__individual_right_reading, base_extractiveness, 2015, 0.82).
narrative_ontology:measurement(seco_be_t2022, second_amendment_arms_right__individual_right_reading, base_extractiveness, 2022, 0.85).
narrative_ontology:measurement(seco_be_t2030, second_amendment_arms_right__individual_right_reading, base_extractiveness, 2030, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(seco_su_t1980, second_amendment_arms_right__individual_right_reading, suppression_requirement, 1980, 0.55).
narrative_ontology:measurement(seco_su_t1990, second_amendment_arms_right__individual_right_reading, suppression_requirement, 1990, 0.6).
narrative_ontology:measurement(seco_su_t2000, second_amendment_arms_right__individual_right_reading, suppression_requirement, 2000, 0.68).
narrative_ontology:measurement(seco_su_t2008, second_amendment_arms_right__individual_right_reading, suppression_requirement, 2008, 0.75).
narrative_ontology:measurement(seco_su_t2015, second_amendment_arms_right__individual_right_reading, suppression_requirement, 2015, 0.77).
narrative_ontology:measurement(seco_su_t2022, second_amendment_arms_right__individual_right_reading, suppression_requirement, 2022, 0.78).
narrative_ontology:measurement(seco_su_t2030, second_amendment_arms_right__individual_right_reading, suppression_requirement, 2030, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_arms_right__individual_right_reading, identity_coordination).
narrative_ontology:affects_constraint(second_amendment_arms_right__individual_right_reading, gun_control_legislation).
narrative_ontology:affects_constraint(second_amendment_arms_right__individual_right_reading, self_defense_laws).
narrative_ontology:affects_constraint(second_amendment_arms_right__individual_right_reading, second_amendment_arms_right__collective_right_reading).
narrative_ontology:affects_constraint(second_amendment_arms_right__individual_right_reading, second_amendment_arms_right__civic_republican_reading).

% DUAL FORMULATION NOTE:
% This is one of three primary readings of the Second Amendment, each with distinct structural implications for gun ownership and government regulation. This reading directly influences and largely forecloses the 'collective right' reading in legal practice.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
