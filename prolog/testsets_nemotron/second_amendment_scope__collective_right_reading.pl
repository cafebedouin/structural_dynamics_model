% ============================================================================
% CONSTRAINT STORY: second_amendment_scope__collective_right_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_second_amendment_scope__collective_right_reading, []).

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
 *   constraint_id: second_amendment_scope__collective_right_reading
 *   human_readable: Second Amendment — Collective Right Reading (State Militia Authority)
 *   domain: constitutional_law/political_theory/rights_jurisprudence
 *
 * SUMMARY:
 *   The collective-right reading of the Second Amendment holds that the
 *   Amendment protects state authority to maintain organized militias, not an
 *   individual right to firearms ownership. This reading treats the prefatory
 *   clause ('A well regulated Militia, being necessary to the security of a
 *   free State') as definitional — the right exists only within the militia
 *   context. States and their organized militia units are the beneficiaries;
 *   individuals claiming personal firearms rights are excluded. The
 *   constraint operates with low extractiveness because it enables state
 *   coordination of defense without extracting from individuals (individual
 *   regulation remains a state police power, not a federal constitutional
 *   question). The reading is a rope: genuine coordination of state defense
 *   authority, minimal coercive overhead, participants (states) are net
 *   beneficiaries, alternatives (state constitutions, federal militia
 *   clauses) are not suppressed.
 *
 * KEY AGENTS:
 *   - state_governments: Primary beneficiary (institutional/generational/arbitrage) — hold militia authority
 *   - organized_militia_units: Primary beneficiary (organized/generational/constrained) — exercise the coordinated right
 *   - individual_citizens: Excluded (powerless/biographical/trapped) — no constitutional claim under this reading
 *   - federal_government: Observer (institutional/civilizational/analytical) — limited role in militia regulation
 *   - state_military_departments: Beneficiary (institutional/generational/arbitrage) — administer militia structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_scope__collective_right_reading, 0.12).
domain_priors:suppression_score(second_amendment_scope__collective_right_reading, 0.18).
domain_priors:theater_ratio(second_amendment_scope__collective_right_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_scope__collective_right_reading, extractiveness, 0.12).
narrative_ontology:constraint_metric(second_amendment_scope__collective_right_reading, suppression_requirement, 0.18).
narrative_ontology:constraint_metric(second_amendment_scope__collective_right_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_scope__collective_right_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(second_amendment_scope__collective_right_reading, resistance, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_scope__collective_right_reading, rope).
narrative_ontology:human_readable(second_amendment_scope__collective_right_reading, "Second Amendment — Collective Right Reading (State Militia Authority)").
narrative_ontology:topic_domain(second_amendment_scope__collective_right_reading, "constitutional_law/political_theory/rights_jurisprudence").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_scope__collective_right_reading, '43de1d4e-2899-44b6-baa4-44c05c25c275').
narrative_ontology:cs_kernel_codification('43de1d4e-2899-44b6-baa4-44c05c25c275', fixed_text).
narrative_ontology:cs_authority_grounding('43de1d4e-2899-44b6-baa4-44c05c25c275', lineage).
narrative_ontology:cs_interpretation_layer_present('43de1d4e-2899-44b6-baa4-44c05c25c275').
narrative_ontology:cs_reading_relation('43de1d4e-2899-44b6-baa4-44c05c25c275', second_amendment_scope__individual_right_reading, coexists_with).
narrative_ontology:cs_reading_relation('43de1d4e-2899-44b6-baa4-44c05c25c275', second_amendment_scope__civic_right_reading, coexists_with).
narrative_ontology:cs_axiom('43de1d4e-2899-44b6-baa4-44c05c25c275', foundational, prefatory_clause_definitional).
narrative_ontology:cs_axiom_status(prefatory_clause_definitional, holdable).
narrative_ontology:cs_axiom_grounding('43de1d4e-2899-44b6-baa4-44c05c25c275', prefatory_clause_definitional, conventional).
narrative_ontology:cs_axiom('43de1d4e-2899-44b6-baa4-44c05c25c275', foundational, militia_context_exhaustive).
narrative_ontology:cs_axiom_status(militia_context_exhaustive, holdable).
narrative_ontology:cs_axiom_grounding('43de1d4e-2899-44b6-baa4-44c05c25c275', militia_context_exhaustive, conventional).
narrative_ontology:cs_reference_frame('43de1d4e-2899-44b6-baa4-44c05c25c275', founding_federalism_militia_authority).
narrative_ontology:cs_drift_state('43de1d4e-2899-44b6-baa4-44c05c25c275', post_incorporation_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('43de1d4e-2899-44b6-baa4-44c05c25c275', '').
narrative_ontology:cs_kernel_id(second_amendment_scope__collective_right_reading, second_amendment_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_scope__collective_right_reading, state_governments).
narrative_ontology:constraint_beneficiary(second_amendment_scope__collective_right_reading, organized_militia_units).
narrative_ontology:constraint_beneficiary(second_amendment_scope__collective_right_reading, state_military_departments).
narrative_ontology:constraint_vindicates(second_amendment_scope__collective_right_reading, state_militia_authority_doctrine).
narrative_ontology:constraint_vindicates(second_amendment_scope__collective_right_reading, collective_second_amendment_interpretation).
narrative_ontology:constraint_vindicates(second_amendment_scope__collective_right_reading, federalism_in_arms_regulation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold constitutional authority over militia organization and regulation. Can structure state defense forces, appoint officers, and regulate militia training without federal constitutional interference. Exercise authority through state military departments and National Guard structures. Benefit from federalism protection against federal disarmament.
narrative_ontology:constraint_stakeholder(second_amendment_scope__collective_right_reading, state_governments, beneficiary,
    institutional, generational, arbitrage, national).

% Exercise the coordinated right to 'keep and bear Arms' within the militia context. Include National Guard units and state defense forces. Their existence and federal recognition depend on the collective-right interpretation. Administer training, deployment, and equipment under state authority with federal support.
narrative_ontology:constraint_stakeholder(second_amendment_scope__collective_right_reading, organized_militia_units, beneficiary,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(second_amendment_scope__collective_right_reading, organized_militia_units, agenda_setter).

% Have no federal constitutional claim to firearms ownership under this reading. Subject to state police power regulation of firearms without Second Amendment constraint. Their only recourse is state constitutional provisions, statutory protections, or political advocacy. Cannot exit the constraint's exclusionary effect without changing the constitutional interpretation itself.
narrative_ontology:constraint_stakeholder(second_amendment_scope__collective_right_reading, individual_citizens, excluded,
    powerless, biographical, trapped, national).

% Limited role in militia regulation under this reading — can call forth militia for federal service but cannot disarm or regulate state militia authority. The collective-right reading constrains federal power more than it constrains individuals. Observes and adjudicates disputes between state and federal militia authority.
narrative_ontology:constraint_stakeholder(second_amendment_scope__collective_right_reading, federal_government, observer,
    institutional, civilizational, analytical, national).

% Administer the state's militia/National Guard structure: organization, training, equipping, officer appointments. Directly exercise the state's constitutional authority. Benefit from clear federalism boundary that protects their administrative domain from federal encroachment.
narrative_ontology:constraint_stakeholder(second_amendment_scope__collective_right_reading, state_military_departments, beneficiary,
    institutional, generational, arbitrage, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(second_amendment_scope__collective_right_reading, state_governments).
narrative_ontology:fixing_cost_class(second_amendment_scope__collective_right_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates state authority to maintain organized militias for common defense against federal disarmament, enabling states to structure, equip, and deploy military forces under their own command without federal constitutional interference.
% TRANSFER_FUNCTION: Allocates constitutional authority over militia regulation from federal to state level. No resource transfer occurs — the constraint distributes jurisdictional authority. States gain regulatory autonomy; individuals lose federal constitutional standing to challenge state firearms regulation.
% ABSENT_VOICES: Individual citizens seeking constitutional protection against state firearms regulation are structurally excluded — they would argue for individual-right or civic-right readings but have no standing in the collective-right framework. Their absence is the constraint's defining exclusion.
% DISAPPEARANCE_RATIONALE: If the collective-right reading vanished overnight, the constitutional baseline would shift to either individual-right or civic-right readings. States would lose their exclusive constitutional claim to militia authority, federal power to regulate firearms would expand (under incorporation), and individuals would gain constitutional standing. The federalism balance in firearms regulation would fundamentally reorganize.
% FOUNDING_PROBLEM: Founding-era fear that the new federal government would disarm state militias, leaving states defenseless against federal tyranny or unable to suppress insurrections. The Amendment was built to secure state militia authority as a federalism safeguard.
% FOUNDING_PROBLEM_CORROBORATION: Founding-era state ratification debates and militia statutes corroborate the state-authority purpose (outside the benefiting parties: Anti-Federalist writings, state convention records). However, 14th Amendment incorporation doctrine and the National Guard's dual state/federal structure substantially transformed the founding problem. Modern scholars and jurists dispute whether the original problem persists in recognizable form. No single corroborating source outside state governments attests the problem is fully live in its original terms.
narrative_ontology:disappearance_verdict(second_amendment_scope__collective_right_reading, world_rearranges).
narrative_ontology:founding_problem_status(second_amendment_scope__collective_right_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_scope__collective_right_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(second_amendment_scope__collective_right_reading, 'none', 1).
narrative_ontology:epsilon_provenance(second_amendment_scope__collective_right_reading, 0.12, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_scope__collective_right_reading_tests).
:- end_tests(second_amendment_scope__collective_right_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.12) because the reading does not extract resources from individuals — it allocates constitutional authority to states. Suppression is low (0.18) because the reading operates through doctrinal exclusion rather than active enforcement against individuals; individuals are excluded from the constitutional claim but remain subject to state regulation (which is ordinary police power, not constitutional suppression). Theater ratio is near zero (0.05) — the reading performs little theatrical maintenance because its institutional scope is narrow and its coordination function (state militia authority) is genuine. Accessibility collapse is moderate (0.35) — alternatives exist (state constitutions, individual-right readings) but the collective-right frame structurally limits constitutional challenge paths. Resistance is moderate (0.42) — the individual-right reading has substantial scholarly, political, and judicial support creating active contestation.
 *
 * PERSPECTIVAL GAP:
 *   From the state government seat: the reading is pure coordination — states need militia authority for security, the Amendment secures it, no extraction occurs. From the individual citizen seat: the reading is extractive exclusion — constitutional protection is denied while state regulatory power remains plenary. From the organized militia seat: genuine coordination benefit with minimal cost. The engine computes this divergence from the structural data — state governments and militia units have low directionality (beneficiaries), individuals have high directionality (excluded/targets of exclusion).
 *
 * DIRECTIONALITY LOGIC:
 *   State governments and organized militia units are declared beneficiaries — they hold the constitutional authority and exercise the coordinated right. Individuals are excluded from the beneficiary set and bear the cost of having no federal constitutional claim against state regulation. Federal government is an observer with analytical distance. The collective-right frame means the constraint's extraction (if any) falls on individuals denied constitutional standing, but the reading itself treats this as non-extractive because individual regulation was never a federal question under this interpretation. The engine's directionality derivation assigns low d to beneficiaries (states/militias) and high d to excluded individuals.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (state militia authority against federal disarmament) was live in 1791 and remained live through the 19th century. By the 20th century, the militia system had transformed into the National Guard under dual state/federal control, substantially changing the founding problem's nature. The collective-right reading persists as a doctrinal position despite this transformation — its coordination function (state militia authority) has been largely absorbed into the modern National Guard framework, but the constitutional reading remains available as a limiting principle on federal power. This is not mandatrophy because the reading still serves a live coordination function (state authority in federalism disputes), but its scope has narrowed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is the collective-right reading a genuine coordination constraint (rope) or a doctrinal cover for state power consolidation?',
    'Historical analysis of Founding-era militia statutes and state constitutional provisions; comparative study of state vs. federal arms regulation 1791-1868.',
    'If the reading functions as state power consolidation rather than genuine coordination, classification shifts toward tangled_rope or snare with state governments as beneficiaries and individuals as victims.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether the collective-right reading coordinates genuine state security needs or extracts regulatory authority from individuals.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (legal barriers to individual claims) or internalized (doctrinal acceptance of state authority)?',
    'Post-Heller litigation tracking: if individuals continue to experience rights restriction after collective-right doctrine is rejected, suppression was structural; if restriction lifts, internalized component was significant.',
    'If internalized, effective suppression is higher than structural measure suggests — individuals carry doctrinal constraint even after legal barrier removal.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in constitutional rights context.').

omega_variable(
    cs_framing_underdetermination,
    'Does the collective-right reading instantiate a commitment-system constraint grounded in text (fixed_text kernel) or in institutional extraction (extraction authority)?',
    'Analyze whether state authority over militias is exercised through interpretive tradition (lineage/practice) or through active prevention of kernel revision (extraction). Compare state militia regulation patterns pre/post incorporation.',
    'If extraction-grounded, the reading''s authority_grounding shifts from lineage to extraction, changing CS pattern classification and drift analysis.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(cs_framing_underdetermination, conceptual, 'CS framing ambiguity: text-grounded coordination vs. extraction-grounded authority.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_scope__collective_right_reading, 1791, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(seco_tr_t1791, second_amendment_scope__collective_right_reading, theater_ratio, 1791, 0.02).
narrative_ontology:measurement(seco_tr_t1868, second_amendment_scope__collective_right_reading, theater_ratio, 1868, 0.03).
narrative_ontology:measurement(seco_tr_t1939, second_amendment_scope__collective_right_reading, theater_ratio, 1939, 0.05).
narrative_ontology:measurement(seco_tr_t2008, second_amendment_scope__collective_right_reading, theater_ratio, 2008, 0.05).
narrative_ontology:measurement(seco_tr_t2010, second_amendment_scope__collective_right_reading, theater_ratio, 2010, 0.05).
narrative_ontology:measurement(seco_tr_t2024, second_amendment_scope__collective_right_reading, theater_ratio, 2024, 0.05).

% Extraction over time
narrative_ontology:measurement(seco_be_t1791, second_amendment_scope__collective_right_reading, base_extractiveness, 1791, 0.08).
narrative_ontology:measurement(seco_be_t1868, second_amendment_scope__collective_right_reading, base_extractiveness, 1868, 0.1).
narrative_ontology:measurement(seco_be_t1939, second_amendment_scope__collective_right_reading, base_extractiveness, 1939, 0.12).
narrative_ontology:measurement(seco_be_t2008, second_amendment_scope__collective_right_reading, base_extractiveness, 2008, 0.12).
narrative_ontology:measurement(seco_be_t2010, second_amendment_scope__collective_right_reading, base_extractiveness, 2010, 0.12).
narrative_ontology:measurement(seco_be_t2024, second_amendment_scope__collective_right_reading, base_extractiveness, 2024, 0.12).

% Suppression requirement over time
narrative_ontology:measurement(seco_su_t1791, second_amendment_scope__collective_right_reading, suppression_requirement, 1791, 0.15).
narrative_ontology:measurement(seco_su_t1868, second_amendment_scope__collective_right_reading, suppression_requirement, 1868, 0.18).
narrative_ontology:measurement(seco_su_t1939, second_amendment_scope__collective_right_reading, suppression_requirement, 1939, 0.18).
narrative_ontology:measurement(seco_su_t2008, second_amendment_scope__collective_right_reading, suppression_requirement, 2008, 0.18).
narrative_ontology:measurement(seco_su_t2010, second_amendment_scope__collective_right_reading, suppression_requirement, 2010, 0.18).
narrative_ontology:measurement(seco_su_t2024, second_amendment_scope__collective_right_reading, suppression_requirement, 2024, 0.18).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_scope__collective_right_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(second_amendment_scope__collective_right_reading, 0.1).
narrative_ontology:affects_constraint(second_amendment_scope__collective_right_reading, second_amendment_scope__individual_right_reading).
narrative_ontology:affects_constraint(second_amendment_scope__collective_right_reading, second_amendment_scope__civic_right_reading).
narrative_ontology:affects_constraint(second_amendment_scope__collective_right_reading, national_guard_dual_authority).
narrative_ontology:affects_constraint(second_amendment_scope__collective_right_reading, state_police_powers_firearms).

% DUAL FORMULATION NOTE:
% Second Amendment kernel decomposes into three readings: collective-right (state militia authority, low ε, rope), individual-right (individual ownership, high ε, tangled_rope/snare), civic-right (conditional individual right, moderate ε, tangled_rope). The collective-right reading is the upstream constraint — its low-extraction coordination of state authority is cited by the other readings as the historical baseline they must distinguish from. Downstream readings carry higher extraction because they must overcome the collective-right frame's institutional entrenchment.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(second_amendment_scope__collective_right_reading, powerless, 0.85).
constraint_indexing:directionality_override(second_amendment_scope__collective_right_reading, institutional, 0.1).
constraint_indexing:directionality_override(second_amendment_scope__collective_right_reading, organized, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
