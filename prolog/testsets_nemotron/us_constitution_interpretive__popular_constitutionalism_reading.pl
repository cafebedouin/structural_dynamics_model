% ============================================================================
% CONSTRAINT STORY: us_constitution_interpretive__popular_constitutionalism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_constitution_interpretive__popular_constitutionalism_reading, []).

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
 *   constraint_id: us_constitution_interpretive__popular_constitutionalism_reading
 *   human_readable: Popular Constitutionalism Interpretive Reading
 *   domain: constitutional_law/political_theory
 *
 * SUMMARY:
 *   This constraint story models the popular constitutionalism reading of the
 *   U.S. Constitution's interpretive kernel. It asserts that constitutional
 *   meaning is not settled by judicial pronouncement alone but emerges from
 *   an ongoing contest among political branches, social movements, and the
 *   public. The reading treats the Constitution as a framework for democratic
 *   argument rather than a set of fixed commands enforced by courts.
 *   Beneficiaries are those who gain interpretive voice when courts do not
 *   have the final word: mass movements, elected majorities, and anti-elitist
 *   claimants. Victims are those who lose the stabilizing,
 *   counter-majoritarian backstop of judicial supremacy: advocates of
 *   judicial finality, minorities who depend on courts to block majority
 *   overreach, and actors who require stable constitutional settlements for
 *   planning and legitimacy. The constraint has low extractiveness and
 *   suppression because it does not itself coerce; it reallocates
 *   interpretive authority toward sites of political contestation.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_constitution_interpretive__popular_constitutionalism_reading, 0.22).
domain_priors:suppression_score(us_constitution_interpretive__popular_constitutionalism_reading, 0.15).
domain_priors:theater_ratio(us_constitution_interpretive__popular_constitutionalism_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_constitution_interpretive__popular_constitutionalism_reading, extractiveness, 0.22).
narrative_ontology:constraint_metric(us_constitution_interpretive__popular_constitutionalism_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(us_constitution_interpretive__popular_constitutionalism_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(us_constitution_interpretive__popular_constitutionalism_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(us_constitution_interpretive__popular_constitutionalism_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_constitution_interpretive__popular_constitutionalism_reading, rope).
narrative_ontology:human_readable(us_constitution_interpretive__popular_constitutionalism_reading, "Popular Constitutionalism Interpretive Reading").
narrative_ontology:topic_domain(us_constitution_interpretive__popular_constitutionalism_reading, "constitutional_law/political_theory").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(us_constitution_interpretive__popular_constitutionalism_reading, '918f02e0-7671-4100-b7f5-ccc6abab011b').
narrative_ontology:cs_kernel_codification('918f02e0-7671-4100-b7f5-ccc6abab011b', fixed_text).
narrative_ontology:cs_authority_grounding('918f02e0-7671-4100-b7f5-ccc6abab011b', distributed).
narrative_ontology:cs_reading_relation('918f02e0-7671-4100-b7f5-ccc6abab011b', us_constitution_interpretive__originalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('918f02e0-7671-4100-b7f5-ccc6abab011b', us_constitution_interpretive__living_constitution_reading, coexists_with).
narrative_ontology:cs_axiom('918f02e0-7671-4100-b7f5-ccc6abab011b', foundational, interpretive_authority_resides_in_the_people).
narrative_ontology:cs_axiom_status(interpretive_authority_resides_in_the_people, holdable).
narrative_ontology:cs_axiom_grounding('918f02e0-7671-4100-b7f5-ccc6abab011b', interpretive_authority_resides_in_the_people, deontological).
narrative_ontology:cs_axiom('918f02e0-7671-4100-b7f5-ccc6abab011b', foundational, judicial_supremacy_is_illegitimate).
narrative_ontology:cs_axiom_status(judicial_supremacy_is_illegitimate, holdable).
narrative_ontology:cs_axiom_grounding('918f02e0-7671-4100-b7f5-ccc6abab011b', judicial_supremacy_is_illegitimate, deontological).
narrative_ontology:cs_reference_frame('918f02e0-7671-4100-b7f5-ccc6abab011b', popular_sovereignty_interpretive_authority).
narrative_ontology:cs_drift_state('918f02e0-7671-4100-b7f5-ccc6abab011b', contemporary_judicial_supremacy_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('918f02e0-7671-4100-b7f5-ccc6abab011b', '').
narrative_ontology:cs_kernel_id(us_constitution_interpretive__popular_constitutionalism_reading, us_constitution_interpretive).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_constitution_interpretive__popular_constitutionalism_reading, popular_movements).
narrative_ontology:constraint_beneficiary(us_constitution_interpretive__popular_constitutionalism_reading, legislative_majorities).
narrative_ontology:constraint_beneficiary(us_constitution_interpretive__popular_constitutionalism_reading, anti_elitist_claimants).
narrative_ontology:constraint_victim(us_constitution_interpretive__popular_constitutionalism_reading, judicial_finality_advocates).
narrative_ontology:constraint_victim(us_constitution_interpretive__popular_constitutionalism_reading, counter_majoritarian_minorities).
narrative_ontology:constraint_victim(us_constitution_interpretive__popular_constitutionalism_reading, constitutional_settlement_dependents).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Social movements (civil rights, labor, conservative legal movement, reproductive rights, etc.) gain recognized interpretive standing when courts are not the sole constitutional expositors. They mobilize public opinion, elect officials, and pressure branches to adopt their constitutional vision. Their exit is constrained — they cannot easily leave the constitutional order, but they can shift strategies between judicial litigation and political mobilization.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__popular_constitutionalism_reading, popular_movements, beneficiary,
    organized, biographical, constrained, national).

% Congress and state legislatures recover departmentalist authority to interpret the Constitution for themselves when enacting legislation. They benefit from reduced judicial invalidation risk and greater policy latitude. Their exit is mobile — they can adjust legislative strategy, but they remain within the constitutional system.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__popular_constitutionalism_reading, legislative_majorities, beneficiary,
    institutional, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(us_constitution_interpretive__popular_constitutionalism_reading, legislative_majorities, agenda_setter).

% Populist and anti-establishment actors who frame constitutional meaning as the people's property, not the judiciary's. They benefit from the reading's delegitimization of judicial supremacy. Their exit is constrained — they operate within the same constitutional culture they contest.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__popular_constitutionalism_reading, anti_elitist_claimants, beneficiary,
    moderate, biographical, constrained, national).

% Judges, legal scholars, and institutionalists who view judicial supremacy as essential to constitutional stability, rule of law, and protection of minority rights. They lose the finality and authority of judicial pronouncements. Their exit is identity-locked — professional identity and institutional role are constituted by the very supremacy this reading contests.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__popular_constitutionalism_reading, judicial_finality_advocates, payer,
    institutional, generational, identity_locked, national).

% Discrete and insular minorities (racial, religious, political, sexual) who historically relied on counter-majoritarian judicial review to block majority oppression. Under popular constitutionalism, their constitutional protections depend on the contingency of popular mobilization, which may be hostile. Their exit is trapped — they cannot exit the constitutional order and lack the numbers to prevail in majoritarian contestation.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__popular_constitutionalism_reading, counter_majoritarian_minorities, payer,
    powerless, generational, trapped, national).

% Actors (businesses, lower courts, state officials, citizens) who rely on stable, predictable constitutional rules for planning, compliance, and legitimacy. Frequent interpretive contestation increases uncertainty and compliance costs. Their exit is constrained — they must operate under whatever interpretive regime prevails.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__popular_constitutionalism_reading, constitutional_settlement_dependents, payer,
    moderate, biographical, constrained, national).

% Scholars who analyze, critique, and compare interpretive theories. They do not collect rents or bear costs from the constraint directly but shape the intellectual environment in which it operates. Their seat is analytical — they see the full structure from outside.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__popular_constitutionalism_reading, constitutional_theorists, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the democratic legitimacy problem of constitutional authority: how can a constitution bind generations if its meaning is settled by unelected judges? By dispersing interpretive authority to elected branches and the mobilized public, the reading coordinates constitutional meaning with ongoing democratic consent.
% TRANSFER_FUNCTION: Transfers interpretive finality from the judiciary (especially the Supreme Court) to a distributed field of political actors: Congress, the President, state governments, and organized social movements. The transfer is not of material resources but of authoritative voice — who gets to say what the Constitution means and have that answer stick.
% ABSENT_VOICES: Future generations (who inherit the constitutional order but cannot contest its current meaning), non-citizens subject to U.S. constitutional power (territorial residents, detainees, migrants), and the judiciary itself as an institutional voice (its claim to final say is the very thing contested). These voices are structurally excluded from the popular contestation the reading celebrates.
% DISAPPEARANCE_RATIONALE: If the popular constitutionalism reading vanished overnight, interpretive authority would re-consolidate in the judiciary (judicial supremacy), legislative and executive constitutionalism would retreat, and social movements would lose a theoretical basis for claiming constitutional authority. The institutional landscape of constitutional argument would reorganize around judicial finality.
% FOUNDING_PROBLEM: The founding problem is the democratic legitimacy deficit of judicial supremacy: how can a constitution enacted by 'We the People' be authoritatively interpreted by unelected judges with life tenure, especially when their interpretations bind democratic majorities and entrench prior elite understandings?
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by political scientists (e.g., Dahl on the Court as a national policymaker), historians of the Warren and Rehnquist Courts, and critics across the ideological spectrum (Bickel's counter-majoritarian difficulty, Tushnet's populist constitutional law). No single beneficiary group monopolizes the attestation — the problem is recognized by scholars who do not themselves endorse the popular constitutionalist solution.
narrative_ontology:disappearance_verdict(us_constitution_interpretive__popular_constitutionalism_reading, world_rearranges).
narrative_ontology:founding_problem_status(us_constitution_interpretive__popular_constitutionalism_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(us_constitution_interpretive__popular_constitutionalism_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(us_constitution_interpretive__popular_constitutionalism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(us_constitution_interpretive__popular_constitutionalism_reading, 0.22, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_constitution_interpretive__popular_constitutionalism_reading_tests).
:- end_tests(us_constitution_interpretive__popular_constitutionalism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.22) because the reading does not extract resources — it redistributes interpretive authority. Suppression is low (0.15) because the reading expands the circle of authorized interpreters rather than narrowing it. Theater ratio is modest (0.18) reflecting that some institutional performances of judicial supremacy persist even under this reading. Accessibility collapse is moderate (0.35): the reading keeps alternatives open (originalism, living constitutionalism remain live). Resistance is higher (0.55) because the reading faces entrenched institutional and professional opposition from the legal academy and judiciary. The claim of 'rope' reflects genuine coordination — it solves the problem of constitutional legitimacy in a democracy by dispersing interpretive authority — with minimal coercive overhead.
 *
 * PERSPECTIVAL GAP:
 *   From the judicial_finality_advocate seat, the reading looks like a snare: it destabilizes settled expectations and removes the counter-majoritarian check. From the popular_movement seat, it looks like a rope: it coordinates democratic participation around constitutional meaning. From the legislative_majority seat, it is a scaffold — it empowers legislative constitutionalism but only while political mobilization sustains it. The engine computes these per-seat classifications from the structural data; the declared claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (popular_movements, legislative_majorities, anti_elitist_claimants) gain interpretive authority and democratic agency — their structural position is d near 0.0 (beneficiary). Victims (judicial_finality_advocates, counter_majoritarian_minorities, constitutional_settlement_dependents) lose the protective finality of judicial review and the stability of settled precedent — their d is near 1.0 (target). The reading does not extract materially from victims but imposes a structural cost: the loss of a reliable interpretive backstop. This is why victims are declared despite low base extractiveness — the cost is institutional and positional, not fiscal.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (legitimizing constitutional authority in a democracy) remains live and contested. The reading does not suffer mandatrophy because its coordination function — dispersing interpretive authority to sustain democratic legitimacy — is still demanded by the conditions that motivated it. No concentrated beneficiary captures the arrangement; the gains are diffuse democratic participation. The constraint persists because the problem it coordinates around persists.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    interpretive_authority_allocation_ambiguity,
    'Does popular constitutionalism allocate interpretive authority to all popular movements equally, or does it privilege movements that achieve sustained political power?',
    'Historical analysis of which movements have successfully shifted constitutional meaning under periods of claimed popular constitutionalist influence (e.g., New Deal, civil rights, conservative legal movement).',
    'If authority tracks power, the reading covertly replicates the majoritarian extraction it claims to democratize; if authority is genuinely diffuse, the coordination function is more robust.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interpretive_authority_allocation_ambiguity, conceptual, 'Whether interpretive authority under this reading is genuinely democratic or power-tracking.').

omega_variable(
    counter_majoritarian_protection_gap,
    'Can the reading provide reliable protection for discrete and insular minorities when popular movements are hostile to their claims?',
    'Case study comparison: outcomes for minority rights claims under periods of strong popular constitutionalist mobilization vs. periods of judicial supremacy.',
    'If protection fails systematically, the reading''s victim structure is more severe than the low suppression metric suggests — the cost to minorities is existential, not merely institutional.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(counter_majoritarian_protection_gap, empirical, 'Whether the reading''s victim cost for counter-majoritarian minorities is structural or contingent.').

omega_variable(
    kernel_reading_identity,
    'Is this reading a distinct interpretive framework or a meta-framework that subsumes originalism and living constitutionalism as moments of popular contestation?',
    'Doctrinal analysis: do leading popular constitutionalist theorists (e.g., Kramer, Tushnet, Balkin) treat the reading as a standalone method or as an account of how all constitutional interpretation actually works?',
    'If meta-framework, the reading does not compete with siblings on the same plane — it re-describes them, changing the classification problem entirely.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Ontological status of the popular constitutionalism reading relative to its siblings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_constitution_interpretive__popular_constitutionalism_reading, 0, 230).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(us_c_tr_t0, us_constitution_interpretive__popular_constitutionalism_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(us_c_tr_t50, us_constitution_interpretive__popular_constitutionalism_reading, theater_ratio, 50, 0.12).
narrative_ontology:measurement(us_c_tr_t100, us_constitution_interpretive__popular_constitutionalism_reading, theater_ratio, 100, 0.15).
narrative_ontology:measurement(us_c_tr_t150, us_constitution_interpretive__popular_constitutionalism_reading, theater_ratio, 150, 0.17).
narrative_ontology:measurement(us_c_tr_t200, us_constitution_interpretive__popular_constitutionalism_reading, theater_ratio, 200, 0.17).
narrative_ontology:measurement(us_c_tr_t230, us_constitution_interpretive__popular_constitutionalism_reading, theater_ratio, 230, 0.18).

% Extraction over time
narrative_ontology:measurement(us_c_be_t0, us_constitution_interpretive__popular_constitutionalism_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(us_c_be_t50, us_constitution_interpretive__popular_constitutionalism_reading, base_extractiveness, 50, 0.18).
narrative_ontology:measurement(us_c_be_t100, us_constitution_interpretive__popular_constitutionalism_reading, base_extractiveness, 100, 0.2).
narrative_ontology:measurement(us_c_be_t150, us_constitution_interpretive__popular_constitutionalism_reading, base_extractiveness, 150, 0.2).
narrative_ontology:measurement(us_c_be_t200, us_constitution_interpretive__popular_constitutionalism_reading, base_extractiveness, 200, 0.21).
narrative_ontology:measurement(us_c_be_t230, us_constitution_interpretive__popular_constitutionalism_reading, base_extractiveness, 230, 0.22).

% Suppression requirement over time
narrative_ontology:measurement(us_c_su_t0, us_constitution_interpretive__popular_constitutionalism_reading, suppression_requirement, 0, 0.08).
narrative_ontology:measurement(us_c_su_t50, us_constitution_interpretive__popular_constitutionalism_reading, suppression_requirement, 50, 0.1).
narrative_ontology:measurement(us_c_su_t100, us_constitution_interpretive__popular_constitutionalism_reading, suppression_requirement, 100, 0.12).
narrative_ontology:measurement(us_c_su_t150, us_constitution_interpretive__popular_constitutionalism_reading, suppression_requirement, 150, 0.14).
narrative_ontology:measurement(us_c_su_t200, us_constitution_interpretive__popular_constitutionalism_reading, suppression_requirement, 200, 0.15).
narrative_ontology:measurement(us_c_su_t230, us_constitution_interpretive__popular_constitutionalism_reading, suppression_requirement, 230, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_constitution_interpretive__popular_constitutionalism_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(us_constitution_interpretive__popular_constitutionalism_reading, 0.1).
narrative_ontology:affects_constraint(us_constitution_interpretive__popular_constitutionalism_reading, us_constitution_interpretive__originalist_reading).
narrative_ontology:affects_constraint(us_constitution_interpretive__popular_constitutionalism_reading, us_constitution_interpretive__living_constitution_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the us_constitution_interpretive kernel. It differs from originalist_reading (fixed meaning at ratification) and living_constitution_reading (evolving meaning via judicial adaptation) by locating interpretive authority in democratic contestation rather than in judicial or textual fidelity. All three readings share the kernel but instantiate different constraints with different beneficiary/victim structures and extractiveness profiles. The family is linked via affects_constraints in all three stories.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(us_constitution_interpretive__popular_constitutionalism_reading, institutional, 0.7).
constraint_indexing:directionality_override(us_constitution_interpretive__popular_constitutionalism_reading, organized, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
