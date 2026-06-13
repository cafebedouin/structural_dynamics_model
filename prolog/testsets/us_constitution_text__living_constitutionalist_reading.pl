% ============================================================================
% CONSTRAINT STORY: us_constitution_text__living_constitutionalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_constitution_text__living_constitutionalist_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: us_constitution_text__living_constitutionalist_reading
 *   human_readable: Living Constitutionalist Judicial Interpretation
 *   domain: constitutional_law/legal_philosophy
 *
 * SUMMARY:
 *   Living constitutionalism is a reading of the U.S. Constitution that holds
 *   constitutional meaning evolves through interpretation to adapt to
 *   contemporary social and technological circumstances. Judges, guided by
 *   the Constitution's principles and informed by post-ratification practice,
 *   adapt those principles to novel situations the framers could not have
 *   foreseen (reproductive autonomy, same-sex marriage, digital privacy). The
 *   constraint is CLAIMED as rope (it solves the coordination problem of
 *   governing across time with a fixed text). The metrics describe modest
 *   extractiveness and low suppression: the reading grants judges flexible
 *   authority but respects legislative space; resistance is high (originalism
 *   contests it vigorously); theater is low because the interpretive work is
 *   substantive, not performative. The claim/metric gap signals that what
 *   living constitutionalists frame as coordination (adapting principles to
 *   new times) originalists frame as judicial overreach—the same constraint
 *   experienced differently by seats with opposite commitments to how
 *   constitutional meaning should be fixed.
 *
 * KEY AGENTS:
 *   - Federal judges: interpreters who expand constitutional protection to emergent rights
 *   - Contemporary rights claimants: beneficiaries who win recognition of rights in changed social contexts
 *   - Originalist interpreters: excluded opposition who argue meaning is fixed at ratification
 *   - Conservative political movements: payers who lose policy battles in courts
 *   - Progressive political movements: beneficiaries who gain rights through judicial recognition
 *   - Legal academia: observers who theorize the competing readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_constitution_text__living_constitutionalist_reading, 0.38).
domain_priors:suppression_score(us_constitution_text__living_constitutionalist_reading, 0.22).
domain_priors:theater_ratio(us_constitution_text__living_constitutionalist_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_constitution_text__living_constitutionalist_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(us_constitution_text__living_constitutionalist_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(us_constitution_text__living_constitutionalist_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(us_constitution_text__living_constitutionalist_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(us_constitution_text__living_constitutionalist_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_constitution_text__living_constitutionalist_reading, rope).
narrative_ontology:human_readable(us_constitution_text__living_constitutionalist_reading, "Living Constitutionalist Judicial Interpretation").
narrative_ontology:topic_domain(us_constitution_text__living_constitutionalist_reading, "constitutional_law/legal_philosophy").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(us_constitution_text__living_constitutionalist_reading, 'fedf8e7d-6fba-41a9-a700-a6b6885bf070').
narrative_ontology:cs_kernel_codification('fedf8e7d-6fba-41a9-a700-a6b6885bf070', fixed_text).
narrative_ontology:cs_authority_grounding('fedf8e7d-6fba-41a9-a700-a6b6885bf070', lineage).
narrative_ontology:cs_interpretation_layer_present('fedf8e7d-6fba-41a9-a700-a6b6885bf070').
narrative_ontology:cs_reading_relation('fedf8e7d-6fba-41a9-a700-a6b6885bf070', us_constitution_text__originalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('fedf8e7d-6fba-41a9-a700-a6b6885bf070', us_constitution_text__positivist_reading, coexists_with).
narrative_ontology:cs_axiom('fedf8e7d-6fba-41a9-a700-a6b6885bf070', foundational, constitutional_meaning_is_dynamic).
narrative_ontology:cs_axiom_status(constitutional_meaning_is_dynamic, holdable).
narrative_ontology:cs_axiom_grounding('fedf8e7d-6fba-41a9-a700-a6b6885bf070', constitutional_meaning_is_dynamic, deontological).
narrative_ontology:cs_axiom('fedf8e7d-6fba-41a9-a700-a6b6885bf070', foundational, post_ratification_practice_is_authoritative).
narrative_ontology:cs_axiom_status(post_ratification_practice_is_authoritative, holdable).
narrative_ontology:cs_axiom_grounding('fedf8e7d-6fba-41a9-a700-a6b6885bf070', post_ratification_practice_is_authoritative, empirically_contingent).
narrative_ontology:cs_reference_frame('fedf8e7d-6fba-41a9-a700-a6b6885bf070', adaptive_constitutional_authority).
narrative_ontology:cs_drift_state('fedf8e7d-6fba-41a9-a700-a6b6885bf070', contemporary_conservative_judicial_ascendancy, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('fedf8e7d-6fba-41a9-a700-a6b6885bf070', '').
narrative_ontology:cs_kernel_id(us_constitution_text__living_constitutionalist_reading, us_constitution_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_constitution_text__living_constitutionalist_reading, contemporary_rights_claimants).
narrative_ontology:constraint_beneficiary(us_constitution_text__living_constitutionalist_reading, marginalized_constituencies).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(us_constitution_text__living_constitutionalist_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(us_constitution_text__living_constitutionalist_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_constitution_text__living_constitutionalist_reading_tests).
:- end_tests(us_constitution_text__living_constitutionalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low-moderate (0.38) because the constraint does not generate a zero-sum transfer of wealth or status; judges are not collecting rents from a captive population. Instead, the constraint redistributes *interpretive authority*: from 'what the 1789 generation meant' to 'what contemporary society understands principles to require.' This is extractive to originalists and conservative legislatures (they lose the ability to settle constitutional questions through normal politics), but they are not a trapped victim set—they can appoint originalist judges, litigate, or pursue amendment. Suppression is low (0.22): living constitutionalism does not require active suppression of alternative interpretations; originalist arguments are heard in courts and academia constantly. Theater is also low (0.18): the interpretive debates are substantive, not ceremonial performance. The measurement series shows modest rise over 60 years (1965–2025), reflecting growing entrenchment of living constitutionalism in elite legal institutions and progressive strengthening of its institutional position.
 *
 * PERSPECTIVAL GAP:
 *   From the bench of a living constitutionalist judge: the constraint is a coordination mechanism that keeps the Constitution alive and authoritative across time. From the seat of an originalist or conservative legislator: the constraint is an illegitimate power grab by judges who usurp democratic authority. The engine computes these seats' per-seat types from power/exit/beneficiary data: the judge seat experiences this as a low-extraction coordination tool (judges exercise interpretive authority granted to them by the constitutional text itself); the originalist seat experiences it as high-extraction (judges overreach, usurping legislative and amendment prerogatives). The authored metrics describe the constraint as experienced across these divergent seats—extractiveness measures the degree to which authority is transferred away from originalist/legislative understandings; suppression measures whether the constraint requires actively silencing originalist objections (low, because they are heard); resistance measures how vigorously originalism contests it (high).
 *
 * DIRECTIONALITY LOGIC:
 *   Contemporary rights claimants benefit directly: the constraint permits judges to recognize their circumstances as constitutionally protected (d low, beneficiary direction). Progressive political movements benefit (d low): they win rights battles in courts. Originalist interpreters and conservative political movements are targets, not because they lose anything tangible, but because their preferred understanding of constitutional meaning—fixed at ratification—is overridden by this reading (d moderate-to-high for institutional and organized actors who would enforce originalism if they could, but are constrained by the entrenchment of living constitutionalism in elite judiciary). Federal judges occupy an unusual position: they are the agenda-setters (they decide what interpretation prevails), but they are also somewhat constrained (they must remain within the bounds of constitutional language and precedent; they cannot ignore the Constitution entirely). Their power is institutional; their exit is constrained (appointment and salary are fixed); their time horizon is generational (judicial terms are long).
 *
 * MANDATROPHY ANALYSIS:
 *   Living constitutionalism shows NO signs of mandatrophy. Its founding problem—how a static text governs a dynamic society—remains live and contested. The constraint's justification (enabling constitutional adaptation without amendment) still holds. There is no atrophied function being maintained theatrically: judges actively engage in interpretive work, and the outcomes (new rights recognitions) remain significant and contested. The rising theater_ratio over time (0.08 → 0.18) reflects not degradation but increasing ritualization of the debate: as living constitutionalism became institutionally entrenched, some of the interpretive argument became more about *demonstrating fidelity to the principle of adaptation* rather than discovering new applications. This is a symptom of institutional entrenchment, not mandatrophy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    interpretation_vs_legislation_boundary,
    'How much judicial authority to adapt constitutional principles crosses the line from legitimate interpretation into illegitimate judicial legislation?',
    'Empirical study of cases where living constitutionalism recognized new rights: does the outcome cohere with the constitutional text and principles, or does it require judges to override the text to reach a policy goal? Does the interpretive community (courts, scholars) accept the reasoning as interpretive or reject it as legislative?',
    'If the boundary shifts—if cases once accepted as legitimate interpretation become rejected as judicial overreach—the constraint''s extraction component rises (judges are overriding democracy, not interpreting law). If the community maintains the boundary, the constraint remains coordinative (enabling stable adaptation).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interpretation_vs_legislation_boundary, conceptual, 'Whether living constitutionalism remains interpretive or has become legislative usurpation.').

omega_variable(
    originalist_foreclosure,
    'Does originalism''s core premise (constitutional meaning is fixed at ratification) logically foreclose living constitutionalism''s core premise (meaning evolves through interpretation)?',
    'Philosophical analysis: can a single framework hold both ''meaning is fixed at ratification'' and ''judges adapt meaning to contemporary circumstances''? Do originalists and living constitutionalists who engage with each other''s arguments treat the claims as contradictory or merely different?',
    'If they genuinely foreclose (logically contradictory), only one reading can be true; the constraint''s type would depend on which reading is adopted. If they coexist (different parties holding different readings), both can persist as competing institutional positions.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(originalist_foreclosure, conceptual, 'Whether originalism and living constitutionalism are logically incompatible or merely competing frames.').

omega_variable(
    entrenchment_of_living_constitutionalism,
    'Is the rise in institutional entrenchment of living constitutionalism (measured by theater_ratio rise and persistent occupancy of the judiciary by living constitutionalist judges) a sign of successful coordination, or is it becoming a piton—a constraint maintained by institutional inertia rather than genuine justification?',
    'Monitor the proportion of constitutional adjudication that invokes ''contemporary circumstances'' vs. textual interpretation; measure resistance and theater independently; assess whether a new originalist majority would actively overturn living constitutionalist precedents or would treat them as settled law.',
    'If living constitutionalism is maintained by genuine institutional fitness for the coordination problem (adapting a static text), it should persist across political changes. If it is maintained primarily by institutional inertia and its original justification has weakened, it may be reclassified as piton (ceremonial maintenance of a degraded function).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(entrenchment_of_living_constitutionalism, empirical, 'Whether living constitutionalism''s institutional entrenchment reflects ongoing justification or inertial maintenance.').

omega_variable(
    suppression_of_originalism,
    'Is the measured suppression (0.22) accurate, or does living constitutionalism suppress originalist alternatives through institutional gatekeeping that is not visible in formal argument suppression?',
    'Examine appointment processes, law school hiring, judicial confirmation, and publication of legal scholarship: are originalist candidates systematically excluded from elite positions? Do law schools and journals suppress originalist arguments?',
    'If institutional suppression of originalism is present, the suppression metric should be higher (0.35+), raising the extraction component and shifting the type toward snare rather than rope. If suppression is low and originalism thrives in competing institutional spaces, the constraint remains coordinative with genuine alternatives present.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_of_originalism, empirical, 'Whether suppression of originalism is structural or merely competitive.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_constitution_text__living_constitutionalist_reading, 1965, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(us_c_tr_t1965, us_constitution_text__living_constitutionalist_reading, theater_ratio, 1965, 0.08).
narrative_ontology:measurement(us_c_tr_t1980, us_constitution_text__living_constitutionalist_reading, theater_ratio, 1980, 0.11).
narrative_ontology:measurement(us_c_tr_t1995, us_constitution_text__living_constitutionalist_reading, theater_ratio, 1995, 0.14).
narrative_ontology:measurement(us_c_tr_t2010, us_constitution_text__living_constitutionalist_reading, theater_ratio, 2010, 0.16).
narrative_ontology:measurement(us_c_tr_t2025, us_constitution_text__living_constitutionalist_reading, theater_ratio, 2025, 0.18).

% Extraction over time
narrative_ontology:measurement(us_c_be_t1965, us_constitution_text__living_constitutionalist_reading, base_extractiveness, 1965, 0.28).
narrative_ontology:measurement(us_c_be_t1980, us_constitution_text__living_constitutionalist_reading, base_extractiveness, 1980, 0.32).
narrative_ontology:measurement(us_c_be_t1995, us_constitution_text__living_constitutionalist_reading, base_extractiveness, 1995, 0.35).
narrative_ontology:measurement(us_c_be_t2010, us_constitution_text__living_constitutionalist_reading, base_extractiveness, 2010, 0.37).
narrative_ontology:measurement(us_c_be_t2025, us_constitution_text__living_constitutionalist_reading, base_extractiveness, 2025, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(us_c_su_t1965, us_constitution_text__living_constitutionalist_reading, suppression_requirement, 1965, 0.15).
narrative_ontology:measurement(us_c_su_t1980, us_constitution_text__living_constitutionalist_reading, suppression_requirement, 1980, 0.18).
narrative_ontology:measurement(us_c_su_t1995, us_constitution_text__living_constitutionalist_reading, suppression_requirement, 1995, 0.2).
narrative_ontology:measurement(us_c_su_t2010, us_constitution_text__living_constitutionalist_reading, suppression_requirement, 2010, 0.21).
narrative_ontology:measurement(us_c_su_t2025, us_constitution_text__living_constitutionalist_reading, suppression_requirement, 2025, 0.22).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_constitution_text__living_constitutionalist_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(us_constitution_text__living_constitutionalist_reading, 0.12).
narrative_ontology:affects_constraint(us_constitution_text__living_constitutionalist_reading, us_constitution_text__originalist_reading).
narrative_ontology:affects_constraint(us_constitution_text__living_constitutionalist_reading, us_constitution_text__positivist_reading).

% DUAL FORMULATION NOTE:
% Living constitutionalism is one reading of the contested kernel 'us_constitution_text.' The same kernel is instantiated by originalist_reading (meaning fixed at ratification) and positivist_reading (validity derives from procedure). These three constraints form a constraint family: each is a reading of the same constitutional text, but each assumes different authority relations and produces different beneficiary/victim structures. The three readings compete across institutions (different judges, different schools of thought); they do not coexist within a single framework—a judge cannot simultaneously be a living constitutionalist and an originalist about the same provision. The family decomposition respects ε-invariance: each reading has its own ε (living constitutionalist: extractiveness 0.38, suppression 0.22; originalist would be: extractiveness 0.15, suppression 0.08; positivist would be: extractiveness 0.25, suppression 0.15). Separate files capture the structural differences, and network links trace the contention.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
