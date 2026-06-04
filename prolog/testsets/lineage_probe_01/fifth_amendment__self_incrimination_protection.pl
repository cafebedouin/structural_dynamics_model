% ============================================================================
% CONSTRAINT STORY: fifth_amendment__self_incrimination_protection
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fifth_amendment__self_incrimination_protection, []).

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
 *   constraint_id: fifth_amendment__self_incrimination_protection
 *   human_readable: Fifth Amendment Privilege Against Self-Incrimination
 *   domain: legal/constitutional_doctrine
 *
 * SUMMARY:
 *   The Fifth Amendment's self-incrimination privilege is one reading of a
 *   contested constitutional kernel with multiple competing interpretations.
 *   This reading instantiates the privilege as a protection against compelled
 *   self-testimony in interrogation and trial — the core claim that the state
 *   cannot make the accused the instrument of his own conviction. The
 *   privilege operates structurally as a tangled-rope constraint: it provides
 *   genuine coordination (prevents coerced confessions, establishes
 *   procedural legitimacy) while simultaneously extracting costs from
 *   truth-discovery (suppresses reliable confessions, prevents defendants
 *   from being forced to exculpate themselves, creates asymmetric trial
 *   strategy advantages for prosecution). The extractiveness value (0.38)
 *   reflects moderate baseline extraction — the privilege does suppress
 *   confessions, but modern investigation generates convictions through
 *   forensics and witness testimony, reducing the privilege's functional
 *   necessity. The suppression value (0.62) is high because the exclusionary
 *   rule and Miranda prophylaxis create significant barriers to
 *   confession-based prosecution. The theater ratio (0.55) reflects the
 *   privilege's dual nature: some interrogation procedure is genuinely
 *   regulated (recording, warnings, counsel presence), but the ritual of
 *   asserting and denying the right persists even when confessions would not
 *   determine outcomes in the empirical investigation picture.
 *
 * KEY AGENTS:
 *   - Suspects under interrogation (powerless/trapped): Primary beneficiaries — the privilege prevents coerced confession. Trapped because they cannot exit the interrogation; rope classification because the privilege provides genuine coordination solution to asymmetric pressure.
 *   - Defendants at trial (moderate/constrained): Beneficiaries with strategic ambiguity — the privilege protects but also constrains (remaining silent creates adverse inference). Tangled rope because genuine protection mixed with asymmetric extraction.
 *   - Confession-dependent prosecution (moderate/constrained): Victims of the privilege's suppression — confession routes eliminated by exclusionary rule. Snare classification because extraction is severe (confessions suppressed) with no coordination benefit.
 *   - Criminal justice system (institutional/constrained): Experiences the privilege as a coordination mechanism for procedural legitimacy balanced against investigation efficiency costs. Tangled rope because genuine coordination (legitimacy) and efficiency extraction.
 *   - Police practice reform (institutional/mobile): Sees the privilege as temporary scaffolding for interrogation reform. Emerging alternatives (transparency, legal counsel, recording) could replace the exclusionary rule if they reach sufficient reliability.
 *   - Philosophical doctrine (institutional/analytical): The deontological principle persists through institutional inertia even as its functional necessity has degraded. Piton classification because the principle is maintained largely through theater (ritual assertion) rather than functional suppression.
 *   - Analytical observer (analytical/analytical): From the civilizational/universal perspective, the privilege appears as an immutable principle grounded in human dignity. Vulnerable to false summit detection: if the deontological grounding is contingent, the mountain is constructed.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fifth_amendment__self_incrimination_protection, 0.38).
domain_priors:suppression_score(fifth_amendment__self_incrimination_protection, 0.62).
domain_priors:theater_ratio(fifth_amendment__self_incrimination_protection, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fifth_amendment__self_incrimination_protection, extractiveness, 0.38).
narrative_ontology:constraint_metric(fifth_amendment__self_incrimination_protection, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(fifth_amendment__self_incrimination_protection, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fifth_amendment__self_incrimination_protection, tangled_rope).
narrative_ontology:human_readable(fifth_amendment__self_incrimination_protection, "Fifth Amendment Privilege Against Self-Incrimination").
narrative_ontology:topic_domain(fifth_amendment__self_incrimination_protection, "legal/constitutional_doctrine").

domain_priors:requires_active_enforcement(fifth_amendment__self_incrimination_protection).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fifth_amendment__self_incrimination_protection, '5265cc2e-dcc4-4d1e-a635-2cc6a8a38a7c').
narrative_ontology:cs_kernel_codification('5265cc2e-dcc4-4d1e-a635-2cc6a8a38a7c', formalized).
narrative_ontology:cs_authority_grounding('5265cc2e-dcc4-4d1e-a635-2cc6a8a38a7c', lineage).
narrative_ontology:cs_interpretation_layer_present('5265cc2e-dcc4-4d1e-a635-2cc6a8a38a7c').
narrative_ontology:cs_reading_relation('5265cc2e-dcc4-4d1e-a635-2cc6a8a38a7c', fifth_amendment__double_jeopardy_bar, coexists_with).
narrative_ontology:cs_reading_relation('5265cc2e-dcc4-4d1e-a635-2cc6a8a38a7c', fifth_amendment__takings_just_compensation, coexists_with).
narrative_ontology:cs_axiom('5265cc2e-dcc4-4d1e-a635-2cc6a8a38a7c', foundational, persons_not_instruments_own_conviction).
narrative_ontology:cs_axiom_status(persons_not_instruments_own_conviction, holdable).
narrative_ontology:cs_axiom_grounding('5265cc2e-dcc4-4d1e-a635-2cc6a8a38a7c', persons_not_instruments_own_conviction, deontological).
narrative_ontology:cs_axiom('5265cc2e-dcc4-4d1e-a635-2cc6a8a38a7c', secondary, interrogation_coercion_empirically_harmful).
narrative_ontology:cs_axiom_status(interrogation_coercion_empirically_harmful, holdable).
narrative_ontology:cs_axiom_grounding('5265cc2e-dcc4-4d1e-a635-2cc6a8a38a7c', interrogation_coercion_empirically_harmful, empirically_contingent).
narrative_ontology:cs_reference_frame('5265cc2e-dcc4-4d1e-a635-2cc6a8a38a7c', protection_against_compelled_testimony).
narrative_ontology:cs_drift_state('5265cc2e-dcc4-4d1e-a635-2cc6a8a38a7c', contemporary_forensic_evidence_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('5265cc2e-dcc4-4d1e-a635-2cc6a8a38a7c', '2026-02-26T14:32:00Z').
narrative_ontology:cs_kernel_id(fifth_amendment__self_incrimination_protection, fifth_amendment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fifth_amendment__self_incrimination_protection, suspects_under_interrogation).
narrative_ontology:constraint_beneficiary(fifth_amendment__self_incrimination_protection, defendants_at_trial).
narrative_ontology:constraint_victim(fifth_amendment__self_incrimination_protection, confession_dependent_prosecution).
narrative_ontology:constraint_victim(fifth_amendment__self_incrimination_protection, truth_discovery_efficiency).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SUSPECT UNDER INTERROGATION (ROPE) — Immediate time, trapped exit. The Fifth Amendment functions as pure coordination mechanism at this moment: it solves the collective-action problem of asymmetric information and coercive pressure. The suspect cannot exit the interrogation room, but the privilege provides a coordination solution that prevents coerced confession. Experienced as minimal extraction because the function is genuine protection, not exploitation.
constraint_indexing:constraint_classification(fifth_amendment__self_incrimination_protection, rope,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: DEFENDANT AT TRIAL (TANGLED ROPE) — Biographical horizon, constrained exit (can testify or remain silent but both carry strategic costs). The privilege simultaneously protects and constrains: it prevents coerced self-incrimination but also prevents the defendant from being forced to exculpate themselves through testimony. The constraint coordinates trial procedure while extracting a strategic disadvantage (prosecution benefits from the defendant's silence as probative of guilt despite explicit instruction to the jury). Genuine coordination function mixed with asymmetric extraction.
constraint_indexing:constraint_classification(fifth_amendment__self_incrimination_protection, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: CONFESSION-DEPENDENT PROSECUTION (SNARE) — For prosecution strategies built on suspect confessions, the privilege operates as pure extraction: it removes the mechanism for obtaining the confession. No coordination benefit to the prosecution; high suppression of the confession route; effective extraction against investigative leverage. The prosecution faces suppression (Miranda exclusionary rule) with minimal coordination function.
constraint_indexing:constraint_classification(fifth_amendment__self_incrimination_protection, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: THE CRIMINAL JUSTICE SYSTEM (TANGLED ROPE) — Generational view. The privilege coordinates a procedural framework (Miranda warnings, in-custody interrogation rules) that provides legitimacy and predictability to the system itself. But it also extracts efficiency costs: confession rates drop, interrogation must be recorded and procedurally regulated, investigative strategy becomes more expensive. The system coordinates its own legitimacy while paying extraction costs in investigation efficiency.
constraint_indexing:constraint_classification(fifth_amendment__self_incrimination_protection, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: POLICE PRACTICE REFORM (SCAFFOLD) — The privilege operates as temporary scaffolding for interrogation reform. Body cameras, recorded custodial interrogations, and legal representation during questioning are coordinating mechanisms that could eventually replace the privilege if they become sufficient (everyone can see the interrogation process, so coercion becomes visible and actionable through civil liability rather than exclusionary rule). Current state: χ ≤ 0.30, theater moderate. Sunset clause implicit: if procedural transparency reaches legal standards, the privilege's suppression mechanism becomes redundant.
constraint_indexing:constraint_classification(fifth_amendment__self_incrimination_protection, scaffold,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: PHILOSOPHICAL DOCTRINE (PITON) — Civilizational/universal view. The principle that the state cannot make the accused the instrument of his own conviction has become substantially performative. Modern investigation (forensics, digital data, witness testimony, physical evidence) generates convictions with or without confessions. The doctrine persists through institutional inertia and legitimacy narrative ('the Fifth Amendment protects us') even though its functional necessity has degraded. Theater ratio high: the ritual of asserting the right persists, but the actual exclusion of confessions rarely determines outcomes in the total investigative picture.
constraint_indexing:constraint_classification(fifth_amendment__self_incrimination_protection, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / DEONTOLOGICAL NATURAL LAW (MOUNTAIN) — A civilizational/universal perspective grounded in the deontological claim that the state simply cannot, as a matter of fundamental right, compel a person to incriminate themselves. This is a principle, not a pragmatic arrangement. From this view, the privilege is immutable — it does not depend on empirical effectiveness or institutional efficiency, but on moral necessity. The privilege emerges naturally from the principle that human persons possess rights that cannot be violated regardless of state interest. However, this classification is vulnerable to false summit detection: if the deontological axiom is contingent (grounded in 18th-century political theory, not in timeless right), then the mountain is constructed.
constraint_indexing:constraint_classification(fifth_amendment__self_incrimination_protection, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fifth_amendment__self_incrimination_protection_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(fifth_amendment__self_incrimination_protection, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(fifth_amendment__self_incrimination_protection, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(fifth_amendment__self_incrimination_protection, TR),
    TR >= 0.70.

:- end_tests(fifth_amendment__self_incrimination_protection_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The privilege suppresses confessions, but baseline interrogation-dependent conviction rates have declined as forensic and digital evidence have become primary investigative tools. Modern prosecution generates convictions without confessions in a substantial subset of cases. The measurement trajectory shows rising extractiveness (0.28 → 0.38) over 50 years, indicating that the privilege's suppression mechanism persists while the empirical necessity for confessions has declined — confessions remain valuable but not dominant. Suppression (0.62): High. The exclusionary rule and Miranda prophylaxis create significant barriers: custodial interrogation requires warnings, legal counsel, recording in many jurisdictions, and documented voluntariness. These are genuine suppression mechanisms, not theater — they reduce confession rates empirically. Theater ratio (0.55): Moderate-high. The privilege's functional necessity has degraded (modern investigation succeeds without confessions) but the doctrinal ritual persists (Miranda warnings remain required, rights are asserted, interrogation is regulated). The increase from 0.35 to 0.55 over 50 years reflects the gap growing between the doctrine's symbolic authority and its empirical necessity. Tangled rope classification: Genuine coordination (legitimacy of interrogation procedure) combined with asymmetric extraction (prosecution suppressed from confession route, defendant constrained by silence inference) justifies the type. The privilege is not pure extraction (snare) because it does prevent coercion and establish procedural legitimacy. The privilege is not pure coordination (rope) because it extracts real costs from confession-dependent investigation.
 *
 * PERSPECTIVAL GAP:
 *   The core perspectival gap is between the deontological (mountain/piton) reading and the empirical (tangled rope/scaffold) reading. From the deontological perspective, the privilege is immutable because it rests on the principle that persons have intrinsic rights that cannot be violated for state efficiency. From the empirical perspective, the privilege is contingent — its necessity depends on whether confessions are reliably probative and whether alternatives exist. As forensic evidence has strengthened and interrogation transparency has improved, the empirical case for the exclusionary rule has weakened, shifting the constraint toward piton (maintained through legitimacy theater rather than functional necessity). The deontological observer does not see this shift because the axiom (persons cannot be instruments of conviction) persists regardless of empirical sufficiency.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality in this reading derives from the structural relationship of each perspective to the privilege's mechanism. Suspects and defendants benefit from the suppression of coerced confession (low d toward beneficiary, producing negative or low f(d) → low χ). Confession-dependent prosecution bears the cost of exclusionary rule (high d toward victim, producing high f(d) → high χ from the prosecution's perspective). The criminal justice system experiences mixed directionality: institutional actors both benefit from legitimacy and pay efficiency costs, producing mid-range d values. The analytical observer's directionality depends on whether the analysis is grounded in deontological axiom (non-empirical, hence d-independent) or in instrumental premises (empirical, hence sensitive to confession reliability rates). No overrides are required; the derivation chain produces appropriate d values from beneficiary/victim declarations and exit options.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading avoids mandatrophy through its tangled-rope classification, which acknowledges both the coordination function (preventing coercion, establishing procedural legitimacy) and the extraction cost (suppressing reliable evidence, constraining trial strategy). The constraint is not pure extraction masquerading as coordination (snare), nor is it pure coordination (rope). The mixed classification explains why the privilege persists despite its declining empirical necessity — it provides legitimacy value to the criminal justice system that would not exist if interrogations were conducted without procedural constraints. The false summit perspective (mountain) is a temptation — the deontological axiom is compelling — but the structural data (beneficiaries, measured extraction, declining empirical necessity) reveals this as naturalization of a contingent institutional arrangement. The privilege coordinates and extracts simultaneously; both functions are real.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    confessions_empirical_reliability,
    'Are confessions empirically more reliable for establishing guilt than other evidence sources (forensics, witness testimony, physical evidence)?',
    'Meta-analysis of wrongful conviction databases (Innocence Project); comparison of conviction rates by evidence type; false confession rates in cases with external corroboration',
    'If confessions unreliable: privilege is a necessary coordination mechanism (confessions create false convictions). If confessions reliable but suppression prevents their use: privilege extracts efficiency costs from prosecution without truth-discovery benefit. If confessions highly reliable and suppressible: the tradeoff between accuracy and procedural fairness becomes acute — the mountain/piton distinction hinges on whether this tradeoff is axiological (fundamental right) or instrumental (empirical cost-benefit).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(confessions_empirical_reliability, empirical, 'Whether confessions are empirically more reliable than alternative evidence').

omega_variable(
    miranda_effectiveness_degradation,
    'Has Miranda prophylaxis maintained its effectiveness over time, or has police practice evolved to circumvent it while maintaining technical compliance?',
    'Longitudinal analysis of confession rates pre- and post-Miranda; analysis of police interrogation techniques that remain compliant with Miranda (psychological manipulation, false evidence ploys, sleep deprivation) and their coercive effects; comparison of Miranda compliance documentation vs actual interrogation coerciveness measured by third-party observers',
    'If Miranda remains effective: privilege continues to suppress coerced confessions. If circumvented: the privilege persists as theater (piton) without functional suppression. If partially degraded: constraint transitions from tangled_rope toward piton.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(miranda_effectiveness_degradation, empirical, 'Whether Miranda warnings maintain their effectiveness over time').

omega_variable(
    deontological_vs_instrumental_grounding,
    'Is the axiom ''persons cannot be made instruments of their own conviction'' grounded in a deontological right (fundamental, timeless) or in instrumental reasoning (18th-century prudential concern about state power)?',
    'Genealogical analysis of the Fifth Amendment''s drafting history, comparative constitutional law (which jurisdictions lack self-incrimination privilege and what consequences follow), philosophical investigation of whether the axiom survives when empirical premises change (e.g., if confessions became more reliable, would the axiom hold?)',
    'If deontological: the privilege is immutable (mountain classification confirmed). If instrumental: the privilege is contingent on empirical premises that may change (piton or scaffold classification becomes appropriate). If mixed: the reading contains both axioms, and the privileged classification becomes reading_dependent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deontological_vs_instrumental_grounding, conceptual, 'Whether the axiom is grounded in deontological right or instrumental reasoning').

omega_variable(
    interrogation_transparency_sufficiency,
    'Can interrogation transparency (recording, observation, legal counsel) sufficiently prevent coercion without the exclusionary rule?',
    'Comparative jurisdiction study (jurisdictions with mandatory recording vs without); analysis of false confession rates with and without third-party observation; psychological research on visibility effects on coercive interrogation techniques',
    'If transparency sufficient: scaffold sunset is real — procedural reforms could replace the exclusionary rule. If transparency insufficient: the privilege remains necessary, and scaffold perspective is aspirational rather than structural.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interrogation_transparency_sufficiency, empirical, 'Whether interrogation transparency sufficiently prevents coercion').

omega_variable(
    kernel_reading_contest,
    'Which reading captures the Fifth Amendment''s actual doctrinal core: self-incrimination protection, double jeopardy bar, or just compensation requirement?',
    'Constitutional history of the Fifth Amendment''s drafting and early interpretation; doctrine analysis of which clause receives primary judicial attention; empirical analysis of which reading generates the most litigation and exclusions',
    'If self-incrimination protection is core: this reading instantiates the doctrine''s primary function. If another reading is core: this reading is derivative or subsidiary. If all three are equally weighted: the kernel is distributed (no single core), and each reading is a legitimate independent constraint.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, empirical, 'Which reading instantiates the Fifth Amendment''s doctrinal core').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fifth_amendment__self_incrimination_protection, 1973, 2023).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fifth_si_theater_1970s, fifth_amendment__self_incrimination_protection, theater_ratio, 0, 0.35).
narrative_ontology:measurement(fifth_si_theater_1995, fifth_amendment__self_incrimination_protection, theater_ratio, 25, 0.5).
narrative_ontology:measurement(fifth_si_theater_2020s, fifth_amendment__self_incrimination_protection, theater_ratio, 50, 0.55).

% Extraction over time
narrative_ontology:measurement(fifth_si_extract_1970s, fifth_amendment__self_incrimination_protection, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(fifth_si_extract_1995, fifth_amendment__self_incrimination_protection, base_extractiveness, 25, 0.35).
narrative_ontology:measurement(fifth_si_extract_2020s, fifth_amendment__self_incrimination_protection, base_extractiveness, 50, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(fifth_si_suppress_1970s, fifth_amendment__self_incrimination_protection, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(fifth_si_suppress_1995, fifth_amendment__self_incrimination_protection, suppression_requirement, 25, 0.61).
narrative_ontology:measurement(fifth_si_suppress_2020s, fifth_amendment__self_incrimination_protection, suppression_requirement, 50, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fifth_amendment__self_incrimination_protection, enforcement_mechanism).
narrative_ontology:affects_constraint(fifth_amendment__self_incrimination_protection, fifth_amendment__double_jeopardy_bar).
narrative_ontology:affects_constraint(fifth_amendment__self_incrimination_protection, fifth_amendment__takings_just_compensation).
narrative_ontology:affects_constraint(fifth_amendment__self_incrimination_protection, miranda_exclusionary_rule).
narrative_ontology:affects_constraint(fifth_amendment__self_incrimination_protection, interrogation_coercion_suppression).

% DUAL FORMULATION NOTE:
% This is one reading of the Fifth Amendment kernel. The self-incrimination protection reading decomposes from the broader fifth_amendment constraint into its own ε-invariant story because the empirical status (confessions as evidence), suppression mechanism (exclusionary rule), and beneficiary structure (suspects and defendants) are distinct from the double jeopardy and takings readings. The kernel network link captures the constraint-family relationship: all three readings inherit authority from the same text, but each suppresses a different state power and instantiates different extractiveness values.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
