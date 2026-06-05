% ============================================================================
% CONSTRAINT STORY: incorporation_doctrine__reverse_incorporation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_incorporation_doctrine__reverse_incorporation_reading, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: incorporation_doctrine__reverse_incorporation_reading
 *   human_readable: Incorporation Doctrine — Reverse Incorporation Reading (Bolling's Federal Equal Protection via Fifth Amendment Due Process)
 *   domain: legal/constitutional_doctrine
 *
 * SUMMARY:
 *   The reverse incorporation reading of the incorporation doctrine holds
 *   that the Fifth Amendment's Due Process Clause incorporates the Fourteenth
 *   Amendment's Equal Protection guarantee by implication, binding the
 *   federal government to the same standard that Brown v. Board imposed on
 *   the states. The kernel — the contested principle of how individual rights
 *   are distributed between the Fifth and Fourteenth Amendments — admits
 *   multiple readings. This reading resolves the D.C. segregation crisis
 *   (Bolling v. Sharpe, 1954) by running the incorporation doctrine backward:
 *   instead of asking which rights are so fundamental that they must be
 *   incorporated into the Fourteenth and applied to the states, it asks
 *   whether equal protection is so fundamental that the Fifth Amendment
 *   cannot be read to permit federal segregation while the Fourteenth forbids
 *   state segregation. The reading is one of three live doctrinal options in
 *   constitutional law: the selective incorporation reading (which proceeded
 *   right-by-right, testing each guarantee for fundamentality and absorbing
 *   it case by case), the total incorporation reading (Justice Black's claim
 *   that the Fourteenth Amendment wholesale absorbed the Bill of Rights at
 *   ratification), and this reverse incorporation reading (which imports
 *   equal protection into the Fifth Amendment through due process logic). The
 *   constraint exhibits the structure of a tangled rope: it coordinates
 *   federal and state equal protection obligations (genuine coordination
 *   function) while imposing an asymmetric burden on those who object that
 *   the Fifth Amendment's text does not explicitly name equal protection
 *   (victims bear the cost of doctrinal reinterpretation).
 *
 * KEY AGENTS:
 *   - Claimants Against Federal Discrimination: Beneficiaries (organized/constrained) — the doctrine creates a federal equal protection cause of action where none existed before, closing a sanctuary that the federal government could previously exploit
 *   - Textual Symmetry Objection: Primary victim (analytical/trapped) — the objection has no institutional venue to compel reconsideration; it is overridden by the doctrine's adoption and can only be raised in dissent or scholarly critique
 *   - Federalism Interest: Secondary victim (institutional/constrained) — the preservation of federal/state asymmetry in equal protection scope served a structural coordination function; the doctrine collapses this asymmetry, imposing new constraints on federal action
 *   - Warren Court Coalition: Institutional architect (institutional/mobile) — the judicial coalition that adopted Bolling understood it as a bridging solution to the D.C. segregation crisis, with eventual supersession by more comprehensive doctrinal solutions
 *   - Conservative Originalist Judiciary: Institutional steward (institutional/arbitrage) — maintains the doctrine as binding precedent while narrowing its practical scope through other doctrinal moves, creating performative compliance
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing the reading as a logical necessity of constitutional coherence, obscuring the normative choices embedded in the doctrine
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(incorporation_doctrine__reverse_incorporation_reading, 0.32).
domain_priors:suppression_score(incorporation_doctrine__reverse_incorporation_reading, 0.48).
domain_priors:theater_ratio(incorporation_doctrine__reverse_incorporation_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(incorporation_doctrine__reverse_incorporation_reading, extractiveness, 0.32).
narrative_ontology:constraint_metric(incorporation_doctrine__reverse_incorporation_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(incorporation_doctrine__reverse_incorporation_reading, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(incorporation_doctrine__reverse_incorporation_reading, tangled_rope).
narrative_ontology:human_readable(incorporation_doctrine__reverse_incorporation_reading, "Incorporation Doctrine — Reverse Incorporation Reading (Bolling's Federal Equal Protection via Fifth Amendment Due Process)").
narrative_ontology:topic_domain(incorporation_doctrine__reverse_incorporation_reading, "legal/constitutional_doctrine").

domain_priors:requires_active_enforcement(incorporation_doctrine__reverse_incorporation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(incorporation_doctrine__reverse_incorporation_reading, '03a03cc3-123d-4a51-bcf5-8fb48c58f93d').
narrative_ontology:cs_kernel_codification('03a03cc3-123d-4a51-bcf5-8fb48c58f93d', formalized).
narrative_ontology:cs_authority_grounding('03a03cc3-123d-4a51-bcf5-8fb48c58f93d', lineage).
narrative_ontology:cs_interpretation_layer_present('03a03cc3-123d-4a51-bcf5-8fb48c58f93d').
narrative_ontology:cs_reading_relation('03a03cc3-123d-4a51-bcf5-8fb48c58f93d', incorporation_doctrine__selective_incorporation_reading, coexists_with).
narrative_ontology:cs_reading_relation('03a03cc3-123d-4a51-bcf5-8fb48c58f93d', incorporation_doctrine__total_incorporation_reading, coexists_with).
narrative_ontology:cs_axiom('03a03cc3-123d-4a51-bcf5-8fb48c58f93d', foundational, constitutional_coherence_trumps_textual_symmetry).
narrative_ontology:cs_axiom_status(constitutional_coherence_trumps_textual_symmetry, holdable).
narrative_ontology:cs_axiom_grounding('03a03cc3-123d-4a51-bcf5-8fb48c58f93d', constitutional_coherence_trumps_textual_symmetry, deontological).
narrative_ontology:cs_axiom('03a03cc3-123d-4a51-bcf5-8fb48c58f93d', foundational, federal_caste_incompatible_with_state_equal_protection).
narrative_ontology:cs_axiom_status(federal_caste_incompatible_with_state_equal_protection, holdable).
narrative_ontology:cs_axiom_grounding('03a03cc3-123d-4a51-bcf5-8fb48c58f93d', federal_caste_incompatible_with_state_equal_protection, deontological).
narrative_ontology:cs_reference_frame('03a03cc3-123d-4a51-bcf5-8fb48c58f93d', textual_equal_protection_symmetry).
narrative_ontology:cs_drift_state('03a03cc3-123d-4a51-bcf5-8fb48c58f93d', contemporary_originalist_jurisprudence, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('03a03cc3-123d-4a51-bcf5-8fb48c58f93d', '').
narrative_ontology:cs_kernel_id(incorporation_doctrine__reverse_incorporation_reading, incorporation_doctrine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(incorporation_doctrine__reverse_incorporation_reading, claimants_against_federal_discrimination).
narrative_ontology:constraint_beneficiary(incorporation_doctrine__reverse_incorporation_reading, equal_protection_claimants).
narrative_ontology:constraint_victim(incorporation_doctrine__reverse_incorporation_reading, textual_symmetry_objection).
narrative_ontology:constraint_victim(incorporation_doctrine__reverse_incorporation_reading, federalism_constraint).
narrative_ontology:constraint_victim(incorporation_doctrine__reverse_incorporation_reading, structural_gap_preservation).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TEXTUAL SYMMETRY OBJECTION (SNARE) — The Fifth Amendment contains no Equal Protection Clause; the Fourteenth Amendment explicitly names equal protection as a limitation on states. A doctrine that reads equal protection into the Fifth Amendment through the back door of due process is bootstrapping a right not explicitly granted to the federal government. This perspective sees the constraint as pure extraction of constitutional authority divorced from text. Maximum experienced extraction because the objection has no institutional venue to compel reconsideration and no political exit option.
constraint_indexing:constraint_classification(incorporation_doctrine__reverse_incorporation_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: FEDERALISM INTEREST (TANGLED ROPE) — The preservation of a federal/state asymmetry in equal protection obligations serves a genuine coordination function: it reserved questions of local social ordering to state legislatures and courts, while simultaneously imposing a structural constraint on federal overreach. The reverse incorporation reading collapses this asymmetry, creating a uniform equal protection obligation across both governments. The federalism interest experiences mixed extraction and coordination: genuine structural benefit from asymmetry (coordination between federal and state authority domains) combined with asymmetric imposition of a new federal obligation that was not textually specified.
constraint_indexing:constraint_classification(incorporation_doctrine__reverse_incorporation_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: CIVIL RIGHTS CLAIMANTS AND ADVOCATES (ROPE) — For claimants facing federal discrimination (housing discrimination by federal agencies, employment discrimination by federal contractors, segregation in D.C. schools), this doctrine solves a coordination problem: it creates a unified equal protection standard that prevents the federal government from maintaining a caste system while the states are bound by Brown. The constraint functions as coordination — establishing a single legitimacy standard across both sovereigns — with minimal extraction overhead. The civil rights perspective sees the doctrine as reducing extraction by closing the federal sanctuary.
constraint_indexing:constraint_classification(incorporation_doctrine__reverse_incorporation_reading, rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: CONSERVATIVE ORIGINALIST JUDICIARY (PITON) — The reverse incorporation reading has become largely performative within originalist jurisprudence: judges claim to follow it while simultaneously narrowing its scope through other doctrinal moves (equal protection rational-basis review for federal action remains toothless; strict scrutiny standards differ between federal and state action). The doctrine persists through institutional inertia — recognized as binding authority from Bolling onward — but the active enforcement mechanism (vigorous equal protection review of federal action) has atrophied. Theater ratio is high because the reading is invoked as precedent but generates minimal actual constraint on federal authority.
constraint_indexing:constraint_classification(incorporation_doctrine__reverse_incorporation_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a constitutional necessity perspective, once Brown v. Board locked the states into equal protection, logical consistency required that the federal government be bound by the same principle. A reading of the Fifth Amendment's Due Process Clause to incorporate equal protection is seen as a structural inevitability: the alternative (federal government running a segregated capital while Brown prohibits segregated states) is jurisprudentially impossible. This perspective naturalizes the reverse incorporation reading as a logical consequence of constitutional coherence. However, this classification is vulnerable to FSM analysis: the 'necessity' claim assumes that unequal treatment of federal and state governments is intolerable, which is itself a normative choice, not a natural law.
constraint_indexing:constraint_classification(incorporation_doctrine__reverse_incorporation_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 6: THE WARREN COURT COALITION (SCAFFOLD) — For the judicial and legislative actors who adopted and propagated this doctrine in the 1950s-1960s, the reverse incorporation reading served as a temporary bridging mechanism: a sunset-constrained solution to the D.C. segregation crisis that would eventually be superseded by more comprehensive incorporation mechanisms (either total incorporation of the Fourteenth Amendment or explicit congressional equal protection legislation directed at federal action). The coalition saw this as a stopgap — necessary to reach the right outcome (D.C. students cannot be segregated) but understood as part of a broader doctrinal evolution. Theater is moderate because the doctrine genuinely solved the immediate problem while remaining scaffolding toward a more stable solution.
constraint_indexing:constraint_classification(incorporation_doctrine__reverse_incorporation_reading, scaffold,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(incorporation_doctrine__reverse_incorporation_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(incorporation_doctrine__reverse_incorporation_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(incorporation_doctrine__reverse_incorporation_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(incorporation_doctrine__reverse_incorporation_reading, TR),
    TR >= 0.70.

:- end_tests(incorporation_doctrine__reverse_incorporation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.32): Moderate-low, trending upward over time. At t=0 (1954), the doctrine's extractiveness was low (0.18) because it genuinely solved an urgent problem (D.C. segregation) that had no textual remedy in the Fifth Amendment; the coordination function dominated. Over the subsequent 20 years (t=0 to t=20), extractiveness increased (0.18 → 0.32) as the doctrine's use expanded beyond its immediate D.C. context and as conservative forces began narrowing its scope through other doctrinal moves. The increase reflects accumulating enforcement burden on the federal government without corresponding increase in practical constraint on federal discrimination (many federal discrimination claims continue to fail on rational basis review). Suppression (0.48): Moderate-high. The textual symmetry objection is suppressed — it cannot compel reconsideration of the doctrine and has no institutional venue for enforcement. Federalism interests are suppressed — states cannot appeal to the Fifth Amendment asymmetry to reclaim authority. But suppression is not total: originalist judges maintain the text-as-written objection in dissent and scholarly discourse, and federalism interests persist in structural political dynamics. Theater ratio (0.55): Moderate-high, trending upward. At t=0, theater was lower (0.38) because the doctrine was freshly adopted as a solution to an urgent problem. Over time, theater increased (0.38 → 0.55) as the doctrine became absorbed into standard constitutional canon and its active enforcement mechanism atrophied relative to its invocation as precedent. Claimed type (tangled_rope): The doctrine exhibits both coordination (uniform equal protection standard) and extraction (asymmetric reinterpretation of the Fifth Amendment; burden on federalism interests). It requires active enforcement (requires_active_enforcement: true) to remain viable against textual objections.
 *
 * PERSPECTIVAL GAP:
 *   The gap between perspectives reflects the structural tension embedded in the doctrine itself. The textual symmetry objection sees pure extraction (snare) — the doctrine imposes an obligation not grounded in the Fifth Amendment's text. The federalism interest sees mixed coordination and extraction (tangled rope) — the doctrine solves a coherence problem while imposing asymmetric burden. Civil rights claimants see coordination (rope) — the doctrine closes a federal sanctuary and establishes equality. The originalist judiciary sees performative obligation (piton) — the doctrine is binding precedent whose practical scope is narrowed through other mechanisms. The Warren Court coalition saw temporary bridging (scaffold) — the doctrine was understood as a sunset solution. The analytical observer risks seeing natural law (mountain) — the doctrine is naturalized as a logical necessity of constitutional coherence. The perspectival gap reveals that the 'correctness' of the doctrine depends entirely on whether one prioritizes textual fidelity (where reverse incorporation fails), structural coherence (where reverse incorporation succeeds), federalism preservation (where reverse incorporation imposes costs), or civil rights protection (where reverse incorporation provides gains).
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality is derived from its structural relationship to the extraction flow. Claimants against federal discrimination are beneficiaries with constrained exit — they cannot opt out of federal authority and had no prior federal equal protection remedy; their d-value is low (~0.20), yielding low experienced extraction (chi). The textual symmetry objection is a victim with trapped exit — it cannot escape the doctrine once adopted and has no institutional venue for enforcement; its d-value is high (~0.90), yielding high experienced extraction. The federalism interest is an institutional victim with constrained exit — it cannot prevent federal action but can preserve some federalism through other constitutional mechanisms; its d-value is moderate (~0.55). The Warren Court coalition is an institutional beneficiary with mobile exit — it can choose whether and how aggressively to enforce the doctrine; its d-value is low (~0.25). The originalist judiciary is an institutional beneficiary with arbitrage exit — it maintains doctrinal compliance while narrowing scope; its d-value is very low (~0.10). The analytical observer has d-value ~0.72 (standard canonical fallback for analytical power), experiencing moderate extraction from the requirement to navigate the tension between coherence and text.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    back_door_incorporation_vs_independent_ground,
    'Is the reverse incorporation reading an illegitimate ''back door'' incorporation of the Fourteenth Amendment''s equality guarantee into the Fifth Amendment, or a legitimate independent recognition of equal protection inherent in Fifth Amendment due process?',
    'Historical analysis of Framers'' intent regarding Fifth Amendment scope; textual comparison with Fourteenth Amendment''s explicit equal protection language; jurisprudential review of due process doctrines that predate and postdate Bolling',
    'If back-door incorporation: the constraint''s extractiveness should be higher (χ ≥ 0.60, classification Snare for textual perspectives). If independent ground: extractiveness justified as coordinate recognition, not extraction (χ ≤ 0.45, classification Rope for textualist perspectives).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(back_door_incorporation_vs_independent_ground, conceptual, 'Whether reverse incorporation is illegitimate doctrinal overreach or legitimate independent constitutional ground').

omega_variable(
    federalism_gap_continuity,
    'Does the reverse incorporation reading genuinely suppress a federal gap in equality law, or does it create a new obligation while leaving the federal government''s previously-existing equal protection duties (statutory civil rights law, equal protection in administrative procedure) untouched?',
    'Doctrinal archaeology: catalog all federal equal protection doctrines and duties before Bolling (1954) vs. after; identify which duties originate from reverse incorporation vs. which from other sources (civil rights statutes, administrative law)',
    'If gap-suppressing: beneficiary classification is justified; extractiveness reflects genuine closure of a sanctuary. If gap-creating: the doctrine creates new obligations while leaving the structural asymmetry partly intact; extractiveness higher because federal discrimination was already prohibited through other channels (criminal statute, civil rights act); reverse incorporation merely refocuses it through constitutional doctrine.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(federalism_gap_continuity, empirical, 'Whether the reverse incorporation reading suppresses or creates a federal equal protection gap').

omega_variable(
    textual_symmetry_cost_vs_coherence_gain,
    'What is the weight of the cost imposed by violating textual symmetry (Fifth Amendment does not explicitly name equal protection) against the coherence gain (uniform equal protection standard across federal and state governments)?',
    'Comparative jurisprudence: review cases where courts chose textual fidelity over doctrinal coherence vs. cases where coherence overrode text; identify doctrinal stability and legitimacy consequences of each choice',
    'If symmetry is weighted heavily: doctrine is extractive (victims bear the cost of reinterpreting the Fifth Amendment). If coherence is weighted heavily: doctrine is coordinate (beneficiaries gain enforcement of a logical constitutional principle).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(textual_symmetry_cost_vs_coherence_gain, preference, 'Normative weight of textual symmetry costs vs. constitutional coherence gains').

omega_variable(
    reverse_vs_forward_incorporation_boundary,
    'Does the reverse incorporation reading foreclose the selective incorporation reading, or do both remain live doctrinal options that the judiciary can choose between on a case-by-case basis?',
    'Doctrinal observation: analyze post-Bolling jurisprudence to determine whether courts cite Bolling as binding precedent that forecloses selective incorporation, or whether selective incorporation continues to operate as an alternative method for absorbing rights into the Fourteenth Amendment',
    'If forecloses: reading_relations includes foreclosure edge. If coexists: relation is coexists_with. Classification consequence: forecloses raises stakes of the doctrinal dispute; coexists_with suggests the readings are compatible strategies operating at different levels.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reverse_vs_forward_incorporation_boundary, empirical, 'Whether reverse incorporation forecloses selective incorporation or coexists with it').

omega_variable(
    federal_equal_protection_enforcement_scope,
    'Does the reverse incorporation reading bind only federal legislative and executive action, or does it also govern the federal judiciary''s own discrimination (e.g., in appointment, courtroom access, jury selection)?',
    'Case law analysis: track equal protection claims against federal courts and judges; identify whether Bolling reasoning extends to judicial conduct or only to nonjudicial federal action',
    'If limited to nonjudicial action: the constraint''s suppression is incomplete; federal caste preservation mechanisms within the judiciary remain protected. If universal: suppression is more complete, but the doctrine''s enforcement burden is higher (federal courts must police their own discrimination), creating complexity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(federal_equal_protection_enforcement_scope, empirical, 'Scope of federal equal protection obligation: nonjudicial action only, or including judicial conduct').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(incorporation_doctrine__reverse_incorporation_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(incorp_rev_theater_t0, incorporation_doctrine__reverse_incorporation_reading, theater_ratio, 0, 0.38).
narrative_ontology:measurement(incorp_rev_theater_t10, incorporation_doctrine__reverse_incorporation_reading, theater_ratio, 10, 0.52).
narrative_ontology:measurement(incorp_rev_theater_t20, incorporation_doctrine__reverse_incorporation_reading, theater_ratio, 20, 0.55).

% Extraction over time
narrative_ontology:measurement(incorp_rev_extract_t0, incorporation_doctrine__reverse_incorporation_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(incorp_rev_extract_t10, incorporation_doctrine__reverse_incorporation_reading, base_extractiveness, 10, 0.28).
narrative_ontology:measurement(incorp_rev_extract_t20, incorporation_doctrine__reverse_incorporation_reading, base_extractiveness, 20, 0.32).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(incorporation_doctrine__reverse_incorporation_reading, information_standard).
narrative_ontology:affects_constraint(incorporation_doctrine__reverse_incorporation_reading, incorporation_doctrine__selective_incorporation_reading).
narrative_ontology:affects_constraint(incorporation_doctrine__reverse_incorporation_reading, incorporation_doctrine__total_incorporation_reading).
narrative_ontology:affects_constraint(incorporation_doctrine__reverse_incorporation_reading, fifth_amendment_due_process_scope).
narrative_ontology:affects_constraint(incorporation_doctrine__reverse_incorporation_reading, federal_equal_protection_rational_basis_floor).

% DUAL FORMULATION NOTE:
% The reverse incorporation reading is one of three structurally distinct readings of the incorporation doctrine kernel. Each reading has different extractiveness and suppression values reflecting different normative and textual commitments. The readings are linked through the incorporation_doctrine kernel; each story models one reading's classification across multiple perspectives. All three readings are live options in contemporary constitutional jurisprudence, though with different institutional support and practical enforcement rates.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(incorporation_doctrine__reverse_incorporation_reading, analytical, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
