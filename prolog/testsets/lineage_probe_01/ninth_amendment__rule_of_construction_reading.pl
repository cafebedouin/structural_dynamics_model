% ============================================================================
% CONSTRAINT STORY: ninth_amendment__rule_of_construction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ninth_amendment__rule_of_construction_reading, []).

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
 *   constraint_id: ninth_amendment__rule_of_construction_reading
 *   human_readable: The Ninth Amendment as a Rule of Construction (Not a Rights Fount)
 *   domain: constitutional_law/textual_interpretation
 *
 * SUMMARY:
 *   The Ninth Amendment reads: 'The enumeration in the Constitution of
 *   certain rights shall not be construed to deny or disparage others
 *   retained by the people.' This constraint is one reading of a contested
 *   kernel — the ambiguous text itself. The rule-of-construction reading
 *   interprets the Ninth as a hermeneutic rule: it forbids a single faulty
 *   inference (that enumeration denies unenumerated rights) but grants no
 *   substantive rights itself. It is a constraint on constitutional
 *   reasoning, not a fount of new doctrine. This reading contrasts sharply
 *   with the rights-reservoir reading (the Ninth is a reservoir of
 *   unenumerated rights retained by the people, a textual spring for liberty
 *   doctrine) and the judicial-unusability reading (the Ninth has left courts
 *   without guidance — no holding rests on it alone). The
 *   rule-of-construction reading instantiates constructional modesty: it
 *   treats the Ninth as what it most literally says — a rule forbidding one
 *   interpretive move — and denies that the clause generates substantive
 *   rights. This reading has minimal extractiveness (0.18) because it
 *   supplies pure coordination: it constrains inference without granting
 *   benefits to any actor. However, it is extractive toward those seeking
 *   doctrinal support for unenumerated rights — the reading forecloses their
 *   use of the Ninth's text as that support.
 *
 * KEY AGENTS:
 *   - Constructional Modesty (principle/beneficiary): The interpretive stance that forbids one inference move; benefits from clarity about what the Ninth does and does not do
 *   - Rights-Reservoir Reading (sibling constraint/victim): The alternative reading that treats the Ninth as a substantive fount; extracted by the rule-of-construction's negation
 *   - Claimants for Unenumerated Rights (powerless/trapped): Those seeking textual foundation for privacy, autonomy, associational liberty; cannot use the Ninth under this reading
 *   - Textualist Judiciary (institutional/arbitrage): Courts committed to limiting the Ninth to its text; benefit from the clarity the rule-of-construction reading provides
 *   - Analytical Observer (civilizational): The universal principle that enumeration does not entail denial
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ninth_amendment__rule_of_construction_reading, 0.18).
domain_priors:suppression_score(ninth_amendment__rule_of_construction_reading, 0.32).
domain_priors:theater_ratio(ninth_amendment__rule_of_construction_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ninth_amendment__rule_of_construction_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(ninth_amendment__rule_of_construction_reading, suppression_requirement, 0.32).
narrative_ontology:constraint_metric(ninth_amendment__rule_of_construction_reading, theater_ratio, 0.25).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ninth_amendment__rule_of_construction_reading, rope).
narrative_ontology:human_readable(ninth_amendment__rule_of_construction_reading, "The Ninth Amendment as a Rule of Construction (Not a Rights Fount)").
narrative_ontology:topic_domain(ninth_amendment__rule_of_construction_reading, "constitutional_law/textual_interpretation").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ninth_amendment__rule_of_construction_reading, 'e41debe6-7ce4-4ddb-9f77-c1b13218a9a9').
narrative_ontology:cs_kernel_codification('e41debe6-7ce4-4ddb-9f77-c1b13218a9a9', fixed_text).
narrative_ontology:cs_authority_grounding('e41debe6-7ce4-4ddb-9f77-c1b13218a9a9', lineage).
narrative_ontology:cs_interpretation_layer_present('e41debe6-7ce4-4ddb-9f77-c1b13218a9a9').
narrative_ontology:cs_reading_relation('e41debe6-7ce4-4ddb-9f77-c1b13218a9a9', ninth_amendment__judicial_unusability_reading, coexists_with).
narrative_ontology:cs_reading_relation('e41debe6-7ce4-4ddb-9f77-c1b13218a9a9', ninth_amendment__rights_reservoir_reading, forecloses).
narrative_ontology:cs_axiom('e41debe6-7ce4-4ddb-9f77-c1b13218a9a9', foundational, enumeration_non_denial_principle).
narrative_ontology:cs_axiom_status(enumeration_non_denial_principle, holdable).
narrative_ontology:cs_axiom_grounding('e41debe6-7ce4-4ddb-9f77-c1b13218a9a9', enumeration_non_denial_principle, deontological).
narrative_ontology:cs_axiom('e41debe6-7ce4-4ddb-9f77-c1b13218a9a9', foundational, ninth_as_construction_rule_not_fount).
narrative_ontology:cs_axiom_status(ninth_as_construction_rule_not_fount, holdable).
narrative_ontology:cs_axiom_grounding('e41debe6-7ce4-4ddb-9f77-c1b13218a9a9', ninth_as_construction_rule_not_fount, conventional).
narrative_ontology:cs_reference_frame('e41debe6-7ce4-4ddb-9f77-c1b13218a9a9', ninth_amendment_as_hermeneutic_negative).
narrative_ontology:cs_drift_state('e41debe6-7ce4-4ddb-9f77-c1b13218a9a9', contemporary_unenumerated_rights_jurisprudence, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('e41debe6-7ce4-4ddb-9f77-c1b13218a9a9', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(ninth_amendment__rule_of_construction_reading, ninth_amendment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ninth_amendment__rule_of_construction_reading, constructional_modesty).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RULE-OF-CONSTRUCTION ANALYST (ROPE) — This reading interprets the Ninth as a hermeneutic constraint on judicial inference, not as a rights-generator. Low extractiveness: the clause suppresses a single logical move (enumeration → denial of others) without granting substantive rights. Pure coordination function: the Ninth coordinates constitutional meaning-making by forbidding one interpretive pathway, enabling cleaner doctrinal reasoning.
constraint_indexing:constraint_classification(ninth_amendment__rule_of_construction_reading, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(national))).

% PERSPECTIVE 2: TEXTUALIST JUDICIARY (ROPE) — Institutional actors committed to textualism (originalist judges) experience this reading as a coordination solution. The Ninth as a rule of construction is manageable: courts apply it as a negative injunction against faulty inference, not as a mandate to discover unenumerated rights. Low extraction cost; high coordination benefit — the rule clarifies what courts should NOT do.
constraint_indexing:constraint_classification(ninth_amendment__rule_of_construction_reading, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: CLAIMANTS SEEKING UNENUMERATED RIGHTS (SNARE) — This reading is extractive for those seeking doctrinal foundation for unenumerated rights (privacy, bodily autonomy, associational liberty). The rule-of-construction reading forbids them to use the Ninth's text as that foundation — it says the clause grants nothing, only constrains inference. Trapped: no alternative textual hook available; suppressed: the reading linguistically forecloses the rights-based argument.
constraint_indexing:constraint_classification(ninth_amendment__rule_of_construction_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 4: LOGICAL GRAMMAR OF ENUMERATION (MOUNTAIN) — From a universal/civilizational perspective, this reading invokes a structural principle of language: enumeration of some things does not logically entail denial of others. This principle holds across all legal systems, all texts, all times. The Ninth enforces this principle. No alternatives exist at this level — the principle is constitutive of how meaning works.
constraint_indexing:constraint_classification(ninth_amendment__rule_of_construction_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ninth_amendment__rule_of_construction_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ninth_amendment__rule_of_construction_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ninth_amendment__rule_of_construction_reading, TypeOther, context(agent_power(analytical), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(ninth_amendment__rule_of_construction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.18): This reading has minimal extractiveness because its primary function is to suppress a single faulty inference, not to extract benefits or rights. The Ninth, read as a construction rule, coordinates interpretation around a negative injunction: 'do not infer that enumeration denies others.' No substantive right is granted; no group is benefited substantively. However, the reading is extractive toward those seeking unenumerated-rights doctrine — it forecloses the textual argument. The extractiveness value reflects the low structural extraction from most perspectives, with the exception of powerless claimants (who experience it as snare-like). Suppression (0.32): Moderate. The reading suppresses one interpretive pathway explicitly, but suppression is limited to that single inference. Courts can and do still recognize unenumerated rights — they simply cannot ground them in the Ninth alone. The suppression is real (claimants cannot use the Ninth) but targeted (it does not suppress rights-recognition per se, only this specific doctrinal hook). Theater ratio (0.25): Low. The rule-of-construction reading has minimal performative content. It states a straightforward logical principle (enumeration does not entail denial) and applies it to the text. The reasoning is transparent; the operation is clear.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is sharp and structural. Textualist judges and constructional-modesty advocates see this reading as a coordination success — it clarifies what the Ninth does and does not do, enabling cleaner jurisprudence. The analytical observer at the universal level sees an immutable principle of logical grammar. But claimants seeking unenumerated rights experience this reading as pure extraction — it forecloses their textual argument, traps them in seeking rights through alternative doctrinal paths (Due Process, substantive liberty, common-law traditions), and suppresses the Ninth's use as a substantive hook. The powerless perspective produces a snare classification: the claimants are trapped (no alternative textual vehicle available at the time of the claim), suppressed (the reading linguistically forecloses this path), and derive zero benefit from the rule.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is computed from the structural position of each agent relative to the constraint. Beneficiaries of constructional modesty (textualist judges, analysts favoring clarity) have low d values — they experience the constraint as coordination rather than extraction. Victims (claimants seeking unenumerated-rights doctrine) have high d values — they are trapped by the reading's negation and suppressed in their use of the text. The analytical observer's canonical d (0.72) reflects the observational distance, which in this case produces a mountain classification — the principle of enumeration-non-denial holds universally. The perspectival gap emerges because beneficiaries and victims occupy opposite positions in the directionality space.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading resolves mandatrophy through specificity: the constraint is NOT a source of new rights (which would make it a tangled_rope or snare). It is a rule forbidding one inference move. The mandatrophy would arise if the reading claimed BOTH to supply substantive rights AND to forbid their derivation — that would be a contradictory hybrid. Instead, the reading separates: the Ninth forbids the faulty inference; other doctrinal sources (Due Process, liberty) supply substantive rights. This separation prevents the mandatrophy. The reading is pure coordination (rope) from most perspectives, snare only from the perspective of powerless claimants seeking this specific doctrinal hook.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    constructional_vs_substantive_reading,
    'Is the Ninth''s primary function hermeneutic (constraining inference) or substantive (grounding unenumerated rights)?',
    'Historical analysis of framers'' intent; examination of ratification debates for explicit discussion of whether the Ninth was meant to reserve substantive rights or merely clarify the construction rule',
    'If hermeneutic: rule-of-construction reading is canonical, extractiveness remains ~0.18 (pure coordination). If substantive: rights-reservoir reading gains structural strength, extractiveness rises to ~0.35-0.40 (mixed coordination-rights recognition).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(constructional_vs_substantive_reading, empirical, 'Whether Ninth''s function is hermeneutic rule or substantive rights reservoir').

omega_variable(
    inference_suppression_completeness,
    'Does forbidding the enumeration-denial inference actually prevent courts from generating unenumerated rights doctrine, or does it merely redirect the justificatory path?',
    'Doctrinal review: compare privacy/liberty holdings under rule-of-construction frame (e.g., justified via Due Process Clause) with holdings under rights-reservoir frame (e.g., justified via Ninth Amendment directly). If substantive output is identical, suppression is incomplete.',
    'If suppression is complete: the rule-of-construction reading genuinely constrains doctrine, and extraction on powerless claimants is real (they cannot use Ninth text). If doctrinal output unchanged: the reading is performative (piton), and actual extraction is lower.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(inference_suppression_completeness, empirical, 'Whether enumeration-inference suppression actually constrains rights doctrine').

omega_variable(
    reading_institutional_adoption,
    'Which reading do current institutional adjudicators (courts, bar bodies) actually instantiate in practice: rule-of-construction, judicial-unusability, or rights-reservoir?',
    'Systematic review of Supreme Court opinions invoking the Ninth Amendment (post-1965); classification of holdings as resting on: pure construction rule, no substantive right-finding (rule-of-construction), explicit avoidance language (judicial-unusability), or positive rights claims (rights-reservoir)',
    'If institutional adoption favors rule-of-construction: this reading''s ε remains stable. If courts adopt mixed framing: constraint becomes tangled_rope (coordination + extraction hybrid). If courts adopt rights-reservoir: this reading''s legitimacy erodes.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_institutional_adoption, empirical, 'Which Ninth Amendment reading institutional practice actually instantiates').

omega_variable(
    kernel_reading_distinctness,
    'Is the rule-of-construction reading truly distinct from the judicial-unusability reading, or are they the same constraint described differently?',
    'Structural comparison: rule-of-construction = the Ninth forbids inference; judicial-unusability = courts have no guidance from the clause. If the first entails the second empirically but not logically, the readings diverge. If they entail each other both ways, they are a single constraint with two labels.',
    'If distinct: both readings warrant separate constraint stories. If identical: merge stories or declare one as redundant framing of the other.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_distinctness, conceptual, 'Whether rule-of-construction and judicial-unusability readings are structurally distinct').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ninth_amendment__rule_of_construction_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nint_tr_t0, ninth_amendment__rule_of_construction_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(nint_tr_t50, ninth_amendment__rule_of_construction_reading, theater_ratio, 50, 0.25).
narrative_ontology:measurement(nint_tr_t100, ninth_amendment__rule_of_construction_reading, theater_ratio, 100, 0.25).

% Extraction over time
narrative_ontology:measurement(nint_be_t0, ninth_amendment__rule_of_construction_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(nint_be_t50, ninth_amendment__rule_of_construction_reading, base_extractiveness, 50, 0.18).
narrative_ontology:measurement(nint_be_t100, ninth_amendment__rule_of_construction_reading, base_extractiveness, 100, 0.18).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ninth_amendment__rule_of_construction_reading, information_standard).
narrative_ontology:affects_constraint(ninth_amendment__rule_of_construction_reading, ninth_amendment__judicial_unusability_reading).
narrative_ontology:affects_constraint(ninth_amendment__rule_of_construction_reading, ninth_amendment__rights_reservoir_reading).

% DUAL FORMULATION NOTE:
% The Ninth Amendment kernel decomposes into three reading-constraints, each with distinct ε and beneficiary/victim structures. The rule-of-construction reading (this story) has ε=0.18, treats the Ninth as a coordination rule on inference. The rights-reservoir reading has ε~0.35, treats the Ninth as a substantive fount. The judicial-unusability reading has ε~0.25, treats the Ninth as having left courts without guidance. All three readings share the same textual kernel but instantiate different structural constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
