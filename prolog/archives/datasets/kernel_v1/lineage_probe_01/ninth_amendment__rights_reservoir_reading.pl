% ============================================================================
% CONSTRAINT STORY: ninth_amendment__rights_reservoir_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ninth_amendment__rights_reservoir_reading, []).

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
 *   constraint_id: ninth_amendment__rights_reservoir_reading
 *   human_readable: Ninth Amendment Rights Reservoir Reading
 *   domain: constitutional_law/doctrine
 *
 * SUMMARY:
 *   The Ninth Amendment reads: 'The enumeration in the Constitution of
 *   certain rights shall not be construed to deny or disparage others
 *   retained by the people.' The rights-reservoir reading interprets this
 *   clause as creating or recognizing a genuine pool of unenumerated rights
 *   that courts may judicially cognize when appropriate. This reading,
 *   crystallized in Justice Goldberg's Griswold v. Connecticut concurrence
 *   (1965), claims that the Ninth is a textual spring — a constitutional
 *   source — for recognizing liberties like privacy that are not explicitly
 *   enumerated but are grounded in the text's affirmation that retained
 *   rights exist. The constraint operates between two interpretive regimes:
 *   the enumeration-exhaustion reading (that enumeration denies others, or at
 *   least that judges have no authority to enforce unenumerated rights) and
 *   the rights-reservoir reading (that enumeration does not exhaust retained
 *   rights, which courts may recognize). The rights-reservoir reading
 *   suppresses the force of the enumeration-exhaustion argument through
 *   affirmative doctrinal work — it provides a framework for recognizing
 *   unenumerated rights. But this suppression and recognition comes at a
 *   cost: judges must continually justify why particular unenumerated rights
 *   qualify as 'retained by the people,' and the reading commits the
 *   judiciary to an interpretive project that critics view as unconstrained
 *   or lawless. The constraint exhibits a perspectival gap characteristic of
 *   tangled ropes: the reading coordinates the anti-closure interpretation
 *   with judicial authority (beneficiary: unenumerated-rights jurisprudence)
 *   while extracting from judges an ongoing obligation to ground novel rights
 *   in Ninth Amendment penumbrae and from unenumerated-right-bearers the
 *   burden of fitting their claims into doctrinal frameworks rather than
 *   seeking positive amendment.
 *
 * KEY AGENTS:
 *   - Unenumerated-Rights Jurisprudence: Primary beneficiary (institutional/arbitrage) — privacy doctrine, substantive due process, autonomy jurisprudence benefit from Ninth as textual peg for recognizing judicially cognizable liberties
 *   - Enumeration-Exhaustion Interpretation: Primary victim (institutional/constrained) — the reading suppresses the force of the closure argument; originalists and formalists committed to textual enumeration face pressure to explain Ninth's meaning
 *   - Unenumerated Right-Bearer (Powerless): Secondary victim (powerless/trapped) — despite the reading's availability, lacks independent Ninth-based claim; must fit assertion into due process or other doctrinal peg; generational barrier to recognition
 *   - Judges Enforcing the Reading: Institutional implementers (institutional/constrained) — face active enforcement burden of continually justifying unenumerated rights as 'retained by the people'
 *   - Civil Rights Advocates: Organized beneficiaries with exit (organized/mobile) — benefit from Ninth-based arguments but have alternative pathways (state law, amendment, international law)
 *   - Originalist Doctrine: Institutional degraded enforcer (institutional/constrained) — maintains Ninth as constitutional text in theory while sidelining it in operative doctrine (piton perspective)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ninth_amendment__rights_reservoir_reading, 0.38).
domain_priors:suppression_score(ninth_amendment__rights_reservoir_reading, 0.52).
domain_priors:theater_ratio(ninth_amendment__rights_reservoir_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ninth_amendment__rights_reservoir_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(ninth_amendment__rights_reservoir_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(ninth_amendment__rights_reservoir_reading, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ninth_amendment__rights_reservoir_reading, tangled_rope).
narrative_ontology:human_readable(ninth_amendment__rights_reservoir_reading, "Ninth Amendment Rights Reservoir Reading").
narrative_ontology:topic_domain(ninth_amendment__rights_reservoir_reading, "constitutional_law/doctrine").

domain_priors:requires_active_enforcement(ninth_amendment__rights_reservoir_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ninth_amendment__rights_reservoir_reading, '61d4426f-e3ac-4d34-90e7-de41403298bf').
narrative_ontology:cs_kernel_codification('61d4426f-e3ac-4d34-90e7-de41403298bf', fixed_text).
narrative_ontology:cs_authority_grounding('61d4426f-e3ac-4d34-90e7-de41403298bf', lineage).
narrative_ontology:cs_interpretation_layer_present('61d4426f-e3ac-4d34-90e7-de41403298bf').
narrative_ontology:cs_reading_relation('61d4426f-e3ac-4d34-90e7-de41403298bf', ninth_amendment__judicial_unusability_reading, forecloses).
narrative_ontology:cs_reading_relation('61d4426f-e3ac-4d34-90e7-de41403298bf', ninth_amendment__rule_of_construction_reading, influences).
narrative_ontology:cs_axiom('61d4426f-e3ac-4d34-90e7-de41403298bf', foundational, unenumerated_rights_judicially_cognizable).
narrative_ontology:cs_axiom_status(unenumerated_rights_judicially_cognizable, holdable).
narrative_ontology:cs_axiom_grounding('61d4426f-e3ac-4d34-90e7-de41403298bf', unenumerated_rights_judicially_cognizable, deontological).
narrative_ontology:cs_axiom('61d4426f-e3ac-4d34-90e7-de41403298bf', foundational, anti_closure_as_affirmative_source).
narrative_ontology:cs_axiom_status(anti_closure_as_affirmative_source, holdable).
narrative_ontology:cs_axiom_grounding('61d4426f-e3ac-4d34-90e7-de41403298bf', anti_closure_as_affirmative_source, deontological).
narrative_ontology:cs_reference_frame('61d4426f-e3ac-4d34-90e7-de41403298bf', ninth_amendment_rights_retention_framework).
narrative_ontology:cs_drift_state('61d4426f-e3ac-4d34-90e7-de41403298bf', contemporary_originalist_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('61d4426f-e3ac-4d34-90e7-de41403298bf', '2026-02-26T14:32:18Z').
narrative_ontology:cs_kernel_id(ninth_amendment__rights_reservoir_reading, ninth_amendment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ninth_amendment__rights_reservoir_reading, unenumerated_rights_jurisprudence).
narrative_ontology:constraint_victim(ninth_amendment__rights_reservoir_reading, enumeration_closure_interpretation).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: UNENUMERATED RIGHT-BEARER / TRAPPED (SNARE) — A claimant asserting a right not explicitly listed (privacy, dignity, familial autonomy) finds themselves trapped between two exclusionary doctrines. The enumeration-exhaustion reading says enumeration denies others; the judicial-unusability reading says the Ninth provides no actionable claim. Either way, the trapped agent bears full extraction — the supposed reservoir provides no water. Generational time horizon reflects decades of litigation establishing that the Ninth, despite its text, offers no independent judicial cognizance.
constraint_indexing:constraint_classification(ninth_amendment__rights_reservoir_reading, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: MODERATE JUDGE / CONSTRAINED (TANGLED ROPE) — A judge applying the rights-reservoir reading experiences both coordination and extraction. The reading coordinates the interpretation of enumeration (rejection of closure) with judicial authority (recognition of unenumerated rights). But the reading also extracts: it commits the judge to finding textual warrant for novel rights in the Ninth's penumbra rather than in explicit amendment or state decision-making. The constraint is active enforcement of this reading — the judge must continually justify why THIS unenumerated right qualifies as retained by the people. Extraction runs upstream toward the judge's power; the coordination function is real (preventing misreading enumeration as exhaustive).
constraint_indexing:constraint_classification(ninth_amendment__rights_reservoir_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: UNENUMERATED-RIGHTS JURISPRUDENCE / INSTITUTIONAL / ARBITRAGE (ROPE) — From the perspective of privacy doctrine, substantive due process, and family autonomy jurisprudence, the rights-reservoir reading provides pure coordination. These doctrinal clusters benefit from the Ninth's availability as a textual peg for recognizing liberties not enumerated in the Bill of Rights. The reading's core move — that enumeration does not exhaust retained rights — coordinates across cases and generations the project of recognizing privacy, autonomy, and dignity as judicially cognizable. For this beneficiary, extraction is minimal; the reading solves a coordination problem.
constraint_indexing:constraint_classification(ninth_amendment__rights_reservoir_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: ORIGINALIST JURISPRUDENCE / INSTITUTIONAL / CONSTRAINED (PITON) — Originalist doctrine, especially post-Heller, faces pressure to explain why the Ninth matters at all if judges can only apply original public meaning. The rights-reservoir reading is performatively maintained as a constitutional text that exists and means something, while being effectively sidelined by claims that its original meaning is either opaque or actively denies judicial cognizance. The theater here is high: originalism acknowledges the Ninth in scholarly footnotes and canonical lists while its operative doctrine (enumeration exhaustion, or claims of non-justiciability) suppresses its force. This is a Piton — the reading persists through inertia (the Ninth is in the Constitution) but the dominant institutional framework (originalism) treats it as a degraded, unusable constraint.
constraint_indexing:constraint_classification(ninth_amendment__rights_reservoir_reading, piton,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: ANALYTICAL NATURAL-LAW SKEPTICISM (MOUNTAIN) — From a civilizational analytical standpoint, one might claim that unenumerated rights cannot exist as judicially cognizable law: that any legal right must be grounded in text, custom, statute, or amendment, and that the Ninth's invocation of retained rights is an appeal to extra-legal natural law that courts have no authority to enforce. From this view, the Ninth is a natural limit on judicial power — courts cannot cognize rights that have no positive legal ground. However, this perspective risks false-summithood: the mountain classification naturalizes what is actually a contestable jurisprudential commitment (the exhaustion principle, the scope of judicial authority).
constraint_indexing:constraint_classification(ninth_amendment__rights_reservoir_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 6: CIVIL RIGHTS ADVOCACY / ORGANIZED / MOBILE (TANGLED ROPE) — Organized actors (civil liberties organizations, law professors, rights advocates) recognize the rights-reservoir reading as both coordinating and extractive. It coordinates the legal recognition of privacy, autonomy, and dignity; but it also extracts by requiring advocates to fit novel rights into the Ninth's penumbra rather than seeking positive amendment or legislative protection. The constraint is active enforcement of Ninth-dependent argumentation — advocates must continually justify why their desired right is 'retained by the people' rather than simply advocating for its explicit recognition. However, these actors have mobile exit options: state constitutional law, state statutes, federal amendment, or international human-rights frameworks provide alternative legal pathways. Their experienced extraction is real but not total.
constraint_indexing:constraint_classification(ninth_amendment__rights_reservoir_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ninth_amendment__rights_reservoir_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ninth_amendment__rights_reservoir_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ninth_amendment__rights_reservoir_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(ninth_amendment__rights_reservoir_reading, TR),
    TR >= 0.70.

:- end_tests(ninth_amendment__rights_reservoir_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The rights-reservoir reading extracts by requiring judges to justify novel rights as 'retained by the people' rather than grounding them in explicit text or positive law. This extraction is real — the reading constrains the space of legitimate judicial reasoning. But extraction is not high because the reading also solves a coordination problem: it prevents the closure argument and coordinates across doctrinal domains the project of recognizing privacy, autonomy, and dignity. The extractiveness value reflects that the reading has both genuine doctrinal force (benefiting unenumerated-rights jurisprudence) and genuine constraint (burdening judges and right-claimants with justification work). Over the interval (0–100 years, roughly from Plessy through contemporary doctrine), extractiveness has risen slightly as the Ninth has become more performative — cited as a doctrinal foundation without delivering independent holdings, requiring more justificatory labor. Suppression (0.52): Moderate-high. The reading suppresses the force of the enumeration-exhaustion argument through positive force — judges asserting that enumeration does not deny others. But suppression is not total because the alternate reading (enumeration exhaustion, or judicial unusability) remains live among originalists and formalists. The reading's suppressive force has stabilized since Griswold; it is not increasing or decreasing dramatically. Theater ratio (0.58): Moderate-high. The reading has increasingly performative dimensions. Courts cite the Ninth as grounding privacy and autonomy rights, but they typically rest holdings on due process or liberty clauses while treating the Ninth as an interpretive aid or contextual backdrop. The theater reflects that the Ninth performs constitutional legitimacy (affirming that retained rights exist and are judicially cognizable) while other doctrinal tools do the operative work. Theater has increased over the interval as judges have become more cautious about independent Ninth-based holdings and instead used the Ninth as an interpretive compass for recognizing rights through due process.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits the perspectival gap characteristic of tangled ropes. From the unenumerated-rights jurisprudence perspective (institutional/arbitrage), the reading is pure coordination — it enables the recognition of privacy, autonomy, and dignity as judicially cognizable. From the moderate judge's perspective (institutional/constrained), the reading is tangled: it both coordinates (preventing misreading of enumeration) and extracts (requiring ongoing justification). From the powerless right-bearer's perspective (powerless/trapped), the reading is near-snare: despite its availability, the Ninth provides no independent claim; the right-bearer is trapped between the closure argument and the unusability argument. From the originalist perspective (institutional/constrained, piton), the reading is performative — the Ninth is cited and acknowledged but sidelined by operative doctrine. From the civil rights advocacy perspective (organized/mobile), the reading is tangled but with exit paths — it coordinates and extracts, but advocates can exit to state law, amendment, or international frameworks. From the analytical perspective (analytical/analytical), the reading risks appearing as a mountain (a natural limit on judicial authority) if critics successfully argue that unenumerated rights cannot exist as law, but the reading's actual force suggests it is tangled rope, not mountain.
 *
 * DIRECTIONALITY LOGIC:
 *   The rights-reservoir reading's directionality is determined by the beneficiary/victim structure and the agent's power/exit combination. Unenumerated-rights jurisprudence (beneficiary, institutional, arbitrage) experiences low or negative effective extraction — the reading benefits them. Enumeration-exhaustion interpretation (victim, institutional, constrained) experiences higher effective extraction — the reading suppresses its force. The powerless right-bearer (victim, powerless, trapped) experiences maximum extraction relative to their position — the reading's promise of a rights-reservoir goes unfulfilled in actual claims. Judges (implementers, institutional, constrained) experience moderate extraction — they must justify unenumerated rights but have some discretion in doctrinal framing. The directionality overshoot near generational timescales (where the constraint's theater rises) reflects that the reading becomes increasingly performative: it performs the existence of a rights-reservoir while operative doctrine moves away from independent Ninth-based holdings.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint does NOT exhibit mandatrophy in the classical sense (extraction disguised as coordination). Instead, it exhibits a doctrinal-performance divergence: the rights-reservoir reading's theoretical claim (the Ninth is a source of unenumerated rights) diverges from operative doctrine (courts rest holdings on due process or liberty, treating the Ninth as interpretive aid). This is not mandatrophy proper (where the two mechanisms are the same) but rather piton-ward drift within the tangled-rope classification. The reading genuinely coordinates the anti-closure interpretation, but the coordination is increasingly performed rather than operative. The constraint's actual mandatrophy resolution is structural: if courts began resting holdings on the Ninth alone, extractiveness would rise (as judges would be explicitly recognizing novel unenumerated rights) and theater would fall (as the Ninth would do operative work rather than interpretive aid work). The current state — moderate extractiveness with moderate theater, reflecting both real doctrinal coordination and performative citation — represents an equilibrium where the reading exists and has force but is increasingly sidelined by operative doctrine.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reservoir_vs_construction_boundary,
    'Does the Ninth Amendment guarantee the existence of unenumerated rights as substantive legal entitlements, or does it merely forbid the inference that enumeration exhausts retained rights without creating any enforceable right itself?',
    'Jurisprudential analysis of holding-by-holding opinion language: does the Supreme Court ever rest a decision on the Ninth alone as the source of the right, or does it always use the Ninth as a contextual interpretive aid while grounding the right in due process, liberty, or other explicit constitutional sources?',
    'If the Ninth creates substantive rights: the rights-reservoir reading is correct, and extractiveness is lower (the Ninth provides genuine judicial cognizance). If the Ninth is purely a construction rule: the rule-of-construction reading is more accurate, extractiveness is higher (the Ninth provides no independent claim), and the reading is closer to piton (performing a rule that provides no additional force).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reservoir_vs_construction_boundary, empirical, 'Whether the Ninth creates substantive rights or merely forbids an inference').

omega_variable(
    penumbral_legitimacy_anchor,
    'What authorizes a judge to recognize an unenumerated right as ''retained by the people'' in the Ninth Amendment sense? Is it historical ratification intent, contemporary public meaning, natural-law grounding, or doctrinal coherence with enumerated rights?',
    'Historical and semantic analysis of Ninth Amendment ratification materials; comparison of original public meaning at ratification against modern Griswold-lineage applications; examination of whether recognized unenumerated rights track discoverable public retention or instead reflect judicial judgment about desirable rights.',
    'If grounded in historical intent: the reading is more defensible, suppression lower, extractiveness decreases. If no stable anchor exists: extractiveness increases, theater increases (judges appear to be ratifying preferred rights under a constitutional pretext), and the reading approaches piton status.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(penumbral_legitimacy_anchor, conceptual, 'Legitimacy anchor for recognizing unenumerated rights as retained by the people').

omega_variable(
    this_reading_forecloses_unusability,
    'Does asserting that the Ninth is a rights-reservoir logically foreclose the judicial-unusability reading that courts have correctly avoided relying on the Ninth because its open texture is judicially unmanageable?',
    'Metatheoretical analysis: can both readings coexist in the same constitutional framework, or does one reading''s core premises directly contradict the other''s? Is the disagreement about what the Ninth means (a live dispute) or about whether it means anything judicially cognizable (a foreclosure)?',
    'If forecloses: the reading_relations should use ''forecloses'' rather than ''coexists_with''; the kernel involves genuine logical conflict. If coexists: the readings represent different institutional positions (courts practicing judicial restraint vs. courts recognizing Ninth-based rights) that can persist simultaneously across different doctrinal domains.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(this_reading_forecloses_unusability, conceptual, 'Whether rights-reservoir reading forecloses judicial-unusability reading').

omega_variable(
    enumeration_closure_victim_metaphor,
    'Can ''enumeration-closure interpretation'' be properly described as a ''victim'' of the rights-reservoir reading, or is it more accurate to say the reading suppresses one interpretive option in favor of another without creating a genuine victim set?',
    'Structural analysis: does the rights-reservoir reading extract from or disable some agent or doctrinal interest, or does it merely claim that a particular reading is incorrect? Contrast with snare/tangled-rope victimhood where an identifiable agent bears material cost.',
    'If genuine victim: the Tangled Rope classification is correct; the reading suppresses the enumeration-exhaustion reading''s force while providing doctrinal benefit to unenumerated-rights jurisprudence. If metaphorical: the reading is a pure doctrinal dispute without clear beneficiary/victim structure; may reclassify as rope (coordination without asymmetric extraction) or require decomposition into separate stories (one for the coordination function, one for the interpretation suppression).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enumeration_closure_victim_metaphor, conceptual, 'Whether enumeration-closure interpretation is a genuine victim or metaphorical suppressed option').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ninth_amendment__rights_reservoir_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ninth_res_theater_t0, ninth_amendment__rights_reservoir_reading, theater_ratio, 0, 0.4).
narrative_ontology:measurement(ninth_res_theater_t50, ninth_amendment__rights_reservoir_reading, theater_ratio, 50, 0.52).
narrative_ontology:measurement(ninth_res_theater_t100, ninth_amendment__rights_reservoir_reading, theater_ratio, 100, 0.58).

% Extraction over time
narrative_ontology:measurement(ninth_res_extract_t0, ninth_amendment__rights_reservoir_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(ninth_res_extract_t50, ninth_amendment__rights_reservoir_reading, base_extractiveness, 50, 0.33).
narrative_ontology:measurement(ninth_res_extract_t100, ninth_amendment__rights_reservoir_reading, base_extractiveness, 100, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ninth_amendment__rights_reservoir_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(ninth_amendment__rights_reservoir_reading, ninth_amendment__judicial_unusability_reading).
narrative_ontology:affects_constraint(ninth_amendment__rights_reservoir_reading, ninth_amendment__rule_of_construction_reading).
narrative_ontology:affects_constraint(ninth_amendment__rights_reservoir_reading, substantive_due_process_penumbral_extension).
narrative_ontology:affects_constraint(ninth_amendment__rights_reservoir_reading, privacy_doctrine_textual_grounding).

% DUAL FORMULATION NOTE:
% The rights-reservoir reading is one structural interpretation of the Ninth Amendment kernel. The judicial-unusability reading and rule-of-construction reading are sibling constraints in the same kernel family, each with distinct epsilon values and beneficiary/victim structures. The rights-reservoir reading is upstream of privacy_doctrine_textual_grounding (privacy doctrine relies on the Ninth's availability as a rights-source) and substantive_due_process_penumbral_extension (due process doctrines incorporate the anti-closure principle from the Ninth). See kernel_context for sibling relationships.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ninth_amendment__rights_reservoir_reading, institutional, 0.42).
constraint_indexing:directionality_override(ninth_amendment__rights_reservoir_reading, organized, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
