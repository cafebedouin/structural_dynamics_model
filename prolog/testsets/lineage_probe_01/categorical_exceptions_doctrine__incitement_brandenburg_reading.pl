% ============================================================================
% CONSTRAINT STORY: categorical_exceptions_doctrine__incitement_brandenburg_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_categorical_exceptions_doctrine__incitement_brandenburg_reading, []).

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
 *   constraint_id: categorical_exceptions_doctrine__incitement_brandenburg_reading
 *   human_readable: Brandenburg Incitement Doctrine: Imminent Lawless Action Standard
 *   domain: constitutional_law/free_speech
 *
 * SUMMARY:
 *   Brandenburg v. Ohio (1969) established the doctrinal boundary between
 *   protected and unprotected incitement: speech loses First Amendment
 *   protection only when it is directed to imminent lawless action and likely
 *   to incite or produce that action. This constraint defines one reading of
 *   the categorical-exceptions doctrine — a contested kernel around which
 *   different constitutional frameworks organize speech protection. The
 *   Brandenburg reading protects abstract advocacy (speech about the moral
 *   desirability of revolution, criticism of lawless government action, even
 *   approval of lawless principles in the abstract) while suppressing only
 *   speech that directly coordinates immediate illegal acts. The constraint
 *   exhibits mixed coordination and extraction: it coordinates protection for
 *   unpopular speech (the coordination function) while suppressing
 *   dangerousness-talk (the extraction mechanism). Different institutional
 *   actors experience this constraint radically differently: radical
 *   advocates see it as protective rope, prosecutors experience it as
 *   constrained rope, bad-tendency regimes persist in degraded piton form,
 *   civil liberties organizations treat it as a temporary scaffold against
 *   pressure to expand exceptions, and the analytical observer risks
 *   naturalizing it as a law of speech itself.
 *
 * KEY AGENTS:
 *   - Radical Political Advocates: Primary beneficiary (powerless/constrained) — Brandenburg protects abstract advocacy that would be prosecuted under bad-tendency test; net beneficiary of the constraint's protective coordination function.
 *   - Speech Chilled by Dangerousness Language: Primary victim (powerless/trapped) — speakers who self-censor to avoid prosecution under vague dangerousness standards; experience extraction without protection because fear prevents exercise of Brandenburg's protection.
 *   - Law Enforcement / Prosecutorial Authority: Institutional actor (institutional/constrained) — benefits from Brandenburg's clarity (easier to know when prosecution is legitimate) but loses discretionary power to prosecute dangerous rhetoric short of imminent incitement; constrained by the doctrine's protective boundaries.
 *   - Civil Liberties Movement: Organized coalition (organized/mobile) — sees Brandenburg as a temporary holding against pressures to expand categorical exceptions; treats it as a scaffold with a sunset (if courts abandon the imminent standard or create new exceptions).
 *   - Bad-Tendency Prosecution Regime: Vestigial institutional actor (institutional/arbitrage) — formally superseded by Brandenburg but persisting through judicial inertia in some contexts; maintained performatively despite doctrinal loss.
 *   - Analytical Observer: Civilizational analyst (analytical/analytical) — risks seeing the imminent-and-likely boundary as a natural law about the boundary between speech and action, naturalizing what is actually a contested doctrinal choice.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(categorical_exceptions_doctrine__incitement_brandenburg_reading, 0.38).
domain_priors:suppression_score(categorical_exceptions_doctrine__incitement_brandenburg_reading, 0.42).
domain_priors:theater_ratio(categorical_exceptions_doctrine__incitement_brandenburg_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(categorical_exceptions_doctrine__incitement_brandenburg_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(categorical_exceptions_doctrine__incitement_brandenburg_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(categorical_exceptions_doctrine__incitement_brandenburg_reading, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(categorical_exceptions_doctrine__incitement_brandenburg_reading, tangled_rope).
narrative_ontology:human_readable(categorical_exceptions_doctrine__incitement_brandenburg_reading, "Brandenburg Incitement Doctrine: Imminent Lawless Action Standard").
narrative_ontology:topic_domain(categorical_exceptions_doctrine__incitement_brandenburg_reading, "constitutional_law/free_speech").

domain_priors:requires_active_enforcement(categorical_exceptions_doctrine__incitement_brandenburg_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(categorical_exceptions_doctrine__incitement_brandenburg_reading, 'e088568b-4747-4c33-866a-7cb814188512').
narrative_ontology:cs_kernel_codification('e088568b-4747-4c33-866a-7cb814188512', formalized).
narrative_ontology:cs_authority_grounding('e088568b-4747-4c33-866a-7cb814188512', lineage).
narrative_ontology:cs_interpretation_layer_present('e088568b-4747-4c33-866a-7cb814188512').
narrative_ontology:cs_reading_relation('e088568b-4747-4c33-866a-7cb814188512', categorical_exceptions_doctrine__no_new_categories_reading, coexists_with).
narrative_ontology:cs_reading_relation('e088568b-4747-4c33-866a-7cb814188512', categorical_exceptions_doctrine__obscenity_miller_reading, coexists_with).
narrative_ontology:cs_axiom('e088568b-4747-4c33-866a-7cb814188512', foundational, abstract_advocacy_always_protected).
narrative_ontology:cs_axiom_status(abstract_advocacy_always_protected, holdable).
narrative_ontology:cs_axiom_grounding('e088568b-4747-4c33-866a-7cb814188512', abstract_advocacy_always_protected, deontological).
narrative_ontology:cs_axiom('e088568b-4747-4c33-866a-7cb814188512', foundational, imminence_requirement_limits_suppression).
narrative_ontology:cs_axiom_status(imminence_requirement_limits_suppression, holdable).
narrative_ontology:cs_axiom_grounding('e088568b-4747-4c33-866a-7cb814188512', imminence_requirement_limits_suppression, empirically_contingent).
narrative_ontology:cs_reference_frame('e088568b-4747-4c33-866a-7cb814188512', brandenburg_imminent_action_standard).
narrative_ontology:cs_drift_state('e088568b-4747-4c33-866a-7cb814188512', contemporary_post_2020, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('e088568b-4747-4c33-866a-7cb814188512', '2026-02-26T14:32:00Z').
narrative_ontology:cs_kernel_id(categorical_exceptions_doctrine__incitement_brandenburg_reading, categorical_exceptions_doctrine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(categorical_exceptions_doctrine__incitement_brandenburg_reading, radical_political_advocacy).
narrative_ontology:constraint_beneficiary(categorical_exceptions_doctrine__incitement_brandenburg_reading, abstract_speech_protection).
narrative_ontology:constraint_victim(categorical_exceptions_doctrine__incitement_brandenburg_reading, bad_tendency_prosecution_regimes).
narrative_ontology:constraint_victim(categorical_exceptions_doctrine__incitement_brandenburg_reading, speech_chilled_by_dangerousness_talk).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RADICAL ADVOCATE (ROPE) — Agent advocating revolutionary change or moral condemnation of lawless action faces Brandenburg's line: speech loses protection only if directed to imminent action and likely to incite it. Below the line, protection is secure. The constraint coordinates protection for unpopular speech while maintaining a safety valve for imminent incitement. The advocate experiences genuine coordination function — the doctrine enables speech that would be suppressed under bad-tendency tests — alongside a suppressive boundary (the imminent/likely standard itself). This is rope because the net function is protective coordination, not extraction.
constraint_indexing:constraint_classification(categorical_exceptions_doctrine__incitement_brandenburg_reading, rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 2: CHILLED SPEECH VICTIM (SNARE) — Speakers who avoid rhetoric about violence or lawless action to escape prosecution under vague dangerousness standards experience pure extraction with no coordination benefit. The Brandenburg rule technically protects abstract advocacy, but speakers who fear misapplication, hostile judges, or resource costs of defense suppress their own speech preemptively. This victim group bears extraction without gaining protection — they self-censor beneath the Brandenburg line. Trapped because they cannot exercise the protection without risking prosecution; the suppression of dangerousness-talk itself becomes a compliance mechanism.
constraint_indexing:constraint_classification(categorical_exceptions_doctrine__incitement_brandenburg_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 3: LAW ENFORCEMENT (TANGLED ROPE) — Prosecutors benefit from the Brandenburg rule's coordination function: it clarifies when speech prosecution is legitimate and protects law enforcement from suits for suppressing abstract advocacy. Enforcement is easier under Brandenburg than under bad-tendency tests because the imminent + likely standard is more determinate. But prosecutors also bear extraction: they lose the discretionary power to prosecute dangerous rhetoric that falls short of imminent incitement. They must coordinate with free speech values rather than maximizing prosecution. The constraint is both enabling (clearer standard) and constraining (narrower prosecutorial scope). Requires active enforcement because judges must interpret 'imminent' and 'likely' — enforcement is continuous interpretation, not algorithm.
constraint_indexing:constraint_classification(categorical_exceptions_doctrine__incitement_brandenburg_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: CIVIL LIBERTIES COALITION (SCAFFOLD) — Organized agents (ACLU, First Amendment scholars, civil rights organizations) see Brandenburg as a temporary holding against pressures to expand categorical exceptions. The constraint has a structural sunset: if the 'imminent lawless action' standard proves unworkable in practice, if courts begin interpreting it to exclude more speech, or if political pressure builds to create new categorical exceptions (like incitement to insurrection or threats to public health), Brandenburg could be abandoned or narrowed. The coalition treats Brandenburg as a scaffold because it coordinates speech protection while remaining vulnerable to doctrinal drift. The constraint's function will eventually be replaced by either a stronger categorical rule (no exceptions except imminent) or new category-based exceptions. Theater is low (the standard is substantive, not performative) and sunset is real (the coalition recognizes Brandenburg as a contested equilibrium).
constraint_indexing:constraint_classification(categorical_exceptions_doctrine__incitement_brandenburg_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: BAD-TENDENCY REGIME (PITON) — The prosecution standard that Brandenburg replaced (bad-tendency: speech loses protection if it has a tendency to produce lawless conduct) persists in some jurisdictions and in occasional appellate confusion, despite Brandenburg's formal supersession. The bad-tendency regime continues through institutional inertia — it requires minimal interpretation, is intuitive for judges and juries, and aligns with order-preservation instincts. But it is degraded: formally overruled, rarely cited, maintained performatively when judges invoke it without acknowledging Brandenburg conflict. Theater ratio is high because prosecutors and judges in some contexts perform bad-tendency reasoning while nominally following Brandenburg. The regime is inertial, not functional.
constraint_indexing:constraint_classification(categorical_exceptions_doctrine__incitement_brandenburg_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LIMITS VIEW (MOUNTAIN) — From a civilizational and universal perspective, the Brandenburg constraint can appear as a natural law: the imminent-and-likely standard reflects a timeless truth about when speech becomes action, when advocacy becomes coordination of actual harm, when abstract agreement with lawless principles differs structurally from incitement to immediate lawless acts. This perspective holds that the line Brandenburg draws is not contingent doctrine but inevitable recognition of a constitutive boundary. However, the structural data contradicts this — the constraint has identifiable beneficiaries (radical advocacy, abstract speech protection) and victims (bad-tendency prosecution), and requires active enforcement. The mountain classification here is a false summit: Brandenburg naturalizes what is actually a contested institutional choice among competing doctrinal frameworks.
constraint_indexing:constraint_classification(categorical_exceptions_doctrine__incitement_brandenburg_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(categorical_exceptions_doctrine__incitement_brandenburg_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(categorical_exceptions_doctrine__incitement_brandenburg_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(categorical_exceptions_doctrine__incitement_brandenburg_reading, TypeOther, context(agent_power(powerless), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(categorical_exceptions_doctrine__incitement_brandenburg_reading, TR),
    TR >= 0.70.

:- end_tests(categorical_exceptions_doctrine__incitement_brandenburg_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. Brandenburg suppresses dangerousness-talk and creates self-censorship among speakers who fear misapplication or hostile interpretation. The suppression is real and structural — speakers genuinely cannot discuss lawless action in advocacy contexts without risk. But extractiveness is not high because (1) the protection Brandenburg provides for abstract speech is substantial and genuine, (2) the imminent-and-likely standard is more determinate than bad-tendency alternatives, reducing prosecutorial arbitrariness, and (3) organized civil liberties actors actively defend the standard and push back against hostile applications. The measurement trajectory (0.28 → 0.38 over 50 years) reflects increasing pressure to expand categorical exceptions (January 6 insurrection rhetoric, incitement to public health violations, incitement to cancel culture) that narrow Brandenburg's protective scope slightly over time. Suppression (0.42): Moderate. The constraint suppresses dangerousness-talk, which has real communicative costs. But suppression is not high because the suppression is confined to imminent incitement and the standard is relatively stable — speakers can navigate around the boundary through rhetorical choice. Theater ratio (0.35): Low. The Brandenburg standard is substantive, not performative — judges and prosecutors actually apply the imminent-and-likely test, not as a mask for other concerns but as a genuine doctrinal constraint. The constraint has low theater because the standard has real teeth and real effects on case outcomes.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how the same doctrinal boundary produces radically different classifications from different positions. For radical advocates constrained by prosecution risks, Brandenburg appears as protective rope — it enables speech that would otherwise be suppressed. For speakers who self-censor beyond Brandenburg's actual protection, it appears as extractive snare — the constraint they cannot exercise. For prosecutors, it is constrained rope — enabling clear standards but limiting discretion. For civil liberties organizations, it is a temporary scaffold — a holding against pressures to expand exceptions. For vestigial bad-tendency regimes, it is a degraded piton — persisting through inertia. For the analytical observer, it risks appearing as a mountain — a natural law about where speech becomes action — but the structural data reveals this as a false summit. The perspectival gap is the core diagnostic signal: no single classification captures the constraint's full structure, but the indexed set of perspectives reveals why different actors experience it so differently.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's effective extractiveness (χ) is computed from the base extractiveness (0.38), the agent's power level, their exit options, and whether they are beneficiary or victim. Radical advocates with constrained exit who benefit from protection experience low χ (the constraint subsidizes them). Speech-chilled victims with trapped exit experience high χ (the constraint extracts from them). Prosecutors with constrained exit who both benefit (clarity) and lose (discretion) experience moderate χ. The civil liberties coalition with mobile exit experiences low χ (they can exit through building alternative institutions). The analytical observer with analytical position experiences χ derived from their structural relationship to the constraint's naturalizing framing. The piton perspective experiences high theater ratio but relatively low χ because the bad-tendency regime is no longer active — it persists through inertia rather than active extraction.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    imminent_temporal_boundary,
    'What temporal distance constitutes ''imminent'' lawless action? Seconds? Minutes? Hours? Days?',
    'Case law analysis across jurisdictions; comparison of prosecution timelines in Brandenburg-compliant and hostile jurisdictions; judicial opinion patterns on temporal interpretation',
    'If imminent = seconds to minutes: Brandenburg narrows incitement to immediate coordination, protecting almost all abstract advocacy. If imminent = hours to days: prosecutors gain discretion to prosecute speeches that might inspire action within a reasonable preparation window, shifting toward bad-tendency. The boundary''s location determines how much abstract advocacy Brandenburg actually protects.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(imminent_temporal_boundary, conceptual, 'Temporal boundary for ''imminent'' lawless action').

omega_variable(
    likelihood_standard_subjectivity,
    'Does ''likely to incite'' mean statistically probable, or does speaker intent, audience receptivity, and context shift the standard per case?',
    'Doctrinal analysis of appellate decisions on likelihood; comparison of Brandenburg application in hostile vs. protective jurisdictions; linguistic analysis of appellate reasoning about causation',
    'If objective/statistical: Brandenburg confines prosecution to rare cases where incitement is demonstrable. If context-dependent: judges gain discretion, Brandenburg collapses toward bad-tendency application. Extractiveness of the constraint (its suppressive force) varies inversely with this boundary''s clarity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(likelihood_standard_subjectivity, conceptual, 'Whether likelihood standard is objective or context-dependent').

omega_variable(
    directed_versus_categorical,
    'Does ''directed to'' imminent lawless action require speaker''s conscious intent to incite, or can the speech be directed to that action by virtue of its content and context, regardless of speaker purpose?',
    'Comparative analysis of Brandenburg holdings; linguistic patterns in appellate reasoning; contrast with doctrines that require subjective intent vs. those that infer direction from content',
    'If intent-required: Brandenburg strongly protects speakers who use dangerous rhetoric for abstract or educational purposes. If direction-by-content: speakers can lose protection even if their intent was abstract advocacy. This boundary determines how much unpopular speech Brandenburg actually shields.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(directed_versus_categorical, conceptual, 'Whether ''directed to'' requires speaker intent or can be inferred from content').

omega_variable(
    reading_versus_sibling_foreclosure,
    'Does Brandenburg''s imminent-and-likely standard logically foreclose the bad-tendency test, or do the readings coexist as competing institutional choices that different regimes can adopt?',
    'Jurisprudential analysis: can a court consistently apply both standards to the same speech? Do the standards produce contradictory outcomes on identical facts, or do they produce the same outcomes through different reasoning?',
    'If foreclosure: Brandenburg is the only logically coherent doctrine; bad-tendency regimes are incoherent. If coexistence: both are live options institutionally, and the choice between them is political/normative rather than logical. Affects classification of the bad-tendency piton as degraded (foreclosed?) or merely abandoned.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_versus_sibling_foreclosure, conceptual, 'Whether Brandenburg forecloses or coexists with bad-tendency standard').

omega_variable(
    false_summit_naturalness,
    'Is the imminent-and-likely boundary a natural law about the constitutive boundary between speech and action, or a contingent institutional choice that serves specific political beneficiaries (radical advocates, civil liberties organizations)?',
    'Historical analysis: was the boundary discovered as a truth about speech, or constructed as a doctrinal choice? Who benefits from Brandenburg vs. bad-tendency? Is the boundary stable across different political regimes, or does it shift with political power?',
    'If natural: Brandenburg constraint is a mountain. If contingent: Brandenburg is a tangled rope whose mountainous appearance masks coordinated extraction. The false summit detection engine flags this as the core uncertainty.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(false_summit_naturalness, conceptual, 'Whether imminent-and-likely boundary is natural or contingent').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(categorical_exceptions_doctrine__incitement_brandenburg_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Extraction over time
narrative_ontology:measurement(incitement_brandenburg_extract_t0, categorical_exceptions_doctrine__incitement_brandenburg_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(incitement_brandenburg_extract_t25, categorical_exceptions_doctrine__incitement_brandenburg_reading, base_extractiveness, 25, 0.34).
narrative_ontology:measurement(incitement_brandenburg_extract_t50, categorical_exceptions_doctrine__incitement_brandenburg_reading, base_extractiveness, 50, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(incitement_brandenburg_suppress_t0, categorical_exceptions_doctrine__incitement_brandenburg_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(incitement_brandenburg_suppress_t25, categorical_exceptions_doctrine__incitement_brandenburg_reading, suppression_requirement, 25, 0.4).
narrative_ontology:measurement(incitement_brandenburg_suppress_t50, categorical_exceptions_doctrine__incitement_brandenburg_reading, suppression_requirement, 50, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(categorical_exceptions_doctrine__incitement_brandenburg_reading, information_standard).
narrative_ontology:affects_constraint(categorical_exceptions_doctrine__incitement_brandenburg_reading, categorical_exceptions_doctrine__no_new_categories_reading).
narrative_ontology:affects_constraint(categorical_exceptions_doctrine__incitement_brandenburg_reading, categorical_exceptions_doctrine__obscenity_miller_reading).

% DUAL FORMULATION NOTE:
% Brandenburg is one reading of the categorical-exceptions kernel. The kernel is contested: different constitutional frameworks identify the boundary between protected and unprotected speech differently. Brandenburg fixes the boundary at imminent lawless action; Stevens (no_new_categories_reading) fixes it at long-historical categories; Miller (obscenity_miller_reading) maintains a three-part test for obscenity. These are structurally distinct constraints with different ε values, different beneficiary/victim structures, and different extractiveness profiles. Network edges indicate that Brandenburg's success in narrowing incitement prosecutions influences how widely new-category arguments can be deployed (affects Stevens reading); Miller's community-standards framing influences how courts interpret Brandenburg's 'likely' and 'directed to' prongs (affected by Brandenburg, affects Brandenburg). Each reading is a separate constraint story.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
