% ============================================================================
% CONSTRAINT STORY: printing_standardization
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-09
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_printing_standardization, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
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
 *   constraint_id: printing_standardization
 *   human_readable: Printing Standardization of Humanist Latin Orthography
 *   domain: historical_linguistics/philology/intellectual_history
 *
 * SUMMARY:
 *   The printing standardization constraint describes how printing technology
 *   enabled enforcement of humanist Latin orthographic reforms by freezing
 *   variation and suppressing regional manuscript traditions. Between 1470
 *   and 1590, printing shops across Europe adopted increasingly uniform
 *   orthographic conventions based on humanist philological reconstruction of
 *   Classical Latin. This standardization solved a genuine coordination
 *   problem (uniform texts enabled wider circulation) but embedded asymmetric
 *   extraction: regional Latin variants and medieval scholastic orthographic
 *   practices were delegitimized as 'corrupt' and excluded from print
 *   circulation. The constraint exhibits piton characteristics by the late
 *   16th century: the original reform function (correcting genuine textual
 *   corruption vs recovering Classical forms) has atrophied into theatrical
 *   maintenance of a frozen standard through prestige signaling rather than
 *   active philological work. The theater_ratio rises from 0.25 (1470, early
 *   reform period with genuine textual work) to 0.78 (1590, frozen standard
 *   maintained through institutional inertia). Extractiveness rises from 0.15
 *   to 0.35 as the standard locks in, then plateaus as the extraction
 *   mechanism stabilizes. Suppression rises from 0.40 to 0.68 as printing
 *   economics and humanist institutional authority make non-standard
 *   orthography increasingly costly to maintain.
 *
 * KEY AGENTS:
 *   - Regional Latin Variants: Primary victim (powerless/trapped) — local scribal traditions with no exit from standardization pressure; delegitimized as 'corrupt'
 *   - Medieval Scholastic Practitioners: Secondary victim (moderate/constrained) — benefit from Latin's continued use but bear cost of orthographic reclassification; can adopt humanist forms at cost of abandoning transmitted practice
 *   - Printing Industry: Primary beneficiary (institutional/arbitrage) — standardization reduces compositor training costs and enables type reuse; can choose which standard to adopt based on market demand
 *   - Humanist Philologists: Primary beneficiary (powerful/mobile) — their reconstructed standard becomes materially instantiated in print; textual authority gains enforcement mechanism
 *   - Humanist Orthographic Reform Movement: Institutional actor (institutional/constrained) — by late 16th century, maintains frozen standard through institutional inertia rather than active philological work (piton perspective)
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees both genuine coordination function and asymmetric extraction embedded in same mechanism
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(printing_standardization, 0.35).
domain_priors:suppression_score(printing_standardization, 0.68).
domain_priors:theater_ratio(printing_standardization, 0.78).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(printing_standardization, extractiveness, 0.35).
narrative_ontology:constraint_metric(printing_standardization, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(printing_standardization, theater_ratio, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(printing_standardization, piton).
narrative_ontology:human_readable(printing_standardization, "Printing Standardization of Humanist Latin Orthography").
narrative_ontology:topic_domain(printing_standardization, "historical_linguistics/philology/intellectual_history").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(printing_standardization, '475562eb-e8be-4cf8-81a2-22917358ef74').
narrative_ontology:cs_kernel_codification('475562eb-e8be-4cf8-81a2-22917358ef74', fixed_text).
narrative_ontology:cs_authority_grounding('475562eb-e8be-4cf8-81a2-22917358ef74', lineage).
narrative_ontology:cs_interpretation_layer_present('475562eb-e8be-4cf8-81a2-22917358ef74').
narrative_ontology:cs_reading_relation('475562eb-e8be-4cf8-81a2-22917358ef74', printing_standardization__continuity_reading, forecloses).
narrative_ontology:cs_reading_relation('475562eb-e8be-4cf8-81a2-22917358ef74', printing_standardization__hybrid_reading, influences).
narrative_ontology:cs_axiom('475562eb-e8be-4cf8-81a2-22917358ef74', foundational, textual_fidelity_grounds_correctness).
narrative_ontology:cs_axiom_status(textual_fidelity_grounds_correctness, holdable).
narrative_ontology:cs_axiom_grounding('475562eb-e8be-4cf8-81a2-22917358ef74', textual_fidelity_grounds_correctness, conventional).
narrative_ontology:cs_axiom('475562eb-e8be-4cf8-81a2-22917358ef74', secondary, medieval_forms_are_corruption).
narrative_ontology:cs_axiom_status(medieval_forms_are_corruption, holdable).
narrative_ontology:cs_axiom_grounding('475562eb-e8be-4cf8-81a2-22917358ef74', medieval_forms_are_corruption, empirically_contingent).
narrative_ontology:cs_reference_frame('475562eb-e8be-4cf8-81a2-22917358ef74', classical_textual_corpus).
narrative_ontology:cs_drift_state('475562eb-e8be-4cf8-81a2-22917358ef74', late_medieval_period, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('475562eb-e8be-4cf8-81a2-22917358ef74', '').

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(printing_standardization, printing_industry).
narrative_ontology:constraint_beneficiary(printing_standardization, humanist_philologists).
narrative_ontology:constraint_victim(printing_standardization, regional_latin_variants).
narrative_ontology:constraint_victim(printing_standardization, medieval_scholastic_tradition).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(printing_standardization, medieval_scholastic_teachers).
narrative_ontology:constraint_beneficiary(printing_standardization, printing_houses).
narrative_ontology:constraint_victim(printing_standardization, regional_scribal_traditions).
narrative_ontology:constraint_victim(printing_standardization, medieval_scholastic_teachers).
narrative_ontology:constraint_vindicates(printing_standardization, textual_fidelity_doctrine).
narrative_ontology:constraint_vindicates(printing_standardization, classical_purity_ideal).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Local scribal workshops and regional orthographic practices face delegitimization as 'corrupt' by humanist standard. Cannot exit: printing economics favor uniform texts, and humanist institutional authority controls access to prestige and patronage. Their orthographic knowledge becomes unmarketable as printers adopt humanist conventions.
narrative_ontology:constraint_stakeholder(printing_standardization, regional_scribal_traditions, payer,
    powerless, biographical, trapped, regional).

% University teachers and Church scribes benefit from Latin's continued use as scholarly and liturgical language (coordination function) but bear the cost of their transmitted orthographic practices being reclassified as errors. Can adopt humanist forms to remain in print circulation, but at cost of abandoning practices learned through institutional training.
narrative_ontology:constraint_stakeholder(printing_standardization, medieval_scholastic_teachers, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(printing_standardization, medieval_scholastic_teachers, beneficiary).

% Printing shops benefit from orthographic standardization: uniform conventions reduce compositor training costs, enable type reuse across texts, and create market advantage for early adopters of the emerging standard. Can choose which orthographic standard to adopt based on market demand and move between regional markets.
narrative_ontology:constraint_stakeholder(printing_standardization, printing_houses, beneficiary,
    institutional, immediate, arbitrage, continental).

% Humanist scholars set the orthographic standard through textual editions and pedagogical works. Benefit from printing's material enforcement of their reconstructed standard: their textual authority becomes instantiated in printed editions. Mobile across patronage networks and printing centers; their philological expertise is in demand.
narrative_ontology:constraint_stakeholder(printing_standardization, humanist_philologists, agenda_setter,
    powerful, generational, mobile, continental).
narrative_ontology:stakeholder_secondary_role(printing_standardization, humanist_philologists, beneficiary).

% Non-Latinate readers excluded from the standardization debate entirely. Would object to resources spent on Latin orthographic reform rather than vernacular literacy, but are not in the conversation. The constraint operates in a domain (learned Latin) they cannot access.
narrative_ontology:constraint_stakeholder(printing_standardization, vernacular_readers, excluded,
    powerless, biographical, trapped, regional).

% Observes the constraint from outside its operation. Sees both the genuine coordination function (uniform texts enable wider circulation) and the asymmetric extraction (regional variants suppressed, medieval practices delegitimized). Neither collects from nor pays into the standardization mechanism.
narrative_ontology:constraint_stakeholder(printing_standardization, analytical_historian, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Printing standardization solves the coordination problem of cross-regional textual circulation: uniform orthography enables readers trained in one region to read texts printed in another, and enables printers to serve wider markets without retraining compositors for each regional variant.
% TRANSFER_FUNCTION: The arrangement transfers prestige, market access, and institutional authority from regional scribal traditions and medieval scholastic practices to humanist philologists and printing houses. Regional orthographic knowledge becomes unmarketable; humanist textual authority becomes materially instantiated in print.
% ABSENT_VOICES: Vernacular readers are excluded from the debate entirely. They would object to resources spent on Latin orthographic reform rather than vernacular literacy, but the constraint operates in a domain (learned Latin) they cannot access. Their absence enables the humanist-printer coalition to present standardization as purely technical rather than as a choice with distributional consequences.
% DISAPPEARANCE_RATIONALE: If printing standardization disappeared overnight, regional scribal traditions would persist with their local orthographic practices, medieval scholastic conventions would remain legitimate in university and Church contexts, and humanist philologists would lose the material enforcement mechanism for their reconstructed standard. The printing industry would face higher compositor training costs and smaller market reach. The world rearranges because multiple parties' arrangements (printers' production costs, humanists' textual authority, scholastics' orthographic legitimacy) depend on the standardization mechanism.
% FOUNDING_PROBLEM: The founding problem was genuine textual corruption and orthographic inconsistency in manuscript transmission of Classical Latin texts. Humanist philologists identified scribal errors, medieval orthographic innovations, and regional variation as obstacles to recovering authentic Classical forms. The reform was built to solve the problem of textual fidelity: how to recover and transmit Classical Latin accurately across time and space.
% FOUNDING_PROBLEM_CORROBORATION: Humanist philologists and their institutional successors (classical philology departments, critical edition projects) attest that the founding problem remains live: textual criticism continues to identify and correct scribal errors. Regional scribal traditions and medieval scholastic practitioners contest this: they argue that medieval orthographic practices were systematic and functional, not corrupt, and that the 'problem' was constructed by humanist framing to legitimate their authority claim. Analytical historians (e.g., Anthony Grafton, Lisa Jardine) corroborate that the founding problem was partly genuine (some scribal errors existed) and partly constructed (many medieval forms were systematic innovations, not corruptions). The contest is over whether the problem's scope justified the suppression of regional variants.
narrative_ontology:disappearance_verdict(printing_standardization, world_rearranges).
narrative_ontology:founding_problem_status(printing_standardization, contested).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: REGIONAL LATIN VARIANTS (SNARE) — Local scribal traditions and regional orthographic practices have no exit from standardization pressure. Printing economics favor uniform texts over regional variation. Maximum extraction: regional forms are delegitimized as 'corrupt' and excluded from print circulation.
constraint_indexing:constraint_classification(printing_standardization, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: MEDIEVAL SCHOLASTIC PRACTITIONERS (TANGLED ROPE) — University teachers and Church scribes benefit from Latin's continued use (coordination function) but bear the cost of their orthographic practices being reclassified as errors. Constrained exit: can adopt humanist forms to remain in print circulation, but at cost of abandoning transmitted practice.
constraint_indexing:constraint_classification(printing_standardization, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PRINTING INDUSTRY (ROPE) — Printers benefit from standardization: uniform orthography reduces compositor training costs, enables type reuse across texts, and creates market advantage for shops adopting the emerging standard. Net beneficiary with exit options: can choose which orthographic standard to adopt based on market demand.
constraint_indexing:constraint_classification(printing_standardization, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 4: HUMANIST PHILOLOGISTS (ROPE) — Humanist scholars benefit from printing's enforcement of their reconstructed standard. Their textual authority becomes materially instantiated in printed editions. Mobile exit: can move between patronage networks and printing centers; their expertise is in demand.
constraint_indexing:constraint_classification(printing_standardization, rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 5: HUMANIST ORTHOGRAPHIC REFORM (PITON) — By the late 16th century, the reform's primary function (correcting medieval 'corruption') has atrophied. Printing has locked in a standard, but the standard is maintained through institutional inertia and prestige signaling rather than active philological work. The theater ratio is high: continued invocation of 'classical purity' justifies a frozen orthography that no longer corresponds to any living practice or genuine textual recovery.
constraint_indexing:constraint_classification(printing_standardization, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — Printing standardization solves a genuine coordination problem (uniform texts enable wider circulation and cross-regional scholarship) but embeds asymmetric extraction: the humanist standard delegitimates medieval forms and suppresses regional variation. The coordination function is real; the extraction is also real. Both persist through the same mechanism.
constraint_indexing:constraint_classification(printing_standardization, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(printing_standardization_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(printing_standardization, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(printing_standardization, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(printing_standardization, TR),
    TR >= 0.70.

:- end_tests(printing_standardization_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.35): Moderate. The humanist standard captures prestige and market access during the standardization window. Regional variants and medieval scholastic practices are excluded from print circulation, but the extraction is not as severe as pure rent-seeking because printing does solve a genuine coordination problem. The value reflects that career and institutional asymmetry is real but partly offset by coordination benefits. Suppression (0.68): Moderate-high. Significant barriers to maintaining non-standard orthography include printing economics (uniform texts are cheaper to produce), humanist institutional authority (universities and patronage networks favor the standard), and market access (non-standard texts have smaller circulation). But suppression is not total: manuscript culture persists in some contexts, and regional variants survive in non-printed domains. Theater ratio (0.78): High. By the late 16th century, invocation of 'classical purity' and 'textual fidelity' maintains a frozen orthography through prestige signaling rather than active philological work. The standard no longer corresponds to genuine textual recovery (many 'classical' forms are Renaissance innovations) or to any living practice. The reform's original function has atrophied; what remains is institutional performance.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how the same structural phenomenon appears differently from different positions. Regional variants see pure extraction (Snare): their forms are delegitimized with no coordination benefit to them. Medieval scholastic practitioners see mixed coordination and extraction (Tangled Rope): they benefit from Latin's continued use but bear orthographic reclassification costs. The printing industry sees coordination (Rope): standardization solves their production cost problem. Humanist philologists see coordination (Rope): their standard gains material enforcement. The late-stage reform movement sees its own degraded ritual (Piton): the standard persists through institutional inertia rather than active philological work. The analytical observer sees tangled rope at the civilizational scale: genuine coordination function with embedded asymmetric extraction, both persisting through the same mechanism.
 *
 * DIRECTIONALITY LOGIC:
 *   Regional Latin variants are full victims with trapped exit options: they experience maximum extraction because they cannot exit the standardization pressure and are delegitimized by the humanist framing. Medieval scholastic practitioners are partial victims with constrained exit: they benefit from Latin's continued use (coordination function) but bear the cost of orthographic reclassification; they can adopt humanist forms but only by abandoning transmitted practice. The printing industry and humanist philologists are beneficiaries with arbitrage and mobile exit options respectively: they experience low or negative effective extraction because the standardization mechanism runs toward them. The piton classification for the late-stage reform movement derives from the theater gate (high theater_ratio) rather than from high experienced extraction: the movement maintains a frozen standard through institutional inertia, not because it continues to perform its original corrective function.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy by showing that printing standardization is neither pure coordination (Rope) nor pure extraction (Snare) but a hybrid that appears differently from different structural positions. The coordination function is real: uniform orthography enables wider circulation and cross-regional scholarship. The extraction is also real: regional variants are suppressed and medieval practices delegitimized. The piton classification captures the constraint's late-stage degradation: by 1590, the reform's original function (textual correction) has atrophied into theatrical maintenance of a frozen standard. The perspectival gap between the printing industry's rope (coordination benefit) and regional variants' snare (extraction without coordination benefit) is the structural reality the framework measures.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    textual_fidelity_vs_practice,
    'Does the humanist standard actually recover Classical Latin orthography, or does it construct a new standard using Classical texts as legitimating symbols?',
    'Comparison of humanist printed orthography against manuscript evidence from Classical period; identification of Renaissance innovations presented as Classical recovery',
    'If genuine recovery: humanist authority claim is empirically grounded. If constructed standard: the ''corruption'' narrative is itself extractive framing that naturalizes a preference as a correction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(textual_fidelity_vs_practice, empirical, 'Whether humanist orthography recovers or constructs Classical forms').

omega_variable(
    printing_necessity,
    'Is printing technology necessary for orthographic standardization, or does it merely accelerate a process that manuscript culture could achieve?',
    'Comparative analysis of standardization rates in manuscript vs print cultures; examination of pre-print standardization attempts (Carolingian reforms, chancery standards)',
    'If necessary: printing is the constraint''s structural foundation. If accelerant: the constraint is primarily social (humanist authority) with printing as amplifier.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(printing_necessity, empirical, 'Whether printing is necessary or merely accelerant for standardization').

omega_variable(
    regional_variant_legitimacy,
    'Are regional Latin variants genuine evolutionary developments with internal consistency, or are they unsystematic corruptions as humanist framing claims?',
    'Linguistic analysis of medieval Latin orthographic systems for internal coherence, rule-governed variation, and functional adequacy for their communicative contexts',
    'If systematic: suppression of regional variants is extraction from functioning systems. If unsystematic: humanist reform has genuine corrective function.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(regional_variant_legitimacy, empirical, 'Whether regional variants are systematic or corrupt').

omega_variable(
    cs_framing_underdetermination,
    'Is the kernel ''correct Latin'' grounded in the textual corpus (discontinuity reading: texts are the authority) or in the interpretive tradition that reads the texts (continuity reading: practice is the authority)?',
    'Examination of which reading''s axioms are invoked when humanist and scholastic authorities conflict on specific usage questions; tracking whether appeals to ''Cicero'' mean ''what Cicero wrote'' or ''what our tradition says Cicero meant''',
    'If textual: discontinuity reading is structurally correct and the kernel is the ancient corpus. If interpretive: continuity reading is structurally correct and the kernel is the living tradition of reading that corpus. The framing choice determines whether medieval Latin is ''corruption'' (discontinuity) or ''evolution'' (continuity).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cs_framing_underdetermination, conceptual, 'Whether the kernel is the textual corpus or the interpretive tradition').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(printing_standardization, 0, 120).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(print_std_theater_1470, printing_standardization, theater_ratio, 0, 0.25).
narrative_ontology:measurement(print_std_theater_1500, printing_standardization, theater_ratio, 30, 0.45).
narrative_ontology:measurement(print_std_theater_1530, printing_standardization, theater_ratio, 60, 0.62).
narrative_ontology:measurement(print_std_theater_1560, printing_standardization, theater_ratio, 90, 0.73).
narrative_ontology:measurement(print_std_theater_1590, printing_standardization, theater_ratio, 120, 0.78).

% Extraction over time
narrative_ontology:measurement(print_std_extract_1470, printing_standardization, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(print_std_extract_1500, printing_standardization, base_extractiveness, 30, 0.28).
narrative_ontology:measurement(print_std_extract_1530, printing_standardization, base_extractiveness, 60, 0.35).
narrative_ontology:measurement(print_std_extract_1560, printing_standardization, base_extractiveness, 90, 0.36).
narrative_ontology:measurement(print_std_extract_1590, printing_standardization, base_extractiveness, 120, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(print_std_suppress_1470, printing_standardization, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(print_std_suppress_1500, printing_standardization, suppression_requirement, 30, 0.58).
narrative_ontology:measurement(print_std_suppress_1530, printing_standardization, suppression_requirement, 60, 0.68).
narrative_ontology:measurement(print_std_suppress_1560, printing_standardization, suppression_requirement, 90, 0.7).
narrative_ontology:measurement(print_std_suppress_1590, printing_standardization, suppression_requirement, 120, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(printing_standardization, information_standard).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the correct_latin kernel. The continuity_reading and hybrid_reading are sibling constraints with different beneficiary structures and different extraction profiles. The discontinuity_reading (this constraint) has the highest suppression of regional variants because it delegitimates medieval forms most completely.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
