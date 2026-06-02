% ============================================================================
% CONSTRAINT STORY: jihad_quranic_corpus__expansionist_legalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jihad_quranic_corpus__expansionist_legalist_reading, []).

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
 *   constraint_id: jihad_quranic_corpus__expansionist_legalist_reading
 *   human_readable: Jihad as Expansionist Legal Obligation (Legalist Reading)
 *   domain: islamic_jurisprudence/political_theology
 *
 * SUMMARY:
 *   This constraint instantiates one specific reading of the contested kernel
 *   'jihad_quranic_corpus': the expansionist-legalist interpretation that
 *   systematizes offensive military expansion as a legal obligation under the
 *   Islamic state, subject to jurisprudential conditions (invitation to Islam
 *   offered first, imam/caliph authority over declaration, proportionality
 *   and non-combatant protections, cessation if non-Muslims accept Islam or
 *   submit to Islamic authority). This reading emerged and crystallized
 *   during the classical Islamic period (8th-12th centuries) within the major
 *   jurisprudential schools, establishing jihad as a state-monopolized
 *   military institution legitimating territorial expansion while
 *   constraining its application through explicit legal conditions. The
 *   constraint differs from sibling readings: the defensive-only
 *   interpretation restricts jihad to repelling aggression, and the
 *   revolutionary-vanguard reading permits non-state actors to declare jihad
 *   against un-Islamic governments. The expansionist-legalist reading creates
 *   a specific structural outcome: non-Muslims occupy a liminal legal status
 *   (potential dhimmis paying jizya or combatants), the state monopolizes
 *   expansion authority, and systematic conquest is permitted within bounds.
 *   The doctrine has declined operatively in the modern state system
 *   (post-caliphate, international law, nation-state sovereignty) but
 *   persists in Sharia canons as inherited textual authority. This reading is
 *   neither a natural law of Islamic theology nor an arbitrary
 *   interpretation—it is a stabilized commitment grounded in specific textual
 *   readings and legal principles, with identifiable beneficiaries (caliphal
 *   authority, legal establishment) and victims (non-Muslim populations,
 *   dissenting jurists).
 *
 * KEY AGENTS:
 *   - Caliphal Authority and State: Institutional beneficiary (institutional/arbitrage) — monopoly on jihad declaration and legitimacy for territorial expansion within legal bounds
 *   - Islamic Legal Scholars and Establishments: Institutional beneficiary (institutional/arbitrage) — canonical authority over interpretation, prestige in codifying doctrine, power to suppress dissenting readings
 *   - Non-Muslim Populations in Liminal Territories: Primary victim (powerless/trapped) — no structural exit option within the doctrine's framework; only options are conversion, tribute submission, or warfare
 *   - Dissenting Islamic Jurists (Defensive and Spiritual Schools): Secondary victim (moderate/constrained) — face suppression within the legal hierarchy and canonical authority but retain some reinterpretive agency through textual argument
 *   - Adjacent Non-Muslim States and Polities: Secondary actor (organized/constrained) — experience both interaction-rule benefits (defined legal terms of conflict) and extraction (systematic expansion pressure under legal framework)
 *   - Modern Islamic Legal Reform Movements: Organized beneficiary of doctrine decline (organized/mobile) — exit the constraint through reframing (spiritual-only jihad, state-sovereignty adoption, international law accommodation)
 *   - Analytical Observer: Civilizational position (analytical/analytical) — risks naturalizing a kernel reading as immutable theological law; false summit detection necessary
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jihad_quranic_corpus__expansionist_legalist_reading, 0.68).
domain_priors:suppression_score(jihad_quranic_corpus__expansionist_legalist_reading, 0.72).
domain_priors:theater_ratio(jihad_quranic_corpus__expansionist_legalist_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jihad_quranic_corpus__expansionist_legalist_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(jihad_quranic_corpus__expansionist_legalist_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(jihad_quranic_corpus__expansionist_legalist_reading, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jihad_quranic_corpus__expansionist_legalist_reading, tangled_rope).
narrative_ontology:human_readable(jihad_quranic_corpus__expansionist_legalist_reading, "Jihad as Expansionist Legal Obligation (Legalist Reading)").
narrative_ontology:topic_domain(jihad_quranic_corpus__expansionist_legalist_reading, "islamic_jurisprudence/political_theology").

domain_priors:requires_active_enforcement(jihad_quranic_corpus__expansionist_legalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jihad_quranic_corpus__expansionist_legalist_reading, '3bca4894-6946-4e7d-a335-64fdf7a89e92').
narrative_ontology:cs_kernel_codification('3bca4894-6946-4e7d-a335-64fdf7a89e92', fixed_text).
narrative_ontology:cs_authority_grounding('3bca4894-6946-4e7d-a335-64fdf7a89e92', lineage).
narrative_ontology:cs_interpretation_layer_present('3bca4894-6946-4e7d-a335-64fdf7a89e92').
narrative_ontology:cs_reading_relation('3bca4894-6946-4e7d-a335-64fdf7a89e92', jihad_quranic_corpus__defensive_spiritual_reading, coexists_with).
narrative_ontology:cs_reading_relation('3bca4894-6946-4e7d-a335-64fdf7a89e92', jihad_quranic_corpus__revolutionary_vanguard_reading, influences).
narrative_ontology:cs_axiom('3bca4894-6946-4e7d-a335-64fdf7a89e92', foundational, state_monopoly_jihad_declaration).
narrative_ontology:cs_axiom_status(state_monopoly_jihad_declaration, holdable).
narrative_ontology:cs_axiom_grounding('3bca4894-6946-4e7d-a335-64fdf7a89e92', state_monopoly_jihad_declaration, conventional).
narrative_ontology:cs_axiom('3bca4894-6946-4e7d-a335-64fdf7a89e92', foundational, expansion_licit_under_conditions).
narrative_ontology:cs_axiom_status(expansion_licit_under_conditions, holdable).
narrative_ontology:cs_axiom_grounding('3bca4894-6946-4e7d-a335-64fdf7a89e92', expansion_licit_under_conditions, empirically_contingent).
narrative_ontology:cs_reference_frame('3bca4894-6946-4e7d-a335-64fdf7a89e92', state_mandated_expansion_within_bounds).
narrative_ontology:cs_drift_state('3bca4894-6946-4e7d-a335-64fdf7a89e92', contemporary_post_caliphate_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('3bca4894-6946-4e7d-a335-64fdf7a89e92', '').
narrative_ontology:cs_kernel_id(jihad_quranic_corpus__expansionist_legalist_reading, jihad_quranic_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jihad_quranic_corpus__expansionist_legalist_reading, caliphal_authority).
narrative_ontology:constraint_beneficiary(jihad_quranic_corpus__expansionist_legalist_reading, islamic_legal_scholars).
narrative_ontology:constraint_victim(jihad_quranic_corpus__expansionist_legalist_reading, non_muslim_populations).
narrative_ontology:constraint_victim(jihad_quranic_corpus__expansionist_legalist_reading, dissenting_islamic_jurists).
narrative_ontology:constraint_victim(jihad_quranic_corpus__expansionist_legalist_reading, territorial_polities_adjacent_to_dar_islam).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NON-MUSLIM POPULATIONS (SNARE) — Trapped populations in liminal territories lack structural exit or negotiation power. The legalist framework presents conversion, tribute-paying (jizya) submission, or warfare as the only permitted outcomes. No alternative autonomy is permitted within the doctrine; suppression is enforced through legal exhaustion of options.
constraint_indexing:constraint_classification(jihad_quranic_corpus__expansionist_legalist_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: DISSENTING ISLAMIC JURISTS (TANGLED ROPE) — Jurists emphasizing defensive-only or spiritual interpretations face suppression within the legal hierarchy but retain some agency through textual reinterpretation and canonical authority appeals. The constraint extracts doctrinal conformity while offering coordination benefits (participation in scholarly prestige and legal authority). Genuine coordination function (unified jurisprudence) coupled with asymmetric extraction (minority views delegitimized).
constraint_indexing:constraint_classification(jihad_quranic_corpus__expansionist_legalist_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: CALIPHAL AUTHORITY (ROPE) — The expansionist doctrine provides the caliph with legitimacy infrastructure for territorial claims, military mobilization, and monopoly over warfare declaration. The constraint solves the coordination problem of when military expansion is licit (imam authority, legal conditions, proportionality framework). Net beneficiary experiencing minimal extraction — the doctrine exists partly to enable this institutional actor's authority.
constraint_indexing:constraint_classification(jihad_quranic_corpus__expansionist_legalist_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: ADJACENT NON-MUSLIM STATES (TANGLED ROPE) — Neighboring polities experience both coordination benefits (predictable legal framework for treaties, defined terms of war, prohibition on slaughter of non-combatants) and extraction (vulnerability to systematic territorial pressure under legal guise). The constraint creates interaction rules but biases expansion toward Islamic territory.
constraint_indexing:constraint_classification(jihad_quranic_corpus__expansionist_legalist_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 5: MODERN ISLAMIC LEGAL REFORM (SCAFFOLD) — Contemporary movements (Islamic modernism, nationalist reinterpretation) see the expansionist doctrine as a temporary historical artifact being superseded by international law, national sovereignty, and spiritual-only interpretations. Low effective extraction for these actors because they possess exit pathways (reframing jihad as internal struggle, adopting state-sovereignty frameworks). Sunset logic: the doctrine's enforcement mechanism weakens as state system supplants caliphate.
constraint_indexing:constraint_classification(jihad_quranic_corpus__expansionist_legalist_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: TRADITIONAL SHARIA ESTABLISHMENT (PITON) — In contemporary nation-states, the expansionist doctrine persists in legal texts and scholarly canons but lacks operative enforcement mechanism (no caliphal authority to declare jihad, state monopoly on warfare, international law constraints). The doctrine functions largely as inert textual authority, maintained through institutional transmission rather than active use. High theater ratio reflects continued canonical authority despite minimal function.
constraint_indexing:constraint_classification(jihad_quranic_corpus__expansionist_legalist_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, the expansionist doctrine could appear as an immutable feature of classical Islamic political theology, a necessary interpretation of Quranic imperative given the historical context of 7th-century Arabia. However, the structural data contradicts this: identifiable beneficiaries (caliphal authority, legal establishment), suppression enforced through institutional hierarchy, and alternative interpretations demonstrate this is a constructed constraint, not a natural law. Engine will flag as false summit.
constraint_indexing:constraint_classification(jihad_quranic_corpus__expansionist_legalist_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jihad_quranic_corpus__expansionist_legalist_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(jihad_quranic_corpus__expansionist_legalist_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(jihad_quranic_corpus__expansionist_legalist_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(jihad_quranic_corpus__expansionist_legalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(jihad_quranic_corpus__expansionist_legalist_reading, TR),
    TR >= 0.70.

:- end_tests(jihad_quranic_corpus__expansionist_legalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High-moderate. The doctrine enables systematic territorial extraction through military expansion, legitimated by legal conditions (imam authority, proportionality). However, extractiveness is not maximal (0.75+) because genuine legal constraints (invitation first, imam monopoly, non-combatant protections, cessation if submission) do function—they are not pure theater. The constraint combines real coordination (unified jihad authority preventing fragmented warfare) with asymmetric extraction (non-Muslims bear conversion/submission/warfare burden; Islamic populations fund and fight). The trajectory shows rising extractiveness from early caliphate (0.42, when doctrine was less codified and expansion was more opportunistic) to classical period (0.68, when the doctrine crystallized and became systematized legal infrastructure). Suppression (0.72): High. The doctrine suppresses alternative interpretations (defensive-only, spiritual-only readings) through canonical authority and institutional hierarchy. Dissenters are delegitimized even when they appeal to identical Quranic verses. Suppression also suppresses exit options for non-Muslim populations—the framework exhausts licit alternatives (conversion, jizya submission, or warfare). Theater ratio (0.58): Moderate-high. The proportionality and imam-authority conditions add legalistic formality that is partly performed. However, proportionality has genuine bite in many historical cases (Quranic restrictions on non-combatant killing were enforced; imam authority did constrain some campaigns). The theater increases over time as doctrine becomes more codified and scholastic—more legal apparatus surrounds the same expansion mechanism. Claimed type (tangled_rope): The doctrine has both genuine coordination function (unified Islamic military authority, defined rules of engagement) and asymmetric extraction (expansion benefits the Islamic state and scholars; non-Muslim populations bear costs). Both elements are required; neither alone characterizes the constraint.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival diversity here is extreme. Non-Muslim populations experience the constraint as a snare—trapped with no acceptable exit. Dissenters within Islamic jurisprudence experience tangled rope—some agency through reinterpretation but also suppression. The caliphate experiences rope—the doctrine solves the coordination problem of when expansion is licit, benefiting the state. Adjacent states experience tangled rope—legal frameworks enable cooperation but bias expansion. Modern reform movements experience scaffold—the doctrine is being superseded by state sovereignty and spiritual reinterpretation. The traditional Sharia establishment experiences piton—the doctrine persists in canons but lacks operative enforcement (no caliphate, state monopoly on warfare, international law). The civilizational analytical observer risks experiencing the constraint as mountain (immutable Islamic theology) but structural data reveals false summit: the beneficiaries and alternative readings expose the constraint as a kernel reading, not a natural law.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from structural position: beneficiaries with state authority and legal monopoly experience low d (0.15-0.25) and low or negative chi; non-Muslim populations with no exit experience high d (0.90+) and high chi; moderate intermediate actors with constrained exit experience mid-range d. Caliphal authority and legal scholars are beneficiaries—the doctrine legitimates their expansion authority and canonical power—so d is low and they perceive rope (minimal experienced extraction because the constraint exists partly to enable them). Non-Muslim populations are victims trapped without alternative options—d approaches 1.0 and they perceive snare (maximum experienced extraction). The dissenting jurists are both victims (suppressed within the hierarchy) and partially beneficiaries (they participate in legal authority)—d is mid-range and they perceive tangled rope (mixed extraction and coordination benefit).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is high-extractiveness (0.68 > 0.46) with asymmetric extraction and coordination function both present. Mandatrophy is resolved by recognizing that BOTH elements are structurally real: the doctrine genuinely coordinates Islamic military authority (coordination function is not fake), AND it genuinely enables systematic territorial extraction from non-Muslim populations (extraction is not incidental). The doctrine is not a snare masquerading as coordination, nor is it pure coordination with accidental extraction. It is tangled rope—the two functions are fused. The legal conditions (proportionality, imam authority, invitation-first) are both genuinely constraining AND genuinely performative: they constrain *some* campaigns but also provide legal theater that legitimates *other* campaigns that might otherwise appear naked conquest. Mandatrophy resolution requires holding this dual reality: the constraint is designed to enable expansion while constraining its excess. Both design purposes are achieved.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    quranic_text_semantic_stability,
    'Do the Quranic verses (9:5, 9:29, 2:193) on warfare establish an objective mandate for expansionist doctrine, or are they maximally polysemic and compatible with defensive-only or spiritual interpretations?',
    'Comparative analysis of all major Islamic jurisprudential schools (Hanafi, Maliki, Shafi''i, Hanbali, Twelver, Zaydi); identification of semantic commitments each school adds to the text that are not textually required; controlled comparison of how identical verses ground contradictory doctrines across schools',
    'If text is uniquely semantic: expansionist reading is mandated by the kernel (other readings are forced exegesis); classification approaches mountain. If maximally polysemic: the constraint is a reading among several equally valid ones; classification is tangled_rope at the analytical level and confirms coexists_with relations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(quranic_text_semantic_stability, conceptual, 'Semantic stability of Quranic warfare imperatives').

omega_variable(
    historical_caliphal_intent_vs_doctrine_drift,
    'Did the early caliphs (7th-8th centuries) explicitly intend the expansionist doctrine as a vehicle for territorial conquest, or was the doctrine developed post-hoc by jurists to rationalize existing military practice?',
    'Chronological analysis of primary sources: when does the doctrine first appear in jurisprudential texts vs when do military campaigns occur? Attribution analysis: which jurists first codify the expansionist interpretation and in what historical context? Correlation with caliphal policy statements.',
    'If intentional doctrine: supports the reading''s claim that expansion is structurally coded. If post-hoc rationalization: reveals the doctrine as a constraint that emerged from practice-drift and was later canonized — the beneficiaries (caliphal authority, scholars) are revealed as having shaped the doctrine for their institutional benefit.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(historical_caliphal_intent_vs_doctrine_drift, empirical, 'Historical timing of doctrine vs. military practice').

omega_variable(
    proportion_enforcement_asymmetry,
    'Are the proportionality and imam-authority conditions (invitation first, imam monopoly, no slaughter of non-combatants) enforced symmetrically across Islamic and non-Islamic warfare, or are they systematically relaxed in practice against non-Muslim combatants?',
    'Historical analysis of conflicts in classical Islamic world: instances where proportionality or imam conditions were enforced vs violated; comparative analysis with how the doctrine constrains intra-Islamic conflicts; documentation of jurists'' actual judgments on violations',
    'If symmetrically enforced: the constraint is a genuine legal framework with binding conditions (tangled_rope confirmed). If asymmetrically relaxed: the proportionality conditions are theater — the constraint is a snare with cosmetic legalism (extractiveness increases to 0.75+).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proportion_enforcement_asymmetry, empirical, 'Enforcement symmetry of proportionality and authority conditions').

omega_variable(
    doctrine_vs_sibling_interpretations_logical_structure,
    'Is the expansionist reading logically incompatible with the defensive and spiritual readings within a single authoritative framework, or do the readings occupy different problem-spaces (military conquest vs individual ethics vs collective resistance)?',
    'Formal analysis: do the three readings appeal to contradictory interpretive principles, or do they address different contexts? Can a single scholarly authority credibly hold all three simultaneously (defense is primary, but expansion is permissible under conditions; spiritual jihad is primary, but military jihad is secondary but valid)? Historical evidence of jurists holding multiple readings.',
    'If logically incompatible within single framework: reading relation is forecloses (only one can be authoritative). If address different problem-spaces: relation is coexists_with (multiple readings are live). If there is upstream/downstream causal pressure: relation is influences.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(doctrine_vs_sibling_interpretations_logical_structure, conceptual, 'Logical compatibility between expansionist and sibling readings').

omega_variable(
    suppression_mechanism_structural_vs_theological,
    'Is the measured suppression (0.72) primarily structural (caliphal control over military authority, resource monopoly) or theological (belief that the expansion is divinely mandated and thus non-negotiable)?',
    'Analysis of dissenting scholar suppression mechanisms: are dissenters silenced through institutional hierarchy and exclusion, or through canonical authority claims about what Islam requires? Evidence of scholars escaping suppression by exit (geographic relocation, institutional switching) vs inability to escape despite opportunity.',
    'If structural: suppression persists only while institutional hierarchy maintains control; exits by institutions or reduction of caliphal power change suppression profile. If theological: suppression is internalized and persists even absent institutional coercion (rises to identity_locked territory).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_theological, empirical, 'Structural vs theological suppression mechanism').

omega_variable(
    false_summit_natural_law_claim,
    'Is the expansionist doctrine a reading of a contested kernel (other readings are equally valid), or a natural law of Islamic theology (objectively mandated by the tradition)?',
    'Presence of identifiable beneficiaries (caliphal authority, legal scholars) who benefit from the expansionist interpretation. Historical evidence of doctrine-shaping by beneficiary institutions. Comparison with how natural laws (e.g., mathematical theorems, logical necessities) cannot be beneficiary-shaped.',
    'If kernel reading: the constraint is tangled_rope at the analytical level and should be reclassified away from mountain. If natural law: the constraint is genuinely immutable from within Islamic jurisprudence. The presence of beneficiaries and the contrastive existence of alternative readings suggest this is a kernel reading, not a natural law.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(false_summit_natural_law_claim, conceptual, 'False summit detection: kernel reading vs natural law claim').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jihad_quranic_corpus__expansionist_legalist_reading, 0, 1400).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jihad_exp_theater_early_caliphate, jihad_quranic_corpus__expansionist_legalist_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(jihad_exp_theater_classical_period, jihad_quranic_corpus__expansionist_legalist_reading, theater_ratio, 200, 0.58).
narrative_ontology:measurement(jihad_exp_theater_late_medieval, jihad_quranic_corpus__expansionist_legalist_reading, theater_ratio, 400, 0.62).

% Extraction over time
narrative_ontology:measurement(jihad_exp_extractiveness_early_caliphate, jihad_quranic_corpus__expansionist_legalist_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(jihad_exp_extractiveness_classical_period, jihad_quranic_corpus__expansionist_legalist_reading, base_extractiveness, 200, 0.68).
narrative_ontology:measurement(jihad_exp_extractiveness_late_medieval, jihad_quranic_corpus__expansionist_legalist_reading, base_extractiveness, 400, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(jihad_exp_suppression_early_caliphate, jihad_quranic_corpus__expansionist_legalist_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(jihad_exp_suppression_classical_peak, jihad_quranic_corpus__expansionist_legalist_reading, suppression_requirement, 200, 0.72).
narrative_ontology:measurement(jihad_exp_suppression_maintenance, jihad_quranic_corpus__expansionist_legalist_reading, suppression_requirement, 400, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jihad_quranic_corpus__expansionist_legalist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(jihad_quranic_corpus__expansionist_legalist_reading, jizya_tax_non_muslim_subjects).
narrative_ontology:affects_constraint(jihad_quranic_corpus__expansionist_legalist_reading, dar_al_islam_territorial_boundary).
narrative_ontology:affects_constraint(jihad_quranic_corpus__expansionist_legalist_reading, imam_authority_military_declaration).

% DUAL FORMULATION NOTE:
% Jihad as expansionist doctrine is one constraint story (this file); the state-level jizya institution and the theo-legal concept of dar al-Islam (Islamic territory) are separate constraint stories upstream that feed into expansion logic. The imam's monopoly on declaration is a third distinct constraint. All three are linked because the expansionist reading depends on the three-part infrastructure: legal authority (imam monopoly), territorial distinction (dar al-Islam), and tributary status (jizya alternatives). Network decomposition: each story gets its own epsilon and perspectives; the shared structure is the reading of the Quranic corpus.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(jihad_quranic_corpus__expansionist_legalist_reading, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
