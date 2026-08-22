% ============================================================================
% CONSTRAINT STORY: correct_latin__hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_correct_latin__hybrid_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: correct_latin__hybrid_reading
 *   human_readable: Correct Latin: Hybrid Continuity with Textual Correction
 *   domain: intellectual_history/philology
 *
 * SUMMARY:
 *   This constraint embodies a hybrid reading of correct Latin: the
 *   legitimacy framework asserts that Medieval Latin, as transmitted through
 *   ecclesiastical institutions, preserves the grammatical core of Classical
 *   Latin through continuous practice, BUT acknowledges that orthography,
 *   vocabulary, and some morphological details can and should be corrected
 *   via evidence from ancient texts. The framework emerged during the
 *   Renaissance as a compromise between pure continuity (medieval practice is
 *   legitimate evolved Latin) and pure reconstruction (medieval practice is
 *   corrupt deviation). This reading enforces a tangled structure: it
 *   validates medieval institutional authority as the custodian of a living
 *   tradition, while simultaneously legitimating textual correction as a tool
 *   to refine that tradition. The payers are those committed to pure textual
 *   reconstruction (who must accept medieval mediation) and those focused on
 *   medieval forms (who must accept external textual revision). The
 *   beneficiaries are institutional preservationists and moderates who gain
 *   legitimacy from both poles.
 *
 * KEY AGENTS:
 *   - Medieval ecclesiastical establishment (agenda-setter; institutional power; trapped exit; civilizational horizon) — preserves and transmits Latin practice through monastic and cathedral schools, claims legitimacy from continuous stewardship
 *   - Continuity preservationists (beneficiary; powerful; mobile; generational) — scholars defending medieval Latin as legitimate evolution, benefit from framework that validates inherited practice without wholesale reconstruction
 *   - Textual purists (payer; powerful; mobile; generational) — scholars committed to reconstructing Classical form from texts, bear cost of having corrections resisted and reconstructions treated as external impositions
 *   - Classical reconstructionists (payer+beneficiary; organized; constrained; biographical) — philologists conducting systematic textual research, constrained by institutional authority of continuity framework but benefiting from legitimacy the hybrid gives their corrections
 *   - Cathedral schools (agenda-setter+payer; institutional; trapped; civilizational) — teaching institutions bearing overhead cost of integrating textual corrections into curriculum while maintaining continuity authority
 *   - Lay vernacular speakers (excluded; powerless; trapped; biographical) — vernacular populations whose evolved speech patterns evidence natural language evolution, structurally excluded from debate over correct form despite providing empirical data
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(correct_latin__hybrid_reading, 0.42).
domain_priors:suppression_score(correct_latin__hybrid_reading, 0.38).
domain_priors:theater_ratio(correct_latin__hybrid_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(correct_latin__hybrid_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(correct_latin__hybrid_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(correct_latin__hybrid_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(correct_latin__hybrid_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(correct_latin__hybrid_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(correct_latin__hybrid_reading, tangled_rope).
narrative_ontology:human_readable(correct_latin__hybrid_reading, "Correct Latin: Hybrid Continuity with Textual Correction").
narrative_ontology:topic_domain(correct_latin__hybrid_reading, "intellectual_history/philology").

domain_priors:requires_active_enforcement(correct_latin__hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(correct_latin__hybrid_reading, 'ecea08a2-11d2-4ab4-8ea7-dd881e81704b').
narrative_ontology:cs_kernel_codification('ecea08a2-11d2-4ab4-8ea7-dd881e81704b', fixed_text).
narrative_ontology:cs_authority_grounding('ecea08a2-11d2-4ab4-8ea7-dd881e81704b', lineage).
narrative_ontology:cs_interpretation_layer_present('ecea08a2-11d2-4ab4-8ea7-dd881e81704b').
narrative_ontology:cs_reading_relation('ecea08a2-11d2-4ab4-8ea7-dd881e81704b', correct_latin__continuity_reading, influences).
narrative_ontology:cs_reading_relation('ecea08a2-11d2-4ab4-8ea7-dd881e81704b', correct_latin__discontinuity_reading, influences).
narrative_ontology:cs_axiom('ecea08a2-11d2-4ab4-8ea7-dd881e81704b', foundational, medieval_practice_preserves_grammatical_core).
narrative_ontology:cs_axiom_status(medieval_practice_preserves_grammatical_core, holdable).
narrative_ontology:cs_axiom_grounding('ecea08a2-11d2-4ab4-8ea7-dd881e81704b', medieval_practice_preserves_grammatical_core, empirically_contingent).
narrative_ontology:cs_axiom('ecea08a2-11d2-4ab4-8ea7-dd881e81704b', foundational, textual_evidence_permits_targeted_correction).
narrative_ontology:cs_axiom_status(textual_evidence_permits_targeted_correction, holdable).
narrative_ontology:cs_axiom_grounding('ecea08a2-11d2-4ab4-8ea7-dd881e81704b', textual_evidence_permits_targeted_correction, empirically_contingent).
narrative_ontology:cs_reference_frame('ecea08a2-11d2-4ab4-8ea7-dd881e81704b', medieval_transmitted_practice_with_textual_authority).
narrative_ontology:cs_drift_state('ecea08a2-11d2-4ab4-8ea7-dd881e81704b', contemporary_philological_era, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('ecea08a2-11d2-4ab4-8ea7-dd881e81704b', '2026-06-12T14:32:18Z').
narrative_ontology:cs_kernel_id(correct_latin__hybrid_reading, correct_latin).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(correct_latin__hybrid_reading, medieval_ecclesiastical_establishment).
narrative_ontology:constraint_beneficiary(correct_latin__hybrid_reading, continuity_preservationists).
narrative_ontology:constraint_victim(correct_latin__hybrid_reading, textual_purists).
narrative_ontology:constraint_victim(correct_latin__hybrid_reading, classical_reconstructionists).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(correct_latin__hybrid_reading, classical_reconstructionists).
narrative_ontology:constraint_victim(correct_latin__hybrid_reading, cathedral_schools).
narrative_ontology:constraint_vindicates(correct_latin__hybrid_reading, living_language_evolution_doctrine).
narrative_ontology:constraint_vindicates(correct_latin__hybrid_reading, textual_authority_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Church authorities, monastic scribes, and cathedral schools that preserved and transmitted Latin through the medieval period. They administered the Latin language as a living practice, training clergy in the forms they inherited, and claim legitimacy from continuous institutional stewardship. They defend medieval Latin forms as evolved legitimate usage, not corruption. Their exit is trapped: to abandon the continuity framework would be to abandon institutional authority over language transmission.
narrative_ontology:constraint_stakeholder(correct_latin__hybrid_reading, medieval_ecclesiastical_establishment, agenda_setter,
    institutional, civilizational, trapped, universal).

% Humanist and modern scholars who argue for the legitimacy of medieval Latin as evolved Classical form. They benefit from a framework that validates continuous practice without requiring wholesale reconstruction. Their institutional positions, pedagogical methods, and professional identity rest on the assumption that medieval practice preserves substantive continuity with Classical form. They can exit to pure reconstruction scholarship, but would lose institutional standing in cathedral schools and ecclesiastical institutions.
narrative_ontology:constraint_stakeholder(correct_latin__hybrid_reading, continuity_preservationists, beneficiary,
    powerful, generational, mobile, universal).

% Scholars committed to reconstructing Classical Latin from ancient texts and treating medieval forms as corruptions to be corrected. They argue that the medieval tradition obscures the true Classical standard and must be amended via textual evidence. They bear the cost of having their textual corrections resisted and their Classical reconstructions treated as external impositions rather than recoveries of legitimate form. They can form independent scholarly communities committed to pure reconstruction, but face institutional isolation from pedagogical institutions.
narrative_ontology:constraint_stakeholder(correct_latin__hybrid_reading, textual_purists, payer,
    powerful, generational, mobile, universal).

% Philologists and classicists pursuing systematic reconstruction of Classical Latin grammar, orthography, and vocabulary from primary sources. They fund and conduct the textual research that produces correction evidence. They are constrained by the institutional authority of the continuity framework and by the labor cost of establishing textual corrections against entrenched practice. Yet they benefit from the legitimacy the hybrid reading gives their corrections (partial authority, not full rejection of medieval practice), which permits their research to influence pedagogy and institutional standards without requiring total rejection of inherited practice.
narrative_ontology:constraint_stakeholder(correct_latin__hybrid_reading, classical_reconstructionists, payer,
    organized, biographical, constrained, universal).
narrative_ontology:stakeholder_secondary_role(correct_latin__hybrid_reading, classical_reconstructionists, beneficiary).

% Teaching institutions that transmit Latin practice to new generations of clergy and scholars. They teach the medieval forms they receive and enforce them as correct usage. They simultaneously bear the cost of integrating corrections from textual evidence, which requires curriculum revision and pedagogical overhead. They benefit from having legitimate grounds to accept some corrections without abandoning the continuity framework — the hybrid reading permits them to incorporate evidence without losing institutional authority over transmitted practice.
narrative_ontology:constraint_stakeholder(correct_latin__hybrid_reading, cathedral_schools, agenda_setter,
    institutional, civilizational, trapped, regional).
narrative_ontology:stakeholder_secondary_role(correct_latin__hybrid_reading, cathedral_schools, payer).

% The physical record of ancient Classical texts and medieval copies that serve as the empirical ground for establishing what Classical forms actually were. The manuscripts themselves have no agency; this entry marks the evidentiary seat that grounds correction claims. Textual purists and reconstructionists read from this seat; continuity preservationists must engage with it even while arguing for its reinterpretation.
narrative_ontology:constraint_stakeholder(correct_latin__hybrid_reading, manuscript_evidence, observer,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(correct_latin__hybrid_reading, manuscript_evidence).

% Populations that spoke Romance languages (proto-Italian, proto-French, proto-Spanish) evolved from Latin but were excluded from the Latin learning tradition and from the debate over correct form. Their actual speech patterns and language evolution were not consulted in determining what counted as correct Latin, despite providing the most direct evidence of how the language naturally evolved from Classical forms.
narrative_ontology:constraint_stakeholder(correct_latin__hybrid_reading, lay_vernacular_speakers, excluded,
    powerless, biographical, trapped, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(correct_latin__hybrid_reading, medieval_ecclesiastical_establishment).
narrative_ontology:fixing_cost_class(correct_latin__hybrid_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a unified standard for written Latin that permits both continuity with medieval practice and correction via textual authority, allowing scholarly communication across regions and generations without requiring rejection of inherited form.
% TRANSFER_FUNCTION: Moves epistemic and institutional authority from purely textual sources (ancient manuscripts) and from pure reconstruction (Classical form isolated from practice) toward a hybrid that legitimates medieval institutional practice while accepting systematic correction. Textual purists accept medieval mediation of Classical form; continuity preservationists accept textual correction of medieval form.
% ABSENT_VOICES: Lay vernacular speakers whose evolved Romance speech patterns evidence how Latin naturally transformed are entirely excluded from the debate over correct form. Scribal and monastic communities whose actual practices diverged from both medieval standard and Classical form are similarly unheard; the constraint operates at the level of ecclesiastical authority and scholarly reconstruction, not actual usage.
% DISAPPEARANCE_RATIONALE: If the hybrid reading and its enforcement framework vanished, scholars would polarize into pure continuity factions (medieval forms as fully legitimate evolved Latin) and pure discontinuity factions (Classical forms recovered from texts, medieval practice abandoned as corrupt). The institutions transmitting Latin would lose the framework that permits simultaneously honoring medieval inheritance and accepting textual correction; curriculum and standards would reorganize around one pole or the other.
% FOUNDING_PROBLEM: During the Renaissance and Early Modern period, scholars studying ancient texts discovered discrepancies between Classical Latin as evidenced in primary sources and the Latin forms transmitted through medieval monastic and ecclesiastical practice. Neither pure continuity nor pure reconstruction could satisfy both the demand to honor institutional inheritance and the evidence of textual sources.
% FOUNDING_PROBLEM_CORROBORATION: Textual evidence from ancient manuscripts, paleographic analysis of medieval copies, and comparative analysis of form variation across sources all confirm the structural divergence between attested Classical forms and medieval transmitted forms. Scholars in Renaissance philology (Valla, Erasmus) and modern Latin philology (external to the ecclesiastical establishment) documented the divergence from comparative textual grounds. The founding problem persists in contemporary Latin pedagogy and philological practice.
narrative_ontology:disappearance_verdict(correct_latin__hybrid_reading, world_rearranges).
narrative_ontology:founding_problem_status(correct_latin__hybrid_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(correct_latin__hybrid_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(correct_latin__hybrid_reading, 'none', 1).
narrative_ontology:epsilon_provenance(correct_latin__hybrid_reading, 0.42, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(correct_latin__hybrid_reading_tests).
:- end_tests(correct_latin__hybrid_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.42 at interval end) because the constraint operates as genuine coordination (unified standard for written communication) that permits both continuity and correction. However, it is not pure coordination: textual purists must accept medieval mediation, and continuity preservationists must accept textual subordination of their inherited forms — both are partial targets. Suppression is moderate (0.38) because the constraint requires active institutional enforcement of the hybrid framework against pressure from pure-continuity and pure-reconstruction factions. Theater ratio rises over time (0.12 to 0.28) because as textual evidence accumulates and reconstructionist scholarship expands, increasing institutional effort goes to defending the legitimacy of medieval forms against textual corrections, not to the original coordination function. Accessibility collapse is moderate (0.62) because alternatives (pure continuity, pure reconstruction) remain intellectually and institutionally available — the hybrid reading is enforced through authority structures, not through unavoidable logic. Resistance is substantial (0.58) because textual evidence continuously generates pressure to move toward pure reconstruction, and medieval practitioners resist external textual correction. The measurement series tracks how extractiveness and theater rise together from 0 to 30 (as textual scholarship produces more corrections), then plateau slightly (as institutional accommodation reaches equilibrium). The suppression requirement curve mirrors extractiveness, indicating that maintaining the hybrid framework requires escalating institutional effort as alternative framings accumulate evidence.
 *
 * PERSPECTIVAL GAP:
 *   From the ecclesiastical establishment's seat, the constraint is a working compromise: medieval practice is vindicated as legitimate, while textual corrections permit intellectual engagement with evidence. From the textual purist's seat, the same structure is enforced extraction: their reconstructive work is accepted piecemeal (where it serves the continuity narrative) but their comprehensive alternative (pure classical reconstruction) is systematically resisted by institutional authority. Cathedral schools navigate an impossible middle: they must teach medieval forms as authoritative (institutional mandate) while incorporating corrections that students encounter in textual study (intellectual integrity). The divergence is not in what the constraint does — everyone agrees it permits correction within a continuity frame — but in whether that frame is legitimate authority (establishment view) or enforced limitation (purist view).
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary set (ecclesiastical establishment + continuity preservationists) benefits from a framework that validates their inherited practice and institutional position while permitting selective engagement with textual evidence — they have strong institutional power (ecclesiastical establishment is trapped, so dependent on continuity framework; continuity preservationists have mobile power but benefit from framework's legitimacy). The victim set (textual purists, classical reconstructionists) bears the cost of having their reconstruction work constrained by continuity doctrine — their exit options are mobile/constrained (they can adopt pure reconstruction, but at cost of intellectual isolation from ecclesiastical institutions and cathedral schools that control Latin pedagogy). The directionality derivation places beneficiaries at low d (they collect authority without running the risk of pure reconstruction) and victims at higher d (their work is targeted by the constraint's suppression of pure reconstruction). Cathedral schools sit between as secondary victims (they bear the overhead of integration) and secondary agenda-setters (they enforce the hybrid in pedagogy).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — reconciling medieval institutional transmission with Classical textual evidence — is LIVE. The constraint explicitly addresses it through the hybrid framework that permits both continuity and correction. However, as textual scholarship accumulates, pressure builds toward pure reconstruction. The theater ratio rising from 0.12 to 0.28 signals that increasing institutional effort goes to defending medieval form legitimacy against textual evidence, not to the original coordination function. This is the signature of mandatrophy beginning: institutional energy that once went to meaningful coordination (unified standard for communication) now goes to defending legitimacy boundaries against evidence. The constraint is not yet a zombie (it still coordinates unified standards), but the measurement trajectory suggests approaching the phase where preservation of medieval institutional authority becomes a larger fraction of the constraint's function than the original coordination. If textual evidence continues to accumulate and pure-reconstruction scholarship grows, the constraint faces two paths: (1) reformulate the hybrid framework to give greater weight to textual authority (moving d_victims downward), or (2) escalate suppression of reconstruction scholarship (increasing theater and suppression further), deepening the mandatrophy signal.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_authority_boundary,
    'What is the precise boundary between legitimate textual correction (permitted by the hybrid framework) and illegitimate full reconstruction (resisted as imposing external standards on medieval practice)?',
    'Examination of historical cases where textual corrections were accepted or rejected, and the stated justifications for each decision. Pattern analysis of what types of corrections (orthography, vocabulary, morphology, syntax) were admitted versus resisted.',
    'If corrections are admitted selectively based on institutional preference rather than textual evidence, the hybrid reading collapses into pure continuity (medieval authority over all). If corrections are admitted across the board based on textual evidence, the hybrid reading collapses into pure discontinuity (textual authority over all). The empirical boundary determines whether the constraint is genuinely hybrid or whether the ''textual correction'' mechanism is performative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_authority_boundary, empirical, 'Whether the textual correction mechanism is a genuine alternative authority or a bounded legitimation of selective changes').

omega_variable(
    reading_institutional_pressure,
    'Does the rising theater ratio (0.12 to 0.28 over the interval) represent genuine institutional adaptation to accumulating evidence, or does it represent escalating defensive suppression of textual reconstruction scholarship?',
    'Longitudinal analysis of institutional positioning of textual reconstructionists: if they gain professional standing and resources as evidence accumulates, institutional adaptation is occurring; if they face increasing resistance despite evidence, suppression is escalating.',
    'If adaptation: the hybrid reading remains genuinely hybrid and may shift toward higher textual authority over time. If suppression: the constraint is developing mandatrophy — institutional preservation of medieval authority overrides engagement with evidence — and the victim set (textual purists) experiences increasing extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_institutional_pressure, empirical, 'Whether rising theater ratio indicates adaptation to evidence or escalating institutional resistance').

omega_variable(
    kernel_identity_ambiguity,
    'Is the contested kernel fundamentally ''What counts as correct Latin form?'' or is it ''Who decides what counts as correct Latin form (institutional custodians vs. textual evidence)?''',
    'Examination of how disputants frame the disagreement: do they argue about WHAT form is correct (factual claim about ancient texts), or about WHO DECIDES what counts as correct (authority claim about institutional vs. textual jurisdiction)?',
    'If the kernel is substantive form, the three readings are alternative factual claims about Classical language. If the kernel is authority, the three readings are alternative jurisdictions for decision-making. The reading relations change: if substantive, readings can foreclose each other (one has the facts right); if authority-based, readings coexist (different parties control different institutions). This affects whether the hybrid reading is stable or transitional.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_identity_ambiguity, conceptual, 'Whether the kernel contest is about form or about authority').

omega_variable(
    textual_evidence_comprehensiveness,
    'How comprehensive is the textual evidence base for Classical Latin forms? Are there forms where textual sources diverge or are incomplete, forcing reliance on internal reconstruction or institutional practice?',
    'Survey of Latin grammar, orthography, and vocabulary as attested in the corpus of surviving texts versus areas where textual evidence is sparse or contradictory. Assessment of how much of the form system is directly attested versus interpolated.',
    'Where textual evidence is comprehensive and consistent, pure reconstruction is viable. Where textual evidence is sparse or contradictory, the hybrid framework''s reliance on institutional practice for filling gaps is structural necessity rather than compromise. This affects whether textual correction is a dominant mechanism or a bounded supplement.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(textual_evidence_comprehensiveness, empirical, 'The comprehensiveness and consistency of textual evidence for Classical Latin forms').

omega_variable(
    reading_incompatibility_structure,
    'Do the three readings (continuity, discontinuity, hybrid) genuinely represent three distinct logical positions, or are discontinuity and hybrid variants of a shared ''textual evidence matters'' frame that coexist against a pure-continuity alternative?',
    'Logical analysis of the axioms: do hybrid and discontinuity readings share foundational premises about textual authority, differing only on the weight given to medieval practice? If so, they coexist-with continuity but may foreclose each other.',
    'If the three are truly independent: reading relations are coexists_with all. If discontinuity and hybrid share textual-authority axioms: the relation between them is influences or forecloses (depending on whether they can coexist within a single institutional framework). This affects the stability and dominance of the hybrid reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_incompatibility_structure, conceptual, 'Whether the three kernel readings are logically independent or partially overlapping').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(correct_latin__hybrid_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(corr_tr_t0, correct_latin__hybrid_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(corr_tr_t0, observed).
narrative_ontology:measurement(corr_tr_t10, correct_latin__hybrid_reading, theater_ratio, 10, 0.15).
narrative_ontology:measurement_basis(corr_tr_t10, observed).
narrative_ontology:measurement(corr_tr_t20, correct_latin__hybrid_reading, theater_ratio, 20, 0.19).
narrative_ontology:measurement_basis(corr_tr_t20, observed).
narrative_ontology:measurement(corr_tr_t30, correct_latin__hybrid_reading, theater_ratio, 30, 0.24).
narrative_ontology:measurement_basis(corr_tr_t30, observed).
narrative_ontology:measurement(corr_tr_t45, correct_latin__hybrid_reading, theater_ratio, 45, 0.27).
narrative_ontology:measurement_basis(corr_tr_t45, observed).
narrative_ontology:measurement(corr_tr_t60, correct_latin__hybrid_reading, theater_ratio, 60, 0.28).
narrative_ontology:measurement_basis(corr_tr_t60, observed).

% Extraction over time
narrative_ontology:measurement(corr_be_t0, correct_latin__hybrid_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement_basis(corr_be_t0, observed).
narrative_ontology:measurement(corr_be_t10, correct_latin__hybrid_reading, base_extractiveness, 10, 0.32).
narrative_ontology:measurement_basis(corr_be_t10, observed).
narrative_ontology:measurement(corr_be_t20, correct_latin__hybrid_reading, base_extractiveness, 20, 0.38).
narrative_ontology:measurement_basis(corr_be_t20, observed).
narrative_ontology:measurement(corr_be_t30, correct_latin__hybrid_reading, base_extractiveness, 30, 0.42).
narrative_ontology:measurement_basis(corr_be_t30, observed).
narrative_ontology:measurement(corr_be_t45, correct_latin__hybrid_reading, base_extractiveness, 45, 0.41).
narrative_ontology:measurement_basis(corr_be_t45, observed).
narrative_ontology:measurement(corr_be_t60, correct_latin__hybrid_reading, base_extractiveness, 60, 0.42).
narrative_ontology:measurement_basis(corr_be_t60, observed).

% Suppression requirement over time
narrative_ontology:measurement(corr_su_t0, correct_latin__hybrid_reading, suppression_requirement, 0, 0.22).
narrative_ontology:measurement_basis(corr_su_t0, observed).
narrative_ontology:measurement(corr_su_t10, correct_latin__hybrid_reading, suppression_requirement, 10, 0.26).
narrative_ontology:measurement_basis(corr_su_t10, observed).
narrative_ontology:measurement(corr_su_t20, correct_latin__hybrid_reading, suppression_requirement, 20, 0.31).
narrative_ontology:measurement_basis(corr_su_t20, observed).
narrative_ontology:measurement(corr_su_t30, correct_latin__hybrid_reading, suppression_requirement, 30, 0.36).
narrative_ontology:measurement_basis(corr_su_t30, observed).
narrative_ontology:measurement(corr_su_t45, correct_latin__hybrid_reading, suppression_requirement, 45, 0.37).
narrative_ontology:measurement_basis(corr_su_t45, observed).
narrative_ontology:measurement(corr_su_t60, correct_latin__hybrid_reading, suppression_requirement, 60, 0.38).
narrative_ontology:measurement_basis(corr_su_t60, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(correct_latin__hybrid_reading, information_standard).
narrative_ontology:boltzmann_floor_override(correct_latin__hybrid_reading, 0.12).
narrative_ontology:affects_constraint(correct_latin__hybrid_reading, correct_latin__continuity_reading).
narrative_ontology:affects_constraint(correct_latin__hybrid_reading, correct_latin__discontinuity_reading).

% DUAL FORMULATION NOTE:
% The 'correct_latin' kernel decomposes into three constraint stories, one per reading of the kernel. Each reading instantiates a different constraint with different beneficiary/victim structures, ε values, and extraction profiles. The hybrid_reading is the middle position, accepting both medieval institutional authority (beneficiaries: ecclesiastical establishment, continuity preservationists) and textual correction authority (victims: textual purists, classical reconstructionists). The continuity_reading privileges institutional transmission exclusively; the discontinuity_reading privileges textual reconstruction exclusively. These three constraints are linked via network.affects_constraints because each reading's institutional success affects the resource availability and legitimacy conditions for the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(correct_latin__hybrid_reading, institutional, 0.18).
constraint_indexing:directionality_override(correct_latin__hybrid_reading, powerful, 0.68).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
