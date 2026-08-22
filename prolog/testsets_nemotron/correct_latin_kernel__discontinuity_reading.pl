% ============================================================================
% CONSTRAINT STORY: correct_latin_kernel__discontinuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_correct_latin_kernel__discontinuity_reading, []).

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
 *   constraint_id: correct_latin_kernel__discontinuity_reading
 *   human_readable: Classical Latin Correctness as Symbolic Reoccupation (Discontinuity Reading)
 *   domain: historical_linguistics/philology/intellectual_history
 *
 * SUMMARY:
 *   The discontinuity reading of the correct_latin_kernel treats Classical
 *   Latin and Medieval Latin as distinct systems separated by a rupture.
 *   Reconstruction is not correction of a living tradition but symbolic
 *   reoccupation: humanist scholars recovered Latin from texts, treating
 *   medieval forms as corruptions to be purged. This reading instantiated a
 *   constraint that coordinated early modern Latinity around classical
 *   textual authority while extracting epistemic authority from medieval
 *   scribal traditions and vernacular users. The constraint operated as a
 *   tangled rope: genuine coordination (a shared learned language across
 *   Europe) fused with asymmetric extraction (humanist scholars and
 *   institutions gained authority by defining correctness against the
 *   medieval past).
 *
 * KEY AGENTS:
 *   - renaissance_humanist_scholars: Primary beneficiary (institutional/arbitrage) — defined correctness, gained epistemic authority
 *   - early_modern_university_institutions: Beneficiary (institutional/generational) — institutionalized the standard, controlled certification
 *   - classical_philology_discipline: Beneficiary (organized/generational) — emerged from this constraint, inherits its authority structure
 *   - medieval_latin_scribal_traditions: Primary victim (organized/trapped) — excluded from correctness, labeled corrupt
 *   - vernacular_latin_users: Victim (powerless/trapped) — their living Latin deemed incorrect, no voice in standard-setting
 *   - non_classical_textual_traditions: Victim (organized/constrained) — legal, medical, ecclesiastical Latin traditions marginalized
 *   - continuity_reading_adherents: Excluded (moderate/constrained) — competing reading of the same kernel, marginalized in early modern period
 *   - historical_linguist_observer: Observer (analytical/analytical) — sees the kernel's structural contestation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(correct_latin_kernel__discontinuity_reading, 0.38).
domain_priors:suppression_score(correct_latin_kernel__discontinuity_reading, 0.52).
domain_priors:theater_ratio(correct_latin_kernel__discontinuity_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(correct_latin_kernel__discontinuity_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(correct_latin_kernel__discontinuity_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(correct_latin_kernel__discontinuity_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(correct_latin_kernel__discontinuity_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(correct_latin_kernel__discontinuity_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(correct_latin_kernel__discontinuity_reading, tangled_rope).
narrative_ontology:human_readable(correct_latin_kernel__discontinuity_reading, "Classical Latin Correctness as Symbolic Reoccupation (Discontinuity Reading)").
narrative_ontology:topic_domain(correct_latin_kernel__discontinuity_reading, "historical_linguistics/philology/intellectual_history").

domain_priors:requires_active_enforcement(correct_latin_kernel__discontinuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(correct_latin_kernel__discontinuity_reading, '25dbc352-41f1-4f28-844d-4e30f9b73bc0').
narrative_ontology:cs_kernel_codification('25dbc352-41f1-4f28-844d-4e30f9b73bc0', fixed_text).
narrative_ontology:cs_authority_grounding('25dbc352-41f1-4f28-844d-4e30f9b73bc0', lineage).
narrative_ontology:cs_interpretation_layer_present('25dbc352-41f1-4f28-844d-4e30f9b73bc0').
narrative_ontology:cs_reading_relation('25dbc352-41f1-4f28-844d-4e30f9b73bc0', correct_latin_kernel__continuity_reading, forecloses).
narrative_ontology:cs_reading_relation('25dbc352-41f1-4f28-844d-4e30f9b73bc0', correct_latin_kernel__hybrid_reading, coexists_with).
narrative_ontology:cs_axiom('25dbc352-41f1-4f28-844d-4e30f9b73bc0', foundational, classical_latin_as_discrete_system).
narrative_ontology:cs_axiom_status(classical_latin_as_discrete_system, holdable).
narrative_ontology:cs_axiom_grounding('25dbc352-41f1-4f28-844d-4e30f9b73bc0', classical_latin_as_discrete_system, empirically_contingent).
narrative_ontology:cs_axiom('25dbc352-41f1-4f28-844d-4e30f9b73bc0', foundational, textual_recovery_as_sole_legitimate_method).
narrative_ontology:cs_axiom_status(textual_recovery_as_sole_legitimate_method, holdable).
narrative_ontology:cs_axiom_grounding('25dbc352-41f1-4f28-844d-4e30f9b73bc0', textual_recovery_as_sole_legitimate_method, conventional).
narrative_ontology:cs_axiom('25dbc352-41f1-4f28-844d-4e30f9b73bc0', secondary, medieval_forms_as_corruption_not_evolution).
narrative_ontology:cs_axiom_status(medieval_forms_as_corruption_not_evolution, holdable).
narrative_ontology:cs_axiom_grounding('25dbc352-41f1-4f28-844d-4e30f9b73bc0', medieval_forms_as_corruption_not_evolution, empirically_contingent).
narrative_ontology:cs_reference_frame('25dbc352-41f1-4f28-844d-4e30f9b73bc0', classical_textual_canon_as_latin_authority).
narrative_ontology:cs_drift_state('25dbc352-41f1-4f28-844d-4e30f9b73bc0', early_modern_institutionalization_complete, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('25dbc352-41f1-4f28-844d-4e30f9b73bc0', '').
narrative_ontology:cs_kernel_id(correct_latin_kernel__discontinuity_reading, correct_latin_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(correct_latin_kernel__discontinuity_reading, renaissance_humanist_scholars).
narrative_ontology:constraint_beneficiary(correct_latin_kernel__discontinuity_reading, early_modern_university_institutions).
narrative_ontology:constraint_beneficiary(correct_latin_kernel__discontinuity_reading, classical_philology_discipline).
narrative_ontology:constraint_victim(correct_latin_kernel__discontinuity_reading, medieval_latin_scribal_traditions).
narrative_ontology:constraint_victim(correct_latin_kernel__discontinuity_reading, vernacular_latin_users).
narrative_ontology:constraint_victim(correct_latin_kernel__discontinuity_reading, non_classical_textual_traditions).
narrative_ontology:constraint_vindicates(correct_latin_kernel__discontinuity_reading, classical_latin_as_superior_standard).
narrative_ontology:constraint_vindicates(correct_latin_kernel__discontinuity_reading, textual_recovery_as_epistemic_method).
narrative_ontology:constraint_vindicates(correct_latin_kernel__discontinuity_reading, historical_discontinuity_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Defined the classical standard through textual criticism and editorial work. Controlled the canon of 'correct' authors. Gained professional recognition, patronage, and intellectual authority by positioning themselves as recoverers of true Latin. Could move between courts, universities, and print shops — their skills were portable across the Republic of Letters.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__discontinuity_reading, renaissance_humanist_scholars, agenda_setter,
    institutional, generational, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(correct_latin_kernel__discontinuity_reading, renaissance_humanist_scholars, beneficiary).

% Institutionalized the humanist Latin standard in curricula, degree requirements, and faculty appointments. Controlled certification of Latin competence. Benefited from a stable, teachable standard that justified their gatekeeping role. Could adopt the standard without bearing the cost of its enforcement — the humanist scholars did the textual work; universities reaped the institutional legitimacy.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__discontinuity_reading, early_modern_university_institutions, beneficiary,
    institutional, generational, arbitrage, continental).

% Emerged from the humanist textual program and inherited its authority structure. The discipline's professional identity is fused with the discontinuity reading's method: textual recovery as the path to correctness. Exit would mean abandoning the field's founding myth. Their authority derives from maintaining the classical/medieval boundary the discontinuity reading instituted.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__discontinuity_reading, classical_philology_discipline, beneficiary,
    organized, generational, identity_locked, global).

% Maintained living Latin traditions in chanceries, monasteries, and universities for centuries. Their forms were declared 'corrupt' by humanist scholars who had institutional backing. Could not exit the constraint — their practice was the target of the correction. Some adapted by adopting humanist forms; others persisted in documentary Latin but lost scholarly authority. Their texts were mined only as negative evidence.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__discontinuity_reading, medieval_latin_scribal_traditions, payer,
    organized, generational, trapped, continental).

% Used Latin as a practical communication tool in trade, local administration, and popular religion. Their Latin was never consulted in the humanist standard-setting. Had no institutional voice, no textual archive to defend them. The constraint extracted their communicative practice by declaring it incorrect, offering no path to participation in the new standard. Some shifted to vernaculars; others persisted in 'incorrect' Latin.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__discontinuity_reading, vernacular_latin_users, payer,
    powerless, biographical, trapped, regional).

% Legal Latin, medical Latin, ecclesiastical Latin — specialized traditions with their own textual authorities. The humanist standard marginalized them by claiming universal correctness. Some (ecclesiastical) retained institutional protection; others (legal, medical) were gradually displaced by vernaculars or humanist Latin. Their exit was constrained: they could persist in narrow domains but lost the general learned language.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__discontinuity_reading, non_classical_textual_traditions, payer,
    organized, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(correct_latin_kernel__discontinuity_reading, non_classical_textual_traditions, excluded).

% Scholars who maintained that Medieval Latin was the natural evolution of Classical Latin and that reconstruction meant correcting a living tradition, not reoccuping a dead one. Their reading was structurally displaced in the early modern period — the humanist program controlled the presses, universities, and patronage. They persisted in minority positions (some Jesuit scholars, some vernacular humanists) but were excluded from the dominant correctness standard.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__discontinuity_reading, continuity_reading_adherents, excluded,
    moderate, biographical, constrained, continental).

% Analyzes the kernel's structural contestation from outside the early modern power structure. Sees three readings of the same kernel producing different constraints with different extraction profiles. Has no stake in which reading 'wins' but observes that the discontinuity reading's victory was institutional, not purely empirical.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__discontinuity_reading, historical_linguist_observer, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provided a stable, transnational learned language for early modern scholarship by anchoring Latin in a fixed classical textual canon rather than living medieval practice. Solved the coordination problem of communicative fracture across regions and generations.
% TRANSFER_FUNCTION: Moves epistemic authority and institutional legitimacy from medieval scribal traditions and vernacular users to humanist scholars and early modern universities. The transfer vehicle is the 'corruption' label: by declaring medieval forms corrupt, the discontinuity reading authorizes the humanist scholar as the necessary mediator of correct Latin.
% ABSENT_VOICES: Medieval scribes and vernacular Latin users were structurally absent from the humanist standard-setting. Their practices were the object of correction, not participants in it. The continuity reading's adherents were excluded from the dominant institutional channels (presses, universities, patronage). The kernel's own structural contestation — that 'correct Latin' admits multiple coherent readings — was suppressed by the discontinuity reading's institutional victory.
% DISAPPEARANCE_RATIONALE: If the discontinuity reading's constraint vanished overnight, the classical/medieval boundary would dissolve. Medieval Latin traditions would regain scholarly legitimacy. The humanist textual program's authority would collapse. Classical philology would lose its founding method. The Republic of Letters' linguistic unity would fracture into regional Latinities. The early modern university's Latin certification would become incoherent.
% FOUNDING_PROBLEM: Early modern scholarship needed a stable, transnational learned language. Medieval Latin had diverged regionally; no single living tradition could serve as a universal standard. Classical texts offered a fixed, authoritative anchor.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem (need for a stable transnational learned Latin) is attested as dead by the historical record: by 1650, humanist Latin was the established learned language across Europe. The discontinuity reading's own beneficiaries (humanist scholars, universities) declared victory. The continuity reading's adherents and later historical linguists corroborate that the coordination problem was solved — but note the constraint persisted and intensified after the problem was solved, which is the mandatrophy signal. No source outside the beneficiary set attests that the problem remained live after 1650.
narrative_ontology:disappearance_verdict(correct_latin_kernel__discontinuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(correct_latin_kernel__discontinuity_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(correct_latin_kernel__discontinuity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(correct_latin_kernel__discontinuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(correct_latin_kernel__discontinuity_reading, 0.38, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(correct_latin_kernel__discontinuity_reading_tests).
:- end_tests(correct_latin_kernel__discontinuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The discontinuity reading claims to solve a coordination problem: providing a stable, transnational learned language for early modern scholarship. Its coordination function is real — humanist Latin did enable cross-European communication. But the same structure extracts authority from medieval traditions by declaring them 'corrupt' rather than evolved. The suppression (0.52) reflects active enforcement: textual criticism, educational curricula, and scholarly gatekeeping excluded medieval forms. Theater ratio (0.25) is moderate — the philological work was genuine, but a growing share of activity performed correctness rather than recovering lost knowledge. Accessibility collapse (0.65) is significant: once the classical standard was institutionalized, alternatives became scholastically invisible. Resistance (0.45) reflects the persistence of medieval Latin in legal, ecclesiastical, and vernacular contexts despite the humanist program.
 *
 * PERSPECTIVAL GAP:
 *   From the humanist scholar seat (beneficiary/agenda_setter), the constraint is coordination: they built a working Latin from texts. From the medieval scribe seat (victim/trapped), it is extraction: their living tradition was declared corrupt by outsiders with institutional power. From the vernacular user seat (victim/powerless), it is suppression: their Latin was never consulted. The continuity reading adherents (excluded) experience it as foreclosure: their reading of the same kernel was structurally displaced. The engine computes these divergences from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Renaissance humanist scholars are structural beneficiaries: they defined the standard, controlled the texts, and gained professional authority (d ~ 0.15). Early modern universities are beneficiaries: they institutionalized the standard, controlled degrees and appointments (d ~ 0.2). Classical philology inherits this beneficiary position (d ~ 0.25). Medieval scribal traditions are victims: their living practice was excluded from correctness, their texts used only as negative evidence (d ~ 0.85). Vernacular users are trapped victims: no exit from the standard, no voice in its formation (d ~ 0.9). Non-classical traditions are constrained victims: they persisted in specialized domains but lost general authority (d ~ 0.75). Continuity reading adherents are excluded: their structural position is epistemic displacement (d ~ 0.6). The analytical observer sits at d = 0.5.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — providing a stable learned Latin for transnational scholarship — was live in 1350-1550. By 1650-1750, the problem was substantially solved (Latin was the established learned language), but the constraint persisted and intensified (rising extractiveness, peaking suppression). The constraint's mandate atrophied: the coordination function was achieved, but the extraction machinery (textual gatekeeping, correctness enforcement) continued and expanded. This is a classic mandatrophy pattern — a scaffold that lost its sunset clause and became a tangled rope, then a piton. The discontinuity reading's authority structure (textual recovery as method) became self-justifying.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_committer_structure,
    'How does the discontinuity reading''s structural relationship to the correct_latin_kernel differ from the continuity_reading and hybrid_reading?',
    'Map the three readings'' beneficiary/victim structures and extraction profiles; identify which structural elements (victim sets, enforcement mechanisms, epistemic authority claims) shift across readings of the same kernel.',
    'If the three readings produce materially different constraint classifications from the same kernel, the kernel itself is a site of structural contestation, not a stable referent. This would validate the committer-frame decomposition.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'Commitment-kernel decomposition: three readings of correct_latin_kernel with distinct structural profiles').

omega_variable(
    medieval_corruption_vs_evolution_ambiguity,
    'Does treating Medieval Latin forms as ''corruptions'' reflect an empirical linguistic judgment or a normative epistemic stance that serves the discontinuity reading''s authority structure?',
    'Compare the discontinuity reading''s suppression mechanisms (which textual traditions were excluded, which scholars were marginalized) against the continuity reading''s account of natural linguistic change. Assess whether the ''corruption'' label tracks measurable linguistic degradation or institutional exclusion.',
    'If ''corruption'' is a normative label masking institutional suppression, the constraint''s extraction is higher than its coordination function warrants — pushing toward snare. If it tracks genuine linguistic opacity, the coordination function is stronger — supporting tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(medieval_corruption_vs_evolution_ambiguity, conceptual, 'Whether the discontinuity reading''s core evaluative category (''corruption'') is empirical or normative').

omega_variable(
    symbolic_reoccupation_necessity,
    'Was symbolic reoccupation from texts genuinely necessary for Latin''s revival as a learned language, or did it serve to legitimize a specific scholarly class''s authority over linguistic correctness?',
    'Examine whether vernacular Latin users and medieval scribal traditions maintained functional Latin communication without classical textual recovery. Test the counterfactual: would Latin have remained a usable learned language without the humanist textual program?',
    'If reoccupation was functionally necessary, the constraint''s coordination function is genuine. If it was primarily authority-legitimating, the extraction component dominates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(symbolic_reoccupation_necessity, empirical, 'Whether the discontinuity reading''s claimed coordination function (recovery via texts) was functionally necessary or authority-serving').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(correct_latin_kernel__discontinuity_reading, 1350, 1850).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(correct_latin_kernel__discontinuity_reading_tr_t1350, correct_latin_kernel__discontinuity_reading, theater_ratio, 1350, 0.15).
narrative_ontology:measurement(correct_latin_kernel__discontinuity_reading_tr_t1450, correct_latin_kernel__discontinuity_reading, theater_ratio, 1450, 0.2).
narrative_ontology:measurement(correct_latin_kernel__discontinuity_reading_tr_t1550, correct_latin_kernel__discontinuity_reading, theater_ratio, 1550, 0.25).
narrative_ontology:measurement(correct_latin_kernel__discontinuity_reading_tr_t1650, correct_latin_kernel__discontinuity_reading, theater_ratio, 1650, 0.3).
narrative_ontology:measurement(correct_latin_kernel__discontinuity_reading_tr_t1750, correct_latin_kernel__discontinuity_reading, theater_ratio, 1750, 0.35).
narrative_ontology:measurement(correct_latin_kernel__discontinuity_reading_tr_t1850, correct_latin_kernel__discontinuity_reading, theater_ratio, 1850, 0.25).

% Extraction over time
narrative_ontology:measurement(correct_latin_kernel__discontinuity_reading_be_t1350, correct_latin_kernel__discontinuity_reading, base_extractiveness, 1350, 0.25).
narrative_ontology:measurement(correct_latin_kernel__discontinuity_reading_be_t1450, correct_latin_kernel__discontinuity_reading, base_extractiveness, 1450, 0.32).
narrative_ontology:measurement(correct_latin_kernel__discontinuity_reading_be_t1550, correct_latin_kernel__discontinuity_reading, base_extractiveness, 1550, 0.38).
narrative_ontology:measurement(correct_latin_kernel__discontinuity_reading_be_t1650, correct_latin_kernel__discontinuity_reading, base_extractiveness, 1650, 0.42).
narrative_ontology:measurement(correct_latin_kernel__discontinuity_reading_be_t1750, correct_latin_kernel__discontinuity_reading, base_extractiveness, 1750, 0.45).
narrative_ontology:measurement(correct_latin_kernel__discontinuity_reading_be_t1850, correct_latin_kernel__discontinuity_reading, base_extractiveness, 1850, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(correct_latin_kernel__discontinuity_reading_su_t1350, correct_latin_kernel__discontinuity_reading, suppression_requirement, 1350, 0.3).
narrative_ontology:measurement(correct_latin_kernel__discontinuity_reading_su_t1450, correct_latin_kernel__discontinuity_reading, suppression_requirement, 1450, 0.42).
narrative_ontology:measurement(correct_latin_kernel__discontinuity_reading_su_t1550, correct_latin_kernel__discontinuity_reading, suppression_requirement, 1550, 0.52).
narrative_ontology:measurement(correct_latin_kernel__discontinuity_reading_su_t1650, correct_latin_kernel__discontinuity_reading, suppression_requirement, 1650, 0.58).
narrative_ontology:measurement(correct_latin_kernel__discontinuity_reading_su_t1750, correct_latin_kernel__discontinuity_reading, suppression_requirement, 1750, 0.55).
narrative_ontology:measurement(correct_latin_kernel__discontinuity_reading_su_t1850, correct_latin_kernel__discontinuity_reading, suppression_requirement, 1850, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(correct_latin_kernel__discontinuity_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(correct_latin_kernel__discontinuity_reading, 0.08).
narrative_ontology:affects_constraint(correct_latin_kernel__discontinuity_reading, correct_latin_kernel__continuity_reading).
narrative_ontology:affects_constraint(correct_latin_kernel__discontinuity_reading, correct_latin_kernel__hybrid_reading).
narrative_ontology:affects_constraint(correct_latin_kernel__discontinuity_reading, humanist_latin_education_system).
narrative_ontology:affects_constraint(correct_latin_kernel__discontinuity_reading, early_modern_scholarly_republic_of_letters).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the correct_latin_kernel. The continuity_reading (low extraction, mountain-like) and hybrid_reading (moderate extraction, rope-like) share the kernel but instantiate different constraints with different beneficiary/victim structures and ε values. The discontinuity reading has the highest extraction (0.38) and suppression (0.52) because it requires active enforcement against living medieval traditions. The three stories form a constraint family linked by network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(correct_latin_kernel__discontinuity_reading, organized, 0.85).
constraint_indexing:directionality_override(correct_latin_kernel__discontinuity_reading, powerless, 0.9).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
