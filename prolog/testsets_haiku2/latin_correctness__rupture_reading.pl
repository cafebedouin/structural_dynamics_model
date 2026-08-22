% ============================================================================
% CONSTRAINT STORY: latin_correctness__rupture_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_latin_correctness__rupture_reading, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: latin_correctness__rupture_reading
 *   human_readable: Classical Latin Purism as Fixed Standard (Rupture Reading)
 *   domain: intellectual_history/philology/cultural_authority
 *
 * SUMMARY:
 *   The rupture reading declares classical Latin to be a fixed textual
 *   standard requiring reconstruction from ancient sources, and medieval
 *   usage to be corruption. This reading emerged during the humanist recovery
 *   of antiquity and has dominated academic and intellectual institutions
 *   since the Renaissance. The constraint operates by establishing
 *   gatekeeping authority over what counts as 'correct' Latin, delegitimizing
 *   medieval scholars' own linguistic practices, and creating penalties for
 *   technical and vernacular domains that cannot conform to classical purity.
 *   The structural asymmetry is stark: classical philologists and humanist
 *   elites benefit from monopolizing the prestige economy; medieval scholars
 *   and technical practitioners bear costs through epistemic subordination
 *   and functional hobbling. This story instantiates ONE reading of the
 *   contested kernel 'latin_correctness'—the sibling readings (continuity and
 *   hybrid) would produce different constraints with different victim sets
 *   and different ε values. This story measures ONLY the rupture reading's
 *   structural dynamics.
 *
 * KEY AGENTS:
 *   - Classical philologists: Institutional agenda-setters; establish the standard; control curricula and textual commentary; benefit through professional gatekeeping.
 *   - Medieval scholars: Moderate-power payers with identity-locked exit; their own competence is now declared corrupt; cannot exit without abandoning scholarly identity.
 *   - Technical domain practitioners (jurists, physicians, theologians): Powerful-seat payers; need Latin for precision, but classical purity makes precision unattainable; constrained exit.
 *   - Humanist elite: Institutional beneficiaries; control prestige economy; gain authority through mastery of the exclusive standard.
 *   - Textual purism gatekeepers: Institutional agenda-setters and secondary beneficiaries; administers the standard; collects professional authority.
 *   - Continuity-reading adherents: Excluded stakeholders; would contest the rupture reading but are marginalized from prestige-bearing positions.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(latin_correctness__rupture_reading, 0.78).
domain_priors:suppression_score(latin_correctness__rupture_reading, 0.81).
domain_priors:theater_ratio(latin_correctness__rupture_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(latin_correctness__rupture_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(latin_correctness__rupture_reading, suppression_requirement, 0.81).
narrative_ontology:constraint_metric(latin_correctness__rupture_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(latin_correctness__rupture_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(latin_correctness__rupture_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(latin_correctness__rupture_reading, tangled_rope).
narrative_ontology:human_readable(latin_correctness__rupture_reading, "Classical Latin Purism as Fixed Standard (Rupture Reading)").
narrative_ontology:topic_domain(latin_correctness__rupture_reading, "intellectual_history/philology/cultural_authority").

domain_priors:requires_active_enforcement(latin_correctness__rupture_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(latin_correctness__rupture_reading, 'bd8b7cc2-fe21-465b-abfd-1180999781ab').
narrative_ontology:cs_kernel_codification('bd8b7cc2-fe21-465b-abfd-1180999781ab', fixed_text).
narrative_ontology:cs_authority_grounding('bd8b7cc2-fe21-465b-abfd-1180999781ab', lineage).
narrative_ontology:cs_interpretation_layer_present('bd8b7cc2-fe21-465b-abfd-1180999781ab').
narrative_ontology:cs_reading_relation('bd8b7cc2-fe21-465b-abfd-1180999781ab', latin_correctness__continuity_reading, forecloses).
narrative_ontology:cs_reading_relation('bd8b7cc2-fe21-465b-abfd-1180999781ab', latin_correctness__hybrid_reading, influences).
narrative_ontology:cs_axiom('bd8b7cc2-fe21-465b-abfd-1180999781ab', foundational, linguistic_deviation_is_corruption).
narrative_ontology:cs_axiom_status(linguistic_deviation_is_corruption, holdable).
narrative_ontology:cs_axiom_grounding('bd8b7cc2-fe21-465b-abfd-1180999781ab', linguistic_deviation_is_corruption, deontological).
narrative_ontology:cs_axiom('bd8b7cc2-fe21-465b-abfd-1180999781ab', foundational, classical_texts_define_latin_identity).
narrative_ontology:cs_axiom_status(classical_texts_define_latin_identity, holdable).
narrative_ontology:cs_axiom_grounding('bd8b7cc2-fe21-465b-abfd-1180999781ab', classical_texts_define_latin_identity, conventional).
narrative_ontology:cs_reference_frame('bd8b7cc2-fe21-465b-abfd-1180999781ab', classical_text_fixity).
narrative_ontology:cs_drift_state('bd8b7cc2-fe21-465b-abfd-1180999781ab', medieval_usage_divergence, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('bd8b7cc2-fe21-465b-abfd-1180999781ab', '').
narrative_ontology:cs_kernel_id(latin_correctness__rupture_reading, latin_correctness).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(latin_correctness__rupture_reading, classical_philologists).
narrative_ontology:constraint_beneficiary(latin_correctness__rupture_reading, humanist_elite).
narrative_ontology:constraint_beneficiary(latin_correctness__rupture_reading, textual_purism_gatekeepers).
narrative_ontology:constraint_victim(latin_correctness__rupture_reading, medieval_scholars).
narrative_ontology:constraint_victim(latin_correctness__rupture_reading, technical_domain_practitioners).
narrative_ontology:constraint_victim(latin_correctness__rupture_reading, vernacular_writers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(latin_correctness__rupture_reading, medieval_scholars).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Establish and enforce the standard that classical Latin represents a fixed, reconstructed ideal to be studied from ancient texts (Cicero, Vergil, etc.), not a living linguistic system. They control academic curricula, textual commentary traditions, and what constitutes 'correct' Latin usage. They benefit by monopolizing the authority to define correctness and by creating professional gatekeeping around classical expertise.
narrative_ontology:constraint_stakeholder(latin_correctness__rupture_reading, classical_philologists, agenda_setter,
    institutional, generational, arbitrage, continental).

% Their own linguistic practices—the living medieval Latin they used for theology, law, administration, and communication—are systematically delegitimized as 'corruption' or 'barbarous deviation.' They have professional identity invested in their Latin competence, but their competence is now declared incorrect by the ruling standard. They cannot exit without abandoning their scholarly persona. They benefit marginally from access to the accumulated classical corpus, but pay through continuous epistemic subordination.
narrative_ontology:constraint_stakeholder(latin_correctness__rupture_reading, medieval_scholars, payer,
    moderate, biographical, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(latin_correctness__rupture_reading, medieval_scholars, beneficiary).

% Jurists, physicians, theologians, and natural philosophers who must use Latin for precise technical expression in domains where classical vocabulary and morphology are inadequate or absent (medieval legal terminology, anatomical nomenclature, theological neologisms). They are forced to either violate the classical standard (and lose scholarly credibility) or artificially constrain their technical expressiveness to fit classical paradigms. The constraint gives them no exit: their domains require Latin, but conforming to classical purity hobbles their technical function.
narrative_ontology:constraint_stakeholder(latin_correctness__rupture_reading, technical_domain_practitioners, payer,
    powerful, biographical, constrained, continental).

% Poets, chroniclers, and administrators writing in emerging vernacular languages face prestige penalties and institutional marginalizing because their chosen medium is defined implicitly as inferior—Latin is the standard of correctness, and only classical Latin at that. Vernacular writing is tolerated but not honored. They bear the cost of stigmatization and institutional underinvestment relative to Latin-writing peers.
narrative_ontology:constraint_stakeholder(latin_correctness__rupture_reading, vernacular_writers, payer,
    moderate, biographical, mobile, regional).

% Renaissance humanists and later neoclassicists benefit from the rupture reading by controlling the prestige economy: mastery of classical Latin becomes the marker of education, refinement, and intellectual authority. They use this monopoly to exclude non-elite writers and to dominate cultural production. The constraint vindicates their aesthetic and philosophical commitments (return to ancient sources, rejection of 'barbarism').
narrative_ontology:constraint_stakeholder(latin_correctness__rupture_reading, humanist_elite, beneficiary,
    institutional, generational, analytical, continental).

% Editors, grammarians, and textual critics who define and enforce the boundaries of acceptable Latin usage through commentary traditions, grammars, and editorial practices. They collect professional authority and prestige by administering the standard. Their material interest is in maintaining the standard's mystique and difficulty—easier Latin would reduce their gatekeeping power.
narrative_ontology:constraint_stakeholder(latin_correctness__rupture_reading, textual_purism_gatekeepers, agenda_setter,
    institutional, generational, analytical, continental).
narrative_ontology:stakeholder_secondary_role(latin_correctness__rupture_reading, textual_purism_gatekeepers, beneficiary).

% The Church administers Latin education and doctrinal commentary. They have institutional incentives to enforce a fixed, controllable standard (easier to enforce orthodoxy with a fixed language), but also historical dependence on medieval Latin practice and vocabulary for theological expression. They are partly beneficiary (control over language = control over meaning) and partly constrained (medieval theology cannot be retroactively purified without rewriting doctrine).
narrative_ontology:constraint_stakeholder(latin_correctness__rupture_reading, religious_institutional_authorities, agenda_setter,
    institutional, generational, constrained, continental).

% Scholars who defend medieval Latin as a legitimate continuation of classical Latin through natural language change are structurally excluded from prestige-bearing institutional positions. Their arguments are dismissed as 'philologically unsophisticated.' They would have institutional voice but are systematically marginalized by the rupture reading's control of academic authority.
narrative_ontology:constraint_stakeholder(latin_correctness__rupture_reading, continuity_reading_adherents, excluded,
    moderate, biographical, trapped, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(latin_correctness__rupture_reading, textual_purism_gatekeepers).
narrative_ontology:fixing_cost_class(latin_correctness__rupture_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a unified, reconstructed standard that enables scholars across regions and centuries to read the same canonical texts with consistency and to communicate through a shared linguistic reference point—removing ambiguity about what 'correct' Latin is by anchoring it to a fixed corpus of ancient authors.
% TRANSFER_FUNCTION: Moves cultural authority and professional prestige from medieval practitioners and technical specialists to classical philologists and humanist elites who monopolize the power to define correctness. Medieval scholars' labor (producing technical Latin, theological commentary) is revalued downward as 'corruption' even as its output remains essential to institutions.
% ABSENT_VOICES: Continuity-reading scholars are structurally excluded—they would argue that medieval Latin is a legitimate organic development and that the rupture reading artificially freezes language at an arbitrary point. Technical practitioners and medieval jurists would contest that classical purity is incompatible with their functional domains. Speakers of living Romance languages (who might claim descent from Latin) would argue that their languages represent legitimate evolution, not corruption.
% DISAPPEARANCE_RATIONALE: If the rupture reading and its enforcement apparatus vanished, medieval Latin would be rehabilitated as a legitimate historical stage of the language; technical domains would recover neologisms and pragmatic forms now stigmatized; vernacular writing would gain prestige; and the gatekeeping power of classical philologists would dissolve—the prestige economy around Latin expertise would reorganize fundamentally. The scholarly community would redistribute authority toward continuity-reading and hybrid-reading framings, and medieval scholars' past work would be revalued from 'corrupt' to 'historically legitimate.'
% FOUNDING_PROBLEM: Post-classical Latin texts show marked divergences from classical usage (vocabulary, morphology, syntax); the question is whether these represent corruption/degradation or legitimate linguistic change. The rupture reading posits that classical Latin is the standard because it is fixed in the authoritative ancient texts, and all deviation is corruption.
% FOUNDING_PROBLEM_CORROBORATION: Classical philologists attest the problem is that medieval Latin is indeed corrupt and corrupting. Modern historical linguists and sociolinguists outside the classical establishment attest that the founding premise (corruption as a diagnostic category) is theoretically indefensible—all language change is continuous, and 'corruption' is a value judgment, not a descriptive fact. Medieval scholars themselves (in the archive) attest they were aware of classical norms and chose pragmatic forms deliberately, not through ignorance. Comparative Romance linguistics shows medieval forms evolved into the modern Romance languages following regular sound laws—evidence of evolution, not random degradation.
narrative_ontology:disappearance_verdict(latin_correctness__rupture_reading, world_rearranges).
narrative_ontology:founding_problem_status(latin_correctness__rupture_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(latin_correctness__rupture_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(latin_correctness__rupture_reading, 'none', 1).
narrative_ontology:epsilon_provenance(latin_correctness__rupture_reading, 0.78, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(latin_correctness__rupture_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(latin_correctness__rupture_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(latin_correctness__rupture_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.78 at interval end) because the constraint systematically devalues the work of medieval scholars and hobbles technical practitioners without their consent, while transferring prestige to the classical establishment. The constraint is actively enforced (suppression = 0.81) through curriculum control, editorial gatekeeping, and institutional hierarchies that marginalize alternative readings. Theater ratio (0.42) is moderate because the constraint does coordinate real scholarship around a shared canonical corpus—the coordination function is genuine—but a growing share of enforcement effort goes to defending the purity boundary rather than enabling shared understanding. The measurement series shows extraction and suppression rising over time (0-20 interval), reflecting the strengthening of institutional enforcement and the widening prestige gap as humanist values consolidated in universities. Accessibility collapse (0.72) reflects that alternatives (medieval Latin legitimacy, functional pragmatism) become harder to access once the classical standard is internalized as the 'obvious' correct form. Resistance (0.68) is substantial because medieval scholars, technical practitioners, and continuity-reading adherents mount real but unequally-heard counter-claims.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (classical philologists) experiences this as genuine coordination around a reconstructed standard that enables rigorous textual scholarship. The payer seats (medieval scholars, technical practitioners) experience it as enforced devaluation of their competence and functional constraint. The excluded seat (continuity-reading adherents) experiences it as a false summit—a constructed standard defended by institutional power, not by linguistic facts. The engine will compute these divergent types from the structural data: the agenda-setter derives high coordination benefit and low extraction risk (d near beneficiary end); the medieval scholar derives low coordination benefit relative to identity-lock costs (d near target end); technical practitioners sit between, constrained but powerful. The claimed type (tangled_rope) reflects that the structure combines genuine coordination (shared canonical reference) with asymmetric extraction (prestige transfer, epistemic subordination). This gap—between the agenda-setter's coordination story and the payer's extraction story—is exactly what the claim/metric independence principle captures: the engine measures whether the structure is defensible as pure coordination or whether the metrics show extraction dominance.
 *
 * DIRECTIONALITY LOGIC:
 *   Classical philologists and gatekeepers are structural beneficiaries: they monopolize the authority to define correctness, control academic prestige, and create professional scarcity (mastery of classical Latin becomes a rare, high-value skill). Their directionality is toward the beneficiary end (d near 0.0): they collect professional prestige and gatekeeping power from the constraint's operation. Medieval scholars are structural targets: their labor is revalued downward, they cannot exit (identity-locked), and they bear continuous epistemic subordination. Their directionality is toward the target end (d near 1.0). Technical practitioners are constrained targets: they are powerful-seat actors, but the constraint hobbles their function, forcing them to choose between violating the standard (prestige loss) and constraining their expressiveness (functional loss). Their directionality is moderately high (d near 0.65-0.70): powerful enough to resist in some domains, but constrained enough that exit is costly. Humanist elites benefit through cultural authority accumulation (d near 0.1-0.2). The beneficiary/victim declarations drive this directionality computation automatically—the schema enforces that declared beneficiaries and victims appear in the stakeholders array, and the engine derives d from their roles, power atoms, and exit options.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (classical purity as a descriptive standard for post-classical Latin) is arguably dead or contested: modern historical linguistics rejects 'corruption' as a descriptive category and treats medieval Latin as a legitimate stage of language evolution. However, the constraint persists as a powerful institutional fact, enforced through curricula, gatekeeping, and prestige hierarchies. Mandatrophy detection hinges on the mismatch between founding_problem_status (contested/dead) and disappearance_verdict (world_rearranges). If the founding problem is dead but the constraint persists, the constraint is maintained by institutional inertia and power asymmetry, not by genuine need—exactly the mandatrophy signal. The theater_ratio rising from 0.28 to 0.42 suggests performative activity (defending the purity boundary, editing texts, running seminars on classical norms) is growing relative to the functional coordination activity. A constraint that is mostly theater for its own administration, with a defunct founding mandate, is a piton candidate. However, the constraint still coordinates real scholarship and produces genuine scholarly output—it is not pure performance. The classification is tangled_rope (hybrid coordination/extraction) rather than piton, because the coordination function is real enough that many seats genuinely benefit from shared canonical standards, even as the extraction and gatekeeping functions dominate the effort. The mandatrophy risk is high: if the rupture reading's linguistic foundations are undermined (continuity-reading scholarship gains institutional voice), the constraint could flip rapidly from tangled_rope to snare or piton.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    corruption_vs_evolution_fundamental,
    'Is linguistic deviation from classical forms a corruption of Latin, or a legitimate evolution of the language comparable to how all living languages change?',
    'Theoretical linguistics and historical-comparative methodology: if medieval Latin can be shown to follow consistent rules and regular sound/morphological changes (like Romance languages), it is evolution; if deviations are random or rule-violating, it is degradation. Modern linguistics has established that all change is rule-governed, making ''corruption'' a descriptive category error.',
    'If resolution favors evolution, the rupture reading''s core axiom (deviation = corruption) collapses, and the constraint flips from tangled_rope to snare or piton (extracted prestige without genuine functional justification). Medieval scholars would be rehabilitated, and the constraint''s enforcement would become purely extractive.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(corruption_vs_evolution_fundamental, empirical, 'Descriptive validity of ''corruption'' as a category for linguistic change.').

omega_variable(
    classical_purity_functionality_decoupling,
    'Is classical Latin actually superior for technical, theological, and legal expression, or does medieval Latin''s specialized vocabulary perform better for post-classical domains?',
    'Functional comparison: (a) can the same concepts be expressed equivalently in classical and medieval Latin? (b) do medieval technical texts (legal, medical, theological) achieve their communicative goals using classical Latin only? (c) what is the cost in clarity/precision of forcing classical forms onto medieval domains?',
    'If medieval forms are functionally superior or equivalent for their domains, classical purity is a prestige mechanism, not a functional requirement—extraction is unambiguous. The constraint would reclassify as pure snare in technical domains.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(classical_purity_functionality_decoupling, empirical, 'Whether classical purity is functionally necessary or merely prestigious.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the suppression of medieval scholars'' authority structural (external barriers: no career paths outside classical gatekeeping) or internalized (they have absorbed the judgment that their Latin is inferior)?',
    'Post-suppression trajectory: if medieval scholars leave classical institutions and regain confidence in their expertise within continuity-reading frameworks, the suppression was primarily structural; if doubt persists after the constraint is lifted, it is internalized.',
    'If primarily internalized, the constraint''s effective suppression is higher than the base metric (0.81) suggests—the target carries the suppression with them after exit. If primarily structural, alternative institutional spaces might quickly rehabilitate medieval expertise. The distinction matters for counting resistance capacity: internalized suppression makes resistance harder to organize.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether suppression is structural barrier or internalized self-doubt.').

omega_variable(
    reading_boundary_definitional,
    'Does the rupture reading''s insistence that medieval forms are corrupt depend on an essentialist definition of ''Latin'' (Latin IS what ancient texts say it is), or does it permit Latin to be a language with internal variation?',
    'Analyze the readings'' foundational premises: (a) rupture reading: Latin identity is defined by classical texts (essentialist, foreclosing change). (b) continuity reading: Latin identity persists through change like any language. (c) hybrid reading: multiple Latin standards coexist for different domains. If the readings'' core premises contradict on definitional grounds, the relationship is forecloses, not coexists_with.',
    'This is a conceptual omega: whether the rupture and continuity readings can coexist within a single institutional framework depends on whether ''Latin'' can have a changing identity. If Latin identity is essentialist (rupture), change is corruption. If Latin identity is relational (continuity), change is normal. No empirical fact resolves this—it is a framing choice about what ''Latin'' fundamentally is.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_boundary_definitional, conceptual, 'Whether ''Latin'' identity is essentialist (fixed) or relational (evolving).').

omega_variable(
    institutional_power_vs_epistemic_authority,
    'Is the rupture reading''s institutional dominance grounded in genuine epistemic authority (classical texts are more reliable sources for correct Latin) or in institutional power (humanists controlled universities and defined what counts as knowledge)?',
    'Historical analysis: (a) did Renaissance humanists choose the rupture reading because it was more epistemically rigorous, or because it aligned with their aesthetic and political commitments? (b) would a neutral linguistic analysis prefer rupture, continuity, or hybrid framing? (c) did institutional positions go to rupture-reading adherents before or after the reading''s epistemic credentials were tested?',
    'If institutional power preceded epistemic justification, the constraint is power-backed authority, not knowledge-backed authority—a strong signal for extractive governance. The measurement of ''suppression_requirement'' rising over time (0.71 to 0.81) suggests enforcement is intensifying, consistent with power-backed rather than knowledge-backed authority.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_power_vs_epistemic_authority, empirical, 'Whether rupture reading''s authority is grounded in epistemic merit or institutional power.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(latin_correctness__rupture_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(latin_correctness_rupture_tr_t0, latin_correctness__rupture_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(latin_correctness_rupture_tr_t4, latin_correctness__rupture_reading, theater_ratio, 4, 0.31).
narrative_ontology:measurement(latin_correctness_rupture_tr_t8, latin_correctness__rupture_reading, theater_ratio, 8, 0.35).
narrative_ontology:measurement(latin_correctness_rupture_tr_t12, latin_correctness__rupture_reading, theater_ratio, 12, 0.39).
narrative_ontology:measurement(latin_correctness_rupture_tr_t16, latin_correctness__rupture_reading, theater_ratio, 16, 0.41).
narrative_ontology:measurement(latin_correctness_rupture_tr_t20, latin_correctness__rupture_reading, theater_ratio, 20, 0.42).

% Extraction over time
narrative_ontology:measurement(latin_correctness_rupture_be_t0, latin_correctness__rupture_reading, base_extractiveness, 0, 0.62).
narrative_ontology:measurement(latin_correctness_rupture_be_t4, latin_correctness__rupture_reading, base_extractiveness, 4, 0.68).
narrative_ontology:measurement(latin_correctness_rupture_be_t8, latin_correctness__rupture_reading, base_extractiveness, 8, 0.73).
narrative_ontology:measurement(latin_correctness_rupture_be_t12, latin_correctness__rupture_reading, base_extractiveness, 12, 0.76).
narrative_ontology:measurement(latin_correctness_rupture_be_t16, latin_correctness__rupture_reading, base_extractiveness, 16, 0.77).
narrative_ontology:measurement(latin_correctness_rupture_be_t20, latin_correctness__rupture_reading, base_extractiveness, 20, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(latin_correctness_rupture_su_t0, latin_correctness__rupture_reading, suppression_requirement, 0, 0.71).
narrative_ontology:measurement(latin_correctness_rupture_su_t4, latin_correctness__rupture_reading, suppression_requirement, 4, 0.74).
narrative_ontology:measurement(latin_correctness_rupture_su_t8, latin_correctness__rupture_reading, suppression_requirement, 8, 0.77).
narrative_ontology:measurement(latin_correctness_rupture_su_t12, latin_correctness__rupture_reading, suppression_requirement, 12, 0.79).
narrative_ontology:measurement(latin_correctness_rupture_su_t16, latin_correctness__rupture_reading, suppression_requirement, 16, 0.8).
narrative_ontology:measurement(latin_correctness_rupture_su_t20, latin_correctness__rupture_reading, suppression_requirement, 20, 0.81).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(latin_correctness__rupture_reading, attachment_coordination).
narrative_ontology:boltzmann_floor_override(latin_correctness__rupture_reading, 0.12).
narrative_ontology:affects_constraint(latin_correctness__rupture_reading, latin_correctness__continuity_reading).
narrative_ontology:affects_constraint(latin_correctness__rupture_reading, latin_correctness__hybrid_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kernel 'latin_correctness.' The sibling constraints 'continuity_reading' and 'hybrid_reading' instantiate alternative readings of the same kernel. The three constraints share the same referent (post-classical Latin usage and its evaluation) but author different ε values reflecting different readings' structural positions. The rupture reading measures high extraction and suppression because it delegitimizes medieval practice; the continuity reading would measure low extraction because it revalues medieval practice as legitimate; the hybrid reading would measure moderate extraction with domain-differentiated victim sets. Link all three via network.affects_constraints to enable contamination analysis: if one reading's authority is undermined, neighboring readings become viable, potentially triggering rapid institutional reorganization.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(latin_correctness__rupture_reading, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
