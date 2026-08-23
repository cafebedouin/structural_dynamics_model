% ============================================================================
% CONSTRAINT STORY: correct_latin_kernel__hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_correct_latin_kernel__hybrid_reading, []).

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
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: correct_latin_kernel__hybrid_reading
 *   human_readable: Layered Correctness Standard for Latin (Hybrid Reading)
 *   domain: historical_linguistics/philology/intellectual_history
 *
 * SUMMARY:
 *   Between the fifteenth and nineteenth centuries, European learned culture
 *   ran on a rule for what counted as correct Latin. This story instantiates
 *   the hybrid reading of that rule: the inflectional core of the language
 *   reached the medieval world as unbroken inheritance and could be trusted
 *   as received, but sentence construction and vocabulary had drifted far
 *   enough that recovering classical usage required returning to ancient
 *   books; reconstruction therefore proceeded in layers, part correction from
 *   within the tradition, part reoccupation from witnesses. The rule solved a
 *   real problem, repairing a damaged transmission and keeping a pan-European
 *   learned medium intelligible, while redistributing authority:
 *   practitioners of inherited scholastic usage found their working language
 *   reclassified as error, schoolchildren carried a doubled curriculum
 *   enforced by discipline, and the scholars who adjudicated the layers
 *   collected appointments, prestige, and a market for corrected editions.
 *   The claim/metric gap is deliberate: the arrangement is CLAIMED as
 *   tangled_rope (genuine coordination carrying asymmetric extraction) while
 *   the metrics are authored independently from its observable operation; the
 *   engine measures the divergence. KEY AGENTS (by structural relationship):
 *   - humanist_arbiters: agenda-setting beneficiary (institutional /
 *   identity_locked) — administers the layered standard, adjudicates
 *   correctness, collects authority and the editorial market -
 *   printing_house_editors: beneficiary (organized / mobile) — profits from
 *   corrected editions without setting the standard - latin_schoolmasters:
 *   beneficiary with payer costs (moderate / constrained) — teaches the
 *   doubled curriculum under supervision from above -
 *   scholastic_tradition_practitioners: primary target (organized /
 *   constrained) — inherited working usage reclassified as defect -
 *   grammar_school_pupils: primary target (powerless / trapped) — bear the
 *   doubled drill under physical discipline -
 *   women_barred_from_latin_education: excluded voice (powerless / trapped) —
 *   shut out of the schooling the standard guards -
 *   modern_historical_linguists: analytical observer (analytical /
 *   analytical) — sees the full structure with no stake in prescription
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(correct_latin_kernel__hybrid_reading, 0.58).
domain_priors:suppression_score(correct_latin_kernel__hybrid_reading, 0.47).
domain_priors:theater_ratio(correct_latin_kernel__hybrid_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(correct_latin_kernel__hybrid_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(correct_latin_kernel__hybrid_reading, suppression_requirement, 0.47).
narrative_ontology:constraint_metric(correct_latin_kernel__hybrid_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(correct_latin_kernel__hybrid_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(correct_latin_kernel__hybrid_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(correct_latin_kernel__hybrid_reading, tangled_rope).
narrative_ontology:human_readable(correct_latin_kernel__hybrid_reading, "Layered Correctness Standard for Latin (Hybrid Reading)").
narrative_ontology:topic_domain(correct_latin_kernel__hybrid_reading, "historical_linguistics/philology/intellectual_history").

domain_priors:requires_active_enforcement(correct_latin_kernel__hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(correct_latin_kernel__hybrid_reading, '01eb761a-7fec-4e38-a4cd-3eaa11a62a5d').
narrative_ontology:cs_kernel_codification('01eb761a-7fec-4e38-a4cd-3eaa11a62a5d', formalized).
narrative_ontology:cs_authority_grounding('01eb761a-7fec-4e38-a4cd-3eaa11a62a5d', lineage).
narrative_ontology:cs_interpretation_layer_present('01eb761a-7fec-4e38-a4cd-3eaa11a62a5d').
narrative_ontology:cs_reading_relation('01eb761a-7fec-4e38-a4cd-3eaa11a62a5d', correct_latin_kernel__continuity_reading, forecloses).
narrative_ontology:cs_reading_relation('01eb761a-7fec-4e38-a4cd-3eaa11a62a5d', correct_latin_kernel__discontinuity_reading, forecloses).
narrative_ontology:cs_axiom('01eb761a-7fec-4e38-a4cd-3eaa11a62a5d', foundational, morphological_core_is_continuous_inheritance).
narrative_ontology:cs_axiom_status(morphological_core_is_continuous_inheritance, holdable).
narrative_ontology:cs_axiom_grounding('01eb761a-7fec-4e38-a4cd-3eaa11a62a5d', morphological_core_is_continuous_inheritance, empirically_contingent).
narrative_ontology:cs_axiom('01eb761a-7fec-4e38-a4cd-3eaa11a62a5d', foundational, syntax_and_lexicon_require_textual_reoccupation).
narrative_ontology:cs_axiom_status(syntax_and_lexicon_require_textual_reoccupation, holdable).
narrative_ontology:cs_axiom_grounding('01eb761a-7fec-4e38-a4cd-3eaa11a62a5d', syntax_and_lexicon_require_textual_reoccupation, empirically_contingent).
narrative_ontology:cs_reference_frame('01eb761a-7fec-4e38-a4cd-3eaa11a62a5d', continuous_core_recoverable_strata).
narrative_ontology:cs_drift_state('01eb761a-7fec-4e38-a4cd-3eaa11a62a5d', post_neogrammarian_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('01eb761a-7fec-4e38-a4cd-3eaa11a62a5d', '').
narrative_ontology:cs_kernel_id(correct_latin_kernel__hybrid_reading, correct_latin_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(correct_latin_kernel__hybrid_reading, humanist_arbiters).
narrative_ontology:constraint_beneficiary(correct_latin_kernel__hybrid_reading, printing_house_editors).
narrative_ontology:constraint_beneficiary(correct_latin_kernel__hybrid_reading, latin_schoolmasters).
narrative_ontology:constraint_victim(correct_latin_kernel__hybrid_reading, scholastic_tradition_practitioners).
narrative_ontology:constraint_victim(correct_latin_kernel__hybrid_reading, grammar_school_pupils).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(correct_latin_kernel__hybrid_reading, latin_schoolmasters).
narrative_ontology:constraint_vindicates(correct_latin_kernel__hybrid_reading, morphological_continuity_hypothesis).
narrative_ontology:constraint_vindicates(correct_latin_kernel__hybrid_reading, textual_recovery_necessity_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Edit ancient texts, write grammars and dictionaries, and decide which spellings, word orders, and phrases count as proper Latin in schools, editions, and official documents. Their appointments, correspondence networks, and standing rest on being recognized as judges of usage. They police one another as much as anyone else, and leaving the trade would mean abandoning the identity their authority is built on.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__hybrid_reading, humanist_arbiters, agenda_setter,
    institutional, generational, identity_locked, continental).

% Set and sell corrected editions of ancient authors and new school grammars. Every announced discovery of faults in received texts creates demand for a fresh printing. They do not set the standard of correctness but profit from its upkeep, and they can redirect their presses to vernacular fiction, news sheets, or devotional books if the Latin trade shrinks.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__hybrid_reading, printing_house_editors, beneficiary,
    organized, immediate, mobile, continental).

% Make their living teaching a double curriculum: inherited inflectional forms drilled from old grammars, and classical sentence patterns and vocabulary reconstructed from approved authors. They are supervised by the arbiters above them and answer to parents and church authorities below; leaving the schoolroom means losing the rank and income tied to their training.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__hybrid_reading, latin_schoolmasters, beneficiary,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(correct_latin_kernel__hybrid_reading, latin_schoolmasters, payer).

% University masters, canon lawyers, chancery clerks, and physicians who read and write the working Latin handed down through their institutions. Under the recovered standard their sentence structures and technical vocabularies are reclassified as faults to be mended: their professional documents get emended by editors, their textbooks replaced, their style ridiculed as barbarous. They cannot stop writing Latin, because law, theology, and administration run on it, and retraining in classical style costs years they may not have.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__hybrid_reading, scholastic_tradition_practitioners, payer,
    organized, generational, constrained, continental).

% Children drilled in declensions from one tradition and in Ciceronian sentence patterns from another, physically punished for mistakes in either. They choose nothing about the curriculum and cannot leave school; the payoff for compliance arrives decades later, if at all.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__hybrid_reading, grammar_school_pupils, payer,
    powerless, immediate, trapped, local).

% Shut out of the grammar schools and universities where the standard is taught and enforced. Learned conversation, office, and publication in the republic of letters run through the schooling they are denied. They would contest the allocation of so much of society's educational effort to a double-layered dead language if they were admitted to the discussion.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__hybrid_reading, women_barred_from_latin_education, excluded,
    powerless, biographical, trapped, continental).

% Study how spoken Latin turned into the Romance languages and how the medieval written standard actually worked, with no stake in prescribing usage. From this seat the corruption vocabulary looks like a period artifact, the recovery project looks like real but bounded scholarship, and the persistence of prescriptive teaching after the descriptive questions were answered is a measurable social fact.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__hybrid_reading, modern_historical_linguists, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(correct_latin_kernel__hybrid_reading, humanist_arbiters).
narrative_ontology:fixing_cost_class(correct_latin_kernel__hybrid_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains a single teachable written Latin usable across Europe and across centuries: the layered rule tells copyists, teachers, and editors which parts of the inherited language can be trusted as received (inflectional forms) and which must be checked against ancient witnesses (sentence construction, vocabulary), so that damaged transmissions can be repaired and new texts remain intelligible to distant readers and to posterity.
% TRANSFER_FUNCTION: Moves linguistic authority and educational resources from the practitioners of the inherited medieval tradition, whose syntax and lexicon are ruled defective, and from schoolchildren, who carry the doubled curriculum, toward the arbiters of the recovered standard, the schools that teach it, and the presses that sell corrected texts.
% ABSENT_VOICES: Women excluded from Latin schooling; vernacular-only scholars; the medieval authors themselves, corrected posthumously without consent; and the working administrators who never entered the quarrel. All stand outside the republic of letters the standard guards: the first by law and custom, the others by death or by never having been asked.
% DISAPPEARANCE_RATIONALE: Without the layered standard, editorial practice, school curricula, church administration, international scholarship, and the whole apparatus of corrected editions would lose their operating rule overnight; the republic of letters would fragment into regional usages, and access to ancient literature would narrow to scattered specialists until some replacement convention emerged.
% FOUNDING_PROBLEM: After the western empire fell, spoken and written Latin drifted away from the language of the surviving ancient books, and the manuscript chain accumulated copying errors; scholars needed a rule for telling trustworthy inheritance from drift so they could repair the books and keep a common learned language alive.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: nineteenth-century critical editions completed the recovery the standard was built for; Romance-language linguistics showed spoken Latin evolving continuously rather than decaying; and modern descriptive linguists treat the corruption framing as a period artifact. None of these seats collects anything from the standard's upkeep. The arbiters' own intellectual successors concede the descriptive questions are settled while continuing prescriptive teaching.
narrative_ontology:disappearance_verdict(correct_latin_kernel__hybrid_reading, world_rearranges).
narrative_ontology:founding_problem_status(correct_latin_kernel__hybrid_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(correct_latin_kernel__hybrid_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(correct_latin_kernel__hybrid_reading, 'none', 1).
narrative_ontology:epsilon_provenance(correct_latin_kernel__hybrid_reading, 0.58, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(correct_latin_kernel__hybrid_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(correct_latin_kernel__hybrid_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(correct_latin_kernel__hybrid_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction ends at 0.58 and rises from 0.44 as the apparatus institutionalizes (print, schools, career ladders rewarding the policing of usage), peaking around t=18 to t=24 before declining as vernacular science and administration pull work out of Latin and the standard academicizes. Theater rises monotonically 0.12 to 0.55: early activity was mostly functional recovery, while by the nineteenth century much of what remains is ritual composition and examination performance after the descriptive questions were answered. Suppression arcs 0.35 up to 0.60 and back to 0.47: enforcement machinery (school discipline, editorial policing, ecclesiastical pressure) built through the sixteenth and seventeenth centuries and decayed as compulsory Latin receded. Accessibility collapse 0.62: within the domain of Latin usage the layered rule forecloses alternatives, since medieval syntax cannot be legitimated from inside the tradition, but the vernacular exit stays open, so collapse is partial. Resistance 0.45: sustained but losing defenses of scholastic and ecclesiastical usage, the anti-Ciceronian polemic, and later complaints about pedantry. All three series share one nine-point grid; each unit is roughly a decade from the 1440s (t=0 near Valla's Elegantiae, t=18 the early seventeenth-century enforcement peak, t=42 the neogrammarian era).
 *
 * PERSPECTIVAL GAP:
 *   From the arbiter seat the arrangement reads as custodianship: someone must decide which forms are trustworthy or the books stay broken. From the scholastic practitioner seat the same rule reads as confiscation: a working language declared defective by men whose authority consists in declaring it so. Pupils experience neither debate, only the doubled drill and the rod. Printing houses experience the whole quarrel as demand. The engine computes these divergent per-seat classifications from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Humanist arbiters sit nearest the beneficiary end: they administer the rule and collect its yields. Printing-house editors and schoolmasters collect without setting the rule, placing them modestly subsidized, though schoolmasters also bear supervision costs, pulling them toward symmetry. Scholastic practitioners and pupils sit near the target end: they pay in reclassified labor, replaced books, and drilled years, with constrained or no exit. Suppression is authored as a raw structural property and is not scaled by power or scope; only extractiveness is scaled by directionality and scope, and the continental scope of the learned republic amplifies effective extraction on the trapped payer seats because verification of correctness at that scale favors whoever holds the reference texts. Receipt: the extraction demonstrably accrues to the arbiter seat, since appointments, precedence, and the editorial market flow from their rulings, and printing gains are derivative of arbiter decisions, so gain_flow names the arbiters rather than diffuse. Fixing cost: prohibitive, since dissolving the layered rule would break the recovery apparatus, the school system built on it, and the arbiters' own identities, costs exceeding any benefit to the seat able to act.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards both directions. Reading the arrangement as pure coordination would erase the documented redistribution: reclassified practitioners, doubled curricula, authority rents. Reading it as pure extraction would erase the real recovery: the books were repaired, the ancient texts became readable, and a common learned medium functioned for four centuries. The layered structure is precisely why it is a hybrid: the same rule that repairs transmission also disqualifies the inheritors. After roughly t=30 the founding problem is solved and the arrangement's persistence runs on institutional inertia, fused professional identity, and residual niches; theater_ratio crossing 0.5 marks the drift toward vestigial maintenance, and the genealogy interview records the founding problem as dead while the world still rearranges around the standard, which is the mismatch signature of zombie maintenance. The drift declaration (axiom_overriding, substantial, unacknowledged) reflects that the pedagogical apparatus continues prescriptive teaching without reckoning with the descriptive reframing that dissolved its founding vocabulary.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This story instantiates the hybrid reading of the correct-Latin kernel; how would the constraint''s structure change under the sibling readings?',
    'Re-author the same referent under each sibling: the continuity reading removes the textual-recovery layer, leaving no victim set among scholastic practitioners and dropping extraction toward coordination-cost levels; the discontinuity reading extends recovery to morphology, enlarging the defective set and raising extraction and enforcement. Compile all three and compare.',
    'Under the continuity reading the arrangement approaches a pure coordination standard; under the discontinuity reading it approaches enforced reoccupation with a larger victim set; the hybrid''s classification is conditional on the layer boundary holding where this story places it.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer structure: one reading of the correct_latin_kernel; sibling readings redistribute legitimacy and the victim set.').

omega_variable(
    layer_boundary_placement,
    'Is the continuity/recovery boundary really at morphology versus syntax-and-lexicon, or does it cut elsewhere (orthography, word order, derivational forms, technical vocabulary)?',
    'Survey editorial practice and period grammars: track which categories of medieval usage were accepted as received and which were emended, category by category, across the interval.',
    'Moving the boundary transfers forms between the legitimate and defective sets, shifting extraction and the victim roster; a boundary drawn deeper into morphology moves the reading toward discontinuity, a shallower one toward continuity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(layer_boundary_placement, conceptual, 'Placement of the layer boundary on which the hybrid reading depends.').

omega_variable(
    evidential_vs_taste_reoccupation,
    'What share of the textual recovery was driven by witness evidence rather than by contemporary taste dressed as antiquity (Ciceronian preference, lexical purism)?',
    'Compare emendations against manuscript attestation: count corrections adopted before and after supporting witnesses were known, and sample edition prefaces for stated criteria.',
    'A high taste-share raises effective extraction, since authority is collected beyond what the evidence warrants, and strengthens the extraction reading of the arbiter seat; a low taste-share supports the coordination reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(evidential_vs_taste_reoccupation, empirical, 'Evidence-driven versus preference-driven share of the recovery layer.').

omega_variable(
    post_solution_persistence_driver,
    'What holds the standard up after the recovery problem was solved: institutional inertia, fused professional identity, or residual live functions (seminaries, diplomacy, classics as a discipline)?',
    'Track enrollment, appointment patterns, and institutional statements after critical editions stabilized; compare jurisdictions that dropped compulsory Latin with those that kept it.',
    'Pure inertia predicts continued theater-ratio growth toward vestigial maintenance; live residual functions predict stabilization at moderate theater; identity fusion predicts defense of the standard against contrary evidence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(post_solution_persistence_driver, empirical, 'Mechanism maintaining the arrangement after its founding problem was solved.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(correct_latin_kernel__hybrid_reading, 0, 42).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clk_hybrid_tr_t0, correct_latin_kernel__hybrid_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(clk_hybrid_tr_t6, correct_latin_kernel__hybrid_reading, theater_ratio, 6, 0.15).
narrative_ontology:measurement(clk_hybrid_tr_t12, correct_latin_kernel__hybrid_reading, theater_ratio, 12, 0.19).
narrative_ontology:measurement(clk_hybrid_tr_t18, correct_latin_kernel__hybrid_reading, theater_ratio, 18, 0.24).
narrative_ontology:measurement(clk_hybrid_tr_t24, correct_latin_kernel__hybrid_reading, theater_ratio, 24, 0.3).
narrative_ontology:measurement(clk_hybrid_tr_t30, correct_latin_kernel__hybrid_reading, theater_ratio, 30, 0.38).
narrative_ontology:measurement(clk_hybrid_tr_t36, correct_latin_kernel__hybrid_reading, theater_ratio, 36, 0.47).
narrative_ontology:measurement(clk_hybrid_tr_t42, correct_latin_kernel__hybrid_reading, theater_ratio, 42, 0.55).

% Extraction over time
narrative_ontology:measurement(clk_hybrid_be_t0, correct_latin_kernel__hybrid_reading, base_extractiveness, 0, 0.44).
narrative_ontology:measurement(clk_hybrid_be_t6, correct_latin_kernel__hybrid_reading, base_extractiveness, 6, 0.5).
narrative_ontology:measurement(clk_hybrid_be_t12, correct_latin_kernel__hybrid_reading, base_extractiveness, 12, 0.57).
narrative_ontology:measurement(clk_hybrid_be_t18, correct_latin_kernel__hybrid_reading, base_extractiveness, 18, 0.63).
narrative_ontology:measurement(clk_hybrid_be_t24, correct_latin_kernel__hybrid_reading, base_extractiveness, 24, 0.66).
narrative_ontology:measurement(clk_hybrid_be_t30, correct_latin_kernel__hybrid_reading, base_extractiveness, 30, 0.64).
narrative_ontology:measurement(clk_hybrid_be_t36, correct_latin_kernel__hybrid_reading, base_extractiveness, 36, 0.61).
narrative_ontology:measurement(clk_hybrid_be_t42, correct_latin_kernel__hybrid_reading, base_extractiveness, 42, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(clk_hybrid_su_t0, correct_latin_kernel__hybrid_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(clk_hybrid_su_t6, correct_latin_kernel__hybrid_reading, suppression_requirement, 6, 0.42).
narrative_ontology:measurement(clk_hybrid_su_t12, correct_latin_kernel__hybrid_reading, suppression_requirement, 12, 0.5).
narrative_ontology:measurement(clk_hybrid_su_t18, correct_latin_kernel__hybrid_reading, suppression_requirement, 18, 0.56).
narrative_ontology:measurement(clk_hybrid_su_t24, correct_latin_kernel__hybrid_reading, suppression_requirement, 24, 0.6).
narrative_ontology:measurement(clk_hybrid_su_t30, correct_latin_kernel__hybrid_reading, suppression_requirement, 30, 0.58).
narrative_ontology:measurement(clk_hybrid_su_t36, correct_latin_kernel__hybrid_reading, suppression_requirement, 36, 0.52).
narrative_ontology:measurement(clk_hybrid_su_t42, correct_latin_kernel__hybrid_reading, suppression_requirement, 42, 0.47).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(correct_latin_kernel__hybrid_reading, information_standard).
narrative_ontology:affects_constraint(correct_latin_kernel__hybrid_reading, correct_latin_kernel__continuity_reading).
narrative_ontology:affects_constraint(correct_latin_kernel__hybrid_reading, correct_latin_kernel__discontinuity_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'correct Latin' decomposes into three epsilon-invariant readings of one kernel. This hybrid story authors extraction 0.58 over the standing layered arrangement; the continuity sibling would author low extraction over the same referent (pure coordination, no victim set); the discontinuity sibling would author higher extraction (full reoccupation, larger victim set, heavier enforcement). Upstream/downstream: the continuity premise supplied the morphological trust that made the hybrid's partial recovery tractable, and the hybrid's recovery apparatus produced the critical editions the discontinuity program runs on.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
