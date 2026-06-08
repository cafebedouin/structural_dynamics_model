% ============================================================================
% CONSTRAINT STORY: rupture_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_rupture_reading, []).

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
 *   constraint_id: rupture_reading
 *   human_readable: Rupture Reading of Correct Latin (Humanist Reoccupation)
 *   domain: historical_linguistics/philology/intellectual_history
 *
 * SUMMARY:
 *   The rupture reading of 'correct Latin' holds that humanists in the 15th
 *   century reoccupied a dead linguistic kernel — classical Latin — from
 *   textual evidence, and that this reoccupation constituted a genuine
 *   epistemic break from medieval continuity. In this reading, medieval Latin
 *   is 'corrupt' (drifted from classical norms through unguided evolution),
 *   and the humanist project was a recovery of correct forms that had been
 *   lost. This reading instantiates a specific constraint: the authority to
 *   adjudicate 'correct Latin' shifts from living pedagogical practice
 *   (scholastic masters transmitting evolving usage) to textual
 *   reconstruction (humanist philologists recovering classical norms from
 *   manuscripts). The constraint coordinates (establishes a shared
 *   philological standard across Europe, enables the printing industry,
 *   grounds vernacular grammatical reform) while simultaneously extracting
 *   (delegitimates scholastic practitioners, suppresses living linguistic
 *   development, captures institutional authority through exclusive textual
 *   access claims). The extraction is asymmetric: humanist philologists
 *   benefit from positioning themselves as exclusive interpreters of the
 *   textual kernel; scholastic masters and vernacular grammarians bear the
 *   cost of delegitimation. The constraint's theater_ratio (0.58 at endpoint)
 *   reflects that by the early 16th century, much of classical Latin pedagogy
 *   has become performative signaling of institutional affiliation rather
 *   than functional communication or investigation. This is ONE reading of
 *   the kernel 'correct Latin' — sibling readings (continuity: medieval Latin
 *   as legitimate evolution; hybrid: partial gains and partial capture) would
 *   classify differently from the same historical material.
 *
 * KEY AGENTS:
 *   - Humanist Philologists: Primary beneficiary (institutional/arbitrage) — capture authority by claiming exclusive access to 'true' Latin via textual reconstruction; position themselves as gatekeepers of correct usage
 *   - Scholastic Masters: Primary victim (powerless/identity_locked) — professional identity constituted through mastery of medieval Latin; delegitimation as 'corrupt' collapses career structure; cannot exit without abandoning identity
 *   - Vernacular Grammarians: Secondary victim (moderate/constrained) — benefit from philological rigor enabling cross-vernacular work, but constrained by classical norms that delegitimate vernacular innovations
 *   - Printing Industry: Secondary beneficiary (institutional/mobile) — benefits from standardization reducing editorial variance and increasing market for uniform editions
 *   - University Reformers: Mixed position (organized/constrained) — leverage humanist standards to challenge scholastic curriculum, but constrained by rigidity of classical norms limiting pedagogical innovation
 *   - Living Practice Continuity: Abstract victim (powerless/trapped) — the evolutionary linguistic continuity from late antiquity through medieval period, delegitimated as 'corruption' by the rupture reading
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rupture_reading, 0.52).
domain_priors:suppression_score(rupture_reading, 0.68).
domain_priors:theater_ratio(rupture_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rupture_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(rupture_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(rupture_reading, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rupture_reading, tangled_rope).
narrative_ontology:human_readable(rupture_reading, "Rupture Reading of Correct Latin (Humanist Reoccupation)").
narrative_ontology:topic_domain(rupture_reading, "historical_linguistics/philology/intellectual_history").

domain_priors:requires_active_enforcement(rupture_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rupture_reading, 'a7bc1799-47d5-4ec8-aca4-0b8442e39d6a').
narrative_ontology:cs_kernel_codification('a7bc1799-47d5-4ec8-aca4-0b8442e39d6a', fixed_text).
narrative_ontology:cs_authority_grounding('a7bc1799-47d5-4ec8-aca4-0b8442e39d6a', lineage).
narrative_ontology:cs_interpretation_layer_present('a7bc1799-47d5-4ec8-aca4-0b8442e39d6a').
narrative_ontology:cs_reading_relation('a7bc1799-47d5-4ec8-aca4-0b8442e39d6a', rupture_reading__continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('a7bc1799-47d5-4ec8-aca4-0b8442e39d6a', rupture_reading__hybrid_reading, coexists_with).
narrative_ontology:cs_axiom('a7bc1799-47d5-4ec8-aca4-0b8442e39d6a', foundational, textual_recovery_primacy).
narrative_ontology:cs_axiom_status(textual_recovery_primacy, holdable).
narrative_ontology:cs_axiom_grounding('a7bc1799-47d5-4ec8-aca4-0b8442e39d6a', textual_recovery_primacy, empirically_contingent).
narrative_ontology:cs_axiom('a7bc1799-47d5-4ec8-aca4-0b8442e39d6a', foundational, medieval_corruption_thesis).
narrative_ontology:cs_axiom_status(medieval_corruption_thesis, holdable).
narrative_ontology:cs_axiom_grounding('a7bc1799-47d5-4ec8-aca4-0b8442e39d6a', medieval_corruption_thesis, empirically_contingent).
narrative_ontology:cs_axiom('a7bc1799-47d5-4ec8-aca4-0b8442e39d6a', secondary, living_tradition_delegitimacy).
narrative_ontology:cs_axiom_status(living_tradition_delegitimacy, holdable).
narrative_ontology:cs_axiom_grounding('a7bc1799-47d5-4ec8-aca4-0b8442e39d6a', living_tradition_delegitimacy, conventional).
narrative_ontology:cs_reference_frame('a7bc1799-47d5-4ec8-aca4-0b8442e39d6a', ciceronian_golden_age).
narrative_ontology:cs_drift_state('a7bc1799-47d5-4ec8-aca4-0b8442e39d6a', early_sixteenth_century, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('a7bc1799-47d5-4ec8-aca4-0b8442e39d6a', '').
narrative_ontology:cs_kernel_id(rupture_reading, correct_latin).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rupture_reading, humanist_philologists).
narrative_ontology:constraint_beneficiary(rupture_reading, printing_industry).
narrative_ontology:constraint_beneficiary(rupture_reading, university_reformers).
narrative_ontology:constraint_victim(rupture_reading, scholastic_masters).
narrative_ontology:constraint_victim(rupture_reading, vernacular_grammarians).
narrative_ontology:constraint_victim(rupture_reading, living_practice_continuity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SCHOLASTIC MASTER (SNARE) — Identity-locked: professional identity and pedagogical authority are constituted through mastery of medieval Latin usage. The humanist claim that this living tradition is 'corrupt' delegitimates the scholastic master's entire career structure. Exit would require abandoning not just Latin practice but the identity of 'master of arts.' High extraction: career prospects collapse as humanist standards capture university hiring.
constraint_indexing:constraint_classification(rupture_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(regional))).

% PERSPECTIVE 2: VERNACULAR GRAMMARIAN (TANGLED ROPE) — Benefits from the coordination function: standardized Latin grammar enables cross-vernacular philological work. But also constrained by the humanist standard's suppression of living usage — the grammarian's vernacular innovations are measured against a reconstructed classical norm rather than contemporary practice. Mixed experience: gains prestige from philological rigor while bearing cost of delegitimized vernacular forms.
constraint_indexing:constraint_classification(rupture_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: HUMANIST PHILOLOGIST (ROPE) — Primary beneficiary. Captures institutional authority by positioning classical texts as the exclusive gateway to 'correct' Latin. Arbitrage exit: can shift between patronage networks, university appointments, and printing-house collaborations. Experiences the constraint as coordination: standardizing Latin around classical models enables the republic of letters. Net beneficiary — extraction flows toward this agent.
constraint_indexing:constraint_classification(rupture_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 4: PRINTING INDUSTRY (ROPE) — Benefits from standardization: classical Latin norms reduce editorial variance and increase market for standardized textbooks and editions. Mobile exit: can shift production to vernacular markets if Latin demand falls. Coordination function dominates: the constraint solves a real market problem (which Latin to print?) with minimal extraction overhead.
constraint_indexing:constraint_classification(rupture_reading, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 5: UNIVERSITY REFORMERS (TANGLED ROPE) — Organized agents leveraging humanist standards to challenge scholastic curriculum. Benefits from the delegitimation of scholastic Latin (enables curriculum reform) but also constrained by the rigidity of classical norms (limits pedagogical innovation). Mixed experience: gains power to reshape institutions while bearing cost of enforcing a standard that admits no living development.
constraint_indexing:constraint_classification(rupture_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational perspective, the rupture reading both coordinates (establishes a shared philological standard across Europe) and extracts (suppresses living linguistic continuity and delegitimates practitioners of evolved forms). The textual reoccupation creates genuine cross-regional coordination while embedding asymmetric extraction through authority capture. The analytical classification is tangled_rope because both functions are structurally present and neither is reducible to the other.
constraint_indexing:constraint_classification(rupture_reading, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rupture_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(rupture_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(rupture_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(rupture_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(rupture_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The rupture reading's core claim — that medieval Latin is corrupt and classical Latin must be recovered from texts — creates substantial extraction through authority capture. Humanist philologists gain institutional power (university appointments, printing contracts, patronage) by delegitimating the scholastic tradition. Scholastic masters lose career viability as their living practice is declared invalid. However, extraction is not maximal because (a) genuine coordination gains exist (cross-regional philological standardization), (b) some scholastic practitioners successfully transition to humanist standards, and (c) vernacular alternatives emerge reducing total extraction from the Latin standard. Suppression (0.68): High. The rupture reading requires active suppression of medieval forms. University statutes enforce classical norms through examination. Printing industry editorial policy excludes medieval usage. The 'corruption' narrative is institutionalized, making alternatives structurally difficult. Suppression is not total because vernacular grammarians maintain some agency and resistance persists in scholastic strongholds, but exit options are severely constrained. Theater ratio (0.58): Moderate-high. By the early 16th century, significant pedagogical and publishing effort is performative enforcement of classical norms (Ciceronianism, debates over non-Ciceronian vocabulary) rather than functional communication. The theater has increased from genuine philological investigation (0.35 in early period) to credentialing ritual, though it has not reached piton levels (>0.70) because classical Latin pedagogy still transmits real linguistic knowledge alongside the performative enforcement.
 *
 * PERSPECTIVAL GAP:
 *   The rupture reading produces wide perspectival gaps. Humanist philologists see rope: they are solving the coordination problem of establishing correct Latin after medieval drift. Scholastic masters see snare: their living tradition is being delegitimated by an external authority claiming exclusive textual access. Vernacular grammarians see tangled_rope: genuine gains from standardization mixed with costs from norm rigidity. The analytical observer sees tangled_rope: both coordination and extraction are structurally present. The gap reveals that 'correct Latin' is not one constraint but a kernel with multiple readings. The rupture reading's higher extractiveness (0.52) compared to what a continuity reading would show (~0.25: medieval Latin as legitimate evolution, no delegitimation) reflects its specific institutional capture mechanism. The perspectival gap is the measurement: different structural positions experience radically different constraints from the same linguistic-political phenomenon.
 *
 * DIRECTIONALITY LOGIC:
 *   The rupture reading instantiates a specific directionality structure. Humanist philologists are primary beneficiaries: they collect institutional authority, patronage, and career advantage from the constraint's operation. Their d value is low (near 0.0) because extraction flows toward them. Scholastic masters are primary victims: their professional identity and career structure are constituted through medieval Latin mastery, and the 'corruption' framing delegitimates this entirely. Their d value is high (near 1.0) because extraction flows away from them with no compensation. The identity_locked exit option reflects that exit would require abandoning not just Latin practice but professional identity itself — the scholastic master cannot simply 'learn' classical Latin without conceding that their previous mastery was fraudulent. Vernacular grammarians have intermediate d: they benefit from philological standardization enabling cross-vernacular work but bear costs from delegitimation of vernacular innovations measured against classical norms. University reformers have intermediate d: they gain power to reshape curricula by leveraging humanist standards but are constrained by the rigidity of classical norms. The printing industry has low d: standardization solves a real market problem with minimal extraction. The analytical observer sees both coordination and extraction as structurally irreducible, yielding tangled_rope classification.
 *
 * MANDATROPHY ANALYSIS:
 *   The rupture reading resolves mandatrophy by revealing that the constraint's dual function — coordinating philological standards while capturing institutional authority — is not a misclassification but the structural reality. The humanist claim that classical Latin is 'correct' and medieval Latin is 'corrupt' both establishes a shared standard (coordination) and delegitimates competitors (extraction). The tangled_rope classification captures this: the constraint genuinely solves a coordination problem (which Latin to teach, print, and use for cross-regional communication?) while embedding asymmetric extraction (authority flows to those who control textual reconstruction, away from those who practiced living continuity). The theater_ratio (0.58) indicates that by the early 16th century, much of the enforcement is performative — Ciceronianism is credentialing ritual, not functional necessity. But the coordination function persists: standardized Latin enables the republic of letters. The constraint is not 'really' pure coordination mislabeled as extraction, nor 'really' pure extraction disguised as coordination. It is both, from different structural positions, and the indexical classification system measures this directly.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Is the humanist claim that medieval Latin is ''corrupt'' a discovery of linguistic fact or a constructed narrative serving institutional interests?',
    'Historical analysis of (a) which medieval forms were productive innovations vs degradations, (b) whether humanist Latin was actually closer to classical usage or merely different, (c) career and patronage flows around the humanist/scholastic divide.',
    'If discovery: the rupture reading is describing a genuine epistemic correction (lower extractiveness, closer to mountain). If constructed: the rupture reading is describing authority capture through narrative control (higher extractiveness, snare from more perspectives).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Whether medieval corruption is linguistic fact or constructed narrative').

omega_variable(
    textual_accessibility,
    'Were classical texts genuinely inaccessible to medieval scholars, or was the humanist claim of ''rediscovery'' itself a rhetorical move?',
    'Manuscript evidence: which classical texts circulated in medieval universities, which grammatical forms were taught, what continuity existed between late antique and medieval Latin pedagogy. Compare humanist claims of ''recovery'' against actual manuscript transmission records.',
    'If genuinely inaccessible: the rupture reading describes a coordination breakthrough (reestablishing lost standards). If continuously available: the rupture reading describes a jurisdictional capture (delegitimating a living tradition by claiming it lost access to sources it actually possessed).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(textual_accessibility, empirical, 'Whether classical texts were genuinely lost to medieval scholars').

omega_variable(
    sibling_reading_structural_delta,
    'What structural elements distinguish this rupture reading from the continuity reading and hybrid reading of the same kernel?',
    'Cross-reading comparison: the continuity reading would likely show lower extractiveness (medieval Latin as legitimate evolution, no delegitimation) and different beneficiary sets (scholastic continuity as coordination, not extraction). The hybrid reading would show intermediate extractiveness (partial delegitimation offset by genuine philological gains). The rupture reading''s higher extractiveness derives from full delegitimation of the living tradition.',
    'The perspectival gap between sibling readings reveals that ''correct Latin'' is not one constraint but a family of constraints instantiated by different authoritative claims. The rupture reading''s tangled_rope classification reflects its specific institutional capture mechanism; other readings would classify differently from the same base phenomenon.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_structural_delta, conceptual, 'How sibling readings partition the same kernel into different constraint structures').

omega_variable(
    living_tradition_recovery,
    'Could the delegitimated scholastic practitioners recover legitimacy by repositioning their practice as ''living Latin'' rather than competing for ''classical correctness''?',
    'Historical evidence of resistance strategies: did any scholastic masters successfully reframe their practice as a legitimate continuation rather than a degraded form? What institutional conditions would have enabled this reframing?',
    'If recovery possible: the identity_locked exit option for scholastic masters is overstated — some agency remained. If recovery impossible: the identity lock is structural — once ''corruption'' framing is institutionalized, no reframing strategy succeeds within the same cultural context.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(living_tradition_recovery, empirical, 'Whether scholastic masters could escape delegitimation through reframing').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rupture_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rupture_tr_t0, rupture_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(rupture_tr_t3, rupture_reading, theater_ratio, 3, 0.48).
narrative_ontology:measurement(rupture_tr_t6, rupture_reading, theater_ratio, 6, 0.58).
narrative_ontology:measurement(rupture_tr_t10, rupture_reading, theater_ratio, 10, 0.62).

% Extraction over time
narrative_ontology:measurement(rupture_be_t0, rupture_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(rupture_be_t3, rupture_reading, base_extractiveness, 3, 0.44).
narrative_ontology:measurement(rupture_be_t6, rupture_reading, base_extractiveness, 6, 0.52).
narrative_ontology:measurement(rupture_be_t10, rupture_reading, base_extractiveness, 10, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(rupture_su_t0, rupture_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(rupture_su_t3, rupture_reading, suppression_requirement, 3, 0.58).
narrative_ontology:measurement(rupture_su_t6, rupture_reading, suppression_requirement, 6, 0.68).
narrative_ontology:measurement(rupture_su_t10, rupture_reading, suppression_requirement, 10, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rupture_reading, information_standard).
narrative_ontology:affects_constraint(rupture_reading, continuity_reading).
narrative_ontology:affects_constraint(rupture_reading, hybrid_reading).
narrative_ontology:affects_constraint(rupture_reading, printing_standardization).
narrative_ontology:affects_constraint(rupture_reading, vernacular_grammar_emergence).

% DUAL FORMULATION NOTE:
% The rupture reading is one member of the 'correct Latin' kernel family. It affects (influences but does not foreclose) the continuity and hybrid readings, which are sibling constraints instantiated from the same kernel. It also affects downstream constraints: printing standardization (the rupture reading's classical norms drive editorial policy) and vernacular grammar emergence (classical Latin's prestige model shapes vernacular grammatical frameworks).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
