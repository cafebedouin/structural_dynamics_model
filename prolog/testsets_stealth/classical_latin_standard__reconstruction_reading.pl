% ============================================================================
% CONSTRAINT STORY: classical_latin_standard__reconstruction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_classical_latin_standard__reconstruction_reading, []).

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
 *   constraint_id: classical_latin_standard__reconstruction_reading
 *   human_readable: Humanist Reconstruction Standard for Correct Latin
 *   domain: historical_linguistics/philology/commitment_systems
 *
 * SUMMARY:
 *   Between Petrarch's ad fontes program and the mid-fifteenth-century
 *   consolidation of humanist schooling, a new answer to 'what is correct
 *   Latin?' displaced the old one: correct Latin is the Classical form,
 *   recoverable only through philological archaeology — discontinuous return
 *   to the textual sources — with medieval usage reclassified as drift, hence
 *   corruption. The arrangement solved a real problem (regional divergence,
 *   corrupted transmission) while transferring the warrant of linguistic
 *   correctness from practice communities to a credentialed elite, and
 *   building enforcement through reformed curricula, the critical-edition
 *   trade, and patronage. CONSTRAINT FAMILY: the colloquial label 'correct
 *   Latin' decomposes per the epsilon-invariance principle into three linked
 *   stories — this reconstruction reading (epsilon 0.74 over the
 *   reconstructionist arrangement itself), the continuity reading (which
 *   authors low epsilon over the living-practice arrangement it defends), and
 *   the hybrid reading (intermediate). Each story authors its own epsilon
 *   over its own standing arrangement; they are linked via
 *   network.affects_constraints, with this reading upstream of the hybrid
 *   settlement, which concedes the textual anchor this reading established.
 *
 * KEY AGENTS:
 *   - humanist_philologists: agenda-setting beneficiary (organized/identity_locked) — set and administer the recovered norm; their authority is the method
 *   - critical_edition_printers: commercial beneficiary (organized/mobile) — collect revenue from the standard's school and library markets
 *   - elite_patrons: incidental beneficiary (powerful/mobile) — purchase prestige and secretariat quality
 *   - scholastic_theologians: primary institutional target (institutional/identity_locked) — their technical Latin is reclassified as barbarous
 *   - curial_and_chancery_scribes: secondary target (moderate/constrained) — working formulas branded corrupt, retraining costly
 *   - parish_clergy: diffuse target (powerless/trapped) — bear status costs with no retraining path
 *   - technical_tradition_practitioners: excluded seat (organized/constrained) — adjudicated without representation in norm-setting circles
 *   - modern_historical_linguists: analytical observer (analytical/analytical) — see both medieval and classical Latin as lawful objects
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(classical_latin_standard__reconstruction_reading, 0.74).
domain_priors:suppression_score(classical_latin_standard__reconstruction_reading, 0.78).
domain_priors:theater_ratio(classical_latin_standard__reconstruction_reading, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(classical_latin_standard__reconstruction_reading, extractiveness, 0.74).
narrative_ontology:constraint_metric(classical_latin_standard__reconstruction_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(classical_latin_standard__reconstruction_reading, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(classical_latin_standard__reconstruction_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(classical_latin_standard__reconstruction_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(classical_latin_standard__reconstruction_reading, tangled_rope).
narrative_ontology:human_readable(classical_latin_standard__reconstruction_reading, "Humanist Reconstruction Standard for Correct Latin").
narrative_ontology:topic_domain(classical_latin_standard__reconstruction_reading, "historical_linguistics/philology/commitment_systems").

domain_priors:requires_active_enforcement(classical_latin_standard__reconstruction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(classical_latin_standard__reconstruction_reading, 'ed4230cd-50a5-4920-8693-7b8b4e600d58').
narrative_ontology:cs_kernel_codification('ed4230cd-50a5-4920-8693-7b8b4e600d58', fixed_text).
narrative_ontology:cs_authority_grounding('ed4230cd-50a5-4920-8693-7b8b4e600d58', extraction).
narrative_ontology:cs_interpretation_layer_present('ed4230cd-50a5-4920-8693-7b8b4e600d58').
narrative_ontology:cs_reading_relation('ed4230cd-50a5-4920-8693-7b8b4e600d58', classical_latin_standard__continuity_reading, forecloses).
narrative_ontology:cs_reading_relation('ed4230cd-50a5-4920-8693-7b8b4e600d58', classical_latin_standard__hybrid_reading, influences).
narrative_ontology:cs_axiom('ed4230cd-50a5-4920-8693-7b8b4e600d58', foundational, correctness_requires_discontinuous_textual_recovery).
narrative_ontology:cs_axiom_status(correctness_requires_discontinuous_textual_recovery, holdable).
narrative_ontology:cs_axiom_grounding('ed4230cd-50a5-4920-8693-7b8b4e600d58', correctness_requires_discontinuous_textual_recovery, empirically_contingent).
narrative_ontology:cs_axiom('ed4230cd-50a5-4920-8693-7b8b4e600d58', secondary, medieval_usage_is_corruption_not_development).
narrative_ontology:cs_axiom_status(medieval_usage_is_corruption_not_development, overridden).
narrative_ontology:cs_axiom_grounding('ed4230cd-50a5-4920-8693-7b8b4e600d58', medieval_usage_is_corruption_not_development, empirically_contingent).
narrative_ontology:cs_reference_frame('ed4230cd-50a5-4920-8693-7b8b4e600d58', recovered_classical_corpus_norm).
narrative_ontology:cs_drift_state('ed4230cd-50a5-4920-8693-7b8b4e600d58', contemporary_descriptive_linguistics_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('ed4230cd-50a5-4920-8693-7b8b4e600d58', '').
narrative_ontology:cs_kernel_id(classical_latin_standard__reconstruction_reading, classical_latin_standard).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(classical_latin_standard__reconstruction_reading, humanist_philologists).
narrative_ontology:constraint_beneficiary(classical_latin_standard__reconstruction_reading, critical_edition_printers).
narrative_ontology:constraint_beneficiary(classical_latin_standard__reconstruction_reading, elite_patrons).
narrative_ontology:constraint_victim(classical_latin_standard__reconstruction_reading, scholastic_theologians).
narrative_ontology:constraint_victim(classical_latin_standard__reconstruction_reading, curial_and_chancery_scribes).
narrative_ontology:constraint_victim(classical_latin_standard__reconstruction_reading, parish_clergy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Recover and emend classical texts, compose grammars, lexica, and usage manuals (Valla's Elegantiae is the type case), staff the reformed schools, and fix the norm by citation of Cicero and the classical corpus. Their professional standing is constituted by possession of the recovery method itself; leaving the arrangement would mean renouncing the authority their training confers, so they remain inside it and administer it.
narrative_ontology:constraint_stakeholder(classical_latin_standard__reconstruction_reading, humanist_philologists, agenda_setter,
    organized, biographical, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(classical_latin_standard__reconstruction_reading, humanist_philologists, beneficiary).

% Profit from the demand for corrected classical texts and from the school market the standard creates, selling authorized editions and classroom grammars. They collect revenue without administering the norm and can shift their catalogues to other genres if demand moves.
narrative_ontology:constraint_stakeholder(classical_latin_standard__reconstruction_reading, critical_edition_printers, beneficiary,
    organized, biographical, mobile, continental).

% Princes, cardinals, and civic oligarchies fund humanists for prestige, polished secretariats, and cultural capital. They gain standing from association with the recovered standard and can withdraw patronage at little personal cost.
narrative_ontology:constraint_stakeholder(classical_latin_standard__reconstruction_reading, elite_patrons, beneficiary,
    powerful, generational, mobile, continental).

% University theology faculties whose technical vocabulary, built up over generations of commentary and disputation, is reclassified as barbarous by the new norm. Their authority is bound to the transmitted terminology; recasting a century of technical apparatus in classical idiom is prohibitively costly, while refusing means ceding status to the newly credentialed.
narrative_ontology:constraint_stakeholder(classical_latin_standard__reconstruction_reading, scholastic_theologians, payer,
    institutional, generational, identity_locked, continental).

% Draft bulls, briefs, and legal instruments in formulaic Latin descended from late-antique chancery practice. The new norm brands their working formulas corrupt. Retraining in classical composition is available but costly and unevenly rewarded, and their employment is tied to offices that adopt humanist styles on their own schedules.
narrative_ontology:constraint_stakeholder(classical_latin_standard__reconstruction_reading, curial_and_chancery_scribes, payer,
    moderate, biographical, constrained, continental).

% Lower clergy administer sacraments and keep parochial records in inherited liturgical and notarial Latin. They bear the status cost of being pronounced unlettered by the new criterion, ordination binds them to their posts, and no realistic path to philological retraining exists at their station.
narrative_ontology:constraint_stakeholder(classical_latin_standard__reconstruction_reading, parish_clergy, payer,
    powerless, biographical, trapped, regional).

% Legal doctors, physicians, and notary guilds whose working Latin is adjudicated by the new norm without any seat in the academies, schools, or patronage circles where the norm is set. They learn the verdicts secondhand through licensing requirements and censorship, and cannot appeal the rulings to the bodies that issued them.
narrative_ontology:constraint_stakeholder(classical_latin_standard__reconstruction_reading, technical_tradition_practitioners, excluded,
    organized, generational, constrained, continental).

% Study medieval and classical Latin as equally lawful objects of description, document that the condemned 'corruption' was regular language change, and assess the standard's social effects from outside its enforcement machinery.
narrative_ontology:constraint_stakeholder(classical_latin_standard__reconstruction_reading, modern_historical_linguists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(classical_latin_standard__reconstruction_reading, humanist_philologists).
narrative_ontology:fixing_cost_class(classical_latin_standard__reconstruction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single textually anchored written norm for Latin across polities and generations: it addresses divergent regional usages, unstable transmission of authoritative texts, and the absence of a fixed, citable reference for scholarly precision.
% TRANSFER_FUNCTION: Moves linguistic authority, educational resources, and employable status from hereditary practice communities (scholastic faculties, chanceries, parishes) to holders of philological training; moves the warrant of correctness from continuous usage to textual citation.
% ABSENT_VOICES: Technical-tradition practitioners (law, medicine, notariate) and the vernacular-adjacent literate classes would object that their working registers serve them and were condemned without consultation; they sit outside the academies, schools, and patronage networks where the norm was fixed, and reach the verdicts only as recipients.
% DISAPPEARANCE_RATIONALE: If the reconstructionist standard vanished overnight, the humanist school curriculum would lose its spine, the critical-edition trade would lose its warrant, careers priced by philological scarcity would devalue, and written Latin would fragment back toward regional and technical registers pending some other coordination. The Republic of Letters' common reference point is an arrangement many institutions depend on.
% FOUNDING_PROBLEM: Fourteenth-century learned Latin had drifted into regionally divergent forms, and the authoritative ancient texts were corrupted by centuries of copying; scholars lacked a stable reference for precision. The arrangement was built to restore a fixed norm by discontinuous return to the sources.
% FOUNDING_PROBLEM_CORROBORATION: Manuscript-stemmatic evidence and the history of textual criticism corroborate from outside the benefiting parties that copyist corruption and regional divergence were real fourteenth-century problems; historians of scholarship and descriptive linguists further attest that the editorial half of the problem was substantially solved by the very apparatus the standard built, while the norm-setting half persists on institutional momentum. No party outside the humanist lineage attests that the full reconstruction apparatus remains necessary for the original problem.
narrative_ontology:disappearance_verdict(classical_latin_standard__reconstruction_reading, world_rearranges).
narrative_ontology:founding_problem_status(classical_latin_standard__reconstruction_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(classical_latin_standard__reconstruction_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(classical_latin_standard__reconstruction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(classical_latin_standard__reconstruction_reading, 0.74, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(classical_latin_standard__reconstruction_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(classical_latin_standard__reconstruction_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(classical_latin_standard__reconstruction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.74 at interval end) because the arrangement transfers linguistic authority wholesale from practice communities to holders of the recovery method, decoupled from communicative necessity — medieval Latin demonstrably served its users. Suppression is higher still (0.78) because persistence depended on active machinery: reformed curricula, print norms, patronage discipline, and sustained ridicule of 'barbarous' usage; alternatives were not argued down but institutionally starved. Theater ratio is moderate-low (0.32): the philology underneath is real (stemmatic method, emendation, grammar-writing all function), but a growing share of activity is performative classicism — Ciceronian display, purity contests — that maintains the boundary rather than the texts. Accessibility_collapse is moderate (0.45): unlike a natural law, the standard never collapsed its alternatives — liturgical and technical Latins persisted for centuries alongside it — so alternatives remained visible and partly usable throughout. Resistance is substantial (0.6): scholastic counterattacks, curial conservatism, and stubborn regional practice met the standard continuously. The measurement series run on ONE shared time grid (all three metrics at all six points, 1350-1450); trajectories are monotone ratchets, not cycles — enforcement infrastructure matured and hardened as schooling and print scaled, with no oscillation phase, so no intermittent-reinforcement reading applies. Claim/metric independence: claimed_type is tangled_rope because I judge both a genuine coordination function and asymmetric extraction structurally present; the metrics are authored independently as descriptive facts, and the engine computes per-seat types from the structural data.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the agenda-setter seat compute differently from the same structure. From the humanist seat the arrangement is a scholarly achievement it built and staffs — a common reference, rigorous method, careers — computing near-rope. From the scholastic and chancery seats the same structure operates as a verdict on their life's work delivered by people who never practiced their registers — computing near-snare. The excluded technical practitioners experience arbitrariness: rules set in rooms they cannot enter. Same-level dynamics matter: scholastic theologians (institutional power) and humanist philologists (organized power) held comparable cultural standing, yet the constraint re-priced their assets — the humanists' scarce skill was exactly what the standard made valuable, the scholasts' accumulated capital was exactly what it deprecated — so power diverged despite equal nominal rank. Inter-institutional timing differed too: Italian and French chanceries humanized earliest, universities resisted longest, the curia preserved archaic forms well past the interval.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary/victim declarations drive the derivation and no overrides are needed. Humanist philologists sit near the beneficiary pole (d near 0): the standard subsidizes exactly their asset, and identity_lock amplifies their stake — their professional selves are fused with the method, so they defend it beyond any narrow revenue calculation. Printers and patrons are incidental beneficiaries with mobile exit, damping their effective exposure. Scholastic theologians sit near the target pole despite institutional power: identity_lock traps them (their authority IS the deprecated terminology), pushing d toward full-target. Chancery scribes are constrained targets; parish clergy, powerless and trapped, sit nearest the full-target end — the standard's costs land hardest where exit is weakest. Coalition check: the three victim classes could in principle have coaligned (shared interest in defending practice-warrant), and historically they resisted — but fragmentarily, divided by institutional rivalry between faculties, chanceries, and parishes, which the standard's backers exploited. Suppression is authored as a raw structural property (unscaled); only extractiveness gets scaled by directionality and scope in the engine's computation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem had two halves: corrupted transmission and regional divergence. The apparatus this standard built — stemmatic method, critical editions, reference grammars — substantially SOLVED the first half within roughly a century of Valla; texts became recoverable. The second half dissolved the moment the standard succeeded, since a fixed norm ends divergence by fiat. What persisted after the diagnostic mandate was fulfilled is the norm-setting and gatekeeping function: the credential, the curriculum, the boundary between latinate and barbarous. Classifying this as tangled_rope rather than snare preserves the real coordination achievement (a common citable reference genuinely enabled the Republic of Letters); refusing rope preserves the asymmetry (authority moved from practice to credential, and stayed there after the need passed). The founding_problem_status 'contested' encodes exactly this: the editorial mandate is dead, the normative mandate is disputed, and the arrangement persists on the disputed half.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_underdetermination,
    'Is the reconstruction reading the correct instantiation of the classical_latin_standard kernel, or do the continuity or hybrid readings better model what ''correct Latin'' constrains? The disagreement is located in the warrant of correctness: exclusive textual recovery (this reading), unbroken practice transmission (continuity), or a weighted mix conceding technical/ecclesiastical developments (hybrid).',
    'Comparative corpus analysis of which reading''s predictions match actual stabilization outcomes across domains (scholarly, chancery, liturgical), plus genealogical analysis of which warrant the enforcement machinery actually invoked.',
    'Adopting the continuity reading collapses the victim set (no user''s Latin is incorrect) and drops effective extraction toward coordination-cost levels; adopting the hybrid reading shrinks the victim set to non-exempt domains and yields intermediate extraction. This story''s classification holds only under the reconstruction instantiation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_underdetermination, conceptual, 'This constraint is one reading of the classical_latin_standard kernel; sibling readings would restructure beneficiaries and victims.').

omega_variable(
    corruption_vs_legitimate_drift,
    'Is medieval divergence from classical usage genuinely decay relative to a functional norm (''corruption''), or ordinary language change that only appears deviant under an anachronistic benchmark?',
    'Uniformitarian historical-linguistic analysis: compare the mechanisms of medieval change against attested change in undeniably healthy languages; test whether the classical benchmark performs any communicative function the drifted forms could not.',
    'If the drift is legitimate change, the delegitimization premise fails, the constraint loses its corrective warrant, and its residual function is gatekeeping — pushing the classification toward the pure-extraction end. If genuine decay relative to a needed standard, the coordination claim strengthens and part of the measured extraction is the price of restoration.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(corruption_vs_legitimate_drift, conceptual, 'Whether the constraint''s core premise (drift as corruption) is a discovery or a benchmark artifact.').

omega_variable(
    suppression_structural_vs_internalized,
    'How much of the measured suppression is structural (control of schools, print norms, patronage, curricula) versus internalized (trained shame about ''barbarisms'', latinity anxiety that persists after institutional pressure lifts)?',
    'Post-exit trajectory: examine registers and communities that left the standard''s jurisdiction (post-Reformation vernacular institutions, secularized chanceries) — if status anxiety about non-classical Latin persisted after enforcement capacity withdrew, the internalized share is substantial.',
    'If internalized, effective suppression exceeds the structural measure — targets carry the standard''s verdicts with them after exit, raising the cost side of every seat computation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Partition of suppression into enforced versus self-administered components.').

omega_variable(
    norm_necessity_separability,
    'Is a textually anchored classical norm structurally necessary for scholarly precision and stable transmission, or is that function separable from the delegitimization of living practice communities?',
    'Natural experiment across domains: compare fields that kept classical anchoring without full delegitimization of practitioner usage (later scientific nomenclature, editorial conventions) against domains with total delegitimization, and measure whether precision outcomes differ.',
    'If separable, the delegitimization component is pure gatekeeping rent layered on a real coordination function; if inseparable, part of the extraction is the price of the standard itself.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(norm_necessity_separability, conceptual, 'Whether the coordination and delegitimization components of the standard are structurally separable.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(classical_latin_standard__reconstruction_reading, 1350, 1450).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cls_reconstruction_tr_t1350, classical_latin_standard__reconstruction_reading, theater_ratio, 1350, 0.14).
narrative_ontology:measurement(cls_reconstruction_tr_t1370, classical_latin_standard__reconstruction_reading, theater_ratio, 1370, 0.17).
narrative_ontology:measurement(cls_reconstruction_tr_t1390, classical_latin_standard__reconstruction_reading, theater_ratio, 1390, 0.21).
narrative_ontology:measurement(cls_reconstruction_tr_t1410, classical_latin_standard__reconstruction_reading, theater_ratio, 1410, 0.25).
narrative_ontology:measurement(cls_reconstruction_tr_t1430, classical_latin_standard__reconstruction_reading, theater_ratio, 1430, 0.29).
narrative_ontology:measurement(cls_reconstruction_tr_t1450, classical_latin_standard__reconstruction_reading, theater_ratio, 1450, 0.32).

% Extraction over time
narrative_ontology:measurement(cls_reconstruction_be_t1350, classical_latin_standard__reconstruction_reading, base_extractiveness, 1350, 0.46).
narrative_ontology:measurement(cls_reconstruction_be_t1370, classical_latin_standard__reconstruction_reading, base_extractiveness, 1370, 0.54).
narrative_ontology:measurement(cls_reconstruction_be_t1390, classical_latin_standard__reconstruction_reading, base_extractiveness, 1390, 0.61).
narrative_ontology:measurement(cls_reconstruction_be_t1410, classical_latin_standard__reconstruction_reading, base_extractiveness, 1410, 0.67).
narrative_ontology:measurement(cls_reconstruction_be_t1430, classical_latin_standard__reconstruction_reading, base_extractiveness, 1430, 0.71).
narrative_ontology:measurement(cls_reconstruction_be_t1450, classical_latin_standard__reconstruction_reading, base_extractiveness, 1450, 0.74).

% Suppression requirement over time
narrative_ontology:measurement(cls_reconstruction_su_t1350, classical_latin_standard__reconstruction_reading, suppression_requirement, 1350, 0.5).
narrative_ontology:measurement(cls_reconstruction_su_t1370, classical_latin_standard__reconstruction_reading, suppression_requirement, 1370, 0.57).
narrative_ontology:measurement(cls_reconstruction_su_t1390, classical_latin_standard__reconstruction_reading, suppression_requirement, 1390, 0.63).
narrative_ontology:measurement(cls_reconstruction_su_t1410, classical_latin_standard__reconstruction_reading, suppression_requirement, 1410, 0.69).
narrative_ontology:measurement(cls_reconstruction_su_t1430, classical_latin_standard__reconstruction_reading, suppression_requirement, 1430, 0.74).
narrative_ontology:measurement(cls_reconstruction_su_t1450, classical_latin_standard__reconstruction_reading, suppression_requirement, 1450, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(classical_latin_standard__reconstruction_reading, identity_coordination).
narrative_ontology:affects_constraint(classical_latin_standard__reconstruction_reading, classical_latin_standard__continuity_reading).
narrative_ontology:affects_constraint(classical_latin_standard__reconstruction_reading, classical_latin_standard__hybrid_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the label 'correct Latin' decomposes into three structurally distinct constraints per the epsilon-invariance principle. This reconstruction reading carries high epsilon (systematic delegitimization of practice-based authority, new gatekeeping class); the continuity reading authors low epsilon over the living-practice arrangement; the hybrid reading sits between with a partitioned victim set. Upstream/downstream: the continuity arrangement is the pre-humanist baseline this reading attacked; this reading is upstream of the hybrid settlement, which concedes its textual anchor. Each member links the others via affects_constraints; failure or revision of this reading propagates to both siblings' legitimacy conditions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
