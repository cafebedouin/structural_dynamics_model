% ============================================================================
% CONSTRAINT STORY: correct_latin_kernel__continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_correct_latin_kernel__continuity_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: correct_latin_kernel__continuity_reading
 *   human_readable: Continuity Reading of Correct Latin: Medieval Latin as Naturally Evolved Classical Latin
 *   domain: historical_linguistics/philology/intellectual_history
 *
 * SUMMARY:
 *   This story instantiates the continuity reading of the
 *   correct_latin_kernel: the claim that Medieval Latin is Classical Latin
 *   having undergone ordinary, internally-driven linguistic change —
 *   analogous to any language's diachronic drift — such that apparent
 *   'reconstruction' by later scholars was internal correction of an unbroken
 *   system, not recovery of a lost one. This reading benefits the medieval
 *   scribal and ecclesiastical Latin tradition (whose usage is validated as
 *   legitimate rather than corrupt) and the comparative-philological research
 *   program built on unbroken diachronic modeling. It imposes costs on the
 *   humanist reform tradition, whose entire diagnostic and pedagogical
 *   apparatus depended on treating medieval usage as deviation requiring
 *   textual correction — under this reading, humanist reform is recast as
 *   prescriptive purism rather than legitimate restoration. The
 *   discontinuity_reading and hybrid_reading are NOT part of this story; they
 *   are separate constraints with their own ε, beneficiaries, and metrics,
 *   linked via network.affects_constraints.
 *
 * KEY AGENTS:
 *   - medieval_latin_scribal_tradition: primary beneficiary — its usage is validated as legitimate continuation
 *   - romance_philologists_of_continuity: agenda-setters who author and administer the continuity model
 *   - medieval_church_institutional_latin: beneficiary whose doctrinal-continuity claims are underwritten
 *   - renaissance_humanist_scholarship: primary target — its founding diagnostic claim is recast as purism
 *   - classical_purist_pedagogy: secondary target — its curricular authority is undermined
 *   - students_taught_ciceronian_norms: powerless downstream payer — trained judgments lose evaluative basis
 *   - manuscript_paleographers: analytical observer supplying evidence to all readings without adjudicating
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(correct_latin_kernel__continuity_reading, 0.42).
domain_priors:suppression_score(correct_latin_kernel__continuity_reading, 0.55).
domain_priors:theater_ratio(correct_latin_kernel__continuity_reading, 0.5).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(correct_latin_kernel__continuity_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(correct_latin_kernel__continuity_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(correct_latin_kernel__continuity_reading, theater_ratio, 0.5).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(correct_latin_kernel__continuity_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(correct_latin_kernel__continuity_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(correct_latin_kernel__continuity_reading, tangled_rope).
narrative_ontology:human_readable(correct_latin_kernel__continuity_reading, "Continuity Reading of Correct Latin: Medieval Latin as Naturally Evolved Classical Latin").
narrative_ontology:topic_domain(correct_latin_kernel__continuity_reading, "historical_linguistics/philology/intellectual_history").

domain_priors:requires_active_enforcement(correct_latin_kernel__continuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(correct_latin_kernel__continuity_reading, '10344efb-a8aa-4e92-92e9-9d31fbda946b').
narrative_ontology:cs_kernel_codification('10344efb-a8aa-4e92-92e9-9d31fbda946b', distributed).
narrative_ontology:cs_authority_grounding('10344efb-a8aa-4e92-92e9-9d31fbda946b', practice).
narrative_ontology:cs_interpretation_layer_present('10344efb-a8aa-4e92-92e9-9d31fbda946b').
narrative_ontology:cs_reading_relation('10344efb-a8aa-4e92-92e9-9d31fbda946b', correct_latin_kernel__discontinuity_reading, forecloses).
narrative_ontology:cs_reading_relation('10344efb-a8aa-4e92-92e9-9d31fbda946b', correct_latin_kernel__hybrid_reading, influences).
narrative_ontology:cs_axiom('10344efb-a8aa-4e92-92e9-9d31fbda946b', foundational, unbroken_systemic_identity_across_medieval_period).
narrative_ontology:cs_axiom_status(unbroken_systemic_identity_across_medieval_period, holdable).
narrative_ontology:cs_axiom_grounding('10344efb-a8aa-4e92-92e9-9d31fbda946b', unbroken_systemic_identity_across_medieval_period, empirically_contingent).
narrative_ontology:cs_axiom('10344efb-a8aa-4e92-92e9-9d31fbda946b', foundational, reconstruction_is_internal_not_external_correction).
narrative_ontology:cs_axiom_status(reconstruction_is_internal_not_external_correction, holdable).
narrative_ontology:cs_axiom_grounding('10344efb-a8aa-4e92-92e9-9d31fbda946b', reconstruction_is_internal_not_external_correction, empirically_contingent).
narrative_ontology:cs_axiom('10344efb-a8aa-4e92-92e9-9d31fbda946b', secondary, humanist_restoration_recast_as_prescriptive_purism).
narrative_ontology:cs_axiom_status(humanist_restoration_recast_as_prescriptive_purism, holdable).
narrative_ontology:cs_axiom_grounding('10344efb-a8aa-4e92-92e9-9d31fbda946b', humanist_restoration_recast_as_prescriptive_purism, conventional).
narrative_ontology:cs_reference_frame('10344efb-a8aa-4e92-92e9-9d31fbda946b', unbroken_diachronic_latin_system).
narrative_ontology:cs_drift_state('10344efb-a8aa-4e92-92e9-9d31fbda946b', post_comparative_romance_linguistics_consolidation, gap(revival_pressure, substantial, false)).
narrative_ontology:cs_created_at('10344efb-a8aa-4e92-92e9-9d31fbda946b', '').
narrative_ontology:cs_kernel_id(correct_latin_kernel__continuity_reading, correct_latin_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(correct_latin_kernel__continuity_reading, medieval_latin_scribal_tradition).
narrative_ontology:constraint_beneficiary(correct_latin_kernel__continuity_reading, romance_philologists_of_continuity).
narrative_ontology:constraint_beneficiary(correct_latin_kernel__continuity_reading, medieval_church_institutional_latin).
narrative_ontology:constraint_victim(correct_latin_kernel__continuity_reading, renaissance_humanist_scholarship).
narrative_ontology:constraint_victim(correct_latin_kernel__continuity_reading, classical_purist_pedagogy).
narrative_ontology:constraint_victim(correct_latin_kernel__continuity_reading, students_taught_ciceronian_norms).
narrative_ontology:constraint_vindicates(correct_latin_kernel__continuity_reading, linguistic_naturalism_of_change).
narrative_ontology:constraint_vindicates(correct_latin_kernel__continuity_reading, internal_correction_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Chancery clerks, monastic copyists, and ecclesiastical writers whose working Latin (analytic case marking creep, vocabulary drift, altered syntax) is validated as authentic continuation of Classical Latin rather than corruption. Their professional legitimacy and the institutional memory of their scriptoria depend on Medieval Latin being read as the same language evolving, not a broken descendant needing correction from outside.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__continuity_reading, medieval_latin_scribal_tradition, beneficiary,
    institutional, civilizational, identity_locked, continental).

% Historical linguists who model Latin-to-Romance change as an unbroken diachronic chain. They author the continuity framework, publish the comparative reconstructions supporting it, and administer academic prestige and grant structures around 'internal evolution' models. They can revise the model but have professional stakes in its persistence.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__continuity_reading, romance_philologists_of_continuity, agenda_setter,
    institutional, generational, arbitrage, global).

% The medieval Church's use of Latin as living liturgical and administrative language depends on treating its own usage as legitimate continuation of Roman Latin, not degeneration. This underwrites doctrinal and institutional authority claims tied to linguistic continuity with imperial Rome.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__continuity_reading, medieval_church_institutional_latin, beneficiary,
    institutional, civilizational, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(correct_latin_kernel__continuity_reading, medieval_church_institutional_latin, agenda_setter).

% Humanist philologists (Petrarch, Valla, Erasmus) who built their entire reform project on the premise that Medieval Latin had drifted into corruption requiring restoration via classical texts. Under the continuity reading, their central diagnostic claim — that reconstruction from texts was necessary — is recast as prescriptive purism imposed on a healthy, naturally evolved language. Their historical authority as correctors is devalued.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__continuity_reading, renaissance_humanist_scholarship, payer,
    powerful, generational, constrained, continental).

% Institutions teaching Ciceronian Latin as the normative standard against which Medieval usage is graded as error. The continuity reading undermines the pedagogical premise that medieval forms are deviations to be corrected, threatening curricular authority built on classical exclusivity.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__continuity_reading, classical_purist_pedagogy, payer,
    organized, generational, constrained, national).

% Students trained under classical-purist curricula who invested years mastering a normative register premised on Medieval Latin being deficient. If the continuity reading displaces that premise, their trained judgments about 'correct' versus 'corrupt' Latin lose their evaluative basis, though the practical skill itself may remain useful.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__continuity_reading, students_taught_ciceronian_norms, payer,
    powerless, biographical, trapped, national).

% Scholars who examine the physical documentary record of usage across centuries without committing to either reading's overarching narrative; they supply evidence used by both continuity and discontinuity camps but do not adjudicate between them.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__continuity_reading, manuscript_paleographers, observer,
    moderate, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a coherent diachronic model of Latin that lets scholars, teachers, and church institutions treat centuries of documented usage as a single evolving object of study rather than a series of unrelated snapshots — enabling comparative reconstruction methodology across the whole corpus.
% TRANSFER_FUNCTION: Moves interpretive authority and institutional legitimacy from humanist correction-based scholarship toward evolution-based philology; moves pedagogical prestige from classical-purist curricula toward historical-linguistic curricula; moves doctrinal continuity claims toward the medieval Church's own linguistic self-image.
% ABSENT_VOICES: Humanist-tradition classicists whose entire scholarly apparatus treats medieval usage as error are effectively read out of the framework as engaged in 'prescriptive purism' rather than legitimate correction; they are cited mainly to be recast, not consulted on their own terms.
% DISAPPEARANCE_RATIONALE: If the continuity framework vanished, comparative Romance linguistics would lose its principal diachronic model and would need to rebuild transition accounts from the discontinuity or hybrid readings; medieval scribal legitimacy claims and Church continuity narratives would lose a supporting frame. Whether this counts as the 'world rearranging' or merely a shift in which academic faction holds interpretive authority is itself disputed between the reading's proponents and its critics.
% FOUNDING_PROBLEM: How to explain why the Latin visible in ninth-through-fifteenth-century documents differs systematically from Ciceronian norms without treating every attested medieval text as simply wrong — the continuity reading was built to give medieval usage its own internal grammar of change rather than a grammar of error.
% FOUNDING_PROBLEM_CORROBORATION: Comparative Romance linguists (proponents) attest the problem is well-solved by regular sound-change and morphological drift laws with strong predictive power. Independent corroboration comes from dialectological fieldwork on Romance vernaculars, which is not itself a beneficiary of either Latin reading's institutional stakes, and broadly supports gradual systemic drift for phonology and morphology — but the same fieldwork is notably weaker at corroborating continuity for syntax and lexicon, where humanist-tradition philologists (an interested party on the other side) point to documented borrowing and conscious textual imitation rather than organic drift.
narrative_ontology:disappearance_verdict(correct_latin_kernel__continuity_reading, contested).
narrative_ontology:founding_problem_status(correct_latin_kernel__continuity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(correct_latin_kernel__continuity_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(correct_latin_kernel__continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(correct_latin_kernel__continuity_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(correct_latin_kernel__continuity_reading_tests).
:- end_tests(correct_latin_kernel__continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.42) and rises modestly over the interval: the continuity model is not primarily an extraction device, but its ascendancy in academic and institutional discourse does redirect prestige, curricular resources, and doctrinal-legitimacy value away from the humanist-purist camp toward continuity-model proponents. Suppression is moderate-high (0.55) because the continuity reading, once institutionally dominant in comparative philology and Church linguistic self-understanding, treats the humanist framing as a category error ('purism') rather than a live alternative — this forecloses rather than merely disagrees with the rival diagnostic vocabulary in some contexts. Theater ratio rises across the interval (0.2 to 0.5) as the coordination function (a genuinely useful diachronic model for comparative Romance linguistics) becomes increasingly overlaid with performative disciplinary boundary-policing (dismissing humanist-tradition classicists as unscientific purists) rather than substantive engagement with the syntax/lexicon evidence the hybrid reading takes seriously. Accessibility collapse is moderate (0.35): the discontinuity and hybrid framings remain fully articulable and are actively defended by rival scholarly communities, so alternatives have not collapsed, only been marginalized in some institutional venues. Resistance is substantial (0.6): humanist-tradition philology, textual critics, and syntax/lexicon specialists actively contest the continuity model's overreach into non-morphological domains.
 *
 * DIRECTIONALITY LOGIC:
 *   medieval_latin_scribal_tradition and medieval_church_institutional_latin sit near the beneficiary end: the reading validates their historical practice and current doctrinal self-understanding, and their institutional identity is fused with the continuity claim (identity_locked exit — abandoning continuity would mean accepting their own tradition's Latin was 'wrong' for a millennium). romance_philologists_of_continuity are agenda-setters with arbitrage-grade exit (they could revise the model, publish competing accounts, or diversify into hybrid frameworks without institutional collapse) but have professional stakes in the model's persistence. renaissance_humanist_scholarship and classical_purist_pedagogy sit near the target end: their founding diagnostic premise is directly devalued by this reading, and their institutional position (constrained exit — reforming their pedagogy wholesale is costly) makes them structural payers. students_taught_ciceronian_norms are the most powerless payers: trapped by biographical time horizon and sunk educational investment, they bear the cost of a paradigm shift they had no part in choosing.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — explaining systematic medieval-vs-classical divergence without treating every medieval text as an error — remains genuinely live for phonology and morphology (strong sound-change evidence) but is contested for syntax and lexicon, where documented conscious borrowing and textual imitation complicate a pure-continuity account. This is not simple mandatrophy: the coordination function that solves the morphological puzzle has not become obsolete, but its jurisdiction has been over-extended (rising theater_ratio) to cover domains (syntax, lexicon) where the hybrid reading's layered-reconstruction account may be doing the actual explanatory work. The mismatch between founding_problem_status=contested and disappearance_verdict=contested flags exactly this: proponents (an interested party) attest the problem is solved; independent dialectological corroboration supports the morphological core but not the full extension.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    morphology_versus_syntax_lexicon_asymmetry,
    'Does the continuity reading''s strong evidential support for morphological drift license extending the same ''internal correction'' framework to syntax and lexicon, where the evidence for organic drift versus conscious textual imitation is much weaker?',
    'Systematic corpus-based tracking of syntactic constructions and lexical borrowings across medieval documents, cross-checked against known instances of deliberate classicizing imitation versus spontaneous vernacular-influenced innovation, independently of either reading''s proponents.',
    'If syntax/lexicon show substantial conscious textual imitation rather than organic drift, the continuity reading''s jurisdiction should contract to morphology/phonology alone, effectively conceding the syntax/lexicon domain to the hybrid_reading — which would reduce this story''s claimed scope and its rising theater_ratio trajectory.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(morphology_versus_syntax_lexicon_asymmetry, empirical, 'Whether continuity''s core evidence (morphology) generalizes to syntax and lexicon or the hybrid reading''s layered account is more accurate there.').

omega_variable(
    kernel_reading_selection_committer_structure,
    'Is the continuity reading itself a genealogically motivated framing — favored partly because it legitimates the medieval Church''s and medieval scribal tradition''s own historical linguistic practice — rather than a framing selected purely on comparative-linguistic evidential grounds?',
    'Trace the historical emergence of the continuity model against the institutional interests of its earliest proponents (medieval and early-modern grammarians defending Church Latin''s legitimacy) versus its adoption by modern comparative linguists working from independent corpus evidence; assess whether the modern evidential case stands independently of the original legitimating motive.',
    'If the reading''s persistence depends substantially on institutional legitimation interests rather than independent evidence, the classification should weight toward tangled_rope with a stronger extraction component (the beneficiaries would be sustaining a favorable framing rather than a purely evidence-driven model); if the modern evidential case is independent and robust, the reading is closer to a rope with residual, non-determinative historical motive.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection_committer_structure, conceptual, 'Whether the continuity reading''s persistence is evidence-driven or partly sustained by the institutional interests of its historical and current beneficiaries — the committer-structure ambiguity at the heart of this kernel.').

omega_variable(
    discontinuity_reading_foreclosure_scope,
    'Does the continuity reading''s core premise (no genuine systemic break) logically foreclose the discontinuity reading''s core premise (distinct systems requiring external reconstruction) in every framework, or only within frameworks that treat ''system'' at the level of the whole language rather than at the level of individual subsystems?',
    'Formal comparison of how each reading defines ''the same language'': continuity treats gradual accumulated change as identity-preserving at the whole-system level; discontinuity treats the accumulated changes as crossing an identity threshold. Determine whether any single coherent linguistic-identity criterion could accommodate both without contradiction.',
    'If a subsystem-level criterion can reconcile both (as the hybrid reading attempts), the forecloses relation declared here may be too strong and should be revisited toward influences; if no such criterion exists, forecloses is the correct declared relation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(discontinuity_reading_foreclosure_scope, conceptual, 'Whether continuity and discontinuity are strictly incompatible or reconcilable at a finer grain, which the hybrid reading''s existence suggests is at least partially possible.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(correct_latin_kernel__continuity_reading, 0, 900).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(corr_tr_t0, correct_latin_kernel__continuity_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement_basis(corr_tr_t0, observed).
narrative_ontology:measurement(corr_tr_t150, correct_latin_kernel__continuity_reading, theater_ratio, 150, 0.25).
narrative_ontology:measurement_basis(corr_tr_t150, observed).
narrative_ontology:measurement(corr_tr_t300, correct_latin_kernel__continuity_reading, theater_ratio, 300, 0.32).
narrative_ontology:measurement_basis(corr_tr_t300, observed).
narrative_ontology:measurement(corr_tr_t450, correct_latin_kernel__continuity_reading, theater_ratio, 450, 0.4).
narrative_ontology:measurement_basis(corr_tr_t450, observed).
narrative_ontology:measurement(corr_tr_t600, correct_latin_kernel__continuity_reading, theater_ratio, 600, 0.45).
narrative_ontology:measurement_basis(corr_tr_t600, observed).
narrative_ontology:measurement(corr_tr_t750, correct_latin_kernel__continuity_reading, theater_ratio, 750, 0.48).
narrative_ontology:measurement_basis(corr_tr_t750, observed).
narrative_ontology:measurement(corr_tr_t900, correct_latin_kernel__continuity_reading, theater_ratio, 900, 0.5).
narrative_ontology:measurement_basis(corr_tr_t900, observed).

% Extraction over time
narrative_ontology:measurement(corr_be_t0, correct_latin_kernel__continuity_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement_basis(corr_be_t0, observed).
narrative_ontology:measurement(corr_be_t150, correct_latin_kernel__continuity_reading, base_extractiveness, 150, 0.25).
narrative_ontology:measurement_basis(corr_be_t150, observed).
narrative_ontology:measurement(corr_be_t300, correct_latin_kernel__continuity_reading, base_extractiveness, 300, 0.3).
narrative_ontology:measurement_basis(corr_be_t300, observed).
narrative_ontology:measurement(corr_be_t450, correct_latin_kernel__continuity_reading, base_extractiveness, 450, 0.38).
narrative_ontology:measurement_basis(corr_be_t450, observed).
narrative_ontology:measurement(corr_be_t600, correct_latin_kernel__continuity_reading, base_extractiveness, 600, 0.4).
narrative_ontology:measurement_basis(corr_be_t600, observed).
narrative_ontology:measurement(corr_be_t750, correct_latin_kernel__continuity_reading, base_extractiveness, 750, 0.41).
narrative_ontology:measurement_basis(corr_be_t750, observed).
narrative_ontology:measurement(corr_be_t900, correct_latin_kernel__continuity_reading, base_extractiveness, 900, 0.42).
narrative_ontology:measurement_basis(corr_be_t900, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(correct_latin_kernel__continuity_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(correct_latin_kernel__continuity_reading, correct_latin_kernel__discontinuity_reading).
narrative_ontology:affects_constraint(correct_latin_kernel__continuity_reading, correct_latin_kernel__hybrid_reading).

% DUAL FORMULATION NOTE:
% Three constraint stories decompose the natural-language claim 'the BGS-style question of what Medieval Latin's relationship to Classical Latin actually is' per the epsilon-invariance principle: correct_latin_kernel__continuity_reading (this story; extractiveness 0.42, tangled_rope, morphological evidence strong), correct_latin_kernel__discontinuity_reading (distinct systems, reconstruction as symbolic reoccupation — expected higher extraction if humanist correction is itself found to be extractive purism-enforcement), and correct_latin_kernel__hybrid_reading (layered: morphology continuous, syntax/lexicon reconstructed — expected lowest extraction as the most evidentially reconciling reading). Each reading names a structurally different beneficiary/victim pairing and a different account of what 'reconstruction' even means, so they are not the same constraint measured three ways — they are three constraints sharing a kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
