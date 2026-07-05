% ============================================================================
% CONSTRAINT STORY: script_as_identity__phonetic_instrumentalism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_script_as_identity__phonetic_instrumentalism_reading, []).

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
 *   constraint_id: script_as_identity__phonetic_instrumentalism_reading
 *   human_readable: Phonetic Instrumentalism Reading of the 1928 Turkish Script Reform
 *   domain: comparative_linguistics/political_authority/state_building
 *
 * SUMMARY:
 *   This story instantiates ONE reading within the script_as_identity kernel:
 *   the claim that the 1928 Turkish alphabet reform was a technically neutral
 *   optimization — Latin script's Roman vowel letters simply represent
 *   Turkish's eight-vowel harmony system more transparently than the Arabic
 *   abjad's consonant-centric orthography, and the choice can be fully
 *   explained on phonetic-engineering grounds alone. This reading is
 *   deliberately narrow: it does NOT describe the reform's identity-severing
 *   function (that is kemalist_rupture_reading) or contest the prior script's
 *   constitutive status (that is ottoman_continuity_reading). Those are
 *   different constraints with different ε values and different
 *   victim/beneficiary structures; they are linked here only through network
 *   edges and the kernel commentary, not folded into this story's
 *   classification. The phonetic argument is real — Turkish vowel harmony
 *   genuinely is better served by an alphabet with dedicated vowel letters —
 *   but the instrumentalist reading's low measured ε is itself a symptom: a
 *   purely technical account of an alphabet swap does not, on its own,
 *   predict the near-total suppression of the prior script, the specific
 *   timing (bundled with the abolition of the caliphate and religious
 *   courts), or the coercive Nation Schools apparatus used to enforce adult
 *   retraining. That gap between what the phonetic argument alone would
 *   explain and what the reform actually did is exactly what the
 *   depoliticization is functioning to obscure.
 *
 * KEY AGENTS:
 *   - state_literacy_apparatus: agenda_setter (institutional/arbitrage) — enforces the reform and benefits from its apolitical framing
 *   - linguistic_technocrats: beneficiary/agenda_setter (organized/mobile) — supplied the technical justification, gained institutional standing
 *   - arabic_script_literate_generation: payer (powerless/trapped) — rendered functionally illiterate with no exit
 *   - ottoman_archive_dependent_scholars: payer (moderate/constrained) — lost mainstream access to five centuries of record
 *   - religious_education_institutions: payer (organized/constrained) — displaced pedagogical authority
 *   - comparative_linguists: observer (analytical/analytical) — sees the technical claim under-determines the coercive scope
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(script_as_identity__phonetic_instrumentalism_reading, 0.22).
domain_priors:suppression_score(script_as_identity__phonetic_instrumentalism_reading, 0.58).
domain_priors:theater_ratio(script_as_identity__phonetic_instrumentalism_reading, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(script_as_identity__phonetic_instrumentalism_reading, extractiveness, 0.22).
narrative_ontology:constraint_metric(script_as_identity__phonetic_instrumentalism_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(script_as_identity__phonetic_instrumentalism_reading, theater_ratio, 0.62).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(script_as_identity__phonetic_instrumentalism_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(script_as_identity__phonetic_instrumentalism_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(script_as_identity__phonetic_instrumentalism_reading, rope).
narrative_ontology:human_readable(script_as_identity__phonetic_instrumentalism_reading, "Phonetic Instrumentalism Reading of the 1928 Turkish Script Reform").
narrative_ontology:topic_domain(script_as_identity__phonetic_instrumentalism_reading, "comparative_linguistics/political_authority/state_building").

domain_priors:requires_active_enforcement(script_as_identity__phonetic_instrumentalism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(script_as_identity__phonetic_instrumentalism_reading, '412938d5-ec78-473a-80b2-b2569588ffcc').
narrative_ontology:cs_kernel_codification('412938d5-ec78-473a-80b2-b2569588ffcc', distributed).
narrative_ontology:cs_authority_grounding('412938d5-ec78-473a-80b2-b2569588ffcc', extraction).
narrative_ontology:cs_interpretation_layer_present('412938d5-ec78-473a-80b2-b2569588ffcc').
narrative_ontology:cs_reading_relation('412938d5-ec78-473a-80b2-b2569588ffcc', script_as_identity__kemalist_rupture_reading, coexists_with).
narrative_ontology:cs_reading_relation('412938d5-ec78-473a-80b2-b2569588ffcc', script_as_identity__ottoman_continuity_reading, influences).
narrative_ontology:cs_axiom('412938d5-ec78-473a-80b2-b2569588ffcc', foundational, script_is_politically_neutral_medium).
narrative_ontology:cs_axiom_status(script_is_politically_neutral_medium, holdable).
narrative_ontology:cs_axiom_grounding('412938d5-ec78-473a-80b2-b2569588ffcc', script_is_politically_neutral_medium, instrumental).
narrative_ontology:cs_axiom('412938d5-ec78-473a-80b2-b2569588ffcc', secondary, phonetic_fit_is_sufficient_justification).
narrative_ontology:cs_axiom_status(phonetic_fit_is_sufficient_justification, holdable).
narrative_ontology:cs_axiom_grounding('412938d5-ec78-473a-80b2-b2569588ffcc', phonetic_fit_is_sufficient_justification, empirically_contingent).
narrative_ontology:cs_reference_frame('412938d5-ec78-473a-80b2-b2569588ffcc', phonetic_engineering_baseline).
narrative_ontology:cs_drift_state('412938d5-ec78-473a-80b2-b2569588ffcc', post_authoritarian_narrative_consolidation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('412938d5-ec78-473a-80b2-b2569588ffcc', '').
narrative_ontology:cs_kernel_id(script_as_identity__phonetic_instrumentalism_reading, script_as_identity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(script_as_identity__phonetic_instrumentalism_reading, state_literacy_apparatus).
narrative_ontology:constraint_beneficiary(script_as_identity__phonetic_instrumentalism_reading, linguistic_technocrats).
narrative_ontology:constraint_beneficiary(script_as_identity__phonetic_instrumentalism_reading, primary_school_pedagogy_reformers).
narrative_ontology:constraint_victim(script_as_identity__phonetic_instrumentalism_reading, arabic_script_literate_generation).
narrative_ontology:constraint_victim(script_as_identity__phonetic_instrumentalism_reading, ottoman_archive_dependent_scholars).
narrative_ontology:constraint_victim(script_as_identity__phonetic_instrumentalism_reading, religious_education_institutions).
narrative_ontology:constraint_vindicates(script_as_identity__phonetic_instrumentalism_reading, script_neutrality_doctrine).
narrative_ontology:constraint_vindicates(script_as_identity__phonetic_instrumentalism_reading, phonetic_transparency_superiority_claim).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Designs and enforces the 1928 alphabet law, running the Millet Mektepleri (Nation Schools) to retrain the adult population, and frames the change entirely in terms of vowel-harmony fit and literacy-acquisition speed. Collects the legitimacy benefit of appearing as a rational, apolitical modernization measure while wielding full coercive authority to enforce the transition.
narrative_ontology:constraint_stakeholder(script_as_identity__phonetic_instrumentalism_reading, state_literacy_apparatus, agenda_setter,
    institutional, generational, arbitrage, national).

% Commission members and philologists who authored the phonetic-fit argument, gaining institutional standing, publication opportunities, and state patronage by supplying the technical justification the state needed. Their careers and continued relevance depend on the instrumentalist framing being accepted as complete.
narrative_ontology:constraint_stakeholder(script_as_identity__phonetic_instrumentalism_reading, linguistic_technocrats, beneficiary,
    organized, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(script_as_identity__phonetic_instrumentalism_reading, linguistic_technocrats, agenda_setter).

% Benefit from a genuinely easier orthography for teaching reading to children, since Latin vowel letters map Turkish's eight vowels far more directly than the Arabic abjad's vowel-marking conventions. Their gain is real and independent of the identity question, which is what gives the instrumentalist reading its plausibility.
narrative_ontology:constraint_stakeholder(script_as_identity__phonetic_instrumentalism_reading, primary_school_pedagogy_reformers, beneficiary,
    moderate, biographical, constrained, national).

% Adults literate only in the Arabic-Ottoman script became functionally illiterate overnight under the new law; newspapers, signage, and official documents shifted script within a few years. No exit was available except retraining or exclusion from public literacy entirely; the phonetic-efficiency framing offers them no account of this cost.
narrative_ontology:constraint_stakeholder(script_as_identity__phonetic_instrumentalism_reading, arabic_script_literate_generation, payer,
    powerless, biographical, trapped, national).

% Historians, jurists, and archivists whose professional access to five centuries of Ottoman administrative, legal, and literary record depends on Arabic-script literacy, which the reform did not preserve as a mainstream competency. The instrumentalist frame treats this as an unfortunate but unrelated externality rather than as a consequence of the choice itself.
narrative_ontology:constraint_stakeholder(script_as_identity__phonetic_instrumentalism_reading, ottoman_archive_dependent_scholars, payer,
    moderate, generational, constrained, national).

% Medrese-trained clergy and religious educators whose textual authority rested on Arabic-script literacy (needed for Qur'anic Arabic and Ottoman religious jurisprudence) find their pedagogical function marginalized by a national curriculum built entirely around the new alphabet. The phonetic-efficiency argument never addresses their displacement because it does not register their function as part of the calculation.
narrative_ontology:constraint_stakeholder(script_as_identity__phonetic_instrumentalism_reading, religious_education_institutions, payer,
    organized, generational, constrained, national).

% Study script reforms comparatively (Turkish, Vietnamese, Somali, Kazakh) and can assess whether the phonetic-fit argument, taken alone, would predict the timing, coercive method, and near-total suppression of the prior script — or whether the technical argument under-determines those features, which is diagnostic of a reading that is doing more political work than it discloses.
narrative_ontology:constraint_stakeholder(script_as_identity__phonetic_instrumentalism_reading, comparative_linguists, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: A national population needs one orthography that everyone can learn quickly and that maps reliably onto how the language is actually pronounced; Latin script with diacritics resolves ambiguities in representing Turkish's eight-vowel harmony system that the Arabic abjad's vowel-marking conventions leave comparatively underspecified.
% TRANSFER_FUNCTION: Moves literacy capital, institutional relevance, and interpretive authority from Arabic-script-competent generations, archivists, and religious educators toward the new state literacy apparatus, its technocratic authors, and the school-age generation being freshly trained in the new system.
% ABSENT_VOICES: The generation rendered functionally illiterate, and the religious-education institutions displaced by the new curriculum, were not represented in the Language Commission that ratified the phonetic-efficiency rationale; their objections surface only in later social and religious histories, not in the commission's own record.
% DISAPPEARANCE_RATIONALE: If the instrumentalist justification were withdrawn but the alphabet law itself remained, almost nothing observable would change — the reform is already fully entrenched and irreversible after nearly a century. What would change is the interpretive account: without the phonetic-efficiency cover story, the reform would have to be understood openly as identity engineering, which the technocratic beneficiaries and the state's founding narrative have strong reason to resist.
% FOUNDING_PROBLEM: Turkey needed to raise mass literacy quickly in a script that fit the spoken vernacular, at a moment when illiteracy was treated as a primary obstacle to national development and citizen participation.
% FOUNDING_PROBLEM_CORROBORATION: UNESCO literacy assessments and independent orthographic linguists confirm Turkish literacy rates rose sharply after 1928 and that the phonetic-fit problem was real and has been solved for nearly a century; however, those same outside assessments (and historians outside the Kemalist state apparatus, including Ottomanist scholars) note that script reforms with comparable literacy gains have been achieved without full suppression of the prior script, which the instrumentalist account alone does not explain — no source outside the reform's own institutional descendants treats the phonetic-efficiency account as a sufficient explanation of the reform's coercive and identity-transforming scope.
narrative_ontology:disappearance_verdict(script_as_identity__phonetic_instrumentalism_reading, contested).
narrative_ontology:founding_problem_status(script_as_identity__phonetic_instrumentalism_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(script_as_identity__phonetic_instrumentalism_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(script_as_identity__phonetic_instrumentalism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(script_as_identity__phonetic_instrumentalism_reading, 0.22, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(script_as_identity__phonetic_instrumentalism_reading_tests).
:- end_tests(script_as_identity__phonetic_instrumentalism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low (0.22 by interval end) because, taken narrowly as a claim about orthographic fit, the phonetic argument is largely true and its direct economic extraction is minimal — no party directly profits financially from the script choice qua script choice. But theater_ratio rises to 0.62: an increasing share of the instrumentalist framing's public function is performative, sustaining a depoliticized origin story for a reform whose actual operation (coercive relearning mandates, prior-script suppression, timing bundled with disestablishment measures) exceeds what phonetic efficiency alone would require. Suppression starts high (0.75, reflecting the active 1928-era enforcement of the Nation Schools and script ban) and falls over the century as the reform becomes self-sustaining through generational replacement rather than active coercion — the suppression that remains is now structural (no institution teaches Arabic-script literacy as a civic default) rather than enforced.
 *
 * PERSPECTIVAL GAP:
 *   From the state apparatus and technocrat seats, the reform is unambiguously a rational, apolitical literacy optimization — the phonetic argument is sufficient and complete. From the payer seats (older Arabic-literate generation, archive-dependent scholars, religious educators), the same event registers as a rupture imposed with a technical rationale attached after the fact, one that explains the destination but not the manner, speed, or totality of the transition. The engine should compute a genuine seat divergence here even within this single reading, because the beneficiary/victim structure is asymmetric independent of which kernel-reading framing is applied.
 *
 * DIRECTIONALITY LOGIC:
 *   The state apparatus and linguistic technocrats sit near the beneficiary end: they collect institutional legitimacy and standing from the reform succeeding under an apolitical rationale. The Arabic-literate generation sits at the full-target end — trapped, powerless, and bearing an immediate, involuntary loss of functional literacy. Archive-dependent scholars and religious institutions are targets at one remove: their exit options are only 'constrained' rather than 'trapped' because they retain some specialist channels (theological seminaries, academic Ottomanist training) but those channels are now marginal rather than mainstream.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (raise mass literacy quickly in a script suited to the spoken language) is genuinely dead as a live problem — Turkish literacy has been high for decades and the phonetic-fit question is settled. What persists is not an active literacy crisis but the instrumentalist narrative itself, now serving primarily to foreclose examination of the reform's non-technical functions. This is precisely the mandatrophy pattern the founding_problem/status/corroboration triad is designed to surface: status=dead paired with an unchanged institutional narrative is the flag, not a claim this story asserts as settled fact.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    technical_sufficiency_of_phonetic_argument,
    'Does the phonetic-transparency argument, on its own, causally explain the reform''s coercive method, timing, and totality of prior-script suppression — or does it explain only the destination (which script) while leaving the manner of transition unexplained and supplied by other motives?',
    'Comparative script-reform case study: examine reforms with comparable phonetic-fit rationale but without coercive suppression of the prior script (e.g., partial digraphia cases) and test whether phonetic efficiency alone predicts convergence toward full suppression versus coexistence.',
    'If phonetic efficiency alone does not predict the suppression method, this reading''s claim to full explanatory sufficiency is undermined and its low ε becomes evidence of a depoliticization function rather than a complete account — pointing toward reclassification pressure on the reform as a whole (though not on this narrow reading''s own metrics, which describe only the technical claim in isolation).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technical_sufficiency_of_phonetic_argument, empirical, 'Whether the technical argument explains the reform''s coercive scope or only its destination.').

omega_variable(
    script_as_identity_kernel_framing_choice,
    'The script_as_identity kernel could be framed as (a) a technical orthography-design decision with political side effects, or (b) a political identity-construction decision wearing technical justification. This story adopts framing (a) as its reading; the sibling readings adopt variants of (b). Is the technical framing itself defensible as a standalone unit of analysis, or does bracketing the identity question already presuppose the instrumentalist conclusion?',
    'Examine whether Language Commission internal deliberations (minutes, private correspondence) discussed identity and disestablishment goals alongside phonetic criteria — if identity considerations were explicit in the deliberative record, treating the technical claim as a separable, self-contained constraint understates the decision''s actual structure.',
    'If the historical record shows identity motives were explicit and primary in the actual deliberation, this reading''s status as a defensible independent framing weakens considerably, and the ε measured here should be read as an artifact of narrow scoping rather than a discovery about the reform''s true minimal extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(script_as_identity_kernel_framing_choice, conceptual, 'Whether isolating the technical claim from the identity claim is itself a neutral analytic move or already a political framing choice.').

omega_variable(
    beneficiary_status_of_technocrats,
    'Are the linguistic technocrats who authored the phonetic-fit justification genuine independent experts whose conclusions happened to serve state goals, or were they selected and incentivized specifically because their technical conclusions would provide cover for a predetermined political decision?',
    'Archival examination of Language Commission appointment criteria and the sequence of events — was the Latin-script decision made before or after the phonetic commission''s work began?',
    'If the technical conclusion followed rather than preceded the political decision, the instrumentalist reading''s coordination-function claim (this reading treats it as genuine expert coordination) weakens toward a captured-expertise pattern, which would push this reading''s own classification toward tangled_rope rather than rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(beneficiary_status_of_technocrats, empirical, 'Whether the technical experts operated independently or were retained to justify a prior political decision.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(script_as_identity__phonetic_instrumentalism_reading, 0, 90).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(scri_tr_t0, script_as_identity__phonetic_instrumentalism_reading, theater_ratio, 0, 0.4).
narrative_ontology:measurement(scri_tr_t15, script_as_identity__phonetic_instrumentalism_reading, theater_ratio, 15, 0.48).
narrative_ontology:measurement(scri_tr_t30, script_as_identity__phonetic_instrumentalism_reading, theater_ratio, 30, 0.53).
narrative_ontology:measurement(scri_tr_t45, script_as_identity__phonetic_instrumentalism_reading, theater_ratio, 45, 0.57).
narrative_ontology:measurement(scri_tr_t60, script_as_identity__phonetic_instrumentalism_reading, theater_ratio, 60, 0.6).
narrative_ontology:measurement(scri_tr_t75, script_as_identity__phonetic_instrumentalism_reading, theater_ratio, 75, 0.61).
narrative_ontology:measurement(scri_tr_t90, script_as_identity__phonetic_instrumentalism_reading, theater_ratio, 90, 0.62).

% Extraction over time
narrative_ontology:measurement(scri_be_t0, script_as_identity__phonetic_instrumentalism_reading, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(scri_be_t15, script_as_identity__phonetic_instrumentalism_reading, base_extractiveness, 15, 0.18).
narrative_ontology:measurement(scri_be_t30, script_as_identity__phonetic_instrumentalism_reading, base_extractiveness, 30, 0.2).
narrative_ontology:measurement(scri_be_t45, script_as_identity__phonetic_instrumentalism_reading, base_extractiveness, 45, 0.21).
narrative_ontology:measurement(scri_be_t60, script_as_identity__phonetic_instrumentalism_reading, base_extractiveness, 60, 0.22).
narrative_ontology:measurement(scri_be_t75, script_as_identity__phonetic_instrumentalism_reading, base_extractiveness, 75, 0.22).
narrative_ontology:measurement(scri_be_t90, script_as_identity__phonetic_instrumentalism_reading, base_extractiveness, 90, 0.22).

% Suppression requirement over time
narrative_ontology:measurement(scri_su_t0, script_as_identity__phonetic_instrumentalism_reading, suppression_requirement, 0, 0.75).
narrative_ontology:measurement(scri_su_t15, script_as_identity__phonetic_instrumentalism_reading, suppression_requirement, 15, 0.68).
narrative_ontology:measurement(scri_su_t30, script_as_identity__phonetic_instrumentalism_reading, suppression_requirement, 30, 0.6).
narrative_ontology:measurement(scri_su_t45, script_as_identity__phonetic_instrumentalism_reading, suppression_requirement, 45, 0.55).
narrative_ontology:measurement(scri_su_t60, script_as_identity__phonetic_instrumentalism_reading, suppression_requirement, 60, 0.5).
narrative_ontology:measurement(scri_su_t75, script_as_identity__phonetic_instrumentalism_reading, suppression_requirement, 75, 0.47).
narrative_ontology:measurement(scri_su_t90, script_as_identity__phonetic_instrumentalism_reading, suppression_requirement, 90, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(script_as_identity__phonetic_instrumentalism_reading, information_standard).
narrative_ontology:boltzmann_floor_override(script_as_identity__phonetic_instrumentalism_reading, 0.03).
narrative_ontology:affects_constraint(script_as_identity__phonetic_instrumentalism_reading, kemalist_rupture_reading).
narrative_ontology:affects_constraint(script_as_identity__phonetic_instrumentalism_reading, ottoman_continuity_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the script_as_identity kernel (the 1928 Turkish alphabet reform), each with a distinct ε and distinct beneficiary/victim structure: phonetic_instrumentalism_reading (this story, ε≈0.22, claimed rope — the reform as neutral orthographic optimization), kemalist_rupture_reading (higher ε expected — the reform as deliberate identity severance, with the same population of Arabic-literate/religious-institution victims but a different coordination claim: nation-building rather than literacy efficiency), and ottoman_continuity_reading (a reading held by parties who reject the reform's legitimacy altogether, treating Arabic script as constitutive of identity such that the reform itself is the extraction event). All three share the same underlying historical event and population of stakeholders but decompose it into structurally distinct claims per the ε-invariance principle — they are not the same constraint measured three ways.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
