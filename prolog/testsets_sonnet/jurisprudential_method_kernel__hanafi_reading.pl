% ============================================================================
% CONSTRAINT STORY: jurisprudential_method_kernel__hanafi_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jurisprudential_method_kernel__hanafi_reading, []).

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
 *   constraint_id: jurisprudential_method_kernel__hanafi_reading
 *   human_readable: Hanafi Reading of the Jurisprudential Method Kernel (Qiyas + Istihsan)
 *   domain: religious/legal/institutional
 *
 * SUMMARY:
 *   This story is one reading of a single contested kernel — the
 *   jurisprudential method kernel governing how Islamic law relates human
 *   legal reasoning to divine text. The Hanafi reading holds that qiyas
 *   (analogical reasoning) and istihsan (juristic preference, departing from
 *   strict analogy when it produces an inequitable or absurd result) are
 *   legitimate tools for extending divine intent to cases the Qur'an and
 *   Hadith do not explicitly address. This reading does not describe or
 *   average over the sibling readings (Maliki 'amal ahl al-Madina, Shafi'i's
 *   four-tier hierarchy, Hanbali strict textualism) — those are separate
 *   constraints with their own epsilon values, linked here only via network
 *   edges and the omega variables documenting the disagreement's location. As
 *   Abbasid state-favored doctrine, Hanafi method's extraction rose as its
 *   administrative footprint grew: more novel cases under imperial
 *   jurisdiction meant more rulings resting on jurist-dependent analogical
 *   chains rather than traceable text, concentrating interpretive power in a
 *   rationalist-trained class while textualist critics' authenticity claim
 *   was structurally sidelined in court appointments rather than refuted in
 *   argument.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jurisprudential_method_kernel__hanafi_reading, 0.52).
domain_priors:suppression_score(jurisprudential_method_kernel__hanafi_reading, 0.38).
domain_priors:theater_ratio(jurisprudential_method_kernel__hanafi_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jurisprudential_method_kernel__hanafi_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(jurisprudential_method_kernel__hanafi_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(jurisprudential_method_kernel__hanafi_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jurisprudential_method_kernel__hanafi_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(jurisprudential_method_kernel__hanafi_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jurisprudential_method_kernel__hanafi_reading, tangled_rope).
narrative_ontology:human_readable(jurisprudential_method_kernel__hanafi_reading, "Hanafi Reading of the Jurisprudential Method Kernel (Qiyas + Istihsan)").
narrative_ontology:topic_domain(jurisprudential_method_kernel__hanafi_reading, "religious/legal/institutional").

domain_priors:requires_active_enforcement(jurisprudential_method_kernel__hanafi_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jurisprudential_method_kernel__hanafi_reading, 'ffc1fd8b-8255-4620-b7d5-908e3a36e742').
narrative_ontology:cs_kernel_codification('ffc1fd8b-8255-4620-b7d5-908e3a36e742', distributed).
narrative_ontology:cs_authority_grounding('ffc1fd8b-8255-4620-b7d5-908e3a36e742', practice).
narrative_ontology:cs_interpretation_layer_present('ffc1fd8b-8255-4620-b7d5-908e3a36e742').
narrative_ontology:cs_reading_relation('ffc1fd8b-8255-4620-b7d5-908e3a36e742', jurisprudential_method_kernel__hanbali_reading, forecloses).
narrative_ontology:cs_reading_relation('ffc1fd8b-8255-4620-b7d5-908e3a36e742', jurisprudential_method_kernel__maliki_reading, coexists_with).
narrative_ontology:cs_reading_relation('ffc1fd8b-8255-4620-b7d5-908e3a36e742', jurisprudential_method_kernel__shafii_reading, influences).
narrative_ontology:cs_axiom('ffc1fd8b-8255-4620-b7d5-908e3a36e742', foundational, reasoned_analogy_extends_divine_intent).
narrative_ontology:cs_axiom_status(reasoned_analogy_extends_divine_intent, holdable).
narrative_ontology:cs_axiom_grounding('ffc1fd8b-8255-4620-b7d5-908e3a36e742', reasoned_analogy_extends_divine_intent, conventional).
narrative_ontology:cs_axiom('ffc1fd8b-8255-4620-b7d5-908e3a36e742', foundational, juristic_preference_may_override_strict_analogy_for_equity).
narrative_ontology:cs_axiom_status(juristic_preference_may_override_strict_analogy_for_equity, holdable).
narrative_ontology:cs_axiom_grounding('ffc1fd8b-8255-4620-b7d5-908e3a36e742', juristic_preference_may_override_strict_analogy_for_equity, instrumental).
narrative_ontology:cs_reference_frame('ffc1fd8b-8255-4620-b7d5-908e3a36e742', prophetic_and_companion_era_textual_corpus).
narrative_ontology:cs_drift_state('ffc1fd8b-8255-4620-b7d5-908e3a36e742', abbasid_imperial_consolidation, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('ffc1fd8b-8255-4620-b7d5-908e3a36e742', '').
narrative_ontology:cs_kernel_id(jurisprudential_method_kernel__hanafi_reading, jurisprudential_method_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jurisprudential_method_kernel__hanafi_reading, rationalist_trained_jurists).
narrative_ontology:constraint_beneficiary(jurisprudential_method_kernel__hanafi_reading, hanafi_court_administrators).
narrative_ontology:constraint_beneficiary(jurisprudential_method_kernel__hanafi_reading, abbasid_state_bureaucracy).
narrative_ontology:constraint_victim(jurisprudential_method_kernel__hanafi_reading, textualist_scholars).
narrative_ontology:constraint_victim(jurisprudential_method_kernel__hanafi_reading, litigants_facing_unpredictable_analogical_rulings).
narrative_ontology:constraint_victim(jurisprudential_method_kernel__hanafi_reading, non_hanafi_minority_communities_under_hanafi_courts).
narrative_ontology:constraint_vindicates(jurisprudential_method_kernel__hanafi_reading, reason_extends_divine_intent).
narrative_ontology:constraint_vindicates(jurisprudential_method_kernel__hanafi_reading, novel_case_coverage_is_a_legal_good).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Trained for years in qiyas and istihsan technique, they hold interpretive monopoly over how novel cases are resolved. Their scarce, difficult-to-replicate expertise becomes indispensable precisely because the method licenses extension beyond explicit text — the more cases require analogical reasoning, the more their training pays off. They set school doctrine and staff qadi courts.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanafi_reading, rationalist_trained_jurists, beneficiary,
    institutional, generational, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(jurisprudential_method_kernel__hanafi_reading, rationalist_trained_jurists, agenda_setter).

% Administer the qadi court system across a vast, diverse empire where explicit textual rulings do not cover most commercial and administrative disputes. Istihsan gives them flexibility to adapt rulings to local custom and state need, which is precisely why the Abbasid state favored Hanafi jurists for official appointments.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanafi_reading, hanafi_court_administrators, agenda_setter,
    institutional, generational, arbitrage, continental).

% Governs a multi-ethnic, rapidly expanding empire whose administrative needs (taxation, contracts, land tenure, criminal procedure across new territories) vastly outstrip what Qur'an and Hadith explicitly address. Sponsors and institutionalizes the school whose method produces the most usable rulings for the widest range of governance problems.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanafi_reading, abbasid_state_bureaucracy, beneficiary,
    institutional, generational, arbitrage, continental).

% Hold that qiyas and especially istihsan introduce human preference where only transmitted text and consensus should govern, calling istihsan 'legislating by whim.' Their claim to represent the most authentic, least-corrupted transmission of Prophetic practice is structurally devalued wherever Hanafi courts hold administrative power, since the reasoning-based method displaces their textual authority in practice even where it cannot refute it in principle.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanafi_reading, textualist_scholars, payer,
    organized, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(jurisprudential_method_kernel__hanafi_reading, textualist_scholars, excluded).

% Bring disputes to Hanafi courts where outcomes on novel matters depend on a particular jurist's chain of analogical reasoning rather than a fixed textual rule. Cannot predict rulings in advance, cannot appeal to a text the judge is bound to follow, and cannot afford to travel to a court applying a different school's method.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanafi_reading, litigants_facing_unpredictable_analogical_rulings, payer,
    powerless, immediate, trapped, local).

% Live under the jurisdiction of Hanafi-staffed courts (the imperially favored school) despite adhering to different legal traditions. Bear the costs of rulings shaped by a method and juristic preference not their own, with no meaningful venue choice under a unified state court system.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanafi_reading, non_hanafi_minority_communities_under_hanafi_courts, payer,
    powerless, biographical, trapped, regional).

% Study the accumulated body of qiyas- and istihsan-derived rulings across centuries, assessing whether the method's flexibility produced coherent doctrine or an ever-expanding, jurist-dependent body of precedent that has drifted from any single traceable textual root.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanafi_reading, later_hanafi_jurists, observer,
    analytical, civilizational, analytical, continental).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a working method for extending a finite, historically bounded revelatory corpus (Qur'an and Hadith) to the unbounded and growing set of novel legal, commercial, and administrative cases an expanding empire actually generates — without this, vast swaths of governance would have no jurisprudential coverage at all.
% TRANSFER_FUNCTION: Moves interpretive authority (and the material rewards of holding qadi appointments, issuing binding rulings, and shaping doctrine) toward jurists trained in analogical technique, and moves the burden of unpredictable, judge-dependent outcomes onto litigants and communities who cannot verify a ruling against a fixed text.
% ABSENT_VOICES: Textualist scholars object that istihsan is unprincipled judicial discretion dressed as method, but their objection is structurally muted wherever Hanafi jurists hold the state-backed court appointments; non-Hanafi minority litigants have no venue voice in a state system that favors one school's method as administrative default.
% DISAPPEARANCE_RATIONALE: If the Hanafi method's legitimacy were withdrawn overnight, the vast body of rulings on cases without explicit textual coverage would lose their claimed grounding in divine intent; courts would need to either revert cases to unresolved status, adopt a rival school's narrower or differently-sourced method, or openly reclassify centuries of istihsan-based rulings as human legislation rather than derived law — a live doctrinal and political rupture, not a null event.
% FOUNDING_PROBLEM: The Qur'an and even the assembled Hadith corpus do not explicitly address most cases that arise in a rapidly expanding, commercially complex, multi-ethnic empire; some method was needed to extend recognizably divine authority to unaddressed situations without simply declaring them unregulated or leaving rulings to naked political will.
% FOUNDING_PROBLEM_CORROBORATION: Hanafi jurists themselves attest the problem remains permanently live (novel cases never stop arising). Textualist and Hanbali critics, writing from outside the Hanafi beneficiary group, attest that the original problem of textual silence was real but argue the Hanafi solution overshot into human legislation masquerading as extension of divine intent — their corroboration confirms the founding problem's original reality while disputing that istihsan remains a faithful solution to it rather than a captured one.
narrative_ontology:disappearance_verdict(jurisprudential_method_kernel__hanafi_reading, world_rearranges).
narrative_ontology:founding_problem_status(jurisprudential_method_kernel__hanafi_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jurisprudential_method_kernel__hanafi_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(jurisprudential_method_kernel__hanafi_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jurisprudential_method_kernel__hanafi_reading, 0.52, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jurisprudential_method_kernel__hanafi_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jurisprudential_method_kernel__hanafi_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jurisprudential_method_kernel__hanafi_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52) reflects that istihsan licenses departure from strict analogy based on the deciding jurist's judgment of equity or public interest — a real coordination function (covering the vast space of unaddressed cases) bundled with a real extraction (interpretive authority and appointment leverage flowing disproportionately to jurists whose training makes them indispensable under this method, and unpredictability costs falling on litigants who cannot verify rulings against fixed text). Suppression (0.38) is moderate rather than high: the method does not forbid textualist argument, but state sponsorship of Hanafi court appointments across the Abbasid and later Ottoman administrations gave the reading disproportionate institutional reach, which functions as a soft suppression of rival readings' practical authority even without doctrinal prohibition. Accessibility collapse (0.45) is middling — textualist and other schools remained live, coexisting traditions (this is the coexists_with relation below), so alternatives never fully collapsed the way they would under a genuine mountain. Resistance (0.55) is substantial: Hanbali polemics against istihsan as 'legislating by whim' represent centuries of sustained organized pushback, not passive acceptance.
 *
 * DIRECTIONALITY LOGIC:
 *   Rationalist-trained jurists and the administrators who staff courts with them sit at the beneficiary end: their scarce interpretive skill is the very currency the method creates demand for, and their exit options are effectively arbitrage-grade (they can move between doctrinal schools of thought and administrative postings while remaining Hanafi-credentialed). The Abbasid bureaucracy benefits by having a workable, expansive method for the huge governance gap Qur'an/Hadith leave open. Litigants facing unpredictable rulings and non-Hanafi minority communities under Hanafi courts sit at the target end: trapped or constrained, unable to select a different court's method, bearing the transaction costs of jurist-dependent unpredictability. Textualist scholars are payers in a different sense — not materially extracted from, but structurally devalued: their claim to exclusive authenticity is undercut in practice wherever the state favors Hanafi appointments, even though their doctrinal argument against istihsan remains fully articulable and unrefuted on its own terms.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (textual silence on most real cases in an expanding empire) remains genuinely live — new commercial and administrative situations never stop arising, so this is not a case of an obsolete mandate persisting by inertia. What keeps this a tangled_rope rather than a clean rope is that the coordination function (extending divine intent to novel cases) is inseparably bundled with asymmetric extraction (concentrating interpretive authority and appointment power in a specific trained class, with real costs falling on unpredictability-exposed litigants) that requires active state enforcement (favored court appointments) to sustain its practical dominance over rival readings.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest_location,
    'This constraint is one reading (hanafi_reading) of the jurisprudential_method_kernel. The disagreement with sibling readings (maliki_reading, shafii_reading, hanbali_reading) is located specifically in whether reasoned extension (qiyas/istihsan) counts as a legitimate source of law equal in kind to text and consensus, or as human innovation that corrupts a purely transmission-based kernel. What would resolving this disagreement in favor of one reading change structurally?',
    'No empirical resolution mechanism exists within the tradition itself — the dispute is intra-doctrinal and unsettled after twelve centuries. A structural marker: track whether a given reading''s method produces rulings on novel cases that other schools independently reach via different routes (convergence would weaken the hanbali charge of arbitrary human legislation) versus rulings that diverge sharply by school (divergence would support the charge).',
    'If the hanbali reading''s premise (only unanimous consensus plus literal text is valid) were adopted as exclusive, the hanafi reading''s entire qiyas/istihsan apparatus would be foreclosed as bid''ah rather than coexisting as a live alternative — this is the one sibling relationship that approaches forecloses rather than coexists_with, though in practice all four schools have coexisted institutionally for centuries, suggesting no single reading has actually achieved exclusive authority.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest_location, conceptual, 'Where exactly the four sibling readings of the jurisprudential method kernel disagree, and what adopting one exclusively would foreclose.').

omega_variable(
    istihsan_as_extraction_or_equity,
    'Is juristic preference (istihsan) better modeled as principled equity-correction within a coherent method, or as a naming device that licenses whatever ruling a jurist independently prefers, retroactively justified as ''departing from strict analogy for a stronger reason''?',
    'Systematic review of a large sample of istihsan rulings across centuries to test whether the ''stronger reason'' cited is itself derivable from stable, textually-grounded principles (supporting principled equity) or varies unpredictably by jurist and case in ways only explicable by extralegal considerations (supporting extraction reading).',
    'If principled, extractiveness is overstated by treating necessary judicial discretion as rent; if arbitrary, extractiveness may be understated, since the unpredictability cost to litigants would be a feature of the method rather than an incidental side effect.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(istihsan_as_extraction_or_equity, empirical, 'Whether istihsan functions as principled equity-correction or as unconstrained juristic discretion.').

omega_variable(
    state_sponsorship_vs_intrinsic_merit,
    'Did the Hanafi method become dominant across the Abbasid and Ottoman administrative systems because of its intrinsic methodological merit for governing a complex empire, or because early state sponsorship created a self-reinforcing advantage (more court appointments -> more institutional resources -> more scholarly output -> more perceived authority) independent of the method''s comparative soundness?',
    'Comparative historical analysis of regions and periods where non-Hanafi schools held equivalent state backing, checking whether their administrative outcomes and doctrinal elaboration matched Hanafi results absent the sponsorship advantage.',
    'If sponsorship-driven, the beneficiary structure (rationalist-trained jurists, Hanafi administrators) reflects captured institutional advantage rather than the method''s coordination function alone, strengthening the tangled_rope classification; if merit-driven, the extraction component would be smaller relative to the genuine coordination benefit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_sponsorship_vs_intrinsic_merit, empirical, 'Whether Hanafi dominance reflects state sponsorship or intrinsic methodological advantage.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jurisprudential_method_kernel__hanafi_reading, 0, 300).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(juri_tr_t0, jurisprudential_method_kernel__hanafi_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(juri_tr_t50, jurisprudential_method_kernel__hanafi_reading, theater_ratio, 50, 0.13).
narrative_ontology:measurement(juri_tr_t100, jurisprudential_method_kernel__hanafi_reading, theater_ratio, 100, 0.16).
narrative_ontology:measurement(juri_tr_t150, jurisprudential_method_kernel__hanafi_reading, theater_ratio, 150, 0.18).
narrative_ontology:measurement(juri_tr_t200, jurisprudential_method_kernel__hanafi_reading, theater_ratio, 200, 0.2).
narrative_ontology:measurement(juri_tr_t250, jurisprudential_method_kernel__hanafi_reading, theater_ratio, 250, 0.21).
narrative_ontology:measurement(juri_tr_t300, jurisprudential_method_kernel__hanafi_reading, theater_ratio, 300, 0.22).

% Extraction over time
narrative_ontology:measurement(juri_be_t0, jurisprudential_method_kernel__hanafi_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(juri_be_t50, jurisprudential_method_kernel__hanafi_reading, base_extractiveness, 50, 0.35).
narrative_ontology:measurement(juri_be_t100, jurisprudential_method_kernel__hanafi_reading, base_extractiveness, 100, 0.42).
narrative_ontology:measurement(juri_be_t150, jurisprudential_method_kernel__hanafi_reading, base_extractiveness, 150, 0.47).
narrative_ontology:measurement(juri_be_t200, jurisprudential_method_kernel__hanafi_reading, base_extractiveness, 200, 0.5).
narrative_ontology:measurement(juri_be_t250, jurisprudential_method_kernel__hanafi_reading, base_extractiveness, 250, 0.51).
narrative_ontology:measurement(juri_be_t300, jurisprudential_method_kernel__hanafi_reading, base_extractiveness, 300, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(juri_su_t0, jurisprudential_method_kernel__hanafi_reading, suppression_requirement, 0, 0.22).
narrative_ontology:measurement(juri_su_t50, jurisprudential_method_kernel__hanafi_reading, suppression_requirement, 50, 0.27).
narrative_ontology:measurement(juri_su_t100, jurisprudential_method_kernel__hanafi_reading, suppression_requirement, 100, 0.31).
narrative_ontology:measurement(juri_su_t150, jurisprudential_method_kernel__hanafi_reading, suppression_requirement, 150, 0.34).
narrative_ontology:measurement(juri_su_t200, jurisprudential_method_kernel__hanafi_reading, suppression_requirement, 200, 0.36).
narrative_ontology:measurement(juri_su_t250, jurisprudential_method_kernel__hanafi_reading, suppression_requirement, 250, 0.37).
narrative_ontology:measurement(juri_su_t300, jurisprudential_method_kernel__hanafi_reading, suppression_requirement, 300, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jurisprudential_method_kernel__hanafi_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(jurisprudential_method_kernel__hanafi_reading, 0.12).
narrative_ontology:affects_constraint(jurisprudential_method_kernel__hanafi_reading, jurisprudential_method_kernel__maliki_reading).
narrative_ontology:affects_constraint(jurisprudential_method_kernel__hanafi_reading, jurisprudential_method_kernel__shafii_reading).
narrative_ontology:affects_constraint(jurisprudential_method_kernel__hanafi_reading, jurisprudential_method_kernel__hanbali_reading).

% DUAL FORMULATION NOTE:
% This story is one of four sibling constraints decomposing the single natural-language label 'Islamic jurisprudential method' into structurally distinct readings of one contested kernel. Each reading has its own epsilon, its own beneficiary/victim structure, and its own classification, per the epsilon-invariance principle: the hanafi_reading (this story) computes as tangled_rope with moderate-high extraction concentrated on rationalist-trained jurists as beneficiaries; the hanbali_reading is expected to compute with much lower extraction (a stricter, less discretion-dependent method) but higher suppression of rival readings as illegitimate innovation; the shafii_reading is expected to sit closer to rope, since its four-tier hierarchy constrains discretion more than istihsan does; the maliki_reading's extraction profile depends on how 'amal ahl al-Madina functions in practice, a distinct empirical question. All four are linked here as family members; none of the sibling files should be treated as measuring the 'same' constraint under a different observable — they are four different claims about what legitimately constitutes law.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
