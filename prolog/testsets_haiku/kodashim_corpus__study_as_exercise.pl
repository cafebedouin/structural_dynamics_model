% ============================================================================
% CONSTRAINT STORY: kodashim_corpus__study_as_exercise
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kodashim_corpus__study_as_exercise, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: kodashim_corpus__study_as_exercise
 *   human_readable: Kodashim Study as Mitzvah Performance (Study-as-Exercise Reading)
 *   domain: religious/commitment_system/rabbinic_judaism
 *
 * SUMMARY:
 *   The Kodashim constraint family addresses the post-Temple Jewish problem:
 *   How does a mitzvah whose literal performance is impossible remain
 *   obligatory? Three structurally distinct readings contest the kernel. The
 *   study-as-exercise reading declares that the mitzvah IS the
 *   intellectual-spiritual engagement with the law itself. Under this
 *   reading, no victim exists—scholars are not deprived; no extraction
 *   occurs—the community gains no material benefit from the arrangement. What
 *   is gained is hermeneutical coherence: the mitzvah persists through
 *   textual engagement, the obligation does not lapse, and the scholars
 *   maintain their role as cosmic participants through study. This reading
 *   has been live in rabbinic Judaism since the Talmudic era and remains a
 *   dominant framework in contemporary Jewish practice.
 *
 * KEY AGENTS:
 *   - torah_scholars_engaged_in_kodashim: scholars who understand Kodashim study as the mitzvah performance itself
 *   - rabbinic_interpretive_community: authorities who validate and transmit the study-as-exercise reading
 *   - physical_temple_restoration_advocates: communities holding the performance_only reading, structurally excluded from this reading's legitimacy claims
 *   - non_jewish_scholars: academic observers who study Kodashim as historical text without participation in the commitment system
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kodashim_corpus__study_as_exercise, 0.0).
domain_priors:suppression_score(kodashim_corpus__study_as_exercise, 0.0).
domain_priors:theater_ratio(kodashim_corpus__study_as_exercise, 0.0).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kodashim_corpus__study_as_exercise, extractiveness, 0.0).
narrative_ontology:constraint_metric(kodashim_corpus__study_as_exercise, suppression_requirement, 0.0).
narrative_ontology:constraint_metric(kodashim_corpus__study_as_exercise, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kodashim_corpus__study_as_exercise, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(kodashim_corpus__study_as_exercise, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kodashim_corpus__study_as_exercise, rope).
narrative_ontology:human_readable(kodashim_corpus__study_as_exercise, "Kodashim Study as Mitzvah Performance (Study-as-Exercise Reading)").
narrative_ontology:topic_domain(kodashim_corpus__study_as_exercise, "religious/commitment_system/rabbinic_judaism").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kodashim_corpus__study_as_exercise, 'c5ff5ccd-655e-49f7-b696-913fd35e7dfb').
narrative_ontology:cs_kernel_codification('c5ff5ccd-655e-49f7-b696-913fd35e7dfb', fixed_text).
narrative_ontology:cs_authority_grounding('c5ff5ccd-655e-49f7-b696-913fd35e7dfb', lineage).
narrative_ontology:cs_interpretation_layer_present('c5ff5ccd-655e-49f7-b696-913fd35e7dfb').
narrative_ontology:cs_reading_relation('c5ff5ccd-655e-49f7-b696-913fd35e7dfb', kodashim_corpus__performance_only, coexists_with).
narrative_ontology:cs_reading_relation('c5ff5ccd-655e-49f7-b696-913fd35e7dfb', kodashim_corpus__substitution_archive, coexists_with).
narrative_ontology:cs_axiom('c5ff5ccd-655e-49f7-b696-913fd35e7dfb', foundational, study_is_mitzvah_performance).
narrative_ontology:cs_axiom_status(study_is_mitzvah_performance, holdable).
narrative_ontology:cs_axiom_grounding('c5ff5ccd-655e-49f7-b696-913fd35e7dfb', study_is_mitzvah_performance, deontological).
narrative_ontology:cs_axiom('c5ff5ccd-655e-49f7-b696-913fd35e7dfb', foundational, textual_engagement_maintains_obligation_continuity).
narrative_ontology:cs_axiom_status(textual_engagement_maintains_obligation_continuity, holdable).
narrative_ontology:cs_axiom_grounding('c5ff5ccd-655e-49f7-b696-913fd35e7dfb', textual_engagement_maintains_obligation_continuity, deontological).
narrative_ontology:cs_reference_frame('c5ff5ccd-655e-49f7-b696-913fd35e7dfb', post_temple_obligation_maintained_through_study).
narrative_ontology:cs_drift_state('c5ff5ccd-655e-49f7-b696-913fd35e7dfb', contemporary_jewish_practice, gap(stable, minor, true)).
narrative_ontology:cs_created_at('c5ff5ccd-655e-49f7-b696-913fd35e7dfb', '').
narrative_ontology:cs_kernel_id(kodashim_corpus__study_as_exercise, kodashim_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kodashim_corpus__study_as_exercise, torah_scholars_engaged_in_kodashim).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(kodashim_corpus__study_as_exercise, rabbinic_interpretive_community).
narrative_ontology:constraint_vindicates(kodashim_corpus__study_as_exercise, continuous_textual_engagement_occupies_mitzvah).
narrative_ontology:constraint_vindicates(kodashim_corpus__study_as_exercise, intellectual_practice_maintains_cosmic_order).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Jewish scholars and religious practitioners engaged in sustained study of the Kodashim (the Mishnaic tractates on Temple sacrifice). They understand this study not as historical archiving or preparation for future restoration, but as the performance of the mitzvah itself—the commandment is fulfilled through intellectual-spiritual engagement with the text. The constraint binds their religious identity to a specific hermeneutical practice: studying sacrifice law IS doing the mitzvah. Exit would mean abandoning a constitutive understanding of Jewish obligation and cosmic function.
narrative_ontology:constraint_stakeholder(kodashim_corpus__study_as_exercise, torah_scholars_engaged_in_kodashim, beneficiary,
    organized, civilizational, identity_locked, global).

% The living chain of rabbinic authorities who interpret and transmit the Kodashim tradition, validate the study-as-exercise reading, and maintain the hermeneutical framework that makes engagement with sacrifice law spiritually functional. They set the standard for what constitutes proper engagement and authenticate the reading through lineage authority. Their authority is grounded in continuity with the Talmudic tradition itself.
narrative_ontology:constraint_stakeholder(kodashim_corpus__study_as_exercise, rabbinic_interpretive_community, beneficiary,
    organized, civilizational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(kodashim_corpus__study_as_exercise, rabbinic_interpretive_community, agenda_setter).

% Some Jewish communities and scholars who hold that Kodashim study is preparatory—that the law is archived for the day when the Temple is restored and physical sacrifice resumes. From their perspective, the study-as-exercise reading forecloses or substantially diminishes the urgency of working toward actual restoration. They are excluded from the study-as-exercise reading's own legitimacy claims and would argue for a different hermeneutical framework.
narrative_ontology:constraint_stakeholder(kodashim_corpus__study_as_exercise, physical_temple_restoration_advocates, excluded,
    organized, civilizational, constrained, global).

% Academic historians and comparative religionists who study Kodashim as a historical document or anthropological artifact. They observe the constraint without being bound by its identity-constituting function. Their analysis can illuminate how the reading operates but they are not participants in the commitment system the reading instantiates.
narrative_ontology:constraint_stakeholder(kodashim_corpus__study_as_exercise, non_jewish_scholars, observer,
    analytical, biographical, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The study-as-exercise reading coordinates Jewish scholars around a shared interpretive practice: the understanding that sustained engagement with sacrifice law—close reading, disputation, application of hermeneutical rules—constitutes the fulfillment of the mitzvah. This solves a post-Temple functional problem: How do Jews continue to perform a commandment whose literal, physical enactment is impossible? Answer: through intellectual-spiritual engagement with the law itself. The coordination is around the meaning of the mitzvah, not around a visible outcome.
% TRANSFER_FUNCTION: No extraction occurs under this reading. The scholars who study receive no material benefit they would not otherwise have; the community that validates the reading extracts no rent from those engaged in study. What is transferred is hermeneutical authority—the rabbinic community that holds the study-as-exercise reading as authoritative shapes what kinds of engagement 'count' as proper mitzvah performance, and this shapes the practice of scholars within the community. But this is coordinate authority, not extractive authority.
% ABSENT_VOICES: Scholars and communities holding the performance_only reading (the Temple is a sealed archive awaiting restoration) and the substitution_archive reading (prayer and Torah study replaced sacrifice permanently) are in structural disagreement with this reading. They are not excluded from the conversation—the contest is fully live within Rabbinic Judaism—but they disagree fundamentally on what the mitzvah IS and whether study occupies it. Their presence would argue: study is preparation, not performance; or study memorializes what was superseded, not what continues.
% DISAPPEARANCE_RATIONALE: If the study-as-exercise reading vanished—if it were suddenly not available as a framework for understanding Kodashim—the Jewish obligation to engage with sacrifice law would lose its standing as a mitzvah. Study of Kodashim would become historical scholarship or legal archaeology rather than religious practice. The cosmos-maintaining function the reading claims would be absent. The community that has organized its religious practice around this understanding would face a crisis of obligation: either adopt one of the sibling readings, or confront a gap in the mitzvot.
% FOUNDING_PROBLEM: After the destruction of the Second Temple in 70 CE, Jews could no longer perform the Temple sacrifices physically. Rabbinic Judaism faced the problem: Does the mitzvah of sacrifice cease to exist? Is it deferred? The Kodashim tractates codify the laws of sacrifice in minute detail despite their practical non-applicability. The founding problem is: What is the status of a mitzvah whose performance is temporally impossible? How do Jews remain in covenant with this commandment?
% FOUNDING_PROBLEM_CORROBORATION: The Talmud itself addresses this problem directly—the Rabbis debate whether the study of sacrifice law compensates for the inability to perform actual sacrifice (Menachot 110a). Rabbinic tradition has maintained that study of Kodashim remains obligatory (gemara and early medieval responsa). Contemporary Jewish scholars outside the benefiting parties (secular Judaica scholars, comparative religionists) document that this hermeneutical solution has been live in Jewish practice for nearly 2,000 years. The founding problem has never been resolved—Jews still face it whenever they ask why they continue to study laws they cannot practice.
narrative_ontology:disappearance_verdict(kodashim_corpus__study_as_exercise, world_rearranges).
narrative_ontology:founding_problem_status(kodashim_corpus__study_as_exercise, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kodashim_corpus__study_as_exercise, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(kodashim_corpus__study_as_exercise, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kodashim_corpus__study_as_exercise_tests).
:- end_tests(kodashim_corpus__study_as_exercise_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is zero because the arrangement contains no asymmetry of benefit and burden. Scholars who study Kodashim receive hermeneutical guidance and community validation—both of which they seek and accept. The rabbinic community that provides this guidance receives continuity of tradition and maintenance of their authority—but this is a symmetric exchange in a commitment system, not an extraction. Suppression is near-zero because participation is identity-locked, not coerced—scholars exit the arrangement only by exiting their religious identity, which is not suppression but identity commitment. Theater ratio is zero because the function of the arrangement (maintaining the mitzvah through study) is identical to its stated function; there is no proxy goal replacing the real one. Accessibility collapse is very high (0.95) because once a scholar understands their religious identity as constituted through engagement with Torah, the alternative of non-study is not merely costly—it is identity-dissolving. The readings operate at different structural levels: performance_only and substitution_archive are incompatible with study-as-exercise within any single framework, but they coexist as live positions held by different communities. The measurement series shows flat trajectories because the study-as-exercise reading has been stable in its function and ethical structure for nearly 2,000 years; no accumulation of extraction, no rise in performative theater, no intensification of suppression.
 *
 * PERSPECTIVAL GAP:
 *   There is minimal perspectival gap under this reading because all participants share the same hermeneutical framework and benefit from it symmetrically. Scholars understand themselves as doing the mitzvah; the rabbinic community understands itself as maintaining the chain of obligation. Both seats experience the constraint as coordinate, not extractive. The gap emerges only when comparing this reading to the sibling readings: from the performance_only perspective, study-as-exercise forecloses the possibility of genuine restoration; from the substitution_archive perspective, study-as-exercise misidentifies a memorial as an occupied mitzvah. But these are inter-reading disagreements, not intra-reading perspectival gaps.
 *
 * DIRECTIONALITY LOGIC:
 *   All identified stakeholders in this reading are beneficiaries or agenda-setters; there are no victims. Scholars engaged in Kodashim study benefit from a framework that makes their intellectual practice spiritually meaningful. The rabbinic interpretive community benefits from being the authorities who validate and transmit this meaning. The directionality for both is toward the beneficiary end (low d, near 0.0) because they are coordinated, not targets. The excluded physical_temple_restoration_advocates are not harmed by this reading—they simply hold a different reading—so they do not appear in a victim set. The constraint does not target them; it forecloses their reading within the study-as-exercise framework, but foreclosure is a logical relation within commitment systems, not an extractive relation. Non-jewish scholars are observers with analytical exit; they are unaffected by the constraint's directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   The study-as-exercise reading has not experienced mandatrophy—the mandate (engage with sacrifice law to maintain the mitzvah) remains aligned with its function (making Jewish obligation continuous despite Temple absence). The founding problem is still live: Jews still cannot perform physical sacrifice, so they still need a hermeneutical answer to 'What is the status of this mitzvah?' The reading solves this problem in every generation of scholars who take it up. Mandatrophy would emerge only if the founding problem were resolved (Temple restored, physical sacrifice resumed) or if the reading's function were superseded by a stronger alternative—neither has occurred in 2,000 years of continuous engagement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    study_as_performance_vs_substitution,
    'Does study of sacrifice law constitute the performance of the mitzvah (study-as-exercise), or does it merely substitute for an obligation that was always understood as centered on physical performance (substitution_archive)?',
    'Historical-hermeneutical analysis of early Rabbinic sources to determine whether the Talmud itself treats study as the mitzvah or as compensation for the mitzvah''s impossibility; examination of contemporary Jewish practice to see whether communities experience study as fulfillment or as proxy.',
    'If study is the mitzvah, the reading is a genuine rope (coordinate engagement, no extraction). If study is only a substitute, the reading might be better classified as a scaffold (temporary measure until restoration) or even as a piton (maintained by inertia, no longer fulfilling its original function). This is the core identity question for the reading itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(study_as_performance_vs_substitution, conceptual, 'Whether study constitutes performance or substitutes for it—the boundary between this reading and substitution_archive.').

omega_variable(
    identity_lock_mechanism,
    'Is the identification of religious identity with Kodashim study a necessary feature of Jewish religious commitment, or is it a contingent tradition that could be otherwise?',
    'Comparison with other Jewish reading traditions (Chasidic communities that focus on mystical interpretation, rationalist communities that prioritize philosophical extraction, communities that emphasize observance over study). If the identity-lock exists across all traditions, it is necessary; if alternative traditions maintain Jewish religious identity without the same tight coupling to Kodashim study, the lock is contingent.',
    'If identity-lock is necessary, the constraint operates with structural force independent of any individual''s choice—it is truly identity-constituting. If contingent, the constraint''s persistence depends on ongoing reinforcement by the community, which might make it more vulnerable to reformation. The accessible_collapse metric (0.95) assumes necessary coupling; if the coupling is only contingent, accessibility might be lower.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_mechanism, empirical, 'Whether the coupling of religious identity to Kodashim study is structurally necessary or contingently reinforced.').

omega_variable(
    cosmic_order_claim_verification,
    'Does the study of sacrifice law actually maintain cosmic order (as the reading claims), or is this a theological claim that cannot be empirically verified?',
    'This is structured as a preference-type omega because it turns on fundamental theological commitments that are not empirically resolvable. The resolution is to acknowledge that the claim is internal to the commitment system (the reading asserts it as true within the framework of Jewish obligation) and to recognize that outside observers cannot verify or falsify it.',
    'If the claim is accepted within the framework of the reading, it justifies continued engagement with Kodashim as meaningful (not theatrical). If the claim is rejected as unverifiable, the reading becomes vulnerable to reframing as performative without real content—it might begin to look more like a piton. But the classification should follow from the structural facts (extraction, theater) not from judgment on the cosmic claim itself.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(cosmic_order_claim_verification, preference, 'Whether the theological claim that study maintains cosmic order is meaningful within the commitment system or illusory from outside it.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kodashim_corpus__study_as_exercise, 0, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(koda_tr_t0, kodashim_corpus__study_as_exercise, theater_ratio, 0, 0.0).
narrative_ontology:measurement_basis(koda_tr_t0, observed).
narrative_ontology:measurement(koda_tr_t500, kodashim_corpus__study_as_exercise, theater_ratio, 500, 0.0).
narrative_ontology:measurement_basis(koda_tr_t500, observed).
narrative_ontology:measurement(koda_tr_t1000, kodashim_corpus__study_as_exercise, theater_ratio, 1000, 0.0).
narrative_ontology:measurement_basis(koda_tr_t1000, observed).
narrative_ontology:measurement(koda_tr_t1500, kodashim_corpus__study_as_exercise, theater_ratio, 1500, 0.0).
narrative_ontology:measurement_basis(koda_tr_t1500, observed).
narrative_ontology:measurement(koda_tr_t2000, kodashim_corpus__study_as_exercise, theater_ratio, 2000, 0.0).
narrative_ontology:measurement_basis(koda_tr_t2000, observed).

% Extraction over time
narrative_ontology:measurement(koda_be_t0, kodashim_corpus__study_as_exercise, base_extractiveness, 0, 0.0).
narrative_ontology:measurement_basis(koda_be_t0, observed).
narrative_ontology:measurement(koda_be_t500, kodashim_corpus__study_as_exercise, base_extractiveness, 500, 0.0).
narrative_ontology:measurement_basis(koda_be_t500, observed).
narrative_ontology:measurement(koda_be_t1000, kodashim_corpus__study_as_exercise, base_extractiveness, 1000, 0.0).
narrative_ontology:measurement_basis(koda_be_t1000, observed).
narrative_ontology:measurement(koda_be_t1500, kodashim_corpus__study_as_exercise, base_extractiveness, 1500, 0.0).
narrative_ontology:measurement_basis(koda_be_t1500, observed).
narrative_ontology:measurement(koda_be_t2000, kodashim_corpus__study_as_exercise, base_extractiveness, 2000, 0.0).
narrative_ontology:measurement_basis(koda_be_t2000, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(kodashim_corpus__study_as_exercise, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kodashim_corpus__study_as_exercise, identity_coordination).
narrative_ontology:boltzmann_floor_override(kodashim_corpus__study_as_exercise, 0.0).
narrative_ontology:affects_constraint(kodashim_corpus__study_as_exercise, kodashim_corpus__performance_only).
narrative_ontology:affects_constraint(kodashim_corpus__study_as_exercise, kodashim_corpus__substitution_archive).

% DUAL FORMULATION NOTE:
% The Kodashim corpus kernel admits three structurally distinct constraint readings. The study-as-exercise reading (THIS STORY) treats engagement with sacrifice law as the mitzvah performance itself, with zero extractiveness and identity-locked participation. The performance_only reading treats the corpus as a sealed blueprint awaiting messianic restoration, making study preparatory rather than performative. The substitution_archive reading treats the corpus as a memorial to an obligation that was permanently replaced by prayer and study. Each reading has its own ε, its own beneficiary/victim structure, and its own type. They coexist as live positions in Jewish tradition and cannot be unified into a single constraint without losing the structural differences that make the reading contest meaningful. All three readings are linked via affects_constraints to indicate kernel membership.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
