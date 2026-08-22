% ============================================================================
% CONSTRAINT STORY: classical_latin_standard__continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_classical_latin_standard__continuity_reading, []).

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
 *   constraint_id: classical_latin_standard__continuity_reading
 *   human_readable: Living Latin: Correctness as Unbroken Transmitted Practice
 *   domain: historical_linguistics/philology/commitment_systems
 *
 * SUMMARY:
 *   This constraint instantiates the continuity reading of the contested
 *   'correct Latin' kernel: correctness is defined by unbroken transmission
 *   of living practice, with linguistic drift (phonological, lexical,
 *   syntactic change across the medieval and Neo-Latin periods) accepted as
 *   legitimate development rather than corruption. This is the standard
 *   actually operative within the Church's liturgical Latin tradition and
 *   within Neo-Latin/Living-Latin scholarly and pedagogical communities. It
 *   is a distinct constraint from the reconstruction reading (which treats
 *   only philologically-recovered Classical usage as correct) and the hybrid
 *   reading (which requires fidelity to Classical norms AND acceptance of
 *   domain-specific post-Classical developments) — each of those readings has
 *   its own beneficiary structure, its own victim set, and its own extraction
 *   profile, and is authored as a separate story per the ε-invariance
 *   principle.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(classical_latin_standard__continuity_reading, 0.38).
domain_priors:suppression_score(classical_latin_standard__continuity_reading, 0.22).
domain_priors:theater_ratio(classical_latin_standard__continuity_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(classical_latin_standard__continuity_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(classical_latin_standard__continuity_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(classical_latin_standard__continuity_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(classical_latin_standard__continuity_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(classical_latin_standard__continuity_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(classical_latin_standard__continuity_reading, rope).
narrative_ontology:human_readable(classical_latin_standard__continuity_reading, "Living Latin: Correctness as Unbroken Transmitted Practice").
narrative_ontology:topic_domain(classical_latin_standard__continuity_reading, "historical_linguistics/philology/commitment_systems").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(classical_latin_standard__continuity_reading, '49ab3a48-d7db-4e1b-a8c0-81fec9075dc4').
narrative_ontology:cs_kernel_codification('49ab3a48-d7db-4e1b-a8c0-81fec9075dc4', distributed).
narrative_ontology:cs_authority_grounding('49ab3a48-d7db-4e1b-a8c0-81fec9075dc4', practice).
narrative_ontology:cs_interpretation_layer_present('49ab3a48-d7db-4e1b-a8c0-81fec9075dc4').
narrative_ontology:cs_reading_relation('49ab3a48-d7db-4e1b-a8c0-81fec9075dc4', classical_latin_standard__reconstruction_reading, coexists_with).
narrative_ontology:cs_reading_relation('49ab3a48-d7db-4e1b-a8c0-81fec9075dc4', classical_latin_standard__hybrid_reading, influences).
narrative_ontology:cs_axiom('49ab3a48-d7db-4e1b-a8c0-81fec9075dc4', foundational, unbroken_practice_constitutes_correctness).
narrative_ontology:cs_axiom_status(unbroken_practice_constitutes_correctness, holdable).
narrative_ontology:cs_axiom_grounding('49ab3a48-d7db-4e1b-a8c0-81fec9075dc4', unbroken_practice_constitutes_correctness, conventional).
narrative_ontology:cs_axiom('49ab3a48-d7db-4e1b-a8c0-81fec9075dc4', foundational, linguistic_drift_is_legitimate_development_not_corruption).
narrative_ontology:cs_axiom_status(linguistic_drift_is_legitimate_development_not_corruption, holdable).
narrative_ontology:cs_axiom_grounding('49ab3a48-d7db-4e1b-a8c0-81fec9075dc4', linguistic_drift_is_legitimate_development_not_corruption, empirically_contingent).
narrative_ontology:cs_reference_frame('49ab3a48-d7db-4e1b-a8c0-81fec9075dc4', living_ecclesiastical_and_scholarly_transmission).
narrative_ontology:cs_drift_state('49ab3a48-d7db-4e1b-a8c0-81fec9075dc4', contemporary_neo_latin_and_liturgical_practice, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('49ab3a48-d7db-4e1b-a8c0-81fec9075dc4', '').
narrative_ontology:cs_kernel_id(classical_latin_standard__continuity_reading, classical_latin_standard).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(classical_latin_standard__continuity_reading, ecclesiastical_latin_institutions).
narrative_ontology:constraint_beneficiary(classical_latin_standard__continuity_reading, medieval_and_neo_latin_scholarly_communities).
narrative_ontology:constraint_beneficiary(classical_latin_standard__continuity_reading, living_latin_pedagogy_networks).
narrative_ontology:constraint_victim(classical_latin_standard__continuity_reading, vernacular_only_speakers_seeking_institutional_latin_access).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(classical_latin_standard__continuity_reading, practicing_latinists_general).
narrative_ontology:constraint_victim(classical_latin_standard__continuity_reading, practicing_latinists_general).
narrative_ontology:constraint_vindicates(classical_latin_standard__continuity_reading, language_as_continuous_practice_doctrine).
narrative_ontology:constraint_vindicates(classical_latin_standard__continuity_reading, natural_drift_legitimacy_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers Latin as a living liturgical and administrative language, treating post-Classical forms (ecclesiastical pronunciation, medieval vocabulary, curial usage) as legitimate continuations rather than corruptions. Sets what counts as acceptable Latin for its own documents and rites, and benefits from a standard flexible enough to keep absorbing new coinages without needing philological permission from outside scholarship.
narrative_ontology:constraint_stakeholder(classical_latin_standard__continuity_reading, ecclesiastical_latin_institutions, agenda_setter,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(classical_latin_standard__continuity_reading, ecclesiastical_latin_institutions, beneficiary).

% Study and produce texts in post-Classical Latin (medieval chronicles, Renaissance humanist Latin, modern Neo-Latin composition). The continuity reading validates their entire corpus as legitimate Latin rather than degraded Latin, which sustains their field's subject matter and institutional standing.
narrative_ontology:constraint_stakeholder(classical_latin_standard__continuity_reading, medieval_and_neo_latin_scholarly_communities, beneficiary,
    organized, generational, mobile, global).

% Teach Latin as a spoken, actively used language (immersion schools, conversational Latin movements) and rely on the continuity reading to justify natural extension of vocabulary and idiom for modern concepts, rather than being bound to attested Classical usage alone.
narrative_ontology:constraint_stakeholder(classical_latin_standard__continuity_reading, living_latin_pedagogy_networks, beneficiary,
    organized, generational, mobile, continental).
narrative_ontology:stakeholder_secondary_role(classical_latin_standard__continuity_reading, living_latin_pedagogy_networks, agenda_setter).

% Learn and use Latin for religious, academic, or hobbyist purposes. They benefit from a lower bar of entry — usage that would be flagged as anachronistic under a strict reconstructionist standard is accepted here — but still bear the ordinary cost of acquiring a difficult inflected language and occasionally face informal correction for genuine barbarisms (errors even the living-practice standard rejects).
narrative_ontology:constraint_stakeholder(classical_latin_standard__continuity_reading, practicing_latinists_general, beneficiary,
    moderate, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(classical_latin_standard__continuity_reading, practicing_latinists_general, payer).

% Individuals who need to interact with an institution that transacts partly in Latin (certain liturgical, legal-historical, or ceremonial contexts) but have no Latin training at all. The continuity standard's low bar relative to reconstructionism does not help them; any Latin competence requirement, however loosely defined, still excludes those with none, and institutional access sometimes assumes at least passive comprehension.
narrative_ontology:constraint_stakeholder(classical_latin_standard__continuity_reading, vernacular_only_speakers_seeking_institutional_latin_access, payer,
    powerless, immediate, constrained, local).

% Hold the reconstructionist view that only Augustan-era usage, recovered through textual archaeology, is 'correct.' They are not part of the continuity-reading's own institutional conversation — their objections that medieval and ecclesiastical forms are corruptions are treated by this reading's adherents as a category error, not adjudicated within it.
narrative_ontology:constraint_stakeholder(classical_latin_standard__continuity_reading, classical_philologists, excluded,
    organized, generational, mobile, global).

% Study the continuity reading as one instance of a general pattern in how living and liturgical languages construct correctness standards, without a stake in which reading of the Latin kernel prevails.
narrative_ontology:constraint_stakeholder(classical_latin_standard__continuity_reading, comparative_linguists, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single working standard of 'correct Latin' that permits continuous use across nearly two millennia of practice — liturgical, scholarly, and pedagogical communities can produce and evaluate Latin text without needing to resolve every usage against a fixed Classical-era baseline, because drift itself is treated as legitimate development rather than error.
% TRANSFER_FUNCTION: Moves interpretive authority and institutional legitimacy toward communities that use post-Classical Latin forms (Church, medieval and Neo-Latin scholarship, living-Latin pedagogy) and away from a purely archaeological standard that would delegitimize their usage; imposes essentially no transfer burden on outsiders beyond the ordinary cost of Latin literacy itself.
% ABSENT_VOICES: Reconstructionist classical philologists are structurally outside this reading's own adjudication process — their claim that ecclesiastical and medieval Latin are corruptions of the true standard is not something the continuity reading treats as a live objection within its framework; it is treated as a different, incompatible project.
% DISAPPEARANCE_RATIONALE: If the continuity reading vanished and a strict reconstructionist standard replaced it as the sole legitimate one, the Church's liturgical Latin, the medieval and Neo-Latin scholarly corpus, and the entire living-Latin pedagogy movement would be reclassified as corrupted or illegitimate Latin overnight — a significant rearrangement for those institutions. But the underlying linguistic practices (people speaking, writing, and teaching Latin in continuous use) would likely continue regardless of which side wins the correctness argument, which is why the verdict is contested rather than a clean world_rearranges.
% FOUNDING_PROBLEM: As Latin diverged from its Classical form through centuries of continuous liturgical, administrative, and vernacular-influenced use, a standard was needed to determine whether the language actually spoken and written by living communities of Latin users counted as 'real' Latin, or whether only texts matching a frozen historical baseline did.
% FOUNDING_PROBLEM_CORROBORATION: Historians of the language attest that the continuity/reconstruction dispute has recurred at multiple points (Carolingian Latin reforms, Renaissance humanist attacks on 'monkish' Latin, 19th-20th century classical philology vs. Church Latin controversies) — this corroboration comes from linguistic historians outside both the ecclesiastical and reconstructionist camps, documenting the dispute as a recurring structural feature of Latin's institutional life rather than a settled matter either side can claim outright.
narrative_ontology:disappearance_verdict(classical_latin_standard__continuity_reading, contested).
narrative_ontology:founding_problem_status(classical_latin_standard__continuity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(classical_latin_standard__continuity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(classical_latin_standard__continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(classical_latin_standard__continuity_reading, 0.38, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(classical_latin_standard__continuity_reading_tests).
:- end_tests(classical_latin_standard__continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored moderate (0.38) because the continuity reading does gatekeep institutional participation (you must acquire Latin literacy, however loosely bounded, to participate in the Church's or the scholarly community's Latin-mediated activities) but does NOT systematically delegitimize any existing practice community's usage — the entire point of this reading is inclusion of drift. Suppression is low (0.22): alternatives (vernacular liturgy, vernacular scholarship) are not suppressed by this reading; people who want to use Latin under a different correctness standard, or not use Latin at all, are not coerced. Accessibility collapse is moderate-low (0.35) — genuine alternatives to Latin literacy persist for almost every purpose this standard serves. Resistance is low-moderate (0.3), coming primarily from reconstructionist philologists who reject this reading's legitimation of drift, not from within the communities that operate under it. Theater ratio rises modestly over the interval (0.10 to 0.28) as more of the standard's maintenance becomes about institutional self-perpetuation (seminary curricula, ceremonial correctness) rather than living communicative need, though it stays well below the piton threshold.
 *
 * DIRECTIONALITY LOGIC:
 *   Ecclesiastical institutions and Neo-Latin/Living-Latin scholarly and pedagogical networks are declared beneficiaries: the continuity reading directly validates their linguistic practice as 'correct Latin' rather than as corrupted or illegitimate, which sustains their institutional and disciplinary standing. Practicing Latinists generally benefit from a lower bar of entry than reconstructionism would impose, though they still bear the ordinary acquisition cost of the language, hence a dual beneficiary/payer role. Vernacular-only speakers who need occasional institutional access are the nearest thing to a victim class here, but the set is genuinely minimal, as the structural delta anticipates — their exclusion is a byproduct of Latin literacy requirements in general, not something the continuity reading specifically constructs or intensifies relative to no standard at all.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (how to treat linguistic communities' actual living usage as legitimate rather than as failure to match a frozen historical target) remains live wherever institutions keep using Latin productively rather than archivally — this is not a mandate that has quietly outlived its function while persisting on inertia. The rising theater ratio is worth watching (ceremonial correctness performance growing relative to communicative need) but has not crossed into the territory where the standard's maintenance is pure performance with no live coordination function underneath.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    continuity_vs_reconstruction_kernel_locus,
    'Is the disagreement between the continuity reading and the reconstruction reading a genuine dispute about what ''Latin'' the kernel refers to, or are they simply talking about two different objects (living-tradition Latin vs. Classical-corpus Latin) that happen to share a name?',
    'Examine whether adherents of each reading, when shown a specific medieval or ecclesiastical usage, disagree about its grammatical status (genuine same-kernel dispute) or simply decline to evaluate it as out-of-scope (different-object talking past each other).',
    'If genuine same-kernel dispute, the two readings genuinely forecloses one another on specific usage judgments even while both persisting institutionally; if talking past each other, the coexists_with relation is even more clearly correct and the ''dispute'' is largely rhetorical.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(continuity_vs_reconstruction_kernel_locus, conceptual, 'Whether continuity and reconstruction readings share a genuine kernel-level disagreement or merely a shared label.').

omega_variable(
    drift_legitimacy_boundary,
    'Where does the continuity reading itself draw the line between legitimate drift and a genuine barbarism/error — and is that line non-arbitrary, or does it just track whichever usage the current institutional gatekeepers already accept?',
    'Historical study of cases where the Church or Neo-Latin scholarly community rejected a proposed usage as a ''barbarism'' versus accepted a structurally similar one as legitimate development — check for a principled distinguishing criterion versus ad hoc institutional preference.',
    'If the line tracks institutional preference rather than a principled linguistic criterion, the continuity reading''s low extractiveness score should be revisited upward, since the ''barbarism'' exclusion becomes a discretionary gatekeeping tool rather than a neutral linguistic judgment.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(drift_legitimacy_boundary, empirical, 'Whether the legitimate-drift/barbarism boundary is principled or institutionally discretionary.').

omega_variable(
    beneficiary_capture_of_naturalness_claim,
    'Does the continuity reading''s framing of drift as ''natural'' function partly as a legitimating cover story for institutions (the Church, Neo-Latin academia) that benefit from validating their own historical usage, independent of any linguistic fact about naturalness?',
    'Compare the continuity reading''s treatment of drift favorable to institutional incumbents (e.g., ecclesiastical vocabulary innovations) against its treatment of drift that would favor outsiders (e.g., vernacular-influenced popular Latin outside institutional control) — asymmetric acceptance would indicate cover-story function.',
    'Symmetric acceptance supports the rope/low-extraction reading as authored; asymmetric acceptance favoring incumbents would push the classification toward tangled_rope, since a genuine coordination story (accepting real linguistic change) would be functioning alongside asymmetric institutional gatekeeping.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_capture_of_naturalness_claim, empirical, 'Whether ''natural drift'' legitimacy is applied symmetrically or selectively favors institutional incumbents.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(classical_latin_standard__continuity_reading, 0, 1600).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clas_tr_t0, classical_latin_standard__continuity_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(clas_tr_t300, classical_latin_standard__continuity_reading, theater_ratio, 300, 0.14).
narrative_ontology:measurement(clas_tr_t700, classical_latin_standard__continuity_reading, theater_ratio, 700, 0.18).
narrative_ontology:measurement(clas_tr_t1000, classical_latin_standard__continuity_reading, theater_ratio, 1000, 0.22).
narrative_ontology:measurement(clas_tr_t1300, classical_latin_standard__continuity_reading, theater_ratio, 1300, 0.25).
narrative_ontology:measurement(clas_tr_t1600, classical_latin_standard__continuity_reading, theater_ratio, 1600, 0.28).

% Extraction over time
narrative_ontology:measurement(clas_be_t0, classical_latin_standard__continuity_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(clas_be_t300, classical_latin_standard__continuity_reading, base_extractiveness, 300, 0.25).
narrative_ontology:measurement(clas_be_t700, classical_latin_standard__continuity_reading, base_extractiveness, 700, 0.3).
narrative_ontology:measurement(clas_be_t1000, classical_latin_standard__continuity_reading, base_extractiveness, 1000, 0.33).
narrative_ontology:measurement(clas_be_t1300, classical_latin_standard__continuity_reading, base_extractiveness, 1300, 0.36).
narrative_ontology:measurement(clas_be_t1600, classical_latin_standard__continuity_reading, base_extractiveness, 1600, 0.38).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(classical_latin_standard__continuity_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(classical_latin_standard__continuity_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(classical_latin_standard__continuity_reading, 0.1).
narrative_ontology:affects_constraint(classical_latin_standard__continuity_reading, classical_latin_standard__reconstruction_reading).
narrative_ontology:affects_constraint(classical_latin_standard__continuity_reading, classical_latin_standard__hybrid_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling constraints decomposing the natural-language concept 'correct Latin' per the ε-invariance principle. continuity_reading (this story, ε≈0.38, rope) treats drift as legitimate; reconstruction_reading (ε expected higher, likely tangled_rope or snare — delegitimizes over a millennium of medieval/ecclesiastical usage as corruption, actively suppressing living-tradition practice in favor of a philologically-reconstructed standard) and hybrid_reading (ε expected intermediate — requires Classical fidelity AND permits domain-specific post-Classical development) are separate files. Each reading has a stable, non-averaged ε assessed by that reading's own lights, per the kernel-reading ε referent rule.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
