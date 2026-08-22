% ============================================================================
% CONSTRAINT STORY: kodashim_commandment_status__study_as_performance
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kodashim_commandment_status__study_as_performance, []).

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
 *   constraint_id: kodashim_commandment_status__study_as_performance
 *   human_readable: Torah Study of Sacrificial Law as Commandment Fulfillment (Kodashim as Performed Mitzvah)
 *   domain: religious/halakhic
 *
 * SUMMARY:
 *   This story instantiates one reading within a three-way kernel contest
 *   over the status of the biblical sacrificial commandments (Kodashim) after
 *   the destruction of the Second Temple. The kernel is the persisting
 *   commitment: 'the sacrificial commandments remain binding.' Three
 *   communities of practice read that commitment differently. This story is
 *   the study_as_performance reading: the classical rabbinic doctrine (traced
 *   to statements attributed to Rav in tractate Menachot, 'whoever studies
 *   the laws of the sin-offering is as though he had offered a sin-offering')
 *   holding that intellectual engagement with the sacrificial law texts
 *   itself discharges the commandment, in full, now, without residue or
 *   deferral. This is NOT the same constraint as performance_only (which
 *   treats the commandment as presently suspended, a husk awaiting an altar)
 *   or messianic_deferral (which treats study as readiness-maintenance for a
 *   still-outstanding future performance). Under this reading specifically,
 *   there is no gap, no debt, no suspension — the commandment's kernel is
 *   fully and continuously occupied by the act of study, so ε is authored
 *   near zero: there is no unperformed obligation for anyone to bear the cost
 *   of.
 *
 * KEY AGENTS:
 *   - torah_scholars: Primary agenda-setters and beneficiaries (institutional/identity_locked) — administer study curricula that constitute the reading's continued authority
 *   - yeshiva_institutions: Secondary beneficiaries (organized/constrained) — derive institutional purpose and resource allocation from the doctrine
 *   - diaspora_observant_community: Beneficiaries (moderate/mobile) — relieved of standing unfulfillable-obligation status
 *   - performance_only_adherents: Excluded voice (moderate/mobile) — hold a competing, non-harmed but sidelined reading
 *   - halakhic_courts: Analytical observers (institutional/analytical) — adjudicate across readings without resolving the kernel
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kodashim_commandment_status__study_as_performance, 0.03).
domain_priors:suppression_score(kodashim_commandment_status__study_as_performance, 0.08).
domain_priors:theater_ratio(kodashim_commandment_status__study_as_performance, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kodashim_commandment_status__study_as_performance, extractiveness, 0.03).
narrative_ontology:constraint_metric(kodashim_commandment_status__study_as_performance, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(kodashim_commandment_status__study_as_performance, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kodashim_commandment_status__study_as_performance, accessibility_collapse, 0.2).
narrative_ontology:constraint_metric(kodashim_commandment_status__study_as_performance, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kodashim_commandment_status__study_as_performance, rope).
narrative_ontology:human_readable(kodashim_commandment_status__study_as_performance, "Torah Study of Sacrificial Law as Commandment Fulfillment (Kodashim as Performed Mitzvah)").
narrative_ontology:topic_domain(kodashim_commandment_status__study_as_performance, "religious/halakhic").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kodashim_commandment_status__study_as_performance, 'dc6ac4d7-18c2-42e4-b159-1dfe021bc4af').
narrative_ontology:cs_kernel_codification('dc6ac4d7-18c2-42e4-b159-1dfe021bc4af', fixed_text).
narrative_ontology:cs_authority_grounding('dc6ac4d7-18c2-42e4-b159-1dfe021bc4af', lineage).
narrative_ontology:cs_interpretation_layer_present('dc6ac4d7-18c2-42e4-b159-1dfe021bc4af').
narrative_ontology:cs_reading_relation('dc6ac4d7-18c2-42e4-b159-1dfe021bc4af', kodashim_commandment_status__performance_only, forecloses).
narrative_ontology:cs_reading_relation('dc6ac4d7-18c2-42e4-b159-1dfe021bc4af', kodashim_commandment_status__messianic_deferral, coexists_with).
narrative_ontology:cs_axiom('dc6ac4d7-18c2-42e4-b159-1dfe021bc4af', foundational, study_constitutes_full_commandment_fulfillment).
narrative_ontology:cs_axiom_status(study_constitutes_full_commandment_fulfillment, holdable).
narrative_ontology:cs_axiom_grounding('dc6ac4d7-18c2-42e4-b159-1dfe021bc4af', study_constitutes_full_commandment_fulfillment, conventional).
narrative_ontology:cs_axiom('dc6ac4d7-18c2-42e4-b159-1dfe021bc4af', secondary, no_outstanding_ritual_debt_exists_post_destruction).
narrative_ontology:cs_axiom_status(no_outstanding_ritual_debt_exists_post_destruction, holdable).
narrative_ontology:cs_axiom_grounding('dc6ac4d7-18c2-42e4-b159-1dfe021bc4af', no_outstanding_ritual_debt_exists_post_destruction, deontological).
narrative_ontology:cs_reference_frame('dc6ac4d7-18c2-42e4-b159-1dfe021bc4af', sacrificial_cult_operative_temple_era).
narrative_ontology:cs_drift_state('dc6ac4d7-18c2-42e4-b159-1dfe021bc4af', post_destruction_rabbinic_consolidation, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('dc6ac4d7-18c2-42e4-b159-1dfe021bc4af', '').
narrative_ontology:cs_kernel_id(kodashim_commandment_status__study_as_performance, kodashim_commandment_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kodashim_commandment_status__study_as_performance, torah_scholars).
narrative_ontology:constraint_beneficiary(kodashim_commandment_status__study_as_performance, yeshiva_institutions).
narrative_ontology:constraint_beneficiary(kodashim_commandment_status__study_as_performance, diaspora_observant_community).
narrative_ontology:constraint_vindicates(kodashim_commandment_status__study_as_performance, torah_study_equivalence_doctrine).
narrative_ontology:constraint_vindicates(kodashim_commandment_status__study_as_performance, oral_recitation_as_sacrificial_substitute).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Study, teach, and transmit the tractates of Kodashim (Zevachim, Menachot, and related sacrificial law) as an unbroken intellectual practice. Their study is held, within this reading, to itself discharge the commandment. They administer this reading through yeshiva curricula and halakhic responsa that treat textual mastery as commandment-equivalent, and they derive scholarly standing, teaching authority, and communal role from being the custodians of the kernel's continued occupation.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__study_as_performance, torah_scholars, agenda_setter,
    institutional, civilizational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(kodashim_commandment_status__study_as_performance, torah_scholars, beneficiary).

% Organize curricula, ordination tracks, and communal prestige structures around the study of Kodashim as a full and living discipline rather than a historical curiosity. Their institutional continuity benefits from a reading in which the commandment is fully alive through study, since it justifies dedicating substantial communal resources to a body of law with no physical referent.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__study_as_performance, yeshiva_institutions, beneficiary,
    organized, generational, constrained, global).

% Live under a religious framework in which the absence of the Temple and its sacrificial cult does not represent an unresolved rupture or ongoing deficiency. Under this reading they are not falling short of a commandment they cannot perform; the study available to them in synagogue and home settings is itself sufficient. This resolves what would otherwise be a standing, unfulfillable obligation into a fully satisfiable one.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__study_as_performance, diaspora_observant_community, beneficiary,
    moderate, generational, mobile, global).

% Hold that the commandment is contingent on an operative altar and is presently suspended rather than fulfilled through study. They are not harmed by this reading but are structurally sidelined by it within communities that adopt study-as-performance as normative practice; their competing account of the kernel's status is not represented in the study-as-performance framework's own self-description.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__study_as_performance, performance_only_adherents, excluded,
    moderate, generational, mobile, global).

% Adjudicate disputes about which reading of the sacrificial commandment's status governs practice, liturgy, and legal standing in matters such as vows or communal obligation. They can examine all three readings but do not unilaterally resolve the underlying theological contest.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__study_as_performance, halakhic_courts, observer,
    institutional, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates continuity of religious and legal knowledge across a multi-millennial gap in Temple-based practice: it keeps an entire body of law (Kodashim) alive, transmitted, and authoritative among practitioners even though the physical institution it originally regulated does not exist, preventing the law from becoming a dead letter.
% TRANSFER_FUNCTION: Moves scholarly prestige, institutional resources, and communal legitimacy toward those who master and teach Kodashim; it does not move material goods or labor from any victim group, since the reading holds that no unperformed obligation, and therefore no unpaid debt, exists.
% ABSENT_VOICES: Adherents of the performance_only reading, who hold the commandment merely suspended rather than fulfilled, are not represented within the study-as-performance framework's own self-justification; their account would treat the current arrangement as a graceful holding pattern rather than complete satisfaction.
% DISAPPEARANCE_RATIONALE: If the study-as-performance doctrine vanished, the enormous communal investment in Kodashim study would lose its distinctive theological warrant (equivalence to sacrificial performance) and could contract toward historical or academic study alone; some communities would shift toward the messianic_deferral framing (mere readiness-maintenance) or the performance_only framing (acknowledged suspension), changing how scholarly time and institutional resources are allocated and how observant Jews without Temple access understand their standing before the commandment.
% FOUNDING_PROBLEM: After the Temple's destruction, the sacrificial commandments could no longer be physically performed, creating a apparent permanent breach in a body of divine commandments with no clear halakhic resolution for how observant practice should relate to laws it can never again enact.
% FOUNDING_PROBLEM_CORROBORATION: Talmudic sources themselves (attributed within the tradition to Rav, in tractate Menachot) are cited by proponents as founding the doctrine, which is an internal source rather than an outside check. Academic historians of rabbinic literature, writing from outside the community that benefits from the doctrine, corroborate that the equivalence doctrine emerged as a documented adaptive response to the Temple's destruction rather than as a claim present in the Torah text itself, supporting a contested rather than settled status for whether study genuinely completes the commandment or is better read as a coping and continuity mechanism.
narrative_ontology:disappearance_verdict(kodashim_commandment_status__study_as_performance, world_rearranges).
narrative_ontology:founding_problem_status(kodashim_commandment_status__study_as_performance, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kodashim_commandment_status__study_as_performance, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(kodashim_commandment_status__study_as_performance, 'none', 1).
narrative_ontology:epsilon_provenance(kodashim_commandment_status__study_as_performance, 0.03, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kodashim_commandment_status__study_as_performance_tests).
:- end_tests(kodashim_commandment_status__study_as_performance_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored near-zero (0.03) because this reading's own structural claim is that no one is shorted: the commandment is fully satisfied by study, so there is no unperformed labor, no withheld sacrifice, no deferred cost landing on any party. Suppression is low (0.08) because no coercive machinery is required to sustain the doctrine — its persistence rests on voluntary scholarly and communal adoption, reinforced by centuries of citation rather than enforcement. Theater ratio is authored low-but-nonzero and slowly rising (0.05 to 0.10 across the interval) to reflect that some study of Kodashim, especially in contexts with little practical halakhic application, functions partly as ritualized continuity-performance (reciting texts on fixed liturgical schedules) alongside genuine intellectual engagement, without this becoming the dominant mode. Accessibility collapse (0.20) and resistance (0.15) are both modest: alternative readings (performance_only, messianic_deferral) remain fully visible and openly held by other communities, so this reading has not collapsed the alternative-framing space, and it meets little active resistance because it is not compulsory — it wins adoption through use rather than suppression of dissent.
 *
 * DIRECTIONALITY LOGIC:
 *   Torah scholars and yeshiva institutions sit near the full-beneficiary end: their authority, curriculum, and communal function are constituted by treating study as commandment-fulfilling, so directionality derives low d for them structurally, reinforced by the beneficiary declarations. The diaspora observant community also derives low-to-symmetric d: they benefit from a resolved theological status (no standing unfulfillable duty) at negligible cost. There is no declared victim group in this reading — the expected structural delta explicitly states the victim set is empty, since no one is harmed by non-performance under this reading's own terms. Performance_only_adherents are marked excluded rather than victim: they experience a competing framework being sidelined in communities that adopt this reading, not extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading resists a certain mandatrophy misreading: one might assume that because the physical Temple and altar no longer exist, any commandment referencing them must be a dead mandate persisting only through institutional inertia (a piton). The study_as_performance doctrine forecloses that misreading on its own terms by relocating the commandment's site of fulfillment from the altar to the study hall — the founding problem (unfulfillable commandment after Temple destruction) is treated as fully resolved, not merely deferred or reduced to husk-maintenance, so the founding_problem_status is authored as contested rather than dead: proponents hold it fully live and fully met; outside historians corroborate the doctrine's documented adaptive origin without adjudicating its theological truth.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    study_as_genuine_fulfillment_vs_coping_doctrine,
    'Is ''study equals sacrifice'' a genuine theological discovery about the nature of the commandment, or a historically contingent coping doctrine constructed to preserve communal coherence and scholarly authority after the Temple''s destruction?',
    'Comparative textual analysis of pre-destruction versus post-destruction rabbinic sources: if the equivalence principle has meaningful pre-destruction analogues (e.g., broader statements about Torah study substituting for other mitzvot), that would support a genuine-doctrine reading; if the doctrine appears abruptly and specifically calibrated to the sacrificial gap, that supports a constructed-coping reading.',
    'If constructed-coping, the near-zero extractiveness this story authors may understate a hidden cost: the doctrine could be functioning to suppress recognition of genuine religious loss, redirecting what would otherwise be grief or reform pressure into scholarly prestige accumulation for torah_scholars — which would push this reading''s true structure toward a mild tangled_rope rather than a clean rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(study_as_genuine_fulfillment_vs_coping_doctrine, conceptual, 'Whether the study-equivalence doctrine is a genuine theological insight or an adaptive institutional coping mechanism.').

omega_variable(
    kernel_framing_under_determination,
    'Is the kernel best framed as ''the sacrificial commandment''s binding status'' (the framing this story adopts, which yields three symmetric sibling readings) or as ''rabbinic authority to reinterpret Torah commandments without a Sanhedrin or prophetic warrant'' (an alternative framing under which this story and its siblings become instances of a single, more contested claim about interpretive authority itself)?',
    'Examine whether halakhic courts (or comparable communal bodies) treat disputes among the three readings as disputes about the commandment''s content, or as disputes about who has standing to declare a commandment''s status changed. If courts consistently invoke authority-of-interpretation arguments rather than content arguments, the alternative framing is operative.',
    'Under the content framing (adopted here), this story is a clean, low-extraction rope-like reading with empty victim set. Under the authority framing, all three readings would be recast as competing claims about who gets to speak for the tradition, and the study_as_performance reading in particular would show elevated extraction toward torah_scholars, since it is the reading that most concentrates interpretive and commandment-fulfilling authority in the scholarly class itself.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_under_determination, conceptual, 'Whether the kernel is about commandment content or about interpretive authority, with different classification consequences for this reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kodashim_commandment_status__study_as_performance, 0, 1950).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(koda_tr_t0, kodashim_commandment_status__study_as_performance, theater_ratio, 0, 0.05).
narrative_ontology:measurement(koda_tr_t400, kodashim_commandment_status__study_as_performance, theater_ratio, 400, 0.06).
narrative_ontology:measurement(koda_tr_t800, kodashim_commandment_status__study_as_performance, theater_ratio, 800, 0.08).
narrative_ontology:measurement(koda_tr_t1200, kodashim_commandment_status__study_as_performance, theater_ratio, 1200, 0.08).
narrative_ontology:measurement(koda_tr_t1600, kodashim_commandment_status__study_as_performance, theater_ratio, 1600, 0.09).
narrative_ontology:measurement(koda_tr_t1950, kodashim_commandment_status__study_as_performance, theater_ratio, 1950, 0.1).

% Extraction over time
narrative_ontology:measurement(koda_be_t0, kodashim_commandment_status__study_as_performance, base_extractiveness, 0, 0.02).
narrative_ontology:measurement(koda_be_t400, kodashim_commandment_status__study_as_performance, base_extractiveness, 400, 0.02).
narrative_ontology:measurement(koda_be_t800, kodashim_commandment_status__study_as_performance, base_extractiveness, 800, 0.03).
narrative_ontology:measurement(koda_be_t1200, kodashim_commandment_status__study_as_performance, base_extractiveness, 1200, 0.03).
narrative_ontology:measurement(koda_be_t1600, kodashim_commandment_status__study_as_performance, base_extractiveness, 1600, 0.03).
narrative_ontology:measurement(koda_be_t1950, kodashim_commandment_status__study_as_performance, base_extractiveness, 1950, 0.03).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(kodashim_commandment_status__study_as_performance, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(kodashim_commandment_status__study_as_performance, kodashim_commandment_status__performance_only).
narrative_ontology:affects_constraint(kodashim_commandment_status__study_as_performance, kodashim_commandment_status__messianic_deferral).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the kodashim_commandment_status kernel. performance_only authors substantially higher ε (an acknowledged, unresolved deficiency treated as a suspended husk) and a nonempty victim-adjacent framing (observant Jews experiencing an unfulfilled duty); messianic_deferral authors an intermediate ε (study as readiness-maintenance for a still-outstanding future performance, so a debt is acknowledged but not yet due). study_as_performance authors the lowest ε of the three because it alone claims the debt is fully and presently discharged. All three share the same underlying text corpus (tractates Zevachim, Menachot, Tamid) and the same absent physical referent (no operative Temple); they differ only in how each reading resolves what that absence means for the commandment's present status.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
