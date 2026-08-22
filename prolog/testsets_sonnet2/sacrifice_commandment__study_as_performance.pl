% ============================================================================
% CONSTRAINT STORY: sacrifice_commandment__study_as_performance
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sacrifice_commandment__study_as_performance, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
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
 *   constraint_id: sacrifice_commandment__study_as_performance
 *   human_readable: Study of Sacrifice Law as Fulfillment of the Commandment
 *   domain: religious/halakhic
 *
 * SUMMARY:
 *   This constraint isolates one specific doctrinal reading of the sacrifice
 *   commandment under the loss of the Temple: that intellectual study of the
 *   laws of sacrifice (Kodashim) is not merely preparatory but IS itself the
 *   performance of the commandment, discharging the obligation directly
 *   through intellectual engagement. This is one of three structurally
 *   distinct readings of the same kernel — the persisting textual commitment
 *   to the sacrificial commandments in the absence of a functioning Temple.
 *   The archive_maintenance reading treats study as preservation for a future
 *   restoration (a different beneficiary structure: future priesthood, not
 *   present scholar). The performance_only reading holds the commandment is
 *   simply suspended, unfulfillable by any substitute (this reading has
 *   victims: the pious who cannot discharge a live obligation). This reading,
 *   study_as_performance, is the one under analysis here: it authors
 *   near-zero extraction because no coercion or asymmetric cost is present in
 *   its account — the scholar chooses freely, and the coordination good
 *   (sustaining communal religious practice after catastrophic institutional
 *   loss) accrues without an identifiable payer.
 *
 * KEY AGENTS:
 *   - scholar_worshippers: beneficiary/agenda_setter (moderate/mobile) — treats study as fulfillment
 *   - yeshiva_institutions: beneficiary (organized/mobile) — structures curricula around the equivalence doctrine
 *   - temple_restorationists: excluded (moderate/constrained) — holds the sibling archive_maintenance reading
 *   - strict_constructionist_halakhists: excluded (moderate/constrained) — holds the sibling performance_only reading
 *   - religious_studies_observers: observer (analytical/analytical) — traces the doctrine's genealogy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sacrifice_commandment__study_as_performance, 0.03).
domain_priors:suppression_score(sacrifice_commandment__study_as_performance, 0.08).
domain_priors:theater_ratio(sacrifice_commandment__study_as_performance, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sacrifice_commandment__study_as_performance, extractiveness, 0.03).
narrative_ontology:constraint_metric(sacrifice_commandment__study_as_performance, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(sacrifice_commandment__study_as_performance, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sacrifice_commandment__study_as_performance, accessibility_collapse, 0.2).
narrative_ontology:constraint_metric(sacrifice_commandment__study_as_performance, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sacrifice_commandment__study_as_performance, rope).
narrative_ontology:human_readable(sacrifice_commandment__study_as_performance, "Study of Sacrifice Law as Fulfillment of the Commandment").
narrative_ontology:topic_domain(sacrifice_commandment__study_as_performance, "religious/halakhic").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sacrifice_commandment__study_as_performance, '2431d59b-4ddb-4e5b-b0da-a1d5fadf5434').
narrative_ontology:cs_kernel_codification('2431d59b-4ddb-4e5b-b0da-a1d5fadf5434', fixed_text).
narrative_ontology:cs_authority_grounding('2431d59b-4ddb-4e5b-b0da-a1d5fadf5434', practice).
narrative_ontology:cs_interpretation_layer_present('2431d59b-4ddb-4e5b-b0da-a1d5fadf5434').
narrative_ontology:cs_reading_relation('2431d59b-4ddb-4e5b-b0da-a1d5fadf5434', sacrifice_commandment__performance_only, coexists_with).
narrative_ontology:cs_reading_relation('2431d59b-4ddb-4e5b-b0da-a1d5fadf5434', sacrifice_commandment__archive_maintenance, influences).
narrative_ontology:cs_axiom('2431d59b-4ddb-4e5b-b0da-a1d5fadf5434', foundational, intellectual_engagement_constitutes_performance).
narrative_ontology:cs_axiom_status(intellectual_engagement_constitutes_performance, holdable).
narrative_ontology:cs_axiom_grounding('2431d59b-4ddb-4e5b-b0da-a1d5fadf5434', intellectual_engagement_constitutes_performance, theological).
narrative_ontology:cs_axiom('2431d59b-4ddb-4e5b-b0da-a1d5fadf5434', secondary, study_discharges_obligation_without_remainder).
narrative_ontology:cs_axiom_status(study_discharges_obligation_without_remainder, holdable).
narrative_ontology:cs_axiom_grounding('2431d59b-4ddb-4e5b-b0da-a1d5fadf5434', study_discharges_obligation_without_remainder, conventional).
narrative_ontology:cs_reference_frame('2431d59b-4ddb-4e5b-b0da-a1d5fadf5434', temple_era_sacrificial_praxis).
narrative_ontology:cs_drift_state('2431d59b-4ddb-4e5b-b0da-a1d5fadf5434', post_destruction_rabbinic_era, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('2431d59b-4ddb-4e5b-b0da-a1d5fadf5434', '').
narrative_ontology:cs_kernel_id(sacrifice_commandment__study_as_performance, sacrifice_commandment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sacrifice_commandment__study_as_performance, scholar_worshippers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(sacrifice_commandment__study_as_performance, yeshiva_institutions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Engage in sustained study of the sacrificial (Kodashim/Zevachim) tractates as their primary devotional practice. They report that the study itself — the intellectual reconstruction of the sacrificial order, its parameters, disqualifications, and procedures — constitutes fulfillment of the commandment in the absence of the Temple. Nothing is extracted from them by this reading; the practice is claimed as intrinsically rewarding and freely chosen among available forms of religious observance.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__study_as_performance, scholar_worshippers, beneficiary,
    moderate, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(sacrifice_commandment__study_as_performance, scholar_worshippers, agenda_setter).

% Structure curricula around intensive study of Kodashim, teaching that such study is not preparatory but is itself the performance of the mitzvah. This legitimizes the centrality of textual study as the primary religious activity of the institution and its students, and channels communal prestige and resources toward Talmudic scholarship of sacrificial law specifically.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__study_as_performance, yeshiva_institutions, beneficiary,
    organized, generational, mobile, national).

% Hold that study is preparatory to eventual physical restoration of sacrificial worship, not a substitute for it. They are not victims of this reading, but their framing (archive_maintenance) is displaced from the ritual center of gravity when study is treated as performance rather than preservation; they would object that this reading risks satisfying the longing for restoration rather than sustaining it.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__study_as_performance, temple_restorationists, excluded,
    moderate, generational, constrained, national).

% Hold the performance_only reading: that the commandment requires physical execution and is suspended, not fulfilled, absent the Temple. They are not in this constraint's conversation because this reading proceeds as if the equivalence claim is settled; from their seat the study-as-performance claim substitutes a comforting substitute for genuine unfulfilled obligation.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__study_as_performance, strict_constructionist_halakhists, excluded,
    moderate, generational, constrained, national).

% Analyze the doctrinal history of the study-as-performance claim (rooted in rabbinic statements such as 'whoever studies the laws of the sin-offering is as if he offered a sin-offering') and its function in sustaining religious continuity after 70 CE without asserting its theological truth.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__study_as_performance, religious_studies_observers, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, practicable form of religious observance for a commandment whose physical performance became structurally impossible after the Temple's destruction, allowing the community's devotional and educational life to continue coordinated around a shared text and shared standard of fulfillment.
% TRANSFER_FUNCTION: Moves religious legitimacy and communal esteem toward textual scholarship and scholars of sacrificial law; moves nothing extractive from any party — no payment, labor, or deference is coercively transferred from a victim group to a beneficiary group under this reading.
% ABSENT_VOICES: Temple restorationists and strict constructionists would object that this reading resolves an unresolved rupture too comfortably, treating a genuine absence (no functioning Temple, no priesthood in service) as already made whole by intellectual activity; they are structurally present in the wider tradition but excluded from this reading's own internal account of itself.
% DISAPPEARANCE_RATIONALE: If this reading vanished, dedicated study of Kodashim would likely continue as a scholarly or archival practice (per archive_maintenance) or would be recast as unfulfilled obligation awaiting restoration (per performance_only) — the practice of study itself would very likely persist, but its theological status and the esteem attached to it would shift, which is why the parties dispute how much would actually change.
% FOUNDING_PROBLEM: After the destruction of the Second Temple in 70 CE, the commandment to offer sacrifices became physically unperformable, creating an urgent theological problem: how can obligations grounded in a now-impossible physical act remain binding and dischargeable.
% FOUNDING_PROBLEM_CORROBORATION: Rabbinic sources internal to the tradition (e.g., attributed statements in Talmud Menachot and Vayikra Rabbah equating study of sacrificial law with the sacrificial act) attest the problem was met by asserting study's equivalence. Historians of religion, working from outside the beneficiary community, corroborate that the doctrine emerged specifically as a documented post-destruction adaptation rather than a pre-existing teaching, which supports its reading as a genealogically motivated solution rather than an eternal truth, without adjudicating its theological validity.
narrative_ontology:disappearance_verdict(sacrifice_commandment__study_as_performance, contested).
narrative_ontology:founding_problem_status(sacrifice_commandment__study_as_performance, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sacrifice_commandment__study_as_performance, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(sacrifice_commandment__study_as_performance, 'none', 1).
narrative_ontology:epsilon_provenance(sacrifice_commandment__study_as_performance, 0.03, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sacrifice_commandment__study_as_performance_tests).
:- end_tests(sacrifice_commandment__study_as_performance_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored near zero (0.03) because, by this reading's own lights, no party pays a cost through the arrangement: study is voluntary, self-rewarding, and the beneficiary and the participant are the same person. Suppression is low (0.08) — no one is coerced into believing the equivalence doctrine; it competes openly with the sibling readings within the tradition. Theater ratio is low but slowly rising (0.05 to 0.10 across the interval) reflecting a plausible drift where institutional study of Kodashim becomes partly performative status-signaling within yeshiva culture over centuries, without approaching a level that would indicate the coordination function has been hollowed out. Accessibility collapse is moderate (0.2): once inside a tradition that accepts the equivalence doctrine, alternative framings recede but are never fully foreclosed, since the sibling readings persist as live options within the same broader community.
 *
 * PERSPECTIVAL GAP:
 *   From the scholar-worshipper's seat, the constraint computes as pure coordination (rope): a genuine, freely chosen devotional practice sustaining continuity of religious life. From the strict constructionist's seat, the same textual practice would compute differently under their own reading (a different constraint, performance_only, not this one) as an unfulfilled and possibly self-deceiving substitute. This story does not average those seats — it authors only the study_as_performance seat's own account, per Rule 1's ε-invariance discipline.
 *
 * DIRECTIONALITY LOGIC:
 *   Scholar-worshippers and yeshiva institutions are declared beneficiaries because, under this reading, the practice is intrinsically rewarding, non-coerced, and enhances their standing without extracting from any other party. No victims are declared under this reading, consistent with the expected structural delta: this is a coordination-only reading with no asymmetric extraction. The excluded parties (restorationists, strict constructionists) are not victims of this constraint — they simply hold different readings of the same kernel and are excluded from this reading's internal frame, not harmed by it.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (unperformable Temple commandments after 70 CE) is authored as contested-status rather than flatly dead or live: the physical Temple service remains unperformable (in that narrow sense the founding problem persists), but this reading's own claim is that the problem has been genuinely resolved via reinterpretation, not merely papered over. The mismatch check (status=contested against disappearance_verdict=contested) does not trigger a zombie-arrangement flag — both axes register the same live internal dispute rather than a status/verdict divergence, which is the honest signal for a doctrine still actively defended and actively opposed after nearly two millennia rather than one that has quietly ossified.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    equivalence_doctrine_theological_status,
    'Is the claim that ''study of sacrifice law fulfills the commandment'' a genuine theological discovery recovering an eternal truth, or a constructed doctrinal adaptation manufactured specifically to resolve the crisis of the Temple''s destruction?',
    'Textual-historical analysis of when and where the equivalence statements (Talmud Menachot 110a, Vayikra Rabbah) first appear relative to the destruction of the Temple, and whether structurally similar equivalence claims exist for other commandments that were never rendered impossible (which would suggest the doctrine is a general hermeneutic principle rather than a destruction-specific improvisation).',
    'If the doctrine is shown to be a documented post-destruction innovation with no earlier textual basis, the reading''s claim to be recovering rather than constructing an equivalence becomes harder to sustain, though this would not by itself change the zero-extraction structural finding for who benefits and who pays under the reading as currently held.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(equivalence_doctrine_theological_status, empirical, 'Whether the study-as-performance doctrine is a recovered eternal truth or a constructed crisis-response adaptation.').

omega_variable(
    sibling_reading_coexistence_stability,
    'Can the study_as_performance, performance_only, and archive_maintenance readings coexist indefinitely within the same tradition, or does the ascendance of one reading structurally erode the communal resources and legitimacy available to the others over time?',
    'Track institutional resource allocation (yeshiva curricula, publishing volume, communal prestige markers) across communities that emphasize each reading, over multi-generational intervals, to detect whether one reading''s growth measurably displaces the others.',
    'If study_as_performance''s institutional dominance is shown to be displacing resources from restorationist practice (e.g., reduced communal urgency toward Temple-rebuilding activity), the ''influences'' relation to archive_maintenance in cs_structure would need strengthening from mere coexistence toward measurable structural pressure.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sibling_reading_coexistence_stability, conceptual, 'Whether the three kernel readings genuinely coexist or one gradually displaces communal investment in the others.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sacrifice_commandment__study_as_performance, 0, 1950).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sacr_tr_t0, sacrifice_commandment__study_as_performance, theater_ratio, 0, 0.05).
narrative_ontology:measurement(sacr_tr_t400, sacrifice_commandment__study_as_performance, theater_ratio, 400, 0.06).
narrative_ontology:measurement(sacr_tr_t900, sacrifice_commandment__study_as_performance, theater_ratio, 900, 0.08).
narrative_ontology:measurement(sacr_tr_t1400, sacrifice_commandment__study_as_performance, theater_ratio, 1400, 0.09).
narrative_ontology:measurement(sacr_tr_t1750, sacrifice_commandment__study_as_performance, theater_ratio, 1750, 0.1).
narrative_ontology:measurement(sacr_tr_t1950, sacrifice_commandment__study_as_performance, theater_ratio, 1950, 0.1).

% Extraction over time
narrative_ontology:measurement(sacr_be_t0, sacrifice_commandment__study_as_performance, base_extractiveness, 0, 0.02).
narrative_ontology:measurement(sacr_be_t400, sacrifice_commandment__study_as_performance, base_extractiveness, 400, 0.02).
narrative_ontology:measurement(sacr_be_t900, sacrifice_commandment__study_as_performance, base_extractiveness, 900, 0.03).
narrative_ontology:measurement(sacr_be_t1400, sacrifice_commandment__study_as_performance, base_extractiveness, 1400, 0.03).
narrative_ontology:measurement(sacr_be_t1750, sacrifice_commandment__study_as_performance, base_extractiveness, 1750, 0.03).
narrative_ontology:measurement(sacr_be_t1950, sacrifice_commandment__study_as_performance, base_extractiveness, 1950, 0.03).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(sacrifice_commandment__study_as_performance, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sacrifice_commandment__study_as_performance, identity_coordination).
narrative_ontology:affects_constraint(sacrifice_commandment__study_as_performance, sacrifice_commandment__performance_only).
narrative_ontology:affects_constraint(sacrifice_commandment__study_as_performance, sacrifice_commandment__archive_maintenance).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the sacrifice_commandment kernel. sacrifice_commandment__performance_only authors the reading in which the commandment is suspended rather than fulfilled (with an identifiable victim class: the pious bound to an unfulfillable obligation). sacrifice_commandment__archive_maintenance authors the reading in which study is preparatory preservation for a future Temple restoration, with the beneficiary displaced to a future priesthood rather than the present scholar. Each reading carries its own ε, its own beneficiary/victim structure, and its own claimed_type; they are linked here rather than merged, per the ε-invariance principle — the three readings do not average to one 'true' constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
