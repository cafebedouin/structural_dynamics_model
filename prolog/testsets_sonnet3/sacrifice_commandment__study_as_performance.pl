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
 *   constraint_id: sacrifice_commandment__study_as_performance
 *   human_readable: Study of Sacrifice Law as Fulfillment of the Commandment (Talmud Torah Reading)
 *   domain: religious/legal-theological
 *
 * SUMMARY:
 *   This story instantiates the 'study_as_performance' reading of the
 *   sacrifice_commandment kernel: the Talmudic and later halakhic position
 *   that engaged study of the laws of sacrifice (korbanot) constitutes actual
 *   fulfillment of the sacrificial commandments themselves, not merely a
 *   substitute or preparatory activity, following the principle derived from
 *   Menachot 110a that 'whoever engages in the study of the Torah of the
 *   sin-offering is as though he had brought a sin-offering.' This reading is
 *   generated as its own clean constraint, independent of the sibling
 *   readings (performance_only: physical execution required, commandment
 *   suspended without Temple; archive_maintenance: study preserves technical
 *   knowledge for future restoration, not present worship). The three
 *   readings are not measurement variants of one constraint — they have
 *   structurally distinct victim/beneficiary sets, distinct extractiveness
 *   profiles, and distinct claims about what discharges obligation, so per
 *   the ε-invariance principle they are authored as three separate constraint
 *   files linked through network.affects_constraints, not folded into one
 *   story with a parameter.
 *
 * KEY AGENTS:
 *   - scholar_worshippers: Primary beneficiary (moderate/mobile) — study fulfills obligation with no cost incurred
 *   - rabbinic_interpretive_tradition: Agenda-setter (institutional/mobile) — transmits and authorizes the doctrine
 *   - diaspora_and_post_destruction_communities: Beneficiary (powerless/constrained) — relieved of an otherwise unfulfillable obligation
 *   - advocates_of_performance_only_reading: Excluded from this constraint — holds sibling reading
 *   - advocates_of_archive_maintenance_reading: Excluded from this constraint — holds sibling reading
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sacrifice_commandment__study_as_performance, 0.03).
domain_priors:suppression_score(sacrifice_commandment__study_as_performance, 0.05).
domain_priors:theater_ratio(sacrifice_commandment__study_as_performance, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sacrifice_commandment__study_as_performance, extractiveness, 0.03).
narrative_ontology:constraint_metric(sacrifice_commandment__study_as_performance, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(sacrifice_commandment__study_as_performance, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sacrifice_commandment__study_as_performance, accessibility_collapse, 0.15).
narrative_ontology:constraint_metric(sacrifice_commandment__study_as_performance, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sacrifice_commandment__study_as_performance, rope).
narrative_ontology:human_readable(sacrifice_commandment__study_as_performance, "Study of Sacrifice Law as Fulfillment of the Commandment (Talmud Torah Reading)").
narrative_ontology:topic_domain(sacrifice_commandment__study_as_performance, "religious/legal-theological").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sacrifice_commandment__study_as_performance, '88b4d240-8b9e-40bb-bfc4-49df9eedcbb7').
narrative_ontology:cs_kernel_codification('88b4d240-8b9e-40bb-bfc4-49df9eedcbb7', fixed_text).
narrative_ontology:cs_authority_grounding('88b4d240-8b9e-40bb-bfc4-49df9eedcbb7', lineage).
narrative_ontology:cs_interpretation_layer_present('88b4d240-8b9e-40bb-bfc4-49df9eedcbb7').
narrative_ontology:cs_reading_relation('88b4d240-8b9e-40bb-bfc4-49df9eedcbb7', sacrifice_commandment__performance_only, coexists_with).
narrative_ontology:cs_reading_relation('88b4d240-8b9e-40bb-bfc4-49df9eedcbb7', sacrifice_commandment__archive_maintenance, influences).
narrative_ontology:cs_axiom('88b4d240-8b9e-40bb-bfc4-49df9eedcbb7', foundational, intellectual_engagement_constitutes_ritual_discharge).
narrative_ontology:cs_axiom_status(intellectual_engagement_constitutes_ritual_discharge, holdable).
narrative_ontology:cs_axiom_grounding('88b4d240-8b9e-40bb-bfc4-49df9eedcbb7', intellectual_engagement_constitutes_ritual_discharge, conventional).
narrative_ontology:cs_axiom('88b4d240-8b9e-40bb-bfc4-49df9eedcbb7', secondary, verbal_torah_study_ontologically_equivalent_to_physical_offering).
narrative_ontology:cs_axiom_status(verbal_torah_study_ontologically_equivalent_to_physical_offering, holdable).
narrative_ontology:cs_axiom_grounding('88b4d240-8b9e-40bb-bfc4-49df9eedcbb7', verbal_torah_study_ontologically_equivalent_to_physical_offering, theological).
narrative_ontology:cs_reference_frame('88b4d240-8b9e-40bb-bfc4-49df9eedcbb7', temple_era_direct_sacrificial_service).
narrative_ontology:cs_drift_state('88b4d240-8b9e-40bb-bfc4-49df9eedcbb7', post_destruction_rabbinic_reconstitution, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('88b4d240-8b9e-40bb-bfc4-49df9eedcbb7', '').
narrative_ontology:cs_kernel_id(sacrifice_commandment__study_as_performance, sacrifice_commandment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sacrifice_commandment__study_as_performance, scholar_worshippers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(sacrifice_commandment__study_as_performance, diaspora_and_post_destruction_communities).
narrative_ontology:constraint_vindicates(sacrifice_commandment__study_as_performance, torah_study_equivalent_to_sacrifice_doctrine).
narrative_ontology:constraint_vindicates(sacrifice_commandment__study_as_performance, verbal_and_intellectual_service_as_avodah).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Study the sacrificial statutes (Zevachim, Menachot, and related tractates) as a devotional and legal practice, holding that the act of intellectual engagement with the law of a given sacrifice discharges the commandment associated with it, per the Talmudic dictum on מסכת תמורה/מנחות study substituting for offering. They receive nothing extractive from anyone; the practice is self-contained — the study itself constitutes the payment and the receipt at once. Exit is available in the ordinary sense that any person can choose not to study, without external sanction, though within the framework the practice is understood as spiritually rewarding rather than obligatory-under-coercion.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__study_as_performance, scholar_worshippers, beneficiary,
    moderate, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(sacrifice_commandment__study_as_performance, scholar_worshippers, agenda_setter).

% Transmits and authorizes the doctrine (rooted in Talmud Menachot 110a and Taanit 27b) that recitation and study of sacrificial law stand in place of the offering itself. This body sets which texts count as canonical study, how the doctrine is taught, and how it interacts with law codes (Shulchan Aruch's inclusion of korbanot study in daily liturgy). It does not collect resources from adherents; its authority is exercised through teaching and textual transmission, not compulsion.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__study_as_performance, rabbinic_interpretive_tradition, agenda_setter,
    institutional, civilizational, mobile, global).

% Communities living without Temple access for two millennia, for whom this reading supplies a way to remain in full covenantal standing regarding sacrificial commandments despite total physical impossibility of performing them. Their exit options regarding the underlying condition (no Temple) are essentially nil, but the reading itself imposes no cost on them — it removes a burden (the sense of standing obligation unfulfillable) rather than adding one.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__study_as_performance, diaspora_and_post_destruction_communities, beneficiary,
    powerless, civilizational, constrained, global).

% Hold that the commandment requires physical execution and is suspended, not fulfilled, absent the Temple. They are not part of this constraint's operation — this story is about the study-as-performance reading only — but they would object that intellectual engagement is a consolation practice rather than actual discharge of the mitzvah, and that this reading risks complacency about restoration.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__study_as_performance, advocates_of_performance_only_reading, excluded,
    moderate, civilizational, mobile, global).

% Hold that study preserves technical knowledge for a future rebuilt Temple and is preparatory rather than present worship. They are not part of this constraint's operation. They would characterize the study-as-performance reading as theologically satisfying but practically risking loss of urgency toward actual restoration, since the felt need is dissolved by the substitution.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__study_as_performance, advocates_of_archive_maintenance_reading, excluded,
    moderate, civilizational, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a community's relationship to an unperformable commandment by locating full discharge of the obligation inside intellectual/verbal engagement with its legal text, so that religious practice can continue coherently in the absence of the Temple.
% TRANSFER_FUNCTION: Nothing is transferred between parties; the practice is not extractive. What moves is internal — a person's engagement with a text is reclassified from 'substitute activity' to 'the commandment itself,' altering the psychological and legal status of that engagement, not moving resources between agents.
% ABSENT_VOICES: The performance_only and archive_maintenance readings are structurally absent from this constraint (they are separate constraints in this family); within THIS reading's own community, dissenting voices who worry the doctrine dulls messianic urgency are a minority position tolerated within normative discourse, not suppressed.
% DISAPPEARANCE_RATIONALE: If this reading vanished, scholar-worshippers who hold it would lose the doctrinal basis for treating their study as commandment-fulfillment rather than mere preparation or scholarship, which the study_as_performance camp says would leave them believing themselves in enduring, unfulfillable default. Advocates of the sibling readings would say nothing of substance changes, since their frameworks already treat study as either suspended-obligation or preparatory rather than as fulfillment — hence 'contested' rather than a single verdict.
% FOUNDING_PROBLEM: After the Temple's destruction in 70 CE, the sacrificial commandments became physically impossible to perform, creating a theological crisis: was the covenant now permanently in breach regarding an entire category of commandments, with no path to good standing?
% FOUNDING_PROBLEM_CORROBORATION: The doctrine is attested inside its own tradition (Talmud Menachot 110a, Taanit 27b, later codified by Rabbi Yosef Karo's inclusion of korbanot recitation in daily prayer) — this is lineage-internal attestation. Outside corroboration is harder to locate precisely because the question is theological rather than empirical: comparative religion scholars (e.g., studies of rabbinic substitution theology post-70 CE) describe the doctrine as a documented historical adaptation strategy, which corroborates that the founding problem was real and historically located, without adjudicating whether the study-as-performance solution is theologically correct. No fully external (non-adherent, non-scholarly) corroboration exists or would be expected to for a claim of this kind.
narrative_ontology:disappearance_verdict(sacrifice_commandment__study_as_performance, contested).
narrative_ontology:founding_problem_status(sacrifice_commandment__study_as_performance, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sacrifice_commandment__study_as_performance, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
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
 *   Extractiveness is authored near zero (0.03) because under this reading nothing is extracted from anyone — no party pays a cost so that another may benefit; the practice is closed-loop (the scholar-worshipper is both the one who engages and the one who receives the religious benefit). Suppression is low (0.05) because no coercive apparatus enforces the doctrine; it persists through voluntary transmission and textual authority, not compulsion. Theater ratio is low but nonzero (0.08, drifting slightly upward over the interval) reflecting that some communal recitation of korbanot passages in liturgy has, for some practitioners, become rote performance of text rather than the substantive intellectual engagement the doctrine actually requires — a mild, honestly-authored drift, not a structural indictment. Accessibility collapse is modest (0.15): alternative theological framings (the sibling readings) remain fully available and openly taught within the same tradition, so this reading has not foreclosed live alternatives for adherents. Resistance is low (0.1): the doctrine is broadly accepted within normative rabbinic Judaism, with disagreement occurring mainly as scholarly/theological debate rather than active resistance.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (scholar_worshippers, diaspora_and_post_destruction_communities) sit near the pure-beneficiary end of directionality because the constraint subsidizes them psychologically and religiously — it resolves an otherwise unresolvable obligation gap at zero cost. There is no victim group in this reading: the expected structural delta (zero extractiveness, no victim set, beneficiary is the scholar-worshipper) is honored exactly. The rabbinic_interpretive_tradition is an agenda-setter but not an extractor — it authorizes and transmits without collecting rents, tuition, or compliance costs, which is why it is not also marked payer.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading is a case where mandatrophy analysis should NOT declare the mandate obsolete: the founding problem (unperformable commandment post-Temple) remains structurally live for the community (no Temple has been rebuilt), and the study_as_performance doctrine is not a degraded vestige of a once-functional practice — it was, from its founding, a solution designed for exactly the standing condition it still addresses. Because it produces no rents, requires no enforcement, and meets little resistance, it does not exhibit rope-to-piton or rope-to-snare drift; the mild theater_ratio increase reflects liturgical routinization at the margins, not capture.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    study_as_genuine_discharge_or_consolation,
    'Does the study_as_performance doctrine genuinely and fully discharge the sacrificial commandment, or does it function as a psychologically necessary consolation that the tradition has upgraded to formal doctrine because the alternative (permanent unfulfillable obligation) was theologically intolerable?',
    'This is not resolvable by external empirical means; it is resolvable only within the framework''s own hermeneutic authority (further textual analysis of Menachot 110a and its halakhic reception) or left as a standing theological question across generations of poskim (halakhic decisors).',
    'If the doctrine is best understood as consolation retrofitted into formal discharge, the constraint''s coordination function (resolving covenantal anxiety) remains intact but its claim to literal equivalence with sacrifice would be weakened, potentially shifting classification toward scaffold (transitional coping measure) rather than rope (stable, non-extractive coordination).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(study_as_genuine_discharge_or_consolation, conceptual, 'Whether the reading is genuine doctrinal discharge or theologically necessary consolation.').

omega_variable(
    kernel_disagreement_locus,
    'Where exactly do the three sibling readings of the sacrifice_commandment kernel disagree — is it about the ONTOLOGY of the commandment (what it fundamentally requires: physical act vs. intellectual engagement vs. preserved knowledge), or about the EPISTEMOLOGY of fulfillment (how one would know the obligation is satisfied)?',
    'Close comparative reading of the Talmudic sources each reading cites as its foundation, and of later halakhic codifiers'' explicit reasoning for adopting one framing over the others (e.g., why Shulchan Aruch includes korbanot recitation in daily liturgy rather than treating it as optional study).',
    'If the disagreement is ontological, the study_as_performance reading FORECLOSES performance_only within a single coherent framework (a commandment cannot simultaneously require only physical execution and be discharged by study). If it is merely epistemological (different fulfillment-criteria for the same underlying obligation), the readings can coexist as parallel, non-foreclosing positions held by different communities — which is the relation actually declared below (coexists_with), reflecting that in practice all three readings are live within contemporary Orthodox Judaism without one having formally displaced the others.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_disagreement_locus, conceptual, 'Whether sibling kernel readings disagree ontologically (mutually exclusive) or epistemologically (coexisting).').

omega_variable(
    future_temple_restoration_effect,
    'Would the study_as_performance doctrine''s status change if the Temple were rebuilt and physical sacrifice became possible again — would study cease to constitute fulfillment, or would the doctrine''s adherents maintain that study remains an equally valid (or even superior) form of the commandment?',
    'No empirical resolution is possible absent the counterfactual event; historically, some strands of rabbinic thought hold study is intrinsically valuable regardless of Temple status (per Torah study''s independent commandment status), suggesting the doctrine might persist as a parallel-but-not-required practice, while others treat it strictly as a stopgap.',
    'If restoration would eliminate the study-as-performance framing, the doctrine is closer to scaffold (transitional coordination pending the ''real'' solution); if it would persist as co-equal to physical sacrifice, it is closer to genuine rope (self-standing worship form, not a stopgap).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(future_temple_restoration_effect, conceptual, 'Whether the doctrine is a permanent worship form or a transitional scaffold pending Temple restoration.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sacrifice_commandment__study_as_performance, 0, 1900).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sacr_tr_t0, sacrifice_commandment__study_as_performance, theater_ratio, 0, 0.05).
narrative_ontology:measurement(sacr_tr_t300, sacrifice_commandment__study_as_performance, theater_ratio, 300, 0.06).
narrative_ontology:measurement(sacr_tr_t700, sacrifice_commandment__study_as_performance, theater_ratio, 700, 0.07).
narrative_ontology:measurement(sacr_tr_t1100, sacrifice_commandment__study_as_performance, theater_ratio, 1100, 0.08).
narrative_ontology:measurement(sacr_tr_t1500, sacrifice_commandment__study_as_performance, theater_ratio, 1500, 0.08).
narrative_ontology:measurement(sacr_tr_t1900, sacrifice_commandment__study_as_performance, theater_ratio, 1900, 0.08).

% Extraction over time
narrative_ontology:measurement(sacr_be_t0, sacrifice_commandment__study_as_performance, base_extractiveness, 0, 0.02).
narrative_ontology:measurement(sacr_be_t300, sacrifice_commandment__study_as_performance, base_extractiveness, 300, 0.02).
narrative_ontology:measurement(sacr_be_t700, sacrifice_commandment__study_as_performance, base_extractiveness, 700, 0.03).
narrative_ontology:measurement(sacr_be_t1100, sacrifice_commandment__study_as_performance, base_extractiveness, 1100, 0.03).
narrative_ontology:measurement(sacr_be_t1500, sacrifice_commandment__study_as_performance, base_extractiveness, 1500, 0.03).
narrative_ontology:measurement(sacr_be_t1900, sacrifice_commandment__study_as_performance, base_extractiveness, 1900, 0.03).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(sacrifice_commandment__study_as_performance, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sacrifice_commandment__study_as_performance, identity_coordination).
narrative_ontology:boltzmann_floor_override(sacrifice_commandment__study_as_performance, 0.08).
narrative_ontology:affects_constraint(sacrifice_commandment__study_as_performance, sacrifice_commandment__performance_only).
narrative_ontology:affects_constraint(sacrifice_commandment__study_as_performance, sacrifice_commandment__archive_maintenance).

% DUAL FORMULATION NOTE:
% This story is one of three constraints decomposing the natural-language concept 'the sacrifice commandment after the Temple's destruction.' Each sibling reading (performance_only, archive_maintenance, study_as_performance) has a structurally distinct ε, beneficiary/victim set, and classification, per the ε-invariance principle — they are not the same constraint measured three ways. This reading (study_as_performance) has the lowest extractiveness of the three by construction, since it authors present, complete, cost-free discharge; performance_only likely authors a suspended-obligation structure with different dynamics, and archive_maintenance likely authors a scaffold-flavored preparatory structure oriented toward a future contingency. All three should link to each other via affects_constraints to preserve the family structure for contamination/coupling analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
