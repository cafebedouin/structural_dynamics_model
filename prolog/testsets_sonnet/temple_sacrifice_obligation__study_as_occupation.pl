% ============================================================================
% CONSTRAINT STORY: temple_sacrifice_obligation__study_as_occupation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_temple_sacrifice_obligation__study_as_occupation, []).

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
 *   constraint_id: temple_sacrifice_obligation__study_as_occupation
 *   human_readable: Talmudic Study of Korbanot as Fulfillment of the Sacrificial Obligation
 *   domain: religious/halakhic
 *
 * SUMMARY:
 *   In the absence of the Temple, the biblical commandments concerning
 *   sacrificial offerings (korbanot) cannot be physically performed, yet
 *   rabbinic Judaism holds them as still binding. Multiple readings of how
 *   the obligation is handled under this impossibility coexist: this story
 *   instantiates the 'study-as-occupation' reading, under which sustained
 *   study of sacrificial law itself discharges the obligation — not merely
 *   preserves it (study_as_archiving) or leaves it suspended
 *   (messianic_suspension). This reading is grounded chiefly in the amoraic
 *   dictum (Menachot 110a) treating Torah study of the sacrificial order as
 *   equivalent to the offering itself, and is the reading that has become
 *   dominant in mainstream rabbinic curricula (notably in the emphasis on
 *   Seder Kodashim in yeshiva learning). The three readings are NOT the same
 *   constraint measured differently — they have different beneficiary
 *   structures, different victim exposure, and different persistence
 *   mechanisms, and are authored as three separate linked stories per the
 *   ε-invariance principle.
 *
 * KEY AGENTS:
 *   - rabbinic_scholarly_class: institutional authority that formulated and transmits the doctrine, deeply identity-bound to it
 *   - yeshiva_institutions: organized beneficiaries whose curricula and prestige structures depend on the doctrine's operative status
 *   - observant_laity_seeking_continuity: moderate-power beneficiaries who receive psychological/religious resolution
 *   - messianic_restorationist_factions: excluded voice holding the suspension reading, structurally marginalized by curricular dominance
 *   - archiving_reading_proponents: excluded voice holding an intermediate reading
 *   - comparative_religion_scholars: analytical observers
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(temple_sacrifice_obligation__study_as_occupation, 0.08).
domain_priors:suppression_score(temple_sacrifice_obligation__study_as_occupation, 0.12).
domain_priors:theater_ratio(temple_sacrifice_obligation__study_as_occupation, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(temple_sacrifice_obligation__study_as_occupation, extractiveness, 0.08).
narrative_ontology:constraint_metric(temple_sacrifice_obligation__study_as_occupation, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(temple_sacrifice_obligation__study_as_occupation, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(temple_sacrifice_obligation__study_as_occupation, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(temple_sacrifice_obligation__study_as_occupation, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(temple_sacrifice_obligation__study_as_occupation, rope).
narrative_ontology:human_readable(temple_sacrifice_obligation__study_as_occupation, "Talmudic Study of Korbanot as Fulfillment of the Sacrificial Obligation").
narrative_ontology:topic_domain(temple_sacrifice_obligation__study_as_occupation, "religious/halakhic").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(temple_sacrifice_obligation__study_as_occupation, '0136d177-f4ec-41fd-b7a7-0a00bf9624c4').
narrative_ontology:cs_kernel_codification('0136d177-f4ec-41fd-b7a7-0a00bf9624c4', fixed_text).
narrative_ontology:cs_authority_grounding('0136d177-f4ec-41fd-b7a7-0a00bf9624c4', lineage).
narrative_ontology:cs_interpretation_layer_present('0136d177-f4ec-41fd-b7a7-0a00bf9624c4').
narrative_ontology:cs_reading_relation('0136d177-f4ec-41fd-b7a7-0a00bf9624c4', temple_sacrifice_obligation__study_as_archiving, influences).
narrative_ontology:cs_reading_relation('0136d177-f4ec-41fd-b7a7-0a00bf9624c4', temple_sacrifice_obligation__messianic_suspension, coexists_with).
narrative_ontology:cs_axiom('0136d177-f4ec-41fd-b7a7-0a00bf9624c4', foundational, study_of_law_constitutes_performance_equivalence).
narrative_ontology:cs_axiom_status(study_of_law_constitutes_performance_equivalence, holdable).
narrative_ontology:cs_axiom_grounding('0136d177-f4ec-41fd-b7a7-0a00bf9624c4', study_of_law_constitutes_performance_equivalence, conventional).
narrative_ontology:cs_axiom('0136d177-f4ec-41fd-b7a7-0a00bf9624c4', secondary, present_era_is_adequate_locus_of_obligation_fulfillment).
narrative_ontology:cs_axiom_status(present_era_is_adequate_locus_of_obligation_fulfillment, holdable).
narrative_ontology:cs_axiom_grounding('0136d177-f4ec-41fd-b7a7-0a00bf9624c4', present_era_is_adequate_locus_of_obligation_fulfillment, conventional).
narrative_ontology:cs_reference_frame('0136d177-f4ec-41fd-b7a7-0a00bf9624c4', temple_era_sacrificial_performance).
narrative_ontology:cs_drift_state('0136d177-f4ec-41fd-b7a7-0a00bf9624c4', post_second_temple_destruction_rabbinic_era, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('0136d177-f4ec-41fd-b7a7-0a00bf9624c4', '').
narrative_ontology:cs_kernel_id(temple_sacrifice_obligation__study_as_occupation, temple_sacrifice_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(temple_sacrifice_obligation__study_as_occupation, rabbinic_scholarly_class).
narrative_ontology:constraint_beneficiary(temple_sacrifice_obligation__study_as_occupation, yeshiva_institutions).
narrative_ontology:constraint_beneficiary(temple_sacrifice_obligation__study_as_occupation, observant_laity_seeking_continuity).
narrative_ontology:constraint_vindicates(temple_sacrifice_obligation__study_as_occupation, torah_study_equivalent_to_practice_doctrine).
narrative_ontology:constraint_vindicates(temple_sacrifice_obligation__study_as_occupation, oral_law_generative_authority_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Develops and transmits the doctrine (rooted in readings of Menachot 110a and the daily liturgical recitation of sacrificial passages) that sustained study of the laws of korbanot constitutes 'as if' the sacrifice were offered. This class administers yeshiva curricula built substantially around sacrificial tractates (Kodashim), trains successive generations in this framework, and derives its institutional centrality and continuity from the claim that study occupies the obligation. Its professional identity and life's work are constituted through this reading; abandoning it would not merely change a legal opinion but dissolve the rationale for a large share of its vocational activity.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__study_as_occupation, rabbinic_scholarly_class, agenda_setter,
    institutional, civilizational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(temple_sacrifice_obligation__study_as_occupation, rabbinic_scholarly_class, beneficiary).

% Structure academic calendars, ordination tracks, and prestige hierarchies around mastery of Seder Kodashim, a body of law with no live practical application outside the study-as-occupation frame. Institutional funding, curricular legitimacy, and scholarly prestige flow from the premise that this study is not merely historical or preparatory but obligation-fulfilling in itself.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__study_as_occupation, yeshiva_institutions, beneficiary,
    organized, generational, constrained, global).

% Recite sacrificial liturgy daily and engage with study of Kodashim (individually or communally) as a way of remaining in active relationship with a commandment they cannot physically perform. For this group the doctrine resolves a genuine existential problem: how to be in compliance with a still-binding command when the object of the command (the Temple) does not exist. They receive psychological and religious continuity, not material extraction.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__study_as_occupation, observant_laity_seeking_continuity, beneficiary,
    moderate, biographical, constrained, global).

% Hold that the obligation is suspended rather than satisfied by study, and that treating study as occupation risks dulling the urgency of restoration and normalizing the Temple's absence indefinitely. Their objection is doctrinal, not economic: they are not harmed materially, but their theological position is structurally marginalized by an authority apparatus (mainstream rabbinic curricula, halakhic codes) that has largely settled on the study-as-occupation reading as operative practice.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__study_as_occupation, messianic_restorationist_factions, excluded,
    moderate, civilizational, identity_locked, regional).

% Hold that study preserves knowledge for eventual restoration but does not itself discharge the obligation — a position with real halakhic pedigree that nonetheless occupies less institutional airtime than the occupation reading, since the occupation reading better justifies present-tense curricular investment and communal practice as sufficient rather than merely custodial.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__study_as_occupation, archiving_reading_proponents, excluded,
    moderate, generational, constrained, regional).

% Analyze how post-Temple Judaism reorganized obligation around text and study rather than performance, comparing this to analogous substitution mechanisms in other traditions whose central ritual sites were lost or destroyed.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__study_as_occupation, comparative_religion_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a coherent, actionable way for an entire religious community to remain in good standing with a still-binding but currently unperformable commandment, preventing two failure modes: despair/abandonment of the law, or perpetual unresolved guilt over structural non-compliance.
% TRANSFER_FUNCTION: Moves communal attention, curricular time, and institutional prestige toward the rabbinic scholarly class and the study institutions that administer Kodashim; moves psychological resolution and continuity to observant laity. No material transfer from an identifiable victim class — the 'cost' is opportunity cost of alternative doctrinal emphasis (e.g., restorationist urgency, archiving humility) rather than extraction from a payer.
% ABSENT_VOICES: Messianic restorationists and archiving-reading proponents hold coherent halakhic positions but operate with less institutional reach than the mainstream occupation reading; they are not suppressed by force, but the reading that best justifies current institutional structure predictably receives more curricular and communal reinforcement.
% DISAPPEARANCE_RATIONALE: If the study-as-occupation doctrine were repudiated, the entire post-Temple curricular emphasis on Kodashim would lose its obligation-discharging rationale, yeshiva structures would need to reframe such study as purely custodial or historical, and observant communities would face a renewed unresolved-obligation problem the doctrine currently absorbs.
% FOUNDING_PROBLEM: The destruction of the Second Temple (70 CE) rendered the biblically mandated sacrificial system permanently unperformable, creating an acute crisis: a body of commandments the community was obligated to keep but structurally could not perform, with no fixed date for restoration.
% FOUNDING_PROBLEM_CORROBORATION: Historians of Second Temple Judaism and rabbinic literature (outside the rabbinic institutions that benefit from the doctrine) corroborate that the Temple's destruction created exactly this crisis and that the study-as-occupation move is a documented, datable rabbinic innovation (traceable substantially to amoraic-era exegesis) rather than a Sinaitic given; the founding problem — the Temple remains absent — is independently verifiable and uncontested even by those who reject the occupation reading.
narrative_ontology:disappearance_verdict(temple_sacrifice_obligation__study_as_occupation, world_rearranges).
narrative_ontology:founding_problem_status(temple_sacrifice_obligation__study_as_occupation, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(temple_sacrifice_obligation__study_as_occupation, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(temple_sacrifice_obligation__study_as_occupation, 'none', 1).
narrative_ontology:epsilon_provenance(temple_sacrifice_obligation__study_as_occupation, 0.08, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(temple_sacrifice_obligation__study_as_occupation_tests).
:- end_tests(temple_sacrifice_obligation__study_as_occupation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low (0.08) and essentially flat across nearly two millennia because this reading does not identify a payer class from whom something is extracted — the doctrine resolves an obligation problem rather than transferring value from victims to beneficiaries. Suppression is low (0.12): sibling readings are not coercively suppressed, they simply receive less curricular reinforcement given the institutional convenience of the occupation reading. Theater ratio rises modestly (0.15 to 0.28) reflecting a slow accretion of performative/ceremonial study practices (e.g., communal siyyum cycles on Kodashim, liturgical formalization) layered atop the substantive study function over centuries, but this remains well below the threshold that would indicate the coordination function has hollowed out. Accessibility collapse is moderate (0.35), not mountain-high: alternative readings (archiving, suspension) remain live and articulable within the tradition, they are simply less institutionally central. Resistance is low (0.15): what resistance exists is doctrinal disagreement among scholars, not active resistance from a coerced population.
 *
 * PERSPECTIVAL GAP:
 *   From the rabbinic scholarly class's seat, the doctrine is settled, well-grounded halakhic reasoning continuous with the tradition's own interpretive methods. From a messianic-restorationist seat, the same doctrine looks like a convenient institutional accommodation that quietly forecloses the urgency of restoration by making the present arrangement feel sufficient. The engine computes these as structurally different seats reading the same constraint; this story does not adjudicate between them, only supplies the structural data for the occupation reading specifically.
 *
 * DIRECTIONALITY LOGIC:
 *   The rabbinic scholarly class and yeshiva institutions sit near the beneficiary end: they administer the doctrine, derive institutional and professional continuity from it, and are identity-locked into it as a matter of vocational structure. Observant laity are moderate beneficiaries — they receive existential/religious resolution without bearing an extractive cost; their d is closer to symmetric-to-beneficiary since compliance is undertaken willingly as relief from an otherwise unresolved obligation, not as coerced payment. No group is authored as a victim because no group bears a cost imposed by this specific reading's operation. The excluded factions are marginalized in curricular airtime, not materially extracted from, which is why they are placed as excluded rather than payer.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (Temple destruction, unperformable sacrificial obligation) remains genuinely live — the Temple has not been rebuilt — so this is not a case of an arrangement persisting after its problem vanished. What is distinct is that the SOLUTION itself (study substituting for performance) is a rabbinic innovation dated centuries after the founding crisis, not an original feature of the commandment. The classification as rope-like coordination rather than tangled_rope or snare rests on the absence of an identifiable victim class paying into the arrangement — the closest thing to a 'cost' is the relative institutional deprioritization of sibling readings, which is a legitimacy/attention allocation effect, not an extraction of value from a coerced payer.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    study_as_occupation_versus_archiving_boundary,
    'Does the Menachot 110a dictum (''whoever engages in Torah study of the sacrificial order is as if they offered it'') establish full obligation-discharge, or is it aggadic/hortatory language later read by institutional interests as establishing a formal halakhic equivalence stronger than originally intended?',
    'Close textual-historical analysis of how the dictum was cited and applied across geonic, rishonic, and acharonic halakhic literature — tracking whether early citations treat it as formal discharge or as consolation/motivation for study.',
    'If the dictum was originally hortatory and only later hardened into formal discharge doctrine by institutions with a stake in validating Kodashim study as sufficient, this reading would look more like a constructed legitimation than a straightforward halakhic conclusion — pushing toward reclassification nearer tangled_rope if an extraction structure could be identified, though no clear victim class has been.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(study_as_occupation_versus_archiving_boundary, conceptual, 'Whether the doctrine''s textual basis supports full discharge or was extended beyond its original hortatory scope.').

omega_variable(
    curricular_dominance_versus_doctrinal_pluralism,
    'Is the study-as-occupation reading''s dominance in yeshiva curricula a fair reflection of its halakhic strength, or does it persist partly because it is the reading most convenient for an already-existing institutional structure (yeshivot organized around textual study) to validate?',
    'Comparative analysis of how much curricular time and communal reinforcement sibling readings (archiving, suspension) receive in institutions not structurally invested in study-centrality, e.g. communities with stronger messianic-activist orientations.',
    'If curricular dominance tracks institutional convenience rather than halakhic merit, the reading''s persistence is partly self-reinforcing (the institutions that teach it also validate it) rather than purely a function of textual authority — relevant to whether the interpretation_layer here is neutral or self-interested.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(curricular_dominance_versus_doctrinal_pluralism, conceptual, 'Whether the reading''s institutional dominance reflects doctrinal merit or self-reinforcing institutional convenience.').

omega_variable(
    natural_versus_constructed_solution_ambiguity,
    'Is study-as-occupation better understood as a naturally emergent, near-inevitable communal response to an unperformable but binding obligation (any tradition facing this crisis would converge on some form of textual substitution), or as one specific, contestable rabbinic policy choice among genuinely available alternatives?',
    'Comparative religious-history analysis of other traditions that lost access to central ritual sites, examining whether textual/study substitution is a structurally near-universal adaptation or a distinctively rabbinic choice among multiple live options.',
    'If near-universal, the reading''s low extractiveness and rope-like character are further supported as a genuine coordination solution to a real crisis. If a distinctively chosen policy among genuine alternatives, the beneficiary declarations here (rabbinic_scholarly_class benefiting from the choice they administer) carry more analytical weight and the reading sits closer to a legitimation the institution supplies for itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_versus_constructed_solution_ambiguity, empirical, 'Whether textual substitution for lost ritual practice is a near-universal adaptation or a specific contestable choice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(temple_sacrifice_obligation__study_as_occupation, 0, 1900).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(temp_tr_t0, temple_sacrifice_obligation__study_as_occupation, theater_ratio, 0, 0.15).
narrative_ontology:measurement(temp_tr_t300, temple_sacrifice_obligation__study_as_occupation, theater_ratio, 300, 0.18).
narrative_ontology:measurement(temp_tr_t700, temple_sacrifice_obligation__study_as_occupation, theater_ratio, 700, 0.22).
narrative_ontology:measurement(temp_tr_t1100, temple_sacrifice_obligation__study_as_occupation, theater_ratio, 1100, 0.24).
narrative_ontology:measurement(temp_tr_t1500, temple_sacrifice_obligation__study_as_occupation, theater_ratio, 1500, 0.26).
narrative_ontology:measurement(temp_tr_t1900, temple_sacrifice_obligation__study_as_occupation, theater_ratio, 1900, 0.28).

% Extraction over time
narrative_ontology:measurement(temp_be_t0, temple_sacrifice_obligation__study_as_occupation, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(temp_be_t300, temple_sacrifice_obligation__study_as_occupation, base_extractiveness, 300, 0.09).
narrative_ontology:measurement(temp_be_t700, temple_sacrifice_obligation__study_as_occupation, base_extractiveness, 700, 0.08).
narrative_ontology:measurement(temp_be_t1100, temple_sacrifice_obligation__study_as_occupation, base_extractiveness, 1100, 0.08).
narrative_ontology:measurement(temp_be_t1500, temple_sacrifice_obligation__study_as_occupation, base_extractiveness, 1500, 0.08).
narrative_ontology:measurement(temp_be_t1900, temple_sacrifice_obligation__study_as_occupation, base_extractiveness, 1900, 0.08).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(temple_sacrifice_obligation__study_as_occupation, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(temple_sacrifice_obligation__study_as_occupation, identity_coordination).
narrative_ontology:boltzmann_floor_override(temple_sacrifice_obligation__study_as_occupation, 0.08).
narrative_ontology:affects_constraint(temple_sacrifice_obligation__study_as_occupation, temple_sacrifice_obligation__study_as_archiving).
narrative_ontology:affects_constraint(temple_sacrifice_obligation__study_as_occupation, temple_sacrifice_obligation__messianic_suspension).

% DUAL FORMULATION NOTE:
% This constraint is one of three linked readings of the temple_sacrifice_obligation kernel: study_as_occupation (this story — study fully discharges the obligation, low extraction, institutionally dominant), study_as_archiving (study preserves but does not discharge — an intermediate, custodial reading with its own distinct stakeholder emphasis), and messianic_suspension (the obligation is neither fulfilled nor violated, but held in abeyance — a reading emphasized by restorationist factions who are excluded stakeholders in this story). Each reading has its own ε and its own type; they are not the same constraint viewed three ways. The occupation reading structurally influences the archiving reading (a stronger claim crowds out the weaker one for curricular attention) and stands in tension with the suspension reading (they cannot both be operative descriptions of the same present moment within a single community's practice, though both persist as live positions across different communities).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
