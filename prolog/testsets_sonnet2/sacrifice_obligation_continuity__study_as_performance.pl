% ============================================================================
% CONSTRAINT STORY: sacrifice_obligation_continuity__study_as_performance
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sacrifice_obligation_continuity__study_as_performance, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: sacrifice_obligation_continuity__study_as_performance
 *   human_readable: Study of Sacrificial Law as Fulfillment of the Commandment
 *   domain: religious_law/ritual_studies/textual_tradition
 *
 * SUMMARY:
 *   This story instantiates one reading of a contested kernel concerning the
 *   status of Temple-era sacrificial law after the Temple's destruction.
 *   Under this reading (study_as_performance), sustained textual engagement
 *   with the laws of sacrifice is not preparation for a future restored
 *   performance and not a suspended obligation awaiting messianic resolution
 *   — it is itself the fulfillment of the commandment, discharging the
 *   underlying duty through the act of study. This gives an entire body of
 *   law a live, accessible, non-extractive mode of observance. The sibling
 *   readings (performance_only, messianic_suspension, archival_preservation)
 *   are separate constraints with their own ε and their own
 *   beneficiary/victim structures — they are not represented inside this
 *   story's accounting, per the ε-invariance principle.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sacrifice_obligation_continuity__study_as_performance, 0.08).
domain_priors:suppression_score(sacrifice_obligation_continuity__study_as_performance, 0.12).
domain_priors:theater_ratio(sacrifice_obligation_continuity__study_as_performance, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__study_as_performance, extractiveness, 0.08).
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__study_as_performance, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__study_as_performance, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__study_as_performance, accessibility_collapse, 0.2).
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__study_as_performance, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sacrifice_obligation_continuity__study_as_performance, rope).
narrative_ontology:human_readable(sacrifice_obligation_continuity__study_as_performance, "Study of Sacrificial Law as Fulfillment of the Commandment").
narrative_ontology:topic_domain(sacrifice_obligation_continuity__study_as_performance, "religious_law/ritual_studies/textual_tradition").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sacrifice_obligation_continuity__study_as_performance, 'dd8e683e-6077-4a33-9cf0-bbd9f42f76b4').
narrative_ontology:cs_kernel_codification('dd8e683e-6077-4a33-9cf0-bbd9f42f76b4', fixed_text).
narrative_ontology:cs_authority_grounding('dd8e683e-6077-4a33-9cf0-bbd9f42f76b4', lineage).
narrative_ontology:cs_interpretation_layer_present('dd8e683e-6077-4a33-9cf0-bbd9f42f76b4').
narrative_ontology:cs_reading_relation('dd8e683e-6077-4a33-9cf0-bbd9f42f76b4', sacrifice_obligation_continuity__performance_only, coexists_with).
narrative_ontology:cs_reading_relation('dd8e683e-6077-4a33-9cf0-bbd9f42f76b4', sacrifice_obligation_continuity__messianic_suspension, coexists_with).
narrative_ontology:cs_reading_relation('dd8e683e-6077-4a33-9cf0-bbd9f42f76b4', sacrifice_obligation_continuity__archival_preservation, forecloses).
narrative_ontology:cs_axiom('dd8e683e-6077-4a33-9cf0-bbd9f42f76b4', foundational, study_constitutes_ritual_performance).
narrative_ontology:cs_axiom_status(study_constitutes_ritual_performance, holdable).
narrative_ontology:cs_axiom_grounding('dd8e683e-6077-4a33-9cf0-bbd9f42f76b4', study_constitutes_ritual_performance, theological).
narrative_ontology:cs_axiom('dd8e683e-6077-4a33-9cf0-bbd9f42f76b4', foundational, sacrifice_obligation_remains_normatively_binding).
narrative_ontology:cs_axiom_status(sacrifice_obligation_remains_normatively_binding, holdable).
narrative_ontology:cs_axiom_grounding('dd8e683e-6077-4a33-9cf0-bbd9f42f76b4', sacrifice_obligation_remains_normatively_binding, theological).
narrative_ontology:cs_reference_frame('dd8e683e-6077-4a33-9cf0-bbd9f42f76b4', talmudic_study_as_avodah).
narrative_ontology:cs_drift_state('dd8e683e-6077-4a33-9cf0-bbd9f42f76b4', contemporary_post_temple_diaspora, gap(stable, minor, true)).
narrative_ontology:cs_created_at('dd8e683e-6077-4a33-9cf0-bbd9f42f76b4', '').
narrative_ontology:cs_kernel_id(sacrifice_obligation_continuity__study_as_performance, sacrifice_obligation_continuity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sacrifice_obligation_continuity__study_as_performance, torah_scholars).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_continuity__study_as_performance, yeshiva_students).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_continuity__study_as_performance, diaspora_communities).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_continuity__study_as_performance, rabbinic_interpretive_tradition).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Engage in structured study of the sacrificial statutes (korbanot) as their primary mode of religious practice in the absence of the Temple. They hold that this study discharges the underlying commandment rather than merely preparing for its future performance. Their standing within the tradition, their teaching authority, and their sense of religious completeness all rest on this reading holding.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__study_as_performance, torah_scholars, beneficiary,
    moderate, civilizational, mobile, global).

% Study tractates on sacrificial law (e.g. Zevachim, Menachot) as part of ordinary curriculum. Under this reading, the hours spent in the study hall are themselves the fulfillment of a commandment they could otherwise never perform, since no Temple stands. They can leave the study hall at will; nothing coerces their attendance beyond ordinary communal and pedagogical norms.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__study_as_performance, yeshiva_students, beneficiary,
    powerless, biographical, mobile, national).

% Live at geographic and historical distance from any functioning Temple. This reading gives their communal life a route to full religious observance of an entire legal domain that would otherwise sit permanently unfulfillable. They participate through communal Torah-reading cycles, study groups, and liturgy referencing the sacrificial order; no one is compelled to adopt this framework, and other readings of the same texts remain available to them within the tradition.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__study_as_performance, diaspora_communities, beneficiary,
    organized, generational, mobile, global).

% Authorities and commentators (drawing on Talmudic and later sources) articulate and transmit the doctrine that engaged study of sacrificial law substitutes for its physical performance. They administer which texts count, how study should be structured, and how the doctrine is taught, but they do not personally collect any material benefit from its adoption — their stake is in the coherence and continuity of the interpretive tradition itself.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__study_as_performance, rabbinic_interpretive_tradition, agenda_setter,
    institutional, civilizational, analytical, global).

% Communities and authorities who hold the performance_only, messianic_suspension, or archival_preservation readings of the same kernel are not part of this constraint's internal accounting — they would dispute that study alone discharges the obligation, or would deny the obligation is presently live at all, or would deny it retains normative force. Their objections belong to their own constraint stories, not this one, but they are the natural source of contest against this reading.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__study_as_performance, sibling_reading_adherents, excluded,
    organized, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a coherent, universally accessible mode of religious observance for an entire body of law (sacrificial statutes) that cannot otherwise be performed absent a functioning Temple, allowing communal and individual religious life to remain complete rather than permanently deficient in one domain.
% TRANSFER_FUNCTION: No material transfer occurs; the arrangement moves normative status — from 'unfulfillable obligation' to 'obligation discharged through study' — without moving money, labor, or goods between parties.
% ABSENT_VOICES: Adherents of the performance_only and messianic_suspension readings would object that study is preparatory or readiness-maintaining rather than substitutive fulfillment; they are not silenced within the broader tradition but simply hold a different constraint (a different reading of the same kernel), and are not part of this story's internal parties.
% DISAPPEARANCE_RATIONALE: If the study-as-performance doctrine vanished, the entire body of sacrificial law would revert to being either suspended, purely preparatory, or archival for communities that currently treat structured study of it as active religious fulfillment — significant curricular time, liturgical framing, and a sense of religious completeness for diaspora life would need to be reorganized around one of the sibling readings instead.
% FOUNDING_PROBLEM: After the Temple's destruction, the sacrificial commandments (a major portion of Torah law) became physically unperformable, threatening to leave a permanent, uncloseable gap in observance and raising the question of whether the tradition could remain coherent without a mechanism to fulfill or account for laws no one could carry out.
% FOUNDING_PROBLEM_CORROBORATION: Historians of rabbinic Judaism (writing from outside the community of practitioners who benefit from the doctrine) corroborate that the Temple's absence created exactly this gap and that the study-as-fulfillment doctrine emerged as a documented rabbinic response (traceable to Talmudic sources, e.g. Menachot 110a) rather than as a later retrospective justification; the problem — how to relate to an unperformable body of commandments — remains structurally live for any tradition without a Temple.
narrative_ontology:disappearance_verdict(sacrifice_obligation_continuity__study_as_performance, world_rearranges).
narrative_ontology:founding_problem_status(sacrifice_obligation_continuity__study_as_performance, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sacrifice_obligation_continuity__study_as_performance, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(sacrifice_obligation_continuity__study_as_performance, 'none', 1).
narrative_ontology:epsilon_provenance(sacrifice_obligation_continuity__study_as_performance, 0.08, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sacrifice_obligation_continuity__study_as_performance_tests).
:- end_tests(sacrifice_obligation_continuity__study_as_performance_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low (0.08) because study is freely accessible (books, teachers, communal study halls), requires no coercive infrastructure, and imposes no identifiable cost on a distinguishable victim class — the entire structure is closer to a genuine coordination solution (how to remain observant of an unperformable law) than to an extraction mechanism. Suppression is low because no one is compelled into this reading over the sibling readings; a scholar or community may adopt performance_only or messianic_suspension without formal sanction. Accessibility collapse is modest (0.2): alternatives (the sibling readings) remain visibly available within the same tradition, so this reading has not foreclosed them, only out-competed them in certain communities. Resistance is low: the doctrine is longstanding, textually grounded, and not experienced as burdensome by those who hold it.
 *
 * DIRECTIONALITY LOGIC:
 *   Torah scholars, yeshiva students, and diaspora communities are declared beneficiaries because the doctrine converts what would otherwise be a permanent gap in observance into an actively dischargeable obligation — it subsidizes their religious completeness rather than extracting anything from them. The rabbinic interpretive tradition sits as agenda_setter (it articulates and transmits the doctrine) but collects no material rent, which is why no victim class is declared and requires_active_enforcement is false. There being no victims and low extraction is precisely the structural delta this reading is expected to carry relative to its siblings.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (an unperformable body of commandments threatening a permanent observance gap) remains live wherever no Temple stands, so this is not a case of a mandate persisting past its function — the mechanism (study-as-fulfillment) directly answers a problem that has not gone away. This prevents misclassifying the doctrine as inert theater: the study practice is functionally load-bearing for maintaining a coherent relationship to an entire legal domain, not merely ceremonial.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    study_as_fulfillment_vs_preparation,
    'Is study of sacrificial law genuinely substitutive fulfillment of the commandment, or is it functionally indistinguishable from the performance_only reading''s ''preparation for restoration'' except in doctrinal labeling?',
    'Comparative analysis of how adherents of each reading structure their actual practice: does the study_as_performance community treat the obligation as closed (no residual felt duty toward eventual physical performance) while the performance_only community treats it as open (residual duty persists)? A felt-completeness survey or halakhic-literature analysis of how each tradition discusses the obligation''s status could distinguish them.',
    'If practice is indistinguishable across readings, the doctrinal difference is primarily interpretive/rhetorical rather than structurally consequential, which would lower confidence in treating these as fully separate constraints rather than framings of one.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(study_as_fulfillment_vs_preparation, conceptual, 'Whether study_as_performance is structurally distinct from performance_only in lived practice, or only in doctrinal self-description.').

omega_variable(
    kernel_framing_selection,
    'Is the choice to treat this as one of four coexisting sibling readings (rather than, say, a majority doctrine with three minority dissents, or a historically sequential development where later readings supersede earlier ones) the correct framing of the kernel contest?',
    'Historical-textual analysis of the relative prevalence and chronological emergence of each reading within different communities (e.g. rabbinic vs. Karaite, Ashkenazi vs. Sephardi, pre- vs. post-19th-century reform movements) would clarify whether these are genuinely coexisting live options or a majority/minority structure.',
    'If one reading is demonstrably dominant across the tradition rather than merely one of four equally live options, the coexists_with relations declared here may need revision toward an influences structure where the dominant reading structurally marginalizes the others'' resource base (teaching time, institutional support) without logically foreclosing them.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_selection, conceptual, 'Whether treating the four readings as symmetric coexisting options versus a majority/minority structure is the right framing.').

omega_variable(
    beneficiary_or_vindicated_proposition,
    'Is ''torah_study_equivalent_to_sacrifice'' correctly classified as a vindicated proposition (collecting no rents) rather than functioning as a beneficiary in its own right, given that its doctrinal authority is itself sustained by the very communities who benefit from it being true?',
    'Examine whether any institutional actor (e.g. a specific school or sect) derives material or authority-based advantage specifically from propagating this doctrine over the sibling readings, as opposed to the doctrine being a genuinely free-floating interpretive claim.',
    'If an identifiable institutional actor gains authority or resources specifically from promoting this reading over its siblings, that actor should be added to beneficiaries and the constraint''s extraction profile reassessed upward.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_or_vindicated_proposition, conceptual, 'Whether the vindicated proposition is truly rent-free or whether it quietly encodes an institutional beneficiary.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sacrifice_obligation_continuity__study_as_performance, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sacr_tr_t0, sacrifice_obligation_continuity__study_as_performance, theater_ratio, 0, 0.1).
narrative_ontology:measurement(sacr_tr_t20, sacrifice_obligation_continuity__study_as_performance, theater_ratio, 20, 0.12).
narrative_ontology:measurement(sacr_tr_t40, sacrifice_obligation_continuity__study_as_performance, theater_ratio, 40, 0.13).
narrative_ontology:measurement(sacr_tr_t60, sacrifice_obligation_continuity__study_as_performance, theater_ratio, 60, 0.14).
narrative_ontology:measurement(sacr_tr_t80, sacrifice_obligation_continuity__study_as_performance, theater_ratio, 80, 0.15).
narrative_ontology:measurement(sacr_tr_t100, sacrifice_obligation_continuity__study_as_performance, theater_ratio, 100, 0.15).

% Extraction over time
narrative_ontology:measurement(sacr_be_t0, sacrifice_obligation_continuity__study_as_performance, base_extractiveness, 0, 0.06).
narrative_ontology:measurement(sacr_be_t20, sacrifice_obligation_continuity__study_as_performance, base_extractiveness, 20, 0.07).
narrative_ontology:measurement(sacr_be_t40, sacrifice_obligation_continuity__study_as_performance, base_extractiveness, 40, 0.07).
narrative_ontology:measurement(sacr_be_t60, sacrifice_obligation_continuity__study_as_performance, base_extractiveness, 60, 0.08).
narrative_ontology:measurement(sacr_be_t80, sacrifice_obligation_continuity__study_as_performance, base_extractiveness, 80, 0.08).
narrative_ontology:measurement(sacr_be_t100, sacrifice_obligation_continuity__study_as_performance, base_extractiveness, 100, 0.08).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(sacrifice_obligation_continuity__study_as_performance, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sacrifice_obligation_continuity__study_as_performance, identity_coordination).
narrative_ontology:boltzmann_floor_override(sacrifice_obligation_continuity__study_as_performance, 0.05).
narrative_ontology:affects_constraint(sacrifice_obligation_continuity__study_as_performance, sacrifice_obligation_continuity__performance_only).
narrative_ontology:affects_constraint(sacrifice_obligation_continuity__study_as_performance, sacrifice_obligation_continuity__messianic_suspension).
narrative_ontology:affects_constraint(sacrifice_obligation_continuity__study_as_performance, sacrifice_obligation_continuity__archival_preservation).

% DUAL FORMULATION NOTE:
% This constraint is one of four sibling readings of the sacrifice_obligation_continuity kernel, decomposed per the ε-invariance principle: performance_only (obligation persists but requires future physical performance; study is preparatory), messianic_suspension (obligation is suspended pending restoration; study maintains readiness), archival_preservation (obligation is no longer binding; study is cultural memory without normative force), and this reading, study_as_performance (study itself discharges the obligation). Each carries a distinct ε, distinct beneficiary/victim structure, and distinct classification; they are linked here rather than merged because averaging or hedging across them would violate the closed-constraint identity the framework requires.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
