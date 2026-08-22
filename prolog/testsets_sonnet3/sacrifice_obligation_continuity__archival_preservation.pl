% ============================================================================
% CONSTRAINT STORY: sacrifice_obligation_continuity__archival_preservation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sacrifice_obligation_continuity__archival_preservation, []).

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
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: sacrifice_obligation_continuity__archival_preservation
 *   human_readable: Sacrifice Law as Archival/Cultural-Memory Study (Non-Normative Reading)
 *   domain: religious_law/ritual_studies/textual_tradition
 *
 * SUMMARY:
 *   This story instantiates the archival_preservation reading of the
 *   sacrifice-obligation-continuity kernel: sacrifice law is treated as no
 *   longer normatively binding, and study of it functions purely as cultural
 *   memory and textual-tradition preservation, carrying no religious
 *   obligation. This is structurally distinct from sibling readings that keep
 *   the obligation alive in some form (study_as_performance treats study
 *   itself as fulfillment; performance_only treats study as mere preparation
 *   pending literal performance; messianic_suspension treats the obligation
 *   as dormant, not dead). Under this reading, the constraint space that
 *   would carry extraction (obligation, sanction, normative pressure) is
 *   empty — what remains is voluntary scholarly and communal activity with
 *   negligible extractiveness.
 *
 * KEY AGENTS:
 *   - textual_scholars: analytical/administrative seat, sets study agenda, mobile exit
 *   - cultural_memory_institutions: organized beneficiary, preserves the corpus, mobile exit
 *   - lay_readers_of_tradition: powerless beneficiary, voluntary participation, mobile exit
 *   - messianic_restorationist_communities: excluded seat, disputes the dead-letter framing
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sacrifice_obligation_continuity__archival_preservation, 0.03).
domain_priors:suppression_score(sacrifice_obligation_continuity__archival_preservation, 0.05).
domain_priors:theater_ratio(sacrifice_obligation_continuity__archival_preservation, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__archival_preservation, extractiveness, 0.03).
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__archival_preservation, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__archival_preservation, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__archival_preservation, accessibility_collapse, 0.1).
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__archival_preservation, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sacrifice_obligation_continuity__archival_preservation, rope).
narrative_ontology:human_readable(sacrifice_obligation_continuity__archival_preservation, "Sacrifice Law as Archival/Cultural-Memory Study (Non-Normative Reading)").
narrative_ontology:topic_domain(sacrifice_obligation_continuity__archival_preservation, "religious_law/ritual_studies/textual_tradition").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sacrifice_obligation_continuity__archival_preservation, '3ffc07cf-d643-41dc-b94b-9d565a73706c').
narrative_ontology:cs_kernel_codification('3ffc07cf-d643-41dc-b94b-9d565a73706c', fixed_text).
narrative_ontology:cs_authority_grounding('3ffc07cf-d643-41dc-b94b-9d565a73706c', practice).
narrative_ontology:cs_interpretation_layer_present('3ffc07cf-d643-41dc-b94b-9d565a73706c').
narrative_ontology:cs_reading_relation('3ffc07cf-d643-41dc-b94b-9d565a73706c', sacrifice_obligation_continuity__study_as_performance, coexists_with).
narrative_ontology:cs_reading_relation('3ffc07cf-d643-41dc-b94b-9d565a73706c', sacrifice_obligation_continuity__performance_only, coexists_with).
narrative_ontology:cs_reading_relation('3ffc07cf-d643-41dc-b94b-9d565a73706c', sacrifice_obligation_continuity__messianic_suspension, influences).
narrative_ontology:cs_axiom('3ffc07cf-d643-41dc-b94b-9d565a73706c', foundational, sacrifice_obligation_lapsed_with_conditions).
narrative_ontology:cs_axiom_status(sacrifice_obligation_lapsed_with_conditions, holdable).
narrative_ontology:cs_axiom_grounding('3ffc07cf-d643-41dc-b94b-9d565a73706c', sacrifice_obligation_lapsed_with_conditions, conventional).
narrative_ontology:cs_axiom('3ffc07cf-d643-41dc-b94b-9d565a73706c', secondary, study_carries_no_normative_force).
narrative_ontology:cs_axiom_status(study_carries_no_normative_force, holdable).
narrative_ontology:cs_axiom_grounding('3ffc07cf-d643-41dc-b94b-9d565a73706c', study_carries_no_normative_force, conventional).
narrative_ontology:cs_reference_frame('3ffc07cf-d643-41dc-b94b-9d565a73706c', temple_era_performative_obligation).
narrative_ontology:cs_drift_state('3ffc07cf-d643-41dc-b94b-9d565a73706c', contemporary_post_temple_scholarship, gap(codification_collapse, substantial, true)).
narrative_ontology:cs_created_at('3ffc07cf-d643-41dc-b94b-9d565a73706c', '').
narrative_ontology:cs_kernel_id(sacrifice_obligation_continuity__archival_preservation, sacrifice_obligation_continuity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sacrifice_obligation_continuity__archival_preservation, textual_scholars).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_continuity__archival_preservation, cultural_memory_institutions).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_continuity__archival_preservation, lay_readers_of_tradition).
narrative_ontology:constraint_vindicates(sacrifice_obligation_continuity__archival_preservation, historical_continuity_of_textual_tradition).
narrative_ontology:constraint_vindicates(sacrifice_obligation_continuity__archival_preservation, sacrifice_law_as_dead_letter).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Study sacrifice law as a historical-textual corpus — philology, redaction history, comparative ritual studies. They set the interpretive agenda for how the texts are read in academic and communal-education settings, but nothing compels their conclusions and no sanction attaches to studying it differently or not at all. They gain scholarly output, credentialing, and standing within a field; they can leave the field for any other area of study at no structural cost beyond ordinary career switching.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__archival_preservation, textual_scholars, beneficiary,
    moderate, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(sacrifice_obligation_continuity__archival_preservation, textual_scholars, agenda_setter).

% Museums, seminaries, and communal archives preserve sacrifice-law texts and commentary as part of a living cultural-historical record. They benefit from continuity of transmission and community interest, but their institutional survival does not depend on anyone accepting the law as currently binding; they could pivot to other holdings or emphases without existential threat.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__archival_preservation, cultural_memory_institutions, beneficiary,
    organized, civilizational, mobile, global).

% Individuals who read or study the sacrifice-law texts as part of communal education, holiday liturgy, or personal interest. They gain a sense of historical rootedness and access to a shared textual heritage. Nothing is extracted from them: participation is voluntary, carries no material cost, and non-participation carries no sanction under this reading.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__archival_preservation, lay_readers_of_tradition, beneficiary,
    powerless, biographical, mobile, local).

% Hold that the obligation is merely suspended and that study should function as readiness-maintenance for future literal performance. They are not addressed by this reading's framing — under archival_preservation their normative claim is treated as historically superseded rather than argued with, which they would dispute if consulted.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__archival_preservation, messianic_restorationist_communities, excluded,
    moderate, civilizational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(sacrifice_obligation_continuity__archival_preservation, diffuse).
narrative_ontology:fixing_cost_class(sacrifice_obligation_continuity__archival_preservation, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates transmission of a historically significant legal-textual corpus across generations of scholars, educators, and communities so that the corpus is not lost, without asking anyone to treat its provisions as currently actionable.
% TRANSFER_FUNCTION: Moves interpretive labor and institutional attention toward preserving and explaining the texts; moves no material resource, ritual obligation, or sanction between any parties. Nothing is extracted from any participant under this reading.
% ABSENT_VOICES: Messianic-restorationist and performance-only communities would object that this reading treats the obligation as dead rather than dormant or pending, effectively pre-empting their theological claim by redescribing it as antiquarian. They are present in the broader kernel contest but not addressed inside this reading's own framing.
% DISAPPEARANCE_RATIONALE: If archival study of sacrifice law stopped entirely tomorrow, no material transfer would cease, no institution would lose its operating basis, and no party currently bears a cost that would be lifted. Some scholarship and cultural programming would end, but no one's obligations, entitlements, or exit options would change — the practice is additive cultural activity, not load-bearing coordination.
% FOUNDING_PROBLEM: After the historical conditions for literal sacrifice performance ended (loss of the physical site and its infrastructure), communities needed a way to retain the textual and legal tradition without either pretending the law was still operative or discarding centuries of legal-textual scholarship.
% FOUNDING_PROBLEM_CORROBORATION: Historians of religion and comparative-law scholars outside any sacrifice-observant community corroborate that the operative conditions for the law's performance ceased long ago and that its current transmission functions as cultural-historical preservation; this is also the position taken by academic religious-studies departments with no stake in the tradition's continuation as living law.
narrative_ontology:disappearance_verdict(sacrifice_obligation_continuity__archival_preservation, world_unchanged).
narrative_ontology:founding_problem_status(sacrifice_obligation_continuity__archival_preservation, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sacrifice_obligation_continuity__archival_preservation, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(sacrifice_obligation_continuity__archival_preservation, 'none', 1).
narrative_ontology:epsilon_provenance(sacrifice_obligation_continuity__archival_preservation, 0.03, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sacrifice_obligation_continuity__archival_preservation_tests).
:- end_tests(sacrifice_obligation_continuity__archival_preservation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored near zero (0.03-0.04) because under this reading no normative claim is enforced against anyone: no sanction attaches to non-study, no material transfer is compelled, and no party's exit is blocked. Suppression is low (0.05) for the same reason — accessibility_collapse and resistance are both authored low because the alternative (not studying, or studying differently) is neither foreclosed nor actively resisted; the tradition simply continues as an available cultural option. Theater_ratio is modest and rises slightly over the interval (0.10 to 0.15) reflecting that some institutional preservation activity (commemorative programming, museum framing) carries a performative cultural-identity function alongside genuine scholarly/archival substance, without ever approaching a Goodhart-drift threshold.
 *
 * DIRECTIONALITY LOGIC:
 *   All named agents are beneficiaries or analytically excluded, not victims, because this reading removes the obligation from constraint space entirely — there is no one from whom this reading extracts. Textual scholars and cultural institutions gain professional and institutional value from the preserved corpus; lay readers gain optional access to heritage; none bear a cost imposed by the arrangement. Messianic-restorationist communities are marked excluded rather than payer because the harm they experience is dispute over framing (their claim is redescribed as obsolete), not material extraction — this is a disagreement about which reading is correct, which belongs in the omega variables and cs_structure, not in this reading's victim declarations.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents mislabeling continued scholarly interest as evidence that the underlying obligation persists (which would falsely inflate extraction by attributing coercive force to voluntary cultural activity). Conversely, it prevents dismissing the study tradition as mere theater by acknowledging its genuine coordination value in preserving textual continuity — the founding_problem_status of 'dead' combined with continued study is exactly the mandatrophy-adjacent pattern (an arrangement outliving its original necessity) resolved here by classifying the residual practice as low-extraction voluntary coordination (rope) rather than either snare (if obligation were falsely enforced) or scaffold (if a sunset were pending).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    which_reading_is_operative_for_a_given_community,
    'For any specific community or individual studying sacrifice law, is the archival_preservation reading actually the one operative for them, or do they in fact hold study_as_performance, performance_only, or messianic_suspension — meaning this story''s zero-extraction classification would not apply to their actual practice?',
    'Survey or ethnographic work distinguishing communities'' stated theological commitments regarding the current normative status of sacrifice law; self-report of whether study is experienced as obligatory versus optional cultural engagement.',
    'If a community actually holds a live-obligation reading (study_as_performance or messianic_suspension) while this story is applied to describe them, the zero-extractiveness classification would misdescribe genuine felt religious obligation as voluntary hobbyism — a category error in the opposite direction from false-summit detection.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(which_reading_is_operative_for_a_given_community, conceptual, 'Whether a given community''s actual practice matches the archival_preservation reading or a sibling reading.').

omega_variable(
    reframing_as_quiet_foreclosure,
    'Does treating the obligation as simply ''dead'' constitute an implicit theological argument against the messianic_suspension and performance_only readings, even though this story does not explicitly argue against them?',
    'Textual and rhetorical analysis of how archival-preservation framing is used in academic and communal settings — does it present itself as neutral historical description or as an implicit denial of restorationist theology?',
    'If archival framing functions as de facto argument rather than neutral description, its adoption in educational institutions could exert real structural pressure on restorationist communities'' legitimacy, which would warrant an ''influences'' rather than pure ''coexists_with'' relation to messianic_suspension.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reframing_as_quiet_foreclosure, conceptual, 'Whether framing sacrifice law as archival is theologically neutral or implicitly argumentative against live-obligation readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sacrifice_obligation_continuity__archival_preservation, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sacr_tr_t0, sacrifice_obligation_continuity__archival_preservation, theater_ratio, 0, 0.1).
narrative_ontology:measurement(sacr_tr_t10, sacrifice_obligation_continuity__archival_preservation, theater_ratio, 10, 0.12).
narrative_ontology:measurement(sacr_tr_t20, sacrifice_obligation_continuity__archival_preservation, theater_ratio, 20, 0.13).
narrative_ontology:measurement(sacr_tr_t30, sacrifice_obligation_continuity__archival_preservation, theater_ratio, 30, 0.14).
narrative_ontology:measurement(sacr_tr_t40, sacrifice_obligation_continuity__archival_preservation, theater_ratio, 40, 0.15).
narrative_ontology:measurement(sacr_tr_t50, sacrifice_obligation_continuity__archival_preservation, theater_ratio, 50, 0.15).

% Extraction over time
narrative_ontology:measurement(sacr_be_t0, sacrifice_obligation_continuity__archival_preservation, base_extractiveness, 0, 0.04).
narrative_ontology:measurement(sacr_be_t10, sacrifice_obligation_continuity__archival_preservation, base_extractiveness, 10, 0.04).
narrative_ontology:measurement(sacr_be_t20, sacrifice_obligation_continuity__archival_preservation, base_extractiveness, 20, 0.03).
narrative_ontology:measurement(sacr_be_t30, sacrifice_obligation_continuity__archival_preservation, base_extractiveness, 30, 0.03).
narrative_ontology:measurement(sacr_be_t40, sacrifice_obligation_continuity__archival_preservation, base_extractiveness, 40, 0.03).
narrative_ontology:measurement(sacr_be_t50, sacrifice_obligation_continuity__archival_preservation, base_extractiveness, 50, 0.03).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(sacrifice_obligation_continuity__archival_preservation, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sacrifice_obligation_continuity__archival_preservation, identity_coordination).
narrative_ontology:boltzmann_floor_override(sacrifice_obligation_continuity__archival_preservation, 0.08).
narrative_ontology:affects_constraint(sacrifice_obligation_continuity__archival_preservation, sacrifice_obligation_continuity__messianic_suspension).
narrative_ontology:affects_constraint(sacrifice_obligation_continuity__archival_preservation, sacrifice_obligation_continuity__performance_only).
narrative_ontology:affects_constraint(sacrifice_obligation_continuity__archival_preservation, sacrifice_obligation_continuity__study_as_performance).

% DUAL FORMULATION NOTE:
% This story is one of four constraints decomposing the natural-language 'sacrifice obligation continuity' kernel per the ε-invariance principle. archival_preservation authors near-zero extractiveness (obligation exits constraint space); study_as_performance and performance_only author substantial extractiveness (obligation persists, enforced through communal/religious sanction on non-study or non-readiness); messianic_suspension sits between (dormant obligation, readiness-maintenance pressure). Each sibling has its own ε and its own beneficiary/victim structure; they are linked here rather than merged because measuring 'is sacrifice law binding' by different communities' own lights yields genuinely different extraction values, not one constraint viewed from different angles.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
