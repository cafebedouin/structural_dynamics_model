% ============================================================================
% CONSTRAINT STORY: vatican_ii_authority__continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vatican_ii_authority__continuity_reading, []).

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
 *   constraint_id: vatican_ii_authority__continuity_reading
 *   human_readable: Vatican II as Organic Development in Continuity ('Hermeneutic of Continuity')
 *   domain: theology/ecclesiology/religious_authority
 *
 * SUMMARY:
 *   This story instantiates the continuity reading of the Vatican II
 *   authority kernel: the claim that the sixteen conciliar documents
 *   represent organic development of the unchanging deposit of faith, such
 *   that post-conciliar reforms are legitimate expressions of tradition
 *   rather than departures from it, with ambiguous passages resolvable
 *   through traditional hermeneutical method (the 'hermeneutic of continuity'
 *   associated with Benedict XVI's 2005 Curia address). This is one of three
 *   sibling constraints reading the same kernel — the rupture reading and the
 *   composite-overdetermination reading are separate constraint files with
 *   their own ε values, beneficiary/victim structures, and classifications.
 *   This story does not describe or average over those readings; it is a
 *   clean, ε-invariant account of the continuity position alone.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vatican_ii_authority__continuity_reading, 0.22).
domain_priors:suppression_score(vatican_ii_authority__continuity_reading, 0.28).
domain_priors:theater_ratio(vatican_ii_authority__continuity_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vatican_ii_authority__continuity_reading, extractiveness, 0.22).
narrative_ontology:constraint_metric(vatican_ii_authority__continuity_reading, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(vatican_ii_authority__continuity_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vatican_ii_authority__continuity_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(vatican_ii_authority__continuity_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vatican_ii_authority__continuity_reading, rope).
narrative_ontology:human_readable(vatican_ii_authority__continuity_reading, "Vatican II as Organic Development in Continuity ('Hermeneutic of Continuity')").
narrative_ontology:topic_domain(vatican_ii_authority__continuity_reading, "theology/ecclesiology/religious_authority").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vatican_ii_authority__continuity_reading, '2fa6e2d1-fe8a-4928-b086-62ce00d64164').
narrative_ontology:cs_kernel_codification('2fa6e2d1-fe8a-4928-b086-62ce00d64164', fixed_text).
narrative_ontology:cs_authority_grounding('2fa6e2d1-fe8a-4928-b086-62ce00d64164', lineage).
narrative_ontology:cs_interpretation_layer_present('2fa6e2d1-fe8a-4928-b086-62ce00d64164').
narrative_ontology:cs_reading_relation('2fa6e2d1-fe8a-4928-b086-62ce00d64164', vatican_ii_authority__rupture_reading, forecloses).
narrative_ontology:cs_reading_relation('2fa6e2d1-fe8a-4928-b086-62ce00d64164', vatican_ii_authority__composite_overdetermination_reading, influences).
narrative_ontology:cs_axiom('2fa6e2d1-fe8a-4928-b086-62ce00d64164', foundational, unbroken_deposit_of_faith).
narrative_ontology:cs_axiom_status(unbroken_deposit_of_faith, holdable).
narrative_ontology:cs_axiom_grounding('2fa6e2d1-fe8a-4928-b086-62ce00d64164', unbroken_deposit_of_faith, deontological).
narrative_ontology:cs_axiom('2fa6e2d1-fe8a-4928-b086-62ce00d64164', foundational, ambiguity_resolvable_by_traditional_hermeneutics).
narrative_ontology:cs_axiom_status(ambiguity_resolvable_by_traditional_hermeneutics, holdable).
narrative_ontology:cs_axiom_grounding('2fa6e2d1-fe8a-4928-b086-62ce00d64164', ambiguity_resolvable_by_traditional_hermeneutics, conventional).
narrative_ontology:cs_reference_frame('2fa6e2d1-fe8a-4928-b086-62ce00d64164', unbroken_apostolic_deposit_of_faith).
narrative_ontology:cs_drift_state('2fa6e2d1-fe8a-4928-b086-62ce00d64164', post_benedict_xvi_hermeneutic_address, gap(stable, minor, true)).
narrative_ontology:cs_created_at('2fa6e2d1-fe8a-4928-b086-62ce00d64164', '').
narrative_ontology:cs_kernel_id(vatican_ii_authority__continuity_reading, vatican_ii_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vatican_ii_authority__continuity_reading, progressive_reformers_claiming_continuity).
narrative_ontology:constraint_beneficiary(vatican_ii_authority__continuity_reading, conciliar_bishops_and_successors).
narrative_ontology:constraint_beneficiary(vatican_ii_authority__continuity_reading, post_conciliar_magisterium).
narrative_ontology:constraint_beneficiary(vatican_ii_authority__continuity_reading, ecumenical_dialogue_participants).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Clergy, theologians, and lay movements who implemented liturgical, ecumenical, and pastoral reforms after the Council. Under the continuity reading, their reforms are vindicated as legitimate developments of the deposit of faith rather than departures from it, which grants their work theological legitimacy and institutional protection from charges of heresy or discontinuity.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__continuity_reading, progressive_reformers_claiming_continuity, beneficiary,
    organized, generational, mobile, global).

% The papal and episcopal teaching authority that has, since the Council, consistently affirmed the continuity reading through documents, catechisms, and papal statements. Administers the interpretive framework, adjudicates disputes about conciliar meaning, and has institutional interest in a reading that preserves unbroken authority across the conciliar threshold.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__continuity_reading, post_conciliar_magisterium, agenda_setter,
    institutional, civilizational, analytical, global).

% Groups and individuals (from SSPX-aligned communities to milder traditionalist currents within full communion) who hold that specific conciliar texts or their implementation constitute genuine rupture. Their objection — that the continuity reading obscures real doctrinal discontinuity on religious liberty, ecumenism, and collegiality — is treated as marginal or schismatic rather than engaged as a live theological alternative within mainstream discourse.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__continuity_reading, traditionalist_communities, excluded,
    moderate, generational, constrained, national).

% The bishops who promulgated the sixteen documents and their successors in office. The continuity reading confirms the validity and binding authority of everything they signed and everything subsequently built on it; a rupture verdict would destabilize their own teaching authority and the authority of everything issued since.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__continuity_reading, conciliar_bishops_and_successors, beneficiary,
    institutional, civilizational, analytical, global).
narrative_ontology:stakeholder_secondary_role(vatican_ii_authority__continuity_reading, conciliar_bishops_and_successors, agenda_setter).

% Non-Catholic Christian communities and interfaith partners engaged through Nostra Aetate and Unitatis Redintegratio. Their dialogue relationships depend on the Council's ecumenical openings being read as authentic development of Catholic teaching rather than a temporary or erroneous deviation that a future magisterium could simply reverse.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__continuity_reading, ecumenical_dialogue_participants, beneficiary,
    moderate, generational, mobile, global).

% Historians and theologians studying the Council's textual and redaction history, comparing conciliar language against prior magisterial statements to assess doctrinal continuity or discontinuity on a document-by-document basis, largely outside confessional stakes in the outcome.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__continuity_reading, academic_theologians_neutral, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single interpretive key that allows the entire body of post-conciliar teaching, liturgy, and pastoral practice to be received as authoritative without requiring each believer or bishop to individually adjudicate whether each conciliar document is compatible with prior tradition — it coordinates reception of forty-plus years of magisterial output under one hermeneutical umbrella.
% TRANSFER_FUNCTION: Transfers interpretive legitimacy and freedom from doctrinal jeopardy to those who implemented conciliar reforms, while transferring the burden of proof onto anyone who wishes to argue a given reform is discontinuous with tradition — such critics must now argue against a settled magisterial default rather than being met on neutral ground.
% ABSENT_VOICES: Traditionalist communities and theologians who read specific texts (on religious liberty, collegiality, ecumenism) as substantively discontinuous with prior teaching are treated as fringe rather than as holding a live alternative reading; their arguments are largely absent from mainstream catechetical and magisterial discourse, which is itself already committed to the continuity framework before their objections are heard.
% DISAPPEARANCE_RATIONALE: If the continuity reading were abandoned by the magisterium tomorrow, its institutional beneficiaries dispute what would happen: the post-conciliar magisterium and conciliar successors would face a crisis of retroactive legitimacy for everything taught since 1965, and reformist communities would lose their doctrinal cover; but rupture-reading adherents argue the world would simply become honest about a discontinuity they already believe occurred, so nothing structural would rearrange, only the label. This is exactly the kind of split verdict a kernel contest produces.
% FOUNDING_PROBLEM: The Council needed to reconcile its explicit self-description as pastoral and non-defining with its practical function of authorizing sweeping changes to liturgy, ecumenical posture, and church-state relations — the continuity reading solves the problem of how texts that claimed not to define new dogma could nonetheless licitly ground doctrinal and practical change.
% FOUNDING_PROBLEM_CORROBORATION: Independent historians of the Council (e.g. those associated with the Bologna school and its critics alike) attest that the tension between the Council's pastoral self-description and its substantive effects is a real and still-debated feature of the documents, not merely a partisan invention of either the continuity or rupture camps — though these same historians disagree sharply on how the tension should be resolved, which is itself evidence the founding problem remains unsettled rather than closed by fiat.
narrative_ontology:disappearance_verdict(vatican_ii_authority__continuity_reading, contested).
narrative_ontology:founding_problem_status(vatican_ii_authority__continuity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vatican_ii_authority__continuity_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(vatican_ii_authority__continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vatican_ii_authority__continuity_reading, 0.22, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vatican_ii_authority__continuity_reading_tests).
:- end_tests(vatican_ii_authority__continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low (0.22, rising modestly over sixty years) because the continuity reading, taken on its own terms, imposes minimal coercive cost: it does not claim anyone is harmed, and its own account holds that reforms are cost-free developments. Suppression is moderate (0.28) rather than low, reflecting that maintaining the continuity reading against visible textual tensions (religious liberty vs. prior condemnations of indifferentism, collegiality vs. prior papal primacy language) requires some interpretive labor and occasional marginalization of dissenting readings, but does not require coercive enforcement machinery — no one is excommunicated for doubting continuity per se. Theater ratio is low (0.15) because the interpretive work is substantive theological argument, not empty performance. Accessibility collapse is moderate (0.45): once inside the continuity framework, alternative readings become harder to entertain, but they have not collapsed entirely — rupture and composite readings remain live in theological literature. Resistance is moderate-high (0.55) because traditionalist communities mount real, sustained theological resistance to this reading, even though they remain a minority voice.
 *
 * PERSPECTIVAL GAP:
 *   From the magisterium's own seat, this reading is coordination: a single hermeneutic that lets the whole body of post-conciliar teaching be received without repeated crises of legitimacy. From the excluded traditionalist seat, the same reading looks like an assertion that forecloses their objection by definitional fiat rather than engaging it — they experience the low authored extraction/suppression scores as themselves part of the problem: the reading's power lies precisely in appearing costless and uncontested from the inside.
 *
 * DIRECTIONALITY LOGIC:
 *   The post-conciliar magisterium and the bishops who promulgated the documents are the clearest beneficiaries: the continuity reading validates their own authority and everything built on it since 1965, so they sit near the beneficiary end of directionality. Progressive reformers who implemented liturgical and pastoral changes similarly benefit — the reading licenses their work. Ecumenical dialogue partners benefit indirectly, since continuity assures them the Church's openness to dialogue is not merely provisional. No group is authored as a victim within THIS reading, because the continuity reading's own internal logic holds that reforms are cost-free developments — there is no loser in its account of itself. Traditionalist communities are not victims of the constraint's operation in the technical sense (nothing is extracted from them); rather they are excluded voices whose alternative theological reading is structurally sidelined by the dominance of this one. That exclusion is captured in six_questions.absent_voices and the excluded stakeholder role, not in base_properties.victims, since victimhood in the DR sense requires the constraint to extract from them, and the continuity reading does not claim to do so.
 *
 * MANDATROPHY ANALYSIS:
 *   The continuity reading resists mandatrophy mislabeling by keeping its founding problem (reconciling a pastoral council's self-description with its substantive doctrinal effects) explicitly live rather than declaring it settled by fiat. Framing this as ongoing organic development rather than a one-time rupture prevents treating the Council as either dead letter or permanent crisis; it is a coordination device that lets doctrine evolve incrementally under a continuity umbrella rather than requiring each development to be separately re-litigated against the whole prior tradition.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    textual_discontinuity_residue,
    'Do specific conciliar passages (on religious liberty in Dignitatis Humanae, collegiality in Lumen Gentium, ecumenism in Unitatis Redintegratio) contain genuine propositional contradictions with prior magisterial statements (e.g. Quanta Cura, Mortalium Animos), or are the apparent tensions resolvable as development-within-continuity given a sufficiently rich theory of doctrinal development?',
    'Close comparative textual analysis of the conciliar documents against the specific prior magisterial statements alleged to be contradicted, adjudicated against an explicit, pre-agreed theory of what counts as organic development versus reversal — absent such a shared theory, the question is not resolvable by textual analysis alone since both readings can accommodate the same texts under different development criteria.',
    'If genuine propositional contradiction is established under any widely accepted theory of doctrinal development, the continuity reading''s core axiom (unbroken_deposit_of_faith) becomes harder to hold as empirically_contingent rather than merely asserted, strengthening the rupture reading''s claim; if no such contradiction is established, the continuity reading is empirically vindicated relative to its rival.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(textual_discontinuity_residue, conceptual, 'Whether apparent conciliar/pre-conciliar tensions are real contradictions or resolvable developments — the load-bearing empirical/hermeneutical question the whole kernel contest turns on.').

omega_variable(
    beneficiary_capture_of_hermeneutic,
    'Is the continuity reading naturally entailed by careful theological method applied to the texts, or is it substantially shaped by the institutional interest of the post-conciliar magisterium and conciliar successors in preserving the validity of their own authority and everything built on it since 1965?',
    'Compare the continuity reading''s reception pattern across institutional actors with a stake in conciliar validity versus independent historians and theologians outside the magisterial hierarchy who have no comparable institutional stake; convergence would weaken the capture hypothesis, divergence correlated with institutional position would strengthen it.',
    'If the reading is substantially explained by beneficiary interest rather than textual/theological merit, the constraint''s low authored extractiveness may understate a subtler form of extraction — legitimacy capture rather than material extraction — and the FSM-adjacent concern (a claim of natural doctrinal development that happens to benefit those who administer it) becomes live even though this story is not authored as a mountain.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_capture_of_hermeneutic, conceptual, 'Whether the continuity reading is theologically neutral or shaped by the institutional interest of those who benefit from it.').

omega_variable(
    traditionalist_exclusion_proportionality,
    'Is the marginalization of the rupture reading within mainstream Catholic institutional discourse proportionate to its theological weakness, or does it reflect institutional power asymmetry (the magisterium controls catechesis, seminary formation, and official commentary) independent of the argument''s merits?',
    'Examine whether rupture-reading arguments receive engagement proportionate to their prevalence in serious theological literature within official Catholic educational and catechetical materials, versus being treated as automatically schismatic regardless of argumentative content.',
    'If exclusion is disproportionate to argumentative merit, the absent_voices assessment understates a real suppression mechanism operating through institutional control of formation and catechesis rather than through explicit coercion, which would push the suppression metric upward from its currently authored moderate value.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(traditionalist_exclusion_proportionality, empirical, 'Whether traditionalist exclusion from mainstream discourse tracks argument quality or institutional control of formation channels.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vatican_ii_authority__continuity_reading, 1965, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vati_tr_t1965, vatican_ii_authority__continuity_reading, theater_ratio, 1965, 0.08).
narrative_ontology:measurement(vati_tr_t1975, vatican_ii_authority__continuity_reading, theater_ratio, 1975, 0.1).
narrative_ontology:measurement(vati_tr_t1985, vatican_ii_authority__continuity_reading, theater_ratio, 1985, 0.11).
narrative_ontology:measurement(vati_tr_t2000, vatican_ii_authority__continuity_reading, theater_ratio, 2000, 0.13).
narrative_ontology:measurement(vati_tr_t2013, vatican_ii_authority__continuity_reading, theater_ratio, 2013, 0.14).
narrative_ontology:measurement(vati_tr_t2025, vatican_ii_authority__continuity_reading, theater_ratio, 2025, 0.15).

% Extraction over time
narrative_ontology:measurement(vati_be_t1965, vatican_ii_authority__continuity_reading, base_extractiveness, 1965, 0.15).
narrative_ontology:measurement(vati_be_t1975, vatican_ii_authority__continuity_reading, base_extractiveness, 1975, 0.17).
narrative_ontology:measurement(vati_be_t1985, vatican_ii_authority__continuity_reading, base_extractiveness, 1985, 0.18).
narrative_ontology:measurement(vati_be_t2000, vatican_ii_authority__continuity_reading, base_extractiveness, 2000, 0.19).
narrative_ontology:measurement(vati_be_t2013, vatican_ii_authority__continuity_reading, base_extractiveness, 2013, 0.2).
narrative_ontology:measurement(vati_be_t2025, vatican_ii_authority__continuity_reading, base_extractiveness, 2025, 0.22).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(vatican_ii_authority__continuity_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vatican_ii_authority__continuity_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(vatican_ii_authority__continuity_reading, 0.08).
narrative_ontology:affects_constraint(vatican_ii_authority__continuity_reading, vatican_ii_authority__rupture_reading).
narrative_ontology:affects_constraint(vatican_ii_authority__continuity_reading, vatican_ii_authority__composite_overdetermination_reading).

% DUAL FORMULATION NOTE:
% This story is one of three constraint files reading the same vatican_ii_authority kernel. The continuity_reading (this file) authors low extractiveness (0.22) and no victims, treating reforms as cost-free development. The rupture_reading is expected to author substantially higher extractiveness and name victims (those who accepted reforms believing them continuous with tradition, or those disciplined for holding the rupture view). The composite_overdetermination_reading is expected to reject a single ε value entirely in favor of documenting structural ambiguity across distinct doctrinal shifts. All three share the same sixteen conciliar documents as their object but diverge in claimed_type, beneficiary/victim structure, and the coherence of the interpretive project itself. Per the ε-invariance principle, these are three distinct constraints, not one constraint measured three ways.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
