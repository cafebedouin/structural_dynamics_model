% ============================================================================
% CONSTRAINT STORY: kodashim_corpus__substitution_archive
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kodashim_corpus__substitution_archive, []).

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
 *   constraint_id: kodashim_corpus__substitution_archive
 *   human_readable: Kodashim as Memorial Archive Superseding Sacrifice (Substitution Reading)
 *   domain: religious/textual/institutional
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the kodashim_corpus kernel — the
 *   substitution_archive reading, in which prayer and Torah study did not
 *   merely fill a gap left by the Temple's destruction but actively REPLACED
 *   sacrifice, rendering the Kodashim tractates a memorial archive of a
 *   superseded practice rather than dormant law awaiting reactivation. Under
 *   this reading, the corpus is studied as historical/legal record —
 *   valuable, canonical, even sacred — but its status is settled rather than
 *   provisional. This is structurally distinct from the performance_only
 *   reading (a husk awaiting messianic restoration) and the study_as_exercise
 *   reading (study itself occupies and fulfills the kernel). Each reading has
 *   a different ε: performance_only treats the corpus as low-extraction
 *   dormant record (closer to mountain/piton, no live beneficiary capturing
 *   rents from a suspended practice); study_as_exercise treats it as a
 *   genuinely occupied rope (study itself IS the mitzvah, high coordination,
 *   low asymmetric extraction); this substitution_archive reading sits in
 *   between and above both on extraction, because it makes an affirmative
 *   continuity claim — 'the mitzvah has been fully transferred to us' — that
 *   both licenses institutional centrality for text-study authorities AND
 *   forecloses restorationist aspiration by fiat rather than by argued
 *   ruling. That double move (claim continuity + deny restoration) is exactly
 *   what produces the tangled_rope structure: genuine coordination function
 *   (portable, Temple-independent religious life) coexisting with asymmetric
 *   extraction (restorationists bear the cost of a foreclosure that is
 *   asserted, not adjudicated).
 *
 * KEY AGENTS:
 *   - rabbinic_text_study_institutions: primary beneficiary — institutional centrality derives from the substitution claim
 *   - prayer_liturgy_authorities: secondary beneficiary — liturgical legitimacy rests on same claim
 *   - restorationist_sacrificial_practitioners: primary target — told the matter is settled, not argued with
 *   - temple_mount_activist_communities: secondary target — delegitimized politically via the same framing
 *   - comparative_religion_scholars: analytical observer — traces the historical construction of the reading
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kodashim_corpus__substitution_archive, 0.52).
domain_priors:suppression_score(kodashim_corpus__substitution_archive, 0.44).
domain_priors:theater_ratio(kodashim_corpus__substitution_archive, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kodashim_corpus__substitution_archive, extractiveness, 0.52).
narrative_ontology:constraint_metric(kodashim_corpus__substitution_archive, suppression_requirement, 0.44).
narrative_ontology:constraint_metric(kodashim_corpus__substitution_archive, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kodashim_corpus__substitution_archive, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(kodashim_corpus__substitution_archive, resistance, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kodashim_corpus__substitution_archive, tangled_rope).
narrative_ontology:human_readable(kodashim_corpus__substitution_archive, "Kodashim as Memorial Archive Superseding Sacrifice (Substitution Reading)").
narrative_ontology:topic_domain(kodashim_corpus__substitution_archive, "religious/textual/institutional").

domain_priors:requires_active_enforcement(kodashim_corpus__substitution_archive).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kodashim_corpus__substitution_archive, '4633e7a2-0c7f-4f34-b4d3-837d2b0f6b73').
narrative_ontology:cs_kernel_codification('4633e7a2-0c7f-4f34-b4d3-837d2b0f6b73', fixed_text).
narrative_ontology:cs_authority_grounding('4633e7a2-0c7f-4f34-b4d3-837d2b0f6b73', lineage).
narrative_ontology:cs_interpretation_layer_present('4633e7a2-0c7f-4f34-b4d3-837d2b0f6b73').
narrative_ontology:cs_reading_relation('4633e7a2-0c7f-4f34-b4d3-837d2b0f6b73', kodashim_corpus__performance_only, coexists_with).
narrative_ontology:cs_reading_relation('4633e7a2-0c7f-4f34-b4d3-837d2b0f6b73', kodashim_corpus__study_as_exercise, influences).
narrative_ontology:cs_axiom('4633e7a2-0c7f-4f34-b4d3-837d2b0f6b73', foundational, sacrifice_permanently_transferred_to_prayer_and_study).
narrative_ontology:cs_axiom_status(sacrifice_permanently_transferred_to_prayer_and_study, holdable).
narrative_ontology:cs_axiom_grounding('4633e7a2-0c7f-4f34-b4d3-837d2b0f6b73', sacrifice_permanently_transferred_to_prayer_and_study, conventional).
narrative_ontology:cs_axiom('4633e7a2-0c7f-4f34-b4d3-837d2b0f6b73', secondary, kodashim_is_historical_record_not_live_law).
narrative_ontology:cs_axiom_status(kodashim_is_historical_record_not_live_law, holdable).
narrative_ontology:cs_axiom_grounding('4633e7a2-0c7f-4f34-b4d3-837d2b0f6b73', kodashim_is_historical_record_not_live_law, conventional).
narrative_ontology:cs_reference_frame('4633e7a2-0c7f-4f34-b4d3-837d2b0f6b73', second_temple_operative_sacrificial_order).
narrative_ontology:cs_drift_state('4633e7a2-0c7f-4f34-b4d3-837d2b0f6b73', post_destruction_rabbinic_consolidation, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('4633e7a2-0c7f-4f34-b4d3-837d2b0f6b73', '').
narrative_ontology:cs_kernel_id(kodashim_corpus__substitution_archive, kodashim_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kodashim_corpus__substitution_archive, rabbinic_text_study_institutions).
narrative_ontology:constraint_beneficiary(kodashim_corpus__substitution_archive, prayer_liturgy_authorities).
narrative_ontology:constraint_victim(kodashim_corpus__substitution_archive, restorationist_sacrificial_practitioners).
narrative_ontology:constraint_victim(kodashim_corpus__substitution_archive, temple_mount_activist_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(kodashim_corpus__substitution_archive, diaspora_lay_communities).
narrative_ontology:constraint_victim(kodashim_corpus__substitution_archive, diaspora_lay_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Yeshivot and rabbinic academies teach Kodashim tractates as a closed corpus of law about a discontinued practice. They derive institutional authority, curricular centrality, and communal legitimacy from being the custodians of the record of what prayer and study replaced. Their continuity claim — that study stands in for sacrifice — is what licenses their centrality; they set the terms under which Kodashim is read and taught.
narrative_ontology:constraint_stakeholder(kodashim_corpus__substitution_archive, rabbinic_text_study_institutions, beneficiary,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(kodashim_corpus__substitution_archive, rabbinic_text_study_institutions, agenda_setter).

% Communities and clergy whose liturgical practice (fixed prayer times keyed to former sacrificial hours, textual recitation in place of offering) draws its warrant from the claim that prayer has taken over sacrifice's function. They benefit from Kodashim being read as superseded-but-honored, since that framing makes their substitute practice the legitimate continuation rather than a mere placeholder.
narrative_ontology:constraint_stakeholder(kodashim_corpus__substitution_archive, prayer_liturgy_authorities, beneficiary,
    organized, generational, constrained, global).

% Individuals and small movements who wish to see sacrificial practice actually resumed are told by the dominant institutional reading that the matter is settled — permanently transferred to prayer and study, not merely suspended. Their aspiration is treated as either naive, dangerous, or premature by the institutions that hold interpretive authority, foreclosing their practice without an explicit ruling against its possibility.
narrative_ontology:constraint_stakeholder(kodashim_corpus__substitution_archive, restorationist_sacrificial_practitioners, payer,
    powerless, generational, trapped, regional).

% Groups organizing toward physical Temple access or reconstruction preparation find the substitution-archive reading used against them politically and religiously: mainstream authorities cite the completed-transfer framing to delegitimize their organizing as theologically unnecessary or provocative, even though the same authorities do not formally rule restoration impossible.
narrative_ontology:constraint_stakeholder(kodashim_corpus__substitution_archive, temple_mount_activist_communities, payer,
    moderate, biographical, constrained, regional).

% Ordinary practitioners receive a workable, portable religious life (prayer, study) that does not require Temple, priesthood, or land access — a genuine coordination benefit given exile and dispersion. They also, by the same token, are handed a settled account of sacrifice's obsolescence that forecloses inquiry into restoration as a live communal project, absorbing the substitution framing without much choice in how it is taught to them.
narrative_ontology:constraint_stakeholder(kodashim_corpus__substitution_archive, diaspora_lay_communities, beneficiary,
    moderate, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(kodashim_corpus__substitution_archive, diaspora_lay_communities, payer).

% Academic historians of religion trace how the Kodashim corpus was actively reframed from active law into memorial literature across the tannaitic and amoraic periods, documenting the institutional interests served by fixing the 'superseded, not occupied' reading as the mainstream position.
narrative_ontology:constraint_stakeholder(kodashim_corpus__substitution_archive, comparative_religion_scholars, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, portable religious practice (fixed prayer, structured Torah study) that lets communal and individual religious life continue coherently without Temple, priesthood, or sacrificial infrastructure — a genuine solution to the collective-action problem of maintaining religious continuity through exile and dispersion.
% TRANSFER_FUNCTION: Moves religious authority and communal legitimacy from a (currently unavailable) sacrificial priesthood toward text-study institutions and prayer-liturgy authorities, while transferring the cost of foreclosed restorationist aspiration onto those who still seek the possibility of resumed physical sacrifice.
% ABSENT_VOICES: Restorationist practitioners and Temple-focused activist communities are rarely given a formal hearing within mainstream institutional structures; their position is treated as settled by the substitution framing rather than argued against directly, so their objection is structurally excluded from where doctrine actually gets made.
% DISAPPEARANCE_RATIONALE: If the substitution-archive reading disappeared — if Kodashim were reclassified overnight as merely dormant law awaiting restoration rather than settled memorial — the institutional centrality of text-study academies as the terminus of the sacrificial mitzvah would weaken, restorationist movements would gain doctrinal standing they currently lack, and prayer's status would shift from replacement to placeholder, altering liturgical self-understanding across mainstream communities.
% FOUNDING_PROBLEM: After the Temple's destruction, sacrificial practice became physically impossible; the community needed a way to maintain religious continuity, communal identity, and covenantal practice without an operative Temple, priesthood, or altar.
% FOUNDING_PROBLEM_CORROBORATION: Text-study institutions and prayer authorities themselves attest the founding problem is fully and permanently solved by substitution. Restorationist practitioners and some independent historians of Jewish liturgy attest, from outside the benefiting institutions, that the classical rabbinic sources (e.g., prayers keyed explicitly to future restoration, liturgical language of 'speedily rebuild') treat the substitution as provisional rather than final — supporting a contested rather than settled status.
narrative_ontology:disappearance_verdict(kodashim_corpus__substitution_archive, world_rearranges).
narrative_ontology:founding_problem_status(kodashim_corpus__substitution_archive, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kodashim_corpus__substitution_archive, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(kodashim_corpus__substitution_archive, 'none', 1).
narrative_ontology:epsilon_provenance(kodashim_corpus__substitution_archive, 0.52, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kodashim_corpus__substitution_archive_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(kodashim_corpus__substitution_archive, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(kodashim_corpus__substitution_archive_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.52 at interval end, per the expected structural delta) rather than high, because the coordination function is real and substantial — diaspora religious continuity genuinely required something like prayer/study substitution, and this is not primarily a rent-extraction scheme. But it is not low, because the reading does more than describe a historical fact: it forecloses a live alternative (restoration) by asserting settled status, and that foreclosure disproportionately costs a specific, identifiable group. Theater ratio rises modestly over the interval (0.15 to 0.38) as the corpus's study becomes increasingly ceremonial/canonical rather than tied to any active legal question, and as institutional investment in the settled-substitution narrative deepens with time. Suppression is present but moderate (0.44) — this is not coercive suppression via force, but interpretive/institutional suppression: restorationist positions are not banned, but they are structurally denied a hearing within the bodies that set mainstream doctrine.
 *
 * DIRECTIONALITY LOGIC:
 *   Rabbinic text-study institutions and prayer-liturgy authorities are declared beneficiaries because their centrality and legitimacy are constituted by the substitution claim being true and settled — d sits low for them. Restorationist practitioners and Temple activists are declared victims because the same claim, treated as settled rather than contested, directly costs them: their aspiration is foreclosed without being formally argued against, denying them the possibility of a ruled decision they could contest on its merits. Diaspora lay communities occupy a genuinely mixed position — real beneficiaries of a workable portable practice, but also payers in the sense that the settled framing is handed to them without their having chosen it or been offered the live alternative.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — how to sustain religious life without an operative Temple — is genuinely dead in the narrow sense that no operative Temple currently exists. But whether the SUBSTITUTION is permanent (dead problem forever) or provisional (live problem, currently unsolvable) is exactly what is contested between this reading and its siblings. Tangled_rope, rather than mountain or piton, is the right classification because: (a) there IS a real, non-trivial coordination function (portable practice under diasporic conditions) — this is not pure theater; (b) there IS active enforcement in the interpretive sense — mainstream institutions actively maintain the settled-substitution framing against restorationist challenge rather than leaving the matter genuinely open; (c) there IS an identifiable victim group bearing a real cost (foreclosed aspiration, political delegitimization) through the same structure that delivers the coordination benefit. Classifying this as a mountain (as performance_only might, treating supersession as simply what happened) would hide the beneficiary structure; classifying it as a pure rope (as study_as_exercise does, from inside its own reading) would hide the foreclosure cost. Tangled_rope holds both facts open simultaneously, which is the correct diagnosis for a reading that claims continuity while denying restoration.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    provisional_vs_permanent_substitution,
    'Do the classical liturgical sources (e.g., petitions for rebuilding, restoration of sacrificial order) support the substitution_archive reading''s claim of permanent transfer, or do they preserve the transfer as explicitly provisional pending restoration — which would collapse this reading toward performance_only?',
    'Close textual analysis of the liturgy''s own self-description (does the prayer describe itself as replacing sacrifice forever, or as substituting until restoration?) cross-checked against how major halakhic authorities across eras have ruled on the question of resumed sacrifice''s permissibility absent messianic conditions.',
    'If the sources support provisional transfer, the substitution_archive reading''s core premise weakens and its extraction from restorationists becomes harder to justify as settled fact rather than institutional preference; if permanent transfer is well-supported, the tangled_rope classification''s asymmetric-extraction component is more clearly warranted rather than merely alleged.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(provisional_vs_permanent_substitution, empirical, 'Whether classical sources support permanent or provisional substitution.').

omega_variable(
    kernel_reading_selection_ambiguity,
    'Is the choice among performance_only, study_as_exercise, and substitution_archive itself a live theological dispute with real communities holding each position, or has mainstream institutional practice effectively already selected substitution_archive as the operative default regardless of formal pluralism?',
    'Survey of contemporary rabbinic teaching materials, yeshiva curricula framing language, and liturgical commentary to determine whether all three readings are genuinely taught as live options or whether substitution_archive has become the unmarked default while the others are marginalized as minority or historical positions.',
    'If substitution_archive functions as the unmarked institutional default, its suppression metric should be read as higher than a purely doctrinal analysis would suggest, since the foreclosure of sibling readings is itself part of how the constraint operates — not merely a byproduct of scholarly consensus.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection_ambiguity, conceptual, 'Whether the reading functions as one option among several or as the unmarked institutional default.').

omega_variable(
    restoration_movement_growth_trajectory,
    'Is restorationist sentiment (interest in resumed sacrificial practice, Temple Mount activism) growing, static, or declining, and does that trajectory change how costly the foreclosure is over time?',
    'Longitudinal tracking of restorationist organizational membership, publication volume, and political salience across the measured interval.',
    'A growing restorationist movement would suggest the founding_problem_status is more genuinely contested than settled, strengthening the case against treating substitution as closed; a declining or stable-marginal movement would support the mainstream institutions'' settled-fact framing.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(restoration_movement_growth_trajectory, empirical, 'Whether restorationist sentiment trend affects the weight of the foreclosure cost.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kodashim_corpus__substitution_archive, 0, 1900).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(koda_tr_t0, kodashim_corpus__substitution_archive, theater_ratio, 0, 0.15).
narrative_ontology:measurement(koda_tr_t300, kodashim_corpus__substitution_archive, theater_ratio, 300, 0.2).
narrative_ontology:measurement(koda_tr_t700, kodashim_corpus__substitution_archive, theater_ratio, 700, 0.27).
narrative_ontology:measurement(koda_tr_t1100, kodashim_corpus__substitution_archive, theater_ratio, 1100, 0.31).
narrative_ontology:measurement(koda_tr_t1500, kodashim_corpus__substitution_archive, theater_ratio, 1500, 0.35).
narrative_ontology:measurement(koda_tr_t1900, kodashim_corpus__substitution_archive, theater_ratio, 1900, 0.38).

% Extraction over time
narrative_ontology:measurement(koda_be_t0, kodashim_corpus__substitution_archive, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(koda_be_t300, kodashim_corpus__substitution_archive, base_extractiveness, 300, 0.4).
narrative_ontology:measurement(koda_be_t700, kodashim_corpus__substitution_archive, base_extractiveness, 700, 0.45).
narrative_ontology:measurement(koda_be_t1100, kodashim_corpus__substitution_archive, base_extractiveness, 1100, 0.48).
narrative_ontology:measurement(koda_be_t1500, kodashim_corpus__substitution_archive, base_extractiveness, 1500, 0.5).
narrative_ontology:measurement(koda_be_t1900, kodashim_corpus__substitution_archive, base_extractiveness, 1900, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(koda_su_t0, kodashim_corpus__substitution_archive, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(koda_su_t300, kodashim_corpus__substitution_archive, suppression_requirement, 300, 0.3).
narrative_ontology:measurement(koda_su_t700, kodashim_corpus__substitution_archive, suppression_requirement, 700, 0.35).
narrative_ontology:measurement(koda_su_t1100, kodashim_corpus__substitution_archive, suppression_requirement, 1100, 0.38).
narrative_ontology:measurement(koda_su_t1500, kodashim_corpus__substitution_archive, suppression_requirement, 1500, 0.41).
narrative_ontology:measurement(koda_su_t1900, kodashim_corpus__substitution_archive, suppression_requirement, 1900, 0.44).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kodashim_corpus__substitution_archive, identity_coordination).
narrative_ontology:boltzmann_floor_override(kodashim_corpus__substitution_archive, 0.1).
narrative_ontology:affects_constraint(kodashim_corpus__substitution_archive, kodashim_corpus__study_as_exercise).
narrative_ontology:affects_constraint(kodashim_corpus__substitution_archive, kodashim_corpus__performance_only).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the kodashim_corpus kernel: performance_only (dormant law awaiting restoration, low-extraction mountain/piton-leaning), study_as_exercise (study itself occupies the mitzvah, high-coordination rope-leaning), and substitution_archive (this file — permanent transfer claimed, tangled_rope). Each carries its own ε, beneficiary/victim structure, and classification; none averages over the others. The relationship is documented reciprocally in each sibling's commentary.kernel_context.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
