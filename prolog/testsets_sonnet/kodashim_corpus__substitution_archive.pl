% ============================================================================
% CONSTRAINT STORY: kodashim_corpus__substitution_archive
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
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
 *   constraint_id: kodashim_corpus__substitution_archive
 *   human_readable: Kodashim as Memorial Archive of Superseded Sacrificial Practice
 *   domain: religious/legal/institutional
 *
 * SUMMARY:
 *   Following the Temple's destruction in 70 CE, rabbinic Judaism developed
 *   multiple theological accounts of what happened to the sacrificial
 *   obligation. The substitution_archive reading — associated with strands of
 *   thought that read Hosea 14:3 ('we will render for bullocks the offering
 *   of our lips') and rabbinic statements equating prayer with sacrifice as
 *   literal functional replacement rather than temporary accommodation —
 *   holds that Kodashim documents a superseded system. This framing
 *   coordinates real continuity of religious practice across two millennia
 *   without a Temple, but it also does structural work: it forecloses
 *   restorationist claims by declaring the matter settled, and it
 *   consolidates authority in the textual and liturgical institutions that
 *   administer prayer and study rather than in a hypothetical restored
 *   priesthood.
 *
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
narrative_ontology:constraint_metric(kodashim_corpus__substitution_archive, resistance, 0.47).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kodashim_corpus__substitution_archive, tangled_rope).
narrative_ontology:human_readable(kodashim_corpus__substitution_archive, "Kodashim as Memorial Archive of Superseded Sacrificial Practice").
narrative_ontology:topic_domain(kodashim_corpus__substitution_archive, "religious/legal/institutional").

domain_priors:requires_active_enforcement(kodashim_corpus__substitution_archive).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kodashim_corpus__substitution_archive, 'dff582ae-956d-4842-bc72-47f4f528fbbf').
narrative_ontology:cs_kernel_codification('dff582ae-956d-4842-bc72-47f4f528fbbf', fixed_text).
narrative_ontology:cs_authority_grounding('dff582ae-956d-4842-bc72-47f4f528fbbf', lineage).
narrative_ontology:cs_interpretation_layer_present('dff582ae-956d-4842-bc72-47f4f528fbbf').
narrative_ontology:cs_reading_relation('dff582ae-956d-4842-bc72-47f4f528fbbf', kodashim_corpus__performance_only, coexists_with).
narrative_ontology:cs_reading_relation('dff582ae-956d-4842-bc72-47f4f528fbbf', kodashim_corpus__study_as_exercise, influences).
narrative_ontology:cs_axiom('dff582ae-956d-4842-bc72-47f4f528fbbf', foundational, prayer_functionally_replaces_sacrifice).
narrative_ontology:cs_axiom_status(prayer_functionally_replaces_sacrifice, holdable).
narrative_ontology:cs_axiom_grounding('dff582ae-956d-4842-bc72-47f4f528fbbf', prayer_functionally_replaces_sacrifice, conventional).
narrative_ontology:cs_axiom('dff582ae-956d-4842-bc72-47f4f528fbbf', foundational, sacrificial_obligation_discharged_not_suspended).
narrative_ontology:cs_axiom_status(sacrificial_obligation_discharged_not_suspended, holdable).
narrative_ontology:cs_axiom_grounding('dff582ae-956d-4842-bc72-47f4f528fbbf', sacrificial_obligation_discharged_not_suspended, deontological).
narrative_ontology:cs_reference_frame('dff582ae-956d-4842-bc72-47f4f528fbbf', second_temple_sacrificial_cult).
narrative_ontology:cs_drift_state('dff582ae-956d-4842-bc72-47f4f528fbbf', post_talmudic_diaspora_consolidation, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('dff582ae-956d-4842-bc72-47f4f528fbbf', '').
narrative_ontology:cs_kernel_id(kodashim_corpus__substitution_archive, kodashim_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kodashim_corpus__substitution_archive, rabbinic_text_study_institutions).
narrative_ontology:constraint_beneficiary(kodashim_corpus__substitution_archive, prayer_liturgy_establishment).
narrative_ontology:constraint_victim(kodashim_corpus__substitution_archive, restorationist_practitioners).
narrative_ontology:constraint_victim(kodashim_corpus__substitution_archive, temple_reconstruction_movements).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(kodashim_corpus__substitution_archive, diaspora_communities).
narrative_ontology:constraint_victim(kodashim_corpus__substitution_archive, diaspora_communities).
narrative_ontology:constraint_vindicates(kodashim_corpus__substitution_archive, prayer_as_service_of_the_heart).
narrative_ontology:constraint_vindicates(kodashim_corpus__substitution_archive, torah_study_equivalent_to_sacrifice).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Yeshivot, batei midrash, and the rabbinic curriculum treat sustained textual study of Kodashim as the primary and sufficient continuation of the sacrificial order. They administer the canon, train the interpreters, and certify what counts as faithful transmission. Their institutional authority, funding, and prestige structures depend on study being accepted as the substitute rather than a placeholder awaiting restoration.
narrative_ontology:constraint_stakeholder(kodashim_corpus__substitution_archive, rabbinic_text_study_institutions, beneficiary,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(kodashim_corpus__substitution_archive, rabbinic_text_study_institutions, agenda_setter).

% The synagogue liturgical system (fixed prayer times mapped onto the sacrificial schedule, the Amidah as verbal korban) draws its legitimacy from the claim that prayer already accomplished the substitution. Cantors, prayer leaders, and liturgical scholars have built continuous institutional life on this reading; a restoration reading would relativize their centrality.
narrative_ontology:constraint_stakeholder(kodashim_corpus__substitution_archive, prayer_liturgy_establishment, beneficiary,
    institutional, civilizational, arbitrage, global).

% Individuals and small movements who hold that the sacrificial service remains a live, restorable obligation are told by the mainstream institutional consensus that their aspiration is either premature (performance_only) or already fulfilled in essence by study (this reading). Under the substitution_archive frame specifically, they bear the cost of being told the matter is closed, not merely deferred — their practice is treated as a category error rather than a legitimate minority position awaiting circumstance. They cannot easily exit the interpretive community without losing standing within it.
narrative_ontology:constraint_stakeholder(kodashim_corpus__substitution_archive, restorationist_practitioners, payer,
    moderate, generational, constrained, national).

% Organizations preparing implements, priestly genealogies, and ritual training for an eventual rebuilt Temple face active institutional dismissal under this reading: their preparatory labor is framed as answering a question that has already been answered by history (superseded), not one still open. They have few resources and no comparable institutional platform to contest the dominant textual establishment's framing.
narrative_ontology:constraint_stakeholder(kodashim_corpus__substitution_archive, temple_reconstruction_movements, payer,
    powerless, biographical, trapped, national).

% Ordinary observant communities receive a coherent, portable religious practice that requires no Temple, no priesthood, and no land-based cult — prayer and study travel wherever Jews live. They also inherit, without much choice, the theological closure that comes with it: the sense that the sacrificial system is a finished chapter rather than a suspended one, which shapes how the next generation is taught to relate to Kodashim.
narrative_ontology:constraint_stakeholder(kodashim_corpus__substitution_archive, diaspora_communities, beneficiary,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(kodashim_corpus__substitution_archive, diaspora_communities, payer).

% Adherents of the performance_only reading (archive awaiting restoration) and the study_as_exercise reading (study itself occupies the kernel) hold live alternative positions within the same textual tradition but are not the dominant institutional voice in most contemporary liturgical and educational settings; their framings are acknowledged in commentary but rarely structure practice.
narrative_ontology:constraint_stakeholder(kodashim_corpus__substitution_archive, sibling_reading_communities, excluded,
    moderate, generational, constrained, global).

% Study how substitution theology functions across traditions (post-Temple Judaism, post-sacrificial Christianity's typological readings) and can compare the substitution_archive claim against the historical record of what actually changed in practice and institutional power after 70 CE.
narrative_ontology:constraint_stakeholder(kodashim_corpus__substitution_archive, comparative_religion_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(kodashim_corpus__substitution_archive, rabbinic_text_study_institutions).
narrative_ontology:fixing_cost_class(kodashim_corpus__substitution_archive, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides religious continuity after the Temple's destruction: a portable, text-and-prayer-based practice that preserves communal identity, ethical formation, and a shared symbolic vocabulary without requiring a functioning Temple, priesthood, or sacrificial economy.
% TRANSFER_FUNCTION: Moves religious authority and legitimacy from the priestly/sacrificial apparatus to the rabbinic textual and liturgical establishment; moves the burden of proof onto anyone who wants to treat the sacrificial system as still operative or restorable.
% ABSENT_VOICES: Restorationist practitioners and Temple reconstruction movements hold views on the ongoing validity of sacrificial obligation but are structurally marginal to the institutions that produce mainstream liturgy, curriculum, and theological consensus; their objections surface in minority polemical literature, not in the dominant prayerbook or yeshiva curriculum.
% DISAPPEARANCE_RATIONALE: If the substitution_archive framing vanished overnight, the prayer liturgy and study curriculum would not disappear — they would persist as practice — but their theological self-justification would destabilize: prayer would need a different warrant than 'this replaced sacrifice,' and Kodashim study would need a different warrant than 'this documents what is over.' Rabbinic institutions dispute how much would actually change versus how much is settled bedrock; restorationist communities would say the field reopens.
% FOUNDING_PROBLEM: After 70 CE the Temple was destroyed and the sacrificial cult became physically impossible; the tradition needed a framework that let religious life continue without collapsing into either despair or open-ended messianic suspension.
% FOUNDING_PROBLEM_CORROBORATION: Rabbinic institutions and the liturgical establishment attest the substitution is complete and the founding problem (how to continue without a Temple) is solved by prayer and study. Restorationist practitioners, Temple reconstruction movements, and some independent historians of religion attest the founding problem (loss of sacrificial access) is not resolved but merely suspended by circumstance, and that the substitution_archive reading performs closure it has no independent warrant to claim — this dissent comes from outside the benefiting rabbinic-institutional seat.
narrative_ontology:disappearance_verdict(kodashim_corpus__substitution_archive, contested).
narrative_ontology:founding_problem_status(kodashim_corpus__substitution_archive, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kodashim_corpus__substitution_archive, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
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
 *   Extractiveness is moderate (0.52) because the coordination function is real — prayer and study genuinely sustained communal religious life for two millennia — but the substitution claim also transfers authority and legitimacy away from restorationist voices without engaging their claims on the merits, simply declaring the question closed. Suppression is moderate (0.44): there is no coercive apparatus, but there is a strong soft-suppression through curriculum, communal norms, and the absence of institutional platforms for restorationist positions. Theater ratio rises over the interval (0.15 to 0.38) as study of Kodashim becomes increasingly ceremonial-devotional (recited on fixed calendrical cycles, e.g., daily/annual Mishnah study cycles) rather than tied to any live practical question — a drift toward performance of the archive itself rather than functional engagement with its content.
 *
 * PERSPECTIVAL GAP:
 *   From the rabbinic-institutional seat, this is a Rope: pure coordination that solved an existential crisis for the tradition. From the restorationist seat, the same structure is closer to a Snare wearing coordination's clothing: it claims to have resolved a question it has actually just declared won. The tangled_rope classification holds both readings as true of different seats simultaneously — the engine computes this divergence from the stakeholder structure rather than requiring a single verdict.
 *
 * DIRECTIONALITY LOGIC:
 *   Rabbinic text-study institutions and the prayer-liturgy establishment are structural beneficiaries: the substitution reading is the theological foundation of their entire institutional existence and they administer the interpretive apparatus that maintains it. Restorationist practitioners and Temple reconstruction movements are targets: the reading directly delegitimizes their central claim, denying them not just resources but interpretive standing. Diaspora communities are near-symmetric beneficiary/payers — they gain a workable, portable practice but inherit a foreclosed theological horizon without much individual choice in the matter.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (how to sustain religious life without an operative Temple) is contested as live/dead: mainstream institutions treat it as dead (fully solved by substitution) while restorationist movements treat it as merely suspended, meaning the substitution_archive claim itself is exactly the kind of totalizing closure move that would obscure an unresolved mandatrophy if it were mistaken. Distinguishing coordination from extraction here requires taking seriously that the SAME textual and liturgical apparatus that keeps communities functioning is also the apparatus that forecloses a live minority theological position — the classification does not let either fact erase the other.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    substitution_vs_suspension_ambiguity,
    'Did prayer and study genuinely functionally replace sacrifice (this reading), or did they only provisionally fill a gap pending restoration (performance_only), or is the study itself the ongoing performance of the sacrificial mitzvah (study_as_exercise)?',
    'There is no empirical resolution available within the tradition itself since it turns on theological interpretation of tannaitic and amoraic statements (e.g., the Hosea 14:3 prayer-as-bullocks tradition, Rav''s statement that Torah study of sacrificial laws is ''as if'' the offering were brought) whose intended force (literal replacement vs. interim accommodation vs. equivalence-through-study) is contested among the traditions'' own authoritative voices.',
    'If the sibling performance_only or study_as_exercise reading is correct, then this reading''s institutional foreclosure of restorationist claims lacks warrant, raising the effective extractiveness score; if this reading is correct, restorationist objections are a category error and the current classification''s moderate extractiveness may overstate the victim harm.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(substitution_vs_suspension_ambiguity, conceptual, 'Whether substitution is genuine, provisional, or performative — the kernel''s central interpretive fork.').

omega_variable(
    who_corroborates_closure_from_outside,
    'Is there any voice outside the rabbinic-institutional and liturgical-establishment beneficiary seats that independently corroborates that the founding problem is fully and correctly resolved by substitution, rather than merely institutionally convenient to declare resolved?',
    'Survey historical and comparative-religion scholarship (outside confessional rabbinic sources) on whether functional replacement claims of this kind typically track genuine theological resolution or institutional consolidation after the loss of a central cultic site.',
    'If independent scholarship finds the pattern typically tracks institutional consolidation rather than settled theological resolution, this strengthens the tangled_rope classification and the victim status of restorationist communities; if independent corroboration exists, extractiveness should be revised downward.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(who_corroborates_closure_from_outside, empirical, 'Whether outside corroboration exists for the claim that the substitution is complete rather than institutionally convenient.').

omega_variable(
    cs_framing_kernel_vs_legitimacy_claim,
    'Should the CS analysis treat the Kodashim corpus itself as the kernel (the obvious framing), or should it treat the substitution theology''s claim of functional replacement as a second-order legitimacy layer ABOVE the corpus, which is the actual object under contest between the three readings?',
    'Compare classification outcomes under both framings: kernel-as-corpus treats all three readings as interpretive disputes over one fixed text; kernel-as-legitimacy-claim treats the substitution/performance/study-exercise readings as competing claims about what kind of authority the corpus retains, which more directly explains why the readings produce different institutional beneficiary structures.',
    'Adopting kernel-as-legitimacy-claim would sharpen the tangled_rope diagnosis (the extraction is in the legitimacy claim, not the text itself) but requires re-deriving axioms and reading_relations around continuity-claims rather than textual content; the current file uses kernel-as-corpus per the supplied kernel_id and treats the legitimacy question through axioms instead.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cs_framing_kernel_vs_legitimacy_claim, conceptual, 'Alternative framing of what the kernel actually is: the text, or the continuity claim layered above it.').


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
narrative_ontology:measurement(koda_be_t0, kodashim_corpus__substitution_archive, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(koda_be_t300, kodashim_corpus__substitution_archive, base_extractiveness, 300, 0.38).
narrative_ontology:measurement(koda_be_t700, kodashim_corpus__substitution_archive, base_extractiveness, 700, 0.44).
narrative_ontology:measurement(koda_be_t1100, kodashim_corpus__substitution_archive, base_extractiveness, 1100, 0.48).
narrative_ontology:measurement(koda_be_t1500, kodashim_corpus__substitution_archive, base_extractiveness, 1500, 0.5).
narrative_ontology:measurement(koda_be_t1900, kodashim_corpus__substitution_archive, base_extractiveness, 1900, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(koda_su_t0, kodashim_corpus__substitution_archive, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(koda_su_t300, kodashim_corpus__substitution_archive, suppression_requirement, 300, 0.38).
narrative_ontology:measurement(koda_su_t700, kodashim_corpus__substitution_archive, suppression_requirement, 700, 0.4).
narrative_ontology:measurement(koda_su_t1100, kodashim_corpus__substitution_archive, suppression_requirement, 1100, 0.41).
narrative_ontology:measurement(koda_su_t1500, kodashim_corpus__substitution_archive, suppression_requirement, 1500, 0.43).
narrative_ontology:measurement(koda_su_t1900, kodashim_corpus__substitution_archive, suppression_requirement, 1900, 0.44).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kodashim_corpus__substitution_archive, identity_coordination).
narrative_ontology:boltzmann_floor_override(kodashim_corpus__substitution_archive, 0.1).
narrative_ontology:affects_constraint(kodashim_corpus__substitution_archive, kodashim_corpus__performance_only).
narrative_ontology:affects_constraint(kodashim_corpus__substitution_archive, kodashim_corpus__study_as_exercise).

% DUAL FORMULATION NOTE:
% This story is one of three readings of the kodashim_corpus kernel. performance_only treats the corpus as a dormant blueprint awaiting literal messianic restoration (low extraction, closer to scaffold-with-indefinite-sunset). study_as_exercise treats continuous study itself as occupying and fulfilling the kernel (closer to rope — genuine ongoing coordination with minimal extraction). This reading, substitution_archive, treats the underlying obligation as functionally discharged by prayer and study, which is the reading that most directly forecloses restorationist claims and therefore carries the highest measured extractiveness and victim structure of the three. All three share the same underlying textual kernel (Mishnah/Talmud Seder Kodashim) but instantiate structurally distinct constraints with distinct beneficiary/victim profiles.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
