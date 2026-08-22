% ============================================================================
% CONSTRAINT STORY: kodashim_corpus__performance_only
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kodashim_corpus__performance_only, []).

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
 *   constraint_id: kodashim_corpus__performance_only
 *   human_readable: Kodashim as Suspended Blueprint Awaiting Messianic Restoration
 *   domain: religious/institutional
 *
 * SUMMARY:
 *   This story instantiates the performance_only reading of the Kodashim
 *   kernel: the corpus of sacrificial law is treated as an archived
 *   blueprint, entirely suspended pending a literal messianic restoration of
 *   physical sacrifice. Under this reading, no present activity — study,
 *   prayer, or otherwise — discharges the underlying mitzvah; the text sits
 *   inert, awaiting an unrealizable-in-the-present future state. This reading
 *   is generated as a single, ε-invariant constraint distinct from its
 *   siblings (study_as_exercise, which holds that study itself IS the
 *   performance, and substitution_archive, which holds the corpus is a closed
 *   memorial of a superseded practice). Those are separate constraints with
 *   separate ε values, linked only through the kernel and the network edges
 *   below — this file does not average across them or describe their contest
 *   internally.
 *
 * KEY AGENTS:
 *   - messianic_preparation_institutions: primary beneficiary — draws institutional legitimacy and resources from the preparation narrative regardless of restoration ever occurring
 *   - temple_reconstruction_organizations: secondary beneficiary — uses the husk framing as doctrinal ballast for a political reconstruction project
 *   - lay_students_treating_archive_as_living_practice: primary victim — devotional labor allocated toward a performance the reading declares cannot currently exist
 *   - diaspora_communities_seeking_present_atonement: secondary victim — structurally denied a present atonement mechanism this reading could in principle supply but does not
 *   - rabbinic_authorities_favoring_substitution: excluded voice — holds a competing reading not engaged by this one's internal logic
 *   - comparative_religion_scholars: analytical observer
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kodashim_corpus__performance_only, 0.71).
domain_priors:suppression_score(kodashim_corpus__performance_only, 0.48).
domain_priors:theater_ratio(kodashim_corpus__performance_only, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kodashim_corpus__performance_only, extractiveness, 0.71).
narrative_ontology:constraint_metric(kodashim_corpus__performance_only, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(kodashim_corpus__performance_only, theater_ratio, 0.62).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kodashim_corpus__performance_only, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(kodashim_corpus__performance_only, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kodashim_corpus__performance_only, snare).
narrative_ontology:human_readable(kodashim_corpus__performance_only, "Kodashim as Suspended Blueprint Awaiting Messianic Restoration").
narrative_ontology:topic_domain(kodashim_corpus__performance_only, "religious/institutional").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kodashim_corpus__performance_only, '753536c1-9af6-4cee-b6e3-eefcbd5d8b73').
narrative_ontology:cs_kernel_codification('753536c1-9af6-4cee-b6e3-eefcbd5d8b73', fixed_text).
narrative_ontology:cs_authority_grounding('753536c1-9af6-4cee-b6e3-eefcbd5d8b73', lineage).
narrative_ontology:cs_interpretation_layer_present('753536c1-9af6-4cee-b6e3-eefcbd5d8b73').
narrative_ontology:cs_reading_relation('753536c1-9af6-4cee-b6e3-eefcbd5d8b73', kodashim_corpus__study_as_exercise, coexists_with).
narrative_ontology:cs_reading_relation('753536c1-9af6-4cee-b6e3-eefcbd5d8b73', kodashim_corpus__substitution_archive, coexists_with).
narrative_ontology:cs_axiom('753536c1-9af6-4cee-b6e3-eefcbd5d8b73', foundational, sacrificial_mitzvah_requires_physical_performance).
narrative_ontology:cs_axiom_status(sacrificial_mitzvah_requires_physical_performance, holdable).
narrative_ontology:cs_axiom_grounding('753536c1-9af6-4cee-b6e3-eefcbd5d8b73', sacrificial_mitzvah_requires_physical_performance, deontological).
narrative_ontology:cs_axiom('753536c1-9af6-4cee-b6e3-eefcbd5d8b73', secondary, present_study_cannot_discharge_a_performance_mitzvah).
narrative_ontology:cs_axiom_status(present_study_cannot_discharge_a_performance_mitzvah, holdable).
narrative_ontology:cs_axiom_grounding('753536c1-9af6-4cee-b6e3-eefcbd5d8b73', present_study_cannot_discharge_a_performance_mitzvah, deontological).
narrative_ontology:cs_reference_frame('753536c1-9af6-4cee-b6e3-eefcbd5d8b73', temple_era_physical_sacrifice_as_normative_baseline).
narrative_ontology:cs_drift_state('753536c1-9af6-4cee-b6e3-eefcbd5d8b73', contemporary_diaspora_praxis, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('753536c1-9af6-4cee-b6e3-eefcbd5d8b73', '').
narrative_ontology:cs_kernel_id(kodashim_corpus__performance_only, kodashim_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kodashim_corpus__performance_only, messianic_preparation_institutions).
narrative_ontology:constraint_beneficiary(kodashim_corpus__performance_only, temple_reconstruction_organizations).
narrative_ontology:constraint_victim(kodashim_corpus__performance_only, lay_students_treating_archive_as_living_practice).
narrative_ontology:constraint_victim(kodashim_corpus__performance_only, diaspora_communities_seeking_present_atonement).
narrative_ontology:constraint_vindicates(kodashim_corpus__performance_only, future_third_temple_restoration_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Yeshivas, kollels, and temple-institute organizations that teach Kodashim as a suspended blueprint for a literal future restoration. They derive institutional legitimacy, funding, and recruiting narrative from the claim that this study prepares practitioners for an imminent, physically-resumed sacrificial order. Their standing does not depend on the restoration ever occurring — the preparation narrative itself sustains donor interest, student enrollment, and political alliances with restoration-oriented movements, regardless of whether the messianic event arrives.
narrative_ontology:constraint_stakeholder(kodashim_corpus__performance_only, messianic_preparation_institutions, beneficiary,
    organized, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(kodashim_corpus__performance_only, messianic_preparation_institutions, agenda_setter).

% Groups that fundraise and lobby toward literal Temple rebuilding, using the performance-only reading of Kodashim as doctrinal ballast: if the corpus is a husk awaiting resumed physical sacrifice, their political project is the necessary precondition for the mitzvah's fulfillment. They benefit from the archive being read as inert-but-pending rather than as already-satisfied through study or substitution.
narrative_ontology:constraint_stakeholder(kodashim_corpus__performance_only, temple_reconstruction_organizations, beneficiary,
    organized, civilizational, mobile, national).

% Individuals who devote years to detailed sacrificial law under the belief that mastering it constitutes meaningful religious performance, only to be told by this reading that their engagement is preparatory clerical rehearsal for an event outside their control and likely outside their lifetime. Their devotion is misallocated relative to what the performance-only reading says the corpus can actually deliver right now: nothing performative, only anticipatory.
narrative_ontology:constraint_stakeholder(kodashim_corpus__performance_only, lay_students_treating_archive_as_living_practice, payer,
    powerless, biographical, constrained, local).

% Communities whose religious life includes seeking atonement and closeness to the sacrificial ideal through study or liturgy. Under the performance-only reading, no present mechanism substitutes for the suspended sacrificial function — atonement remains formally unaddressed until restoration, leaving them structurally short a resource other readings (prayer-as-substitution, study-as-performance) would supply.
narrative_ontology:constraint_stakeholder(kodashim_corpus__performance_only, diaspora_communities_seeking_present_atonement, payer,
    powerless, generational, constrained, global).

% Authorities within the same tradition who hold that prayer and study already discharge the underlying religious function and that treating the corpus as a pending-performance husk devalues millennia of substitutionary practice. Their competing doctrinal claim is present within the broader kernel contest but is not part of THIS reading's own operating logic — this reading proceeds without needing to answer them.
narrative_ontology:constraint_stakeholder(kodashim_corpus__performance_only, rabbinic_authorities_favoring_substitution, excluded,
    institutional, civilizational, mobile, global).

% Academic observers analyzing how the three readings of the Kodashim kernel distribute legitimacy, institutional resources, and devotional labor differently, without themselves holding a stake in which reading prevails.
narrative_ontology:constraint_stakeholder(kodashim_corpus__performance_only, comparative_religion_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(kodashim_corpus__performance_only, messianic_preparation_institutions).
narrative_ontology:fixing_cost_class(kodashim_corpus__performance_only, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves detailed technical knowledge of sacrificial procedure across generations so that, should a restoration occur, the procedural blueprint exists intact and is not lost to historical discontinuity.
% TRANSFER_FUNCTION: Moves devotional labor, institutional funding, and doctrinal legitimacy from present practitioners and diaspora communities toward messianic-preparation and reconstruction institutions, in exchange for a promised future fulfillment that the reading itself declares cannot currently be delivered.
% ABSENT_VOICES: Rabbinic authorities who hold the substitution or study-as-performance readings would object that declaring the corpus inert-until-restoration devalues the present religious function they attribute to prayer and study; they are doctrinally present in the wider tradition but structurally absent from this reading's internal justification, which does not need to engage them to sustain itself.
% DISAPPEARANCE_RATIONALE: If the performance-only reading disappeared overnight, messianic-preparation institutions would lose their primary doctrinal warrant for treating Kodashim study as restoration-rehearsal, donor and enrollment narratives tied to imminent Temple rebuilding would need to shift ground, and lay students currently told their study is anticipatory-only could instead adopt the study-as-exercise reading and treat their engagement as already-complete performance — a substantial reallocation of legitimacy and resources.
% FOUNDING_PROBLEM: After the Temple's destruction, the sacrificial system could no longer be physically performed, and a way was needed to keep the procedural knowledge intact against the possibility of future restoration without either abandoning the corpus or falsely claiming it was being currently fulfilled.
% FOUNDING_PROBLEM_CORROBORATION: Messianic-preparation institutions themselves attest the founding problem is live (restoration is imminent and pending). Comparative religion scholars, writing from outside the benefiting institutions, note that the 'preparation' framing has persisted essentially unchanged for nearly two millennia regardless of any actual change in restoration prospects, and that this stability is more consistent with the framing sustaining institutional identity than with it tracking a genuinely live, resolvable problem.
narrative_ontology:disappearance_verdict(kodashim_corpus__performance_only, world_rearranges).
narrative_ontology:founding_problem_status(kodashim_corpus__performance_only, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kodashim_corpus__performance_only, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(kodashim_corpus__performance_only, 'none', 1).
narrative_ontology:epsilon_provenance(kodashim_corpus__performance_only, 0.71, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kodashim_corpus__performance_only_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(kodashim_corpus__performance_only, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(kodashim_corpus__performance_only_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (0.71 at interval end) because the reading extracts present legitimacy — institutional funding, devotional labor, doctrinal authority — from a performance it explicitly locates in an unrealizable future state; the gap between what is collected now and what is delivered now is the extraction. Suppression is moderate (0.48) because no active enforcement bars adoption of a rival reading (study_as_exercise and substitution_archive coexist as live alternatives within the same tradition), so exit is doctrinal rather than coercive. Theater ratio is authored as substantial and rising (0.40 to 0.62) because a growing share of the institutional activity built on this reading is anticipatory performance — teaching procedure that cannot be enacted — rather than any function that discharges in the present. Accessibility collapse is moderate-low (0.40) since sibling readings remain doctrinally available; resistance is moderate (0.55), reflecting ongoing rabbinic and lay pushback favoring substitution or exercise readings.
 *
 * DIRECTIONALITY LOGIC:
 *   Messianic-preparation and reconstruction institutions sit near the beneficiary end: they collect funding, enrollment, and legitimacy from the preparation narrative independent of restoration's occurrence, and their exit options are effectively arbitrage-grade (they can pivot narrative emphasis without losing institutional standing). Lay students and diaspora communities sit near the target end: their devotional labor and religious need are the resource being spent against a promise this reading itself declares cannot currently be honored, and their exit options are constrained by community embeddedness and limited doctrinal literacy to evaluate the kernel contest.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — preserving procedural knowledge against future restoration — was genuinely live at the point of Temple destruction. This reading's own status answer is 'contested' precisely because two millennia have passed without the triggering condition (resumed sacrifice) occurring, while the institutional apparatus built on 'imminent restoration' has not correspondingly weakened. That asymmetry — problem status uncertain, institutional benefit undiminished — is the mandatrophy signature: the mandate (preserve-for-restoration) may have outlived any evidentiary basis for treating restoration as near, while the benefiting institutions have no structural incentive to revise the framing. The classification as snare (rather than mountain or rope) turns on exactly this: a coordination story (procedural preservation) is real, but it now functions chiefly as cover for legitimacy extraction from parties who cannot verify the future performance's imminence.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    husk_versus_occupied_kernel_framing,
    'Is the Kodashim corpus genuinely an inert blueprint awaiting a future triggering event (this reading), or is it currently occupied and discharged through study (study_as_exercise) or already fully superseded and closed (substitution_archive)?',
    'No empirical resolution is possible in principle — the question is not internal to Jewish law but concerns whether a specific historical-messianic event (Temple restoration with resumed physical sacrifice) will occur, and if it does, whether that vindicates this reading retroactively. Absent that event, the three readings remain co-held positions distinguished by doctrinal commitment, not by any accessible fact.',
    'If the restoration event never occurs and communities increasingly adopt study_as_exercise or substitution_archive as functionally adequate, resources and legitimacy currently flowing to messianic-preparation institutions under this reading would migrate toward institutions organized around the sibling readings — this reading''s beneficiary set would shrink over time even without any doctrinal ''defeat.''',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(husk_versus_occupied_kernel_framing, conceptual, 'Whether the kernel is genuinely suspended-pending-restoration or already occupied/closed by rival readings — a framing question with no internal empirical adjudication.').

omega_variable(
    misallocated_devotion_measurement,
    'How would one determine, from within the tradition, whether a lay student''s devotional labor under this reading is ''misallocated'' (spent on an undeliverable performance) versus fully legitimate anticipatory piety (valid regardless of delivery timing)?',
    'This is not resolvable by external empirical test; it depends on which theology of anticipatory versus enacted mitzvah-performance one holds. A partial empirical proxy: surveying whether practitioners under this reading report the same subjective sense of religious fulfillment as those under study_as_exercise, which would suggest the ''undeliverable performance'' framing is not experientially load-bearing for many adherents.',
    'If practitioners report equivalent fulfillment regardless of reading, the ''victim'' framing (misallocated devotion) in this story''s base_properties may overstate the cost borne by lay students; if fulfillment differs sharply and negatively under this reading, it corroborates the victim declaration.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(misallocated_devotion_measurement, preference, 'Whether calling lay devotion under this reading ''misallocated'' is itself a contestable value judgment rather than a structural fact.').

omega_variable(
    institutional_incentive_versus_sincere_belief,
    'Do messianic-preparation institutions promote the performance_only reading primarily because they sincerely hold it to be true, or because it is the reading most structurally favorable to their continued institutional funding and relevance?',
    'Comparative analysis of institutional behavior under counterfactual conditions — e.g., whether such institutions have historically shifted emphasis toward or away from imminent-restoration rhetoric in response to funding cycles, political opportunity, or external pressure, independent of any theological development.',
    'If institutional emphasis tracks funding and political opportunity more than doctrinal development, this strengthens the snare classification (legitimacy extraction is the operative mechanism); if emphasis tracks internal theological reasoning independent of material incentive, the coordination function (preservation of knowledge) is more load-bearing than the extraction framing suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_incentive_versus_sincere_belief, empirical, 'Whether institutional advocacy for this reading is incentive-driven or belief-driven — bears directly on how much of the measured extractiveness is genuine versus opportunistic.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kodashim_corpus__performance_only, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(koda_tr_t0, kodashim_corpus__performance_only, theater_ratio, 0, 0.4).
narrative_ontology:measurement(koda_tr_t20, kodashim_corpus__performance_only, theater_ratio, 20, 0.45).
narrative_ontology:measurement(koda_tr_t40, kodashim_corpus__performance_only, theater_ratio, 40, 0.5).
narrative_ontology:measurement(koda_tr_t60, kodashim_corpus__performance_only, theater_ratio, 60, 0.54).
narrative_ontology:measurement(koda_tr_t80, kodashim_corpus__performance_only, theater_ratio, 80, 0.58).
narrative_ontology:measurement(koda_tr_t100, kodashim_corpus__performance_only, theater_ratio, 100, 0.62).

% Extraction over time
narrative_ontology:measurement(koda_be_t0, kodashim_corpus__performance_only, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(koda_be_t20, kodashim_corpus__performance_only, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(koda_be_t40, kodashim_corpus__performance_only, base_extractiveness, 40, 0.62).
narrative_ontology:measurement(koda_be_t60, kodashim_corpus__performance_only, base_extractiveness, 60, 0.65).
narrative_ontology:measurement(koda_be_t80, kodashim_corpus__performance_only, base_extractiveness, 80, 0.68).
narrative_ontology:measurement(koda_be_t100, kodashim_corpus__performance_only, base_extractiveness, 100, 0.71).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(kodashim_corpus__performance_only, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kodashim_corpus__performance_only, identity_coordination).
narrative_ontology:affects_constraint(kodashim_corpus__performance_only, kodashim_corpus__study_as_exercise).
narrative_ontology:affects_constraint(kodashim_corpus__performance_only, kodashim_corpus__substitution_archive).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the kodashim_corpus kernel, decomposed per the ε-invariance principle: performance_only (this file, high ε, snare — legitimacy drawn from an undeliverable future performance), study_as_exercise (expected low-to-moderate ε, rope-leaning — study itself constitutes the performance, so no unrealizable-future dependency exists), and substitution_archive (expected low ε, mountain/rope-leaning — the corpus is a closed memorial with nothing further owed). All three share the same fixed text and lineage-based authority structure but diverge sharply on where legitimacy is located, producing three distinct ε values and three distinct beneficiary/victim structures. They are linked here rather than merged into one story precisely because merging would violate the ε-invariance principle: the corpus is one kernel, not one constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
