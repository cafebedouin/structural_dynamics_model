% ============================================================================
% CONSTRAINT STORY: territorial_sovereignty_legitimacy__self_determination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_territorial_sovereignty_legitimacy__self_determination_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: territorial_sovereignty_legitimacy__self_determination_reading
 *   human_readable: Territorial Sovereignty Legitimacy — Self-Determination Reading (Modern Demographic-Majority Claim)
 *   domain: political_theory/international_relations/territorial_sovereignty
 *
 * SUMMARY:
 *   The self-determination reading treats sovereign legitimacy as flowing
 *   from a population's demonstrated historical majority and unbroken
 *   residence during the period in which modern territorial nation-states
 *   were formed and internationally recognized (roughly Ottoman decline
 *   through the Mandate and into 1948). Under this reading, the 1947 UN
 *   Partition Plan and subsequent 1948 war are read as an imposition by
 *   external powers (British Mandate authorities, the UN General Assembly,
 *   and the newly declared Israeli state) upon a population that held
 *   demographic and residential primacy, and the resulting statelessness and
 *   displacement of that population constitute an unremedied injustice that
 *   persists as long as return and sovereign restoration remain unrealized.
 *   The claim functions simultaneously as a genuine coordination mechanism —
 *   organizing a dispersed, stateless population around a shared
 *   legal-political vocabulary that can be pressed in international forums —
 *   and as a structure whose non-resolution imposes real, compounding costs
 *   on refugee and occupied populations across generations, while political
 *   and diplomatic actors who invoke the claim (national movement leadership,
 *   allied states) bear comparatively little of that direct cost.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(territorial_sovereignty_legitimacy__self_determination_reading, 0.58).
domain_priors:suppression_score(territorial_sovereignty_legitimacy__self_determination_reading, 0.71).
domain_priors:theater_ratio(territorial_sovereignty_legitimacy__self_determination_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__self_determination_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__self_determination_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__self_determination_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__self_determination_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__self_determination_reading, resistance, 0.82).

% --- Constraint claim ---
narrative_ontology:constraint_claim(territorial_sovereignty_legitimacy__self_determination_reading, tangled_rope).
narrative_ontology:human_readable(territorial_sovereignty_legitimacy__self_determination_reading, "Territorial Sovereignty Legitimacy — Self-Determination Reading (Modern Demographic-Majority Claim)").
narrative_ontology:topic_domain(territorial_sovereignty_legitimacy__self_determination_reading, "political_theory/international_relations/territorial_sovereignty").

domain_priors:requires_active_enforcement(territorial_sovereignty_legitimacy__self_determination_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(territorial_sovereignty_legitimacy__self_determination_reading, 'e2e2a284-826a-456d-b2a6-b55bf5d1a002').
narrative_ontology:cs_kernel_codification('e2e2a284-826a-456d-b2a6-b55bf5d1a002', distributed).
narrative_ontology:cs_authority_grounding('e2e2a284-826a-456d-b2a6-b55bf5d1a002', distributed).
narrative_ontology:cs_reading_relation('e2e2a284-826a-456d-b2a6-b55bf5d1a002', territorial_sovereignty_legitimacy__covenant_continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('e2e2a284-826a-456d-b2a6-b55bf5d1a002', territorial_sovereignty_legitimacy__existential_matrix_reading, influences).
narrative_ontology:cs_axiom('e2e2a284-826a-456d-b2a6-b55bf5d1a002', foundational, modern_self_determination_as_sole_legitimacy_ground).
narrative_ontology:cs_axiom_status(modern_self_determination_as_sole_legitimacy_ground, holdable).
narrative_ontology:cs_axiom_grounding('e2e2a284-826a-456d-b2a6-b55bf5d1a002', modern_self_determination_as_sole_legitimacy_ground, deontological).
narrative_ontology:cs_axiom('e2e2a284-826a-456d-b2a6-b55bf5d1a002', secondary, partition_as_unjust_external_imposition).
narrative_ontology:cs_axiom_status(partition_as_unjust_external_imposition, holdable).
narrative_ontology:cs_axiom_grounding('e2e2a284-826a-456d-b2a6-b55bf5d1a002', partition_as_unjust_external_imposition, conventional).
narrative_ontology:cs_reference_frame('e2e2a284-826a-456d-b2a6-b55bf5d1a002', pre_partition_demographic_majority_status).
narrative_ontology:cs_drift_state('e2e2a284-826a-456d-b2a6-b55bf5d1a002', post_oslo_stalemate_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('e2e2a284-826a-456d-b2a6-b55bf5d1a002', '').
narrative_ontology:cs_kernel_id(territorial_sovereignty_legitimacy__self_determination_reading, territorial_sovereignty_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(territorial_sovereignty_legitimacy__self_determination_reading, palestinian_national_movement).
narrative_ontology:constraint_beneficiary(territorial_sovereignty_legitimacy__self_determination_reading, arab_majority_residents_pre_1948).
narrative_ontology:constraint_beneficiary(territorial_sovereignty_legitimacy__self_determination_reading, arab_states_supporting_return_claims).
narrative_ontology:constraint_victim(territorial_sovereignty_legitimacy__self_determination_reading, palestinian_refugees_and_descendants).
narrative_ontology:constraint_victim(territorial_sovereignty_legitimacy__self_determination_reading, arab_population_in_israeli_administered_territory).
narrative_ontology:constraint_victim(territorial_sovereignty_legitimacy__self_determination_reading, palestinian_communities_displaced_1948_1967).
narrative_ontology:constraint_vindicates(territorial_sovereignty_legitimacy__self_determination_reading, self_determination_doctrine).
narrative_ontology:constraint_vindicates(territorial_sovereignty_legitimacy__self_determination_reading, demographic_continuity_as_sovereignty_ground).
narrative_ontology:constraint_vindicates(territorial_sovereignty_legitimacy__self_determination_reading, partition_as_external_imposition_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Advances the self-determination claim in international forums, diplomatic negotiations, and legal venues (ICJ advisory opinions, UN resolutions), asserting that continuous demographic majority and residence through the modern period ground sovereign title. Sets the political and legal agenda for what counts as a legitimate resolution, but lacks a sovereign state apparatus to enforce the claim directly and depends on international recognition and leverage from allied states.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__self_determination_reading, palestinian_national_movement, agenda_setter,
    organized, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(territorial_sovereignty_legitimacy__self_determination_reading, palestinian_national_movement, beneficiary).

% A historical population category (not a present-day actor) whose demographic majority and continuous residence during the late Ottoman and Mandate periods is the evidentiary anchor of the self-determination claim. Cited as the foundation of legitimacy but cannot itself act; the claim on their behalf is carried forward by descendants and political representatives.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__self_determination_reading, arab_majority_residents_pre_1948, beneficiary,
    powerless, civilizational, trapped, regional).
narrative_ontology:stakeholder_non_agent(territorial_sovereignty_legitimacy__self_determination_reading, arab_majority_residents_pre_1948).

% Provide diplomatic, financial, and rhetorical support for the self-determination and right-of-return framing, using it to advance regional political positions and domestic legitimacy. Bear little direct cost from the claim's persistence and retain full exit — they can recalibrate support levels as their own strategic interests shift (e.g., normalization agreements).
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__self_determination_reading, arab_states_supporting_return_claims, beneficiary,
    institutional, generational, mobile, regional).

% Live in refugee camps or diaspora communities across the region and beyond, generation after generation, with legal statelessness or precarious residency in host countries. The unresolved sovereignty question and the right-of-return doctrine keep their status juridically frozen — they bear the accumulated cost of a claim that has not converted into restored territory or full citizenship anywhere, decade after decade.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__self_determination_reading, palestinian_refugees_and_descendants, payer,
    powerless, generational, trapped, regional).

% Live under conditions shaped directly by the unresolved sovereignty contest — checkpoints, permit regimes, restricted movement, contested land and building rights. The self-determination claim frames their situation as awaiting historical vindication, but in the interim they bear daily administrative and physical costs of the contest with no individual exit from the territory or the dispute.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__self_determination_reading, arab_population_in_israeli_administered_territory, payer,
    powerless, biographical, trapped, local).

% Lost property, land, and residence during 1948 and 1967 and have never received restitution or resettlement matching the pre-displacement status quo. The self-determination reading treats their claim as the moral core of the dispute, which keeps the claim politically alive but has not yet produced material remedy across three-plus generations.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__self_determination_reading, palestinian_communities_displaced_1948_1967, payer,
    powerless, civilizational, trapped, regional).

% Rejects the framing of its founding as a colonial project displacing a rightful sovereign, and is treated within this reading primarily as the entity whose legitimacy the self-determination claim contests rather than as a party whose own historical and legal arguments are adjudicated on equal footing. Its own covenant-continuity and existential-security arguments are structurally external to this reading — they belong to sibling readings of the same kernel.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__self_determination_reading, israeli_state, excluded,
    institutional, generational, constrained, regional).

% UN bodies, the ICJ, and international law scholarship evaluate self-determination claims against evolving doctrine (uti possidetis, remedial secession, demographic continuity tests). They produce advisory opinions and resolutions that shape but do not enforce outcomes, and their own doctrinal frameworks are contested inputs rather than neutral adjudication.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__self_determination_reading, international_legal_bodies, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a legal and political vocabulary — self-determination, demographic continuity, anti-colonial restitution — through which a dispersed and stateless population can coordinate a unified claim to sovereignty and press it through international institutions rather than only through force.
% TRANSFER_FUNCTION: The claim itself does not directly transfer material resources; rather it allocates moral and legal legitimacy, diplomatic recognition, and negotiating leverage toward the Palestinian national movement and away from the Israeli state's competing legitimacy claims, while its non-resolution transfers accumulated displacement costs onto refugee and occupied populations across generations.
% ABSENT_VOICES: Israeli historical and security arguments are structurally bracketed within this reading rather than weighed against it (they belong to the covenant_continuity_reading and existential_matrix_reading, generated as separate constraints). Also largely absent: intra-Palestinian dissent about strategy (return-maximalist vs. negotiated-compromise factions), and the voices of Jewish populations displaced from Arab and Muslim-majority countries in the same period, whose parallel displacement claims are not addressed by this reading's framework.
% DISAPPEARANCE_RATIONALE: If the self-determination framing vanished overnight, the underlying territorial dispute and the displaced populations would remain; what would disappear is the specific legal-moral vocabulary organizing Palestinian claims. Some analysts argue the conflict would revert to a raw power contest without this framework's diplomatic leverage function (world_rearranges for the movement's negotiating position); others argue the material facts on the ground — refugee status, occupation, displacement — persist independent of which legitimacy vocabulary is used to describe them (world_unchanged for the affected populations' daily conditions). The verdict genuinely depends on which population's situation is being asked about.
% FOUNDING_PROBLEM: The dissolution of Ottoman authority and the imposition of the British Mandate, followed by 1947-49 partition and war, produced a population with demonstrable historical majority presence and residence that found itself without sovereign statehood, and whose displacement was not remedied by the international order that authorized the partition.
% FOUNDING_PROBLEM_CORROBORATION: UN bodies (UNRWA's continued operation, periodic UNGA and UNSC resolutions reaffirming unresolved refugee status) and independent international law scholars outside the Palestinian national movement corroborate that the underlying statelessness and displacement problem remains factually unresolved; this corroboration addresses only the empirical persistence of statelessness, not the contested normative question of which sovereignty framework should resolve it — the covenant_continuity_reading and existential_matrix_reading dispute the resolution while not disputing the fact of continued Palestinian statelessness.
narrative_ontology:disappearance_verdict(territorial_sovereignty_legitimacy__self_determination_reading, contested).
narrative_ontology:founding_problem_status(territorial_sovereignty_legitimacy__self_determination_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(territorial_sovereignty_legitimacy__self_determination_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(territorial_sovereignty_legitimacy__self_determination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(territorial_sovereignty_legitimacy__self_determination_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(territorial_sovereignty_legitimacy__self_determination_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(territorial_sovereignty_legitimacy__self_determination_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(territorial_sovereignty_legitimacy__self_determination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at a substantial-but-not-extreme 0.58: the claim's persistence without resolution imposes real costs (statelessness, blocked return, restricted movement) on refugee and occupied populations, but the claim itself also performs genuine coordination work — it is not pure extraction dressed as coordination, it is coordination whose non-resolution has become extractive for those who bear the interim costs. Suppression is authored higher (0.71) reflecting the active machinery — permit regimes, movement restriction, unresolved refugee legal status — required to hold the current territorial arrangement in place against the claim, which the self-determination reading treats as suppression of a rightful sovereign majority. Theater ratio (0.28) reflects that a meaningful share of diplomatic and institutional activity around the claim (UN resolutions, advisory opinions, periodic negotiation rounds) has become performative relative to material resolution, particularly visible in the 1993-2024 measurements where theater rises even as underlying extraction plateaus — process without settlement. Resistance is high (0.82): the claim is actively and continuously contested by the Israeli state and by the covenant-continuity and existential-matrix readings of the same kernel, and is not passively accepted. Accessibility collapse is moderate (0.45): alternative framings (negotiated two-state compromise, one-state binational proposals, permanent-status-quo arrangements) remain live and have not fully collapsed into a single accepted resolution, unlike a settled natural-law-type constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   The Palestinian national movement and allied Arab states sit toward the beneficiary end: they derive diplomatic leverage, moral standing, and political coordination value from the claim's articulation and persistence, with mobile or organized exit options that let them adjust engagement without bearing the claim's material costs directly. Palestinian refugees, their descendants, and populations under continued administrative restriction sit at the target end: trapped exit options, civilizational or generational time horizons, and no individual capacity to resolve their own status independent of the larger unresolved sovereignty question — the claim's non-resolution is a cost they carry bodily and administratively every day, regardless of its diplomatic value at the leadership level. This is the seat divergence the engine should surface: the same constraint computes very differently from the agenda-setting/beneficiary seats than from the trapped-payer seats.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (statelessness and unremedied displacement following partition and 1948) remains empirically live per corroboration from international bodies operating outside the Palestinian national movement itself, so this does not present as a classic mandatrophy case (dead problem, persisting mandate). What the tangled_rope classification captures instead is that the SAME structure that genuinely coordinates a stateless population's claim also now imposes accumulating extraction on that population's most vulnerable members the longer it goes unresolved — coordination and extraction riding the same rails, not one masquerading as the other. Classifying this as pure snare would erase the genuine coordination function and historical grievance; classifying it as pure rope would erase the real, compounding costs borne disproportionately by refugees and occupied populations relative to leadership and allied-state seats.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_selection_omega,
    'Is the self-determination reading the uniquely correct framework for adjudicating this sovereignty dispute, or is it one of (at least) three structurally incompatible readings of the same underlying kernel — the others being covenant-continuity (ancient promise plus modern recognition) and existential-matrix (survival-based, non-juridical) — each of which a different party holds as primary?',
    'No empirical resolution mechanism exists for this omega: the choice among readings is a framing/normative commitment, not a fact discoverable by evidence. It could be documented (not resolved) by comparative analysis of which international legal doctrines, religious-historical arguments, and security-based arguments each reading treats as dispositive versus irrelevant.',
    'If a party or institution adopts a different reading (covenant_continuity or existential_matrix) as primary, the beneficiary/victim structure, the temporal scope of relevant history, and the classification of the Israeli state''s founding all invert or shift substantially — this is exactly the ε-invariance decomposition rule: each reading is authored as its own constraint with its own stable ε rather than blended into one.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_selection_omega, conceptual, 'Whether this reading''s premises are the dispositive framework or one of several incompatible sibling framings of the same contested kernel.').

omega_variable(
    demographic_continuity_evidentiary_ambiguity,
    'How is ''continuous residence'' and ''demographic majority during the modern period'' precisely bounded and evidenced — what start date, what population categories count, and how does in-migration during the Mandate period (in both directions) affect the continuity claim this reading rests on?',
    'Historical demographic census records (Ottoman, British Mandate) and their scholarly reinterpretation; the dispute is partly empirical (what the numbers show) and partly conceptual (which population movements count as disrupting or not disrupting continuity).',
    'A narrower or contested reading of demographic continuity could weaken the self-determination reading''s evidentiary foundation without changing its normative structure, while a robust reading strengthens the coordination/legitimacy function this constraint''s beneficiaries derive from it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(demographic_continuity_evidentiary_ambiguity, empirical, 'Evidentiary uncertainty underlying the demographic-continuity premise this reading is built on.').

omega_variable(
    coordination_versus_extraction_ratio_over_time,
    'As the claim has moved from active anti-colonial mobilization (1917-1948) through war and occupation (1948-1993) into a prolonged negotiation-and-stalemate period (1993-2024), has the ratio of genuine coordination benefit to accumulated extraction cost shifted meaningfully, and for whom?',
    'Longitudinal tracking of material outcomes (refugee resettlement rates, movement restriction intensity, negotiation outcomes) against diplomatic/political capital generated for leadership and allied-state actors across the same period.',
    'If the ratio has shifted heavily toward extraction for trapped populations while coordination benefit concentrates at the leadership/allied-state level, this would sharpen the tangled_rope classification toward the snare boundary for the payer seats specifically, without necessarily changing the beneficiary-seat classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_versus_extraction_ratio_over_time, empirical, 'Whether the coordination-to-extraction balance has drifted over the measured interval, and for which stakeholder seats.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(territorial_sovereignty_legitimacy__self_determination_reading, 1917, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(terr_tr_t1917, territorial_sovereignty_legitimacy__self_determination_reading, theater_ratio, 1917, 0.1).
narrative_ontology:measurement(terr_tr_t1948, territorial_sovereignty_legitimacy__self_determination_reading, theater_ratio, 1948, 0.15).
narrative_ontology:measurement(terr_tr_t1967, territorial_sovereignty_legitimacy__self_determination_reading, theater_ratio, 1967, 0.18).
narrative_ontology:measurement(terr_tr_t1993, territorial_sovereignty_legitimacy__self_determination_reading, theater_ratio, 1993, 0.32).
narrative_ontology:measurement(terr_tr_t2000, territorial_sovereignty_legitimacy__self_determination_reading, theater_ratio, 2000, 0.3).
narrative_ontology:measurement(terr_tr_t2010, territorial_sovereignty_legitimacy__self_determination_reading, theater_ratio, 2010, 0.35).
narrative_ontology:measurement(terr_tr_t2024, territorial_sovereignty_legitimacy__self_determination_reading, theater_ratio, 2024, 0.28).

% Extraction over time
narrative_ontology:measurement(terr_be_t1917, territorial_sovereignty_legitimacy__self_determination_reading, base_extractiveness, 1917, 0.15).
narrative_ontology:measurement(terr_be_t1948, territorial_sovereignty_legitimacy__self_determination_reading, base_extractiveness, 1948, 0.45).
narrative_ontology:measurement(terr_be_t1967, territorial_sovereignty_legitimacy__self_determination_reading, base_extractiveness, 1967, 0.55).
narrative_ontology:measurement(terr_be_t1993, territorial_sovereignty_legitimacy__self_determination_reading, base_extractiveness, 1993, 0.5).
narrative_ontology:measurement(terr_be_t2000, territorial_sovereignty_legitimacy__self_determination_reading, base_extractiveness, 2000, 0.6).
narrative_ontology:measurement(terr_be_t2010, territorial_sovereignty_legitimacy__self_determination_reading, base_extractiveness, 2010, 0.62).
narrative_ontology:measurement(terr_be_t2024, territorial_sovereignty_legitimacy__self_determination_reading, base_extractiveness, 2024, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(terr_su_t1917, territorial_sovereignty_legitimacy__self_determination_reading, suppression_requirement, 1917, 0.2).
narrative_ontology:measurement(terr_su_t1948, territorial_sovereignty_legitimacy__self_determination_reading, suppression_requirement, 1948, 0.6).
narrative_ontology:measurement(terr_su_t1967, territorial_sovereignty_legitimacy__self_determination_reading, suppression_requirement, 1967, 0.72).
narrative_ontology:measurement(terr_su_t1993, territorial_sovereignty_legitimacy__self_determination_reading, suppression_requirement, 1993, 0.65).
narrative_ontology:measurement(terr_su_t2000, territorial_sovereignty_legitimacy__self_determination_reading, suppression_requirement, 2000, 0.7).
narrative_ontology:measurement(terr_su_t2010, territorial_sovereignty_legitimacy__self_determination_reading, suppression_requirement, 2010, 0.75).
narrative_ontology:measurement(terr_su_t2024, territorial_sovereignty_legitimacy__self_determination_reading, suppression_requirement, 2024, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(territorial_sovereignty_legitimacy__self_determination_reading, identity_coordination).
narrative_ontology:affects_constraint(territorial_sovereignty_legitimacy__self_determination_reading, territorial_sovereignty_legitimacy__covenant_continuity_reading).
narrative_ontology:affects_constraint(territorial_sovereignty_legitimacy__self_determination_reading, territorial_sovereignty_legitimacy__existential_matrix_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked constraints decomposing the natural-language label 'sovereignty legitimacy over the territory' per the ε-invariance principle: measuring legitimacy through the self-determination/demographic-continuity observable yields a structurally distinct ε, beneficiary set, and victim set from measuring it through ancient-covenant-plus-recognition (covenant_continuity_reading) or through existential-survival-necessity (existential_matrix_reading). Each reading is authored as its own constraint with its own stable ε; none is a measurement-parameter variant of the others. All three are linked bidirectionally via affects_constraints to preserve the kernel-family structure for contamination/propagation analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
