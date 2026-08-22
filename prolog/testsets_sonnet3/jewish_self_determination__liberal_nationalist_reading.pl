% ============================================================================
% CONSTRAINT STORY: jewish_self_determination__liberal_nationalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jewish_self_determination__liberal_nationalist_reading, []).

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
 *   constraint_id: jewish_self_determination__liberal_nationalist_reading
 *   human_readable: Liberal Nationalist Reading: Jewish Peoplehood and Equal National Self-Determination
 *   domain: political_philosophy/nationalism_studies
 *
 * SUMMARY:
 *   This story instantiates the liberal-nationalist reading of the contested
 *   Jewish self-determination kernel: the claim that Jewish people constitute
 *   a nation with a claim to self-determination on par with other peoples,
 *   and that this claim can in principle be satisfied through a coordination
 *   mechanism — partition or parallel statehood — that also honors a
 *   competing Palestinian national claim, rather than requiring the
 *   extinction of either. Under this reading alone, the constraint is
 *   authored as low-to-moderate extraction: the arrangement is intended as
 *   reciprocal recognition, not as extraction from a named victim group, so
 *   no victims are declared. This is emphatically ONE reading among five
 *   sibling constraints in the same kernel (diasporist, indigenous_return,
 *   religious_covenant, settler_colonial); each sibling authors a
 *   structurally different constraint with a different ε, different
 *   beneficiaries, and in several cases named victims. This story does not
 *   describe, average, or hedge across those siblings — it is the clean
 *   liberal-nationalist claim, assessed by its own lights, per the
 *   ε-invariance principle.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_self_determination__liberal_nationalist_reading, 0.32).
domain_priors:suppression_score(jewish_self_determination__liberal_nationalist_reading, 0.28).
domain_priors:theater_ratio(jewish_self_determination__liberal_nationalist_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_self_determination__liberal_nationalist_reading, extractiveness, 0.32).
narrative_ontology:constraint_metric(jewish_self_determination__liberal_nationalist_reading, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(jewish_self_determination__liberal_nationalist_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_self_determination__liberal_nationalist_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(jewish_self_determination__liberal_nationalist_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_self_determination__liberal_nationalist_reading, rope).
narrative_ontology:human_readable(jewish_self_determination__liberal_nationalist_reading, "Liberal Nationalist Reading: Jewish Peoplehood and Equal National Self-Determination").
narrative_ontology:topic_domain(jewish_self_determination__liberal_nationalist_reading, "political_philosophy/nationalism_studies").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_self_determination__liberal_nationalist_reading, 'c13c0086-b683-467a-a652-e03498b0a272').
narrative_ontology:cs_kernel_codification('c13c0086-b683-467a-a652-e03498b0a272', distributed).
narrative_ontology:cs_authority_grounding('c13c0086-b683-467a-a652-e03498b0a272', distributed).
narrative_ontology:cs_reading_relation('c13c0086-b683-467a-a652-e03498b0a272', jewish_self_determination__indigenous_return_reading, coexists_with).
narrative_ontology:cs_reading_relation('c13c0086-b683-467a-a652-e03498b0a272', jewish_self_determination__settler_colonial_reading, coexists_with).
narrative_ontology:cs_reading_relation('c13c0086-b683-467a-a652-e03498b0a272', jewish_self_determination__religious_covenant_reading, coexists_with).
narrative_ontology:cs_reading_relation('c13c0086-b683-467a-a652-e03498b0a272', jewish_self_determination__diasporist_reading, forecloses).
narrative_ontology:cs_axiom('c13c0086-b683-467a-a652-e03498b0a272', foundational, national_self_determination_is_universal_and_symmetric).
narrative_ontology:cs_axiom_status(national_self_determination_is_universal_and_symmetric, holdable).
narrative_ontology:cs_axiom_grounding('c13c0086-b683-467a-a652-e03498b0a272', national_self_determination_is_universal_and_symmetric, deontological).
narrative_ontology:cs_axiom('c13c0086-b683-467a-a652-e03498b0a272', foundational, territorial_sovereignty_is_the_appropriate_remedy_for_statelessness).
narrative_ontology:cs_axiom_status(territorial_sovereignty_is_the_appropriate_remedy_for_statelessness, holdable).
narrative_ontology:cs_axiom_grounding('c13c0086-b683-467a-a652-e03498b0a272', territorial_sovereignty_is_the_appropriate_remedy_for_statelessness, instrumental).
narrative_ontology:cs_reference_frame('c13c0086-b683-467a-a652-e03498b0a272', post_westphalian_nation_state_parity_norm).
narrative_ontology:cs_drift_state('c13c0086-b683-467a-a652-e03498b0a272', contemporary_post_1990s_international_order, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('c13c0086-b683-467a-a652-e03498b0a272', '').
narrative_ontology:cs_kernel_id(jewish_self_determination__liberal_nationalist_reading, jewish_self_determination).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_self_determination__liberal_nationalist_reading, jewish_diaspora_seeking_refuge).
narrative_ontology:constraint_beneficiary(jewish_self_determination__liberal_nationalist_reading, israeli_jewish_citizens).
narrative_ontology:constraint_vindicates(jewish_self_determination__liberal_nationalist_reading, national_self_determination_universalism).
narrative_ontology:constraint_vindicates(jewish_self_determination__liberal_nationalist_reading, peoplehood_status_of_jews).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Historically dispersed across states with recurring episodes of exclusion, expulsion, and genocide. Under this reading, the claim to a state of their own is a remedy for statelessness and vulnerability comparable to the national claims other peoples make. Exit from persecution elsewhere is understood to run through the availability of this national home.
narrative_ontology:constraint_stakeholder(jewish_self_determination__liberal_nationalist_reading, jewish_diaspora_seeking_refuge, beneficiary,
    moderate, generational, constrained, global).

% Live under a state built to embody Jewish national self-determination, participate in its institutions, and bear its defense burdens. They administer the state apparatus that gives the claim concrete form, and their security is treated as the proof-condition of the coordination problem the reading names.
narrative_ontology:constraint_stakeholder(jewish_self_determination__liberal_nationalist_reading, israeli_jewish_citizens, beneficiary,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(jewish_self_determination__liberal_nationalist_reading, israeli_jewish_citizens, agenda_setter).

% Asserts a competing and, on many tellings, prior national claim to overlapping territory. This reading treats their claim as symmetric in principle — a second nation also entitled to self-determination — and imagines partition as the coordination solution, but the movement itself is not a party constructing this reading; whether the reading's own logic actually delivers reciprocal sovereignty to them is precisely what is contested elsewhere in the kernel.
narrative_ontology:constraint_stakeholder(jewish_self_determination__liberal_nationalist_reading, palestinian_national_movement, excluded,
    organized, generational, trapped, regional).

% The UN partition framework, international law bodies, and liberal states that have historically endorsed a two-state or partition logic as the mechanism for adjudicating between competing national claims. They evaluate proposals, extend or withhold recognition, and are invoked by this reading as the universalist standard against which the Jewish claim is measured for parity with other peoples' claims.
narrative_ontology:constraint_stakeholder(jewish_self_determination__liberal_nationalist_reading, international_liberal_order_institutions, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Adjudicates between two peoples asserting national claims to overlapping territory by applying a universal principle — self-determination applies equally to nations as such — and proposing that partition or parallel statehood resolves the competition rather than requiring either side to renounce peoplehood.
% TRANSFER_FUNCTION: In principle, moves recognition and territorial sovereignty to the Jewish national collective without transferring anything away from a parallel Palestinian national claim, since the reading is constructed on the premise that both claims can be satisfied through division rather than exclusion.
% ABSENT_VOICES: Palestinian national institutions do not author or ratify this reading's premises; they are treated as the symmetric other claimant inside the argument's structure but are not present in constructing the liberal-nationalist case itself. Their own account of whether partition has in fact been offered or honored belongs to the sibling readings, not this one.
% DISAPPEARANCE_RATIONALE: If the liberal-nationalist framing vanished, the case for Jewish statehood would have to be remade on other grounds (indigenous return, covenant, or diasporist rejection) — for Israeli Jewish citizens and diaspora communities invested in a rights-based universalist justification, that would be a significant rearrangement of the state's legitimating story even though the state's brute existence would not evaporate; other readings dispute whether this framing was ever load-bearing to begin with.
% FOUNDING_PROBLEM: Statelessness and repeated catastrophic persecution of a dispersed people lacking any sovereign territory or state protection, most acutely crystallized by the Holocaust, in a world order increasingly organized around nation-states as the unit of protection and recognition.
% FOUNDING_PROBLEM_CORROBORATION: Liberal internationalist historians and post-WWII diplomatic records (e.g., UN General Assembly deliberations) attest that statelessness and the absence of great-power protection for Jews was treated as a live problem by non-Jewish drafters of the partition framework, corroborating the reading from outside its direct beneficiaries. Palestinian historians and settler-colonial-reading scholars dispute that this framing captures the founding problem completely, arguing it elides pre-existing indigenous presence — that dispute is exactly the boundary this reading sits on.
narrative_ontology:disappearance_verdict(jewish_self_determination__liberal_nationalist_reading, contested).
narrative_ontology:founding_problem_status(jewish_self_determination__liberal_nationalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_self_determination__liberal_nationalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(jewish_self_determination__liberal_nationalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jewish_self_determination__liberal_nationalist_reading, 0.32, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jewish_self_determination__liberal_nationalist_reading_tests).
:- end_tests(jewish_self_determination__liberal_nationalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored low-to-moderate (0.32 at story end) because the reading's own premises assume the coordination problem is soluble without a losing party — the theoretical architecture is rope-shaped by design. Suppression is authored moderate-low (0.28) reflecting that the reading does not require coercing anyone out of a claim in principle, though its application in practice (which this story does not adjudicate) has generated contestation the reading's own framework doesn't fully metabolize. Resistance is authored higher (0.55) because, notwithstanding the reading's internal coherence, it meets substantial contestation from other seats (the sibling readings) about whether its premises hold in the world.
 *
 * PERSPECTIVAL GAP:
 *   From the beneficiary seats, this reading computes as coordination: two peoples, one soluble territorial puzzle, mutual recognition as the exit. From the excluded seat, the same structure is exactly what the settler_colonial_reading and indigenous_return_reading interrogate — whether 'equal claim' as applied in practice actually produced reciprocal sovereignty or asymmetric displacement is the live dispute the kernel exists to hold. This story does not resolve that dispute; it authors only the liberal-nationalist premise cleanly.
 *
 * DIRECTIONALITY LOGIC:
 *   Jewish diaspora communities and Israeli Jewish citizens are declared beneficiaries because the reading's coordination logic is constructed to deliver them a state and a rights-parity claim; their directionality sits toward the beneficiary end. The Palestinian national movement is deliberately NOT declared a victim in this reading — the reading's own premise is that partition delivers symmetric benefit to a second nation — but is marked 'excluded' because it does not author or ratify the premise and its own account of outcomes is the subject of sibling constraints, not this one.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (Jewish statelessness and catastrophic persecution) is authored as contested-status rather than flatly live or dead: post-WWII international consensus corroborates it as a real historical problem the framework was built to solve, which cuts against treating the state as pure inertial residue; but whether the liberal-nationalist coordination mechanism (partition, reciprocal statehood) actually discharged its founding promise to the second party is exactly what the sibling readings dispute, which is why founding_problem_status is authored 'contested' rather than 'live' outright.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    partition_feasibility_ambiguity,
    'Does the liberal-nationalist coordination mechanism (partition/parallel statehood) actually deliver reciprocal sovereignty to the Palestinian national claim, or does its practical implementation collapse into asymmetric extraction — which would mean this reading''s low ε is an artifact of theoretical premise rather than descriptive of the arrangement''s operation?',
    'Comparative analysis of actual partition outcomes (1947 UN plan non-implementation, subsequent territorial and demographic changes) against the reading''s own stated success conditions; cross-reference against the settler_colonial_reading''s and indigenous_return_reading''s independently authored ε and victim declarations for the same underlying territorial history.',
    'If partition/reciprocity was never actually realized, the liberal-nationalist reading''s rope classification describes an unrealized theoretical ideal rather than the standing arrangement, and downstream analysis should weight the sibling readings (which do name victims) more heavily as descriptive of the arrangement''s actual operation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(partition_feasibility_ambiguity, conceptual, 'Whether the reading''s no-victim premise survives contact with implementation history.').

omega_variable(
    peoplehood_naturalness_or_construction,
    'Is Jewish peoplehood-as-nation a naturally occurring collective identity to which self-determination principles simply apply, or is ''the Jewish nation'' itself a late-19th/20th century constructed political category (per some diasporist and post-colonial scholarship) — meaning the reading''s beneficiaries are the product of the framework rather than pre-existing subjects of it?',
    'Historical and sociological scholarship on the emergence of Jewish nationalism as a modern political movement versus continuity claims from religious and cultural historiography; examine whether ''nation'' status was contested within Jewish communities themselves prior to Zionist organizing.',
    'If peoplehood is substantially constructed rather than natural, the reading''s claim to ''equal parity with other peoples'' national claims'' rests on a category that itself required political work to stabilize — which does not invalidate the claim but changes whether it should be read as a discovery or an achievement, bearing on how the reading''s beneficiaries should be understood.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(peoplehood_naturalness_or_construction, conceptual, 'Whether the reading''s core subject (the Jewish nation) is a natural or constructed category.').

omega_variable(
    excluded_seat_ratification,
    'Can a coordination reading legitimately claim to produce no victims when the second party to the proposed coordination (the Palestinian national movement) has not ratified the premises of this specific reading and disputes, via sibling readings, that the coordination in fact occurred symmetrically?',
    'Track whether Palestinian national institutions or representative bodies have, at any point, endorsed the liberal-nationalist partition logic as a fair resolution versus rejecting its terms as imposed; examine the historical record of the 1947 partition vote and subsequent negotiations.',
    'If the excluded party never ratified the coordination framing, the ''no victim in principle'' declaration is a feature of this reading''s internal premises rather than a verified absence of extraction — reinforcing that this story is one contested reading, not a settled account.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(excluded_seat_ratification, empirical, 'Whether the absence of a declared victim reflects verified symmetry or unratified premise.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_self_determination__liberal_nationalist_reading, 0, 76).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jewi_tr_t0, jewish_self_determination__liberal_nationalist_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(jewi_tr_t15, jewish_self_determination__liberal_nationalist_reading, theater_ratio, 15, 0.1).
narrative_ontology:measurement(jewi_tr_t30, jewish_self_determination__liberal_nationalist_reading, theater_ratio, 30, 0.12).
narrative_ontology:measurement(jewi_tr_t45, jewish_self_determination__liberal_nationalist_reading, theater_ratio, 45, 0.13).
narrative_ontology:measurement(jewi_tr_t60, jewish_self_determination__liberal_nationalist_reading, theater_ratio, 60, 0.14).
narrative_ontology:measurement(jewi_tr_t76, jewish_self_determination__liberal_nationalist_reading, theater_ratio, 76, 0.15).

% Extraction over time
narrative_ontology:measurement(jewi_be_t0, jewish_self_determination__liberal_nationalist_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(jewi_be_t15, jewish_self_determination__liberal_nationalist_reading, base_extractiveness, 15, 0.26).
narrative_ontology:measurement(jewi_be_t30, jewish_self_determination__liberal_nationalist_reading, base_extractiveness, 30, 0.29).
narrative_ontology:measurement(jewi_be_t45, jewish_self_determination__liberal_nationalist_reading, base_extractiveness, 45, 0.31).
narrative_ontology:measurement(jewi_be_t60, jewish_self_determination__liberal_nationalist_reading, base_extractiveness, 60, 0.32).
narrative_ontology:measurement(jewi_be_t76, jewish_self_determination__liberal_nationalist_reading, base_extractiveness, 76, 0.32).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(jewish_self_determination__liberal_nationalist_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_self_determination__liberal_nationalist_reading, identity_coordination).
narrative_ontology:affects_constraint(jewish_self_determination__liberal_nationalist_reading, indigenous_return_reading).
narrative_ontology:affects_constraint(jewish_self_determination__liberal_nationalist_reading, settler_colonial_reading).
narrative_ontology:affects_constraint(jewish_self_determination__liberal_nationalist_reading, religious_covenant_reading).
narrative_ontology:affects_constraint(jewish_self_determination__liberal_nationalist_reading, diasporist_reading).

% DUAL FORMULATION NOTE:
% This story is one of five sibling constraints decomposing the natural-language 'Jewish self-determination / Zionism' claim per the ε-invariance principle. Each sibling reading (indigenous_return, settler_colonial, religious_covenant, diasporist, and this liberal_nationalist reading) authors a distinct ε, distinct beneficiary/victim structure, and distinct claimed_type from the same underlying kernel — the historical and ongoing project of Jewish national sovereignty over territory in historic Palestine/Israel. This reading is the lowest-ε member of the family (rope, no declared victims), reflecting its own internal premise that the coordination problem is soluble without extraction; settler_colonial_reading is expected to author substantially higher ε with named Palestinian victims describing the same underlying historical events. The divergence across siblings IS the kernel contest — it is not resolved by any single story, including this one.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
