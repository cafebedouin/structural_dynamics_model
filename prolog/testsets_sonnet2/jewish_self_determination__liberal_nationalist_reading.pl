% ============================================================================
% CONSTRAINT STORY: jewish_self_determination__liberal_nationalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
 *   constraint_id: jewish_self_determination__liberal_nationalist_reading
 *   human_readable: Jewish National Self-Determination as Liberal-Nationalist Coordination Claim
 *   domain: political_philosophy/nationalism_studies
 *
 * SUMMARY:
 *   This story authors ONLY the liberal-nationalist reading of the contested
 *   Jewish self-determination kernel: the claim that Jewish people constitute
 *   a nation possessing the same in-principle right to self-determination as
 *   other peoples recognized under the post-Versailles/post-1945
 *   international order, and that a coordination mechanism (partition, mutual
 *   recognition) can in principle satisfy competing national claims without
 *   victims. This is a rope-type coordination claim precisely because, on its
 *   own terms, it denies any necessary victim — the extraction that would
 *   make it a tangled rope or snare arises only if partition fails
 *   empirically, which is a question this reading treats as contingent, not
 *   structural. The sibling readings (indigenous_return, settler_colonial,
 *   religious_covenant, diasporist) are NOT part of this constraint; they are
 *   separate constraints with their own ε, their own beneficiary/victim
 *   structures, linked via network.affects_constraints.
 *
 * KEY AGENTS:
 *   - jewish_diaspora_seeking_sovereignty: primary beneficiary (organized/constrained) — the claiming national group
 *   - israeli_jewish_citizens: beneficiary and agenda-setter (institutional/constrained) — inherits and administers the resulting sovereign apparatus
 *   - palestinian_national_movement: excluded/structurally parallel claimant (organized/trapped) — whose symmetrical claim this reading acknowledges in principle but does not center
 *   - international_state_system: analytical observer (institutional/analytical) — applies the self-determination principle unevenly across claimants
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
narrative_ontology:constraint_metric(jewish_self_determination__liberal_nationalist_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(jewish_self_determination__liberal_nationalist_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_self_determination__liberal_nationalist_reading, rope).
narrative_ontology:human_readable(jewish_self_determination__liberal_nationalist_reading, "Jewish National Self-Determination as Liberal-Nationalist Coordination Claim").
narrative_ontology:topic_domain(jewish_self_determination__liberal_nationalist_reading, "political_philosophy/nationalism_studies").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_self_determination__liberal_nationalist_reading, 'd233ae94-a8af-4905-b419-85fdf35ed8c5').
narrative_ontology:cs_kernel_codification('d233ae94-a8af-4905-b419-85fdf35ed8c5', distributed).
narrative_ontology:cs_authority_grounding('d233ae94-a8af-4905-b419-85fdf35ed8c5', distributed).
narrative_ontology:cs_reading_relation('d233ae94-a8af-4905-b419-85fdf35ed8c5', jewish_self_determination__indigenous_return_reading, coexists_with).
narrative_ontology:cs_reading_relation('d233ae94-a8af-4905-b419-85fdf35ed8c5', jewish_self_determination__settler_colonial_reading, coexists_with).
narrative_ontology:cs_reading_relation('d233ae94-a8af-4905-b419-85fdf35ed8c5', jewish_self_determination__religious_covenant_reading, coexists_with).
narrative_ontology:cs_reading_relation('d233ae94-a8af-4905-b419-85fdf35ed8c5', jewish_self_determination__diasporist_reading, influences).
narrative_ontology:cs_axiom('d233ae94-a8af-4905-b419-85fdf35ed8c5', foundational, peoplehood_grounds_symmetrical_national_claim).
narrative_ontology:cs_axiom_status(peoplehood_grounds_symmetrical_national_claim, holdable).
narrative_ontology:cs_axiom_grounding('d233ae94-a8af-4905-b419-85fdf35ed8c5', peoplehood_grounds_symmetrical_national_claim, conventional).
narrative_ontology:cs_axiom('d233ae94-a8af-4905-b419-85fdf35ed8c5', foundational, partition_resolves_competing_claims_without_victim).
narrative_ontology:cs_axiom_status(partition_resolves_competing_claims_without_victim, holdable).
narrative_ontology:cs_axiom_grounding('d233ae94-a8af-4905-b419-85fdf35ed8c5', partition_resolves_competing_claims_without_victim, empirically_contingent).
narrative_ontology:cs_reference_frame('d233ae94-a8af-4905-b419-85fdf35ed8c5', comparative_national_self_determination_framework).
narrative_ontology:cs_drift_state('d233ae94-a8af-4905-b419-85fdf35ed8c5', post_1993_oslo_and_after, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('d233ae94-a8af-4905-b419-85fdf35ed8c5', '').
narrative_ontology:cs_kernel_id(jewish_self_determination__liberal_nationalist_reading, jewish_self_determination).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_self_determination__liberal_nationalist_reading, jewish_diaspora_seeking_sovereignty).
narrative_ontology:constraint_beneficiary(jewish_self_determination__liberal_nationalist_reading, israeli_jewish_citizens).
narrative_ontology:constraint_vindicates(jewish_self_determination__liberal_nationalist_reading, national_self_determination_principle).
narrative_ontology:constraint_vindicates(jewish_self_determination__liberal_nationalist_reading, peoplehood_criterion_for_statehood).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Historically dispersed and subject to recurring persecution culminating in genocide, this population organized political movements claiming a right, parallel to other peoples' national movements, to territorial self-governance. The reading treats this claim as symmetrical to Polish, Czech, or Kurdish national claims: a people with shared language revival, historical memory, and persecution history seeking the ordinary remedy of statehood.
narrative_ontology:constraint_stakeholder(jewish_self_determination__liberal_nationalist_reading, jewish_diaspora_seeking_sovereignty, beneficiary,
    organized, generational, constrained, global).

% Live under the sovereign apparatus this claim justifies, holding full citizenship and political voice in the resulting state. They administer the state institutions that instantiate the self-determination claim and bear its security burdens and international legitimacy costs.
narrative_ontology:constraint_stakeholder(jewish_self_determination__liberal_nationalist_reading, israeli_jewish_citizens, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(jewish_self_determination__liberal_nationalist_reading, israeli_jewish_citizens, agenda_setter).

% Asserts a parallel and geographically overlapping national claim to the same territory. Under this reading's own logic (symmetrical peoplehood claims, partition as the coordination solution), Palestinian national self-determination is treated as an equally valid claim requiring its own state alongside, not instead of, the Jewish one — but the reading's optimistic partition premise has not been realized on the ground, leaving this population's claim structurally unresolved rather than foreclosed.
narrative_ontology:constraint_stakeholder(jewish_self_determination__liberal_nationalist_reading, palestinian_national_movement, excluded,
    organized, generational, trapped, regional).

% Adjudicates competing national claims through recognition, UN resolutions, and treaty frameworks. Applies (unevenly) the same self-determination principle this reading invokes to other peoples, creating pressure to treat the Jewish and Palestinian claims as structurally parallel cases requiring a shared resolution mechanism.
narrative_ontology:constraint_stakeholder(jewish_self_determination__liberal_nationalist_reading, international_state_system, observer,
    institutional, civilizational, analytical, global).

% Sibling framings of the same underlying kernel — divine covenant, indigenous return, settler-colonial critique, diasporist pluralism — are not represented as actors here but shape the discursive field in which the liberal-nationalist claim must argue on secular, comparative-peoplehood grounds rather than theological or purely historical-continuity grounds.
narrative_ontology:constraint_stakeholder(jewish_self_determination__liberal_nationalist_reading, religious_and_indigenous_rival_readings, excluded,
    analytical, civilizational, analytical, global).
narrative_ontology:stakeholder_non_agent(jewish_self_determination__liberal_nationalist_reading, religious_and_indigenous_rival_readings).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a secular, comparative framework — analogous to other 19th/20th century national self-determination movements — for resolving a genuine problem: a stateless, historically persecuted population seeking a coordination mechanism (sovereignty) to secure physical safety and collective self-governance, symmetrical to claims already granted to other peoples under the post-Versailles and post-1945 international order.
% TRANSFER_FUNCTION: In principle, the arrangement transfers recognition and sovereignty-backed security from a state of statelessness/vulnerability to the claiming population; it does not, in this reading's own logic, require transferring land or rights away from another people, since the reading's premise is that a parallel partition-based coordination resolves both claims without a zero-sum transfer. Whether that premise holds empirically is exactly what the sibling readings and the omega variables below contest.
% ABSENT_VOICES: The Palestinian national movement is structurally present as a claimant this reading acknowledges in principle (symmetry demands it) but whose voice is not centered in the reading's own justificatory architecture, which argues from Jewish historical persecution and comparative peoplehood rather than from the territorial and demographic consequences borne by Palestinians. Religious-covenant and indigenous-return advocates within the pro-Zionist camp are also marginalized by this reading's insistence on a secular, universalizable justification.
% DISAPPEARANCE_RATIONALE: If the liberal-nationalist justification for Jewish self-determination were withdrawn as a legitimating framework, the state of Israel would not vanish (it has other legitimating claims available — religious covenant, indigenous return, established fact of sovereignty) but its claim to symmetry with other national movements and to standing within liberal international law would be substantially weakened, altering its diplomatic and moral position without altering the underlying institutional facts on the ground. Whether the 'world rearranges' or stays materially unchanged is precisely what separates this reading from the settler-colonial reading.
% FOUNDING_PROBLEM: Sustained European antisemitism, culminating in pogroms and the Holocaust, demonstrated that minority-rights protections and diaspora existence, however constituted, could not reliably secure Jewish physical safety; the founding problem was statelessness-as-vulnerability, to be solved by the same remedy (territorial sovereignty) that other persecuted or stateless peoples sought and were granted in the same historical period.
% FOUNDING_PROBLEM_CORROBORATION: Historians of nationalism (e.g., comparative work on 19th-20th century national movements) and international legal scholars attest that the underlying pattern — persecuted diaspora seeking sovereignty as security guarantee — parallels other recognized national movements, corroborating the founding problem's historical reality from outside directly-benefiting parties. However, Palestinian scholars, historians of the Nakba, and postcolonial theorists attest that the resolution mechanism (partition, state-building) itself generated a new and comparably severe problem of Palestinian dispossession, meaning the founding problem's 'solution' is contested precisely because its stated resolution created displacement for a population outside the original justificatory frame. No fully neutral corroborating source exists; both attestations come from parties with a stake in the outcome, which is itself part of what the sibling readings dispute.
narrative_ontology:disappearance_verdict(jewish_self_determination__liberal_nationalist_reading, contested).
narrative_ontology:founding_problem_status(jewish_self_determination__liberal_nationalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_self_determination__liberal_nationalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
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
 *   Extractiveness is authored low-to-moderate (0.32) reflecting the reading's own premise that partition and mutual recognition resolve competing claims without a necessary victim — this is markedly lower than what the settler_colonial_reading would author for the same underlying territorial arrangement, precisely because that is a different constraint with a different beneficiary/victim structure and a different ε. Suppression is low-moderate (0.28): the liberal-nationalist claim does not, on its own terms, require suppressing alternatives so much as it requires winning an argument about symmetry with other national movements. Resistance is moderately high (0.55) because the claim is heavily contested by the sibling readings and by the Palestinian national movement, even though this reading's own theater ratio stays low — the coordination function it claims is substantively argued, not merely performed. The rising extractiveness trend across 1917-1967 tracks the period when the coordination-versus-conquest tension was most acute (Balfour, partition, 1967 war) before settling into a contested plateau.
 *
 * PERSPECTIVAL GAP:
 *   From the beneficiary seats (Jewish diaspora, Israeli citizens), the arrangement genuinely reads as a rope: a coordination solution to a real historical problem of statelessness-driven vulnerability, symmetrical to other recognized national movements. From the excluded seat (Palestinian national movement), the SAME underlying territorial arrangement is not visible through this reading at all — it is visible through the settler_colonial_reading or indigenous-competing-claims framing, which is why those are separate constraint stories rather than a different 'observable' of this one. The engine computes each seat's classification from the structural data authored here; the divergence between how the liberal-nationalist reading's own metrics classify versus how a Palestinian-centered reading of the same territory would classify is exactly the kind of divergence the ε-invariance principle requires decomposing into separate files, which has been done.
 *
 * DIRECTIONALITY LOGIC:
 *   Jewish diaspora and Israeli citizens are declared beneficiaries: the constraint (the recognized principle of symmetrical national self-determination) subsidizes their claim to statehood and security, so directionality sits toward the beneficiary end. No victims are declared in this reading's own structural data, consistent with its premise that partition resolves rather than displaces competing claims — this is a deliberate authored choice reflecting what THIS reading claims, not an empirical judgment that no one was in fact displaced (that judgment belongs to the settler_colonial_reading and indigenous_return_reading siblings, authored separately with their own beneficiary/victim sets. The Palestinian national movement is marked 'excluded' rather than 'payer' specifically because this reading does not structurally assign them victim status — it assigns them a parallel, unresolved claim.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (persecution-driven statelessness as an unresolved security vulnerability) is authored as contested rather than flatly live or dead: from within this reading, diaspora vulnerability remains a live concern (rising global antisemitism is cited as ongoing corroboration), while critics note the specific historical catastrophe (the Holocaust) that most urgently motivated 20th-century Zionism is not itself recurring, meaning the arrangement's original acute justification has partially receded even as security concerns persist in modified form. This keeps the classification honest: labeling the founding problem simply 'dead' would ignore continuing antisemitism; labeling it simply 'live' in its original acute form would overstate continuity. The contested status is itself the finding.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    partition_feasibility_ambiguity,
    'Does a two-state partition arrangement actually resolve the competing national claims without producing a victim class, or does the liberal-nationalist reading''s no-victim premise depend on a partition outcome that has not been, and may not be, achievable on the ground?',
    'Track whether a mutually recognized, viable Palestinian state emerges alongside continued Israeli sovereignty; absence of such an outcome after an extended period would undermine the reading''s structural claim that no victim is required in principle.',
    'If partition proves durably infeasible, this reading''s low ε and empty victims array would need revision toward the tangled_rope or snare profile the settler_colonial_reading already authors for the same territory — though as a SEPARATE constraint, not by amending this one.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(partition_feasibility_ambiguity, empirical, 'Whether the reading''s no-victim premise survives empirical partition outcomes.').

omega_variable(
    symmetry_with_other_national_movements,
    'Is the comparative-peoplehood analogy to other 19th/20th century national movements (Polish, Czech, Kurdish, etc.) structurally sound, or does it elide morally relevant asymmetries — particularly the presence of an existing indigenous population with its own unbroken territorial claim — that make the analogy inapt?',
    'Comparative historical and political-theory analysis of cases where national self-determination movements territorialized onto land with a pre-existing distinct national population, versus cases (most cited comparators) where the movement territorialized within an already largely co-ethnic or contiguous homeland.',
    'If the analogy fails on this dimension, the liberal-nationalist reading''s claim to moral symmetry weakens substantially, strengthening the case for treating this as one contested reading among several rather than the default framing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(symmetry_with_other_national_movements, conceptual, 'Whether the comparative-nationalism analogy underlying this reading holds up under scrutiny.').

omega_variable(
    sole_or_shared_referent,
    'Does this reading''s claim to be a ''rope'' (pure coordination, no necessary victim) depend on treating Palestinian national self-determination as an equally weighted, simultaneously-realized parallel claim — and if that simultaneity has not occurred in practice, does the reading''s classification hold only counterfactually rather than descriptively?',
    'Assess whether recognition, statehood, and security guarantees have in fact been extended symmetrically to both claimed peoples, or whether one claim has been substantially realized while the other remains unresolved for an extended period.',
    'Asymmetric realization would suggest the reading''s rope classification describes an aspirational framework rather than the actual operating constraint, which is exactly the divergence the corpus is built to surface rather than paper over.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sole_or_shared_referent, empirical, 'Whether the reading''s stated symmetry has been realized in practice or remains aspirational.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_self_determination__liberal_nationalist_reading, 1897, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jewi_tr_t1897, jewish_self_determination__liberal_nationalist_reading, theater_ratio, 1897, 0.05).
narrative_ontology:measurement(jewi_tr_t1917, jewish_self_determination__liberal_nationalist_reading, theater_ratio, 1917, 0.08).
narrative_ontology:measurement(jewi_tr_t1948, jewish_self_determination__liberal_nationalist_reading, theater_ratio, 1948, 0.1).
narrative_ontology:measurement(jewi_tr_t1967, jewish_self_determination__liberal_nationalist_reading, theater_ratio, 1967, 0.13).
narrative_ontology:measurement(jewi_tr_t1993, jewish_self_determination__liberal_nationalist_reading, theater_ratio, 1993, 0.14).
narrative_ontology:measurement(jewi_tr_t2024, jewish_self_determination__liberal_nationalist_reading, theater_ratio, 2024, 0.15).

% Extraction over time
narrative_ontology:measurement(jewi_be_t1897, jewish_self_determination__liberal_nationalist_reading, base_extractiveness, 1897, 0.15).
narrative_ontology:measurement(jewi_be_t1917, jewish_self_determination__liberal_nationalist_reading, base_extractiveness, 1917, 0.18).
narrative_ontology:measurement(jewi_be_t1948, jewish_self_determination__liberal_nationalist_reading, base_extractiveness, 1948, 0.3).
narrative_ontology:measurement(jewi_be_t1967, jewish_self_determination__liberal_nationalist_reading, base_extractiveness, 1967, 0.35).
narrative_ontology:measurement(jewi_be_t1993, jewish_self_determination__liberal_nationalist_reading, base_extractiveness, 1993, 0.3).
narrative_ontology:measurement(jewi_be_t2024, jewish_self_determination__liberal_nationalist_reading, base_extractiveness, 2024, 0.32).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(jewish_self_determination__liberal_nationalist_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_self_determination__liberal_nationalist_reading, identity_coordination).
narrative_ontology:affects_constraint(jewish_self_determination__liberal_nationalist_reading, jewish_self_determination__indigenous_return_reading).
narrative_ontology:affects_constraint(jewish_self_determination__liberal_nationalist_reading, jewish_self_determination__settler_colonial_reading).
narrative_ontology:affects_constraint(jewish_self_determination__liberal_nationalist_reading, jewish_self_determination__religious_covenant_reading).
narrative_ontology:affects_constraint(jewish_self_determination__liberal_nationalist_reading, jewish_self_determination__diasporist_reading).

% DUAL FORMULATION NOTE:
% This story is one of five members of the jewish_self_determination constraint family, each authoring a structurally distinct reading of the same contested kernel with its own ε, beneficiary/victim structure, and claimed type. The liberal_nationalist_reading (this file) authors the lowest ε among the pro-sovereignty readings because it structurally denies a necessary victim, unlike the settler_colonial_reading (high ε, explicit Palestinian victims) and unlike the indigenous_return_reading (which grounds legitimacy in continuity claims that foreclose the settler-colonial framing outright). The religious_covenant_reading and diasporist_reading each ground legitimacy on different axes entirely (theological necessity vs. rejection of territorial sovereignty as the remedy). All five are linked bidirectionally; this file's affects_constraints lists all four siblings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
