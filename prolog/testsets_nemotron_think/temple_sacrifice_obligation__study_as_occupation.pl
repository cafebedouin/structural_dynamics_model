% ============================================================================
% CONSTRAINT STORY: temple_sacrifice_obligation__study_as_occupation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
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
 *   human_readable: Study of Sacrifice Law as Occupation of Obligation
 *   domain: religious/halakhic/commitment_system
 *
 * SUMMARY:
 *   This constraint story captures the halakhic reading that study of
 *   sacrificial law (particularly the Kodashim tractates) constitutes
 *   legitimate occupation — not merely commemoration or preparation — of the
 *   biblical obligation to offer sacrifices, which became impossible after
 *   the Second Temple's destruction (70 CE). The reading draws on Talmudic
 *   statements (Menachot 110a: 'whoever studies the laws of the olah is as if
 *   he offered an olah') and operates as the primary mechanism by which the
 *   halakhic system absorbs the impossibility of performance without
 *   declaring the obligation suspended or void. The constraint coordinates
 *   Jewish ritual identity, preserves sacrificial knowledge, and channels
 *   communal resources into study institutions. No party extracts from
 *   another; the 'cost' of study is voluntarily borne as meritorious
 *   activity. The authority structure (rabbinic tradition, halakhic decisors)
 *   maintains the kernel's legitimacy through an interpretive layer that
 *   resolves the impossibility internally.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(temple_sacrifice_obligation__study_as_occupation, 0.15).
domain_priors:suppression_score(temple_sacrifice_obligation__study_as_occupation, 0.1).
domain_priors:theater_ratio(temple_sacrifice_obligation__study_as_occupation, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(temple_sacrifice_obligation__study_as_occupation, extractiveness, 0.15).
narrative_ontology:constraint_metric(temple_sacrifice_obligation__study_as_occupation, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(temple_sacrifice_obligation__study_as_occupation, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(temple_sacrifice_obligation__study_as_occupation, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(temple_sacrifice_obligation__study_as_occupation, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(temple_sacrifice_obligation__study_as_occupation, rope).
narrative_ontology:human_readable(temple_sacrifice_obligation__study_as_occupation, "Study of Sacrifice Law as Occupation of Obligation").
narrative_ontology:topic_domain(temple_sacrifice_obligation__study_as_occupation, "religious/halakhic/commitment_system").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(temple_sacrifice_obligation__study_as_occupation, '89787639-9641-4e4c-a39d-99c81f92e715').
narrative_ontology:cs_kernel_codification('89787639-9641-4e4c-a39d-99c81f92e715', fixed_text).
narrative_ontology:cs_authority_grounding('89787639-9641-4e4c-a39d-99c81f92e715', lineage).
narrative_ontology:cs_interpretation_layer_present('89787639-9641-4e4c-a39d-99c81f92e715').
narrative_ontology:cs_reading_relation('89787639-9641-4e4c-a39d-99c81f92e715', temple_sacrifice_obligation__messianic_suspension, coexists_with).
narrative_ontology:cs_reading_relation('89787639-9641-4e4c-a39d-99c81f92e715', temple_sacrifice_obligation__study_as_archiving, coexists_with).
narrative_ontology:cs_axiom('89787639-9641-4e4c-a39d-99c81f92e715', foundational, study_fulfills_sacrificial_obligation).
narrative_ontology:cs_axiom_status(study_fulfills_sacrificial_obligation, holdable).
narrative_ontology:cs_axiom_grounding('89787639-9641-4e4c-a39d-99c81f92e715', study_fulfills_sacrificial_obligation, conventional).
narrative_ontology:cs_axiom('89787639-9641-4e4c-a39d-99c81f92e715', secondary, talmud_torah_kneged_kulam_applies_to_kodashim).
narrative_ontology:cs_axiom_status(talmud_torah_kneged_kulam_applies_to_kodashim, holdable).
narrative_ontology:cs_axiom_grounding('89787639-9641-4e4c-a39d-99c81f92e715', talmud_torah_kneged_kulam_applies_to_kodashim, conventional).
narrative_ontology:cs_reference_frame('89787639-9641-4e4c-a39d-99c81f92e715', tannaitic_sacrificial_obligation).
narrative_ontology:cs_drift_state('89787639-9641-4e4c-a39d-99c81f92e715', contemporary_post_temple_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('89787639-9641-4e4c-a39d-99c81f92e715', '').
narrative_ontology:cs_kernel_id(temple_sacrifice_obligation__study_as_occupation, temple_sacrifice_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(temple_sacrifice_obligation__study_as_occupation, torah_scholars).
narrative_ontology:constraint_beneficiary(temple_sacrifice_obligation__study_as_occupation, observant_jewish_community).
narrative_ontology:constraint_beneficiary(temple_sacrifice_obligation__study_as_occupation, yeshiva_institutions).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(temple_sacrifice_obligation__study_as_occupation, observant_jewish_community).
narrative_ontology:constraint_vindicates(temple_sacrifice_obligation__study_as_occupation, talmud_torah_kneged_kulam).
narrative_ontology:constraint_vindicates(temple_sacrifice_obligation__study_as_occupation, study_as_sacrificial_fulfillment).
narrative_ontology:constraint_vindicates(temple_sacrifice_obligation__study_as_occupation, halakhic_continuity_through_learning).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Engage in intensive study of sacrificial tractates (Kodashim, Menachot, Zevachim) as primary occupation of the sacrificial obligation. Gain spiritual merit, scholarly status, and communal authority through this study. Can choose depth and focus of study; exit means shifting to other areas of Torah learning without penalty.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__study_as_occupation, torah_scholars, beneficiary,
    organized, generational, mobile, global).

% Collectively maintains the sacrificial obligation through supporting institutions of study (yeshivas, kollels, shiurim). Gains ritual continuity and identity coherence. Bears indirect costs through communal funding of study institutions. Exit from the communal framework is socially and identity-constrained.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__study_as_occupation, observant_jewish_community, beneficiary,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(temple_sacrifice_obligation__study_as_occupation, observant_jewish_community, payer).

% Structure curricula around sacrificial law, allocate resources to Kodashim study, certify scholarly achievement. Administer the constraint's operation by defining what counts as legitimate occupation. Can modify emphasis but operate within the halakhic frame that study fulfills the obligation.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__study_as_occupation, yeshiva_institutions, agenda_setter,
    institutional, generational, mobile, global).

% Issue rulings on the scope and sufficiency of study-as-fulfillment (which texts, what intensity, whether it covers all sacrifices or only some). Their interpretive decisions shape the constraint's boundaries without revising the kernel. Exit is analytical — they evaluate from within the tradition.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__study_as_occupation, halakhic_authorities, agenda_setter,
    institutional, generational, analytical, global).

% Hold the sibling reading that the obligation is suspended pending messianic restoration. They participate in the same ritual world (pray for Temple restoration, study sacrifices) but reject that study constitutes fulfillment. Their voice is present in liturgy and theology but excluded from this constraint's operational logic.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__study_as_occupation, messianic_expectants, excluded,
    moderate, generational, identity_locked, global).

% Hold the sibling reading that study preserves knowledge for future restoration but does not fulfill the obligation now. They engage in the same textual work but frame its telos differently. Excluded from this constraint's beneficiary structure because they deny its core claim.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__study_as_occupation, archival_scholars, excluded,
    moderate, biographical, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves sacrificial law knowledge and Jewish ritual identity during the Temple's absence by channeling the obligation into study, maintaining continuity without physical performance.
% TRANSFER_FUNCTION: Moves intellectual effort and time from practitioners to the maintenance of the halakhic sacrificial corpus; no material transfer occurs, only merit and status accrue within the tradition's economy.
% ABSENT_VOICES: Proponents of messianic_suspension (obligation suspended, not fulfilled) and study_as_archiving (study only preserves for future restoration) are present in halakhic discourse and liturgy but their readings are not instantiated in this constraint's operational logic — they would object to the claim that study constitutes complete fulfillment.
% DISAPPEARANCE_RATIONALE: If the study-as-occupation reading vanished overnight, the halakhic system would lose its primary mechanism for occupying the sacrificial obligation in exile, creating a vacuum in ritual continuity that would force adoption of suspension or archival readings, restructuring the community's relationship to the Temple's absence.
% FOUNDING_PROBLEM: The destruction of the Second Temple (70 CE) made sacrificial performance impossible, creating a crisis of obligation fulfillment for a system centered on Temple service.
% FOUNDING_PROBLEM_CORROBORATION: Talmudic sources (Menachot 110a, Ta'anit 27b) attest study-as-fulfillment; later authorities (Rambam Hilkhot Teshuvah 3:4, Ritva on Menachot) debate whether this is complete fulfillment or partial; modern scholars (Haym Soloveitchik, Moshe Halbertal) note the reading resolves the crisis but the messianic frame remains live in liturgy and prayer — corroboration comes from within the tradition's own disputants, not from beneficiaries alone.
narrative_ontology:disappearance_verdict(temple_sacrifice_obligation__study_as_occupation, world_rearranges).
narrative_ontology:founding_problem_status(temple_sacrifice_obligation__study_as_occupation, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(temple_sacrifice_obligation__study_as_occupation, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(temple_sacrifice_obligation__study_as_occupation, 'none', 1).
narrative_ontology:epsilon_provenance(temple_sacrifice_obligation__study_as_occupation, 0.15, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness is low (0.15) because study is voluntary, meritorious, and carries no coercive transfer — participants gain spiritual/status benefits. Suppression is minimal (0.10) because no one is prevented from alternative fulfillments (prayer, charity, messianic expectation). Theater ratio (0.30) reflects that while study is genuine learning, a growing share of institutional energy performs the occupation ritual (dedicated Kodashim sedarim, siyumim, funding structures) rather than engaging the sacrificial corpus substantively. Accessibility collapse (0.40) is moderate: alternatives (prayer-as-service, messianic suspension) exist but the study reading dominates institutional halakha. Resistance (0.15) is low: the reading is widely accepted across Orthodox communities, with dissent confined to theoretical debate.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seats (yeshivas, poskim), the constraint is genuine coordination solving the impossibility crisis. From the excluded seats (messianic expectants, archival scholars), the same structure appears as a premature closure that obscures the obligation's true status. The engine computes this divergence from the structural data: beneficiaries experience low χ, excluded seats experience the constraint as a foreclosure of their reading.
 *
 * DIRECTIONALITY LOGIC:
 *   Torah scholars and yeshiva institutions sit near the beneficiary end (d ~ 0.15): they gain status, resources, and authority from administering the study regime. The observant community is near-symmetric (d ~ 0.45): they fund the institutions but receive identity continuity and ritual coherence. Halakhic authorities are analytical (d = 0.5): they interpret but do not personally extract. Messianic expectants and archival scholars are excluded — their directionality is not computed by this constraint's operation because they reject its core claim. No stakeholder bears net extraction; the constraint operates as pure coordination.
 *
 * MANDATROPHY ANALYSIS:
 *   The reading prevents mislabeling by declaring the founding problem (Temple destruction) as 'contested' — some authorities hold study completely resolves it, others hold the problem persists until restoration. The mandate (sacrificial obligation) has not atrophied because the reading claims active fulfillment, not suspension. However, the rising theater_ratio over 1950 years suggests performative maintenance may be replacing substantive engagement, a drift the mandatrophy lens would flag for investigation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Does this constraint instantiate a distinct reading of the temple_sacrifice_obligation kernel, or is it a policy variant within a single reading?',
    'Compare the structural commitments: if study_as_occupation claims the obligation IS fulfilled (no residual duty), while messianic_suspension claims it IS NOT fulfilled (duty suspended), they occupy different logical positions in the same kernel — distinct readings. If both agree the obligation remains unfulfilled but differ on what to do meanwhile, they are policy variants.',
    'If distinct readings, each gets its own ε and classification; if variants, they share a constraint with internal tension. The ε-invariance principle requires decomposition when observables yield different ε.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether the three declared positions are separate kernel readings or intra-reading variants.').

omega_variable(
    fulfillment_vs_preservation_boundary,
    'Where does the study_as_occupation reading draw the line between sacrifices whose obligation is fulfilled by study versus those requiring physical performance?',
    'Halakhic analysis of which korbanot (sacrifices) the Talmudic dictum covers: Menachot 110a mentions olah, minchat, chatat, asham — but what of communal sacrifices (temidim, musafim) or purification rites (parah adumah)? Poskim disagree; mapping the boundary would clarify the constraint''s scope.',
    'If the reading covers all sacrifices, its coordination function is total (ε remains low). If it covers only individual sacrifices, communal obligations remain unfulfilled, creating a residual extraction gap (higher ε for communal dimension).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fulfillment_vs_preservation_boundary, empirical, 'Scope of the fulfillment claim across sacrificial categories.').

omega_variable(
    interpretive_layer_absorption_capacity,
    'How much drift can the interpretive layer absorb before the authority structure must surface revision?',
    'Track halakhic responsa over time: when new impossibilities arise (e.g., inability to identify kohanim, loss of techelet, Temple Mount access restrictions), does the study reading expand to cover them, or does the authority structure acknowledge a limit?',
    'If absorption capacity is unlimited, the constraint behaves as a mountain (invariant). If finite, it behaves as a scaffold with an implicit sunset (messianic restoration). The current theater_ratio rise suggests absorption is becoming performative.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(interpretive_layer_absorption_capacity, conceptual, 'Whether the interpretive layer''s drift-absorption has structural limits.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(temple_sacrifice_obligation__study_as_occupation, 0, 1950).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tso_study_tr_t0, temple_sacrifice_obligation__study_as_occupation, theater_ratio, 0, 0.15).
narrative_ontology:measurement(tso_study_tr_t400, temple_sacrifice_obligation__study_as_occupation, theater_ratio, 400, 0.2).
narrative_ontology:measurement(tso_study_tr_t800, temple_sacrifice_obligation__study_as_occupation, theater_ratio, 800, 0.25).
narrative_ontology:measurement(tso_study_tr_t1200, temple_sacrifice_obligation__study_as_occupation, theater_ratio, 1200, 0.28).
narrative_ontology:measurement(tso_study_tr_t1600, temple_sacrifice_obligation__study_as_occupation, theater_ratio, 1600, 0.3).
narrative_ontology:measurement(tso_study_tr_t1950, temple_sacrifice_obligation__study_as_occupation, theater_ratio, 1950, 0.3).

% Extraction over time
narrative_ontology:measurement(tso_study_be_t0, temple_sacrifice_obligation__study_as_occupation, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(tso_study_be_t400, temple_sacrifice_obligation__study_as_occupation, base_extractiveness, 400, 0.12).
narrative_ontology:measurement(tso_study_be_t800, temple_sacrifice_obligation__study_as_occupation, base_extractiveness, 800, 0.13).
narrative_ontology:measurement(tso_study_be_t1200, temple_sacrifice_obligation__study_as_occupation, base_extractiveness, 1200, 0.14).
narrative_ontology:measurement(tso_study_be_t1600, temple_sacrifice_obligation__study_as_occupation, base_extractiveness, 1600, 0.15).
narrative_ontology:measurement(tso_study_be_t1950, temple_sacrifice_obligation__study_as_occupation, base_extractiveness, 1950, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(tso_study_su_t0, temple_sacrifice_obligation__study_as_occupation, suppression_requirement, 0, 0.08).
narrative_ontology:measurement(tso_study_su_t400, temple_sacrifice_obligation__study_as_occupation, suppression_requirement, 400, 0.09).
narrative_ontology:measurement(tso_study_su_t800, temple_sacrifice_obligation__study_as_occupation, suppression_requirement, 800, 0.1).
narrative_ontology:measurement(tso_study_su_t1200, temple_sacrifice_obligation__study_as_occupation, suppression_requirement, 1200, 0.1).
narrative_ontology:measurement(tso_study_su_t1600, temple_sacrifice_obligation__study_as_occupation, suppression_requirement, 1600, 0.1).
narrative_ontology:measurement(tso_study_su_t1950, temple_sacrifice_obligation__study_as_occupation, suppression_requirement, 1950, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(temple_sacrifice_obligation__study_as_occupation, identity_coordination).
narrative_ontology:boltzmann_floor_override(temple_sacrifice_obligation__study_as_occupation, 0.08).
narrative_ontology:affects_constraint(temple_sacrifice_obligation__study_as_occupation, temple_sacrifice_obligation__messianic_suspension).
narrative_ontology:affects_constraint(temple_sacrifice_obligation__study_as_occupation, temple_sacrifice_obligation__study_as_archiving).

% DUAL FORMULATION NOTE:
% This constraint family decomposes the temple_sacrifice_obligation kernel into three readings with distinct ε values and beneficiary structures. study_as_occupation (this story) claims low ε, no victims, coordination fulfillment. messianic_suspension claims near-zero ε (obligation suspended, no current operation), different beneficiary structure (messianic hope). study_as_archiving claims low ε but different transfer function (knowledge preservation, not fulfillment). All three link via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
