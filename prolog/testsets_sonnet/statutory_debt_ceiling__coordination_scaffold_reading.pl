% ============================================================================
% CONSTRAINT STORY: statutory_debt_ceiling__coordination_scaffold_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_statutory_debt_ceiling__coordination_scaffold_reading, []).

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
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: statutory_debt_ceiling__coordination_scaffold_reading
 *   human_readable: Statutory Debt Ceiling as Treasury Operational Coordination Mechanism
 *   domain: constitutional/political economy/fiscal governance
 *
 * SUMMARY:
 *   This story instantiates the coordination_scaffold_reading of the
 *   statutory debt ceiling kernel: the ceiling as a legislative housekeeping
 *   device that consolidates authorization for federal borrowing into a
 *   single periodically-adjusted aggregate limit, sparing Congress from
 *   voting on each individual Treasury debt instrument. Under this reading,
 *   routine adjustments (the vast majority of the ~100+ ceiling changes since
 *   1917) are low-friction, low-extraction procedural events. This reading
 *   brackets, rather than denies, the contested episodes (2011, 2013, 2023)
 *   where the same statutory mechanism was used for leverage — those episodes
 *   are the subject of the sibling extraction_snare_reading, a structurally
 *   distinct constraint with its own ε.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(statutory_debt_ceiling__coordination_scaffold_reading, 0.22).
domain_priors:suppression_score(statutory_debt_ceiling__coordination_scaffold_reading, 0.18).
domain_priors:theater_ratio(statutory_debt_ceiling__coordination_scaffold_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(statutory_debt_ceiling__coordination_scaffold_reading, extractiveness, 0.22).
narrative_ontology:constraint_metric(statutory_debt_ceiling__coordination_scaffold_reading, suppression_requirement, 0.18).
narrative_ontology:constraint_metric(statutory_debt_ceiling__coordination_scaffold_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(statutory_debt_ceiling__coordination_scaffold_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(statutory_debt_ceiling__coordination_scaffold_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(statutory_debt_ceiling__coordination_scaffold_reading, scaffold).
narrative_ontology:human_readable(statutory_debt_ceiling__coordination_scaffold_reading, "Statutory Debt Ceiling as Treasury Operational Coordination Mechanism").
narrative_ontology:topic_domain(statutory_debt_ceiling__coordination_scaffold_reading, "constitutional/political economy/fiscal governance").

domain_priors:requires_active_enforcement(statutory_debt_ceiling__coordination_scaffold_reading).
narrative_ontology:has_sunset_clause(statutory_debt_ceiling__coordination_scaffold_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(statutory_debt_ceiling__coordination_scaffold_reading, '5476f2e0-51fb-4ef5-b552-6d5b60d43070').
narrative_ontology:cs_kernel_codification('5476f2e0-51fb-4ef5-b552-6d5b60d43070', formalized).
narrative_ontology:cs_authority_grounding('5476f2e0-51fb-4ef5-b552-6d5b60d43070', practice).
narrative_ontology:cs_interpretation_layer_present('5476f2e0-51fb-4ef5-b552-6d5b60d43070').
narrative_ontology:cs_reading_relation('5476f2e0-51fb-4ef5-b552-6d5b60d43070', statutory_debt_ceiling__extraction_snare_reading, coexists_with).
narrative_ontology:cs_reading_relation('5476f2e0-51fb-4ef5-b552-6d5b60d43070', statutory_debt_ceiling__constitutional_nullity_reading, influences).
narrative_ontology:cs_axiom('5476f2e0-51fb-4ef5-b552-6d5b60d43070', foundational, aggregate_limit_is_genuine_administrative_efficiency).
narrative_ontology:cs_axiom_status(aggregate_limit_is_genuine_administrative_efficiency, holdable).
narrative_ontology:cs_axiom_grounding('5476f2e0-51fb-4ef5-b552-6d5b60d43070', aggregate_limit_is_genuine_administrative_efficiency, instrumental).
narrative_ontology:cs_axiom('5476f2e0-51fb-4ef5-b552-6d5b60d43070', secondary, periodic_adjustment_preserves_meaningful_congressional_oversight).
narrative_ontology:cs_axiom_status(periodic_adjustment_preserves_meaningful_congressional_oversight, holdable).
narrative_ontology:cs_axiom_grounding('5476f2e0-51fb-4ef5-b552-6d5b60d43070', periodic_adjustment_preserves_meaningful_congressional_oversight, conventional).
narrative_ontology:cs_reference_frame('5476f2e0-51fb-4ef5-b552-6d5b60d43070', aggregate_authorization_consolidation_1917).
narrative_ontology:cs_drift_state('5476f2e0-51fb-4ef5-b552-6d5b60d43070', post_2011_brinkmanship_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('5476f2e0-51fb-4ef5-b552-6d5b60d43070', '').
narrative_ontology:cs_kernel_id(statutory_debt_ceiling__coordination_scaffold_reading, statutory_debt_ceiling).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(statutory_debt_ceiling__coordination_scaffold_reading, treasury_department).
narrative_ontology:constraint_beneficiary(statutory_debt_ceiling__coordination_scaffold_reading, congress_appropriators).
narrative_ontology:constraint_beneficiary(statutory_debt_ceiling__coordination_scaffold_reading, bond_market_participants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(statutory_debt_ceiling__coordination_scaffold_reading, federal_agencies_and_beneficiary_programs).
narrative_ontology:constraint_vindicates(statutory_debt_ceiling__coordination_scaffold_reading, legislative_power_of_the_purse_doctrine).
narrative_ontology:constraint_vindicates(statutory_debt_ceiling__coordination_scaffold_reading, aggregate_borrowing_authority_efficiency).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Manages federal borrowing operations under the aggregate ceiling set by statute, using extraordinary measures to smooth cash management when the limit approaches. In this reading, the ceiling gives Treasury a single, periodically-adjusted authorization to issue debt rather than requiring bond-by-bond or program-by-program congressional sign-off, letting day-to-day debt management run administratively within a legislatively-set aggregate.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__coordination_scaffold_reading, treasury_department, agenda_setter,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(statutory_debt_ceiling__coordination_scaffold_reading, treasury_department, beneficiary).

% Sets appropriations and tax law that determine actual borrowing need, then periodically raises or suspends the ceiling to match. Retains the power-of-the-purse review point without having to authorize every individual debt instrument, preserving oversight at the aggregate level rather than the transactional level.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__coordination_scaffold_reading, congress_appropriators, agenda_setter,
    institutional, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(statutory_debt_ceiling__coordination_scaffold_reading, congress_appropriators, beneficiary).

% Rely on predictable, rule-bound Treasury issuance to price and hold federal debt. Under routine (non-crisis) operation, the periodic ceiling adjustment process is a known calendar event priced into markets well in advance, rather than a source of ongoing uncertainty.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__coordination_scaffold_reading, bond_market_participants, beneficiary,
    organized, immediate, mobile, global).

% Depend on continued, uninterrupted federal disbursements that Treasury's borrowing authority makes possible. When the ceiling process functions as a routine coordination step rather than a standoff, disbursements continue without disruption to program beneficiaries.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__coordination_scaffold_reading, federal_agencies_and_beneficiary_programs, beneficiary,
    moderate, immediate, trapped, national).

% Bear the long-run fiscal consequences of aggregate federal debt levels but are not party to the periodic adjustment process itself. In the coordination reading, this seat mainly experiences the ceiling as background legislative housekeeping, not as a live threat to their own transactions.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__coordination_scaffold_reading, general_public_taxpayers, observer,
    powerless, generational, trapped, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(statutory_debt_ceiling__coordination_scaffold_reading, diffuse).
narrative_ontology:fixing_cost_class(statutory_debt_ceiling__coordination_scaffold_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides Congress a single periodic checkpoint (the aggregate ceiling adjustment) to affirm the borrowing implied by its own appropriations and tax decisions, instead of requiring a fresh congressional vote to authorize each individual Treasury debt issuance, note, or bond sale.
% TRANSFER_FUNCTION: Under this reading, the mechanism does not transfer value between parties — it allocates a review checkpoint: legislative oversight is concentrated at the aggregate-limit-setting moment rather than distributed across every individual debt transaction.
% ABSENT_VOICES: Foreign central banks and institutional holders of Treasury securities have a strong interest in the ceiling functioning as routine housekeeping rather than a periodic hostage-taking event, but they participate in the process only indirectly, through market pricing signals rather than through the legislative process itself.
% DISAPPEARANCE_RATIONALE: Under the coordination reading, removing the statutory ceiling would return borrowing-authorization review to whatever mechanism replaced it (bond-by-bond authorization, or reliance on appropriations bills alone as implicit authorization) — a real administrative rearrangement for Treasury's issuance calendar, though a modest one. Whether the world meaningfully rearranges depends heavily on which sibling reading of the kernel is correct; under this reading alone the change is procedural rather than fiscal.
% FOUNDING_PROBLEM: Prior to 1917, each individual bond issuance required specific congressional authorization, which was administratively unworkable for wartime and modern financing needs; the Second Liberty Bond Act consolidated authorization into a single aggregate limit so Treasury could manage debt issuance operationally within a legislatively-set ceiling.
% FOUNDING_PROBLEM_CORROBORATION: Congressional Research Service reports and Treasury Department historical accounts (both outside any single beneficiary's self-interest) corroborate the 1917 administrative-consolidation origin. However, government accountability offices and independent budget scholars outside Treasury and congressional leadership increasingly attest that the founding administrative-convenience problem has been substantially solved by modern cash-management tools, and that the ceiling's contemporary function has drifted toward periodic political leverage rather than pure coordination — which is precisely the contest this reading brackets rather than resolves.
narrative_ontology:disappearance_verdict(statutory_debt_ceiling__coordination_scaffold_reading, contested).
narrative_ontology:founding_problem_status(statutory_debt_ceiling__coordination_scaffold_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(statutory_debt_ceiling__coordination_scaffold_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(statutory_debt_ceiling__coordination_scaffold_reading, 'none', 1).
narrative_ontology:epsilon_provenance(statutory_debt_ceiling__coordination_scaffold_reading, 0.22, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(statutory_debt_ceiling__coordination_scaffold_reading_tests).
:- end_tests(statutory_debt_ceiling__coordination_scaffold_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low (0.22) and rising only modestly, reflecting the coordination reading's claim that most historical operation has been routine administrative adjustment. Theater ratio rises over the interval (0.10 to 0.40) because even under the coordination reading, an increasing share of ceiling-adjustment episodes since the 1980s involve public political theater (debt-limit brinkmanship rhetoric) layered atop what remains, in most years, procedurally routine action — this is descriptively honest without conceding the extraction reading's core claim. Accessibility collapse is moderate (0.35): Congress always retains the alternative of repealing or restructuring the mechanism, so alternatives have not fully collapsed, consistent with a scaffold rather than a mountain.
 *
 * DIRECTIONALITY LOGIC:
 *   Treasury and congressional appropriators are the structural beneficiaries under this reading — the ceiling reduces their transaction costs (Treasury avoids per-issuance authorization; Congress avoids per-issuance voting). Bond market participants and dependent federal programs benefit from the predictability the routine-adjustment pattern provides. No victim group is named in this reading, consistent with its claim that the mechanism is genuine low-extraction coordination rather than extraction — the sibling extraction_snare_reading names distinct victims (federal beneficiaries during shutdown brinkmanship, holders of short-term Treasury bills during near-default episodes) that belong to that separate constraint, not this one.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (administrative unworkability of per-bond congressional authorization) is largely solved by modern Treasury cash-management infrastructure, yet the statutory ceiling persists — this is the seed of a mandatrophy question, but the coordination reading holds that the mechanism still performs a live, if narrower, function: providing Congress a periodic checkpoint to affirm implied borrowing from its own appropriations decisions. Whether that checkpoint function is worth its now-demonstrated theatrical costs is exactly the question this reading defers to the founding_problem_status: contested field rather than resolving unilaterally.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    routine_vs_brinkmanship_episode_classification,
    'What fraction of the ~103 historical debt-ceiling adjustments since 1917 were genuinely routine administrative actions versus politically leveraged brinkmanship episodes, and does that fraction support characterizing the mechanism''s PRIMARY historical operation as coordination rather than extraction?',
    'Systematic historical coding of each ceiling-adjustment episode against objective brinkmanship indicators (near-default market signals, explicit legislative demands conditioned on the vote, credit rating agency warnings) rather than post-hoc narrative framing.',
    'If routine episodes vastly outnumber brinkmanship episodes, this reading''s low-ε coordination characterization is well-supported as the modal case; if brinkmanship episodes are increasing in frequency and severity, the coordination reading may describe a vanishing historical pattern rather than a stable current one.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(routine_vs_brinkmanship_episode_classification, empirical, 'Whether routine coordination or extraction is the historically dominant operating mode of the statutory ceiling.').

omega_variable(
    kernel_reading_selection_criterion,
    'Is there a principled, non-question-begging criterion for selecting the coordination_scaffold_reading over the extraction_snare_reading or constitutional_nullity_reading as the operative characterization of THE statutory debt ceiling — or are these genuinely coexisting readings held by different institutional actors with no fact of the matter resolving which is ''true''?',
    'This is likely irreducibly conceptual: it depends on what baseline of comparison is used (per-episode brinkmanship frequency, aggregate fiscal consequence, constitutional theory of the borrowing power). No empirical study fully resolves it because the three readings partition the same statutory text along different analytical dimensions (procedural function, extraction dynamics, and constitutional validity respectively).',
    'If the extraction_snare_reading is judged dominant, then treating this coordination reading as descriptively primary understates the mechanism''s actual political-economy function; if the coordination reading is dominant, the extraction episodes are better modeled as periodic capture events layered atop a genuinely low-extraction baseline structure, which is exactly the modeling choice this file makes.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection_criterion, conceptual, 'Whether a single dominant reading of the debt-ceiling kernel exists or whether the readings are genuinely and permanently plural.').

omega_variable(
    theater_ratio_drift_interpretation,
    'Does the rising theater_ratio (0.10 to 0.40 over the interval) indicate that even routine ceiling adjustments have been increasingly captured by performative political conflict, suggesting a drift toward the extraction_snare_reading over time — i.e., is this reading itself becoming less descriptively accurate as a characterization of the CURRENT mechanism, even if it accurately described the mechanism circa 1917-1980?',
    'T17-style temporal decomposition of the theater_ratio series against the extraction_snare_reading''s own extractiveness trajectory, to test whether the two readings'' metrics are converging over time (suggesting kernel-level drift) or remaining stably distinct (supporting the two-reading partition as historically stable).',
    'If convergence is found, it would suggest the kernel itself has structurally shifted over the 20th-21st century, and a single coordination_scaffold_reading may not adequately characterize the post-2011 period even under this reading''s own terms — possibly warranting a third temporal split (pre-1980 vs post-1980 coordination readings) rather than treating the interval as one continuous ε.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theater_ratio_drift_interpretation, empirical, 'Whether the coordination reading''s descriptive accuracy is itself time-varying and possibly degrading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(statutory_debt_ceiling__coordination_scaffold_reading, 1917, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t1917, statutory_debt_ceiling__coordination_scaffold_reading, theater_ratio, 1917, 0.1).
narrative_ontology:measurement_basis(stat_tr_t1917, observed).
narrative_ontology:measurement(stat_tr_t1960, statutory_debt_ceiling__coordination_scaffold_reading, theater_ratio, 1960, 0.18).
narrative_ontology:measurement_basis(stat_tr_t1960, observed).
narrative_ontology:measurement(stat_tr_t1985, statutory_debt_ceiling__coordination_scaffold_reading, theater_ratio, 1985, 0.25).
narrative_ontology:measurement_basis(stat_tr_t1985, observed).
narrative_ontology:measurement(stat_tr_t2000, statutory_debt_ceiling__coordination_scaffold_reading, theater_ratio, 2000, 0.3).
narrative_ontology:measurement_basis(stat_tr_t2000, observed).
narrative_ontology:measurement(stat_tr_t2013, statutory_debt_ceiling__coordination_scaffold_reading, theater_ratio, 2013, 0.38).
narrative_ontology:measurement_basis(stat_tr_t2013, observed).
narrative_ontology:measurement(stat_tr_t2024, statutory_debt_ceiling__coordination_scaffold_reading, theater_ratio, 2024, 0.4).
narrative_ontology:measurement_basis(stat_tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(stat_be_t1917, statutory_debt_ceiling__coordination_scaffold_reading, base_extractiveness, 1917, 0.08).
narrative_ontology:measurement_basis(stat_be_t1917, observed).
narrative_ontology:measurement(stat_be_t1960, statutory_debt_ceiling__coordination_scaffold_reading, base_extractiveness, 1960, 0.1).
narrative_ontology:measurement_basis(stat_be_t1960, observed).
narrative_ontology:measurement(stat_be_t1985, statutory_debt_ceiling__coordination_scaffold_reading, base_extractiveness, 1985, 0.13).
narrative_ontology:measurement_basis(stat_be_t1985, observed).
narrative_ontology:measurement(stat_be_t2000, statutory_debt_ceiling__coordination_scaffold_reading, base_extractiveness, 2000, 0.15).
narrative_ontology:measurement_basis(stat_be_t2000, observed).
narrative_ontology:measurement(stat_be_t2013, statutory_debt_ceiling__coordination_scaffold_reading, base_extractiveness, 2013, 0.19).
narrative_ontology:measurement_basis(stat_be_t2013, observed).
narrative_ontology:measurement(stat_be_t2024, statutory_debt_ceiling__coordination_scaffold_reading, base_extractiveness, 2024, 0.22).
narrative_ontology:measurement_basis(stat_be_t2024, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(statutory_debt_ceiling__coordination_scaffold_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(statutory_debt_ceiling__coordination_scaffold_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(statutory_debt_ceiling__coordination_scaffold_reading, 0.12).
narrative_ontology:affects_constraint(statutory_debt_ceiling__coordination_scaffold_reading, extraction_snare_reading).
narrative_ontology:affects_constraint(statutory_debt_ceiling__coordination_scaffold_reading, constitutional_nullity_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the statutory_debt_ceiling kernel. coordination_scaffold_reading (this file) treats the ceiling as low-extraction administrative consolidation; extraction_snare_reading treats the same statutory text as a weaponized boundary for legislative-minority extraction under default threat, with distinct beneficiaries/victims and a substantially higher ε; constitutional_nullity_reading treats the entire kernel as constitutionally void under the 14th Amendment Section 4 public debt clause, which if adopted would dissolve the coordination function modeled here. The three files are not to be averaged or reconciled — each has independent ε, independent stakeholders, and independent classification, per the ε-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
