% ============================================================================
% CONSTRAINT STORY: state_commitment_installation_mechanism__exogenous_imposition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_state_commitment_installation_mechanism__exogenous_imposition_reading, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: state_commitment_installation_mechanism__exogenous_imposition_reading
 *   human_readable: Top-Down Commitment Installation by Transformation Mandate (Exogenous Imposition Reading)
 *   domain: historical_sociology/state_formation/cultural_authority
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the kernel 'how new commitments
 *   gain legitimacy': the exogenous imposition reading, on which legitimacy
 *   flows from top-down installation by an authority holding a transformation
 *   mandate. The standing arrangement under contest — and therefore the
 *   referent of epsilon — is the decree-installation mechanism itself,
 *   assessed by this reading's own lights: the authority's mandate suffices
 *   to confer legitimacy, uptake follows the decree, and no grassroots
 *   advocacy participates. The reading's endorsed alternative (whatever
 *   climbs endogenously) is NOT the referent. Per the epsilon-invariance
 *   principle, the colloquial label 'how new commitments gain legitimacy'
 *   decomposes into a three-member constraint family: this reading (abrupt
 *   adoption via decree, state as beneficiary, resistance at base — the
 *   highest-extraction member, epsilon 0.68, because validation from below is
 *   bypassed entirely and capture concentrates at the apex); the endogenous
 *   climb reading (legitimacy earned through demonstrated superiority at the
 *   fringes — participation voluntary, exits open, extraction
 *   near-negligible); and the hybrid cascade reading (apex installation
 *   cascading down but stabilized by fringe validation — intermediate
 *   epsilon, victims confined to episodes where validation is suppressed).
 *   Each member is a separate file with its own epsilon, beneficiaries, and
 *   victims; all are linked via network.affects_constraints.
 *
 * KEY AGENTS:
 *   - transformation_authority: agenda-setter and primary beneficiary (institutional/constrained) — issues the decree, builds the enforcement machinery, collects legitimacy and administrative reach
 *   - aligned_administrative_elites: secondary beneficiary (powerful/mobile) — staff the conformity apparatus, collect offices and patronage, carry implementation load and purge risk
 *   - base_population_subjects: primary target (powerless/trapped) — receive the decree as binding, bear transition costs and enforcement visits
 *   - legacy_commitment_communities: concentrated target (moderate/identity_locked) — custodians of the displaced commitment whose practice becomes registered nonconformity
 *   - grassroots_reform_advocates: excluded voice (organized/constrained) — built the bottom-up case the decree preempts; never consulted on design or sequencing
 *   - comparative_historical_scholars: analytical observer (analytical/analytical) — compare adoption sequences across episodes and adjudicate between mechanism-stories
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_commitment_installation_mechanism__exogenous_imposition_reading, 0.68).
domain_priors:suppression_score(state_commitment_installation_mechanism__exogenous_imposition_reading, 0.66).
domain_priors:theater_ratio(state_commitment_installation_mechanism__exogenous_imposition_reading, 0.34).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__exogenous_imposition_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__exogenous_imposition_reading, suppression_requirement, 0.66).
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__exogenous_imposition_reading, theater_ratio, 0.34).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__exogenous_imposition_reading, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__exogenous_imposition_reading, resistance, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_commitment_installation_mechanism__exogenous_imposition_reading, tangled_rope).
narrative_ontology:human_readable(state_commitment_installation_mechanism__exogenous_imposition_reading, "Top-Down Commitment Installation by Transformation Mandate (Exogenous Imposition Reading)").
narrative_ontology:topic_domain(state_commitment_installation_mechanism__exogenous_imposition_reading, "historical_sociology/state_formation/cultural_authority").

domain_priors:requires_active_enforcement(state_commitment_installation_mechanism__exogenous_imposition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_commitment_installation_mechanism__exogenous_imposition_reading, '672671f9-ce4f-40ac-8d4f-af031d6470fe').
narrative_ontology:cs_kernel_codification('672671f9-ce4f-40ac-8d4f-af031d6470fe', distributed).
narrative_ontology:cs_authority_grounding('672671f9-ce4f-40ac-8d4f-af031d6470fe', distributed).
narrative_ontology:cs_reading_relation('672671f9-ce4f-40ac-8d4f-af031d6470fe', state_commitment_installation_mechanism__endogenous_climb_reading, coexists_with).
narrative_ontology:cs_reading_relation('672671f9-ce4f-40ac-8d4f-af031d6470fe', state_commitment_installation_mechanism__hybrid_cascade_reading, coexists_with).
narrative_ontology:cs_axiom('672671f9-ce4f-40ac-8d4f-af031d6470fe', foundational, mandate_installation_confers_legitimacy).
narrative_ontology:cs_axiom_status(mandate_installation_confers_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('672671f9-ce4f-40ac-8d4f-af031d6470fe', mandate_installation_confers_legitimacy, empirically_contingent).
narrative_ontology:cs_axiom('672671f9-ce4f-40ac-8d4f-af031d6470fe', secondary, transformation_mandate_authorizes_consensus_bypass).
narrative_ontology:cs_axiom_status(transformation_mandate_authorizes_consensus_bypass, holdable).
narrative_ontology:cs_axiom_grounding('672671f9-ce4f-40ac-8d4f-af031d6470fe', transformation_mandate_authorizes_consensus_bypass, conventional).
narrative_ontology:cs_reference_frame('672671f9-ce4f-40ac-8d4f-af031d6470fe', decree_precedence_legitimacy_order).
narrative_ontology:cs_drift_state('672671f9-ce4f-40ac-8d4f-af031d6470fe', contemporary_comparative_historical_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('672671f9-ce4f-40ac-8d4f-af031d6470fe', '').
narrative_ontology:cs_kernel_id(state_commitment_installation_mechanism__exogenous_imposition_reading, state_commitment_installation_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_commitment_installation_mechanism__exogenous_imposition_reading, transformation_authority).
narrative_ontology:constraint_beneficiary(state_commitment_installation_mechanism__exogenous_imposition_reading, aligned_administrative_elites).
narrative_ontology:constraint_victim(state_commitment_installation_mechanism__exogenous_imposition_reading, base_population_subjects).
narrative_ontology:constraint_victim(state_commitment_installation_mechanism__exogenous_imposition_reading, legacy_commitment_communities).
narrative_ontology:constraint_vindicates(state_commitment_installation_mechanism__exogenous_imposition_reading, transformation_mandate_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Issues the decree installing the new commitment, builds the inspection and penal machinery that administers conformity, appoints the officials who carry it out, and collects the resulting legitimacy, administrative reach, and unification of allegiance. Its own credibility is staked on the program's success, so halting mid-course carries severe political cost even though nothing external forces it to continue.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__exogenous_imposition_reading, transformation_authority, agenda_setter,
    institutional, generational, constrained, national).

% Staff the ministries, courts, schools, and registries through which conformity is administered. Advancement tracks demonstrated loyalty to the installed commitment; they collect offices, salaries, and patronage. Their gain is contingent: they carry the implementation workload and face demotion or purge if the program falters or changes course, and their skills travel to other posts and jurisdictions.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__exogenous_imposition_reading, aligned_administrative_elites, beneficiary,
    powerful, biographical, mobile, national).

% Receive the decree as binding before any deliberation occurs. They must adopt the new script, calendar, dress, oath, tax form, or mode of worship under penalty, absorb the transition costs of replacing familiar practice, and host the enforcement visits that verify compliance. Leaving the jurisdiction is prohibitively expensive, and their prior commitments lose official standing overnight.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__exogenous_imposition_reading, base_population_subjects, payer,
    powerless, immediate, trapped, national).

% Custodians of the displaced commitment: clergy of the old rite, teachers of the old script, guild masters of the old craft order. Their institutions lose legal standing and their practice becomes registered nonconformity. Exiting would mean abandoning the identity they exist to transmit, so they absorb persecution, retreat into quietism, or migrate internally while keeping the commitment alive below the surface.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__exogenous_imposition_reading, legacy_commitment_communities, payer,
    moderate, generational, identity_locked, regional).

% Local modernizers who had been building the case for the new commitment from below through demonstration projects, pamphlets, and model communities. The decree arrives over their heads: they gain an ally in the content but lose ownership of the process, and are never consulted on design, sequencing, or accommodation of those who resist.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__exogenous_imposition_reading, grassroots_reform_advocates, excluded,
    organized, biographical, constrained, national).

% Compare adoption sequences across transformation episodes — script reforms, legal codifications, religious reimpositions, secularization campaigns — to determine whether durable uptake followed the decree or preceded it. They read the archives of every seat, publish classifications of episodes, and their findings shift which mechanism-story commands assent.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__exogenous_imposition_reading, comparative_historical_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(state_commitment_installation_mechanism__exogenous_imposition_reading, transformation_authority).
narrative_ontology:fixing_cost_class(state_commitment_installation_mechanism__exogenous_imposition_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Achieves uniform normative commitment across a large, recently consolidated jurisdiction within a single political generation — solving the collective-action problem that decentralized convergence answers only over centuries and leaves enclaves of divergence that rival centers of allegiance can exploit.
% TRANSFER_FUNCTION: Moves legitimacy, compliance, and administrative reach upward to the mandating authority; moves transition costs, enforcement burdens, and the displacement of inherited practice downward onto the base population and the legacy commitment communities.
% ABSENT_VOICES: Two voices are outside the room by design: grassroots advocates of the new commitment, who would have argued for participatory sequencing and owned implementation, and stewards of the legacy commitment, who would have argued for accommodations of conscience. The decree precedes consultation; unanimity at the apex reflects the absence of both, not their agreement.
% DISAPPEARANCE_RATIONALE: If the decree-installation channel vanished overnight, new commitments would have to climb from demonstrated local superiority — slower, patchier, contested at every rung. States would lose their principal instrument of rapid normative unification; script reform, legal codification, and religious realignment would stretch across generations, stall, or fail, and the administrative economies built on uniform commitment would reorganize around negotiated or federated diversity.
% FOUNDING_PROBLEM: Post-consolidation fragmentation: a newly unified or crisis-forged polity inherits divergent laws, rites, scripts, calendars, and loyalties that impede administration, complicate taxation and conscription, and invite rival centers of allegiance.
% FOUNDING_PROBLEM_CORROBORATION: Comparative-historical scholarship outside the benefiting parties corroborates that the fragmentation problem was real in the canonical episodes — administrative records, consular reports, and missionary correspondence document the coordination failures consolidation faced. The same literature, however, disputes whether decree-installation was necessary to solve it, attesting the problem while contesting the mechanism.
narrative_ontology:disappearance_verdict(state_commitment_installation_mechanism__exogenous_imposition_reading, world_rearranges).
narrative_ontology:founding_problem_status(state_commitment_installation_mechanism__exogenous_imposition_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(state_commitment_installation_mechanism__exogenous_imposition_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(state_commitment_installation_mechanism__exogenous_imposition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(state_commitment_installation_mechanism__exogenous_imposition_reading, 0.68, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(state_commitment_installation_mechanism__exogenous_imposition_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(state_commitment_installation_mechanism__exogenous_imposition_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(state_commitment_installation_mechanism__exogenous_imposition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.68 at interval end) because the legitimacy and compliance the mechanism produces flow to the apex while the costs of transition land on those never consulted; the rate is set by the mandate, not by any negotiation with the governed. Suppression (0.66) is a raw structural property, unscaled by power or scope: persistence depends on inspection regimes, penalties for nonconformity, and the criminalization of the legacy commitment's public practice — the mechanism does not hold by participant preference. Theater ratio (0.34) is moderate: the installation does real work (uniform registers, working courts, functioning schools in the new form), but a growing share of activity is ceremonial legitimation — oaths, festivals, staged conversions — substituting for uptake that stalls at the base. Accessibility collapse (0.52) is middling: the decree forecloses public alternatives but private divergence persists in households, villages, and memory, which is precisely why enforcement must continue. Resistance (0.64) is high and constitutive of this reading's expected structural delta: resistance at the base is not noise but the predictable response of trapped payers and identity-locked custodians. The measurement series run on one shared time grid (T=0..40, six points, every tracked metric authored at every point): suppression_requirement rises through mid-program as enforcement machinery matures, then partially decays as compliance habituates; extraction creeps up and plateaus; theater grows steadily as ceremony replaces stalled uptake. Claim and metrics are independent authored facts: claimed_type is tangled_rope because the mechanism genuinely solves a coordination problem (rapid standardization) while asymmetrically extracting through enforced adoption — the engine computes per-seat types from the structural data regardless of this claim.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the transformation_authority's position the arrangement is the coordination instrument it built and the vehicle of its mandate — rapid unification where drift would mean dissolution. From the base_population_subjects' position the same structure operates as an unconsulted transfer of costs under penalty. The aligned_administrative_elites sit between: beneficiaries whose gain is contingent on the program surviving. The engine derives these divergent classifications from the declared roles, power atoms, and exit options; this commentary does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   The transformation_authority sits nearest the beneficiary pole (d near 0.0): it collects the legitimacy gains and controls the rules. Aligned_administrative_elites derive low d from their beneficiary role, but the derivation alone would overstate their subsidy — hence the explicit override to d=0.22: their gain is contingent on program survival, they carry the implementation workload, and they face purge risk if the mandate shifts; they are beneficiaries with skin in the game, not pure collectors. Base_population_subjects sit near the full-target pole (d near 1.0): trapped payers bearing the transfer with no arbitrage-grade exit. Legacy_commitment_communities sit at or beyond the full-target pole: identity_lock means exit would dissolve the agent as constituted, so effective extraction is amplified past what a mobile payer would experience. Grassroots_reform_advocates are excluded rather than coordinated — the mechanism's defining move is proceeding without them, and their exclusion is what distinguishes this reading from the hybrid cascade sibling.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards against two opposite mislabels. Against pure-snare labeling: the mechanism does solve a real coordination problem — post-consolidation polities genuinely face fragmentation, and rapid standardization has standalone value that decentralized convergence cannot deliver on the same clock; the founding problem is live, corroborated from outside the benefiting parties. Against rope labeling: the coordination is achieved by suppressing the alternative (bottom-up validation and legacy practice alike), the gains concentrate at the apex, and persistence requires active enforcement — the signatures of extraction riding on coordination. Against piton labeling: the administrator (the authority) profits enough to maintain the machinery, so this is not inertial maintenance of an atrophied function; and there is no sunset clause, so the scaffold gate does not apply. The R5 mismatch consumer should find no zombie flag here: founding_problem_status is live and disappearance_verdict is world_rearranges — the arrangement persists because the problem persists, not because anyone forgot to bury it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    mechanism_selection_across_episodes,
    'In observed transformation episodes, does durable uptake actually follow the decree, or does it precede and enable it?',
    'Process-tracing adoption sequences against decree dates across a systematic episode sample: archival uptake timing (school enrollment, court filings, market usage of the new script or rite) compared with the promulgation record.',
    'If uptake generally precedes decree, this reading''s constraint describes a degenerate case and its epsilon misattributes legitimacy formation to the state; if uptake reliably follows decree and holds, the exogenous mechanism is confirmed as a real producer of legitimacy and the extraction profile stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mechanism_selection_across_episodes, empirical, 'Which mechanism actually generates legitimacy in the historical record.').

omega_variable(
    authority_net_benefit_uncertainty,
    'Does the transformation authority capture a net legitimacy rent, or do enforcement costs, backlash, and administrative strain consume the gain?',
    'Fiscal and coercive-capacity accounting of transformation programs — enforcement expenditure, unrest suppression costs, elite turnover — set against measured gains in administrative reach and allegiance concentration.',
    'If costs exceed gains, the authority seat''s derived near-beneficiary directionality is wrong: the arrangement would look less like captured rent and more like a trap the authority itself is caught in, changing the seat-level classification at the apex.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authority_net_benefit_uncertainty, empirical, 'Whether the apex seat truly profits from the mechanism it runs.').

omega_variable(
    kernel_reading_underdetermination,
    'Is the exogenous imposition reading the right instantiation of the kernel, or do the sibling readings (endogenous climb, hybrid cascade) capture the same episodes with structurally different beneficiary and victim sets?',
    'Cross-episode comparison adjudicating the relative weight of mandate, demonstrated superiority, and fringe validation in producing durable uptake; episodes where all three covary are uninformative and must be excluded.',
    'Adopting the endogenous sibling dissolves this story''s victim set almost entirely (voluntary adoption, open exits, negligible epsilon); adopting the hybrid sibling adds fringe validators as co-beneficiaries, narrows the victim set to suppression-of-validation cases, and lowers epsilon. The classification of every seat moves with the choice.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_underdetermination, conceptual, 'Committer structure: this constraint is one of three readings of the installation-mechanism kernel; the choice of reading determines the structural data.').

omega_variable(
    resistance_metabolized_or_failing,
    'Is base resistance evidence that the mechanism fails to generate legitimacy, or a managed input that the enforcement machinery metabolizes into eventual compliance?',
    'Track resistant cohorts across two generations without renewed coercion: if descendants adopt the installed commitment, resistance was a transition cost; if divergence persists or revives, the mechanism''s legitimacy output is hollow.',
    'If resistance is metabolized, the suppression series overstates fragility and the mechanism is more durable than its enforcement profile suggests; if divergence persists, the measured legitimacy is largely performed and theater_ratio understates the performative share — pushing the arrangement toward piton-like maintenance in later phases.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(resistance_metabolized_or_failing, empirical, 'Whether base resistance signals failure or is absorbed as transition cost.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_commitment_installation_mechanism__exogenous_imposition_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t0, state_commitment_installation_mechanism__exogenous_imposition_reading, theater_ratio, 0, 0.14).
narrative_ontology:measurement(stat_tr_t8, state_commitment_installation_mechanism__exogenous_imposition_reading, theater_ratio, 8, 0.17).
narrative_ontology:measurement(stat_tr_t16, state_commitment_installation_mechanism__exogenous_imposition_reading, theater_ratio, 16, 0.21).
narrative_ontology:measurement(stat_tr_t24, state_commitment_installation_mechanism__exogenous_imposition_reading, theater_ratio, 24, 0.26).
narrative_ontology:measurement(stat_tr_t32, state_commitment_installation_mechanism__exogenous_imposition_reading, theater_ratio, 32, 0.3).
narrative_ontology:measurement(stat_tr_t40, state_commitment_installation_mechanism__exogenous_imposition_reading, theater_ratio, 40, 0.34).

% Extraction over time
narrative_ontology:measurement(stat_be_t0, state_commitment_installation_mechanism__exogenous_imposition_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(stat_be_t8, state_commitment_installation_mechanism__exogenous_imposition_reading, base_extractiveness, 8, 0.54).
narrative_ontology:measurement(stat_be_t16, state_commitment_installation_mechanism__exogenous_imposition_reading, base_extractiveness, 16, 0.61).
narrative_ontology:measurement(stat_be_t24, state_commitment_installation_mechanism__exogenous_imposition_reading, base_extractiveness, 24, 0.65).
narrative_ontology:measurement(stat_be_t32, state_commitment_installation_mechanism__exogenous_imposition_reading, base_extractiveness, 32, 0.67).
narrative_ontology:measurement(stat_be_t40, state_commitment_installation_mechanism__exogenous_imposition_reading, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t0, state_commitment_installation_mechanism__exogenous_imposition_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(stat_su_t8, state_commitment_installation_mechanism__exogenous_imposition_reading, suppression_requirement, 8, 0.6).
narrative_ontology:measurement(stat_su_t16, state_commitment_installation_mechanism__exogenous_imposition_reading, suppression_requirement, 16, 0.68).
narrative_ontology:measurement(stat_su_t24, state_commitment_installation_mechanism__exogenous_imposition_reading, suppression_requirement, 24, 0.71).
narrative_ontology:measurement(stat_su_t32, state_commitment_installation_mechanism__exogenous_imposition_reading, suppression_requirement, 32, 0.69).
narrative_ontology:measurement(stat_su_t40, state_commitment_installation_mechanism__exogenous_imposition_reading, suppression_requirement, 40, 0.66).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(state_commitment_installation_mechanism__exogenous_imposition_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(state_commitment_installation_mechanism__exogenous_imposition_reading, state_commitment_installation_mechanism__endogenous_climb_reading).
narrative_ontology:affects_constraint(state_commitment_installation_mechanism__exogenous_imposition_reading, state_commitment_installation_mechanism__hybrid_cascade_reading).

% DUAL FORMULATION NOTE:
% Constraint family decomposition of the colloquial label 'how new commitments gain legitimacy' (epsilon-invariance principle). The label conflates three structurally distinct claims: exogenous imposition (this file — decree-first, state as beneficiary, bypassed validation, epsilon 0.68), endogenous climb (superiority-first, voluntary uptake, negligible extraction), and hybrid cascade (decree plus required fringe validation, intermediate extraction). The upstream member (endogenous climb, highest empirical confidence in the record) influences the downstream members because endogenous-success cases are cited as evidence against decree sufficiency. Each member carries its own epsilon, beneficiaries, victims, and claimed type; this file links both siblings via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(state_commitment_installation_mechanism__exogenous_imposition_reading, powerful, 0.22).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
