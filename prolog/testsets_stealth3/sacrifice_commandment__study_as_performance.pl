% ============================================================================
% CONSTRAINT STORY: sacrifice_commandment__study_as_performance
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sacrifice_commandment__study_as_performance, []).

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
 *   constraint_id: sacrifice_commandment__study_as_performance
 *   human_readable: Study-as-Performance Reading of the Sacrifice Commandment
 *   domain: religious/halakhic/commitment-system
 *
 * SUMMARY:
 *   Within rabbinic Judaism, the sacrificial commandments of the Torah became
 *   unperformable after the destruction of the Second Temple in 70 CE. The
 *   kernel 'sacrifice commandment' — the standing question of what discharges
 *   those obligations in the Temple's absence — is read three ways in the
 *   tradition, and each reading instantiates a different constraint. This
 *   story instantiates the study-as-performance reading: the Talmudic
 *   teaching (Megillah 31b; Menachot 110a) that one who engages in the study
 *   of the sacrificial laws is as if he had offered them, so that
 *   intellectual engagement itself fulfills the divine obligation. Under this
 *   reading the arrangement is non-coercive and near-non-extractive: the
 *   scholar-worshipper's study is simultaneously the obligation's cost and
 *   its fulfillment, the covenantal community retains a live commandment, and
 *   the academies that house the study gain centrality. No party bears a net
 *   burden; the sibling readings persist as live alternatives rather than
 *   suppressed exits. This file is one reading of the kernel only; the
 *   siblings are separate constraints with their own ε, beneficiary
 *   structures, and classifications. KEY AGENTS (by structural relationship):
 *   - scholar_worshippers: primary beneficiary (moderate/constrained) —
 *   performs the study that constitutes fulfillment - covenantal_community:
 *   collective beneficiary (organized/generational/constrained) — retains a
 *   fulfillable covenant across the Temple's absence - rabbinic_academies:
 *   secondary beneficiary and practice-administrator
 *   (organized/generational/constrained) — houses the study, gains
 *   institutional centrality - unlettered_laypersons: excluded seat
 *   (powerless/biographical/constrained) — recitation-level access to
 *   fulfillment, absent from the interpretive conversation -
 *   academic_rabbinics_scholars: analytical observer
 *   (institutional/generational/analytical) — sees the full three-reading
 *   structure from outside the tradition
 *
 * KEY AGENTS:
 *   - scholar_worshippers: primary beneficiary (moderate/constrained) — performs the study that constitutes fulfillment
 *   - covenantal_community: collective beneficiary (organized/generational/constrained) — retains a fulfillable covenant across the Temple's absence
 *   - rabbinic_academies: secondary beneficiary and practice-administrator (organized/generational/constrained) — houses the study, gains institutional centrality
 *   - unlettered_laypersons: excluded seat (powerless/biographical/constrained) — recitation-level access to fulfillment, absent from the interpretive conversation
 *   - academic_rabbinics_scholars: analytical observer (institutional/generational/analytical) — sees the full three-reading structure from outside the tradition
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sacrifice_commandment__study_as_performance, 0.05).
domain_priors:suppression_score(sacrifice_commandment__study_as_performance, 0.05).
domain_priors:theater_ratio(sacrifice_commandment__study_as_performance, 0.06).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sacrifice_commandment__study_as_performance, extractiveness, 0.05).
narrative_ontology:constraint_metric(sacrifice_commandment__study_as_performance, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(sacrifice_commandment__study_as_performance, theater_ratio, 0.06).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sacrifice_commandment__study_as_performance, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(sacrifice_commandment__study_as_performance, resistance, 0.25).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sacrifice_commandment__study_as_performance, rope).
narrative_ontology:human_readable(sacrifice_commandment__study_as_performance, "Study-as-Performance Reading of the Sacrifice Commandment").
narrative_ontology:topic_domain(sacrifice_commandment__study_as_performance, "religious/halakhic/commitment-system").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sacrifice_commandment__study_as_performance, 'a354ea49-06d0-490e-81d5-7bfed9cddfc7').
narrative_ontology:cs_kernel_codification('a354ea49-06d0-490e-81d5-7bfed9cddfc7', fixed_text).
narrative_ontology:cs_authority_grounding('a354ea49-06d0-490e-81d5-7bfed9cddfc7', lineage).
narrative_ontology:cs_interpretation_layer_present('a354ea49-06d0-490e-81d5-7bfed9cddfc7').
narrative_ontology:cs_reading_relation('a354ea49-06d0-490e-81d5-7bfed9cddfc7', sacrifice_commandment__performance_only, forecloses).
narrative_ontology:cs_reading_relation('a354ea49-06d0-490e-81d5-7bfed9cddfc7', sacrifice_commandment__archive_maintenance, forecloses).
narrative_ontology:cs_axiom('a354ea49-06d0-490e-81d5-7bfed9cddfc7', foundational, study_discharges_sacrifice_obligation).
narrative_ontology:cs_axiom_status(study_discharges_sacrifice_obligation, holdable).
narrative_ontology:cs_axiom_grounding('a354ea49-06d0-490e-81d5-7bfed9cddfc7', study_discharges_sacrifice_obligation, theological).
narrative_ontology:cs_axiom('a354ea49-06d0-490e-81d5-7bfed9cddfc7', foundational, commandment_essence_is_intellectual_engagement).
narrative_ontology:cs_axiom_status(commandment_essence_is_intellectual_engagement, holdable).
narrative_ontology:cs_axiom_grounding('a354ea49-06d0-490e-81d5-7bfed9cddfc7', commandment_essence_is_intellectual_engagement, deontological).
narrative_ontology:cs_reference_frame('a354ea49-06d0-490e-81d5-7bfed9cddfc7', intellect_accessible_covenant).
narrative_ontology:cs_drift_state('a354ea49-06d0-490e-81d5-7bfed9cddfc7', contemporary, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('a354ea49-06d0-490e-81d5-7bfed9cddfc7', '').
narrative_ontology:cs_kernel_id(sacrifice_commandment__study_as_performance, sacrifice_commandment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sacrifice_commandment__study_as_performance, scholar_worshippers).
narrative_ontology:constraint_beneficiary(sacrifice_commandment__study_as_performance, covenantal_community).
narrative_ontology:constraint_beneficiary(sacrifice_commandment__study_as_performance, rabbinic_academies).
narrative_ontology:constraint_vindicates(sacrifice_commandment__study_as_performance, study_as_sacrifice_equivalence_doctrine).
narrative_ontology:constraint_vindicates(sacrifice_commandment__study_as_performance, intellect_accessibility_of_commandments).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Study the laws of sacrifices — the Talmudic orders of offerings, the daily recitation of the sacrifice passages in the liturgy — as the act that fulfills the divine obligation to bring offerings. What the arrangement asks of them is the study itself, which is also what they receive: the discharge of the obligation is experienced in and as the studying. Leaving the arrangement would cost not money or standing but a rewritten covenantal self-understanding; adopting the suspension reading means conceding that what they do each morning discharges nothing.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__study_as_performance, scholar_worshippers, beneficiary,
    moderate, biographical, constrained, global).

% Carries the covenant across the Temple's absence. Under this reading the community's obligations remain live and fulfillable, so continuity of practice and identity does not depend on a rebuilt altar. The community bears no net burden; its stake is that the arrangement keeps the covenant's terms operative rather than suspended.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__study_as_performance, covenantal_community, beneficiary,
    organized, generational, constrained, global).

% House and administer the practice: curricula center the orders of sacrifices, ordination tracks run through them, and liturgical authorities standardize the daily recitation. The reading makes the academy the site where the commandment is fulfilled, which concentrates students, esteem, and institutional centrality there. They set how the practice is organized day to day, though the reading itself predates them and rests on textual authority they transmit rather than originate.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__study_as_performance, rabbinic_academies, beneficiary,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(sacrifice_commandment__study_as_performance, rabbinic_academies, agenda_setter).

% Recite the sacrifice passages in the daily liturgy without the learning to engage the laws at depth. The tradition counts their recitation as discharge, but the interpretive conversation about what fulfillment requires is conducted among the learned; they inherit its terms rather than set them. Their alternative — adopting the suspension reading — would not improve their access; it would tell them the obligation is simply unfulfillable for now.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__study_as_performance, unlettered_laypersons, excluded,
    powerless, biographical, constrained, global).

% Study the reading's emergence and function from outside the tradition's authority claims: they date the equivalence doctrines in the Talmudic corpus, compare the three readings' social functions, and attest the post-destruction crisis from the external historical record. They collect and produce analysis; they neither perform the practice nor set its terms.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__study_as_performance, academic_rabbinics_scholars, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(sacrifice_commandment__study_as_performance, scholar_worshippers).
narrative_ontology:fixing_cost_class(sacrifice_commandment__study_as_performance, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains the continuity of covenantal obligation and practice after the destruction of the Temple: the community keeps a fulfillable commandment and a daily practice of engagement with the sacrificial system, coordinated around study rather than altar service. It also solves a status-coordination problem: where the community's center of gravity moves when the altar is gone — to the study hall.
% TRANSFER_FUNCTION: Moves the scholar-worshipper's time and intellectual labor into the position the sacrificial rite occupied, and moves covenantal standing — the discharge of the obligation — to the one who studies. Status and institutional centrality move toward the academies that house the study. No material goods move; what transfers is time, attention, standing, and status.
% ABSENT_VOICES: The unlettered layperson is present in the practice (daily recitation of the sacrifice passages) but absent from the interpretive conversation that defines what counts as fulfillment — the terms are set by those for whom deep study is available. Adherents of the performance_only reading are present in the textual tradition but marginalized in communities that have adopted this reading; they would object that recitation and study discharge nothing and that calling study 'performance' launders a suspension into a fulfillment.
% DISAPPEARANCE_RATIONALE: If the reading vanished overnight, the community would face the lapsed-obligation problem it was built to solve: either the commandment is suspended (the performance_only reading takes its place) or study is re-justified as preparation for restoration (the archive reading takes its place). Daily liturgy would lose the korbanot passages' discharge-function, academy curricula would lose their warrant for centering the sacrificial orders, and the community's covenantal self-understanding — an obligation still live and still fulfillable — would reorganize around whichever replacement reading won.
% FOUNDING_PROBLEM: After the destruction of the Second Temple in 70 CE, a covenant built around altar service held commandments with no infrastructure: were the sacrificial obligations suspended, and if not, how were they discharged? This reading was built to answer: the obligation remains live and is discharged through study of the sacrificial laws.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested from outside the beneficiary set: the destruction itself is a matter of external historical record (Josephus's account, Roman sources), and academic scholarship on post-70 Judaism independently attests the fulfillment crisis the rabbinic readings answered. Within the tradition, adherents of the performance_only reading corroborate that the problem is real — they dispute the solution, not the crisis. No source outside the tradition attests that the equivalence doctrine is the correct solution; that claim rests on the tradition's own textual authority.
narrative_ontology:disappearance_verdict(sacrifice_commandment__study_as_performance, world_rearranges).
narrative_ontology:founding_problem_status(sacrifice_commandment__study_as_performance, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sacrifice_commandment__study_as_performance, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(sacrifice_commandment__study_as_performance, 'none', 1).
narrative_ontology:epsilon_provenance(sacrifice_commandment__study_as_performance, 0.05, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sacrifice_commandment__study_as_performance_tests).
:- end_tests(sacrifice_commandment__study_as_performance_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored near zero (0.05) because under this reading the arrangement's only cost — the scholar's time and attention — is the worship itself: cost and benefit are the same act. The 0.05 is not literally zero because the practice has institutionalized: academy status, communal esteem, and standardized daily recitation create a residual social cost to non-engagement, but that residual sits below the identity-coordination floor (0.08), i.e., within what the framework treats as coordination cost rather than extractive overhead. ε's referent is the standing arrangement this reading holds — study-as-discharge as actually practiced — not the arrangement any sibling would put in place. Suppression is minimal (0.05): the reading is held by textual authority and persuasion; the sibling readings persist unsuppressed as live positions, and no enforcement machinery exists — one cannot be compelled to study. Theater is low (0.06) and slowly rising: the function (engaged study as fulfillment) is real, but a growing share of practice is speed-recitation of the korbanot liturgy without engagement — study-shaped performance rather than study. Accessibility collapse is moderate (0.40): for a holder of this reading the strong forms of the alternatives collapse, but weak hybrid forms (study as fulfillment AND restoration-preparation) remain coherent, so alternatives do not vanish. Resistance is low-moderate (0.25): performance_only adherents deny that study discharges the obligation, and outside critics read the equivalence as a post-destruction rationalization, but the reading is deeply entrenched in text, liturgy, and curriculum. The identity-coordination framing is genuine here, not a cover: the covenantal boundary being maintained (which obligations are live and how they are met) really did have to evolve when the Temple fell. The measurement series run on one shared time grid (t=0..100, spanning the reading's Talmudic codification to the present, normalized) with both tracked metrics authored at every point; suppression_requirement is deliberately not tracked because no enforcement capacity changes over the interval — the enforcement picture is static and near-absent, already captured by the scalar. Receipt and cost: gain_flow names the scholar-worshipper seat because the arrangement's product — discharged obligation experienced as worship — lands there; fixing_cost is prohibitive because the only candidate fix (abandoning the reading for a sibling) costs a global, millennium-deep practice reorientation against a nil benefit. Neither signals capture: extraction sits below the coordination floor and no seat gains at another's expense.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the scholar-worshipper seat the arrangement is not experienced as a constraint at all: the act that would be its burden is the act the seat values most, so the seat computes near-pure benefit. From the unlettered layperson seat the same arrangement defines fulfillment in terms only partially accessible to them — recitation-level participation without entry to the interpretive conversation that sets the terms. From the academic observer seat the arrangement is one of three mutually exclusive readings with a visible construction history in a post-destruction crisis, and its near-zero ε is a property of the reading's own lights rather than a settled fact about the commandment. The engine computes these divergences from the structural data; this story does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Every declared party sits at or near the beneficiary end. The scholar-worshippers receive the arrangement's product directly — covenantal fulfillment experienced as worship — so their d sits near 0.0; their constrained (not arbitrage) exit reflects that leaving means rewriting a covenantal self-understanding, not escaping a burden. The covenantal community benefits derivatively: a live, fulfillable commandment sustains continuity of obligation across the Temple's absence. The academies benefit in status and centrality and administer the practice, but the gains land on the studier, not the administrator — receipt is not captured at the administrative seat. With no declared victims, no seat derives a high d; the arrangement has no target end. The residual 0.05 ε attaches diffusely to participants as the social cost of non-engagement, below the coordination floor.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — obligations rendered unperformable by the Temple's destruction — is still live: the Temple is not rebuilt, and the community still needs a discharge path. founding_problem_status 'live' paired with disappearance_verdict 'world_rearranges' is the consistent, non-zombie cell: the arrangement persists because its problem persists, not because its mandate has atrophied. The rope classification prevents the two standard mislabels: reading the arrangement as covert extraction (the academy status-economy as rent collection under worship cover — the omegas hold that question open rather than letting the near-zero claim close it), and reading it as natural fact (the equivalence as discovered truth rather than a reading with a construction history — the naturalness omega holds that open). If the founding problem died — Temple rebuilt, sacrifices resumed — this reading would not become an inert vestige; it would either dissolve into the archive reading (study as preparation, its transitional form) or persist as voluntary supererogation, and the classification should be re-authored at that point.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_selection_underdetermination,
    'This story is one reading of the sacrifice_commandment kernel; would instantiating a sibling reading (performance_only or archive_maintenance) change the constraint''s beneficiary structure, victim set, and type?',
    'Author the sibling stories and compare: performance_only yields a suspended-obligation arrangement with no fulfillment path; archive_maintenance yields a transitional preparation arrangement whose justification dies at restoration.',
    'If a given community''s actual practice instantiates archive_maintenance rather than this reading, the constraint should be re-authored as a transitional arrangement with a restoration sunset rather than an open-ended coordination solution.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_selection_underdetermination, conceptual, 'Which reading of the sacrifice-commandment kernel a community actually instantiates.').

omega_variable(
    naturalness_vs_crisis_construction,
    'Is the study-as-performance equivalence a discovery of the commandment''s inherent structure (the obligation was always intellect-accessible) or a post-destruction construction designed to solve the fulfillment crisis?',
    'Historical-critical analysis of the reading''s emergence in the Talmudic corpus: dating the equivalence claims, tracking whether they appear before or only after 70 CE, and whether earlier strata treat study as preparatory rather than discharging.',
    'If constructed, the arrangement is a designed coordination solution and the rope classification stands with ε near zero; if discovered, the claim approaches natural-law status within the system and the reading''s resistance should drop further.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(naturalness_vs_crisis_construction, empirical, 'Whether the equivalence doctrine is discovered structure or crisis response.').

omega_variable(
    accessibility_hierarchy,
    'Does defining fulfillment as intellectual engagement create an accessibility hierarchy — fulfillment proportional to learning — that imposes a diffuse cost on unlettered participants?',
    'Compare fulfillment outcomes across literacy strata within communities holding the reading: does daily recitation fully extend discharge to the unlettered, or does deep fulfillment remain an elite good?',
    'If a real hierarchy exists, the arrangement acquires a diffuse payer seat and ε rises above the coordination floor, complicating the no-victim-set claim; if recitation fully democratizes discharge, the excluded seat''s situation is a discourse exclusion only, with no cost attached.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(accessibility_hierarchy, empirical, 'Whether the reading''s fulfillment criterion is stratified by learning.').

omega_variable(
    institutional_status_extraction,
    'Does the status-economy around scholarship (communal esteem, academy centrality, family and communal resources devoted to supporting full-time study) constitute extraction riding on the worship framing, or a benign byproduct?',
    'Trace resource flows: do non-studying community members bear net costs (funding, deference, marriage-market structures) that accrue to the scholarly class under the worship justification?',
    'If yes, a payer seat emerges (supporting households and non-scholar community members), ε rises, and the arrangement drifts toward a hybrid coordination/extraction structure; if the flows are voluntary reciprocal support, the near-zero ε stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_status_extraction, empirical, 'Whether the academy status-economy extracts from non-scholars.').

omega_variable(
    foreclosure_hybrid_form,
    'The strong forms of the three readings are mutually exclusive, but weak hybrid forms (study as present fulfillment AND restoration-preparation) circulate in the tradition — do the hybrids dissolve the foreclosure structure declared in reading_relations?',
    'Survey the hybrid positions in the commentarial literature and test whether any single framework holds the strong form of this reading alongside the strong form of a sibling, rather than a weakened blend.',
    'If hybrids are the dominant live positions, the foreclosure edges should be downgraded to influences and the kernel''s contest is less exclusive than the strong-form analysis suggests; the per-reading classifications survive either way.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(foreclosure_hybrid_form, conceptual, 'Whether hybrid readings weaken the foreclosure structure among the kernel''s readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sacrifice_commandment__study_as_performance, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sacr_tr_t0, sacrifice_commandment__study_as_performance, theater_ratio, 0, 0.02).
narrative_ontology:measurement(sacr_tr_t20, sacrifice_commandment__study_as_performance, theater_ratio, 20, 0.03).
narrative_ontology:measurement(sacr_tr_t40, sacrifice_commandment__study_as_performance, theater_ratio, 40, 0.03).
narrative_ontology:measurement(sacr_tr_t60, sacrifice_commandment__study_as_performance, theater_ratio, 60, 0.04).
narrative_ontology:measurement(sacr_tr_t80, sacrifice_commandment__study_as_performance, theater_ratio, 80, 0.05).
narrative_ontology:measurement(sacr_tr_t100, sacrifice_commandment__study_as_performance, theater_ratio, 100, 0.06).

% Extraction over time
narrative_ontology:measurement(sacr_be_t0, sacrifice_commandment__study_as_performance, base_extractiveness, 0, 0.03).
narrative_ontology:measurement(sacr_be_t20, sacrifice_commandment__study_as_performance, base_extractiveness, 20, 0.03).
narrative_ontology:measurement(sacr_be_t40, sacrifice_commandment__study_as_performance, base_extractiveness, 40, 0.04).
narrative_ontology:measurement(sacr_be_t60, sacrifice_commandment__study_as_performance, base_extractiveness, 60, 0.04).
narrative_ontology:measurement(sacr_be_t80, sacrifice_commandment__study_as_performance, base_extractiveness, 80, 0.05).
narrative_ontology:measurement(sacr_be_t100, sacrifice_commandment__study_as_performance, base_extractiveness, 100, 0.05).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(sacrifice_commandment__study_as_performance, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sacrifice_commandment__study_as_performance, identity_coordination).
narrative_ontology:affects_constraint(sacrifice_commandment__study_as_performance, sacrifice_commandment__performance_only).
narrative_ontology:affects_constraint(sacrifice_commandment__study_as_performance, sacrifice_commandment__archive_maintenance).

% DUAL FORMULATION NOTE:
% The natural-language label 'the sacrifice commandment' covers three structurally distinct claims sharing one kernel: what discharges the sacrificial obligations while the Temple stands destroyed. Per the ε-invariance principle the label is decomposed into three stories: this one (study_as_performance — study discharges the obligation now; ε near zero, no victim set), performance_only (the obligation is suspended without physical execution), and archive_maintenance (study is restoration-preparation, not present worship). Their ε values, beneficiary structures, and types differ because they are different constraints, not one constraint viewed from different angles; each file links the others. This file is the family's textual anchor: its doctrine (Megillah 31b, Menachot 110a) is the warrant the other two readings argue against, so the kernel's contest is routed through it.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
