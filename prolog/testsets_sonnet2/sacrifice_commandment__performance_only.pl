% ============================================================================
% CONSTRAINT STORY: sacrifice_commandment__performance_only
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sacrifice_commandment__performance_only, []).

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
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: sacrifice_commandment__performance_only
 *   human_readable: Sacrifice Commandment as Performance-Only Obligation (Suspended, Not Fulfilled, Absent Temple)
 *   domain: religious/halakhic
 *
 * SUMMARY:
 *   Following the Temple's destruction, a doctrinal position crystallized
 *   holding that the biblical commandment to offer sacrifices remains
 *   formally binding but categorically unperformable without a functioning
 *   Temple and altar — and that no substitute activity, including the
 *   intensive rabbinic study of sacrificial law that in fact absorbed
 *   enormous scholarly resources, discharges the obligation. Under this
 *   reading, the commandment sits in permanent suspension: not void, not
 *   fulfilled, simply unenactable pending Temple restoration. This story
 *   evaluates that specific reading as a constraint on how scholarly and
 *   communal energy is directed.
 *
 * KEY AGENTS:
 *   - messianic_restorationist_authorities: institutional beneficiary, sets doctrinal terms of suspension
 *   - temple_reconstruction_institutions: beneficiary, mandate depends on non-fulfillment
 *   - talmudic_academies: institutional payer, generations of labor on unperformable law
 *   - individual_torah_scholars: biographical payer, identity-locked scholarly investment
 *   - study_as_performance_advocates: excluded rival reading, denied co-authorship of fulfillment criteria
 *   - contemporary_lay_practitioners: diffuse payer/beneficiary, inherits liturgy without doctrinal voice
 *   - halakhic_historians: analytical observer
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sacrifice_commandment__performance_only, 0.71).
domain_priors:suppression_score(sacrifice_commandment__performance_only, 0.58).
domain_priors:theater_ratio(sacrifice_commandment__performance_only, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sacrifice_commandment__performance_only, extractiveness, 0.71).
narrative_ontology:constraint_metric(sacrifice_commandment__performance_only, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(sacrifice_commandment__performance_only, theater_ratio, 0.62).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sacrifice_commandment__performance_only, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(sacrifice_commandment__performance_only, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sacrifice_commandment__performance_only, tangled_rope).
narrative_ontology:human_readable(sacrifice_commandment__performance_only, "Sacrifice Commandment as Performance-Only Obligation (Suspended, Not Fulfilled, Absent Temple)").
narrative_ontology:topic_domain(sacrifice_commandment__performance_only, "religious/halakhic").

domain_priors:requires_active_enforcement(sacrifice_commandment__performance_only).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sacrifice_commandment__performance_only, '26ace8bd-1d64-4035-a061-3395d7a57d7a').
narrative_ontology:cs_kernel_codification('26ace8bd-1d64-4035-a061-3395d7a57d7a', fixed_text).
narrative_ontology:cs_authority_grounding('26ace8bd-1d64-4035-a061-3395d7a57d7a', lineage).
narrative_ontology:cs_interpretation_layer_present('26ace8bd-1d64-4035-a061-3395d7a57d7a').
narrative_ontology:cs_reading_relation('26ace8bd-1d64-4035-a061-3395d7a57d7a', sacrifice_commandment__study_as_performance, forecloses).
narrative_ontology:cs_reading_relation('26ace8bd-1d64-4035-a061-3395d7a57d7a', sacrifice_commandment__archive_maintenance, coexists_with).
narrative_ontology:cs_axiom('26ace8bd-1d64-4035-a061-3395d7a57d7a', foundational, fulfillment_requires_material_enactment).
narrative_ontology:cs_axiom_status(fulfillment_requires_material_enactment, holdable).
narrative_ontology:cs_axiom_grounding('26ace8bd-1d64-4035-a061-3395d7a57d7a', fulfillment_requires_material_enactment, deontological).
narrative_ontology:cs_axiom('26ace8bd-1d64-4035-a061-3395d7a57d7a', secondary, suspension_is_not_discharge).
narrative_ontology:cs_axiom_status(suspension_is_not_discharge, holdable).
narrative_ontology:cs_axiom_grounding('26ace8bd-1d64-4035-a061-3395d7a57d7a', suspension_is_not_discharge, conventional).
narrative_ontology:cs_reference_frame('26ace8bd-1d64-4035-a061-3395d7a57d7a', temple_era_literal_sacrificial_praxis).
narrative_ontology:cs_drift_state('26ace8bd-1d64-4035-a061-3395d7a57d7a', post_destruction_rabbinic_consolidation, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('26ace8bd-1d64-4035-a061-3395d7a57d7a', '').
narrative_ontology:cs_kernel_id(sacrifice_commandment__performance_only, sacrifice_commandment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sacrifice_commandment__performance_only, messianic_restorationist_authorities).
narrative_ontology:constraint_beneficiary(sacrifice_commandment__performance_only, temple_reconstruction_institutions).
narrative_ontology:constraint_victim(sacrifice_commandment__performance_only, talmudic_academies).
narrative_ontology:constraint_victim(sacrifice_commandment__performance_only, individual_torah_scholars).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(sacrifice_commandment__performance_only, contemporary_lay_practitioners).
narrative_ontology:constraint_victim(sacrifice_commandment__performance_only, contemporary_lay_practitioners).
narrative_ontology:constraint_vindicates(sacrifice_commandment__performance_only, divine_command_requires_material_enactment).
narrative_ontology:constraint_vindicates(sacrifice_commandment__performance_only, temple_centrality_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Rabbinic and communal leaders who hold that sacrifice is suspended, not transmuted into study, derive institutional legitimacy from maintaining messianic anticipation as the live horizon under which all Temple law is held in abeyance. They administer liturgy, communal fasts commemorating the Temple's absence, and doctrinal boundaries against readings that would declare the commandment already fulfilled through study. Their position costs them nothing materially; it consolidates their authority over what counts as legitimate religious labor in the interim.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__performance_only, messianic_restorationist_authorities, beneficiary,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(sacrifice_commandment__performance_only, messianic_restorationist_authorities, agenda_setter).

% Organizations dedicated to priestly genealogical preservation, ritual object reconstruction, and Temple Mount access advocacy benefit directly from the performance-only reading: if study alone fulfilled the commandment, their entire institutional mandate would dissolve. Their funding, membership, and purpose depend on the commandment remaining unperformed and unperformable-by-substitute.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__performance_only, temple_reconstruction_institutions, beneficiary,
    organized, civilizational, arbitrage, regional).

% Yeshivot devote enormous scholarly labor — entire orders of the Mishnah and Talmud (Kodashim, much of Zevachim, Menachot) — to sacrificial law that, under this reading, produces no independent religious fulfillment of its own; the study is preparatory or commemorative at best, never itself the commandment. Centuries of the ablest minds in the tradition are directed toward legal mechanics of an act none of them will ever perform, while the same intellectual capital could address commandments currently live and performable. Exit is constrained: the curriculum is canonical and abandoning it invites charges of diminishing Torah's scope.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__performance_only, talmudic_academies, payer,
    organized, generational, constrained, global).

% A scholar who spends a career mastering the laws of sacrificial gradations, priestly disqualifications, and altar geometry does so knowing that, on this reading, none of it discharges any obligation — it is suspended, not enacted, by study. Their professional identity as a master of this material is real and biographically load-bearing, but the reading they've committed to denies their labor the status of religious performance that the study_as_performance reading would grant it. Exit would mean either switching to the rival reading (available, but requires abandoning years of framing) or accepting that a life's central scholarly labor sits permanently in a suspended, unfulfilled category.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__performance_only, individual_torah_scholars, payer,
    moderate, biographical, identity_locked, local).

% Communities and thinkers (drawing on statements like 'the study of the laws of sacrifice is accounted as if the sacrifice were offered') who hold that engagement itself fulfills the commandment are structurally excluded from setting the terms of this reading's obligation-status; the performance-only authorities treat their position as consolation rhetoric, not a live halakhic alternative with equal force.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__performance_only, study_as_performance_advocates, excluded,
    organized, generational, mobile, global).

% Ordinary observant Jews inherit the liturgical structure built around the suspended commandment — daily prayers referencing sacrifice, fast days mourning the Temple's destruction — without having chosen the underlying doctrinal reading. They bear the psychological weight of an unfulfillable obligation embedded in daily practice, while also receiving the coherence and continuity that a stable liturgical calendar provides. They have essentially no voice in which kernel reading is authoritative.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__performance_only, contemporary_lay_practitioners, payer,
    powerless, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(sacrifice_commandment__performance_only, contemporary_lay_practitioners, beneficiary).

% Scholars of religious law and history who trace how the performance-only reading became institutionally dominant after 70 CE, and how it interacts with rabbinic innovations that substituted prayer and study for sacrifice in practice while denying that substitution any formal fulfillment status. They document but do not adjudicate the doctrinal dispute.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__performance_only, halakhic_historians, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves a sharp distinction between what the tradition currently does (prayer, study, communal mourning) and what it is formally obligated to do (physical sacrifice at a functioning Temple), preventing the community from declaring victory over its own exile and thereby coordinating continued messianic expectation, communal identity around loss, and doctrinal resistance to premature closure of the obligation.
% TRANSFER_FUNCTION: Directs scholarly attention, institutional funding, and psychological/liturgical energy toward maintaining and studying an unperformable commandment, and away from commandments that are currently live and dischargeable; the transfer runs from working scholars and lay practitioners' daily religious energy toward the doctrinal authority of restorationist institutions who benefit from the obligation remaining perpetually open.
% ABSENT_VOICES: Study_as_performance advocates and their communities are excluded from co-authoring what counts as fulfillment; had they equal standing, centuries of Kodashim study might carry formal fulfillment status rather than suspended status, materially changing how scholarly labor in this domain is valued and rewarded.
% DISAPPEARANCE_RATIONALE: If the performance-only reading vanished overnight in favor of study_as_performance, the practical liturgical calendar would likely remain nearly identical — the same texts would be studied, the same fasts observed — but the STATUS of that study would change from commemorative/preparatory to fulfilling, altering scholarly self-conception, institutional legitimacy claims for Temple-reconstruction bodies, and possibly reducing the psychological weight lay practitioners carry around an 'unfulfilled' obligation. Restorationist and reconstruction institutions dispute that anything would or should change; study-as-performance communities hold that a great deal would change for scholars' religious standing.
% FOUNDING_PROBLEM: After the Temple's destruction in 70 CE, the tradition needed to determine whether commandments requiring the Temple were now void, transferred to a substitute performance, or suspended in a state of unfulfilled obligation pending restoration — a genealogical and theological crisis about whether Judaism's sacrificial core survived its own infrastructure's destruction.
% FOUNDING_PROBLEM_CORROBORATION: Restorationist authorities and Temple-institute advocates (the reading's own beneficiaries) attest the founding problem remains fully live — the Temple has not been rebuilt, so the suspension persists by definition. Independent historians of religion and comparative scholars of ritual substitution (e.g. work tracing how post-Destruction Judaism institutionalized prayer as korban substitute in Berakhot while denying it formal equivalence) attest from outside the beneficiary set that the 'problem' as originally framed — an obligation genuinely awaiting literal performance — has been functionally supplanted in lived practice for nineteen centuries, even where doctrine refuses to say so; this outside corroboration is precisely what the performance-only reading's own institutions have the strongest incentive to deny.
narrative_ontology:disappearance_verdict(sacrifice_commandment__performance_only, contested).
narrative_ontology:founding_problem_status(sacrifice_commandment__performance_only, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sacrifice_commandment__performance_only, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(sacrifice_commandment__performance_only, 'none', 1).
narrative_ontology:epsilon_provenance(sacrifice_commandment__performance_only, 0.71, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sacrifice_commandment__performance_only_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(sacrifice_commandment__performance_only, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(sacrifice_commandment__performance_only_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored high (0.71 at t=1900) because, under this reading's own terms, nineteen centuries of the tradition's most rigorous scholarly attention has been directed at law governing an act nobody performs and which that same attention does not fulfill — the labor produces no independent religious discharge, only preparatory or commemorative value contingent on a restoration that has not occurred. Suppression (0.58) reflects real but partial doctrinal enforcement: communities that treat study as fulfillment are not physically coerced, but they are excluded from setting authoritative fulfillment criteria, and dissenting from performance-only doctrine carries real communal costs. Theater ratio (0.62) captures that a substantial share of the liturgical and pedagogical apparatus built around the suspended commandment (fast-day liturgies, curricular emphasis on Kodashim) functions as commemorative performance of loss and anticipation rather than movement toward the commandment's actual conditions of fulfillment. Accessibility collapse is moderate (0.40), not high, because rival readings (study_as_performance, archive_maintenance) remain live and held by real communities — this is not a mountain-like foreclosure of alternatives, it is one contested reading among several.
 *
 * DIRECTIONALITY LOGIC:
 *   Restorationist authorities and Temple-reconstruction institutions sit at the beneficiary end: their institutional purpose and authority depend on the commandment remaining unperformed-and-unfulfilled-by-substitute, so d is low for them under this reading. Talmudic academies and individual scholars sit near the target end: their labor is structurally devalued to 'suspended' rather than 'fulfilling' status, and their exit options are constrained (institutional) or identity-locked (individual) because switching readings mid-career or mid-institution carries real cost. Lay practitioners are dual-positioned — genuine liturgical coherence benefits them, but they also carry the psychological weight of institutionalized non-fulfillment without having chosen the reading.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (whether sacrificial commandments survived the Temple's destruction) is authored contested rather than flatly dead, because the performance-only reading's own logic makes the problem permanently live by definition — it cannot be solved by anything except literal Temple restoration, which has not occurred. This is exactly the structure a mandatrophy analysis should flag: a reading whose founding problem is defined such that only one specific, currently-unavailable event could ever resolve it functions to guarantee its own indefinite continuation, regardless of how much scholarly and liturgical labor accumulates around it. The tangled_rope classification captures both halves: a genuine coordination function (preventing premature theological closure, preserving communal memory and messianic hope) coexisting with asymmetric extraction (labor and psychological cost falling on scholars and laity while doctrinal authority accrues to restorationist institutions).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_authority_ambiguity,
    'Is the performance_only reading the historically dominant position because it best reflects the plain sense of the commandment''s material requirements, or because institutions with a stake in perpetual non-fulfillment (messianic authorities, Temple-reconstruction bodies) have had disproportionate influence over which reading became authoritative?',
    'Comparative textual-historical analysis of when and by whom the performance_only position was first formalized against study_as_performance and archive_maintenance formulations, cross-referenced with the institutional interests of the earliest codifiers.',
    'If the performance_only reading''s dominance tracks institutional interest rather than independent textual argument, its classification shifts further toward tangled_rope/snare; if it tracks genuine textual constraint independent of institutional benefit, the coordination function is stronger relative to the extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_authority_ambiguity, conceptual, 'Whether performance_only''s dominance reflects textual necessity or institutional capture of the kernel.').

omega_variable(
    sibling_reading_structural_delta,
    'How would the beneficiary/victim structure invert if study_as_performance or archive_maintenance were adopted as the authoritative reading instead?',
    'This is precisely what the sibling constraint stories (study_as_performance, archive_maintenance) model independently — each carries its own ε, beneficiaries, and victims as its own advocates and critics understand it. Compare the three stories'' extractiveness and beneficiary/victim sets directly rather than treating this as an unresolved question within this single story.',
    'Confirms the ε-invariance principle: this story''s high extraction is a property of the performance_only reading specifically, not of ''the sacrifice commandment'' as an undifferentiated topic. The sibling stories should show materially different extraction profiles — study_as_performance in particular should show much lower extraction against scholarly labor, since it grants that labor fulfillment status.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_structural_delta, conceptual, 'Cross-reading structural comparison, resolved by consulting sibling constraint stories rather than internally.').

omega_variable(
    identity_lock_reversibility,
    'For individual scholars identity-locked into sacrificial law study under the performance_only frame, how reversible is the psychological/professional cost if they later adopt study_as_performance?',
    'Biographical and sociological study of scholars who have publicly shifted between these doctrinal readings mid-career, tracking whether their sense of their prior study''s religious value was retrospectively upgraded.',
    'High reversibility would mean the extraction is more contingent/psychological than structural; low reversibility would mean the performance_only reading inflicts durable, non-recoverable devaluation on decades of scholarly labor.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_reversibility, empirical, 'Whether identity-locked scholarly investment under this reading can be recovered by later reading-switching.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sacrifice_commandment__performance_only, 0, 1900).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sacr_tr_t0, sacrifice_commandment__performance_only, theater_ratio, 0, 0.4).
narrative_ontology:measurement_basis(sacr_tr_t0, observed).
narrative_ontology:measurement(sacr_tr_t300, sacrifice_commandment__performance_only, theater_ratio, 300, 0.46).
narrative_ontology:measurement_basis(sacr_tr_t300, observed).
narrative_ontology:measurement(sacr_tr_t700, sacrifice_commandment__performance_only, theater_ratio, 700, 0.5).
narrative_ontology:measurement_basis(sacr_tr_t700, observed).
narrative_ontology:measurement(sacr_tr_t1100, sacrifice_commandment__performance_only, theater_ratio, 1100, 0.55).
narrative_ontology:measurement_basis(sacr_tr_t1100, observed).
narrative_ontology:measurement(sacr_tr_t1500, sacrifice_commandment__performance_only, theater_ratio, 1500, 0.59).
narrative_ontology:measurement_basis(sacr_tr_t1500, observed).
narrative_ontology:measurement(sacr_tr_t1900, sacrifice_commandment__performance_only, theater_ratio, 1900, 0.62).
narrative_ontology:measurement_basis(sacr_tr_t1900, observed).

% Extraction over time
narrative_ontology:measurement(sacr_be_t0, sacrifice_commandment__performance_only, base_extractiveness, 0, 0.35).
narrative_ontology:measurement_basis(sacr_be_t0, observed).
narrative_ontology:measurement(sacr_be_t300, sacrifice_commandment__performance_only, base_extractiveness, 300, 0.48).
narrative_ontology:measurement_basis(sacr_be_t300, observed).
narrative_ontology:measurement(sacr_be_t700, sacrifice_commandment__performance_only, base_extractiveness, 700, 0.55).
narrative_ontology:measurement_basis(sacr_be_t700, observed).
narrative_ontology:measurement(sacr_be_t1100, sacrifice_commandment__performance_only, base_extractiveness, 1100, 0.62).
narrative_ontology:measurement_basis(sacr_be_t1100, observed).
narrative_ontology:measurement(sacr_be_t1500, sacrifice_commandment__performance_only, base_extractiveness, 1500, 0.67).
narrative_ontology:measurement_basis(sacr_be_t1500, observed).
narrative_ontology:measurement(sacr_be_t1900, sacrifice_commandment__performance_only, base_extractiveness, 1900, 0.71).
narrative_ontology:measurement_basis(sacr_be_t1900, observed).

% Suppression requirement over time
narrative_ontology:measurement(sacr_su_t0, sacrifice_commandment__performance_only, suppression_requirement, 0, 0.4).
narrative_ontology:measurement_basis(sacr_su_t0, observed).
narrative_ontology:measurement(sacr_su_t300, sacrifice_commandment__performance_only, suppression_requirement, 300, 0.45).
narrative_ontology:measurement_basis(sacr_su_t300, observed).
narrative_ontology:measurement(sacr_su_t700, sacrifice_commandment__performance_only, suppression_requirement, 700, 0.48).
narrative_ontology:measurement_basis(sacr_su_t700, observed).
narrative_ontology:measurement(sacr_su_t1100, sacrifice_commandment__performance_only, suppression_requirement, 1100, 0.52).
narrative_ontology:measurement_basis(sacr_su_t1100, observed).
narrative_ontology:measurement(sacr_su_t1500, sacrifice_commandment__performance_only, suppression_requirement, 1500, 0.55).
narrative_ontology:measurement_basis(sacr_su_t1500, observed).
narrative_ontology:measurement(sacr_su_t1900, sacrifice_commandment__performance_only, suppression_requirement, 1900, 0.58).
narrative_ontology:measurement_basis(sacr_su_t1900, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sacrifice_commandment__performance_only, identity_coordination).
narrative_ontology:boltzmann_floor_override(sacrifice_commandment__performance_only, 0.1).
narrative_ontology:affects_constraint(sacrifice_commandment__performance_only, study_as_performance).
narrative_ontology:affects_constraint(sacrifice_commandment__performance_only, archive_maintenance).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the sacrifice_commandment kernel. performance_only (this story) authors high extraction because it denies fulfillment status to the study labor that in fact absorbs the tradition's scholarly resources. study_as_performance should author substantially lower extraction, since it grants that same labor the status of actual commandment-fulfillment. archive_maintenance occupies a middle position: study has real instrumental value (preserving knowledge for restoration) without claiming either fulfillment or mere suspension. All three share the same historical kernel (the post-70-CE crisis over sacrificial commandments) but are structurally distinct constraints with different beneficiary/victim sets and different ε values, per the ε-invariance principle — they are not the same constraint measured three ways.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
