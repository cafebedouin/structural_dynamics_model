% ============================================================================
% CONSTRAINT STORY: constitutional_text_authority__originalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-21
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_text_authority__originalist_reading, []).

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
 *   constraint_id: constitutional_text_authority__originalist_reading
 *   human_readable: Originalist Fixation of Constitutional Meaning
 *   domain: legal/constitutional/jurisprudential
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the constitutional_text_authority
 *   kernel: the originalist_reading, under which constitutional meaning was
 *   fixed at ratification and legitimate change runs only through Article V.
 *   It is not the kernel itself and not a hedge across readings — the
 *   living_constitutionalist_reading and positivist_reading instantiate
 *   different constraints with different payer sets and are modeled as
 *   sibling files linked below. As a constraint operating in the actual legal
 *   system, the fixity rule has a genuine coordination function (a public
 *   settlement standard that disciplines judicial discretion) AND an
 *   asymmetric transfer: decision authority over contested questions moves
 *   from contemporary majorities and unenumerated-rights claimants to the
 *   gatekeepers of the historical record and the movement whose program the
 *   fixed meanings entrench. It persists only through active enforcement —
 *   appointment pipelines, doctrinal policing, methodological vetting — not
 *   through naturalness; rival methods remain fully available in the
 *   profession and are resisted, not absent. The interval indexes years since
 *   the mid-1970s, when the constraint first crystallized as an organized
 *   program (t=0 ≈ 1975; t=50 ≈ 2025), so the measurement series tracks its
 *   growth from fringe theory to governing methodology.
 *
 * KEY AGENTS:
 *   - originalist_judicial_bloc: agenda-setter (institutional/identity_locked) — administers the fixity rule; historical evidence gates permissible outcomes from its seat
 *   - conservative_legal_movement: primary beneficiary and receipt seat (organized/constrained) — built the pipeline; the transferred decision authority lands as victories for its long-run program
 *   - originalist_legal_scholars: beneficiary (organized/constrained) — supply the methodology and credential the gatekeepers; career capital tracks the method's dominance
 *   - unenumerated_rights_claimants: primary payer (powerless/trapped) — claims not legible in ratification-era registers fail at the threshold; Article V exit practically unreachable
 *   - contemporary_majority_coalitions: payer (moderate/trapped) — can win elections but find contested constitutional questions pre-decided by past supermajorities
 *   - living_constitutionalist_jurists: excluded (organized/constrained) — methodologically unheard within originalist courts; continue in dissents, lower courts, academy
 *   - legal_historians: analytical observer (analytical/analytical) — the verification layer the gate runs on; attest where the record is indeterminate
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_text_authority__originalist_reading, 0.42).
domain_priors:suppression_score(constitutional_text_authority__originalist_reading, 0.5).
domain_priors:theater_ratio(constitutional_text_authority__originalist_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_text_authority__originalist_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(constitutional_text_authority__originalist_reading, suppression_requirement, 0.5).
narrative_ontology:constraint_metric(constitutional_text_authority__originalist_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_text_authority__originalist_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(constitutional_text_authority__originalist_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_text_authority__originalist_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_text_authority__originalist_reading, "Originalist Fixation of Constitutional Meaning").
narrative_ontology:topic_domain(constitutional_text_authority__originalist_reading, "legal/constitutional/jurisprudential").

domain_priors:requires_active_enforcement(constitutional_text_authority__originalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_text_authority__originalist_reading, 'e59f01a7-8687-4089-9e15-5522ef32aa56').
narrative_ontology:cs_kernel_codification('e59f01a7-8687-4089-9e15-5522ef32aa56', fixed_text).
narrative_ontology:cs_authority_grounding('e59f01a7-8687-4089-9e15-5522ef32aa56', lineage).
narrative_ontology:cs_interpretation_layer_present('e59f01a7-8687-4089-9e15-5522ef32aa56').
narrative_ontology:cs_reading_relation('e59f01a7-8687-4089-9e15-5522ef32aa56', constitutional_text_authority__living_constitutionalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('e59f01a7-8687-4089-9e15-5522ef32aa56', constitutional_text_authority__positivist_reading, coexists_with).
narrative_ontology:cs_axiom('e59f01a7-8687-4089-9e15-5522ef32aa56', foundational, ratified_meaning_binds_until_amended).
narrative_ontology:cs_axiom_status(ratified_meaning_binds_until_amended, holdable).
narrative_ontology:cs_axiom_grounding('e59f01a7-8687-4089-9e15-5522ef32aa56', ratified_meaning_binds_until_amended, conventional).
narrative_ontology:cs_axiom('e59f01a7-8687-4089-9e15-5522ef32aa56', secondary, historical_constraint_disciplines_judging).
narrative_ontology:cs_axiom_status(historical_constraint_disciplines_judging, holdable).
narrative_ontology:cs_axiom_grounding('e59f01a7-8687-4089-9e15-5522ef32aa56', historical_constraint_disciplines_judging, instrumental).
narrative_ontology:cs_axiom('e59f01a7-8687-4089-9e15-5522ef32aa56', secondary, framers_subjective_intent_governs).
narrative_ontology:cs_axiom_status(framers_subjective_intent_governs, overridden).
narrative_ontology:cs_axiom_grounding('e59f01a7-8687-4089-9e15-5522ef32aa56', framers_subjective_intent_governs, empirically_contingent).
narrative_ontology:cs_reference_frame('e59f01a7-8687-4089-9e15-5522ef32aa56', ratified_public_meaning_settlement).
narrative_ontology:cs_drift_state('e59f01a7-8687-4089-9e15-5522ef32aa56', contemporary, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('e59f01a7-8687-4089-9e15-5522ef32aa56', '').
narrative_ontology:cs_kernel_id(constitutional_text_authority__originalist_reading, constitutional_text_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_text_authority__originalist_reading, originalist_judicial_bloc).
narrative_ontology:constraint_beneficiary(constitutional_text_authority__originalist_reading, conservative_legal_movement).
narrative_ontology:constraint_beneficiary(constitutional_text_authority__originalist_reading, originalist_legal_scholars).
narrative_ontology:constraint_victim(constitutional_text_authority__originalist_reading, unenumerated_rights_claimants).
narrative_ontology:constraint_victim(constitutional_text_authority__originalist_reading, contemporary_majority_coalitions).
narrative_ontology:constraint_vindicates(constitutional_text_authority__originalist_reading, fixation_thesis_of_meaning).
narrative_ontology:constraint_vindicates(constitutional_text_authority__originalist_reading, democratic_legitimacy_of_enactment).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Judges and justices appointed through a vetting process that treats commitment to ratification-fixed meaning as a qualification. They author opinions in which historical evidence about founding-era public understanding determines which claims are cognizable and which remedies are available. Their professional standing inside the coalition that appointed them is built on methodological fidelity; publicly abandoning the method would read as apostasy and would cost them the identity their appointments ratified. They hold the adjudicative authority and administer the gate.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__originalist_reading, originalist_judicial_bloc, agenda_setter,
    institutional, generational, identity_locked, national).

% The network of legal organizations, donor structures, and appointment-pipeline institutions that spent two generations building an originalist judiciary. It runs the vetting and credentialing machinery that selects who sits on courts, and its long-run policy program — on abortion, guns, regulatory power, religious exercise — is the set of outcomes that ratification-fixed meaning entrenches against ordinary democratic revision. The transferred decision authority over contested constitutional questions lands as victories for its program. It could redirect its infrastructure to other projects, but the movement as currently constituted exists for this one.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__originalist_reading, conservative_legal_movement, beneficiary,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(constitutional_text_authority__originalist_reading, conservative_legal_movement, agenda_setter).

% Academic lawyers whose expertise in founding-era sources became the discipline's gatekeeping currency. They supply the historical-evidence methodology, produce the citations opinions run on, and credential the next cohort of clerks and judges. Their influence and career capital rise with the method's dominance; pivoting to a rival methodology late in a career would discount much of that capital. They receive standing, citations, and pipeline positions rather than direct payments.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__originalist_reading, originalist_legal_scholars, beneficiary,
    organized, biographical, constrained, national).

% People whose claims — over intimacy, family structure, bodily autonomy, and other arrangements postdating ratification — are not legible in 1791 or 1868 registers. When the gate requires showing the claimed right was understood as protected at ratification, their cases fail at the threshold regardless of present-day harm or consensus. They cannot leave the constitutional order, and the only sanctioned route to recognition, the amendment process, has been practically unreachable for a generation. The cost of the fixity rule falls on them as denied recognition and withdrawn remedies.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__originalist_reading, unenumerated_rights_claimants, payer,
    powerless, biographical, trapped, national).

% Present-day electoral majorities whose preferences on contested questions are blocked not by a rival majority but by meanings fixed by past supermajorities. They can win elections and pass statutes, yet find the constitutional questions pre-decided by a record they had no part in making. Their only corrective is the amendment process, which requires supermajorities no contemporary coalition can assemble. What they lose is decision authority over questions they regard as belonging to the present.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__originalist_reading, contemporary_majority_coalitions, payer,
    moderate, immediate, trapped, national).

% Judges and scholars who hold that constitutional meaning legitimately evolves with social circumstances. They are not removed from institutions — they write dissents, sit on lower courts, and dominate parts of the academy — but within courts governed by the fixity rule their methodology is treated as illegitimate rather than merely losing on the merits. Their arguments are excluded from the register in which the dominant bloc will hear a case. They continue working in the spaces the constraint leaves open.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__originalist_reading, living_constitutionalist_jurists, excluded,
    organized, biographical, constrained, national).

% Academic historians whose record of the founding era is the verification layer the gate runs on. They produce and assess the evidence both sides cite, and many publicly attest where the historical record is indeterminate or has been selectively read. They neither collect from the arrangement nor pay its costs; their professional stake is in the integrity of the record itself.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__originalist_reading, legal_historians, observer,
    analytical, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(constitutional_text_authority__originalist_reading, conservative_legal_movement).
narrative_ontology:fixing_cost_class(constitutional_text_authority__originalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Settles constitutional meaning against an external, publicly fixed standard so that judges, officials, and citizens coordinate on a common legal framework without re-litigating first principles each generation. Historical evidence about ratification-era public understanding gates which outcomes are permissible, which disciplines judicial discretion and gives the system a settlement function rival methods claim to lack.
% TRANSFER_FUNCTION: Moves decision authority over contested rights and structural questions away from contemporary majorities and evolving professional consensus, toward the recorded understandings of ratifying generations and toward the gatekeepers of the historical record — the originalist judicial bloc that administers the gate and the movement whose policy program the fixed meanings entrench. Recognition and remedies are withdrawn from claimants whose rights do not map onto ratification-era registers.
% ABSENT_VOICES: Living constitutionalist jurists are present but methodologically unheard inside originalist courts — their register is excluded, not their persons. Deeper absences: the people excluded from the ratifying publics themselves (women, enslaved and free Black persons, the non-propertied), whose absence from the authority-conferring moment is the constraint's contested foundation, and future claimants whose circumstances have no ratification-era analogue at all.
% DISAPPEARANCE_RATIONALE: If the fixity rule vanished overnight, constitutional adjudication would reorganize around rival methods within one appointment cycle; doctrinal lines built on originalist reasoning would lose their justificatory spine; the movement's judicial-selection project would lose its defining criterion; and claimants currently gated out by historical evidence would gain access to registers where present-day consensus counts.
% FOUNDING_PROBLEM: Built to solve perceived judicial overreach: the mid-twentieth-century expansion of judicially declared rights was diagnosed as unmoored from the enacted text and democratically unaccountable, and originalism promised to re-anchor adjudication in the ratified text so judges would be constrained by law rather than preference.
% FOUNDING_PROBLEM_CORROBORATION: Partial corroboration from outside the benefiting parties: some liberal legal scholars conceded parts of the founding diagnosis — that portions of mid-century rights jurisprudence lacked firm textual anchors — and academic legal historians document genuine interpretive departures from founding-era meaning. But living constitutionalist jurists contest the premise that this was overreach rather than legitimate elaboration, and historians of founding-era practice document a pluralism that complicates the founding story itself. No uncontested external attestation exists; the genealogy is disputed terrain.
narrative_ontology:disappearance_verdict(constitutional_text_authority__originalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_text_authority__originalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_text_authority__originalist_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-18',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k3', 'max_tokens=32000,temperature=default,reasoning=max').
narrative_ontology:story_seed(constitutional_text_authority__originalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_text_authority__originalist_reading, 0.42, 'kimi-k3', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_text_authority__originalist_reading_tests).
:- end_tests(constitutional_text_authority__originalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.42) is real but channeled: the constraint takes recognition, remedies, and decision authority from identifiable payers, but the taking runs through a public text that also binds the interpreters some of the time, and nominal exit via Article V exists even if unreachable — this is not the unconstrained taking of a pure capture arrangement. Suppression (0.50) reflects active enforcement that matured over the interval: appointment vetting, doctrinal policing, and the treatment of rival methodology as illegitimate rather than merely losing — yet the rivals remain seated and vocal, so suppression is nowhere near total. Theater ratio (0.38) credits the documented 'law-office history' critique — a substantial share of cited history is motivated or selectively read — while acknowledging the method does real gating work and is not pure performance. Accessibility_collapse (0.35) is low because alternatives are not collapsed: living constitutionalism and positivism remain live, institutionally seated positions. Resistance (0.58) is high: the constraint meets sustained organized opposition from rival jurisprudential factions, affected claimants, and parts of the academy. The measurement series run on one shared six-point grid (t ≈ 1975, 1985, 1995, 2005, 2015, 2025); all three tracked metrics are authored at every point, with end values equal to the base scalars. Suppression_requirement is authored because the narrative specifically tracks enforcement-capacity maturation — from fringe theory (0.15) through pipeline construction (0.25–0.40) to a committed judicial majority policing the method (0.50).
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter and beneficiary seats, the constraint reads as genuine coordination purchased at personal cost: the judicial bloc experiences methodological constraint as discipline, the movement experiences it as legitimacy finally restored, and scholars experience it as rigor rewarded. From the payer seats the same structure operates as a gate they cannot pass: a claimant whose life postdates ratification experiences the historical-evidence requirement not as neutral method but as a rule that her claim is constitutionally invisible, and a contemporary majority experiences it as being outvoted by the dead. The engine computes this divergence from the structural data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   The judicial bloc, movement, and scholars sit at the beneficiary end: they collect authority, outcomes, and standing from the arrangement and hold the exit advantages (the bloc's identity-lock is a cost of leaving, not of staying). Unenumerated-rights claimants sit nearest the full-target end: they pay in denied recognition and are trapped, with no arbitrage-grade exit — the amendment route is nominal. Contemporary majority coalitions are targets of the authority transfer but retain electoral power over ordinary law, damping their position relative to the claimants. Living constitutionalist jurists are excluded rather than coordinated — the constraint's enforcement object includes their methodology itself. Historians sit at the analytical end. No directionality overrides are authored; the derivation from these declarations produces the right ordering.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — judicial discretion unmoored from enacted text — is declared contested, not dead: the corroboration record shows even parties outside the beneficiary set concede parts of the diagnosis. Classifying this constraint as tangled_rope is exactly the mandatrophy guard: it prevents the defenders' mislabeling (pure rope — mere coordination on settlement, with the asymmetric transfer edited out) and prevents the sharpest critics' mislabeling (pure snare — which would erase the real settlement function and the real self-constraint the method sometimes imposes on its own coalition). The genealogy interview carries the obsolescence question: whether the constraint now performs the outcome-entrenchment it was built to prevent is the contested status the engine should read against the disappearance verdict, not a premise to be settled by authorial fiat.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_situatedness,
    'This constraint is one reading of the constitutional_text_authority kernel — the originalist_reading. What would the sibling readings change structurally, and is the reading decomposition itself the right framing?',
    'Generate the sibling readings as their own constraint files and compare beneficiary/victim structure, epsilon, and drift_state across the family; test whether any real-world actor holds a framework combining readings (e.g., fixed semantic meaning with evolving construction), which would indicate the readings are not cleanly separable constraints.',
    'Under the living_constitutionalist_reading the payer set inverts — parties whose settled arrangements are disrupted by evolving meaning bear the costs this story assigns as benefits; under the positivist_reading the historical-evidence gate dissolves and interpretive legitimacy routes to enactment validity. If hybrid frameworks prove coherent and widespread, the kernel decomposition understates the constraint count.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_situatedness, conceptual, 'This story is one reading of a contested kernel; sibling readings are different constraints, not this one.').

omega_variable(
    historical_record_determinacy,
    'Is ratification-era public meaning determinate enough to gate modern adjudication, or does the historical record underdetermine outcomes precisely in the cases where the constraint bites hardest?',
    'Historiographic consensus studies, corpus-linguistic analysis of founding-era usage, and convergence audits testing whether originalist opinions reach common outcomes independently of author ideology.',
    'If the record underdetermines the hard cases, discretion is transferred rather than eliminated — to the judges and historians selecting the evidence — and the authored theater_ratio understates the performative share, pushing computed classification toward higher extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(historical_record_determinacy, empirical, 'Whether the historical gate actually constrains or merely relocates discretion.').

omega_variable(
    founding_era_self_application,
    'Were founding-era interpreters themselves originalists? If ratification-era practice included purposive, evolving, or pluralist interpretation, the constraint''s authority premise is historically self-undercutting.',
    'Historical scholarship on founding-era interpretive practice — early judicial reliance on purpose, consequence, natural law, and common-law evolution versus fixity-at-enactment methods.',
    'Evidence of founding-era pluralism would weaken the lineage authority grounding and recast the constraint as a modern movement construction wearing historical dress, raising effective extraction for payer seats and strengthening the theater component.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_era_self_application, empirical, 'Whether the method''s own authority claim survives self-application.').

omega_variable(
    selective_fidelity_asymmetry,
    'Is the fixity rule applied symmetrically, or does stare decisis selectively retain non-originalist precedent (incorporation doctrine, the administrative state, monetary arrangements) while originalist redesign proceeds in movement-priority areas?',
    'Quantitative analysis of which doctrinal lines receive originalist re-examination versus retention, coded against the movement''s stated policy priorities.',
    'If retention systematically tracks movement priorities, the constraint operates as a ratchet rather than a rule — the named gain_flow seat is confirmed as receipt of the transfer and theater_ratio should be revised upward; if retention tracks neutral reliance interests, the coordination framing strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(selective_fidelity_asymmetry, empirical, 'Whether stare decisis operates neutrally or as a movement-priority filter.').

omega_variable(
    dead_hand_authority_legitimacy,
    'Does ratification by past supermajorities — from which women, enslaved persons, and the non-propertied were excluded — confer legitimate authority over contemporary populations, or is ongoing consent required?',
    'Not resolvable by data; this is a value disagreement about the source of constitutional authority (enactment versus ongoing consent) that no empirical finding settles.',
    'If ongoing consent is required, the transfer from contemporary_majority_coalitions is unjustified dead-hand rule and the constraint trends toward pure capture for those seats; if enactment suffices, the same transfer is the ordinary price of constitutionalism and the coordination reading strengthens. The classification difference is preference-driven, not fact-driven.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(dead_hand_authority_legitimacy, preference, 'The legitimacy of rule by ratification-era publics over the living.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_text_authority__originalist_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t0, constitutional_text_authority__originalist_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(cons_tr_t10, constitutional_text_authority__originalist_reading, theater_ratio, 10, 0.18).
narrative_ontology:measurement(cons_tr_t20, constitutional_text_authority__originalist_reading, theater_ratio, 20, 0.24).
narrative_ontology:measurement(cons_tr_t30, constitutional_text_authority__originalist_reading, theater_ratio, 30, 0.28).
narrative_ontology:measurement(cons_tr_t40, constitutional_text_authority__originalist_reading, theater_ratio, 40, 0.33).
narrative_ontology:measurement(cons_tr_t50, constitutional_text_authority__originalist_reading, theater_ratio, 50, 0.38).

% Extraction over time
narrative_ontology:measurement(cons_be_t0, constitutional_text_authority__originalist_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(cons_be_t10, constitutional_text_authority__originalist_reading, base_extractiveness, 10, 0.22).
narrative_ontology:measurement(cons_be_t20, constitutional_text_authority__originalist_reading, base_extractiveness, 20, 0.28).
narrative_ontology:measurement(cons_be_t30, constitutional_text_authority__originalist_reading, base_extractiveness, 30, 0.33).
narrative_ontology:measurement(cons_be_t40, constitutional_text_authority__originalist_reading, base_extractiveness, 40, 0.38).
narrative_ontology:measurement(cons_be_t50, constitutional_text_authority__originalist_reading, base_extractiveness, 50, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(cons_su_t0, constitutional_text_authority__originalist_reading, suppression_requirement, 0, 0.15).
narrative_ontology:measurement(cons_su_t10, constitutional_text_authority__originalist_reading, suppression_requirement, 10, 0.25).
narrative_ontology:measurement(cons_su_t20, constitutional_text_authority__originalist_reading, suppression_requirement, 20, 0.32).
narrative_ontology:measurement(cons_su_t30, constitutional_text_authority__originalist_reading, suppression_requirement, 30, 0.4).
narrative_ontology:measurement(cons_su_t40, constitutional_text_authority__originalist_reading, suppression_requirement, 40, 0.46).
narrative_ontology:measurement(cons_su_t50, constitutional_text_authority__originalist_reading, suppression_requirement, 50, 0.5).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_text_authority__originalist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(constitutional_text_authority__originalist_reading, constitutional_text_authority__living_constitutionalist_reading).
narrative_ontology:affects_constraint(constitutional_text_authority__originalist_reading, constitutional_text_authority__positivist_reading).

% DUAL FORMULATION NOTE:
% The natural-language label 'constitutional interpretation' conflates structurally distinct constraints. This kernel decomposes into one constraint per reading of the constitutional_text_authority commitment. The originalist_reading (this file) fixes meaning at ratification and gates outcomes through historical evidence; its epsilon, beneficiary/victim structure, and drift profile differ from the living_constitutionalist_reading (meaning evolves with social circumstances; payers are parties whose settled arrangements evolving meaning disrupts) and the positivist_reading (validity from enactment procedure; the historical-evidence gate dissolves). The readings compete for the same adjudicative institutions, so each reading's operational strength structurally affects the others' legitimacy conditions and resource availability — hence mutual affects_constraints edges across the family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
