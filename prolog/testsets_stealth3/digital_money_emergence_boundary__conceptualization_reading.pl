% ============================================================================
% CONSTRAINT STORY: digital_money_emergence_boundary__conceptualization_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_digital_money_emergence_boundary__conceptualization_reading, []).

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
 *   constraint_id: digital_money_emergence_boundary__conceptualization_reading
 *   human_readable: Conceptualization Boundary of Digital Money Emergence (Theory-First Reading)
 *   domain: economic/historiographical/technological
 *
 * SUMMARY:
 *   This story instantiates the conceptualization_reading of the
 *   digital_money_emergence_boundary kernel (see commentary.kernel_context
 *   and the linked sibling files for the contest structure). The constraint
 *   under classification is the historiographical convention - operative in
 *   surveys, textbooks, and curricula since roughly 1990 - that fixes the
 *   origin of digital money at theoretical thinkability: the 1960s
 *   telecommunications advances that made electronic value transfer
 *   conceivable, culminating in the 1985 formalization of privacy-preserving
 *   electronic cash. The convention performs a real coordination service (a
 *   shared periodization lets economists, historians, and technologists
 *   communicate) while allocating intellectual priority through the same
 *   structure: founding-figure status, citation capital, and narrative
 *   authority concentrate on the formalization lineage, while deployment-era
 *   actors - interbank-network builders and consumer e-money ventures - are
 *   positioned as pre-history or mere implementation. Beneficiary and victim
 *   declarations drive the directionality computation; per-seat
 *   classifications are computed by the engine from the structural data,
 *   never asserted here. KEY AGENTS (by structural relationship): -
 *   formal_cryptographers: Primary beneficiary (organized/identity_locked) -
 *   collects citation capital and founding-figure status -
 *   computing_history_canonicalizers: Agenda setter
 *   (institutional/constrained) - administers the canonical dating through
 *   surveys and syllabi - monetary_economists: Dual-positioned user
 *   (institutional/constrained) - gains an analytic boundary, absorbs
 *   misdating costs - payments_infrastructure_engineers: Payer with high exit
 *   (institutional/mobile) - demoted to pre-history; funds counter-histories
 *   - electronic_money_entrepreneurs: Trapped payer (moderate/trapped) -
 *   deployment record fixed as implementation - central_bank_archivists:
 *   Excluded voice (organized/trapped) - holds rival operational records
 *   outside adjudicating venues - sts_origin_critics: Analytical observer -
 *   sees the full construction of the origin narrative
 *
 * KEY AGENTS:
 *   - formal_cryptographers: Primary beneficiary (organized/identity_locked) - collects priority rents from the theory-first boundary
 *   - computing_history_canonicalizers: Agenda setter (institutional/generational horizon, constrained) - administers the canonical dating
 *   - monetary_economists: Dual-positioned consumer (institutional) - analytic utility and misdating cost in one seat
 *   - payments_infrastructure_engineers: Nominal target with arbitrage-grade exit (institutional/mobile)
 *   - electronic_money_entrepreneurs: Trapped payer (moderate) - bears the demotion most heavily
 *   - central_bank_archivists: Excluded voice holding rival operational records
 *   - sts_origin_critics: Analytical observer over the whole origin-construction process
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(digital_money_emergence_boundary__conceptualization_reading, 0.38).
domain_priors:suppression_score(digital_money_emergence_boundary__conceptualization_reading, 0.3).
domain_priors:theater_ratio(digital_money_emergence_boundary__conceptualization_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(digital_money_emergence_boundary__conceptualization_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(digital_money_emergence_boundary__conceptualization_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(digital_money_emergence_boundary__conceptualization_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(digital_money_emergence_boundary__conceptualization_reading, accessibility_collapse, 0.22).
narrative_ontology:constraint_metric(digital_money_emergence_boundary__conceptualization_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(digital_money_emergence_boundary__conceptualization_reading, tangled_rope).
narrative_ontology:human_readable(digital_money_emergence_boundary__conceptualization_reading, "Conceptualization Boundary of Digital Money Emergence (Theory-First Reading)").
narrative_ontology:topic_domain(digital_money_emergence_boundary__conceptualization_reading, "economic/historiographical/technological").

domain_priors:requires_active_enforcement(digital_money_emergence_boundary__conceptualization_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(digital_money_emergence_boundary__conceptualization_reading, 'ea6ae5b2-169f-4919-bc3c-aa5750cd2002').
narrative_ontology:cs_kernel_codification('ea6ae5b2-169f-4919-bc3c-aa5750cd2002', distributed).
narrative_ontology:cs_authority_grounding('ea6ae5b2-169f-4919-bc3c-aa5750cd2002', expertise).
narrative_ontology:cs_interpretation_layer_present('ea6ae5b2-169f-4919-bc3c-aa5750cd2002').
narrative_ontology:cs_reading_relation('ea6ae5b2-169f-4919-bc3c-aa5750cd2002', digital_money_emergence_boundary__infrastructure_reading, influences).
narrative_ontology:cs_reading_relation('ea6ae5b2-169f-4919-bc3c-aa5750cd2002', digital_money_emergence_boundary__consumer_holdings_reading, coexists_with).
narrative_ontology:cs_axiom('ea6ae5b2-169f-4919-bc3c-aa5750cd2002', foundational, emergence_equals_theoretical_thinkability).
narrative_ontology:cs_axiom_status(emergence_equals_theoretical_thinkability, holdable).
narrative_ontology:cs_axiom_grounding('ea6ae5b2-169f-4919-bc3c-aa5750cd2002', emergence_equals_theoretical_thinkability, conventional).
narrative_ontology:cs_axiom('ea6ae5b2-169f-4919-bc3c-aa5750cd2002', secondary, formalization_constitutes_emergence_event).
narrative_ontology:cs_axiom_status(formalization_constitutes_emergence_event, holdable).
narrative_ontology:cs_axiom_grounding('ea6ae5b2-169f-4919-bc3c-aa5750cd2002', formalization_constitutes_emergence_event, conventional).
narrative_ontology:cs_reference_frame('ea6ae5b2-169f-4919-bc3c-aa5750cd2002', conceptualization_origin_frame).
narrative_ontology:cs_drift_state('ea6ae5b2-169f-4919-bc3c-aa5750cd2002', post_blockchain_historiography, gap(practice_drift, minor, false)).
narrative_ontology:cs_created_at('ea6ae5b2-169f-4919-bc3c-aa5750cd2002', '2026-06-11T09:30:00Z').
narrative_ontology:cs_kernel_id(digital_money_emergence_boundary__conceptualization_reading, digital_money_emergence_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(digital_money_emergence_boundary__conceptualization_reading, formal_cryptographers).
narrative_ontology:constraint_beneficiary(digital_money_emergence_boundary__conceptualization_reading, computing_history_canonicalizers).
narrative_ontology:constraint_victim(digital_money_emergence_boundary__conceptualization_reading, payments_infrastructure_engineers).
narrative_ontology:constraint_victim(digital_money_emergence_boundary__conceptualization_reading, electronic_money_entrepreneurs).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(digital_money_emergence_boundary__conceptualization_reading, monetary_economists).
narrative_ontology:constraint_victim(digital_money_emergence_boundary__conceptualization_reading, monetary_economists).
narrative_ontology:constraint_vindicates(digital_money_emergence_boundary__conceptualization_reading, chaum_blind_signature_foundational_status).
narrative_ontology:constraint_vindicates(digital_money_emergence_boundary__conceptualization_reading, theory_precedes_deployment_origins_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Researchers in the formalization lineage of electronic payments - blind signatures, privacy-preserving transaction protocols. Their papers define what counts as a foundational result in the field; citation flows, invited keynotes, and founding-figure status accrue to the authors of the formalizations that surveys treat as origins. They also staff the program committees and editorial boards where the dating is reproduced. Leaving the frame would mean renouncing the priority claims their careers are built on; within the field, the frame is their professional home.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__conceptualization_reading, formal_cryptographers, beneficiary,
    organized, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(digital_money_emergence_boundary__conceptualization_reading, formal_cryptographers, agenda_setter).

% Survey authors, textbook writers, and historians of computing who fix the origin date in canonical narratives. They choose which milestone opens the story; their works become the references others cite, so the dating reproduces itself through syllabi and bibliographies across generations. Revising the dating would devalue their own canonical texts, so they defend the established frame while fielding recurring challenges from rival datelines.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__conceptualization_reading, computing_history_canonicalizers, agenda_setter,
    institutional, generational, constrained, global).

% Economists modeling payment-system evolution and advising central banks. The fixed boundary hands them a clean starting line for models and policy histories, simplifying analysis; the same boundary can misdate the phenomena their models need, forcing awkward adjustments when deployment realities fall outside the canonical window. They can adopt rival datelines in principle, but journal conventions reward consistency with the canonical frame.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__conceptualization_reading, monetary_economists, beneficiary,
    institutional, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(digital_money_emergence_boundary__conceptualization_reading, monetary_economists, payer).

% Institutions and engineer-chroniclers behind interbank networks and self-service banking hardware - automated teller deployments, electronic clearing houses, interbank messaging systems. Under the canonical dating their operational achievements are pre-history rather than emergence. They bear diffuse reputational cost in the scholarly record, but their institutions command ample resources to publish counter-histories, commission anniversary volumes, and fund their own archives, so the cost lands lightly.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__conceptualization_reading, payments_infrastructure_engineers, payer,
    institutional, generational, mobile, global).

% Founders and teams behind consumer-facing electronic cash products - smart-card purses, early online payment schemes. The literature frames their deployment record as implementation of ideas conceived earlier, and cites their commercial failures as evidence the underlying concepts were premature rather than flawed. After wind-down they cannot re-enter the origin narrative; their legacy positioning is fixed by the works that cite them.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__conceptualization_reading, electronic_money_entrepreneurs, payer,
    moderate, biographical, trapped, global).

% Record-keepers and historians employed by central banks and payment-system operators. Their archives document when electronic instruments first cleared, settled, and reached customers - dates that rival the canonical scholarly boundary. They publish technical histories but hold no seat in the academic venues where the origin question is adjudicated; their objections circulate in grey literature the canon rarely cites.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__conceptualization_reading, central_bank_archivists, excluded,
    organized, generational, trapped, global).

% Scholars of science and technology studies who analyze how technical fields construct origin stories. They observe the full contest among theory-first datelines, infrastructure datelines, and user-side datelines without collecting from or paying into any particular boundary. Their analyses inform periodic historiographical revisions but carry no administrative authority over the canon.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__conceptualization_reading, sts_origin_critics, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(digital_money_emergence_boundary__conceptualization_reading, formal_cryptographers).
narrative_ontology:fixing_cost_class(digital_money_emergence_boundary__conceptualization_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Fixes one shared temporal boundary for the origin of digital money, allowing economists, historians, curricula, and policy narratives to align on a common opening date, cite a common founding literature, and delimit a researchable field - stated without evaluation.
% TRANSFER_FUNCTION: Moves intellectual priority - citation capital, founding-figure status, keynote and grant-framing advantage, narrative authority over the domain's origin story - away from deployment-era practitioners (interbank-network builders, consumer e-money ventures) and toward the formalization lineage whose work the boundary canonizes.
% ABSENT_VOICES: Central-bank archivists, payments-industry chroniclers, and veteran e-money operators hold operational records that rival the canonical boundary but lack seats in the academic venues where the origin question is adjudicated; they would object that first clearance runs, first customer-held instruments, and first interbank settlement constitute emergence regardless of formal publication. Their objections survive in grey literature the canon rarely cites.
% DISAPPEARANCE_RATIONALE: Surveys and textbooks would lose their canonical opening chapter; priority claims would reopen and rival datelines would compete for the vacated origin slot; crypto-industry founding mythologies anchored on the 1985 formalization would need re-dating; policy histories citing digital money's birth year would shift their baselines. Arrangements demonstrably depend on the boundary.
% FOUNDING_PROBLEM: After Chaum's formalization made electronic cash rigorously specifiable, the emerging literature needed a determinate answer to where digital money begins - to delimit a researchable field, structure surveys, and attribute foundational credit among telecommunications visionaries, protocol formalizers, and deployment engineers.
% FOUNDING_PROBLEM_CORROBORATION: State plainly: no one outside the benefiting academic community attests that the founding problem remains live in its original formulation. Corroboration for the CONTESTED status arrives from outside the beneficiary set - central-bank and payments-industry historical publications attest the problem is answered operationally by deployment milestones, and science-and-technology-studies analyses attest the origin question currently functions as priority allocation. The liveness claim survives only inside the benefiting set.
narrative_ontology:disappearance_verdict(digital_money_emergence_boundary__conceptualization_reading, world_rearranges).
narrative_ontology:founding_problem_status(digital_money_emergence_boundary__conceptualization_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(digital_money_emergence_boundary__conceptualization_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(digital_money_emergence_boundary__conceptualization_reading, 'none', 1).
narrative_ontology:epsilon_provenance(digital_money_emergence_boundary__conceptualization_reading, 0.38, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(digital_money_emergence_boundary__conceptualization_reading_tests).
:- end_tests(digital_money_emergence_boundary__conceptualization_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.38 at interval end) is moderate-low: the boundary extracts priority and prestige, currencies that are real in academic economies but are not coercively collected - no party is fined, barred, or imprisoned for adopting a rival dateline. Suppression (0.30) reflects epistemic gatekeeping: citation norms, editorial selection, and curriculum inertia raise the cost of rival datelines without eliminating them. Theater (0.28) captures anniversary commemorations, founding-myth keynotes, and founding-father framing layered onto a dating function that still does genuine work. Accessibility collapse is low (0.22): the three datelines of this kernel remain fully available positions - nothing collapses when the convention is understood - which is why resistance (0.45) stays elevated through continuous revisionist publishing and industry counter-histories. The measurement series shares one time grid (1990-2025) across all tracked metrics. The enforcement arc traces the frame's contest cycle: low enforcement effort while uncontested (1990), hardening through the 2008-2017 blockchain-era revisionist surge that made the question of who invented digital cash commercially valuable narrative, easing afterward as pluralist special issues normalized rival datelines. The oscillation is partly an extraction mechanism in its own right: each speculative boom re-prices priority claims, renewing the theory-first frame's salience and the beneficiaries' rents - intermittent reinforcement rather than noise. Suppression here is a raw structural property of the gatekeeping apparatus, unscaled; only extractiveness is scaled by directionality and scope downstream.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute divergently by construction. From the formal_cryptographers seat the boundary is constitutive justice - credit assigned where the possibility space opened - and exit is unthinkable because the frame is their professional home (identity_locked fusion of career and origin story). From the payments_infrastructure_engineers seat the same convention is credit expropriation, but mobile exit - control of their own archives and anniversary machinery - damps what they effectively bear. From the electronic_money_entrepreneurs seat, post-failure and unable to re-enter the narrative, the demotion binds hardest. The canonicalizer seat experiences stewardship, not extraction. The engine derives per-seat classifications from power, exit, and directional position; this commentary explains why they must diverge, not what each seat computes.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations (formal_cryptographers, computing_history_canonicalizers) derive directionality near the subsidy end: the convention pays them in priority. Victim declarations map to real relationships: payments_infrastructure_engineers and electronic_money_entrepreneurs surrender narrative standing to the same structure that coordinates everyone's communication. Exit modulates effective placement: the infrastructure seat's arbitrage-grade exit places it far from the full-target end despite nominal victim status, while the entrepreneurs' trapped position places them near it - the asymmetry the hybrid structure turns on. Monetary economists sit near symmetric: genuine analytic utility received, real misdating cost absorbed. No directionality overrides were authored: beneficiary/victim declarations plus exit options reproduce the intended d-ordering without correction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem - delimiting a researchable field and attributing foundational credit after the formalization wave - is contested rather than dead: the periodization function still solves a live coordination problem (every survey needs some boundary), while the priority-allocation function has drifted toward rent defense. Classifying this as tangled_rope keeps both faces visible: a snare reading would erase the genuine periodization utility and misread gatekeeping as coercive force it does not exercise; a rope reading would erase the demoted practitioners whose standing the same structure reallocates. Because the mandate is contested rather than expired, mandatrophy is left unresolved, and the R5 mismatch consumer should expect no clean dead-mandate signal from this story.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_underdetermination,
    'This story instantiates the conceptualization_reading of the digital_money_emergence_boundary kernel; sibling readings (infrastructure_reading, consumer_holdings_reading) instantiate different constraints with different epsilon, beneficiary sets, and victim sets. Which reading an analyst adopts determines the classification - is the contest resolvable, and on what data?',
    'Comparative citation-adoption analysis across the three sibling stories: measure which dateline dominates citable scholarly practice, central-bank publications, and industry histories; adjudicate by whichever reading commands the operative consensus.',
    'If the infrastructure reading dominates operative usage, this story''s beneficiary structure (academic priority claims) understates practitioner-side rents and the boundary''s governance effect shifts toward system-operator institutions; if consumer_holdings dominates, the affected set shifts to infrastructure-era actors.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_underdetermination, conceptual, 'Kernel contest among three emergence-boundary readings; classification is reading-indexed.').

omega_variable(
    priority_rent_magnitude,
    'How large are the actual rents flowing to the theory-first boundary''s beneficiaries - citation capital, keynote circuits, grant-framing advantage - relative to ordinary academic returns?',
    'Bibliometric difference-in-differences comparing citation trajectories of formalization-lineage versus deployment-lineage payment-system literature; funding-agency portfolio analysis of electronic-payments program framing.',
    'A near-zero measurable differential would downgrade extraction toward a pure coordination device; a large durable differential confirms the hybrid structure with the formal_cryptographers seat as capturer.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(priority_rent_magnitude, empirical, 'Magnitude of academic priority rents attributable to origin attribution.').

omega_variable(
    enforcement_qualification,
    'Does citation-gatekeeping, editorial selection, and curriculum inertia constitute active enforcement sufficient for a hybrid coordination/extraction structure, or is the arrangement better read as a self-sustaining convention?',
    'Trace concrete enforcement acts: desk-rejections keyed to framing, survey authors'' refusal to cite rival datelines, syllabus audits; documented deliberate sanctioning distinguishes enforcement from drift.',
    'If enforcement is merely conventional drift, the constraint computes closer to a pure coordination device and the payer seats'' costs become incidental; if deliberate sanctioning is documented, the hybrid classification is confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_qualification, conceptual, 'Whether soft epistemic gatekeeping qualifies as active enforcement.').

omega_variable(
    policy_dateline_spillover,
    'Is epsilon stable across the boundary''s two operational uses - scholarly periodization versus regulatory timeline calibration, where supervisors date innovation risk from the canonical birth year rather than deployment dates?',
    'Audit supervisory and legislative documents citing digital-money origins; compare the dateline invoked and its consequences for scoping decisions on electronic-payment oversight.',
    'If regulatory usage dominates and misdates supervision, the extraction referent widens beyond academia to include mis-calibration costs borne by payment firms and supervisors, raising effective extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(policy_dateline_spillover, empirical, 'Whether the convention''s policy-side use adds extraction beyond the academic surface.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(digital_money_emergence_boundary__conceptualization_reading, 1990, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dme_conceptualization_tr_t1990, digital_money_emergence_boundary__conceptualization_reading, theater_ratio, 1990, 0.12).
narrative_ontology:measurement(dme_conceptualization_tr_t1998, digital_money_emergence_boundary__conceptualization_reading, theater_ratio, 1998, 0.16).
narrative_ontology:measurement(dme_conceptualization_tr_t2006, digital_money_emergence_boundary__conceptualization_reading, theater_ratio, 2006, 0.2).
narrative_ontology:measurement(dme_conceptualization_tr_t2014, digital_money_emergence_boundary__conceptualization_reading, theater_ratio, 2014, 0.3).
narrative_ontology:measurement(dme_conceptualization_tr_t2019, digital_money_emergence_boundary__conceptualization_reading, theater_ratio, 2019, 0.32).
narrative_ontology:measurement(dme_conceptualization_tr_t2025, digital_money_emergence_boundary__conceptualization_reading, theater_ratio, 2025, 0.28).

% Extraction over time
narrative_ontology:measurement(dme_conceptualization_be_t1990, digital_money_emergence_boundary__conceptualization_reading, base_extractiveness, 1990, 0.28).
narrative_ontology:measurement(dme_conceptualization_be_t1998, digital_money_emergence_boundary__conceptualization_reading, base_extractiveness, 1998, 0.33).
narrative_ontology:measurement(dme_conceptualization_be_t2006, digital_money_emergence_boundary__conceptualization_reading, base_extractiveness, 2006, 0.36).
narrative_ontology:measurement(dme_conceptualization_be_t2014, digital_money_emergence_boundary__conceptualization_reading, base_extractiveness, 2014, 0.4).
narrative_ontology:measurement(dme_conceptualization_be_t2019, digital_money_emergence_boundary__conceptualization_reading, base_extractiveness, 2019, 0.41).
narrative_ontology:measurement(dme_conceptualization_be_t2025, digital_money_emergence_boundary__conceptualization_reading, base_extractiveness, 2025, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(dme_conceptualization_su_t1990, digital_money_emergence_boundary__conceptualization_reading, suppression_requirement, 1990, 0.2).
narrative_ontology:measurement(dme_conceptualization_su_t1998, digital_money_emergence_boundary__conceptualization_reading, suppression_requirement, 1998, 0.24).
narrative_ontology:measurement(dme_conceptualization_su_t2006, digital_money_emergence_boundary__conceptualization_reading, suppression_requirement, 2006, 0.27).
narrative_ontology:measurement(dme_conceptualization_su_t2014, digital_money_emergence_boundary__conceptualization_reading, suppression_requirement, 2014, 0.34).
narrative_ontology:measurement(dme_conceptualization_su_t2019, digital_money_emergence_boundary__conceptualization_reading, suppression_requirement, 2019, 0.31).
narrative_ontology:measurement(dme_conceptualization_su_t2025, digital_money_emergence_boundary__conceptualization_reading, suppression_requirement, 2025, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(digital_money_emergence_boundary__conceptualization_reading, information_standard).
narrative_ontology:affects_constraint(digital_money_emergence_boundary__conceptualization_reading, digital_money_emergence_boundary__infrastructure_reading).
narrative_ontology:affects_constraint(digital_money_emergence_boundary__conceptualization_reading, digital_money_emergence_boundary__consumer_holdings_reading).

% DUAL FORMULATION NOTE:
% The colloquial question 'when did digital money emerge' decomposes into three structurally distinct boundary claims per the epsilon-invariance principle. This story fixes emergence at theoretical thinkability (1960s telecommunications advances; 1985 Chaum formalization) and authors epsilon for the scholarly convention that treats conceptualization as origin - beneficiaries are priority-claiming academics. The infrastructure reading (1967 ATMs, 1972 ACH, 1977 SWIFT) carries a different epsilon and a different beneficiary set (system-operator institutions); the consumer-holdings reading (1990s e-purses, 2000 EMD) carries yet another victim/beneficiary geometry. Each is a separate file with its own stable epsilon. The upstream theory-first frame influences the siblings by supplying the definitional vocabulary their datelines must accept or rebut.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
