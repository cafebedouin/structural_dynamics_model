% ============================================================================
% CONSTRAINT STORY: udhr_article_3__procedural_hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_udhr_article_3__procedural_hybrid_reading, []).

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
 *   constraint_id: udhr_article_3__procedural_hybrid_reading
 *   human_readable: UDHR Article 3 — Procedural Hybrid Reading (Due Process Floor)
 *   domain: legal/political
 *
 * SUMMARY:
 *   UDHR Article 3 ('everyone has the right to life, liberty and security of
 *   person') is a contested kernel: its text fixes no substantive content for
 *   liberty or security. This story instantiates the procedural hybrid
 *   reading, under which the article's operative content is a set of
 *   judicially enforceable process guarantees — habeas corpus, review of
 *   detention, absolute prohibition of torture — that deliberately leave the
 *   substantive liberty-versus-welfare contest unresolved. The epsilon
 *   referent is the standing procedural-guarantee arrangement as this reading
 *   assesses it: a real protective floor with asymmetric leakage at the
 *   emergency margin, not the substantive arrangements either sibling reading
 *   would build. Sibling stories (udhr_article_3__negative_liberty_reading,
 *   udhr_article_3__positive_entitlement_reading) instantiate the other two
 *   readings; all three are linked through network.affects_constraints.
 *
 * KEY AGENTS:
 *   - - judiciary: agenda-setter and primary institutional beneficiary (institutional/identity_locked) — runs the habeas and review machinery; collects jurisdiction, budgets, and doctrinal territory
 *   - - executive_security_apparatus: primary payer (institutional/constrained) — bears compliance costs; retains the self-certified derogation lever
 *   - - persons_facing_state_coercion: intended beneficiary with residual payer exposure (powerless/trapped) — protected in ordinary process, exposed at the margins
 *   - - administrative_detainees: concentrated payers at the emergency margin (powerless/trapped) — hearings occur, custody continues
 *   - - legal_profession: secondary beneficiary (organized/mobile) — fees, standing, and caseload from enforcement activity
 *   - - taxpayers: diffuse payers (moderate/mobile) — fund courts, legal aid, and oversight
 *   - - human_rights_monitoring_bodies: observers (institutional/analytical) — doctrinal authority without enforcement power
 *   - - populations_outside_procedural_floor: excluded (powerless/trapped) — no seat in any constitutional conversation the guarantee governs
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(udhr_article_3__procedural_hybrid_reading, 0.44).
domain_priors:suppression_score(udhr_article_3__procedural_hybrid_reading, 0.5).
domain_priors:theater_ratio(udhr_article_3__procedural_hybrid_reading, 0.31).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(udhr_article_3__procedural_hybrid_reading, extractiveness, 0.44).
narrative_ontology:constraint_metric(udhr_article_3__procedural_hybrid_reading, suppression_requirement, 0.5).
narrative_ontology:constraint_metric(udhr_article_3__procedural_hybrid_reading, theater_ratio, 0.31).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(udhr_article_3__procedural_hybrid_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(udhr_article_3__procedural_hybrid_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(udhr_article_3__procedural_hybrid_reading, tangled_rope).
narrative_ontology:human_readable(udhr_article_3__procedural_hybrid_reading, "UDHR Article 3 — Procedural Hybrid Reading (Due Process Floor)").
narrative_ontology:topic_domain(udhr_article_3__procedural_hybrid_reading, "legal/political").

domain_priors:requires_active_enforcement(udhr_article_3__procedural_hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(udhr_article_3__procedural_hybrid_reading, '542afea4-c840-49c3-a9e8-06791a1bf56d').
narrative_ontology:cs_kernel_codification('542afea4-c840-49c3-a9e8-06791a1bf56d', fixed_text).
narrative_ontology:cs_authority_grounding('542afea4-c840-49c3-a9e8-06791a1bf56d', distributed).
narrative_ontology:cs_reading_relation('542afea4-c840-49c3-a9e8-06791a1bf56d', udhr_article_3__negative_liberty_reading, coexists_with).
narrative_ontology:cs_reading_relation('542afea4-c840-49c3-a9e8-06791a1bf56d', udhr_article_3__positive_entitlement_reading, coexists_with).
narrative_ontology:cs_axiom('542afea4-c840-49c3-a9e8-06791a1bf56d', foundational, article3_exhausted_by_procedural_guarantees).
narrative_ontology:cs_axiom_status(article3_exhausted_by_procedural_guarantees, holdable).
narrative_ontology:cs_axiom_grounding('542afea4-c840-49c3-a9e8-06791a1bf56d', article3_exhausted_by_procedural_guarantees, conventional).
narrative_ontology:cs_axiom('542afea4-c840-49c3-a9e8-06791a1bf56d', secondary, rights_secured_through_judicial_process).
narrative_ontology:cs_axiom_status(rights_secured_through_judicial_process, holdable).
narrative_ontology:cs_axiom_grounding('542afea4-c840-49c3-a9e8-06791a1bf56d', rights_secured_through_judicial_process, conventional).
narrative_ontology:cs_reference_frame('542afea4-c840-49c3-a9e8-06791a1bf56d', procedural_due_process_floor).
narrative_ontology:cs_drift_state('542afea4-c840-49c3-a9e8-06791a1bf56d', contemporary_emergency_practice_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('542afea4-c840-49c3-a9e8-06791a1bf56d', '2026-08-05T09:41:17Z').
narrative_ontology:cs_kernel_id(udhr_article_3__procedural_hybrid_reading, udhr_article_3).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(udhr_article_3__procedural_hybrid_reading, judiciary).
narrative_ontology:constraint_beneficiary(udhr_article_3__procedural_hybrid_reading, legal_profession).
narrative_ontology:constraint_beneficiary(udhr_article_3__procedural_hybrid_reading, persons_facing_state_coercion).
narrative_ontology:constraint_victim(udhr_article_3__procedural_hybrid_reading, executive_security_apparatus).
narrative_ontology:constraint_victim(udhr_article_3__procedural_hybrid_reading, administrative_detainees).
narrative_ontology:constraint_victim(udhr_article_3__procedural_hybrid_reading, taxpayers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(udhr_article_3__procedural_hybrid_reading, persons_facing_state_coercion).
narrative_ontology:constraint_vindicates(udhr_article_3__procedural_hybrid_reading, rule_of_law_doctrine).
narrative_ontology:constraint_vindicates(udhr_article_3__procedural_hybrid_reading, habeas_corpus_tradition).
narrative_ontology:constraint_vindicates(udhr_article_3__procedural_hybrid_reading, non_derogable_torture_norm).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Runs the habeas and judicial-review machinery: hears detention challenges, quashes unlawful custody, reviews emergency measures. Each petition expands the court's docket, staffing, and doctrinal territory. Judges cannot decline the guardian role without dissolving the institution's own justification; stepping back from review would undo the court's claim to authority.
narrative_ontology:constraint_stakeholder(udhr_article_3__procedural_hybrid_reading, judiciary, agenda_setter,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(udhr_article_3__procedural_hybrid_reading, judiciary, beneficiary).

% Must justify every deprivation of liberty before a court, document interrogation practice, and answer for custodial deaths. Carries the compliance burden in staffing, record-keeping, and lost operational speed. Retains one lever the text leaves open: declaring emergencies and derogating, which suspends parts of the guarantee under self-certified necessity. Cannot abandon the constitutional order it administers without overthrowing it.
narrative_ontology:constraint_stakeholder(udhr_article_3__procedural_hybrid_reading, executive_security_apparatus, payer,
    institutional, generational, constrained, national).

% Anyone arrested, detained, or interrogated holds enforceable process rights: prompt appearance before a judge, access to counsel, freedom from torture. Protection is strongest in ordinary criminal process and thins in immigration, preventive, and emergency custody, where the same person may wait months for a hearing that defers to the state. They cannot exit custody; the guarantee is the only channel they have.
narrative_ontology:constraint_stakeholder(udhr_article_3__procedural_hybrid_reading, persons_facing_state_coercion, beneficiary,
    powerless, immediate, trapped, national).
narrative_ontology:stakeholder_secondary_role(udhr_article_3__procedural_hybrid_reading, persons_facing_state_coercion, payer).

% Held under immigration, preventive, or emergency-regime custody where detention is authorized administratively and reviewed deferentially. Formally inside the procedural system — petitions filed, hearings held — while custody continues through it. Bear the longest detentions and the thinnest hearings in the system.
narrative_ontology:constraint_stakeholder(udhr_article_3__procedural_hybrid_reading, administrative_detainees, payer,
    powerless, immediate, trapped, national).

% Bar associations, defense counsel, and human-rights litigators earn fees, standing, and caseload from the guarantee's enforcement: petitions, custody challenges, treaty submissions. Can move between practice areas or jurisdictions if the work dries up; the work does not dry up.
narrative_ontology:constraint_stakeholder(udhr_article_3__procedural_hybrid_reading, legal_profession, beneficiary,
    organized, biographical, mobile, national).

% Fund the courts, legal aid, public defender systems, and custodial oversight that the guarantee requires. The cost is diffuse and unbundled from any visible service they personally receive; emigration is the only exit and almost nobody takes it for this reason.
narrative_ontology:constraint_stakeholder(udhr_article_3__procedural_hybrid_reading, taxpayers, payer,
    moderate, biographical, mobile, national).

% Treaty bodies and regional courts review state reports, issue general comments, and hear individual complaints about detention and torture. Hold moral and doctrinal authority without enforcement power; their findings shape legitimacy but bind no one directly.
narrative_ontology:constraint_stakeholder(udhr_article_3__procedural_hybrid_reading, human_rights_monitoring_bodies, observer,
    institutional, generational, analytical, global).

% Live under regimes with no effective habeas, no independent judiciary, and practiced torture. Would object that a guarantee which stops at the border of constitutional states protects the already-protected, but hold no seat in any constitutional conversation the guarantee governs.
narrative_ontology:constraint_stakeholder(udhr_article_3__procedural_hybrid_reading, populations_outside_procedural_floor, excluded,
    powerless, immediate, trapped, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(udhr_article_3__procedural_hybrid_reading, judiciary).
narrative_ontology:fixing_cost_class(udhr_article_3__procedural_hybrid_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared procedural floor — habeas corpus, judicial review of detention, absolute torture prohibition — on which parties holding incompatible substantive visions of liberty and welfare can coexist in one legal order, and under which anyone's custody can be tested by someone other than the custodian.
% TRANSFER_FUNCTION: Moves adjudicative authority and public funds from the executive and taxpayer base to courts and the bar; moves protection to persons in state custody, at full strength in ordinary process and thinning at the emergency and administrative margins; moves compliance costs to the security apparatus.
% ABSENT_VOICES: Populations under regimes with no procedural floor are wholly outside the conversation and would object that the guarantee protects the already-protected; unrepresented detainees inside administrative regimes would object that hearings without counsel are process in form only. Neither voice reaches the drafting, amendment, or interpretive tables.
% DISAPPEARANCE_RATIONALE: Overnight loss of habeas and the torture prohibition would make custody unreviewable, remove the last internal brake on interrogation practice, and collapse the executive-judicial settlement on which constitutional government in these states rests; detention practice and interstate human-rights politics would reorganize around the absence within months.
% FOUNDING_PROBLEM: Arbitrary state detention and torture: the lettre de cachet, the star chamber, colonial emergency ordinances, and the mid-twentieth-century catalog of secret police and camps that the post-war drafters codified individual security against.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: UN Human Rights Committee concluding observations and ECtHR judgments document continuing arbitrary detention and torture across jurisdictions; Amnesty International and Human Rights Watch reporting; national truth commissions. The judiciary and bar also attest the problem, but they profit from the machinery, so the external treaty-body and NGO record carries the attestation.
narrative_ontology:disappearance_verdict(udhr_article_3__procedural_hybrid_reading, world_rearranges).
narrative_ontology:founding_problem_status(udhr_article_3__procedural_hybrid_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(udhr_article_3__procedural_hybrid_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(udhr_article_3__procedural_hybrid_reading, 'none', 1).
narrative_ontology:epsilon_provenance(udhr_article_3__procedural_hybrid_reading, 0.44, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(udhr_article_3__procedural_hybrid_reading_tests).
:- end_tests(udhr_article_3__procedural_hybrid_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.44 at interval end) because the guarantee delivers genuine protection in ordinary process while leaking asymmetrically at the margins: the emergency-derogation channel is exercised by the very organ it restrains, administrative-detention regimes survive deferential review, and effective access tracks resources. Suppression (0.50) is the coercive force holding the arrangement in place — courts compelling the executive, the constitutional order foreclosing extrajudicial alternatives — and is authored as a raw structural property, unscaled by power or scope; only extractiveness is scaled downstream by directionality and scope. Theater (0.31) reflects real daily function in ordinary criminal process with ceremonial surplus concentrating where enforcement is weakest. Resistance (0.55) records sustained executive pushback — emergency self-certification, deference-seeking, periodic jurisdiction-stripping proposals — plus wholesale rejection outside constitutional states. Accessibility collapse (0.40): alternatives remain conceivable and are foreclosed only inside constitutional orders. Claimed type and metrics are authored independently: tangled_rope is asserted from the structure (a real coordination floor plus same-structure payment by administrative detainees and the executive), and the metric values describe observed operation without tuning toward any predicted engine output. All three tracked series run on one shared seven-point grid (1948–2025) so no metric row borrows another's end-state; the trajectories show enforcement capacity built through the late twentieth century, a post-2001 extraction and theater peak, and partial retreat with ratchet residue.
 *
 * PERSPECTIVAL GAP:
 *   Four seats read the same text differently. The judiciary experiences the guarantee as its own institutional substance — review is not something the court does for others but what the court is; its exit is identity-bound. The executive experiences it as friction with a self-certified relief valve, tolerable because derogation recovers in emergencies what compliance costs in peacetime. Administrative detainees experience the procedural form without the protective substance — hearings occur and custody continues. Taxpayers barely perceive the arrangement at all. The engine computes these divergent per-seat classifications from the structural data; nothing in the authored claim adjudicates among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive the judiciary, bar, and persons-facing-coercion toward the low-d end; victim declarations drive the executive, administrative detainees, and taxpayers toward the high-d end. Persons facing state coercion are dual-positioned (protected in ordinary process, exposed at the margins), which the secondary payer role encodes; their effective d sits intermediate-low rather than at the beneficiary pole. The derivation chain suffices here, so no directionality overrides are authored: an override keyed to the institutional power atom would hit both the judiciary and the executive, which sit at opposite ends. One known residual: the executive's legitimacy dividend from operating under review is invisible to the derivation, so its computed extraction may slightly overshoot — accepted rather than corrected at the available granularity.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — arbitrary detention and torture — is live and externally corroborated, so no mandate has outlived its function; the R5 mismatch consumer finds status=live against verdict=world_rearranges, producing no zombie flag. The classification discipline cuts both ways: the genuine coordination function (a common floor letting incompatible substantive visions share one legal order) blocks a pure-extraction reading, while the same-structure payment by administrative detainees and the executive blocks a pure-coordination reading. If the emergency channel were closed and access equalized, the arrangement would migrate toward the coordination pole; if derogation became the rule rather than the exception, it would drift toward the extraction pole. The temporal series shows both pressures — the post-2001 peak and partial retreat — with neither pole winning.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_location_disagreement,
    'This constraint is one reading of kernel udhr_article_3 (procedural_hybrid_reading). Where exactly do the three readings diverge, and what would each sibling change structurally?',
    'Doctrinal analysis of the drafting history (Travaux préparatoires of the UDHR and ICCPR Article 9) plus comparative uptake: which obligations domestic and regional courts actually enforce under ''life, liberty and security of person.''',
    'negative_liberty_reading would rebuild the arrangement with the state as systematic target and immunity-from-interference, not process, as the protected good; positive_entitlement_reading would rebuild it with provider obligations and a material transfer surface. Victim sets, transfer functions, and epsilon are reading-indexed and not comparable across the family.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_location_disagreement, conceptual, 'Committer structure: one kernel, three readings; disagreement located in whether ''liberty and security of person'' carries substantive content beyond process.').

omega_variable(
    emergency_derogation_structural_status,
    'Is the emergency-derogation channel a designed safety valve integral to the guarantee''s durability, or a channel that suspends the guarantee precisely when it is most needed?',
    'Comparative derogation data: notification frequency and duration under ICCPR Article 4, judicial scrutiny intensity of derogation claims, and correlation between derogation episodes and subsequent permanent detention legislation.',
    'Valve-reading attributes part of measured extraction to the price of durability; abuse-reading raises epsilon at the emergency margin and supports treating derogation as the arrangement''s load-bearing weakness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(emergency_derogation_structural_status, empirical, 'Whether derogation is a functional relief valve or the guarantee''s practical negation.').

omega_variable(
    torture_enforcement_gap_attribution,
    'Do rendition, complicity, and diplomatic-assurance practices count as failures of the torture prohibition or deviations from it?',
    'Trace enforcement responses to documented cases — prosecutions, inquiries, reparations — against continued practice rates; an arrangement that sanctions deviation is failing, one that lacks reach is truncated.',
    'Failure-attribution raises epsilon and the theater measure at the torture margin; deviation-attribution holds epsilon moderate and locates the defect outside the guarantee itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(torture_enforcement_gap_attribution, empirical, 'Attribution of the enforcement gap between the declared torture prohibition and actual practice.').

omega_variable(
    two_tier_access_resource_dependence,
    'Is effective access to habeas corpus and judicial review sufficiently resource-dependent that the guarantee operates as two-tier protection?',
    'Legal aid funding series, pro se petition outcome rates, and detention-length distributions by representation status.',
    'Confirmed two-tier access raises epsilon (costs concentrate on the least resourced) and strengthens the judiciary-and-bar reading of where the arrangement''s gains land.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(two_tier_access_resource_dependence, empirical, 'Resource dependence of effective procedural access.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(udhr_article_3__procedural_hybrid_reading, 1948, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(udhr_tr_t1948, udhr_article_3__procedural_hybrid_reading, theater_ratio, 1948, 0.18).
narrative_ontology:measurement_basis(udhr_tr_t1948, observed).
narrative_ontology:measurement(udhr_tr_t1966, udhr_article_3__procedural_hybrid_reading, theater_ratio, 1966, 0.23).
narrative_ontology:measurement_basis(udhr_tr_t1966, observed).
narrative_ontology:measurement(udhr_tr_t1980, udhr_article_3__procedural_hybrid_reading, theater_ratio, 1980, 0.26).
narrative_ontology:measurement_basis(udhr_tr_t1980, observed).
narrative_ontology:measurement(udhr_tr_t1994, udhr_article_3__procedural_hybrid_reading, theater_ratio, 1994, 0.28).
narrative_ontology:measurement_basis(udhr_tr_t1994, observed).
narrative_ontology:measurement(udhr_tr_t2001, udhr_article_3__procedural_hybrid_reading, theater_ratio, 2001, 0.39).
narrative_ontology:measurement_basis(udhr_tr_t2001, observed).
narrative_ontology:measurement(udhr_tr_t2010, udhr_article_3__procedural_hybrid_reading, theater_ratio, 2010, 0.35).
narrative_ontology:measurement_basis(udhr_tr_t2010, observed).
narrative_ontology:measurement(udhr_tr_t2025, udhr_article_3__procedural_hybrid_reading, theater_ratio, 2025, 0.31).
narrative_ontology:measurement_basis(udhr_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(udhr_be_t1948, udhr_article_3__procedural_hybrid_reading, base_extractiveness, 1948, 0.26).
narrative_ontology:measurement_basis(udhr_be_t1948, observed).
narrative_ontology:measurement(udhr_be_t1966, udhr_article_3__procedural_hybrid_reading, base_extractiveness, 1966, 0.32).
narrative_ontology:measurement_basis(udhr_be_t1966, observed).
narrative_ontology:measurement(udhr_be_t1980, udhr_article_3__procedural_hybrid_reading, base_extractiveness, 1980, 0.37).
narrative_ontology:measurement_basis(udhr_be_t1980, observed).
narrative_ontology:measurement(udhr_be_t1994, udhr_article_3__procedural_hybrid_reading, base_extractiveness, 1994, 0.39).
narrative_ontology:measurement_basis(udhr_be_t1994, observed).
narrative_ontology:measurement(udhr_be_t2001, udhr_article_3__procedural_hybrid_reading, base_extractiveness, 2001, 0.46).
narrative_ontology:measurement_basis(udhr_be_t2001, observed).
narrative_ontology:measurement(udhr_be_t2010, udhr_article_3__procedural_hybrid_reading, base_extractiveness, 2010, 0.48).
narrative_ontology:measurement_basis(udhr_be_t2010, observed).
narrative_ontology:measurement(udhr_be_t2025, udhr_article_3__procedural_hybrid_reading, base_extractiveness, 2025, 0.44).
narrative_ontology:measurement_basis(udhr_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(udhr_su_t1948, udhr_article_3__procedural_hybrid_reading, suppression_requirement, 1948, 0.34).
narrative_ontology:measurement_basis(udhr_su_t1948, observed).
narrative_ontology:measurement(udhr_su_t1966, udhr_article_3__procedural_hybrid_reading, suppression_requirement, 1966, 0.41).
narrative_ontology:measurement_basis(udhr_su_t1966, observed).
narrative_ontology:measurement(udhr_su_t1980, udhr_article_3__procedural_hybrid_reading, suppression_requirement, 1980, 0.49).
narrative_ontology:measurement_basis(udhr_su_t1980, observed).
narrative_ontology:measurement(udhr_su_t1994, udhr_article_3__procedural_hybrid_reading, suppression_requirement, 1994, 0.55).
narrative_ontology:measurement_basis(udhr_su_t1994, observed).
narrative_ontology:measurement(udhr_su_t2001, udhr_article_3__procedural_hybrid_reading, suppression_requirement, 2001, 0.58).
narrative_ontology:measurement_basis(udhr_su_t2001, observed).
narrative_ontology:measurement(udhr_su_t2010, udhr_article_3__procedural_hybrid_reading, suppression_requirement, 2010, 0.53).
narrative_ontology:measurement_basis(udhr_su_t2010, observed).
narrative_ontology:measurement(udhr_su_t2025, udhr_article_3__procedural_hybrid_reading, suppression_requirement, 2025, 0.5).
narrative_ontology:measurement_basis(udhr_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(udhr_article_3__procedural_hybrid_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(udhr_article_3__procedural_hybrid_reading, udhr_article_3__negative_liberty_reading).
narrative_ontology:affects_constraint(udhr_article_3__procedural_hybrid_reading, udhr_article_3__positive_entitlement_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'Article 3 rights' covers three structurally distinct claims with different epsilon values, victim sets, and transfer surfaces; per the epsilon-invariance principle they are authored as separate stories in one family. This procedural reading is the common substrate: both substantive siblings presuppose some procedural machinery while denying it is exhaustive. Edges run from this story to both siblings; the substantive readings, when authored, would link back and to each other.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
