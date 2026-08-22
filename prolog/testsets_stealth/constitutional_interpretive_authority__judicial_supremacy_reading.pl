% ============================================================================
% CONSTRAINT STORY: constitutional_interpretive_authority__judicial_supremacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_interpretive_authority__judicial_supremacy_reading, []).

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
 *   constraint_id: constitutional_interpretive_authority__judicial_supremacy_reading
 *   human_readable: Judicial Supremacy in Constitutional Interpretation (Final-Authority Reading)
 *   domain: constitutional law/jurisprudence/political theory
 *
 * SUMMARY:
 *   Courts claim and exercise final interpretive authority over the
 *   constitution: their nullification of legislative acts binds, their
 *   precedents fix operative meaning, and no external institution adjudicates
 *   the boundaries of their own power. The arrangement solves a real
 *   coordination problem — constitutional disputes must terminate somewhere —
 *   while simultaneously transferring an effective veto over democratic
 *   lawmaking to an appointed, tenure-insulated body that certified its own
 *   authority. This file instantiates ONE reading of the
 *   constitutional_interpretive_authority kernel (the
 *   judicial_supremacy_reading) as a clean, epsilon-invariant constraint; the
 *   parliamentary and coordinate-construction siblings are separate files,
 *   not hedges folded into this one. KEY AGENTS (by structural relationship):
 *   constitutional_courts — agenda-setter and primary beneficiary
 *   (institutional/identity_locked), administers nullification and collects
 *   interpretive authority; elected_legislatures — primary target
 *   (powerful/constrained), acts subject to nullification;
 *   democratic_majorities — secondary target (organized/constrained), policy
 *   preferences overridden; rights_claiming_minorities — secondary
 *   beneficiary (powerless/constrained), protected forum; legal_profession —
 *   incidental beneficiary (moderate/constrained); executive_branch —
 *   dual-positioned (powerful/constrained), bears review and collects
 *   validation; popular_constitutionalism_advocates — excluded voice
 *   (moderate/constrained); comparative_constitutional_scholars — analytical
 *   observer (analytical/analytical).
 *
 * KEY AGENTS:
 *   - constitutional_courts: agenda-setter and primary beneficiary (institutional/identity_locked) — administers nullification, writes operative meaning, collects interpretive authority
 *   - elected_legislatures: primary target (powerful/constrained) — drafts under anticipated review, acts voidable after enactment
 *   - democratic_majorities: secondary target (organized/constrained) — electoral mandates overridden, remedies run through slow channels
 *   - rights_claiming_minorities: secondary beneficiary (powerless/constrained) — forum where rights claims beat recent majorities
 *   - legal_profession: incidental beneficiary (moderate/constrained) — prestige and expertise premised on the authoritative interpreter
 *   - executive_branch: dual-positioned payer/beneficiary (powerful/constrained) — subject to review, beneficiary of validation and appointments
 *   - popular_constitutionalism_advocates: excluded voice (moderate/constrained) — would relocate interpretive authority to citizens; holds no seat
 *   - comparative_constitutional_scholars: analytical observer (analytical/analytical) — evaluates the arrangement against sibling allocations
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_interpretive_authority__judicial_supremacy_reading, 0.63).
domain_priors:suppression_score(constitutional_interpretive_authority__judicial_supremacy_reading, 0.62).
domain_priors:theater_ratio(constitutional_interpretive_authority__judicial_supremacy_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_interpretive_authority__judicial_supremacy_reading, extractiveness, 0.63).
narrative_ontology:constraint_metric(constitutional_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(constitutional_interpretive_authority__judicial_supremacy_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_interpretive_authority__judicial_supremacy_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(constitutional_interpretive_authority__judicial_supremacy_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_interpretive_authority__judicial_supremacy_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_interpretive_authority__judicial_supremacy_reading, "Judicial Supremacy in Constitutional Interpretation (Final-Authority Reading)").
narrative_ontology:topic_domain(constitutional_interpretive_authority__judicial_supremacy_reading, "constitutional law/jurisprudence/political theory").

domain_priors:requires_active_enforcement(constitutional_interpretive_authority__judicial_supremacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_interpretive_authority__judicial_supremacy_reading, '7af9452b-c663-4903-99d2-1bc532575e8b').
narrative_ontology:cs_kernel_codification('7af9452b-c663-4903-99d2-1bc532575e8b', fixed_text).
narrative_ontology:cs_authority_grounding('7af9452b-c663-4903-99d2-1bc532575e8b', lineage).
narrative_ontology:cs_interpretation_layer_present('7af9452b-c663-4903-99d2-1bc532575e8b').
narrative_ontology:cs_reading_relation('7af9452b-c663-4903-99d2-1bc532575e8b', constitutional_interpretive_authority__parliamentary_supremacy_reading, forecloses).
narrative_ontology:cs_reading_relation('7af9452b-c663-4903-99d2-1bc532575e8b', constitutional_interpretive_authority__coordinate_construction_reading, forecloses).
narrative_ontology:cs_axiom('7af9452b-c663-4903-99d2-1bc532575e8b', foundational, judicial_finality_in_interpretation).
narrative_ontology:cs_axiom_status(judicial_finality_in_interpretation, holdable).
narrative_ontology:cs_axiom_grounding('7af9452b-c663-4903-99d2-1bc532575e8b', judicial_finality_in_interpretation, conventional).
narrative_ontology:cs_axiom('7af9452b-c663-4903-99d2-1bc532575e8b', foundational, rights_require_countermajoritarian_guardianship).
narrative_ontology:cs_axiom_status(rights_require_countermajoritarian_guardianship, holdable).
narrative_ontology:cs_axiom_grounding('7af9452b-c663-4903-99d2-1bc532575e8b', rights_require_countermajoritarian_guardianship, deontological).
narrative_ontology:cs_axiom('7af9452b-c663-4903-99d2-1bc532575e8b', secondary, nullification_voids_contrary_legislation).
narrative_ontology:cs_axiom_status(nullification_voids_contrary_legislation, holdable).
narrative_ontology:cs_axiom_grounding('7af9452b-c663-4903-99d2-1bc532575e8b', nullification_voids_contrary_legislation, conventional).
narrative_ontology:cs_reference_frame('7af9452b-c663-4903-99d2-1bc532575e8b', court_guarded_textual_supremacy).
narrative_ontology:cs_drift_state('7af9452b-c663-4903-99d2-1bc532575e8b', contemporary_consolidation_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('7af9452b-c663-4903-99d2-1bc532575e8b', '').
narrative_ontology:cs_kernel_id(constitutional_interpretive_authority__judicial_supremacy_reading, constitutional_interpretive_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_interpretive_authority__judicial_supremacy_reading, constitutional_courts).
narrative_ontology:constraint_beneficiary(constitutional_interpretive_authority__judicial_supremacy_reading, rights_claiming_minorities).
narrative_ontology:constraint_beneficiary(constitutional_interpretive_authority__judicial_supremacy_reading, legal_profession).
narrative_ontology:constraint_victim(constitutional_interpretive_authority__judicial_supremacy_reading, elected_legislatures).
narrative_ontology:constraint_victim(constitutional_interpretive_authority__judicial_supremacy_reading, democratic_majorities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(constitutional_interpretive_authority__judicial_supremacy_reading, executive_branch).
narrative_ontology:constraint_victim(constitutional_interpretive_authority__judicial_supremacy_reading, executive_branch).
narrative_ontology:constraint_vindicates(constitutional_interpretive_authority__judicial_supremacy_reading, countermajoritarian_rights_guardianship_doctrine).
narrative_ontology:constraint_vindicates(constitutional_interpretive_authority__judicial_supremacy_reading, constitutional_supremacy_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Decide which statutes and official acts conform to the constitution, and their declarations of invalidity bind the other branches. Through precedent they write the operative meaning of constitutional provisions, and in most systems they control their own docket and the reach of their jurisdiction. Members sit for long or life terms, insulated from electoral replacement. Relinquishing final interpretive authority would mean dissolving the role the institution exists to perform.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__judicial_supremacy_reading, constitutional_courts, agenda_setter,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(constitutional_interpretive_authority__judicial_supremacy_reading, constitutional_courts, beneficiary).

% Draft and enact statutes, raise revenue, and set policy on electoral mandates. Any statute can be struck down after enactment by a court applying its own reading of constitutional limits, so drafting proceeds under anticipated judicial review. Overriding an adverse ruling requires re-legislation shaped to the court's stated objections, constitutional amendment by supermajority, or waiting for the bench's composition to change.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__judicial_supremacy_reading, elected_legislatures, payer,
    powerful, biographical, constrained, national).

% Administers the state under the same judicial checking: agency rules can be vacated and official actions enjoined. At the same time, executives gain when courts validate their actions and lend them legitimacy, and they shape the bench through appointments. Their posture toward the arrangement alternates with whether they are currently winning or losing before it.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__judicial_supremacy_reading, executive_branch, payer,
    powerful, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(constitutional_interpretive_authority__judicial_supremacy_reading, executive_branch, beneficiary).

% Win elections and pass the measures they campaigned on, only to see contested ones suspended or voided by judges no one elected. Their remedies run through slow channels: repeated electoral wins until appointments shift, amendment campaigns that require supermajorities, or jurisdiction-curbing legislation that faces its own constitutional doubts.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__judicial_supremacy_reading, democratic_majorities, payer,
    organized, biographical, constrained, national).

% Use the courts as a forum where individual rights claims can prevail against recent electoral majorities: desegregation litigants, religious dissenters, criminal defendants, speech claimants. When the bench turns hostile they have little parallel recourse; the depth of their protection tracks the court's composition rather than their own organizing strength.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__judicial_supremacy_reading, rights_claiming_minorities, beneficiary,
    powerless, biographical, constrained, national).

% Argues the cases, staffs the chambers, and supplies the doctrinal commentary through which constitutional meaning circulates. Prestige and market value concentrate around constitutional litigation, and the profession's standing as a governing expertise presupposes an authoritative interpreter whose questions only it can answer.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__judicial_supremacy_reading, legal_profession, beneficiary,
    moderate, biographical, constrained, national).

% Movements and theorists who hold that constitutional meaning should be made by citizens, juries, and electoral politics rather than sealed by judicial pronouncement. They publish, organize, and occasionally win amendments, but hold no seat in the courtroom conversation where the operative meaning is actually fixed.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__judicial_supremacy_reading, popular_constitutionalism_advocates, excluded,
    moderate, generational, constrained, national).

% Study how different polities allocate interpretive finality — parliamentary-sovereignty systems, dialogue-model systems, strong-form review systems — and evaluate the arrangement's performance against its alternatives. They observe, testify, and propose, but decide nothing.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__judicial_supremacy_reading, comparative_constitutional_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(constitutional_interpretive_authority__judicial_supremacy_reading, constitutional_courts).
narrative_ontology:fixing_cost_class(constitutional_interpretive_authority__judicial_supremacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single final arbiter for disputes about what the constitution permits, so inter-branch conflicts terminate instead of escalating into recurring constitutional crises; gives dispersed individuals a standardized forum for rights claims; produces settled, citable meaning that officials, firms, and citizens can plan around.
% TRANSFER_FUNCTION: Moves interpretive authority — and with it an effective veto over legislation and administration — from elected legislatures and electoral majorities to an appointed, tenure-insulated judiciary; moves decision-making on contested moral and structural questions from legislative bargaining into adjudication.
% ABSENT_VOICES: Popular-constitutionalist movements and parliamentary-supremacy partisans stand outside the courtroom where operative meaning is fixed; future generations are bound by precedents they had no part in making; in systems where judicial review was established by judicial decision rather than constitutional text, the citizenry never expressly consented to the arrangement at all.
% DISAPPEARANCE_RATIONALE: If judicial nullification vanished overnight, legislatures would reclaim the final word on constitutional limits, rights enforcement would migrate into statutory protections, electoral competition, and amendment politics, and constitutional meaning would be continuously renegotiated among the branches as in dialogue-model systems. Thousands of precedents governing daily administration would lose their enforcing institution.
% FOUNDING_PROBLEM: Secure constitutional limits and fundamental rights against transient legislative majorities and inter-branch encroachment, and settle who decides when the branches disagree about the constitution's boundaries. The postwar European constitutional courts were built directly against majoritarian rights abuse; the American assertion answered a question the silent text left open.
% FOUNDING_PROBLEM_CORROBORATION: Minority-rights organizations and international human-rights bodies attest that majoritarian threats to fundamental rights persist. Comparative constitutional scholarship — including critics of judicial supremacy such as political-safeguards theorists and popular constitutionalists — attests both that the underlying problem remains live and that the parties dispute whether courts are its solution or its newest instance. The attestation does not rest on the judiciary's own account of itself.
narrative_ontology:disappearance_verdict(constitutional_interpretive_authority__judicial_supremacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_interpretive_authority__judicial_supremacy_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_interpretive_authority__judicial_supremacy_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(constitutional_interpretive_authority__judicial_supremacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_interpretive_authority__judicial_supremacy_reading, 0.63, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_interpretive_authority__judicial_supremacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(constitutional_interpretive_authority__judicial_supremacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(constitutional_interpretive_authority__judicial_supremacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is substantial (0.63 at interval end) because the arrangement transfers an effective legislative veto to a self-certifying body: the commission-like decoupling analog is that the scope of judicial power is set by the beneficiary of that power. Suppression (0.62) reflects active enforcement — injunctions, contempt, binding invalidation — plus the foreclosure of rival allocations inside the system; it is authored as a raw structural property and is NOT scaled by power or scope (only extractiveness is scaled, by directionality and spatial scope, in the engine's computation). Theater ratio (0.38) is rising: neutral-principle rhetoric, ceremonial opinion forms, and shadow-docket opacity increasingly dress policy choices as textual compulsion, though the core function — disputes actually terminate, rights claims actually adjudicated — remains real. Accessibility collapse (0.58) is moderate: once the arrangement is understood, ignoring a ruling means constitutional crisis, yet the sibling allocations persist as live positions elsewhere and in scholarship. Resistance (0.60) is high and recurring: court-curbing bills, jurisdiction-stripping proposals, packing threats, open non-compliance talk. Fixing cost is prohibitive: removal requires cross-generational supermajorities or successful court-curbing against entrenched precedent, exceeding any single actor's benefit. The measurement series run on ONE shared eight-point grid (1803–2026) so every tracked metric is authored at every examined time point. The extractiveness series OSCILLATES rather than drifting monotonically — Lochner-era excess (1905), New Deal retreat (1937), Warren-era resurgence (1954–1973), modern consolidation with a slight Dobbs-era dip (2000–2026). The cycle is driven by appointment pipelines and legitimacy feedback: excess triggers backlash, backlash reshapes the bench, the reshaped bench generates new excess. The oscillation is not noise and not intermittent reinforcement — it is the arrangement's homeostatic legitimacy mechanism, and base_properties are measured at the 2026 endpoint (late-consolidation phase).
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently from identical structural data. From the bench, the arrangement is guardianship: the coordination function (termination of constitutional dispute, rights forum, settled meaning) is experienced directly and daily. From the legislature, the same structure is subordination: a veto held by an unaccountable chamber over everything enacted. From the minority seat it is protection; from the majority seat it is override. The executive alternates between the two experiences depending on whether it is currently winning. The engine computes these per-seat classifications from power, exit, and directional position — the divergence between the agenda-setter's experience and the payers' experience is the measurement, not a defect to be reconciled. Democratic majorities retain coalition levers (appointment pipelines, amendment conventions) that individual powerless actors lack; their 'organized' power atom is what keeps their exit constrained rather than trapped.
 *
 * DIRECTIONALITY LOGIC:
 *   Constitutional_courts sit nearest the beneficiary pole: they administer the arrangement AND collect its product (interpretive authority), with identity-locked exit amplifying their entrenchment. Elected_legislatures and democratic_majorities sit near the target pole — they bear the transfer, and constrained exit (amendment only by supermajority, relief only via bench turnover) pushes their effective extraction upward. Rights_claiming_minorities are beneficiaries despite powerlessness: the arrangement subsidizes them with a protected forum, so their directionality stays low even though their exit is constrained. Legal_profession derives low-to-moderate directionality as incidental beneficiaries whose livelihood rides on the interpreter existing. Executive_branch is genuinely dual-positioned and lands mid-range: it pays when reviewed and collects when validated. Popular-constitutionalism advocates are excluded rather than coordinated — the arrangement's enforcement exists precisely to keep the finality question off their table. No directionality overrides were needed: the beneficiary/victim declarations plus exit atoms produce the correct relationships without correction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is live, so this is not a mandatrophy case: majoritarian rights abuse persists, inter-branch disputes recur, and the guardian function has continuous work. The tangled-rope classification guards against two symmetric errors. Reading the arrangement as pure extraction (snare) ignores that the coordination function is genuine and load-bearing — abolish finality overnight and constitutional conflict loses its termination point, which is why even fierce critics propose reform rather than abolition. Reading it as pure coordination (rope) ignores the asymmetric capture: the beneficiary of interpretive authority certifies the scope of its own authority, the victim set (legislatures, majorities) is identifiable, and enforcement is active and coercive. The hybrid classification keeps both facts on the table and lets the engine's per-seat computation surface the divergence the counter-majoritarian debate has argued about for two centuries.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_structural_delta,
    'This constraint is one reading of the constitutional_interpretive_authority kernel; how would instantiating the parliamentary_supremacy or coordinate_construction sibling change the beneficiary/victim structure and epsilon?',
    'Author and compile the sibling stories; compare computed per-seat classifications and effective extraction across the three readings over matched scenarios.',
    'Under parliamentary supremacy the judiciary leaves the beneficiary set and the legislature becomes the coordinating seat; under coordinate construction no seat captures interpretive authority and extraction diffuses across branches. The asymmetric profile authored here is contingent on the finality allocation, not on constitutionalism as such.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_structural_delta, conceptual, 'Committer structure: reading-contingent beneficiary/victim allocation within the interpretive-authority kernel.').

omega_variable(
    self_certified_scope_extraction,
    'How much of the measured extraction is intrinsic to final-authority arrangements generally, versus an artifact of the judiciary certifying its own jurisdiction, justiciability doctrines, and remedial powers?',
    'Compare strong-form review systems where review scope is enumerated in the constitutional text against systems where the court declared its own scope; measure nullification rates and subject-matter reach across the two designs.',
    'If externally enumerated scope narrows extraction materially, a constitutional amendment fixing jurisdiction would convert much of the authority transfer from capture into delegated coordination; if not, the extraction is intrinsic to judicial finality itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(self_certified_scope_extraction, empirical, 'Self-referential scope certification as a source of extraction.').

omega_variable(
    rights_legitimation_vs_suppression,
    'Does legitimation via rights-compliance reduce the arrangement''s true suppressive force (norm-internalized voluntary compliance) or conceal it (measured suppression understates the coercion legislators actually face)?',
    'Compliance studies contrasting legislative behavior after rights-framed nullifications versus policy-framed ones; surveys of legislators on anticipated-review chilling effects on the drafting stage.',
    'If chilling is broad, effective suppression exceeds the scalar and the arrangement constrains the legislative agenda far more than enforcement events show; if narrow, suppression is concentrated at the enforcement event and accurately measured.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rights_legitimation_vs_suppression, empirical, 'Whether rights-legitimated coercion is internalized or masked.').

omega_variable(
    guardian_or_policy_maker,
    'Do nullification decisions track rights-protective principle (guardian function dominant) or ideological policy preference (policy-making conducted in rights guise)?',
    'Systematic coding of nullification outcomes for rights-involvedness with judge-ideology controls; natural experiments from bench turnover altering outcomes on unchanged dockets.',
    'Guardian-dominant operation supports a coordination-heavy hybrid profile; preference-driven operation pushes the arrangement toward the pure-extraction boundary and raises effective epsilon.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(guardian_or_policy_maker, empirical, 'Coordination-dominant versus extraction-dominant operation of the nullification power.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_interpretive_authority__judicial_supremacy_reading, 1803, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t1803, constitutional_interpretive_authority__judicial_supremacy_reading, theater_ratio, 1803, 0.15).
narrative_ontology:measurement_basis(cons_tr_t1803, observed).
narrative_ontology:measurement(cons_tr_t1857, constitutional_interpretive_authority__judicial_supremacy_reading, theater_ratio, 1857, 0.22).
narrative_ontology:measurement_basis(cons_tr_t1857, observed).
narrative_ontology:measurement(cons_tr_t1905, constitutional_interpretive_authority__judicial_supremacy_reading, theater_ratio, 1905, 0.28).
narrative_ontology:measurement_basis(cons_tr_t1905, observed).
narrative_ontology:measurement(cons_tr_t1937, constitutional_interpretive_authority__judicial_supremacy_reading, theater_ratio, 1937, 0.2).
narrative_ontology:measurement_basis(cons_tr_t1937, observed).
narrative_ontology:measurement(cons_tr_t1954, constitutional_interpretive_authority__judicial_supremacy_reading, theater_ratio, 1954, 0.18).
narrative_ontology:measurement_basis(cons_tr_t1954, observed).
narrative_ontology:measurement(cons_tr_t1973, constitutional_interpretive_authority__judicial_supremacy_reading, theater_ratio, 1973, 0.26).
narrative_ontology:measurement_basis(cons_tr_t1973, observed).
narrative_ontology:measurement(cons_tr_t2000, constitutional_interpretive_authority__judicial_supremacy_reading, theater_ratio, 2000, 0.34).
narrative_ontology:measurement_basis(cons_tr_t2000, observed).
narrative_ontology:measurement(cons_tr_t2026, constitutional_interpretive_authority__judicial_supremacy_reading, theater_ratio, 2026, 0.38).
narrative_ontology:measurement_basis(cons_tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(cons_be_t1803, constitutional_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 1803, 0.28).
narrative_ontology:measurement_basis(cons_be_t1803, observed).
narrative_ontology:measurement(cons_be_t1857, constitutional_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 1857, 0.42).
narrative_ontology:measurement_basis(cons_be_t1857, observed).
narrative_ontology:measurement(cons_be_t1905, constitutional_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 1905, 0.56).
narrative_ontology:measurement_basis(cons_be_t1905, observed).
narrative_ontology:measurement(cons_be_t1937, constitutional_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 1937, 0.38).
narrative_ontology:measurement_basis(cons_be_t1937, observed).
narrative_ontology:measurement(cons_be_t1954, constitutional_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 1954, 0.48).
narrative_ontology:measurement_basis(cons_be_t1954, observed).
narrative_ontology:measurement(cons_be_t1973, constitutional_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 1973, 0.6).
narrative_ontology:measurement_basis(cons_be_t1973, observed).
narrative_ontology:measurement(cons_be_t2000, constitutional_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 2000, 0.66).
narrative_ontology:measurement_basis(cons_be_t2000, observed).
narrative_ontology:measurement(cons_be_t2026, constitutional_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 2026, 0.63).
narrative_ontology:measurement_basis(cons_be_t2026, observed).

% Suppression requirement over time
narrative_ontology:measurement(cons_su_t1803, constitutional_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 1803, 0.2).
narrative_ontology:measurement_basis(cons_su_t1803, observed).
narrative_ontology:measurement(cons_su_t1857, constitutional_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 1857, 0.3).
narrative_ontology:measurement_basis(cons_su_t1857, observed).
narrative_ontology:measurement(cons_su_t1905, constitutional_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 1905, 0.38).
narrative_ontology:measurement_basis(cons_su_t1905, observed).
narrative_ontology:measurement(cons_su_t1937, constitutional_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 1937, 0.45).
narrative_ontology:measurement_basis(cons_su_t1937, observed).
narrative_ontology:measurement(cons_su_t1954, constitutional_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 1954, 0.52).
narrative_ontology:measurement_basis(cons_su_t1954, observed).
narrative_ontology:measurement(cons_su_t1973, constitutional_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 1973, 0.55).
narrative_ontology:measurement_basis(cons_su_t1973, observed).
narrative_ontology:measurement(cons_su_t2000, constitutional_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 2000, 0.58).
narrative_ontology:measurement_basis(cons_su_t2000, observed).
narrative_ontology:measurement(cons_su_t2026, constitutional_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 2026, 0.62).
narrative_ontology:measurement_basis(cons_su_t2026, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_interpretive_authority__judicial_supremacy_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(constitutional_interpretive_authority__judicial_supremacy_reading, constitutional_interpretive_authority__parliamentary_supremacy_reading).
narrative_ontology:affects_constraint(constitutional_interpretive_authority__judicial_supremacy_reading, constitutional_interpretive_authority__coordinate_construction_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'judicial review' conflates three structurally distinct arrangements differing on the locus of final interpretive authority. Decomposed per the epsilon-invariance principle into a three-story family sharing the kernel constitutional_interpretive_authority: this file (judicial supremacy), parliamentary_supremacy_reading, and coordinate_construction_reading. Each carries its own epsilon, beneficiary/victim set, and classification; the edges record family membership. The judicial-supremacy reading is the historically assertive member — its self-establishment created the structural conditions to which the sibling readings respond.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
