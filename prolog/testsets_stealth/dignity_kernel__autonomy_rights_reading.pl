% ============================================================================
% CONSTRAINT STORY: dignity_kernel__autonomy_rights_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dignity_kernel__autonomy_rights_reading, []).

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
 *   constraint_id: dignity_kernel__autonomy_rights_reading
 *   human_readable: Autonomy-Rights Grounding of Human Dignity (AI Governance Reading)
 *   domain: theological ethics/technology governance/philosophical anthropology
 *
 * SUMMARY:
 *   A secular, rights-based account of human dignity — worth grounded in
 *   autonomy, rationality, and enforceable rights rather than in divine image
 *   or any capability-transcending source — operates as the governing
 *   standard for AI governance across most liberal jurisdictions: it
 *   justifies transparency mandates, accountability and audit duties, labor
 *   and privacy protections against algorithmic management, and a cautious,
 *   rights-gated posture toward human enhancement. This file instantiates ONE
 *   reading of the dignity_kernel (autonomy_rights_reading); the imago_dei
 *   and posthumanist readings are separate constraint stories with their own
 *   epsilon values, beneficiary/victim structures, and classifications,
 *   linked through network.affects_constraints. The epsilon authored here
 *   refers to the standing arrangement this reading produces —
 *   autonomy-rights-grounded AI governance as it actually operates — assessed
 *   by the reading's own lights: the reading affirms the protection the
 *   arrangement delivers and registers its own shortfalls (uneven
 *   enforcement, compliance-cost concentration, capacity-threshold
 *   exclusions, proof burdens shifted onto injured individuals). KEY AGENTS
 *   (by structural relationship): - rights_protected_individuals: Primary
 *   beneficiary (moderate/constrained) — receives disclosure, contestation
 *   channels, unevenly activated - large_ai_developers: Dual-positioned
 *   beneficiary-payer (institutional/arbitrage) — pays compliance costs,
 *   collects moat and legitimacy - audit_certification_industry: Secondary
 *   beneficiary (organized/mobile) — collects the accountability fee stream -
 *   algorithmically_managed_workers: Primary target (powerless/constrained) —
 *   bears monitoring and proof burdens - low_autonomy_persons: Structural
 *   casualty of the ground-claim (powerless/trapped) — protection runs
 *   derivatively through proxies - small_ai_developers: Target
 *   (moderate/constrained) — fixed compliance costs concentrate the market
 *   against them - constitutional_courts_rights_bodies: Agenda setter
 *   (institutional/constrained) — administers the standard, requires secular
 *   justification - disability_rights_advocates, theological_bioethicists:
 *   Excluded voices — their ground-level objections sit outside the official
 *   register of reasons - technology_ethics_scholars: Analytical observer —
 *   sees the full structure, collects nothing
 *
 * KEY AGENTS:
 *   - rights_protected_individuals: Primary beneficiary (moderate/constrained) — holds disclosure and contestation entitlements, activated unevenly
 *   - large_ai_developers: Dual-positioned beneficiary-payer (institutional/arbitrage) — funds compliance, harvests moat and social license
 *   - audit_certification_industry: Secondary beneficiary (organized/mobile) — collects assessment and certification revenue
 *   - algorithmically_managed_workers: Primary target (powerless/constrained) — bears automated management and individualized proof burdens
 *   - low_autonomy_persons: Structural casualty of the ground-claim (powerless/trapped) — claims mediated through guardianship and best-interest proxies
 *   - small_ai_developers: Target (moderate/constrained) — fixed compliance costs push toward acquisition or exit
 *   - constitutional_courts_rights_bodies: Agenda setter (institutional/constrained) — adjudicates the standard, enforces confession-neutrality
 *   - disability_rights_advocates: Excluded voice (organized/generational) — capacity-critique unaccommodated inside operative tests
 *   - theological_bioethicists: Excluded voice (moderate/generational) — ground-claim barred from official justification
 *   - technology_ethics_scholars: Analytical observer (analytical/generational) — measures enforcement gaps and theater
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dignity_kernel__autonomy_rights_reading, 0.48).
domain_priors:suppression_score(dignity_kernel__autonomy_rights_reading, 0.52).
domain_priors:theater_ratio(dignity_kernel__autonomy_rights_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dignity_kernel__autonomy_rights_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(dignity_kernel__autonomy_rights_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(dignity_kernel__autonomy_rights_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dignity_kernel__autonomy_rights_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(dignity_kernel__autonomy_rights_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dignity_kernel__autonomy_rights_reading, tangled_rope).
narrative_ontology:human_readable(dignity_kernel__autonomy_rights_reading, "Autonomy-Rights Grounding of Human Dignity (AI Governance Reading)").
narrative_ontology:topic_domain(dignity_kernel__autonomy_rights_reading, "theological ethics/technology governance/philosophical anthropology").

domain_priors:requires_active_enforcement(dignity_kernel__autonomy_rights_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dignity_kernel__autonomy_rights_reading, 'cc5d142d-c8c4-4313-8886-d43462446cb5').
narrative_ontology:cs_kernel_codification('cc5d142d-c8c4-4313-8886-d43462446cb5', formalized).
narrative_ontology:cs_authority_grounding('cc5d142d-c8c4-4313-8886-d43462446cb5', lineage).
narrative_ontology:cs_interpretation_layer_present('cc5d142d-c8c4-4313-8886-d43462446cb5').
narrative_ontology:cs_reading_relation('cc5d142d-c8c4-4313-8886-d43462446cb5', dignity_kernel__imago_dei_reading, forecloses).
narrative_ontology:cs_reading_relation('cc5d142d-c8c4-4313-8886-d43462446cb5', dignity_kernel__posthumanist_reading, influences).
narrative_ontology:cs_axiom('cc5d142d-c8c4-4313-8886-d43462446cb5', foundational, dignity_ground_is_autonomy_rationality).
narrative_ontology:cs_axiom_status(dignity_ground_is_autonomy_rationality, holdable).
narrative_ontology:cs_axiom_grounding('cc5d142d-c8c4-4313-8886-d43462446cb5', dignity_ground_is_autonomy_rationality, deontological).
narrative_ontology:cs_axiom('cc5d142d-c8c4-4313-8886-d43462446cb5', foundational, enhancement_permitted_only_within_rights_limits).
narrative_ontology:cs_axiom_status(enhancement_permitted_only_within_rights_limits, holdable).
narrative_ontology:cs_axiom_grounding('cc5d142d-c8c4-4313-8886-d43462446cb5', enhancement_permitted_only_within_rights_limits, instrumental).
narrative_ontology:cs_axiom('cc5d142d-c8c4-4313-8886-d43462446cb5', secondary, legitimate_ai_treatment_requires_transparency_and_accountability).
narrative_ontology:cs_axiom_status(legitimate_ai_treatment_requires_transparency_and_accountability, holdable).
narrative_ontology:cs_axiom_grounding('cc5d142d-c8c4-4313-8886-d43462446cb5', legitimate_ai_treatment_requires_transparency_and_accountability, instrumental).
narrative_ontology:cs_reference_frame('cc5d142d-c8c4-4313-8886-d43462446cb5', autonomy_grounded_rights_framework).
narrative_ontology:cs_drift_state('cc5d142d-c8c4-4313-8886-d43462446cb5', contemporary_ai_governance_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('cc5d142d-c8c4-4313-8886-d43462446cb5', '').
narrative_ontology:cs_kernel_id(dignity_kernel__autonomy_rights_reading, dignity_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dignity_kernel__autonomy_rights_reading, rights_protected_individuals).
narrative_ontology:constraint_beneficiary(dignity_kernel__autonomy_rights_reading, large_ai_developers).
narrative_ontology:constraint_beneficiary(dignity_kernel__autonomy_rights_reading, audit_certification_industry).
narrative_ontology:constraint_victim(dignity_kernel__autonomy_rights_reading, algorithmically_managed_workers).
narrative_ontology:constraint_victim(dignity_kernel__autonomy_rights_reading, low_autonomy_persons).
narrative_ontology:constraint_victim(dignity_kernel__autonomy_rights_reading, small_ai_developers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(dignity_kernel__autonomy_rights_reading, large_ai_developers).
narrative_ontology:constraint_victim(dignity_kernel__autonomy_rights_reading, enhancement_applicants).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold data-protection and due-process entitlements against AI-mediated decisions about them: disclosure of automated decision-making, access and correction rights, channels to contest outcomes. They receive the frame's protections unevenly, since activation requires detecting harm and filing claims, and they carry consent fatigue from pervasive notice-and-consent requests. Leaving the frame's jurisdiction means emigration or digital abstention, both costly.
narrative_ontology:constraint_stakeholder(dignity_kernel__autonomy_rights_reading, rights_protected_individuals, beneficiary,
    moderate, biographical, constrained, global).

% Operate the large training and deployment pipelines the governance regime targets. They fund compliance programs, publish model documentation, and staff ethics functions; in exchange they accumulate certifications, regulatory goodwill, and a compliance barrier that smaller rivals struggle to match. They can shift operations, incorporate in favorable jurisdictions, or lobby to reshape the rules; exiting regulated markets altogether is rarely worth it given installed demand.
narrative_ontology:constraint_stakeholder(dignity_kernel__autonomy_rights_reading, large_ai_developers, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(dignity_kernel__autonomy_rights_reading, large_ai_developers, payer).

% Sells conformity assessments, bias audits, impact assessments, and certification marks to deployers facing statutory accountability duties. Revenue scales with the volume of mandated assessment; the industry has no exposure to the downstream harms its assessments certify against. Its work is portable across clients and jurisdictions.
narrative_ontology:constraint_stakeholder(dignity_kernel__autonomy_rights_reading, audit_certification_industry, beneficiary,
    organized, biographical, mobile, global).

% Are scheduled, rated, dispatched, and sometimes dismissed by automated management systems. On paper they hold transparency and contestation entitlements; exercising them means individually discovering the logic that ranked them, requesting explanations, and appealing through processes built around documentation they rarely possess. Income depends on staying on the platforms; deleting the account is the nominal exit.
narrative_ontology:constraint_stakeholder(dignity_kernel__autonomy_rights_reading, algorithmically_managed_workers, payer,
    powerless, biographical, constrained, global).

% Infants, people with severe cognitive disabilities, and those with advanced dementia are the persons an autonomy-centered account of worth reaches least well: their claims proceed through guardianship, substituted judgment, and best-interest mediation rather than direct exercise of rights. Where AI systems allocate care resources, triage, or benefits, their interests are represented by others and weighted by criteria keyed to capacity they lack. They cannot exit dependence on arrangements made for them.
narrative_ontology:constraint_stakeholder(dignity_kernel__autonomy_rights_reading, low_autonomy_persons, payer,
    powerless, biographical, trapped, global).

% Face the same documentation, assessment, and registration duties as incumbents but without dedicated compliance staff. Fixed per-product compliance costs push them toward niche markets, acquisition, or withdrawal; several have relocated to lighter-touch jurisdictions. Selling the company or pivoting to unregulated tooling is the realistic exit.
narrative_ontology:constraint_stakeholder(dignity_kernel__autonomy_rights_reading, small_ai_developers, payer,
    moderate, immediate, constrained, regional).

% Seek cognitive, biological, or reproductive enhancements that the frame permits only after rights-compatible review: safety, consent validity, fairness of access, and non-coercion screening. They bear waiting periods, application costs, and denials; grey-market acquisition exists but carries legal and health risk. Relocating to permissive jurisdictions is available mainly to the wealthy.
narrative_ontology:constraint_stakeholder(dignity_kernel__autonomy_rights_reading, enhancement_applicants, payer,
    moderate, biographical, constrained, national).

% Adjudicate what the dignity standard requires in concrete disputes: proportionality review of deployers' practices, precedent-setting on automated decision-making, and treaty-body supervision of national implementations. They maintain the standard's confession-neutrality by requiring secular justification in official reasoning. They cannot leave the framework they administer; their discretion runs to interpretation, not exit.
narrative_ontology:constraint_stakeholder(dignity_kernel__autonomy_rights_reading, constitutional_courts_rights_bodies, agenda_setter,
    institutional, generational, constrained, continental).

% Organize against capacity-keyed conceptions of worth and for relational understandings that attach status independent of rational capacity. They participate in consultations and litigation, but their ground-level objection — that anchoring status in autonomy excludes their constituents — finds no accommodation inside the frame's operative tests, which continue to route their constituents' claims through capacity proxies.
narrative_ontology:constraint_stakeholder(dignity_kernel__autonomy_rights_reading, disability_rights_advocates, excluded,
    organized, generational, constrained, global).

% Hold that worth attaches prior to any capability and argue from traditions that ground it theologically. Secular-justification conventions exclude their reasoning from regulatory and judicial decision documents; they publish, advise informally, and litigate at the margins, but the official register of reasons is closed to their ground-claim.
narrative_ontology:constraint_stakeholder(dignity_kernel__autonomy_rights_reading, theological_bioethicists, excluded,
    moderate, generational, constrained, global).

% Study the frame's operation: measuring enforcement gaps, documenting ethics-washing, tracing compliance-cost incidence, and comparing readings of the dignity standard across jurisdictions. They bear none of the frame's costs and collect none of its fees; publication and advisory roles are their entire exposure.
narrative_ontology:constraint_stakeholder(dignity_kernel__autonomy_rights_reading, technology_ethics_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(dignity_kernel__autonomy_rights_reading, large_ai_developers).
narrative_ontology:fixing_cost_class(dignity_kernel__autonomy_rights_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Supplies a portable, confession-neutral standard for what may be done to persons, enabling AI governance, research ethics, and cross-border data regimes among populations that disagree about ultimate grounds; coordinates deployers, regulators, and individuals on transparency, consent, and contestability expectations.
% TRANSFER_FUNCTION: Moves compliance expenditure from AI deployers to auditors, certifiers, and legal intermediaries; places justification burdens on deployers, who must show lawful basis and provide transparency; places remediation-initiation burdens on individuals, who must detect and prove rights violations to activate remedies; and converts regulatory compliance into market advantage for incumbents.
% ABSENT_VOICES: Theological bioethicists are barred from official justification by secular-justification conventions; disability-rights advocates' capacity-critique of the ground-claim is heard in consultation but not accommodated within the frame's operative reasoning; low_autonomy_persons themselves cannot speak in the autonomy-key the frame requires — their interests arrive only through proxies.
% DISAPPEARANCE_RATIONALE: If the autonomy-rights grounding vanished overnight, AI governance loses its operative justificatory standard: data-protection and accountability law lose their doctrinal basis, transparency mandates become unenforceable preferences, cross-border data-flow agreements fragment along theological or national lines, and the accountability industry loses its jurisdiction. Deployers would reorganize around whatever ground each jurisdiction supplies — corporate terms of service, state directives, or religious authority.
% FOUNDING_PROBLEM: Post-war reconstruction needed a basis for human-rights obligations that could bind states regardless of religious confession, after the mid-century catastrophes demonstrated where state-defined and capability-defined worth leads; subsequently, the rise of opaque algorithmic systems created the need for a portable standard governing artifacts that treat persons as data.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: the post-war drafting record (UDHR preparatory work, Nuremberg proceedings) is independently documented by historians; disability-rights organizations and theological ethicists — who reject this reading's ground — nonetheless attest that the underlying problem of institutional power treating persons as mere means or data remains live; competition-authority economic analyses independently document the compliance-cost concentration. No beneficiary-only attestation is relied upon.
narrative_ontology:disappearance_verdict(dignity_kernel__autonomy_rights_reading, world_rearranges).
narrative_ontology:founding_problem_status(dignity_kernel__autonomy_rights_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dignity_kernel__autonomy_rights_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(dignity_kernel__autonomy_rights_reading, 'none', 1).
narrative_ontology:epsilon_provenance(dignity_kernel__autonomy_rights_reading, 0.48, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dignity_kernel__autonomy_rights_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(dignity_kernel__autonomy_rights_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(dignity_kernel__autonomy_rights_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.48 at interval end) because the arrangement delivers real protection while carrying three identifiable extraction channels through the same structure: compliance costs that concentrate market power among incumbents, remediation burdens placed on the individuals harmed, and the capacity-threshold effect by which grounding worth in autonomy leaves low-capacity persons with derivative rather than direct protection. Suppression (0.52) is authored as a raw structural property — it is NOT scaled by power or scope in the way extractiveness is — reflecting the secular-justification requirement that bars rival grounds from official reasoning and the rights-gate that restricts enhancement alternatives; alternatives do persist (voluntary ethics regimes, religious bioethics councils, grey-market enhancement, jurisdictional arbitrage), which keeps accessibility_collapse moderate-low (0.40). Theater_ratio (0.42) reflects the documented ethics-washing problem: a substantial share of board, assessment, and audit activity certifies rather than changes systems, while transparency and data-protection enforcement remain functionally real. Resistance (0.50) records sustained industry pushback, disability-movement critique, theological objection, and accelerationist pressure. The claimed_type (tangled_rope) is authored from structure — a genuine coordination function (confession-neutral portable standard) PLUS asymmetric extraction PLUS active enforcement — independently of the metrics; the engine computes per-seat types from the structural data. All three temporal series run on one shared grid (T=0..30 in five-unit steps, one unit ≈ one year from the mid-1990s onset of comprehensive data-protection governance), so every tracked metric is authored at every examined time point. Coalition note: algorithmically_managed_workers hold the strongest latent coalition lever (platform-worker unionization), which is the main path by which that seat's computed position could shift.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the beneficiary/agenda-setter seats should compute differently. From the audit industry's seat the arrangement is a fee stream with no downside exposure; from the incumbent developer's seat it is a manageable cost that doubles as a barrier against smaller rivals; from the algorithmically managed worker's seat it is a paper entitlement whose activation requires resources they lack; from the low-autonomy person's seat it is a framework that weighs them through proxies keyed to capacity they do not have; from the constitutional court's seat it is a doctrine to be maintained and interpreted. Same-nominal-level actors diverge too: large and small developers face identical formal duties but opposite effective positions, because fixed compliance costs are regressive across firm size. The engine computes this divergence from the structural data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality: rights_protected_individuals (subsidized by the arrangement's protections, d near the beneficiary end), audit_certification_industry (pure collector, nearest the beneficiary end), large_ai_developers (declared beneficiary with a payer secondary role — the derivation blends moat gains against compliance costs). Victim declarations drive high directionality: algorithmically_managed_workers (constrained exit amplifies toward full-target), low_autonomy_persons (trapped, nearest the full-target end), small_ai_developers (constrained exit, high). The agenda setter (constitutional_courts_rights_bodies) sits near-symmetric: it administers the standard without collecting its fee streams. Excluded seats (disability_rights_advocates, theological_bioethicists) are commentary-grade absences, never correction-grade inputs. No directionality_overrides are authored: the derivation chain from beneficiary/victim declarations plus exit options reproduces the true structural relationships for every seat, and the schema's override surface is keyed by power atom rather than agent, so an override would leak across same-power seats with different relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem remains live (state and algorithmic power over persons persists; opaque systems proliferate), so the mismatch consumer reads status=live x verdict=world_rearranges — no zombie flag, and mandatrophy_resolved is authored false. The classification discipline prevents mislabeling in both directions: calling this arrangement a rope would hide the compliance-cost concentration, the proof-burden shifting, and the capacity-threshold exclusion that ride through the same enforcement machinery; calling it a snare would erase the genuine coordination achievement — a confession-neutral standard that makes AI governance possible among populations that disagree about ultimate grounds, and that no available alternative replicates at equal portability. The tangled_rope claim keeps both halves visible: the coordination function is real, and the extraction is real, and they are structurally entangled rather than separable.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_structural_delta,
    'This constraint instantiates the autonomy_rights_reading of dignity_kernel; what structural differences would adoption of the sibling readings (imago_dei_reading, posthumanist_reading) produce in victim sets, enhancement policy, and governance machinery?',
    'Cross-file comparison of the three reading stories'' beneficiary/victim arrays and structural deltas; the disagreement is located in the ground-of-dignity premise itself.',
    'If the imago_dei reading governed, the capacity-threshold victim seat shrinks (status attaches prior to capability) while enhancement restriction broadens; if the posthumanist reading governed, the rights-gate on enhancement dissolves and this reading''s enforcement layer loses part of its object. This file''s classification holds only for the autonomy-rights instantiation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_structural_delta, conceptual, 'Committer structure: reading-indexed classification of the dignity kernel; sibling readings are other constraints, not hedges inside this one.').

omega_variable(
    capacity_threshold_exclusion_status,
    'Is the weakened direct protection of low-capacity persons (infants, severe cognitive disability, advanced dementia) an inherent consequence of grounding dignity in autonomy and rationality, or an implementation artifact correctable within the reading?',
    'Comparative jurisprudence: track jurisdictions that supplement autonomy-grounded rights with relational-dignity doctrines and observe whether direct protection extends without abandoning the autonomy ground.',
    'If inherent, the low_autonomy_persons victim seat is permanent under this reading and that seat''s computed extraction stays elevated; if an artifact, the seat''s position is reformable and the arrangement trends back toward cleaner coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capacity_threshold_exclusion_status, conceptual, 'Whether the reading''s ground-claim necessarily excludes low-capacity persons from direct rights-bearing.').

omega_variable(
    compliance_cost_incidence,
    'Do transparency and accountability mandates function primarily as protection for data subjects or as fixed costs concentrating market power among incumbent developers?',
    'Market-concentration and small-firm-exit studies across GDPR-era and AI-Act-style compliance regimes; audit-fee flow analysis against measured harm reduction.',
    'If moat-dominated, the enforcement layer reads as an extraction channel and effective extraction for the payer seats rises; if protection-dominated, the coordination half of the arrangement strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(compliance_cost_incidence, empirical, 'Incidence of the accountability apparatus: protection delivered versus barrier rent accrued.').

omega_variable(
    accountability_theater_depth,
    'How much of the accountability apparatus (ethics boards, impact assessments, audit reports) alters system behavior versus certifying it?',
    'Track remediation outcomes following adverse audit findings; compare documented-change rates across certified systems over successive assessment cycles.',
    'High theater would push theater_ratio above 0.5 and date a drift signal toward inertial maintenance for the enforcement layer; low theater supports the tangled_rope reading with a functional enforcement half.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(accountability_theater_depth, empirical, 'Functional versus performative share of the transparency-and-accountability machinery.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dignity_kernel__autonomy_rights_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dign_tr_t0, dignity_kernel__autonomy_rights_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(dign_tr_t5, dignity_kernel__autonomy_rights_reading, theater_ratio, 5, 0.28).
narrative_ontology:measurement(dign_tr_t10, dignity_kernel__autonomy_rights_reading, theater_ratio, 10, 0.31).
narrative_ontology:measurement(dign_tr_t15, dignity_kernel__autonomy_rights_reading, theater_ratio, 15, 0.34).
narrative_ontology:measurement(dign_tr_t20, dignity_kernel__autonomy_rights_reading, theater_ratio, 20, 0.37).
narrative_ontology:measurement(dign_tr_t25, dignity_kernel__autonomy_rights_reading, theater_ratio, 25, 0.4).
narrative_ontology:measurement(dign_tr_t30, dignity_kernel__autonomy_rights_reading, theater_ratio, 30, 0.42).

% Extraction over time
narrative_ontology:measurement(dign_be_t0, dignity_kernel__autonomy_rights_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(dign_be_t5, dignity_kernel__autonomy_rights_reading, base_extractiveness, 5, 0.37).
narrative_ontology:measurement(dign_be_t10, dignity_kernel__autonomy_rights_reading, base_extractiveness, 10, 0.4).
narrative_ontology:measurement(dign_be_t15, dignity_kernel__autonomy_rights_reading, base_extractiveness, 15, 0.42).
narrative_ontology:measurement(dign_be_t20, dignity_kernel__autonomy_rights_reading, base_extractiveness, 20, 0.44).
narrative_ontology:measurement(dign_be_t25, dignity_kernel__autonomy_rights_reading, base_extractiveness, 25, 0.46).
narrative_ontology:measurement(dign_be_t30, dignity_kernel__autonomy_rights_reading, base_extractiveness, 30, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(dign_su_t0, dignity_kernel__autonomy_rights_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(dign_su_t5, dignity_kernel__autonomy_rights_reading, suppression_requirement, 5, 0.41).
narrative_ontology:measurement(dign_su_t10, dignity_kernel__autonomy_rights_reading, suppression_requirement, 10, 0.44).
narrative_ontology:measurement(dign_su_t15, dignity_kernel__autonomy_rights_reading, suppression_requirement, 15, 0.47).
narrative_ontology:measurement(dign_su_t20, dignity_kernel__autonomy_rights_reading, suppression_requirement, 20, 0.5).
narrative_ontology:measurement(dign_su_t25, dignity_kernel__autonomy_rights_reading, suppression_requirement, 25, 0.51).
narrative_ontology:measurement(dign_su_t30, dignity_kernel__autonomy_rights_reading, suppression_requirement, 30, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dignity_kernel__autonomy_rights_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(dignity_kernel__autonomy_rights_reading, dignity_kernel__imago_dei_reading).
narrative_ontology:affects_constraint(dignity_kernel__autonomy_rights_reading, dignity_kernel__posthumanist_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'human dignity' covers three structurally distinct claims about dignity's ground, decomposed per the epsilon-invariance principle into three stories sharing the dignity_kernel. Each member authors its own epsilon for its own instantiated arrangement — this file authors epsilon only for the autonomy-rights-grounded AI-governance arrangement as this reading assesses it; the imago_dei and posthumanist files author epsilon for their respective arrangements, and no value is averaged or hedged across readings. Upstream/downstream structure: the autonomy-rights reading is the operative legal frame in most jurisdictions, so it sets the legitimacy conditions under which the posthumanist enhancement program must operate (influences edge), while its exclusive ground-claim stands in direct contradiction to the imago-dei ground-claim (forecloses edge). All three files link one another through network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
