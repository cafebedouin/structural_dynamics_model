% ============================================================================
% CONSTRAINT STORY: human_dignity_ai_governance__secular_humanist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_human_dignity_ai_governance__secular_humanist_reading, []).

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
 *   constraint_id: human_dignity_ai_governance__secular_humanist_reading
 *   human_readable: Secular Humanist Reading: Rights-Based Democratic AI Governance
 *   domain: theological_ethics/technology_governance/political_economy
 *
 * SUMMARY:
 *   This story instantiates the secular_humanist_reading of the
 *   human_dignity_ai_governance kernel: dignity grounded in rational autonomy
 *   and equal moral status under the UDHR framework, with AI governance
 *   authority vested in democratic legislatures and courts rather than
 *   religious magisterial authority. The reading produces moderate,
 *   rights-bounded constraints on AI systems — privacy, non-discrimination,
 *   due process — without requiring any comprehensive theological
 *   anthropology to be embedded in code or regulation. This is a genuinely
 *   distinct constraint from the sibling readings (magisterial_integralist,
 *   techno_optimist, pluralist_pragmatic), each of which grounds legitimacy,
 *   beneficiary sets, and enforcement mechanisms differently; per the
 *   ε-invariance principle, no attempt is made here to average across or
 *   gesture toward those readings within this story's classification.
 *
 * KEY AGENTS:
 *   - democratic_legislatures: agenda_setter (institutional/arbitrage) — draft and revise AI rights statutes through majoritarian process
 *   - courts: agenda_setter/observer (institutional/analytical) — interpret and enforce rights-based AI law
 *   - rights_bearing_citizens: beneficiary (organized/constrained) — hold enforceable legal claims against AI harms
 *   - non_citizen_migrants_excluded_from_franchise: payer (powerless/trapped) — subject to AI systems justified under a framework whose legitimacy source excludes them
 *   - religious_authorities: excluded (organized/constrained) — structurally denied formal governance standing by design
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(human_dignity_ai_governance__secular_humanist_reading, 0.32).
domain_priors:suppression_score(human_dignity_ai_governance__secular_humanist_reading, 0.28).
domain_priors:theater_ratio(human_dignity_ai_governance__secular_humanist_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(human_dignity_ai_governance__secular_humanist_reading, extractiveness, 0.32).
narrative_ontology:constraint_metric(human_dignity_ai_governance__secular_humanist_reading, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(human_dignity_ai_governance__secular_humanist_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(human_dignity_ai_governance__secular_humanist_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(human_dignity_ai_governance__secular_humanist_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(human_dignity_ai_governance__secular_humanist_reading, rope).
narrative_ontology:human_readable(human_dignity_ai_governance__secular_humanist_reading, "Secular Humanist Reading: Rights-Based Democratic AI Governance").
narrative_ontology:topic_domain(human_dignity_ai_governance__secular_humanist_reading, "theological_ethics/technology_governance/political_economy").

domain_priors:requires_active_enforcement(human_dignity_ai_governance__secular_humanist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(human_dignity_ai_governance__secular_humanist_reading, '6c06f6cb-01d0-42eb-80e9-6280299ee1c2').
narrative_ontology:cs_kernel_codification('6c06f6cb-01d0-42eb-80e9-6280299ee1c2', distributed).
narrative_ontology:cs_authority_grounding('6c06f6cb-01d0-42eb-80e9-6280299ee1c2', distributed).
narrative_ontology:cs_reading_relation('6c06f6cb-01d0-42eb-80e9-6280299ee1c2', human_dignity_ai_governance__magisterial_integralist_reading, forecloses).
narrative_ontology:cs_reading_relation('6c06f6cb-01d0-42eb-80e9-6280299ee1c2', human_dignity_ai_governance__techno_optimist_reading, coexists_with).
narrative_ontology:cs_reading_relation('6c06f6cb-01d0-42eb-80e9-6280299ee1c2', human_dignity_ai_governance__pluralist_pragmatic_reading, influences).
narrative_ontology:cs_axiom('6c06f6cb-01d0-42eb-80e9-6280299ee1c2', foundational, dignity_grounded_in_rational_autonomy_not_revelation).
narrative_ontology:cs_axiom_status(dignity_grounded_in_rational_autonomy_not_revelation, holdable).
narrative_ontology:cs_axiom_grounding('6c06f6cb-01d0-42eb-80e9-6280299ee1c2', dignity_grounded_in_rational_autonomy_not_revelation, deontological).
narrative_ontology:cs_axiom('6c06f6cb-01d0-42eb-80e9-6280299ee1c2', foundational, democratic_deliberation_is_sole_legitimate_source_of_binding_ai_governance_authority).
narrative_ontology:cs_axiom_status(democratic_deliberation_is_sole_legitimate_source_of_binding_ai_governance_authority, holdable).
narrative_ontology:cs_axiom_grounding('6c06f6cb-01d0-42eb-80e9-6280299ee1c2', democratic_deliberation_is_sole_legitimate_source_of_binding_ai_governance_authority, conventional).
narrative_ontology:cs_reference_frame('6c06f6cb-01d0-42eb-80e9-6280299ee1c2', liberal_democratic_rights_consensus).
narrative_ontology:cs_drift_state('6c06f6cb-01d0-42eb-80e9-6280299ee1c2', contemporary_ai_governance_era, gap(revival_pressure, substantial, false)).
narrative_ontology:cs_created_at('6c06f6cb-01d0-42eb-80e9-6280299ee1c2', '').
narrative_ontology:cs_kernel_id(human_dignity_ai_governance__secular_humanist_reading, human_dignity_ai_governance).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(human_dignity_ai_governance__secular_humanist_reading, rights_bearing_citizens).
narrative_ontology:constraint_beneficiary(human_dignity_ai_governance__secular_humanist_reading, democratic_legislatures).
narrative_ontology:constraint_beneficiary(human_dignity_ai_governance__secular_humanist_reading, human_rights_litigants).
narrative_ontology:constraint_beneficiary(human_dignity_ai_governance__secular_humanist_reading, secular_civil_society_organizations).
narrative_ontology:constraint_victim(human_dignity_ai_governance__secular_humanist_reading, non_citizen_migrants_excluded_from_franchise).
narrative_ontology:constraint_victim(human_dignity_ai_governance__secular_humanist_reading, religious_minorities_seeking_faith_based_exemptions).
narrative_ontology:constraint_victim(human_dignity_ai_governance__secular_humanist_reading, populations_under_non_democratic_governments).
narrative_ontology:constraint_victim(human_dignity_ai_governance__secular_humanist_reading, stateless_persons).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enjoy legal protections against AI-driven discrimination, privacy violations, and due-process failures because courts and legislatures ground dignity in rights language they can invoke directly. Their standing to sue or lobby depends on citizenship and legal personhood recognized by the democratic state.
narrative_ontology:constraint_stakeholder(human_dignity_ai_governance__secular_humanist_reading, rights_bearing_citizens, beneficiary,
    organized, generational, constrained, national).

% Draft and pass AI regulation (privacy statutes, anti-discrimination law, algorithmic due-process requirements) through deliberative procedure. They derive legitimacy from majoritarian mandate, not theological warrant, and can revise the framework through ordinary political process — giving them far more flexibility than a framework bound to a fixed doctrinal text.
narrative_ontology:constraint_stakeholder(human_dignity_ai_governance__secular_humanist_reading, democratic_legislatures, agenda_setter,
    institutional, generational, arbitrage, national).

% Interpret and enforce rights-based AI statutes, adjudicating disputes about algorithmic discrimination or privacy breach. They ground rulings in constitutional and human-rights jurisprudence rather than in any single religious tradition, which lets them arbitrate across a pluralistic citizenry but also makes their authority contingent on continued public confidence in secular legal reasoning.
narrative_ontology:constraint_stakeholder(human_dignity_ai_governance__secular_humanist_reading, courts, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(human_dignity_ai_governance__secular_humanist_reading, courts, observer).

% Bring cases against AI systems (facial recognition misidentification, algorithmic hiring bias) using UDHR-derived legal doctrine. They benefit from a rights vocabulary that does not require them to share any particular faith commitment to claim protection, but their remedy depends entirely on functioning courts and enforceable statute.
narrative_ontology:constraint_stakeholder(human_dignity_ai_governance__secular_humanist_reading, human_rights_litigants, beneficiary,
    moderate, biographical, constrained, national).

% NGOs and advocacy groups build policy influence, funding, and institutional standing by advancing the rights-based framing of AI governance in legislative hearings and international bodies. Their expertise and access are the direct product of dignity being defined in terms courts and parliaments recognize.
narrative_ontology:constraint_stakeholder(human_dignity_ai_governance__secular_humanist_reading, secular_civil_society_organizations, beneficiary,
    organized, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(human_dignity_ai_governance__secular_humanist_reading, secular_civil_society_organizations, agenda_setter).

% Are subject to AI-driven immigration and border-control systems (risk scoring, biometric surveillance) but have no vote and limited standing in the democratic deliberation that sets the governing rules. The framework's legitimacy claim rests on democratic process they are structurally excluded from.
narrative_ontology:constraint_stakeholder(human_dignity_ai_governance__secular_humanist_reading, non_citizen_migrants_excluded_from_franchise, payer,
    powerless, immediate, trapped, national).

% Object on theological grounds to specific AI applications (e.g., predictive policing profiling, reproductive-health algorithms) but find the secular legal framework structurally unreceptive to claims grounded in doctrine rather than rights language; their exemption requests must be translated into secular legal categories to be heard at all, which some communities regard as a form of erasure.
narrative_ontology:constraint_stakeholder(human_dignity_ai_governance__secular_humanist_reading, religious_minorities_seeking_faith_based_exemptions, payer,
    moderate, biographical, constrained, national).

% Live under AI governance regimes where no functioning democratic deliberation exists to generate or enforce the rights protections this reading presupposes. The framework's remedy mechanism (courts, legislatures) is simply unavailable to them, leaving the dignity claim theoretically applicable but practically inert.
narrative_ontology:constraint_stakeholder(human_dignity_ai_governance__secular_humanist_reading, populations_under_non_democratic_governments, excluded,
    powerless, generational, trapped, national).

% Fall outside any single state's citizenry and thus outside the primary enforcement unit (national courts and legislatures) that operationalizes UDHR-derived rights against AI harms. International human-rights instruments nominally cover them, but the enforcement machinery this reading relies on is state-bound.
narrative_ontology:constraint_stakeholder(human_dignity_ai_governance__secular_humanist_reading, stateless_persons, payer,
    powerless, immediate, trapped, global).

% Are structurally excluded from binding AI governance authority under this reading — their theological anthropology may inform private moral reasoning but carries no formal weight in legislative or judicial determination of AI regulation, a deliberate design feature of the secular framework rather than an oversight.
narrative_ontology:constraint_stakeholder(human_dignity_ai_governance__secular_humanist_reading, religious_authorities, excluded,
    organized, civilizational, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a common, religiously-neutral vocabulary (rational autonomy, equal moral status, universal rights) that lets a pluralistic citizenry — across faiths and none — converge on enforceable AI governance rules without requiring agreement on any single theological or metaphysical foundation.
% TRANSFER_FUNCTION: Moves rule-making authority over AI systems away from religious institutions and toward democratically elected legislatures and courts; moves protective legal standing toward those recognized as rights-bearing citizens and away from those outside the citizenry or outside functioning democratic process.
% ABSENT_VOICES: Non-citizen migrants, stateless persons, and populations under non-democratic governments would object that the framework's legitimacy claim (grounded in democratic deliberation) structurally cannot reach them, yet AI systems justified under this framework are deployed against them (border AI, biometric surveillance) regardless. Religious minorities seeking faith-based exemptions are present in the polity but must translate their claims into secular rights language to be heard, muting the theological register entirely.
% DISAPPEARANCE_RATIONALE: If this reading's legal architecture vanished overnight, AI governance would revert to contested ground among the sibling readings: religious authorities would press for magisterial standing, industry actors would press for minimal-restriction techno-optimist governance, and existing rights-based statutes, court precedents, and litigation strategies built on UDHR grounding would lose their doctrinal anchor — a substantial portion of current AI regulation (GDPR-style privacy law, anti-discrimination statute, algorithmic due-process requirements) is built directly on this framework's premises.
% FOUNDING_PROBLEM: Post-WWII international order needed a basis for universal human rights claims that did not require agreement on any single religion or metaphysics, so states with radically different theological and cultural traditions could still commit to common baseline protections — and, more recently, so AI governance could proceed without ceding authority to any one faith tradition in pluralistic democracies.
% FOUNDING_PROBLEM_CORROBORATION: Secular civil society organizations and constitutional courts attest the founding problem remains live — pluralistic societies still require a religiously-neutral basis for binding law. Independent political theorists and comparative-law scholars outside the rights-advocacy community corroborate that the neutrality claim is only partially achieved: empirical study of AI governance litigation shows secular rights frameworks still encode particular liberal-individualist assumptions that some religious and communitarian traditions dispute as covertly metaphysical rather than genuinely neutral.
narrative_ontology:disappearance_verdict(human_dignity_ai_governance__secular_humanist_reading, world_rearranges).
narrative_ontology:founding_problem_status(human_dignity_ai_governance__secular_humanist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(human_dignity_ai_governance__secular_humanist_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(human_dignity_ai_governance__secular_humanist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(human_dignity_ai_governance__secular_humanist_reading, 0.32, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(human_dignity_ai_governance__secular_humanist_reading_tests).
:- end_tests(human_dignity_ai_governance__secular_humanist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low-to-moderate (0.32 at interval end) because this reading's transfer is primarily jurisdictional (who decides) rather than material extraction — it moves governance authority from religious to secular-democratic institutions and legal standing toward citizens, but this is a genuine coordination gain for the pluralistic majority who can invoke a shared rights vocabulary regardless of faith. Suppression is authored moderate-low (0.28) because the mechanism operates through ordinary legal and legislative procedure rather than coercive enforcement against dissenters; the chief suppressive effect is exclusionary (non-citizens, non-democratic-state populations, and theologically-framed claims are simply outside the framework's reach) rather than actively coercive. Theater ratio is low (0.22) reflecting that courts and legislatures substantially do perform the rights-adjudication function claimed, though a rising trend reflects increasing use of rights language as legitimating cover in international AI-governance forums where enforcement capacity lags rhetoric.
 *
 * DIRECTIONALITY LOGIC:
 *   Rights-bearing citizens, courts, and legislatures sit near the beneficiary end: they hold standing, authority, or enforceable claims that the framework's own vocabulary directly produces. Non-citizen migrants, stateless persons, and populations under non-democratic governments sit near the target end: AI systems are deployed against them under justifications drawn from this framework, but the framework's own legitimacy mechanism (democratic deliberation, national court standing) is unavailable to them — extraction without corresponding voice. Religious minorities occupy an intermediate position: nominally covered as citizens, but their theologically-framed objections must be translated into secular rights categories to register at all, which the commentary treats as a structural cost distinct from simple non-coverage.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — needing a religiously-neutral basis for binding rights claims in pluralistic societies — remains substantially live wherever functioning democratic institutions exist, which prevents this reading from being classified as pure inertial extraction. However, the founding_problem_status is authored contested because the framework's claim to genuine metaphysical neutrality is itself disputed by outside comparative-law scholarship, which is exactly the kind of divergence the R5 corroboration question is designed to surface rather than paper over.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    secular_neutrality_or_substantive_metaphysics,
    'Is the UDHR/rational-autonomy grounding for dignity genuinely neutral among competing worldviews, or does it smuggle in a substantive liberal-individualist metaphysics that itself displaces theological and communitarian accounts rather than merely bracketing them?',
    'Comparative jurisprudence and political-theory analysis of whether secular rights frameworks, in practice, accommodate collectivist, theocratic, or communitarian dignity claims on equal footing, or whether they systematically translate such claims into individualist categories as a condition of legal cognizability.',
    'If the framework is substantively liberal-individualist rather than neutral, its claim to superior legitimacy over the magisterial_integralist and pluralist_pragmatic readings weakens considerably, and its classification could shift from rope toward tangled_rope (coordination for the liberal-democratic majority, extraction from those whose dignity claims resist individualist translation).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(secular_neutrality_or_substantive_metaphysics, conceptual, 'Whether secular rights neutrality is genuine or a disguised comprehensive worldview.').

omega_variable(
    democratic_process_exclusion_extent,
    'How large is the population meaningfully governed by AI systems justified under this framework''s rights language while lacking any democratic voice in shaping that framework — non-citizens, residents of non-democratic states subject to exported AI systems, stateless persons?',
    'Empirical mapping of AI deployment (border control, international development, cross-border platform governance) against the citizenship and franchise status of affected populations.',
    'A large excluded population would indicate the framework''s beneficiary/victim split is structurally severe rather than incidental, pushing classification toward tangled_rope; a small population would support the rope classification as descriptively accurate with limited, addressable edge cases.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(democratic_process_exclusion_extent, empirical, 'Scale of the population extracted from without democratic voice.').

omega_variable(
    sibling_reading_convergence_pressure,
    'Does the secular_humanist_reading''s dominance in international AI-governance forums (OECD, UN AI advisory bodies) create structural pressure that forecloses meaningful uptake of the magisterial_integralist or pluralist_pragmatic readings in binding law, even where those readings retain live constituencies?',
    'Track record of binding international AI-governance instruments: which reading''s vocabulary and enforcement mechanism actually gets codified versus which remains aspirational commentary.',
    'If secular_humanist codification is crowding out the alternatives in binding instruments, the influences relation to pluralist_pragmatic_reading understates the effect — it may function closer to a soft-forecloses in practice, even though logically the readings could coexist.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sibling_reading_convergence_pressure, conceptual, 'Whether institutional dominance functions as de facto foreclosure of sibling readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(human_dignity_ai_governance__secular_humanist_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(huma_tr_t0, human_dignity_ai_governance__secular_humanist_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(huma_tr_t6, human_dignity_ai_governance__secular_humanist_reading, theater_ratio, 6, 0.14).
narrative_ontology:measurement(huma_tr_t12, human_dignity_ai_governance__secular_humanist_reading, theater_ratio, 12, 0.17).
narrative_ontology:measurement(huma_tr_t18, human_dignity_ai_governance__secular_humanist_reading, theater_ratio, 18, 0.19).
narrative_ontology:measurement(huma_tr_t24, human_dignity_ai_governance__secular_humanist_reading, theater_ratio, 24, 0.21).
narrative_ontology:measurement(huma_tr_t30, human_dignity_ai_governance__secular_humanist_reading, theater_ratio, 30, 0.22).

% Extraction over time
narrative_ontology:measurement(huma_be_t0, human_dignity_ai_governance__secular_humanist_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(huma_be_t6, human_dignity_ai_governance__secular_humanist_reading, base_extractiveness, 6, 0.24).
narrative_ontology:measurement(huma_be_t12, human_dignity_ai_governance__secular_humanist_reading, base_extractiveness, 12, 0.27).
narrative_ontology:measurement(huma_be_t18, human_dignity_ai_governance__secular_humanist_reading, base_extractiveness, 18, 0.29).
narrative_ontology:measurement(huma_be_t24, human_dignity_ai_governance__secular_humanist_reading, base_extractiveness, 24, 0.31).
narrative_ontology:measurement(huma_be_t30, human_dignity_ai_governance__secular_humanist_reading, base_extractiveness, 30, 0.32).

% Suppression requirement over time
narrative_ontology:measurement(huma_su_t0, human_dignity_ai_governance__secular_humanist_reading, suppression_requirement, 0, 0.18).
narrative_ontology:measurement(huma_su_t6, human_dignity_ai_governance__secular_humanist_reading, suppression_requirement, 6, 0.2).
narrative_ontology:measurement(huma_su_t12, human_dignity_ai_governance__secular_humanist_reading, suppression_requirement, 12, 0.23).
narrative_ontology:measurement(huma_su_t18, human_dignity_ai_governance__secular_humanist_reading, suppression_requirement, 18, 0.25).
narrative_ontology:measurement(huma_su_t24, human_dignity_ai_governance__secular_humanist_reading, suppression_requirement, 24, 0.27).
narrative_ontology:measurement(huma_su_t30, human_dignity_ai_governance__secular_humanist_reading, suppression_requirement, 30, 0.28).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(human_dignity_ai_governance__secular_humanist_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(human_dignity_ai_governance__secular_humanist_reading, 0.1).
narrative_ontology:affects_constraint(human_dignity_ai_governance__secular_humanist_reading, human_dignity_ai_governance__magisterial_integralist_reading).
narrative_ontology:affects_constraint(human_dignity_ai_governance__secular_humanist_reading, human_dignity_ai_governance__techno_optimist_reading).
narrative_ontology:affects_constraint(human_dignity_ai_governance__secular_humanist_reading, human_dignity_ai_governance__pluralist_pragmatic_reading).

% DUAL FORMULATION NOTE:
% This story is one of four sibling constraints decomposing the natural-language label 'human dignity grounds AI governance' per the ε-invariance principle. Each reading (secular_humanist, magisterial_integralist, techno_optimist, pluralist_pragmatic) has its own ε, beneficiary/victim structure, enforcement mechanism, and claimed type — they are not the same constraint viewed from different angles but four structurally distinct constraints that happen to share a contested kernel (human_dignity_ai_governance) and compete for institutional codification. All four are linked bidirectionally via affects_constraints to preserve the family structure for contamination-propagation analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
