% ============================================================================
% CONSTRAINT STORY: basic_law_interpretive_authority__judicial_supremacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_basic_law_interpretive_authority__judicial_supremacy_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: basic_law_interpretive_authority__judicial_supremacy_reading
 *   human_readable: Judicial Supremacy in Constitutional Interpretation
 *   domain: constitutional_law/political_theory/institutional_design
 *
 * SUMMARY:
 *   This constraint story instantiates the judicial_supremacy_reading of the
 *   basic_law_interpretive_authority kernel. The kernel is the constitutional
 *   commitment to authoritative interpretation; three readings contest it.
 *   This reading holds that courts, through specialized legal expertise and
 *   independence from political pressure, hold final interpretive authority.
 *   The sibling readings — parliamentary_sovereignty_reading and
 *   popular_constitutionalism_reading — distribute interpretive authority
 *   differently. The constraint operates as a tangled rope: genuine
 *   coordination (rights protection, interpretive stability) coexists with
 *   asymmetric extraction (judiciary and legal profession gain
 *   authority/rents; legislature and majorities lose policy autonomy). The
 *   ε-invariance principle applies: this reading has its own ε (0.58),
 *   beneficiaries, and victims, distinct from the sibling readings.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(basic_law_interpretive_authority__judicial_supremacy_reading, 0.58).
domain_priors:suppression_score(basic_law_interpretive_authority__judicial_supremacy_reading, 0.42).
domain_priors:theater_ratio(basic_law_interpretive_authority__judicial_supremacy_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(basic_law_interpretive_authority__judicial_supremacy_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(basic_law_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(basic_law_interpretive_authority__judicial_supremacy_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(basic_law_interpretive_authority__judicial_supremacy_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(basic_law_interpretive_authority__judicial_supremacy_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(basic_law_interpretive_authority__judicial_supremacy_reading, tangled_rope).
narrative_ontology:human_readable(basic_law_interpretive_authority__judicial_supremacy_reading, "Judicial Supremacy in Constitutional Interpretation").
narrative_ontology:topic_domain(basic_law_interpretive_authority__judicial_supremacy_reading, "constitutional_law/political_theory/institutional_design").

domain_priors:requires_active_enforcement(basic_law_interpretive_authority__judicial_supremacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(basic_law_interpretive_authority__judicial_supremacy_reading, '8e52c127-8aed-4a23-b152-476ac8177dbf').
narrative_ontology:cs_kernel_codification('8e52c127-8aed-4a23-b152-476ac8177dbf', formalized).
narrative_ontology:cs_authority_grounding('8e52c127-8aed-4a23-b152-476ac8177dbf', expertise).
narrative_ontology:cs_interpretation_layer_present('8e52c127-8aed-4a23-b152-476ac8177dbf').
narrative_ontology:cs_reading_relation('8e52c127-8aed-4a23-b152-476ac8177dbf', basic_law_interpretive_authority__parliamentary_sovereignty_reading, coexists_with).
narrative_ontology:cs_reading_relation('8e52c127-8aed-4a23-b152-476ac8177dbf', basic_law_interpretive_authority__popular_constitutionalism_reading, coexists_with).
narrative_ontology:cs_axiom('8e52c127-8aed-4a23-b152-476ac8177dbf', foundational, judicial_expertise_entitles_final_interpretive_authority).
narrative_ontology:cs_axiom_status(judicial_expertise_entitles_final_interpretive_authority, holdable).
narrative_ontology:cs_axiom_grounding('8e52c127-8aed-4a23-b152-476ac8177dbf', judicial_expertise_entitles_final_interpretive_authority, instrumental).
narrative_ontology:cs_axiom('8e52c127-8aed-4a23-b152-476ac8177dbf', foundational, independence_from_political_pressure_requires_insulated_appointment).
narrative_ontology:cs_axiom_status(independence_from_political_pressure_requires_insulated_appointment, holdable).
narrative_ontology:cs_axiom_grounding('8e52c127-8aed-4a23-b152-476ac8177dbf', independence_from_political_pressure_requires_insulated_appointment, conventional).
narrative_ontology:cs_reference_frame('8e52c127-8aed-4a23-b152-476ac8177dbf', countermajoritarian_expert_authority).
narrative_ontology:cs_drift_state('8e52c127-8aed-4a23-b152-476ac8177dbf', contemporary_politicized_judiciary, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('8e52c127-8aed-4a23-b152-476ac8177dbf', '').
narrative_ontology:cs_kernel_id(basic_law_interpretive_authority__judicial_supremacy_reading, basic_law_interpretive_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(basic_law_interpretive_authority__judicial_supremacy_reading, constitutional_court).
narrative_ontology:constraint_beneficiary(basic_law_interpretive_authority__judicial_supremacy_reading, legal_profession).
narrative_ontology:constraint_beneficiary(basic_law_interpretive_authority__judicial_supremacy_reading, rights_advocacy_organizations).
narrative_ontology:constraint_victim(basic_law_interpretive_authority__judicial_supremacy_reading, legislature).
narrative_ontology:constraint_victim(basic_law_interpretive_authority__judicial_supremacy_reading, electoral_majorities).
narrative_ontology:constraint_victim(basic_law_interpretive_authority__judicial_supremacy_reading, executive_branch).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(basic_law_interpretive_authority__judicial_supremacy_reading, rights_advocacy_organizations).
narrative_ontology:constraint_vindicates(basic_law_interpretive_authority__judicial_supremacy_reading, rule_of_law_doctrine).
narrative_ontology:constraint_vindicates(basic_law_interpretive_authority__judicial_supremacy_reading, countermajoritarian_protection).
narrative_ontology:constraint_vindicates(basic_law_interpretive_authority__judicial_supremacy_reading, legal_professional_autonomy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds final authority to interpret the constitution; its rulings bind all other branches. Justifies this authority through specialized legal training, institutional independence, and the need for a neutral arbiter. Judges' careers and professional identity are fused with the court's interpretive monopoly — exit means abandoning the only role that validates their expertise. Collects institutional authority and legitimacy as the constitution's authoritative voice.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__judicial_supremacy_reading, constitutional_court, agenda_setter,
    institutional, generational, identity_locked, national).

% Controls the pipeline to judicial office, the language of constitutional argument, and the professional credentials required to practice before the court. Judicial supremacy creates a mandatory market for elite constitutional litigation and scholarly commentary. Exit is constrained by the profession's monopoly on legal licensing and the court's gatekeeping of authoritative interpretation.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__judicial_supremacy_reading, legal_profession, beneficiary,
    organized, biographical, constrained, national).

% Use judicial review to protect minority rights against legislative majorities. They benefit from the court's countermajoritarian function but pay the cost of framing all rights claims in legalistic terms acceptable to the court, and depend on the legal_profession for litigation capacity. Exit means accepting legislative determination of rights — which they view as existentially risky.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__judicial_supremacy_reading, rights_advocacy_organizations, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(basic_law_interpretive_authority__judicial_supremacy_reading, rights_advocacy_organizations, payer).

% Loses final interpretive authority over laws it enacts; must anticipate and accommodate judicial preferences or face invalidation. Bears gridlock costs when judicial rulings create policy vacuums or require legislative fixes the court will accept. Exit is constrained by the constitution's supremacy clause and the political cost of open defiance. Some legislatures develop internal legalistic cultures that internalize the constraint.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__judicial_supremacy_reading, legislature, payer,
    institutional, biographical, constrained, national).

% See their preferred policies blocked by unelected judges interpreting the constitution against majority will. Have no direct exit from the constraint — constitutional amendment is prohibitively difficult, court-packing is politically costly and norm-eroding. Their only leverage is indirect: appointments, jurisdiction legislation, or popular constitutionalist mobilization, all slow and uncertain.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__judicial_supremacy_reading, electoral_majorities, payer,
    organized, biographical, trapped, national).

% Must implement judicial rulings even when they conflict with executive policy priorities or statutory mandates. Loses interpretive discretion in enforcement. Exit is constrained by the duty to faithfully execute the law as the court declares it; defiance triggers constitutional crisis.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__judicial_supremacy_reading, executive_branch, payer,
    institutional, biographical, constrained, national).

% Study the constraint's operation across regimes and time. They see the full structure: the coordination function (rights protection, stability) and the extraction function (judicial agenda-setting, professional rents). Their analysis feeds back into the constraint through academic citation in opinions and confirmation hearings.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__judicial_supremacy_reading, political_scientists_constitutional_theorists, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, authoritative settlement of constitutional meaning that binds all branches, preventing interpretive chaos and enabling rights protection against temporary majorities. Solves the coordination problem of 'who decides what the constitution means' by designating a specialized, relatively insulated body.
% TRANSFER_FUNCTION: Moves final interpretive authority from the legislature and electoral majorities to the constitutional court, mediated by the legal profession. The court collects institutional authority and legitimacy; the legal profession collects professional rents and gatekeeping power; rights advocacy organizations collect policy victories framed as rights. Legislatures and majorities pay in lost policy autonomy and gridlock costs.
% ABSENT_VOICES: Future generations (bound by today's interpretations without participation), minority communities without litigation resources (excluded from the legalistic rights frame), popular constitutionalist movements (structurally excluded from authoritative interpretation by the constraint's design).
% DISAPPEARANCE_RATIONALE: If judicial supremacy vanished overnight, constitutional interpretation would fragment across legislature, executive, and public contestation. Legislatures would immediately assert interpretive authority; rights advocacy would shift to legislative and electoral strategies; the legal profession would lose its monopoly on authoritative constitutional argument. The entire institutional equilibrium would reorganize.
% FOUNDING_PROBLEM: Post-authoritarian constitutional orders needed a credible commitment to rights protection that legislative majorities could not credibly provide. Judicial supremacy was the institutional solution: an independent court with final interpretive authority could constrain majoritarian overreach.
% FOUNDING_PROBLEM_CORROBORATION: The constitutional court and legal profession attest the founding problem remains live (ongoing majoritarian threats to rights). Political scientists (e.g., Tushnet, Kramer, Waldron) and popular constitutionalist theorists attest the founding problem is substantially solved or mischaracterized — legislative majorities in stable democracies rarely threaten core rights, and judicial supremacy now often blocks progressive legislation. No consensus outside the benefiting parties.
narrative_ontology:disappearance_verdict(basic_law_interpretive_authority__judicial_supremacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(basic_law_interpretive_authority__judicial_supremacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(basic_law_interpretive_authority__judicial_supremacy_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(basic_law_interpretive_authority__judicial_supremacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(basic_law_interpretive_authority__judicial_supremacy_reading, 0.58, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(basic_law_interpretive_authority__judicial_supremacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(basic_law_interpretive_authority__judicial_supremacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(basic_law_interpretive_authority__judicial_supremacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) reflects the substantial authority transfer from elected branches to courts, rising over the interval as judicial review expanded into policy domains. Suppression (0.42) is moderate: the constraint does not forbid legislative interpretation but makes it legally non-final and politically costly to sustain. Theater ratio (0.28) captures the growing gap between the expertise justification and the ideological/policy character of many high-stakes rulings. Accessibility collapse (0.35) is low — alternative interpretive practices (legislative, popular) persist vigorously. Resistance (0.55) is significant: court-curbing measures, academic critique, and popular constitutionalist movements actively contest the arrangement.
 *
 * PERSPECTIVAL GAP:
 *   From the court's seat, the constraint is genuine coordination — it provides the expertise and independence the constitutional order requires. From the legislature's seat, it is extraction — their policy choices are vetoed by an unelected body. From electoral majorities' seat, it is a snare — their will is blocked with no exit. The engine computes this divergence from the structural data: different power atoms, exit options, and beneficiary/victim declarations produce different effective extraction per seat.
 *
 * DIRECTIONALITY LOGIC:
 *   The constitutional_court is the primary beneficiary (d near 0.0 — collects authority, identity-locked). The legal_profession is a concentrated beneficiary (d ~ 0.15 — professional rents, gatekeeping). Rights_advocacy_organizations are diffuse beneficiaries with payer characteristics (d ~ 0.4 — win rights but pay framing costs). The legislature and executive_branch are payers with constrained exit (d ~ 0.75). Electoral_majorities are trapped payers (d ~ 0.95 — no exit, bear gridlock costs). The political_scientists_constitutional_theorists are analytical observers (d = 0.5). The identity_locked exit for the court reflects professional identity fusion: judges cannot exit the interpretive monopoly without ceasing to be judges in the relevant sense.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (credible rights protection against majoritarian overreach) is contested: benefiting parties say it persists; outside observers say it has shifted or been solved. The constraint persists despite this contestation — classic mandatrophy. The coordination function (rights protection) remains real but the extraction function (judicial agenda-setting, professional rents) has grown. The theater ratio rise tracks this: more enforcement energy defends the court's authority than the rights protection function. The constraint is not a piton — the court actively maintains and expands its authority — but the mandatrophy is unresolved.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint a genuine coordination mechanism or an extractive institutional arrangement masquerading as expertise?',
    'Compare the constraint''s operation across sibling readings of the basic_law_interpretive_authority kernel. If judicial_supremacy_reading consistently extracts authority from legislature and electoral majorities while parliamentary_sovereignty_reading and popular_constitutionalism_reading distribute interpretive authority differently without comparable extraction, the coordination claim is falsified.',
    'If extraction is structural rather than coordinative, reclassify from tangled_rope toward snare. The kernel context (three readings of one commitment) means the classification is reading-indexed; this omega documents the irreducible ambiguity at the kernel level.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether judicial supremacy is coordination or extraction, indexed to kernel basic_law_interpretive_authority, reading judicial_supremacy_reading').

omega_variable(
    expertise_claim_verification,
    'Does specialized legal expertise actually produce better constitutional outcomes, or is expertise a cover for ideological/policy preferences?',
    'Empirical study of judicial decision-making: measure correlation between legal training and decision quality (however defined), controlling for ideology, policy preference, and institutional incentives. Compare outcomes under judicial review vs. legislative or popular constitutionalism.',
    'If expertise claims are unsubstantiated, the constraint''s coordination function (expertise-based authority) collapses, leaving only the extraction function (institutional power). This would shift classification toward snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(expertise_claim_verification, empirical, 'Whether the expertise justification for judicial supremacy is empirically grounded or ideological cover').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of legislative and majoritarian interpretive authority structural (institutional barriers, appointment processes) or internalized (legislative deference, democratic self-censorship)?',
    'Post-reform trajectory: if legislative interpretive activity rebounds after court-packing threats, jurisdiction stripping, or popular constitutionalist movements gain traction, suppression has an internalized component. If suppression persists regardless of structural openings, it is primarily structural.',
    'If internalized, effective suppression is higher than institutional measures suggest — the legislature carries the constraint''s suppression internally. This affects directionality for legislative seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression of non-judicial interpretive authority').

omega_variable(
    beneficiary_boundary,
    'Do rights_advocacy_organizations genuinely benefit from judicial supremacy, or do they capture the constraint''s coordination function while the legal_profession captures its extraction?',
    'Track litigation outcomes: when rights_advocacy_organizations win, does the legal_profession (litigation costs, precedent-setting, professional prestige) capture disproportionate value? Compare resource flows to advocacy organizations vs. legal professionals in constitutional litigation.',
    'If legal_profession is the concentrated beneficiary and advocacy organizations are diffuse, the constraint''s extraction is more snare-like than tangled_rope-like. Affects gain_flow analysis.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(beneficiary_boundary, empirical, 'Whether advocacy organizations or legal professionals are the true concentrated beneficiaries of judicial supremacy').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(basic_law_interpretive_authority__judicial_supremacy_reading, 1945, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(basi_tr_t1945, basic_law_interpretive_authority__judicial_supremacy_reading, theater_ratio, 1945, 0.12).
narrative_ontology:measurement(basi_tr_t1965, basic_law_interpretive_authority__judicial_supremacy_reading, theater_ratio, 1965, 0.18).
narrative_ontology:measurement(basi_tr_t1985, basic_law_interpretive_authority__judicial_supremacy_reading, theater_ratio, 1985, 0.22).
narrative_ontology:measurement(basi_tr_t2005, basic_law_interpretive_authority__judicial_supremacy_reading, theater_ratio, 2005, 0.25).
narrative_ontology:measurement(basi_tr_t2024, basic_law_interpretive_authority__judicial_supremacy_reading, theater_ratio, 2024, 0.28).

% Extraction over time
narrative_ontology:measurement(basi_be_t1945, basic_law_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 1945, 0.25).
narrative_ontology:measurement(basi_be_t1965, basic_law_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 1965, 0.38).
narrative_ontology:measurement(basi_be_t1985, basic_law_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 1985, 0.48).
narrative_ontology:measurement(basi_be_t2005, basic_law_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 2005, 0.52).
narrative_ontology:measurement(basi_be_t2024, basic_law_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 2024, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(basi_su_t1945, basic_law_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 1945, 0.2).
narrative_ontology:measurement(basi_su_t1965, basic_law_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 1965, 0.3).
narrative_ontology:measurement(basi_su_t1985, basic_law_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 1985, 0.38).
narrative_ontology:measurement(basi_su_t2005, basic_law_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 2005, 0.4).
narrative_ontology:measurement(basi_su_t2024, basic_law_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 2024, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(basic_law_interpretive_authority__judicial_supremacy_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(basic_law_interpretive_authority__judicial_supremacy_reading, 0.12).
narrative_ontology:affects_constraint(basic_law_interpretive_authority__judicial_supremacy_reading, basic_law_interpretive_authority__parliamentary_sovereignty_reading).
narrative_ontology:affects_constraint(basic_law_interpretive_authority__judicial_supremacy_reading, basic_law_interpretive_authority__popular_constitutionalism_reading).
narrative_ontology:affects_constraint(basic_law_interpretive_authority__judicial_supremacy_reading, judicial_review_expansion).
narrative_ontology:affects_constraint(basic_law_interpretive_authority__judicial_supremacy_reading, constitutional_amendment_difficulty).
narrative_ontology:affects_constraint(basic_law_interpretive_authority__judicial_supremacy_reading, legal_profession_gatekeeping).

% DUAL FORMULATION NOTE:
% This constraint is one member of the basic_law_interpretive_authority kernel family. The three readings share a kernel (the constitutional commitment to authoritative interpretation) but instantiate different constraints with different ε, beneficiaries, victims, and types. Judicial_supremacy_reading concentrates authority in courts (tangled_rope). Parliamentary_sovereignty_reading distributes authority to legislature (rope or scaffold depending on enforcement). Popular_constitutionalism_reading diffuses authority across public contestation (rope or mountain depending on enforcement). They are linked by network.affects_constraints and the shared kernel_id.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(basic_law_interpretive_authority__judicial_supremacy_reading, institutional, 0.1).
constraint_indexing:directionality_override(basic_law_interpretive_authority__judicial_supremacy_reading, organized, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
