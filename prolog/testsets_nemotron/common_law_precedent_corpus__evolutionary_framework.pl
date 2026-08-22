% ============================================================================
% CONSTRAINT STORY: common_law_precedent_corpus__evolutionary_framework
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_common_law_precedent_corpus__evolutionary_framework, []).

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
 *   constraint_id: common_law_precedent_corpus__evolutionary_framework
 *   human_readable: Common Law Precedent as Evolutionary Framework
 *   domain: legal_theory/jurisprudence/constitutional_law
 *
 * SUMMARY:
 *   This constraint story models the evolutionary_framework reading of the
 *   common_law_precedent_corpus kernel. Under this reading, precedent is not
 *   a backward-binding chain but an adaptive framework: courts may
 *   reinterpret or overrule prior decisions when contemporary normative
 *   evolution warrants it. The reading empowers courts as normative updaters
 *   and gives litigants a structured pathway to challenge existing doctrine.
 *   It contrasts with strict_stare_decisis (precedent binds as backward
 *   constraint; departure requires extraordinary justification) and
 *   pluralist_balancing (precedent weight varies by domain and context;
 *   case-by-case balancing of stability and adaptation).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(common_law_precedent_corpus__evolutionary_framework, 0.18).
domain_priors:suppression_score(common_law_precedent_corpus__evolutionary_framework, 0.12).
domain_priors:theater_ratio(common_law_precedent_corpus__evolutionary_framework, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(common_law_precedent_corpus__evolutionary_framework, extractiveness, 0.18).
narrative_ontology:constraint_metric(common_law_precedent_corpus__evolutionary_framework, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(common_law_precedent_corpus__evolutionary_framework, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(common_law_precedent_corpus__evolutionary_framework, accessibility_collapse, 0.22).
narrative_ontology:constraint_metric(common_law_precedent_corpus__evolutionary_framework, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(common_law_precedent_corpus__evolutionary_framework, rope).
narrative_ontology:human_readable(common_law_precedent_corpus__evolutionary_framework, "Common Law Precedent as Evolutionary Framework").
narrative_ontology:topic_domain(common_law_precedent_corpus__evolutionary_framework, "legal_theory/jurisprudence/constitutional_law").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(common_law_precedent_corpus__evolutionary_framework, '19018e8b-7883-4f9d-9f33-9fe9c5d232c8').
narrative_ontology:cs_kernel_codification('19018e8b-7883-4f9d-9f33-9fe9c5d232c8', distributed).
narrative_ontology:cs_authority_grounding('19018e8b-7883-4f9d-9f33-9fe9c5d232c8', practice).
narrative_ontology:cs_interpretation_layer_present('19018e8b-7883-4f9d-9f33-9fe9c5d232c8').
narrative_ontology:cs_reading_relation('19018e8b-7883-4f9d-9f33-9fe9c5d232c8', common_law_precedent_corpus__strict_stare_decisis, coexists_with).
narrative_ontology:cs_reading_relation('19018e8b-7883-4f9d-9f33-9fe9c5d232c8', common_law_precedent_corpus__pluralist_balancing, coexists_with).
narrative_ontology:cs_axiom('19018e8b-7883-4f9d-9f33-9fe9c5d232c8', foundational, precedent_revisable_by_normative_evolution).
narrative_ontology:cs_axiom_status(precedent_revisable_by_normative_evolution, holdable).
narrative_ontology:cs_axiom_grounding('19018e8b-7883-4f9d-9f33-9fe9c5d232c8', precedent_revisable_by_normative_evolution, conventional).
narrative_ontology:cs_axiom('19018e8b-7883-4f9d-9f33-9fe9c5d232c8', foundational, courts_legitimate_normative_updaters).
narrative_ontology:cs_axiom_status(courts_legitimate_normative_updaters, holdable).
narrative_ontology:cs_axiom_grounding('19018e8b-7883-4f9d-9f33-9fe9c5d232c8', courts_legitimate_normative_updaters, conventional).
narrative_ontology:cs_reference_frame('19018e8b-7883-4f9d-9f33-9fe9c5d232c8', common_law_incremental_adaptation).
narrative_ontology:cs_drift_state('19018e8b-7883-4f9d-9f33-9fe9c5d232c8', contemporary_rights_jurisprudence, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('19018e8b-7883-4f9d-9f33-9fe9c5d232c8', '').
narrative_ontology:cs_kernel_id(common_law_precedent_corpus__evolutionary_framework, common_law_precedent_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(common_law_precedent_corpus__evolutionary_framework, litigants_seeking_norm_change).
narrative_ontology:constraint_beneficiary(common_law_precedent_corpus__evolutionary_framework, courts_as_normative_updaters).
narrative_ontology:constraint_beneficiary(common_law_precedent_corpus__evolutionary_framework, legal_doctrine_scholars).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(common_law_precedent_corpus__evolutionary_framework, lower_courts).
narrative_ontology:constraint_victim(common_law_precedent_corpus__evolutionary_framework, reliance_interest_holders).
narrative_ontology:constraint_victim(common_law_precedent_corpus__evolutionary_framework, lower_courts).
narrative_ontology:constraint_vindicates(common_law_precedent_corpus__evolutionary_framework, common_law_adaptive_capacity).
narrative_ontology:constraint_vindicates(common_law_precedent_corpus__evolutionary_framework, living_constitutionalism_normative_evolution).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Litigants who challenge existing precedents gain a recognized pathway: they can argue that contemporary normative evolution warrants reinterpretation or overruling. Their success depends on persuading courts that social values have shifted. Exit means accepting the existing precedent or pursuing legislative change instead.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__evolutionary_framework, litigants_seeking_norm_change, beneficiary,
    moderate, biographical, constrained, national).

% Courts hold the authority to reinterpret or overrule precedent when they determine that contemporary normative evolution justifies it. They exercise this power case-by-case, balancing stability against adaptation. Their institutional role is strengthened by the legitimacy this framework grants them as normative updaters.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__evolutionary_framework, courts_as_normative_updaters, agenda_setter,
    institutional, generational, arbitrage, national).

% Scholars who develop and critique normative frameworks gain professional relevance and citation authority when courts treat precedent as adaptable. Their work directly shapes the arguments litigants make and the principles courts adopt. Exit is mobile — they can shift to other doctrinal domains or theoretical frameworks.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__evolutionary_framework, legal_doctrine_scholars, beneficiary,
    organized, generational, mobile, global).

% Parties who have structured their affairs, contracts, or institutional practices around existing precedent bear the cost when courts overrule or substantially reinterpret. Their reliance interests are acknowledged but treated as defeasible against normative evolution. Exit is constrained — they cannot easily undo settled arrangements.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__evolutionary_framework, reliance_interest_holders, payer,
    moderate, biographical, constrained, national).

% Legislatures observe judicial adaptation and may respond by codifying, overturning, or preempting court-driven doctrinal shifts. They hold ultimate democratic authority but operate on a slower time horizon. Their analytical seat allows them to assess whether judicial updating serves or undermines legislative goals.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__evolutionary_framework, legislatures, observer,
    institutional, generational, analytical, national).

% Lower courts must apply evolving precedent from above, which creates uncertainty and workload (cost). But they also gain flexibility to distinguish or develop doctrine within the adaptive framework (benefit). Their exit is constrained by hierarchical obligation and the need for predictable decision-making.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__evolutionary_framework, lower_courts, payer,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(common_law_precedent_corpus__evolutionary_framework, lower_courts, beneficiary).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a structured mechanism for legal norms to adapt to changing social values without requiring legislative action for every shift. Precedent operates as a stable-but-revisable coordination platform: courts, litigants, and scholars share a common framework for proposing, testing, and legitimating normative updates.
% TRANSFER_FUNCTION: Transfers interpretive authority and normative updating power from the static text of past decisions to the living judgment of contemporary courts. Litigants gain standing to challenge precedent; courts gain legitimacy to overrule; reliance interest holders bear the adjustment costs of normative change.
% ABSENT_VOICES: Future generations who will inherit the doctrinal landscape shaped by today's overrulings — they cannot object to current adaptive moves. Also excluded: parties in jurisdictions without strong common law traditions who experience the downstream effects of exported doctrinal innovations without participatory input.
% DISAPPEARANCE_RATIONALE: If the evolutionary framework vanished overnight and were replaced by strict stare decisis, courts would lose legitimate authority to overrule precedent except in extraordinary circumstances. Litigants' pathway for norm challenge would narrow dramatically. Reliance interests would gain near-absolute protection. The common law's adaptive capacity would shift from judicial to legislative channels, fundamentally reorganizing how legal norms evolve.
% FOUNDING_PROBLEM: The common law needed a mechanism to prevent legal norms from ossifying while maintaining the stability and predictability that precedent provides. The founding problem was: how can a system bound by its own past decisions nevertheless adapt to new social conditions without collapsing into judicial discretion or legislative paralysis?
% FOUNDING_PROBLEM_CORROBORATION: Legal historians (Friedman, Horwitz) and comparative law scholars (Zweigert & Kötz) attest that the adaptive function remains live across common law jurisdictions. The founding problem is corroborated by the persistent tension between stability and adaptation in contemporary jurisprudence — noted by judges across the ideological spectrum (e.g., Scalia's dissent in Planned Parenthood v. Casey acknowledging the Court's adaptive role while criticizing its exercise; Breyer's active defense in Active Liberty). No single beneficiary group monopolizes this attestation.
narrative_ontology:disappearance_verdict(common_law_precedent_corpus__evolutionary_framework, world_rearranges).
narrative_ontology:founding_problem_status(common_law_precedent_corpus__evolutionary_framework, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(common_law_precedent_corpus__evolutionary_framework, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(common_law_precedent_corpus__evolutionary_framework, 'none', 1).
narrative_ontology:epsilon_provenance(common_law_precedent_corpus__evolutionary_framework, 0.18, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(common_law_precedent_corpus__evolutionary_framework_tests).
:- end_tests(common_law_precedent_corpus__evolutionary_framework_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is low (0.18) because the framework's primary function is coordination — providing a shared mechanism for normative adaptation — and the costs (reliance interest disruption) are diffuse and defeasible. Suppression is very low (0.12) because the constraint does not coerce; parties can accept precedent, challenge it, or seek legislative override. Theater is minimal (0.08) — the adaptive function is genuine and actively used. Accessibility collapse is low (0.22) because alternative pathways (legislative change, constitutional amendment, scholarly critique) remain fully available. Resistance is moderate (0.35) because the reading faces genuine contestation from strict_stare_decisis proponents who argue it undermines rule-of-law values.
 *
 * PERSPECTIVAL GAP:
 *   From the court's seat, the framework is genuine coordination — it solves the problem of adaptive stability. From the reliance_interest_holder's seat, the same structure operates as periodic extraction of settled expectations. The engine computes this divergence from the structural data. The evolutionary reading's legitimacy depends on whether the coordination function (adaptive capacity) genuinely outweighs the extraction (reliance disruption) across the system.
 *
 * DIRECTIONALITY LOGIC:
 *   Courts_as_normative_updaters are the agenda_setter with institutional power and arbitrage-grade exit (they can shift interpretive methodology case-by-case). Litigants_seeking_norm_change are beneficiaries with constrained exit — they gain a pathway but must persuade courts. Legal_doctrine_scholars are beneficiaries with mobile exit — their professional relevance tracks the framework's adoption. Reliance_interest_holders are payers bearing adjustment costs with constrained exit. Lower_courts are dual-positioned: they pay compliance costs but gain doctrinal flexibility. Legislatures are observers with analytical exit.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (adaptive stability) remains live — the common law continues to face novel social conditions requiring normative evolution. The framework has not atrophied into a piton because courts actively use overruling and reinterpretation as corrective tools (e.g., Brown v. Board overruling Plessy; Lawrence v. Texas overruling Bowers; Dobbs overruling Roe/Casey). The mandate is actively exercised, not theatrically maintained.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    normative_evolution_measurement,
    'How is ''contemporary normative evolution'' measured or identified such that courts can legitimately claim to track it rather than impose their own preferences?',
    'Empirical study of citation patterns, amicus briefing, public opinion data, legislative trends, and cross-jurisdictional convergence that courts cite when overruling precedent.',
    'If normative evolution is empirically trackable, the reading''s coordination function is strengthened and extraction concerns diminish. If it is a judicial construct, the reading approaches a snare where courts extract legitimacy from an unverifiable claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(normative_evolution_measurement, empirical, 'Whether the adaptive trigger (normative evolution) is an observable phenomenon or a judicial construction.').

omega_variable(
    reliance_vs_adaptation_boundary,
    'Where is the structural boundary between legitimate reliance interests that should constrain adaptation and those that merely entrench outdated norms?',
    'Doctrinal analysis of reliance-interest jurisprudence across jurisdictions (e.g., Payne v. Tennessee''s reliance factors; Canadian Charter s.1 proportionality; German Federal Constitutional Court''s Vertrauensschutz).',
    'If the boundary is coherent and consistently applied, the reading remains a rope. If it is manipulable to justify preferred outcomes, the reading drifts toward tangled_rope or snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reliance_vs_adaptation_boundary, conceptual, 'Whether the reliance/adaptation tradeoff has a principled structural resolution.').

omega_variable(
    reading_framing_alternative,
    'Does the evolutionary_framework reading represent the only coherent adaptive reading, or does it conflate distinct adaptive logics (e.g., democratic constitutionalism vs. common law incrementalism)?',
    'Comparative analysis of the kernel''s sibling readings: if pluralist_balancing and evolutionary_framework make the same empirical predictions but differ in normative justification, the framing ambiguity is conceptual. If they predict different overruling patterns, it is empirical.',
    'If the reading conflates distinct adaptive logics, the constraint story should decompose into multiple stories per the ε-invariance principle — each with its own ε, beneficiaries, and classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_framing_alternative, conceptual, 'Whether the reading''s framing captures one adaptive logic or several that should be separated.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(common_law_precedent_corpus__evolutionary_framework, 1800, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comm_tr_t1800, common_law_precedent_corpus__evolutionary_framework, theater_ratio, 1800, 0.05).
narrative_ontology:measurement(comm_tr_t1850, common_law_precedent_corpus__evolutionary_framework, theater_ratio, 1850, 0.05).
narrative_ontology:measurement(comm_tr_t1900, common_law_precedent_corpus__evolutionary_framework, theater_ratio, 1900, 0.06).
narrative_ontology:measurement(comm_tr_t1950, common_law_precedent_corpus__evolutionary_framework, theater_ratio, 1950, 0.07).
narrative_ontology:measurement(comm_tr_t2000, common_law_precedent_corpus__evolutionary_framework, theater_ratio, 2000, 0.07).
narrative_ontology:measurement(comm_tr_t2025, common_law_precedent_corpus__evolutionary_framework, theater_ratio, 2025, 0.08).

% Extraction over time
narrative_ontology:measurement(comm_be_t1800, common_law_precedent_corpus__evolutionary_framework, base_extractiveness, 1800, 0.08).
narrative_ontology:measurement(comm_be_t1850, common_law_precedent_corpus__evolutionary_framework, base_extractiveness, 1850, 0.1).
narrative_ontology:measurement(comm_be_t1900, common_law_precedent_corpus__evolutionary_framework, base_extractiveness, 1900, 0.12).
narrative_ontology:measurement(comm_be_t1950, common_law_precedent_corpus__evolutionary_framework, base_extractiveness, 1950, 0.15).
narrative_ontology:measurement(comm_be_t2000, common_law_precedent_corpus__evolutionary_framework, base_extractiveness, 2000, 0.17).
narrative_ontology:measurement(comm_be_t2025, common_law_precedent_corpus__evolutionary_framework, base_extractiveness, 2025, 0.18).

% Suppression requirement over time
narrative_ontology:measurement(comm_su_t1800, common_law_precedent_corpus__evolutionary_framework, suppression_requirement, 1800, 0.08).
narrative_ontology:measurement(comm_su_t1850, common_law_precedent_corpus__evolutionary_framework, suppression_requirement, 1850, 0.09).
narrative_ontology:measurement(comm_su_t1900, common_law_precedent_corpus__evolutionary_framework, suppression_requirement, 1900, 0.1).
narrative_ontology:measurement(comm_su_t1950, common_law_precedent_corpus__evolutionary_framework, suppression_requirement, 1950, 0.11).
narrative_ontology:measurement(comm_su_t2000, common_law_precedent_corpus__evolutionary_framework, suppression_requirement, 2000, 0.11).
narrative_ontology:measurement(comm_su_t2025, common_law_precedent_corpus__evolutionary_framework, suppression_requirement, 2025, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(common_law_precedent_corpus__evolutionary_framework, information_standard).
narrative_ontology:boltzmann_floor_override(common_law_precedent_corpus__evolutionary_framework, 0.02).
narrative_ontology:affects_constraint(common_law_precedent_corpus__evolutionary_framework, common_law_precedent_corpus__strict_stare_decisis).
narrative_ontology:affects_constraint(common_law_precedent_corpus__evolutionary_framework, common_law_precedent_corpus__pluralist_balancing).

% DUAL FORMULATION NOTE:
% This story is one member of the common_law_precedent_corpus constraint family. The three readings (evolutionary_framework, strict_stare_decisis, pluralist_balancing) decompose the kernel's single natural-language concept into structurally distinct constraints with different ε values, beneficiary structures, and operational logics. They are linked via network.affects_constraints to enable contamination propagation analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
