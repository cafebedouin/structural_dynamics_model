% ============================================================================
% CONSTRAINT STORY: humane_treatment_standard__proportionality_balancing
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_humane_treatment_standard__proportionality_balancing, []).

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
 *   constraint_id: humane_treatment_standard__proportionality_balancing
 *   human_readable: Common Article 3 Proportionality Balancing Standard
 *   domain: international_humanitarian_law/state_security/human_rights
 *
 * SUMMARY:
 *   The proportionality_balancing reading of Common Article 3's humane
 *   treatment standard emerged from judicial practice (Israeli HCJ 1999,
 *   ICTY, ECHR) as a middle path between absolute prohibition of coercive
 *   interrogation and unlimited security discretion. It makes courts the
 *   gatekeepers of treatment permissibility through case-by-case balancing of
 *   detainee dignity against security necessity. This constraint story models
 *   that judicial doctrine as a structural constraint on state security
 *   apparatuses in non-international armed conflicts.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(humane_treatment_standard__proportionality_balancing, 0.42).
domain_priors:suppression_score(humane_treatment_standard__proportionality_balancing, 0.55).
domain_priors:theater_ratio(humane_treatment_standard__proportionality_balancing, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(humane_treatment_standard__proportionality_balancing, extractiveness, 0.42).
narrative_ontology:constraint_metric(humane_treatment_standard__proportionality_balancing, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(humane_treatment_standard__proportionality_balancing, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(humane_treatment_standard__proportionality_balancing, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(humane_treatment_standard__proportionality_balancing, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(humane_treatment_standard__proportionality_balancing, tangled_rope).
narrative_ontology:human_readable(humane_treatment_standard__proportionality_balancing, "Common Article 3 Proportionality Balancing Standard").
narrative_ontology:topic_domain(humane_treatment_standard__proportionality_balancing, "international_humanitarian_law/state_security/human_rights").

domain_priors:requires_active_enforcement(humane_treatment_standard__proportionality_balancing).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(humane_treatment_standard__proportionality_balancing, '4e50d1c0-8eee-46ea-a490-0f2e3be4e505').
narrative_ontology:cs_kernel_codification('4e50d1c0-8eee-46ea-a490-0f2e3be4e505', fixed_text).
narrative_ontology:cs_authority_grounding('4e50d1c0-8eee-46ea-a490-0f2e3be4e505', lineage).
narrative_ontology:cs_interpretation_layer_present('4e50d1c0-8eee-46ea-a490-0f2e3be4e505').
narrative_ontology:cs_reading_relation('4e50d1c0-8eee-46ea-a490-0f2e3be4e505', humane_treatment_standard__absolute_prohibition, coexists_with).
narrative_ontology:cs_reading_relation('4e50d1c0-8eee-46ea-a490-0f2e3be4e505', humane_treatment_standard__contextual_necessity, influences).
narrative_ontology:cs_axiom('4e50d1c0-8eee-46ea-a490-0f2e3be4e505', foundational, judicial_gatekeeping_required).
narrative_ontology:cs_axiom_status(judicial_gatekeeping_required, holdable).
narrative_ontology:cs_axiom_grounding('4e50d1c0-8eee-46ea-a490-0f2e3be4e505', judicial_gatekeeping_required, conventional).
narrative_ontology:cs_axiom('4e50d1c0-8eee-46ea-a490-0f2e3be4e505', foundational, proportionality_as_operational_standard).
narrative_ontology:cs_axiom_status(proportionality_as_operational_standard, holdable).
narrative_ontology:cs_axiom_grounding('4e50d1c0-8eee-46ea-a490-0f2e3be4e505', proportionality_as_operational_standard, instrumental).
narrative_ontology:cs_axiom('4e50d1c0-8eee-46ea-a490-0f2e3be4e505', secondary, no_per_se_ban_on_coercive_interrogation).
narrative_ontology:cs_axiom_status(no_per_se_ban_on_coercive_interrogation, holdable).
narrative_ontology:cs_axiom_grounding('4e50d1c0-8eee-46ea-a490-0f2e3be4e505', no_per_se_ban_on_coercive_interrogation, conventional).
narrative_ontology:cs_reference_frame('4e50d1c0-8eee-46ea-a490-0f2e3be4e505', common_article_3_textual_minimum).
narrative_ontology:cs_drift_state('4e50d1c0-8eee-46ea-a490-0f2e3be4e505', post_war_on_terror_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('4e50d1c0-8eee-46ea-a490-0f2e3be4e505', '').
narrative_ontology:cs_kernel_id(humane_treatment_standard__proportionality_balancing, humane_treatment_standard).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(humane_treatment_standard__proportionality_balancing, detainees_niac).
narrative_ontology:constraint_beneficiary(humane_treatment_standard__proportionality_balancing, human_rights_monitors).
narrative_ontology:constraint_victim(humane_treatment_standard__proportionality_balancing, state_security_apparatus).
narrative_ontology:constraint_victim(humane_treatment_standard__proportionality_balancing, interrogation_personnel).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(humane_treatment_standard__proportionality_balancing, executive_legal_advisors).
narrative_ontology:constraint_vindicates(humane_treatment_standard__proportionality_balancing, judicial_gatekeeping_doctrine).
narrative_ontology:constraint_vindicates(humane_treatment_standard__proportionality_balancing, proportionality_as_legal_standard).
narrative_ontology:constraint_vindicates(humane_treatment_standard__proportionality_balancing, non_derogable_minimum_protections).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Persons detained in non-international armed conflicts who receive baseline protections against torture and degrading treatment through the proportionality balancing test. They cannot exit the detention context; their protections depend entirely on courts enforcing the balancing standard against detaining authorities. The standard gives them a procedural foothold but not absolute immunity.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__proportionality_balancing, detainees_niac, beneficiary,
    powerless, biographical, trapped, global).

% NGOs, treaty bodies, and special rapporteurs who use the proportionality standard as an advocacy tool and monitoring benchmark. They benefit from a justiciable standard that courts can apply case-by-case. Their exit is mobile — they can shift focus to other mechanisms — but the standard's existence amplifies their institutional leverage.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__proportionality_balancing, human_rights_monitors, beneficiary,
    organized, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(humane_treatment_standard__proportionality_balancing, human_rights_monitors, observer).

% Military, intelligence, and law enforcement agencies that conduct detention and interrogation in NIACs. They bear the compliance costs of the balancing test: judicial review of interrogation policies, procedural safeguards, documentation requirements, and the risk of liability for disproportionate measures. Their exit is constrained — they cannot abandon security operations but can push for broader discretion through legal arguments and policy shifts.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__proportionality_balancing, state_security_apparatus, payer,
    institutional, biographical, constrained, national).

% Individual interrogators and their direct commanders who face operational constraints: required legal advisement, approved technique lists, recording obligations, and personal liability exposure. They experience the standard as a daily procedural burden. Exit is constrained — they can leave the role but not easily the institution; the standard shapes their professional identity and risk calculus.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__proportionality_balancing, interrogation_personnel, payer,
    moderate, biographical, constrained, national).

% Domestic and international courts that adjudicate proportionality challenges — habeas corpus petitions, civil damages suits, criminal prosecutions of interrogators, treaty body complaints. They define the balancing test's contours case-by-case, setting precedents that become the effective standard. They have arbitrage-grade exit: they can interpret narrowly or broadly, and their institutional legitimacy depends on being seen as neutral arbiters.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__proportionality_balancing, judicial_gatekeepers, agenda_setter,
    institutional, generational, arbitrage, national).

% Government lawyers (OJC, DOD General Counsel, State Department legal advisers) who craft interrogation policies to survive judicial review. They both administer the constraint (designing compliant frameworks) and bear its costs (restricted policy options, reputational risk when courts strike down their work). Their exit is constrained by professional duty and institutional loyalty.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__proportionality_balancing, executive_legal_advisors, agenda_setter,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(humane_treatment_standard__proportionality_balancing, executive_legal_advisors, payer).

% Legal scholars, historians, and political scientists who analyze the balancing test's evolution, its empirical effects on detention practices, and its doctrinal coherence. They neither collect nor pay; they map the constraint's structure across time and jurisdictions.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__proportionality_balancing, academic_observers, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a justiciable legal standard for determining permissible treatment in NIAC detention, replacing the vacuum of no standard or the paralysis of absolute prohibition. It coordinates state security operations with minimal human dignity protections by giving courts a structured framework (necessity, proportionality, humanity) to review executive decisions case-by-case.
% TRANSFER_FUNCTION: Moves operational discretion and risk from interrogation personnel and security agencies to judicial gatekeepers. The state bears compliance costs (legal review, procedural safeguards, foregone intelligence from prohibited techniques); detainees receive procedural protections and a remedy pathway. The transfer is not monetary but allocative: decision-authority shifts from the executive to the judiciary at the margin.
% ABSENT_VOICES: Detainees in extraterritorial 'black sites' and proxy detention facilities where the balancing test's reach is contested; future detainees in conflicts not yet arisen; victims of torture who died before any judicial review could occur. These voices are structurally excluded by geography, secrecy, and mortality — the standard's protections presume access to a court.
% DISAPPEARANCE_RATIONALE: If the proportionality balancing standard vanished overnight, states would revert to either unfettered executive discretion (contextual_necessity reading) or face pressure for absolute prohibitions (absolute_prohibition reading). The NIAC detention framework would lose its central judicial gatekeeping mechanism; interrogation policies would shift toward either maximal discretion or legislative codification of absolute bans. The institutional equilibrium between security claims and dignity claims would collapse into a different configuration.
% FOUNDING_PROBLEM: Common Article 3's 'humane treatment' mandate was too vague to operationalize in NIACs where states claimed unlimited security discretion. The proportionality balancing test was developed by courts (notably the Israeli HCJ in Public Committee Against Torture v. Israel, the ICTY, and later the ECHR) to give concrete meaning to 'humane treatment' without imposing an absolute ban that states would reject outright — creating a standard that could actually be enforced against security apparatuses.
% FOUNDING_PROBLEM_CORROBORATION: The Israeli High Court of Justice (1999) explicitly adopted balancing to avoid both absolute prohibition and unlimited discretion; the ICTY in Furundžija and Kunarac treated humane treatment as a proportionality inquiry; the ECHR in Al-Skeini and Hassan v. UK applied proportionality to detention standards. Critics from the absolute_prohibition camp (UN CAT, ICRC commentaries, dissents in HCJ) argue the founding problem was a false dilemma — that Common Article 3's text and history support absolute minimums. The 'contested' status reflects this live doctrinal dispute, corroborated by opposing judicial opinions and treaty body outputs.
narrative_ontology:disappearance_verdict(humane_treatment_standard__proportionality_balancing, world_rearranges).
narrative_ontology:founding_problem_status(humane_treatment_standard__proportionality_balancing, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(humane_treatment_standard__proportionality_balancing, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(humane_treatment_standard__proportionality_balancing, 'none', 1).
narrative_ontology:epsilon_provenance(humane_treatment_standard__proportionality_balancing, 0.42, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(humane_treatment_standard__proportionality_balancing_tests).
:- end_tests(humane_treatment_standard__proportionality_balancing_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.42) reflects the compliance burden on security agencies: legal review, procedural safeguards, technique restrictions, and liability exposure — real but not confiscatory. Suppression (0.55) is moderate: states cannot ignore the standard without judicial and reputational consequences, but the balancing test's inherent flexibility means suppression is not total; states retain significant discretion within the framework. Theater ratio (0.28) captures that judicial review is genuine but incomplete — some courts defer heavily to executive security claims, creating performative compliance. Accessibility collapse (0.45) and resistance (0.52) are moderate: alternative frameworks (absolute prohibition, contextual necessity) remain live and contested; states actively resist through legal arguments and policy design.
 *
 * PERSPECTIVAL GAP:
 *   From the judicial gatekeeper seat, the balancing test is genuine coordination: it solves the problem of giving legal effect to 'humane treatment' in NIACs. From the state security apparatus seat, it is asymmetric extraction: a judicial imposition that constrains operational discretion without eliminating the security threats that motivate coercive interrogation. From the detainee seat, it is partial protection: better than nothing but far from absolute. The engine computes this divergence from the declared roles, power, and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   Detainees are full beneficiaries (d→0) — the standard exists for their protection, they bear no compliance costs, and their exit is trapped. Human rights monitors are beneficiaries with mobile exit. State security apparatus and interrogation personnel are payers (d→1) — they bear the constraint's operational costs; their exit is constrained by institutional role. Judicial gatekeepers are agenda_setters with arbitrage exit — they define the standard's meaning. Executive legal advisors are dual-role: they administer the constraint (designing compliant policies) and bear its costs (restricted options). The engine will compute per-seat χ from these structural positions.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (vagueness of 'humane treatment' in NIACs) remains contested — states still claim security imperatives require broader discretion; human rights bodies argue the balancing test has become a loophole. The constraint has not atrophied into a piton: courts actively apply it, new precedents emerge, and the doctrinal debate is live. But mandatrophy risk exists: if courts consistently defer to executive security claims, the balancing test becomes theatrical — a coordination ritual that extracts compliance costs without delivering proportional protection. The theater_ratio trajectory (rising to 0.28 then plateauing) suggests this risk is monitored but not yet realized.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    balancing_test_coherence,
    'Does the proportionality balancing test have a coherent doctrinal core, or is it an empty vessel that courts fill with their prior policy preferences?',
    'Comparative analysis of balancing outcomes across jurisdictions (Israeli HCJ, ECHR, US courts, ICTY/ICTR, ICC) — if similarly situated cases produce divergent results without principled distinction, the test lacks coherence.',
    'If incoherent, the constraint''s coordination function is illusory — it provides cover for judicial policy-making rather than a genuine standard. Extraction would be higher (judges impose their preferences) and suppression lower (states can predict/manipulate outcomes).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(balancing_test_coherence, conceptual, 'Whether the balancing test operates as a genuine legal standard or a discretionary mask.').

omega_variable(
    extraterritorial_reach,
    'Does the proportionality balancing standard apply extraterritorially to proxy detention and ''black site'' operations, or does its effective suppression collapse beyond state territory?',
    'Track judicial rulings on extraterritorial application (e.g., ECHR Al-Skeini/Hassan, US Boumediene/Al-Nashiri, ICC jurisdiction decisions) and state practice in covert detention.',
    'If the standard does not reach extraterritorial detention, its suppression is geographically bounded — states can evade it by outsourcing detention. The constraint''s effective extraction on security apparatus would be lower (evasion possible) but its extraction on detainees would be higher (protection gaps).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(extraterritorial_reach, empirical, 'Geographic limits of the balancing test''s enforcement.').

omega_variable(
    committer_frame_ambiguity,
    'Is the proportionality_balancing reading a stable doctrinal position, or a transitional compromise that will collapse into either absolute_prohibition or contextual_necessity?',
    'Longitudinal doctrinal tracking: monitor whether appellate courts converge on a consistent balancing methodology (stability) or whether the test''s factors become so manipulable that it dissolves into de facto executive discretion (collapse to contextual_necessity) or so rigid that it becomes a per se ban (collapse to absolute_prohibition).',
    'If transitional, the constraint''s current metrics describe a moment in a drift trajectory, not a stable equilibrium. The engine''s classification would need temporal updating.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_frame_ambiguity, conceptual, 'Structural stability of the proportionality_balancing reading within the kernel.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (judicial enforcement, treaty obligations) or internalized (security apparatus self-restraint through professionalization and legal training)?',
    'Post-compliance suppression trajectory: if judicial enforcement were removed (e.g., jurisdiction stripping), would security agencies maintain proportionality safeguards? Compare jurisdictions with strong vs. weak judicial review.',
    'If substantially internalized, the constraint''s effective suppression is higher than structural measures suggest — the security apparatus carries the constraint internally. If purely structural, suppression collapses when courts are sidelined.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression in security apparatus compliance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(humane_treatment_standard__proportionality_balancing, 1999, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hts_pb_tr_t1999, humane_treatment_standard__proportionality_balancing, theater_ratio, 1999, 0.15).
narrative_ontology:measurement(hts_pb_tr_t2004, humane_treatment_standard__proportionality_balancing, theater_ratio, 2004, 0.22).
narrative_ontology:measurement(hts_pb_tr_t2009, humane_treatment_standard__proportionality_balancing, theater_ratio, 2009, 0.26).
narrative_ontology:measurement(hts_pb_tr_t2014, humane_treatment_standard__proportionality_balancing, theater_ratio, 2014, 0.28).
narrative_ontology:measurement(hts_pb_tr_t2019, humane_treatment_standard__proportionality_balancing, theater_ratio, 2019, 0.28).
narrative_ontology:measurement(hts_pb_tr_t2024, humane_treatment_standard__proportionality_balancing, theater_ratio, 2024, 0.28).

% Extraction over time
narrative_ontology:measurement(hts_pb_be_t1999, humane_treatment_standard__proportionality_balancing, base_extractiveness, 1999, 0.25).
narrative_ontology:measurement(hts_pb_be_t2004, humane_treatment_standard__proportionality_balancing, base_extractiveness, 2004, 0.32).
narrative_ontology:measurement(hts_pb_be_t2009, humane_treatment_standard__proportionality_balancing, base_extractiveness, 2009, 0.38).
narrative_ontology:measurement(hts_pb_be_t2014, humane_treatment_standard__proportionality_balancing, base_extractiveness, 2014, 0.41).
narrative_ontology:measurement(hts_pb_be_t2019, humane_treatment_standard__proportionality_balancing, base_extractiveness, 2019, 0.42).
narrative_ontology:measurement(hts_pb_be_t2024, humane_treatment_standard__proportionality_balancing, base_extractiveness, 2024, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(hts_pb_su_t1999, humane_treatment_standard__proportionality_balancing, suppression_requirement, 1999, 0.4).
narrative_ontology:measurement(hts_pb_su_t2004, humane_treatment_standard__proportionality_balancing, suppression_requirement, 2004, 0.48).
narrative_ontology:measurement(hts_pb_su_t2009, humane_treatment_standard__proportionality_balancing, suppression_requirement, 2009, 0.52).
narrative_ontology:measurement(hts_pb_su_t2014, humane_treatment_standard__proportionality_balancing, suppression_requirement, 2014, 0.55).
narrative_ontology:measurement(hts_pb_su_t2019, humane_treatment_standard__proportionality_balancing, suppression_requirement, 2019, 0.55).
narrative_ontology:measurement(hts_pb_su_t2024, humane_treatment_standard__proportionality_balancing, suppression_requirement, 2024, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(humane_treatment_standard__proportionality_balancing, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(humane_treatment_standard__proportionality_balancing, 0.12).
narrative_ontology:affects_constraint(humane_treatment_standard__proportionality_balancing, humane_treatment_standard__absolute_prohibition).
narrative_ontology:affects_constraint(humane_treatment_standard__proportionality_balancing, humane_treatment_standard__contextual_necessity).

% DUAL FORMULATION NOTE:
% This constraint is the proportionality_balancing reading of the humane_treatment_standard kernel. It decomposes the kernel into a judicial gatekeeping doctrine with case-by-case balancing. The absolute_prohibition reading (non-derogable minimums) and contextual_necessity reading (security imperatives override) are sibling constraints with different ε values and stakeholder structures. This reading's ε (0.42) reflects moderate extraction on security apparatus through judicial review; absolute_prohibition would have higher extraction on states (near-total constraint) and lower on detainees (stronger protections); contextual_necessity would have lower extraction on states (broad discretion) and higher on detainees (weaker protections). The three readings form a constraint family linked by mutual doctrinal contestation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(humane_treatment_standard__proportionality_balancing, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
