% ============================================================================
% CONSTRAINT STORY: nsl_legal_text__jurisdictional_capture_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nsl_legal_text__jurisdictional_capture_reading, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: nsl_legal_text__jurisdictional_capture_reading
 *   human_readable: NSL Jurisdictional Capture Reading
 *   domain: constitutional_law/political_sociology/international_relations
 *
 * SUMMARY:
 *   The NSL (Law of the PRC on Safeguarding National Security in the HKSAR),
 *   imposed by Beijing on June 30, 2020, bypassing Hong Kong's legislature,
 *   is read here as a vehicle for transplanting mainland China's socialist
 *   legal system into Hong Kong's common law jurisdiction. This reading
 *   identifies the constraint's core operation as jurisdictional capture: the
 *   NPCSC's binding interpretation power (Article 65), the Chief Executive's
 *   designation of national security judges (Article 44), the Committee for
 *   Safeguarding National Security's authority to remove cases from ordinary
 *   courts (Article 46), and the Office for Safeguarding National Security's
 *   extraterritorial operation (Article 58) collectively displace common law
 *   adjudication with mainland-style political-legal logic. The claimed
 *   coordination function (national security order) is real but the
 *   extraction — institutional independence of the judiciary and legal
 *   profession — is structurally asymmetric and actively enforced.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nsl_legal_text__jurisdictional_capture_reading, 0.65).
domain_priors:suppression_score(nsl_legal_text__jurisdictional_capture_reading, 0.75).
domain_priors:theater_ratio(nsl_legal_text__jurisdictional_capture_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nsl_legal_text__jurisdictional_capture_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(nsl_legal_text__jurisdictional_capture_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(nsl_legal_text__jurisdictional_capture_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nsl_legal_text__jurisdictional_capture_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(nsl_legal_text__jurisdictional_capture_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nsl_legal_text__jurisdictional_capture_reading, tangled_rope).
narrative_ontology:human_readable(nsl_legal_text__jurisdictional_capture_reading, "NSL Jurisdictional Capture Reading").
narrative_ontology:topic_domain(nsl_legal_text__jurisdictional_capture_reading, "constitutional_law/political_sociology/international_relations").

domain_priors:requires_active_enforcement(nsl_legal_text__jurisdictional_capture_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nsl_legal_text__jurisdictional_capture_reading, '9ac0f212-e2c9-4e02-abcc-4a918d0c6caf').
narrative_ontology:cs_kernel_codification('9ac0f212-e2c9-4e02-abcc-4a918d0c6caf', formalized).
narrative_ontology:cs_authority_grounding('9ac0f212-e2c9-4e02-abcc-4a918d0c6caf', extraction).
narrative_ontology:cs_interpretation_layer_present('9ac0f212-e2c9-4e02-abcc-4a918d0c6caf').
narrative_ontology:cs_reading_relation('9ac0f212-e2c9-4e02-abcc-4a918d0c6caf', nsl_legal_text__sovereignty_restoration_reading, coexists_with).
narrative_ontology:cs_reading_relation('9ac0f212-e2c9-4e02-abcc-4a918d0c6caf', nsl_legal_text__democratic_enclosure_reading, coexists_with).
narrative_ontology:cs_axiom('9ac0f212-e2c9-4e02-abcc-4a918d0c6caf', foundational, mainland_legal_transplantation_necessary_for_security).
narrative_ontology:cs_axiom_status(mainland_legal_transplantation_necessary_for_security, holdable).
narrative_ontology:cs_axiom_grounding('9ac0f212-e2c9-4e02-abcc-4a918d0c6caf', mainland_legal_transplantation_necessary_for_security, instrumental).
narrative_ontology:cs_axiom('9ac0f212-e2c9-4e02-abcc-4a918d0c6caf', secondary, common_law_autonomy_incompatible_with_national_security).
narrative_ontology:cs_axiom_status(common_law_autonomy_incompatible_with_national_security, holdable).
narrative_ontology:cs_axiom_grounding('9ac0f212-e2c9-4e02-abcc-4a918d0c6caf', common_law_autonomy_incompatible_with_national_security, instrumental).
narrative_ontology:cs_reference_frame('9ac0f212-e2c9-4e02-abcc-4a918d0c6caf', common_law_autonomy_pre_nsl).
narrative_ontology:cs_drift_state('9ac0f212-e2c9-4e02-abcc-4a918d0c6caf', post_nsl_implementation, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('9ac0f212-e2c9-4e02-abcc-4a918d0c6caf', '2026-08-03T12:00:00Z').
narrative_ontology:cs_kernel_id(nsl_legal_text__jurisdictional_capture_reading, nsl_legal_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nsl_legal_text__jurisdictional_capture_reading, mainland_security_apparatus).
narrative_ontology:constraint_beneficiary(nsl_legal_text__jurisdictional_capture_reading, ccp_legal_political_system).
narrative_ontology:constraint_victim(nsl_legal_text__jurisdictional_capture_reading, hk_judiciary).
narrative_ontology:constraint_victim(nsl_legal_text__jurisdictional_capture_reading, hk_legal_profession).
narrative_ontology:constraint_victim(nsl_legal_text__jurisdictional_capture_reading, common_law_tradition).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(nsl_legal_text__jurisdictional_capture_reading, hong_kong_residents).
narrative_ontology:constraint_vindicates(nsl_legal_text__jurisdictional_capture_reading, national_security_primacy).
narrative_ontology:constraint_vindicates(nsl_legal_text__jurisdictional_capture_reading, sovereign_legal_unification).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Drafts, promulgates, and authoritatively interprets the NSL through the NPCSC; designates cases for mainland jurisdiction; operates the Office for Safeguarding National Security in Hong Kong with extraterritorial powers. Collects institutional expansion: the NSL extends mainland legal authority into a common law jurisdiction without treaty or local legislative consent.
narrative_ontology:constraint_stakeholder(nsl_legal_text__jurisdictional_capture_reading, mainland_security_apparatus, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(nsl_legal_text__jurisdictional_capture_reading, mainland_security_apparatus, beneficiary).

% Gains a normalized channel for transplanting socialist rule-of-law concepts into Hong Kong's legal order, legitimizing the Party's leadership over law. The NSL's interpretation mechanism (Article 65) makes NPCSC interpretations binding on HK courts, creating a standing transplantation pipeline.
narrative_ontology:constraint_stakeholder(nsl_legal_text__jurisdictional_capture_reading, ccp_legal_political_system, beneficiary,
    institutional, generational, arbitrage, global).

% Loses control over national security case assignment (Article 44/46: Chief Executive designates judges; CSNS can remove jury trial; NPCSC interprets law). Judges face political vetting for designation. Professional autonomy erodes as common law interpretive methods yield to mainland statutory teleology. Exit means resignation or retirement — the judicial career path is identity-locked to the Hong Kong legal system.
narrative_ontology:constraint_stakeholder(nsl_legal_text__jurisdictional_capture_reading, hk_judiciary, payer,
    organized, biographical, constrained, local).

% Barristers and solicitors face disqualification risk for representing national security defendants; professional ethics (duty to court, client confidentiality) conflict with NSL reporting obligations. The Law Society and Bar Association have been pressured to align with national security priorities. Emigration is possible but means abandoning a practice built on common law expertise.
narrative_ontology:constraint_stakeholder(nsl_legal_text__jurisdictional_capture_reading, hk_legal_profession, payer,
    organized, biographical, constrained, local).

% The body of precedent, interpretive methodology (stare decisis, purposive construction), and professional culture that defined Hong Kong law since 1842. Has no voice in the NSL's operation but bears the structural displacement: precedent is sidelined when NPCSC interpretations bind; common law rights jurisprudence is superseded by national security teleology.
narrative_ontology:constraint_stakeholder(nsl_legal_text__jurisdictional_capture_reading, common_law_tradition, excluded,
    moderate, civilizational, trapped, local).
narrative_ontology:stakeholder_non_agent(nsl_legal_text__jurisdictional_capture_reading, common_law_tradition).

% Lose the protective shield of common law procedural rights (jury trial, open justice, independent judiciary) in national security cases. Face extraterritorial application (Article 38). Emigration is the primary exit but is costly and not universally available.
narrative_ontology:constraint_stakeholder(nsl_legal_text__jurisdictional_capture_reading, hong_kong_residents, payer,
    moderate, biographical, constrained, local).

% UN human rights bodies, foreign bar associations, and academic commentators document the constraint's operation from outside. They cannot enforce remedies but create reputational costs and evidentiary records for future accountability.
narrative_ontology:constraint_stakeholder(nsl_legal_text__jurisdictional_capture_reading, international_legal_observers, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains national security and legal order in Hong Kong through a unified legal framework that closes the Article 23 legislative gap and provides central authority over secession, subversion, terrorism, and collusion offenses.
% TRANSFER_FUNCTION: Moves interpretive authority and judicial independence from Hong Kong common law institutions to mainland security-designated bodies, transferring legal autonomy to central sovereign control via the NPCSC interpretation power (Article 65), designated judges (Article 44), and the Committee for Safeguarding National Security (Articles 12-14).
% ABSENT_VOICES: Hong Kong civil society organizations, independent legal academics, pro-democracy legislators (disqualified or resigned), and international human rights bodies who would challenge the erosion of common law protections but are excluded from the NSL's legislative and interpretive process.
% DISAPPEARANCE_RATIONALE: If the NSL's jurisdictional capture provisions vanished overnight, Hong Kong courts would revert to common law interpretation of security offenses, the Committee for Safeguarding National Security would lose its case assignment and jurisdiction-transfer powers, the NPCSC interpretation mechanism would cease to bind HK courts, and the legal profession would regain professional autonomy over national security defense — the Hong Kong legal order would reorganize around common law constitutionalism.
% FOUNDING_PROBLEM: The perceived inability of Hong Kong's common law system to address national security threats after the 2019 protests, and Beijing's determination to close the 'legal vacuum' in Hong Kong's mini-constitution (Basic Law Article 23) which had remained unimplemented for 23 years.
% FOUNDING_PROBLEM_CORROBORATION: Beijing and Hong Kong government officials attest the founding problem remains live (ongoing security threats requiring centralized legal response). Hong Kong Bar Association, international legal scholars (e.g. Bingham Centre, ICJ), and former common law judges (e.g. Lord Sumption, Lord Hoffmann) attest the founding problem was manufactured or exaggerated to justify legal system transplantation; the 2019 protests were political, not a national security vacuum requiring mainland legal implantation.
narrative_ontology:disappearance_verdict(nsl_legal_text__jurisdictional_capture_reading, world_rearranges).
narrative_ontology:founding_problem_status(nsl_legal_text__jurisdictional_capture_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nsl_legal_text__jurisdictional_capture_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(nsl_legal_text__jurisdictional_capture_reading, 'none', 1).
narrative_ontology:epsilon_provenance(nsl_legal_text__jurisdictional_capture_reading, 0.65, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nsl_legal_text__jurisdictional_capture_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(nsl_legal_text__jurisdictional_capture_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(nsl_legal_text__jurisdictional_capture_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.65) is moderate-high: the constraint captures the institutional independence of a common law system — its judiciary, bar, and precedent tradition — transferring interpretive authority to a mainland security apparatus. Suppression (0.75) is high: the constraint's persistence depends on active exclusion of common law interpretive methods, disqualification of lawyers, secret trials, and extraterritorial reach. Theater ratio (0.40) is moderate: the security review and public order functions are real, but a growing share of enforcement (designated judges, closed hearings, NPCSC interpretations) serves the transplantation logic rather than genuine security coordination. Accessibility collapse (0.70) is high: once the NSL's interpretive framework is accepted, common law alternatives (proportionality review, open justice, jury trial) become structurally unavailable in national security cases. Resistance (0.60) is moderate-high: the legal profession, international observers, and some judges have resisted, but institutional capture limits effective opposition.
 *
 * PERSPECTIVAL GAP:
 *   From the mainland security apparatus seat, the NSL is coordination: it solves the genuine problem of Hong Kong's missing Article 23 legislation and provides unified national security law. From the HK judiciary seat, the same structure is extraction: it removes their constitutional role as independent interpreters, subjects them to political vetting, and replaces common law method with statutory teleology. From the legal profession seat, it is a snare: professional ethics are overridden, defense becomes dangerous, and the common law skill set is devalued. The engine computes this seat divergence from the stakeholder power/exit/scope data.
 *
 * DIRECTIONALITY LOGIC:
 *   Mainland security apparatus and CCP legal-political system are structural beneficiaries (d near 0.0): they gain jurisdictional expansion and normalized transplantation pipeline with minimal cost. HK judiciary and legal profession are targets (d near 1.0): they bear the full cost of institutional displacement with constrained exit (identity-locked to HK legal system). Common law tradition is excluded but trapped — it cannot exit the jurisdiction it defines. Hong Kong residents are payers with constrained exit (emigration possible but costly). International observers are analytical (d=0.5). The engine derives directionality from these structural positions.
 *
 * MANDATROPHY ANALYSIS:
 *   The NSL's founding mandate (close Article 23 gap, restore order post-2019) is contested as live vs. manufactured. If the founding problem is dead (2019 unrest resolved, no genuine security vacuum), the constraint persists as mandatrophy — a legal transplantation justified by an expired emergency. The reading's structure captures this: the transplantation pipeline (NPCSC interpretation, designated judges) has no sunset and expands beyond the original mandate (e.g., 2024 Article 23 legislation broadening NSL offenses). The theater ratio rising from 0.20 to 0.40 over the interval tracks this: more enforcement energy goes to maintaining the transplantation than to security coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_boundary,
    'Where does the jurisdictional capture reading end and the democratic enclosure reading begin — are they structurally distinct constraints or facets of the same extraction?',
    'Test whether the beneficiary/victim sets differ: jurisdictional capture targets legal institutions (judiciary, bar, precedent); democratic enclosure targets political actors (protesters, legislators, voters). If the extraction mechanisms (NPCSC interpretation, designated judges) serve both, they may be a single constraint with multiple extraction channels.',
    'If they are one constraint, extractiveness is higher (multiple victim sets) and the coordination story is thinner. If distinct, each has lower extractiveness but the family shows systemic extraction across legal and political domains.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_boundary, conceptual, 'Whether jurisdictional capture and democratic enclosure are one constraint or two linked constraints.').

omega_variable(
    common_law_resilience,
    'Can the common law tradition survive as a living practice within the NSL''s interpretive framework, or is displacement total?',
    'Track whether HK courts develop a ''common law within national security'' jurisprudence (e.g., using proportionality, procedural fairness) that preserves professional autonomy, or whether NPCSC interpretations and designated judges fully determine outcomes.',
    'If common law adapts (hybrid jurisprudence emerges), extractiveness is lower and the constraint may be tangled_rope with genuine coordination residual. If displacement is total, extractiveness approaches snare levels and the constraint is jurisdictional capture in its purest form.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(common_law_resilience, empirical, 'Whether common law interpretive methods retain any operative space in national security adjudication.').

omega_variable(
    transplantation_vs_restoration_ambiguity,
    'Is the NSL''s legal transplantation a deliberate CCP strategy or an emergent consequence of sovereign assertion?',
    'Analyze internal Party documents, NPCSC interpretation reasoning, and the sequence of legal changes: if transplantation language (socialist rule of law, Party leadership over law) appears in authoritative interpretations before HK courts adopt it, strategy is indicated. If HK courts independently converge, emergence is indicated.',
    'If deliberate strategy, the constraint''s extraction is designed (snare-like intent). If emergent, it may be tangled_rope with coordination function genuinely believed by some beneficiaries. This affects mandatrophy analysis: designed extraction persists after mandate expiry; emergent may self-correct.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(transplantation_vs_restoration_ambiguity, conceptual, 'Whether mainland legal transplantation is intentional design or structural emergence.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (legal barriers, institutional redesign) or internalized (professional self-censorship, anticipatory conformity)?',
    'Post-exit suppression trajectory: track lawyers and judges who emigrate or retire — if suppression persists in their professional identity and practice abroad, reclassify as partially internalized. Survey legal profession on risk perception vs. actual sanction rates.',
    'If internalized, effective suppression is higher than structural measure suggests — the constraint operates through professional identity fusion, not just legal rules. This would increase extractiveness for the legal profession seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in the legal profession.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nsl_legal_text__jurisdictional_capture_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nsl_jurisdictional_capture_tr_t0, nsl_legal_text__jurisdictional_capture_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement_basis(nsl_jurisdictional_capture_tr_t0, observed).
narrative_ontology:measurement(nsl_jurisdictional_capture_tr_t2, nsl_legal_text__jurisdictional_capture_reading, theater_ratio, 2, 0.25).
narrative_ontology:measurement_basis(nsl_jurisdictional_capture_tr_t2, observed).
narrative_ontology:measurement(nsl_jurisdictional_capture_tr_t5, nsl_legal_text__jurisdictional_capture_reading, theater_ratio, 5, 0.3).
narrative_ontology:measurement_basis(nsl_jurisdictional_capture_tr_t5, observed).
narrative_ontology:measurement(nsl_jurisdictional_capture_tr_t7, nsl_legal_text__jurisdictional_capture_reading, theater_ratio, 7, 0.35).
narrative_ontology:measurement_basis(nsl_jurisdictional_capture_tr_t7, observed).
narrative_ontology:measurement(nsl_jurisdictional_capture_tr_t10, nsl_legal_text__jurisdictional_capture_reading, theater_ratio, 10, 0.4).
narrative_ontology:measurement_basis(nsl_jurisdictional_capture_tr_t10, observed).

% Extraction over time
narrative_ontology:measurement(nsl_jurisdictional_capture_be_t0, nsl_legal_text__jurisdictional_capture_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement_basis(nsl_jurisdictional_capture_be_t0, observed).
narrative_ontology:measurement(nsl_jurisdictional_capture_be_t2, nsl_legal_text__jurisdictional_capture_reading, base_extractiveness, 2, 0.55).
narrative_ontology:measurement_basis(nsl_jurisdictional_capture_be_t2, observed).
narrative_ontology:measurement(nsl_jurisdictional_capture_be_t5, nsl_legal_text__jurisdictional_capture_reading, base_extractiveness, 5, 0.6).
narrative_ontology:measurement_basis(nsl_jurisdictional_capture_be_t5, observed).
narrative_ontology:measurement(nsl_jurisdictional_capture_be_t7, nsl_legal_text__jurisdictional_capture_reading, base_extractiveness, 7, 0.63).
narrative_ontology:measurement_basis(nsl_jurisdictional_capture_be_t7, observed).
narrative_ontology:measurement(nsl_jurisdictional_capture_be_t10, nsl_legal_text__jurisdictional_capture_reading, base_extractiveness, 10, 0.65).
narrative_ontology:measurement_basis(nsl_jurisdictional_capture_be_t10, observed).

% Suppression requirement over time
narrative_ontology:measurement(nsl_jurisdictional_capture_su_t0, nsl_legal_text__jurisdictional_capture_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement_basis(nsl_jurisdictional_capture_su_t0, observed).
narrative_ontology:measurement(nsl_jurisdictional_capture_su_t2, nsl_legal_text__jurisdictional_capture_reading, suppression_requirement, 2, 0.65).
narrative_ontology:measurement_basis(nsl_jurisdictional_capture_su_t2, observed).
narrative_ontology:measurement(nsl_jurisdictional_capture_su_t5, nsl_legal_text__jurisdictional_capture_reading, suppression_requirement, 5, 0.7).
narrative_ontology:measurement_basis(nsl_jurisdictional_capture_su_t5, observed).
narrative_ontology:measurement(nsl_jurisdictional_capture_su_t7, nsl_legal_text__jurisdictional_capture_reading, suppression_requirement, 7, 0.73).
narrative_ontology:measurement_basis(nsl_jurisdictional_capture_su_t7, observed).
narrative_ontology:measurement(nsl_jurisdictional_capture_su_t10, nsl_legal_text__jurisdictional_capture_reading, suppression_requirement, 10, 0.75).
narrative_ontology:measurement_basis(nsl_jurisdictional_capture_su_t10, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nsl_legal_text__jurisdictional_capture_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(nsl_legal_text__jurisdictional_capture_reading, 0.1).
narrative_ontology:affects_constraint(nsl_legal_text__jurisdictional_capture_reading, nsl_legal_text__sovereignty_restoration_reading).
narrative_ontology:affects_constraint(nsl_legal_text__jurisdictional_capture_reading, nsl_legal_text__democratic_enclosure_reading).
narrative_ontology:affects_constraint(nsl_legal_text__jurisdictional_capture_reading, hong_kong_article_23_legislation).
narrative_ontology:affects_constraint(nsl_legal_text__jurisdictional_capture_reading, mainland_legal_aid_in_hong_kong).

% DUAL FORMULATION NOTE:
% This reading (jurisdictional capture) and its siblings (sovereignty restoration, democratic enclosure) form the NSL legal text constraint family. They share kernel nsl_legal_text but instantiate different constraints: this reading has moderate-high extractiveness (0.65) with legal-institutional victims; sovereignty restoration has low extractiveness (~0.2) with no victims (claimed mountain); democratic enclosure has high extractiveness (~0.8) with political-civil society victims. The family demonstrates how one legal text generates multiple constraint stories via reading-indexed ε.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(nsl_legal_text__jurisdictional_capture_reading, organized, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
