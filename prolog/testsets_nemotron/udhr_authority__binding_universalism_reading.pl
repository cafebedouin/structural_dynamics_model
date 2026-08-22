% ============================================================================
% CONSTRAINT STORY: udhr_authority__binding_universalism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_udhr_authority__binding_universalism_reading, []).

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
 *   constraint_id: udhr_authority__binding_universalism_reading
 *   human_readable: UDHR Binding Universalism: Justiciable Rights Enforceable Against States Regardless of Consent
 *   domain: international_law/political_philosophy/human_rights
 *
 * SUMMARY:
 *   The binding universalism reading of UDHR authority asserts that the
 *   Declaration (and its treaty progeny: ICCPR, ICESCR, regional conventions)
 *   creates justiciable individual rights enforceable against states
 *   regardless of their ongoing consent. This reading powers the entire
 *   architecture of international human rights law: treaty bodies with
 *   complaint mechanisms, regional courts with binding judgments, universal
 *   jurisdiction claims, and the responsibility to protect doctrine. The
 *   constraint is structurally a tangled rope because it performs genuine
 *   coordination (universal baseline, cross-border accountability vocabulary,
 *   prevention of atrocity) while simultaneously extracting sovereignty from
 *   states and subordinating traditional authority structures to an external
 *   interpretive hierarchy they did not choose. The enforcement machinery
 *   (tribunals, monitoring bodies, NGO litigation networks) actively
 *   maintains the constraint against state resistance. The claimed_type is
 *   tangled_rope — the constraint's own proponents frame it as rope (pure
 *   coordination), but the authored metrics reflect the asymmetric extraction
 *   on non-consenting states and traditional orders.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(udhr_authority__binding_universalism_reading, 0.68).
domain_priors:suppression_score(udhr_authority__binding_universalism_reading, 0.72).
domain_priors:theater_ratio(udhr_authority__binding_universalism_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(udhr_authority__binding_universalism_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(udhr_authority__binding_universalism_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(udhr_authority__binding_universalism_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(udhr_authority__binding_universalism_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(udhr_authority__binding_universalism_reading, resistance, 0.61).

% --- Constraint claim ---
narrative_ontology:constraint_claim(udhr_authority__binding_universalism_reading, tangled_rope).
narrative_ontology:human_readable(udhr_authority__binding_universalism_reading, "UDHR Binding Universalism: Justiciable Rights Enforceable Against States Regardless of Consent").
narrative_ontology:topic_domain(udhr_authority__binding_universalism_reading, "international_law/political_philosophy/human_rights").

domain_priors:requires_active_enforcement(udhr_authority__binding_universalism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(udhr_authority__binding_universalism_reading, '7b5f58c6-c1d5-4ca9-9139-54076d51c6b0').
narrative_ontology:cs_kernel_codification('7b5f58c6-c1d5-4ca9-9139-54076d51c6b0', fixed_text).
narrative_ontology:cs_authority_grounding('7b5f58c6-c1d5-4ca9-9139-54076d51c6b0', extraction).
narrative_ontology:cs_interpretation_layer_present('7b5f58c6-c1d5-4ca9-9139-54076d51c6b0').
narrative_ontology:cs_reading_relation('7b5f58c6-c1d5-4ca9-9139-54076d51c6b0', udhr_authority__aspirational_sovereignty_reading, forecloses).
narrative_ontology:cs_reading_relation('7b5f58c6-c1d5-4ca9-9139-54076d51c6b0', udhr_authority__customary_emergence_reading, influences).
narrative_ontology:cs_axiom('7b5f58c6-c1d5-4ca9-9139-54076d51c6b0', foundational, individual_rights_override_state_consent).
narrative_ontology:cs_axiom_status(individual_rights_override_state_consent, holdable).
narrative_ontology:cs_axiom_grounding('7b5f58c6-c1d5-4ca9-9139-54076d51c6b0', individual_rights_override_state_consent, deontological).
narrative_ontology:cs_axiom('7b5f58c6-c1d5-4ca9-9139-54076d51c6b0', foundational, tribunals_as_authentic_interpreters_of_universal_dignity).
narrative_ontology:cs_axiom_status(tribunals_as_authentic_interpreters_of_universal_dignity, holdable).
narrative_ontology:cs_axiom_grounding('7b5f58c6-c1d5-4ca9-9139-54076d51c6b0', tribunals_as_authentic_interpreters_of_universal_dignity, conventional).
narrative_ontology:cs_reference_frame('7b5f58c6-c1d5-4ca9-9139-54076d51c6b0', udhr_as_moral_declaration_1948).
narrative_ontology:cs_drift_state('7b5f58c6-c1d5-4ca9-9139-54076d51c6b0', contemporary_judicialized_regime, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('7b5f58c6-c1d5-4ca9-9139-54076d51c6b0', '').
narrative_ontology:cs_kernel_id(udhr_authority__binding_universalism_reading, udhr_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(udhr_authority__binding_universalism_reading, individuals_rights_holders).
narrative_ontology:constraint_beneficiary(udhr_authority__binding_universalism_reading, international_tribunals).
narrative_ontology:constraint_beneficiary(udhr_authority__binding_universalism_reading, human_rights_ngos).
narrative_ontology:constraint_victim(udhr_authority__binding_universalism_reading, state_sovereignty_claimants).
narrative_ontology:constraint_victim(udhr_authority__binding_universalism_reading, non_consenting_states).
narrative_ontology:constraint_victim(udhr_authority__binding_universalism_reading, traditional_authority_structures).
narrative_ontology:constraint_vindicates(udhr_authority__binding_universalism_reading, universal_human_dignity_above_state_authority).
narrative_ontology:constraint_vindicates(udhr_authority__binding_universalism_reading, justiciability_of_social_economic_rights).
narrative_ontology:constraint_vindicates(udhr_authority__binding_universalism_reading, obligation_erga_omnes_nature_of_human_rights).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Persons whose rights claims gain enforceable standing against states through tribunals they cannot directly access; their identity as rights-bearing subjects is constituted by the regime's recognition. Exit means abandoning the rights framework that constitutes their legal personhood.
narrative_ontology:constraint_stakeholder(udhr_authority__binding_universalism_reading, individuals_rights_holders, beneficiary,
    powerless, biographical, identity_locked, universal).

% Courts and quasi-judicial bodies (ICJ, ICC, regional human rights courts, UN treaty bodies) that interpret and enforce UDHR-derived norms. They gain institutional authority, caseload, and legitimacy from the binding reading. Can shift interpretive doctrine but are institutionally committed to the regime's expansion.
narrative_ontology:constraint_stakeholder(udhr_authority__binding_universalism_reading, international_tribunals, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(udhr_authority__binding_universalism_reading, international_tribunals, beneficiary).

% Advocacy organizations that litigate, document, and campaign using the binding framework. They receive funding, access, and moral authority from the regime's enforceability. Can pivot to other frameworks but their operational model is built around justiciable rights.
narrative_ontology:constraint_stakeholder(udhr_authority__binding_universalism_reading, human_rights_ngos, beneficiary,
    organized, biographical, mobile, global).

% States that assert sovereign prerogative over domestic jurisdiction. Bear costs of compliance, litigation, reputational sanction, and loss of policy autonomy. Exit means withdrawing from treaty regimes or rejecting tribunal jurisdiction — politically costly and diplomatically isolating.
narrative_ontology:constraint_stakeholder(udhr_authority__binding_universalism_reading, state_sovereignty_claimants, payer,
    powerful, generational, constrained, national).

% States that never ratified key treaties or entered reservations, yet face customary law pressure and universal jurisdiction claims. Their objection is structurally overridden by the regime's universalist claim. No effective exit without great-power patronage or pariah status.
narrative_ontology:constraint_stakeholder(udhr_authority__binding_universalism_reading, non_consenting_states, payer,
    moderate, generational, trapped, national).

% Religious, customary, and communal governance systems whose normative orders conflict with universal rights framings (e.g., family law, gender norms, blasphemy). Subjected to external adjudication without representation in the interpretive bodies. Exit means cultural assimilation or enclave isolation.
narrative_ontology:constraint_stakeholder(udhr_authority__binding_universalism_reading, traditional_authority_structures, payer,
    organized, generational, constrained, local).
narrative_ontology:stakeholder_secondary_role(udhr_authority__binding_universalism_reading, traditional_authority_structures, excluded).

% Academic and professional interpreters who produce the doctrinal architecture. Their careers and citations track the regime's evolution. Analytical seat: they do not collect rents or bear compliance costs directly.
narrative_ontology:constraint_stakeholder(udhr_authority__binding_universalism_reading, legal_scholars_international_lawyers, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a universal normative baseline that enables cross-border rights claims, coordinates state behavior toward minimum standards, and creates a common vocabulary for accountability that did not exist before 1948.
% TRANSFER_FUNCTION: Transfers decision-making authority over domestic rights questions from state institutions to international tribunals; transfers compliance costs (legislative reform, litigation, reparations) from rights-holders to states; transfers legitimating narrative control from sovereign political orders to universalist legal discourse.
% ABSENT_VOICES: Populations in states that reject the framework entirely (e.g., certain theocratic or authoritarian regimes) and traditional communities whose internal normative orders are not represented in treaty bodies. They would object to the universalist claim's cultural particularism but are excluded from the interpretive conversation by design — the regime claims to speak for them.
% DISAPPEARANCE_RATIONALE: If binding universalism vanished overnight, treaty bodies would lose compulsory jurisdiction, states would reclaim exclusive domestic jurisdiction over rights questions, enforcement would revert to diplomatic pressure and consent-based mechanisms, and the global human rights NGO ecosystem would lose its primary litigation architecture. The world would rearrange toward aspirational_sovereignty or customary_emergence readings.
% FOUNDING_PROBLEM: Post-WWII moral vacuum: the Holocaust and total war revealed that state sovereignty without external moral constraint permits industrial atrocity. The founding problem was how to make 'never again' institutionally effective — how to give individuals standing against the states that might destroy them.
% FOUNDING_PROBLEM_CORROBORATION: The Nuremberg prosecutors and UDHR drafters (Eleanor Roosevelt, René Cassin, Charles Malik) attested the founding problem was preventing state-perpetrated atrocity through external constraint. Contemporary critics (e.g., Carl Schmitt's intellectual heirs, postcolonial scholars like Makau Mutua) attest the problem was framed in Eurocentric terms and the solution universalized a particular liberal anthropology. No single corroborating source outside the beneficiary set commands consensus.
narrative_ontology:disappearance_verdict(udhr_authority__binding_universalism_reading, world_rearranges).
narrative_ontology:founding_problem_status(udhr_authority__binding_universalism_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(udhr_authority__binding_universalism_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(udhr_authority__binding_universalism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(udhr_authority__binding_universalism_reading, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(udhr_authority__binding_universalism_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(udhr_authority__binding_universalism_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(udhr_authority__binding_universalism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) reflects the transfer of decision authority from states to tribunals and the compliance costs imposed on non-consenting states. Suppression (0.72) is high because the regime structurally overrides objection: reservations are narrowly interpreted, persistent objector doctrine is weak for jus cogens, and universal jurisdiction reaches non-parties. Theater ratio (0.38) has risen as the coordination function (atrocity prevention) has become a smaller share of the regime's activity relative to rights-expansion litigation. Accessibility collapse (0.55) is moderate: alternatives (aspirational sovereignty, customary emergence) persist intellectually but are institutionally marginalized. Resistance (0.61) is substantial: states resist through non-ratification, reservations, non-compliance, withdrawal threats, and counter-narratives (sovereignty, cultural relativism, civilizationalism). The measurement grid tracks the regime's evolution from aspirational declaration (1948) through treaty codification (1966), monitoring machinery (1976), post-Cold War judicialization (1993), ICC establishment (2002), to contemporary universalist peak (2024).
 *
 * PERSPECTIVAL GAP:
 *   From the tribunal/NGO seat, the constraint appears as rope: a genuine coordination solution to the founding problem of atrocity prevention. From the state_sovereignty and traditional_authority seats, it appears as snare: an extractive regime that overrides consent and imposes alien norms. The engine computes this divergence from the structural data — the same constraint, different directionalities. The binding_universalism reading's core move is treating the UDHR's moral aspiration as a jurisdictional grant; the sibling readings deny that move.
 *
 * DIRECTIONALITY LOGIC:
 *   Individuals_rights_holders are identity-locked beneficiaries: the regime constitutes their legal subjectivity; exit means losing the framework that makes them rights-bearing. International_tribunals are institutional agenda-setters with arbitrage-grade exit (they could reinterpret or contract, but institutional logic pushes expansion). Human_rights_ngos are organized beneficiaries with mobile exit. State_sovereignty_claimants are powerful but constrained payers: they bear costs but can partially exit via reservations or non-compliance. Non_consenting_states are moderate-power trapped payers: the regime's universalist claim structurally overrides their objection. Traditional_authority_structures are organized payers with constrained exit, secondarily excluded from interpretation. The analytical seat sees the full structure without collecting or paying.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing state atrocity through external constraint) remains live in the sense that state-perpetrated atrocity persists, but the binding universalism regime has expanded far beyond atrocity prevention into comprehensive social regulation (family law, education, speech, property). This mission creep is the mandatrophy signal: the coordination function that justified the constraint has been exceeded by an extraction function (tribunal authority expansion, NGO ecosystem sustainment) that the founding problem does not justify. The contested status reflects that proponents deny creep (all rights are atrocity-prevention-adjacent) while critics assert it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    justiciability_boundary,
    'Where does the binding reading''s justiciability claim stop? Does it cover only civil-political rights (classic negative liberties) or also social-economic-cultural rights (positive entitlements requiring resource transfers)?',
    'Track treaty body jurisprudence on ICESCR justiciability (Optional Protocol 2008, General Comments) and state resistance patterns. The boundary determines the extraction magnitude on state budgets.',
    'If justiciability extends fully to positive rights, extractiveness on state autonomy rises substantially (resource transfers compelled by tribunals). If limited to negative rights, extraction is primarily jurisdictional (decision authority) not fiscal.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(justiciability_boundary, conceptual, 'Scope of justiciability claim within the binding reading itself.').

omega_variable(
    customary_law_formation_mechanism,
    'Does the binding reading''s claim that UDHR norms bind non-consenting states rest on genuine customary international law formation (state practice + opinio juris) or on a doctrinal bootstrap where tribunal assertions create the custom they claim to discover?',
    'Analyze ICJ and ILC practice on customary law identification: do they cite actual state practice or tribunal precedent as evidence of opinio juris? Compare with skeptical accounts (e.g., Klabbers, Koskenniemi).',
    'If bootstrap, the constraint''s legitimacy is circular — tribunals create the law they apply. This would increase suppression score (structural override of objection is doctrinally manufactured). If genuine custom, the constraint has stronger coordination credentials.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(customary_law_formation_mechanism, conceptual, 'Epistemic status of the customary law claim that binds non-consenting states.').

omega_variable(
    enforcement_asymmetry,
    'Is the regime''s enforcement capacity symmetrically distributed (great powers equally subject) or does it function as a discipline mechanism primarily for weak and middle powers?',
    'Compare compliance rates, treaty body engagement, and sanctions exposure across power tiers. Track Security Council veto usage on human rights referrals.',
    'If asymmetrical, the constraint is a snare from the weak-state seat (extraction without reciprocal constraint on strong states). If symmetrical, the tangled_rope coordination function is more credible. The current metrics assume moderate asymmetry.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_asymmetry, empirical, 'Whether enforcement asymmetry undermines the coordination claim.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(udhr_authority__binding_universalism_reading, 1948, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(udhr_binding_tr_t1948, udhr_authority__binding_universalism_reading, theater_ratio, 1948, 0.1).
narrative_ontology:measurement(udhr_binding_tr_t1966, udhr_authority__binding_universalism_reading, theater_ratio, 1966, 0.15).
narrative_ontology:measurement(udhr_binding_tr_t1976, udhr_authority__binding_universalism_reading, theater_ratio, 1976, 0.22).
narrative_ontology:measurement(udhr_binding_tr_t1993, udhr_authority__binding_universalism_reading, theater_ratio, 1993, 0.31).
narrative_ontology:measurement(udhr_binding_tr_t2002, udhr_authority__binding_universalism_reading, theater_ratio, 2002, 0.35).
narrative_ontology:measurement(udhr_binding_tr_t2024, udhr_authority__binding_universalism_reading, theater_ratio, 2024, 0.38).

% Extraction over time
narrative_ontology:measurement(udhr_binding_be_t1948, udhr_authority__binding_universalism_reading, base_extractiveness, 1948, 0.15).
narrative_ontology:measurement(udhr_binding_be_t1966, udhr_authority__binding_universalism_reading, base_extractiveness, 1966, 0.28).
narrative_ontology:measurement(udhr_binding_be_t1976, udhr_authority__binding_universalism_reading, base_extractiveness, 1976, 0.38).
narrative_ontology:measurement(udhr_binding_be_t1993, udhr_authority__binding_universalism_reading, base_extractiveness, 1993, 0.52).
narrative_ontology:measurement(udhr_binding_be_t2002, udhr_authority__binding_universalism_reading, base_extractiveness, 2002, 0.61).
narrative_ontology:measurement(udhr_binding_be_t2024, udhr_authority__binding_universalism_reading, base_extractiveness, 2024, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(udhr_binding_su_t1948, udhr_authority__binding_universalism_reading, suppression_requirement, 1948, 0.2).
narrative_ontology:measurement(udhr_binding_su_t1966, udhr_authority__binding_universalism_reading, suppression_requirement, 1966, 0.35).
narrative_ontology:measurement(udhr_binding_su_t1976, udhr_authority__binding_universalism_reading, suppression_requirement, 1976, 0.48).
narrative_ontology:measurement(udhr_binding_su_t1993, udhr_authority__binding_universalism_reading, suppression_requirement, 1993, 0.62).
narrative_ontology:measurement(udhr_binding_su_t2002, udhr_authority__binding_universalism_reading, suppression_requirement, 2002, 0.68).
narrative_ontology:measurement(udhr_binding_su_t2024, udhr_authority__binding_universalism_reading, suppression_requirement, 2024, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(udhr_authority__binding_universalism_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(udhr_authority__binding_universalism_reading, 0.12).
narrative_ontology:affects_constraint(udhr_authority__binding_universalism_reading, udhr_authority__aspirational_sovereignty_reading).
narrative_ontology:affects_constraint(udhr_authority__binding_universalism_reading, udhr_authority__customary_emergence_reading).
narrative_ontology:affects_constraint(udhr_authority__binding_universalism_reading, icc_complementarity_regime).
narrative_ontology:affects_constraint(udhr_authority__binding_universalism_reading, r2p_doctrine).
narrative_ontology:affects_constraint(udhr_authority__binding_universalism_reading, universal_jurisdiction_claims).

% DUAL FORMULATION NOTE:
% The udhr_authority kernel decomposes into three constraint stories. binding_universalism_reading claims UDHR creates immediate justiciable obligations; aspirational_sovereignty_reading claims UDHR is moral guidance requiring consent; customary_emergence_reading claims binding force emerged through state practice. Each has different ε: binding_reading highest (coercive tribunals), customary_reading medium (practice-dependent), aspirational_reading lowest (no enforcement machinery). They form a constraint family linked by affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(udhr_authority__binding_universalism_reading, institutional, 0.15).
constraint_indexing:directionality_override(udhr_authority__binding_universalism_reading, powerful, 0.78).
constraint_indexing:directionality_override(udhr_authority__binding_universalism_reading, moderate, 0.85).
constraint_indexing:directionality_override(udhr_authority__binding_universalism_reading, organized, 0.72).
constraint_indexing:directionality_override(udhr_authority__binding_universalism_reading, powerless, 0.08).
constraint_indexing:directionality_override(udhr_authority__binding_universalism_reading, analytical, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
