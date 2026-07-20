% ============================================================================
% CONSTRAINT STORY: second_amendment_arms_right__individual_right_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_second_amendment_arms_right__individual_right_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: second_amendment_arms_right__individual_right_reading
 *   human_readable: Second Amendment Individual Right Reading (Pre-existing Liberty)
 *   domain: constitutional_law/political_philosophy
 *
 * SUMMARY:
 *   This constraint instantiates the individual_right_reading of the
 *   contested kernel second_amendment_arms_right. The sibling readings are
 *   collective_right_reading (militia-centered) and civic_republican_reading
 *   (armed citizenship). This reading treats the right as a pre-existing
 *   individual liberty protected against federal (and state) infringement,
 *   displacing the militia-dependency framing. It coordinates around a stable
 *   liberty guarantee while extracting vast regulatory capacity from federal
 *   and state governments.
 *
 * KEY AGENTS:
 *   - individual_gun_owners: Primary beneficiary (organized/mobile) â receives constitutional protection against prohibition
 *   - federal_regulatory_authority: Primary target (institutional/constrained) â bears loss of legislative authority
 *   - state_regulatory_authority: Secondary target (institutional/constrained) â bears incorporation-driven narrowing of police power
 *   - federal_judiciary: Agenda setter/enforcer (institutional/constrained) â administers constraint through judicial review
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_arms_right__individual_right_reading, 0.72).
domain_priors:suppression_score(second_amendment_arms_right__individual_right_reading, 0.8).
domain_priors:theater_ratio(second_amendment_arms_right__individual_right_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_arms_right__individual_right_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(second_amendment_arms_right__individual_right_reading, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(second_amendment_arms_right__individual_right_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_arms_right__individual_right_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(second_amendment_arms_right__individual_right_reading, resistance, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_arms_right__individual_right_reading, tangled_rope).
narrative_ontology:human_readable(second_amendment_arms_right__individual_right_reading, "Second Amendment Individual Right Reading (Pre-existing Liberty)").
narrative_ontology:topic_domain(second_amendment_arms_right__individual_right_reading, "constitutional_law/political_philosophy").

domain_priors:requires_active_enforcement(second_amendment_arms_right__individual_right_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_arms_right__individual_right_reading, 'f720bd98-f485-4649-adbb-595f773b855a').
narrative_ontology:cs_kernel_codification('f720bd98-f485-4649-adbb-595f773b855a', fixed_text).
narrative_ontology:cs_authority_grounding('f720bd98-f485-4649-adbb-595f773b855a', lineage).
narrative_ontology:cs_interpretation_layer_present('f720bd98-f485-4649-adbb-595f773b855a').
narrative_ontology:cs_reading_relation('f720bd98-f485-4649-adbb-595f773b855a', second_amendment_arms_right__collective_right_reading, forecloses).
narrative_ontology:cs_reading_relation('f720bd98-f485-4649-adbb-595f773b855a', second_amendment_arms_right__civic_republican_reading, coexists_with).
narrative_ontology:cs_axiom('f720bd98-f485-4649-adbb-595f773b855a', foundational, individual_arms_right_preexists_polity).
narrative_ontology:cs_axiom_status(individual_arms_right_preexists_polity, holdable).
narrative_ontology:cs_axiom_grounding('f720bd98-f485-4649-adbb-595f773b855a', individual_arms_right_preexists_polity, deontological).
narrative_ontology:cs_axiom('f720bd98-f485-4649-adbb-595f773b855a', secondary, operative_clause_independent_of_prefatory).
narrative_ontology:cs_axiom_status(operative_clause_independent_of_prefatory, holdable).
narrative_ontology:cs_axiom_grounding('f720bd98-f485-4649-adbb-595f773b855a', operative_clause_independent_of_prefatory, conventional).
narrative_ontology:cs_reference_frame('f720bd98-f485-4649-adbb-595f773b855a', founding_era_public_meaning).
narrative_ontology:cs_drift_state('f720bd98-f485-4649-adbb-595f773b855a', contemporary_post_bruen, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('f720bd98-f485-4649-adbb-595f773b855a', '').
narrative_ontology:cs_kernel_id(second_amendment_arms_right__individual_right_reading, second_amendment_arms_right).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_arms_right__individual_right_reading, individual_gun_owners).
narrative_ontology:constraint_victim(second_amendment_arms_right__individual_right_reading, federal_regulatory_authority).
narrative_ontology:constraint_victim(second_amendment_arms_right__individual_right_reading, state_regulatory_authority).
narrative_ontology:constraint_vindicates(second_amendment_arms_right__individual_right_reading, original_public_meaning_jurisprudence).
narrative_ontology:constraint_vindicates(second_amendment_arms_right__individual_right_reading, natural_rights_theory).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Possess and acquire firearms for self-defense, sport, or collection under constitutional protection that forecloses legislative prohibition. Benefit from judicial invalidation of restrictive laws. Exit is mobile: they can choose not to own firearms, but the constraint protects their option to do so regardless of majority preference.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__individual_right_reading, individual_gun_owners, beneficiary,
    organized, generational, mobile, national).

% Enacts criminal law, tax law, and regulatory schemes concerning firearms. Bears the cost of having entire categories of prohibitionist legislation struck down or chilled by constitutional litigation. Exit is constrained: bound by Article VI and judicial supremacy to comply with Supreme Court interpretations.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__individual_right_reading, federal_regulatory_authority, payer,
    institutional, generational, constrained, national).

% Enacts police-power regulations including licensing, registration, and assault-weapon bans. Bears extraction as incorporation and Bruen's history-tradition test invalidate or narrow state regulatory choices. Exit is constrained: preemption and incorporation bind state actors to federal constitutional floor.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__individual_right_reading, state_regulatory_authority, payer,
    institutional, generational, constrained, national).

% Interprets the Second Amendment, develops methodological tests (text-history-tradition), and strikes down or upholds legislation. Administers the constraint without directly capturing the substantive liberty; gains institutional authority as the final arbiter of a contested constitutional right.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__individual_right_reading, federal_judiciary, agenda_setter,
    institutional, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(second_amendment_arms_right__individual_right_reading, individual_gun_owners).
narrative_ontology:fixing_cost_class(second_amendment_arms_right__individual_right_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a judicially enforceable boundary that prevents federal (and incorporated state) government from prohibiting or severely restricting the possession of commonly held arms by law-abiding individuals, thereby reducing legal uncertainty about the limits of state power over personal self-defense.
% TRANSFER_FUNCTION: Transfers the authority to prohibit or regulate personal arms possession from federal and state legislatures to individual citizens and federal courts; moves the veto power over prohibitionist legislation from democratic majorities to constitutional litigants and judges.
% ABSENT_VOICES: Urban communities experiencing concentrated gun violence, public health researchers treating firearm mortality as epidemic, and foreign jurisdictions with prohibitionist regimes are present in discourse but structurally disadvantaged: the constraint removes entire categories of regulatory intervention from legislative reach regardless of local majority preference or empirical demonstration of public safety benefit.
% DISAPPEARANCE_RATIONALE: If the individual right reading vanished, federal and state legislatures could enact broad prohibition, registration, and capacity limits currently foreclosed by constitutional litigation; the judicial docket would lose Second Amendment scrutiny; law enforcement and regulatory agencies would regain the full scope of criminal law authority over arms possession.
% FOUNDING_PROBLEM: Fear of centralized military power and standing armies; ensuring that the people retain the independent means of self-defense against foreign invasion, domestic insurrection, and government tyranny.
% FOUNDING_PROBLEM_CORROBORATION: Dissenting justices in Heller and Bruen, along with academic historians outside the gun-rights advocacy community, contest the majority's historical narrative, arguing the original understanding tied the right to militia service rather than individual self-defense detached from collective security.
narrative_ontology:disappearance_verdict(second_amendment_arms_right__individual_right_reading, world_rearranges).
narrative_ontology:founding_problem_status(second_amendment_arms_right__individual_right_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_arms_right__individual_right_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(second_amendment_arms_right__individual_right_reading, 'none', 1).
narrative_ontology:epsilon_provenance(second_amendment_arms_right__individual_right_reading, 0.72, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_arms_right__individual_right_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(second_amendment_arms_right__individual_right_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(second_amendment_arms_right__individual_right_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.72) because the constraint extracts entire categories of regulatory authority from democratic legislatures. Suppression is higher (0.8) because the constraint's persistence depends on active judicial enforcement striking down or chilling democratically enacted laws; legislatures lack workable alternatives to prohibition once the right is invoked. Theater ratio is moderate (0.48): original public meaning methodology provides genuine interpretive structure, but a substantial share of judicial rhetoric performs adherence to 1791 context in modern regulatory environments far removed from the founding. Accessibility collapse is high (0.8) because constitutional entitlement collapses regulatory alternatives for the government. Resistance is substantial (0.65) from dissenting jurists, resisting states, and academic critics.
 *
 * PERSPECTIVAL GAP:
 *   From the individual gun owner's seat, the constraint appears as a protective rope securing fundamental liberty against government overreach. From the federal and state regulatory seats, the identical structure operates as an enforced extraction of democratic policymaking capacity â a judicially imposed barrier to majoritarian public safety regulation. The federal judiciary experiences it as an interpretive enforcement duty that expands its institutional authority without delivering the substantive liberty benefit to its own seat. The engine computes these divergent classifications from the same structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Individual gun owners are declared beneficiaries with mobile exit options, placing their directionality near the full-beneficiary end (low d, damped or negative effective extraction). Federal and state regulatory authorities are declared victims with constrained exit, placing their directionality near the full-target end (high d, amplified effective extraction). The federal judiciary is not declared in either beneficiary or victim arrays; its directionality reverts to the institutional power atom's canonical fallback, reflecting its neutral administrative position.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â fear of standing armies and government tyranny â is contested in contemporary context where the United States maintains a permanent professional military. The constraint's mandate may have shifted from a civic-republican coordination mechanism (armed populace as check on power) toward a purely individual self-defense liberty decoupled from collective security. However, the constraint is not a piton: beneficiaries (gun owners) actively maintain and litigate it, and the extraction from government is substantive rather than merely theatrical. The theater ratio below 0.5 confirms that genuine legal force remains.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_or_positive_construct,
    'Is the individual right genuinely pre-political natural law, or is it a judicial construct read into a contested constitutional text?',
    'Constitutional historical discovery or philosophical argument establishing whether the right exists independent of positive law.',
    'If purely natural law, directionality for government shifts toward mountain-like inevitability; if construct, extraction is contingent on interpretive choice and the constraint is more clearly tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_or_positive_construct, conceptual, 'Whether the right is natural or constructed').

omega_variable(
    scope_of_modern_arms,
    'Does the individual right reading extend to arms technologies and social contexts unimaginable in 1791, and on what principled basis?',
    'Series of Supreme Court cases defining the outer boundary of ''bearable arms'' in modern context.',
    'If the constraint extends to all modern weapons, extractiveness from regulatory authority is maximal; if limited to 1791 analogues, the extraction is narrower and the constraint more scaffold-like.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scope_of_modern_arms, empirical, 'Scope of protected modern arms').

omega_variable(
    incorporation_against_states_settled,
    'Is the Second Amendment''s incorporation against the states fully settled doctrine, or does it remain contested in certain regulatory contexts?',
    'Post-McDonald litigation tracking state resistance and lower-court compliance.',
    'If contested, spatial scope is effectively national rather than universal for state regulatory authority, modulating directionality for states.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(incorporation_against_states_settled, empirical, 'Incorporation against states status').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_arms_right__individual_right_reading, 0, 44).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(seco_tr_t0, second_amendment_arms_right__individual_right_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(seco_tr_t10, second_amendment_arms_right__individual_right_reading, theater_ratio, 10, 0.3).
narrative_ontology:measurement(seco_tr_t20, second_amendment_arms_right__individual_right_reading, theater_ratio, 20, 0.4).
narrative_ontology:measurement(seco_tr_t30, second_amendment_arms_right__individual_right_reading, theater_ratio, 30, 0.5).
narrative_ontology:measurement(seco_tr_t40, second_amendment_arms_right__individual_right_reading, theater_ratio, 40, 0.52).
narrative_ontology:measurement(seco_tr_t44, second_amendment_arms_right__individual_right_reading, theater_ratio, 44, 0.48).

% Extraction over time
narrative_ontology:measurement(seco_be_t0, second_amendment_arms_right__individual_right_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(seco_be_t10, second_amendment_arms_right__individual_right_reading, base_extractiveness, 10, 0.35).
narrative_ontology:measurement(seco_be_t20, second_amendment_arms_right__individual_right_reading, base_extractiveness, 20, 0.45).
narrative_ontology:measurement(seco_be_t30, second_amendment_arms_right__individual_right_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement(seco_be_t40, second_amendment_arms_right__individual_right_reading, base_extractiveness, 40, 0.7).
narrative_ontology:measurement(seco_be_t44, second_amendment_arms_right__individual_right_reading, base_extractiveness, 44, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(seco_su_t0, second_amendment_arms_right__individual_right_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(seco_su_t10, second_amendment_arms_right__individual_right_reading, suppression_requirement, 10, 0.35).
narrative_ontology:measurement(seco_su_t20, second_amendment_arms_right__individual_right_reading, suppression_requirement, 20, 0.5).
narrative_ontology:measurement(seco_su_t30, second_amendment_arms_right__individual_right_reading, suppression_requirement, 30, 0.75).
narrative_ontology:measurement(seco_su_t40, second_amendment_arms_right__individual_right_reading, suppression_requirement, 40, 0.78).
narrative_ontology:measurement(seco_su_t44, second_amendment_arms_right__individual_right_reading, suppression_requirement, 44, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
