% ============================================================================
% CONSTRAINT STORY: public_health_mandate_authority__bodily_autonomy_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-14
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_public_health_mandate_authority__bodily_autonomy_primary, []).

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
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: public_health_mandate_authority__bodily_autonomy_primary
 *   human_readable: Non-consensual Medical Intervention Mandate (Bodily Autonomy Primary Reading)
 *   domain: public_health_law/constitutional_rights/bioethics
 *
 * SUMMARY:
 *   This constraint story instantiates the bodily_autonomy_primary reading of
 *   the public_health_mandate_authority kernel. The reading holds that any
 *   non-consensual medical intervention is a categorical violation of bodily
 *   sovereignty — no collective benefit, no matter how large, can justify it.
 *   The constraint is the standing mandate architecture (Jacobson-derived,
 *   NCVIA/PREP Act-fortified, COVID-expanded) as experienced by those it
 *   coerces. Extraction is high because the mandate transfers bodily risk and
 *   sovereignty to state-pharma actors who bear none of it. Suppression is
 *   near-total because alternatives (early treatment, targeted protection,
 *   voluntary vaccination) are actively suppressed to maintain the mandate's
 *   necessity claim. Theater is low because the mandate's enforcement is real
 *   and brutal — the performance is not the enforcement but the 'public
 *   health' framing that covers it.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(public_health_mandate_authority__bodily_autonomy_primary, 0.82).
domain_priors:suppression_score(public_health_mandate_authority__bodily_autonomy_primary, 0.91).
domain_priors:theater_ratio(public_health_mandate_authority__bodily_autonomy_primary, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(public_health_mandate_authority__bodily_autonomy_primary, extractiveness, 0.82).
narrative_ontology:constraint_metric(public_health_mandate_authority__bodily_autonomy_primary, suppression_requirement, 0.91).
narrative_ontology:constraint_metric(public_health_mandate_authority__bodily_autonomy_primary, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(public_health_mandate_authority__bodily_autonomy_primary, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(public_health_mandate_authority__bodily_autonomy_primary, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(public_health_mandate_authority__bodily_autonomy_primary, snare).
narrative_ontology:human_readable(public_health_mandate_authority__bodily_autonomy_primary, "Non-consensual Medical Intervention Mandate (Bodily Autonomy Primary Reading)").
narrative_ontology:topic_domain(public_health_mandate_authority__bodily_autonomy_primary, "public_health_law/constitutional_rights/bioethics").

domain_priors:requires_active_enforcement(public_health_mandate_authority__bodily_autonomy_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(public_health_mandate_authority__bodily_autonomy_primary, '146d9af9-8f2c-40ec-b932-183f65acc975').
narrative_ontology:cs_kernel_codification('146d9af9-8f2c-40ec-b932-183f65acc975', formalized).
narrative_ontology:cs_authority_grounding('146d9af9-8f2c-40ec-b932-183f65acc975', extraction).
narrative_ontology:cs_interpretation_layer_present('146d9af9-8f2c-40ec-b932-183f65acc975').
narrative_ontology:cs_reading_relation('146d9af9-8f2c-40ec-b932-183f65acc975', public_health_mandate_authority__public_health_primary, forecloses).
narrative_ontology:cs_reading_relation('146d9af9-8f2c-40ec-b932-183f65acc975', public_health_mandate_authority__proportionality_reading, coexists_with).
narrative_ontology:cs_axiom('146d9af9-8f2c-40ec-b932-183f65acc975', foundational, bodily_sovereignty_categorical).
narrative_ontology:cs_axiom_status(bodily_sovereignty_categorical, holdable).
narrative_ontology:cs_axiom_grounding('146d9af9-8f2c-40ec-b932-183f65acc975', bodily_sovereignty_categorical, deontological).
narrative_ontology:cs_axiom('146d9af9-8f2c-40ec-b932-183f65acc975', foundational, informed_consent_non_derogable).
narrative_ontology:cs_axiom_status(informed_consent_non_derogable, holdable).
narrative_ontology:cs_axiom_grounding('146d9af9-8f2c-40ec-b932-183f65acc975', informed_consent_non_derogable, deontological).
narrative_ontology:cs_axiom('146d9af9-8f2c-40ec-b932-183f65acc975', secondary, state_ownership_of_bodies_denied).
narrative_ontology:cs_axiom_status(state_ownership_of_bodies_denied, holdable).
narrative_ontology:cs_axiom_grounding('146d9af9-8f2c-40ec-b932-183f65acc975', state_ownership_of_bodies_denied, deontological).
narrative_ontology:cs_reference_frame('146d9af9-8f2c-40ec-b932-183f65acc975', jacobson_1905_narrow_holding).
narrative_ontology:cs_drift_state('146d9af9-8f2c-40ec-b932-183f65acc975', covid_mandate_expansion_2020_2024, gap(axiom_overriding, severe, false)).
narrative_ontology:cs_created_at('146d9af9-8f2c-40ec-b932-183f65acc975', '').
narrative_ontology:cs_kernel_id(public_health_mandate_authority__bodily_autonomy_primary, public_health_mandate_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(public_health_mandate_authority__bodily_autonomy_primary, public_health_authorities).
narrative_ontology:constraint_beneficiary(public_health_mandate_authority__bodily_autonomy_primary, pharmaceutical_manufacturers).
narrative_ontology:constraint_beneficiary(public_health_mandate_authority__bodily_autonomy_primary, state_legitimacy_apparatus).
narrative_ontology:constraint_victim(public_health_mandate_authority__bodily_autonomy_primary, non_consenting_individuals).
narrative_ontology:constraint_victim(public_health_mandate_authority__bodily_autonomy_primary, bodily_autonomy_advocates).
narrative_ontology:constraint_victim(public_health_mandate_authority__bodily_autonomy_primary, vaccine_injured_parties).
narrative_ontology:constraint_vindicates(public_health_mandate_authority__bodily_autonomy_primary, bodily_sovereignty_categorical).
narrative_ontology:constraint_vindicates(public_health_mandate_authority__bodily_autonomy_primary, informed_consent_absolute).
narrative_ontology:constraint_vindicates(public_health_mandate_authority__bodily_autonomy_primary, state_ownership_of_bodies_denied).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design and enforce mandate policies, define exemptions, control enforcement machinery (school exclusion, employment restrictions, travel limitations). Collect institutional legitimacy and budgetary authority from mandate administration. Face no personal bodily risk from the mandates they impose.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__bodily_autonomy_primary, public_health_authorities, agenda_setter,
    institutional, generational, arbitrage, national).

% Receive guaranteed demand, liability shields (PREP Act, NCVIA), and accelerated approval pathways tied to mandate adoption. Profit directly from the coercive structure without bearing its bodily risks. Influence policy through regulatory capture and lobbying.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__bodily_autonomy_primary, pharmaceutical_manufacturers, beneficiary,
    institutional, biographical, arbitrage, global).

% Uses mandate compliance as a legitimacy ritual — demonstrating state power over the biological body secures the state's claim to ultimate authority. The mandate functions as a sovereignty performance; the health outcome is secondary to the submission it extracts.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__bodily_autonomy_primary, state_legitimacy_apparatus, beneficiary,
    institutional, civilizational, arbitrage, national).

% Face direct bodily intrusion, loss of livelihood, exclusion from public life, and social ostracism for refusing. No meaningful exit: relocation is economically impossible, homeschooling is legally constrained, religious/philosophical exemptions are narrowly constructed or eliminated. Bear 100% of the bodily risk with zero liability recourse.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__bodily_autonomy_primary, non_consenting_individuals, payer,
    powerless, biographical, trapped, local).

% Organize legal challenges, public education, and political resistance. Their professional and personal identities are fused to this fight — exit means abandoning life's work and core self-concept. Face professional retaliation, deplatforming, and institutional exclusion. Extraction is reputational, financial, and existential.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__bodily_autonomy_primary, bodily_autonomy_advocates, payer,
    organized, generational, identity_locked, national).

% Bear the physical costs of the mandate's enforcement (adverse events) with no compensation pathway (CICP denial rate >99%). Medically gaslit by the same authorities that mandated the intervention. No exit from the injury; the constraint that harmed them continues to operate and expand.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__bodily_autonomy_primary, vaccine_injured_parties, payer,
    powerless, biographical, trapped, local).

% Cited as the primary justification for mandates, yet their actual protection needs (clean air, prophylactic antivirals, targeted shielding, non-pharmaceutical interventions) are systematically deprioritized in favor of universal mandates. Their vulnerability is instrumentalized to override others' bodily sovereignty while their specific needs go unmet.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__bodily_autonomy_primary, immunocompromised_individuals, excluded,
    powerless, biographical, constrained, local).

% Hold the sibling reading that mandates are obligations to protect the vulnerable commons. Experience zero coercion from this reading's constraint — they are not forced to vaccinate against their will, nor are they excluded from society. Their advocacy extracts nothing from them; the extraction flows one way.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__bodily_autonomy_primary, public_health_primary_advocates, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The reading posits NO genuine coordination function — the mandate is not a solution to a collective action problem but a unilateral imposition. Any coordination (disease surveillance, voluntary vaccination, targeted protection) is available without the coercive structure.
% TRANSFER_FUNCTION: Moves bodily sovereignty, risk, and liability from the individual to the state-pharma alliance. The individual bears all bodily risk (adverse events, long-term unknowns) and social cost (exclusion, livelihood loss); the authorities and manufacturers collect the compliance revenue, liability protection, and legitimacy rent.
% ABSENT_VOICES: Children subjected to school mandates without consent capacity; future generations who inherit the precedent of state bodily ownership; religious minorities whose objections are legally overruled; the vaccine-injured who are erased from public discourse. They are structurally excluded from the mandate-setting process — their presence would break the unanimity the mandate's legitimacy performs.
% DISAPPEARANCE_RATIONALE: If the mandate vanished overnight, the pharmaceutical revenue guarantee and liability shield would collapse; public health authorities would lose their primary enforcement lever; the state legitimacy ritual would lose its most potent performance. Voluntary vaccination, targeted protection, and early treatment protocols would reorganize the field — the world rearranges because the constraint IS the architecture of extraction.
% FOUNDING_PROBLEM: The 1905 Jacobson v. Massachusetts decision upheld a smallpox vaccine mandate during an active epidemic with a vaccine that prevented transmission and conferred sterilizing immunity. That specific factual matrix (lethal pathogen, sterilizing vaccine, no alternatives) was generalized into a permanent doctrine of state authority over the body — the founding problem was smallpox; the living constraint is all mandates forever.
% FOUNDING_PROBLEM_CORROBORATION: Jacobson's actual holding was narrow (epidemic conditions, $5 fine alternative, not forced vaccination). Legal historians (Gostin, Jacobson himself in later writings) and the Jacobson Court's own language confirm the holding was fact-bound. The expansion to universal, permanent, liability-free mandates for non-sterilizing products is a judicial and legislative construction with no corroborating authority outside the benefiting institutions.
narrative_ontology:disappearance_verdict(public_health_mandate_authority__bodily_autonomy_primary, world_rearranges).
narrative_ontology:founding_problem_status(public_health_mandate_authority__bodily_autonomy_primary, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(public_health_mandate_authority__bodily_autonomy_primary, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(public_health_mandate_authority__bodily_autonomy_primary, 'none', 1).
narrative_ontology:epsilon_provenance(public_health_mandate_authority__bodily_autonomy_primary, 0.82, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(public_health_mandate_authority__bodily_autonomy_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(public_health_mandate_authority__bodily_autonomy_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(public_health_mandate_authority__bodily_autonomy_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.82) reflects the near-total transfer of bodily risk to non-consenting individuals while manufacturers hold liability shields and authorities hold enforcement monopoly. Suppression (0.91) captures the active elimination of alternatives (ivermectin protocols, vitamin D, natural immunity recognition, focused protection) and the censorship of dissenting scientists. Theater ratio (0.15) is low because the coercion is real — but the slight rise to 0.28 in 2020 reflects the performative 'we're all in this together' messaging that masked the mandate's true extraction architecture. Accessibility collapse (0.35) is moderate because alternatives exist conceptually (bodily autonomy is thinkable) but are practically inaccessible under the regime. Resistance (0.78) is high because the constraint meets organized, sustained, identity-locked opposition.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (authorities) experiences the constraint as legitimate coordination; the payer seats (non-consenting individuals, advocates, injured) experience it as pure extraction. The engine computes this divergence from the structural data — the bodily_autonomy_primary reading names the extraction; the public_health_primary reading names the coordination. They are not perspectives on one constraint; they are different constraints from the same kernel.
 *
 * DIRECTIONALITY LOGIC:
 *   Public health authorities, manufacturers, and the state legitimacy apparatus are structural beneficiaries (d ≈ 0.1-0.2) — they collect revenue, liability protection, and sovereignty performance. Non-consenting individuals, autonomy advocates, and vaccine-injured are structural targets (d ≈ 0.9-1.0) — they bear bodily intrusion, livelihood loss, social death, and injury with no recourse. Immunocompromised individuals are excluded from the victim set in THIS reading because the mandate does not actually serve their protection needs — their vulnerability is instrumentalized. Public-health-primary advocates experience zero extractiveness (d ≈ 0.0) because no coercion is imposed on them; they are the sibling reading's constituency.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (smallpox epidemic, sterilizing vaccine, no alternatives) is dead. The constraint persists because it serves the extraction interests of the state-pharma-legitimacy alliance — a classic mandatrophy. The mandate has outlived its epidemiological justification but not its extraction function. The Jacobson precedent is the cover story; the NCVIA/PREP Act liability shield is the extraction infrastructure; the COVID expansion revealed the architecture's true scope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is the public_health_mandate_authority kernel a single persisting commitment with multiple readings, or are these structurally distinct constraints that only share a label?',
    'Trace the institutional genealogy: does the same legal/administrative structure (Jacobson → NCVIA → PREP Act → COVID mandates) instantiate all three readings, or do they operate through different mechanisms?',
    'If single kernel, the readings are in genuine structural tension — the framework''s committer-axis machinery applies. If distinct constraints, the ε-invariance principle requires separate stories with no kernel link.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether the three declared readings share a kernel or are distinct constraints').

omega_variable(
    foreclosure_vs_coexistence,
    'Does the bodily_autonomy_primary reading genuinely foreclose the public_health_primary reading within a single legal framework, or do they coexist as competing but structurally compatible positions?',
    'Examine constitutional jurisprudence: can a single court simultaneously hold that bodily sovereignty is categorical AND that the state has a compelling interest in mandating vaccination? Jacobson''s balancing test suggests coexistence; the bodily_autonomy_primary reading''s absolutism suggests foreclosure.',
    'If foreclosure holds, the engine''s cs_axiom_contradiction will compute terminal state divergence. If coexistence, the kernel remains a live dispute with no reading eliminated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(foreclosure_vs_coexistence, conceptual, 'Whether absolutist bodily sovereignty logically eliminates collective-protection mandates in one framework').

omega_variable(
    immunocompromised_instrumentalization,
    'Are immunocompromised individuals genuinely served by universal mandates, or is their vulnerability instrumentalized to expand the mandate''s extraction surface?',
    'Compare mandate policies to actual immunocompromised protection needs: clean air mandates, prophylactic antivirals, targeted shielding programs, non-pharmaceutical interventions. If these are absent/deprioritized while universal mandates expand, instrumentalization is evidenced.',
    'If instrumentalized, the public_health_primary reading''s claimed beneficiary (the vulnerable) is a cover story — the constraint is snare, not tangled_rope. This reading''s victim/excluded assignment is validated.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(immunocompromised_instrumentalization, empirical, 'Whether the mandate''s stated beneficiaries actually benefit or are instrumentally used').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (legal barriers, economic coercion, censorship) or internalized (moral internalization of ''anti-vaxxer'' stigma, identity fusion with compliance)?',
    'Post-mandate suppression trajectory: if suppression persists after legal mandates lift (via social ostracism, professional blacklisting, self-censorship), reclassify as partially internalized.',
    'If internalized, effective suppression is higher than structural measure suggests — the target carries the suppression after formal exit. Affects piton/theater analysis for identity_locked agents.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for bodily autonomy advocates').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(public_health_mandate_authority__bodily_autonomy_primary, 1905, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(publ_tr_t1905, public_health_mandate_authority__bodily_autonomy_primary, theater_ratio, 1905, 0.05).
narrative_ontology:measurement(publ_tr_t1960, public_health_mandate_authority__bodily_autonomy_primary, theater_ratio, 1960, 0.08).
narrative_ontology:measurement(publ_tr_t1986, public_health_mandate_authority__bodily_autonomy_primary, theater_ratio, 1986, 0.12).
narrative_ontology:measurement(publ_tr_t2005, public_health_mandate_authority__bodily_autonomy_primary, theater_ratio, 2005, 0.18).
narrative_ontology:measurement(publ_tr_t2015, public_health_mandate_authority__bodily_autonomy_primary, theater_ratio, 2015, 0.22).
narrative_ontology:measurement(publ_tr_t2020, public_health_mandate_authority__bodily_autonomy_primary, theater_ratio, 2020, 0.28).
narrative_ontology:measurement(publ_tr_t2024, public_health_mandate_authority__bodily_autonomy_primary, theater_ratio, 2024, 0.15).

% Extraction over time
narrative_ontology:measurement(publ_be_t1905, public_health_mandate_authority__bodily_autonomy_primary, base_extractiveness, 1905, 0.12).
narrative_ontology:measurement(publ_be_t1960, public_health_mandate_authority__bodily_autonomy_primary, base_extractiveness, 1960, 0.18).
narrative_ontology:measurement(publ_be_t1986, public_health_mandate_authority__bodily_autonomy_primary, base_extractiveness, 1986, 0.35).
narrative_ontology:measurement(publ_be_t2005, public_health_mandate_authority__bodily_autonomy_primary, base_extractiveness, 2005, 0.48).
narrative_ontology:measurement(publ_be_t2015, public_health_mandate_authority__bodily_autonomy_primary, base_extractiveness, 2015, 0.56).
narrative_ontology:measurement(publ_be_t2020, public_health_mandate_authority__bodily_autonomy_primary, base_extractiveness, 2020, 0.72).
narrative_ontology:measurement(publ_be_t2024, public_health_mandate_authority__bodily_autonomy_primary, base_extractiveness, 2024, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(publ_su_t1905, public_health_mandate_authority__bodily_autonomy_primary, suppression_requirement, 1905, 0.25).
narrative_ontology:measurement(publ_su_t1960, public_health_mandate_authority__bodily_autonomy_primary, suppression_requirement, 1960, 0.32).
narrative_ontology:measurement(publ_su_t1986, public_health_mandate_authority__bodily_autonomy_primary, suppression_requirement, 1986, 0.58).
narrative_ontology:measurement(publ_su_t2005, public_health_mandate_authority__bodily_autonomy_primary, suppression_requirement, 2005, 0.68).
narrative_ontology:measurement(publ_su_t2015, public_health_mandate_authority__bodily_autonomy_primary, suppression_requirement, 2015, 0.75).
narrative_ontology:measurement(publ_su_t2020, public_health_mandate_authority__bodily_autonomy_primary, suppression_requirement, 2020, 0.89).
narrative_ontology:measurement(publ_su_t2024, public_health_mandate_authority__bodily_autonomy_primary, suppression_requirement, 2024, 0.91).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(public_health_mandate_authority__bodily_autonomy_primary, enforcement_mechanism).
narrative_ontology:affects_constraint(public_health_mandate_authority__bodily_autonomy_primary, public_health_mandate_authority__public_health_primary).
narrative_ontology:affects_constraint(public_health_mandate_authority__bodily_autonomy_primary, public_health_mandate_authority__proportionality_reading).
narrative_ontology:affects_constraint(public_health_mandate_authority__bodily_autonomy_primary, vaccine_liability_shield_ncvia).
narrative_ontology:affects_constraint(public_health_mandate_authority__bodily_autonomy_primary, emergency_use_authorization_architecture).
narrative_ontology:affects_constraint(public_health_mandate_authority__bodily_autonomy_primary, school_exclusion_mandate_regime).

% DUAL FORMULATION NOTE:
% This constraint is the bodily_autonomy_primary reading of the public_health_mandate_authority kernel. It decomposes the colloquial 'vaccine mandate' into three structurally distinct constraints with different ε values, victim sets, and extraction architectures. The public_health_primary reading (ε ≈ 0.15) claims coordination; the proportionality_reading (ε ≈ 0.45) claims balanced coordination; this reading (ε ≈ 0.82) identifies pure extraction. The kernel label 'public health mandate authority' conflates them.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(public_health_mandate_authority__bodily_autonomy_primary, institutional, 0.05).
constraint_indexing:directionality_override(public_health_mandate_authority__bodily_autonomy_primary, powerless, 0.98).
constraint_indexing:directionality_override(public_health_mandate_authority__bodily_autonomy_primary, organized, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
