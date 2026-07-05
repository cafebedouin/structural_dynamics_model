% ============================================================================
% CONSTRAINT STORY: federation_membership_obligations__integration_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_federation_membership_obligations__integration_primary, []).

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
 *   constraint_id: federation_membership_obligations__integration_primary
 *   human_readable: EU Free Movement as Constitutive of Citizenship (Integration-Primary Reading)
 *   domain: political_economy/federalism/migration_policy/welfare_state_theory
 *
 * SUMMARY:
 *   This story instantiates the integration-primary reading of the federation
 *   membership obligations kernel: free movement is treated as constitutive
 *   of EU citizenship itself, not a conditional policy tradeoff, so
 *   member-state welfare boundaries must yield when they conflict with
 *   mobility rights. This is a genuine coordination structure — a
 *   continent-wide labor market with enforceable, person-portable rights —
 *   layered with asymmetric extraction: displaced local labor and existing
 *   welfare claimants in receiving states bear adjustment costs they did not
 *   choose and cannot contest through the same institutional channels that
 *   expanded the right. The ECJ's case-law-driven expansion (Martinez Sala
 *   onward) requires continuous active enforcement against member-state
 *   resistance, which is exactly the tangled-rope signature: real
 *   coordination function, real victims, real enforcement machinery holding
 *   it together.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federation_membership_obligations__integration_primary, 0.52).
domain_priors:suppression_score(federation_membership_obligations__integration_primary, 0.61).
domain_priors:theater_ratio(federation_membership_obligations__integration_primary, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federation_membership_obligations__integration_primary, extractiveness, 0.52).
narrative_ontology:constraint_metric(federation_membership_obligations__integration_primary, suppression_requirement, 0.61).
narrative_ontology:constraint_metric(federation_membership_obligations__integration_primary, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(federation_membership_obligations__integration_primary, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(federation_membership_obligations__integration_primary, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federation_membership_obligations__integration_primary, tangled_rope).
narrative_ontology:human_readable(federation_membership_obligations__integration_primary, "EU Free Movement as Constitutive of Citizenship (Integration-Primary Reading)").
narrative_ontology:topic_domain(federation_membership_obligations__integration_primary, "political_economy/federalism/migration_policy/welfare_state_theory").

domain_priors:requires_active_enforcement(federation_membership_obligations__integration_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federation_membership_obligations__integration_primary, '29cd8150-b5a9-48ee-92d7-c6007a842cad').
narrative_ontology:cs_kernel_codification('29cd8150-b5a9-48ee-92d7-c6007a842cad', fixed_text).
narrative_ontology:cs_authority_grounding('29cd8150-b5a9-48ee-92d7-c6007a842cad', lineage).
narrative_ontology:cs_interpretation_layer_present('29cd8150-b5a9-48ee-92d7-c6007a842cad').
narrative_ontology:cs_reading_relation('29cd8150-b5a9-48ee-92d7-c6007a842cad', federation_membership_obligations__member_sovereignty_primary, forecloses).
narrative_ontology:cs_reading_relation('29cd8150-b5a9-48ee-92d7-c6007a842cad', federation_membership_obligations__selective_solidarity, influences).
narrative_ontology:cs_axiom('29cd8150-b5a9-48ee-92d7-c6007a842cad', foundational, citizenship_entails_unconditional_equal_treatment).
narrative_ontology:cs_axiom_status(citizenship_entails_unconditional_equal_treatment, holdable).
narrative_ontology:cs_axiom_grounding('29cd8150-b5a9-48ee-92d7-c6007a842cad', citizenship_entails_unconditional_equal_treatment, conventional).
narrative_ontology:cs_axiom('29cd8150-b5a9-48ee-92d7-c6007a842cad', foundational, free_movement_is_constitutive_not_instrumental).
narrative_ontology:cs_axiom_status(free_movement_is_constitutive_not_instrumental, holdable).
narrative_ontology:cs_axiom_grounding('29cd8150-b5a9-48ee-92d7-c6007a842cad', free_movement_is_constitutive_not_instrumental, conventional).
narrative_ontology:cs_reference_frame('29cd8150-b5a9-48ee-92d7-c6007a842cad', treaty_of_rome_economic_mobility_baseline).
narrative_ontology:cs_drift_state('29cd8150-b5a9-48ee-92d7-c6007a842cad', post_maastricht_citizenship_jurisprudence, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('29cd8150-b5a9-48ee-92d7-c6007a842cad', '').
narrative_ontology:cs_kernel_id(federation_membership_obligations__integration_primary, federation_membership_obligations).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federation_membership_obligations__integration_primary, mobile_eu_workers).
narrative_ontology:constraint_beneficiary(federation_membership_obligations__integration_primary, single_market_employers).
narrative_ontology:constraint_beneficiary(federation_membership_obligations__integration_primary, eu_commission).
narrative_ontology:constraint_beneficiary(federation_membership_obligations__integration_primary, european_court_of_justice).
narrative_ontology:constraint_victim(federation_membership_obligations__integration_primary, displaced_local_labor).
narrative_ontology:constraint_victim(federation_membership_obligations__integration_primary, receiving_state_welfare_systems).
narrative_ontology:constraint_victim(federation_membership_obligations__integration_primary, long_term_unemployed_natives).
narrative_ontology:constraint_vindicates(federation_membership_obligations__integration_primary, eu_citizenship_supranational_doctrine).
narrative_ontology:constraint_vindicates(federation_membership_obligations__integration_primary, internal_market_indivisibility_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Move from lower-wage member states to higher-wage ones under treaty-guaranteed free movement, gaining access to host-state welfare benefits, healthcare, and labor protections after qualifying periods. Their exit option is the entire point of the arrangement — they can relocate again if a host state tightens access, which is precisely what disciplines member states against restricting benefits.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__integration_primary, mobile_eu_workers, beneficiary,
    moderate, biographical, mobile, continental).

% Draw on a continent-wide labor pool without needing work-permit sponsorship, hiring mobile workers at wages below what would clear a closed local market. They can also relocate operations across borders to access whichever labor pool is cheapest, which is itself enabled by the same free-movement architecture.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__integration_primary, single_market_employers, beneficiary,
    powerful, generational, arbitrage, continental).

% Interprets treaty free-movement provisions expansively through case law (Martinez Sala, Grzelczyk, Dano and their successors), progressively defining EU citizenship as carrying an entitlement to equal treatment in welfare access. Each ruling forecloses member-state discretion a bit further and is binding on all national courts; the Court has no removal mechanism accountable to member electorates and cannot be overruled except by unanimous treaty change.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__integration_primary, european_court_of_justice, agenda_setter,
    institutional, civilizational, analytical, continental).

% Initiates infringement proceedings against member states that restrict mobile-worker welfare access, treating free movement as an unconditional pillar of integration rather than a negotiable policy tradeoff. Its institutional legitimacy and mandate depend on integration deepening, giving it a direct stake in the expansive reading it enforces.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__integration_primary, eu_commission, agenda_setter,
    institutional, civilizational, analytical, continental).
narrative_ontology:stakeholder_secondary_role(federation_membership_obligations__integration_primary, eu_commission, beneficiary).

% Compete for low- and mid-skill jobs against an enlarged labor supply and see local wage growth suppressed in sectors with high mobile-worker concentration. Lack the capital, language advantage, or professional credentials to relocate themselves, and have no legal channel to seek local labor-market protection since national quotas or preference rules would themselves violate free-movement law.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__integration_primary, displaced_local_labor, payer,
    powerless, biographical, trapped, national).

% Fund welfare, housing, and public-service capacity calibrated to a national contributor base but must extend it to newly arrived mobile workers under the equal-treatment principle. They can lobby for treaty change or fight individual cases at the ECJ but cannot unilaterally exclude EU nationals without breaching membership obligations; noncompliance risks infringement proceedings and fines.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__integration_primary, receiving_state_welfare_systems, payer,
    institutional, generational, constrained, national).

% Draw on the same finite pool of local social housing, activation programs, and unemployment services now shared with a larger eligible population, lengthening queues and tightening means-testing thresholds nationally applied. Have no standing before the ECJ and no organized voice in Brussels; their claims are processed entirely through domestic welfare administration, which is itself bound by supranational equal-treatment rules.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__integration_primary, long_term_unemployed_natives, payer,
    powerless, biographical, trapped, national).

% Lose working-age taxpayers and see domestic labor shortages and skill drain in health care, construction, and agriculture as citizens emigrate for higher wages, but have no forum to raise 'brain drain' as a free-movement cost since the treaty framework treats emigration as an individual right, not a state-level externality to be negotiated.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__integration_primary, sending_state_governments, excluded,
    moderate, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(federation_membership_obligations__integration_primary, diffuse).
narrative_ontology:fixing_cost_class(federation_membership_obligations__integration_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables a genuinely single labor market across member states: workers can move to where their skills are valued, employers can staff across a continent-wide pool, and EU citizenship acquires substantive content beyond passport symbolism by attaching enforceable rights that travel with the person.
% TRANSFER_FUNCTION: Moves labor-market opportunity and welfare-system capacity from receiving-state incumbents (displaced local labor, existing benefit claimants) to mobile workers and the employers who hire them; moves human capital and tax base from sending states to receiving states.
% ABSENT_VOICES: Displaced local labor and long-term unemployed natives have no standing before the ECJ and no organized lobby in the treaty-revision process; sending-state governments experiencing brain drain have no forum to characterize emigration as an externality requiring compensation or coordination, since free movement is framed exclusively as an individual right.
% DISAPPEARANCE_RATIONALE: If free movement's welfare-constitutive status were overnight replaced with member-state discretion, receiving states would immediately reintroduce residence or contribution tests, mobile-worker relocation flows would reprice around welfare access rather than wage differentials alone, and the ECJ's four-decade case-law architecture on equal treatment would need wholesale reconstruction — the single market's labor pillar and EU citizenship's substantive content would both change materially.
% FOUNDING_PROBLEM: The original problem was economic: a common market required labor mobility to allocate workers efficiently across borders, and early free-movement provisions (1957 Treaty of Rome, Regulation 1612/68) were framed narrowly around workers' economic function. The subsequent extension to non-economic welfare entitlement was a later, contested addition layered onto the economic-mobility rationale via citizenship jurisprudence beginning in the late 1990s.
% FOUNDING_PROBLEM_CORROBORATION: The ECJ and Commission attest the extension to welfare-constitutive citizenship is the logical maturation of the founding economic-integration problem. Independent public-finance economists and several national constitutional courts (notably German Federal Constitutional Court commentary on Solange-adjacent welfare cases, and UK pre-Brexit domestic tribunals) have attested from outside the integrationist institutions that the welfare-entitlement extension addresses a different problem than the original labor-mobility rationale and was not clearly mandated by the founding treaties.
narrative_ontology:disappearance_verdict(federation_membership_obligations__integration_primary, world_rearranges).
narrative_ontology:founding_problem_status(federation_membership_obligations__integration_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(federation_membership_obligations__integration_primary, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(federation_membership_obligations__integration_primary, 'none', 1).
narrative_ontology:epsilon_provenance(federation_membership_obligations__integration_primary, 0.52, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(federation_membership_obligations__integration_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(federation_membership_obligations__integration_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(federation_membership_obligations__integration_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-high (0.52) and has risen steadily since Maastricht-era citizenship jurisprudence began (1993 baseline 0.28) as case law progressively narrowed member-state discretion to condition benefits on residence or contribution history. Suppression (0.61) reflects the infringement-proceeding machinery and the binding, non-negotiable character of ECJ rulings on national welfare administration — this is a raw structural property of how the enforcement operates, not scaled by scope or power. Theater ratio stays low-moderate (0.22) because the coordination function (single labor market, portable citizenship rights) is substantively real, not primarily performative; the drift is in extraction accumulating on top of a genuine coordination core, not in the coordination hollowing out into theater.
 *
 * DIRECTIONALITY LOGIC:
 *   Mobile workers and single-market employers sit near the beneficiary end: workers gain welfare access and relocation optionality, employers gain a larger, undifferentiated labor pool, and both retain meaningful exit (workers can re-relocate; employers can relocate operations). The ECJ and Commission are structural agenda-setters whose institutional mandate is itself vindicated by the expansive reading, giving them a stake in its persistence beyond neutral administration. Displaced local labor and long-term unemployed natives sit at the full-target end: trapped exit options, no standing before the adjudicating body, and costs delivered through the same equal-treatment machinery that delivers mobile workers' benefits. Receiving-state welfare systems are institutional payers with constrained exit — they can litigate or lobby for treaty change but cannot unilaterally restrict access without breaching membership obligations.
 *
 * MANDATROPHY ANALYSIS:
 *   The coordination function (single labor market, substantive EU citizenship) remains genuinely live — it is not vestigial. What prevents this from being classified as pure extraction is that free movement still solves a real allocative problem and delivers real benefits to a large, non-trivial beneficiary class. What prevents it from being classified as pure coordination (rope) is the presence of identifiable victims (displaced local labor, long-term unemployed natives, receiving-state fiscal capacity) who bear costs through the same structure and cannot exit or contest it — this is precisely why tangled_rope, not rope, is the structurally correct claim for this reading.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest_federation_membership,
    'Is the integration-primary reading of EU free movement (welfare boundaries yield unconditionally to mobility rights) the structurally correct reading of the founding treaties, or is it a jurisprudential extension beyond what member states consented to at accession?',
    'Comparative analysis of treaty text at each accession point against subsequent ECJ case law; examination of whether member states'' negotiating positions and public referenda debates anticipated welfare-constitutive citizenship or only economic labor mobility. Cross-reference against the sibling readings member_sovereignty_primary and selective_solidarity, which would each predict different treaty-text emphases and different case-law trajectories.',
    'If the integration-primary reading is jurisprudential overreach beyond original consent, the classification shifts toward snare (extraction dressed as constitutive right, imposed by an unaccountable court); if it is the correct reading of an evolving ''ever closer union'' commitment that member states did consent to, tangled_rope remains apt — real coordination function, real but legitimately-incurred costs.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest_federation_membership, conceptual, 'Whether integration-primary is the treaty-faithful reading or a court-driven extension beyond original member-state consent.').

omega_variable(
    sibling_reading_structural_delta,
    'How would the classification change under the member_sovereignty_primary or selective_solidarity readings of the same kernel?',
    'Generate and compare the sibling constraint stories directly: member_sovereignty_primary would show a smaller victim set (displaced local labor protected by conditional access) but a correspondingly smaller beneficiary set among mobile workers; selective_solidarity would show contribution-tiered beneficiaries, reducing but not eliminating the tangled_rope structure.',
    'Confirms that ε and classification differ meaningfully across readings of the same kernel — validates the decomposition into three separate constraint stories rather than one story with an averaged or hedged ε.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_structural_delta, conceptual, 'Cross-reading comparison confirming ε-invariance requires separate stories for each kernel reading.').

omega_variable(
    displaced_labor_coalition_potential,
    'Could displaced local labor and long-term unemployed natives across multiple receiving states form a coalition to press for treaty-level renegotiation, given they are individually powerless but numerous across the EU?',
    'Track whether any pan-European labor organizations or populist political coalitions have successfully aggregated this constituency''s grievances into treaty-amendment pressure (e.g., post-2016 debates on welfare tourism, benefit-access reforms negotiated in the UK''s pre-Brexit renegotiation).',
    'If coalition formation is structurally blocked (no EU-wide labor organizing capacity, national labor movements fragmented by the same mobility dynamics), the powerless-victim classification is durable; if coalition capacity exists and has produced concrete treaty concessions, the victim class has more agency than the current stakeholder data reflects.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(displaced_labor_coalition_potential, empirical, 'Whether displaced local labor''s dispersed powerlessness admits coalition-based correction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership_obligations__integration_primary, 1993, 2023).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fede_tr_t1993, federation_membership_obligations__integration_primary, theater_ratio, 1993, 0.1).
narrative_ontology:measurement(fede_tr_t1999, federation_membership_obligations__integration_primary, theater_ratio, 1999, 0.12).
narrative_ontology:measurement(fede_tr_t2005, federation_membership_obligations__integration_primary, theater_ratio, 2005, 0.15).
narrative_ontology:measurement(fede_tr_t2011, federation_membership_obligations__integration_primary, theater_ratio, 2011, 0.18).
narrative_ontology:measurement(fede_tr_t2017, federation_membership_obligations__integration_primary, theater_ratio, 2017, 0.2).
narrative_ontology:measurement(fede_tr_t2023, federation_membership_obligations__integration_primary, theater_ratio, 2023, 0.22).

% Extraction over time
narrative_ontology:measurement(fede_be_t1993, federation_membership_obligations__integration_primary, base_extractiveness, 1993, 0.28).
narrative_ontology:measurement(fede_be_t1999, federation_membership_obligations__integration_primary, base_extractiveness, 1999, 0.34).
narrative_ontology:measurement(fede_be_t2005, federation_membership_obligations__integration_primary, base_extractiveness, 2005, 0.41).
narrative_ontology:measurement(fede_be_t2011, federation_membership_obligations__integration_primary, base_extractiveness, 2011, 0.46).
narrative_ontology:measurement(fede_be_t2017, federation_membership_obligations__integration_primary, base_extractiveness, 2017, 0.5).
narrative_ontology:measurement(fede_be_t2023, federation_membership_obligations__integration_primary, base_extractiveness, 2023, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(fede_su_t1993, federation_membership_obligations__integration_primary, suppression_requirement, 1993, 0.35).
narrative_ontology:measurement(fede_su_t1999, federation_membership_obligations__integration_primary, suppression_requirement, 1999, 0.42).
narrative_ontology:measurement(fede_su_t2005, federation_membership_obligations__integration_primary, suppression_requirement, 2005, 0.49).
narrative_ontology:measurement(fede_su_t2011, federation_membership_obligations__integration_primary, suppression_requirement, 2011, 0.54).
narrative_ontology:measurement(fede_su_t2017, federation_membership_obligations__integration_primary, suppression_requirement, 2017, 0.58).
narrative_ontology:measurement(fede_su_t2023, federation_membership_obligations__integration_primary, suppression_requirement, 2023, 0.61).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(federation_membership_obligations__integration_primary, resource_allocation).
narrative_ontology:boltzmann_floor_override(federation_membership_obligations__integration_primary, 0.12).
narrative_ontology:affects_constraint(federation_membership_obligations__integration_primary, federation_membership_obligations__member_sovereignty_primary).
narrative_ontology:affects_constraint(federation_membership_obligations__integration_primary, federation_membership_obligations__selective_solidarity).

% DUAL FORMULATION NOTE:
% This story is one of three constraint files decomposing the single natural-language kernel 'federation membership obligations under free movement.' Each reading (integration_primary, member_sovereignty_primary, selective_solidarity) has its own ε, its own beneficiary/victim structure, and its own classification, per the ε-invariance principle — they are not the same constraint measured three ways but three structurally distinct constraints that share a contested kernel. Linked via affects_constraints because ECJ rulings under this reading directly constrain the policy space available to the sibling readings' proponents (e.g., a ruling expanding equal-treatment doctrine forecloses some sovereignty-primary policy options and narrows the contribution-tiering options available to selective_solidarity).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
