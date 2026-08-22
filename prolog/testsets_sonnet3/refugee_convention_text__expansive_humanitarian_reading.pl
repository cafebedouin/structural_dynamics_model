% ============================================================================
% CONSTRAINT STORY: refugee_convention_text__expansive_humanitarian_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_refugee_convention_text__expansive_humanitarian_reading, []).

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
 *   constraint_id: refugee_convention_text__expansive_humanitarian_reading
 *   human_readable: Expansive Humanitarian Reading of the Refugee Convention
 *   domain: international_law/migration_governance/human_rights
 *
 * SUMMARY:
 *   This story authors the expansive humanitarian reading of the 1951 Refugee
 *   Convention / 1967 Protocol as its own constraint, distinct from the
 *   restrictive sovereignty reading and the procedural integrity reading
 *   (each authored separately). Under this reading, the Convention functions
 *   as an unbendable humanitarian mandate: 'well-founded fear' extends to
 *   generalized violence and non-state persecution, and 'particular social
 *   group' extends to gender, sexual orientation/gender identity, and clan
 *   membership. This reading treats interdiction and offshore processing as
 *   refoulement violations and imposes a duty to assess claims on their
 *   substantive merits rather than filtering by procedural or categorical
 *   shortcuts. The referent of extractiveness here is the standing
 *   arrangement AS THIS READING SEES IT currently operating in international
 *   jurisprudence and UNHCR guidance — not the reading's own endorsed ideal,
 *   and not the sibling readings' accounts.
 *
 * KEY AGENTS:
 *   - asylum_seekers_fleeing_generalized_violence: primary beneficiary (powerless/trapped) — gains recognition and non-refoulement protection unavailable under narrower readings
 *   - gender_persecution_claimants, lgbtq_asylum_seekers, clan_based_persecution_claimants: primary beneficiaries (powerless/trapped) — gain cognizable social-group status
 *   - unhcr_protection_mandate: agenda-setter/beneficiary (institutional/analytical) — drives and legitimizes the expansive interpretation, its mandate scope grows with it
 *   - destination_state_border_agencies, frontline_reception_states, domestic_taxpayers_in_host_states: payers (institutional/moderate/powerless) — absorb administrative, fiscal, and political costs of expanded eligibility
 *   - restrictive_sovereignty_states: excluded (institutional) — hold the competing interpretive position but are increasingly out-argued in the interpretive community this reading dominates
 *   - international_refugee_law_scholars: analytical observer
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(refugee_convention_text__expansive_humanitarian_reading, 0.42).
domain_priors:suppression_score(refugee_convention_text__expansive_humanitarian_reading, 0.55).
domain_priors:theater_ratio(refugee_convention_text__expansive_humanitarian_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(refugee_convention_text__expansive_humanitarian_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(refugee_convention_text__expansive_humanitarian_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(refugee_convention_text__expansive_humanitarian_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(refugee_convention_text__expansive_humanitarian_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(refugee_convention_text__expansive_humanitarian_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(refugee_convention_text__expansive_humanitarian_reading, tangled_rope).
narrative_ontology:human_readable(refugee_convention_text__expansive_humanitarian_reading, "Expansive Humanitarian Reading of the Refugee Convention").
narrative_ontology:topic_domain(refugee_convention_text__expansive_humanitarian_reading, "international_law/migration_governance/human_rights").

domain_priors:requires_active_enforcement(refugee_convention_text__expansive_humanitarian_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(refugee_convention_text__expansive_humanitarian_reading, 'e78b7287-c64d-498b-901a-049c6a6cb9c2').
narrative_ontology:cs_kernel_codification('e78b7287-c64d-498b-901a-049c6a6cb9c2', fixed_text).
narrative_ontology:cs_authority_grounding('e78b7287-c64d-498b-901a-049c6a6cb9c2', practice).
narrative_ontology:cs_interpretation_layer_present('e78b7287-c64d-498b-901a-049c6a6cb9c2').
narrative_ontology:cs_reading_relation('e78b7287-c64d-498b-901a-049c6a6cb9c2', refugee_convention_text__restrictive_sovereignty_reading, forecloses).
narrative_ontology:cs_reading_relation('e78b7287-c64d-498b-901a-049c6a6cb9c2', refugee_convention_text__procedural_integrity_reading, influences).
narrative_ontology:cs_axiom('e78b7287-c64d-498b-901a-049c6a6cb9c2', foundational, non_refoulement_admits_no_derogation_for_administrative_convenience).
narrative_ontology:cs_axiom_status(non_refoulement_admits_no_derogation_for_administrative_convenience, holdable).
narrative_ontology:cs_axiom_grounding('e78b7287-c64d-498b-901a-049c6a6cb9c2', non_refoulement_admits_no_derogation_for_administrative_convenience, deontological).
narrative_ontology:cs_axiom('e78b7287-c64d-498b-901a-049c6a6cb9c2', foundational, persecution_agency_is_immaterial_to_protection_need).
narrative_ontology:cs_axiom_status(persecution_agency_is_immaterial_to_protection_need, holdable).
narrative_ontology:cs_axiom_grounding('e78b7287-c64d-498b-901a-049c6a6cb9c2', persecution_agency_is_immaterial_to_protection_need, empirically_contingent).
narrative_ontology:cs_reference_frame('e78b7287-c64d-498b-901a-049c6a6cb9c2', id_1951_bounded_crisis_text).
narrative_ontology:cs_drift_state('e78b7287-c64d-498b-901a-049c6a6cb9c2', contemporary_global_displacement_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('e78b7287-c64d-498b-901a-049c6a6cb9c2', '').
narrative_ontology:cs_kernel_id(refugee_convention_text__expansive_humanitarian_reading, refugee_convention_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(refugee_convention_text__expansive_humanitarian_reading, asylum_seekers_fleeing_generalized_violence).
narrative_ontology:constraint_beneficiary(refugee_convention_text__expansive_humanitarian_reading, gender_persecution_claimants).
narrative_ontology:constraint_beneficiary(refugee_convention_text__expansive_humanitarian_reading, lgbtq_asylum_seekers).
narrative_ontology:constraint_beneficiary(refugee_convention_text__expansive_humanitarian_reading, clan_based_persecution_claimants).
narrative_ontology:constraint_beneficiary(refugee_convention_text__expansive_humanitarian_reading, unhcr_protection_mandate).
narrative_ontology:constraint_victim(refugee_convention_text__expansive_humanitarian_reading, destination_state_border_agencies).
narrative_ontology:constraint_victim(refugee_convention_text__expansive_humanitarian_reading, frontline_reception_states).
narrative_ontology:constraint_victim(refugee_convention_text__expansive_humanitarian_reading, domestic_taxpayers_in_host_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Flee conditions of widespread armed conflict, gang control, or societal breakdown that do not fit a narrow individualized-persecution template. Under this reading, they qualify for protection because the Convention's 'well-founded fear' language is read to reach generalized danger from non-state actors, not only targeted state persecution. Without this reading, most would be returned as failing to show individualized risk.
narrative_ontology:constraint_stakeholder(refugee_convention_text__expansive_humanitarian_reading, asylum_seekers_fleeing_generalized_violence, beneficiary,
    powerless, biographical, trapped, global).

% Face domestic violence, forced marriage, FGM, or honor-based violence often tolerated or unaddressed by their state of origin. This reading recognizes 'women in [country]' or similarly gendered formulations as a cognizable particular social group, opening protection routes that a narrower textual reading forecloses.
narrative_ontology:constraint_stakeholder(refugee_convention_text__expansive_humanitarian_reading, gender_persecution_claimants, beneficiary,
    powerless, biographical, trapped, global).

% Face criminalization, mob violence, or family/community persecution on the basis of sexual orientation or gender identity, frequently without state protection or with active state complicity. This reading treats sexual orientation and gender identity as an immutable or fundamental characteristic satisfying 'particular social group' without requiring the claimant to demonstrate additional social visibility barriers some tribunals impose.
narrative_ontology:constraint_stakeholder(refugee_convention_text__expansive_humanitarian_reading, lgbtq_asylum_seekers, beneficiary,
    powerless, biographical, trapped, global).

% Face persecution rooted in clan, caste, or kinship-network identity in contexts of state collapse (e.g., minority clans in fragmented states). This reading extends 'particular social group' to clan membership even where the state itself is not the persecuting agent.
narrative_ontology:constraint_stakeholder(refugee_convention_text__expansive_humanitarian_reading, clan_based_persecution_claimants, beneficiary,
    powerless, biographical, trapped, global).

% Issues guidelines, intervenes in litigation, and advocates for expansive interpretation as fulfilling the Convention's humanitarian object and purpose. Its institutional legitimacy and mandate scope are enlarged by broad readings being adopted; it has no exit from advocating this position since it is constituted around exactly this protective function.
narrative_ontology:constraint_stakeholder(refugee_convention_text__expansive_humanitarian_reading, unhcr_protection_mandate, agenda_setter,
    institutional, civilizational, analytical, global).
narrative_ontology:stakeholder_secondary_role(refugee_convention_text__expansive_humanitarian_reading, unhcr_protection_mandate, beneficiary).

% Must substantively assess a much larger and more heterogeneous pool of claims, including diffuse-harm and non-state-actor claims that are harder to verify and adjudicate quickly. Bear the administrative, fiscal, and political cost of expanded eligibility criteria they did not choose and cannot unilaterally narrow while remaining Convention-compliant. Interdiction and offshore processing practices are treated as refoulement violations under this reading, foreclosing tools these agencies use to manage volume.
narrative_ontology:constraint_stakeholder(refugee_convention_text__expansive_humanitarian_reading, destination_state_border_agencies, payer,
    institutional, biographical, constrained, national).

% States geographically proximate to conflict and instability absorb disproportionate claim volume under an expansive reading, since generalized-violence and non-state-actor claims scale with regional instability rather than being filtered to targeted individual cases. They lack the wealth of distant destination states to build capacity at the same pace protection obligations expand.
narrative_ontology:constraint_stakeholder(refugee_convention_text__expansive_humanitarian_reading, frontline_reception_states, payer,
    moderate, generational, constrained, regional).

% Fund expanded reception, adjudication, and integration infrastructure through general taxation and public services, without having been party to the interpretive choice that expanded the mandate's scope. Their exit is nominally the ballot box, but treaty interpretation sits largely outside domestic electoral control.
narrative_ontology:constraint_stakeholder(refugee_convention_text__expansive_humanitarian_reading, domestic_taxpayers_in_host_states, payer,
    powerless, biographical, trapped, national).

% States that prefer the minimum-floor sovereignty reading are structurally out-argued in international fora and appellate jurisprudence increasingly citing UNHCR guidance and expansive comparative case law; their preferred textual reading is treated as retrogressive rather than a live alternative within the same interpretive community.
narrative_ontology:constraint_stakeholder(refugee_convention_text__expansive_humanitarian_reading, restrictive_sovereignty_states, excluded,
    institutional, generational, constrained, national).

% Study, catalogue, and often actively promote the doctrinal drift toward expansive readings across jurisdictions, treating the trend as either principled humanitarian progress or as an interpretive overreach beyond the treaty's negotiated text, depending on their own commitments.
narrative_ontology:constraint_stakeholder(refugee_convention_text__expansive_humanitarian_reading, international_refugee_law_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(refugee_convention_text__expansive_humanitarian_reading, diffuse).
narrative_ontology:fixing_cost_class(refugee_convention_text__expansive_humanitarian_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a common international floor for identifying who counts as a refugee, so that persons facing serious harm are not returned to face it, and so that no single state must resolve, alone, hard questions about which forms of harm and identity warrant protection.
% TRANSFER_FUNCTION: Moves the cost of protection — reception, adjudication, integration, and the political cost of expanded eligibility — from countries of origin (which do not bear it) and from claimants themselves (who cannot bear it) onto destination and frontline states and their domestic populations; moves recognition and safety toward the broadened beneficiary classes.
% ABSENT_VOICES: Countries of origin whose internal conditions generate the claims are not parties to the interpretive dispute at all — the entire adjudicatory burden is externalized to destination states while origin-state accountability is untouched. Domestic populations bearing fiscal and social integration costs in frontline and destination states have no direct seat in treaty interpretation, which occurs in tribunals, UNHCR guidance, and appellate courts.
% DISAPPEARANCE_RATIONALE: If this reading collapsed and the restrictive reading prevailed instead, a large share of currently-recognized claimants (generalized-violence, non-state-actor, and several social-group categories) would face return; the world would clearly rearrange for those specific claimant populations and for the tribunals now citing this jurisprudence. Whether it 'rearranges' at systemic level is contested: proponents say the humanitarian architecture would collapse into pure sovereign discretion; skeptics say state practice would simply revert to what most states already do at the margins, since many never fully adopted the expansive reading in the first place.
% FOUNDING_PROBLEM: The 1951 Convention was built to address a defined post-WWII refugee crisis: individuals with well-founded fear of persecution by state actors on discrete grounds, arising from a specific and largely closed set of European displacement events.
% FOUNDING_PROBLEM_CORROBORATION: UNHCR and refugee-rights scholarship attest the founding problem has evolved but the underlying humanitarian rationale (protecting those who cannot rely on state protection) remains fully live and in fact more urgent given contemporary conflict and persecution patterns. Restrictive-reading states and some comparative-law scholars, from outside the expansive-reading's own advocacy network, attest that the founding problem as textually negotiated (individualized, state-linked persecution from a bounded historical crisis) has been substantially supplanted by a much broader mandate never agreed to by the drafting parties or by many current treaty signatories — this is a genuine outside-corroborated dispute, not resolved by either side's own assertion.
narrative_ontology:disappearance_verdict(refugee_convention_text__expansive_humanitarian_reading, contested).
narrative_ontology:founding_problem_status(refugee_convention_text__expansive_humanitarian_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(refugee_convention_text__expansive_humanitarian_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(refugee_convention_text__expansive_humanitarian_reading, 'none', 1).
narrative_ontology:epsilon_provenance(refugee_convention_text__expansive_humanitarian_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(refugee_convention_text__expansive_humanitarian_reading_tests).
:- end_tests(refugee_convention_text__expansive_humanitarian_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.42) reflects a real but moderate transfer: this reading does not extract wealth for a concentrated rent-collector — no single agent profits from the doctrine — but it does impose real, growing, unconsented costs on destination and frontline states and their taxpayers who never negotiated this scope of obligation. Suppression (0.55) is substantial because states attempting to narrow the reading through domestic legislation or the restrictive reading face non-refoulement litigation, UNHCR intervention, and reputational costs in the international legal community — genuine coercive pressure toward compliance with the expansive reading exists, even without a central enforcer. Theater ratio (0.28) is moderate-low: the substantive assessment duty and recognition of new social-group categories are real functional changes, not merely performative, though some 'protection' announcements by destination states substitute favorable rhetoric for actual capacity-building. Accessibility collapse (0.35) is lower than a mountain's, appropriately: states retain meaningful room to interpret 'well-founded fear' and 'particular social group' variably across jurisdictions — the restrictive and procedural readings remain live alternatives elsewhere, which is precisely why this is a kernel-reading story rather than a settled fact. Resistance (0.75) is high: this reading meets sustained, organized push-back from destination states, populist political movements, and states asserting sovereign discretion — it is far from an uncontested norm.
 *
 * DIRECTIONALITY LOGIC:
 *   Asylum seekers across all four newly-recognized categories sit at the full-beneficiary end: the reading exists specifically to bring their claims within scope, and they have essentially no alternative protection route if this reading is rejected (trapped exit). UNHCR, as agenda-setter and institutional advocate, is coded as a secondary beneficiary because its mandate scope and legitimacy expand with the reading's adoption, even though it does not collect a material rent. Destination and frontline states are payers: exit is constrained (they remain Convention parties bound by pacta sunt servanda and reputational costs of withdrawal) rather than trapped, but they bear real costs they did not choose. Domestic taxpayers are trapped payers with no direct voice in treaty interpretation. Restrictive sovereignty states are marked excluded rather than payer because their objection is to the interpretive claim itself, not merely to bearing its costs — they contest the reading's legitimacy, not just its price.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding-problem status is authored as contested rather than dead or fully live, which is the correct answer for a kernel-reading story: proponents (UNHCR, humanitarian scholarship) attest the underlying rationale — protecting those without effective state protection — is more urgent than in 1951, not less. Genuinely outside corroboration (comparative law scholarship critical of doctrinal drift, and the plain fact that many treaty signatories never joined the interpretive expansion) supports the view that THIS SPECIFIC textual reading (individualized, state-linked persecution from a bounded 1951 crisis) has been substantially supplanted without renegotiation of the actual treaty text. This is not resolved by declaring the reading a pure extraction device or a pure vindicated mandate — it is a live contest between readings of the same kernel, which is exactly what this story is one instance of.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indeterminacy_particular_social_group,
    'Does the Convention''s text and negotiating history support reading ''particular social group'' to include gender, sexual orientation/gender identity, and clan membership, or does this exceed what the 1951 drafters intended and what many current signatory states have consented to?',
    'Comparative analysis of state practice across major receiving jurisdictions (EU Qualification Directive case law, US BIA/circuit court precedent, UK/Canadian/Australian appellate jurisprudence) against the Convention''s travaux préparatoires; convergence or persistent divergence would indicate whether this reading has become customary international law or remains one contested interpretation among several.',
    'If state practice has converged strongly on the expansive reading, this constraint is closer to settled international law (lower suppression, higher legitimacy). If divergence persists and is substantial, the expansive reading is better understood as an advocacy position within an unresolved interpretive contest, and its coercive weight against restrictive states is correspondingly less legitimate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_indeterminacy_particular_social_group, conceptual, 'Whether the expansive social-group reading reflects converged customary law or a still-contested interpretive claim.').

omega_variable(
    sibling_reading_structural_delta,
    'What specifically would change if the restrictive_sovereignty_reading or procedural_integrity_reading prevailed instead of this reading in a given jurisdiction''s courts?',
    'Track jurisdictions that have shifted from one reading to another over time (e.g., tightening of social-group tests, narrowing of non-state-actor persecution recognition) and measure recognition-rate deltas pre/post shift.',
    'Under restrictive_sovereignty_reading, the beneficiary set (generalized-violence and non-state-actor claimants, several social-group categories) would substantially shrink and would need individualized state-linked persecution proof; under procedural_integrity_reading, the same claimants might still be assessed but the outcome would depend on process quality rather than a guaranteed substantive duty — the victim set (frontline/destination states) would face different cost profiles (bounded procedural cost vs. open-ended substantive assessment cost) under each sibling.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_structural_delta, conceptual, 'The structural delta between this reading and its two siblings, routed here rather than into the constraint''s own metrics.').

omega_variable(
    extraction_referent_stability,
    'Is the 0.42 extractiveness figure stable across the interval, or does it understate extraction in jurisdictions where the expansive reading has become de facto mandatory via litigation risk, versus overstate it in jurisdictions where it remains aspirational UNHCR guidance with little binding force?',
    'Disaggregate extractiveness by jurisdiction-cluster (binding appellate precedent vs. guidance-only) rather than treating the reading as a single global average.',
    'A jurisdiction-disaggregated analysis might reveal this story is itself an average masking two further sub-constraints (binding-precedent expansive reading vs. guidance-only expansive reading) that should be decomposed per the ε-invariance principle if the divergence proves large.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(extraction_referent_stability, empirical, 'Whether treating this reading as one global constraint masks jurisdiction-level ε variation large enough to require further decomposition.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(refugee_convention_text__expansive_humanitarian_reading, 1951, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(refu_tr_t1951, refugee_convention_text__expansive_humanitarian_reading, theater_ratio, 1951, 0.1).
narrative_ontology:measurement(refu_tr_t1975, refugee_convention_text__expansive_humanitarian_reading, theater_ratio, 1975, 0.14).
narrative_ontology:measurement(refu_tr_t1990, refugee_convention_text__expansive_humanitarian_reading, theater_ratio, 1990, 0.18).
narrative_ontology:measurement(refu_tr_t2005, refugee_convention_text__expansive_humanitarian_reading, theater_ratio, 2005, 0.22).
narrative_ontology:measurement(refu_tr_t2015, refugee_convention_text__expansive_humanitarian_reading, theater_ratio, 2015, 0.25).
narrative_ontology:measurement(refu_tr_t2024, refugee_convention_text__expansive_humanitarian_reading, theater_ratio, 2024, 0.28).

% Extraction over time
narrative_ontology:measurement(refu_be_t1951, refugee_convention_text__expansive_humanitarian_reading, base_extractiveness, 1951, 0.15).
narrative_ontology:measurement(refu_be_t1975, refugee_convention_text__expansive_humanitarian_reading, base_extractiveness, 1975, 0.22).
narrative_ontology:measurement(refu_be_t1990, refugee_convention_text__expansive_humanitarian_reading, base_extractiveness, 1990, 0.28).
narrative_ontology:measurement(refu_be_t2005, refugee_convention_text__expansive_humanitarian_reading, base_extractiveness, 2005, 0.34).
narrative_ontology:measurement(refu_be_t2015, refugee_convention_text__expansive_humanitarian_reading, base_extractiveness, 2015, 0.38).
narrative_ontology:measurement(refu_be_t2024, refugee_convention_text__expansive_humanitarian_reading, base_extractiveness, 2024, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(refu_su_t1951, refugee_convention_text__expansive_humanitarian_reading, suppression_requirement, 1951, 0.25).
narrative_ontology:measurement(refu_su_t1975, refugee_convention_text__expansive_humanitarian_reading, suppression_requirement, 1975, 0.32).
narrative_ontology:measurement(refu_su_t1990, refugee_convention_text__expansive_humanitarian_reading, suppression_requirement, 1990, 0.38).
narrative_ontology:measurement(refu_su_t2005, refugee_convention_text__expansive_humanitarian_reading, suppression_requirement, 2005, 0.45).
narrative_ontology:measurement(refu_su_t2015, refugee_convention_text__expansive_humanitarian_reading, suppression_requirement, 2015, 0.5).
narrative_ontology:measurement(refu_su_t2024, refugee_convention_text__expansive_humanitarian_reading, suppression_requirement, 2024, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(refugee_convention_text__expansive_humanitarian_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(refugee_convention_text__expansive_humanitarian_reading, 0.12).
narrative_ontology:affects_constraint(refugee_convention_text__expansive_humanitarian_reading, restrictive_sovereignty_reading).
narrative_ontology:affects_constraint(refugee_convention_text__expansive_humanitarian_reading, procedural_integrity_reading).

% DUAL FORMULATION NOTE:
% This story is one of three constraint stories decomposing the natural-language concept 'the Refugee Convention's protection scope' per the ε-invariance principle. The kernel (refugee_convention_text) is a single treaty text with contested authoritative meaning; this reading (expansive_humanitarian_reading), the restrictive_sovereignty_reading, and the procedural_integrity_reading are three structurally distinct constraints with different beneficiary/victim sets, different ε, and different classifications, linked here rather than merged into one story with a hidden interpretation parameter.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
