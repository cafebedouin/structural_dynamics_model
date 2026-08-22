% ============================================================================
% CONSTRAINT STORY: sovereign_legitimacy__republican_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sovereign_legitimacy__republican_reading, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: sovereign_legitimacy__republican_reading
 *   human_readable: Republican Legitimacy: Popular Sovereignty Through Delegated Consent
 *   domain: political/philosophical
 *
 * SUMMARY:
 *   The republican reading instantiates legitimate authority as flowing
 *   upward from the people through delegated consent, grounded in popular
 *   sovereignty and social contract theory. This reading emerged from
 *   Enlightenment contract theory (Locke, Rousseau) and was institutionalized
 *   in the American and French revolutionary settlements. The constraint
 *   operates as a tangled rope: it genuinely coordinates collective
 *   self-governance through electoral mechanisms (beneficiaries include
 *   enfranchised citizens, elected officials, constitutional institutions),
 *   while simultaneously extracting compliance from and excluding those
 *   outside the franchise or consent mechanisms (disenfranchised populations,
 *   permanent minorities, stateless persons). The extraction is moderate —
 *   legitimacy requires ongoing validation through elections — and
 *   accountability exists through removal mechanisms, but the structure
 *   remains vulnerable to majoritarian tyranny. The coordination function
 *   (peaceful transfer of power, collective decision-making) is real, but the
 *   exclusionary boundary is actively maintained through citizenship laws,
 *   voter qualification rules, and territorial sovereignty claims.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sovereign_legitimacy__republican_reading, 0.42).
domain_priors:suppression_score(sovereign_legitimacy__republican_reading, 0.38).
domain_priors:theater_ratio(sovereign_legitimacy__republican_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sovereign_legitimacy__republican_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(sovereign_legitimacy__republican_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(sovereign_legitimacy__republican_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sovereign_legitimacy__republican_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(sovereign_legitimacy__republican_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sovereign_legitimacy__republican_reading, tangled_rope).
narrative_ontology:human_readable(sovereign_legitimacy__republican_reading, "Republican Legitimacy: Popular Sovereignty Through Delegated Consent").
narrative_ontology:topic_domain(sovereign_legitimacy__republican_reading, "political/philosophical").

domain_priors:requires_active_enforcement(sovereign_legitimacy__republican_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sovereign_legitimacy__republican_reading, '693fb6b1-8813-49a3-b56d-3e1252163ca2').
narrative_ontology:cs_kernel_codification('693fb6b1-8813-49a3-b56d-3e1252163ca2', formalized).
narrative_ontology:cs_authority_grounding('693fb6b1-8813-49a3-b56d-3e1252163ca2', lineage).
narrative_ontology:cs_interpretation_layer_present('693fb6b1-8813-49a3-b56d-3e1252163ca2').
narrative_ontology:cs_reading_relation('693fb6b1-8813-49a3-b56d-3e1252163ca2', sovereign_legitimacy__monarchical_reading, forecloses).
narrative_ontology:cs_reading_relation('693fb6b1-8813-49a3-b56d-3e1252163ca2', sovereign_legitimacy__constitutional_hybrid_reading, coexists_with).
narrative_ontology:cs_axiom('693fb6b1-8813-49a3-b56d-3e1252163ca2', foundational, all_legitimate_authority_derives_from_consent).
narrative_ontology:cs_axiom_status(all_legitimate_authority_derives_from_consent, holdable).
narrative_ontology:cs_axiom_grounding('693fb6b1-8813-49a3-b56d-3e1252163ca2', all_legitimate_authority_derives_from_consent, deontological).
narrative_ontology:cs_axiom('693fb6b1-8813-49a3-b56d-3e1252163ca2', foundational, consent_requires_actual_participation).
narrative_ontology:cs_axiom_status(consent_requires_actual_participation, holdable).
narrative_ontology:cs_axiom_grounding('693fb6b1-8813-49a3-b56d-3e1252163ca2', consent_requires_actual_participation, empirically_contingent).
narrative_ontology:cs_axiom('693fb6b1-8813-49a3-b56d-3e1252163ca2', secondary, majority_rule_bounded_by_rights).
narrative_ontology:cs_axiom_status(majority_rule_bounded_by_rights, holdable).
narrative_ontology:cs_axiom_grounding('693fb6b1-8813-49a3-b56d-3e1252163ca2', majority_rule_bounded_by_rights, conventional).
narrative_ontology:cs_reference_frame('693fb6b1-8813-49a3-b56d-3e1252163ca2', social_contract_origin).
narrative_ontology:cs_drift_state('693fb6b1-8813-49a3-b56d-3e1252163ca2', contemporary_democratic_erosion, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('693fb6b1-8813-49a3-b56d-3e1252163ca2', '').
narrative_ontology:cs_kernel_id(sovereign_legitimacy__republican_reading, sovereign_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sovereign_legitimacy__republican_reading, enfranchised_citizenry).
narrative_ontology:constraint_beneficiary(sovereign_legitimacy__republican_reading, elected_officials).
narrative_ontology:constraint_beneficiary(sovereign_legitimacy__republican_reading, constitutional_institutions).
narrative_ontology:constraint_victim(sovereign_legitimacy__republican_reading, disenfranchised_populations).
narrative_ontology:constraint_victim(sovereign_legitimacy__republican_reading, permanent_minorities).
narrative_ontology:constraint_victim(sovereign_legitimacy__republican_reading, stateless_persons).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold voting rights and participate in electoral cycles that validate the government's legitimacy. Receive representation, rights protection, and collective goods (public services, security, legal standing). Can exit through emigration but face high costs (leaving community, assets, cultural ties). Their consent is the legitimating fuel; they also bear compliance costs (taxes, laws) but with voice.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__republican_reading, enfranchised_citizenry, beneficiary,
    organized, biographical, constrained, national).

% Administer the delegated consent structure: set legislative agendas, control executive resources, appoint judges. Their authority derives from and is limited by electoral cycles. They benefit materially (office perks, post-office opportunities) and symbolically (status, legacy). Exit is mobile: they can leave office, join private sector, or run for different office. Their power is institutional but time-bounded.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__republican_reading, elected_officials, agenda_setter,
    institutional, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(sovereign_legitimacy__republican_reading, elected_officials, beneficiary).

% Courts, legislatures, electoral commissions, civil service — the infrastructure that makes delegated consent operational. They interpret the social contract, enforce constitutional boundaries, and manage succession. They benefit from institutional perpetuity, resource control, and epistemic authority. Exit is arbitrage-grade: institutions persist across personnel changes; their survival is the constraint's survival.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__republican_reading, constitutional_institutions, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(sovereign_legitimacy__republican_reading, constitutional_institutions, beneficiary).

% Subject to laws and policies they cannot vote on: felons, non-citizen residents, territories without representation, historical exclusions (women pre-suffrage, racial minorities pre-VRA). Bear full compliance costs (taxes, criminal law, regulation) with zero formal voice. Exit is constrained: emigration possible but legally and economically difficult. Their exclusion is maintained by the same citizenship and franchise rules the constraint legitimates.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__republican_reading, disenfranchised_populations, payer,
    powerless, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(sovereign_legitimacy__republican_reading, disenfranchised_populations, excluded).

% Groups that consistently lose in majoritarian contests (ethnic, religious, ideological minorities in homogeneous polities). Have formal voting rights but structurally cannot convert them into governing power. Bear costs of majority policies (cultural assimilation pressure, resource allocation, symbolic exclusion) without effective veto. Exit is identity_locked: leaving the polity means abandoning the communal identity constituted within it. Their situation is the republican reading's structural vulnerability.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__republican_reading, permanent_minorities, payer,
    moderate, generational, identity_locked, national).

% Persons with no recognized citizenship anywhere: refugees, denaturalized persons, children of stateless parents. Subject to the constraint's territorial sovereignty claims (border enforcement, detention, denial of rights) with no access to any delegated-consent mechanism anywhere. Exit is trapped: no polity claims them, no polity admits them. They are the constraint's outer boundary — the people for whom popular sovereignty produces only exclusion.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__republican_reading, stateless_persons, payer,
    powerless, biographical, trapped, global).
narrative_ontology:stakeholder_secondary_role(sovereign_legitimacy__republican_reading, stateless_persons, excluded).

% Analyze, critique, and reconstruct the legitimacy claim from outside the operational machinery. They do not collect rents or bear compliance costs directly. Their exit is analytical: they can change frameworks, compare readings, propose alternatives. They see the full structure — the coordination, the extraction, the exclusions — but their judgment carries no enforcement weight.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__republican_reading, political_theorists, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(sovereign_legitimacy__republican_reading, constitutional_institutions).
narrative_ontology:fixing_cost_class(sovereign_legitimacy__republican_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the problem of peaceful, legitimate collective decision-making and power transfer without violence or arbitrary rule. Replaces the succession crisis and the tyrant's whim with a known procedure (elections) that all enfranchised parties accept as authoritative.
% TRANSFER_FUNCTION: Moves authority (legitimate coercion, resource allocation, lawmaking) from the citizenry (as principal) to elected officials and institutions (as agents), in exchange for accountability (electoral removal, constitutional constraints). Moves compliance costs from the governors to the governed, with the governed's consent as the price.
% ABSENT_VOICES: The permanently excluded (stateless persons, future generations, non-human nature) and the structurally silenced (permanent minorities whose votes never pivot outcomes). They are not in the room because the constraint defines the room as 'the citizenry' and the citizenry as 'those who vote.' The monarchical and hybrid readings would also exclude them, but for different genealogical reasons.
% DISAPPEARANCE_RATIONALE: If republican legitimacy vanished overnight, the coercive apparatus (police, military, tax administration) would lose its authorizing narrative. Power would not disappear but would reorganize around alternative legitimating claims: martial law, technocratic competence, charismatic leadership, or monarchical restoration. The world rearranges because the constraint is the operating system for modern state authority.
% FOUNDING_PROBLEM: Arbitrary rule without consent: hereditary monarchs claiming divine right, colonial governors answering to distant capitals, warlords imposing order through violence alone. The problem was not just tyranny but the absence of a peaceful mechanism to replace tyrants and constrain power.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem (arbitrary rule without consent) is attested as live by democracy indices (V-Dem, Freedom House) showing democratic backsliding, by social movements demanding inclusion, and by political theorists (Arendt, Pateman, Rancière) arguing consent remains partial. It is attested as dead by institutionalists (Fukuyama, Huntington) who view procedural democracy as consolidated. The corroboration from OUTSIDE the beneficiary set comes from the excluded themselves: disenfranchised populations and stateless persons experience the founding problem as live daily — their testimony is the strongest corroboration.
narrative_ontology:disappearance_verdict(sovereign_legitimacy__republican_reading, world_rearranges).
narrative_ontology:founding_problem_status(sovereign_legitimacy__republican_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sovereign_legitimacy__republican_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(sovereign_legitimacy__republican_reading, 'none', 1).
narrative_ontology:epsilon_provenance(sovereign_legitimacy__republican_reading, 0.42, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sovereign_legitimacy__republican_reading_tests).
:- end_tests(sovereign_legitimacy__republican_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.42) reflects that the constraint demands compliance and participation from all while distributing voice unequally — the extraction is the cost of legitimacy maintenance (elections, institutional upkeep) borne disproportionately by those with limited exit. Suppression (0.38) is moderate: the constraint does not primarily rely on coercion but on the structural exclusion of non-citizens and the majoritarian mechanism that can silence permanent minorities. Theater ratio (0.28) captures the gap between the consent narrative and the reality that consent is manufactured through the very institutions it legitimates. Accessibility collapse (0.35) is low for a political constraint: alternatives (monarchy, technocracy, anarchism) remain conceptually available. Resistance (0.55) is significant: the reading faces continuous contestation from excluded groups, sovereignty challengers, and competing legitimacy claims.
 *
 * PERSPECTIVAL GAP:
 *   The engine should compute divergent seat classifications: from the enfranchised citizen seat, the constraint reads as rope (genuine coordination with costs); from the disenfranchised seat, it reads as snare (extraction without representation); from the institutional seat, it reads as scaffold (transitional legitimacy requiring periodic renewal). The claimed tangled_rope captures the structural duality but no single seat experiences it as such.
 *
 * DIRECTIONALITY LOGIC:
 *   Enfranchised citizenry are primary beneficiaries (d ≈ 0.2): they receive representation, rights protection, and collective goods. Elected officials and constitutional institutions are secondary beneficiaries (d ≈ 0.15): they gain authority and resources from the delegated consent structure. Disenfranchised populations (d ≈ 0.85) and permanent minorities (d ≈ 0.75) are victims: they bear compliance costs without voice. Stateless persons (d ≈ 0.9) are fully excluded targets. The exit options differ radically: citizens have constrained exit (emigration possible but costly), while stateless persons are trapped. Power atoms range from institutional (elected officials) to organized (citizenry) to powerless (stateless persons).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — arbitrary rule without consent — remains live (contested status), but the specific mechanism (periodic elections, territorial citizenship) shows mandatrophy symptoms: the consent ritual persists while substantive accountability erodes (gerrymandering, campaign finance, administrative state insulation). The constraint does not resolve the mandatrophy; it manages it through theater.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_framing_underdetermination,
    'Is the sovereign_legitimacy kernel best framed as (a) the institutional arrangement for authorizing coercion, (b) the normative claim about where authority originates, or (c) the empirical pattern of compliance and resistance?',
    'Disciplinary triangulation: political philosophy (b), constitutional law (a), comparative politics (c). Each framing yields different ε and different victim/beneficiary sets for the same reading.',
    'If (a), the republican reading''s victims are those subject to laws they didn''t consent to. If (b), the victims are those whose consent is structurally impossible to give. If (c), the victims are those who comply without belief. The classification shifts from tangled_rope toward snare under (c).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_underdetermination, conceptual, 'Irreducible framing ambiguity in the kernel itself — the reading inherits the kernel''s under-determination.').

omega_variable(
    consent_manufacture_vs_genuine_delegation,
    'Does the electoral mechanism genuinely register consent, or does it manufacture the consent it claims to measure?',
    'Longitudinal study of preference formation: if policy preferences shift to match available electoral options rather than options shifting to match preferences, consent is manufactured. Compare issue salience before/after elite agenda-setting.',
    'If manufactured, the coordination function is partially illusory and ε is higher than measured — the constraint extracts the appearance of consent. If genuine, the moderate ε stands. The tangent between rope and tangled_rope hinges on this.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(consent_manufacture_vs_genuine_delegation, empirical, 'Core epistemic ambiguity in the republican reading''s self-justification.').

omega_variable(
    majoritarian_tyranny_as_structural_or_contingent,
    'Is the vulnerability to majoritarian tyranny a structural feature of the republican reading (any delegated-consent mechanism produces it) or a contingent failure of specific institutional designs (judicial review, federalism, supermajority rules)?',
    'Counterfactual institutional comparison: do republican systems with strong counter-majoritarian checks show systematically lower extraction on permanent minorities? If yes, tyranny is contingent; if no, structural.',
    'If structural, the republican reading has an inescapable snare component for permanent minorities — the tangled_rope classification understates the victim seat''s experience. If contingent, the current ε reflects design choices, not the reading''s essence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(majoritarian_tyranny_as_structural_or_contingent, conceptual, 'Whether the reading''s extraction on minorities is necessary or remediable within the reading''s own logic.').

omega_variable(
    reading_relations_underdetermination,
    'Does the republican reading foreclose the monarchical reading, or do they coexist as live options in different frameworks?',
    'Conceptual analysis: can a single polity coherently hold both that authority originates in popular sovereignty AND that some authority (ceremonial, residual) originates in hereditary right? The constitutional_hybrid_reading claims yes; pure republicans claim no.',
    'If forecloses, the republican reading''s claim to exclusivity is logically necessary. If coexists_with, the exclusivity is a political choice, not a logical one — the monarchical reading remains a live alternative within the same kernel, which changes the legitimacy stakes.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_relations_underdetermination, conceptual, 'Logical relationship between this reading and the monarchical sibling — the forecloses/coexists_with distinction is foundational to the reading''s self-understanding.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sovereign_legitimacy__republican_reading, 1789, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sov_leg_rep_tr_t1789, sovereign_legitimacy__republican_reading, theater_ratio, 1789, 0.35).
narrative_ontology:measurement(sov_leg_rep_tr_t1850, sovereign_legitimacy__republican_reading, theater_ratio, 1850, 0.32).
narrative_ontology:measurement(sov_leg_rep_tr_t1920, sovereign_legitimacy__republican_reading, theater_ratio, 1920, 0.28).
narrative_ontology:measurement(sov_leg_rep_tr_t1965, sovereign_legitimacy__republican_reading, theater_ratio, 1965, 0.25).
narrative_ontology:measurement(sov_leg_rep_tr_t2000, sovereign_legitimacy__republican_reading, theater_ratio, 2000, 0.27).
narrative_ontology:measurement(sov_leg_rep_tr_t2024, sovereign_legitimacy__republican_reading, theater_ratio, 2024, 0.28).

% Extraction over time
narrative_ontology:measurement(sov_leg_rep_be_t1789, sovereign_legitimacy__republican_reading, base_extractiveness, 1789, 0.55).
narrative_ontology:measurement(sov_leg_rep_be_t1850, sovereign_legitimacy__republican_reading, base_extractiveness, 1850, 0.5).
narrative_ontology:measurement(sov_leg_rep_be_t1920, sovereign_legitimacy__republican_reading, base_extractiveness, 1920, 0.45).
narrative_ontology:measurement(sov_leg_rep_be_t1965, sovereign_legitimacy__republican_reading, base_extractiveness, 1965, 0.42).
narrative_ontology:measurement(sov_leg_rep_be_t2000, sovereign_legitimacy__republican_reading, base_extractiveness, 2000, 0.4).
narrative_ontology:measurement(sov_leg_rep_be_t2024, sovereign_legitimacy__republican_reading, base_extractiveness, 2024, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(sov_leg_rep_su_t1789, sovereign_legitimacy__republican_reading, suppression_requirement, 1789, 0.65).
narrative_ontology:measurement(sov_leg_rep_su_t1850, sovereign_legitimacy__republican_reading, suppression_requirement, 1850, 0.55).
narrative_ontology:measurement(sov_leg_rep_su_t1920, sovereign_legitimacy__republican_reading, suppression_requirement, 1920, 0.45).
narrative_ontology:measurement(sov_leg_rep_su_t1965, sovereign_legitimacy__republican_reading, suppression_requirement, 1965, 0.38).
narrative_ontology:measurement(sov_leg_rep_su_t2000, sovereign_legitimacy__republican_reading, suppression_requirement, 2000, 0.35).
narrative_ontology:measurement(sov_leg_rep_su_t2024, sovereign_legitimacy__republican_reading, suppression_requirement, 2024, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sovereign_legitimacy__republican_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(sovereign_legitimacy__republican_reading, 0.12).
narrative_ontology:affects_constraint(sovereign_legitimacy__republican_reading, sovereign_legitimacy__monarchical_reading).
narrative_ontology:affects_constraint(sovereign_legitimacy__republican_reading, sovereign_legitimacy__constitutional_hybrid_reading).
narrative_ontology:affects_constraint(sovereign_legitimacy__republican_reading, electoral_accountability_mechanism).
narrative_ontology:affects_constraint(sovereign_legitimacy__republican_reading, citizenship_boundary_enforcement).
narrative_ontology:affects_constraint(sovereign_legitimacy__republican_reading, constitutional_amendment_procedure).

% DUAL FORMULATION NOTE:
% Part of the sovereign_legitimacy constraint family (kernel_id: sovereign_legitimacy). This reading (republican_reading) differs structurally from monarchical_reading (ε ≈ 0.65, snare-class, beneficiaries: hereditary sovereign + court) and constitutional_hybrid_reading (ε ≈ 0.35, tangled_rope, dual beneficiary sets). The republican reading's ε is moderate because ongoing electoral validation distributes extraction across cycles rather than concentrating it in a sovereign; the hybrid reading lowers ε further by splitting the authorization function. All three readings share the kernel's contested legitimacy referent but instantiate different constraints with different stakeholder structures.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sovereign_legitimacy__republican_reading, institutional, 0.15).
constraint_indexing:directionality_override(sovereign_legitimacy__republican_reading, organized, 0.2).
constraint_indexing:directionality_override(sovereign_legitimacy__republican_reading, powerless, 0.9).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
