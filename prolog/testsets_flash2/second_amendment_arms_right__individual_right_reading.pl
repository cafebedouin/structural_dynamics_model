% ============================================================================
% CONSTRAINT STORY: second_amendment_arms_right__individual_right_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: second_amendment_arms_right__individual_right_reading
 *   human_readable: Second Amendment: Individual Right to Bear Arms
 *   domain: constitutional_law/political_philosophy/legal_interpretation
 *
 * SUMMARY:
 *   This constraint represents the 'individual right' reading of the Second
 *   Amendment, which interprets the right to keep and bear arms as an
 *   individual liberty pre-existing government, protected against federal
 *   infringement. This reading gained significant legal traction with the
 *   Heller (2008) and McDonald (2010) Supreme Court decisions. It places
 *   individual gun owners in a beneficiary position and federal/state
 *   regulatory authorities in a constrained, payer position. The claimed type
 *   is 'tangled_rope' because it genuinely coordinates individual liberty
 *   with state power, but with significant asymmetric extraction from
 *   regulatory bodies and public safety efforts.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_arms_right__individual_right_reading, 0.7).
domain_priors:suppression_score(second_amendment_arms_right__individual_right_reading, 0.6).
domain_priors:theater_ratio(second_amendment_arms_right__individual_right_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_arms_right__individual_right_reading, extractiveness, 0.7).
narrative_ontology:constraint_metric(second_amendment_arms_right__individual_right_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(second_amendment_arms_right__individual_right_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_arms_right__individual_right_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(second_amendment_arms_right__individual_right_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_arms_right__individual_right_reading, tangled_rope).
narrative_ontology:human_readable(second_amendment_arms_right__individual_right_reading, "Second Amendment: Individual Right to Bear Arms").
narrative_ontology:topic_domain(second_amendment_arms_right__individual_right_reading, "constitutional_law/political_philosophy/legal_interpretation").

domain_priors:requires_active_enforcement(second_amendment_arms_right__individual_right_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_arms_right__individual_right_reading, '81392eb9-8968-4762-9ab2-18598cf7222a').
narrative_ontology:cs_kernel_codification('81392eb9-8968-4762-9ab2-18598cf7222a', fixed_text).
narrative_ontology:cs_authority_grounding('81392eb9-8968-4762-9ab2-18598cf7222a', lineage).
narrative_ontology:cs_interpretation_layer_present('81392eb9-8968-4762-9ab2-18598cf7222a').
narrative_ontology:cs_reading_relation('81392eb9-8968-4762-9ab2-18598cf7222a', second_amendment_arms_right__collective_right_reading, coexists_with).
narrative_ontology:cs_reading_relation('81392eb9-8968-4762-9ab2-18598cf7222a', second_amendment_arms_right__civic_republican_reading, coexists_with).
narrative_ontology:cs_axiom('81392eb9-8968-4762-9ab2-18598cf7222a', foundational, individual_right_precedes_government).
narrative_ontology:cs_axiom_status(individual_right_precedes_government, holdable).
narrative_ontology:cs_axiom_grounding('81392eb9-8968-4762-9ab2-18598cf7222a', individual_right_precedes_government, deontological).
narrative_ontology:cs_axiom('81392eb9-8968-4762-9ab2-18598cf7222a', foundational, self_defense_is_fundamental).
narrative_ontology:cs_axiom_status(self_defense_is_fundamental, holdable).
narrative_ontology:cs_axiom_grounding('81392eb9-8968-4762-9ab2-18598cf7222a', self_defense_is_fundamental, deontological).
narrative_ontology:cs_reference_frame('81392eb9-8968-4762-9ab2-18598cf7222a', founding_era_natural_rights).
narrative_ontology:cs_drift_state('81392eb9-8968-4762-9ab2-18598cf7222a', contemporary_judicial_precedent, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('81392eb9-8968-4762-9ab2-18598cf7222a', '').
narrative_ontology:cs_kernel_id(second_amendment_arms_right__individual_right_reading, second_amendment_arms_right).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_arms_right__individual_right_reading, individual_gun_owners).
narrative_ontology:constraint_beneficiary(second_amendment_arms_right__individual_right_reading, firearms_industry).
narrative_ontology:constraint_victim(second_amendment_arms_right__individual_right_reading, federal_regulatory_agencies).
narrative_ontology:constraint_victim(second_amendment_arms_right__individual_right_reading, state_legislatures_seeking_prohibition).
narrative_ontology:constraint_vindicates(second_amendment_arms_right__individual_right_reading, natural_rights_doctrine).
narrative_ontology:constraint_vindicates(second_amendment_arms_right__individual_right_reading, individual_liberty_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Assert their right to own firearms for self-defense and other lawful purposes, viewing government restrictions as infringements on a pre-existing liberty. They benefit from the legal protection of this right, but face increasing social and political pressure.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__individual_right_reading, individual_gun_owners, beneficiary,
    organized, biographical, constrained, national).

% Benefits directly from the legal framework that protects firearm ownership, enabling the manufacture and sale of arms. They actively lobby to maintain and expand this interpretation, viewing it as essential to their business model.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__individual_right_reading, firearms_industry, beneficiary,
    powerful, generational, mobile, national).

% Are constrained in their ability to enact and enforce firearm regulations due to this interpretation. They bear the cost of legal challenges and political opposition when attempting to implement public safety measures related to firearms.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__individual_right_reading, federal_regulatory_agencies, payer,
    institutional, generational, constrained, national).

% Face significant legal hurdles and judicial review when attempting to pass strict firearm control laws, as such laws are often challenged as infringing on individual Second Amendment rights. They bear the political and legal costs of these challenges.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__individual_right_reading, state_legislatures_seeking_prohibition, payer,
    institutional, generational, constrained, regional).

% Advocate for stricter gun control measures to reduce violence, but their policy proposals are often foreclosed or severely limited by the individual rights interpretation of the Second Amendment. They are excluded from the policy space defined by this reading.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__individual_right_reading, public_safety_advocates, excluded,
    organized, generational, constrained, national).

% Analyze and interpret the Second Amendment, often supporting the individual rights reading based on historical context, textual analysis, and philosophical arguments about natural rights. Their work provides intellectual grounding for the legal and political positions of beneficiaries.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__individual_right_reading, constitutional_scholars_individualist, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a legal framework that coordinates the rights of individual citizens to possess firearms with the powers of government to regulate them, aiming to prevent both tyranny and anarchy by empowering citizens.
% TRANSFER_FUNCTION: Transfers the burden of proof and legal justification from individual gun owners to government entities seeking to restrict firearm possession, effectively transferring regulatory power and associated costs.
% ABSENT_VOICES: Advocates for a collective or civic republican interpretation of the Second Amendment, who would argue for greater state authority over militias or a focus on civic duty rather than individual self-defense, are marginalized in the discourse dominated by the individual rights reading.
% DISAPPEARANCE_RATIONALE: If this individual rights interpretation vanished, federal and state governments would immediately gain significantly more power to regulate and prohibit firearms, leading to a rapid and profound restructuring of gun ownership laws, the firearms industry, and public safety policy across the nation.
% FOUNDING_PROBLEM: The founding problem was to ensure that citizens retained the means to resist potential government overreach and to participate in a well-regulated militia, reflecting a fear of standing armies and a belief in popular sovereignty.
% FOUNDING_PROBLEM_CORROBORATION: Individual gun owners and the firearms industry attest that the threat of government overreach remains live, and the right to bear arms is a crucial check. Public safety advocates and some constitutional scholars argue the original problem is largely superseded by modern military and policing structures, and the current interpretation creates new problems, making the status contested.
narrative_ontology:disappearance_verdict(second_amendment_arms_right__individual_right_reading, world_rearranges).
narrative_ontology:founding_problem_status(second_amendment_arms_right__individual_right_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_arms_right__individual_right_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(second_amendment_arms_right__individual_right_reading, 'none', 1).
narrative_ontology:epsilon_provenance(second_amendment_arms_right__individual_right_reading, 0.7, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is high (0.7) because the interpretation imposes substantial costs on government efforts to regulate firearms, effectively transferring power and resources to individual owners and the firearms industry. Suppression (0.6) is also high, as it actively suppresses alternative regulatory approaches and limits the options of legislative bodies. Resistance (0.8) is very high, reflecting ongoing political and legal battles over gun control. The theater ratio (0.2) is relatively low, as the enforcement of this right is a genuine, active legal and political process, not merely performative. The historical measurements show a clear trend of increasing extractiveness and suppression as this interpretation has gained legal dominance.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of individual gun owners, this constraint is a vital 'rope' protecting fundamental liberty. From the perspective of federal regulators or public safety advocates, it operates as a 'snare' or 'tangled_rope' that extracts significant social costs (e.g., in terms of public health and safety) by limiting effective governance. The engine's classification will reflect this divergence based on the structural positions and metrics.
 *
 * DIRECTIONALITY LOGIC:
 *   Individual gun owners and the firearms industry are clear beneficiaries, as the constraint protects their interests and enables commerce (low directionality). Federal and state regulatory agencies are targets, as their power to regulate is curtailed (high directionality). Public safety advocates are excluded, as their policy goals are structurally suppressed by this reading. Constitutional scholars act as observers, analyzing and influencing the interpretation.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    original_intent_ambiguity,
    'To what extent does the ''individual right'' reading accurately reflect the original intent of the Second Amendment''s framers, considering historical context and contemporary understandings of ''militia''?',
    'Further historical and linguistic scholarship, potentially new textual discoveries, or a definitive Supreme Court ruling that re-evaluates historical evidence.',
    'If original intent is definitively found to favor a collective or civic republican reading, the legitimacy of the individual rights interpretation would be severely undermined, potentially leading to a reclassification towards a ''snare'' or ''piton'' for its beneficiaries.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(original_intent_ambiguity, empirical, 'Ambiguity regarding the original intent behind the Second Amendment''s ''right to keep and bear arms'' clause.').

omega_variable(
    social_cost_vs_liberty_valuation,
    'How should the social costs (e.g., gun violence, public health impacts) associated with widespread firearm ownership be weighed against the individual liberty interest protected by this reading?',
    'Societal consensus through democratic processes, legislative action, or a shift in judicial philosophy that explicitly incorporates public welfare considerations into constitutional interpretation.',
    'A higher valuation of social costs could lead to a re-evaluation of the constraint''s extractiveness and suppression, potentially shifting its classification towards a ''snare'' if the costs are deemed disproportionate to the liberty benefit.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(social_cost_vs_liberty_valuation, preference, 'The irreducible tension between individual liberty and collective public safety in the context of firearm ownership.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_arms_right__individual_right_reading, 1791, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(seco_tr_t1791, second_amendment_arms_right__individual_right_reading, theater_ratio, 1791, 0.05).
narrative_ontology:measurement(seco_tr_t1868, second_amendment_arms_right__individual_right_reading, theater_ratio, 1868, 0.08).
narrative_ontology:measurement(seco_tr_t1939, second_amendment_arms_right__individual_right_reading, theater_ratio, 1939, 0.1).
narrative_ontology:measurement(seco_tr_t1980, second_amendment_arms_right__individual_right_reading, theater_ratio, 1980, 0.15).
narrative_ontology:measurement(seco_tr_t2008, second_amendment_arms_right__individual_right_reading, theater_ratio, 2008, 0.18).
narrative_ontology:measurement(seco_tr_t2024, second_amendment_arms_right__individual_right_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(seco_be_t1791, second_amendment_arms_right__individual_right_reading, base_extractiveness, 1791, 0.1).
narrative_ontology:measurement(seco_be_t1868, second_amendment_arms_right__individual_right_reading, base_extractiveness, 1868, 0.2).
narrative_ontology:measurement(seco_be_t1939, second_amendment_arms_right__individual_right_reading, base_extractiveness, 1939, 0.3).
narrative_ontology:measurement(seco_be_t1980, second_amendment_arms_right__individual_right_reading, base_extractiveness, 1980, 0.5).
narrative_ontology:measurement(seco_be_t2008, second_amendment_arms_right__individual_right_reading, base_extractiveness, 2008, 0.65).
narrative_ontology:measurement(seco_be_t2024, second_amendment_arms_right__individual_right_reading, base_extractiveness, 2024, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(seco_su_t1791, second_amendment_arms_right__individual_right_reading, suppression_requirement, 1791, 0.1).
narrative_ontology:measurement(seco_su_t1868, second_amendment_arms_right__individual_right_reading, suppression_requirement, 1868, 0.2).
narrative_ontology:measurement(seco_su_t1939, second_amendment_arms_right__individual_right_reading, suppression_requirement, 1939, 0.3).
narrative_ontology:measurement(seco_su_t1980, second_amendment_arms_right__individual_right_reading, suppression_requirement, 1980, 0.45).
narrative_ontology:measurement(seco_su_t2008, second_amendment_arms_right__individual_right_reading, suppression_requirement, 2008, 0.55).
narrative_ontology:measurement(seco_su_t2024, second_amendment_arms_right__individual_right_reading, suppression_requirement, 2024, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_arms_right__individual_right_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(second_amendment_arms_right__individual_right_reading, federal_firearms_regulation).
narrative_ontology:affects_constraint(second_amendment_arms_right__individual_right_reading, state_gun_control_laws).
narrative_ontology:affects_constraint(second_amendment_arms_right__individual_right_reading, public_safety_funding).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the Second Amendment's right to bear arms. This 'individual_right_reading' emphasizes personal liberty, while 'collective_right_reading' focuses on state militia authority, and 'civic_republican_reading' on armed citizenship for self-governance. Each reading constitutes a separate constraint with different beneficiaries, victims, and classifications.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
