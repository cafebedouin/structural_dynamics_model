% ============================================================================
% CONSTRAINT STORY: second_amendment_scope__individual_right_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_second_amendment_scope__individual_right_reading, []).

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
 *   constraint_id: second_amendment_scope__individual_right_reading
 *   human_readable: Second Amendment: Individual Right to Bear Arms
 *   domain: constitutional_law/rights_jurisprudence
 *
 * SUMMARY:
 *   This constraint story instantiates the 'individual right' reading of the
 *   Second Amendment, which holds that the right to keep and bear arms is an
 *   individual right unconnected to militia service. This interpretation,
 *   solidified by Supreme Court decisions like Heller (2008) and McDonald
 *   (2010), significantly constrains state and federal governments' ability
 *   to regulate firearms. While proponents frame it as protecting a
 *   fundamental liberty, opponents view it as extracting public safety and
 *   regulatory capacity.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_scope__individual_right_reading, 0.75).
domain_priors:suppression_score(second_amendment_scope__individual_right_reading, 0.6).
domain_priors:theater_ratio(second_amendment_scope__individual_right_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_scope__individual_right_reading, extractiveness, 0.75).
narrative_ontology:constraint_metric(second_amendment_scope__individual_right_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(second_amendment_scope__individual_right_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_scope__individual_right_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(second_amendment_scope__individual_right_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_scope__individual_right_reading, tangled_rope).
narrative_ontology:human_readable(second_amendment_scope__individual_right_reading, "Second Amendment: Individual Right to Bear Arms").
narrative_ontology:topic_domain(second_amendment_scope__individual_right_reading, "constitutional_law/rights_jurisprudence").

domain_priors:requires_active_enforcement(second_amendment_scope__individual_right_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_scope__individual_right_reading, 'bb62abc8-0a31-4228-9762-75770081c9b6').
narrative_ontology:cs_kernel_codification('bb62abc8-0a31-4228-9762-75770081c9b6', fixed_text).
narrative_ontology:cs_authority_grounding('bb62abc8-0a31-4228-9762-75770081c9b6', lineage).
narrative_ontology:cs_interpretation_layer_present('bb62abc8-0a31-4228-9762-75770081c9b6').
narrative_ontology:cs_reading_relation('bb62abc8-0a31-4228-9762-75770081c9b6', second_amendment_scope__collective_right_reading, forecloses).
narrative_ontology:cs_reading_relation('bb62abc8-0a31-4228-9762-75770081c9b6', second_amendment_scope__civic_right_reading, forecloses).
narrative_ontology:cs_axiom('bb62abc8-0a31-4228-9762-75770081c9b6', foundational, individual_self_defense_fundamental).
narrative_ontology:cs_axiom_status(individual_self_defense_fundamental, holdable).
narrative_ontology:cs_axiom_grounding('bb62abc8-0a31-4228-9762-75770081c9b6', individual_self_defense_fundamental, deontological).
narrative_ontology:cs_axiom('bb62abc8-0a31-4228-9762-75770081c9b6', foundational, militia_clause_prefatory).
narrative_ontology:cs_axiom_status(militia_clause_prefatory, holdable).
narrative_ontology:cs_axiom_grounding('bb62abc8-0a31-4228-9762-75770081c9b6', militia_clause_prefatory, conventional).
narrative_ontology:cs_reference_frame('bb62abc8-0a31-4228-9762-75770081c9b6', original_individual_right_1791).
narrative_ontology:cs_drift_state('bb62abc8-0a31-4228-9762-75770081c9b6', post_heller_mcdonald_era, gap(axiom_overriding, severe, true)).
narrative_ontology:cs_created_at('bb62abc8-0a31-4228-9762-75770081c9b6', '').
narrative_ontology:cs_kernel_id(second_amendment_scope__individual_right_reading, second_amendment_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_scope__individual_right_reading, firearms_owners).
narrative_ontology:constraint_beneficiary(second_amendment_scope__individual_right_reading, firearms_manufacturers).
narrative_ontology:constraint_beneficiary(second_amendment_scope__individual_right_reading, gun_rights_advocates).
narrative_ontology:constraint_victim(second_amendment_scope__individual_right_reading, state_legislatures).
narrative_ontology:constraint_victim(second_amendment_scope__individual_right_reading, victims_of_gun_violence).
narrative_ontology:constraint_victim(second_amendment_scope__individual_right_reading, public_safety_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from the legal protection of their right to own firearms for various purposes, including self-defense, sport, and collection, without a direct connection to militia service. Their ability to acquire and possess firearms is largely unburdened by state-level restrictions.
narrative_ontology:constraint_stakeholder(second_amendment_scope__individual_right_reading, firearms_owners, beneficiary,
    organized, biographical, constrained, national).

% Profit from an expanded market for firearms and accessories, as the individual right interpretation limits state and federal regulatory power over sales and types of weapons. They actively lobby to maintain and strengthen this interpretation.
narrative_ontology:constraint_stakeholder(second_amendment_scope__individual_right_reading, firearms_manufacturers, beneficiary,
    institutional, generational, arbitrage, global).

% Actively champion and defend the individual right interpretation through litigation, lobbying, and public education. They benefit from the legal victories that enshrine this reading and expand its scope, shaping the legal and political landscape.
narrative_ontology:constraint_stakeholder(second_amendment_scope__individual_right_reading, gun_rights_advocates, agenda_setter,
    organized, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(second_amendment_scope__individual_right_reading, gun_rights_advocates, beneficiary).

% Bear the costs of limited regulatory authority over firearms within their jurisdictions, often facing legal challenges when attempting to enact stricter gun control measures. They are constrained by federal court rulings upholding the individual right.
narrative_ontology:constraint_stakeholder(second_amendment_scope__individual_right_reading, state_legislatures, payer,
    institutional, immediate, constrained, national).

% Bear the direct and indirect costs of gun violence, including physical harm, psychological trauma, and loss of life. Their ability to seek legislative remedies for gun violence is significantly hampered by the individual right interpretation.
narrative_ontology:constraint_stakeholder(second_amendment_scope__individual_right_reading, victims_of_gun_violence, payer,
    powerless, immediate, trapped, local).

% Work to reduce gun violence and improve public safety, but face significant legal and political obstacles due to the individual right interpretation. They bear the cost of diminished legislative options and prolonged legal battles.
narrative_ontology:constraint_stakeholder(second_amendment_scope__individual_right_reading, public_safety_advocates, payer,
    organized, biographical, constrained, national).

% As the ultimate arbiter of constitutional meaning, the Supreme Court has established and continues to refine the individual right interpretation, setting precedents that bind lower courts and legislatures. Its decisions are the primary enforcement mechanism for this constraint.
narrative_ontology:constraint_stakeholder(second_amendment_scope__individual_right_reading, supreme_court, agenda_setter,
    institutional, civilizational, analytical, national).

% Advocate for an interpretation where the Second Amendment protects state authority to maintain militias, not individual ownership rights. Their view has been largely superseded by the individual right reading, effectively excluding them from the dominant legal discourse.
narrative_ontology:constraint_stakeholder(second_amendment_scope__individual_right_reading, collective_right_proponents, excluded,
    organized, generational, constrained, national).

% Argue for an individual right conditioned on civic militia participation. While closer to the individual right, their emphasis on militia connection is rejected by the dominant interpretation, placing them outside the prevailing legal framework.
narrative_ontology:constraint_stakeholder(second_amendment_scope__individual_right_reading, civic_right_proponents, excluded,
    organized, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(second_amendment_scope__individual_right_reading, firearms_manufacturers).
narrative_ontology:fixing_cost_class(second_amendment_scope__individual_right_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a clear, judicially enforced standard for individual firearms ownership across jurisdictions, preventing arbitrary state infringement and ensuring a uniform understanding of the right.
% TRANSFER_FUNCTION: Transfers significant regulatory power from state and local governments to individual citizens, and shifts the costs associated with gun violence and public safety measures from individual gun owners to the broader public and state resources.
% ABSENT_VOICES: Proponents of the collective right and civic right readings are structurally excluded from the dominant interpretation, as their views are legally superseded by Supreme Court precedent. They would argue for a more limited scope of the right, emphasizing state regulatory power or militia connection.
% DISAPPEARANCE_RATIONALE: If this interpretation vanished overnight, state and local governments would immediately move to enact stricter gun control laws, fundamentally altering the landscape of firearms ownership, sales, and public safety. The legal and political battles would shift dramatically.
% FOUNDING_PROBLEM: To ensure individuals could possess arms for self-defense and to resist potential tyranny, reflecting a post-revolutionary concern for individual liberty and a distrust of centralized power, as well as the practical need for a citizenry capable of self-defense.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars and historians outside of gun rights advocacy groups attest to the historical context of individual self-defense and resistance to tyranny, though they often dispute the scope and connection to militia service. Public safety advocates contest the contemporary relevance of the 'tyranny' aspect.
narrative_ontology:disappearance_verdict(second_amendment_scope__individual_right_reading, world_rearranges).
narrative_ontology:founding_problem_status(second_amendment_scope__individual_right_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_scope__individual_right_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(second_amendment_scope__individual_right_reading, 'none', 1).
narrative_ontology:epsilon_provenance(second_amendment_scope__individual_right_reading, 0.75, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_scope__individual_right_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(second_amendment_scope__individual_right_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(second_amendment_scope__individual_right_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.75) is high because this interpretation broadly covers firearms ownership, imposing strict scrutiny on regulatory attempts and effectively extracting the capacity for states to enact desired gun control measures. Suppression (0.60) is moderate, reflecting the active judicial enforcement that strikes down restrictive laws. Theater ratio (0.15) is low, as the right is actively asserted and defended, with real-world consequences for policy and public safety. Accessibility collapse (0.60) is moderate, as it significantly limits legislative alternatives for gun control. Resistance (0.30) is moderate-low, as this interpretation is widely accepted in legal precedent, though politically contested.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of firearms owners and gun rights advocates, this constraint is a vital protection of a fundamental right, ensuring individual liberty. From the perspective of state legislatures and public safety advocates, it operates as a significant extraction of their ability to protect citizens and regulate dangerous weapons, imposing substantial societal costs.
 *
 * DIRECTIONALITY LOGIC:
 *   Firearms owners, manufacturers, and gun rights advocates are clear beneficiaries, gaining legal protection and market access. State legislatures, victims of gun violence, and public safety advocates are targets, bearing the costs of restricted regulatory power and increased societal risk. The Supreme Court acts as the primary agenda-setter, enforcing this interpretation.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate, as interpreted by this reading, is actively asserted and defended, so mandatrophy is not resolved. The founding problem of individual self-defense and resistance to tyranny is contested in its contemporary relevance, but the constraint persists with high extractiveness, suggesting a potential for the original coordination function to be overshadowed by rent-seeking (in the form of expanded market access for manufacturers and unburdened ownership for some individuals, at the cost of public safety).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    historical_intent_ambiguity,
    'To what extent does the ''individual right'' interpretation accurately reflect the original public meaning and historical intent of the Second Amendment, particularly regarding the connection to militia service?',
    'Further historical and legal scholarship, potentially new archival discoveries, or a re-evaluation of existing evidence by a future Supreme Court.',
    'If the historical record strongly supports a militia-connected or collective right, the legitimacy of the individual right reading would be undermined, potentially leading to a reclassification towards a more limited scope and lower extractiveness from state regulatory power.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(historical_intent_ambiguity, empirical, 'Ambiguity regarding the historical grounding of the individual right unconnected to militia service.').

omega_variable(
    public_safety_cost_attribution,
    'What is the quantifiable societal cost (e.g., gun violence, public health burden) directly attributable to the broad scope of the individual right interpretation, as opposed to other factors?',
    'Longitudinal epidemiological studies, comparative analyses of gun violence rates in jurisdictions with differing regulatory regimes, and economic modeling of public health impacts.',
    'Clear attribution of high societal costs would strengthen arguments for re-evaluating the constraint''s balance between individual liberty and public safety, potentially leading to a reclassification with higher effective extraction from the public good.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(public_safety_cost_attribution, empirical, 'Uncertainty in attributing public safety costs directly to the individual right interpretation.').

omega_variable(
    judicial_activism_vs_interpretation,
    'Is the Supreme Court''s establishment and expansion of the individual right interpretation an act of judicial activism, or a faithful and evolving interpretation of constitutional text and principles?',
    'This is a conceptual and preference-based question, unlikely to be resolved empirically. It depends on one''s jurisprudential philosophy (e.g., originalism vs. living constitutionalism) and political values.',
    'Resolution would primarily affect the perceived legitimacy of the constraint among different legal and political factions, influencing political will for legislative or constitutional challenges, rather than directly altering its structural classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(judicial_activism_vs_interpretation, conceptual, 'Debate over the nature of judicial action in defining the individual right.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_scope__individual_right_reading, 1970, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(seco_tr_t1970, second_amendment_scope__individual_right_reading, theater_ratio, 1970, 0.1).
narrative_ontology:measurement(seco_tr_t1985, second_amendment_scope__individual_right_reading, theater_ratio, 1985, 0.12).
narrative_ontology:measurement(seco_tr_t2000, second_amendment_scope__individual_right_reading, theater_ratio, 2000, 0.14).
narrative_ontology:measurement(seco_tr_t2008, second_amendment_scope__individual_right_reading, theater_ratio, 2008, 0.15).
narrative_ontology:measurement(seco_tr_t2016, second_amendment_scope__individual_right_reading, theater_ratio, 2016, 0.15).
narrative_ontology:measurement(seco_tr_t2024, second_amendment_scope__individual_right_reading, theater_ratio, 2024, 0.15).

% Extraction over time
narrative_ontology:measurement(seco_be_t1970, second_amendment_scope__individual_right_reading, base_extractiveness, 1970, 0.45).
narrative_ontology:measurement(seco_be_t1985, second_amendment_scope__individual_right_reading, base_extractiveness, 1985, 0.55).
narrative_ontology:measurement(seco_be_t2000, second_amendment_scope__individual_right_reading, base_extractiveness, 2000, 0.65).
narrative_ontology:measurement(seco_be_t2008, second_amendment_scope__individual_right_reading, base_extractiveness, 2008, 0.7).
narrative_ontology:measurement(seco_be_t2016, second_amendment_scope__individual_right_reading, base_extractiveness, 2016, 0.73).
narrative_ontology:measurement(seco_be_t2024, second_amendment_scope__individual_right_reading, base_extractiveness, 2024, 0.75).

% Suppression requirement over time
narrative_ontology:measurement(seco_su_t1970, second_amendment_scope__individual_right_reading, suppression_requirement, 1970, 0.35).
narrative_ontology:measurement(seco_su_t1985, second_amendment_scope__individual_right_reading, suppression_requirement, 1985, 0.45).
narrative_ontology:measurement(seco_su_t2000, second_amendment_scope__individual_right_reading, suppression_requirement, 2000, 0.55).
narrative_ontology:measurement(seco_su_t2008, second_amendment_scope__individual_right_reading, suppression_requirement, 2008, 0.58).
narrative_ontology:measurement(seco_su_t2016, second_amendment_scope__individual_right_reading, suppression_requirement, 2016, 0.59).
narrative_ontology:measurement(seco_su_t2024, second_amendment_scope__individual_right_reading, suppression_requirement, 2024, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_scope__individual_right_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(second_amendment_scope__individual_right_reading, state_gun_control_laws).
narrative_ontology:affects_constraint(second_amendment_scope__individual_right_reading, federal_firearms_regulations).

% DUAL FORMULATION NOTE:
% This is one reading of the 'second_amendment_scope' kernel. Other readings include 'collective_right_reading' and 'civic_right_reading', which offer alternative interpretations of the Second Amendment's scope and purpose.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
