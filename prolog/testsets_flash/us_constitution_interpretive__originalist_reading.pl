% ============================================================================
% CONSTRAINT STORY: us_constitution_interpretive__originalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_constitution_interpretive__originalist_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: us_constitution_interpretive__originalist_reading
 *   human_readable: US Constitution: Originalist Interpretation
 *   domain: constitutional_law/legal_interpretation/political_theory
 *
 * SUMMARY:
 *   This constraint describes the 'originalist_reading' of the US
 *   Constitution, where its meaning is fixed at the time of ratification, and
 *   interpretive authority derives from fidelity to the framers' intent or
 *   the original public meaning of the text. This reading aims to limit
 *   judicial discretion and maintain a stable, historically grounded
 *   constitutional framework. It is a contested interpretation within the
 *   broader field of constitutional law.
 *
 * KEY AGENTS:
 *   - supreme_court_justices: Agenda-setter (institutional/generational) — interprets and applies the originalist framework.
 *   - federalism_advocates: Beneficiary (organized/generational) — benefits from interpretations that limit federal power and preserve state autonomy.
 *   - unenumerated_rights_claimants: Victim (organized/generational) — bears costs from interpretations that restrict rights not explicitly mentioned or historically recognized.
 *   - conservative_legal_movement: Beneficiary (organized/generational) — actively promotes and benefits from the adoption of originalist interpretations.
 *   - social_justice_advocates: Victim (organized/generational) — bears costs from interpretations that hinder progressive social and economic policies.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_constitution_interpretive__originalist_reading, 0.6).
domain_priors:suppression_score(us_constitution_interpretive__originalist_reading, 0.7).
domain_priors:theater_ratio(us_constitution_interpretive__originalist_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_constitution_interpretive__originalist_reading, extractiveness, 0.6).
narrative_ontology:constraint_metric(us_constitution_interpretive__originalist_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(us_constitution_interpretive__originalist_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(us_constitution_interpretive__originalist_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(us_constitution_interpretive__originalist_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_constitution_interpretive__originalist_reading, tangled_rope).
narrative_ontology:human_readable(us_constitution_interpretive__originalist_reading, "US Constitution: Originalist Interpretation").
narrative_ontology:topic_domain(us_constitution_interpretive__originalist_reading, "constitutional_law/legal_interpretation/political_theory").

domain_priors:requires_active_enforcement(us_constitution_interpretive__originalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(us_constitution_interpretive__originalist_reading, '94d9c61a-e33d-49fc-b509-aff36c248ae4').
narrative_ontology:cs_kernel_codification('94d9c61a-e33d-49fc-b509-aff36c248ae4', fixed_text).
narrative_ontology:cs_authority_grounding('94d9c61a-e33d-49fc-b509-aff36c248ae4', lineage).
narrative_ontology:cs_interpretation_layer_present('94d9c61a-e33d-49fc-b509-aff36c248ae4').
narrative_ontology:cs_reading_relation('94d9c61a-e33d-49fc-b509-aff36c248ae4', us_constitution_interpretive__living_constitution_reading, coexists_with).
narrative_ontology:cs_reading_relation('94d9c61a-e33d-49fc-b509-aff36c248ae4', us_constitution_interpretive__popular_constitutionalism_reading, coexists_with).
narrative_ontology:cs_axiom('94d9c61a-e33d-49fc-b509-aff36c248ae4', foundational, constitutional_meaning_fixed_at_ratification).
narrative_ontology:cs_axiom_status(constitutional_meaning_fixed_at_ratification, holdable).
narrative_ontology:cs_axiom_grounding('94d9c61a-e33d-49fc-b509-aff36c248ae4', constitutional_meaning_fixed_at_ratification, deontological).
narrative_ontology:cs_axiom('94d9c61a-e33d-49fc-b509-aff36c248ae4', foundational, judicial_role_limited_to_original_meaning).
narrative_ontology:cs_axiom_status(judicial_role_limited_to_original_meaning, holdable).
narrative_ontology:cs_axiom_grounding('94d9c61a-e33d-49fc-b509-aff36c248ae4', judicial_role_limited_to_original_meaning, deontological).
narrative_ontology:cs_reference_frame('94d9c61a-e33d-49fc-b509-aff36c248ae4', framers_original_intent).
narrative_ontology:cs_drift_state('94d9c61a-e33d-49fc-b509-aff36c248ae4', contemporary_legal_discourse, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('94d9c61a-e33d-49fc-b509-aff36c248ae4', '').
narrative_ontology:cs_kernel_id(us_constitution_interpretive__originalist_reading, us_constitution_interpretive).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_constitution_interpretive__originalist_reading, federalism_advocates).
narrative_ontology:constraint_beneficiary(us_constitution_interpretive__originalist_reading, religious_liberty_claimants_originalist).
narrative_ontology:constraint_beneficiary(us_constitution_interpretive__originalist_reading, property_rights_defenders).
narrative_ontology:constraint_beneficiary(us_constitution_interpretive__originalist_reading, conservative_legal_movement).
narrative_ontology:constraint_victim(us_constitution_interpretive__originalist_reading, unenumerated_rights_claimants).
narrative_ontology:constraint_victim(us_constitution_interpretive__originalist_reading, federal_regulatory_expansion_advocates).
narrative_ontology:constraint_victim(us_constitution_interpretive__originalist_reading, social_justice_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% As the ultimate arbiters of constitutional meaning, they apply and enforce the originalist interpretive method, shaping its evolution and impact. Their adherence to originalism is often a matter of judicial philosophy and political alignment.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__originalist_reading, supreme_court_justices, agenda_setter,
    institutional, generational, constrained, national).

% Benefit from originalist interpretations that limit federal power and preserve the autonomy of states, aligning with their political and ideological goals. They actively lobby for and support originalist judges and legal theories.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__originalist_reading, federalism_advocates, beneficiary,
    organized, generational, mobile, national).

% Often find their claims for religious freedom strengthened by originalist interpretations that emphasize historical understandings of the First Amendment, particularly regarding government non-interference.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__originalist_reading, religious_liberty_claimants_originalist, beneficiary,
    moderate, biographical, constrained, national).

% Benefit from originalist readings that protect property rights based on historical understandings, often limiting government regulation or eminent domain powers.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__originalist_reading, property_rights_defenders, beneficiary,
    organized, generational, mobile, national).

% Bear the costs of originalist interpretations that restrict the recognition of rights not explicitly listed in the Constitution or historically understood, such as privacy rights or certain aspects of bodily autonomy.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__originalist_reading, unenumerated_rights_claimants, payer,
    organized, generational, constrained, national).

% Face limitations on federal government's ability to regulate economic and social issues, as originalism often interprets the Commerce Clause and other grants of power narrowly, hindering modern regulatory efforts.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__originalist_reading, federal_regulatory_expansion_advocates, payer,
    organized, generational, constrained, national).

% Often find their efforts to achieve equality and address systemic injustices impeded by originalist interpretations that prioritize historical understandings over evolving societal norms and needs.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__originalist_reading, social_justice_advocates, payer,
    organized, generational, constrained, national).

% Actively promotes and benefits from the ascendancy of originalist interpretations, viewing it as a means to achieve their broader political and legal objectives. They invest heavily in legal education, judicial appointments, and advocacy.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__originalist_reading, conservative_legal_movement, beneficiary,
    institutional, generational, arbitrage, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(us_constitution_interpretive__originalist_reading, conservative_legal_movement).
narrative_ontology:fixing_cost_class(us_constitution_interpretive__originalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, historically grounded method for interpreting the US Constitution, aiming to limit judicial discretion and ensure fidelity to the original text and intent, thereby coordinating legal expectations across time.
% TRANSFER_FUNCTION: Transfers interpretive authority from contemporary judicial discretion to historical sources (framers' intent, original public meaning), thereby shifting power and benefits to groups whose interests align with those historical understandings, and imposing costs on those seeking modern adaptations.
% ABSENT_VOICES: Advocates for a 'living constitution' or 'popular constitutionalism' are often marginalized in originalist discourse, as their interpretive methods are deemed illegitimate. They would argue for a more dynamic and democratically responsive constitutional meaning.
% DISAPPEARANCE_RATIONALE: If originalist interpretation vanished overnight, the entire landscape of US constitutional law would fundamentally shift. Judicial decisions would likely become more responsive to contemporary societal values, federal power might expand, and new rights could be recognized. The legal and political systems would undergo a profound reorganization.
% FOUNDING_PROBLEM: The problem of ensuring a stable, objective, and non-arbitrary basis for constitutional interpretation, preventing judges from imposing their personal policy preferences under the guise of constitutional law.
% FOUNDING_PROBLEM_CORROBORATION: The problem of judicial activism and the need for interpretive constraint is attested by legal scholars across the ideological spectrum, though they disagree on the solution. The conservative legal movement, in particular, consistently highlights this as a live problem, corroborated by public opinion polls showing concerns about judicial overreach.
narrative_ontology:disappearance_verdict(us_constitution_interpretive__originalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(us_constitution_interpretive__originalist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(us_constitution_interpretive__originalist_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(us_constitution_interpretive__originalist_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_constitution_interpretive__originalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_constitution_interpretive__originalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(us_constitution_interpretive__originalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.6) arises from the constraint's tendency to limit the scope of federal power and individual rights to an 18th-century understanding, which can impose costs on those seeking to adapt the Constitution to modern challenges. Suppression (0.7) is high because this interpretive method actively suppresses alternative readings and judicial discretion, requiring active enforcement by courts and legal scholars. The theater ratio (0.2) is relatively low, as the interpretive method is genuinely applied, though arguments about 'fidelity' can sometimes mask policy preferences. Accessibility collapse (0.4) is moderate, as alternative interpretive methods exist but are actively resisted within the originalist framework. Resistance (0.6) is high, reflecting ongoing legal and political battles over the dominance of originalism.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of originalist proponents (e.g., conservative_legal_movement, some supreme_court_justices), this constraint is a 'rope' that provides stability and fidelity to the founding document, preventing judicial overreach. From the perspective of those advocating for evolving rights or federal regulatory power (e.g., unenumerated_rights_claimants, federal_regulatory_expansion_advocates), it operates as a 'snare' or 'tangled_rope' that extracts benefits for certain groups by suppressing modern interpretations and limiting governmental capacity to address contemporary issues.
 *
 * DIRECTIONALITY LOGIC:
 *   The 'supreme_court_justices' (especially those aligned with originalism) are agenda-setters, benefiting from the perceived legitimacy and stability originalism offers. 'Federalism_advocates' and 'property_rights_defenders' are beneficiaries, as originalist interpretations often align with their goals. 'Unenumerated_rights_claimants' and 'federal_regulatory_expansion_advocates' are victims, as their claims are often curtailed by originalist readings. The 'conservative_legal_movement' is a beneficiary, actively promoting this interpretive method. The 'social_justice_advocates' are victims, as originalism often impedes their policy goals.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate is to provide a stable, objective basis for constitutional interpretation. Mandatrophy is resolved by acknowledging the ongoing contestation over whether this mandate is still 'live' or if the originalist framework has become a tool for specific political outcomes. The high resistance and contested founding problem status indicate that the constraint's function is not universally accepted as serving its original purpose, but rather as a mechanism for power distribution.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    originalism_vs_living_constitution,
    'Is the US Constitution''s meaning fixed at ratification (originalism) or does it evolve with society (living constitutionalism)?',
    'Judicial precedent over time, legislative action, or constitutional amendment reflecting a dominant interpretive philosophy.',
    'If originalism prevails, judicial power is narrowed, federal power is constrained by 1787 understanding, and unenumerated rights are suppressed. If living constitutionalism prevails, judicial power may expand to adapt the Constitution to contemporary issues, potentially benefiting federal regulatory expansion and unenumerated rights.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(originalism_vs_living_constitution, conceptual, 'This constraint is the ''originalist_reading'' of the ''us_constitution_interpretive'' kernel. A ''living_constitution_reading'' would yield different beneficiaries and victims.').

omega_variable(
    original_intent_vs_public_meaning,
    'Within originalism, should interpretation focus on the framers'' subjective intent or the original public meaning of the text?',
    'Scholarly consensus within the originalist movement or a definitive Supreme Court ruling clarifying the preferred methodology.',
    'Focus on framers'' intent might lead to more restrictive interpretations based on historical figures'' specific views, while original public meaning might allow for slightly broader interpretations based on how the text was generally understood at the time. This impacts the scope of rights and powers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(original_intent_vs_public_meaning, conceptual, 'Ambiguity within the originalist framework regarding the precise interpretive method.').

omega_variable(
    originalism_vs_popular_constitutionalism,
    'Is constitutional meaning primarily determined by judicial interpretation (originalism) or by popular political movements and democratic contestation (popular constitutionalism)?',
    'A shift in the balance of power between the judiciary and other branches of government, or a sustained period of popular mobilization that successfully redefines constitutional norms outside of judicial review.',
    'If popular constitutionalism gains ascendancy, the judiciary''s role in defining constitutional meaning would diminish, potentially leading to more fluid and politically responsive interpretations, which could benefit groups currently disfavored by originalist interpretations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(originalism_vs_popular_constitutionalism, conceptual, 'This constraint is the ''originalist_reading'' of the ''us_constitution_interpretive'' kernel. A ''popular_constitutionalism_reading'' would yield different beneficiaries and victims.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_constitution_interpretive__originalist_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(us_c_tr_t0, us_constitution_interpretive__originalist_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(us_c_tr_t10, us_constitution_interpretive__originalist_reading, theater_ratio, 10, 0.15).
narrative_ontology:measurement(us_c_tr_t20, us_constitution_interpretive__originalist_reading, theater_ratio, 20, 0.2).

% Extraction over time
narrative_ontology:measurement(us_c_be_t0, us_constitution_interpretive__originalist_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(us_c_be_t10, us_constitution_interpretive__originalist_reading, base_extractiveness, 10, 0.5).
narrative_ontology:measurement(us_c_be_t20, us_constitution_interpretive__originalist_reading, base_extractiveness, 20, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(us_c_su_t0, us_constitution_interpretive__originalist_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(us_c_su_t10, us_constitution_interpretive__originalist_reading, suppression_requirement, 10, 0.6).
narrative_ontology:measurement(us_c_su_t20, us_constitution_interpretive__originalist_reading, suppression_requirement, 20, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_constitution_interpretive__originalist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(us_constitution_interpretive__originalist_reading, us_federal_power_scope).
narrative_ontology:affects_constraint(us_constitution_interpretive__originalist_reading, us_state_sovereignty).
narrative_ontology:affects_constraint(us_constitution_interpretive__originalist_reading, us_individual_rights_scope).

% DUAL FORMULATION NOTE:
% This constraint is one reading (originalist_reading) of the 'us_constitution_interpretive' kernel. Sibling readings include 'living_constitution_reading' and 'popular_constitutionalism_reading', each with distinct structural properties and classifications.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
