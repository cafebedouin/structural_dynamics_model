% ============================================================================
% CONSTRAINT STORY: permissive_license_text__copyleft_counterfactual_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_permissive_license_text__copyleft_counterfactual_reading, []).

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
 *   constraint_id: permissive_license_text__copyleft_counterfactual_reading
 *   human_readable: Permissive License Text (Copyleft Counterfactual Reading)
 *   domain: software_licensing/intellectual_property/technology_governance
 *
 * SUMMARY:
 *   This constraint story represents the 'copyleft counterfactual reading' of
 *   permissive license text. From this perspective, the absence of a
 *   reciprocity requirement in permissive licenses (like MIT or BSD) is not a
 *   feature but a structural flaw that enables exploitation. The constraint
 *   is the *permissive license text itself*, but interpreted through the lens
 *   of what it *fails to do* (enforce reciprocity), thereby enabling a form
 *   of extraction by proprietary interests. The claimed type is
 *   'tangled_rope' because while permissive licenses do coordinate widespread
 *   reuse, they simultaneously facilitate asymmetric extraction by allowing
 *   proprietary enclosure of derivative works. The metrics reflect this high
 *   extractiveness and the active 'suppression' of alternative, more
 *   reciprocal licensing models.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(permissive_license_text__copyleft_counterfactual_reading, 0.78).
domain_priors:suppression_score(permissive_license_text__copyleft_counterfactual_reading, 0.65).
domain_priors:theater_ratio(permissive_license_text__copyleft_counterfactual_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(permissive_license_text__copyleft_counterfactual_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(permissive_license_text__copyleft_counterfactual_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(permissive_license_text__copyleft_counterfactual_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(permissive_license_text__copyleft_counterfactual_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(permissive_license_text__copyleft_counterfactual_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(permissive_license_text__copyleft_counterfactual_reading, tangled_rope).
narrative_ontology:human_readable(permissive_license_text__copyleft_counterfactual_reading, "Permissive License Text (Copyleft Counterfactual Reading)").
narrative_ontology:topic_domain(permissive_license_text__copyleft_counterfactual_reading, "software_licensing/intellectual_property/technology_governance").

domain_priors:requires_active_enforcement(permissive_license_text__copyleft_counterfactual_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(permissive_license_text__copyleft_counterfactual_reading, '676f0bb6-0e7c-47e3-9af1-192c8f6a6fbc').
narrative_ontology:cs_kernel_codification('676f0bb6-0e7c-47e3-9af1-192c8f6a6fbc', fixed_text).
narrative_ontology:cs_authority_grounding('676f0bb6-0e7c-47e3-9af1-192c8f6a6fbc', practice).
narrative_ontology:cs_interpretation_layer_present('676f0bb6-0e7c-47e3-9af1-192c8f6a6fbc').
narrative_ontology:cs_reading_relation('676f0bb6-0e7c-47e3-9af1-192c8f6a6fbc', permissive_license_text__commons_coordination_reading, coexists_with).
narrative_ontology:cs_reading_relation('676f0bb6-0e7c-47e3-9af1-192c8f6a6fbc', permissive_license_text__corporate_moat_reading, coexists_with).
narrative_ontology:cs_axiom('676f0bb6-0e7c-47e3-9af1-192c8f6a6fbc', foundational, reciprocity_is_essential_for_commons_sustainability).
narrative_ontology:cs_axiom_status(reciprocity_is_essential_for_commons_sustainability, holdable).
narrative_ontology:cs_axiom_grounding('676f0bb6-0e7c-47e3-9af1-192c8f6a6fbc', reciprocity_is_essential_for_commons_sustainability, deontological).
narrative_ontology:cs_axiom('676f0bb6-0e7c-47e3-9af1-192c8f6a6fbc', foundational, unrestricted_privatization_of_collective_work_is_exploitation).
narrative_ontology:cs_axiom_status(unrestricted_privatization_of_collective_work_is_exploitation, holdable).
narrative_ontology:cs_axiom_grounding('676f0bb6-0e7c-47e3-9af1-192c8f6a6fbc', unrestricted_privatization_of_collective_work_is_exploitation, deontological).
narrative_ontology:cs_reference_frame('676f0bb6-0e7c-47e3-9af1-192c8f6a6fbc', copyleft_as_ideal_commons_governance).
narrative_ontology:cs_drift_state('676f0bb6-0e7c-47e3-9af1-192c8f6a6fbc', contemporary_open_source_landscape, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('676f0bb6-0e7c-47e3-9af1-192c8f6a6fbc', '').
narrative_ontology:cs_kernel_id(permissive_license_text__copyleft_counterfactual_reading, permissive_license_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(permissive_license_text__copyleft_counterfactual_reading, copyleft_advocates).
narrative_ontology:constraint_beneficiary(permissive_license_text__copyleft_counterfactual_reading, open_source_foundations).
narrative_ontology:constraint_victim(permissive_license_text__copyleft_counterfactual_reading, proprietary_software_companies).
narrative_ontology:constraint_victim(permissive_license_text__copyleft_counterfactual_reading, derivative_product_developers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from the viral reciprocity of copyleft licenses, which ensures that derivative works remain open. They actively promote and defend copyleft as a necessary mechanism to prevent enclosure of the digital commons. Their 'benefit' is the continued existence and expansion of the copyleft ecosystem, which they see as a counterfactual to the exploitative potential of purely permissive licenses.
narrative_ontology:constraint_stakeholder(permissive_license_text__copyleft_counterfactual_reading, copyleft_advocates, beneficiary,
    organized, generational, constrained, global).

% Administer and enforce copyleft licenses, providing legal backing and community support. They see permissive licenses as a structural weakness that allows proprietary interests to privatize collective effort. Their role is to maintain the integrity of the copyleft ecosystem and advocate for its adoption as an alternative to permissive licensing.
narrative_ontology:constraint_stakeholder(permissive_license_text__copyleft_counterfactual_reading, open_source_foundations, agenda_setter,
    institutional, generational, constrained, global).

% Are 'victimized' by copyleft's viral nature, as it prevents them from incorporating copylefted code into proprietary products without opening their own code. They prefer permissive licenses that allow them to build proprietary derivative works without reciprocity. From this reading's perspective, they are forced to 'pay' by either avoiding copylefted code or by contributing to the commons against their commercial interest.
narrative_ontology:constraint_stakeholder(permissive_license_text__copyleft_counterfactual_reading, proprietary_software_companies, payer,
    powerful, biographical, constrained, global).

% Similar to proprietary companies but with less leverage. They face the choice of using copylefted components and adhering to reciprocity, or avoiding them and potentially missing out on valuable open-source innovation. This reading frames their situation as being forced to choose between contributing to the commons or being excluded from certain technological stacks.
narrative_ontology:constraint_stakeholder(permissive_license_text__copyleft_counterfactual_reading, derivative_product_developers, payer,
    moderate, biographical, constrained, global).

% Prefer permissive licenses for their flexibility and minimal legal overhead, allowing them to use and adapt code without concern for reciprocity. From this copyleft-centric reading, their voice is 'absent' in the debate about the structural exploitation enabled by such licenses, as they do not perceive themselves as contributing to the problem.
narrative_ontology:constraint_stakeholder(permissive_license_text__copyleft_counterfactual_reading, permissive_license_users, excluded,
    moderate, biographical, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The constraint (permissive licensing) coordinates widespread adoption and reuse of software by minimizing legal friction. The counterfactual (copyleft) coordinates the maintenance of a shared, open digital commons by enforcing reciprocity.
% TRANSFER_FUNCTION: Permissive licenses transfer the right to use, modify, and distribute software with minimal obligation, effectively allowing proprietary interests to extract value from open contributions without reciprocal sharing. Copyleft, as the counterfactual, aims to prevent this transfer by enforcing a reciprocal sharing obligation.
% ABSENT_VOICES: Developers and companies who exclusively use permissive licenses and do not perceive any exploitation are absent from this reading's critique. They would argue that permissive licenses foster innovation and freedom, rather than enabling exploitation.
% DISAPPEARANCE_RATIONALE: If the permissive license text (and the implicit lack of reciprocity enforcement) vanished, and only copyleft licenses remained, the entire software ecosystem would fundamentally rearrange. Proprietary development models would be severely challenged, and the digital commons would expand dramatically, forcing a re-evaluation of intellectual property norms.
% FOUNDING_PROBLEM: The original problem permissive licenses aimed to solve was the friction and legal complexity associated with traditional copyright, hindering widespread software adoption and collaboration.
% FOUNDING_PROBLEM_CORROBORATION: Advocates for permissive licenses attest the problem is still live, citing the need for maximum freedom in software reuse. Copyleft advocates (the beneficiaries of this reading) and some legal scholars attest that while the original problem was real, the solution (permissive licensing) created a new problem of enclosure and exploitation, making the founding problem 'dead' in its original framing and replaced by a new, more complex one.
narrative_ontology:disappearance_verdict(permissive_license_text__copyleft_counterfactual_reading, world_rearranges).
narrative_ontology:founding_problem_status(permissive_license_text__copyleft_counterfactual_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(permissive_license_text__copyleft_counterfactual_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(permissive_license_text__copyleft_counterfactual_reading, 'none', 1).
narrative_ontology:epsilon_provenance(permissive_license_text__copyleft_counterfactual_reading, 0.78, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(permissive_license_text__copyleft_counterfactual_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(permissive_license_text__copyleft_counterfactual_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(permissive_license_text__copyleft_counterfactual_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.78) because proprietary companies can take open-source code, add value, and then close off their derivative work, effectively privatizing a public good without fair compensation to the commons. Suppression (0.65) reflects the market dominance of proprietary models and the legal/cultural inertia that favors minimal-obligation licensing, making it harder for copyleft alternatives to gain traction. Theater ratio is low (0.20) because permissive licenses are genuinely functional in facilitating reuse, but their 'permissiveness' is seen as a cover for enabling exploitation. The increasing extractiveness over time reflects the growing awareness and critique from copyleft advocates regarding the long-term effects of permissive licensing on the digital commons.
 *
 * PERSPECTIVAL GAP:
 *   The core perspectival gap is between those who see permissive licenses as maximizing freedom and those (like this reading) who see them as enabling exploitation. The engine's classification will highlight how the same 'text' (permissive license) can be read as a coordination mechanism by some and an extractive mechanism by others, depending on the declared beneficiaries, victims, and the underlying normative axioms.
 *
 * DIRECTIONALITY LOGIC:
 *   Copyleft advocates and open-source foundations are beneficiaries because this reading validates their stance and highlights the necessity of their work. Proprietary software companies and derivative product developers are victims because copyleft, as the counterfactual, imposes obligations that they seek to avoid. Their 'victimization' is the cost of reciprocity that permissive licenses allow them to bypass. Permissive license users are excluded because their perspective, which values minimal obligation, is seen as complicit in the structural exploitation from this reading's viewpoint.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    exploitation_definition_ambiguity,
    'Is the ''uncompensated extraction'' enabled by permissive licenses truly exploitation, or is it a legitimate exercise of freedom to build upon open foundations?',
    'A shift in legal precedent or widely accepted ethical norms regarding the ''enclosure'' of digital commons, or a consensus on the economic value transferred from open to proprietary ecosystems.',
    'If deemed legitimate, the extractiveness metric would be re-evaluated downwards, potentially reclassifying the constraint towards a Rope. If confirmed as exploitation, the Tangled Rope classification would be strengthened, potentially moving towards Snare if the coordination function is deemed negligible.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(exploitation_definition_ambiguity, conceptual, 'Ambiguity in the definition of ''exploitation'' in the context of open-source licensing.').

omega_variable(
    copyleft_necessity_empirical,
    'Is viral reciprocity (copyleft) empirically necessary to sustain the digital commons, or can permissive licensing models also lead to robust, self-sustaining open ecosystems?',
    'Longitudinal studies comparing the health, growth, and contribution patterns of copyleft vs. permissively licensed projects over decades, controlling for other factors.',
    'If permissive models are shown to sustain commons effectively, the ''necessity'' claim of copyleft would weaken, reducing the perceived ''suppression'' of alternatives and potentially lowering extractiveness. If copyleft is empirically validated as essential, the current metrics would be reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(copyleft_necessity_empirical, empirical, 'Empirical evidence for the necessity of copyleft''s viral reciprocity.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(permissive_license_text__copyleft_counterfactual_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(perm_tr_t0, permissive_license_text__copyleft_counterfactual_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(perm_tr_t5, permissive_license_text__copyleft_counterfactual_reading, theater_ratio, 5, 0.12).
narrative_ontology:measurement(perm_tr_t10, permissive_license_text__copyleft_counterfactual_reading, theater_ratio, 10, 0.15).
narrative_ontology:measurement(perm_tr_t15, permissive_license_text__copyleft_counterfactual_reading, theater_ratio, 15, 0.17).
narrative_ontology:measurement(perm_tr_t20, permissive_license_text__copyleft_counterfactual_reading, theater_ratio, 20, 0.19).
narrative_ontology:measurement(perm_tr_t25, permissive_license_text__copyleft_counterfactual_reading, theater_ratio, 25, 0.2).

% Extraction over time
narrative_ontology:measurement(perm_be_t0, permissive_license_text__copyleft_counterfactual_reading, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(perm_be_t5, permissive_license_text__copyleft_counterfactual_reading, base_extractiveness, 5, 0.65).
narrative_ontology:measurement(perm_be_t10, permissive_license_text__copyleft_counterfactual_reading, base_extractiveness, 10, 0.7).
narrative_ontology:measurement(perm_be_t15, permissive_license_text__copyleft_counterfactual_reading, base_extractiveness, 15, 0.74).
narrative_ontology:measurement(perm_be_t20, permissive_license_text__copyleft_counterfactual_reading, base_extractiveness, 20, 0.76).
narrative_ontology:measurement(perm_be_t25, permissive_license_text__copyleft_counterfactual_reading, base_extractiveness, 25, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(perm_su_t0, permissive_license_text__copyleft_counterfactual_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(perm_su_t5, permissive_license_text__copyleft_counterfactual_reading, suppression_requirement, 5, 0.55).
narrative_ontology:measurement(perm_su_t10, permissive_license_text__copyleft_counterfactual_reading, suppression_requirement, 10, 0.6).
narrative_ontology:measurement(perm_su_t15, permissive_license_text__copyleft_counterfactual_reading, suppression_requirement, 15, 0.62).
narrative_ontology:measurement(perm_su_t20, permissive_license_text__copyleft_counterfactual_reading, suppression_requirement, 20, 0.64).
narrative_ontology:measurement(perm_su_t25, permissive_license_text__copyleft_counterfactual_reading, suppression_requirement, 25, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(permissive_license_text__copyleft_counterfactual_reading, information_standard).
narrative_ontology:affects_constraint(permissive_license_text__copyleft_counterfactual_reading, permissive_license_text__commons_coordination_reading).
narrative_ontology:affects_constraint(permissive_license_text__copyleft_counterfactual_reading, permissive_license_text__corporate_moat_reading).

% DUAL FORMULATION NOTE:
% This is one of three readings of the 'permissive license text' kernel. This reading focuses on the exploitative potential enabled by the lack of reciprocity, contrasting with the 'commons coordination' and 'corporate moat' readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
