% ============================================================================
% CONSTRAINT STORY: us_constitution_meaning__originalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_constitution_meaning__originalist_reading, []).

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
 *   constraint_id: us_constitution_meaning__originalist_reading
 *   human_readable: Originalist Constitutional Interpretation Constraint
 *   domain: legal/constitutional
 *
 * SUMMARY:
 *   The originalist reading of the U.S. Constitution asserts that
 *   constitutional meaning is fixed at the moment of ratification (or
 *   amendment), determined by historical public meaning, and not subject to
 *   reinterpretation by judges responding to contemporary circumstances or
 *   evolved social understanding. This reading creates a structural
 *   constraint: judges are bound by historical evidence; contemporary moral
 *   or policy arguments are inadmissible as grounds for constitutional
 *   development. The constraint benefits those (originalist judges,
 *   counter-majoritarian advocates) who want to limit judicial expansion of
 *   rights and federal power. It harms rights claimants whose claims lack
 *   18th-century historical support, who must pursue formal amendment instead
 *   of judicial development. This constraint is ONE reading of a contested
 *   kernel (the meaning of the Constitution itself); the
 *   living-constitutionalist and positivist readings are structurally
 *   different claims, not variations on this one.
 *
 * KEY AGENTS:
 *   - originalist_judicial_coalition: Institutional power holder; sets the frame for legitimate interpretation; benefits from the constraint as a tool to limit their own discretion and limit living-constitutionalist competitors
 *   - counter_majoritarian_constraint_advocates: Organized beneficiaries; use the constraint to block expansive rights interpretations they oppose
 *   - rights_claimants_without_historical_support: Powerless victims; structurally excluded from claiming new rights on constitutional basis; forced to pursue formal amendment
 *   - progressive_constitutional_interpreters: Moderate-power payers; their interpretive framework is delegitimized; they must control judicial appointments to resist the constraint
 *   - supreme_court: The institution through which the constraint operates; composition determines whether originalism dominates
 *   - amendment_process: The escape hatch; requires supermajority consensus, making exit from the constraint extremely difficult
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_constitution_meaning__originalist_reading, 0.68).
domain_priors:suppression_score(us_constitution_meaning__originalist_reading, 0.79).
domain_priors:theater_ratio(us_constitution_meaning__originalist_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_constitution_meaning__originalist_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(us_constitution_meaning__originalist_reading, suppression_requirement, 0.79).
narrative_ontology:constraint_metric(us_constitution_meaning__originalist_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(us_constitution_meaning__originalist_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(us_constitution_meaning__originalist_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_constitution_meaning__originalist_reading, tangled_rope).
narrative_ontology:human_readable(us_constitution_meaning__originalist_reading, "Originalist Constitutional Interpretation Constraint").
narrative_ontology:topic_domain(us_constitution_meaning__originalist_reading, "legal/constitutional").

domain_priors:requires_active_enforcement(us_constitution_meaning__originalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(us_constitution_meaning__originalist_reading, 'f424efd0-de1b-4591-b49e-b7121f8ac367').
narrative_ontology:cs_kernel_codification('f424efd0-de1b-4591-b49e-b7121f8ac367', fixed_text).
narrative_ontology:cs_authority_grounding('f424efd0-de1b-4591-b49e-b7121f8ac367', extraction).
narrative_ontology:cs_interpretation_layer_present('f424efd0-de1b-4591-b49e-b7121f8ac367').
narrative_ontology:cs_reading_relation('f424efd0-de1b-4591-b49e-b7121f8ac367', us_constitution_meaning__living_constitutionalist_reading, forecloses).
narrative_ontology:cs_reading_relation('f424efd0-de1b-4591-b49e-b7121f8ac367', us_constitution_meaning__positivist_reading, coexists_with).
narrative_ontology:cs_axiom('f424efd0-de1b-4591-b49e-b7121f8ac367', foundational, meaning_fixed_at_ratification).
narrative_ontology:cs_axiom_status(meaning_fixed_at_ratification, holdable).
narrative_ontology:cs_axiom_grounding('f424efd0-de1b-4591-b49e-b7121f8ac367', meaning_fixed_at_ratification, conventional).
narrative_ontology:cs_axiom('f424efd0-de1b-4591-b49e-b7121f8ac367', foundational, historical_public_meaning_supremacy).
narrative_ontology:cs_axiom_status(historical_public_meaning_supremacy, holdable).
narrative_ontology:cs_axiom_grounding('f424efd0-de1b-4591-b49e-b7121f8ac367', historical_public_meaning_supremacy, empirically_contingent).
narrative_ontology:cs_reference_frame('f424efd0-de1b-4591-b49e-b7121f8ac367', constitution_as_fixed_text_binding_all_judges).
narrative_ontology:cs_drift_state('f424efd0-de1b-4591-b49e-b7121f8ac367', contemporary_2026, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('f424efd0-de1b-4591-b49e-b7121f8ac367', '').
narrative_ontology:cs_kernel_id(us_constitution_meaning__originalist_reading, us_constitution_meaning).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_constitution_meaning__originalist_reading, originalist_judicial_coalition).
narrative_ontology:constraint_beneficiary(us_constitution_meaning__originalist_reading, counter_majoritarian_constraint_advocates).
narrative_ontology:constraint_victim(us_constitution_meaning__originalist_reading, rights_claimants_without_historical_support).
narrative_ontology:constraint_victim(us_constitution_meaning__originalist_reading, progressive_constitutional_interpreters).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(us_constitution_meaning__originalist_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(us_constitution_meaning__originalist_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_constitution_meaning__originalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_constitution_meaning__originalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(us_constitution_meaning__originalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-to-high (0.68) because the constraint systematically privileges certain constitutional claims (those with historical support) over others, regardless of contemporary justice considerations. This is extraction in the sense that a particular interpretive community (originalists) has monopolized the authority to say what the Constitution means, and they use that monopoly to block interpretations they disfavor. Suppression is high (0.79) because the constraint requires active enforcement: originalist judges must suppress non-originalist reasoning, reject living-constitutionalist arguments as illegitimate, and control what counts as valid constitutional discourse. Theater is moderate (0.42) because the originalist project genuinely aims at constraint (not pure performance), but a growing share of its effort goes to suppressing competing interpretive frameworks rather than to the original positive project of discovering historical meaning. The measurement trajectory shows extractiveness and suppression rising over time as originalism has become a dominant judicial coalition: early originalism was a minority interpretive project with lower extractiveness; contemporary originalism controls the Supreme Court and can impose its frame more completely. Accessibility collapse is high (0.72) because once the originalist frame is accepted, the alternatives are largely closed off within the Court's logic (you must find historical support or pursue amendment — there is no third path). Resistance is moderate (0.58) because living-constitutionalist advocates, civil-rights groups, and progressive scholars mount real resistance, but the institutional structure of the courts gives originalists structural advantage.
 *
 * PERSPECTIVAL GAP:
 *   The originalist coalition perceives this as genuine constraint (fixing meaning to prevent judicial overreach); the rights-claimant victims perceive it as suppression of their legitimate claims. The progressive interpreters perceive it as a power grab masquerading as restraint. The engine should compute these divergences from the structural data: originalists derive low χ from the beneficiary side (they are coordinating on a rule that constrains all judges equally); victims derive high χ from the target side (the constraint systematically forecloses their claims). The perspectival gap is structural, not a matter of opinion.
 *
 * DIRECTIONALITY LOGIC:
 *   Originalist judges and counter-majoritarian advocates are the beneficiaries (d low — the constraint serves their interests and requires their active defense). Rights claimants without historical support are the victims (d high — the constraint systematically forecloses their claims and requires them to achieve supermajority consensus for amendment, a substantially higher bar than judicial development). Progressive interpreters are payers (d moderate-to-high — their interpretive framework is delegitimized and they must invest in controlling judicial appointments to resist the constraint, a constrained exit option). The amendment process remains a theoretical exit (arbitrage-grade for organized advocates with sufficient consensus), but for most rights claimants it is prohibitively expensive.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint genuinely performs a coordination function: it anchors constitutional meaning to a determinate referent, enabling judges to coordinate on a common interpretive method rather than each rewriting the Constitution. But the constraint also performs extraction: by fixing meaning at ratification, it systematically privileges claims that match 18th-century categories and suppresses claims that require evolutionary interpretation. The coordination function and the extraction function are inseparable structurally — you cannot have the constraint's coordination benefit without also accepting its extraction cost. This is the defining feature of tangled rope: genuine coordination WITH asymmetric extraction, requiring active enforcement. The founding problem (judicial overreach without a fixed meaning) is real but contestable — it assumes judges cannot be constrained by other mechanisms (precedent, institutional norms, appointment politics). That contestation is captured by the moderate accessibility collapse (0.72) and moderate resistance (0.58): alternatives are not completely collapsed, and the constraint meets real pushback.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    historical_public_meaning_retrievability,
    'Can historical public meaning be reliably retrieved from 18th-century sources, or is the retrieval process itself interpretively underdetermined, requiring the same kind of scholarly judgment originalism claims to avoid?',
    'Comparative analysis of originalist historical conclusions across different originalist scholars; examination of cases where historical evidence is genuinely ambiguous or contradictory; study of whether originalist and non-originalist historians reach different conclusions from the same sources.',
    'If historical retrieval is genuinely underdetermined, originalism has not escaped interpretive discretion but relocated it to the historical archive. This would reclassify the constraint from a rule-based coordination mechanism toward a more strongly extractive form (interpretation disguised as discovery).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(historical_public_meaning_retrievability, empirical, 'Whether historical public meaning is determinate or interpretively underdetermined.').

omega_variable(
    fixation_moment_selection,
    'Why is ratification (or amendment date) the moment at which meaning is fixed? What about pre-ratification understanding among framers, or immediate post-ratification understanding as the Constitution was implemented?',
    'Foundational originalist texts (Scalia, Balkin, Solum) address this, but the question is whether the choice itself is justified or arbitrary — whether there is a principled basis for ratification-moment fixation or whether it is a conventional choice.',
    'If the fixation moment is conventionally chosen rather than discovered, originalism is structurally less constraining than it claims (the framework contains an embedded discretionary choice). This would support the extraction reading and suggest the constraint''s coordination function is weaker than advertised.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(fixation_moment_selection, conceptual, 'Whether the originalist fixation moment is principled or conventional.').

omega_variable(
    application_meaning_boundary,
    'How clearly can the ''meaning/application'' distinction be maintained? If applying an 18th-century principle to a 21st-century situation requires judgment about what the principle entails in new contexts, have you reintroduced the interpretive discretion originalism claims to avoid?',
    'Case-by-case examination of originalist reasoning in novel situations (e.g., digital privacy, online speech). Analysis of whether originalist judges disagree less about applications than living-constitutionalist judges.',
    'If the meaning/application boundary is permeable and application requires substantial judgment, originalism reduces rather than eliminates judicial discretion. The constraint would be weaker and less coordination-like than claimed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(application_meaning_boundary, empirical, 'Whether the meaning/application distinction contains interpretive discretion.').

omega_variable(
    amendment_as_true_exit,
    'Is the amendment process a genuine exit option for rights claimants whose claims lack historical support, or is the supermajority requirement so stringent that amendment is practically unavailable, making it a theoretical but not real exit?',
    'Historical analysis of amendment success rates; study of what coalitions can achieve amendment (how large, how sustained); comparison with amendment processes in other democracies.',
    'If amendment is practically unavailable, rights claimants are truly trapped by the constraint, and the suppression and extractiveness scores should be higher. If amendment is a viable (though difficult) path, the constraint permits at least one structured exit.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(amendment_as_true_exit, empirical, 'Whether the amendment process functions as a real exit or is practically unavailable.').

omega_variable(
    kernel_reading_boundary,
    'Is originalism a READING of a single kernel (the Constitution''s meaning), or is it a DIFFERENT commitment altogether (to historical authority as the source of legitimacy)? If the latter, it is not one reading among others but a competing framework.',
    'Examination of whether originalists and living-constitutionalists agree they are debating how to interpret the same thing, or whether the disagreement is more fundamental (about what counts as legitimate constitutional authority).',
    'If originalism is a fundamentally different commitment (not a reading of the shared kernel), the constraint family structure breaks down and this constraint does not decompose as a sibling but stands alone. The framework for decomposing kernel readings would require revision.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_boundary, conceptual, 'Whether originalism is a reading of a contested kernel or a fundamentally different commitment.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_constitution_meaning__originalist_reading, 1788, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(us_c_tr_t1788, us_constitution_meaning__originalist_reading, theater_ratio, 1788, 0.05).
narrative_ontology:measurement(us_c_tr_t1870, us_constitution_meaning__originalist_reading, theater_ratio, 1870, 0.08).
narrative_ontology:measurement(us_c_tr_t1950, us_constitution_meaning__originalist_reading, theater_ratio, 1950, 0.15).
narrative_ontology:measurement(us_c_tr_t1980, us_constitution_meaning__originalist_reading, theater_ratio, 1980, 0.25).
narrative_ontology:measurement(us_c_tr_t2005, us_constitution_meaning__originalist_reading, theater_ratio, 2005, 0.38).
narrative_ontology:measurement(us_c_tr_t2026, us_constitution_meaning__originalist_reading, theater_ratio, 2026, 0.42).

% Extraction over time
narrative_ontology:measurement(us_c_be_t1788, us_constitution_meaning__originalist_reading, base_extractiveness, 1788, 0.15).
narrative_ontology:measurement(us_c_be_t1870, us_constitution_meaning__originalist_reading, base_extractiveness, 1870, 0.28).
narrative_ontology:measurement(us_c_be_t1950, us_constitution_meaning__originalist_reading, base_extractiveness, 1950, 0.45).
narrative_ontology:measurement(us_c_be_t1980, us_constitution_meaning__originalist_reading, base_extractiveness, 1980, 0.52).
narrative_ontology:measurement(us_c_be_t2005, us_constitution_meaning__originalist_reading, base_extractiveness, 2005, 0.62).
narrative_ontology:measurement(us_c_be_t2026, us_constitution_meaning__originalist_reading, base_extractiveness, 2026, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(us_c_su_t1788, us_constitution_meaning__originalist_reading, suppression_requirement, 1788, 0.35).
narrative_ontology:measurement(us_c_su_t1870, us_constitution_meaning__originalist_reading, suppression_requirement, 1870, 0.48).
narrative_ontology:measurement(us_c_su_t1950, us_constitution_meaning__originalist_reading, suppression_requirement, 1950, 0.62).
narrative_ontology:measurement(us_c_su_t1980, us_constitution_meaning__originalist_reading, suppression_requirement, 1980, 0.7).
narrative_ontology:measurement(us_c_su_t2005, us_constitution_meaning__originalist_reading, suppression_requirement, 2005, 0.75).
narrative_ontology:measurement(us_c_su_t2026, us_constitution_meaning__originalist_reading, suppression_requirement, 2026, 0.79).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_constitution_meaning__originalist_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(us_constitution_meaning__originalist_reading, 0.18).
narrative_ontology:affects_constraint(us_constitution_meaning__originalist_reading, us_constitution_meaning__living_constitutionalist_reading).
narrative_ontology:affects_constraint(us_constitution_meaning__originalist_reading, us_constitution_meaning__positivist_reading).

% DUAL FORMULATION NOTE:
% The U.S. Constitution's meaning is a contested kernel instantiated in three structurally distinct constraint stories: originalist reading (fixed meaning at ratification), living-constitutionalist reading (evolutionary meaning), and positivist reading (validity from formal authority). Each story has a different ε, different beneficiary/victim structure, and different type. They are not the same constraint viewed from different angles; they are different constraints that share a common kernel. This story instantiates the originalist reading only. The sibling readings are separate constraint stories linked via network.affects_constraints. The floor override reflects that enforcement mechanisms typically have higher inherent coordination cost (0.10 baseline); originalism's enforcement cost is higher because suppressing non-originalist discourse requires sustained institutional effort and boundary policing, justifying the 0.18 override.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(us_constitution_meaning__originalist_reading, powerless, 0.92).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
