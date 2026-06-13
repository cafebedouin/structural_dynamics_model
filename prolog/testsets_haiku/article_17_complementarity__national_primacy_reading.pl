% ============================================================================
% CONSTRAINT STORY: article_17_complementarity__national_primacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article_17_complementarity__national_primacy_reading, []).

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
 *   constraint_id: article_17_complementarity__national_primacy_reading
 *   human_readable: Article 17 Complementarity: National Primacy Reading
 *   domain: international_law/criminal_justice
 *
 * SUMMARY:
 *   Article 17 of the Rome Statute codifies the principle of complementarity:
 *   the ICC has jurisdiction only when national courts are 'unwilling or
 *   unable' to prosecute. The national primacy reading interprets this as a
 *   presumption in favor of state adequacy, placing a high evidentiary burden
 *   on the ICC to demonstrate that proceedings are a sham. This reading
 *   benefits national judiciaries and sovereignty-maximizing states by
 *   restricting international oversight. Victims in states with
 *   weak-but-genuine proceedings—and marginalized populations prosecuted
 *   selectively within functioning systems—bear the costs of this
 *   interpretation. The constraint is claimed as tangled_rope (real
 *   coordination function: preserving legitimate state sovereignty) paired
 *   with substantial extraction (beneficiaries are national governments;
 *   victims are excluded populations). The measurement series tracks rising
 *   extractiveness as the reading has become entrenched in ICC practice, and
 *   rising theater as invocations of national adequacy become increasingly
 *   performative.
 *
 * KEY AGENTS:
 *   - national_judiciaries: institutional beneficiaries maintaining presumptive primacy
 *   - sovereignty_maximizing_states: institutional beneficiaries shielded from ICC scrutiny
 *   - de_facto_regimes: powerful beneficiaries exploiting flexible 'genuine proceedings' standard
 *   - victims_in_weak_but_genuine_proceedings: powerless targets excluded by high inadmissibility threshold
 *   - marginalized_populations_in_functioning_states: identity-locked targets bound to selective systems
 *   - icc_prosecutors: institutional payers bearing the burden of proving sham
 *   - international_human_rights_bodies: analytical observers reporting on exclusions
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_17_complementarity__national_primacy_reading, 0.68).
domain_priors:suppression_score(article_17_complementarity__national_primacy_reading, 0.72).
domain_priors:theater_ratio(article_17_complementarity__national_primacy_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_17_complementarity__national_primacy_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(article_17_complementarity__national_primacy_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(article_17_complementarity__national_primacy_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_17_complementarity__national_primacy_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(article_17_complementarity__national_primacy_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_17_complementarity__national_primacy_reading, tangled_rope).
narrative_ontology:human_readable(article_17_complementarity__national_primacy_reading, "Article 17 Complementarity: National Primacy Reading").
narrative_ontology:topic_domain(article_17_complementarity__national_primacy_reading, "international_law/criminal_justice").

domain_priors:requires_active_enforcement(article_17_complementarity__national_primacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_17_complementarity__national_primacy_reading, 'd14251a3-f207-41ab-a7a3-0758e72d2f68').
narrative_ontology:cs_kernel_codification('d14251a3-f207-41ab-a7a3-0758e72d2f68', fixed_text).
narrative_ontology:cs_authority_grounding('d14251a3-f207-41ab-a7a3-0758e72d2f68', extraction).
narrative_ontology:cs_interpretation_layer_present('d14251a3-f207-41ab-a7a3-0758e72d2f68').
narrative_ontology:cs_reading_relation('d14251a3-f207-41ab-a7a3-0758e72d2f68', article_17_complementarity__international_oversight_reading, coexists_with).
narrative_ontology:cs_axiom('d14251a3-f207-41ab-a7a3-0758e72d2f68', foundational, state_judicial_primacy_presumption).
narrative_ontology:cs_axiom_status(state_judicial_primacy_presumption, holdable).
narrative_ontology:cs_axiom_grounding('d14251a3-f207-41ab-a7a3-0758e72d2f68', state_judicial_primacy_presumption, deontological).
narrative_ontology:cs_axiom('d14251a3-f207-41ab-a7a3-0758e72d2f68', foundational, burden_of_proof_on_icc_to_demonstrate_inadequacy).
narrative_ontology:cs_axiom_status(burden_of_proof_on_icc_to_demonstrate_inadequacy, holdable).
narrative_ontology:cs_axiom_grounding('d14251a3-f207-41ab-a7a3-0758e72d2f68', burden_of_proof_on_icc_to_demonstrate_inadequacy, conventional).
narrative_ontology:cs_reference_frame('d14251a3-f207-41ab-a7a3-0758e72d2f68', state_sovereignty_primacy_framework).
narrative_ontology:cs_drift_state('d14251a3-f207-41ab-a7a3-0758e72d2f68', contemporary_human_rights_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('d14251a3-f207-41ab-a7a3-0758e72d2f68', '').
narrative_ontology:cs_kernel_id(article_17_complementarity__national_primacy_reading, article_17_complementarity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_17_complementarity__national_primacy_reading, national_judiciaries).
narrative_ontology:constraint_beneficiary(article_17_complementarity__national_primacy_reading, sovereignty_maximizing_states).
narrative_ontology:constraint_beneficiary(article_17_complementarity__national_primacy_reading, de_facto_regimes).
narrative_ontology:constraint_victim(article_17_complementarity__national_primacy_reading, victims_in_weak_but_genuine_proceedings).
narrative_ontology:constraint_victim(article_17_complementarity__national_primacy_reading, marginalized_populations_in_functioning_states).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_17_complementarity__national_primacy_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(article_17_complementarity__national_primacy_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article_17_complementarity__national_primacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(article_17_complementarity__national_primacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(article_17_complementarity__national_primacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.68) because the constraint systematically excludes victim access to international remedy based on a presumption that privileges state institutions over victim agency. The burden-shifting from state to ICC creates asymmetric extraction: beneficiary states face a near-impossible evidentiary bar to challenge (proving sham requires bad faith, not inadequacy). Suppression is high (0.72) because the constraint's enforcement depends on maintaining the presumption of adequacy despite mounting evidence of selective prosecution and weak capacity in many states. Theater is moderate-high (0.48) and rising: invocations of 'national proceedings' in cases of clear elite immunity and selective prosecution are increasingly performative. The measurement series shows extractiveness stabilizing at 0.68 around year 20, theater plateauing at 0.48—suggesting the reading has reached its steady-state extraction level and is now maintained primarily through institutional inertia and performative reference to sovereignty.
 *
 * PERSPECTIVAL GAP:
 *   From the national judiciary seat, the arrangement is legitimate coordination—a principled preservation of state sovereignty and institutional responsibility. From the victim seat (particularly victims in weak-but-genuine systems), the same constraint operates as enforced exclusion from remedy. From the ICC prosecutor seat, the reading imposes a burden-of-proof asymmetry that protects elite defendants. The engine computes these seat-specific types from the structural data: the beneficiary seats likely compute as rope or tangled_rope coordination; the victim seats likely compute as snare or tangled_rope extraction. Seat divergence is the point of the framework—the same constraint does different work at different seats.
 *
 * DIRECTIONALITY LOGIC:
 *   National beneficiaries have arbitrage exit: they can claim proceedings are adequate and sidestep ICC jurisdiction, or they can cooperate selectively and maintain control. Victims have trapped exit: they cannot challenge the adequacy presumption without proving sham, a nearly impossible standard. ICC prosecutors have constrained exit: they are obligated by Rome Statute to respect complementarity but bear the burden of proving its conditions are met. The beneficiary/victim structure is stark: beneficiaries (national states and judiciaries) are institutional; victims are predominantly powerless or identity-locked populations.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem—preserving legitimate state sovereignty against ICC override—is real and live for state actors. But the mechanism has calcified: states use Article 17 presumption of adequacy as a shield against accountability even when their proceedings are transparent covers for elite immunity. The extraction component (denial of remedy to victims) has grown relative to the coordination component (legitimate sovereignty protection) over time, as evidenced by rising theater_ratio. This is a candidate for mandatrophy: the founding function (sovereignty preservation) persists, but the mechanism has become increasingly performative in its deployment. Victims are excluded not because states genuinely need sovereignty space but because the presumption of adequacy makes exclusion cheap for beneficiary states. The theater rise suggests the constraint is increasingly maintained through institutional narrative (invoking sovereignty) rather than genuine coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sham_proceeding_evidentiary_burden,
    'What standard of evidence is sufficient to prove national proceedings are a sham under Article 17? Is bad faith required, or does systematic inadequacy suffice?',
    'ICC Pre-Trial Chamber and Appeals jurisprudence over time; academic analysis of admissibility decisions; comparative review of cases admitted vs. ruled inadmissible on complementarity grounds.',
    'If bad faith is required, the presumption of adequacy is nearly irrefutable and victims in selective prosecution systems remain excluded. If systematic inadequacy suffices, the threshold drops and more cases become admissible. The reading''s extractiveness depends on this interpretation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sham_proceeding_evidentiary_burden, conceptual, 'Whether the sham standard is bad faith (near-impossible) or systematic inadequacy (achievable)').

omega_variable(
    genuine_proceedings_vs_performative_proceedings,
    'How does one distinguish ''genuine but weak'' proceedings (which satisfy complementarity under this reading) from ''performative'' proceedings (which might not)? What threshold separates the two?',
    'Comparative empirical analysis of state capacity, prosecutorial independence, and case outcomes across jurisdictions; mapping of which cases states present as ''proceedings'' and how ICC Pre-Trial Chambers evaluate their genuineness.',
    'If the distinction is drawn tightly, more cases are ruled non-sham and victims are excluded. If the distinction requires actual independence and capacity, more cases become admissible. Beneficiary states have strong incentive to claim genuine proceedings; the ambiguity allows them to exploit it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(genuine_proceedings_vs_performative_proceedings, empirical, 'Empirical threshold separating weak-but-genuine from performative proceedings').

omega_variable(
    sovereignty_doctrine_vs_accountability_doctrine_reading_choice,
    'Is Article 17 primarily a sovereignty-protection mechanism (this reading) or primarily an accountability-trigger mechanism (international oversight reading)? What authority grounding justifies one reading over the other?',
    'Rome Statute negotiating history; preamble language and state intent; subsequent practice in state ratification and Assembly of States Parties declarations; ICC case law and preliminary examination decisions.',
    'If the sovereignty reading is grounded in state intent (lineage authority), the burden of proof allocation favors beneficiary states. If the accountability reading is grounded in victims'' rights doctrine (deontological), the burden should shift to the state. This is the core contested framing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sovereignty_doctrine_vs_accountability_doctrine_reading_choice, conceptual, 'Whether Article 17 is read as sovereignty-protection or accountability-trigger').

omega_variable(
    elite_capture_in_proceeding_adequacy_assessment,
    'Does systematic selective prosecution of lower-level perpetrators while elite officials escape prosecution count as sham proceedings, or is the state still deemed adequate because proceedings exist and function?',
    'Comparative analysis of conviction patterns by defendant rank in state court cases; case-by-case assessment of whether elite immunity is masked by functioning lower-level prosecutions.',
    'If selective immunity at the elite level is compatible with ''genuine proceedings,'' victims of crimes by elites remain excluded from ICC remedy. If selective immunity triggers the sham finding, many more cases become admissible and extractiveness drops.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(elite_capture_in_proceeding_adequacy_assessment, empirical, 'Whether selective prosecution of non-elite perpetrators satisfies adequacy despite elite immunity').

omega_variable(
    reading_foreclosure_via_axiom_overriding,
    'As empirical evidence accumulates showing systematic elite immunity in ''adequate'' state proceedings, does this axiom-override (empirical challenge to the state-capacity assumption underlying this reading) eventually foreclose the national primacy reading altogether, or does the reading adapt by redefining the sham threshold upward?',
    'Long-term observation of ICC jurisprudence and state practice; assessment of whether Pre-Trial Chamber decisions increasingly admit cases despite state claims of adequate proceedings; or whether states successfully raise the sham threshold to match evidence.',
    'Foreclosure would shift the constraint to international oversight reading. Adaptation (threshold-raising) would maintain this reading but increase its extractiveness. The reading''s persistence depends on this dynamic.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_foreclosure_via_axiom_overriding, conceptual, 'Whether empirical axiom-override eventually forecloses this reading or triggers threshold adaptation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_17_complementarity__national_primacy_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t0, article_17_complementarity__national_primacy_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(arti_tr_t5, article_17_complementarity__national_primacy_reading, theater_ratio, 5, 0.4).
narrative_ontology:measurement(arti_tr_t10, article_17_complementarity__national_primacy_reading, theater_ratio, 10, 0.44).
narrative_ontology:measurement(arti_tr_t15, article_17_complementarity__national_primacy_reading, theater_ratio, 15, 0.47).
narrative_ontology:measurement(arti_tr_t20, article_17_complementarity__national_primacy_reading, theater_ratio, 20, 0.48).
narrative_ontology:measurement(arti_tr_t25, article_17_complementarity__national_primacy_reading, theater_ratio, 25, 0.48).

% Extraction over time
narrative_ontology:measurement(arti_be_t0, article_17_complementarity__national_primacy_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(arti_be_t5, article_17_complementarity__national_primacy_reading, base_extractiveness, 5, 0.6).
narrative_ontology:measurement(arti_be_t10, article_17_complementarity__national_primacy_reading, base_extractiveness, 10, 0.65).
narrative_ontology:measurement(arti_be_t15, article_17_complementarity__national_primacy_reading, base_extractiveness, 15, 0.67).
narrative_ontology:measurement(arti_be_t20, article_17_complementarity__national_primacy_reading, base_extractiveness, 20, 0.68).
narrative_ontology:measurement(arti_be_t25, article_17_complementarity__national_primacy_reading, base_extractiveness, 25, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t0, article_17_complementarity__national_primacy_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(arti_su_t5, article_17_complementarity__national_primacy_reading, suppression_requirement, 5, 0.62).
narrative_ontology:measurement(arti_su_t10, article_17_complementarity__national_primacy_reading, suppression_requirement, 10, 0.66).
narrative_ontology:measurement(arti_su_t15, article_17_complementarity__national_primacy_reading, suppression_requirement, 15, 0.7).
narrative_ontology:measurement(arti_su_t20, article_17_complementarity__national_primacy_reading, suppression_requirement, 20, 0.72).
narrative_ontology:measurement(arti_su_t25, article_17_complementarity__national_primacy_reading, suppression_requirement, 25, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_17_complementarity__national_primacy_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(article_17_complementarity__national_primacy_reading, 0.12).
narrative_ontology:affects_constraint(article_17_complementarity__national_primacy_reading, article_17_complementarity__international_oversight_reading).

% DUAL FORMULATION NOTE:
% Article 17 complementarity instantiates as two structurally distinct constraints depending on how 'unwilling or unable' is read. The national_primacy_reading presumes state adequacy unless proven sham (high inadmissibility threshold, high beneficiary power); the international_oversight_reading presumes ICC scrutiny triggers unless state cooperation is genuine (low inadmissibility threshold, high victim access). These are not observations of the same constraint—they are different constraint objects sharing a common textual kernel. Each has its own ε, beneficiary/victim structure, and type. The readings coexist in the same institution; jurisdictions and courts choose readings based on framing and political pressure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(article_17_complementarity__national_primacy_reading, powerful, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
