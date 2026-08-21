% ============================================================================
% CONSTRAINT STORY: constitutional_text_authority__originalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_text_authority__originalist_reading, []).

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
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: constitutional_text_authority__originalist_reading
 *   human_readable: Originalist Reading of Constitutional Textual Authority
 *   domain: legal/political/interpretive_jurisprudence
 *
 * SUMMARY:
 *   This constraint represents the originalist reading of constitutional
 *   authority, which holds that the meaning of the Constitution is fixed at
 *   the time of its ratification and should be interpreted according to the
 *   historical public understanding of its text. Authority derives from this
 *   historical understanding, acting as a rigid constraint on judicial
 *   discretion. This reading is one of several competing interpretations of
 *   the 'constitutional_text_authority' kernel.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_text_authority__originalist_reading, 0.7).
domain_priors:suppression_score(constitutional_text_authority__originalist_reading, 0.8).
domain_priors:theater_ratio(constitutional_text_authority__originalist_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_text_authority__originalist_reading, extractiveness, 0.7).
narrative_ontology:constraint_metric(constitutional_text_authority__originalist_reading, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(constitutional_text_authority__originalist_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_text_authority__originalist_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(constitutional_text_authority__originalist_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_text_authority__originalist_reading, mountain).
narrative_ontology:human_readable(constitutional_text_authority__originalist_reading, "Originalist Reading of Constitutional Textual Authority").
narrative_ontology:topic_domain(constitutional_text_authority__originalist_reading, "legal/political/interpretive_jurisprudence").

domain_priors:requires_active_enforcement(constitutional_text_authority__originalist_reading).
domain_priors:emerges_naturally(constitutional_text_authority__originalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_text_authority__originalist_reading, '5feea969-595c-4baa-b9fa-1385652720c0').
narrative_ontology:cs_kernel_codification('5feea969-595c-4baa-b9fa-1385652720c0', fixed_text).
narrative_ontology:cs_authority_grounding('5feea969-595c-4baa-b9fa-1385652720c0', lineage).
narrative_ontology:cs_interpretation_layer_present('5feea969-595c-4baa-b9fa-1385652720c0').
narrative_ontology:cs_reading_relation('5feea969-595c-4baa-b9fa-1385652720c0', constitutional_text_authority__living_constitutionalist_reading, forecloses).
narrative_ontology:cs_reading_relation('5feea969-595c-4baa-b9fa-1385652720c0', constitutional_text_authority__positivist_reading, coexists_with).
narrative_ontology:cs_axiom('5feea969-595c-4baa-b9fa-1385652720c0', foundational, constitutional_meaning_fixed_at_ratification).
narrative_ontology:cs_axiom_status(constitutional_meaning_fixed_at_ratification, holdable).
narrative_ontology:cs_axiom_grounding('5feea969-595c-4baa-b9fa-1385652720c0', constitutional_meaning_fixed_at_ratification, deontological).
narrative_ontology:cs_axiom('5feea969-595c-4baa-b9fa-1385652720c0', foundational, judicial_role_limited_to_original_meaning).
narrative_ontology:cs_axiom_status(judicial_role_limited_to_original_meaning, holdable).
narrative_ontology:cs_axiom_grounding('5feea969-595c-4baa-b9fa-1385652720c0', judicial_role_limited_to_original_meaning, conventional).
narrative_ontology:cs_reference_frame('5feea969-595c-4baa-b9fa-1385652720c0', ratification_era_public_meaning).
narrative_ontology:cs_drift_state('5feea969-595c-4baa-b9fa-1385652720c0', contemporary_legal_discourse, gap(revival_pressure, substantial, false)).
narrative_ontology:cs_created_at('5feea969-595c-4baa-b9fa-1385652720c0', '').
narrative_ontology:cs_kernel_id(constitutional_text_authority__originalist_reading, constitutional_text_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_text_authority__originalist_reading, originalist_legal_scholars).
narrative_ontology:constraint_beneficiary(constitutional_text_authority__originalist_reading, conservative_political_actors).
narrative_ontology:constraint_victim(constitutional_text_authority__originalist_reading, groups_seeking_unenumerated_rights).
narrative_ontology:constraint_victim(constitutional_text_authority__originalist_reading, progressive_social_movements).
narrative_ontology:constraint_vindicates(constitutional_text_authority__originalist_reading, constitutional_originalism_doctrine).
narrative_ontology:constraint_vindicates(constitutional_text_authority__originalist_reading, judicial_restraint_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interpret and advocate for original meaning, shaping legal discourse, judicial appointments, and public understanding of the Constitution. Their professional identity is often tied to this interpretive method.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__originalist_reading, originalist_legal_scholars, agenda_setter,
    institutional, generational, identity_locked, national).

% Benefit from interpretations that align with their policy goals, using originalism to justify legislative and judicial actions and to resist progressive social change. They can shift strategies if originalism becomes politically unviable.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__originalist_reading, conservative_political_actors, beneficiary,
    powerful, biographical, mobile, national).

% Bear the cost of interpretations that deny or restrict rights not explicitly listed in the text or understood at ratification, facing high barriers to legal change and often needing to resort to the difficult amendment process.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__originalist_reading, groups_seeking_unenumerated_rights, payer,
    powerless, biographical, constrained, national).

% Face legal obstacles when advocating for social changes that require evolving constitutional interpretations. They must either pursue constitutional amendments or challenge existing precedents, both of which are costly and difficult.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__originalist_reading, progressive_social_movements, payer,
    organized, biographical, constrained, national).

% Advocate for evolving constitutional meaning but are often marginalized in originalist-dominated legal discourse, judicial appointments, and public debate, despite their academic contributions.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__originalist_reading, living_constitutionalist_scholars, excluded,
    institutional, generational, identity_locked, national).

% Apply originalist principles in their rulings, shaping the practical effect of the constraint on society. Their judicial philosophy often becomes a core part of their public and professional identity.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__originalist_reading, supreme_court_justices, agenda_setter,
    institutional, biographical, identity_locked, national).

% The formal mechanism for changing the Constitution (Article V), which originalism emphasizes as the *only* legitimate way to alter constitutional meaning. It is a slow and difficult process, making it a high barrier to change.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__originalist_reading, constitutional_amendment_process, observer,
    institutional, civilizational, trapped, national).
narrative_ontology:stakeholder_non_agent(constitutional_text_authority__originalist_reading, constitutional_amendment_process).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a fixed, stable, and purportedly objective basis for legal interpretation, aiming to limit judicial discretion and ensure predictability in law by rooting meaning in historical public understanding at ratification.
% TRANSFER_FUNCTION: Transfers interpretive authority from contemporary societal values or judicial discretion to historical intent/public meaning, effectively transferring power to those who control historical narratives and legal scholarship, and away from those seeking evolving rights.
% ABSENT_VOICES: Future generations, whose values and circumstances are not reflected in the original understanding, are structurally absent from the interpretive process. Legal scholars advocating for non-originalist methods are often excluded from influential judicial appointments and legal discourse.
% DISAPPEARANCE_RATIONALE: If the originalist reading vanished, judicial interpretation would immediately shift, likely leading to a re-evaluation of numerous precedents, potential expansion of unenumerated rights, and a significant rebalancing of power between branches and social groups. The entire legal landscape would reorganize.
% FOUNDING_PROBLEM: To prevent arbitrary judicial rule, ensure fidelity to the framers' intent, and maintain the democratic legitimacy of constitutional law by rooting it in popular sovereignty at the time of ratification, thereby limiting judicial activism.
% FOUNDING_PROBLEM_CORROBORATION: Originalist scholars and conservative political actors strongly assert the problem of judicial overreach is still live. Critics (living constitutionalists, legal realists) argue that the problem of arbitrary rule is often exacerbated by rigid originalism, and that the 'popular sovereignty' claim is often a cover for specific policy outcomes. Independent historical analysis often shows the 'original intent' itself was contested or ambiguous, undermining the claim of objective historical discovery.
narrative_ontology:disappearance_verdict(constitutional_text_authority__originalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_text_authority__originalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_text_authority__originalist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(constitutional_text_authority__originalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_text_authority__originalist_reading, 0.7, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_text_authority__originalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(constitutional_text_authority__originalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(constitutional_text_authority__originalist_reading, ExtMetricName, E),
    domain_priors:suppression_score(constitutional_text_authority__originalist_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(constitutional_text_authority__originalist_reading),
    narrative_ontology:constraint_metric(constitutional_text_authority__originalist_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(constitutional_text_authority__originalist_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(constitutional_text_authority__originalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (0.7) and suppression (0.8) reflect the practical impact of this reading: it often denies or restricts rights not explicitly enumerated or historically recognized, effectively extracting opportunities for social and legal evolution from affected groups. Suppression is high because it actively disallows alternative interpretive methods and requires significant enforcement (judicial rulings, political appointments) to maintain its dominance. The theater ratio (0.4) is moderate; while genuine historical and legal scholarship is involved, there's also a performative aspect in asserting a singular, discoverable 'original intent' that often aligns with contemporary political agendas. The claimed type is 'mountain' because originalists assert the meaning is fixed and unchangeable, like a natural law, despite its constructed and enforced nature.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of originalist proponents, this constraint is a 'mountain' – an objective, fixed truth that naturally emerges from the text and history, ensuring judicial fidelity and democratic legitimacy. From the perspective of those whose rights are denied or whose social progress is impeded, it operates as a highly extractive and suppressive 'snare' or 'tangled_rope', leveraging historical claims to enforce contemporary power dynamics.
 *
 * DIRECTIONALITY LOGIC:
 *   Originalist legal scholars and conservative political actors are primary beneficiaries, as this reading provides a powerful framework for their legal and political goals. Groups seeking unenumerated rights and progressive social movements are victims, as their aspirations are often curtailed by rigid historical interpretations. Supreme Court justices, when adopting this philosophy, act as agenda-setters, enforcing the constraint. Living constitutionalist scholars are excluded, as their interpretive framework is often marginalized.
 *
 * MANDATROPHY ANALYSIS:
 *   The originalist reading claims to solve the problem of judicial overreach and arbitrary rule. However, critics argue that it often shifts, rather than eliminates, judicial discretion (e.g., to historical interpretation) and can be used to achieve specific policy outcomes under the guise of fidelity. If the original problem of arbitrary judicial rule is substantially mitigated by other means (e.g., robust democratic processes), but originalism persists primarily to achieve specific political ends, it could be seen as having undergone mandatrophy, with its original justification serving as cover for continued extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    original_intent_determinacy,
    'Is ''original intent'' or ''public meaning at ratification'' a truly discoverable, unambiguous, and singular historical fact, or is it inherently contested and subject to contemporary interpretive biases?',
    'Extensive interdisciplinary historical and linguistic analysis, including studies of framing-era debates, dictionaries, and public discourse, assessed for consensus among diverse scholars. If significant, persistent ambiguity is found, the claim of determinacy is weakened.',
    'If original intent is found to be largely indeterminate, the ''mountain'' claim of fixed meaning collapses, and the constraint''s persistence would be reclassified as relying more heavily on active enforcement and political power, likely shifting its type towards a ''tangled_rope'' or ''snare''.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(original_intent_determinacy, empirical, 'Ambiguity of historical constitutional meaning.').

omega_variable(
    democratic_legitimacy_source,
    'Does the democratic legitimacy of constitutional law derive solely from past popular sovereignty at ratification, or also from ongoing consent and evolving societal values?',
    'Conceptual analysis of political philosophy and democratic theory, alongside empirical studies of public attitudes towards constitutional change and interpretation. This is a conceptual/preference question that cannot be resolved by empirical data alone.',
    'If legitimacy is understood to require ongoing consent, the originalist reading''s claim to democratic superiority is weakened, and its rigidity might be seen as an impediment to, rather than a guarantor of, democratic self-governance. This would shift the classification towards a more extractive type from the perspective of those seeking contemporary democratic expression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(democratic_legitimacy_source, conceptual, 'Source of constitutional democratic legitimacy.').

omega_variable(
    judicial_discretion_vs_rigidity,
    'Does originalism genuinely limit judicial discretion, or does it merely shift the locus of discretion from contemporary policy choices to historical interpretation, potentially introducing new forms of judicial activism?',
    'Empirical analysis of judicial opinions applying originalist methods, examining whether historical arguments consistently lead to predetermined outcomes or if they allow for significant interpretive choice. Comparative studies with non-originalist methods could also shed light.',
    'If originalism is found to merely re-route, rather than eliminate, judicial discretion, its core justification for limiting judicial power is undermined. This would expose the constraint as less ''natural'' and more ''constructed'', increasing its perceived extractiveness and suppression from the perspective of those seeking genuine limits on judicial power.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judicial_discretion_vs_rigidity, empirical, 'Impact of originalism on judicial discretion.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_text_authority__originalist_reading, 1970, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t1970, constitutional_text_authority__originalist_reading, theater_ratio, 1970, 0.2).
narrative_ontology:measurement(cons_tr_t1980, constitutional_text_authority__originalist_reading, theater_ratio, 1980, 0.25).
narrative_ontology:measurement(cons_tr_t1990, constitutional_text_authority__originalist_reading, theater_ratio, 1990, 0.3).
narrative_ontology:measurement(cons_tr_t2000, constitutional_text_authority__originalist_reading, theater_ratio, 2000, 0.35).
narrative_ontology:measurement(cons_tr_t2010, constitutional_text_authority__originalist_reading, theater_ratio, 2010, 0.38).
narrative_ontology:measurement(cons_tr_t2025, constitutional_text_authority__originalist_reading, theater_ratio, 2025, 0.4).

% Extraction over time
narrative_ontology:measurement(cons_be_t1970, constitutional_text_authority__originalist_reading, base_extractiveness, 1970, 0.45).
narrative_ontology:measurement(cons_be_t1980, constitutional_text_authority__originalist_reading, base_extractiveness, 1980, 0.55).
narrative_ontology:measurement(cons_be_t1990, constitutional_text_authority__originalist_reading, base_extractiveness, 1990, 0.62).
narrative_ontology:measurement(cons_be_t2000, constitutional_text_authority__originalist_reading, base_extractiveness, 2000, 0.66).
narrative_ontology:measurement(cons_be_t2010, constitutional_text_authority__originalist_reading, base_extractiveness, 2010, 0.68).
narrative_ontology:measurement(cons_be_t2025, constitutional_text_authority__originalist_reading, base_extractiveness, 2025, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(cons_su_t1970, constitutional_text_authority__originalist_reading, suppression_requirement, 1970, 0.5).
narrative_ontology:measurement(cons_su_t1980, constitutional_text_authority__originalist_reading, suppression_requirement, 1980, 0.6).
narrative_ontology:measurement(cons_su_t1990, constitutional_text_authority__originalist_reading, suppression_requirement, 1990, 0.7).
narrative_ontology:measurement(cons_su_t2000, constitutional_text_authority__originalist_reading, suppression_requirement, 2000, 0.75).
narrative_ontology:measurement(cons_su_t2010, constitutional_text_authority__originalist_reading, suppression_requirement, 2010, 0.78).
narrative_ontology:measurement(cons_su_t2025, constitutional_text_authority__originalist_reading, suppression_requirement, 2025, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_text_authority__originalist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(constitutional_text_authority__originalist_reading, judicial_review_scope).
narrative_ontology:affects_constraint(constitutional_text_authority__originalist_reading, unenumerated_rights_recognition).
narrative_ontology:affects_constraint(constitutional_text_authority__originalist_reading, constitutional_text_authority__living_constitutionalist_reading).
narrative_ontology:affects_constraint(constitutional_text_authority__originalist_reading, constitutional_text_authority__positivist_reading).

% DUAL FORMULATION NOTE:
% This is one of three distinct readings of the 'constitutional_text_authority' kernel, each with its own structural properties and classification. This reading (originalism) directly influences the operational space and legitimacy claims of the 'living_constitutionalist_reading' and 'positivist_reading' by asserting a fixed interpretive methodology.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
