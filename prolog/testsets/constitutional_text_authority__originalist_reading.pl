% ============================================================================
% CONSTRAINT STORY: constitutional_text_authority__originalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
 *   constraint_id: constitutional_text_authority__originalist_reading
 *   human_readable: Originalist Constitutional Meaning Constraint (Ratification-Fixed Authority)
 *   domain: constitutional_law/interpretive_jurisprudence
 *
 * SUMMARY:
 *   Originalism is a dominant interpretive methodology in contemporary U.S.
 *   constitutional law, particularly in the federal judiciary after the
 *   Federalist Society's institutional investment in appointing originalist
 *   judges. The constraint asserts that constitutional meaning is fixed at
 *   ratification and discoverable through historical public understanding.
 *   This reading generates a specific structural relationship between
 *   authority (historical evidence), discretion (judges are constrained to
 *   historical sources), and outcome (unenumerated rights are difficult to
 *   recognize; post-ratification social change requires Article V amendment,
 *   not judicial innovation). This is ONE reading of the contested kernel
 *   'constitutional_text_authority.' The alternative readings (living
 *   constitutionalism, positivism) instantiate different constraints with
 *   different ε values and beneficiary structures. Each is a separate story;
 *   they are linked via network.affects_constraints.
 *
 * KEY AGENTS:
 *   - Originalist judiciary: institutional agenda-setters who control doctrine through opinions; structurally benefit from the legitimacy claim of constraint-through-history
 *   - Conservative political movements: powerful beneficiaries who gain from skepticism toward unenumerated rights and expansive federal power
 *   - Progressive constitutional interpreters: identity-locked payers who cannot leave the profession but whose interpretive moves are foreclosed
 *   - Rights claimants for unenumerated protections: powerless victims whose claims are structurally disfavored; trapped by Article V supermajority requirement
 *   - Originalist legal scholarship: organized beneficiaries and agenda-setters who produce historical arguments and legitimize the methodology
 *   - Alternative interpretive schools: excluded moderate power; marginalized from dominant institutional positions
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_text_authority__originalist_reading, 0.68).
domain_priors:suppression_score(constitutional_text_authority__originalist_reading, 0.62).
domain_priors:theater_ratio(constitutional_text_authority__originalist_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_text_authority__originalist_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(constitutional_text_authority__originalist_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(constitutional_text_authority__originalist_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_text_authority__originalist_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(constitutional_text_authority__originalist_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_text_authority__originalist_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_text_authority__originalist_reading, "Originalist Constitutional Meaning Constraint (Ratification-Fixed Authority)").
narrative_ontology:topic_domain(constitutional_text_authority__originalist_reading, "constitutional_law/interpretive_jurisprudence").

domain_priors:requires_active_enforcement(constitutional_text_authority__originalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_text_authority__originalist_reading, 'ea22674a-c688-4c46-a65b-37353664d717').
narrative_ontology:cs_kernel_codification('ea22674a-c688-4c46-a65b-37353664d717', fixed_text).
narrative_ontology:cs_authority_grounding('ea22674a-c688-4c46-a65b-37353664d717', lineage).
narrative_ontology:cs_interpretation_layer_present('ea22674a-c688-4c46-a65b-37353664d717').
narrative_ontology:cs_reading_relation('ea22674a-c688-4c46-a65b-37353664d717', constitutional_text_authority__living_constitutionalist_reading, forecloses).
narrative_ontology:cs_reading_relation('ea22674a-c688-4c46-a65b-37353664d717', constitutional_text_authority__positivist_reading, coexists_with).
narrative_ontology:cs_axiom('ea22674a-c688-4c46-a65b-37353664d717', foundational, meaning_fixed_at_ratification).
narrative_ontology:cs_axiom_status(meaning_fixed_at_ratification, holdable).
narrative_ontology:cs_axiom_grounding('ea22674a-c688-4c46-a65b-37353664d717', meaning_fixed_at_ratification, conventional).
narrative_ontology:cs_axiom('ea22674a-c688-4c46-a65b-37353664d717', foundational, historical_public_understanding_authority).
narrative_ontology:cs_axiom_status(historical_public_understanding_authority, holdable).
narrative_ontology:cs_axiom_grounding('ea22674a-c688-4c46-a65b-37353664d717', historical_public_understanding_authority, empirically_contingent).
narrative_ontology:cs_reference_frame('ea22674a-c688-4c46-a65b-37353664d717', founding_era_textual_authority).
narrative_ontology:cs_drift_state('ea22674a-c688-4c46-a65b-37353664d717', contemporary_2025, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('ea22674a-c688-4c46-a65b-37353664d717', '2026-06-12T14:32:00Z').
narrative_ontology:cs_kernel_id(constitutional_text_authority__originalist_reading, constitutional_text_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_text_authority__originalist_reading, originalist_judiciary).
narrative_ontology:constraint_beneficiary(constitutional_text_authority__originalist_reading, conservative_political_movements).
narrative_ontology:constraint_victim(constitutional_text_authority__originalist_reading, progressive_constitutional_interpreters).
narrative_ontology:constraint_victim(constitutional_text_authority__originalist_reading, rights_claimants_seeking_unenumerated_protections).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_text_authority__originalist_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(constitutional_text_authority__originalist_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_text_authority__originalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(constitutional_text_authority__originalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(constitutional_text_authority__originalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.68 at interval end) because the constraint systematically forecloses judicial recognition of rights outside historical public meaning, transferring the burden of constitutional change to Article V amendment—a nearly impossible bar. The measurement series shows extraction RISING from 0.31 (1985, when originalism was ascendant but not yet institutionally dominant) to 0.68 (2025, after decades of appointments concentrated on originalists). Suppression is high (0.62) because the constraint is actively defended through appointment strategy, scholarly production, and judicial opinions dismissing alternative methodologies as unprincipled judicial legislation. Theater ratio rises moderately (0.25→0.41) because an increasing share of originalist opinions' analytical effort goes to historical source-selection and evidentiary boundary-policing rather than substantive constitutional reasoning—the theatrical performance of constraint-discovery rather than actual constraint application. Suppression_requirement captures the active enforcement machinery needed: the constraint persists because originalist judges and scholars invest continuous effort in producing historical arguments, defending the methodology against critique, and excluding alternative approaches from prestige institutions. Without that enforcement, the constraint would erode as non-originalist perspectives reassert interpretive authority.
 *
 * PERSPECTIVAL GAP:
 *   The originalist judiciary and conservative beneficiaries experience this as genuine coordination—a principled constraint that disciplines judicial discretion. From their seat, meaning-fixation is not extraction but fidelity to law. Progressive interpreters and rights claimants experience the same constraint as asymmetric: their interpretive moves are foreclosed while conservative outcomes are legitimized through the same 'historical' methodology, applied asymmetrically (originalists find historical support for unenumerated property rights but deny it for unenumerated privacy rights, suggesting the constraint is selective rather than uniformly constraining). The engine computes this divergence from the structural data: same power level, opposed roles (beneficiary vs. payer), different exit options (arbitrage vs. identity-locked). The perpespectival gap emerges from asymmetric exit and structural position, not from irreconcilable values.
 *
 * DIRECTIONALITY LOGIC:
 *   Originalist judiciary: institutional power, arbitrage-grade exit (can change methodologies without losing identity, though at career cost), agenda-setting role → d near beneficiary end (0.15–0.25). Conservative political movements: powerful, arbitrage exit, beneficiary role (gain from outcomes) → d near beneficiary end (0.20–0.30). Progressive interpreters: moderate power, identity_locked exit (cannot leave the profession/discipline of constitutional law without losing professional identity), payer role (bear the constraint on their interpretive moves) → d near target end (0.70–0.80). Rights claimants: powerless, trapped exit (no exit at all; constitutional rights are not negotiable), victim role → d at full target (0.95–1.0). The directionality derivation from beneficiary/victim + exit captures why the same constraint looks protective to one seat and extractive to another.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (controlling judicial discretion in constitutional interpretation) was live in 1985 and arguably remains contested in 2025. However, originalism has accumulated extractive layers that drift away from the coordination function: the constraint now primarily forecloses particular constitutional claims while legitimizing conservative outcomes through the same historical methodology—a selective constraint that appears to discipline all interpretation but operationally disciplines progressive interpretation asymmetrically. The theater_ratio rising (0.25→0.41) indicates that an increasing share of originalist judicial effort is spent defending the constraint's legitimacy (source-selection disputes, evidentiary boundaries) rather than applying a neutral constraint. This pattern is consistent with mandatrophy: the coordination function (disciplining discretion) is subordinated to the extraction function (blocking particular outcomes), and theatrical maintenance of the historical-constraint narrative becomes necessary because the constraint is no longer genuinely constraining in a symmetric way. The measurement series documents this drift: base_extractiveness rises as institutional dominance grows, suggesting the constraint has shifted from coordination (when contested and less powerful) to enforced extraction (when institutionalized and powerful).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    historical_public_meaning_indeterminacy,
    'Is ''historical public meaning at ratification'' a determinate, discoverable fact, or is it inherently plural, contested, and selected by the interpreter?',
    'Methodological scrutiny of originalist historical arguments in high-stakes cases (same constitutional text, originalist judges producing different historical readings; systematic study of whether historical evidence underdetermines outcomes). Comparison with peer methodologies in textual history (biblical exegesis, literary interpretation) to assess whether historical meaning-recovery is ever determinate.',
    'If historical meaning is indeterminate and interpreter-selected, originalism''s constraint function collapses—the constraint does not fix meaning, it merely redirects discretion toward historical sources. The constraint would reclassify from tangled_rope (coordination + extraction) to snare (pure extraction with coordination cover). If historical meaning is genuinely determinate and discoverable, originalism is a legitimate coordination constraint and the classification stands.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(historical_public_meaning_indeterminacy, empirical, 'Whether the methodology''s core claim—that historical meaning is determinate—is true.').

omega_variable(
    asymmetric_methodological_application,
    'Does originalist doctrine apply the historical-meaning constraint symmetrically to both progressive and conservative constitutional claims, or does it apply it asymmetrically, accepting historical arguments for conservative outcomes while rejecting them for progressive outcomes?',
    'Quantitative analysis of originalist opinions across ideological outcome classes: do originalists equally accept/reject historical arguments for libertarian limits on federal power (conservative) and historical arguments for unenumerated privacy rights (progressive)? Do they weight evidence (original intent, public meaning, text) differently depending on the outcome''s ideological valence?',
    'Asymmetric application would establish the constraint as selective rather than uniformly constraining, supporting reclassification from tangled_rope (genuine coordination with asymmetric extraction) to snare (extraction disguised as coordination). Symmetric application would support the tangled_rope classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(asymmetric_methodological_application, empirical, 'Whether the constraint operates as a genuine limiting principle or as ideological cover for preferred outcomes.').

omega_variable(
    alternative_methodology_foreclosure,
    'Is the institutional foreclosure of non-originalist methodologies (living constitutionalism, purposivism, pragmatism) a necessary consequence of originalism''s intellectual superiority, or a consequence of political investment in originalist judicial appointments?',
    'Counterfactual institutional history: what would legal academia and the judiciary look like if the Federalist Society and conservative movement had not invested resources in promoting originalism specifically? Comparison of citation patterns, hiring trends, and institutional prestige across methodologies before (1970–1985) and after (1985–2025) the appointment campaign.',
    'If foreclosure is intellectual-driven, it reflects the market dominance of a superior methodology. If foreclosure is appointment-driven, it reflects the extraction function of the constraint: concentrated power using institutional mechanisms to exclude competing interpretive frameworks, making the constraint a snare. The measurement series and the rising theater_ratio are consistent with the second hypothesis.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_methodology_foreclosure, empirical, 'Whether the constraint''s institutional dominance reflects genuine methodological superiority or political and institutional power concentration.').

omega_variable(
    committer_kernel_reading_indeterminacy,
    'Is there a genuine logical incompatibility between the originalist reading (meaning fixed at ratification) and the living constitutionalist reading (meaning evolves), or can both coexist as different legitimate frameworks for different interpretive communities?',
    'Philosophical/logical analysis: do the two readings contradict each other at the level of core claims, or do they differ in scope, application, or institutional role? Can a judge coherently hold that meaning is ''fixed at ratification for statutory interpretation but evolving for constitutional interpretation,'' or does that reveal incoherence in one or both frameworks?',
    'If the readings are logically incompatible (forecloses relation), they cannot both be true in a single coherent framework—only institutional power determines which prevails. If they coexist without logical contradiction (coexists_with relation), the constraint landscape includes genuine methodological pluralism and the institutional dominance of originalism is a contingent political fact, not a logical necessity. This affects how the constraint is classified as persistent: does it persist because it is true, or because it has institutional power?',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(committer_kernel_reading_indeterminacy, conceptual, 'Whether alternative readings of the constitutional kernel are logically foreclosed by originalism or merely politically marginalized.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_text_authority__originalist_reading, 1985, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t1985, constitutional_text_authority__originalist_reading, theater_ratio, 1985, 0.25).
narrative_ontology:measurement(cons_tr_t1995, constitutional_text_authority__originalist_reading, theater_ratio, 1995, 0.29).
narrative_ontology:measurement(cons_tr_t2005, constitutional_text_authority__originalist_reading, theater_ratio, 2005, 0.34).
narrative_ontology:measurement(cons_tr_t2015, constitutional_text_authority__originalist_reading, theater_ratio, 2015, 0.38).
narrative_ontology:measurement(cons_tr_t2025, constitutional_text_authority__originalist_reading, theater_ratio, 2025, 0.41).

% Extraction over time
narrative_ontology:measurement(cons_be_t1985, constitutional_text_authority__originalist_reading, base_extractiveness, 1985, 0.31).
narrative_ontology:measurement(cons_be_t1995, constitutional_text_authority__originalist_reading, base_extractiveness, 1995, 0.42).
narrative_ontology:measurement(cons_be_t2005, constitutional_text_authority__originalist_reading, base_extractiveness, 2005, 0.54).
narrative_ontology:measurement(cons_be_t2015, constitutional_text_authority__originalist_reading, base_extractiveness, 2015, 0.62).
narrative_ontology:measurement(cons_be_t2025, constitutional_text_authority__originalist_reading, base_extractiveness, 2025, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(cons_su_t1985, constitutional_text_authority__originalist_reading, suppression_requirement, 1985, 0.48).
narrative_ontology:measurement(cons_su_t1995, constitutional_text_authority__originalist_reading, suppression_requirement, 1995, 0.53).
narrative_ontology:measurement(cons_su_t2005, constitutional_text_authority__originalist_reading, suppression_requirement, 2005, 0.58).
narrative_ontology:measurement(cons_su_t2015, constitutional_text_authority__originalist_reading, suppression_requirement, 2015, 0.61).
narrative_ontology:measurement(cons_su_t2025, constitutional_text_authority__originalist_reading, suppression_requirement, 2025, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_text_authority__originalist_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(constitutional_text_authority__originalist_reading, 0.12).
narrative_ontology:affects_constraint(constitutional_text_authority__originalist_reading, constitutional_text_authority__living_constitutionalist_reading).
narrative_ontology:affects_constraint(constitutional_text_authority__originalist_reading, constitutional_text_authority__positivist_reading).
narrative_ontology:affects_constraint(constitutional_text_authority__originalist_reading, judicial_discretion_constraint_mechanism).
narrative_ontology:affects_constraint(constitutional_text_authority__originalist_reading, unenumerated_rights_recognition_barrier).
narrative_ontology:affects_constraint(constitutional_text_authority__originalist_reading, article_v_amendment_necessity_gate).

% DUAL FORMULATION NOTE:
% The constraint 'constitutional_text_authority' is a contested kernel with three reading-based constraint stories: originalist_reading (this story), living_constitutionalist_reading, and positivist_reading. Each instantiates a different structural relationship between authority (historical vs. contemporary vs. formal), discretion (constrained vs. active vs. procedural), and outcome (foreclosure of unenumerated rights vs. openness to recognition vs. validity-as-formal-process). They are not perspectives on one constraint; they are different constraints with different ε values and beneficiary structures. The originalist reading's institutional dominance (1985–2025, measured in appointments and prestige) directly affects the operative force of the other readings. A shift in judicial composition toward living constitutionalism would alter this constraint's extraction and enforcement requirements—the network edge captures this structural coupling.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(constitutional_text_authority__originalist_reading, moderate, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
