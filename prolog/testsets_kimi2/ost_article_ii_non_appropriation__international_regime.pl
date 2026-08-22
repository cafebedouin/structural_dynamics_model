% ============================================================================
% CONSTRAINT STORY: ost_article_ii_non_appropriation__international_regime
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ost_article_ii_non_appropriation__international_regime, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: ost_article_ii_non_appropriation__international_regime
 *   human_readable: Article II Non-Appropriation: International Regime Deferral Reading
 *   domain: international/law/space_governance
 *
 * SUMMARY:
 *   This constraint story instantiates the 'international_regime' reading of
 *   the contested Article II non-appropriation kernel in the 1967 Outer Space
 *   Treaty. Under this reading, Article II's prohibition on national
 *   appropriation by claim of sovereignty, use, occupation, or any other
 *   means functions as a scaffold: it prevents immediate territorial seizures
 *   but does not definitively authorize or prohibit private resource
 *   extraction, instead deferring that resolution to a future multilateral
 *   international regime analogous to the Article XI reference in the Moon
 *   Agreement. The scaffold has drifted: regime negotiation is stalled by
 *   zero-sum distributional conflict among spacefaring and non-spacefaring
 *   states, while first-mover extractive firms operate under domestic
 *   licenses in a regulatory grey zone. The sibling
 *   readingsâextraction_permissive and commons_conservationâare treated
 *   as separate constraints in the kernel family. The claim/metric
 *   independence principle is observed: the claimed type is scaffold
 *   (transitional coordination), while the metrics describe a stalled,
 *   increasingly theatrical deferral with moderate extraction through legal
 *   ambiguity.
 *
 * KEY AGENTS:
 *   - un_copuos: Agenda-setter (institutional/constrained) â maintains the multilateral negotiation forum and the diplomatic frame that a regime is forthcoming
 *   - spacefaring_extractive_states: Primary beneficiary (institutional/constrained) â advance domestic space resource laws that exploit the legal grey zone created by deferred resolution
 *   - first_mover_extractive_firms: Primary target (powerful/constrained) â bear legal uncertainty and political risk despite domestic licensing
 *   - non_spacefaring_states: Secondary target (organized/constrained) â bear distributional costs of first-mover capture under unresolved regime
 *   - conservation_advocates: Excluded voice (moderate/constrained) â argue for immediate prohibition but are marginalized in state practice
 *   - space_law_scholars: Analytical observer (analytical/analytical) â document interpretive instability and drift
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ost_article_ii_non_appropriation__international_regime, 0.42).
domain_priors:suppression_score(ost_article_ii_non_appropriation__international_regime, 0.55).
domain_priors:theater_ratio(ost_article_ii_non_appropriation__international_regime, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__international_regime, extractiveness, 0.42).
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__international_regime, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__international_regime, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__international_regime, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__international_regime, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ost_article_ii_non_appropriation__international_regime, scaffold).
narrative_ontology:human_readable(ost_article_ii_non_appropriation__international_regime, "Article II Non-Appropriation: International Regime Deferral Reading").
narrative_ontology:topic_domain(ost_article_ii_non_appropriation__international_regime, "international/law/space_governance").

domain_priors:requires_active_enforcement(ost_article_ii_non_appropriation__international_regime).
narrative_ontology:has_sunset_clause(ost_article_ii_non_appropriation__international_regime).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ost_article_ii_non_appropriation__international_regime, '0af9d111-1d17-4a92-ae1c-81d0b00fb9d3').
narrative_ontology:cs_kernel_codification('0af9d111-1d17-4a92-ae1c-81d0b00fb9d3', formalized).
narrative_ontology:cs_authority_grounding('0af9d111-1d17-4a92-ae1c-81d0b00fb9d3', distributed).
narrative_ontology:cs_reading_relation('0af9d111-1d17-4a92-ae1c-81d0b00fb9d3', ost_article_ii_non_appropriation__extraction_permissive, coexists_with).
narrative_ontology:cs_reading_relation('0af9d111-1d17-4a92-ae1c-81d0b00fb9d3', ost_article_ii_non_appropriation__commons_conservation, coexists_with).
narrative_ontology:cs_axiom('0af9d111-1d17-4a92-ae1c-81d0b00fb9d3', foundational, appropriation_deferred_to_future_regime).
narrative_ontology:cs_axiom_status(appropriation_deferred_to_future_regime, holdable).
narrative_ontology:cs_axiom_grounding('0af9d111-1d17-4a92-ae1c-81d0b00fb9d3', appropriation_deferred_to_future_regime, conventional).
narrative_ontology:cs_axiom('0af9d111-1d17-4a92-ae1c-81d0b00fb9d3', foundational, unilateral_interpretations_non_authoritative).
narrative_ontology:cs_axiom_status(unilateral_interpretations_non_authoritative, holdable).
narrative_ontology:cs_axiom_grounding('0af9d111-1d17-4a92-ae1c-81d0b00fb9d3', unilateral_interpretations_non_authoritative, conventional).
narrative_ontology:cs_reference_frame('0af9d111-1d17-4a92-ae1c-81d0b00fb9d3', multilateral_regime_deferral).
narrative_ontology:cs_drift_state('0af9d111-1d17-4a92-ae1c-81d0b00fb9d3', contemporary_resource_rush_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('0af9d111-1d17-4a92-ae1c-81d0b00fb9d3', '').
narrative_ontology:cs_kernel_id(ost_article_ii_non_appropriation__international_regime, ost_article_ii_non_appropriation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ost_article_ii_non_appropriation__international_regime, spacefaring_extractive_states).
narrative_ontology:constraint_victim(ost_article_ii_non_appropriation__international_regime, first_mover_extractive_firms).
narrative_ontology:constraint_victim(ost_article_ii_non_appropriation__international_regime, non_spacefaring_states).
narrative_ontology:constraint_vindicates(ost_article_ii_non_appropriation__international_regime, article_xi_regime_analogue).
narrative_ontology:constraint_vindicates(ost_article_ii_non_appropriation__international_regime, non_appropriation_without_use_ban).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the multilateral negotiation forum for space governance under the Outer Space Treaty framework. Maintains working groups and diplomatic conferences on space resource activity, preserving the formal position that an international regime is the intended resolution mechanism, despite stalled progress and zero-sum distributional deadlock among member states.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__international_regime, un_copuos, agenda_setter,
    institutional, generational, constrained, global).

% Have enacted domestic space resource utilization statutes (e.g., United States, Luxembourg, United Arab Emirates) that license private extraction activity. Benefit from the legal grey zone created by Article II's deferral because their national frameworks operate without direct treaty prohibition, while the unresolved multilateral status prevents opposing states from closing off resource access through binding conservation rules.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__international_regime, spacefaring_extractive_states, beneficiary,
    institutional, generational, constrained, global).

% Lunar and asteroid resource ventures operating under national licenses. Can conduct activity but lack internationally recognized property rights, dispute resolution forums, or enforcement against competing claimants. Bear the legal and political risk that a future multilateral regime may override their claims, impose revenue-sharing obligations, or deny grandfathering.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__international_regime, first_mover_extractive_firms, payer,
    powerful, biographical, constrained, global).

% Lack the technological or capital capacity to participate in near-term space resource extraction. Bear the distributional cost of a deferred regime that allows first-mover capture of resource sites and rents by technologically advanced actors before an equitable benefit-sharing framework is negotiated.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__international_regime, non_spacefaring_states, payer,
    organized, generational, constrained, global).

% Argue that Article II's prohibition on use or occupation as means of appropriation should be read to prohibit all resource extraction as de facto appropriation. Are structurally excluded from the dominant interpretive and state-practice framework, which treats extraction as permissible pending a future regime rather than prohibited by existing treaty.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__international_regime, conservation_advocates, excluded,
    moderate, civilizational, constrained, global).

% Analyze the divergence between treaty text, subsequent state practice, emerging customary international law, and domestic space legislation. Document the interpretive instability, the absence of effective enforcement mechanisms for either extraction or conservation readings, and the functional drift of Article II from transitional placeholder to permanent grey-zone generator.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__international_regime, space_law_scholars, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents immediate sovereign territorial claims to celestial bodies and maintains a diplomatic framework in which final resolution of resource extraction rights is deferred to a future multilateral regime, avoiding premature unilateral closure of the space commons during technologically premature phases.
% TRANSFER_FUNCTION: Transfers legal certainty about space property rights from present private and state actors to a hypothetical future international regime; transfers first-mover advantage and regulatory latitude to technologically advanced spacefaring states and their licensed firms at the distributional expense of non-spacefaring states.
% ABSENT_VOICES: Non-spacefaring states with prospective but not current resource interests are underrepresented in interpretive developments dominated by spacefaring practice; future generations inherit the unresolved commons framework; conservation advocates arguing for immediate extraction prohibition are marginalized in state-level interpretive discourse.
% DISAPPEARANCE_RATIONALE: If the deferral reading vanished, extraction-permissive states and firms would likely accelerate unilateral resource claims under domestic law, consolidating first-mover property systems. Conservation advocates would push for immediate multilateral prohibition. The space economy and international space law would reorganize around either market-dominant or conservation-dominant governance rather than maintained legal ambiguity.
% FOUNDING_PROBLEM: The 1967 Outer Space Treaty drafters needed to prevent Cold War superpower territorial competition over the Moon and other celestial bodies while leaving the question of resource extraction unresolved, as the technology was then remote and political premature closure risked either blocking future development or enabling premature colonial capture.
% FOUNDING_PROBLEM_CORROBORATION: Space law historians attest the original drafter intent was narrowly focused on preventing sovereign territorial claims, with resource use deliberately unresolved. Non-spacefaring states in COPUOS attest the deferral was intended as a temporary bridge to an equitable regime. Spacefaring states with domestic extraction statutes attest the deferral was never meant to permanently block commercially viable resource use. No external party outside these contested positions corroborates that the current indefinite stall represents the original design.
narrative_ontology:disappearance_verdict(ost_article_ii_non_appropriation__international_regime, contested).
narrative_ontology:founding_problem_status(ost_article_ii_non_appropriation__international_regime, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ost_article_ii_non_appropriation__international_regime, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(ost_article_ii_non_appropriation__international_regime, 'none', 1).
narrative_ontology:epsilon_provenance(ost_article_ii_non_appropriation__international_regime, 0.42, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ost_article_ii_non_appropriation__international_regime_tests).
:- end_tests(ost_article_ii_non_appropriation__international_regime_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.42) is moderate because the deferral does not extract resources directly, but the legal ambiguity enables first-mover capture of resource sites and rents by technologically advanced actors, functioning as a distributional transfer from latecomers to incumbents. Suppression (0.55) reflects the constraint's suppression of both clear extraction title and clear conservation rules, replacing both with uncertainty. Theater_ratio (0.58) is elevated because the ritual of COPUOS negotiations and Article XI-style regime rhetoric persists despite stalled working groups and the near-certainty that no binding multilateral space resource agreement will emerge in the medium term. Accessibility_collapse (0.45) is moderate: alternatives (unilateral claims, clear extraction rights, conservation moratorium) are legally possible but politically blocked by the consensus requirement. Resistance (0.72) is high because both extraction-permissive and conservation camps actively contest the deferral. Temporal measurements trace the drift from low-extraction placeholder (1967) to stalled scaffold with accumulation of extraction-by-ambiguity (2025).
 *
 * PERSPECTIVAL GAP:
 *   The spacefaring extractive state seat experiences the constraint as coordination: it preserves diplomatic space, prevents conservation absolutism, and licenses domestic industry. The non-spacefaring state and first-mover firm seats experience it as extraction or at least cost-bearing: the former lose distributional equity to first-mover capture, the latter bear legal risk and inability to perfect title. The engine computes this divergence from the structural data rather than the authored claim.
 *
 * DIRECTIONALITY LOGIC:
 *   spacefaring_extractive_states are declared beneficiaries and have institutional power but constrained exit (bound by treaty and customary law), placing them on the beneficiary side of the directionality spectrum. first_mover_extractive_firms and non_spacefaring_states are declared victims (payers), yielding higher directionality; the firms are powerful but still constrained by the international legal grey zone, while the non-spacefaring states are organized with minimal leverage, pushing their effective extraction toward the higher end.
 *
 * MANDATROPHY ANALYSIS:
 *   The scaffold classification captures the transitional intent of Article II, preventing misclassification as either a permanent coordination mechanism (rope) or a pure extraction device (snare). However, the elevated theater_ratio and the temporal drift toward higher extractiveness signal mandatrophy risk: the scaffold may be becoming a piton (performative maintenance of a transition that is not occurring) or a tangled_rope (coordination of non-appropriation layered with extraction of first-mover advantage). The founding_problem_status is contested because spacefaring states with domestic extraction laws argue the original problem (preventing sovereign territorial claims) is solved, while non-spacefaring states argue the deferral was meant to enable equitable regime formation, not permanent ambiguity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    regime_stall_permanence,
    'Is the Article II deferral a genuinely transitional scaffold awaiting an eventual multilateral regime, or has it become a permanent structural feature masking power-political capture?',
    'Binding multilateral space resource agreement concluded and ratified by major spacefaring powers would confirm transitional status; permanent absence of such agreement with continued first-mover practice would confirm permanence.',
    'If transitional, classification remains scaffold; if permanent with concentrated first-mover gains, reclassification to tangled_rope or piton is warranted.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regime_stall_permanence, empirical, 'Whether the scaffold is actually transitioning or permanently stalled').

omega_variable(
    grey_zone_distributional_impact,
    'Does the legal grey zone primarily enable extraction by advanced spacefaring actors at the expense of non-spacefaring states, or does it constrain all parties equally?',
    'Empirical analysis of registered space resource claims, investment flows, and domestic licensing regimes under national space laws.',
    'If the grey zone enables asymmetric capture, the constraint functions as tangled_rope; if it constrains all equally, it remains a stalled scaffold.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(grey_zone_distributional_impact, empirical, 'Distributional asymmetry of the legal grey zone').

omega_variable(
    kernel_reading_contestation,
    'How would the constraint''s classification change under the extraction_permissive or commons_conservation readings of the same kernel?',
    'Comparative constraint-story analysis across the three kernel readings as separate files in the corpus.',
    'Extraction_permissive would likely classify as rope or snare with extractive firms as beneficiaries; commons_conservation would classify as rope or tangled_rope with conservation states as beneficiaries and extractive actors as payers.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Structural delta across kernel sibling readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ost_article_ii_non_appropriation__international_regime, 0, 58).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ost__tr_t0, ost_article_ii_non_appropriation__international_regime, theater_ratio, 0, 0.05).
narrative_ontology:measurement(ost__tr_t12, ost_article_ii_non_appropriation__international_regime, theater_ratio, 12, 0.15).
narrative_ontology:measurement(ost__tr_t24, ost_article_ii_non_appropriation__international_regime, theater_ratio, 24, 0.25).
narrative_ontology:measurement(ost__tr_t36, ost_article_ii_non_appropriation__international_regime, theater_ratio, 36, 0.38).
narrative_ontology:measurement(ost__tr_t48, ost_article_ii_non_appropriation__international_regime, theater_ratio, 48, 0.5).
narrative_ontology:measurement(ost__tr_t58, ost_article_ii_non_appropriation__international_regime, theater_ratio, 58, 0.58).

% Extraction over time
narrative_ontology:measurement(ost__be_t0, ost_article_ii_non_appropriation__international_regime, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(ost__be_t12, ost_article_ii_non_appropriation__international_regime, base_extractiveness, 12, 0.22).
narrative_ontology:measurement(ost__be_t24, ost_article_ii_non_appropriation__international_regime, base_extractiveness, 24, 0.28).
narrative_ontology:measurement(ost__be_t36, ost_article_ii_non_appropriation__international_regime, base_extractiveness, 36, 0.32).
narrative_ontology:measurement(ost__be_t48, ost_article_ii_non_appropriation__international_regime, base_extractiveness, 48, 0.38).
narrative_ontology:measurement(ost__be_t58, ost_article_ii_non_appropriation__international_regime, base_extractiveness, 58, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(ost__su_t0, ost_article_ii_non_appropriation__international_regime, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(ost__su_t12, ost_article_ii_non_appropriation__international_regime, suppression_requirement, 12, 0.35).
narrative_ontology:measurement(ost__su_t24, ost_article_ii_non_appropriation__international_regime, suppression_requirement, 24, 0.4).
narrative_ontology:measurement(ost__su_t36, ost_article_ii_non_appropriation__international_regime, suppression_requirement, 36, 0.45).
narrative_ontology:measurement(ost__su_t48, ost_article_ii_non_appropriation__international_regime, suppression_requirement, 48, 0.52).
narrative_ontology:measurement(ost__su_t58, ost_article_ii_non_appropriation__international_regime, suppression_requirement, 58, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(ost_article_ii_non_appropriation__international_regime, ost_article_ii_non_appropriation__extraction_permissive).
narrative_ontology:affects_constraint(ost_article_ii_non_appropriation__international_regime, ost_article_ii_non_appropriation__commons_conservation).

% DUAL FORMULATION NOTE:
% This constraint is one of three structurally distinct readings of the ost_article_ii_non_appropriation kernel. Each reading carries a different epsilon, beneficiary structure, and classification. The international_regime reading treats Article II as a deferral scaffold; the extraction_permissive reading treats it as enabling private extraction; the commons_conservation reading treats it as prohibiting extraction. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
