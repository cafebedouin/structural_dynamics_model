% ============================================================================
% CONSTRAINT STORY: fair_use_four_factor_test__creator_centric_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fair_use_four_factor_test__creator_centric_reading, []).

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
 *   constraint_id: fair_use_four_factor_test__creator_centric_reading
 *   human_readable: Fair Use Four-Factor Test (Creator-Centric Reading)
 *   domain: legal/intellectual_property
 *
 * SUMMARY:
 *   This constraint story instantiates the creator-centric reading of the
 *   fair-use four-factor kernel. Under this reading, fair use is a narrow
 *   exception to a broad property right, and the statutory factors are
 *   weighed primarily to preserve creator incentives and market control. The
 *   constraint coordinates the production of creative works by promising
 *   enforceable exclusivity, but it simultaneously extracts from
 *   transformative users, documentary makers, and public-domain access
 *   seekers by chilling reuse and extending proprietary boundaries. The claim
 *   is tangled_rope: a genuine coordination function (solving the
 *   public-goods problem of cultural production) is fused with asymmetric
 *   extraction (rights-holder capture of derivative and reuse markets). The
 *   sibling readingsâuser-centric and transformative-useâare treated as
 *   separate constraints in the same kernel family.
 *
 * KEY AGENTS:
 *   - rights_holders: Primary beneficiary (powerful/mobile) â collect rents from narrow fair use interpretation
 *   - transformative_users: Primary target (moderate/constrained) â bear litigation risk and licensing burden
 *   - access_seekers: Secondary target (powerless/constrained) â bear costs of delayed public domain and restricted access
 *   - copyright_judiciary: Agenda-setter (institutional/analytical) â interprets and applies the four-factor test
 *   - user_advocacy_groups: Observer (moderate/analytical) â argue for broader reading, systematically outweighed
 *   - small_documentary_filmmakers: Excluded (powerless/trapped) â self-censor due to clearance costs and fear of suit
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fair_use_four_factor_test__creator_centric_reading, 0.78).
domain_priors:suppression_score(fair_use_four_factor_test__creator_centric_reading, 0.72).
domain_priors:theater_ratio(fair_use_four_factor_test__creator_centric_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fair_use_four_factor_test__creator_centric_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(fair_use_four_factor_test__creator_centric_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(fair_use_four_factor_test__creator_centric_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fair_use_four_factor_test__creator_centric_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(fair_use_four_factor_test__creator_centric_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fair_use_four_factor_test__creator_centric_reading, tangled_rope).
narrative_ontology:human_readable(fair_use_four_factor_test__creator_centric_reading, "Fair Use Four-Factor Test (Creator-Centric Reading)").
narrative_ontology:topic_domain(fair_use_four_factor_test__creator_centric_reading, "legal/intellectual_property").

domain_priors:requires_active_enforcement(fair_use_four_factor_test__creator_centric_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fair_use_four_factor_test__creator_centric_reading, '8f79acfd-719d-4b12-a629-9701d6258687').
narrative_ontology:cs_kernel_codification('8f79acfd-719d-4b12-a629-9701d6258687', fixed_text).
narrative_ontology:cs_authority_grounding('8f79acfd-719d-4b12-a629-9701d6258687', lineage).
narrative_ontology:cs_interpretation_layer_present('8f79acfd-719d-4b12-a629-9701d6258687').
narrative_ontology:cs_reading_relation('8f79acfd-719d-4b12-a629-9701d6258687', fair_use_four_factor_test__user_centric_reading, coexists_with).
narrative_ontology:cs_reading_relation('8f79acfd-719d-4b12-a629-9701d6258687', fair_use_four_factor_test__transformative_use_reading, influences).
narrative_ontology:cs_axiom('8f79acfd-719d-4b12-a629-9701d6258687', foundational, creator_incentive_primacy).
narrative_ontology:cs_axiom_status(creator_incentive_primacy, holdable).
narrative_ontology:cs_axiom_grounding('8f79acfd-719d-4b12-a629-9701d6258687', creator_incentive_primacy, instrumental).
narrative_ontology:cs_axiom('8f79acfd-719d-4b12-a629-9701d6258687', foundational, market_harm_presumption).
narrative_ontology:cs_axiom_status(market_harm_presumption, holdable).
narrative_ontology:cs_axiom_grounding('8f79acfd-719d-4b12-a629-9701d6258687', market_harm_presumption, conventional).
narrative_ontology:cs_reference_frame('8f79acfd-719d-4b12-a629-9701d6258687', statutory_creator_monopoly).
narrative_ontology:cs_drift_state('8f79acfd-719d-4b12-a629-9701d6258687', post_digital_reproduction_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('8f79acfd-719d-4b12-a629-9701d6258687', '').
narrative_ontology:cs_kernel_id(fair_use_four_factor_test__creator_centric_reading, fair_use_four_factor_test).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fair_use_four_factor_test__creator_centric_reading, rights_holders).
narrative_ontology:constraint_victim(fair_use_four_factor_test__creator_centric_reading, transformative_users).
narrative_ontology:constraint_victim(fair_use_four_factor_test__creator_centric_reading, access_seekers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Control reproduction and derivative rights under copyright; benefit from licensing revenue, injunctive relief, and the ability to suppress unauthorized reuse. The creator-centric reading of the four-factor test maximizes their property entitlements by treating fair use as a narrow exception.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__creator_centric_reading, rights_holders, beneficiary,
    powerful, generational, mobile, national).

% Produce remixes, documentaries, criticism, and educational materials that build on existing works. Under a narrow fair-use regime, they face high legal uncertainty, statutory-damage exposure, and prohibitive licensing costs that chill legitimate follow-on creation.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__creator_centric_reading, transformative_users, payer,
    moderate, biographical, constrained, national).

% Seek affordable access to cultural, educational, and historical works for research, preservation, and personal use. The creator-centric reading extends proprietary control, delays public-domain entry, and restricts library digitization and non-commercial access.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__creator_centric_reading, access_seekers, payer,
    powerless, generational, constrained, national).

% Interprets and applies the four statutory fair-use factors. In this reading, courts systematically weigh market harm and the effect on potential markets toward preserving creator incentives, thereby narrowing the exception and expanding rights-holder control.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__creator_centric_reading, copyright_judiciary, agenda_setter,
    institutional, generational, analytical, national).

% Argue for broader fair use and public access, filing amicus briefs and empirical studies. Their arguments are heard but systematically outweighed in the balancing framework by market-harm and creator-incentive considerations.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__creator_centric_reading, user_advocacy_groups, observer,
    moderate, generational, analytical, national).

% Rely on fair use for archival footage and incidental music but lack litigation budgets to defend suits. They often self-censor or pay prohibitive clearances rather than test the doctrine in court, and their voices are underrepresented in statutory interpretation.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__creator_centric_reading, small_documentary_filmmakers, excluded,
    powerless, biographical, trapped, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(fair_use_four_factor_test__creator_centric_reading, rights_holders).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves creator incentives by granting limited, enforceable monopoly rights over reproductions and derivatives, aiming to solve the public-goods problem where copies are cheap and original creation is costly.
% TRANSFER_FUNCTION: Moves control over cultural works and derivative markets from transformative users and the public domain to rights holders, via judicial and statutory narrowing of the fair-use exception.
% ABSENT_VOICES: Small documentary filmmakers, remix artists without litigation resources, and public-domain archivists are structurally underrepresented; their reliance on fair use is chilled before it reaches adjudication.
% DISAPPEARANCE_RATIONALE: If the creator-centric four-factor framework vanished, rights holders would lose the doctrinal tool to enclose derivative markets; transformative reuse would expand, licensing revenue models would shift, and the boundary between protected work and public domain would be redrawn.
% FOUNDING_PROBLEM: The market for creative works suffers from free-rider problems where copies are cheap and originals are costly; without limited exclusivity, creators might underproduce.
% FOUNDING_PROBLEM_CORROBORATION: Rights-holder lobbies and some law-and-economics scholars attest the problem is live. User advocacy organizations, empirical studies on non-proprietary creative production (open source, fan culture), and independent legal scholars attest it is overstated or dead, noting the doctrine now serves enclosure more than incentive.
narrative_ontology:disappearance_verdict(fair_use_four_factor_test__creator_centric_reading, world_rearranges).
narrative_ontology:founding_problem_status(fair_use_four_factor_test__creator_centric_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fair_use_four_factor_test__creator_centric_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(fair_use_four_factor_test__creator_centric_reading, 'none', 1).
narrative_ontology:epsilon_provenance(fair_use_four_factor_test__creator_centric_reading, 0.78, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fair_use_four_factor_test__creator_centric_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(fair_use_four_factor_test__creator_centric_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(fair_use_four_factor_test__creator_centric_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.78) is high because the creator-centric reading systematically transfers control of derivative and secondary markets to rights holders, far beyond what is necessary to elicit the original work. Suppression (0.72) is high because statutory damages and preliminary injunctive relief create a chilling effect that suppresses transformative use before it reaches adjudication. Theater_ratio (0.45) reflects the performative neutrality of the four-factor balancing test, which appears to weigh all uses even as outcomes structurally favor the creator. Accessibility_collapse (0.60) captures the closure of alternatives: licensing is available in theory but often priced prohibitively or withheld entirely, and the public domain is progressively enclosed. Resistance (0.55) is moderate: user advocacy and some lower-court judges push back, but rights-holder lobbies and the statutory text dominate.
 *
 * PERSPECTIVAL GAP:
 *   From the rights-holder seat, the constraint is a necessary rope that prevents market failure in creative goods; from the transformative-user seat, it is an actively enforced snare that extracts licensing rents and suppresses legitimate reuse. The engine resolves this divergence from the structural data rather than the narrative claim. The authored claim of tangled_rope acknowledges both functions without adjudicating the perspectival dispute.
 *
 * DIRECTIONALITY LOGIC:
 *   Rights_holders are declared beneficiaries with mobile exit options; the engine will derive a low directionality (d near 0.0), damping effective extraction into a net subsidy. Transformative_users and access_seekers are declared victims with constrained or trapped exit; the engine derives high directionality (d near 1.0), amplifying effective extraction. The copyright_judiciary sits at institutional power with analytical exit, placing it near neutral. The structural asymmetry is sharp: the same legal doctrine that subsidizes rights-holder control penalizes follow-on creators.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâunderproduction of creative works due to free-ridingâis contested. Empirical evidence from open-source communities, fan fiction, and markets with broader fair use suggests the problem is overstated. Nevertheless, the constraint is not a pure piton because rights holders continue to invest real resources in lobbying for and litigating under the creator-centric reading, and some genuine incentive effect likely persists at the margin. The classification as tangled_rope captures this hybrid state: the coordination story is not entirely cover, but the extraction has grown far beyond the coordination need.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_contest_resolution,
    'Does the statutory text of the four-factor test structurally compel a creator-centric reading, or is the transformative-use reading equally textually available?',
    'Comparative doctrinal analysis of judicial opinions and statutory history; empirical measurement of outcome variance across circuits with different dominant readings.',
    'If the text compels creator-centrism, the constraint approaches a mountain-like legal structure; if the reading is discretionary, the constraint is a constructed extraction mechanism maintained by judicial choice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_contest_resolution, conceptual, 'Whether the kernel text dictates this reading or permits alternatives.').

omega_variable(
    incentive_effect_empirical_validity,
    'Does narrowing fair use actually increase creative production, or merely redistribute rents to existing rights holders?',
    'Cross-national natural experiments comparing creative output metrics in jurisdictions with broader vs. narrower fair use doctrines, controlling for GDP and education.',
    'If incentive effects are negligible, the coordination function is cover and the constraint reclassifies toward snare; if real, it remains tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(incentive_effect_empirical_validity, empirical, 'Empirical validity of the incentive justification.').

omega_variable(
    chilling_effect_quantification,
    'How much derivative and transformative production is suppressed by the chilling effect of narrow fair use interpretation?',
    'Studies of abandoned or licensed-away projects that would have proceeded under a broader fair use regime; litigation cost surveys of transformative creators.',
    'Quantifies the victim-side extraction, distinguishing deadweight loss from legitimate incentive preservation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(chilling_effect_quantification, empirical, 'Magnitude of chilling effect on transformative users.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fair_use_four_factor_test__creator_centric_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fair_use_cc_tr_t0, fair_use_four_factor_test__creator_centric_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(fair_use_cc_tr_t10, fair_use_four_factor_test__creator_centric_reading, theater_ratio, 10, 0.28).
narrative_ontology:measurement(fair_use_cc_tr_t20, fair_use_four_factor_test__creator_centric_reading, theater_ratio, 20, 0.35).
narrative_ontology:measurement(fair_use_cc_tr_t30, fair_use_four_factor_test__creator_centric_reading, theater_ratio, 30, 0.4).
narrative_ontology:measurement(fair_use_cc_tr_t40, fair_use_four_factor_test__creator_centric_reading, theater_ratio, 40, 0.45).

% Extraction over time
narrative_ontology:measurement(fair_use_cc_be_t0, fair_use_four_factor_test__creator_centric_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(fair_use_cc_be_t10, fair_use_four_factor_test__creator_centric_reading, base_extractiveness, 10, 0.62).
narrative_ontology:measurement(fair_use_cc_be_t20, fair_use_four_factor_test__creator_centric_reading, base_extractiveness, 20, 0.68).
narrative_ontology:measurement(fair_use_cc_be_t30, fair_use_four_factor_test__creator_centric_reading, base_extractiveness, 30, 0.74).
narrative_ontology:measurement(fair_use_cc_be_t40, fair_use_four_factor_test__creator_centric_reading, base_extractiveness, 40, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(fair_use_cc_su_t0, fair_use_four_factor_test__creator_centric_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(fair_use_cc_su_t10, fair_use_four_factor_test__creator_centric_reading, suppression_requirement, 10, 0.58).
narrative_ontology:measurement(fair_use_cc_su_t20, fair_use_four_factor_test__creator_centric_reading, suppression_requirement, 20, 0.65).
narrative_ontology:measurement(fair_use_cc_su_t30, fair_use_four_factor_test__creator_centric_reading, suppression_requirement, 30, 0.69).
narrative_ontology:measurement(fair_use_cc_su_t40, fair_use_four_factor_test__creator_centric_reading, suppression_requirement, 40, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fair_use_four_factor_test__creator_centric_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(fair_use_four_factor_test__creator_centric_reading, fair_use_four_factor_test__user_centric_reading).
narrative_ontology:affects_constraint(fair_use_four_factor_test__creator_centric_reading, fair_use_four_factor_test__transformative_use_reading).

% DUAL FORMULATION NOTE:
% The natural-language label 'fair use' conflates three structurally distinct readings. Each reading is authored as a separate constraint story with its own epsilon, beneficiary/victim structure, and classification, linked in a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
