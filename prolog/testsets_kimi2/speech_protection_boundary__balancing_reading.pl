% ============================================================================
% CONSTRAINT STORY: speech_protection_boundary__balancing_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_speech_protection_boundary__balancing_reading, []).

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
 *   constraint_id: speech_protection_boundary__balancing_reading
 *   human_readable: First Amendment Balancing Test (Case-by-Case Weighing)
 *   domain: constitutional_law/political_philosophy
 *
 * SUMMARY:
 *   This constraint instantiates the balancing reading of the First Amendment
 *   speech protection kernel: courts determine whether expression is
 *   protected by weighing First Amendment interests against competing
 *   constitutional values and demonstrated harms on a case-by-case basis.
 *   Unlike the absolutist reading (near-categorical protection) or the
 *   harm-limited reading (categorical exclusion of harmful speech), the
 *   balancing reading produces a shifting, context-dependent boundary. The
 *   judiciary acts as distributed gatekeeper, and the doctrine's persistence
 *   depends on active judicial enforcement through constitutional review.
 *
 * KEY AGENTS:
 *   - Federal Judiciary: Agenda-setter (institutional/analytical) â administers the balancing test and accumulates interpretive authority.
 *   - State Regulators: Beneficiary (institutional/constrained) â gain regulatory breathing room when harms are weighed against speech.
 *   - Dissident Speakers: Primary target (powerless/constrained) â bear uncertainty and chilling costs.
 *   - Controversial Publishers: Secondary target (organized/constrained) â face litigation risk and self-censorship incentives.
 *   - Constitutional Absolutists: Excluded voice (organized/trapped) â structurally marginalized by the doctrine's methodological premises.
 *   - Legal Academics: Analytical observer (analytical/analytical) â map and critique the doctrine without controlling it.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(speech_protection_boundary__balancing_reading, 0.58).
domain_priors:suppression_score(speech_protection_boundary__balancing_reading, 0.68).
domain_priors:theater_ratio(speech_protection_boundary__balancing_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(speech_protection_boundary__balancing_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(speech_protection_boundary__balancing_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(speech_protection_boundary__balancing_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(speech_protection_boundary__balancing_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(speech_protection_boundary__balancing_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speech_protection_boundary__balancing_reading, tangled_rope).
narrative_ontology:human_readable(speech_protection_boundary__balancing_reading, "First Amendment Balancing Test (Case-by-Case Weighing)").
narrative_ontology:topic_domain(speech_protection_boundary__balancing_reading, "constitutional_law/political_philosophy").

domain_priors:requires_active_enforcement(speech_protection_boundary__balancing_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(speech_protection_boundary__balancing_reading, 'c6143081-f8f2-4af9-9575-a9b17e741397').
narrative_ontology:cs_kernel_codification('c6143081-f8f2-4af9-9575-a9b17e741397', fixed_text).
narrative_ontology:cs_authority_grounding('c6143081-f8f2-4af9-9575-a9b17e741397', lineage).
narrative_ontology:cs_interpretation_layer_present('c6143081-f8f2-4af9-9575-a9b17e741397').
narrative_ontology:cs_reading_relation('c6143081-f8f2-4af9-9575-a9b17e741397', speech_protection_boundary__absolutist_reading, coexists_with).
narrative_ontology:cs_reading_relation('c6143081-f8f2-4af9-9575-a9b17e741397', speech_protection_boundary__harm_limited_reading, coexists_with).
narrative_ontology:cs_axiom('c6143081-f8f2-4af9-9575-a9b17e741397', foundational, constitutional_values_are_commensurable).
narrative_ontology:cs_axiom_status(constitutional_values_are_commensurable, holdable).
narrative_ontology:cs_axiom_grounding('c6143081-f8f2-4af9-9575-a9b17e741397', constitutional_values_are_commensurable, conventional).
narrative_ontology:cs_axiom('c6143081-f8f2-4af9-9575-a9b17e741397', foundational, judicial_discretion_is_necessary_for_speech_boundaries).
narrative_ontology:cs_axiom_status(judicial_discretion_is_necessary_for_speech_boundaries, holdable).
narrative_ontology:cs_axiom_grounding('c6143081-f8f2-4af9-9575-a9b17e741397', judicial_discretion_is_necessary_for_speech_boundaries, conventional).
narrative_ontology:cs_reference_frame('c6143081-f8f2-4af9-9575-a9b17e741397', ad_hoc_judicial_balancing).
narrative_ontology:cs_drift_state('c6143081-f8f2-4af9-9575-a9b17e741397', contemporary_legal_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('c6143081-f8f2-4af9-9575-a9b17e741397', '').
narrative_ontology:cs_kernel_id(speech_protection_boundary__balancing_reading, speech_protection_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speech_protection_boundary__balancing_reading, federal_judiciary).
narrative_ontology:constraint_beneficiary(speech_protection_boundary__balancing_reading, state_regulators).
narrative_ontology:constraint_victim(speech_protection_boundary__balancing_reading, dissident_speakers).
narrative_ontology:constraint_victim(speech_protection_boundary__balancing_reading, controversial_publishers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Retains ultimate interpretive authority over speech boundaries through case-by-case adjudication. Each case reinforces the judiciary's role as the indispensable gatekeeper between expressive liberty and competing constitutional values. Can revise doctrine through new opinions but remains structurally embedded in the precedent system it administers.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__balancing_reading, federal_judiciary, agenda_setter,
    institutional, generational, analytical, national).

% Benefit from regulatory latitude when courts weigh state interests (public order, equality, dignity) against speech claims. Their regulations survive judicial review if the balancing test finds the harm sufficient, giving them a fluid but real expansion of police power compared to categorical protection regimes.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__balancing_reading, state_regulators, beneficiary,
    institutional, biographical, constrained, national).

% Bear the cost of doctrinal uncertainty: cannot know in advance whether their political dissent will be protected because protection depends on a post-hoc judicial weighing of interests against harms. Face chilling effect and litigation costs with limited resources to influence the balancing calculus.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__balancing_reading, dissident_speakers, payer,
    powerless, immediate, constrained, national).

% Publish content touching systemic harms, coded speech, or divisive topics. Must litigate or self-censor because the boundary of protection shifts with judicial mood and social context. Larger organizations can fund litigation; smaller outlets fold under liability risk.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__balancing_reading, controversial_publishers, payer,
    organized, biographical, constrained, national).

% Argue that any balancing of speech against other values betrays the First Amendment's categorical command. Their methodological position is represented in dissents and academic literature but is structurally excluded from the majority doctrine, which treats their view as legally irrelevant to the standard analysis.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__balancing_reading, constitutional_absolutists, excluded,
    organized, generational, trapped, national).

% Analyze and critique the doctrine from outside the bench. Produce empirical studies on chilling effects, comparative constitutional analyses, and normative arguments. Their work influences the discourse but does not determine case outcomes.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__balancing_reading, legal_academics, observer,
    analytical, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves unavoidable conflicts between expressive liberty and other constitutional values (equality, dignity, public safety, privacy) through a unified judicial methodology that prevents either value from automatically extinguishing the other.
% TRANSFER_FUNCTION: Moves interpretive authority over speech boundaries from categorical legislative or textual rules to case-specific judicial weighing; moves the costs of uncertainty, litigation, and chilling effects to speakers and publishers while expanding regulatory breathing room for the state.
% ABSENT_VOICES: Constitutional absolutists who reject the commensurability of speech with other values; speakers from jurisdictions with categorical protections who would argue that the uncertainty itself is an unconstitutional tax on expression; democratic majorities whose regulatory preferences are overridden or enabled unpredictably.
% DISAPPEARANCE_RATIONALE: If the balancing test vanished overnight, speech jurisprudence would revert toward categorical rules (near-absolute protection under absolutism or harm-limited conditional protection). Judicial dockets would shift from fact-intensive balancing to threshold categorization; state regulatory space would either contract sharply or expand predictably depending on which replacement doctrine dominated.
% FOUNDING_PROBLEM: Early twentieth-century speech doctrine (bad-tendency test) permitted nearly unlimited suppression of dissent under vague claims of public harm, collapsing the boundary between protected advocacy and punishable conduct.
% FOUNDING_PROBLEM_CORROBORATION: Historical scholarship from outside the judiciary documents the bad-tendency era's suppression of labor and anti-war speech. Civil liberties organizations attest that balancing continues to enable significant chilling. Originalist scholars contest that the founding problem justifies judicial discretion rather than textual categorical protection.
narrative_ontology:disappearance_verdict(speech_protection_boundary__balancing_reading, world_rearranges).
narrative_ontology:founding_problem_status(speech_protection_boundary__balancing_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(speech_protection_boundary__balancing_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(speech_protection_boundary__balancing_reading, 'none', 1).
narrative_ontology:epsilon_provenance(speech_protection_boundary__balancing_reading, 0.58, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(speech_protection_boundary__balancing_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(speech_protection_boundary__balancing_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(speech_protection_boundary__balancing_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) reflects the significant but not total extraction from speakers: the chilling effect is real but partial, as many forms of speech remain protected. Suppression (0.68) is higher because the constraint's operation depends on judicial enforcement of state-favorable outcomes and the implicit threat of liability. Theater_ratio (0.25) is relatively low because judicial opinions are substantive performances, though the ritual of 'weighing' can obscure policy-driven outcomes. Accessibility_collapse (0.65) captures how categorical alternatives (absolutist or harm-limited) have been marginalized in American doctrine despite their prominence elsewhere. Resistance (0.45) reflects ongoing scholarly and judicial dissent. The cyclical measurement pattern (U-shaped from 1950â2020) reflects Cold War suppression, libertarian retrenchment, and post-9/11/security-state re-expansion.
 *
 * PERSPECTIVAL GAP:
 *   From the federal judiciary's seat, the constraint is necessary coordination among incommensurable constitutional values in a pluralistic society; from the dissident speaker's seat, it is unpredictable extraction of expressive security. The engine computes this divergence: the agenda-setter/beneficiary seats (low d) experience coordination authority, while the payer seats (high d, constrained exit) experience extraction. State regulators sit between, benefiting from the coordination but constrained by judicial supremacy.
 *
 * DIRECTIONALITY LOGIC:
 *   Federal judiciary and state regulators derive low directionality (near-beneficiary) because the constraint expands their institutional authority and regulatory space respectively. Dissident speakers and controversial publishers derive high directionality (near-target) because they bear the costs of uncertainty and liability. The derivation follows from beneficiary/victim declarations combined with exit modulations: analytical exit for the judiciary dampens effective extraction into institutional authority accumulation, while constrained exit for speakers amplifies it into chilling effect.
 *
 * MANDATROPHY ANALYSIS:
 *   The balancing framework prevents mislabeling in both directions. Without acknowledging its coordination function, it would appear as a pure judicial power grab (snare); the doctrine genuinely resolves value conflicts that categorical rules cannot easily address. Without acknowledging its extraction, it would appear as a neutral rope; the case-by-case method systematically advantages institutional litigants and state actors over resource-poor speakers. The Tangled Rope classification captures that both coordination and extraction are structurally present and jointly necessary for the constraint's persistence.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_location,
    'Is the case-by-case balancing test a reading intrinsic to the First Amendment kernel, or an interpretive innovation that displaces the text''s apparent categorical structure (''Congress shall make no law'')?',
    'Founding-era historical analysis of speech regulation practices; originalist textual analysis of ''abridging'' and ''the freedom of speech''; comparative study of other constitutional texts that explicitly authorize proportionality analysis.',
    'If the kernel is categorical, the balancing reading functions as extractive judicial construction (high d for the judiciary as beneficiary of expanded power). If the kernel is open-textured, the reading is a natural coordination mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_location, conceptual, 'Whether the balancing reading is grounded in the constitutional kernel or is an external imposition.').

omega_variable(
    commensurability_assumption,
    'Can First Amendment expressive interests be meaningfully weighed against dignity harms, equality impacts, and national security claims, or does the balancing framework presuppose a commensurability that does not exist?',
    'Philosophical analysis of incommensurable values; empirical study of judicial outcomes to detect whether ''balancing'' produces predictable weights or merely ratifies judges'' priors.',
    'If values are incommensurable, the constraint is largely performative (theater rises) and extraction consists in arbitrary judicial power; if commensurable, the constraint provides genuine information coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(commensurability_assumption, conceptual, 'Whether constitutional values subjected to balancing are truly commensurable.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the chilling effect on speakers structural (actual prosecutions and civil liability) or internalized (self-censorship driven by uncertainty about where the line falls)?',
    'Post-decision suppression trajectory studies: measure publication rates and content shifts in jurisdictions after major balancing decisions. If suppression persists without enforcement, it is internalized.',
    'If internalized, effective extraction exceeds the structural measure because speakers carry the constraint with them even in the absence of state action; this would also raise the theater_ratio as the doctrine''s visible enforcement understates its actual reach.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs internalized suppression mechanism in speech chilling.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_protection_boundary__balancing_reading, 0, 70).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sp_bal_tr_t0, speech_protection_boundary__balancing_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(sp_bal_tr_t14, speech_protection_boundary__balancing_reading, theater_ratio, 14, 0.12).
narrative_ontology:measurement(sp_bal_tr_t28, speech_protection_boundary__balancing_reading, theater_ratio, 28, 0.15).
narrative_ontology:measurement(sp_bal_tr_t42, speech_protection_boundary__balancing_reading, theater_ratio, 42, 0.18).
narrative_ontology:measurement(sp_bal_tr_t56, speech_protection_boundary__balancing_reading, theater_ratio, 56, 0.22).
narrative_ontology:measurement(sp_bal_tr_t70, speech_protection_boundary__balancing_reading, theater_ratio, 70, 0.25).

% Extraction over time
narrative_ontology:measurement(sp_bal_be_t0, speech_protection_boundary__balancing_reading, base_extractiveness, 0, 0.68).
narrative_ontology:measurement(sp_bal_be_t14, speech_protection_boundary__balancing_reading, base_extractiveness, 14, 0.55).
narrative_ontology:measurement(sp_bal_be_t28, speech_protection_boundary__balancing_reading, base_extractiveness, 28, 0.42).
narrative_ontology:measurement(sp_bal_be_t42, speech_protection_boundary__balancing_reading, base_extractiveness, 42, 0.45).
narrative_ontology:measurement(sp_bal_be_t56, speech_protection_boundary__balancing_reading, base_extractiveness, 56, 0.52).
narrative_ontology:measurement(sp_bal_be_t70, speech_protection_boundary__balancing_reading, base_extractiveness, 70, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(sp_bal_su_t0, speech_protection_boundary__balancing_reading, suppression_requirement, 0, 0.75).
narrative_ontology:measurement(sp_bal_su_t14, speech_protection_boundary__balancing_reading, suppression_requirement, 14, 0.6).
narrative_ontology:measurement(sp_bal_su_t28, speech_protection_boundary__balancing_reading, suppression_requirement, 28, 0.48).
narrative_ontology:measurement(sp_bal_su_t42, speech_protection_boundary__balancing_reading, suppression_requirement, 42, 0.52).
narrative_ontology:measurement(sp_bal_su_t56, speech_protection_boundary__balancing_reading, suppression_requirement, 56, 0.62).
narrative_ontology:measurement(sp_bal_su_t70, speech_protection_boundary__balancing_reading, suppression_requirement, 70, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(speech_protection_boundary__balancing_reading, absolutist_reading).
narrative_ontology:affects_constraint(speech_protection_boundary__balancing_reading, harm_limited_reading).

% DUAL FORMULATION NOTE:
% The speech_protection_boundary kernel decomposes into three structurally distinct constraints: absolutist_reading (categorical protection), balancing_reading (context-dependent weighing), and harm_limited_reading (categorical harm exclusion). Each has a distinct epsilon, beneficiary/victim structure, and gatekeeper configuration. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
