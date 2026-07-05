% ============================================================================
% CONSTRAINT STORY: hoa_covenant_scope__behavioral_control_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hoa_covenant_scope__behavioral_control_reading, []).

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
 *   constraint_id: hoa_covenant_scope__behavioral_control_reading
 *   human_readable: HOA Covenant as Aesthetic/Behavioral Conformity Enforcement
 *   domain: property_law/collective_governance/urban_planning
 *
 * SUMMARY:
 *   This story instantiates the behavioral-control reading of the HOA
 *   covenant kernel: the covenant is read as an aesthetic-uniformity and
 *   lifestyle-conformity enforcement mechanism, justified as property-value
 *   maximization but operating primarily to suppress visible difference —
 *   including political speech (yard signs, flags), nonconforming aesthetic
 *   choices, and lifestyle markers that deviate from an implicit household
 *   norm. This is deliberately distinct from the coordination_reading
 *   (genuine shared-infrastructure and externality management) and the
 *   extraction_reading (revenue generation and board power consolidation via
 *   fines) — those are separate constraints with their own ε values, not
 *   alternative measurements of this one. Under ε-invariance, conflating them
 *   would produce an unstable ε; they are authored as three linked but
 *   independent stories.
 *
 * KEY AGENTS:
 *   - board_aligned_homeowners: primary beneficiary (organized/mobile) — shapes and rarely faces the standard
 *   - conformist_majority: primary beneficiary (organized/mobile) — ratifies the standard, benefits from settled aesthetic disputes
 *   - architectural_review_board: agenda_setter (institutional/constrained) — interprets and enforces subjective standards
 *   - aesthetic_nonconformists: primary target (powerless/trapped) — fined for visible difference
 *   - political_speech_residents: primary target (powerless/trapped) — speech suppressed under appearance clauses
 *   - lifestyle_minority_households: primary target (powerless/trapped) — living patterns policed under 'residential character' clauses
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hoa_covenant_scope__behavioral_control_reading, 0.42).
domain_priors:suppression_score(hoa_covenant_scope__behavioral_control_reading, 0.58).
domain_priors:theater_ratio(hoa_covenant_scope__behavioral_control_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hoa_covenant_scope__behavioral_control_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(hoa_covenant_scope__behavioral_control_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(hoa_covenant_scope__behavioral_control_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hoa_covenant_scope__behavioral_control_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(hoa_covenant_scope__behavioral_control_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hoa_covenant_scope__behavioral_control_reading, snare).
narrative_ontology:human_readable(hoa_covenant_scope__behavioral_control_reading, "HOA Covenant as Aesthetic/Behavioral Conformity Enforcement").
narrative_ontology:topic_domain(hoa_covenant_scope__behavioral_control_reading, "property_law/collective_governance/urban_planning").

domain_priors:requires_active_enforcement(hoa_covenant_scope__behavioral_control_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hoa_covenant_scope__behavioral_control_reading, '1da50e3b-446f-48f3-be56-09c97c6f3346').
narrative_ontology:cs_kernel_codification('1da50e3b-446f-48f3-be56-09c97c6f3346', formalized).
narrative_ontology:cs_authority_grounding('1da50e3b-446f-48f3-be56-09c97c6f3346', extraction).
narrative_ontology:cs_interpretation_layer_present('1da50e3b-446f-48f3-be56-09c97c6f3346').
narrative_ontology:cs_reading_relation('1da50e3b-446f-48f3-be56-09c97c6f3346', hoa_covenant_scope__coordination_reading, coexists_with).
narrative_ontology:cs_reading_relation('1da50e3b-446f-48f3-be56-09c97c6f3346', hoa_covenant_scope__extraction_reading, influences).
narrative_ontology:cs_axiom('1da50e3b-446f-48f3-be56-09c97c6f3346', foundational, visible_uniformity_is_a_legitimate_collective_interest).
narrative_ontology:cs_axiom_status(visible_uniformity_is_a_legitimate_collective_interest, holdable).
narrative_ontology:cs_axiom_grounding('1da50e3b-446f-48f3-be56-09c97c6f3346', visible_uniformity_is_a_legitimate_collective_interest, instrumental).
narrative_ontology:cs_axiom('1da50e3b-446f-48f3-be56-09c97c6f3346', foundational, subjective_aesthetic_judgment_may_override_individual_expression).
narrative_ontology:cs_axiom_status(subjective_aesthetic_judgment_may_override_individual_expression, holdable).
narrative_ontology:cs_axiom_grounding('1da50e3b-446f-48f3-be56-09c97c6f3346', subjective_aesthetic_judgment_may_override_individual_expression, conventional).
narrative_ontology:cs_reference_frame('1da50e3b-446f-48f3-be56-09c97c6f3346', developer_era_marketing_uniformity_standard).
narrative_ontology:cs_drift_state('1da50e3b-446f-48f3-be56-09c97c6f3346', contemporary_fair_housing_and_free_speech_scrutiny, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('1da50e3b-446f-48f3-be56-09c97c6f3346', '').
narrative_ontology:cs_kernel_id(hoa_covenant_scope__behavioral_control_reading, hoa_covenant_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hoa_covenant_scope__behavioral_control_reading, conformist_majority).
narrative_ontology:constraint_beneficiary(hoa_covenant_scope__behavioral_control_reading, board_aligned_homeowners).
narrative_ontology:constraint_victim(hoa_covenant_scope__behavioral_control_reading, aesthetic_nonconformists).
narrative_ontology:constraint_victim(hoa_covenant_scope__behavioral_control_reading, political_speech_residents).
narrative_ontology:constraint_victim(hoa_covenant_scope__behavioral_control_reading, lifestyle_minority_households).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Homeowners whose tastes and lifestyles already match the covenant's baked-in norms (uniform landscaping, muted exterior colors, no visible political signage, no home businesses). They rarely face enforcement, sit on or elect the architectural review board, and experience the covenant as effortless background order that protects the neighborhood's look and, they believe, its resale value.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__behavioral_control_reading, board_aligned_homeowners, beneficiary,
    organized, biographical, mobile, local).
narrative_ontology:stakeholder_secondary_role(hoa_covenant_scope__behavioral_control_reading, board_aligned_homeowners, agenda_setter).

% The voting bloc that ratified and continues to ratify the covenant's aesthetic and behavioral provisions, reasoning that visible uniformity signals stability to future buyers. They benefit from not having to negotiate with neighbors over paint colors, yard displays, or vehicle storage — the covenant settles those disputes in advance, in their preferred direction.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__behavioral_control_reading, conformist_majority, beneficiary,
    organized, biographical, mobile, local).

% Volunteer or semi-professional body that interprets vague aesthetic standards ('harmonious with the neighborhood,' 'unobtrusive') on a case-by-case basis, issues violation notices, and can compel removal of noncompliant items under threat of fines or liens. Its members are themselves homeowners subject to reelection by the conformist majority, which shapes which complaints get pursued.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__behavioral_control_reading, architectural_review_board, agenda_setter,
    institutional, biographical, constrained, local).

% Residents whose preferred garden style, exterior color, or artistic displays fall outside the board's subjective comfort zone. They face repeated violation letters and escalating fines for choices that harm no one materially; selling and moving is the only real exit, and it comes at real financial and social cost.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__behavioral_control_reading, aesthetic_nonconformists, payer,
    powerless, biographical, trapped, local).

% Homeowners who want to display yard signs, flags, or seasonal decorations expressing political or personal views. The covenant's uniform-appearance clauses are invoked to force removal, effectively suppressing visible expression that has no bearing on structural upkeep or shared costs.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__behavioral_control_reading, political_speech_residents, payer,
    powerless, immediate, trapped, local).

% Multigenerational families, home-based workers, or households whose visible living patterns (parked work vehicles, multiple cars, non-nuclear family configurations) draw scrutiny under 'residential character' clauses that were written with a narrow household model in mind. They absorb fines or must alter private living arrangements to avoid conflict.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__behavioral_control_reading, lifestyle_minority_households, payer,
    powerless, biographical, trapped, local).

% Weigh the covenant's promised uniformity against the visible enforcement friction when deciding whether to buy in. Some are attracted by the conformity promise; others are warned off by disclosed violation histories and litigation records.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__behavioral_control_reading, prospective_buyers, observer,
    moderate, immediate, mobile, local).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(hoa_covenant_scope__behavioral_control_reading, conformist_majority).
narrative_ontology:fixing_cost_class(hoa_covenant_scope__behavioral_control_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: In its behavioral-control framing, the covenant claims to coordinate a shared aesthetic standard that maximizes collective property value by preventing any single owner's visible choices from 'dragging down' neighborhood appearance.
% TRANSFER_FUNCTION: Moves discretion over private aesthetic and lifestyle choices from individual homeowners to the board and the conformist majority that elects it; moves money from cited nonconformists to the association via fines, and moves expressive freedom away from residents whose speech or lifestyle markers are visible from the street.
% ABSENT_VOICES: Renters and future owners not yet in the neighborhood have no vote on the standards that will bind them. Residents who have already been fined into silence or who moved away after repeated disputes are not present to testify at board meetings where new restrictions are adopted.
% DISAPPEARANCE_RATIONALE: The conformist majority insists property values would fall and disputes over noise, storage, and appearance would multiply without covenant enforcement. Nonconformist and speech-suppressed residents argue the neighborhood would look and function almost identically, minus the fines, notices, and forced removals — genuine externality disputes (noise, structural upkeep, shared infrastructure) would still be handled by ordinary nuisance law and municipal code, which predate and survive the covenant.
% FOUNDING_PROBLEM: Original developers and early buyers wanted assurance that neighboring lots would not be used in ways that visibly undercut the marketing image used to sell the subdivision — protecting the appearance that justified the initial price premium.
% FOUNDING_PROBLEM_CORROBORATION: The board and conformist majority attest the appearance-protection problem remains live and cite resale-comparison data. Real estate researchers and fair-housing advocates outside the association's membership have documented that many aesthetic covenants outlive any measurable resale effect and increasingly function as vehicles for enforcing subjective taste and suppressing disfavored speech rather than protecting value; several municipal fair-housing offices have fielded complaints characterizing enforcement patterns as selectively targeting minority or lower-income households within otherwise-uniform developments.
narrative_ontology:disappearance_verdict(hoa_covenant_scope__behavioral_control_reading, contested).
narrative_ontology:founding_problem_status(hoa_covenant_scope__behavioral_control_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hoa_covenant_scope__behavioral_control_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(hoa_covenant_scope__behavioral_control_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hoa_covenant_scope__behavioral_control_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hoa_covenant_scope__behavioral_control_reading_tests).
:- end_tests(hoa_covenant_scope__behavioral_control_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.42) sits in the moderate band the reading predicts: real financial cost via fines and forced removals, but bounded by the fact that most enforcement targets appearance rather than large monetary transfer. Suppression (0.58) is higher, reflecting that the mechanism depends on continued willingness to police and cite deviation — the enforcement apparatus (subjective standards, escalating notices, liens) has to be actively maintained to hold uniformity in place. Theater ratio is moderate-low (0.30) because the appearance-protection function is not purely performative — it does correlate with some marketability claims — but a rising share of activity over the interval targets political and lifestyle expression that has no plausible link to structural value.
 *
 * PERSPECTIVAL GAP:
 *   From the board-aligned/conformist-majority seat, the covenant looks like successful, low-friction coordination: disputes over taste are resolved once, in advance, and everyone benefits from a predictable streetscape. From the nonconformist, speech-suppressed, and lifestyle-minority seats, the identical clauses operate as an enforcement regime that reaches into private expression and living arrangements with no genuine externality at stake — the engine's computed divergence between these seats is the structural signal this reading exists to surface.
 *
 * DIRECTIONALITY LOGIC:
 *   Board-aligned homeowners and the conformist majority are declared beneficiaries: they set or elect the standard, rarely face it, and gain the settled-dispute benefit — low d, near the beneficiary end. Aesthetic nonconformists, political-speech residents, and lifestyle-minority households are declared victims: trapped exit (selling is costly and slow), powerless relative to the board, and bear the fines/removal costs directly — high d, near the full-target end. The architectural review board sits institutionally between: it administers rather than purely benefits, but its composition is captured by the majority that elects it, which is why it is not declared a beneficiary despite proximity to the extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (protecting a marketing-era appearance premise) is contested as still live: board seats defend it with resale comparisons, but corroboration from outside the association — fair-housing offices and housing researchers — documents enforcement drifting toward taste and speech policing untethered from measurable value effects. Classifying this reading as a snare rather than folding it into the coordination_reading prevents mislabeling a genuinely extractive, exclusionary enforcement pattern as innocuous shared-infrastructure coordination; the coordination function these clauses claim is real for structural upkeep provisions but not for aesthetic/behavioral ones, which is exactly why the two are separate constraints in this family.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    aesthetic_uniformity_value_link,
    'Does enforced aesthetic/behavioral uniformity actually produce a measurable property-value premium, or is the value-maximization justification a post-hoc rationalization for taste and speech policing that would persist even if no value effect existed?',
    'Comparative resale-price studies between covenant-enforced and covenant-light developments matched on location, age, and amenity, controlling for the correlation between covenant presence and other value-relevant factors (school district, HOA-funded amenities).',
    'If no measurable premium exists, the behavioral-control reading''s stated justification collapses and the constraint reads as pure preference-enforcement (moving it further toward snare); if a real premium exists specifically for the aesthetic/behavioral clauses, the reading would sit closer to a hybrid coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(aesthetic_uniformity_value_link, empirical, 'Whether enforced uniformity produces the claimed value effect or only rationalizes conformity enforcement.').

omega_variable(
    speech_suppression_severability,
    'Are the political-speech and lifestyle-restriction clauses severable from the legitimate structural/aesthetic clauses, or are they structurally entangled in the same ''harmonious appearance'' language such that removing the speech-suppressive reach would require rewriting the aesthetic standard itself?',
    'Track jurisdictions that have passed statutory carve-outs protecting yard signs/flags from HOA aesthetic enforcement: if boards adapt cleanly by narrowing the standard''s text, the clauses are severable; if enforcement of other aesthetic provisions collapses or becomes arbitrary, they were entangled.',
    'Severability would suggest the speech-suppressive extraction is an add-on to an otherwise-defensible coordination function; entanglement would suggest the vague-standard drafting itself is the extraction mechanism.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(speech_suppression_severability, conceptual, 'Whether behavioral/speech control is separable from legitimate aesthetic coordination within the same covenant text.').

omega_variable(
    kernel_reading_boundary_ambiguity,
    'Where exactly does the coordination_reading''s legitimate aesthetic-consistency function end and this behavioral_control_reading''s conformity-enforcement function begin, given both readings draw on the same covenant clauses?',
    'Clause-by-clause classification exercise sorting covenant provisions by whether violation causes a measurable externality (structural decay, drainage, safety) versus purely subjective/expressive disagreement (color preference, political display, household composition).',
    'A clean clause-level split would validate treating these as genuinely separate constraints per the ε-invariance principle; a messy split (most disputes touching both functions simultaneously) would suggest the kernel''s readings are less separable than modeled and future revisions might need finer-grained per-clause constraints rather than per-reading ones.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_boundary_ambiguity, conceptual, 'How cleanly the coordination and behavioral-control readings partition the underlying covenant text.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hoa_covenant_scope__behavioral_control_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hoa__tr_t0, hoa_covenant_scope__behavioral_control_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(hoa__tr_t4, hoa_covenant_scope__behavioral_control_reading, theater_ratio, 4, 0.2).
narrative_ontology:measurement(hoa__tr_t8, hoa_covenant_scope__behavioral_control_reading, theater_ratio, 8, 0.22).
narrative_ontology:measurement(hoa__tr_t12, hoa_covenant_scope__behavioral_control_reading, theater_ratio, 12, 0.25).
narrative_ontology:measurement(hoa__tr_t16, hoa_covenant_scope__behavioral_control_reading, theater_ratio, 16, 0.27).
narrative_ontology:measurement(hoa__tr_t20, hoa_covenant_scope__behavioral_control_reading, theater_ratio, 20, 0.29).
narrative_ontology:measurement(hoa__tr_t24, hoa_covenant_scope__behavioral_control_reading, theater_ratio, 24, 0.3).

% Extraction over time
narrative_ontology:measurement(hoa__be_t0, hoa_covenant_scope__behavioral_control_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(hoa__be_t4, hoa_covenant_scope__behavioral_control_reading, base_extractiveness, 4, 0.32).
narrative_ontology:measurement(hoa__be_t8, hoa_covenant_scope__behavioral_control_reading, base_extractiveness, 8, 0.35).
narrative_ontology:measurement(hoa__be_t12, hoa_covenant_scope__behavioral_control_reading, base_extractiveness, 12, 0.38).
narrative_ontology:measurement(hoa__be_t16, hoa_covenant_scope__behavioral_control_reading, base_extractiveness, 16, 0.4).
narrative_ontology:measurement(hoa__be_t20, hoa_covenant_scope__behavioral_control_reading, base_extractiveness, 20, 0.41).
narrative_ontology:measurement(hoa__be_t24, hoa_covenant_scope__behavioral_control_reading, base_extractiveness, 24, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(hoa__su_t0, hoa_covenant_scope__behavioral_control_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(hoa__su_t4, hoa_covenant_scope__behavioral_control_reading, suppression_requirement, 4, 0.45).
narrative_ontology:measurement(hoa__su_t8, hoa_covenant_scope__behavioral_control_reading, suppression_requirement, 8, 0.49).
narrative_ontology:measurement(hoa__su_t12, hoa_covenant_scope__behavioral_control_reading, suppression_requirement, 12, 0.52).
narrative_ontology:measurement(hoa__su_t16, hoa_covenant_scope__behavioral_control_reading, suppression_requirement, 16, 0.55).
narrative_ontology:measurement(hoa__su_t20, hoa_covenant_scope__behavioral_control_reading, suppression_requirement, 20, 0.57).
narrative_ontology:measurement(hoa__su_t24, hoa_covenant_scope__behavioral_control_reading, suppression_requirement, 24, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hoa_covenant_scope__behavioral_control_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(hoa_covenant_scope__behavioral_control_reading, 0.08).
narrative_ontology:affects_constraint(hoa_covenant_scope__behavioral_control_reading, hoa_covenant_scope__coordination_reading).
narrative_ontology:affects_constraint(hoa_covenant_scope__behavioral_control_reading, hoa_covenant_scope__extraction_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked constraints decomposing the natural-language label 'HOA covenant' per the ε-invariance principle. coordination_reading models the genuine shared-infrastructure/externality function (low ε, Rope-shaped). extraction_reading models the revenue/board-power-consolidation function via fines and selective enforcement (Tangled-Rope-or-Snare, money-flow centered). This behavioral_control_reading isolates the aesthetic-uniformity and lifestyle/speech-suppression function (moderate ε, Snare-shaped). All three read the same covenant text but instantiate structurally distinct claims with distinct beneficiary/victim sets and distinct ε values; they are linked via affects_constraints rather than merged into one story with a measurement parameter.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
