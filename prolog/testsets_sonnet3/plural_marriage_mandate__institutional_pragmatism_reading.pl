% ============================================================================
% CONSTRAINT STORY: plural_marriage_mandate__institutional_pragmatism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_plural_marriage_mandate__institutional_pragmatism_reading, []).

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
 *   constraint_id: plural_marriage_mandate__institutional_pragmatism_reading
 *   human_readable: 1890 Manifesto as Institutional Pragmatism (Doctrine as Survival Legitimation)
 *   domain: religious_institutional_history/commitment_systems/political_theology
 *
 * SUMMARY:
 *   In 1890, under sustained federal legal assault (Edmunds-Tucker Act
 *   disincorporation, property seizure, disenfranchisement, mass imprisonment
 *   of practitioners), church president Wilford Woodruff issued the Manifesto
 *   publicly ending the sanctioning of new plural marriages, framed in
 *   official church discourse as continued revelation rather than forced
 *   retreat. This reading treats the doctrinal claim itself as the
 *   coordination mechanism that let the institution survive an existential
 *   external threat while preserving its authority structure intact — but
 *   treats that same claim as extracting real costs from powerless parties
 *   who had no say in either the original practice's scale or its abrupt,
 *   selectively-enforced reversal.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(plural_marriage_mandate__institutional_pragmatism_reading, 0.68).
domain_priors:suppression_score(plural_marriage_mandate__institutional_pragmatism_reading, 0.72).
domain_priors:theater_ratio(plural_marriage_mandate__institutional_pragmatism_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(plural_marriage_mandate__institutional_pragmatism_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(plural_marriage_mandate__institutional_pragmatism_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(plural_marriage_mandate__institutional_pragmatism_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(plural_marriage_mandate__institutional_pragmatism_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(plural_marriage_mandate__institutional_pragmatism_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(plural_marriage_mandate__institutional_pragmatism_reading, tangled_rope).
narrative_ontology:human_readable(plural_marriage_mandate__institutional_pragmatism_reading, "1890 Manifesto as Institutional Pragmatism (Doctrine as Survival Legitimation)").
narrative_ontology:topic_domain(plural_marriage_mandate__institutional_pragmatism_reading, "religious_institutional_history/commitment_systems/political_theology").

domain_priors:requires_active_enforcement(plural_marriage_mandate__institutional_pragmatism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(plural_marriage_mandate__institutional_pragmatism_reading, '989bd9b0-20b0-4f9a-bff9-96d2704da4d7').
narrative_ontology:cs_kernel_codification('989bd9b0-20b0-4f9a-bff9-96d2704da4d7', formalized).
narrative_ontology:cs_authority_grounding('989bd9b0-20b0-4f9a-bff9-96d2704da4d7', extraction).
narrative_ontology:cs_interpretation_layer_present('989bd9b0-20b0-4f9a-bff9-96d2704da4d7').
narrative_ontology:cs_reading_relation('989bd9b0-20b0-4f9a-bff9-96d2704da4d7', plural_marriage_mandate__exogenous_override_reading, coexists_with).
narrative_ontology:cs_reading_relation('989bd9b0-20b0-4f9a-bff9-96d2704da4d7', plural_marriage_mandate__endogenous_reinterpretation_reading, influences).
narrative_ontology:cs_axiom('989bd9b0-20b0-4f9a-bff9-96d2704da4d7', foundational, doctrine_functions_as_survival_instrument).
narrative_ontology:cs_axiom_status(doctrine_functions_as_survival_instrument, holdable).
narrative_ontology:cs_axiom_grounding('989bd9b0-20b0-4f9a-bff9-96d2704da4d7', doctrine_functions_as_survival_instrument, instrumental).
narrative_ontology:cs_axiom('989bd9b0-20b0-4f9a-bff9-96d2704da4d7', foundational, revelation_claim_is_legitimation_not_disclosure).
narrative_ontology:cs_axiom_status(revelation_claim_is_legitimation_not_disclosure, holdable).
narrative_ontology:cs_axiom_grounding('989bd9b0-20b0-4f9a-bff9-96d2704da4d7', revelation_claim_is_legitimation_not_disclosure, empirically_contingent).
narrative_ontology:cs_reference_frame('989bd9b0-20b0-4f9a-bff9-96d2704da4d7', prophetic_continuity_survival_doctrine).
narrative_ontology:cs_drift_state('989bd9b0-20b0-4f9a-bff9-96d2704da4d7', post_second_manifesto_1904, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('989bd9b0-20b0-4f9a-bff9-96d2704da4d7', '').
narrative_ontology:cs_kernel_id(plural_marriage_mandate__institutional_pragmatism_reading, plural_marriage_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(plural_marriage_mandate__institutional_pragmatism_reading, church_hierarchy_leadership).
narrative_ontology:constraint_beneficiary(plural_marriage_mandate__institutional_pragmatism_reading, church_institutional_apparatus).
narrative_ontology:constraint_victim(plural_marriage_mandate__institutional_pragmatism_reading, coerced_polygamist_families).
narrative_ontology:constraint_victim(plural_marriage_mandate__institutional_pragmatism_reading, deceived_monogamist_converts).
narrative_ontology:constraint_victim(plural_marriage_mandate__institutional_pragmatism_reading, plural_wives_and_children).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Issues the 1890 Manifesto framed as revelation, publicly ending new plural marriages while privately authorizing and in some documented cases performing continued sealings through roughly 1904. Negotiates directly with federal authorities for institutional survival, amnesty, and eventual statehood/restored voting and property rights. Controls the doctrinal narrative that frames capitulation as continued divine guidance rather than defeat, preserving hierarchical legitimacy across the transition.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__institutional_pragmatism_reading, church_hierarchy_leadership, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(plural_marriage_mandate__institutional_pragmatism_reading, church_hierarchy_leadership, beneficiary).

% The corporate church survives disincorporation threats, recovers seized properties and temple access, and secures a durable path to statehood and political normalization. Its continuity as an institution depends on the Manifesto's public credibility as genuine revelation, which converts an externally forced retreat into an internally sourced doctrinal event.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__institutional_pragmatism_reading, church_institutional_apparatus, beneficiary,
    institutional, civilizational, arbitrage, national).

% Existing plural families face an impossible choice after 1890: publicly dissolve unions and disinherit children, continue in secrecy under threat of prosecution and excommunication, or relocate. Many bear the practical cost of a policy reversal they did not choose and cannot appeal, since dissent from the 'revelation' framing is itself grounds for church discipline.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__institutional_pragmatism_reading, coerced_polygamist_families, payer,
    powerless, biographical, trapped, regional).

% New converts and members who joined or remained on the understanding that plural marriage had been definitively ended in 1890 discover, when secret continuations surface, that leadership's public statements to federal officials and the membership diverged from private practice. They bear a credibility cost and are given no honest account of the gap between the two.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__institutional_pragmatism_reading, deceived_monogamist_converts, payer,
    powerless, biographical, constrained, national).

% Wives in existing plural unions after 1890, and their children, lose legal and social legitimacy overnight by administrative fiat dressed as revelation. Some are pressured into denial of the marriage for legal protection of the husband or the institution; inheritance, legitimacy, and social standing are all downstream of a decision made entirely above them.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__institutional_pragmatism_reading, plural_wives_and_children, payer,
    powerless, biographical, trapped, regional).

% Applies escalating coercive pressure (Edmunds-Tucker Act, disincorporation, disenfranchisement, imprisonment of practitioners) that structurally forces the Manifesto's issuance, then accepts the revelation framing at face value for purposes of restoring statehood negotiations — effectively co-signing the legitimation narrative without being part of its construction or bearing any of its costs.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__institutional_pragmatism_reading, federal_government_authorities, excluded,
    institutional, biographical, analytical, national).

% Documents the gap between the Manifesto's public revelation claim and the archival record of post-1890 plural marriages (the 'Second Manifesto' of 1904 exists precisely because the first failed to stop practice). Their work is often institutionally unwelcome and can carry professional or membership costs, but it is the primary source of the M-set gap evidence this reading rests on.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__institutional_pragmatism_reading, later_church_historians_and_dissidents, observer,
    moderate, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(plural_marriage_mandate__institutional_pragmatism_reading, church_hierarchy_leadership).
narrative_ontology:fixing_cost_class(plural_marriage_mandate__institutional_pragmatism_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides the institution a mechanism to exit an untenable, existentially threatening confrontation with federal power while preserving hierarchical authority, membership cohesion, and a continuous theological self-narrative — coordinating a mass behavioral reversal without an admission that doctrine was ever wrong or coerced.
% TRANSFER_FUNCTION: Moves the cost of the reversal onto existing plural families and their children (who absorb the legal, social, and inheritance fallout) and onto members who trusted the public statement at face value, while moving institutional survival, restored property, and eventual political rehabilitation to the leadership and the corporate church.
% ABSENT_VOICES: Plural wives and children had no seat in the negotiation between church leadership and federal authorities; their consent was neither sought for the original practice's cessation nor for the selective secret continuations. Federal authorities are present as the coercive force but excluded from the interpretive frame that later theologizes the outcome.
% DISAPPEARANCE_RATIONALE: If the Manifesto's revelation framing were withdrawn and the event narrated plainly as coerced capitulation, the church's institutional continuity claim (that its leadership speaks with unbroken prophetic authority across the transition) would be structurally undermined, and the legal/social status of plural families before and after 1890 would require renegotiation without the cover story bridging the two periods.
% FOUNDING_PROBLEM: The federal government's escalating coercive campaign (property seizure, disincorporation, disenfranchisement, mass imprisonment) made continued institutional existence as an organized church incompatible with continued public practice of plural marriage.
% FOUNDING_PROBLEM_CORROBORATION: The immediate coercive crisis (disincorporation, seizure, imprisonment) that necessitated the reversal ended with statehood in 1896 and is attested by federal legal records and independent historians outside the church hierarchy. Church-internal sources continue to attest the Manifesto as a live, ongoing revelation rather than a resolved historical exigency; the divergence between the archival record of post-1890 secret continuations (documented by non-church historians and the 1904 Second Manifesto's own text, which concedes the first failed to stop the practice) and the institution's public narrative is the central corroborating gap this reading is built on.
narrative_ontology:disappearance_verdict(plural_marriage_mandate__institutional_pragmatism_reading, world_rearranges).
narrative_ontology:founding_problem_status(plural_marriage_mandate__institutional_pragmatism_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(plural_marriage_mandate__institutional_pragmatism_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(plural_marriage_mandate__institutional_pragmatism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(plural_marriage_mandate__institutional_pragmatism_reading, 0.68, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(plural_marriage_mandate__institutional_pragmatism_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(plural_marriage_mandate__institutional_pragmatism_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(plural_marriage_mandate__institutional_pragmatism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises sharply across 1886-1890 (0.48 to 0.60) tracking the coercive escalation and the moment the institution's public story diverges most from lived practice, and continues rising through 1896-1904 (0.66 to 0.68) as the gap between public claim and private continuation becomes the institution's own sustained cost to manage. Theater ratio jumps at 1890 itself (0.28 to 0.55) — the revelation framing is where performative doctrinal continuity work is heaviest, precisely because the underlying practice did not immediately stop. Suppression peaks at 1890 (0.78) reflecting active enforcement against members who continued practice or spoke of the gap, then moderates somewhat post-statehood (1896 onward) as external federal pressure eases but internal discipline against dissenting narrations persists.
 *
 * PERSPECTIVAL GAP:
 *   From the church hierarchy's seat, the Manifesto is continuous revelation and genuine institutional coordination securing survival for the whole membership. From the seat of plural wives, children, and coerced families, the same instrument is an imposed reversal that transfers cost downward with no negotiated exit. The engine should compute a tangled_rope for the story as a whole (real coordination function for institutional survival, real asymmetric extraction from powerless parties) while individual seats compute divergently — this divergence is the observable the reading is built to surface, not a defect to reconcile.
 *
 * DIRECTIONALITY LOGIC:
 *   Church hierarchy and the institutional apparatus sit near the full-beneficiary end: they set the agenda, control the narrative, and capture the durable benefit (survival, restored rights, statehood). Coerced polygamist families and plural wives/children sit near the full-target end: trapped exit options, no voice in the decision, and the entire cost of both the original practice's abrupt cessation and its selective secret continuation lands on them. Deceived monogamist converts sit closer to but not at the target end — their cost is reputational and epistemic (betrayed trust) rather than existential, but their exit is meaningfully constrained by the same institutional and social structures.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading treats the founding problem (existential federal coercion) as resolved by 1896 (founding_problem_status: dead) while the doctrinal legitimation apparatus built to manage that crisis persists indefinitely as though the crisis were ongoing revelation rather than resolved historical exigency. Reading the constraint as tangled_rope rather than as a pure mountain (immutable revealed truth) or a pure snare (nothing but coercion) prevents two mislabelings: it does not let the genuine coordination achievement (avoiding institutional destruction) launder the real costs imposed on powerless parties, and it does not let the real coercion erase the fact that leadership retained agency in how the capitulation was narrated and to whom its costs were distributed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    revelation_sincerity_vs_strategic_construction,
    'Did church leadership in 1890 sincerely believe the Manifesto reflected genuine revelation, or was the revelation framing consciously adopted as the most effective available legitimation strategy for a decision made primarily on pragmatic survival grounds?',
    'Close reading of contemporaneous private correspondence and diaries of the issuing leadership (as distinct from public statements), cross-referenced against the documented timeline and knowledge-holders of post-1890 continued sealings; sincere belief and strategic construction are not mutually exclusive and the record may support a mixed reading.',
    'If sincere belief dominates, this reading''s ''legitimation as instrument'' framing overstates conscious strategic intent and the story converges partially toward the endogenous_reinterpretation_reading; if strategic construction dominates, this reading''s extraction framing is strengthened.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(revelation_sincerity_vs_strategic_construction, conceptual, 'Whether leadership''s revelation claim was sincere belief, conscious strategy, or an inseparable mixture.').

omega_variable(
    mset_gap_evidentiary_status,
    'How complete and reliable is the documentary record of post-1890 plural marriages performed with hierarchical knowledge or authorization, given that such records were deliberately kept secret or later destroyed?',
    'Ongoing historical scholarship comparing church-internal sealing records (where accessible), federal prosecution case files from 1890-1904, and the text and context of the 1904 Second Manifesto, which itself concedes continued practice.',
    'A thinner-than-assumed evidentiary record would weaken this reading''s central M-set gap claim relative to the endogenous_reinterpretation_reading''s account of a clean 1890 cessation; a thicker record strengthens the pragmatism reading''s case that the 1890 statement was understood internally as a public-facing measure rather than a practice-ending one.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mset_gap_evidentiary_status, empirical, 'How well-evidenced the 1890-1904 continuation gap actually is.').

omega_variable(
    beneficiary_boundary_within_hierarchy,
    'Did all levels of church leadership benefit uniformly from the survival-legitimation arrangement, or did some leaders (e.g. those who continued authorizing plural marriages and were later disciplined or excommunicated in 1904-1911) bear costs the top hierarchy did not?',
    'Comparative case analysis of leaders excommunicated or disciplined after the Second Manifesto against those who retained standing, examining whether cost fell disproportionately on mid-level authorizers rather than the apex leadership that issued the public statement.',
    'If costs were unevenly distributed within the beneficiary group itself, the beneficiary set in base_properties should be narrowed to top hierarchy specifically, refining rather than overturning the tangled_rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_boundary_within_hierarchy, empirical, 'Whether the institutional beneficiary set is uniform or internally stratified.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(plural_marriage_mandate__institutional_pragmatism_reading, 1880, 1910).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(plur_tr_t1880, plural_marriage_mandate__institutional_pragmatism_reading, theater_ratio, 1880, 0.15).
narrative_ontology:measurement(plur_tr_t1886, plural_marriage_mandate__institutional_pragmatism_reading, theater_ratio, 1886, 0.28).
narrative_ontology:measurement(plur_tr_t1890, plural_marriage_mandate__institutional_pragmatism_reading, theater_ratio, 1890, 0.55).
narrative_ontology:measurement(plur_tr_t1896, plural_marriage_mandate__institutional_pragmatism_reading, theater_ratio, 1896, 0.6).
narrative_ontology:measurement(plur_tr_t1904, plural_marriage_mandate__institutional_pragmatism_reading, theater_ratio, 1904, 0.62).
narrative_ontology:measurement(plur_tr_t1910, plural_marriage_mandate__institutional_pragmatism_reading, theater_ratio, 1910, 0.58).

% Extraction over time
narrative_ontology:measurement(plur_be_t1880, plural_marriage_mandate__institutional_pragmatism_reading, base_extractiveness, 1880, 0.35).
narrative_ontology:measurement(plur_be_t1886, plural_marriage_mandate__institutional_pragmatism_reading, base_extractiveness, 1886, 0.48).
narrative_ontology:measurement(plur_be_t1890, plural_marriage_mandate__institutional_pragmatism_reading, base_extractiveness, 1890, 0.6).
narrative_ontology:measurement(plur_be_t1896, plural_marriage_mandate__institutional_pragmatism_reading, base_extractiveness, 1896, 0.66).
narrative_ontology:measurement(plur_be_t1904, plural_marriage_mandate__institutional_pragmatism_reading, base_extractiveness, 1904, 0.68).
narrative_ontology:measurement(plur_be_t1910, plural_marriage_mandate__institutional_pragmatism_reading, base_extractiveness, 1910, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(plur_su_t1880, plural_marriage_mandate__institutional_pragmatism_reading, suppression_requirement, 1880, 0.4).
narrative_ontology:measurement(plur_su_t1886, plural_marriage_mandate__institutional_pragmatism_reading, suppression_requirement, 1886, 0.65).
narrative_ontology:measurement(plur_su_t1890, plural_marriage_mandate__institutional_pragmatism_reading, suppression_requirement, 1890, 0.78).
narrative_ontology:measurement(plur_su_t1896, plural_marriage_mandate__institutional_pragmatism_reading, suppression_requirement, 1896, 0.7).
narrative_ontology:measurement(plur_su_t1904, plural_marriage_mandate__institutional_pragmatism_reading, suppression_requirement, 1904, 0.75).
narrative_ontology:measurement(plur_su_t1910, plural_marriage_mandate__institutional_pragmatism_reading, suppression_requirement, 1910, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(plural_marriage_mandate__institutional_pragmatism_reading, identity_coordination).
narrative_ontology:affects_constraint(plural_marriage_mandate__institutional_pragmatism_reading, plural_marriage_mandate__exogenous_override_reading).
narrative_ontology:affects_constraint(plural_marriage_mandate__institutional_pragmatism_reading, plural_marriage_mandate__endogenous_reinterpretation_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the plural_marriage_mandate kernel, decomposed per the ε-invariance principle because the natural-language label '1890 Manifesto' conflates structurally distinct claims about coercion, revelation, and strategy. The exogenous_override_reading treats the event as pure coercion overriding a still-binding divine command (near-snare, near-zero legitimate coordination function). The endogenous_reinterpretation_reading treats it as authentic prophetic authority exercising legitimate temporal discretion (closer to rope/mountain, minimal extraction). This institutional_pragmatism_reading occupies the tangled_rope middle: it grants a real coordination function (institutional survival under existential threat) while insisting on real, asymmetric extraction from powerless parties, and treats the doctrinal narrative as instrumentally serving that survival rather than as either pure external imposition or pure authentic revelation. Each reading carries its own stable ε; none is a hedge or average of the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
