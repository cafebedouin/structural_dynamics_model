% ============================================================================
% CONSTRAINT STORY: divine_marriage_command__substitutionist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_divine_marriage_command__substitutionist_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: divine_marriage_command__substitutionist_reading
 *   human_readable: Divine Monogamy Command (Substitutionist Reading)
 *   domain: religious_authority/commitment_systems/political_theology
 *
 * SUMMARY:
 *   This constraint represents the 'substitutionist' reading of a divine
 *   marriage command, where a new revelation (the Manifesto) superseded the
 *   prior command for polygamy, making monogamy the new doctrinal
 *   requirement. This reading was crucial for the institutional leadership to
 *   maintain legitimacy while conforming to federal law. The constraint is
 *   claimed as a Tangled Rope because it performs a coordination function
 *   (aligning with external law) but also extracts heavily from those who
 *   adhere to the prior command, requiring active enforcement to suppress
 *   dissent and excommunicate fundamentalists. The metrics reflect the
 *   coercive and extractive nature of this doctrinal shift.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(divine_marriage_command__substitutionist_reading, 0.65).
domain_priors:suppression_score(divine_marriage_command__substitutionist_reading, 0.78).
domain_priors:theater_ratio(divine_marriage_command__substitutionist_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(divine_marriage_command__substitutionist_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(divine_marriage_command__substitutionist_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(divine_marriage_command__substitutionist_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(divine_marriage_command__substitutionist_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(divine_marriage_command__substitutionist_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(divine_marriage_command__substitutionist_reading, tangled_rope).
narrative_ontology:human_readable(divine_marriage_command__substitutionist_reading, "Divine Monogamy Command (Substitutionist Reading)").
narrative_ontology:topic_domain(divine_marriage_command__substitutionist_reading, "religious_authority/commitment_systems/political_theology").

domain_priors:requires_active_enforcement(divine_marriage_command__substitutionist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(divine_marriage_command__substitutionist_reading, 'bf9cebc9-112e-49f8-af04-6a68b75cdbce').
narrative_ontology:cs_kernel_codification('bf9cebc9-112e-49f8-af04-6a68b75cdbce', formalized).
narrative_ontology:cs_authority_grounding('bf9cebc9-112e-49f8-af04-6a68b75cdbce', lineage).
narrative_ontology:cs_interpretation_layer_present('bf9cebc9-112e-49f8-af04-6a68b75cdbce').
narrative_ontology:cs_reading_relation('bf9cebc9-112e-49f8-af04-6a68b75cdbce', divine_marriage_command__continuationist_reading, forecloses).
narrative_ontology:cs_reading_relation('bf9cebc9-112e-49f8-af04-6a68b75cdbce', divine_marriage_command__coercion_visibility_reading, influences).
narrative_ontology:cs_axiom('bf9cebc9-112e-49f8-af04-6a68b75cdbce', foundational, new_revelation_supersedes_prior_command).
narrative_ontology:cs_axiom_status(new_revelation_supersedes_prior_command, holdable).
narrative_ontology:cs_axiom_grounding('bf9cebc9-112e-49f8-af04-6a68b75cdbce', new_revelation_supersedes_prior_command, theological).
narrative_ontology:cs_axiom('bf9cebc9-112e-49f8-af04-6a68b75cdbce', foundational, monogamy_is_divine_law).
narrative_ontology:cs_axiom_status(monogamy_is_divine_law, holdable).
narrative_ontology:cs_axiom_grounding('bf9cebc9-112e-49f8-af04-6a68b75cdbce', monogamy_is_divine_law, theological).
narrative_ontology:cs_reference_frame('bf9cebc9-112e-49f8-af04-6a68b75cdbce', post_manifesto_monogamous_order).
narrative_ontology:cs_drift_state('bf9cebc9-112e-49f8-af04-6a68b75cdbce', contemporary_fundamentalist_schisms, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('bf9cebc9-112e-49f8-af04-6a68b75cdbce', '').
narrative_ontology:cs_kernel_id(divine_marriage_command__substitutionist_reading, divine_marriage_command).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(divine_marriage_command__substitutionist_reading, institutional_leadership).
narrative_ontology:constraint_beneficiary(divine_marriage_command__substitutionist_reading, monogamous_members).
narrative_ontology:constraint_victim(divine_marriage_command__substitutionist_reading, polygamous_fundamentalists).
narrative_ontology:constraint_victim(divine_marriage_command__substitutionist_reading, displaced_plural_wives).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The highest ecclesiastical authority, responsible for interpreting and declaring divine will. They promulgated the Manifesto as a new revelation, shifting the doctrinal basis of marriage. They benefit from maintaining institutional legitimacy and avoiding federal persecution, but are identity-locked into the theological framework.
narrative_ontology:constraint_stakeholder(divine_marriage_command__substitutionist_reading, institutional_leadership, agenda_setter,
    institutional, generational, identity_locked, global).

% Members who either never practiced polygamy or readily adopted monogamy. They benefit from social acceptance, legal conformity, and the stability of the institution. Their identity is tied to the church, making exit costly, but they face less direct coercion than fundamentalists.
narrative_ontology:constraint_stakeholder(divine_marriage_command__substitutionist_reading, monogamous_members, beneficiary,
    organized, biographical, constrained, global).

% Members who believe polygamy is a divinely commanded practice and reject the Manifesto as a doctrinal change. They face excommunication, social ostracization, and loss of community. Their identity is deeply fused with the prior command, making exit unthinkable, yet they are actively suppressed.
narrative_ontology:constraint_stakeholder(divine_marriage_command__substitutionist_reading, polygamous_fundamentalists, payer,
    powerless, generational, trapped, local).

% Women who were in polygamous marriages prior to the Manifesto and were subsequently disavowed or left without support. They lost social status, economic security, and familial networks. Their options are severely limited by social and economic dependency, making them trapped.
narrative_ontology:constraint_stakeholder(divine_marriage_command__substitutionist_reading, displaced_plural_wives, payer,
    powerless, biographical, trapped, local).

% The external authority that imposed legal and political pressure against polygamy, leading to the Manifesto. It observes the church's compliance and maintains legal sanctions against polygamous practices. Its role is primarily external enforcement and monitoring.
narrative_ontology:constraint_stakeholder(divine_marriage_command__substitutionist_reading, federal_government, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the marriage practices of a large religious community with the legal and social norms of the surrounding nation-state, ensuring institutional survival and social integration.
% TRANSFER_FUNCTION: Transfers the right to practice polygamy from individual members to the institutional leadership's interpretive authority, in exchange for legal and social legitimacy. It also transfers social and economic costs onto those who resist the new doctrine.
% ABSENT_VOICES: The voices of those who believe in the divine mandate of polygamy, particularly those who were excommunicated or marginalized, are absent from the official narrative of doctrinal change. Their theological arguments for continuationism are suppressed within the institutional discourse.
% DISAPPEARANCE_RATIONALE: If this specific reading of the divine marriage command vanished, the institutional legitimacy of the church would be severely challenged. The historical narrative of revelation would unravel, leading to a crisis of faith for many members and potentially a resurgence of fundamentalist claims. The social and legal standing of the church would be destabilized.
% FOUNDING_PROBLEM: The church faced existential threats from the federal government due to its practice of polygamy, including confiscation of property, disenfranchisement, and imprisonment of leaders. The problem was institutional survival in the face of overwhelming external coercion.
% FOUNDING_PROBLEM_CORROBORATION: The institutional leadership maintains that the problem of aligning divine command with societal norms is an ongoing, live issue. However, historians and external observers, including the federal government, largely agree that the immediate existential threat from polygamy was resolved by the Manifesto's implementation. The problem of legal persecution is dead, but the constraint persists due to internal theological reinterpretation.
narrative_ontology:disappearance_verdict(divine_marriage_command__substitutionist_reading, world_rearranges).
narrative_ontology:founding_problem_status(divine_marriage_command__substitutionist_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(divine_marriage_command__substitutionist_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(divine_marriage_command__substitutionist_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(divine_marriage_command__substitutionist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(divine_marriage_command__substitutionist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(divine_marriage_command__substitutionist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.65) is high because the shift in doctrine imposed significant costs on those who had built their lives around polygamy, including social dislocation and loss of religious standing. Suppression (0.78) is very high, as the institutional leadership actively enforced the new doctrine through excommunication and social pressure, effectively trapping fundamentalists. Theater ratio (0.40) is moderate, reflecting the performative aspect of framing a coerced policy change as a new divine revelation, while still maintaining some genuine theological function. The temporal measurements show a rise in extractiveness and suppression as the new doctrine was consolidated and enforced, and a corresponding increase in theatricality as the narrative of 'revelation' was solidified.
 *
 * PERSPECTIVAL GAP:
 *   The institutional leadership experiences this as a necessary, divinely guided coordination to preserve the church. For polygamous fundamentalists and displaced wives, it is a coercive extraction that dismantled their families and communities, enforced by the very authority they once trusted. The engine's per-seat classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Institutional leadership and monogamous members are beneficiaries, as they gain social acceptance and institutional stability. Polygamous fundamentalists and displaced plural wives are clear victims, bearing the direct costs of excommunication, social ostracization, and loss of family structure. The federal government acts as an external observer and enforcer, creating the initial pressure for the doctrinal shift.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (institutional survival from federal persecution) is 'dead' in its original form, yet the constraint persists and even intensifies its extractiveness and suppression. This indicates a mandatrophy where the mechanism (doctrinal shift to monogamy) outlived its original mandate and became a tool for internal control and consolidation of power, rather than merely a response to external threat. The 'revelation' framing serves to mask this shift, preventing reclassification as a pure Snare by maintaining a coordination narrative.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    revelation_vs_coercion,
    'To what extent was the Manifesto a genuine new revelation, and to what extent was it a pragmatic response to federal coercion, framed as revelation for internal legitimacy?',
    'Analysis of internal church documents and leadership communications from the period, comparing public statements with private deliberations, and examining the timing of ''revelations'' relative to federal legal pressures.',
    'If primarily coercion, the ''divine command'' aspect of the constraint''s legitimacy is theatrical, increasing its effective extractiveness and shifting its classification closer to a Snare. If primarily revelation, the coordination function is more robust.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(revelation_vs_coercion, conceptual, 'Ambiguity between divine revelation and pragmatic coercion as the source of the monogamy command.').

omega_variable(
    internalized_suppression_of_fundamentalists,
    'How much of the suppression experienced by polygamous fundamentalists is structural (excommunication, social ostracization) versus internalized (self-censorship, identity fusion with the institution despite disagreement)?',
    'Post-exit suppression trajectory: if fundamentalists who leave the church continue to self-censor or struggle with identity, it suggests a significant internalized component. Ethnographic studies of excommunicated groups.',
    'If internalized suppression is high, the constraint''s effective suppression is higher than the structural measures suggest, as the targets carry the suppression with them even after formal exit, making their ''trapped'' status more profound.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalized_suppression_of_fundamentalists, empirical, 'Structural vs. internalized suppression mechanism for fundamentalists.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(divine_marriage_command__substitutionist_reading, 1890, 1950).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(divi_tr_t1890, divine_marriage_command__substitutionist_reading, theater_ratio, 1890, 0.1).
narrative_ontology:measurement(divi_tr_t1900, divine_marriage_command__substitutionist_reading, theater_ratio, 1900, 0.2).
narrative_ontology:measurement(divi_tr_t1910, divine_marriage_command__substitutionist_reading, theater_ratio, 1910, 0.3).
narrative_ontology:measurement(divi_tr_t1920, divine_marriage_command__substitutionist_reading, theater_ratio, 1920, 0.35).
narrative_ontology:measurement(divi_tr_t1930, divine_marriage_command__substitutionist_reading, theater_ratio, 1930, 0.4).
narrative_ontology:measurement(divi_tr_t1940, divine_marriage_command__substitutionist_reading, theater_ratio, 1940, 0.4).
narrative_ontology:measurement(divi_tr_t1950, divine_marriage_command__substitutionist_reading, theater_ratio, 1950, 0.4).

% Extraction over time
narrative_ontology:measurement(divi_be_t1890, divine_marriage_command__substitutionist_reading, base_extractiveness, 1890, 0.4).
narrative_ontology:measurement(divi_be_t1900, divine_marriage_command__substitutionist_reading, base_extractiveness, 1900, 0.5).
narrative_ontology:measurement(divi_be_t1910, divine_marriage_command__substitutionist_reading, base_extractiveness, 1910, 0.58).
narrative_ontology:measurement(divi_be_t1920, divine_marriage_command__substitutionist_reading, base_extractiveness, 1920, 0.62).
narrative_ontology:measurement(divi_be_t1930, divine_marriage_command__substitutionist_reading, base_extractiveness, 1930, 0.65).
narrative_ontology:measurement(divi_be_t1940, divine_marriage_command__substitutionist_reading, base_extractiveness, 1940, 0.65).
narrative_ontology:measurement(divi_be_t1950, divine_marriage_command__substitutionist_reading, base_extractiveness, 1950, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(divi_su_t1890, divine_marriage_command__substitutionist_reading, suppression_requirement, 1890, 0.6).
narrative_ontology:measurement(divi_su_t1900, divine_marriage_command__substitutionist_reading, suppression_requirement, 1900, 0.7).
narrative_ontology:measurement(divi_su_t1910, divine_marriage_command__substitutionist_reading, suppression_requirement, 1910, 0.75).
narrative_ontology:measurement(divi_su_t1920, divine_marriage_command__substitutionist_reading, suppression_requirement, 1920, 0.78).
narrative_ontology:measurement(divi_su_t1930, divine_marriage_command__substitutionist_reading, suppression_requirement, 1930, 0.78).
narrative_ontology:measurement(divi_su_t1940, divine_marriage_command__substitutionist_reading, suppression_requirement, 1940, 0.78).
narrative_ontology:measurement(divi_su_t1950, divine_marriage_command__substitutionist_reading, suppression_requirement, 1950, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(divine_marriage_command__substitutionist_reading, identity_coordination).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'divine_marriage_command' kernel. The 'coercion_visibility_reading' frames the Manifesto as a response to federal pressure, while the 'continuationist_reading' views polygamy as still doctrinally valid, with the Manifesto as a temporary suspension. This 'substitutionist_reading' asserts a complete doctrinal shift.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
