% ============================================================================
% CONSTRAINT STORY: digital_money_emergence_boundary__conceptualization_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_digital_money_emergence_boundary__conceptualization_reading, []).

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
 *   constraint_id: digital_money_emergence_boundary__conceptualization_reading
 *   human_readable: Digital Money Emergence Boundary â Conceptualization Reading
 *   domain: monetary economics / financial history / technology governance
 *
 * SUMMARY:
 *   This constraint story instantiates the conceptualization_reading of the
 *   contested kernel digital_money_emergence_boundary. The kernel asks: when
 *   did digital money emerge? This reading places the boundary at the point
 *   of theoretical thinkability â 1960s telecommunications research and
 *   especially David Chaum's 1985 formalization of cryptographic protocols
 *   for anonymous electronic cash. Under this reading, money exists as a
 *   conceptual object before it circulates; prototypes and 'potential money'
 *   count as emergence. The reading is enforced through academic citation
 *   networks, peer review, and curriculum design. It is contested by the
 *   infrastructure_reading (ATMs, ACH, SWIFT as the material emergence) and
 *   the consumer_holdings_reading (e-purses and direct consumer
 *   transactability from the 1990s). The academic research community is the
 *   concentrated beneficiary, gaining disciplinary priority and funding
 *   legitimacy from an early, theory-centric origin myth.
 *
 * KEY AGENTS:
 *   - academic_research_community: Agenda-setter and beneficiary (institutional/constrained) â defines and enforces the conceptualization boundary through peer review and citation.
 *   - infrastructure_providers: Payer (powerful/constrained) â built material electronic transfer systems that are narratively deprioritized under the conceptualization threshold.
 *   - consumer_adoption_advocates: Payer (moderate/constrained) â advanced consumer-facing digital instruments that are excluded from origination status.
 *   - materialist_historians: Observer (moderate/analytical) â document the material turn but face gatekeeping in mainstream journals.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(digital_money_emergence_boundary__conceptualization_reading, 0.52).
domain_priors:suppression_score(digital_money_emergence_boundary__conceptualization_reading, 0.6).
domain_priors:theater_ratio(digital_money_emergence_boundary__conceptualization_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(digital_money_emergence_boundary__conceptualization_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(digital_money_emergence_boundary__conceptualization_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(digital_money_emergence_boundary__conceptualization_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(digital_money_emergence_boundary__conceptualization_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(digital_money_emergence_boundary__conceptualization_reading, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(digital_money_emergence_boundary__conceptualization_reading, tangled_rope).
narrative_ontology:human_readable(digital_money_emergence_boundary__conceptualization_reading, "Digital Money Emergence Boundary â Conceptualization Reading").
narrative_ontology:topic_domain(digital_money_emergence_boundary__conceptualization_reading, "monetary economics / financial history / technology governance").

domain_priors:requires_active_enforcement(digital_money_emergence_boundary__conceptualization_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(digital_money_emergence_boundary__conceptualization_reading, '80365545-3f44-4892-a701-35e2b58d7587').
narrative_ontology:cs_kernel_codification('80365545-3f44-4892-a701-35e2b58d7587', distributed).
narrative_ontology:cs_authority_grounding('80365545-3f44-4892-a701-35e2b58d7587', expertise).
narrative_ontology:cs_interpretation_layer_present('80365545-3f44-4892-a701-35e2b58d7587').
narrative_ontology:cs_reading_relation('80365545-3f44-4892-a701-35e2b58d7587', digital_money_emergence_boundary__infrastructure_reading, coexists_with).
narrative_ontology:cs_reading_relation('80365545-3f44-4892-a701-35e2b58d7587', digital_money_emergence_boundary__consumer_holdings_reading, coexists_with).
narrative_ontology:cs_axiom('80365545-3f44-4892-a701-35e2b58d7587', foundational, cognitive_threshold_for_monetary_origin).
narrative_ontology:cs_axiom_status(cognitive_threshold_for_monetary_origin, holdable).
narrative_ontology:cs_axiom_grounding('80365545-3f44-4892-a701-35e2b58d7587', cognitive_threshold_for_monetary_origin, conventional).
narrative_ontology:cs_axiom('80365545-3f44-4892-a701-35e2b58d7587', foundational, cryptographic_formalization_as_genesis_event).
narrative_ontology:cs_axiom_status(cryptographic_formalization_as_genesis_event, holdable).
narrative_ontology:cs_axiom_grounding('80365545-3f44-4892-a701-35e2b58d7587', cryptographic_formalization_as_genesis_event, conventional).
narrative_ontology:cs_reference_frame('80365545-3f44-4892-a701-35e2b58d7587', conceptual_sovereignty_frame).
narrative_ontology:cs_drift_state('80365545-3f44-4892-a701-35e2b58d7587', contemporary_crypto_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('80365545-3f44-4892-a701-35e2b58d7587', '').
narrative_ontology:cs_kernel_id(digital_money_emergence_boundary__conceptualization_reading, digital_money_emergence_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(digital_money_emergence_boundary__conceptualization_reading, academic_research_community).
narrative_ontology:constraint_victim(digital_money_emergence_boundary__conceptualization_reading, infrastructure_providers).
narrative_ontology:constraint_victim(digital_money_emergence_boundary__conceptualization_reading, consumer_adoption_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Defines the emergence boundary of digital money through peer-reviewed historiography, curriculum design, and funding priorities. Establishes theoretical thinkability (1960s telecommunications research, 1985 Chaum formalization) as the threshold, securing disciplinary priority and research funding for foundational conceptual work. Enforces the boundary by peer-reviewing out competing origin narratives that center infrastructure or consumer adoption.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__conceptualization_reading, academic_research_community, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(digital_money_emergence_boundary__conceptualization_reading, academic_research_community, beneficiary).

% Built the electronic transfer infrastructure (ATMs, ACH, SWIFT) that materially moved value before and alongside theoretical digital money constructs. Under the conceptualization reading, their contributions are categorized as pre-digital or non-monetary plumbing, depriving them of origin-status in monetary history and redirecting historical legitimacy toward academic theorists.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__conceptualization_reading, infrastructure_providers, payer,
    powerful, biographical, constrained, global).

% Developed and promoted consumer-facing digital instruments (e-purses, electronic money directives) from the 1990s onward. Under the conceptualization reading, their lived-practice and adoption-centric boundary is excluded from the emergence threshold; their historical role is treated as downstream implementation rather than origination, suppressing their claims to foundational status.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__conceptualization_reading, consumer_adoption_advocates, payer,
    moderate, biographical, constrained, global).

% Document financial history through material infrastructure and consumer practice. They observe that the conceptualization reading systematically displaces the material turn in monetary historiography, but their counter-narratives face peer-review friction in mainstream journals that enforce the theory-centric origin story.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__conceptualization_reading, materialist_historians, observer,
    moderate, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(digital_money_emergence_boundary__conceptualization_reading, academic_research_community).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a shared disciplinary narrative about the origin of digital money, enabling cumulative research programs, citation networks, graduate curricula, and intergenerational knowledge transfer within computer science, cryptography, and monetary economics.
% TRANSFER_FUNCTION: Moves historical priority, disciplinary legitimacy, and research funding from infrastructure builders and consumer-adoption advocates to the academic research community that produced the theoretical formalization.
% ABSENT_VOICES: Consumer holders of 1990s e-purses, retail bankers who operated ATMs and ACH networks, and policymakers focused on financial inclusion are absent from the definitional conversation; they would argue that money is defined by circulation and holding, not by theoretical possibility.
% DISAPPEARANCE_RATIONALE: If the conceptualization boundary vanished overnight, digital money historiography would reorganize around material infrastructure or consumer holdings; curricula would retell the story from ATMs or e-purses rather than from 1960s telecom theory and Chaum; research funding would shift toward implementation and adoption studies.
% FOUNDING_PROBLEM: The need for a coherent origin narrative to anchor an interdisciplinary research field spanning cryptography, computer science, and monetary economics, and to distinguish 'digital money' as a specific object from general electronic banking.
% FOUNDING_PROBLEM_CORROBORATION: The academic research community attests the problem is still live, citing the need for theoretical grounding. Infrastructure providers and consumer adoption advocates attest the problem is solved and the arrangement persists as disciplinary rent-seeking; monetary historians outside the benefiting parties note that origin myths are common in new fields but become contested as the field matures.
narrative_ontology:disappearance_verdict(digital_money_emergence_boundary__conceptualization_reading, world_rearranges).
narrative_ontology:founding_problem_status(digital_money_emergence_boundary__conceptualization_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(digital_money_emergence_boundary__conceptualization_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(digital_money_emergence_boundary__conceptualization_reading, 'none', 1).
narrative_ontology:epsilon_provenance(digital_money_emergence_boundary__conceptualization_reading, 0.52, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(digital_money_emergence_boundary__conceptualization_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(digital_money_emergence_boundary__conceptualization_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(digital_money_emergence_boundary__conceptualization_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52) is moderate: the extraction is ideational rather than financial, capturing historical priority, citation rents, and research funding. Suppression (0.60) is moderate-to-high because peer review and tenure committees actively enforce the conceptualization frame, marginalizing infrastructure and consumer-adoption historiography. Theater ratio (0.48) is significant: ritual citation of Chaum and 1960s telecom research performs disciplinary belonging even when the cited work is not directly relevant to the empirical object under study. Accessibility collapse (0.65) is relatively high because once the conceptualization frame is adopted, material infrastructure appears as mere 'plumbing' or prehistory, collapsing the visibility of alternatives. Resistance (0.48) reflects ongoing contestation from engineers, materialist historians, and the cryptocurrency community, which champions Satoshi over Chaum.
 *
 * PERSPECTIVAL GAP:
 *   The academic research community experiences the constraint as genuine coordination: a shared origin narrative enables cumulative research, conference ecosystems, and graduate training. Infrastructure providers and consumer advocates experience the same structure as extraction: their contributions are rendered invisible or secondary, and their attempts to reframe the origin are rejected by peer-review processes. The engine computes this divergence from the structural data â identical historical events produce opposite seat classifications depending on directional position.
 *
 * DIRECTIONALITY LOGIC:
 *   The academic_research_community is declared as beneficiary and agenda-setter with constrained exit (low d, near beneficiary pole). Infrastructure_providers and consumer_adoption_advocates are declared as victims with constrained exit (high d, near target pole). The constrained exit on both sides reflects that all parties are embedded in academic discursive structures â beneficiaries are identity-locked to the disciplinary narrative, while payers are locked out by peer-review gatekeeping. Effective extraction is thus damped for the research community and amplified for the materialist seats.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â providing a coherent origin narrative for an interdisciplinary field â was genuinely live in the 1980s and 1990s. However, as digital money practice proliferated (Bitcoin, mobile payments, CBDCs), the conceptualization boundary persisted beyond its coordinative need. It now functions partly as disciplinary rent-seeking, extracting priority for theorists over implementers. The R5 genealogy (founding_problem_status: contested) signals that the arrangement may have outlived its original coordination function, though it has not yet atrophied into pure performance (piton) because the coordination function in graduate training and citation networks remains partially real.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cognitive_vs_material_origin,
    'Is the conceptualization boundary a convention enacted by the research community, or does it reflect an objective historical fact about monetary emergence?',
    'Comparative historiography examining whether theoretical formalization causally preceded material implementation, or merely preceded it in disciplinary memory.',
    'If merely conventional, the extraction is higher â the boundary is a disciplinary allocation mechanism. If objective, the coordination function is stronger and extraction lower.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cognitive_vs_material_origin, conceptual, 'Nature of the conceptualization boundary as convention or fact.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the marginalization of infrastructure and consumer-adoption narratives structural (peer review, tenure incentives) or internalized (researchers genuinely perceive theory as origination)?',
    'Citation network analysis and ethnography of peer review panels; post-exit trajectory of scholars who switch to materialist historiography.',
    'If internalized, effective suppression exceeds structural measures and the constraint is more deeply embedded. If structural, reform of review incentives could shift the boundary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression of competing origin narratives.').

omega_variable(
    reading_family_foreclosure,
    'Does the conceptualization reading logically foreclose the infrastructure and consumer-holdings readings, or do they merely coexist within different disciplinary communities?',
    'Analysis of whether a single coherent historiographic framework can integrate all three emergence thresholds; if not, foreclosure is structural.',
    'If foreclosure holds, the kernel is zero-sum and extraction is inherent. If coexistence holds, the constraint''s suppression is lower than measured.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_family_foreclosure, conceptual, 'Logical relationship between sibling readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(digital_money_emergence_boundary__conceptualization_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(digi_tr_t0, digital_money_emergence_boundary__conceptualization_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(digi_tr_t10, digital_money_emergence_boundary__conceptualization_reading, theater_ratio, 10, 0.08).
narrative_ontology:measurement(digi_tr_t20, digital_money_emergence_boundary__conceptualization_reading, theater_ratio, 20, 0.18).
narrative_ontology:measurement(digi_tr_t30, digital_money_emergence_boundary__conceptualization_reading, theater_ratio, 30, 0.28).
narrative_ontology:measurement(digi_tr_t40, digital_money_emergence_boundary__conceptualization_reading, theater_ratio, 40, 0.36).
narrative_ontology:measurement(digi_tr_t50, digital_money_emergence_boundary__conceptualization_reading, theater_ratio, 50, 0.42).
narrative_ontology:measurement(digi_tr_t60, digital_money_emergence_boundary__conceptualization_reading, theater_ratio, 60, 0.48).

% Extraction over time
narrative_ontology:measurement(digi_be_t0, digital_money_emergence_boundary__conceptualization_reading, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(digi_be_t10, digital_money_emergence_boundary__conceptualization_reading, base_extractiveness, 10, 0.12).
narrative_ontology:measurement(digi_be_t20, digital_money_emergence_boundary__conceptualization_reading, base_extractiveness, 20, 0.22).
narrative_ontology:measurement(digi_be_t30, digital_money_emergence_boundary__conceptualization_reading, base_extractiveness, 30, 0.32).
narrative_ontology:measurement(digi_be_t40, digital_money_emergence_boundary__conceptualization_reading, base_extractiveness, 40, 0.4).
narrative_ontology:measurement(digi_be_t50, digital_money_emergence_boundary__conceptualization_reading, base_extractiveness, 50, 0.46).
narrative_ontology:measurement(digi_be_t60, digital_money_emergence_boundary__conceptualization_reading, base_extractiveness, 60, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(digi_su_t0, digital_money_emergence_boundary__conceptualization_reading, suppression_requirement, 0, 0.08).
narrative_ontology:measurement(digi_su_t10, digital_money_emergence_boundary__conceptualization_reading, suppression_requirement, 10, 0.12).
narrative_ontology:measurement(digi_su_t20, digital_money_emergence_boundary__conceptualization_reading, suppression_requirement, 20, 0.25).
narrative_ontology:measurement(digi_su_t30, digital_money_emergence_boundary__conceptualization_reading, suppression_requirement, 30, 0.38).
narrative_ontology:measurement(digi_su_t40, digital_money_emergence_boundary__conceptualization_reading, suppression_requirement, 40, 0.48).
narrative_ontology:measurement(digi_su_t50, digital_money_emergence_boundary__conceptualization_reading, suppression_requirement, 50, 0.55).
narrative_ontology:measurement(digi_su_t60, digital_money_emergence_boundary__conceptualization_reading, suppression_requirement, 60, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(digital_money_emergence_boundary__conceptualization_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(digital_money_emergence_boundary__conceptualization_reading, 0.08).
narrative_ontology:affects_constraint(digital_money_emergence_boundary__conceptualization_reading, digital_money_emergence_boundary__infrastructure_reading).
narrative_ontology:affects_constraint(digital_money_emergence_boundary__conceptualization_reading, digital_money_emergence_boundary__consumer_holdings_reading).

% DUAL FORMULATION NOTE:
% The natural-language label 'digital money emergence' conflates three structurally distinct claims about the threshold of monetary validity: theoretical thinkability (this reading), material infrastructure enablement (infrastructure_reading), and direct consumer transactability (consumer_holdings_reading). Each has a different beneficiary structure, Îµ profile, and enforcement mechanism. They are modeled as a constraint family linked by affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
