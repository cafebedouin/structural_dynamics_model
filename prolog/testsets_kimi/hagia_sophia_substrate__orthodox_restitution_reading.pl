% ============================================================================
% CONSTRAINT STORY: hagia_sophia_substrate__orthodox_restitution_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hagia_sophia_substrate__orthodox_restitution_reading, []).

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
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: hagia_sophia_substrate__orthodox_restitution_reading
 *   human_readable: Hagia Sophia Orthodox Restitution Claim
 *   domain: cultural_heritage/sovereignty/religious_authority
 *
 * SUMMARY:
 *   This constraint story instantiates the orthodox_restitution_reading of
 *   the hagia_sophia_substrate kernel. The claim asserts that Hagia Sophia's
 *   legitimacy derives from its sixth-century Christian founding and either
 *   demands return to Orthodox ecclesiastical control or permanent neutral
 *   status to honor Byzantine origins. The constraint operates as an
 *   atrophied ideological claim: it generates symbolic capital for the
 *   Eastern Orthodox diaspora and diplomatic leverage for the Greek state,
 *   while imposing symbolic costs on Turkish sovereignty and Islamic worship
 *   continuity. It has no realistic enforcement pathway and persists
 *   primarily through institutional performance rather than material
 *   coercion. The sibling readings are islamic_sovereignty_reading (which
 *   this reading forecloses) and universal_heritage_reading (with which it
 *   coexists in international discourse).
 *
 * KEY AGENTS:
 *   - Greek state: Primary agenda setter (institutional/national) â administers the diplomatic claim without material enforcement.
 *   - Eastern Orthodox diaspora: Primary symbolic beneficiary (organized/global) â identity-locked to the restitution narrative as a constitutive anchor.
 *   - Turkish sovereignty: Primary target (institutional/national) â bears diffuse symbolic and diplomatic costs of an external territorial claim.
 *   - Islamic worship continuity: Secondary target (organized/local) â bears identity costs from the delegitimization of ongoing waqf practice.
 *   - International heritage observers: Analytical seat (institutional/global) â tracks the contest without enforcing any reading.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hagia_sophia_substrate__orthodox_restitution_reading, 0.28).
domain_priors:suppression_score(hagia_sophia_substrate__orthodox_restitution_reading, 0.25).
domain_priors:theater_ratio(hagia_sophia_substrate__orthodox_restitution_reading, 0.72).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hagia_sophia_substrate__orthodox_restitution_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(hagia_sophia_substrate__orthodox_restitution_reading, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(hagia_sophia_substrate__orthodox_restitution_reading, theater_ratio, 0.72).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hagia_sophia_substrate__orthodox_restitution_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(hagia_sophia_substrate__orthodox_restitution_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hagia_sophia_substrate__orthodox_restitution_reading, piton).
narrative_ontology:human_readable(hagia_sophia_substrate__orthodox_restitution_reading, "Hagia Sophia Orthodox Restitution Claim").
narrative_ontology:topic_domain(hagia_sophia_substrate__orthodox_restitution_reading, "cultural_heritage/sovereignty/religious_authority").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hagia_sophia_substrate__orthodox_restitution_reading, 'e0ee1919-404b-4f51-aa0e-bf3adf29ff14').
narrative_ontology:cs_kernel_codification('e0ee1919-404b-4f51-aa0e-bf3adf29ff14', fixed_text).
narrative_ontology:cs_authority_grounding('e0ee1919-404b-4f51-aa0e-bf3adf29ff14', lineage).
narrative_ontology:cs_interpretation_layer_present('e0ee1919-404b-4f51-aa0e-bf3adf29ff14').
narrative_ontology:cs_reading_relation('e0ee1919-404b-4f51-aa0e-bf3adf29ff14', hagia_sophia_substrate__islamic_sovereignty_reading, forecloses).
narrative_ontology:cs_reading_relation('e0ee1919-404b-4f51-aa0e-bf3adf29ff14', hagia_sophia_substrate__universal_heritage_reading, coexists_with).
narrative_ontology:cs_axiom('e0ee1919-404b-4f51-aa0e-bf3adf29ff14', foundational, byzantine_founding_creates_perpetual_orthodox_title).
narrative_ontology:cs_axiom_status(byzantine_founding_creates_perpetual_orthodox_title, holdable).
narrative_ontology:cs_axiom_grounding('e0ee1919-404b-4f51-aa0e-bf3adf29ff14', byzantine_founding_creates_perpetual_orthodox_title, theological).
narrative_ontology:cs_axiom('e0ee1919-404b-4f51-aa0e-bf3adf29ff14', foundational, waqf_status_is_subordinate_to_pre_conquest_title).
narrative_ontology:cs_axiom_status(waqf_status_is_subordinate_to_pre_conquest_title, holdable).
narrative_ontology:cs_axiom_grounding('e0ee1919-404b-4f51-aa0e-bf3adf29ff14', waqf_status_is_subordinate_to_pre_conquest_title, deontological).
narrative_ontology:cs_reference_frame('e0ee1919-404b-4f51-aa0e-bf3adf29ff14', byzantine_ecclesiastical_supremacy).
narrative_ontology:cs_drift_state('e0ee1919-404b-4f51-aa0e-bf3adf29ff14', contemporary_turkish_sovereignty_era, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('e0ee1919-404b-4f51-aa0e-bf3adf29ff14', '').
narrative_ontology:cs_kernel_id(hagia_sophia_substrate__orthodox_restitution_reading, hagia_sophia_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hagia_sophia_substrate__orthodox_restitution_reading, eastern_orthodox_diaspora).
narrative_ontology:constraint_beneficiary(hagia_sophia_substrate__orthodox_restitution_reading, greek_state).
narrative_ontology:constraint_victim(hagia_sophia_substrate__orthodox_restitution_reading, turkish_sovereignty).
narrative_ontology:constraint_victim(hagia_sophia_substrate__orthodox_restitution_reading, islamic_worship_continuity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Periodically raises the Hagia Sophia restitution claim in bilateral and multilateral forums as a low-cost source of diplomatic leverage and domestic political signaling. Does not pursue material enforcement because no pathway exists, but maintains the claim institutionally through commemorative rhetoric and periodic diplomatic notes.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__orthodox_restitution_reading, greek_state, agenda_setter,
    institutional, generational, constrained, national).

% Derives collective religious and cultural identity from the symbolic continuity of Hagia Sophia as the historic center of Eastern Christianity. The restitution claim is constitutive of transgenerational identity; abandonment would represent a rupture in historical self-understanding. Benefits symbolically from the persistence of the narrative.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__orthodox_restitution_reading, eastern_orthodox_diaspora, beneficiary,
    organized, civilizational, identity_locked, global).

% Bears the symbolic and diplomatic cost of an external religious and national claim on sovereign territory. The restitution narrative constrains full international legitimacy regarding the site and generates recurring friction in heritage and diplomatic forums, despite uncontested de facto control.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__orthodox_restitution_reading, turkish_sovereignty, payer,
    institutional, civilizational, trapped, national).

% The ongoing Islamic worship and waqf tradition at the site is symbolically interrupted by the assertion that the building should return to Christian control or neutrality. This creates a defensive institutional posture and identity pressure on the worshipping community, which experiences the claim as a threat to religious continuity.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__orthodox_restitution_reading, islamic_worship_continuity, payer,
    organized, generational, identity_locked, local).

% Monitors competing legitimacy claims through UNESCO and academic heritage frameworks. Does not enforce restitution but documents and mediates between sovereign, religious, and universal heritage narratives, tracking how the Orthodox claim performs in international discourse relative to other readings.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__orthodox_restitution_reading, international_heritage_observers, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(hagia_sophia_substrate__orthodox_restitution_reading, diffuse).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a transnational Eastern Orthodox identity and diplomatic bloc around a shared symbolic anchor, providing a focal point for religious continuity and historical justice claims across dispersed communities.
% TRANSFER_FUNCTION: Moves symbolic legitimacy and diplomatic leverage from Turkish state sovereignty and Islamic worship continuity to the Eastern Orthodox diaspora and Greek state, without material resource transfer.
% ABSENT_VOICES: Turkish Cypriot Orthodox communities and secular Turkish heritage professionals who might advocate for shared governance or strict neutrality are excluded from the Orthodox restitution narrative; the Turkish state's own heritage bureaucracy is treated as illegitimate by the restitution frame rather than as a negotiating partner.
% DISAPPEARANCE_RATIONALE: For the Orthodox diaspora and Greek state, the claim's disappearance would mean relinquishing a foundational symbolic grievance and diplomatic tool, rearranging their identity politics. For Turkey, it would remove a persistent external challenge to territorial legitimacy. The parties dispute whether the world rearranges or stays the same.
% FOUNDING_PROBLEM: The 1453 Ottoman conquest and subsequent conversion of Hagia Sophia from a Christian cathedral to a mosque created a rupture in Orthodox ecclesiastical continuity and symbolic authority.
% FOUNDING_PROBLEM_CORROBORATION: Secular historians attest the conquest occurred, but corroboration that the ongoing restitution claim addresses a live rather than historical grievance comes only from within the Orthodox and Greek diplomatic tradition. The Turkish state and UNESCO both treat the issue as settled sovereignty, not an open problem; no independent non-beneficiary party attests the founding problem is currently live.
narrative_ontology:disappearance_verdict(hagia_sophia_substrate__orthodox_restitution_reading, contested).
narrative_ontology:founding_problem_status(hagia_sophia_substrate__orthodox_restitution_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hagia_sophia_substrate__orthodox_restitution_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(hagia_sophia_substrate__orthodox_restitution_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hagia_sophia_substrate__orthodox_restitution_reading, 0.28, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hagia_sophia_substrate__orthodox_restitution_reading_tests).
:- end_tests(hagia_sophia_substrate__orthodox_restitution_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is claimed as piton because its primary function (actual restitution of the site) has atrophied over centuries, yet the claim persists through institutional inertia and theatrical diplomatic maintenance. Extractiveness is low (0.28) because no material resources are transferred and no enforcement pathway exists; the cost is purely symbolic and diplomatic. Theater ratio is high (0.72) because the bulk of the constraint's activity is performative â commemorative rhetoric, diplomatic notes, and identity ritual â rather than functional pursuit of restitution. Suppression is low (0.25) because the claim lacks coercive machinery. Resistance is moderate (0.60) because Turkey consistently rejects the claim and asserts counter-sovereignty. The measurement series show a slow rise in extractiveness and theater over the interval, reflecting increasing Greek-Turkish tension and the conversion of the site back to a mosque in 2020, which intensified the performative cycle without creating enforcement.
 *
 * PERSPECTIVAL GAP:
 *   From the Greek state and Orthodox diaspora seats, the constraint appears as a legitimate historical justice claim honoring an uninterrupted (if displaced) ecclesiastical lineage. From the Turkish sovereignty and Islamic worship seats, the same structure appears as an illegitimate external claim on sovereign territory and an attack on religious continuity. The engine computes this divergence from the structural data: the former sit near the beneficiary end of directionality, the latter near the target end.
 *
 * DIRECTIONALITY LOGIC:
 *   The Greek state and Eastern Orthodox diaspora are declared beneficiaries, deriving low directionality (subsidy/identity confirmation). The Turkish state and Islamic worship continuity are declared victims, yielding high directionality (symbolic extraction). The spatial scope differential amplifies this: the diaspora's universal scope means its identity benefit is diffuse, while the Turkish state's national scope concentrates the sovereignty cost. Effective extraction is therefore structurally asymmetric even though base epsilon is low.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â reversing the loss of the cathedral â is dead, but the arrangement (the restitution claim) persists as ideological performance. This prevents mislabeling the constraint as a functioning rope (it has victims and no coordination benefit for them) or as an active snare (it lacks coercion and enforcement). The R5 mismatch between founding_problem_status=dead and disappearance_verdict=contested flags the constraint as a zombie/piton, confirming the classification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    orthodox_restitution_committer_location,
    'Does this constraint represent a genuine sovereignty claim with potential enforcement, or a symbolic performance of identity with no material pathway?',
    'Tracking of diplomatic expenditure and legal action: if Greece or the Patriarchate files formal sovereignty litigation or sanctions requests, the claim has enforcement potential; if activity remains confined to rhetorical commemoration, it is symbolic performance.',
    'If the former, classification shifts toward snare or tangled_rope; if the latter, piton classification is confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(orthodox_restitution_committer_location, empirical, 'Whether the Orthodox restitution claim is materially enforced or purely symbolic.').

omega_variable(
    symbolic_vs_material_extraction,
    'Is the extraction measured as symbolic diplomatic friction and identity cost, or as material resource transfer?',
    'Economic accounting of diplomatic costs and tourism or revenue flow disruptions; assessment of whether the claim blocks material resource access or only legitimacy recognition.',
    'If material, extractiveness should be revised upward; if purely symbolic, the low epsilon reading is confirmed but the constraint''s classification as piton depends on theater ratio.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(symbolic_vs_material_extraction, conceptual, 'Whether the extraction from Turkish sovereignty is material or symbolic.').

omega_variable(
    kernel_reading_underdetermination,
    'If the kernel were read through universal heritage rather than Orthodox restitution framing, would the beneficiary-victim structure invert or dissolve?',
    'Comparative structural analysis of the universal_heritage_reading constraint story; identification of whether the same actors appear as beneficiaries, victims, or neutral parties under the alternative reading.',
    'Resolves whether the kernel''s readings are mutually constitutive or structurally independent; informs contamination propagation if one reading''s purity degrades.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_underdetermination, conceptual, 'How sibling readings of the Hagia Sophia kernel redistribute beneficiary and victim seats.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hagia_sophia_substrate__orthodox_restitution_reading, 0, 90).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hagia_sophia_orthodox_tr_t0, hagia_sophia_substrate__orthodox_restitution_reading, theater_ratio, 0, 0.6).
narrative_ontology:measurement(hagia_sophia_orthodox_tr_t30, hagia_sophia_substrate__orthodox_restitution_reading, theater_ratio, 30, 0.65).
narrative_ontology:measurement(hagia_sophia_orthodox_tr_t60, hagia_sophia_substrate__orthodox_restitution_reading, theater_ratio, 60, 0.7).
narrative_ontology:measurement(hagia_sophia_orthodox_tr_t90, hagia_sophia_substrate__orthodox_restitution_reading, theater_ratio, 90, 0.72).

% Extraction over time
narrative_ontology:measurement(hagia_sophia_orthodox_be_t0, hagia_sophia_substrate__orthodox_restitution_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(hagia_sophia_orthodox_be_t30, hagia_sophia_substrate__orthodox_restitution_reading, base_extractiveness, 30, 0.22).
narrative_ontology:measurement(hagia_sophia_orthodox_be_t60, hagia_sophia_substrate__orthodox_restitution_reading, base_extractiveness, 60, 0.25).
narrative_ontology:measurement(hagia_sophia_orthodox_be_t90, hagia_sophia_substrate__orthodox_restitution_reading, base_extractiveness, 90, 0.28).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(hagia_sophia_substrate__orthodox_restitution_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(hagia_sophia_substrate__orthodox_restitution_reading, hagia_sophia_substrate__islamic_sovereignty_reading).
narrative_ontology:affects_constraint(hagia_sophia_substrate__orthodox_restitution_reading, hagia_sophia_substrate__universal_heritage_reading).

% DUAL FORMULATION NOTE:
% The hagia_sophia_substrate kernel decomposes into three structurally distinct constraints because the natural-language label 'Hagia Sophia legitimacy' conflates competing claims with different epsilon values, beneficiary structures, and enforcement profiles. The Orthodox restitution reading (piton, low epsilon, no enforcement) is not the same constraint as the Islamic sovereignty reading (active state enforcement) or the universal heritage reading (coordination-oriented, victimless). Each reading gets its own constraint story; they are linked as a constraint family via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
