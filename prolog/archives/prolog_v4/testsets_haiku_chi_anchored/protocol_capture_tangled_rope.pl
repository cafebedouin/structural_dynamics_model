% ============================================================================
% CONSTRAINT STORY: protocol_capture_tangled_rope
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_protocol_capture_tangled_rope, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: protocol_capture_tangled_rope
 *   human_readable: The Captured Commons (Embrace, Extend, Extinguish)
 *   domain: technological/economic
 *
 * SUMMARY:
 *   The Embrace, Extend, Extinguish (EEE) strategy represents a canonical
 *   mechanism for converting decentralized commons into captured,
 *   proprietarized systems. The dominant platform entity adopts an open
 *   protocol, extends it with proprietary features that become essential for
 *   user experience, and gradually renders the original open alternative
 *   incompatible and obsolete. This constraint exhibits Tangled Rope
 *   characteristics: there is genuine coordination function (enabling
 *   interoperability across heterogeneous systems), but that coordination is
 *   asymmetrically controlled by the dominant entity, which extracts value
 *   through lock-in, network effects, and ecosystem control. The constraint's
 *   suppression (0.68) reflects multiple barriers: technical switching costs
 *   (migration complexity), network effects (value of compatibility),
 *   switching costs (data portability), and ecosystem dependency (third-party
 *   extensions built on proprietary APIs). Theater ratio (0.64) reflects that
 *   the dominant entity maintains nominal commitment to open standards
 *   (performative openness) while simultaneously rendering those standards
 *   functionally subordinate to proprietary extensions. Independent
 *   implementations remain theoretically possible but practically
 *   unmaintainable as the proprietary feature set diverges from the open
 *   baseline.
 *
 * KEY AGENTS:
 *   - Dominant Platform Entity: Primary beneficiary (institutional/arbitrage) — captures monopoly rent through lock-in; controls protocol evolution direction
 *   - Protocol Commons: Primary victim (powerless/trapped) — abstract collective that cannot exit; gradually rendered extinct as proprietary extensions become essential
 *   - Independent Implementers: Secondary victim (powerless/trapped) — small companies, open-source projects cannot match proprietary feature velocity; eventually abandoned by users
 *   - End Users: Secondary victim (moderate/constrained) — benefit from enhanced functionality but lose exit options; trapped by switching costs and network effects
 *   - Standards Bodies & OSS Communities: Mixed (organized/constrained) — maintain nominal governance of open standard but lack enforcement power against dominant entity's unilateral extensions
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees both genuine coordination problem and structural extraction mechanism; prevents false natural law classification
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(protocol_capture_tangled_rope, 0.58).
domain_priors:suppression_score(protocol_capture_tangled_rope, 0.68).
domain_priors:theater_ratio(protocol_capture_tangled_rope, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(protocol_capture_tangled_rope, extractiveness, 0.58).
narrative_ontology:constraint_metric(protocol_capture_tangled_rope, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(protocol_capture_tangled_rope, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(protocol_capture_tangled_rope, tangled_rope).
narrative_ontology:human_readable(protocol_capture_tangled_rope, "The Captured Commons (Embrace, Extend, Extinguish)").
narrative_ontology:topic_domain(protocol_capture_tangled_rope, "technological/economic").

domain_priors:requires_active_enforcement(protocol_capture_tangled_rope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(protocol_capture_tangled_rope, dominant_platform_entity).
narrative_ontology:constraint_victim(protocol_capture_tangled_rope, protocol_commons).
narrative_ontology:constraint_victim(protocol_capture_tangled_rope, independent_implementers).
narrative_ontology:constraint_victim(protocol_capture_tangled_rope, end_users_locked_in).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE PROTOCOL COMMONS (SNARE) — The open standard itself has no agency. Once proprietary extensions become necessary for functionality, the commons is effectively extinguished. No exit: users must adopt the extended version or lose interoperability. d≈0.95, f(d)≈1.42, σ=1.2 → χ≈0.98. Pure extraction through institutional inertia.
constraint_indexing:constraint_classification(protocol_capture_tangled_rope, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: INDEPENDENT IMPLEMENTERS (SNARE) — Small companies, open-source projects, and academic implementations face a choice: implement proprietary extensions (becoming dependent) or remain compatible with the original standard (becoming incompatible with the dominant platform's user base). d≈0.92, f(d)≈1.38, σ=1.2 → χ≈0.95. High suppression: technical lock-in through protocol fragmentation.
constraint_indexing:constraint_classification(protocol_capture_tangled_rope, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: END USERS (SNARE) — Users benefit from the enhanced functionality of proprietary extensions but lose the ability to switch to independent implementations. Trapped: switching costs increase as data, habits, and ecosystem lock-in deepen. d≈0.85, f(d)≈1.15, σ=1.2 → χ≈0.80. Extraction through convenience and network effects.
constraint_indexing:constraint_classification(protocol_capture_tangled_rope, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: STANDARDS BODIES & OSS COMMUNITIES (TANGLED ROPE) — These agents see genuine coordination function (defining interoperability) but are also subject to extraction: the dominant platform's control of the de facto standard allows it to redirect how the protocol evolves. d≈0.55, f(d)≈0.75, σ=1.2 → χ≈0.52. Mixed: coordination is real but asymmetric.
constraint_indexing:constraint_classification(protocol_capture_tangled_rope, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: DOMINANT PLATFORM ENTITY (ROPE) — The dominant entity frames EEE as coordination: embracing open standards, extending them to serve users better, maintaining backward compatibility. From this perspective, the constraint is a legitimate protocol evolution mechanism. d≈0.08, f(d)≈-0.10, σ=1.2 → χ≈-0.07. Net beneficiary through strategic control.
constraint_indexing:constraint_classification(protocol_capture_tangled_rope, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: THE ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational perspective, this constraint exhibits both genuine coordination (interoperability problem) and structural extraction (asymmetric control of evolution). The metrics confirm: ε=0.58, suppression=0.68, theater=0.64. True tangled rope: coordination benefit is real (users do get enhanced functionality), but extraction mechanism is real (lock-in through proprietary extensions). d≈0.65, f(d)≈0.95, σ=1.0 → χ≈0.55.
constraint_indexing:constraint_classification(protocol_capture_tangled_rope, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(protocol_capture_tangled_rope_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(protocol_capture_tangled_rope, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(protocol_capture_tangled_rope, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(protocol_capture_tangled_rope, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(protocol_capture_tangled_rope_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.58): Moderate-high. The dominant entity captures substantial value through lock-in and ecosystem control, but the extraction is not total (users do benefit from the enhanced functionality). The value has grown from 0.15 (when open and proprietary implementations were competitive) to 0.58 as proprietary features become essential. Suppression (0.68): High. Multiple barriers prevent exit: technical lock-in (proprietary APIs), network effects (ecosystem concentration), switching costs (data migration, retraining), and ecosystem dependency. The original open protocol is still technically available, but its practical utility degrades as interoperability with the dominant platform's extensions becomes necessary. Theater ratio (0.64): Moderate-high. The dominant entity maintains nominal openness (open-source libraries, published specs for basic protocol) while the actual coordination mechanism is proprietary extensions. This performative openness serves as legitimacy theater — the entity can claim 'we embrace open standards' while effectively controlling the standard's evolution.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap reflects the classical EEE extraction asymmetry. The dominant platform entity (institutional/arbitrage) genuinely experiences the constraint as Rope — they see themselves as coordinating interoperability and improving the ecosystem. Their d is very low (0.08), making effective extraction negative from their perspective. Independent implementers and the protocol commons experience pure Snare (d ≈ 0.92-0.95): they are trapped with no exit. End users experience Snare with moderately constrained exit (d ≈ 0.85): they can theoretically switch but face high costs. Organized standards bodies experience Tangled Rope (d ≈ 0.55): they maintain coordination function but lack enforcement power. The analytical observer correctly identifies this as Tangled Rope: genuine coordination function exists (the problem of enabling heterogeneous systems to interoperate is real and non-trivial), but the coordination is captured and asymmetrically controlled. The perspectival gap is not a disagreement about facts but about structural position — each agent accurately perceives the constraint from their location in the power hierarchy.
 *
 * DIRECTIONALITY LOGIC:
 *   Dominant platform entity: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Strategic position enables them to unilaterally shape protocol evolution while maintaining public commitment to openness. Effective extraction is negative from their perspective (they are net beneficiary). Protocol commons: Victim + trapped → d≈0.95, f(d)≈1.42. Abstract collective good (interoperability, open standards) has no agency or exit. Extraction through institutional inertia — the original open protocol persists as zombie standard. Independent implementers: Victim + trapped → d≈0.92, f(d)≈1.38. Cannot match proprietary feature velocity; face abandonment as users adopt the extended version. End users: Victim + constrained → d≈0.85, f(d)≈1.15. Can theoretically exit (implement or use alternative) but face high switching costs. As proprietary extensions become essential, constrained exit approaches trapped. Standards bodies/OSS communities: Mixed + constrained → d≈0.55, f(d)≈0.75. Maintain real coordination function but lack asymmetric extraction mechanism. Trapped in coordination role while dominant entity extracts.
 *
 * MANDATROPHY ANALYSIS:
 *   TANGLED ROPE DIAGNOSIS: This constraint avoids mandatrophy through clear structural evidence of BOTH genuine coordination function AND asymmetric extraction. (1) Coordination function is real: the open protocol solves a legitimate problem (heterogeneous system interoperability). Without the coordination mechanism, each platform would be isolated. The protocol commons is a genuine public good. (2) Asymmetric extraction is real: the dominant platform entity captures value by rendering the open baseline subordinate to proprietary extensions, creating lock-in that independent implementers cannot match. The suppression (0.68) reflects real barriers to exit, not just coordination complexity. (3) Active enforcement required: the dominant entity must continuously invest in maintaining the appearance of openness (performative compliance) while extending proprietary features (actual control). This is not self-executing; it requires active institutional effort. The Tangled Rope classification is stable: the constraint exhibits extractive economics (ε=0.58) combined with coordination function (verified by network effects enabling the extraction to work). If the proprietary extensions were purely parasitic (no user value), the lock-in would fail. If the coordination were purely functional with no extraction, independent implementations would remain competitive. The constraint's stability depends on both mechanisms: users benefit from enhanced functionality (coordination works) AND cannot easily escape to alternatives (extraction works).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    extension_necessity_vs_anticompetitive_design,
    'Are proprietary extensions functionally necessary for the use case, or are they deliberately designed to be incompatible with independent implementations?',
    'Technical analysis of extension specifications; comparison with alternative implementations; historical documentation of design decision rationale; user testing of open-only implementations',
    'If necessary: coordination problem (genuine rope from more perspectives). If deliberately anticompetitive: pure extraction (snare from more perspectives, moving toward higher χ)',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(extension_necessity_vs_anticompetitive_design, empirical, 'Whether extensions are functionally necessary or anticompetitively designed').

omega_variable(
    interoperability_collapse_timeline,
    'What timeline threshold defines when the original open protocol becomes effectively extinct in favor of the extended proprietary version?',
    'Market share analysis of open vs proprietary implementations; adoption rates of proprietary extensions; survival metrics for independent implementations; user transition timelines',
    'If threshold < 3 years: rapid extinction suggests deliberate extinguishment strategy. If threshold > 10 years: slower transition suggests genuine utility advantage. Affects whether classification is stable Snare (rapid) vs metastable Tangled Rope degrading to Snare (slow).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interoperability_collapse_timeline, empirical, 'Timeline for protocol transition from open to proprietary dominance').

omega_variable(
    licensing_and_patent_transparency,
    'Does the dominant entity maintain genuine openness around patents and licensing for proprietary extensions, or do patent claims and licensing restrictions create hidden extraction mechanisms?',
    'Patent landscape analysis; licensing history and restrictions; litigation patterns; comparison with stated IP policy; third-party implementation success rates',
    'If transparent and genuinely available: extraction is primarily through switching costs (moderate suppression). If patents weaponized or licenses restrictive: extraction includes direct IP control (high suppression, moves toward Snare classification)',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(licensing_and_patent_transparency, empirical, 'Whether IP licensing enables or restricts independent implementation').

omega_variable(
    coordination_function_degradation,
    'As proprietary extensions accumulate, does the ''protocol coordination'' function degrade into performative compliance (theater), where open implementations are theoretically possible but practically incompatible?',
    'Measurement of theater_ratio over time; tracking of specification complexity and proprietary-ness; assessment of actual vs theoretical interoperability; independent implementation survival rates',
    'If theater rises above 0.70: constraint degrading to Piton (institutional inertia, not real coordination). If theater remains < 0.50: genuine coordination persists despite extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_function_degradation, empirical, 'Whether protocol coordination becomes primarily performative').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(protocol_capture_tangled_rope, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pceee_tr_t0, protocol_capture_tangled_rope, theater_ratio, 0, 0.35).
narrative_ontology:measurement(pceee_tr_t5, protocol_capture_tangled_rope, theater_ratio, 5, 0.5).
narrative_ontology:measurement(pceee_tr_t10, protocol_capture_tangled_rope, theater_ratio, 10, 0.64).

% Extraction over time
narrative_ontology:measurement(pceee_be_t0, protocol_capture_tangled_rope, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(pceee_be_t5, protocol_capture_tangled_rope, base_extractiveness, 5, 0.35).
narrative_ontology:measurement(pceee_be_t10, protocol_capture_tangled_rope, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(protocol_capture_tangled_rope, information_standard).
narrative_ontology:affects_constraint(protocol_capture_tangled_rope, network_effect_consolidation).
narrative_ontology:affects_constraint(protocol_capture_tangled_rope, vendor_lock_in_mechanism).
narrative_ontology:affects_constraint(protocol_capture_tangled_rope, open_source_ecosystem_capture).

% DUAL FORMULATION NOTE:
% The EEE strategy is upstream of specific vendor lock-in instances (e.g., proprietary Bluetooth extensions, EPUB DRM, WebKit rendering engine divergence). This constraint represents the structural mechanism; downstream constraints document specific empirical instances where EEE has been deployed. The network links capture how domination in one protocol enables extraction in dependent protocols.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(protocol_capture_tangled_rope, organized, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
