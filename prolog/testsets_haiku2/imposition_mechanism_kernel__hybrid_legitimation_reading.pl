% ============================================================================
% CONSTRAINT STORY: imposition_mechanism_kernel__hybrid_legitimation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_imposition_mechanism_kernel__hybrid_legitimation_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: imposition_mechanism_kernel__hybrid_legitimation_reading
 *   human_readable: Hybrid Legitimation of New Norms through Imperial Authority and Institutional Incentives
 *   domain: historical_sociology/state_formation/cultural_authority
 *
 * SUMMARY:
 *   An empire achieves widespread adoption of new cultural norms—changes in
 *   dress, ritual behavior, kinship terminology, administrative
 *   procedures—through a mechanism that combines (1) the emperor's exemplary
 *   behavior and its circulation through court culture and administrative
 *   channels, (2) material incentive structures that reward early adoption
 *   and penalize resistance, and (3) moderate coercive enforcement through
 *   local administrators. The constraint is neither bottom-up cultural
 *   diffusion nor top-down coercive imposition, but a hybrid mechanism where
 *   legitimacy is derived from the imperial center's charismatic authority
 *   while compliance is secured through institutional incentives. This is ONE
 *   reading of a contested kernel about how new norms achieve legitimacy and
 *   spread. The sibling readings emphasize either endogenous adoption (the
 *   norm was already ascending and the empire merely accelerated it) or pure
 *   exogenous override (coercion, not authority). This reading claims the
 *   mechanism is genuinely hybrid—both the charisma and the incentives matter
 *   structurally.
 *
 * KEY AGENTS:
 *   - state_administrative_apparatus: Central coordinating actor, sets the agenda through incentive design and imperial symbolism
 *   - imperial_elite: Primary adopters, set the precedent that lower elites follow
 *   - subordinated_populations: Pressured to adopt, gain conditional benefits but lose autonomy
 *   - traditional_authority_holders: Displaced or co-opted, lose legitimacy as new norm becomes authoritative
 *   - administrative_intermediaries: Enforce adoption at the local level through incentives and penalties
 *   - resistance_networks: Excluded from official discourse, their opposition is suppressed and marginalized
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(imposition_mechanism_kernel__hybrid_legitimation_reading, 0.58).
domain_priors:suppression_score(imposition_mechanism_kernel__hybrid_legitimation_reading, 0.42).
domain_priors:theater_ratio(imposition_mechanism_kernel__hybrid_legitimation_reading, 0.61).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(imposition_mechanism_kernel__hybrid_legitimation_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(imposition_mechanism_kernel__hybrid_legitimation_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(imposition_mechanism_kernel__hybrid_legitimation_reading, theater_ratio, 0.61).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(imposition_mechanism_kernel__hybrid_legitimation_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(imposition_mechanism_kernel__hybrid_legitimation_reading, resistance, 0.47).

% --- Constraint claim ---
narrative_ontology:constraint_claim(imposition_mechanism_kernel__hybrid_legitimation_reading, tangled_rope).
narrative_ontology:human_readable(imposition_mechanism_kernel__hybrid_legitimation_reading, "Hybrid Legitimation of New Norms through Imperial Authority and Institutional Incentives").
narrative_ontology:topic_domain(imposition_mechanism_kernel__hybrid_legitimation_reading, "historical_sociology/state_formation/cultural_authority").

domain_priors:requires_active_enforcement(imposition_mechanism_kernel__hybrid_legitimation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(imposition_mechanism_kernel__hybrid_legitimation_reading, 'e5990fa1-9bfe-4594-8837-74df02636351').
narrative_ontology:cs_kernel_codification('e5990fa1-9bfe-4594-8837-74df02636351', distributed).
narrative_ontology:cs_authority_grounding('e5990fa1-9bfe-4594-8837-74df02636351', extraction).
narrative_ontology:cs_interpretation_layer_present('e5990fa1-9bfe-4594-8837-74df02636351').
narrative_ontology:cs_reading_relation('e5990fa1-9bfe-4594-8837-74df02636351', imposition_mechanism_kernel__endogenous_climb_reading, coexists_with).
narrative_ontology:cs_reading_relation('e5990fa1-9bfe-4594-8837-74df02636351', imposition_mechanism_kernel__exogenous_override_reading, coexists_with).
narrative_ontology:cs_axiom('e5990fa1-9bfe-4594-8837-74df02636351', foundational, imperial_charisma_as_primary_legitimating_force).
narrative_ontology:cs_axiom_status(imperial_charisma_as_primary_legitimating_force, holdable).
narrative_ontology:cs_axiom_grounding('e5990fa1-9bfe-4594-8837-74df02636351', imperial_charisma_as_primary_legitimating_force, conventional).
narrative_ontology:cs_axiom('e5990fa1-9bfe-4594-8837-74df02636351', secondary, institutional_incentives_as_compliance_mechanism).
narrative_ontology:cs_axiom_status(institutional_incentives_as_compliance_mechanism, holdable).
narrative_ontology:cs_axiom_grounding('e5990fa1-9bfe-4594-8837-74df02636351', institutional_incentives_as_compliance_mechanism, instrumental).
narrative_ontology:cs_reference_frame('e5990fa1-9bfe-4594-8837-74df02636351', imperial_authority_legitimated_norm_adoption).
narrative_ontology:cs_drift_state('e5990fa1-9bfe-4594-8837-74df02636351', post_initial_enforcement_phase, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('e5990fa1-9bfe-4594-8837-74df02636351', '').
narrative_ontology:cs_kernel_id(imposition_mechanism_kernel__hybrid_legitimation_reading, imposition_mechanism_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(imposition_mechanism_kernel__hybrid_legitimation_reading, state_administrative_apparatus).
narrative_ontology:constraint_beneficiary(imposition_mechanism_kernel__hybrid_legitimation_reading, imperial_elite).
narrative_ontology:constraint_victim(imposition_mechanism_kernel__hybrid_legitimation_reading, subordinated_populations).
narrative_ontology:constraint_victim(imposition_mechanism_kernel__hybrid_legitimation_reading, traditional_authority_holders).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(imposition_mechanism_kernel__hybrid_legitimation_reading, administrative_intermediaries).
narrative_ontology:constraint_vindicates(imposition_mechanism_kernel__hybrid_legitimation_reading, symbolic_authority_transfer_as_legitimation_mechanism).
narrative_ontology:constraint_vindicates(imposition_mechanism_kernel__hybrid_legitimation_reading, charismatic_legitimacy_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Designs and propagates new norms through both imperial example-setting (performative compliance by the ruler) and incentive structures (rewards for adoption, penalties for resistance). Controls the apparatus that enforces adoption at local levels and manages the imperial persona that broadcasts the norm through court culture and documented ritual. Collects increased administrative coherence and reduced coordination costs as the norm spreads.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__hybrid_legitimation_reading, state_administrative_apparatus, agenda_setter,
    institutional, generational, arbitrage, national).

% Adopts the new norm first, both as loyalty signal to the sovereign and as competitive advantage within court hierarchies. Their adoption patterns become the template lower status elites follow. They bear minimal direct cost because compliance is framed as prestige; they gain status elevation and court favor relative to slower adopters.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__hybrid_legitimation_reading, imperial_elite, beneficiary,
    powerful, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(imposition_mechanism_kernel__hybrid_legitimation_reading, imperial_elite, agenda_setter).

% Are pressured to adopt the new norm through a combination of incentive structures (property redistribution conditional on compliance, favorable trade status for norm-adopting communities) and enforcement mechanisms (local administrators' careers depend on adoption metrics in their districts). The framing as 'imperial example' provides a justification that obscures the coercive dimension of the incentives. Exit is not available—the norm spreads through territorial control.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__hybrid_legitimation_reading, subordinated_populations, payer,
    powerless, biographical, trapped, national).

% Lose legitimacy when the imperial norm supersedes their traditional practices. They face a choice: publicly adopt the new norm (accepting loss of distinctive authority) or resist it (risking marginalization or removal). Some are co-opted into the new structure as intermediaries who enforce the norm in exchange for preserved local authority; others are displaced. Their resistance is real but costly.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__hybrid_legitimation_reading, traditional_authority_holders, payer,
    moderate, biographical, constrained, regional).

% Non-agent entity: the symbolic and performative order through which the emperor's example circulates. The constraint operates via court ritual, documented imperial behavior, and cultural narratives that spread downward through hierarchies. The framing of norm adoption as 'following the emperor's wisdom' rather than 'obeying state mandate' is the mechanism by which suppression is modulated.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__hybrid_legitimation_reading, imperial_court_culture, beneficiary,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_non_agent(imposition_mechanism_kernel__hybrid_legitimation_reading, imperial_court_culture).

% Local governors, magistrates, and officials whose career advancement depends on achieving adoption metrics from above. They enforce compliance through incentives (rewarding early adopters, providing technical assistance) and penalties (removing resistant headmen, confiscating property from non-compliant villages). Their enforcement is real, but it is deployed through the framing of the new norm as legitimate innovation rather than coercive imposition.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__hybrid_legitimation_reading, administrative_intermediaries, payer,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(imposition_mechanism_kernel__hybrid_legitimation_reading, administrative_intermediaries, agenda_setter).

% Communities and networks that reject the norm but lack the institutional platform to articulate their resistance publicly. Their opposition is driven by genuine attachment to traditional practices and by accurate perception that the incentive structure penalizes non-compliance. Their voices are marginalized in official histories that frame the norm as voluntarily adopted; their suppression is part of what the constraint's theater accomplishes.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__hybrid_legitimation_reading, resistance_networks, excluded,
    powerless, biographical, trapped, regional).

% Later analysts (historians, sociologists, institutional economists) who examine the constraint's operation and attempt to determine whether the norm achieved legitimacy through bottom-up acceptance, coercive imposition, or the hybrid mechanism. The sources they inherit are already filtered through the imperial court's framing; the administrative records emphasize 'voluntary adoption' while suppressing evidence of enforcement.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__hybrid_legitimation_reading, historical_observers, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(imposition_mechanism_kernel__hybrid_legitimation_reading, state_administrative_apparatus).
narrative_ontology:fixing_cost_class(imposition_mechanism_kernel__hybrid_legitimation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Standardizes cultural norms, practices, and behavioral codes across a dispersed population through a single authoritative reference point (the emperor's exemplary behavior). Solves the collective-action problem of achieving coordinated cultural change without requiring consensus at the grassroots level; the empire-wide norm becomes legible to administrators and creates a shared reference frame that reduces friction in multi-level transactions.
% TRANSFER_FUNCTION: Moves legitimacy authority from local and traditional sources to the imperial center. Moves compliance from voluntary adoption to coerced participation dressed in voluntary language. Moves power and resources from traditional authority holders to imperial administrators and to populations that adopt the norm early (gaining favor and economic benefit). Moves cultural prestige from traditional practices to imperial-approved practices.
% ABSENT_VOICES: Subordinated populations who would accurately characterize the constraint as enforced imposition are structurally excluded from court discourse. Traditional authority holders who benefit from the status quo and have strong reasons to resist are also excluded (though some are co-opted). Communities practicing resistance are silenced through administrative pressure and are not recorded in official histories. The absent-voices class is large and geographically dispersed, making organized articulation difficult.
% DISAPPEARANCE_RATIONALE: If the constraint disappeared, the empire would fragment into patchwork cultural practices again; administrative coherence would decrease; local authority structures would re-establish competing legitimacy claims. The new norm's disappearance would not leave things unchanged—it would require the empire to re-invest in coordination mechanisms or accept reduced administrative reach. The norm persists because the state apparatus continues to enforce it through incentive structures.
% FOUNDING_PROBLEM: A multi-ethnic, multi-regional empire required a unified cultural code to reduce friction in administration, taxation, and law. Diverse local practices made communication and coordination inefficient; a unified norm allowed administrators to understand and predict behavior across regions. The coordination problem was real: achieving standardization through coercion alone was expensive; through bottom-up cultural change alone was slow; the hybrid mechanism (imperial example + incentives) offered a path that reduced both costs.
% FOUNDING_PROBLEM_CORROBORATION: Administrative records attest to the coordination problem and confirm that adoption metrics increased under the incentive regime. Independent historical analysis of settlement patterns and trade data shows accelerating adoption in the decades following the imperial norm's promulgation. However, this corroboration itself is filtered through administrative sources. Resistance narratives, family oral histories, and contemporary local accounts recorded outside the imperial archive attest that adoption was coerced and that the 'imperial example' framing masked enforcement. No single source from OUTSIDE the benefiting apparatus attests to spontaneous, bottom-up acceptance without incentive structures.
narrative_ontology:disappearance_verdict(imposition_mechanism_kernel__hybrid_legitimation_reading, world_rearranges).
narrative_ontology:founding_problem_status(imposition_mechanism_kernel__hybrid_legitimation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(imposition_mechanism_kernel__hybrid_legitimation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(imposition_mechanism_kernel__hybrid_legitimation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(imposition_mechanism_kernel__hybrid_legitimation_reading, 0.58, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(imposition_mechanism_kernel__hybrid_legitimation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(imposition_mechanism_kernel__hybrid_legitimation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(imposition_mechanism_kernel__hybrid_legitimation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58 at endpoint) reflects the constraint's dual character: it genuinely coordinates (reducing administrative friction), but it also extracts legitimacy authority from local sources and redirects it upward, and it extracts compliance from subordinated populations through framed-as-voluntary mechanisms. The trajectory rises steeply early (0.35→0.58 across intervals 0-21) as adoption accelerates under incentive pressure, then plateaus (0.58→0.66 across 21-42) as the norm becomes internalized and resistance costs stabilize, then slightly falls (0.66→0.58 at endpoint) as the constraint transitions from active enforcement to maintenance theater. Suppression (0.42 at endpoint) is LOWER than in a pure snare because the imperial-example framing genuinely reduces the perceived coerciveness from the adopter's perspective; suppression requirement DECREASES over time (0.62→0.38 across 0-35) because the norm becomes culturally accepted and requires less active suppression. Theater ratio (0.61 at endpoint) is ELEVATED because the constraint's persistence depends heavily on maintaining the fiction that adoption is voluntary and inspired by imperial wisdom rather than incentivized compliance. The divergence between extractiveness (rising) and suppression requirement (falling) is diagnostic: as the constraint matures, extractiveness increases (more populations become locked into the new normal) while suppression decreases (the norm is no longer contested because it is normalized), which is exactly the signature of a constraint that succeeds through theater and internalization. All measurements share one time grid (0, 7, 14, 21, 28, 35, 42, 50) with all three metrics present at each point.
 *
 * PERSPECTIVAL GAP:
 *   From the state apparatus and imperial elite perspective, the constraint is genuine coordination—reducing friction, achieving necessary standardization. From the subordinated and traditional authority perspective, the constraint is enforced extraction of compliance dressed in voluntary language. From the administrative intermediary perspective, it is a mixed picture: both coordination (the incentive structure does make sense given the coordination problem) and enforcement (they are evaluated on metrics, not on whether adoption is authentic). Historical observers inherit sources filtered through the imperial archive and thus initially see the constraint as successfully solved coordination; alternative readings emerge only when oral histories, resistance narratives, and external accounts are recovered. The engine computes this perspectival gap from the structural data: high extractiveness + moderate suppression + high theater ratio for powerless/trapped agents points to a constraint that persists through cultural capture rather than coercion alone.
 *
 * DIRECTIONALITY LOGIC:
 *   The imperial elite and state apparatus are structural beneficiaries (they control the norm-setting and collect the coordination benefits without bearing costs; d near 0.0). Subordinated populations are targets (they adopt under pressure, bear compliance costs, lose autonomy; d near 1.0). Traditional authority holders are mixed (they bear costs through displacement, but some are co-opted with preserved authority; d varies by agent, ~0.6-0.8). Administrative intermediaries are complex: they are enforcing the constraint on others but are themselves trapped by metrics and career incentives (d ~0.6). The directionality derivation from beneficiary/victim + power + exit produces these values without override; the constraint's hybrid character (real coordination + real extraction) is reflected in the non-extreme d values.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint avoids the mandatrophy trap (function outliving its founding problem) in the interval [0, 42] because the coordination problem remains live—administrative friction is genuine, the norm's standardization function is still producing value. At the endpoint (interval 50), there is early evidence of mandatrophy: the norm is now internalized and self-maintaining; active enforcement could be reduced without adoption declining; the theater ratio has remained high (0.61) even as suppression requirement falls (0.42), which indicates the constraint is maintained more through cultural narrative than through functional necessity. The measurement trajectory (theater_ratio rising while suppression_requirement falls) is the early signal of mandatrophy onset. Commentary notes in six_questions.founding_problem_status flag this as 'live' but contested, which is accurate for the interval studied; a later reading would likely classify this constraint as piton if the theater ratio remains elevated and extractiveness plateaus while suppression continues to decline.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    bottom_up_vs_imposed_adoption,
    'Did the observed adoption pattern reflect genuine grassroots cultural preference for the new norm, or was adoption coerced through the incentive structures and enforcement mechanisms?',
    'Comparative analysis of adoption rates in regions with vs. without administered incentives; reconstruction of oral histories and family narratives from subordinated populations; examination of administrative records for explicit enforcement metrics and penalties for non-compliance. A natural experiment from regions where incentives were delayed would show whether adoption preceded or followed material rewards.',
    'If adoption was predominantly bottom-up and incentives merely accelerated existing preference, the constraint should be reclassified as rope (genuine coordination). If adoption was predominantly coerced and incentives functioned as carrots accompanying sticks, the constraint should be reclassified as snare (extraction with coordination cover). If the hybrid mechanism is confirmed, the tangled_rope classification stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(bottom_up_vs_imposed_adoption, empirical, 'Whether adoption was driven by authentic grassroots preference or by administered incentives and enforcement.').

omega_variable(
    charisma_as_legitimation_vs_cover_story,
    'Did the emperor''s exemplary behavior and the resulting charismatic authority genuinely confer legitimacy on the new norm, or was it a cover story that masked the incentive-and-enforcement mechanism?',
    'Analysis of contemporary accounts and inscriptions: did populations attribute their adoption to loyalty to the emperor''s example, or did they explicitly note material incentives? Examination of sources written by subordinated populations (when available) vs. administrative records. Comparison with cases where imperial example was invoked but incentives were absent—did adoption occur without material reward?',
    'If charisma was the primary legitimating mechanism and incentives secondary, the constraint''s persistence depends on maintaining the symbolic authority structure, which reduces effective suppression and increases theater ratio—piton-candidate. If incentives were primary and charisma decorative, the constraint is closer to snare (coercion with cultural framing). If both are structurally necessary, the hybrid tangled_rope classification holds.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(charisma_as_legitimation_vs_cover_story, conceptual, 'Whether imperial charisma was a genuine legitimating force or a rationalization for administered compliance.').

omega_variable(
    internalization_vs_sustained_enforcement,
    'As the measurement interval progresses and suppression_requirement declines while theater_ratio rises, has the norm become genuinely internalized (adopted as authentic cultural preference), or is it maintained by declining-but-persistent enforcement infrastructure plus increasing performative theater?',
    'Trajectory analysis at extended time horizons (post-interval-50): if suppression continues to decline and the norm persists, internalization is confirmed. If suppression spikes when incentive structures are weakened or when central authority lapses (e.g., during succession crises), the internalization is not genuine and the constraint is maintained by hidden enforcement. Analysis of what happens when the constraint is no longer actively promoted.',
    'If internalized, the constraint transitions from tangled_rope (active enforcement + extraction) toward rope (genuine cultural norm, coordination without coercion). If maintained by hidden enforcement, it remains tangled_rope or becomes piton (theater masking atrophied function). The endpoint measurement (50) shows early mandatrophy signals; extended measurement would clarify the attractor state.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(internalization_vs_sustained_enforcement, empirical, 'Whether the declining suppression requirement reflects genuine cultural internalization or merely the transition to lower-visibility enforcement maintenance.').

omega_variable(
    kernel_reading_underdetermination,
    'Given the same historical evidence, can each sibling reading (endogenous_climb and exogenous_override) marshal equally defensible narratives from the administrative records and secondary sources?',
    'Systematic audit of which textual sources and empirical findings are compatible with each reading; identification of which sources are definitively incompatible with each reading; assessment of whether the underdetermination arises from genuinely ambiguous evidence or from historian selectivity in which sources are prioritized.',
    'If all three readings remain empirically underdetermined after exhaustive source review, the constraint''s true mechanism is genuinely contestable, and the kernel structure (three coexisting readings) is warranted. If one reading emerges as clearly better-supported, that reading should be reclassified as the primary account and the others as alternative framings (framing under-determination routed to omega, not kernel underdetermination). The contestation itself becomes a fact about the constraint rather than a gap in evidence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_underdetermination, conceptual, 'Whether the kernel''s three readings are equally supported by evidence or whether evidence favors one reading while the others persist as framing artifacts.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(imposition_mechanism_kernel__hybrid_legitimation_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(impo_tr_t0, imposition_mechanism_kernel__hybrid_legitimation_reading, theater_ratio, 0, 0.45).
narrative_ontology:measurement_basis(impo_tr_t0, observed).
narrative_ontology:measurement(impo_tr_t7, imposition_mechanism_kernel__hybrid_legitimation_reading, theater_ratio, 7, 0.52).
narrative_ontology:measurement_basis(impo_tr_t7, observed).
narrative_ontology:measurement(impo_tr_t14, imposition_mechanism_kernel__hybrid_legitimation_reading, theater_ratio, 14, 0.58).
narrative_ontology:measurement_basis(impo_tr_t14, observed).
narrative_ontology:measurement(impo_tr_t21, imposition_mechanism_kernel__hybrid_legitimation_reading, theater_ratio, 21, 0.63).
narrative_ontology:measurement_basis(impo_tr_t21, observed).
narrative_ontology:measurement(impo_tr_t28, imposition_mechanism_kernel__hybrid_legitimation_reading, theater_ratio, 28, 0.68).
narrative_ontology:measurement_basis(impo_tr_t28, observed).
narrative_ontology:measurement(impo_tr_t35, imposition_mechanism_kernel__hybrid_legitimation_reading, theater_ratio, 35, 0.7).
narrative_ontology:measurement_basis(impo_tr_t35, observed).
narrative_ontology:measurement(impo_tr_t42, imposition_mechanism_kernel__hybrid_legitimation_reading, theater_ratio, 42, 0.72).
narrative_ontology:measurement_basis(impo_tr_t42, observed).
narrative_ontology:measurement(impo_tr_t50, imposition_mechanism_kernel__hybrid_legitimation_reading, theater_ratio, 50, 0.61).
narrative_ontology:measurement_basis(impo_tr_t50, observed).

% Extraction over time
narrative_ontology:measurement(impo_be_t0, imposition_mechanism_kernel__hybrid_legitimation_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement_basis(impo_be_t0, observed).
narrative_ontology:measurement(impo_be_t7, imposition_mechanism_kernel__hybrid_legitimation_reading, base_extractiveness, 7, 0.42).
narrative_ontology:measurement_basis(impo_be_t7, observed).
narrative_ontology:measurement(impo_be_t14, imposition_mechanism_kernel__hybrid_legitimation_reading, base_extractiveness, 14, 0.51).
narrative_ontology:measurement_basis(impo_be_t14, observed).
narrative_ontology:measurement(impo_be_t21, imposition_mechanism_kernel__hybrid_legitimation_reading, base_extractiveness, 21, 0.58).
narrative_ontology:measurement_basis(impo_be_t21, observed).
narrative_ontology:measurement(impo_be_t28, imposition_mechanism_kernel__hybrid_legitimation_reading, base_extractiveness, 28, 0.62).
narrative_ontology:measurement_basis(impo_be_t28, observed).
narrative_ontology:measurement(impo_be_t35, imposition_mechanism_kernel__hybrid_legitimation_reading, base_extractiveness, 35, 0.64).
narrative_ontology:measurement_basis(impo_be_t35, observed).
narrative_ontology:measurement(impo_be_t42, imposition_mechanism_kernel__hybrid_legitimation_reading, base_extractiveness, 42, 0.66).
narrative_ontology:measurement_basis(impo_be_t42, observed).
narrative_ontology:measurement(impo_be_t50, imposition_mechanism_kernel__hybrid_legitimation_reading, base_extractiveness, 50, 0.58).
narrative_ontology:measurement_basis(impo_be_t50, observed).

% Suppression requirement over time
narrative_ontology:measurement(impo_su_t0, imposition_mechanism_kernel__hybrid_legitimation_reading, suppression_requirement, 0, 0.62).
narrative_ontology:measurement_basis(impo_su_t0, observed).
narrative_ontology:measurement(impo_su_t7, imposition_mechanism_kernel__hybrid_legitimation_reading, suppression_requirement, 7, 0.58).
narrative_ontology:measurement_basis(impo_su_t7, observed).
narrative_ontology:measurement(impo_su_t14, imposition_mechanism_kernel__hybrid_legitimation_reading, suppression_requirement, 14, 0.5).
narrative_ontology:measurement_basis(impo_su_t14, observed).
narrative_ontology:measurement(impo_su_t21, imposition_mechanism_kernel__hybrid_legitimation_reading, suppression_requirement, 21, 0.44).
narrative_ontology:measurement_basis(impo_su_t21, observed).
narrative_ontology:measurement(impo_su_t28, imposition_mechanism_kernel__hybrid_legitimation_reading, suppression_requirement, 28, 0.4).
narrative_ontology:measurement_basis(impo_su_t28, observed).
narrative_ontology:measurement(impo_su_t35, imposition_mechanism_kernel__hybrid_legitimation_reading, suppression_requirement, 35, 0.38).
narrative_ontology:measurement_basis(impo_su_t35, observed).
narrative_ontology:measurement(impo_su_t42, imposition_mechanism_kernel__hybrid_legitimation_reading, suppression_requirement, 42, 0.42).
narrative_ontology:measurement_basis(impo_su_t42, observed).
narrative_ontology:measurement(impo_su_t50, imposition_mechanism_kernel__hybrid_legitimation_reading, suppression_requirement, 50, 0.42).
narrative_ontology:measurement_basis(impo_su_t50, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(imposition_mechanism_kernel__hybrid_legitimation_reading, attachment_coordination).
narrative_ontology:boltzmann_floor_override(imposition_mechanism_kernel__hybrid_legitimation_reading, 0.12).
narrative_ontology:affects_constraint(imposition_mechanism_kernel__hybrid_legitimation_reading, imposition_mechanism_kernel__endogenous_climb_reading).
narrative_ontology:affects_constraint(imposition_mechanism_kernel__hybrid_legitimation_reading, imposition_mechanism_kernel__exogenous_override_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three structurally distinct readings of the contested kernel 'imposition_mechanism_kernel'. All three readings share the same empirical domain (historical norm adoption in multi-ethnic empires) but differ in which mechanism—endogenous cultural preference, exogenous coercion, or hybrid symbolic authority + incentives—is treated as primary. The hybrid_legitimation_reading claims that neither bottom-up nor purely coercive mechanisms accurately capture the observed pattern; instead, legitimacy is derived from imperial charisma and authority transfer, while compliance is secured through institutional incentives. This reading influences and is influenced by the sibling readings: the endogenous_climb_reading emphasizes pre-existing grassroots preference and downgrades the imperial mechanism to acceleration; the exogenous_override_reading emphasizes coercion and downgrades the charisma to cover story. Empirical evidence is underdetermined between the readings—each can marshal compatible sources from the administrative archive. The network link treats them as an interconnected constraint family where each reading's classification depends partly on how the empirical ambiguities are resolved.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
