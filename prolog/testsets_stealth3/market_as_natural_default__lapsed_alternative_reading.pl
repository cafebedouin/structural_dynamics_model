% ============================================================================
% CONSTRAINT STORY: market_as_natural_default__lapsed_alternative_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_market_as_natural_default__lapsed_alternative_reading, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: market_as_natural_default__lapsed_alternative_reading
 *   human_readable: Market-as-Natural Default (Lapsed-Alternative Reading)
 *   domain: political economy/ideology studies/economic history
 *
 * SUMMARY:
 *   A dominant framing treats market exchange as the natural, default way of
 *   organizing economic life. Under this reading, the framing's persistence
 *   is explained by lapsed memory: historically visible alternatives — guild
 *   systems, commons regimes, cooperative production, planning arrangements —
 *   dropped out of living memory and mainstream pedagogy over the
 *   mid-twentieth century, and the default persists because no one remembers
 *   there was ever a choice. No agent defends the framing, no agent collects
 *   from it, and nothing bars rediscovery of the alternatives; the record
 *   sits in archives, recoverable by research. The arrangement is therefore
 *   inertial rather than enforced: an administrative default kept by nobody,
 *   costing its inhabitants only the options they cannot imagine. KEY AGENTS
 *   (by structural relationship): - economics_commonsense_transmitters:
 *   Administrator of reproduction (institutional/constrained) — transmits the
 *   default without defending it - general_public: Diffuse bearer of
 *   unrecognized costs (moderate/identity_locked) — inhabits the default;
 *   narrowed option-recognition - economic_historians: Analytical observer
 *   (moderate/analytical) — holds the recoverable record of alternatives -
 *   heterodox_economists: Excluded critic (organized/constrained) — objects
 *   from outside the venues of reproduction
 *
 * KEY AGENTS:
 *   - economics_commonsense_transmitters: Administrator of reproduction (institutional/constrained) — transmits the default without defending it
 *   - general_public: Diffuse bearer of unrecognized costs (moderate/identity_locked) — inhabits the default; narrowed option-recognition
 *   - economic_historians: Analytical observer (moderate/analytical) — holds the recoverable record of alternatives
 *   - heterodox_economists: Excluded critic (organized/constrained) — objects from outside the venues of reproduction
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(market_as_natural_default__lapsed_alternative_reading, 0.12).
domain_priors:suppression_score(market_as_natural_default__lapsed_alternative_reading, 0.18).
domain_priors:theater_ratio(market_as_natural_default__lapsed_alternative_reading, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(market_as_natural_default__lapsed_alternative_reading, extractiveness, 0.12).
narrative_ontology:constraint_metric(market_as_natural_default__lapsed_alternative_reading, suppression_requirement, 0.18).
narrative_ontology:constraint_metric(market_as_natural_default__lapsed_alternative_reading, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(market_as_natural_default__lapsed_alternative_reading, accessibility_collapse, 0.25).
narrative_ontology:constraint_metric(market_as_natural_default__lapsed_alternative_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(market_as_natural_default__lapsed_alternative_reading, piton).
narrative_ontology:human_readable(market_as_natural_default__lapsed_alternative_reading, "Market-as-Natural Default (Lapsed-Alternative Reading)").
narrative_ontology:topic_domain(market_as_natural_default__lapsed_alternative_reading, "political economy/ideology studies/economic history").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(market_as_natural_default__lapsed_alternative_reading, 'af846275-872c-4bd7-90c3-b15ec3294b5e').
narrative_ontology:cs_kernel_codification('af846275-872c-4bd7-90c3-b15ec3294b5e', distributed).
narrative_ontology:cs_authority_grounding('af846275-872c-4bd7-90c3-b15ec3294b5e', self_enforcing).
narrative_ontology:cs_reading_relation('af846275-872c-4bd7-90c3-b15ec3294b5e', market_as_natural_default__beneficiary_maintained_reading, forecloses).
narrative_ontology:cs_reading_relation('af846275-872c-4bd7-90c3-b15ec3294b5e', market_as_natural_default__hybrid_amnesia_reading, forecloses).
narrative_ontology:cs_axiom('af846275-872c-4bd7-90c3-b15ec3294b5e', foundational, naturalization_persists_by_lapsed_memory_alone).
narrative_ontology:cs_axiom_status(naturalization_persists_by_lapsed_memory_alone, holdable).
narrative_ontology:cs_axiom_grounding('af846275-872c-4bd7-90c3-b15ec3294b5e', naturalization_persists_by_lapsed_memory_alone, empirically_contingent).
narrative_ontology:cs_axiom('af846275-872c-4bd7-90c3-b15ec3294b5e', secondary, forgotten_alternatives_remain_recoverable).
narrative_ontology:cs_axiom_status(forgotten_alternatives_remain_recoverable, holdable).
narrative_ontology:cs_axiom_grounding('af846275-872c-4bd7-90c3-b15ec3294b5e', forgotten_alternatives_remain_recoverable, empirically_contingent).
narrative_ontology:cs_reference_frame('af846275-872c-4bd7-90c3-b15ec3294b5e', visible_alternative_plurality).
narrative_ontology:cs_drift_state('af846275-872c-4bd7-90c3-b15ec3294b5e', contemporary_policy_pedagogy, gap(codification_collapse, substantial, false)).
narrative_ontology:cs_created_at('af846275-872c-4bd7-90c3-b15ec3294b5e', '').
narrative_ontology:cs_kernel_id(market_as_natural_default__lapsed_alternative_reading, market_as_natural_default).

% --- Structural relationships ---
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(market_as_natural_default__lapsed_alternative_reading, general_public).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Write the textbooks, teach the curricula, and set the terms of financial journalism through which economic common sense is reproduced. They inherited a syllabus in which market exchange appears as the way economies are organized; earlier and adjacent arrangements appear, if at all, as historical curiosities rather than live options. They did not construct this framing and do not campaign for it — reproducing it is simply what the standard curriculum contains. Introducing systematic treatment of non-market arrangements would mean rebuilding course sequences against hiring, accreditation, and citation structures built around the standard canon, with little reward attached to the effort.
narrative_ontology:constraint_stakeholder(market_as_natural_default__lapsed_alternative_reading, economics_commonsense_transmitters, agenda_setter,
    institutional, generational, constrained, global).

% Inhabit the default as part of the background of how things are. Policy preferences, career plans, and civic imagination are formed inside it; arrangements outside the market frame do not register as options because their historical existence is unknown to them. Nothing is taken from them that they notice; what narrows is the set of arrangements they can imagine asking for. The framing is fused with their sense of what an economy simply is, so the only route out is encountering the historical record that other arrangements operated — often recently — and that record sits in institutions that do not teach it.
narrative_ontology:constraint_stakeholder(market_as_natural_default__lapsed_alternative_reading, general_public, payer,
    moderate, biographical, identity_locked, global).

% Work in the archives where the forgotten alternatives are documented: commons charters, guild regulations, cooperative ledgers, planning-era records. From this seat the default has a visible date of birth and a traceable mechanism of spread, and the alternatives are recoverable rather than destroyed. They periodically publish recoveries of specific arrangements, but their findings circulate in journals and monographs that rarely reach the curricula and newsrooms through which the default reproduces.
narrative_ontology:constraint_stakeholder(market_as_natural_default__lapsed_alternative_reading, economic_historians, observer,
    moderate, generational, analytical, global).

% Organize in marginal associations and journals and argue that the default is a constructed framing rather than a fact of nature. Their objections are substantive and persistent, but they circulate outside the venues — flagship curricula, mainstream journals, policy ministries — where the default is reproduced, so the conversation that would contest it never actually convenes. Their position would change materially if the historical record of alternatives were admitted to the standard syllabus.
narrative_ontology:constraint_stakeholder(market_as_natural_default__lapsed_alternative_reading, heterodox_economists, excluded,
    organized, generational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(market_as_natural_default__lapsed_alternative_reading, diffuse).
narrative_ontology:fixing_cost_class(market_as_natural_default__lapsed_alternative_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared default assumption — that exchange is organized through markets — which spares each actor the cost of re-deriving or re-debating economic arrangement; expectations align without deliberation.
% TRANSFER_FUNCTION: Moves nothing measurable: no money, work, attention, or status transfers through the naturalization itself. Its entire effect is confined to narrowing which arrangements are imagined as available.
% ABSENT_VOICES: Economic historians and practitioners of non-market arrangements — commons stewards, cooperative organizers, planners — would object that the default erases demonstrated viable alternatives; they are absent from the curricula, newsrooms, and policy venues where the default reproduces.
% DISAPPEARANCE_RATIONALE: Curricula, policy frameworks, and everyday economic common sense are built on the default. If it vanished overnight — if everyone woke remembering that guild systems, commons regimes, cooperative production, and planning arrangements had operated at scale — policy imagination, business education, and institutional design would reopen around visibly competing alternatives and reorganize before settling into new defaults.
% FOUNDING_PROBLEM: Nothing was deliberately built to solve a problem: the default is what remained when the comparison set vanished. Its functional ancestor — providing a shared baseline while alternatives were still contested and visible — lapsed when postwar pedagogy and living memory dropped the alternatives, leaving a baseline without a remembered choice behind it.
% FOUNDING_PROBLEM_CORROBORATION: No benefiting party attests the genealogy because none exists under this reading; corroboration comes entirely from outside any gaining seat. Curriculum records show comparative-economic-systems instruction contracting after the Cold War, and economic historians document the mid-century disappearance of alternative arrangements from pedagogy and policy discourse — attested from the archival seat, not from any seat served by the default.
narrative_ontology:disappearance_verdict(market_as_natural_default__lapsed_alternative_reading, world_rearranges).
narrative_ontology:founding_problem_status(market_as_natural_default__lapsed_alternative_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(market_as_natural_default__lapsed_alternative_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(market_as_natural_default__lapsed_alternative_reading, 'none', 1).
narrative_ontology:epsilon_provenance(market_as_natural_default__lapsed_alternative_reading, 0.12, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(market_as_natural_default__lapsed_alternative_reading_tests).
:- end_tests(market_as_natural_default__lapsed_alternative_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is 0.12 because no transfer occurs anywhere in the structure: the only cost the arrangement imposes is narrowed imagination, which this reading values small. Suppression is 0.18 because nothing coerces — alternatives are unavailable through absence of memory, not through barriers; the number registers the residual friction of re-encountering the record, not enforcement. Theater is 0.08 because nothing is performed: no maintenance rituals defend the default, no compliance is staged; the arrangement is kept by sheer uninhabitance of the alternative. Accessibility collapse is 0.25 — the decisive divergence from a natural-law profile: understanding the arrangement REOPENS the alternatives rather than closing them, since the reading's core claim is that the record survives and research recovers it. Resistance is 0.15 because the default is invisible to its inhabitants; criticism exists but targets framings its holders perceive as imposed, and under this reading's lights the naturalization draws little resistance precisely because it is not experienced as an imposition. The claimed type (piton) and the metrics were authored independently: the claim states what this reading holds structurally true (dead mandate, inertial persistence, no capturer, costly-to-fix relative to diffuse benefit), the metrics describe the arrangement's actual operation. The measurement series run on one shared time grid (points 0, 6, 12, 18, 24, 30) with every tracked metric authored at every point; suppression_requirement is deliberately not tracked as a series because the enforcement picture is static-null — there is no enforcement machinery whose build-up or decay could be traced, and the scalar suppression value carries that fact.
 *
 * PERSPECTIVAL GAP:
 *   The three inhabited seats experience radically different arrangements despite sharing one referent. From the transmitter seat the default is a neutral professional background — a settled canon that imposes no cost on its user and would be expensive to disturb; classified from inside, it would look like benign infrastructure. From the public seat there is no experienced arrangement at all: the purest inertial case, in which the constraint operates entirely outside awareness and extraction is imperceptible from within. From the historian seat the same arrangement is a dated artifact with a traceable spread mechanism and recoverable alternatives. The engine computes this divergence from the power and exit data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   No beneficiaries and no victims are declared — that absence IS this reading's core structural claim (no identifiable beneficiary class; no extraction-bearing victim class, since nothing transfers). Because the derivation chain finds no Phase-B beneficiary/victim data to read, canonical per-power-atom fallbacks would be uninformative, so explicit overrides pin the near-symmetric positions the reading actually describes: institutional transmitters at d=0.45 (a mild beneficiary-side tilt — the professional convenience of a settled canon — with no collection of the arrangement's gains) and the moderate public at d=0.58 (a mild target-side tilt — narrowed option-recognition — with no transfer borne). Flows are near-zero in both directions, which is what the tight band around 0.5 encodes. Suppression is authored as a raw structural property and is not scaled by power or scope; only extractiveness is scaled in the engine's arithmetic, and the analytical historian seat sits outside the beneficiary/target scaling entirely.
 *
 * MANDATROPHY ANALYSIS:
 *   The arrangement's functional ancestor — supplying a shared decision baseline while alternatives were contested and visible — is dead; what persists is a baseline whose comparison set has been forgotten. Mandatrophy is therefore resolved and declared. The classification prevents mislabeling in both directions: read without this reading's structural data, the naturalization invites a snare-style misread (inventing enforcers and beneficiaries that the record does not show) or a mountain-style misread (asserting a naturality that the archival record contradicts — the arrangement has a birthdate and a spread mechanism, so emerges_naturally is honestly false and the mountain claim is unavailable). The piton designation names the actual condition: dead mandate, inertial persistence, no concentrated capturer (gain_flow is an affirmative diffuse), and a fix whose cost to whoever could perform it — rebuilding economic common sense through curricula and media — is prohibitive relative to the diffuse benefit of fixing.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_membership,
    'This constraint instantiates the lapsed_alternative_reading of the market_as_natural_default kernel; how would the classification change if the same standing arrangement were authored under a sibling reading?',
    'Re-author the same referent under beneficiary_maintained_reading (which adds incumbent beneficiaries and active post-hoc defense, raising epsilon and adding enforcement structure) and under hybrid_amnesia_reading (which adds delayed beneficiary capture riding on the lapsed closure), then compare computed types across the three files.',
    'Only this reading yields a beneficiary-free, enforcement-free, low-epsilon profile; the sibling readings convert the same naturalization into an enforced extraction structure with identifiable seats, moving the computed type toward the extractive categories.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_membership, conceptual, 'Committer-frame membership: one of three readings of the market-naturalization kernel, generated clean and epsilon-invariant.').

omega_variable(
    residual_maintenance_detection,
    'Does any subtle, unorganized maintenance of the naturalization occur — curricular selection pressures, citation-network gating, funding incentives — that would falsify the no-active-closure premise?',
    'Historiographic audit of economics curricula, publishing records, and hiring patterns distinguishing coordinated exclusion of non-market frameworks from mere drift of attention.',
    'Detected maintenance would shift this story toward the hybrid reading (lapsed closure enabling capture) and raise effective extraction above the authored ceiling.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(residual_maintenance_detection, empirical, 'Whether truly no agent performs even implicit upkeep of the default framing.').

omega_variable(
    recovery_dissolution_sufficiency,
    'Would recovering the forgotten alternatives actually dissolve the naturalization, or does the default re-form after each recovery episode?',
    'Track episodes in which alternatives entered mainstream discourse (for example the post-crisis revival of interest in commons and cooperative arrangements) and measure whether the default framing rebounded once attention receded.',
    'If the default re-forms, persistence has a ratchet component that pure lapsed memory does not explain, and the constraint is stickier than this reading''s mechanism accounts for.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(recovery_dissolution_sufficiency, empirical, 'Whether memory restoration alone suffices to dissolve the naturalized default.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(market_as_natural_default__lapsed_alternative_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mark_tr_t0, market_as_natural_default__lapsed_alternative_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(mark_tr_t6, market_as_natural_default__lapsed_alternative_reading, theater_ratio, 6, 0.06).
narrative_ontology:measurement(mark_tr_t12, market_as_natural_default__lapsed_alternative_reading, theater_ratio, 12, 0.06).
narrative_ontology:measurement(mark_tr_t18, market_as_natural_default__lapsed_alternative_reading, theater_ratio, 18, 0.07).
narrative_ontology:measurement(mark_tr_t24, market_as_natural_default__lapsed_alternative_reading, theater_ratio, 24, 0.08).
narrative_ontology:measurement(mark_tr_t30, market_as_natural_default__lapsed_alternative_reading, theater_ratio, 30, 0.08).

% Extraction over time
narrative_ontology:measurement(mark_be_t0, market_as_natural_default__lapsed_alternative_reading, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(mark_be_t6, market_as_natural_default__lapsed_alternative_reading, base_extractiveness, 6, 0.09).
narrative_ontology:measurement(mark_be_t12, market_as_natural_default__lapsed_alternative_reading, base_extractiveness, 12, 0.1).
narrative_ontology:measurement(mark_be_t18, market_as_natural_default__lapsed_alternative_reading, base_extractiveness, 18, 0.1).
narrative_ontology:measurement(mark_be_t24, market_as_natural_default__lapsed_alternative_reading, base_extractiveness, 24, 0.11).
narrative_ontology:measurement(mark_be_t30, market_as_natural_default__lapsed_alternative_reading, base_extractiveness, 30, 0.12).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(market_as_natural_default__lapsed_alternative_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(market_as_natural_default__lapsed_alternative_reading, information_standard).
narrative_ontology:affects_constraint(market_as_natural_default__lapsed_alternative_reading, beneficiary_maintained_reading).
narrative_ontology:affects_constraint(market_as_natural_default__lapsed_alternative_reading, hybrid_amnesia_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'the market is the natural default' covers three structurally distinct claims that differ on the maintenance mechanism and therefore on epsilon. This file instantiates the lapsed-alternative reading: persistence by forgetting alone, no beneficiary class, no enforcement, epsilon approximately 0.12. beneficiary_maintained_reading instantiates active post-hoc defense by incumbents — adds beneficiaries and enforcement machinery, substantially higher epsilon. hybrid_amnesia_reading instantiates lapsed closure followed by beneficiary capture — adds delayed capture dynamics, intermediate epsilon. The readings share one referent (the standing naturalization) and are linked here as a constraint family; each is generated as a clean, epsilon-invariant constraint in its own file, per the epsilon-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(market_as_natural_default__lapsed_alternative_reading, institutional, 0.45).
constraint_indexing:directionality_override(market_as_natural_default__lapsed_alternative_reading, moderate, 0.58).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
