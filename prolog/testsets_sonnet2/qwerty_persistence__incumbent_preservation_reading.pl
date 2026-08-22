% ============================================================================
% CONSTRAINT STORY: qwerty_persistence__incumbent_preservation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_qwerty_persistence__incumbent_preservation_reading, []).

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
 *   constraint_id: qwerty_persistence__incumbent_preservation_reading
 *   human_readable: QWERTY Keyboard Layout Dominance — Incumbent Preservation Reading
 *   domain: technology_history/industrial_standards/path_dependence
 *
 * SUMMARY:
 *   This story authors the incumbent-preservation reading of the QWERTY
 *   persistence kernel: the layout's mechanical-jam-prevention rationale
 *   disappeared with electric and digital keyboards, but manufacturers,
 *   trained typists as a professional class, and training institutions have
 *   capital and credentialing investments sunk into QWERTY specifically and
 *   actively defend its position — through procurement specs, certification
 *   exclusivity, and hardware pricing — against layouts with documented
 *   efficiency and ergonomic advantages. This is a distinct constraint from
 *   the sibling lapsed_alternatives_reading, which holds that alternatives
 *   simply failed to reach adoption critical mass through ordinary
 *   coordination dynamics, with no active defensive suppression. The two
 *   readings share the same surface phenomenon (QWERTY's continued dominance)
 *   but diverge sharply on mechanism and therefore on ε: this reading
 *   includes defensive suppression costs (lobbying, procurement gatekeeping,
 *   certification exclusivity) that the lapsed_alternatives reading has no
 *   analog for, since that reading treats the outcome as an unforced
 *   coordination equilibrium rather than an actively policed one.
 *
 * KEY AGENTS:
 *   - keyboard_manufacturers: institutional beneficiary/agenda_setter — sunk capital in QWERTY tooling, defends via procurement lobbying and pricing
 *   - qwerty_trained_typists: organized beneficiary/payer — benefits from ubiquity, pays via lock-out from switching
 *   - typing_training_institutions: organized beneficiary — credentialing monopoly built on QWERTY exclusivity
 *   - alternative_layout_adopters, efficiency_seeking_typists, ergonomic_injury_sufferers: powerless payers — bear switching costs and injury risk alone
 *   - new_entrant_layout_designers: excluded — no institutional path to market
 *   - standards_historians: analytical observer
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(qwerty_persistence__incumbent_preservation_reading, 0.61).
domain_priors:suppression_score(qwerty_persistence__incumbent_preservation_reading, 0.58).
domain_priors:theater_ratio(qwerty_persistence__incumbent_preservation_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(qwerty_persistence__incumbent_preservation_reading, extractiveness, 0.61).
narrative_ontology:constraint_metric(qwerty_persistence__incumbent_preservation_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(qwerty_persistence__incumbent_preservation_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(qwerty_persistence__incumbent_preservation_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(qwerty_persistence__incumbent_preservation_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(qwerty_persistence__incumbent_preservation_reading, tangled_rope).
narrative_ontology:human_readable(qwerty_persistence__incumbent_preservation_reading, "QWERTY Keyboard Layout Dominance — Incumbent Preservation Reading").
narrative_ontology:topic_domain(qwerty_persistence__incumbent_preservation_reading, "technology_history/industrial_standards/path_dependence").

domain_priors:requires_active_enforcement(qwerty_persistence__incumbent_preservation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(qwerty_persistence__incumbent_preservation_reading, '6463a3fd-fa92-4865-8fe2-97af2dca6a5d').
narrative_ontology:cs_kernel_codification('6463a3fd-fa92-4865-8fe2-97af2dca6a5d', distributed).
narrative_ontology:cs_authority_grounding('6463a3fd-fa92-4865-8fe2-97af2dca6a5d', distributed).
narrative_ontology:cs_reading_relation('6463a3fd-fa92-4865-8fe2-97af2dca6a5d', qwerty_persistence__lapsed_alternatives_reading, coexists_with).
narrative_ontology:cs_axiom('6463a3fd-fa92-4865-8fe2-97af2dca6a5d', foundational, capital_protection_drives_standard_persistence).
narrative_ontology:cs_axiom_status(capital_protection_drives_standard_persistence, holdable).
narrative_ontology:cs_axiom_grounding('6463a3fd-fa92-4865-8fe2-97af2dca6a5d', capital_protection_drives_standard_persistence, empirically_contingent).
narrative_ontology:cs_axiom('6463a3fd-fa92-4865-8fe2-97af2dca6a5d', secondary, defensive_gatekeeping_constitutes_suppression).
narrative_ontology:cs_axiom_status(defensive_gatekeeping_constitutes_suppression, holdable).
narrative_ontology:cs_axiom_grounding('6463a3fd-fa92-4865-8fe2-97af2dca6a5d', defensive_gatekeeping_constitutes_suppression, empirically_contingent).
narrative_ontology:cs_reference_frame('6463a3fd-fa92-4865-8fe2-97af2dca6a5d', mechanical_typewriter_jam_prevention_era).
narrative_ontology:cs_drift_state('6463a3fd-fa92-4865-8fe2-97af2dca6a5d', digital_keyboard_era, gap(axiom_overriding, severe, false)).
narrative_ontology:cs_created_at('6463a3fd-fa92-4865-8fe2-97af2dca6a5d', '').
narrative_ontology:cs_kernel_id(qwerty_persistence__incumbent_preservation_reading, qwerty_persistence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(qwerty_persistence__incumbent_preservation_reading, keyboard_manufacturers).
narrative_ontology:constraint_beneficiary(qwerty_persistence__incumbent_preservation_reading, qwerty_trained_typists).
narrative_ontology:constraint_beneficiary(qwerty_persistence__incumbent_preservation_reading, typing_training_institutions).
narrative_ontology:constraint_beneficiary(qwerty_persistence__incumbent_preservation_reading, office_equipment_incumbents).
narrative_ontology:constraint_victim(qwerty_persistence__incumbent_preservation_reading, alternative_layout_adopters).
narrative_ontology:constraint_victim(qwerty_persistence__incumbent_preservation_reading, efficiency_seeking_typists).
narrative_ontology:constraint_victim(qwerty_persistence__incumbent_preservation_reading, ergonomic_injury_sufferers).
narrative_ontology:constraint_victim(qwerty_persistence__incumbent_preservation_reading, new_entrant_layout_designers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(qwerty_persistence__incumbent_preservation_reading, qwerty_trained_typists).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Have sunk capital into QWERTY tooling, supply chains, and OEM contracts across decades. Actively resist retooling for alternative layouts (e.g., Dvorak, Colemak) by declining to mass-produce them, pricing alternative-layout hardware at a premium, and lobbying standards bodies to keep QWERTY as the reference layout in procurement specs. Their exit from the arrangement would mean writing off substantial capital investment, so they defend the standard rather than migrate.
narrative_ontology:constraint_stakeholder(qwerty_persistence__incumbent_preservation_reading, keyboard_manufacturers, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(qwerty_persistence__incumbent_preservation_reading, keyboard_manufacturers, beneficiary).

% Have invested years of muscle memory and professional training in QWERTY. Benefit from the layout's ubiquity (any keyboard anywhere works for them) but are simultaneously locked out of adopting a more efficient layout without repeating the training investment. Their professional associations and unions have historically opposed retraining mandates, reinforcing the standard's persistence even where individual members might benefit from switching.
narrative_ontology:constraint_stakeholder(qwerty_persistence__incumbent_preservation_reading, qwerty_trained_typists, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(qwerty_persistence__incumbent_preservation_reading, qwerty_trained_typists, payer).

% Built curricula, certification tracks, and instructor expertise entirely around QWERTY. Have no incentive to teach or certify alternative layouts since doing so would fragment their credentialing market and require new instructor training. They actively lobby school boards and vocational bodies to keep QWERTY as the sole certified layout.
narrative_ontology:constraint_stakeholder(qwerty_persistence__incumbent_preservation_reading, typing_training_institutions, beneficiary,
    organized, generational, mobile, national).

% Software and hardware vendors whose products, defaults, and support infrastructure assume QWERTY. Benefit from not having to support layout-switching as a first-class feature and from the interoperability moat QWERTY ubiquity provides against new entrants.
narrative_ontology:constraint_stakeholder(qwerty_persistence__incumbent_preservation_reading, office_equipment_incumbents, beneficiary,
    institutional, generational, arbitrage, global).

% Individuals who adopt or wish to adopt a more efficient layout face incompatible shared hardware, employer typing tests calibrated to QWERTY, lack of institutional training support, and social friction ('why don't you just use normal keys'). They bear the switching cost alone with no coordinated support, and most revert or never switch.
narrative_ontology:constraint_stakeholder(qwerty_persistence__incumbent_preservation_reading, alternative_layout_adopters, payer,
    powerless, biographical, trapped, national).

% Professional and casual typists who would benefit from reduced finger travel and injury risk under an alternative layout but are structurally discouraged: certification exams, workplace keyboards, and shared devices all assume QWERTY, making the individually rational choice (switch) collectively unavailable.
narrative_ontology:constraint_stakeholder(qwerty_persistence__incumbent_preservation_reading, efficiency_seeking_typists, payer,
    powerless, biographical, constrained, national).

% Bear repetitive strain injury risk that some ergonomic research attributes partly to QWERTY's finger-travel and hand-alternation patterns. Cannot easily externalize the cost of switching (retraining time, workplace incompatibility) onto the manufacturers or institutions whose defense of the standard perpetuates the exposure.
narrative_ontology:constraint_stakeholder(qwerty_persistence__incumbent_preservation_reading, ergonomic_injury_sufferers, payer,
    powerless, biographical, trapped, national).

% Engineers and ergonomists who design and promote alternative layouts have no path to market: manufacturers won't tool for them at scale, training institutions won't certify them, and procurement specs exclude them by default. Their technical case is rarely tested against the incumbent within any institution with power to switch.
narrative_ontology:constraint_stakeholder(qwerty_persistence__incumbent_preservation_reading, new_entrant_layout_designers, excluded,
    powerless, biographical, trapped, global).

% Study the QWERTY case as the canonical path-dependence example, documenting the interplay of coordination benefit and defensive lock-in without a stake in the outcome.
narrative_ontology:constraint_stakeholder(qwerty_persistence__incumbent_preservation_reading, standards_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(qwerty_persistence__incumbent_preservation_reading, diffuse).
narrative_ontology:fixing_cost_class(qwerty_persistence__incumbent_preservation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: A shared keyboard layout lets any typist use any machine and any manufacturer sell to any market without layout-specific customization — a genuine, non-trivial coordination good.
% TRANSFER_FUNCTION: Moves the cost of layout-switching entirely onto individuals who would benefit from an alternative (retraining time, injury risk, social and institutional friction) while capital-holders (manufacturers, training institutions, incumbent software vendors) retain the value of their existing sunk investment without bearing any share of the foregone efficiency gains.
% ABSENT_VOICES: New entrant layout designers and the efficiency-seeking typists who would adopt their designs have no seat in the standards bodies, procurement committees, or curriculum boards that could authorize a shift; their technical case is documented in ergonomics literature but rarely reaches the institutions that could act on it.
% DISAPPEARANCE_RATIONALE: If QWERTY's defensive apparatus (manufacturer tooling defaults, training certification exclusivity, procurement lock-in) vanished overnight, hardware and training markets would fragment across competing layouts, some typists would retrain toward measurably lower injury risk and higher speed, and the current capital advantage held by QWERTY-committed manufacturers and institutions would evaporate — this is a constructed arrangement with identifiable capital-protection stakes, not a natural convergence.
% FOUNDING_PROBLEM: Early typewriter mechanics reportedly needed a layout that slowed typists enough to prevent mechanical key-jamming, and later, once mechanical constraints disappeared, a shared layout was needed so typists could move between machines and workplaces without retraining.
% FOUNDING_PROBLEM_CORROBORATION: Ergonomics researchers and typing-efficiency studies (outside the manufacturer and training-institution beneficiary set) attest that the original mechanical jam-prevention rationale disappeared with electric and digital keyboards decades ago, and that the coordination rationale (shared layout across machines) could equally be served by an alternative layout adopted at similar scale — the persistence is attested by these outside observers as capital-protection rather than continued necessity, though manufacturers and training bodies themselves maintain the coordination framing is still primary.
narrative_ontology:disappearance_verdict(qwerty_persistence__incumbent_preservation_reading, world_rearranges).
narrative_ontology:founding_problem_status(qwerty_persistence__incumbent_preservation_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(qwerty_persistence__incumbent_preservation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(qwerty_persistence__incumbent_preservation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(qwerty_persistence__incumbent_preservation_reading, 0.61, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(qwerty_persistence__incumbent_preservation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(qwerty_persistence__incumbent_preservation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(qwerty_persistence__incumbent_preservation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.61) reflects the gap between the coordination value genuinely produced (universal typing skill portability) and the cost imposed on those who would benefit from switching but cannot coordinate a transition alone. Suppression (0.58) captures the active defensive mechanisms — procurement specs, certification gatekeeping, pricing of alternative hardware — that go beyond passive network effects into deliberate incumbent defense. Theater ratio (0.42) is moderate-high: much of the 'this is just how typing works' framing performs naturalness over what this reading holds to be a constructed, actively defended arrangement. Accessibility collapse (0.72) is high because once QWERTY is embedded in training, procurement, and hardware defaults, the practical alternative set for an individual is nearly closed even though technically documented alternatives exist. Resistance (0.55) reflects the real but chronically under-resourced advocacy for alternative layouts.
 *
 * DIRECTIONALITY LOGIC:
 *   Manufacturers, training institutions, and the collective class of already-trained typists sit near the beneficiary end: they collect the coordination value of ubiquity while facing no switching cost themselves (the incumbents) or having already paid the switching cost once (the trained typists, who now benefit from not needing to pay it again). Alternative-layout adopters, efficiency seekers, injury sufferers, and new entrant designers sit near the target end: they bear the switching costs, the foregone efficiency, and the injury risk with no institutional path to socialize the cost of transition. The overlapping beneficiary/payer secondary role on qwerty_trained_typists reflects that this class benefits from ubiquity while simultaneously being locked out of adopting an efficiency gain for themselves — a genuine dual position, not a modeling convenience.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding (mechanical jam-prevention) problem is dead by this reading's own metrics but the coordination-value function (shared layout across machines) remains partially live — this is why the story is authored as tangled_rope rather than snare: there IS a real coordination good being produced, it is simply that its continuation now serves capital-protection more than the original engineering necessity, and a substantial victim class pays for that continuation without any of the original mechanical constraint applying to them.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    defense_vs_inertia_mechanism,
    'Is QWERTY''s persistence better explained by active incumbent defense (procurement lobbying, certification gatekeeping, deliberate pricing of alternatives) or by passive coordination inertia where no single actor defends the standard but switching costs alone prevent migration?',
    'Archival and lobbying-record analysis of standards bodies (ANSI, ISO keyboard working groups) and procurement specifications to determine whether alternative layouts were actively excluded by identifiable actor lobbying versus simply never proposed with sufficient coordinated backing to displace the incumbent.',
    'If active defense predominates, this reading''s tangled_rope classification with defensive suppression cost is supported. If passive inertia predominates, the sibling lapsed_alternatives_reading''s lower-ε rope account better describes the same surface phenomenon, and this story''s beneficiary defense framing overstates deliberate agency.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(defense_vs_inertia_mechanism, empirical, 'Whether QWERTY persistence is driven by active beneficiary defense or passive coordination inertia — the structural fork between this reading and its sibling.').

omega_variable(
    ergonomic_injury_causal_attribution,
    'How much repetitive strain injury risk is causally attributable to QWERTY''s specific finger-travel pattern versus general high-volume typing regardless of layout?',
    'Controlled longitudinal ergonomic studies comparing injury rates across matched typing-volume cohorts using QWERTY versus alternative layouts.',
    'If injury risk is substantially layout-attributable, the victim harm to ergonomic_injury_sufferers is more severe than a coordination-cost framing suggests, strengthening the extraction reading. If injury is mostly volume-driven and layout-independent, that victim category''s inclusion should be weakened or reframed.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(ergonomic_injury_causal_attribution, empirical, 'Whether ergonomic injury is a genuine QWERTY-specific harm or a layout-independent typing-volume effect.').

omega_variable(
    coordination_extraction_separability,
    'Is the coordination good (universal typing portability) separable from the specific QWERTY layout, or does switching costs of any universal standard necessarily reproduce similar incumbent-defense dynamics regardless of which layout became dominant?',
    'Comparative case study of standards transitions that DID succeed (e.g., metric conversion in some countries, screw-thread standardization) to identify what enabled successful transition despite incumbent sunk costs.',
    'If coordination is separable from QWERTY specifically (i.e., an alternative could have served the same coordination function with less injury/inefficiency), the extraction is more clearly attributable to this specific incumbent''s defense rather than to switching costs inherent to any standard. If not separable, some of the measured extraction is the unavoidable cost of maintaining ANY shared standard.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_extraction_separability, conceptual, 'Whether QWERTY''s specific extraction is separable from the general cost of standard-maintenance that any layout would carry.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(qwerty_persistence__incumbent_preservation_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qwer_tr_t0, qwerty_persistence__incumbent_preservation_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(qwer_tr_t20, qwerty_persistence__incumbent_preservation_reading, theater_ratio, 20, 0.22).
narrative_ontology:measurement(qwer_tr_t40, qwerty_persistence__incumbent_preservation_reading, theater_ratio, 40, 0.28).
narrative_ontology:measurement(qwer_tr_t60, qwerty_persistence__incumbent_preservation_reading, theater_ratio, 60, 0.33).
narrative_ontology:measurement(qwer_tr_t80, qwerty_persistence__incumbent_preservation_reading, theater_ratio, 80, 0.38).
narrative_ontology:measurement(qwer_tr_t100, qwerty_persistence__incumbent_preservation_reading, theater_ratio, 100, 0.42).

% Extraction over time
narrative_ontology:measurement(qwer_be_t0, qwerty_persistence__incumbent_preservation_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(qwer_be_t20, qwerty_persistence__incumbent_preservation_reading, base_extractiveness, 20, 0.42).
narrative_ontology:measurement(qwer_be_t40, qwerty_persistence__incumbent_preservation_reading, base_extractiveness, 40, 0.49).
narrative_ontology:measurement(qwer_be_t60, qwerty_persistence__incumbent_preservation_reading, base_extractiveness, 60, 0.55).
narrative_ontology:measurement(qwer_be_t80, qwerty_persistence__incumbent_preservation_reading, base_extractiveness, 80, 0.58).
narrative_ontology:measurement(qwer_be_t100, qwerty_persistence__incumbent_preservation_reading, base_extractiveness, 100, 0.61).

% Suppression requirement over time
narrative_ontology:measurement(qwer_su_t0, qwerty_persistence__incumbent_preservation_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(qwer_su_t20, qwerty_persistence__incumbent_preservation_reading, suppression_requirement, 20, 0.38).
narrative_ontology:measurement(qwer_su_t40, qwerty_persistence__incumbent_preservation_reading, suppression_requirement, 40, 0.45).
narrative_ontology:measurement(qwer_su_t60, qwerty_persistence__incumbent_preservation_reading, suppression_requirement, 60, 0.5).
narrative_ontology:measurement(qwer_su_t80, qwerty_persistence__incumbent_preservation_reading, suppression_requirement, 80, 0.55).
narrative_ontology:measurement(qwer_su_t100, qwerty_persistence__incumbent_preservation_reading, suppression_requirement, 100, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(qwerty_persistence__incumbent_preservation_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(qwerty_persistence__incumbent_preservation_reading, 0.1).
narrative_ontology:affects_constraint(qwerty_persistence__incumbent_preservation_reading, qwerty_persistence__lapsed_alternatives_reading).

% DUAL FORMULATION NOTE:
% This story and qwerty_persistence__lapsed_alternatives_reading are sibling readings of the qwerty_persistence kernel, not two measurements of one constraint. This reading (incumbent_preservation) authors ε=0.61 with an active defensive-suppression mechanism and a tangled_rope classification; the sibling reading authors substantially lower ε under a passive-inertia coordination account with no defensive apparatus, likely landing as rope or a much weaker tangled_rope. Both share the surface observable (QWERTY remains dominant) but attribute it to different causal mechanisms with different victim sets — this reading names alternative_layout_adopters, efficiency_seeking_typists, ergonomic_injury_sufferers, and new_entrant_layout_designers as victims of active defense; the lapsed_alternatives reading would have no comparable victim class since it holds no one is suppressed, merely that adoption thresholds were never met.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
