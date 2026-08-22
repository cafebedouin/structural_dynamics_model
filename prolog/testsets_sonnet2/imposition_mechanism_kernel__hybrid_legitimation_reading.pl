% ============================================================================
% CONSTRAINT STORY: imposition_mechanism_kernel__hybrid_legitimation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: imposition_mechanism_kernel__hybrid_legitimation_reading
 *   human_readable: Hybrid Legitimation Reading of Norm Imposition (Imperial Example + Institutional Incentive)
 *   domain: historical_sociology/state_formation/cultural_authority
 *
 * SUMMARY:
 *   This story instantiates the hybrid legitimation reading of the
 *   imposition_mechanism_kernel: a new normative order that spreads neither
 *   through pure grassroots adoption nor through pure state coercion, but
 *   through the court's own example combined with institutional incentives
 *   that reward conformity. The emperor (or ruling dynasty) adopts a
 *   practice; court-adjacent elites follow quickly because following pays —
 *   offices, honors, precedence; the bureaucracy converts this into a durable
 *   incentive architecture (examinations, appointments, ritual calendars);
 *   and the norm diffuses outward and downward over one to two generations,
 *   reaching provincial gentry under status pressure and rural commoners
 *   last, transmitted rather than chosen. Enforcement exists but is targeted
 *   and moderate rather than pervasive, because symbolic alignment with the
 *   imperial example does most of the persuasive work among those who matter
 *   politically. This is a distinct constraint from its siblings: the
 *   endogenous_climb_reading (constraint_id: to be authored separately)
 *   asserts the norm was popularly adopted before state mandate, implying
 *   near-zero suppression and no imperial-example mechanism; the
 *   exogenous_override_reading asserts the norm was imposed by coercive
 *   monopoly on violence, implying much higher suppression and negligible
 *   legitimacy component. Each reading has a different epsilon and a
 *   different suppression profile because each describes a different causal
 *   claim about where legitimacy actually came from, not a different
 *   observable of the same claim.
 *
 * KEY AGENTS:
 *   - imperial_court: originates norm via personal/dynastic example, pairs it with institutional reward
 *   - early_adopting_elites: convert symbolic alignment into material advantage, become secondary transmitters
 *   - central_bureaucracy: administers the incentive architecture that makes conformity durable
 *   - provincial_gentry: adopt under status pressure without early-adopter rewards
 *   - rural_commoners: bear diffuse costs of the norm's downstream transmission with no voice and no exit
 *   - adherents_of_displaced_norm: lose standing as the new norm becomes the measure of legitimacy, largely absent from deliberation
 *   - court_historians: analytical observers whose record is filtered through court patronage
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(imposition_mechanism_kernel__hybrid_legitimation_reading, 0.52).
domain_priors:suppression_score(imposition_mechanism_kernel__hybrid_legitimation_reading, 0.44).
domain_priors:theater_ratio(imposition_mechanism_kernel__hybrid_legitimation_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(imposition_mechanism_kernel__hybrid_legitimation_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(imposition_mechanism_kernel__hybrid_legitimation_reading, suppression_requirement, 0.44).
narrative_ontology:constraint_metric(imposition_mechanism_kernel__hybrid_legitimation_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(imposition_mechanism_kernel__hybrid_legitimation_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(imposition_mechanism_kernel__hybrid_legitimation_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(imposition_mechanism_kernel__hybrid_legitimation_reading, tangled_rope).
narrative_ontology:human_readable(imposition_mechanism_kernel__hybrid_legitimation_reading, "Hybrid Legitimation Reading of Norm Imposition (Imperial Example + Institutional Incentive)").
narrative_ontology:topic_domain(imposition_mechanism_kernel__hybrid_legitimation_reading, "historical_sociology/state_formation/cultural_authority").

domain_priors:requires_active_enforcement(imposition_mechanism_kernel__hybrid_legitimation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(imposition_mechanism_kernel__hybrid_legitimation_reading, '5d8220ed-6ae4-407f-973a-b915f1ac0db1').
narrative_ontology:cs_kernel_codification('5d8220ed-6ae4-407f-973a-b915f1ac0db1', distributed).
narrative_ontology:cs_authority_grounding('5d8220ed-6ae4-407f-973a-b915f1ac0db1', lineage).
narrative_ontology:cs_interpretation_layer_present('5d8220ed-6ae4-407f-973a-b915f1ac0db1').
narrative_ontology:cs_reading_relation('5d8220ed-6ae4-407f-973a-b915f1ac0db1', imposition_mechanism_kernel__endogenous_climb_reading, coexists_with).
narrative_ontology:cs_reading_relation('5d8220ed-6ae4-407f-973a-b915f1ac0db1', imposition_mechanism_kernel__exogenous_override_reading, coexists_with).
narrative_ontology:cs_axiom('5d8220ed-6ae4-407f-973a-b915f1ac0db1', foundational, legitimacy_transfers_through_exemplary_symbolic_action).
narrative_ontology:cs_axiom_status(legitimacy_transfers_through_exemplary_symbolic_action, holdable).
narrative_ontology:cs_axiom_grounding('5d8220ed-6ae4-407f-973a-b915f1ac0db1', legitimacy_transfers_through_exemplary_symbolic_action, conventional).
narrative_ontology:cs_axiom('5d8220ed-6ae4-407f-973a-b915f1ac0db1', secondary, institutional_incentive_is_necessary_complement_to_symbolic_authority).
narrative_ontology:cs_axiom_status(institutional_incentive_is_necessary_complement_to_symbolic_authority, holdable).
narrative_ontology:cs_axiom_grounding('5d8220ed-6ae4-407f-973a-b915f1ac0db1', institutional_incentive_is_necessary_complement_to_symbolic_authority, instrumental).
narrative_ontology:cs_reference_frame('5d8220ed-6ae4-407f-973a-b915f1ac0db1', imperial_exemplarity_as_normative_anchor).
narrative_ontology:cs_drift_state('5d8220ed-6ae4-407f-973a-b915f1ac0db1', post_dynastic_consolidation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('5d8220ed-6ae4-407f-973a-b915f1ac0db1', '').
narrative_ontology:cs_kernel_id(imposition_mechanism_kernel__hybrid_legitimation_reading, imposition_mechanism_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(imposition_mechanism_kernel__hybrid_legitimation_reading, imperial_court).
narrative_ontology:constraint_beneficiary(imposition_mechanism_kernel__hybrid_legitimation_reading, early_adopting_elites).
narrative_ontology:constraint_beneficiary(imposition_mechanism_kernel__hybrid_legitimation_reading, central_bureaucracy).
narrative_ontology:constraint_victim(imposition_mechanism_kernel__hybrid_legitimation_reading, provincial_gentry).
narrative_ontology:constraint_victim(imposition_mechanism_kernel__hybrid_legitimation_reading, rural_commoners).
narrative_ontology:constraint_victim(imposition_mechanism_kernel__hybrid_legitimation_reading, adherents_of_displaced_norm).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Originates the new norm by personal or dynastic example — the emperor's own practice becomes the template others are invited, then expected, to follow. Pairs the symbolic gesture with concrete institutional incentives (appointments, honors, tax treatment, ritual precedence) that reward conformity. Does not need to coerce broadly because the symbolic weight of imperial example does much of the persuasive work; enforcement is reserved for visible holdouts among those who matter politically.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__hybrid_legitimation_reading, imperial_court, agenda_setter,
    institutional, generational, analytical, national).

% Court-adjacent families and officials who adopt the new norm quickly, converting symbolic alignment with the emperor into real advantage — office, marriage alliances, precedence at court. They become secondary transmitters, modeling the norm downward to the provinces, and gain disproportionately from being early rather than compelled.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__hybrid_legitimation_reading, early_adopting_elites, beneficiary,
    powerful, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(imposition_mechanism_kernel__hybrid_legitimation_reading, early_adopting_elites, agenda_setter).

% Administers the institutional incentive structure — appointments, examinations, ritual calendars — that converts imperial example into durable practice. Benefits from a legible, standardized elite whose conformity is easier to administer and audit than a fragmented status landscape.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__hybrid_legitimation_reading, central_bureaucracy, beneficiary,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(imposition_mechanism_kernel__hybrid_legitimation_reading, central_bureaucracy, agenda_setter).

% Face pressure to adopt the new norm years after the court does, without direct access to the recognition and institutional rewards that made early adoption profitable for court elites. Adoption costs them established local status markers; refusal costs them access to the bureaucratic ladder now calibrated to the new norm. Their exit is constrained by dependence on state-recognized status for standing.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__hybrid_legitimation_reading, provincial_gentry, payer,
    moderate, biographical, constrained, regional).

% Encounter the new norm last, transmitted through local elites and officials rather than directly from the court. Bear diffuse costs of disruption to prior practice — altered ritual obligations, changed labor or tax calendars tied to the new norm — without meaningful voice in its adoption and with essentially no exit from the jurisdiction that enforces it.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__hybrid_legitimation_reading, rural_commoners, payer,
    powerless, generational, trapped, local).

% Practitioners and beneficiaries of the norm being displaced — often a rival lineage tradition, regional custom, or older ritual order — who lose standing as the new imperially-modeled norm becomes the measure of legitimacy. Not consulted in the transition; their objections surface mainly in local resistance and later historical record, not in the court's deliberations.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__hybrid_legitimation_reading, adherents_of_displaced_norm, excluded,
    powerless, biographical, trapped, regional).

% Record the transition, often after the fact, and their accounts become the primary evidence later scholars use to adjudicate whether the norm's legitimacy came from popular embrace, imperial charisma, or coercion. Their access is filtered through court patronage, biasing the surviving record toward the hybrid legitimation narrative.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__hybrid_legitimation_reading, court_historians, observer,
    analytical, civilizational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a single, legible status hierarchy across a large and otherwise heterogeneous realm, reducing the coordination cost of determining who counts as legitimate, respectable, or eligible for office by anchoring status to a single visible template (the emperor's example) rather than to competing local standards.
% TRANSFER_FUNCTION: Moves status recognition, office access, and ritual precedence from adherents of the older, displaced norm and from slow-adopting provincial and rural populations toward the court and the elites who align early with imperial example; institutional rewards (appointments, honors, tax treatment) are the concrete channel through which symbolic alignment converts into material advantage.
% ABSENT_VOICES: Adherents of the displaced norm and rural commoners are structurally absent from the court's deliberation; their objections, where they surface, appear as regional friction or later historiographic dissent rather than as input that shaped the norm's design.
% DISAPPEARANCE_RATIONALE: Court elites and the bureaucracy would argue the realm's status order would fragment without the imperially-anchored standard, reverting to competing local hierarchies (world_rearranges from their seat). Adherents of the displaced norm and much of the rural population would argue the underlying status competition would simply resume its prior, locally-legitimate form — the imperial overlay is what disappears, not the deeper social structure (world_unchanged from their seat). The verdict genuinely depends on whose baseline is taken as the counterfactual.
% FOUNDING_PROBLEM: A newly consolidated or newly ambitious court needed a mechanism to standardize status and conduct across a realm too large and heterogeneous for either pure custom or pure coercion to govern efficiently — imperial example paired with institutional reward offered a lower-cost path than either grassroots consensus-building or continuous military enforcement.
% FOUNDING_PROBLEM_CORROBORATION: Court historians (writing under court patronage) attest the norm solved a live coordination problem and continues to do so generations later. Independent testimony is thin: regional chronicles and later ethnographic-style accounts from displaced-norm regions describe the same period as one of externally imposed status change with no coordination benefit visible from their vantage, suggesting the 'solved problem' framing is substantially a court-side narrative rather than a corroborated external fact.
narrative_ontology:disappearance_verdict(imposition_mechanism_kernel__hybrid_legitimation_reading, contested).
narrative_ontology:founding_problem_status(imposition_mechanism_kernel__hybrid_legitimation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(imposition_mechanism_kernel__hybrid_legitimation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(imposition_mechanism_kernel__hybrid_legitimation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(imposition_mechanism_kernel__hybrid_legitimation_reading, 0.52, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is authored at a moderate 0.52 — lower than a pure coercive override would warrant, because a genuine coordination function (a legible, realm-wide status standard) is present and delivers real benefit to early adopters and the bureaucracy. But it is well above a pure-rope reading because status and material rewards are structurally transferred away from adherents of the displaced norm and from slow-adopting rural populations who never consented to the change. Suppression is authored at 0.44 and falls slightly over the interval (0.50 to 0.44) as the norm normalizes and imperial example alone becomes sufficient to sustain compliance among elites, reducing the need for active enforcement — enforcement is front-loaded against early, visible holdouts and tapers as institutional incentives take over the maintenance work. Theater ratio rises modestly (0.20 to 0.40) as the norm ages: ritual performance of conformity (court ceremony, examination formalism) increasingly substitutes for the original substantive signaling function. All three metrics are authored on one shared time grid.
 *
 * DIRECTIONALITY LOGIC:
 *   The imperial court and central bureaucracy sit at the beneficiary/agenda-setter end: they design the incentive structure and collect its coordination benefits (a governable, legible status order) without bearing its transitional costs. Early-adopting elites are structural beneficiaries who also act as secondary agenda-setters, converting proximity to the court into real advantage — this is the clearest coordination-plus-extraction hybrid signature. Provincial gentry and rural commoners are targets: they bear the disruption cost of the new norm without the early-mover rewards, and their exit options (constrained, trapped) push their effective extraction toward the target end even though the base extractiveness figure is moderate. Adherents of the displaced norm are the clearest victims — they lose standing outright and are structurally excluded from the deliberation that produced the change.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (coordinating status and conduct across a large, heterogeneous realm) plausibly remains partly live — large states still face this coordination problem — which argues against calling this pure mandatrophy. But the corroboration is thin: attestation comes overwhelmingly from court-patronized historians, and the diffuse, generations-later costs borne by rural commoners and displaced-norm adherents are never weighed against the coordination benefit in the surviving record. This is exactly the situation the tangled_rope classification exists to hold: a real coordination function and a real asymmetric extraction riding on the same structure, neither canceling the other.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legitimacy_source_attribution,
    'Did the norm''s spread actually derive its legitimacy from imperial symbolic authority, or is the hybrid narrative a court-historiographic gloss over what was substantively coercive imposition, popular adoption, or both operating with imperial example as mere pretext?',
    'Comparative analysis of adoption timing and enforcement records across regions with differing proximity to court patronage, cross-checked against non-court chronicles and material/archaeological evidence of practice change independent of the official record.',
    'If the hybrid mechanism is substantiated by independent evidence, this reading''s moderate extractiveness and moderate-declining suppression profile hold. If independent evidence instead shows either negligible enforcement (favoring endogenous_climb) or pervasive coercion (favoring exogenous_override), this story''s epsilon and suppression values would need revision — as a distinct constraint, not as a correction to this one.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_source_attribution, empirical, 'Whether the hybrid legitimation mechanism is independently corroborated or primarily a court-side narrative.').

omega_variable(
    stratified_adoption_as_extraction_signal,
    'Is the elites-first, masses-later adoption pattern evidence of a genuine diffusion process (coordination taking time to reach the periphery) or evidence that the incentive structure was designed to reward early insiders at the structural expense of latecomers (extraction disguised as diffusion lag)?',
    'Examine whether institutional rewards for early adoption (offices, honors) were fixed-sum and therefore necessarily depleted for later adopters, versus expandable and available to all eventual adopters on the same terms.',
    'A fixed-sum reward structure would sharpen the tangled_rope reading toward greater extraction (early adopters capture scarce rewards latecomers cannot access); an expandable structure would support a milder coordination-dominant reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(stratified_adoption_as_extraction_signal, conceptual, 'Whether stratified adoption reflects diffusion lag or designed scarcity in reward allocation.').

omega_variable(
    cs_framing_kernel_vs_dynasty,
    'Should the commitment-system kernel here be framed as the imperial court''s authority to set normative example (the obvious framing), or as the deeper legitimacy claim that dynastic charisma itself rests on — the Mandate-style or cosmological warrant that makes imperial example authoritative in the first place?',
    'Compare classification outcomes under both framings: does treating the deeper cosmological/dynastic warrant as the kernel change the authority_grounding from lineage to something closer to theological, and does that shift the drift_state analysis?',
    'Under the court-authority framing (adopted here), authority_grounding is lineage and drift concerns institutional durability. Under the deeper-warrant framing, authority_grounding would shift toward theological or a mixed lineage/theological ground, and the drift_state would need to track erosion of the cosmological warrant itself rather than institutional practice — a materially different analysis.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cs_framing_kernel_vs_dynasty, conceptual, 'Alternative kernel framings (court authority vs. underlying dynastic/cosmological warrant) that would shift the CS classification.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(imposition_mechanism_kernel__hybrid_legitimation_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(impo_tr_t0, imposition_mechanism_kernel__hybrid_legitimation_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(impo_tr_t10, imposition_mechanism_kernel__hybrid_legitimation_reading, theater_ratio, 10, 0.25).
narrative_ontology:measurement(impo_tr_t20, imposition_mechanism_kernel__hybrid_legitimation_reading, theater_ratio, 20, 0.3).
narrative_ontology:measurement(impo_tr_t30, imposition_mechanism_kernel__hybrid_legitimation_reading, theater_ratio, 30, 0.34).
narrative_ontology:measurement(impo_tr_t40, imposition_mechanism_kernel__hybrid_legitimation_reading, theater_ratio, 40, 0.37).
narrative_ontology:measurement(impo_tr_t50, imposition_mechanism_kernel__hybrid_legitimation_reading, theater_ratio, 50, 0.39).
narrative_ontology:measurement(impo_tr_t60, imposition_mechanism_kernel__hybrid_legitimation_reading, theater_ratio, 60, 0.4).

% Extraction over time
narrative_ontology:measurement(impo_be_t0, imposition_mechanism_kernel__hybrid_legitimation_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(impo_be_t10, imposition_mechanism_kernel__hybrid_legitimation_reading, base_extractiveness, 10, 0.36).
narrative_ontology:measurement(impo_be_t20, imposition_mechanism_kernel__hybrid_legitimation_reading, base_extractiveness, 20, 0.42).
narrative_ontology:measurement(impo_be_t30, imposition_mechanism_kernel__hybrid_legitimation_reading, base_extractiveness, 30, 0.46).
narrative_ontology:measurement(impo_be_t40, imposition_mechanism_kernel__hybrid_legitimation_reading, base_extractiveness, 40, 0.49).
narrative_ontology:measurement(impo_be_t50, imposition_mechanism_kernel__hybrid_legitimation_reading, base_extractiveness, 50, 0.51).
narrative_ontology:measurement(impo_be_t60, imposition_mechanism_kernel__hybrid_legitimation_reading, base_extractiveness, 60, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(impo_su_t0, imposition_mechanism_kernel__hybrid_legitimation_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(impo_su_t10, imposition_mechanism_kernel__hybrid_legitimation_reading, suppression_requirement, 10, 0.48).
narrative_ontology:measurement(impo_su_t20, imposition_mechanism_kernel__hybrid_legitimation_reading, suppression_requirement, 20, 0.46).
narrative_ontology:measurement(impo_su_t30, imposition_mechanism_kernel__hybrid_legitimation_reading, suppression_requirement, 30, 0.45).
narrative_ontology:measurement(impo_su_t40, imposition_mechanism_kernel__hybrid_legitimation_reading, suppression_requirement, 40, 0.44).
narrative_ontology:measurement(impo_su_t50, imposition_mechanism_kernel__hybrid_legitimation_reading, suppression_requirement, 50, 0.44).
narrative_ontology:measurement(impo_su_t60, imposition_mechanism_kernel__hybrid_legitimation_reading, suppression_requirement, 60, 0.44).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(imposition_mechanism_kernel__hybrid_legitimation_reading, identity_coordination).
narrative_ontology:affects_constraint(imposition_mechanism_kernel__hybrid_legitimation_reading, imposition_mechanism_kernel__endogenous_climb_reading).
narrative_ontology:affects_constraint(imposition_mechanism_kernel__hybrid_legitimation_reading, imposition_mechanism_kernel__exogenous_override_reading).

% DUAL FORMULATION NOTE:
% This story is one of three siblings decomposing the natural-language claim 'the new norm achieved legitimacy' under the imposition_mechanism_kernel. The endogenous_climb_reading asserts near-zero suppression and bottom-up causation (epsilon should be authored low, coordination-dominant). The exogenous_override_reading asserts high suppression and coercive causation (epsilon should be authored high, extraction-dominant). This hybrid_legitimation_reading occupies the middle: moderate epsilon (0.52), moderate-declining suppression (0.44), and a distinctive stratified-adoption signature (elites first, masses last) that neither sibling reading predicts. The three are linked via affects_constraints because they compete for explanatory primacy over the same historical episode; strengthening the evidentiary case for one reading structurally weakens the others' claim to be the operative mechanism.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
