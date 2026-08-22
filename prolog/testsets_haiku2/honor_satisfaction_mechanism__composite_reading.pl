% ============================================================================
% CONSTRAINT STORY: honor_satisfaction_mechanism__composite_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_honor_satisfaction_mechanism__composite_reading, []).

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
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: honor_satisfaction_mechanism__composite_reading
 *   human_readable: Honor Satisfaction Mechanism (Composite: State Monopoly + Normative + Insurance + Recategorization)
 *   domain: legal/social/normative
 *
 * SUMMARY:
 *   The constraint governing honor satisfaction evolved through multiple
 *   simultaneous mechanisms rather than a single suppressive force. This
 *   composite reading argues that the honor-satisfaction mechanism (initially
 *   legitimizing dueling as the proper response to insult) eroded through:
 *   (1) state monopolization of legitimate violence (law criminalizes
 *   dueling, reserves justice-execution to the state); (2) bourgeois
 *   normative innovation (theater: formal insult protocols, ceremonial
 *   satisfaction without combat, honor codes that perform satisfaction
 *   theatrically); (3) insurance recalibration (commercial risk-shifting
 *   creates profit from dueling suppression); and (4) categorical recasting
 *   in law (dueling redefined as homicide, severing it from the moral
 *   framework that sustained it). These four mechanisms are not substitutes —
 *   they operate simultaneously and asymmetrically across social classes. The
 *   constraint is CLAIMED as tangled_rope (coordination of monopoly violence
 *   PLUS asymmetric extraction of honor from lower classes) while metrics
 *   reflect high suppression, moderate theater, and persistent resistance.
 *   The claim/metric independence principle applies: the claim is structural,
 *   the metrics are descriptive; divergence is the signal the engine
 *   measures.
 *
 * KEY AGENTS:
 *   - state_monopoly_enforcement: Controls criminal law, military discipline, capital punishment; monopolizes legitimate violence; benefits from elimination of dueling as rival authority
 *   - bourgeois_honor_keepers: Upper-class men who solve the honor tension through ceremonial theater; collect honor through recognition while law protects from prosecution
 *   - insurance_underwriters: Profit from dueling-suppression via lower mortality and higher premiums; risk-shifting incentivizes constraint enforcement
 *   - duelers: Identity-locked men (military, aristocrats, professionals) who face criminal liability or social death; most constrained by the composite mechanism
 *   - affected_families: Powerless, generationally impoverished by losses to dueling deaths and legal consequences; trapped by double suppression
 *   - working_classes: Doubly suppressed — no access to bourgeois theater and face harsher prosecution for honor-violence; excluded from coordination benefit
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_satisfaction_mechanism__composite_reading, 0.68).
domain_priors:suppression_score(honor_satisfaction_mechanism__composite_reading, 0.72).
domain_priors:theater_ratio(honor_satisfaction_mechanism__composite_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__composite_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__composite_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__composite_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__composite_reading, accessibility_collapse, 0.64).
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__composite_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_satisfaction_mechanism__composite_reading, tangled_rope).
narrative_ontology:human_readable(honor_satisfaction_mechanism__composite_reading, "Honor Satisfaction Mechanism (Composite: State Monopoly + Normative + Insurance + Recategorization)").
narrative_ontology:topic_domain(honor_satisfaction_mechanism__composite_reading, "legal/social/normative").

domain_priors:requires_active_enforcement(honor_satisfaction_mechanism__composite_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_satisfaction_mechanism__composite_reading, '4a4ce60c-dbef-44c0-8514-5f5a88d37793').
narrative_ontology:cs_kernel_codification('4a4ce60c-dbef-44c0-8514-5f5a88d37793', distributed).
narrative_ontology:cs_authority_grounding('4a4ce60c-dbef-44c0-8514-5f5a88d37793', extraction).
narrative_ontology:cs_interpretation_layer_present('4a4ce60c-dbef-44c0-8514-5f5a88d37793').
narrative_ontology:cs_reading_relation('4a4ce60c-dbef-44c0-8514-5f5a88d37793', honor_satisfaction_mechanism__contraction_reading, influences).
narrative_ontology:cs_reading_relation('4a4ce60c-dbef-44c0-8514-5f5a88d37793', honor_satisfaction_mechanism__decline_reading, coexists_with).
narrative_ontology:cs_axiom('4a4ce60c-dbef-44c0-8514-5f5a88d37793', foundational, honor_satisfiable_via_multiple_mechanisms).
narrative_ontology:cs_axiom_status(honor_satisfiable_via_multiple_mechanisms, holdable).
narrative_ontology:cs_axiom_grounding('4a4ce60c-dbef-44c0-8514-5f5a88d37793', honor_satisfiable_via_multiple_mechanisms, instrumental).
narrative_ontology:cs_axiom('4a4ce60c-dbef-44c0-8514-5f5a88d37793', secondary, mechanism_independence_required_for_persistence).
narrative_ontology:cs_axiom_status(mechanism_independence_required_for_persistence, holdable).
narrative_ontology:cs_axiom_grounding('4a4ce60c-dbef-44c0-8514-5f5a88d37793', mechanism_independence_required_for_persistence, empirically_contingent).
narrative_ontology:cs_reference_frame('4a4ce60c-dbef-44c0-8514-5f5a88d37793', honor_satisfaction_as_private_right).
narrative_ontology:cs_drift_state('4a4ce60c-dbef-44c0-8514-5f5a88d37793', industrial_rationalization_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('4a4ce60c-dbef-44c0-8514-5f5a88d37793', '').
narrative_ontology:cs_kernel_id(honor_satisfaction_mechanism__composite_reading, honor_satisfaction_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_satisfaction_mechanism__composite_reading, state_monopoly_enforcement).
narrative_ontology:constraint_beneficiary(honor_satisfaction_mechanism__composite_reading, bourgeois_honor_keepers).
narrative_ontology:constraint_beneficiary(honor_satisfaction_mechanism__composite_reading, insurance_underwriters).
narrative_ontology:constraint_victim(honor_satisfaction_mechanism__composite_reading, duelers).
narrative_ontology:constraint_victim(honor_satisfaction_mechanism__composite_reading, affected_families).
narrative_ontology:constraint_victim(honor_satisfaction_mechanism__composite_reading, working_classes).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(honor_satisfaction_mechanism__composite_reading, military_institutions).
narrative_ontology:constraint_victim(honor_satisfaction_mechanism__composite_reading, military_institutions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The state, particularly through legal and military apparatus, claims a monopoly on legitimate violence and honor satisfaction. Enforces laws against dueling while simultaneously defending its own honor through military action and capital punishment. Benefits from the contradiction: criminal law prosecutes duelers while state violence remains authorized as justice. Maintains the category-shift by defining dueling as criminal homicide rather than honorable satisfaction.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__composite_reading, state_monopoly_enforcement, agenda_setter,
    institutional, generational, analytical, national).

% Upper-class men whose honor status depends on adherence to dueling norms but who face prosecution if they duel. Solve the tension through theater: ceremonies of honor (formal insult protocols, seconds, code-of-honor invocation) that enact the honor satisfaction WITHOUT dueling. Collect honor through normative recognition while the constraint's suppression protects them from criminal liability. Dependent on widespread agreement that honor can be satisfied through ritual without lethal combat.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__composite_reading, bourgeois_honor_keepers, beneficiary,
    powerful, biographical, constrained, national).

% Insurance firms that profit from uncertainty about dueling mortality. As the constraint suppresses actual dueling, dueling-mortality insurance becomes lower-risk and more profitable (lower payout rate, higher premiums). Benefit from the constraint's success in shifting dueling into theater while holding formal policies that would still pay out if dueling occurred. Extract rents from the cognitive tension.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__composite_reading, insurance_underwriters, beneficiary,
    organized, biographical, mobile, regional).

% Men whose professional or social identity is constituted through honor and martial competence. Face a binding choice: renounce honor (professional/social death in some circles) or risk criminal prosecution. Duelers who engage pay with criminal liability, social exclusion from respectable society, or military discharge. Those who comply with the theater pay through subordination to bourgeois norms they may not endorse. Identity-locked because for many (military officers, aristocrats, professionals with reputation-based careers), exiting the honor framework means exiting the career itself.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__composite_reading, duelers, payer,
    moderate, biographical, identity_locked, national).

% Families of men killed in duels or imprisoned for dueling. Bear the generational cost: loss of breadwinner, legal liability, social stigma, economic impoverishment. Trapped because they neither set the honor norms nor control the legal framework; they carry the cost of the men's choices within both constraints simultaneously.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__composite_reading, affected_families, payer,
    powerless, generational, trapped, national).

% Working people have no access to the theater solution (bourgeois honor rituals) and face harsher prosecution for violence. If they fight over honor, they are prosecuted as common criminals; if they internalize the state's criminalization of honor satisfaction, they have no legal recourse for insult. Excluded from both the dueling norm AND the bourgeois theater. Trapped by double suppression: state law + class-based exclusion from the honor framework itself.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__composite_reading, working_classes, payer,
    powerless, biographical, trapped, national).

% Military maintains internal officer culture where honor and martial courage are foundational virtues. Simultaneously subordinate to state law and state monopoly on legitimate violence. Benefit from dueling suppression (reduces loss of trained officers, maintains discipline), but pay through the cognitive contradiction: official doctrine values honor while official law criminalizes honor satisfaction. Enforce the contradiction internally (secret dueling alongside court-martial for dueling).
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__composite_reading, military_institutions, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(honor_satisfaction_mechanism__composite_reading, military_institutions, payer).

% Judges, prosecutors, and legal scholars who prosecute dueling as criminal homicide while the category itself depends on normative acceptance (honor satisfaction as a distinct moral category) that law simultaneously denies. Enforce the category-shift by refusing to recognize dueling as a legal defense (honor is not a mitigating factor; the kill is homicide). Theater in doctrine: the law recategorizes murder as a technical matter, severing dueling from the moral framework that originally sustained it.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__composite_reading, legal_establishment, agenda_setter,
    institutional, generational, analytical, national).

% Regimes or cultures where dueling remains legal or normatively endorsed. Excluded from the reading's constraint system; their persistence is evidence of the constraint's contingency. Would constitute a competing frame in which honor satisfaction via dueling is coherent; their exclusion allows the composite constraint to operate.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__composite_reading, foreign_honor_systems, excluded,
    institutional, generational, trapped, global).

% External analyst tracking the constraint's operation across multiple mechanisms. Observes that the constraint's persistence depends simultaneously on state enforcement, bourgeois normative theater, insurance risk-shifting, and category-shift in law. Documents how the constraint operates differently across classes and institutions.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__composite_reading, sociological_observer, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(honor_satisfaction_mechanism__composite_reading, state_monopoly_enforcement).
narrative_ontology:fixing_cost_class(honor_satisfaction_mechanism__composite_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinate violence monopoly: redirect honor satisfaction from private dueling (decentralized, lethal, fragmenting authority) to state institutions and authorized substitutes (centralized, rule-governed, authority-preserving).
% TRANSFER_FUNCTION: Transfers authority over honor satisfaction from individuals and communities to the state and to bourgeois normative authority. Transfers risk (insurance firms shift dueling mortality risk to premiums). Transfers the capacity to respond to insult from the insulted person to courts, military discipline, or ceremonial protocol. Working classes experience extraction without transfer — their insult-response options collapse without a substitute.
% ABSENT_VOICES: Men from lower classes whose honor disputes have no legal recourse and cannot access bourgeois ceremonial theater. Military officers whose professional culture valorizes honor combat but whose institutional loyalty forbids it. Families of duelists whose generational costs are borne without representation in either state or bourgeois forums. Foreign regimes and non-Anglophone honor systems where dueling remains coherent and legal.
% DISAPPEARANCE_RATIONALE: The entire institutional architecture of state monopoly on legitimate violence, bourgeois honor theater, and insurance risk structures depends on the constraint. If it vanished, dueling would likely resume in aristocratic and military circles, state violence would need to assert its legitimacy through alternative means, and insurance would face sudden mortality spikes. The modern legal category of homicide itself presupposes that honor is not a legitimate defense — removing the constraint would require categorical legal reconstruction.
% FOUNDING_PROBLEM: Private dueling fragmented state authority, created unacceptable loss of trained personnel (especially military officers), and threatened the state monopoly on legitimate violence.
% FOUNDING_PROBLEM_CORROBORATION: State legal and military establishments attest the problem remains live — periodic dueling revivals occur when suppression weakens. Bourgeois historians and etiquette literature attest the problem is substantially solved through theater and social reorganization. Military historians document the transition from dueling culture to professional discipline (confirming the founding problem was acute). Families of executed duelists and working-class historians attest the problem was never solved for them — they experienced only criminalization without the coordination benefit. No corroboration from outside the state/bourgeois coalition supporting the constraint's founding framing.
narrative_ontology:disappearance_verdict(honor_satisfaction_mechanism__composite_reading, world_rearranges).
narrative_ontology:founding_problem_status(honor_satisfaction_mechanism__composite_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_satisfaction_mechanism__composite_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(honor_satisfaction_mechanism__composite_reading, 'none', 1).
narrative_ontology:epsilon_provenance(honor_satisfaction_mechanism__composite_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(honor_satisfaction_mechanism__composite_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(honor_satisfaction_mechanism__composite_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(honor_satisfaction_mechanism__composite_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The composite reading is extractive (0.68 at interval end) because the four mechanisms together suppress honor-satisfaction alternatives for most populations while creating escape routes (theater) only for the bourgeoisie. Suppression rises from 0.45 to 0.72 across the interval — modeling the hardening of enforcement infrastructure (both legal machinery and normative codification). Theater ratio rises from 0.25 to 0.58 — modeling the shift from actual dueling to ceremonial honor satisfaction in bourgeois circles; the ratio plateaus at 0.58 because theater never fully replaces the coordination function for identity-locked duelers (they experience theater as subordination to norms they may reject). Accessibility_collapse is leveled because the constraint operates differently at different scales: individual duelers face high collapse (nearly all alternatives blocked), organizational (military) faces moderate collapse (official doctrine vs. private culture), class (working) faces low collapse (they were already excluded; the constraint doesn't narrow further), structural (state monopoly) approaches totality. Resistance is high initially (0.68 individual) and declines as normative internalization occurs (0.52 by interval end) — modeling the transition from active resistance to internalized suppression. The plateau in base_extractiveness (0.65–0.68) after time point 15 reflects the constraint reaching its steady state: actual dueling becomes rare (no further suppression gains are achievable), but the extraction persists through theater and category-shift.
 *
 * PERSPECTIVAL GAP:
 *   The bourgeois beneficiary and the state monopoly setting see coordination (violence monopolization, honor alternative, risk management). The duelers and families see tangled_rope with heavy extraction. The working classes see snare — they have no coordination benefit and face harsher suppression. The legal establishment sees category-shift as neutral reclassification (dueling as homicide), not as extraction. The military institution sits between beneficiary and payer: benefits from officer retention via dueling suppression, but pays through the institutional contradiction (doctrine valorizes honor while discipline criminalizes honor satisfaction). The engine should compute these divergences from the structural data (power, exit, beneficiary/victim declarations) — the perpective gap is structural, not a matter of opinion.
 *
 * DIRECTIONALITY LOGIC:
 *   State monopoly and legal establishment have d near 0.0 (full beneficiary: control the rules, monopolize legitimate violence, extract capacity). Bourgeois honor keepers have d near 0.1–0.2 (net beneficiary: honor recognized via theater, protected from prosecution, but subordinate to state authority). Insurance underwriters have d near 0.15 (beneficiary: profit from suppression, though mobile enough to exit if dueling resumed). Duelers have d near 0.85–0.95 (near full target: identity-locked, criminalized, suppressed, offered only the subordinate alternative of theater). Affected families have d = 1.0 (full target: no benefit, total generational cost, trapped). Working classes have d near 0.9 (near full target: double suppression, excluded from coordination, no theater escape). Military institution has d near 0.55 (symmetric, pulled both ways: benefits from officer retention, pays through contradiction). Foreign honor systems and excluded voices: d = 1.0 from the reading's frame (they are barred from participating in the constraint). The directionality derivation chain flows from beneficiary/victim declarations + power + exit, confirmed by these qualitative d positions.
 *
 * MANDATROPHY ANALYSIS:
 *   The composite reading is mandatrophy-CONTESTED. The founding problem (private violence threatens state monopoly) was live at constraint inception and remains contested: the state attests it is still live (dueling would resume if suppression weakened); bourgeois society claims it is substantially solved via theater; families and working-class historians claim it was never solved for them. The disappearance_verdict (world_rearranges) confirms arrangements depend on the constraint. The constraint is NOT a piton (theater ratio stabilizes at 0.58, not crescendoing toward pure performance; suppression remains functional, not theatrical). The constraint is NOT a scaffold (no sunset clause; state monopoly on violence is intended as permanent). The constraint is tangled_rope: coordination of monopoly violence (real problem solved) PLUS asymmetric extraction (suppression of lower-class honor alternatives, forcing them into state-adjudicated justice with no theater escape). The mandatrophy window opens at the intersection of (status: contested, verdict: rearranges, extraction: high). A mandatrophy resolution would require either: (1) resolution of the founding problem dispute (empirical: does dueling resurge if suppression relaxes?), or (2) dissolution of the constraint (explicitly political: end state monopoly on violence, legalize dueling, or universalize theater to all classes). Neither has occurred; the reading is intentionally mandatrophy-unresolved.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    bourgeois_theater_vs_actual_suppression,
    'Is the ceremonial theater of honor satisfaction (formal insult protocols, seconds, codes) genuinely a substitute for dueling that satisfies the honor framework, or a cover story masking the suppression of genuine honor satisfaction?',
    'Ethnographic and historical evidence from bourgeois spaces where theater operates: do participants report that honor is genuinely satisfied through ceremony, or do they report subordination/internalization of suppression? Cross-cultural comparison: where theater mechanisms exist without legal suppression (e.g., aristocratic traditions), do actual duels still occur, or does theater substitute on its own merit?',
    'If theater is genuine substitute, the constraint is coordination (upper classes have exit via theater). If theater is a cover story, the constraint is snare for all classes — the bourgeoisie merely have more convincing propaganda. This distinction affects classification: rope vs. tangled_rope vs. snare depending on seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(bourgeois_theater_vs_actual_suppression, empirical, 'Whether bourgeois honor theater is genuine coordination or suppression theater.').

omega_variable(
    state_monopoly_vs_protection,
    'Does the state monopoly on legitimate violence protect citizens from private violence (coordination benefit), or does it primarily extract capacity to satisfy honor disputes according to private-justice logic (extraction mechanism)?',
    'Comparative legal history: in jurisdictions where dueling was decriminalized or de-enforced, did private violence increase, remain constant, or decrease? Did state courts experience increases in assault/battery petitions? Do victims in dueling-legal regimes seek state intervention or accept private settlement?',
    'If monopoly protects, it is coordination (all seats benefit from reduced private violence). If monopoly primarily extracts capacity, it is extraction (state collects the power to decide honor disputes, suppresses private alternatives). This affects whether the constraint is rope vs. tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_monopoly_vs_protection, empirical, 'Whether state monopoly primarily protects or extracts.').

omega_variable(
    category_shift_reversibility,
    'Is the legal recategorization of dueling from honorable satisfaction to criminal homicide reversible through legal amendment, or has it become so cognitively embedded that dueling could not be recriminalized without fundamental social reorganization?',
    'Historical test: have any jurisdictions successfully recriminalized dueling after decriminalization, or re-legalized it after full criminalization? Do legal scholars or judges treat dueling as a potential legal category, or as conceptually eliminated by modern law?',
    'If reversible, the constraint depends on continuous political enforcement of the category. If cognitively embedded, the category-shift is itself a constraint mechanism (contraction_reading territory). This affects whether the composite mechanisms are independent or whether category-shift is downstream of the others.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(category_shift_reversibility, conceptual, 'Whether legal category-shift is reversible or cognitive-level constraint.').

omega_variable(
    multiple_mechanisms_independence,
    'Are the four mechanisms (state monopoly, bourgeois theater, insurance, category-shift) structurally independent, or are they expressions of a single underlying shift (e.g., modernization, rationalization, commodification)?',
    'Causal analysis: which mechanisms came first historically? Do they reinforce each other, or could one persist without the others? Counterfactual: if state monopoly had been enforced without insurance incentives, would the constraint have held?',
    'If independent, the composite reading is correct — the constraint is fragile and could fail if any mechanism weakens. If dependent (all expressions of modernization), the constraint is more durable — attacking one mechanism does not loosen the others. This affects the reading''s claim that multiple mechanisms are necessary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(multiple_mechanisms_independence, empirical, 'Whether the four mechanisms are structurally independent or expressions of a unified process.').

omega_variable(
    working_class_suppression_mechanism,
    'Is working-class exclusion from the bourgeois honor theater itself a suppression mechanism (the constraint operating differently at different class levels), or are working-class suppression and bourgeois theater two separate constraints?',
    'Historical evidence: did working-class honor violence decline at the same rate as bourgeois dueling, or on a different timeline? Did working-class violence respond to legal suppression or to normative recategorization? Are working-class insult-violence and bourgeois dueling treated the same by courts?',
    'If same constraint operating differently: the composite mechanism is what holds the reading together, and class is a leveled-coercion axis. If separate constraints: the composite reading conflates two distinct mechanisms and should decompose into class-specific stories (ε-invariance principle). This affects whether the composite reading is coherent or should split.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(working_class_suppression_mechanism, empirical, 'Whether class-differentiated suppression is one constraint or multiple constraints.').

omega_variable(
    contraction_vs_composite_boundary,
    'At what point, if any, did the cognitive impossibility of dueling (contraction_reading) become operative? Is this a separate phase transition, or is it the endpoint of the composite mechanisms operating over sufficient time?',
    'Historiographical evidence: when did elite sources stop treating dueling as conceptually thinkable (even while illegal)? When did legal categories begin to treat honor-defense as incoherent rather than merely punishable? Do these transitions coincide with the implementation of the composite mechanisms, or do they precede or follow?',
    'If cognitive impossibility is the endpoint, the readings describe the same constraint at different timescales (composite describes mechanism, contraction describes outcome). If they are distinct processes, the constraint kernel has genuinely different readings that could coexist. This affects whether contraction_reading forecloses composite_reading or coexists_with it.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(contraction_vs_composite_boundary, conceptual, 'Whether contraction and composite mechanisms describe the same process or distinct constraint dynamics.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_satisfaction_mechanism__composite_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hono_tr_t0, honor_satisfaction_mechanism__composite_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(hono_tr_t0, observed).
narrative_ontology:measurement(hono_tr_t5, honor_satisfaction_mechanism__composite_reading, theater_ratio, 5, 0.35).
narrative_ontology:measurement_basis(hono_tr_t5, observed).
narrative_ontology:measurement(hono_tr_t10, honor_satisfaction_mechanism__composite_reading, theater_ratio, 10, 0.45).
narrative_ontology:measurement_basis(hono_tr_t10, observed).
narrative_ontology:measurement(hono_tr_t15, honor_satisfaction_mechanism__composite_reading, theater_ratio, 15, 0.52).
narrative_ontology:measurement_basis(hono_tr_t15, observed).
narrative_ontology:measurement(hono_tr_t20, honor_satisfaction_mechanism__composite_reading, theater_ratio, 20, 0.56).
narrative_ontology:measurement_basis(hono_tr_t20, observed).
narrative_ontology:measurement(hono_tr_t25, honor_satisfaction_mechanism__composite_reading, theater_ratio, 25, 0.58).
narrative_ontology:measurement_basis(hono_tr_t25, observed).
narrative_ontology:measurement(hono_tr_t30, honor_satisfaction_mechanism__composite_reading, theater_ratio, 30, 0.58).
narrative_ontology:measurement_basis(hono_tr_t30, observed).
narrative_ontology:measurement(hono_tr_t40, honor_satisfaction_mechanism__composite_reading, theater_ratio, 40, 0.58).
narrative_ontology:measurement_basis(hono_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(hono_be_t0, honor_satisfaction_mechanism__composite_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement_basis(hono_be_t0, observed).
narrative_ontology:measurement(hono_be_t5, honor_satisfaction_mechanism__composite_reading, base_extractiveness, 5, 0.58).
narrative_ontology:measurement_basis(hono_be_t5, observed).
narrative_ontology:measurement(hono_be_t10, honor_satisfaction_mechanism__composite_reading, base_extractiveness, 10, 0.63).
narrative_ontology:measurement_basis(hono_be_t10, observed).
narrative_ontology:measurement(hono_be_t15, honor_satisfaction_mechanism__composite_reading, base_extractiveness, 15, 0.65).
narrative_ontology:measurement_basis(hono_be_t15, observed).
narrative_ontology:measurement(hono_be_t20, honor_satisfaction_mechanism__composite_reading, base_extractiveness, 20, 0.67).
narrative_ontology:measurement_basis(hono_be_t20, observed).
narrative_ontology:measurement(hono_be_t25, honor_satisfaction_mechanism__composite_reading, base_extractiveness, 25, 0.68).
narrative_ontology:measurement_basis(hono_be_t25, observed).
narrative_ontology:measurement(hono_be_t30, honor_satisfaction_mechanism__composite_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement_basis(hono_be_t30, observed).
narrative_ontology:measurement(hono_be_t40, honor_satisfaction_mechanism__composite_reading, base_extractiveness, 40, 0.68).
narrative_ontology:measurement_basis(hono_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(hono_su_t0, honor_satisfaction_mechanism__composite_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement_basis(hono_su_t0, observed).
narrative_ontology:measurement(hono_su_t5, honor_satisfaction_mechanism__composite_reading, suppression_requirement, 5, 0.54).
narrative_ontology:measurement_basis(hono_su_t5, observed).
narrative_ontology:measurement(hono_su_t10, honor_satisfaction_mechanism__composite_reading, suppression_requirement, 10, 0.62).
narrative_ontology:measurement_basis(hono_su_t10, observed).
narrative_ontology:measurement(hono_su_t15, honor_satisfaction_mechanism__composite_reading, suppression_requirement, 15, 0.68).
narrative_ontology:measurement_basis(hono_su_t15, observed).
narrative_ontology:measurement(hono_su_t20, honor_satisfaction_mechanism__composite_reading, suppression_requirement, 20, 0.71).
narrative_ontology:measurement_basis(hono_su_t20, observed).
narrative_ontology:measurement(hono_su_t25, honor_satisfaction_mechanism__composite_reading, suppression_requirement, 25, 0.72).
narrative_ontology:measurement_basis(hono_su_t25, observed).
narrative_ontology:measurement(hono_su_t30, honor_satisfaction_mechanism__composite_reading, suppression_requirement, 30, 0.72).
narrative_ontology:measurement_basis(hono_su_t30, observed).
narrative_ontology:measurement(hono_su_t40, honor_satisfaction_mechanism__composite_reading, suppression_requirement, 40, 0.72).
narrative_ontology:measurement_basis(hono_su_t40, observed).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=0, tn=40
narrative_ontology:measurement(hono_grid_01, honor_satisfaction_mechanism__composite_reading, accessibility_collapse(class), 0, 0.38).
narrative_ontology:measurement(hono_grid_02, honor_satisfaction_mechanism__composite_reading, accessibility_collapse(class), 40, 0.42).
narrative_ontology:measurement(hono_grid_03, honor_satisfaction_mechanism__composite_reading, accessibility_collapse(individual), 0, 0.48).
narrative_ontology:measurement(hono_grid_04, honor_satisfaction_mechanism__composite_reading, accessibility_collapse(individual), 40, 0.68).
narrative_ontology:measurement(hono_grid_05, honor_satisfaction_mechanism__composite_reading, accessibility_collapse(organizational), 0, 0.55).
narrative_ontology:measurement(hono_grid_06, honor_satisfaction_mechanism__composite_reading, accessibility_collapse(organizational), 40, 0.72).
narrative_ontology:measurement(hono_grid_07, honor_satisfaction_mechanism__composite_reading, accessibility_collapse(structural), 0, 0.65).
narrative_ontology:measurement(hono_grid_08, honor_satisfaction_mechanism__composite_reading, accessibility_collapse(structural), 40, 0.78).
narrative_ontology:measurement(hono_grid_09, honor_satisfaction_mechanism__composite_reading, resistance(class), 0, 0.75).
narrative_ontology:measurement(hono_grid_10, honor_satisfaction_mechanism__composite_reading, resistance(class), 40, 0.68).
narrative_ontology:measurement(hono_grid_11, honor_satisfaction_mechanism__composite_reading, resistance(individual), 0, 0.68).
narrative_ontology:measurement(hono_grid_12, honor_satisfaction_mechanism__composite_reading, resistance(individual), 40, 0.52).
narrative_ontology:measurement(hono_grid_13, honor_satisfaction_mechanism__composite_reading, resistance(organizational), 0, 0.62).
narrative_ontology:measurement(hono_grid_14, honor_satisfaction_mechanism__composite_reading, resistance(organizational), 40, 0.42).
narrative_ontology:measurement(hono_grid_15, honor_satisfaction_mechanism__composite_reading, resistance(structural), 0, 0.58).
narrative_ontology:measurement(hono_grid_16, honor_satisfaction_mechanism__composite_reading, resistance(structural), 40, 0.38).
narrative_ontology:measurement(hono_grid_17, honor_satisfaction_mechanism__composite_reading, stakes_inflation(class), 0, 0.35).
narrative_ontology:measurement(hono_grid_18, honor_satisfaction_mechanism__composite_reading, stakes_inflation(class), 40, 0.38).
narrative_ontology:measurement(hono_grid_19, honor_satisfaction_mechanism__composite_reading, stakes_inflation(individual), 0, 0.42).
narrative_ontology:measurement(hono_grid_20, honor_satisfaction_mechanism__composite_reading, stakes_inflation(individual), 40, 0.62).
narrative_ontology:measurement(hono_grid_21, honor_satisfaction_mechanism__composite_reading, stakes_inflation(organizational), 0, 0.48).
narrative_ontology:measurement(hono_grid_22, honor_satisfaction_mechanism__composite_reading, stakes_inflation(organizational), 40, 0.68).
narrative_ontology:measurement(hono_grid_23, honor_satisfaction_mechanism__composite_reading, stakes_inflation(structural), 0, 0.55).
narrative_ontology:measurement(hono_grid_24, honor_satisfaction_mechanism__composite_reading, stakes_inflation(structural), 40, 0.72).
narrative_ontology:measurement(hono_grid_25, honor_satisfaction_mechanism__composite_reading, suppression(class), 0, 0.52).
narrative_ontology:measurement(hono_grid_26, honor_satisfaction_mechanism__composite_reading, suppression(class), 40, 0.78).
narrative_ontology:measurement(hono_grid_27, honor_satisfaction_mechanism__composite_reading, suppression(individual), 0, 0.4).
narrative_ontology:measurement(hono_grid_28, honor_satisfaction_mechanism__composite_reading, suppression(individual), 40, 0.65).
narrative_ontology:measurement(hono_grid_29, honor_satisfaction_mechanism__composite_reading, suppression(organizational), 0, 0.48).
narrative_ontology:measurement(hono_grid_30, honor_satisfaction_mechanism__composite_reading, suppression(organizational), 40, 0.72).
narrative_ontology:measurement(hono_grid_31, honor_satisfaction_mechanism__composite_reading, suppression(structural), 0, 0.45).
narrative_ontology:measurement(hono_grid_32, honor_satisfaction_mechanism__composite_reading, suppression(structural), 40, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(honor_satisfaction_mechanism__composite_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(honor_satisfaction_mechanism__composite_reading, 0.12).
narrative_ontology:affects_constraint(honor_satisfaction_mechanism__composite_reading, honor_satisfaction_mechanism__contraction_reading).
narrative_ontology:affects_constraint(honor_satisfaction_mechanism__composite_reading, honor_satisfaction_mechanism__decline_reading).

% DUAL FORMULATION NOTE:
% The honor_satisfaction_mechanism kernel has three constraint readings: composite (multiple independent mechanisms), contraction (cognitive impossibility), and decline (frequency attrition). The composite reading models simultaneous operation of state monopoly, normative theater, insurance incentives, and legal category-shift. The contraction reading models the cognitive-level endpoint where dueling becomes unthinkable as a category. The decline reading models persistent but declining frequency until fringe status. Each reading has a different ε, beneficiary/victim structure, and classification. They coexist in historiographical discourse; no single framework forecloses another. This story (composite) influences both siblings by modeling the mechanisms that could produce contraction (if all four operate long enough) or decline (if mechanisms are weaker than claimed).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
