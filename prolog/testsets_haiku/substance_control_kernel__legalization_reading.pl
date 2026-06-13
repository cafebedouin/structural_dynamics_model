% ============================================================================
% CONSTRAINT STORY: substance_control_kernel__legalization_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_substance_control_kernel__legalization_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: substance_control_kernel__legalization_reading
 *   human_readable: Legalization Reading: Substance Use as Individual Liberty with Externality Capture
 *   domain: public_health/criminal_justice/political_economy
 *
 * SUMMARY:
 *   This constraint embodies the legalization reading of the
 *   substance-control kernel: substance use is framed as an individual
 *   liberty issue, state intervention is justified only by preventing
 *   third-party harm and capturing externality costs, and criminal
 *   enforcement is replaced by market regulation and tax collection. This
 *   reading stands in structural opposition to the prohibition reading (which
 *   treats use as moral transgression requiring punishment) and in partial
 *   tension with the harm-reduction reading (which prioritizes pragmatic
 *   intervention independent of legalization status). The kernel contest is
 *   over what problem substance use IS — a liberty violation, a health
 *   condition, or a moral transgression — and that framing determines who
 *   bears costs and who benefits. Under legalization, users transition from
 *   the victim set (criminalized) to the beneficiary set (decriminalized);
 *   the black market is displaced; a legal industry emerges; and externality
 *   costs are transferred to third parties and health infrastructure. The
 *   claimed type is rope because the reading emphasizes genuine coordination
 *   (regulated market, supply safety, known prices) over extraction, though
 *   the measurement series reveal rising extraction as the legal industry
 *   consolidates and externality costs exceed tax capture. The claim/metric
 *   gap is intentional: it marks the space between the reading's core
 *   justification (coordination and liberty) and its actual operation (market
 *   concentration and incomplete externality capture).
 *
 * KEY AGENTS:
 *   - substance_users: transition from criminal-enforcement victims to market participants; exit the victim set
 *   - legal_substance_industry: created by legalization; becomes beneficiary and agenda-setter; consolidates power
 *   - state_tax_collector: new beneficiary; captures revenue; interest in maximizing legal market size
 *   - regulatory_enforcement_apparatus: transitions from criminal to administrative function; remains beneficiary
 *   - third_party_externality_bearers: new victim set; bear costs of impairment, secondhand exposure, overdose response
 *   - adjacent_communities: spatially concentrated victims; bear retail clustering and consumption-related harms
 *   - public_health_infrastructure: victim of unmet demand; tax revenue promises exceed actual funding
 *   - prohibition_advocates: structurally excluded; their core premise (moral transgression requiring punishment) is foreclosed by legalization's axiom
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(substance_control_kernel__legalization_reading, 0.61).
domain_priors:suppression_score(substance_control_kernel__legalization_reading, 0.34).
domain_priors:theater_ratio(substance_control_kernel__legalization_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(substance_control_kernel__legalization_reading, extractiveness, 0.61).
narrative_ontology:constraint_metric(substance_control_kernel__legalization_reading, suppression_requirement, 0.34).
narrative_ontology:constraint_metric(substance_control_kernel__legalization_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(substance_control_kernel__legalization_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(substance_control_kernel__legalization_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(substance_control_kernel__legalization_reading, rope).
narrative_ontology:human_readable(substance_control_kernel__legalization_reading, "Legalization Reading: Substance Use as Individual Liberty with Externality Capture").
narrative_ontology:topic_domain(substance_control_kernel__legalization_reading, "public_health/criminal_justice/political_economy").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(substance_control_kernel__legalization_reading, '6f2d24a3-fa25-43c6-a869-ba65432f7932').
narrative_ontology:cs_kernel_codification('6f2d24a3-fa25-43c6-a869-ba65432f7932', distributed).
narrative_ontology:cs_authority_grounding('6f2d24a3-fa25-43c6-a869-ba65432f7932', distributed).
narrative_ontology:cs_reading_relation('6f2d24a3-fa25-43c6-a869-ba65432f7932', substance_control_kernel__prohibition_reading, forecloses).
narrative_ontology:cs_reading_relation('6f2d24a3-fa25-43c6-a869-ba65432f7932', substance_control_kernel__harm_reduction_reading, coexists_with).
narrative_ontology:cs_axiom('6f2d24a3-fa25-43c6-a869-ba65432f7932', foundational, individual_autonomy_over_substance_choice).
narrative_ontology:cs_axiom_status(individual_autonomy_over_substance_choice, holdable).
narrative_ontology:cs_axiom_grounding('6f2d24a3-fa25-43c6-a869-ba65432f7932', individual_autonomy_over_substance_choice, deontological).
narrative_ontology:cs_axiom('6f2d24a3-fa25-43c6-a869-ba65432f7932', foundational, state_intervention_justified_only_by_externality).
narrative_ontology:cs_axiom_status(state_intervention_justified_only_by_externality, holdable).
narrative_ontology:cs_axiom_grounding('6f2d24a3-fa25-43c6-a869-ba65432f7932', state_intervention_justified_only_by_externality, instrumental).
narrative_ontology:cs_reference_frame('6f2d24a3-fa25-43c6-a869-ba65432f7932', individual_liberty_externality_framework).
narrative_ontology:cs_drift_state('6f2d24a3-fa25-43c6-a869-ba65432f7932', contemporary_consolidation_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('6f2d24a3-fa25-43c6-a869-ba65432f7932', '').
narrative_ontology:cs_kernel_id(substance_control_kernel__legalization_reading, substance_control_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(substance_control_kernel__legalization_reading, legal_substance_industry).
narrative_ontology:constraint_beneficiary(substance_control_kernel__legalization_reading, state_tax_collector).
narrative_ontology:constraint_beneficiary(substance_control_kernel__legalization_reading, regulatory_enforcement_apparatus).
narrative_ontology:constraint_beneficiary(substance_control_kernel__legalization_reading, substance_users).
narrative_ontology:constraint_victim(substance_control_kernel__legalization_reading, third_party_externality_bearers).
narrative_ontology:constraint_victim(substance_control_kernel__legalization_reading, adjacent_communities).
narrative_ontology:constraint_victim(substance_control_kernel__legalization_reading, public_health_infrastructure).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(substance_control_kernel__legalization_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(substance_control_kernel__legalization_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(substance_control_kernel__legalization_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(substance_control_kernel__legalization_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(substance_control_kernel__legalization_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness begins at 0.38 (early legalization emphasizes liberty and coordination functions) and rises to 0.61 by interval end (legal industry consolidates, externality costs outpace tax capture, price floors serve state revenue more than harm reduction). Suppression requirement falls from 0.65 to 0.34 as the constraint shifts from criminal enforcement (high suppression of users and producers) to administrative regulation (lower suppression, compliance through licensing rather than incarceration). Theater ratio rises from 0.12 to 0.28 as the regulatory apparatus increasingly performs legitimacy maintenance (testing, labeling) while industry lobbying reshapes rules to maximize profit rather than public health. The measurement series track the lifecycle dynamics: early legalization reduces suppression and extraction by decriminalizing users; over time, the legal industry's market power rises, regulatory theater increases to manage public-health image, and externality costs (third-party harm) become the dominant extraction mechanism. The shared time grid ensures every metric is authored at every examined point, preventing the misalignment that would date transitions early.
 *
 * PERSPECTIVAL GAP:
 *   From the user's seat, legalization is decriminalization — a rope coordinating safe supply at the cost of market prices and modest regulation. From the third-party-externality seat, legalization is a constraint that permits others' choices to externalize costs onto them with minimal recourse — closer to a snare where the externality-bearing is the hidden transfer. From the industry's seat, legalization is a market opportunity constrained only by regulatory capture and pricing power — extraction through consolidation. From the prohibition-advocate's seat (excluded), legalization forecloses their entire framework by accepting individual autonomy over use. The engine computes each seat's type independently from power, exit, and beneficiary/victim data; the authored claim (rope) reflects one seat's (the user's) framing, while the metrics describe the overall operation (rising extraction, falling suppression, rising theater).
 *
 * DIRECTIONALITY LOGIC:
 *   Users are beneficiaries with moderate power and mobile exit: decriminalization is a decisive benefit that requires no ongoing enforcement against them. Legal industry holds powerful institutional position with arbitrage exit: they shape rules and capture rents. State as agenda-setter and tax collector is institutionally positioned with analytical exit: it collects revenue and distributes narrative authority. Third parties are victims with powerless or organized positions and trapped/constrained exit: they bear impairment, exposure, and overdose costs they did not choose and cannot escape by individual decision. This asymmetry is structural to the legalization reading: it moves users from victim to beneficiary, moves third parties into the victim set, and creates a new institutional beneficiary (the legal industry and state apparatus). No directionality overrides are required; the derivation from beneficiary/victim declarations and exit options captures the asymmetry directly.
 *
 * MANDATROPHY ANALYSIS:
 *   The legalization reading avoids mandatrophy confusion by clearly anchoring its mandate to two distinct problems: (1) the liberty violation of criminalizing users (solved by decriminalization), and (2) the externality problem (captured by tax and regulation). Where the constraint's operation diverges from this mandate — where extraction rises without proportional public-health investment, where regulatory theater masks industry lobbying, where third-party harms exceed tax capture — the reading's own internal logic predicts its classification should shift toward snare. The measurements capture this drift: suppression falls (mandate 1 is satisfied — users are decriminalized), but extractiveness rises (mandate 2 is increasingly unmet — externalities exceed recovery). A piton reading would emerge if legalization persisted as pure theater (high theater_ratio, no actual coordination or externality capture) while enforcement capacity atrophied; this reading shows active enforcement and rising industry consolidation, so piton is not yet the classification. The rising extraction and theater suggest a tangled_rope trajectory if the legal industry consolidates political power without proportional externality capture.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    externality_capture_completeness,
    'Does the excise tax on legalized substances fully capture the third-party externality costs (impaired driving, secondhand exposure, emergency response, health infrastructure overload)?',
    'Comparative cost accounting: measure actual third-party costs (ER visits for overdose, treatment for secondhand exposure, DUI enforcement and victim costs) against total excise tax revenue collected. If costs exceed revenue by more than 10%, externality capture is incomplete.',
    'If externality capture is incomplete, the constraint functions as a snare for third parties (costs are externalized without recovery), which would reclassify the constraint from rope toward tangled_rope or snare depending on the magnitude of the gap. The coherence of the legalization reading depends on externality capture.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(externality_capture_completeness, empirical, 'Whether the tax mechanism actually recovers externality costs as the reading promises.').

omega_variable(
    legal_industry_consolidation_vs_liberty,
    'Does consolidation of the legal substance industry around few large producers undermine the individual liberty justification for legalization, creating a market-power extraction that substitutes for prohibition?',
    'Market-structure data: measure the Herfindahl index of the legal industry over time. If it approaches monopoly (HHI > 2500), ask whether individual choice is genuinely expanded or merely shifted from criminal enforcement to corporate pricing power.',
    'If consolidation is high and industry pricing power is equivalent to prohibition''s price-forcing, the constraint''s beneficiary structure collapses: users are not truly liberated if they face inelastic market prices set by oligopoly. The constraint would reclassify toward snare with users returning to the victim set under different enforcement mechanism (corporate rather than state).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legal_industry_consolidation_vs_liberty, conceptual, 'Whether market-power concentration recreates the constraint that legalization claims to solve.').

omega_variable(
    prohibition_axiom_foreclosure,
    'Does the legalization reading''s axiom of individual autonomy over substance choice logically foreclose the prohibition reading''s axiom of moral transgression, or do they represent incommensurable value systems that coexist in different political communities?',
    'Logical analysis and political ethnography: does anyone coherently hold both axioms simultaneously (individual choice over use + state should punish use as transgression)? If no, foreclosure is real. If yes (e.g., harm to self is punishable but harm to others is managed via tax), the readings coexist.',
    'If foreclosure is real, legalization and prohibition represent genuinely incompatible frameworks, which affects how institutional transitions between readings occur. If they coexist, political conflict persists without logical resolution. This shapes the persistence of black markets and enforcement ambivalence.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(prohibition_axiom_foreclosure, conceptual, 'Whether the autonomy and transgression axioms are logically incompatible or political coexistence of incommensurable values.').

omega_variable(
    administrative_suppression_irreducibility,
    'Can the constraint achieve externality capture and safety coordination without any suppression mechanism (e.g., through purely informational labeling and voluntary reporting of harms), or is some minimum suppression of non-compliance required?',
    'Counterfactual: what happens if licensing and inspection are removed and replaced with pure transparency (users see potency/composition, choose freely, report harms)? If externality capture collapses, suppression is irreducible to the coordination function.',
    'If suppression is irreducible, the constraint carries inherent coercive force even under legalization, which softens the individual-liberty framing. If suppression is reducible, the reading''s libertarian posture is stronger. This affects whether the constraint is a rope (voluntary coordination) or tangled_rope (coordination with irreducible coercion).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(administrative_suppression_irreducibility, empirical, 'Whether administrative suppression is necessary to maintain externality capture or if transparency alone suffices.').

omega_variable(
    sibling_reading_kernel_framing,
    'Are the three readings (legalization, prohibition, harm-reduction) three genuinely distinct constraint structures, or are they three policy positions on a single underlying constraint whose ε and beneficiary structure should be author-invariant?',
    'Meta-analysis: compare the three constraint files (when all are authored). If they have substantially different ε values, different victim sets, and different types, they are three constraints. If the differences are only in narrative framing, they are one constraint with three perspectives — ε-invariance is violated.',
    'If they are three constraints, the kernel decomposition is correct and the corpus should include all three as separate stories. If they are one constraint, the decomposition violates ε-invariance and should be collapsed into a single story with three readings as perspectives (a deferred OQ). This determines whether the corpus includes one or three prohibition/legalization/harm-reduction files.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_kernel_framing, conceptual, 'Whether kernel readings are genuinely distinct constraints or policy framings of one constraint.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(substance_control_kernel__legalization_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(subs_tr_t0, substance_control_kernel__legalization_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(subs_tr_t5, substance_control_kernel__legalization_reading, theater_ratio, 5, 0.16).
narrative_ontology:measurement(subs_tr_t10, substance_control_kernel__legalization_reading, theater_ratio, 10, 0.2).
narrative_ontology:measurement(subs_tr_t15, substance_control_kernel__legalization_reading, theater_ratio, 15, 0.25).
narrative_ontology:measurement(subs_tr_t20, substance_control_kernel__legalization_reading, theater_ratio, 20, 0.27).
narrative_ontology:measurement(subs_tr_t25, substance_control_kernel__legalization_reading, theater_ratio, 25, 0.28).

% Extraction over time
narrative_ontology:measurement(subs_be_t0, substance_control_kernel__legalization_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(subs_be_t5, substance_control_kernel__legalization_reading, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(subs_be_t10, substance_control_kernel__legalization_reading, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(subs_be_t15, substance_control_kernel__legalization_reading, base_extractiveness, 15, 0.58).
narrative_ontology:measurement(subs_be_t20, substance_control_kernel__legalization_reading, base_extractiveness, 20, 0.6).
narrative_ontology:measurement(subs_be_t25, substance_control_kernel__legalization_reading, base_extractiveness, 25, 0.61).

% Suppression requirement over time
narrative_ontology:measurement(subs_su_t0, substance_control_kernel__legalization_reading, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(subs_su_t5, substance_control_kernel__legalization_reading, suppression_requirement, 5, 0.58).
narrative_ontology:measurement(subs_su_t10, substance_control_kernel__legalization_reading, suppression_requirement, 10, 0.48).
narrative_ontology:measurement(subs_su_t15, substance_control_kernel__legalization_reading, suppression_requirement, 15, 0.4).
narrative_ontology:measurement(subs_su_t20, substance_control_kernel__legalization_reading, suppression_requirement, 20, 0.36).
narrative_ontology:measurement(subs_su_t25, substance_control_kernel__legalization_reading, suppression_requirement, 25, 0.34).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(substance_control_kernel__legalization_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(substance_control_kernel__legalization_reading, 0.18).
narrative_ontology:affects_constraint(substance_control_kernel__legalization_reading, substance_control_kernel__prohibition_reading).
narrative_ontology:affects_constraint(substance_control_kernel__legalization_reading, substance_control_kernel__harm_reduction_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the substance_control_kernel. The kernel is contested: does substance use represent a liberty violation, a health condition, or a moral transgression? The legalization reading answers: individual liberty with state intervention limited to externality capture. The prohibition reading answers: moral transgression requiring state punishment. The harm-reduction reading answers: pragmatic health intervention independent of legalization status. Each reading has a distinct constraint file with its own ε, beneficiary/victim structure, and type classification. The three are linked via network.affects_constraints to signal the kernel contest. The ε-invariance principle requires separate files because the three readings produce different structural extraction metrics: prohibition extracts via criminalization (high-ε), legalization extracts via industry consolidation and externality gaps (medium-ε), harm-reduction seeks to reduce extraction regardless of legalization (low-ε). The readings do not represent different measurements of one constraint; they represent different framing commitments that generate different constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
