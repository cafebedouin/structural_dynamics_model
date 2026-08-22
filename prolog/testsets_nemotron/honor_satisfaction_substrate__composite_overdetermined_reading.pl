% ============================================================================
% CONSTRAINT STORY: honor_satisfaction_substrate__composite_overdetermined_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_honor_satisfaction_substrate__composite_overdetermined_reading, []).

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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: honor_satisfaction_substrate__composite_overdetermined_reading
 *   human_readable: Honor Satisfaction Substrate — Composite Overdetermined Decline
 *   domain: historical_sociology/cultural_anthropology/legal_history
 *
 * SUMMARY:
 *   The honor satisfaction substrate — the integrated system of dueling,
 *   honor courts, and aristocratic violence norms that regulated status
 *   disputes among European elites 1750–1900 — declined through causally
 *   entangled exogenous and endogenous mechanisms. Legal suppression (state
 *   monopoly on violence, criminalization of dueling, professional
 *   disciplinary codes) and honor code transformation (bourgeois dignity
 *   culture displacing aristocratic honor culture, commercial reputation
 *   replacing personal satisfaction) operated simultaneously with feedback
 *   loops: legal prosecutions delegitimized the practice normatively, while
 *   cultural shifts made enforcement politically viable. The composite
 *   reading treats this as a single tangled_rope: a genuine coordination
 *   mechanism (managing elite violence without state capacity) that became
 *   extractive (benefiting state legal monopoly, professional classes,
 *   insurance institutions) while extracting from its original practitioners
 *   (aristocratic officers, duelists, honor courts), with active enforcement
 *   sustaining the extraction. This is ONE reading of the contested kernel;
 *   sibling readings isolate one mechanism each.
 *
 * KEY AGENTS:
 *   - state_legal_monopoly: Primary beneficiary (institutional/arbitrage) — absorbs violence regulation into state apparatus
 *   - aristocratic_officer_corps: Primary victim (organized/identity_locked) — loses honor satisfaction mechanism, forced into legal substitution
 *   - emerging_professional_classes: Beneficiary (organized/mobile) — gains status regulation via credentials not violence
 *   - commercial_insurance_institutions: Beneficiary (institutional/arbitrage) — monetizes risk previously managed through honor
 *   - dueling_practitioners: Victim (powerless/trapped) — criminalized for maintaining inherited practice
 *   - honor_court_institutions: Victim (organized/constrained) — abolished or absorbed into state courts
 *   - bourgeois_intellectuals: Observer (analytical/analytical) — theorize dignity culture as honor's successor
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_satisfaction_substrate__composite_overdetermined_reading, 0.68).
domain_priors:suppression_score(honor_satisfaction_substrate__composite_overdetermined_reading, 0.72).
domain_priors:theater_ratio(honor_satisfaction_substrate__composite_overdetermined_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_satisfaction_substrate__composite_overdetermined_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(honor_satisfaction_substrate__composite_overdetermined_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(honor_satisfaction_substrate__composite_overdetermined_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_satisfaction_substrate__composite_overdetermined_reading, accessibility_collapse, 0.63).
narrative_ontology:constraint_metric(honor_satisfaction_substrate__composite_overdetermined_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_satisfaction_substrate__composite_overdetermined_reading, tangled_rope).
narrative_ontology:human_readable(honor_satisfaction_substrate__composite_overdetermined_reading, "Honor Satisfaction Substrate — Composite Overdetermined Decline").
narrative_ontology:topic_domain(honor_satisfaction_substrate__composite_overdetermined_reading, "historical_sociology/cultural_anthropology/legal_history").

domain_priors:requires_active_enforcement(honor_satisfaction_substrate__composite_overdetermined_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_satisfaction_substrate__composite_overdetermined_reading, '39f37b27-ccfd-4c53-9b34-58a2dfe1fa93').
narrative_ontology:cs_kernel_codification('39f37b27-ccfd-4c53-9b34-58a2dfe1fa93', distributed).
narrative_ontology:cs_authority_grounding('39f37b27-ccfd-4c53-9b34-58a2dfe1fa93', practice).
narrative_ontology:cs_interpretation_layer_present('39f37b27-ccfd-4c53-9b34-58a2dfe1fa93').
narrative_ontology:cs_reading_relation('39f37b27-ccfd-4c53-9b34-58a2dfe1fa93', honor_satisfaction_substrate__practice_decline_reading, influences).
narrative_ontology:cs_reading_relation('39f37b27-ccfd-4c53-9b34-58a2dfe1fa93', honor_satisfaction_substrate__cultural_contraction_reading, influences).
narrative_ontology:cs_axiom('39f37b27-ccfd-4c53-9b34-58a2dfe1fa93', foundational, causal_entanglement_of_suppression_and_delegitimation).
narrative_ontology:cs_axiom_status(causal_entanglement_of_suppression_and_delegitimation, holdable).
narrative_ontology:cs_axiom_grounding('39f37b27-ccfd-4c53-9b34-58a2dfe1fa93', causal_entanglement_of_suppression_and_delegitimation, empirically_contingent).
narrative_ontology:cs_axiom('39f37b27-ccfd-4c53-9b34-58a2dfe1fa93', foundational, honor_substrate_as_coordination_technology).
narrative_ontology:cs_axiom_status(honor_substrate_as_coordination_technology, holdable).
narrative_ontology:cs_axiom_grounding('39f37b27-ccfd-4c53-9b34-58a2dfe1fa93', honor_substrate_as_coordination_technology, empirically_contingent).
narrative_ontology:cs_reference_frame('39f37b27-ccfd-4c53-9b34-58a2dfe1fa93', aristocratic_honor_satisfaction_order).
narrative_ontology:cs_drift_state('39f37b27-ccfd-4c53-9b34-58a2dfe1fa93', bourgeois_legal_order_consolidation, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('39f37b27-ccfd-4c53-9b34-58a2dfe1fa93', '').
narrative_ontology:cs_kernel_id(honor_satisfaction_substrate__composite_overdetermined_reading, honor_satisfaction_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_satisfaction_substrate__composite_overdetermined_reading, state_legal_monopoly).
narrative_ontology:constraint_beneficiary(honor_satisfaction_substrate__composite_overdetermined_reading, emerging_professional_classes).
narrative_ontology:constraint_beneficiary(honor_satisfaction_substrate__composite_overdetermined_reading, commercial_insurance_institutions).
narrative_ontology:constraint_victim(honor_satisfaction_substrate__composite_overdetermined_reading, aristocratic_officer_corps).
narrative_ontology:constraint_victim(honor_satisfaction_substrate__composite_overdetermined_reading, dueling_practitioners).
narrative_ontology:constraint_victim(honor_satisfaction_substrate__composite_overdetermined_reading, honor_court_institutions).
narrative_ontology:constraint_vindicates(honor_satisfaction_substrate__composite_overdetermined_reading, state_monopoly_on_violence).
narrative_ontology:constraint_vindicates(honor_satisfaction_substrate__composite_overdetermined_reading, bourgeois_legal_equality).
narrative_ontology:constraint_vindicates(honor_satisfaction_substrate__composite_overdetermined_reading, rational_bureaucratic_order).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Criminalizes dueling through penal codes (e.g., Prussian 1851, French 1791/1810, British common law) and military law. Builds state courts, police, and professional disciplinary systems as replacement coordination. Collects monopoly rents: fines, court fees, professional licensing revenue. Justifies suppression as public order and equality before law.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__composite_overdetermined_reading, state_legal_monopoly, agenda_setter,
    institutional, generational, arbitrage, national).

% Officer identity is constitutively bound to honor satisfaction: 'an officer does not sue, he demands satisfaction.' Dueling is not optional — it is the proof of gentlemanly status. Legal prohibition creates a double bind: duel and face prison/dismissal; refuse and face social death. Exit requires abandoning the identity 'officer and gentleman' — professionally and existentially prohibitive. Many continue dueling covertly (German Mensur, French army duels 1880s–1914).
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__composite_overdetermined_reading, aristocratic_officer_corps, payer,
    organized, biographical, identity_locked, national).

% Lawyers, doctors, civil servants, journalists gain status through state-licensed credentials and professional associations, not personal violence. They benefit from the legal monopoly's suppression of aristocratic violence — it clears the field for meritocratic reputation markets. They advocate for 'cultures of dignity' (legal rights, professional ethics) as the successor to 'cultures of honor.' Exit is easy: they never depended on the honor substrate.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__composite_overdetermined_reading, emerging_professional_classes, beneficiary,
    organized, biographical, mobile, national).

% Life insurance, accident insurance, and liability markets expand precisely as dueling declines. Honor violence created uninsurable, unquantifiable risk; legal substitution creates calculable actuarial risk. Insurance institutions lobby for legal prohibition (reducing moral hazard) and profit from the new risk pools. They are not direct enforcers but structural beneficiaries of the constraint's transformation.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__composite_overdetermined_reading, commercial_insurance_institutions, beneficiary,
    institutional, generational, arbitrage, national).

% Non-aristocratic duelists (students, junior officers, bourgeois youths emulating aristocratic codes) face full criminal penalties without the institutional protections (regimental silence, aristocratic judicial leniency) that shielded elite practitioners. They are trapped: the honor code demands satisfaction, the law demands prosecution, and they lack the social capital to navigate either. Many serve prison terms or die in duels that no longer carry social recognition.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__composite_overdetermined_reading, dueling_practitioners, payer,
    powerless, immediate, trapped, local).

% Formal honor courts (German Ehrengerichte, French courts of honor, Austrian military honor councils) adjudicated satisfaction claims and regulated duels. They are abolished by state decree (Prussia 1847/1851, Austria 1867) or absorbed into military disciplinary systems. Their jurisdiction, prestige, and revenue transfer to state courts. Some persist vestigially in reserve officer corps into the 1930s. Exit means institutional dissolution — constrained by state monopoly.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__composite_overdetermined_reading, honor_court_institutions, payer,
    organized, biographical, constrained, regional).

% Theorize the transition: Kant, Hegel, Weber, Simmel, and later Elias (The Civilizing Process) frame dueling's decline as either rationalization (Weber), psychogenetic pacification (Elias), or bourgeois dignity displacing aristocratic honor (Taylor, Appiah). They do not bear costs or collect rents from the constraint but provide the interpretive framework that legitimizes the new order.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__composite_overdetermined_reading, bourgeois_intellectuals, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Managed elite male violence in societies with insufficient state capacity: provided a bounded, ritualized channel for status disputes that prevented vendettas, assassinations, and uncontrolled brawling. Honor courts adjudicated claims; dueling protocols limited lethality; the code defined who owed satisfaction to whom. This was a genuine coordination mechanism — not merely extraction.
% TRANSFER_FUNCTION: Transferred the monopoly on legitimate violence regulation from aristocratic honor courts to state legal institutions; transferred status regulation from personal violence to professional credentials and commercial reputation; transferred risk management from personal honor to insurance markets. The aristocratic officer corps paid with their traditional satisfaction mechanism and identity; the state, professions, and insurance institutions collected the resulting rents.
% ABSENT_VOICES: Women of the aristocratic classes (whose honor was defended by male relatives but who had no voice in the code), colonial subjects (for whom European honor codes were imposed as tools of domination), and non-elite men (for whom 'honor' meant something entirely different — labor reputation, family reputation — and who were excluded from the dueling franchise). These voices would object to both the aristocratic code AND its bourgeois successor, but neither reading includes them.
% DISAPPEARANCE_RATIONALE: If the honor satisfaction substrate vanished overnight in 1800, European elites would have no recognized mechanism for status dispute resolution — vendettas, assassination, and legal vacuum would follow until state courts and professional codes scaled up. The world rearranged precisely because the constraint was a genuine coordination mechanism whose disappearance created a functional gap. The rearrangement took 150 years and produced the modern legal-professional-insurance complex.
% FOUNDING_PROBLEM: Early modern European states lacked the administrative capacity to adjudicate every status dispute among elites. The honor satisfaction substrate — dueling protocols, honor courts, the aristocratic code — provided a decentralized, self-enforcing coordination mechanism that prevented elite violence from destabilizing fragile state authority. It was built to solve: 'How do elites resolve status conflicts without destroying the state that protects their privileges?'
% FOUNDING_PROBLEM_CORROBORATION: State administrative capacity (bureaucracy, police, courts) demonstrably expanded 1750–1850 (Tilly, Hintze, Weber — outside beneficiaries). Dueling frequency declined as state courts became accessible (court records, prosecution statistics — empirical). The honor code's own theorists (Schopenhauer, Nietzsche) acknowledged the founding problem was solved but lamented the cultural loss — corroboration from within the victim set that the problem was dead.
narrative_ontology:disappearance_verdict(honor_satisfaction_substrate__composite_overdetermined_reading, world_rearranges).
narrative_ontology:founding_problem_status(honor_satisfaction_substrate__composite_overdetermined_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_satisfaction_substrate__composite_overdetermined_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(honor_satisfaction_substrate__composite_overdetermined_reading, 'none', 1).
narrative_ontology:epsilon_provenance(honor_satisfaction_substrate__composite_overdetermined_reading, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(honor_satisfaction_substrate__composite_overdetermined_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(honor_satisfaction_substrate__composite_overdetermined_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(honor_satisfaction_substrate__composite_overdetermined_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) is high because the coordination function (managing elite violence) was real but the arrangement increasingly transferred resources to state courts, professional guilds, and insurance markets — the 'protection' became a rent. Suppression (0.72) is high because the constraint's persistence required active criminalization, military discipline, and professional exclusion — not merely cultural drift. Theater ratio (0.41) is moderate: honor courts and dueling rituals persisted performatively after their functional core was hollowed by legal alternatives. Accessibility collapse (0.63) reflects that alternatives (legal recourse, professional reputation) became viable only gradually and unevenly. Resistance (0.58) is substantial: aristocratic officers maintained dueling cultures despite criminalization, and honor courts operated underground in military contexts. The measurement grid shows rising extraction and suppression across 1750–1900, with theater ratio accelerating after 1815 as the coordination function atrophied.
 *
 * PERSPECTIVAL GAP:
 *   The state legal monopoly (agenda_setter, institutional/arbitrage) experiences the constraint as a rope — it built the replacement coordination. The aristocratic officer corps (payer, organized/identity_locked) experiences it as a snare — their honor substrate was criminalized while the cultural logic they lived by was delegitimized. Emerging professional classes (beneficiary, organized/mobile) experience it as a scaffold — the transition to credential-based status was the justification. The engine computes these divergent seat types from the structural data; the single claimed_type (tangled_rope) reflects the constraint's unified operation across seats.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries declared: state_legal_monopoly (collects monopoly rents on violence regulation), emerging_professional_classes (gain status monopoly via credentials), commercial_insurance_institutions (monetize honor-risk). Victims declared: aristocratic_officer_corps (identity_locked — professional identity fused with honor code; exit means abandoning self-concept as 'officer and gentleman'), dueling_practitioners (trapped — criminalized with no alternative satisfaction mechanism), honor_court_institutions (constrained — abolished by state decree). Directionality derives: identity_locked victims sit at d≈0.85 (near full target), trapped victims at d≈0.9; institutional beneficiaries at d≈0.15; organized beneficiaries at d≈0.25. The entanglement means victims face BOTH legal suppression (high d) AND cultural delegitimation (identity_lock reinforced by bourgeois discourse).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (managing elite violence without state capacity) was substantially solved by 1850 — state courts, police, and professional regulation provided functional alternatives. Yet the honor substrate persisted performatively until ~1900. The mandatrophy is resolved: the arrangement's mandate (coordination) is dead but its extraction (status rents, legal monopoly, insurance markets) persists. This is NOT a piton (no single administrator maintaining it theatrically) but a tangled_rope where the coordination function atrophied while the extraction function intensified and diffused across multiple beneficiaries. The cultural_contraction_reading captures the mandatrophy as 'mountain erosion'; the practice_decline_reading captures it as 'exogenous snare'; this composite reading captures the entangled transition.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'This constraint instantiates the composite_overdetermined_reading of the honor_satisfaction_substrate kernel. How does its ε and beneficiary/victim structure differ from the practice_decline_reading and cultural_contraction_reading?',
    'Compare the three readings'' authored base_properties: practice_decline_reading should show higher suppression (exogenous enforcement), lower theater_ratio; cultural_contraction_reading should show higher accessibility_collapse (unthinkability), lower resistance; composite_overdetermined_reading should show intermediate values with high suppression AND high accessibility_collapse simultaneously, reflecting causal entanglement.',
    'If the three readings show distinct metric profiles, the kernel decomposition is validated; if they converge, the composite reading may not be structurally distinct from its siblings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Structural distinctness of kernel readings within the honor_satisfaction_substrate family').

omega_variable(
    causal_entanglement_vs_additivity,
    'Are the exogenous suppression and endogenous delegitimation mechanisms causally entangled (each amplifies the other) or merely additive (independent parallel pressures)?',
    'Historical counterfactual analysis: trace whether legal prosecutions increased *because* honor codes were already eroding, or whether honor codes eroded *because* legal enforcement created normative cascades. Look for feedback loops in prosecution records, pamphlet literature, and military court martial patterns.',
    'If entangled, the constraint is a single tangled_rope with a unified extraction logic; if additive, it may be two constraints (a snare of legal suppression + a mountain/scaffold of cultural change) linked by network.affects_constraints.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(causal_entanglement_vs_additivity, empirical, 'Whether the two decline mechanisms are causally entangled or merely coincident').

omega_variable(
    honor_substrate_naturalness,
    'Is the honor satisfaction substrate a constructed social technology (coordination mechanism for status/violence management) or a natural/cultural given that ''erodes'' like a mountain?',
    'Cross-cultural comparison: do societies without formal dueling develop functionally equivalent honor-satisfaction mechanisms (blood feuds, ritual combat, legalistic reputation markets)? If yes, the substrate is a coordination technology (rope/tangled_rope); if no, it may be a culturally specific mountain-like formation.',
    'If constructed coordination, the claimed tangled_rope type is reinforced and the cultural_contraction_reading''s ''mountain erosion'' framing is a false summit; if natural/cultural given, the cultural_contraction_reading may capture a real mountain dynamic.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(honor_substrate_naturalness, conceptual, 'Whether the honor substrate is a coordination technology or a natural/cultural given').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_satisfaction_substrate__composite_overdetermined_reading, 1750, 1900).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hono_tr_t1750, honor_satisfaction_substrate__composite_overdetermined_reading, theater_ratio, 1750, 0.12).
narrative_ontology:measurement(hono_tr_t1780, honor_satisfaction_substrate__composite_overdetermined_reading, theater_ratio, 1780, 0.18).
narrative_ontology:measurement(hono_tr_t1810, honor_satisfaction_substrate__composite_overdetermined_reading, theater_ratio, 1810, 0.27).
narrative_ontology:measurement(hono_tr_t1840, honor_satisfaction_substrate__composite_overdetermined_reading, theater_ratio, 1840, 0.34).
narrative_ontology:measurement(hono_tr_t1870, honor_satisfaction_substrate__composite_overdetermined_reading, theater_ratio, 1870, 0.38).
narrative_ontology:measurement(hono_tr_t1900, honor_satisfaction_substrate__composite_overdetermined_reading, theater_ratio, 1900, 0.41).

% Extraction over time
narrative_ontology:measurement(hono_be_t1750, honor_satisfaction_substrate__composite_overdetermined_reading, base_extractiveness, 1750, 0.35).
narrative_ontology:measurement(hono_be_t1780, honor_satisfaction_substrate__composite_overdetermined_reading, base_extractiveness, 1780, 0.42).
narrative_ontology:measurement(hono_be_t1810, honor_satisfaction_substrate__composite_overdetermined_reading, base_extractiveness, 1810, 0.51).
narrative_ontology:measurement(hono_be_t1840, honor_satisfaction_substrate__composite_overdetermined_reading, base_extractiveness, 1840, 0.58).
narrative_ontology:measurement(hono_be_t1870, honor_satisfaction_substrate__composite_overdetermined_reading, base_extractiveness, 1870, 0.65).
narrative_ontology:measurement(hono_be_t1900, honor_satisfaction_substrate__composite_overdetermined_reading, base_extractiveness, 1900, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(hono_su_t1750, honor_satisfaction_substrate__composite_overdetermined_reading, suppression_requirement, 1750, 0.45).
narrative_ontology:measurement(hono_su_t1780, honor_satisfaction_substrate__composite_overdetermined_reading, suppression_requirement, 1780, 0.52).
narrative_ontology:measurement(hono_su_t1810, honor_satisfaction_substrate__composite_overdetermined_reading, suppression_requirement, 1810, 0.61).
narrative_ontology:measurement(hono_su_t1840, honor_satisfaction_substrate__composite_overdetermined_reading, suppression_requirement, 1840, 0.67).
narrative_ontology:measurement(hono_su_t1870, honor_satisfaction_substrate__composite_overdetermined_reading, suppression_requirement, 1870, 0.71).
narrative_ontology:measurement(hono_su_t1900, honor_satisfaction_substrate__composite_overdetermined_reading, suppression_requirement, 1900, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(honor_satisfaction_substrate__composite_overdetermined_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(honor_satisfaction_substrate__composite_overdetermined_reading, 0.12).
narrative_ontology:affects_constraint(honor_satisfaction_substrate__composite_overdetermined_reading, honor_satisfaction_substrate__practice_decline_reading).
narrative_ontology:affects_constraint(honor_satisfaction_substrate__composite_overdetermined_reading, honor_satisfaction_substrate__cultural_contraction_reading).

% DUAL FORMULATION NOTE:
% The honor_satisfaction_substrate kernel decomposes into three structurally distinct constraints: practice_decline_reading (snare — exogenous legal suppression extracting from practitioners while honor code persists), cultural_contraction_reading (mountain/tangled_rope boundary — endogenous cultural transformation with high accessibility_collapse), and this composite_overdetermined_reading (tangled_rope — entangled mechanisms with simultaneous high suppression AND high accessibility_collapse). The composite reading structurally influences both siblings: it provides the causal entanglement hypothesis that, if validated, would reclassify the siblings as partial facets of a single dynamic rather than independent constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(honor_satisfaction_substrate__composite_overdetermined_reading, organized, 0.35).
constraint_indexing:directionality_override(honor_satisfaction_substrate__composite_overdetermined_reading, powerless, 0.92).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
