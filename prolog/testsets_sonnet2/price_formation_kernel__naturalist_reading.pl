% ============================================================================
% CONSTRAINT STORY: price_formation_kernel__naturalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_price_formation_kernel__naturalist_reading, []).

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
    narrative_ontology:suppression_profile/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: price_formation_kernel__naturalist_reading
 *   human_readable: Price Formation as Natural Equilibrium (Naturalist Reading)
 *   domain: political_economy/housing_markets/institutional_analysis
 *
 * SUMMARY:
 *   This story instantiates the naturalist reading of the price formation
 *   kernel: the claim that housing (and market) prices are the emergent
 *   output of buyers and sellers revealing preferences against objectively
 *   scarce supply, with the price mechanism functioning as a discovery
 *   process rather than a constructed instrument. On this reading, a price is
 *   not set by any party — it is found, the way a water level is found.
 *   Policy interventions (rent control, zoning restriction, credit subsidy)
 *   are read as distortions imposed ON a naturally-occurring equilibrium,
 *   producing deadweight loss rather than redistributing an
 *   already-constructed rent. This is one of four readings of a single
 *   contested kernel about how housing prices come to be what they are. The
 *   sibling readings — institutional (prices are constructed by
 *   zoning/lending/tax/platform rules), georgist (prices conflate earned
 *   improvement value with unearned land rent), and financialization (prices
 *   are driven by credit and asset-feedback dynamics) — are NOT represented
 *   in this file; they are separate constraint stories linked via
 *   network.affects_constraints. This file's epsilon is authored strictly for
 *   the standing arrangement AS THE NATURALIST READING SEES IT: an
 *   equilibrium process with no identifiable extractive structure, because on
 *   this reading there is no party positioned to extract from an emergent,
 *   unowned mechanism.
 *
 * KEY AGENTS:
 *   - Market participants (buyers/sellers/renters): treated as price-takers and price-discoverers, not beneficiaries or victims of a constructed structure
 *   - Analytical economist: observes the equilibrium-forming process and models it as natural law
 *   - Policy intervener (background, not a beneficiary/victim on this reading): any deviation from the discovered price is modeled as a deadweight-loss-inducing distortion, not as evidence the price was constructed
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(price_formation_kernel__naturalist_reading, 0.03).
domain_priors:suppression_score(price_formation_kernel__naturalist_reading, 0.02).
domain_priors:theater_ratio(price_formation_kernel__naturalist_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(price_formation_kernel__naturalist_reading, extractiveness, 0.03).
narrative_ontology:constraint_metric(price_formation_kernel__naturalist_reading, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(price_formation_kernel__naturalist_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(price_formation_kernel__naturalist_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(price_formation_kernel__naturalist_reading, resistance, 0.12).

% --- Constraint claim ---
narrative_ontology:constraint_claim(price_formation_kernel__naturalist_reading, mountain).
narrative_ontology:human_readable(price_formation_kernel__naturalist_reading, "Price Formation as Natural Equilibrium (Naturalist Reading)").
narrative_ontology:topic_domain(price_formation_kernel__naturalist_reading, "political_economy/housing_markets/institutional_analysis").

domain_priors:emerges_naturally(price_formation_kernel__naturalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(price_formation_kernel__naturalist_reading, 'f2b7ddee-3f3a-4629-b6fd-df2960f171a1').
narrative_ontology:cs_kernel_codification('f2b7ddee-3f3a-4629-b6fd-df2960f171a1', distributed).
narrative_ontology:cs_authority_grounding('f2b7ddee-3f3a-4629-b6fd-df2960f171a1', diffuse_epistemic).
narrative_ontology:cs_reading_relation('f2b7ddee-3f3a-4629-b6fd-df2960f171a1', price_formation_kernel__institutional_reading, forecloses).
narrative_ontology:cs_reading_relation('f2b7ddee-3f3a-4629-b6fd-df2960f171a1', price_formation_kernel__georgist_reading, forecloses).
narrative_ontology:cs_reading_relation('f2b7ddee-3f3a-4629-b6fd-df2960f171a1', price_formation_kernel__financialization_reading, coexists_with).
narrative_ontology:cs_axiom('f2b7ddee-3f3a-4629-b6fd-df2960f171a1', foundational, price_is_discovered_not_constructed).
narrative_ontology:cs_axiom_status(price_is_discovered_not_constructed, holdable).
narrative_ontology:cs_axiom_grounding('f2b7ddee-3f3a-4629-b6fd-df2960f171a1', price_is_discovered_not_constructed, empirically_contingent).
narrative_ontology:cs_axiom('f2b7ddee-3f3a-4629-b6fd-df2960f171a1', foundational, no_administrator_of_market_clearing).
narrative_ontology:cs_axiom_status(no_administrator_of_market_clearing, holdable).
narrative_ontology:cs_axiom_grounding('f2b7ddee-3f3a-4629-b6fd-df2960f171a1', no_administrator_of_market_clearing, empirically_contingent).
narrative_ontology:cs_reference_frame('f2b7ddee-3f3a-4629-b6fd-df2960f171a1', classical_market_clearing_equilibrium).
narrative_ontology:cs_drift_state('f2b7ddee-3f3a-4629-b6fd-df2960f171a1', post_2008_credit_crisis_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('f2b7ddee-3f3a-4629-b6fd-df2960f171a1', '').
narrative_ontology:cs_kernel_id(price_formation_kernel__naturalist_reading, price_formation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_vindicates(price_formation_kernel__naturalist_reading, subjective_theory_of_value).
narrative_ontology:constraint_vindicates(price_formation_kernel__naturalist_reading, market_clearing_equilibrium_doctrine).
narrative_ontology:constraint_vindicates(price_formation_kernel__naturalist_reading, deadweight_loss_of_intervention).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: On the naturalist reading, the 'coordination function' is the price mechanism itself: it aggregates dispersed information about scarcity and preference into a single clearing signal without requiring any central administrator to collect or process that information directly. This is presented as pure information-aggregation, not as an arrangement that anyone runs.
% TRANSFER_FUNCTION: None, on this reading. Because price formation is modeled as discovery rather than construction, the reading holds that nothing is transferred BY the mechanism from one party to another — money changes hands in voluntary transactions at the discovered price, but the price level itself is not authored by any party who could be said to be moving value toward themselves.
% ABSENT_VOICES: The sibling readings themselves are the absent voices from this file's own internal logic — a renter priced out of a supply-constrained metro, or a Georgist land-value-tax advocate, would object that 'natural equilibrium' obscures a very administrable set of zoning and land-rent facts. Within the naturalist reading's own frame, however, there is no excluded party, because the reading holds there is no administered process for anyone to be excluded from.
% DISAPPEARANCE_RATIONALE: On the naturalist reading, there is nothing to make disappear: the reading claims price formation is not an administered arrangement but an emergent property of decentralized exchange. If one asks 'what if the belief in this reading disappeared,' policy discourse would shift toward the institutional/georgist/financialization framings (which is exactly the contested terrain the sibling files describe) — but the underlying mechanism the naturalist reading describes (buyers and sellers responding to scarcity and preference) would not itself vanish, because on this reading it was never constructed by an agent capable of un-constructing it.
% FOUNDING_PROBLEM: The naturalist reading was not built to solve a problem in the institutional sense — it is offered as a descriptive account of how any decentralized exchange system necessarily behaves given scarcity and heterogeneous preferences, analogous to a physical or mathematical regularity rather than a designed institution.
% FOUNDING_PROBLEM_CORROBORATION: Mainstream neoclassical price theory and much of the economics profession attest that the equilibrium-discovery account is descriptively adequate for well-functioning markets. Outside that community, housing economists studying supply-constrained metros, land-value-tax researchers in the Georgist tradition, and financial-stability researchers studying credit-driven asset bubbles attest that the 'naturalness' framing breaks down specifically in housing markets because supply is administratively constrained (zoning), land value is structurally distinct from improvement value, and credit availability drives demand independent of underlying scarcity or preference — this corroboration comes from outside the naturalist reading's own tradition and is exactly the basis for the sibling readings' existence as separate constraint files.
narrative_ontology:disappearance_verdict(price_formation_kernel__naturalist_reading, world_unchanged).
narrative_ontology:founding_problem_status(price_formation_kernel__naturalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(price_formation_kernel__naturalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(price_formation_kernel__naturalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(price_formation_kernel__naturalist_reading, 0.03, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(price_formation_kernel__naturalist_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(price_formation_kernel__naturalist_reading, ExtMetricName, E),
    domain_priors:suppression_score(price_formation_kernel__naturalist_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(price_formation_kernel__naturalist_reading),
    narrative_ontology:constraint_metric(price_formation_kernel__naturalist_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(price_formation_kernel__naturalist_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(price_formation_kernel__naturalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored near zero (0.03) because, under the naturalist reading, price is not a mechanism through which any party extracts from any other party — it is the resolution of decentralized preference and scarcity information with no rent-collecting seat. Suppression is authored near zero (0.02) because no coercive apparatus is required to sustain a price the reading holds to be spontaneously discovered; nothing needs to be actively suppressed for supply and demand to clear. Accessibility collapse is authored high (0.88) because, on this reading, once the equilibrium logic is understood, no genuine alternative price-discovery mechanism exists that is not itself either a distortion or a reformulation of the same underlying scarcity/preference math — this mirrors how a genuine mountain (a mathematical or physical limit) collapses alternatives once understood. Resistance is authored low (0.12): a real natural-law claim meets little of the kind of organized resistance that constructed arrangements meet; what resistance exists is mostly resistance to the CLAIM (from the sibling readings), not resistance to the mechanism itself, which cannot be organized against because it has no administrator to organize against.
 *
 * PERSPECTIVAL GAP:
 *   There is no seat divergence to report within this file, because the naturalist reading authors no stakeholders and no differential positions — that IS the reading's content. The perspectival gap that matters is EXTERNAL to this file: between this reading and its three siblings, which is exactly what the omega variables below and the network links are for. Within a single reading, the six-questions battery and the mountain NL profile are the entire analytical surface; there is deliberately no payer/beneficiary contrast to explain here.
 *
 * DIRECTIONALITY LOGIC:
 *   No beneficiaries or victims are declared. This is the structurally correct authoring choice for a genuine mountain claim: if the naturalist reading is right that price formation is a natural equilibrium process, then by construction no party occupies a beneficiary or victim seat relative to the mechanism itself — the mechanism has no operator, no enforcement, and no rent-collection point. This is what distinguishes a genuine mountain from a false summit: a false summit mountain would present a constructed arrangement as natural THE MOMENT identifiable beneficiaries exist (which is exactly what the sibling institutional/financialization/georgist readings claim). Declaring beneficiaries here would either be a category error (attributing extraction to a process the reading holds to be extraction-free) or would itself constitute the false-summit signature the FSM detector exists to catch — and this reading's whole content is the denial that such beneficiaries exist. No stakeholders are authored for the same reason: there are no parties whose position vis-a-vis the mechanism is asymmetric enough to name as agenda_setter, beneficiary, or payer, because 'the market' has no seat.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy is not applicable on this reading by construction: mandatrophy requires an arrangement whose founding function has decayed while its mandate persists, which presupposes the arrangement was constructed with a mandate in the first place. A genuine equilibrium process was never founded to solve a problem and cannot outlive one — there is no 'founding' to interrogate. The sibling readings dispute this: the institutional reading would say the mandate WAS founded (zoning circa mid-20th century, lending standards, tax treatment) and has since drifted from its stated purposes toward rent extraction; the georgist reading would say land rent's 'naturalness' obscures an unearned-value transfer that long predates any founding event and has never been fixed. This story does not adjudicate that dispute — see the kernel_disagreement_location omega below.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    naturalist_reading_kernel_identity,
    'Is price formation genuinely a natural equilibrium process (mountain), or is the naturalist reading itself a beneficiary-serving framing of an arrangement that the institutional/georgist/financialization readings show to be constructed?',
    'Compare price behavior across jurisdictions with materially different zoning, lending, tax, and land-value-capture regimes holding underlying scarcity and preference roughly constant; if price levels and volatility diverge sharply with institutional variation rather than converging on a common equilibrium, the naturalist reading''s core premise weakens.',
    'If resolved toward the constructed side, this story''s zero-beneficiary, zero-extraction profile is itself the false-summit signature — the naturalist framing would be functioning as cover for the constructed arrangements the sibling readings identify, and reclassification toward tangled_rope or snare would follow at the underlying-arrangement level, not within this file.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(naturalist_reading_kernel_identity, conceptual, 'Whether the naturalist reading names a genuine mountain or launders a constructed arrangement as natural law.').

omega_variable(
    kernel_disagreement_location,
    'Where exactly do the four readings of the price formation kernel disagree — is it about which INPUTS matter (scarcity/preference vs. zoning/lending vs. land-rent vs. credit), or about whether price formation has an ADMINISTRATOR at all?',
    'Structural decomposition: for each reading, ask whether it names an agent who sets, benefits from, or could revise the price-forming rules. The naturalist reading answers no; the other three each name identifiable administrators (zoning boards/lenders, landowners, credit-issuing institutions).',
    'If the disagreement is fundamentally about administrator-presence rather than input-weighting, the four readings are not measuring the same phenomenon at different resolutions — they are asserting mutually exclusive claims about whether price formation has a party positioned to extract, which sharpens the forecloses/coexists_with choice in cs_structure.reading_relations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_disagreement_location, conceptual, 'Locating the precise structural axis of disagreement among the kernel''s four readings.').

omega_variable(
    policy_intervention_deadweight_or_correction,
    'Are documented cases of policy intervention (rent control, zoning reform, land value taxation) properly modeled as deadweight-loss-inducing distortions of a natural equilibrium, or as corrections to a previously constructed distortion (e.g., correcting for a zoning-created artificial scarcity)?',
    'Case-by-case counterfactual analysis: does removing the intervention return the market to a state with LOWER identifiable transaction costs and administrator presence (supporting naturalist framing) or HIGHER (supporting institutional/georgist framing)?',
    'Determines whether specific historical interventions should be read, within this reading''s own terms, as pure efficiency losses or as the naturalist reading systematically mis-locating a already-existing distortion''s origin.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(policy_intervention_deadweight_or_correction, empirical, 'Whether specific interventions are deadweight loss or distortion-correction under the naturalist reading''s own logic.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(price_formation_kernel__naturalist_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pric_tr_t0, price_formation_kernel__naturalist_reading, theater_ratio, 0, 0.04).
narrative_ontology:measurement(pric_tr_t8, price_formation_kernel__naturalist_reading, theater_ratio, 8, 0.04).
narrative_ontology:measurement(pric_tr_t16, price_formation_kernel__naturalist_reading, theater_ratio, 16, 0.05).
narrative_ontology:measurement(pric_tr_t24, price_formation_kernel__naturalist_reading, theater_ratio, 24, 0.05).
narrative_ontology:measurement(pric_tr_t32, price_formation_kernel__naturalist_reading, theater_ratio, 32, 0.05).
narrative_ontology:measurement(pric_tr_t40, price_formation_kernel__naturalist_reading, theater_ratio, 40, 0.05).

% Extraction over time
narrative_ontology:measurement(pric_be_t0, price_formation_kernel__naturalist_reading, base_extractiveness, 0, 0.02).
narrative_ontology:measurement(pric_be_t8, price_formation_kernel__naturalist_reading, base_extractiveness, 8, 0.02).
narrative_ontology:measurement(pric_be_t16, price_formation_kernel__naturalist_reading, base_extractiveness, 16, 0.03).
narrative_ontology:measurement(pric_be_t24, price_formation_kernel__naturalist_reading, base_extractiveness, 24, 0.03).
narrative_ontology:measurement(pric_be_t32, price_formation_kernel__naturalist_reading, base_extractiveness, 32, 0.03).
narrative_ontology:measurement(pric_be_t40, price_formation_kernel__naturalist_reading, base_extractiveness, 40, 0.03).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(price_formation_kernel__naturalist_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(price_formation_kernel__naturalist_reading, price_formation_kernel__institutional_reading).
narrative_ontology:affects_constraint(price_formation_kernel__naturalist_reading, price_formation_kernel__georgist_reading).
narrative_ontology:affects_constraint(price_formation_kernel__naturalist_reading, price_formation_kernel__financialization_reading).

% DUAL FORMULATION NOTE:
% This file is one of four constraint stories decomposing the colloquial concept 'how housing prices are formed' per the ε-invariance principle. Each sibling reading authors a structurally distinct claim with its own stable epsilon: naturalist_reading (this file, ε≈0.03, mountain, no parties), institutional_reading (constructed by zoning/lending/tax/platforms — expect a tangled_rope or snare with identifiable beneficiaries such as incumbent landowners, licensed intermediaries, and lenders, and victims such as excluded renters/buyers), georgist_reading (separates unearned land rent from earned improvement value — expect land rent captured as extraction with landowners as beneficiaries and labor/capital-value producers as the class bearing the unrecognized transfer), and financialization_reading (credit expansion and asset-price feedback loops — expect leveraged asset holders and credit-issuing institutions as beneficiaries, with first-time buyers and renters priced out as victims). The four files are linked bidirectionally via affects_constraints to preserve the contamination-propagation structure of the shared kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
