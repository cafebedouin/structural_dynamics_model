% ============================================================================
% CONSTRAINT STORY: legitimacy_of_practice_standardization__endogenous_displacement_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legitimacy_of_practice_standardization__endogenous_displacement_reading, []).

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
 *   constraint_id: legitimacy_of_practice_standardization__endogenous_displacement_reading
 *   human_readable: Endogenous Displacement Reading of Practice-Change Legitimacy (Calendar/Dress Reform)
 *   domain: political_history/modernization_studies/institutional_change
 *
 * SUMMARY:
 *   This story authors ONE reading of the contested kernel
 *   legitimacy_of_practice_standardization: the endogenous displacement
 *   reading, under which a shift from a traditional calendar or dress
 *   convention to a new, wider-sphere convention is legitimate because it is
 *   driven by perceived utility and voluntary cultural adoption rather than
 *   state decree. On this reading the structural signature is a gradual
 *   adoption curve with real regional variation, elite-to-mass diffusion led
 *   by traders and urban commercial classes, and a genuinely transitional
 *   'double life' period during which old and new conventions coexist without
 *   coercive enforcement. This is not a story about state imposition (that is
 *   the sibling exogenous_override_reading) nor about a stable
 *   domain-partitioned equilibrium (the sibling
 *   dual_practice_equilibrium_reading) — it is the reading in which the old
 *   convention loses ground because it stops paying for itself, not because
 *   it is outlawed.
 *
 * KEY AGENTS:
 *   - early_adopter_elites: primary beneficiary (powerful/arbitrage) — adopts first for interoperability gain
 *   - cross_border_traders: primary beneficiary (organized/arbitrage) — clearest coordination winner
 *   - rural_traditionalist_communities: bears rising cost of nonconformity (powerless/constrained) — reading treats this as lag, not extraction
 *   - religious_calendrical_authorities: loses interpretive monopoly (organized/constrained) — reading treats this as natural obsolescence
 *   - modernization_historians: analytical observer — adjudicates whether a given case fits this reading or a sibling
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimacy_of_practice_standardization__endogenous_displacement_reading, 0.28).
domain_priors:suppression_score(legitimacy_of_practice_standardization__endogenous_displacement_reading, 0.22).
domain_priors:theater_ratio(legitimacy_of_practice_standardization__endogenous_displacement_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__endogenous_displacement_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__endogenous_displacement_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__endogenous_displacement_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__endogenous_displacement_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__endogenous_displacement_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimacy_of_practice_standardization__endogenous_displacement_reading, rope).
narrative_ontology:human_readable(legitimacy_of_practice_standardization__endogenous_displacement_reading, "Endogenous Displacement Reading of Practice-Change Legitimacy (Calendar/Dress Reform)").
narrative_ontology:topic_domain(legitimacy_of_practice_standardization__endogenous_displacement_reading, "political_history/modernization_studies/institutional_change").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimacy_of_practice_standardization__endogenous_displacement_reading, '52794a48-33db-4860-9ffe-74aee55c85b2').
narrative_ontology:cs_kernel_codification('52794a48-33db-4860-9ffe-74aee55c85b2', distributed).
narrative_ontology:cs_authority_grounding('52794a48-33db-4860-9ffe-74aee55c85b2', practice).
narrative_ontology:cs_interpretation_layer_present('52794a48-33db-4860-9ffe-74aee55c85b2').
narrative_ontology:cs_reading_relation('52794a48-33db-4860-9ffe-74aee55c85b2', legitimacy_of_practice_standardization__exogenous_override_reading, coexists_with).
narrative_ontology:cs_reading_relation('52794a48-33db-4860-9ffe-74aee55c85b2', legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, influences).
narrative_ontology:cs_axiom('52794a48-33db-4860-9ffe-74aee55c85b2', foundational, legitimacy_from_voluntary_uptake).
narrative_ontology:cs_axiom_status(legitimacy_from_voluntary_uptake, holdable).
narrative_ontology:cs_axiom_grounding('52794a48-33db-4860-9ffe-74aee55c85b2', legitimacy_from_voluntary_uptake, conventional).
narrative_ontology:cs_axiom('52794a48-33db-4860-9ffe-74aee55c85b2', foundational, utility_perception_drives_cultural_evolution).
narrative_ontology:cs_axiom_status(utility_perception_drives_cultural_evolution, holdable).
narrative_ontology:cs_axiom_grounding('52794a48-33db-4860-9ffe-74aee55c85b2', utility_perception_drives_cultural_evolution, empirically_contingent).
narrative_ontology:cs_reference_frame('52794a48-33db-4860-9ffe-74aee55c85b2', customary_convention_as_living_practice).
narrative_ontology:cs_drift_state('52794a48-33db-4860-9ffe-74aee55c85b2', post_market_integration_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('52794a48-33db-4860-9ffe-74aee55c85b2', '').
narrative_ontology:cs_kernel_id(legitimacy_of_practice_standardization__endogenous_displacement_reading, legitimacy_of_practice_standardization).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimacy_of_practice_standardization__endogenous_displacement_reading, early_adopter_elites).
narrative_ontology:constraint_beneficiary(legitimacy_of_practice_standardization__endogenous_displacement_reading, urban_commercial_classes).
narrative_ontology:constraint_beneficiary(legitimacy_of_practice_standardization__endogenous_displacement_reading, cross_border_traders).
narrative_ontology:constraint_victim(legitimacy_of_practice_standardization__endogenous_displacement_reading, rural_traditionalist_communities).
narrative_ontology:constraint_victim(legitimacy_of_practice_standardization__endogenous_displacement_reading, religious_calendrical_authorities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(legitimacy_of_practice_standardization__endogenous_displacement_reading, urban_commercial_classes).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Urban professionals, merchants, and reform-minded officials who adopt the new calendar or dress first because it lets them transact more easily with foreign trading partners and signals modernity. They face no coercion; they choose the new practice because it serves their existing interests and can revert or hybridize as convenient.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__endogenous_displacement_reading, early_adopter_elites, beneficiary,
    powerful, biographical, arbitrage, national).

% Shopkeepers and tradespeople who adopt new dress or dating conventions gradually as customers and suppliers do, because interoperability has practical value. Some bear minor costs (new wardrobes, dual bookkeeping during transition) but retain freedom to move at their own pace.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__endogenous_displacement_reading, urban_commercial_classes, beneficiary,
    moderate, biographical, mobile, regional).
narrative_ontology:stakeholder_secondary_role(legitimacy_of_practice_standardization__endogenous_displacement_reading, urban_commercial_classes, payer).

% Merchants operating across regions with different practices adopt the standardized convention because it reduces transaction friction with international partners. They are the clearest net beneficiaries of the coordination function and have the most exit flexibility if the new convention fails to deliver value.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__endogenous_displacement_reading, cross_border_traders, beneficiary,
    organized, generational, arbitrage, continental).

% Villages and agricultural communities whose economic and ritual life is organized around the old calendar or dress conventions. Under this reading their slower adoption is voluntary lag rather than resistance to imposition, but as regional trade and administration standardize around the new practice, they find themselves increasingly out of step with markets and record-keeping, a cost the endogenous reading treats as adjustment friction rather than extraction.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__endogenous_displacement_reading, rural_traditionalist_communities, payer,
    powerless, generational, constrained, regional).

% Clerics and ritual specialists whose authority is tied to the old calendar's festival and observance schedule watch their interpretive monopoly erode as the population voluntarily migrates to the new dating convention for everyday and eventually ceremonial purposes. Their loss is framed, in this reading, as the natural consequence of a convention losing perceived utility, not as a displacement imposed on them.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__endogenous_displacement_reading, religious_calendrical_authorities, payer,
    organized, civilizational, constrained, national).

% Local hybrid practices (partial calendar syncretism, mixed dress conventions) that emerge during the transitional period are not represented in either the old or new standard; historical accounts of adoption curves tend to erase them as noise rather than record them as a genuine third path.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__endogenous_displacement_reading, regional_dialect_variants, excluded,
    powerless, generational, trapped, regional).
narrative_ontology:stakeholder_non_agent(legitimacy_of_practice_standardization__endogenous_displacement_reading, regional_dialect_variants).

% Scholars who reconstruct adoption curves, regional variation, and elite-to-mass diffusion patterns from records, and who must adjudicate whether a given historical case is better read as voluntary cultural evolution, state imposition, or a durable dual-track equilibrium.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__endogenous_displacement_reading, modernization_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(legitimacy_of_practice_standardization__endogenous_displacement_reading, cross_border_traders).
narrative_ontology:fixing_cost_class(legitimacy_of_practice_standardization__endogenous_displacement_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aligning a population's calendar or dress conventions with a wider trading and administrative sphere reduces transaction costs, interoperability friction, and signaling ambiguity for anyone dealing across the old boundary.
% TRANSFER_FUNCTION: Under this reading, nothing is coercively transferred: perceived utility migrates gradually from the old convention's holders (rural communities, ritual authorities) to the new convention's early movers (urban elites, traders) as more parties adopt it, with laggards bearing rising coordination costs of nonconformity rather than direct extraction.
% ABSENT_VOICES: Rural traditionalist communities and religious calendrical authorities are present as payers but are not treated in this reading as objecting to imposition — their slower uptake is read as lag, not resistance, which is itself contested; regional hybrid/dialect practices that never fully converge to either standard are essentially unrecorded.
% DISAPPEARANCE_RATIONALE: If the new practice's social and market pressure vanished overnight, early adopters would likely retain the new convention where it still serves their trade interests, but the broader population's push toward standardization would slow sharply; whether the 'world rearranges' depends on how much of the adoption was genuinely utility-driven versus already locked in by network effects and administrative recordkeeping built on the new standard.
% FOUNDING_PROBLEM: Interoperability friction between a locality's traditional practice and a wider trading, administrative, or diplomatic sphere that uses a different convention, making cross-boundary transactions costly or ambiguous.
% FOUNDING_PROBLEM_CORROBORATION: Cross-border traders and urban commercial classes (direct beneficiaries) attest the friction problem was real and the new convention solved it. Independent economic historians studying regional trade volumes before and after adoption periods corroborate reduced transaction costs for market-facing actors, but note this corroboration says nothing about whether rural and ritual communities experienced the same problem or merely absorbed a cost imposed by others' convergence.
narrative_ontology:disappearance_verdict(legitimacy_of_practice_standardization__endogenous_displacement_reading, contested).
narrative_ontology:founding_problem_status(legitimacy_of_practice_standardization__endogenous_displacement_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimacy_of_practice_standardization__endogenous_displacement_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(legitimacy_of_practice_standardization__endogenous_displacement_reading, 'none', 1).
narrative_ontology:epsilon_provenance(legitimacy_of_practice_standardization__endogenous_displacement_reading, 0.28, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legitimacy_of_practice_standardization__endogenous_displacement_reading_tests).
:- end_tests(legitimacy_of_practice_standardization__endogenous_displacement_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored low-to-moderate (0.28 at interval end) and rising only slowly, because under this reading the mechanism is genuinely voluntaristic: no party is coerced into adoption, and the cost borne by laggards is a byproduct of others' convergence rather than a designed transfer. Suppression is low (0.22) — there is no enforcement apparatus compelling adoption, only market and social pressure. Theater ratio stays low throughout (0.05 to 0.18) because there is minimal performative compliance; adoption is either genuine or it is not adopted. Accessibility collapse is moderate (0.35): alternatives to the new convention do not vanish immediately, they simply become progressively less useful as the network of adopters grows, consistent with the expected gradual-adoption-curve structural delta for this reading.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (early adopter elites, urban commercial classes, cross-border traders) are declared with mobile-to-arbitrage exit options, reflecting that they choose the new convention because it already serves them and can hedge or revert if it stops paying off — this pushes their directionality toward the subsidized end. Victims (rural traditionalist communities, religious calendrical authorities) are declared with constrained exit and powerless/organized-but-structurally-outflanked power, reflecting that under this reading their cost is real but is the byproduct of a positive-sum shift in the wider network, not a targeted extraction — their directionality sits closer to target than the beneficiaries but is authored lower than it would be under the exogenous_override_reading, where the same cost would be read as imposed.
 *
 * MANDATROPHY ANALYSIS:
 *   The endogenous displacement reading resists mandatrophy mislabeling in the direction of over-crediting coordination: because adoption looks voluntary and utility-driven from the vantage of the early movers, it would be easy to certify the whole transition as pure Rope. The declared victims and the contested founding_problem_status are the check against that: the reading concedes that religious authorities and rural communities bear a real, if diffuse, cost, and that whether the founding interoperability problem is 'solved' or merely 'won by one side' remains genuinely contested. The claim (rope) and the authored metrics (moderate extractiveness, moderate accessibility collapse) are left independent so the engine's computed classification is the actual measurement, not a foregone conclusion of the voluntarist framing.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    voluntary_adoption_vs_disguised_coercion,
    'Is the adoption curve genuinely driven by perceived utility among rural and traditional communities, or does ''voluntary'' adoption mask administrative and market pressure (tax filings, contract dating, school calendars) that makes nonadoption practically costly enough to function as de facto coercion?',
    'Compare adoption timing against points where administrative recordkeeping, taxation, or contract law began requiring the new convention — if adoption accelerates sharply at those points rather than tracking organic utility perception, the endogenous reading is less supportable for that population.',
    'If adoption tracks administrative requirement rather than perceived utility, this story''s classification shifts toward the exogenous_override_reading''s territory for the affected population, and extractiveness/suppression should be authored higher for that subgroup.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(voluntary_adoption_vs_disguised_coercion, empirical, 'Whether the adoption curve reflects genuine voluntary utility perception or administratively induced conformity.').

omega_variable(
    framing_selection_ambiguity,
    'Given the same historical calendar/dress reform episode, is the endogenous_displacement_reading, the exogenous_override_reading, or the dual_practice_equilibrium_reading the more defensible framing, and what evidence would move a case from one reading to another?',
    'Examine whether legal decrees preceded or followed mass adoption (favors exogenous if decree precedes), whether domain partitioning persisted stably for generations without displacement (favors dual_practice_equilibrium), or whether uptake shows genuine bottom-up diffusion with regional variation and no enforcement mechanism (favors endogenous, as authored here).',
    'Different readings assign different ε, different victim sets, and different classifications to structurally the same episode; misidentifying the reading would misattribute the extraction pattern to the wrong causal mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(framing_selection_ambiguity, conceptual, 'Alternative kernel framings for the same historical practice-change episode and what would distinguish them.').

omega_variable(
    religious_authority_loss_naturalness,
    'Is the erosion of religious calendrical authorities'' interpretive monopoly a natural consequence of the convention losing utility (as this reading holds), or is it itself evidence that the underlying legitimacy claim is doing work to justify a transfer of religious institutional power to secular/commercial institutions?',
    'Track whether religious authorities retained comparable social function through other means (e.g., retained ritual authority while losing only the dating convention) versus experiencing broader institutional decline correlated with the calendar shift.',
    'If the loss is narrowly confined to the calendrical function, the endogenous reading''s low victim-cost framing holds; if it correlates with broader institutional displacement, the victims_declared cost is understated here.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(religious_authority_loss_naturalness, conceptual, 'Whether religious authority loss is a narrow, natural byproduct or evidence of broader displacement understated by this reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimacy_of_practice_standardization__endogenous_displacement_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legi_tr_t0, legitimacy_of_practice_standardization__endogenous_displacement_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(legi_tr_t8, legitimacy_of_practice_standardization__endogenous_displacement_reading, theater_ratio, 8, 0.08).
narrative_ontology:measurement(legi_tr_t16, legitimacy_of_practice_standardization__endogenous_displacement_reading, theater_ratio, 16, 0.11).
narrative_ontology:measurement(legi_tr_t24, legitimacy_of_practice_standardization__endogenous_displacement_reading, theater_ratio, 24, 0.14).
narrative_ontology:measurement(legi_tr_t32, legitimacy_of_practice_standardization__endogenous_displacement_reading, theater_ratio, 32, 0.16).
narrative_ontology:measurement(legi_tr_t40, legitimacy_of_practice_standardization__endogenous_displacement_reading, theater_ratio, 40, 0.18).

% Extraction over time
narrative_ontology:measurement(legi_be_t0, legitimacy_of_practice_standardization__endogenous_displacement_reading, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(legi_be_t8, legitimacy_of_practice_standardization__endogenous_displacement_reading, base_extractiveness, 8, 0.16).
narrative_ontology:measurement(legi_be_t16, legitimacy_of_practice_standardization__endogenous_displacement_reading, base_extractiveness, 16, 0.2).
narrative_ontology:measurement(legi_be_t24, legitimacy_of_practice_standardization__endogenous_displacement_reading, base_extractiveness, 24, 0.24).
narrative_ontology:measurement(legi_be_t32, legitimacy_of_practice_standardization__endogenous_displacement_reading, base_extractiveness, 32, 0.26).
narrative_ontology:measurement(legi_be_t40, legitimacy_of_practice_standardization__endogenous_displacement_reading, base_extractiveness, 40, 0.28).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(legitimacy_of_practice_standardization__endogenous_displacement_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(legitimacy_of_practice_standardization__endogenous_displacement_reading, legitimacy_of_practice_standardization__exogenous_override_reading).
narrative_ontology:affects_constraint(legitimacy_of_practice_standardization__endogenous_displacement_reading, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three linked readings of the kernel legitimacy_of_practice_standardization, applied to the same class of historical episode (calendar/dress reform under cross-regional trade pressure). The endogenous_displacement_reading (this story) authors low-moderate extraction and low suppression, consistent with voluntary, utility-driven, gradually diffusing adoption. The exogenous_override_reading authors the same episode as state-imposed change and should carry higher suppression and extractiveness with the same or overlapping victim groups recast as targets of imposition rather than lagging adopters. The dual_practice_equilibrium_reading authors a stable, non-displacing partition between public/administrative and private/ritual domains and should show low extraction on both sides with no net displacement — its beneficiary/victim structure differs qualitatively because nothing is actually displaced. All three share ε-invariance individually but diverge from one another because they are different constraints, not different measurements of one constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
