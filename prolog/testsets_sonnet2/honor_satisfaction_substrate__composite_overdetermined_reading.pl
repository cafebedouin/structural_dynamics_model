% ============================================================================
% CONSTRAINT STORY: honor_satisfaction_substrate__composite_overdetermined_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
 *   constraint_id: honor_satisfaction_substrate__composite_overdetermined_reading
 *   human_readable: The Dueling-Honor Substrate: Composite Overdetermined Decline Reading
 *   domain: historical_sociology/legal_history/cultural_anthropology
 *
 * SUMMARY:
 *   Between roughly the late eighteenth and early twentieth centuries, formal
 *   dueling as a means of settling honor disputes disappeared across most of
 *   Western Europe and North America. Two competing historical explanations
 *   exist: one holds that dueling was suppressed by increasingly effective
 *   state legal and military-disciplinary institutions acting against a
 *   persistent honor code; the other holds that the honor code itself
 *   transformed from an honor culture (external, publicly defended
 *   reputation) to a dignity culture (internal, legally adjudicated worth),
 *   making dueling unthinkable rather than merely illegal. This story authors
 *   a third, composite claim: that both mechanisms operated simultaneously
 *   and were causally non-independent — each accelerated the other — such
 *   that the decline cannot be partitioned into an exogenous-suppression
 *   component and an endogenous-delegitimation component without residual
 *   unexplained variance.
 *
 * KEY AGENTS:
 *   - gentry_honor_class_incumbents: bears both loss of old certification mechanism and gain of new status economy
 *   - state_judicial_authorities: administers the suppression arm, consolidates authority
 *   - commercial_bourgeoisie: beneficiary of both suppression and cultural transformation
 *   - dueling_challenged_men: trapped between two collapsing regimes during transition
 *   - historical_sociologists: analytical observers who author the composite claim itself
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_satisfaction_substrate__composite_overdetermined_reading, 0.58).
domain_priors:suppression_score(honor_satisfaction_substrate__composite_overdetermined_reading, 0.62).
domain_priors:theater_ratio(honor_satisfaction_substrate__composite_overdetermined_reading, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_satisfaction_substrate__composite_overdetermined_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(honor_satisfaction_substrate__composite_overdetermined_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(honor_satisfaction_substrate__composite_overdetermined_reading, theater_ratio, 0.44).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_satisfaction_substrate__composite_overdetermined_reading, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(honor_satisfaction_substrate__composite_overdetermined_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_satisfaction_substrate__composite_overdetermined_reading, tangled_rope).
narrative_ontology:human_readable(honor_satisfaction_substrate__composite_overdetermined_reading, "The Dueling-Honor Substrate: Composite Overdetermined Decline Reading").
narrative_ontology:topic_domain(honor_satisfaction_substrate__composite_overdetermined_reading, "historical_sociology/legal_history/cultural_anthropology").

domain_priors:requires_active_enforcement(honor_satisfaction_substrate__composite_overdetermined_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_satisfaction_substrate__composite_overdetermined_reading, '04d63f6c-2366-451f-b84a-38b680dd2f4a').
narrative_ontology:cs_kernel_codification('04d63f6c-2366-451f-b84a-38b680dd2f4a', distributed).
narrative_ontology:cs_authority_grounding('04d63f6c-2366-451f-b84a-38b680dd2f4a', distributed).
narrative_ontology:cs_reading_relation('04d63f6c-2366-451f-b84a-38b680dd2f4a', honor_satisfaction_substrate__cultural_contraction_reading, influences).
narrative_ontology:cs_reading_relation('04d63f6c-2366-451f-b84a-38b680dd2f4a', honor_satisfaction_substrate__practice_decline_reading, influences).
narrative_ontology:cs_axiom('04d63f6c-2366-451f-b84a-38b680dd2f4a', foundational, causal_pathways_are_non_independent).
narrative_ontology:cs_axiom_status(causal_pathways_are_non_independent, holdable).
narrative_ontology:cs_axiom_grounding('04d63f6c-2366-451f-b84a-38b680dd2f4a', causal_pathways_are_non_independent, empirically_contingent).
narrative_ontology:cs_axiom('04d63f6c-2366-451f-b84a-38b680dd2f4a', secondary, single_mechanism_accounts_are_incomplete).
narrative_ontology:cs_axiom_status(single_mechanism_accounts_are_incomplete, holdable).
narrative_ontology:cs_axiom_grounding('04d63f6c-2366-451f-b84a-38b680dd2f4a', single_mechanism_accounts_are_incomplete, empirically_contingent).
narrative_ontology:cs_reference_frame('04d63f6c-2366-451f-b84a-38b680dd2f4a', dual_mechanism_entanglement_thesis).
narrative_ontology:cs_drift_state('04d63f6c-2366-451f-b84a-38b680dd2f4a', post_dignity_culture_historiography, gap(revival_pressure, substantial, false)).
narrative_ontology:cs_created_at('04d63f6c-2366-451f-b84a-38b680dd2f4a', '').
narrative_ontology:cs_kernel_id(honor_satisfaction_substrate__composite_overdetermined_reading, honor_satisfaction_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_satisfaction_substrate__composite_overdetermined_reading, gentry_honor_class_incumbents).
narrative_ontology:constraint_beneficiary(honor_satisfaction_substrate__composite_overdetermined_reading, state_judicial_authorities).
narrative_ontology:constraint_beneficiary(honor_satisfaction_substrate__composite_overdetermined_reading, commercial_bourgeoisie).
narrative_ontology:constraint_victim(honor_satisfaction_substrate__composite_overdetermined_reading, dueling_challenged_men).
narrative_ontology:constraint_victim(honor_satisfaction_substrate__composite_overdetermined_reading, seconds_and_witnesses).
narrative_ontology:constraint_victim(honor_satisfaction_substrate__composite_overdetermined_reading, widows_and_dependents).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(honor_satisfaction_substrate__composite_overdetermined_reading, gentry_honor_class_incumbents).
narrative_ontology:constraint_vindicates(honor_satisfaction_substrate__composite_overdetermined_reading, personal_honor_as_publicly_defensible_property).
narrative_ontology:constraint_vindicates(honor_satisfaction_substrate__composite_overdetermined_reading, state_monopoly_on_legitimate_violence).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Aristocratic and officer-class men whose social standing was historically underwritten by dueling's coordination function: it let peers verify each other's claims to honor without appeal to courts they considered beneath their station. As the honor code itself transforms, they simultaneously lose the mechanism that used to certify their status (payer) and gain admission to a redefined dignity-based status economy that no longer requires risking death to hold rank (beneficiary).
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__composite_overdetermined_reading, gentry_honor_class_incumbents, beneficiary,
    powerful, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(honor_satisfaction_substrate__composite_overdetermined_reading, gentry_honor_class_incumbents, payer).

% Courts, legislatures, and military tribunals that criminalized dueling, prosecuted survivors and seconds, and offered civil defamation and honor-restoration remedies as substitutes. They administer the exogenous suppression arm directly and benefit from the resulting consolidation of violence-adjudication authority in state institutions.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__composite_overdetermined_reading, state_judicial_authorities, agenda_setter,
    institutional, civilizational, arbitrage, national).

% Rising merchant and professional classes whose social capital depended on predictable business relationships, not honor-violence. They benefited from both suppression arms: legal prohibition removed a coercive gentry-only mechanism that excluded them, and honor-code transformation toward 'dignity culture' opened elite status to achievement rather than lineage and combat-readiness.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__composite_overdetermined_reading, commercial_bourgeoisie, beneficiary,
    organized, generational, mobile, national).

% Men who received or issued challenges under the old code bore the compounded cost: if they dueled, they faced prosecution under the new legal regime; if they refused, they faced social death under the still-partially-live honor code during the transition window. This is the structural signature of overdetermination — trapped between two collapsing-but-not-yet-collapsed enforcement systems simultaneously.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__composite_overdetermined_reading, dueling_challenged_men, payer,
    moderate, immediate, trapped, local).

% Friends and peers obligated to serve as seconds bore criminal liability under the tightening legal regime while still facing honor-code sanction for refusing the obligation during the period when the code had not yet fully delegitimated. Their situation is direct evidence of non-independent pathways: legal risk and honor risk rose and fell together, not separately.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__composite_overdetermined_reading, seconds_and_witnesses, payer,
    moderate, immediate, constrained, local).

% Families of men killed or maimed in duels bore the economic and social cost regardless of which causal arm dominated in a given case. They had no voice in either the legal reform process or the honor-code transformation and could not exit the consequences of either mechanism.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__composite_overdetermined_reading, widows_and_dependents, payer,
    powerless, biographical, trapped, local).

% Religious and early public-health voices campaigned against dueling on grounds neither purely legal nor purely honor-internal (moral/theological argument), but this third causal channel is largely absorbed into the two dominant narratives and rarely credited as an independent contributor in either legal or cultural-historical accounts.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__composite_overdetermined_reading, clergy_and_moral_reformers, excluded,
    organized, generational, constrained, national).

% Later scholars (e.g. in the honor-to-dignity culture literature) reconstruct the decline and dispute whether legal suppression or cultural delegitimation was causally prior or whether the two were mutually reinforcing and empirically inseparable in the historical record.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__composite_overdetermined_reading, historical_sociologists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(honor_satisfaction_substrate__composite_overdetermined_reading, diffuse).
narrative_ontology:fixing_cost_class(honor_satisfaction_substrate__composite_overdetermined_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The dueling-honor complex originally solved a genuine coordination problem for status groups without reliable third-party enforcement of reputational claims: it let peers credibly signal willingness to bear costs for honor claims, deterring defamation and free-riding on reputational capital where courts were slow, biased, or beneath the class's dignity to use.
% TRANSFER_FUNCTION: Physical risk, reputational risk, and legal liability moved from the collective honor-class equilibrium onto individual challenged men and their seconds; simultaneously, adjudicatory authority and legitimacy moved from decentralized peer-enforcement toward centralized state courts and toward an emerging bourgeois dignity-based status economy.
% ABSENT_VOICES: Clergy and moral reformers argued an independent normative case against dueling that is now largely subsumed into the 'cultural transformation' narrative rather than treated as its own causal channel; the dead and their dependents left no direct testimony about whether they experienced the decline as legal rescue or cultural obsolescence — their absence is structural, not incidental, since the decisive events (duels, prosecutions) by definition removed the ability of the most affected parties to narrate their own experience.
% DISAPPEARANCE_RATIONALE: If either causal arm alone is removed from the historical account, the overdetermined reading's central empirical claim collapses: the composite reading exists specifically because eliminating either the suppression or the delegitimation narrative changes what counts as evidence for the other, and changes how contested transition-era cases (duels fought under partial legality, in jurisdictions with lagging code change) get classified. The reading itself, if wrong, rearranges how every transitional-era case is coded in the historical record.
% FOUNDING_PROBLEM: Historiographical: how to explain the empirical pattern that dueling declined roughly simultaneously with both legal criminalization AND honor-code transformation across multiple national contexts (France, Germany, the American South, Britain) with different timing and different relative intensities of each mechanism, in a way that neither purely legal nor purely cultural accounts can explain on their own.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians (courts-and-statutes evidence) corroborate the suppression arm from outside the honor-culture literature; cultural/social historians (honor-to-dignity culture scholarship, e.g. work descending from Pitt-Rivers and later dignity-culture sociology) corroborate the delegitimation arm from outside the legal-history tradition. Neither camp, taken alone, has historically endorsed the composite reading as necessary — the overdetermination claim is itself a synthesis proposed by later historical sociologists reading both literatures against each other, not an account either originating tradition offers unprompted.
narrative_ontology:disappearance_verdict(honor_satisfaction_substrate__composite_overdetermined_reading, world_rearranges).
narrative_ontology:founding_problem_status(honor_satisfaction_substrate__composite_overdetermined_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_satisfaction_substrate__composite_overdetermined_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(honor_satisfaction_substrate__composite_overdetermined_reading, 'none', 1).
narrative_ontology:epsilon_provenance(honor_satisfaction_substrate__composite_overdetermined_reading, 0.58, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is authored as moderate-rising (0.28 to 0.58) because the composite reading treats the substrate as extractive from participants throughout the transition — both legal prosecution risk and honor-code social sanction risk were real costs borne primarily by lower-status challengers and seconds, while elites captured most of the benefit of the eventual settlement in either direction. Theater ratio rises substantially (0.10 to 0.44) because as the practice declines, a growing share of remaining honor-code invocations become performative (formal apologies, published retractions, symbolic 'satisfaction') rather than functional threat-backed commitments — this is the signature the composite reading predicts and the practice_decline_reading alone would not emphasize as strongly, since it treats the code as stable rather than performatively hollowing out. Suppression_requirement is tracked because the story's central claim is about enforcement-capacity co-evolution: it rises steeply through the criminalization era (0.25 to 0.65) then plateaus and slightly declines (0.65 to 0.62) once the honor-code transformation itself begins doing enforcement work that legal machinery no longer needs to do alone — this plateau-then-slight-decline is the empirical signature of non-independence the composite reading is built to explain.
 *
 * DIRECTIONALITY LOGIC:
 *   State judicial authorities and commercial bourgeoisie sit near the beneficiary end: they gain adjudicatory authority and status-economy access respectively, with low direct cost. Dueling-challenged men, seconds, and dependents sit near the target end: they bear compounded risk from both enforcement channels operating simultaneously, with no coordinated exit available since neither the legal nor cultural exit route was fully open during the transition window. Gentry incumbents are the genuinely dual-positioned seat — hence the secondary_role — because the same historical actors experience the substrate's decline as both loss (of the honor economy that certified their rank) and gain (of admission to a safer dignity economy), and no single directionality value captures this without flattening the story's central overdetermination claim.
 *
 * MANDATROPHY ANALYSIS:
 *   The composite reading resists a common mislabeling risk in the historiography: treating the decline as either pure external coercion succeeding against a resistant tradition (which would make it look like a pure Snare being dismantled) or pure internal obsolescence (which would make it look like a Mountain quietly eroding on its own). By authoring it as a tangled_rope with active enforcement, real beneficiaries, and real victims whose costs were compounded rather than substituted across the two mechanisms, the classification captures that this was neither a clean coordination-collapse story nor a clean natural-substrate-erosion story — the mandate outlived clean explanation on either axis simultaneously, which is the overdetermination claim in classification terms.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    causal_independence_testability,
    'Can the legal-suppression and honor-code-delegitimation causal pathways be empirically disentangled using variation in timing and intensity across jurisdictions, or is the non-independence claim itself unfalsifiable given available historical data?',
    'Comparative case analysis using jurisdictions where legal suppression intensity and honor-code transformation timing diverge substantially (e.g. comparing German dueling''s late persistence under Weimar with earlier French and British decline) to test whether the composite model''s predicted co-movement holds or whether one arm can be shown to dominate independently in at least one well-documented case.',
    'If a clean case of independent-arm dominance is found, the composite_overdetermined_reading would need to be revised toward one of the sibling readings for that jurisdiction, undermining the claim of general non-independence; if no such case is found across multiple well-documented jurisdictions, the composite reading''s core claim is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(causal_independence_testability, empirical, 'Whether the two decline mechanisms are empirically separable or genuinely entangled.').

omega_variable(
    reading_selection_criterion,
    'What historiographical evidence would distinguish this composite reading from a mere academic hedge between the two cleaner sibling narratives, rather than a genuinely distinct structural claim?',
    'Examine whether the composite reading generates novel, falsifiable predictions the sibling readings do not — specifically, the predicted plateau-then-decline in suppression_requirement as honor-code transformation begins substituting for legal enforcement. If this pattern is found in the historical record independent of the composite reading''s own framing, that supports genuine distinctness.',
    'If the composite reading generates no predictions beyond the union of its siblings, it should be treated as a weighted combination rather than authored as its own constraint with its own ε — this would be a decomposition failure requiring re-authoring as a meta-commentary on the other two stories rather than a third sibling.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_selection_criterion, conceptual, 'Whether the composite reading is structurally distinct or a hedge between siblings.').

omega_variable(
    clergy_reform_channel_attribution,
    'Is the moral/theological reform channel (clergy, early public health arguments) a genuinely independent third causal pathway that both this composite reading and its siblings under-credit, or is it fully absorbed into the cultural delegitimation arm?',
    'Archival analysis of anti-dueling sermon and pamphlet literature to determine whether moral reform arguments preceded, followed, or co-occurred with the honor-to-dignity cultural shift, and whether they had independent causal traction with legislators distinct from secular honor-code arguments.',
    'If clergy-driven moral reform is shown to be causally prior and independent, the composite reading may itself be underdetermined by omitting a third channel, which would motivate a fourth sibling story rather than validating the two-arm composite as complete.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(clergy_reform_channel_attribution, empirical, 'Whether a third causal channel (moral reform) is being incorrectly folded into the cultural arm.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_satisfaction_substrate__composite_overdetermined_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hono_tr_t0, honor_satisfaction_substrate__composite_overdetermined_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(hono_tr_t20, honor_satisfaction_substrate__composite_overdetermined_reading, theater_ratio, 20, 0.18).
narrative_ontology:measurement(hono_tr_t40, honor_satisfaction_substrate__composite_overdetermined_reading, theater_ratio, 40, 0.3).
narrative_ontology:measurement(hono_tr_t60, honor_satisfaction_substrate__composite_overdetermined_reading, theater_ratio, 60, 0.38).
narrative_ontology:measurement(hono_tr_t80, honor_satisfaction_substrate__composite_overdetermined_reading, theater_ratio, 80, 0.42).
narrative_ontology:measurement(hono_tr_t100, honor_satisfaction_substrate__composite_overdetermined_reading, theater_ratio, 100, 0.44).

% Extraction over time
narrative_ontology:measurement(hono_be_t0, honor_satisfaction_substrate__composite_overdetermined_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(hono_be_t20, honor_satisfaction_substrate__composite_overdetermined_reading, base_extractiveness, 20, 0.35).
narrative_ontology:measurement(hono_be_t40, honor_satisfaction_substrate__composite_overdetermined_reading, base_extractiveness, 40, 0.47).
narrative_ontology:measurement(hono_be_t60, honor_satisfaction_substrate__composite_overdetermined_reading, base_extractiveness, 60, 0.55).
narrative_ontology:measurement(hono_be_t80, honor_satisfaction_substrate__composite_overdetermined_reading, base_extractiveness, 80, 0.58).
narrative_ontology:measurement(hono_be_t100, honor_satisfaction_substrate__composite_overdetermined_reading, base_extractiveness, 100, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(hono_su_t0, honor_satisfaction_substrate__composite_overdetermined_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(hono_su_t20, honor_satisfaction_substrate__composite_overdetermined_reading, suppression_requirement, 20, 0.4).
narrative_ontology:measurement(hono_su_t40, honor_satisfaction_substrate__composite_overdetermined_reading, suppression_requirement, 40, 0.58).
narrative_ontology:measurement(hono_su_t60, honor_satisfaction_substrate__composite_overdetermined_reading, suppression_requirement, 60, 0.65).
narrative_ontology:measurement(hono_su_t80, honor_satisfaction_substrate__composite_overdetermined_reading, suppression_requirement, 80, 0.63).
narrative_ontology:measurement(hono_su_t100, honor_satisfaction_substrate__composite_overdetermined_reading, suppression_requirement, 100, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(honor_satisfaction_substrate__composite_overdetermined_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(honor_satisfaction_substrate__composite_overdetermined_reading, 0.1).
narrative_ontology:affects_constraint(honor_satisfaction_substrate__composite_overdetermined_reading, practice_decline_reading).
narrative_ontology:affects_constraint(honor_satisfaction_substrate__composite_overdetermined_reading, cultural_contraction_reading).

% DUAL FORMULATION NOTE:
% This constraint, practice_decline_reading, and cultural_contraction_reading form a three-member constraint family reading the same honor_satisfaction_substrate kernel. practice_decline_reading treats the honor code as stable substrate and dueling's decline as pure rope-breaking under exogenous legal pressure (lower authored extractiveness, no honor-code transformation claim). cultural_contraction_reading treats the substrate itself as eroding (a mountain-erosion account with different beneficiary/victim structure emphasizing cultural elites over legal institutions). This composite reading claims the two are causally entangled rather than either being sufficient alone, and is authored with a distinct, higher extractiveness profile reflecting compounded risk to challenged men and seconds during the transition window that neither sibling captures on its own.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(honor_satisfaction_substrate__composite_overdetermined_reading, powerful, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
