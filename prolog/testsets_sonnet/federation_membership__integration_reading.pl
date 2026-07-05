% ============================================================================
% CONSTRAINT STORY: federation_membership__integration_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_federation_membership__integration_reading, []).

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
 *   constraint_id: federation_membership__integration_reading
 *   human_readable: Federation Membership as Irreversible Integration (Free Movement as Constitutional Right)
 *   domain: political/economic — federalism and migration policy
 *
 * SUMMARY:
 *   This story instantiates the integration reading of the
 *   federation-membership kernel: membership, once acquired, is treated as
 *   constitutionally irreversible; supranational institutions hold legitimate
 *   authority to override member-state border policy; and free movement of
 *   persons is a constitutional right rather than a negotiable treaty term
 *   subject to member-state veto. Under this reading, mobile citizens and
 *   cross-border employers are structural beneficiaries of a continent-wide
 *   labor market, while local labor markets, receiving-region public
 *   services, and immobile incumbent workers absorb the displacement costs
 *   without a corresponding political lever to slow or condition inflows.
 *   This is a distinct constraint from the sovereignty reading
 *   (constraint_id: not generated in this file, referenced via network edge)
 *   in which membership is a conditional treaty, national border authority is
 *   retained, and free movement is negotiable policy subject to national
 *   safeguard clauses — that reading has a different beneficiary/victim
 *   structure and a substantially lower ε, because border restriction is
 *   treated as legitimate policy rather than a constitutional violation. The
 *   two readings are not the same constraint measured differently; they are
 *   two constraints sharing a kernel.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federation_membership__integration_reading, 0.68).
domain_priors:suppression_score(federation_membership__integration_reading, 0.62).
domain_priors:theater_ratio(federation_membership__integration_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federation_membership__integration_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(federation_membership__integration_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(federation_membership__integration_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(federation_membership__integration_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(federation_membership__integration_reading, resistance, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federation_membership__integration_reading, tangled_rope).
narrative_ontology:human_readable(federation_membership__integration_reading, "Federation Membership as Irreversible Integration (Free Movement as Constitutional Right)").
narrative_ontology:topic_domain(federation_membership__integration_reading, "political/economic — federalism and migration policy").

domain_priors:requires_active_enforcement(federation_membership__integration_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federation_membership__integration_reading, '6633d957-3f55-4bd6-9009-df7eab79d567').
narrative_ontology:cs_kernel_codification('6633d957-3f55-4bd6-9009-df7eab79d567', formalized).
narrative_ontology:cs_authority_grounding('6633d957-3f55-4bd6-9009-df7eab79d567', lineage).
narrative_ontology:cs_interpretation_layer_present('6633d957-3f55-4bd6-9009-df7eab79d567').
narrative_ontology:cs_reading_relation('6633d957-3f55-4bd6-9009-df7eab79d567', federation_membership__sovereignty_reading, forecloses).
narrative_ontology:cs_axiom('6633d957-3f55-4bd6-9009-df7eab79d567', foundational, membership_integration_irreversible).
narrative_ontology:cs_axiom_status(membership_integration_irreversible, holdable).
narrative_ontology:cs_axiom_grounding('6633d957-3f55-4bd6-9009-df7eab79d567', membership_integration_irreversible, conventional).
narrative_ontology:cs_axiom('6633d957-3f55-4bd6-9009-df7eab79d567', foundational, free_movement_is_constitutional_right_not_policy).
narrative_ontology:cs_axiom_status(free_movement_is_constitutional_right_not_policy, holdable).
narrative_ontology:cs_axiom_grounding('6633d957-3f55-4bd6-9009-df7eab79d567', free_movement_is_constitutional_right_not_policy, deontological).
narrative_ontology:cs_reference_frame('6633d957-3f55-4bd6-9009-df7eab79d567', founding_treaty_conflict_prevention_mandate).
narrative_ontology:cs_drift_state('6633d957-3f55-4bd6-9009-df7eab79d567', mature_integration_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('6633d957-3f55-4bd6-9009-df7eab79d567', '').
narrative_ontology:cs_kernel_id(federation_membership__integration_reading, federation_membership).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federation_membership__integration_reading, mobile_federation_citizens).
narrative_ontology:constraint_beneficiary(federation_membership__integration_reading, cross_border_employers).
narrative_ontology:constraint_beneficiary(federation_membership__integration_reading, federation_central_institutions).
narrative_ontology:constraint_victim(federation_membership__integration_reading, local_labor_markets).
narrative_ontology:constraint_victim(federation_membership__integration_reading, receiving_region_public_services).
narrative_ontology:constraint_victim(federation_membership__integration_reading, low_mobility_incumbent_workers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(federation_membership__integration_reading, member_state_governments).
narrative_ontology:constraint_vindicates(federation_membership__integration_reading, supranational_authority_doctrine).
narrative_ontology:constraint_vindicates(federation_membership__integration_reading, irreversible_integration_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Can relocate across member-state borders to work, study, or retire without visas or labor-market tests, treating the entire federation as a single opportunity space. The free-movement guarantee is read as a constitutional right that no member state can suspend unilaterally, so their exit options across the whole federation are effectively arbitrage-grade even when their situation in any single locality is precarious.
narrative_ontology:constraint_stakeholder(federation_membership__integration_reading, mobile_federation_citizens, beneficiary,
    moderate, biographical, arbitrage, continental).

% Draw on a continent-wide labor pool without immigration friction, driving down recruitment costs and giving them leverage to source labor from wherever wages are lowest within the federation. They lobby to keep free movement irreversible because it is the source of their flexible labor supply.
narrative_ontology:constraint_stakeholder(federation_membership__integration_reading, cross_border_employers, beneficiary,
    organized, generational, mobile, continental).

% Administer and adjudicate free-movement law, strike down member-state border restrictions as violations of the founding treaties, and treat integration as a one-way ratchet — accession is framed as constitutionally irreversible, and any member-state attempt to reintroduce border controls is litigated as illegitimate. Their institutional survival and expanding mandate depend on the doctrine holding.
narrative_ontology:constraint_stakeholder(federation_membership__integration_reading, federation_central_institutions, agenda_setter,
    institutional, civilizational, analytical, continental).

% Wage levels and job availability in receiving regions shift when inflows of mobile labor arrive faster than local demand absorbs them. Workers here cannot vote to slow inflows because the movement right is constitutionally insulated from ordinary regional or national politics; their only recourse is federation-level litigation or protest, both slow and structurally disadvantaged against a settled treaty right.
narrative_ontology:constraint_stakeholder(federation_membership__integration_reading, local_labor_markets, payer,
    powerless, biographical, trapped, regional).

% Schools, housing, and healthcare systems in high-inflow regions absorb demand surges without receiving compensating fiscal transfers keyed to actual movement patterns, because the treaty framework guarantees the right to move but does not mandate proportional resource redistribution. Local administrators can petition for funds but cannot restrict the inflow itself.
narrative_ontology:constraint_stakeholder(federation_membership__integration_reading, receiving_region_public_services, payer,
    moderate, biographical, constrained, regional).

% Workers without the capital, language skills, or family flexibility to relocate themselves are structurally unable to exercise the same free-movement right that benefits mobile citizens and employers, yet they absorb the downward wage pressure and service strain that mobility produces in their locality. Their exit option is nominally the same constitutional right everyone holds, but is not practically available to them.
narrative_ontology:constraint_stakeholder(federation_membership__integration_reading, low_mobility_incumbent_workers, payer,
    powerless, biographical, trapped, local).

% Face domestic political pressure to restrict inflows or protect local labor markets but are constitutionally barred from unilaterally reimposing border controls once integrated; any attempt is a treaty violation adjudicated by federation courts against them. Their voice on movement policy is procedurally present but substantively foreclosed by the irreversibility doctrine.
narrative_ontology:constraint_stakeholder(federation_membership__integration_reading, member_state_governments, excluded,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(federation_membership__integration_reading, member_state_governments, payer).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(federation_membership__integration_reading, cross_border_employers).
narrative_ontology:fixing_cost_class(federation_membership__integration_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables a genuinely large, integrated labor and consumer market: firms can hire and citizens can move across what would otherwise be dozens of separate immigration regimes, capturing efficiency gains from continent-wide allocation of labor and capital.
% TRANSFER_FUNCTION: Moves labor-market rents from local incumbent workers and public-service capacity in receiving regions to mobile citizens and the employers who hire them, while shifting political authority over border and labor policy from member states to central federation institutions.
% ABSENT_VOICES: Low-mobility incumbent workers and receiving-region municipal administrators have no seat in the treaty-interpretation process; their wage and service-strain concerns are raised, if at all, through national governments that are themselves constitutionally constrained from acting on them.
% DISAPPEARANCE_RATIONALE: If the constitutional free-movement guarantee and its irreversibility doctrine disappeared overnight, member states would reintroduce labor-market tests and border checks, cross-border employers would lose frictionless access to continental labor, mobile citizens would face new barriers, and local labor markets would regain a lever they currently lack — the federation's internal economic geography would visibly reorganize.
% FOUNDING_PROBLEM: Post-war/post-conflict fragmentation of the continent into small, mutually suspicious national markets was seen as economically inefficient and politically dangerous; free movement was built to bind member states together so tightly that renewed conflict or economic nationalism would become structurally costly.
% FOUNDING_PROBLEM_CORROBORATION: Federation central institutions and cross-border employer associations attest the founding problem (fragmentation, conflict risk) remains live and justifies irreversibility. Independent labor economists and receiving-region municipal associations — outside the beneficiary set — attest the peace-and-fragmentation problem is largely solved and the irreversibility doctrine now functions primarily to insulate an economically asymmetric labor-mobility regime from democratic revision.
narrative_ontology:disappearance_verdict(federation_membership__integration_reading, world_rearranges).
narrative_ontology:founding_problem_status(federation_membership__integration_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(federation_membership__integration_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(federation_membership__integration_reading, 'none', 1).
narrative_ontology:epsilon_provenance(federation_membership__integration_reading, 0.68, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(federation_membership__integration_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(federation_membership__integration_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(federation_membership__integration_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises over the measured interval (0.38 to 0.68) as cumulative labor-mobility flows compound: early-stage integration produces modest, broadly shared gains, but as mobility volumes grow and asymmetric wage differentials persist across the federation, the transfer from local labor markets to mobile citizens and employers becomes structurally larger and harder to reverse (consistent with the irreversibility doctrine itself foreclosing corrective policy). Suppression tracks upward (0.40 to 0.62) as central institutions increasingly strike down member-state attempts at emergency labor-market safeguards, hardening the doctrine's enforcement posture. Theater ratio stays comparatively low (0.10 to 0.22) because the coordination function (a genuinely integrated market) is real and substantially functioning, not primarily performative — this is not a piton, it is an actively extractive tangled rope.
 *
 * PERSPECTIVAL GAP:
 *   From the mobile-citizen and employer seats, the constraint is experienced as liberty and market efficiency — a rope, near-Pareto-improving. From the local-labor-market and incumbent-worker seats, the same structure is an enforced transfer with no exit and no vote — closer to snare-like extraction. The engine computes both seats' types from the same structural data; the divergence is exactly what a single 'is it good or bad' verdict would erase.
 *
 * DIRECTIONALITY LOGIC:
 *   Mobile citizens and cross-border employers sit near the beneficiary end: they capture the surplus of an integrated labor market and hold arbitrage/mobile exit options across the whole federation. Federation central institutions are structural agenda-setters whose authority and mandate expand with the doctrine's entrenchment. Local labor markets, receiving-region public services, and low-mobility incumbent workers sit near the target end: they are structurally trapped (no comparable exit — they cannot 'move' the labor market away from the pressure) and bear costs without a corresponding political remedy, because the same constitutional right that benefits mobile citizens forecloses the border-restriction lever that would otherwise let them negotiate relief.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (post-conflict fragmentation, war-prevention through economic entanglement) is genuinely partially solved — the federation has not seen intra-member conflict for decades — while the free-movement/irreversibility apparatus has expanded well past what conflict-prevention alone would require, now functioning as a standing labor-supply mechanism for cross-border employers. Classifying this as tangled_rope rather than snare preserves the genuine coordination residue (market integration is real and mutually valuable in aggregate) while still registering the asymmetric extraction from immobile incumbents — collapsing it to pure snare would erase the authentic coordination function; collapsing it to pure rope would erase the documented victim class.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    irreversibility_as_constructed_vs_natural,
    'Is the irreversibility of federation membership a genuine structural/legal fact (once integrated, disintegration is technically and economically catastrophic) or a constructed doctrine that central institutions maintain because their authority depends on it being unchallengeable?',
    'Examine historical cases of partial or full member-state withdrawal or renegotiation: if withdrawal is technically and legally executable (even at high cost) without triggering doctrine-defined catastrophe, irreversibility is closer to constructed; if disintegration proves genuinely unmanageable in practice, irreversibility has a stronger structural claim.',
    'If irreversibility is substantially constructed, the tangled_rope classification understates suppression — the doctrine functions partly to foreclose a live political option, which strengthens the case for treating central institutions as closer to a beneficiary-agenda-setter than a neutral adjudicator.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(irreversibility_as_constructed_vs_natural, conceptual, 'Whether treaty irreversibility is a structural fact or a maintained doctrine.').

omega_variable(
    kernel_reading_selection_evidence,
    'What structural or political signals justify treating this specific case as an instance of the integration_reading rather than the sovereignty_reading of federation membership?',
    'Examine which reading is actually operative in current adjudicated case law and treaty enforcement practice: if central-institution rulings consistently strike down member-state border/labor restrictions as illegitimate, the integration_reading is the operative constraint; if member states retain enforceable safeguard clauses that are regularly exercised, the sovereignty_reading is operative instead — the two are not merely rhetorical framings but produce measurably different enforcement records.',
    'If the empirical enforcement record more closely matches the sovereignty_reading (safeguard clauses regularly invoked and upheld), this integration_reading constraint would be a poor structural fit for the actual federation and the sovereignty_reading file would be the more accurate description of present reality, though both remain valid as distinct possible constitutional arrangements.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection_evidence, empirical, 'What evidence distinguishes which kernel reading is operative in a given federation''s actual practice.').

omega_variable(
    labor_displacement_measurement_ambiguity,
    'How much of the wage and service-strain effect on local labor markets is attributable to free movement specifically, versus automation, trade exposure, or domestic fiscal policy operating independently?',
    'Comparative regional analysis isolating labor-mobility inflows from other confounding economic shocks in receiving regions with similar industrial composition but different mobility exposure.',
    'If displacement effects are substantially confounded with other factors, the authored extractiveness trajectory may overstate the causal contribution of free movement specifically, though the structural asymmetry (beneficiaries can vote/litigate to preserve the arrangement; payers largely cannot) would remain regardless of the precise causal share.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(labor_displacement_measurement_ambiguity, empirical, 'Causal attribution uncertainty in the extractiveness trajectory.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership__integration_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fede_tr_t0, federation_membership__integration_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(fede_tr_t8, federation_membership__integration_reading, theater_ratio, 8, 0.12).
narrative_ontology:measurement(fede_tr_t16, federation_membership__integration_reading, theater_ratio, 16, 0.15).
narrative_ontology:measurement(fede_tr_t24, federation_membership__integration_reading, theater_ratio, 24, 0.18).
narrative_ontology:measurement(fede_tr_t32, federation_membership__integration_reading, theater_ratio, 32, 0.2).
narrative_ontology:measurement(fede_tr_t40, federation_membership__integration_reading, theater_ratio, 40, 0.22).

% Extraction over time
narrative_ontology:measurement(fede_be_t0, federation_membership__integration_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(fede_be_t8, federation_membership__integration_reading, base_extractiveness, 8, 0.46).
narrative_ontology:measurement(fede_be_t16, federation_membership__integration_reading, base_extractiveness, 16, 0.53).
narrative_ontology:measurement(fede_be_t24, federation_membership__integration_reading, base_extractiveness, 24, 0.59).
narrative_ontology:measurement(fede_be_t32, federation_membership__integration_reading, base_extractiveness, 32, 0.64).
narrative_ontology:measurement(fede_be_t40, federation_membership__integration_reading, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(fede_su_t0, federation_membership__integration_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(fede_su_t8, federation_membership__integration_reading, suppression_requirement, 8, 0.46).
narrative_ontology:measurement(fede_su_t16, federation_membership__integration_reading, suppression_requirement, 16, 0.51).
narrative_ontology:measurement(fede_su_t24, federation_membership__integration_reading, suppression_requirement, 24, 0.55).
narrative_ontology:measurement(fede_su_t32, federation_membership__integration_reading, suppression_requirement, 32, 0.59).
narrative_ontology:measurement(fede_su_t40, federation_membership__integration_reading, suppression_requirement, 40, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(federation_membership__integration_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(federation_membership__integration_reading, 0.15).
narrative_ontology:affects_constraint(federation_membership__integration_reading, federation_membership__sovereignty_reading).

% DUAL FORMULATION NOTE:
% This story (integration_reading) and federation_membership__sovereignty_reading are siblings decomposing the natural-language concept 'federation membership' per the ε-invariance principle. They share a kernel (federation_membership) but differ in beneficiary/victim structure, legitimacy claims about border authority, and measured ε — the integration reading runs substantially more extractive (0.68 at interval end) because it treats border restriction as constitutionally illegitimate, foreclosing the remedy that would otherwise cap the transfer to local labor markets. Do not average or reconcile the two ε values; they are properties of two different constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
