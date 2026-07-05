% ============================================================================
% CONSTRAINT STORY: border_legitimacy__freedom_of_movement_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_border_legitimacy__freedom_of_movement_reading, []).

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
 *   constraint_id: border_legitimacy__freedom_of_movement_reading
 *   human_readable: Border Enforcement as Presumptively Illegitimate Restriction on Freedom of Movement
 *   domain: political_philosophy/migration_studies/international_law
 *
 * SUMMARY:
 *   This story instantiates the freedom-of-movement reading of the contested
 *   border-legitimacy kernel: freedom of movement is treated as a human right
 *   that generates a strong presumption against border restriction, so that
 *   restriction bears the burden of justification rather than admission.
 *   Under this premise, the modern border-enforcement apparatus — visa
 *   regimes, walls, detention, deportation — is a system that extracts wage
 *   premiums and life opportunity from excluded migrants and transfers them
 *   to incumbent citizens, protected labor sectors, and an enforcement
 *   industry with its own institutional interest in continued restriction.
 *   This is a distinct constraint from the sovereignty reading (which locates
 *   legitimate authority in territorial sovereignty and treats exclusion as a
 *   rightful exercise of that authority) and from the humanitarian-obligation
 *   reading (which grounds a narrower duty to admit only those fleeing
 *   persecution or disaster). The three readings are not the same constraint
 *   measured differently — each instantiates a genuinely different ε, victim
 *   set, and beneficiary structure from the same underlying border-control
 *   apparatus, per the ε-invariance principle. This file is the
 *   freedom-of-movement reading only; the sibling readings are separate
 *   constraint stories.
 *
 * KEY AGENTS:
 *   - would_be_migrants: primary target (powerless/trapped) — denied entry, bears the extraction
 *   - receiving_state_incumbent_labor_elites: primary beneficiary (organized/mobile) — protected from labor competition
 *   - border_enforcement_industry: secondary beneficiary (institutional/arbitrage) — collects enforcement budgets
 *   - receiving_state_governments: agenda_setter (institutional/analytical) — administers admission policy under sovereignty framing
 *   - migration_rights_scholars: analytical observer — documents the reading's evidentiary basis
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(border_legitimacy__freedom_of_movement_reading, 0.78).
domain_priors:suppression_score(border_legitimacy__freedom_of_movement_reading, 0.81).
domain_priors:theater_ratio(border_legitimacy__freedom_of_movement_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(border_legitimacy__freedom_of_movement_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(border_legitimacy__freedom_of_movement_reading, suppression_requirement, 0.81).
narrative_ontology:constraint_metric(border_legitimacy__freedom_of_movement_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(border_legitimacy__freedom_of_movement_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(border_legitimacy__freedom_of_movement_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(border_legitimacy__freedom_of_movement_reading, tangled_rope).
narrative_ontology:human_readable(border_legitimacy__freedom_of_movement_reading, "Border Enforcement as Presumptively Illegitimate Restriction on Freedom of Movement").
narrative_ontology:topic_domain(border_legitimacy__freedom_of_movement_reading, "political_philosophy/migration_studies/international_law").

domain_priors:requires_active_enforcement(border_legitimacy__freedom_of_movement_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(border_legitimacy__freedom_of_movement_reading, '367a2da3-fef6-41ca-9e3a-265bbf82a86c').
narrative_ontology:cs_kernel_codification('367a2da3-fef6-41ca-9e3a-265bbf82a86c', distributed).
narrative_ontology:cs_authority_grounding('367a2da3-fef6-41ca-9e3a-265bbf82a86c', distributed).
narrative_ontology:cs_reading_relation('367a2da3-fef6-41ca-9e3a-265bbf82a86c', border_legitimacy__sovereignty_reading, coexists_with).
narrative_ontology:cs_reading_relation('367a2da3-fef6-41ca-9e3a-265bbf82a86c', border_legitimacy__humanitarian_obligation_reading, influences).
narrative_ontology:cs_axiom('367a2da3-fef6-41ca-9e3a-265bbf82a86c', foundational, movement_is_a_universal_human_right).
narrative_ontology:cs_axiom_status(movement_is_a_universal_human_right, holdable).
narrative_ontology:cs_axiom_grounding('367a2da3-fef6-41ca-9e3a-265bbf82a86c', movement_is_a_universal_human_right, deontological).
narrative_ontology:cs_axiom('367a2da3-fef6-41ca-9e3a-265bbf82a86c', secondary, restriction_bears_burden_of_justification).
narrative_ontology:cs_axiom_status(restriction_bears_burden_of_justification, holdable).
narrative_ontology:cs_axiom_grounding('367a2da3-fef6-41ca-9e3a-265bbf82a86c', restriction_bears_burden_of_justification, conventional).
narrative_ontology:cs_reference_frame('367a2da3-fef6-41ca-9e3a-265bbf82a86c', pre_passport_customary_mobility).
narrative_ontology:cs_drift_state('367a2da3-fef6-41ca-9e3a-265bbf82a86c', post_nation_state_consolidation, gap(repudiation_pressure, severe, false)).
narrative_ontology:cs_created_at('367a2da3-fef6-41ca-9e3a-265bbf82a86c', '').
narrative_ontology:cs_kernel_id(border_legitimacy__freedom_of_movement_reading, border_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(border_legitimacy__freedom_of_movement_reading, receiving_state_incumbent_labor_elites).
narrative_ontology:constraint_beneficiary(border_legitimacy__freedom_of_movement_reading, border_enforcement_industry).
narrative_ontology:constraint_beneficiary(border_legitimacy__freedom_of_movement_reading, citizenship_premium_holders).
narrative_ontology:constraint_victim(border_legitimacy__freedom_of_movement_reading, would_be_migrants).
narrative_ontology:constraint_victim(border_legitimacy__freedom_of_movement_reading, displaced_workers_denied_entry).
narrative_ontology:constraint_victim(border_legitimacy__freedom_of_movement_reading, diaspora_separated_families).
narrative_ontology:constraint_victim(border_legitimacy__freedom_of_movement_reading, informal_labor_market_migrants).
narrative_ontology:constraint_vindicates(border_legitimacy__freedom_of_movement_reading, universal_freedom_of_movement_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Seek to cross a border for work, family reunification, or opportunity but are turned back, detained, or forced into irregular crossing by visa regimes and physical barriers. Under this reading, the border itself is the extraction mechanism: it converts a person's labor and mobility into a permission the receiving state sells or withholds, and enforcement (walls, patrols, deportation) is what makes the restriction stick despite no underlying claim of legitimate exclusion.
narrative_ontology:constraint_stakeholder(border_legitimacy__freedom_of_movement_reading, would_be_migrants, payer,
    powerless, biographical, trapped, global).

% Workers whose labor would be more productively deployed across the border are denied entry by quota systems and enforcement, losing wage premiums estimated in development economics literature to be the single largest addressable driver of global income disparity. They bear the cost of a restriction this reading holds has no legitimate basis.
narrative_ontology:constraint_stakeholder(border_legitimacy__freedom_of_movement_reading, displaced_workers_denied_entry, payer,
    powerless, biographical, trapped, global).

% Family members kept apart by visa backlogs, sponsorship requirements, and border denial. Reunification is treated as a discretionary grant rather than a corollary of a movement right, so the family bears an ongoing separation cost that this reading treats as an extraction with no coordination justification.
narrative_ontology:constraint_stakeholder(border_legitimacy__freedom_of_movement_reading, diaspora_separated_families, payer,
    powerless, generational, constrained, global).

% Having crossed without authorization because legal channels were closed, they work in receiving-state informal economies with no legal protection, exposed to wage theft and deportation threat used as leverage against them by employers and by the state's enforcement apparatus.
narrative_ontology:constraint_stakeholder(border_legitimacy__freedom_of_movement_reading, informal_labor_market_migrants, payer,
    powerless, immediate, trapped, national).

% Domestic labor constituencies (unions, licensed professions, protected sectors) whose wage premiums depend on excluding competing labor supply. They lobby to maintain restrictive entry as protection, benefiting directly from the border's function as a labor-supply gate.
narrative_ontology:constraint_stakeholder(border_legitimacy__freedom_of_movement_reading, receiving_state_incumbent_labor_elites, beneficiary,
    organized, biographical, mobile, national).

% Contractors, agencies, and detention operators whose budgets and institutional survival depend on continued enforcement activity. They have no stake in the coordination story succeeding or failing — only in enforcement continuing, which this reading treats as rent extraction riding on state sovereignty claims.
narrative_ontology:constraint_stakeholder(border_legitimacy__freedom_of_movement_reading, border_enforcement_industry, beneficiary,
    institutional, generational, arbitrage, national).

% Citizens of wealthy states hold, by accident of birth, a bundle of rights and access this reading treats as an unearned premium sustained precisely by the border's exclusionary function; they benefit passively from restriction without administering it.
narrative_ontology:constraint_stakeholder(border_legitimacy__freedom_of_movement_reading, citizenship_premium_holders, beneficiary,
    moderate, civilizational, mobile, global).

% Set visa policy, fund enforcement, and adjudicate admission. They frame the border as a sovereign prerogative; under this reading, the sovereignty framing is the cover story enabling a genuinely extractive labor-market and rent-protection function to persist.
narrative_ontology:constraint_stakeholder(border_legitimacy__freedom_of_movement_reading, receiving_state_governments, agenda_setter,
    institutional, generational, analytical, national).

% Bear the effects of restricted emigration (remittance loss, brain drain pressure valves closed) but have no seat in receiving-state admission policy; their objections rarely enter the receiving state's domestic political conversation.
narrative_ontology:constraint_stakeholder(border_legitimacy__freedom_of_movement_reading, sending_state_governments, excluded,
    institutional, generational, constrained, national).

% Analyze border regimes comparatively, document the economic and human costs of restriction, and argue from human-rights and welfare-economics frameworks that the presumption should run toward openness rather than exclusion.
narrative_ontology:constraint_stakeholder(border_legitimacy__freedom_of_movement_reading, migration_rights_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(border_legitimacy__freedom_of_movement_reading, diffuse).
narrative_ontology:fixing_cost_class(border_legitimacy__freedom_of_movement_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To the extent any genuine coordination exists, it is domestic labor-market and fiscal planning — states claim need to sequence admission against absorption capacity, infrastructure, and welfare-system solvency. This reading holds that function is real but narrow, and vastly oversized relative to the restriction actually imposed.
% TRANSFER_FUNCTION: Moves wage premiums, housing access, and welfare-system benefits from excluded would-be migrants (who would otherwise compete for or share them) to incumbent citizens and enforcement institutions; moves labor-market rents to protected domestic sectors; moves budget allocations to the enforcement industry.
% ABSENT_VOICES: Would-be migrants themselves are almost never direct parties to the domestic political process that sets admission policy in the receiving state; sending-state governments and diaspora communities are structurally excluded from the negotiation despite bearing large costs.
% DISAPPEARANCE_RATIONALE: If border restrictions vanished under this reading's premise, global labor markets would reorganize substantially: large-scale migration flows toward wage-premium destinations, collapse of enforcement-industry budgets, renegotiation of domestic welfare-eligibility rules, and disappearance of the informal/irregular migration category entirely (since the illegality that creates it would be gone).
% FOUNDING_PROBLEM: Historically, border control was framed as solving problems of territorial defense, epidemic control, and orderly settlement. This reading holds the founding problem was never migration itself but sovereign consolidation and, later, protection of domestic labor rents — the human-rights framing of unrestricted movement predates and was suppressed by the consolidation of the nation-state passport system in the early 20th century.
% FOUNDING_PROBLEM_CORROBORATION: Migration rights scholars and international human rights bodies (citing the Universal Declaration of Human Rights Article 13's asymmetric guarantee of exit but not entry) attest the restriction-as-necessity framing is a later historical construction, not an original or continuing necessity. Receiving-state governments and incumbent labor beneficiaries dispute this and assert the founding problem (orderly capacity management) remains live; no consensus corroboration exists outside the contest itself.
narrative_ontology:disappearance_verdict(border_legitimacy__freedom_of_movement_reading, world_rearranges).
narrative_ontology:founding_problem_status(border_legitimacy__freedom_of_movement_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(border_legitimacy__freedom_of_movement_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(border_legitimacy__freedom_of_movement_reading, 'none', 1).
narrative_ontology:epsilon_provenance(border_legitimacy__freedom_of_movement_reading, 0.78, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(border_legitimacy__freedom_of_movement_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(border_legitimacy__freedom_of_movement_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(border_legitimacy__freedom_of_movement_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (0.78) because, under this reading's premise, restriction has no independent legitimating function beyond narrow absorption-capacity coordination — most of what the border apparatus does is transfer opportunity from excluded non-citizens to incumbents. Suppression is authored even higher (0.81) because persistence depends on active enforcement infrastructure (physical barriers, detention, deportation) rather than participant consent; migrants have essentially no accessibility-collapse-driven acquiescence — they actively resist restriction (high resistance, 0.72) by irregular crossing, litigation, and political organizing, which is why accessibility_collapse is authored comparatively low (0.40): alternatives to the restriction are not fully foreclosed even though they are criminalized. Theater ratio rises over the interval (0.20 to 0.42) reflecting increasing performative enforcement (high-visibility barrier construction, symbolic deportation operations) relative to any residual coordination function as absorption-capacity arguments become harder to sustain against the scale of demonstrated labor shortages in receiving economies.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (receiving_state_governments) and the payer seats (would_be_migrants, displaced_workers_denied_entry) compute structurally differently: from the government's administrative seat the border is a sovereign coordination tool; from the excluded migrant's seat the same apparatus is enforced extraction with no coordination benefit reaching them. This divergence is exactly what the tangled_rope classification is built to register — genuine (if narrow) coordination function for incumbents, coexisting with asymmetric extraction from the excluded.
 *
 * DIRECTIONALITY LOGIC:
 *   Would-be migrants and displaced workers are declared victims with trapped exit options — they cannot access the benefit the restriction structure produces and bear its full cost, placing them near the full-target end of directionality. Incumbent labor elites and the enforcement industry are declared beneficiaries with mobile/arbitrage exit — they collect protection or budget regardless of restriction's actual necessity, placing them near the full-beneficiary end. Citizenship premium holders are beneficiaries but passive (they do not administer the restriction), which is why they are not also declared agenda_setters — the derivation should place them closer to the beneficiary end than the administering government, reflecting incidental rather than operative benefit.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding-problem interview registers genuine contestation: this reading holds the coordination-necessity justification (orderly absorption) has been substantially decoupled from the scale and form of restriction actually imposed, but stops short of declaring the mandate dead outright, since some genuine capacity-planning function may remain live in specific sectors (housing, acute service strain). The contested status prevents this reading from either whitewashing enforcement as pure coordination or dismissing all border administration as pure extraction — it isolates the extractive layer (protection rents, enforcement-industry capture) from whatever narrow coordination residue remains.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    movement_right_universality_ambiguity,
    'Is freedom of movement a genuine universal human right entailing a strong presumption against border restriction, or is it a contested normative claim that coexists with an equally defensible sovereignty-based right to exclude?',
    'Comparative analysis of international human rights instruments (UDHR Art. 13 grants exit but not entry rights explicitly), state practice, and philosophical argument across the three sibling readings of the border_legitimacy kernel; no empirical resolution exists since this is fundamentally a normative/conceptual dispute among the readings.',
    'If the freedom-of-movement premise is correct, current border enforcement is substantially extractive and the tangled_rope classification (with a real but narrow coordination residue) is appropriate. If the sovereignty premise is correct instead, the same apparatus reclassifies as legitimate exercise of coordination without the extraction framing, and the victim set here dissolves.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(movement_right_universality_ambiguity, conceptual, 'Which kernel reading of border legitimacy is normatively correct is irreducibly contested among the three sibling constraints.').

omega_variable(
    coordination_residue_scope,
    'How much of current border enforcement activity serves a genuine, narrow capacity-coordination function (housing, acute service absorption) versus how much is pure labor-market rent protection or enforcement-industry self-perpetuation?',
    'Empirical decomposition of enforcement budgets and admission-denial reasons by category; comparison of denial rates during periods of demonstrated labor shortage versus periods of genuine absorption strain.',
    'A larger coordination residue would push the classification toward a milder tangled_rope with lower ε; a vanishingly small residue would push toward snare, since the coordination story would be functioning as pure cover.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_residue_scope, empirical, 'What share of current enforcement activity is genuine coordination versus rent protection.').

omega_variable(
    sibling_reading_foreclosure_test,
    'Does the freedom-of-movement reading''s core premise (movement is a right; restriction is presumptively illegitimate) logically foreclose the sovereignty reading''s core premise (territorial sovereignty legitimately grounds a right to exclude), or can both remain live positions held by different parties within the broader kernel contest?',
    'Formal analysis of whether a single legal-political framework could simultaneously hold both premises without contradiction — most liberal-democratic constitutional orders currently hold a version of sovereignty_reading while conceding qualified admission obligations, suggesting practical coexistence rather than logical foreclosure.',
    'If foreclosure holds, adopting this reading would require treating sovereignty_reading as structurally displaced wherever this reading is adopted; if coexistence holds (as currently authored via cs_structure.reading_relations), the two remain live alternatives contested across different jurisdictions and political coalitions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure_test, conceptual, 'Whether this reading logically forecloses the sovereignty reading or merely coexists with it in ongoing political contest.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(border_legitimacy__freedom_of_movement_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bord_tr_t0, border_legitimacy__freedom_of_movement_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(bord_tr_t8, border_legitimacy__freedom_of_movement_reading, theater_ratio, 8, 0.25).
narrative_ontology:measurement(bord_tr_t16, border_legitimacy__freedom_of_movement_reading, theater_ratio, 16, 0.3).
narrative_ontology:measurement(bord_tr_t24, border_legitimacy__freedom_of_movement_reading, theater_ratio, 24, 0.35).
narrative_ontology:measurement(bord_tr_t32, border_legitimacy__freedom_of_movement_reading, theater_ratio, 32, 0.39).
narrative_ontology:measurement(bord_tr_t40, border_legitimacy__freedom_of_movement_reading, theater_ratio, 40, 0.42).

% Extraction over time
narrative_ontology:measurement(bord_be_t0, border_legitimacy__freedom_of_movement_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(bord_be_t8, border_legitimacy__freedom_of_movement_reading, base_extractiveness, 8, 0.6).
narrative_ontology:measurement(bord_be_t16, border_legitimacy__freedom_of_movement_reading, base_extractiveness, 16, 0.66).
narrative_ontology:measurement(bord_be_t24, border_legitimacy__freedom_of_movement_reading, base_extractiveness, 24, 0.71).
narrative_ontology:measurement(bord_be_t32, border_legitimacy__freedom_of_movement_reading, base_extractiveness, 32, 0.75).
narrative_ontology:measurement(bord_be_t40, border_legitimacy__freedom_of_movement_reading, base_extractiveness, 40, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(bord_su_t0, border_legitimacy__freedom_of_movement_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(bord_su_t8, border_legitimacy__freedom_of_movement_reading, suppression_requirement, 8, 0.65).
narrative_ontology:measurement(bord_su_t16, border_legitimacy__freedom_of_movement_reading, suppression_requirement, 16, 0.71).
narrative_ontology:measurement(bord_su_t24, border_legitimacy__freedom_of_movement_reading, suppression_requirement, 24, 0.75).
narrative_ontology:measurement(bord_su_t32, border_legitimacy__freedom_of_movement_reading, suppression_requirement, 32, 0.79).
narrative_ontology:measurement(bord_su_t40, border_legitimacy__freedom_of_movement_reading, suppression_requirement, 40, 0.81).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(border_legitimacy__freedom_of_movement_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(border_legitimacy__freedom_of_movement_reading, 0.08).
narrative_ontology:affects_constraint(border_legitimacy__freedom_of_movement_reading, sovereignty_reading).
narrative_ontology:affects_constraint(border_legitimacy__freedom_of_movement_reading, humanitarian_obligation_reading).

% DUAL FORMULATION NOTE:
% This constraint, sovereignty_reading, and humanitarian_obligation_reading form a three-member constraint family decomposing the natural-language concept 'border legitimacy' per the ε-invariance principle. Each reading of the border_legitimacy kernel produces a structurally distinct constraint with its own ε, beneficiary/victim set, and classification. This reading (freedom_of_movement_reading) is the most extractive of the three (highest declared ε), since it defines the widest victim set (all excluded migrants, not only asylum seekers) and the narrowest legitimating coordination function. All three stories are linked bidirectionally via affects_constraints as members of the same kernel family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
