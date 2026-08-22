% ============================================================================
% CONSTRAINT STORY: legitimacy_of_practice_standardization__exogenous_override_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legitimacy_of_practice_standardization__exogenous_override_reading, []).

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
 *   constraint_id: legitimacy_of_practice_standardization__exogenous_override_reading
 *   human_readable: State-Decreed Practice Standardization (Calendar/Dress Reform by Legal Imposition)
 *   domain: political_history/modernization_studies/institutional_change
 *
 * SUMMARY:
 *   This story instantiates the exogenous-override reading of the
 *   practice-standardization kernel: legitimacy is located in state decree
 *   issued for collective benefit (fiscal alignment, international standing,
 *   modernization prestige), enforced through law and administrative
 *   machinery rather than through voluntary adoption. The delta from the
 *   sibling readings is structural: this reading foregrounds abrupt legal
 *   imposition with enforcement infrastructure, a persistent gap between
 *   formal compliance and underground practice, and a rural population that
 *   maintains the pre-reform calendar for decades — a stable dual-practice
 *   equilibrium rather than a transitional phase toward voluntary
 *   convergence. Where the endogenous-displacement reading would locate
 *   legitimacy in the community's own uptake and the
 *   dual-practice-equilibrium reading would partition legitimacy by domain,
 *   this reading treats the decree itself, backed by enforcement, as the
 *   legitimating act — and evaluates that act's actual operation, not its
 *   self-description.
 *
 * KEY AGENTS:
 *   - modernizing_state_apparatus: primary agenda-setter and beneficiary of international legitimacy
 *   - rural_lunar_calendar_communities: primary target, bears translation labor and penalty risk
 *   - traditional_dress_practitioners: bears direct enforcement cost in public/urban settings
 *   - religious_ritual_timekeepers: displaced civic authority, excluded from drafting
 *   - local_enforcement_officials: intermediate agent, converts decree into selective enforcement and rent
 *   - historians_and_ethnographers: analytical observer documenting the compliance/practice gap
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimacy_of_practice_standardization__exogenous_override_reading, 0.68).
domain_priors:suppression_score(legitimacy_of_practice_standardization__exogenous_override_reading, 0.79).
domain_priors:theater_ratio(legitimacy_of_practice_standardization__exogenous_override_reading, 0.61).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__exogenous_override_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__exogenous_override_reading, suppression_requirement, 0.79).
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__exogenous_override_reading, theater_ratio, 0.61).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__exogenous_override_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__exogenous_override_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimacy_of_practice_standardization__exogenous_override_reading, tangled_rope).
narrative_ontology:human_readable(legitimacy_of_practice_standardization__exogenous_override_reading, "State-Decreed Practice Standardization (Calendar/Dress Reform by Legal Imposition)").
narrative_ontology:topic_domain(legitimacy_of_practice_standardization__exogenous_override_reading, "political_history/modernization_studies/institutional_change").

domain_priors:requires_active_enforcement(legitimacy_of_practice_standardization__exogenous_override_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimacy_of_practice_standardization__exogenous_override_reading, 'e07cf8aa-2a8b-4fbf-80cd-100d006a66e9').
narrative_ontology:cs_kernel_codification('e07cf8aa-2a8b-4fbf-80cd-100d006a66e9', formalized).
narrative_ontology:cs_authority_grounding('e07cf8aa-2a8b-4fbf-80cd-100d006a66e9', extraction).
narrative_ontology:cs_interpretation_layer_present('e07cf8aa-2a8b-4fbf-80cd-100d006a66e9').
narrative_ontology:cs_reading_relation('e07cf8aa-2a8b-4fbf-80cd-100d006a66e9', legitimacy_of_practice_standardization__endogenous_displacement_reading, coexists_with).
narrative_ontology:cs_reading_relation('e07cf8aa-2a8b-4fbf-80cd-100d006a66e9', legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, influences).
narrative_ontology:cs_axiom('e07cf8aa-2a8b-4fbf-80cd-100d006a66e9', foundational, state_decree_for_collective_benefit_is_sufficient_legitimation).
narrative_ontology:cs_axiom_status(state_decree_for_collective_benefit_is_sufficient_legitimation, holdable).
narrative_ontology:cs_axiom_grounding('e07cf8aa-2a8b-4fbf-80cd-100d006a66e9', state_decree_for_collective_benefit_is_sufficient_legitimation, conventional).
narrative_ontology:cs_axiom('e07cf8aa-2a8b-4fbf-80cd-100d006a66e9', secondary, enforcement_apparatus_is_evidence_of_legitimate_authority_not_extraction).
narrative_ontology:cs_axiom_status(enforcement_apparatus_is_evidence_of_legitimate_authority_not_extraction, holdable).
narrative_ontology:cs_axiom_grounding('e07cf8aa-2a8b-4fbf-80cd-100d006a66e9', enforcement_apparatus_is_evidence_of_legitimate_authority_not_extraction, instrumental).
narrative_ontology:cs_reference_frame('e07cf8aa-2a8b-4fbf-80cd-100d006a66e9', state_sovereign_modernization_mandate).
narrative_ontology:cs_drift_state('e07cf8aa-2a8b-4fbf-80cd-100d006a66e9', post_generational_enforcement_persistence, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('e07cf8aa-2a8b-4fbf-80cd-100d006a66e9', '').
narrative_ontology:cs_kernel_id(legitimacy_of_practice_standardization__exogenous_override_reading, legitimacy_of_practice_standardization).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimacy_of_practice_standardization__exogenous_override_reading, modernizing_state_apparatus).
narrative_ontology:constraint_beneficiary(legitimacy_of_practice_standardization__exogenous_override_reading, urban_administrative_class).
narrative_ontology:constraint_beneficiary(legitimacy_of_practice_standardization__exogenous_override_reading, international_trade_partners).
narrative_ontology:constraint_victim(legitimacy_of_practice_standardization__exogenous_override_reading, rural_lunar_calendar_communities).
narrative_ontology:constraint_victim(legitimacy_of_practice_standardization__exogenous_override_reading, traditional_dress_practitioners).
narrative_ontology:constraint_victim(legitimacy_of_practice_standardization__exogenous_override_reading, religious_ritual_timekeepers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(legitimacy_of_practice_standardization__exogenous_override_reading, local_enforcement_officials).
narrative_ontology:constraint_vindicates(legitimacy_of_practice_standardization__exogenous_override_reading, national_modernization_narrative).
narrative_ontology:constraint_vindicates(legitimacy_of_practice_standardization__exogenous_override_reading, fiscal_rationalization_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Decrees the calendar and dress reform by statute, citing fiscal coordination with international markets and modernization prestige. Deploys police, registrars, and school curricula to enforce compliance in visible administrative life — birth records, tax dates, contracts, official dress at state functions. Collects legitimacy and international standing from the reform regardless of whether the underlying practice actually changes.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__exogenous_override_reading, modernizing_state_apparatus, agenda_setter,
    institutional, generational, analytical, national).

% Already operates on state-aligned schedules and dress norms for career and commercial reasons; the decree merely formalizes an advantage they already held. Can navigate both registers fluently and profits from being the interpreters and enforcers of the new standard for others.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__exogenous_override_reading, urban_administrative_class, beneficiary,
    powerful, biographical, mobile, national).

% Benefit from calendar and administrative alignment that reduces transaction friction in trade, diplomacy, and finance. Do not bear any enforcement cost themselves; their preference for standardization is cited by the state as part of the justification but they take no responsibility for the domestic suppression it requires.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__exogenous_override_reading, international_trade_partners, beneficiary,
    institutional, generational, analytical, global).

% Continue to plant, harvest, marry, and worship by the lunar calendar decades after the legal decree, while performing compliance for state registries, taxes, and inspections. Carry two calendars simultaneously — one for the state, one for life — at the cost of constant translation labor and the risk of penalty if the underground practice is discovered or if it conflicts visibly with official deadlines.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__exogenous_override_reading, rural_lunar_calendar_communities, payer,
    powerless, generational, trapped, regional).

% Face fines, exclusion from state employment, or public shaming for wearing traditional dress in administrative or urban public settings; retreat to private and village contexts to maintain the practice. Bear the direct cost of enforcement sweeps and dress inspections that urban elites rarely encounter.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__exogenous_override_reading, traditional_dress_practitioners, payer,
    powerless, biographical, constrained, local).

% Maintain the lunar-liturgical calendar as an inseparable part of religious identity and communal authority; the state reform delegitimizes their calendrical role in the public sphere while leaving them no seat in the decree's drafting. Their calendar knowledge becomes underground expertise rather than a recognized civic function.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__exogenous_override_reading, religious_ritual_timekeepers, payer,
    moderate, civilizational, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(legitimacy_of_practice_standardization__exogenous_override_reading, religious_ritual_timekeepers, excluded).

% Carry out inspections, fines, and registry corrections mandated from the capital, often tolerating underground practice quietly in exchange for compliance theater during formal audits — collecting minor rents (bribes, local standing) from the gap between decree and reality.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__exogenous_override_reading, local_enforcement_officials, agenda_setter,
    organized, immediate, constrained, regional).
narrative_ontology:stakeholder_secondary_role(legitimacy_of_practice_standardization__exogenous_override_reading, local_enforcement_officials, beneficiary).

% Document the persistence of dual practice across generations, the enforcement record, and the gap between official modernization narratives and lived rural experience; take testimony from all sides without administrative power to alter the decree.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__exogenous_override_reading, historians_and_ethnographers, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aligning the state's administrative, fiscal, and diplomatic calendar/dress conventions with international standards reduces friction in trade settlement, treaty dating, and cross-border administration — a genuine coordination problem for the state's external relations and internal bureaucratic uniformity.
% TRANSFER_FUNCTION: Moves social and economic standing toward those already aligned with state-favored conventions (urban administrators, internationally-facing elites) and imposes translation labor, fines, and status penalties on rural and traditionalist populations who must maintain two parallel practice systems.
% ABSENT_VOICES: Rural lunar-calendar communities and traditional dress practitioners were not consulted in the decree's drafting; religious ritual timekeepers, whose civic calendrical authority the reform directly displaces, had no seat despite being the domain experts on the practice being overridden.
% DISAPPEARANCE_RATIONALE: State administrators and international partners would say collapse of the decree unwinds decades of coordination gains and reopens diplomatic friction. Rural communities and ritual timekeepers would say almost nothing changes for them, since the lunar calendar and traditional dress already persist underground regardless of the decree's formal status — the world they actually inhabit barely depends on it continuing.
% FOUNDING_PROBLEM: The state needed a single, internationally legible calendar and dress code to negotiate trade agreements, calculate interest and taxes on aligned fiscal years, and project a modernized image to foreign observers and lenders.
% FOUNDING_PROBLEM_CORROBORATION: The state and international trade partners attest the problem remains live — ongoing need for administrative alignment. Independent historians and ethnographers, corroborated by decades of persistent underground lunar-calendar practice documented outside government records, attest that the coordination problem was solved in the administrative domain within a generation while the enforcement apparatus continued well past that point, now serving prestige-maintenance and rent-extraction functions rather than the original coordination need.
narrative_ontology:disappearance_verdict(legitimacy_of_practice_standardization__exogenous_override_reading, contested).
narrative_ontology:founding_problem_status(legitimacy_of_practice_standardization__exogenous_override_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimacy_of_practice_standardization__exogenous_override_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(legitimacy_of_practice_standardization__exogenous_override_reading, 'none', 1).
narrative_ontology:epsilon_provenance(legitimacy_of_practice_standardization__exogenous_override_reading, 0.68, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legitimacy_of_practice_standardization__exogenous_override_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(legitimacy_of_practice_standardization__exogenous_override_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(legitimacy_of_practice_standardization__exogenous_override_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.45 to 0.68 over the interval as the initial coordination rationale (genuine fiscal/diplomatic alignment need) is progressively outlived by an enforcement apparatus that persists past the point the underlying coordination problem was solved — the classic tangled-rope signature of coordination function decaying while extraction machinery hardens. Theater ratio climbs sharply (0.20 to 0.61) because a growing share of enforcement activity becomes performative: registry corrections, ceremonial dress mandates at state functions, and compliance audits that both sides know mask continued underground practice. Suppression starts very high (0.85) during initial imposition, eases somewhat as the state normalizes bureaucratic administration of the reform (0.70 at midpoint), then rises again (0.79) as later administrations reassert enforcement to counter documented persistence of underground practice — an enforcement ratchet responding to detected non-compliance rather than escalating from a stable baseline.
 *
 * PERSPECTIVAL GAP:
 *   From the state's seat this is coordination: a single national calendar and dress code solving a real administrative and diplomatic problem. From the rural lunar-calendar communities' seat the same structure is enforced extraction with a coordination story as cover — they pay in translation labor and penalty exposure for a national administrative convenience that does not serve their local agricultural or ritual life. The engine should compute a divergent seat classification between the agenda_setter/beneficiary seats and the payer seats precisely because the coordination function (real, for cross-border administration) and the extraction (real, borne locally) both persist through the same enforcement structure — the tangled-rope signature.
 *
 * DIRECTIONALITY LOGIC:
 *   The modernizing state apparatus and urban administrative class sit at the beneficiary end: they set the rule, control its enforcement, and collect international legitimacy or career advantage regardless of the reform's actual penetration into rural life. International trade partners benefit passively without bearing any suppression cost, which is why they are listed as beneficiaries but not agenda-setters — they did not build the enforcement machinery, but the decree's justification depends on their perceived preference. Rural lunar-calendar communities, traditional dress practitioners, and religious ritual timekeepers are targets: trapped, constrained, or identity-locked exit options reflect that leaving the underground practice is not a live option (it is constitutive of communal and religious identity) and leaving the jurisdiction is not realistic for most. Local enforcement officials occupy an intermediate position — agents of the state who nonetheless extract minor local rents from tolerating the gap they are nominally tasked with closing.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (need for internationally legible administrative timekeeping) was substantially achieved within roughly a generation of the decree, once urban administration, trade documentation, and treaty dating had converged. Enforcement against rural and ritual practice continued well past that point, sustained now by prestige-maintenance and the state's reluctance to formally concede that the 'modernization' project did not fully displace the older practice. This is not simple mandatrophy (function fully dead, arrangement a hollow shell) because the international-alignment coordination function remains genuinely live at the administrative layer — it is a mixed case: live coordination function at one layer, dead/converted-to-extraction function at the layer that touches rural populations, which the tangled_rope classification (rather than piton or pure snare) is meant to capture.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coercion_vs_coordination_weighting,
    'How much of the sustained enforcement apparatus, decades after the decree, is genuinely necessary to preserve the international-alignment coordination gain versus purely defending state prestige and administrative face-saving?',
    'Comparative study of jurisdictions that formally legalized dual-calendar or dual-dress systems (domain partition) versus those that maintained exclusive-legal-standard enforcement: compare trade/diplomatic outcomes and enforcement cost trajectories.',
    'If formal dual-system jurisdictions achieve equivalent coordination benefit at lower enforcement cost, the continued suppression in exogenous-override jurisdictions is revealed as excess extraction rather than necessary coordination overhead — strengthening the tangled_rope reading over any rope reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coercion_vs_coordination_weighting, empirical, 'Whether continued enforcement past the coordination-need point is necessary or purely extractive.').

omega_variable(
    reading_selection_and_which_reading_the_record_supports,
    'Given three declared readings of this kernel (exogenous_override, endogenous_displacement, dual_practice_equilibrium), does the empirical record of persistent decades-long underground lunar-calendar practice better fit this reading''s premise (illegitimate imposition sustained by force) or the dual_practice_equilibrium reading''s premise (a legitimate, stable domain partition that was simply never formally recognized)?',
    'Examine whether the state ever formally or informally tolerated the underground practice (supporting dual_practice_equilibrium) versus continuing active punitive enforcement against it when discovered (supporting exogenous_override as authored here). Local enforcement officials'' documented selective tolerance is ambiguous evidence for either reading.',
    'If informal toleration is the dominant pattern, the dual_practice_equilibrium reading may better describe the lived arrangement, and this story''s higher suppression/extraction values would overstate the case; if punitive enforcement dominates when discovered, this reading''s values are the better-supported account.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_selection_and_which_reading_the_record_supports, conceptual, 'Committer-frame ambiguity: which sibling reading the same historical record actually supports is itself contested, and the choice of reading determines which constraint (this one, or the dual_practice_equilibrium sibling) is the correct structural account.').

omega_variable(
    state_legitimacy_claim_naturalness,
    'Is ''state decree for collective benefit'' a genuinely neutral standard of legitimacy, or does the very framing of the reform as serving collective/national benefit already encode the beneficiary group''s interests (urban administrators, international-facing elites) as the default definition of ''the collective''?',
    'Analyze whether alternative decrees more favorable to rural/traditional populations were considered and rejected, or whether the collective-benefit framing was never contested within the state''s own deliberative record.',
    'If the collective-benefit framing systematically excluded rural interests from the definition of ''collective'' from the outset, this substantially strengthens the case that the coordination story functions primarily as cover for asymmetric extraction, reinforcing the tangled_rope (or even snare-leaning) classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(state_legitimacy_claim_naturalness, conceptual, 'Whether the state''s definition of collective benefit already presupposes the urban/international beneficiary set.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimacy_of_practice_standardization__exogenous_override_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legi_tr_t0, legitimacy_of_practice_standardization__exogenous_override_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(legi_tr_t10, legitimacy_of_practice_standardization__exogenous_override_reading, theater_ratio, 10, 0.35).
narrative_ontology:measurement(legi_tr_t20, legitimacy_of_practice_standardization__exogenous_override_reading, theater_ratio, 20, 0.48).
narrative_ontology:measurement(legi_tr_t30, legitimacy_of_practice_standardization__exogenous_override_reading, theater_ratio, 30, 0.55).
narrative_ontology:measurement(legi_tr_t40, legitimacy_of_practice_standardization__exogenous_override_reading, theater_ratio, 40, 0.58).
narrative_ontology:measurement(legi_tr_t50, legitimacy_of_practice_standardization__exogenous_override_reading, theater_ratio, 50, 0.6).
narrative_ontology:measurement(legi_tr_t60, legitimacy_of_practice_standardization__exogenous_override_reading, theater_ratio, 60, 0.61).

% Extraction over time
narrative_ontology:measurement(legi_be_t0, legitimacy_of_practice_standardization__exogenous_override_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(legi_be_t10, legitimacy_of_practice_standardization__exogenous_override_reading, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(legi_be_t20, legitimacy_of_practice_standardization__exogenous_override_reading, base_extractiveness, 20, 0.64).
narrative_ontology:measurement(legi_be_t30, legitimacy_of_practice_standardization__exogenous_override_reading, base_extractiveness, 30, 0.66).
narrative_ontology:measurement(legi_be_t40, legitimacy_of_practice_standardization__exogenous_override_reading, base_extractiveness, 40, 0.67).
narrative_ontology:measurement(legi_be_t50, legitimacy_of_practice_standardization__exogenous_override_reading, base_extractiveness, 50, 0.68).
narrative_ontology:measurement(legi_be_t60, legitimacy_of_practice_standardization__exogenous_override_reading, base_extractiveness, 60, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(legi_su_t0, legitimacy_of_practice_standardization__exogenous_override_reading, suppression_requirement, 0, 0.85).
narrative_ontology:measurement(legi_su_t10, legitimacy_of_practice_standardization__exogenous_override_reading, suppression_requirement, 10, 0.82).
narrative_ontology:measurement(legi_su_t20, legitimacy_of_practice_standardization__exogenous_override_reading, suppression_requirement, 20, 0.76).
narrative_ontology:measurement(legi_su_t30, legitimacy_of_practice_standardization__exogenous_override_reading, suppression_requirement, 30, 0.72).
narrative_ontology:measurement(legi_su_t40, legitimacy_of_practice_standardization__exogenous_override_reading, suppression_requirement, 40, 0.7).
narrative_ontology:measurement(legi_su_t50, legitimacy_of_practice_standardization__exogenous_override_reading, suppression_requirement, 50, 0.79).
narrative_ontology:measurement(legi_su_t60, legitimacy_of_practice_standardization__exogenous_override_reading, suppression_requirement, 60, 0.79).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimacy_of_practice_standardization__exogenous_override_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(legitimacy_of_practice_standardization__exogenous_override_reading, legitimacy_of_practice_standardization__endogenous_displacement_reading).
narrative_ontology:affects_constraint(legitimacy_of_practice_standardization__exogenous_override_reading, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the legitimacy_of_practice_standardization kernel, each authored as a separate story with its own epsilon per the epsilon-invariance principle. This reading (exogenous_override) authors substantially higher extractiveness (0.68) and suppression (0.79) than a dual_practice_equilibrium reading would, because this reading treats the enforcement apparatus and the persistent underground practice as evidence of ongoing coercive cost rather than as a settled, non-extractive domain partition. The endogenous_displacement reading, by contrast, would author near-zero extraction for any practice change that occurred, since it only counts change driven by voluntary uptake as legitimate change at all — under that reading's own lights, the state-imposed calendar reform where it failed to produce voluntary uptake would not even register as the practice-change event being evaluated.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
