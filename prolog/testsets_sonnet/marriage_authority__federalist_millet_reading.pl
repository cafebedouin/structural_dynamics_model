% ============================================================================
% CONSTRAINT STORY: marriage_authority__federalist_millet_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_authority__federalist_millet_reading, []).

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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: marriage_authority__federalist_millet_reading
 *   human_readable: Fragmented Marriage Authority as Consociational Anti-Tyranny Mechanism
 *   domain: legal_pluralism/constitutional_law/comparative_family_law
 *
 * SUMMARY:
 *   This story reads the fragmentation of marriage authority across communal
 *   lines not as a residue of colonial-era religious deference (the
 *   communal_autonomy_reading) but as a deliberate elite-bargained
 *   anti-tyranny device: a consociational architecture in which distributing
 *   family-law jurisdiction across communities is the price paid, and the
 *   guarantee purchased, for a stable governing coalition that no single
 *   demographic majority can use to legislate away minority family-law
 *   practice. Under this reading, legislative paralysis on a uniform civil
 *   code is not dysfunction but the mechanism working as designed — the
 *   inability of the national legislature to act on marriage law IS the
 *   anti-majoritarian safeguard. The ε here is deliberately low: the
 *   coordination function (preventing majoritarian legislative capture of
 *   family law) is genuine, extraction is present but modest and concentrated
 *   in the friction costs borne by dissenters and cross-community couples,
 *   and no active state suppression apparatus is required to sustain it — the
 *   arrangement is self-sustaining through the coalition's own structure
 *   rather than through coercive enforcement.
 *
 * KEY AGENTS:
 *   - minority_religious_communities: primary beneficiary (organized/constrained) — retains self-governing family law jurisdiction
 *   - consociational_political_elites: agenda-setter (institutional/arbitrage) — brokers and maintains the bargain
 *   - communal_leadership_bodies: beneficiary/agenda-setter (organized/constrained) — administers personal law, derives standing from fragmentation
 *   - intra_community_dissenters: primary payer (powerless/trapped) — no external forum for internal rights claims
 *   - cross_community_couples: secondary payer (moderate/constrained) — falls into under-designed residual regime
 *   - national_legislature: excluded (institutional/constrained) — majoritarian voice structurally neutralized by design
 *   - constitutional_courts: analytical observer (institutional/analytical) — adjudicates seams without mandate to dissolve fragmentation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_authority__federalist_millet_reading, 0.28).
domain_priors:suppression_score(marriage_authority__federalist_millet_reading, 0.22).
domain_priors:theater_ratio(marriage_authority__federalist_millet_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_authority__federalist_millet_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(marriage_authority__federalist_millet_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(marriage_authority__federalist_millet_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_authority__federalist_millet_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(marriage_authority__federalist_millet_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_authority__federalist_millet_reading, rope).
narrative_ontology:human_readable(marriage_authority__federalist_millet_reading, "Fragmented Marriage Authority as Consociational Anti-Tyranny Mechanism").
narrative_ontology:topic_domain(marriage_authority__federalist_millet_reading, "legal_pluralism/constitutional_law/comparative_family_law").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_authority__federalist_millet_reading, 'f0de871d-5a54-47a7-81b8-f308ca44507f').
narrative_ontology:cs_kernel_codification('f0de871d-5a54-47a7-81b8-f308ca44507f', distributed).
narrative_ontology:cs_authority_grounding('f0de871d-5a54-47a7-81b8-f308ca44507f', distributed).
narrative_ontology:cs_reading_relation('f0de871d-5a54-47a7-81b8-f308ca44507f', marriage_authority__communal_autonomy_reading, coexists_with).
narrative_ontology:cs_reading_relation('f0de871d-5a54-47a7-81b8-f308ca44507f', marriage_authority__secularist_reading, influences).
narrative_ontology:cs_reading_relation('f0de871d-5a54-47a7-81b8-f308ca44507f', marriage_authority__gender_rights_reading, influences).
narrative_ontology:cs_reading_relation('f0de871d-5a54-47a7-81b8-f308ca44507f', marriage_authority__judicial_harmonization_reading, coexists_with).
narrative_ontology:cs_axiom('f0de871d-5a54-47a7-81b8-f308ca44507f', foundational, fragmentation_is_deliberate_anti_majoritarian_design).
narrative_ontology:cs_axiom_status(fragmentation_is_deliberate_anti_majoritarian_design, holdable).
narrative_ontology:cs_axiom_grounding('f0de871d-5a54-47a7-81b8-f308ca44507f', fragmentation_is_deliberate_anti_majoritarian_design, conventional).
narrative_ontology:cs_axiom('f0de871d-5a54-47a7-81b8-f308ca44507f', secondary, legislative_paralysis_is_stability_not_failure).
narrative_ontology:cs_axiom_status(legislative_paralysis_is_stability_not_failure, holdable).
narrative_ontology:cs_axiom_grounding('f0de871d-5a54-47a7-81b8-f308ca44507f', legislative_paralysis_is_stability_not_failure, instrumental).
narrative_ontology:cs_reference_frame('f0de871d-5a54-47a7-81b8-f308ca44507f', founding_era_anti_majoritarian_settlement).
narrative_ontology:cs_drift_state('f0de871d-5a54-47a7-81b8-f308ca44507f', contemporary_constitutional_equality_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('f0de871d-5a54-47a7-81b8-f308ca44507f', '').
narrative_ontology:cs_kernel_id(marriage_authority__federalist_millet_reading, marriage_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_authority__federalist_millet_reading, minority_religious_communities).
narrative_ontology:constraint_beneficiary(marriage_authority__federalist_millet_reading, consociational_political_elites).
narrative_ontology:constraint_beneficiary(marriage_authority__federalist_millet_reading, communal_leadership_bodies).
narrative_ontology:constraint_victim(marriage_authority__federalist_millet_reading, intra_community_dissenters).
narrative_ontology:constraint_victim(marriage_authority__federalist_millet_reading, cross_community_couples).
narrative_ontology:constraint_vindicates(marriage_authority__federalist_millet_reading, anti_majoritarian_constitutional_design_doctrine).
narrative_ontology:constraint_vindicates(marriage_authority__federalist_millet_reading, consociational_stability_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Retain their own personal law regime governing marriage, divorce, and inheritance instead of being subject to a majority-authored uniform code. The fragmentation itself is what protects the community's ability to self-govern family matters against a legislature where the community is numerically outvoted.
narrative_ontology:constraint_stakeholder(marriage_authority__federalist_millet_reading, minority_religious_communities, beneficiary,
    organized, generational, constrained, national).

% Negotiate and maintain the political bargain that keeps marriage authority split across community lines rather than centralized. They benefit from the durability of the coalition this fragmentation makes possible, and from being the brokers whose continued mediation the arrangement requires. Legislative paralysis on a uniform code is, from this seat, evidence the bargain is holding, not evidence of dysfunction.
narrative_ontology:constraint_stakeholder(marriage_authority__federalist_millet_reading, consociational_political_elites, agenda_setter,
    institutional, generational, arbitrage, national).

% Administer their community's personal law and adjudicate marriage disputes internally, deriving institutional authority and standing directly from the state's decision not to centralize family law. Their continued relevance depends on the fragmentation persisting.
narrative_ontology:constraint_stakeholder(marriage_authority__federalist_millet_reading, communal_leadership_bodies, beneficiary,
    organized, civilizational, constrained, national).
narrative_ontology:stakeholder_secondary_role(marriage_authority__federalist_millet_reading, communal_leadership_bodies, agenda_setter).

% Individuals — often women, often younger members challenging communal norms — who want a different marriage or divorce outcome than their community's personal law provides. The consociational bargain that protects the community from majoritarian override also insulates the community's internal rules from external constitutional challenge, leaving these dissenters with no forum outside the very structure they are contesting.
narrative_ontology:constraint_stakeholder(marriage_authority__federalist_millet_reading, intra_community_dissenters, payer,
    powerless, biographical, trapped, local).

% Couples from different personal-law communities face jurisdictional friction, conflicting rules on marriage validity and dissolution, and often must resort to a residual civil marriage regime that carries social stigma or procedural cost. The fragmentation that protects each community's autonomy produces a gap no single community's law is designed to fill.
narrative_ontology:constraint_stakeholder(marriage_authority__federalist_millet_reading, cross_community_couples, payer,
    moderate, biographical, constrained, national).

% Retains formal constitutional competence to legislate a uniform code but is structurally prevented from exercising it by the same consociational bargain that keeps the coalition governable. Majoritarian legislative will on this question is the thing the arrangement is built to neutralize, so the legislature's own voice on marriage law is the one systematically absent from the outcome.
narrative_ontology:constraint_stakeholder(marriage_authority__federalist_millet_reading, national_legislature, excluded,
    institutional, generational, constrained, national).

% Periodically asked to adjudicate individual claims arising at the seams of the fragmented system — cross-community disputes, dissenter petitions — without a mandate to dissolve the fragmentation itself. Observes the stability the arrangement produces at the aggregate level and the friction it produces at the individual level, without institutional authority to reconcile the two.
narrative_ontology:constraint_stakeholder(marriage_authority__federalist_millet_reading, constitutional_courts, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents any single demographic majority from using ordinary legislative power to impose its family-law norms on numerically smaller communities, by distributing marriage-authority jurisdiction across community lines rather than centralizing it in a majoritarian legislature.
% TRANSFER_FUNCTION: Moves the power to set marriage, divorce, and inheritance norms away from the national legislature and toward communal leadership bodies, and moves the cost of the resulting jurisdictional gaps and internal-dissent lockout onto individuals whose interests diverge from their community's official personal law.
% ABSENT_VOICES: Intra-community dissenters (particularly women contesting unequal divorce or inheritance terms within their own community's law) and cross-community couples both have strong reasons to object to the fragmentation, but neither has a forum: dissenters are structurally routed back into the community's own adjudicative bodies, and couples fall into an under-designed residual regime. The national legislature's majoritarian preference is also structurally excluded by design, which is the point of the arrangement from the elite-bargain seat.
% DISAPPEARANCE_RATIONALE: From the consociational-elite and minority-community seats, dissolution of the fragmented arrangement in favor of a uniform code would rearrange the political settlement entirely — coalition stability, communal institutional standing, and minority protection against majoritarian legislation would all be at stake. From the dissenter and cross-community-couple seats, the disappearance of communal jurisdiction and its replacement by a single code would resolve rather than create disruption. The verdict itself splits along the same fault line the constraint is built to manage.
% FOUNDING_PROBLEM: At founding, a numerically dominant religious/demographic majority controlled the national legislature; without a structural guarantee, that majority could have legislated a uniform family-law code reflecting its own norms and displacing minority communities' marriage and inheritance practices entirely. Fragmenting marriage authority was the mechanism chosen to foreclose that majoritarian override.
% FOUNDING_PROBLEM_CORROBORATION: Comparative constitutional scholars studying consociational and millet-system designs (writing from outside any one community's leadership) corroborate that the founding anti-majoritarian threat was real and that fragmentation functioned as a genuine safeguard in the early post-founding period. Human rights bodies and constitutional courts, also outside the beneficiary set, corroborate that the same structure now also functions to insulate intra-community rights violations from constitutional review — the founding problem's continued vitality is attested by different outside observers differently depending on which half of the structure they examine.
narrative_ontology:disappearance_verdict(marriage_authority__federalist_millet_reading, contested).
narrative_ontology:founding_problem_status(marriage_authority__federalist_millet_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_authority__federalist_millet_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(marriage_authority__federalist_millet_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_authority__federalist_millet_reading, 0.28, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_authority__federalist_millet_reading_tests).
:- end_tests(marriage_authority__federalist_millet_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low (0.28 at interval end) because the dominant function is genuine anti-majoritarian coordination: no single community captures rents from the others via this structure, and the elite bargain is a stability mechanism rather than a rent-extraction machine in its primary operation. What extraction exists is concentrated and real — it lands on intra-community dissenters denied an external forum and cross-community couples navigating jurisdictional gaps — but it is a byproduct of the coordination design, not its purpose. Suppression is low-moderate (0.22): the arrangement does not require an active coercive enforcement apparatus at the state level; the constraint on the legislature is structural/political (coalition-dependent) rather than coercive in the classic sense, though the constraint on dissenters is closer to a suppression mechanism internal to communal adjudication. Theater ratio starts low and rises mildly (0.15 to 0.30) as the founding anti-majoritarian threat recedes in salience while the institutional apparatus of communal jurisdiction persists — some maintenance of the arrangement becomes more performative (invoking founding-era majoritarian threat rhetoric) as the original threat context changes. Resistance is moderate-high (0.55) reflecting the ongoing political contestation from secularist and gender-rights coalitions.
 *
 * PERSPECTIVAL GAP:
 *   From the consociational-elite and communal-leadership seats, the constraint computes as coordination near-Rope: it solves a real collective-action problem (preventing majoritarian legislative capture) with participants who are net beneficiaries of the bargain's stability. From the intra-community-dissenter seat, the same structure is likely to compute closer to extractive, because the coordination benefit accrues to the community as a bloc while the cost of insulation from external constitutional review is borne individually and involuntarily. This divergence is the seat-computation the engine is built to surface — the story does not resolve it by picking one seat's reading as authoritative.
 *
 * DIRECTIONALITY LOGIC:
 *   Minority religious communities and communal leadership bodies sit near the beneficiary end: the fragmentation is what they would lose if a uniform code were imposed, and they hold organized power with constrained (not mobile) exit — they cannot simply leave the polity, but the arrangement structurally protects their position within it. Consociational elites sit furthest toward beneficiary with arbitrage-grade exit: they broker the bargain and are not bound by any single community's law. Intra-community dissenters sit at the full-target end: powerless, trapped (their exit from the community's jurisdiction is itself constrained by the same personal-law regime governing marriage and divorce), and bearing the concentrated cost of the arrangement's insulation from external review. Cross-community couples sit closer to target but with moderate power and constrained rather than trapped exit — they have some resort to civil/residual regimes, imperfect as those are. The national legislature is excluded rather than positioned as beneficiary or payer in the ordinary sense: its majoritarian capacity is the thing structurally neutralized, which is definitionally distinct from bearing a cost.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — majoritarian legislative capture of family law — is contested as to whether it remains live: comparative scholarship suggests the coalition-structural threat that motivated fragmentation has evolved (the demographic and political conditions of founding-era majoritarian dominance are not static), while the insulating function for intra-community rights violations has, if anything, become more salient. This is not a case of clean mandatrophy (function dead, form persisting) nor clean live-function — it is a structure whose original coordination function may be partially live while a secondary extractive function (insulating communal law from constitutional equality review) has grown more consequential over time without ever being the arrangement's declared purpose. The rising theater_ratio series reflects this: founding-era anti-majoritarian rhetoric persists at the level of political justification even as the operative function shifts.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    elite_bargain_vs_organic_tradition,
    'Is the fragmentation best understood as a deliberately engineered elite constitutional bargain (this reading) or as an inherited deference to pre-existing communal religious authority that elites merely ratified (the communal_autonomy_reading)? The historical record may support both simultaneously at different founding moments.',
    'Constitutional drafting history and convention debates: if drafters explicitly framed fragmentation as an anti-majoritarian design choice (rather than a concession to pre-existing communal authority), this reading''s premise is strengthened; if the debates show communal authority was treated as a pre-political given rather than a design choice, the communal_autonomy_reading better fits the historical record.',
    'If the elite-bargain framing is not supported by drafting history, this reading''s structural claim about deliberate anti-tyranny design collapses into the sibling reading, and the low-ε rope classification loses its coordination-function grounding.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(elite_bargain_vs_organic_tradition, conceptual, 'Whether fragmentation was engineered anti-majoritarian design or ratified pre-existing communal authority.').

omega_variable(
    coalition_stability_measurement,
    'Does the consociational bargain actually produce measurably greater political stability (fewer majoritarian legislative crises, lower communal-conflict incidence) than counterfactual centralized regimes in comparable polities, or is the stability claim itself elite self-justification?',
    'Comparative cross-national analysis of polities with centralized versus fragmented family-law jurisdiction, controlling for other stability determinants (economic conditions, external security threats, prior conflict history).',
    'If fragmented regimes show no measurable stability advantage, the coordination-function claim underlying the low ε and Rope classification weakens substantially, and the arrangement looks more like elite rent extraction dressed in anti-tyranny justification — pushing the classification toward tangled_rope or snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coalition_stability_measurement, empirical, 'Whether the claimed stability benefit of consociational fragmentation is empirically supported.').

omega_variable(
    dissenter_exit_mischaracterization,
    'Is ''trapped'' the correct exit_options classification for intra_community_dissenters, or do some have meaningful (if costly) exit via conversion, emigration, or civil marriage registration that the story understates?',
    'Empirical survey of actual exit rates and pathways used by intra-community dissenters across multiple communities under the fragmented regime, including social and economic costs of each pathway.',
    'If meaningful exit exists at non-prohibitive cost, the directionality for this group shifts from full-target toward constrained, lowering the effective extraction the engine computes for this seat and softening the tangled-rope-adjacent reading of that stakeholder''s experience.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dissenter_exit_mischaracterization, empirical, 'Whether dissenters'' exit options are more constrained-than-trapped in practice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_authority__federalist_millet_reading, 0, 70).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t0, marriage_authority__federalist_millet_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(marr_tr_t10, marriage_authority__federalist_millet_reading, theater_ratio, 10, 0.17).
narrative_ontology:measurement(marr_tr_t20, marriage_authority__federalist_millet_reading, theater_ratio, 20, 0.19).
narrative_ontology:measurement(marr_tr_t30, marriage_authority__federalist_millet_reading, theater_ratio, 30, 0.21).
narrative_ontology:measurement(marr_tr_t40, marriage_authority__federalist_millet_reading, theater_ratio, 40, 0.24).
narrative_ontology:measurement(marr_tr_t50, marriage_authority__federalist_millet_reading, theater_ratio, 50, 0.26).
narrative_ontology:measurement(marr_tr_t60, marriage_authority__federalist_millet_reading, theater_ratio, 60, 0.28).
narrative_ontology:measurement(marr_tr_t70, marriage_authority__federalist_millet_reading, theater_ratio, 70, 0.3).

% Extraction over time
narrative_ontology:measurement(marr_be_t0, marriage_authority__federalist_millet_reading, base_extractiveness, 0, 0.14).
narrative_ontology:measurement(marr_be_t10, marriage_authority__federalist_millet_reading, base_extractiveness, 10, 0.16).
narrative_ontology:measurement(marr_be_t20, marriage_authority__federalist_millet_reading, base_extractiveness, 20, 0.19).
narrative_ontology:measurement(marr_be_t30, marriage_authority__federalist_millet_reading, base_extractiveness, 30, 0.21).
narrative_ontology:measurement(marr_be_t40, marriage_authority__federalist_millet_reading, base_extractiveness, 40, 0.23).
narrative_ontology:measurement(marr_be_t50, marriage_authority__federalist_millet_reading, base_extractiveness, 50, 0.25).
narrative_ontology:measurement(marr_be_t60, marriage_authority__federalist_millet_reading, base_extractiveness, 60, 0.27).
narrative_ontology:measurement(marr_be_t70, marriage_authority__federalist_millet_reading, base_extractiveness, 70, 0.28).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(marriage_authority__federalist_millet_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_authority__federalist_millet_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(marriage_authority__federalist_millet_reading, 0.1).
narrative_ontology:affects_constraint(marriage_authority__federalist_millet_reading, communal_autonomy_reading).
narrative_ontology:affects_constraint(marriage_authority__federalist_millet_reading, secularist_reading).
narrative_ontology:affects_constraint(marriage_authority__federalist_millet_reading, gender_rights_reading).
narrative_ontology:affects_constraint(marriage_authority__federalist_millet_reading, judicial_harmonization_reading).

% DUAL FORMULATION NOTE:
% This story is one of five linked readings of the marriage_authority kernel. It shares near-total institutional-fact overlap with communal_autonomy_reading but differs in the causal story assigned to the fragmentation (deliberate elite anti-majoritarian design vs. ratified religious deference) and in evaluative stance toward legislative paralysis (stability feature vs. neutral byproduct of deference). It stands in direct evaluative tension with secularist_reading (which treats the same fragmentation as an anomaly to be eliminated) and with gender_rights_reading (which treats the insulation from constitutional review this reading treats as a stability feature as itself the central harm). judicial_harmonization_reading represents a partially convergent but institutionally distinct mechanism (courts rather than legislative bargains) for addressing the same underlying jurisdictional gaps.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
