% ============================================================================
% CONSTRAINT STORY: herem_command_dt7__durable_separation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_herem_command_dt7__durable_separation_reading, []).

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
 *   constraint_id: herem_command_dt7__durable_separation_reading
 *   human_readable: Herem as Timeless Mandate for Bounded Covenant Identity (Durable Separation Reading)
 *   domain: biblical_hermeneutics/religious_ethics/commitment_systems
 *
 * SUMMARY:
 *   Deuteronomy 7 commands Israel to show no mercy to and not intermarry with
 *   seven named Canaanite nations upon entering the land, citing risk of
 *   religious assimilation. The durable-separation reading holds this command
 *   as timeless divine mandate rather than settlement-period policy: covenant
 *   identity is preserved through permanent, categorical
 *   boundary-maintenance, and the 'outsider' slot in the command's logic is
 *   treated as structurally reusable across historical periods rather than
 *   fixed to the seven named nations. This reading generates concrete costs
 *   for those who fall into the outsider or boundary-crossing categories and
 *   concrete benefits for those positioned to administer or claim clean
 *   lineage within the boundary.
 *
 * KEY AGENTS:
 *   - covenant_community_leadership: institutional/arbitrage — administers and benefits from the boundary
 *   - designated_outsider_nations: powerless/trapped — categorically excluded, bears the extraction
 *   - intermarriage_seeking_covenant_members: moderate/constrained — bears autonomy cost
 *   - biblical_scholars_historical_critical: analytical/analytical — documents the textual and historical basis for competing readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(herem_command_dt7__durable_separation_reading, 0.81).
domain_priors:suppression_score(herem_command_dt7__durable_separation_reading, 0.78).
domain_priors:theater_ratio(herem_command_dt7__durable_separation_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(herem_command_dt7__durable_separation_reading, extractiveness, 0.81).
narrative_ontology:constraint_metric(herem_command_dt7__durable_separation_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(herem_command_dt7__durable_separation_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(herem_command_dt7__durable_separation_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(herem_command_dt7__durable_separation_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(herem_command_dt7__durable_separation_reading, tangled_rope).
narrative_ontology:human_readable(herem_command_dt7__durable_separation_reading, "Herem as Timeless Mandate for Bounded Covenant Identity (Durable Separation Reading)").
narrative_ontology:topic_domain(herem_command_dt7__durable_separation_reading, "biblical_hermeneutics/religious_ethics/commitment_systems").

domain_priors:requires_active_enforcement(herem_command_dt7__durable_separation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(herem_command_dt7__durable_separation_reading, '12fb4db9-704d-4458-bbb9-80b3edb5631d').
narrative_ontology:cs_kernel_codification('12fb4db9-704d-4458-bbb9-80b3edb5631d', fixed_text).
narrative_ontology:cs_authority_grounding('12fb4db9-704d-4458-bbb9-80b3edb5631d', lineage).
narrative_ontology:cs_interpretation_layer_present('12fb4db9-704d-4458-bbb9-80b3edb5631d').
narrative_ontology:cs_reading_relation('12fb4db9-704d-4458-bbb9-80b3edb5631d', herem_command_dt7__contextual_supersession_reading, forecloses).
narrative_ontology:cs_reading_relation('12fb4db9-704d-4458-bbb9-80b3edb5631d', herem_command_dt7__allegorical_displacement_reading, coexists_with).
narrative_ontology:cs_axiom('12fb4db9-704d-4458-bbb9-80b3edb5631d', foundational, covenant_boundary_perpetually_binding).
narrative_ontology:cs_axiom_status(covenant_boundary_perpetually_binding, holdable).
narrative_ontology:cs_axiom_grounding('12fb4db9-704d-4458-bbb9-80b3edb5631d', covenant_boundary_perpetually_binding, deontological).
narrative_ontology:cs_axiom('12fb4db9-704d-4458-bbb9-80b3edb5631d', foundational, outsider_category_literally_ethnic_and_reusable).
narrative_ontology:cs_axiom_status(outsider_category_literally_ethnic_and_reusable, holdable).
narrative_ontology:cs_axiom_grounding('12fb4db9-704d-4458-bbb9-80b3edb5631d', outsider_category_literally_ethnic_and_reusable, conventional).
narrative_ontology:cs_reference_frame('12fb4db9-704d-4458-bbb9-80b3edb5631d', sinai_covenant_perpetual_boundary).
narrative_ontology:cs_drift_state('12fb4db9-704d-4458-bbb9-80b3edb5631d', post_prophetic_universalist_challenge, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('12fb4db9-704d-4458-bbb9-80b3edb5631d', '').
narrative_ontology:cs_kernel_id(herem_command_dt7__durable_separation_reading, herem_command_dt7).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(herem_command_dt7__durable_separation_reading, covenant_community_leadership).
narrative_ontology:constraint_beneficiary(herem_command_dt7__durable_separation_reading, endogamous_lineage_claimants).
narrative_ontology:constraint_victim(herem_command_dt7__durable_separation_reading, designated_outsider_nations).
narrative_ontology:constraint_victim(herem_command_dt7__durable_separation_reading, intermarriage_seeking_covenant_members).
narrative_ontology:constraint_victim(herem_command_dt7__durable_separation_reading, descendants_of_mixed_unions).
narrative_ontology:constraint_vindicates(herem_command_dt7__durable_separation_reading, covenant_purity_doctrine).
narrative_ontology:constraint_vindicates(herem_command_dt7__durable_separation_reading, categorical_separation_theology).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the boundary-marking apparatus — who counts as covenant member, which unions are permissible, which populations are herem-designated. Reading Deuteronomy 7 as timeless mandate consolidates their authority to police membership indefinitely; they draw ongoing legitimacy and control over marriage, inheritance, and communal admission from treating the command as permanently binding rather than historically bounded.
narrative_ontology:constraint_stakeholder(herem_command_dt7__durable_separation_reading, covenant_community_leadership, agenda_setter,
    institutional, civilizational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(herem_command_dt7__durable_separation_reading, covenant_community_leadership, beneficiary).

% Families and lineages whose social and inheritance standing depends on demonstrable covenant purity benefit from a durable rule that keeps the boundary sharp; the timeless reading protects the value of their genealogical claims against dilution by intermarriage or absorption of outsiders.
narrative_ontology:constraint_stakeholder(herem_command_dt7__durable_separation_reading, endogamous_lineage_claimants, beneficiary,
    organized, generational, constrained, national).

% Named categorically as contamination threats (the seven nations and, by extension under this reading, any group occupying the outsider slot) without individual assessment. Under the durable-separation reading their designation is not time-bound to a settlement campaign but recurs as a template: any group can be slotted into the herem category as a permanent theological class, with violence and exclusion authorized by appeal to unchanging divine command.
narrative_ontology:constraint_stakeholder(herem_command_dt7__durable_separation_reading, designated_outsider_nations, payer,
    powerless, civilizational, trapped, national).

% Individuals within the covenant community who wish to marry outside the designated boundary bear the direct cost of the durable-separation reading: their autonomy in partner choice is subordinated to a categorical prohibition treated as permanently binding rather than a policy tied to a specific historical threat of assimilation into idolatrous conquest-era cultures.
narrative_ontology:constraint_stakeholder(herem_command_dt7__durable_separation_reading, intermarriage_seeking_covenant_members, payer,
    moderate, biographical, constrained, national).

% Children of unions the boundary treats as forbidden inherit contested or diminished status under a rule that projects the ancient categorical logic forward indefinitely; they bear costs (exclusion, status ambiguity) for a boundary-drawing exercise conducted before their birth.
narrative_ontology:constraint_stakeholder(herem_command_dt7__durable_separation_reading, descendants_of_mixed_unions, payer,
    powerless, generational, trapped, national).

% Interpretive communities who read herem as historically superseded or as allegorical moral instruction are present in the same textual tradition but are structurally excluded from this reading's operative authority — their objections are theological counter-arguments, not veto power over how the durable-separation reading is enforced where it holds institutional authority.
narrative_ontology:constraint_stakeholder(herem_command_dt7__durable_separation_reading, textual_communities_holding_sibling_readings, excluded,
    organized, generational, constrained, national).

% Study the text's compositional history, ancient Near Eastern warfare rhetoric conventions, and the archaeological record of the conquest narrative. They can document whether the text's own internal markers (temporal, geographic, campaign-specific language) support a durable-mandate reading or a bounded-period reading, without holding institutional power to settle the theological dispute.
narrative_ontology:constraint_stakeholder(herem_command_dt7__durable_separation_reading, biblical_scholars_historical_critical, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(herem_command_dt7__durable_separation_reading, covenant_community_leadership).
narrative_ontology:fixing_cost_class(herem_command_dt7__durable_separation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains a bounded, identifiable covenant community with stable membership criteria, protecting communal cohesion, shared worship practice, and resistance to religious syncretism across generations.
% TRANSFER_FUNCTION: Moves marital and associational autonomy from individual covenant members and from outsider populations to the community's boundary-setting authority; moves social and inheritance security to lineages that can demonstrate unbroken covenant descent, at the cost of exclusion or violence directed at those categorized as permanent outsiders.
% ABSENT_VOICES: Designated outsider populations have no voice in their own categorization — the text speaks of them, never to them, in the surviving record. Descendants of mixed unions and those who intermarried against the norm are likewise absent from the textual and interpretive record that authorizes their exclusion. Sibling reading communities (allegorical and supersession) are present in the broader tradition but excluded from operative authority wherever this reading holds institutional power.
% DISAPPEARANCE_RATIONALE: If the durable-separation reading lost institutional authority, marriage and membership policy in communities that currently enforce it would have to be renegotiated on other grounds (voluntary covenant, ethical universalism, or historically-bounded application); leadership's boundary-policing role would lose its textual warrant, lineage-based status claims would lose a key legitimating mechanism, and populations currently held in the permanent-outsider category would no longer face categorical exclusion authorized by appeal to unchanging command.
% FOUNDING_PROBLEM: Ancient Israel's covenant community faced a genuine assimilation and syncretism risk during settlement: intermarriage and cultural absorption into surrounding cults threatened the community's distinct religious identity and practice at a specific historical moment of vulnerability.
% FOUNDING_PROBLEM_CORROBORATION: Communities and leaders holding the durable-separation reading attest the identity-preservation problem is permanently live (syncretism and assimilation are treated as perennial threats, not settlement-era-specific). Historical-critical scholars outside the benefiting institutional structure, and the sibling-reading communities themselves, attest that the text's own markers (campaign-specific nations, geographic scope, conquest-narrative framing) support a historically-bounded founding problem rather than a timeless one — meaning the corroboration is genuinely split along interpretive-community lines, not merely asserted by beneficiaries alone.
narrative_ontology:disappearance_verdict(herem_command_dt7__durable_separation_reading, world_rearranges).
narrative_ontology:founding_problem_status(herem_command_dt7__durable_separation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(herem_command_dt7__durable_separation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(herem_command_dt7__durable_separation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(herem_command_dt7__durable_separation_reading, 0.81, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(herem_command_dt7__durable_separation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(herem_command_dt7__durable_separation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(herem_command_dt7__durable_separation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (0.81) because the durable-separation reading, by its own structural logic, applies the categorical exclusion and violence-legitimation permanently and to any group slotted into the outsider category — this is a wider and more durable extraction than a bounded historical policy would produce. Suppression (0.78) reflects that the reading requires active enforcement (marriage policing, boundary maintenance, doctrinal reinforcement) to persist against both textual counter-readings and lived pressure toward assimilation. Theater ratio is comparatively low (0.22) because the enforcement, where it occurs, is substantively felt (real marriage prohibition, real exclusion) rather than merely symbolic — though some performative doctrinal affirmation exists. Accessibility collapse (0.62) is moderate-high: once a community accepts the timeless-mandate premise, alternative readings become theologically costly to hold, though they remain available in the broader tradition (hence not higher). Resistance (0.58) reflects real ongoing contestation from sibling-reading communities and from those who bear the costs.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat, the arrangement is coordination: it protects a fragile identity against real assimilation pressure, and its permanence is exactly its virtue. From the payer seats — those who fall into the outsider category or wish to cross the boundary — the same structure is extraction sustained by appeal to unchallengeable divine command, with no individualized justification and no exit. The engine's per-seat computation should reflect this divergence without either side's framing being privileged by the authored claim.
 *
 * DIRECTIONALITY LOGIC:
 *   Covenant leadership and lineage-claimants sit near the beneficiary end: they administer or are protected by the boundary and lose authority/status if the mandate is read as historically bounded or allegorical. Outsider nations, intermarriage-seeking members, and mixed-union descendants sit near the target end: trapped or constrained exit, categorical rather than individualized treatment, and durable (not time-limited) application of costs. The permanent-target structure is exactly what the durable-separation reading adds relative to its siblings — under contextual_supersession the target category would have closed with the settlement period; under allegorical_displacement there would be no literal human target category at all.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (assimilation risk during a specific, historically situated settlement crisis) is contested as still live. If the founding problem is in fact dead — if the assimilation threat was tied to conquest-era polytheistic cultures no longer extant in that form — then a reading that keeps the boundary-enforcement machinery permanently operative is a mandatrophy case: institutional apparatus outliving the problem it was built for, now serving lineage-status and leadership-authority interests rather than the original coordination function. The tangled_rope classification is deliberately chosen over snare because a genuine coordination function (protecting a distinct religious community's continuity) is plausibly real at the founding moment; what distinguishes this reading is that it projects that founding-moment coordination forward as permanently binding, which is where the extraction on non-founding-era outsiders and on constrained-autonomy insiders enters.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_selection_durable_vs_siblings,
    'Is Deuteronomy 7''s herem command better read as a timeless mandate for perpetual categorical separation (this reading), a historically-bounded settlement-period directive superseded by later covenant developments (contextual_supersession_reading), or a typological/allegorical instruction about internal spiritual struggle rather than literal ethnic categories (allegorical_displacement_reading)?',
    'Textual-critical analysis of the command''s internal scope markers (named nations, geographic bounding to the land of Canaan, campaign-specific verbs), comparison with subsequent canonical and post-canonical treatment of intermarriage and outsider status, and assessment of whether the community''s own tradition treats the command as closed (fulfilled at settlement) or open (perpetually applicable).',
    'If the textual and traditional evidence favors historical bounding, this reading''s extraction is not merely high but illegitimately extended beyond the command''s own scope — strengthening the case that durable-separation is a constructed extraction rather than the text''s own claim. If evidence favors durability, the high-extraction profile authored here is the text''s own intended operation, not a misreading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection_durable_vs_siblings, conceptual, 'Which of the three sibling readings the text itself best supports, and what that implies about whether this reading''s ε is warranted or extended.').

omega_variable(
    outsider_category_reusability,
    'Does the durable-separation reading''s treatment of ''outsider nations'' as a permanently reusable template (applicable to any group slotted into that position across history) reflect the command''s own logic, or is this an extrapolation made by later interpretive communities to authorize exclusion of groups the original text never contemplated?',
    'Historical survey of how communities holding this reading have actually applied the outsider category over time — whether application has been confined to groups with plausible typological continuity to the named nations, or has expanded to arbitrary designated out-groups.',
    'If application has expanded arbitrarily, the victim set authored here (all non-covenant outsiders as potential contamination threat) is a conservative-to-accurate description of the reading''s actual operation. If application has stayed narrowly typological, the victim set may be authored too broadly for this specific reading''s historical instantiations.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(outsider_category_reusability, empirical, 'Whether the reusable-outsider-category feature is inherent to the reading or an additional extrapolation layered onto it by particular communities.').

omega_variable(
    coordination_vs_extraction_founding_weight,
    'At the founding moment, how much of the herem command''s function was genuine identity-preservation coordination (protecting a vulnerable minority religious community from cultural absorption) versus how much was already conquest-legitimation serving territorial and political interests of the settling community''s leadership?',
    'Comparative analysis with other ancient Near Eastern herem/conquest-legitimation texts and the archaeological record of the settlement period, assessing whether the coordination story (identity preservation) or the extraction story (conquest justification) better fits the independently reconstructable historical circumstances.',
    'A larger genuine-coordination component supports tangled_rope; a larger founding-extraction component would support reclassifying even the founding-era application as closer to snare, with the durable-separation reading''s error being extension of an already-extractive founding arrangement rather than extension of a genuinely coordinative one.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_vs_extraction_founding_weight, empirical, 'Whether the founding-moment coordination claim underlying the tangled_rope classification is itself well-founded.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(herem_command_dt7__durable_separation_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(here_tr_t0, herem_command_dt7__durable_separation_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(here_tr_t20, herem_command_dt7__durable_separation_reading, theater_ratio, 20, 0.13).
narrative_ontology:measurement(here_tr_t40, herem_command_dt7__durable_separation_reading, theater_ratio, 40, 0.16).
narrative_ontology:measurement(here_tr_t60, herem_command_dt7__durable_separation_reading, theater_ratio, 60, 0.18).
narrative_ontology:measurement(here_tr_t80, herem_command_dt7__durable_separation_reading, theater_ratio, 80, 0.2).
narrative_ontology:measurement(here_tr_t100, herem_command_dt7__durable_separation_reading, theater_ratio, 100, 0.22).

% Extraction over time
narrative_ontology:measurement(here_be_t0, herem_command_dt7__durable_separation_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(here_be_t20, herem_command_dt7__durable_separation_reading, base_extractiveness, 20, 0.6).
narrative_ontology:measurement(here_be_t40, herem_command_dt7__durable_separation_reading, base_extractiveness, 40, 0.68).
narrative_ontology:measurement(here_be_t60, herem_command_dt7__durable_separation_reading, base_extractiveness, 60, 0.74).
narrative_ontology:measurement(here_be_t80, herem_command_dt7__durable_separation_reading, base_extractiveness, 80, 0.78).
narrative_ontology:measurement(here_be_t100, herem_command_dt7__durable_separation_reading, base_extractiveness, 100, 0.81).

% Suppression requirement over time
narrative_ontology:measurement(here_su_t0, herem_command_dt7__durable_separation_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(here_su_t20, herem_command_dt7__durable_separation_reading, suppression_requirement, 20, 0.58).
narrative_ontology:measurement(here_su_t40, herem_command_dt7__durable_separation_reading, suppression_requirement, 40, 0.65).
narrative_ontology:measurement(here_su_t60, herem_command_dt7__durable_separation_reading, suppression_requirement, 60, 0.7).
narrative_ontology:measurement(here_su_t80, herem_command_dt7__durable_separation_reading, suppression_requirement, 80, 0.75).
narrative_ontology:measurement(here_su_t100, herem_command_dt7__durable_separation_reading, suppression_requirement, 100, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(herem_command_dt7__durable_separation_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(herem_command_dt7__durable_separation_reading, 0.08).
narrative_ontology:affects_constraint(herem_command_dt7__durable_separation_reading, herem_command_dt7__contextual_supersession_reading).
narrative_ontology:affects_constraint(herem_command_dt7__durable_separation_reading, herem_command_dt7__allegorical_displacement_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the herem_command_dt7 kernel, each instantiated as a separate constraint per the epsilon-invariance principle. contextual_supersession_reading authors low current-era extraction (the mandate is treated as historically closed). allegorical_displacement_reading authors near-zero literal-victim extraction (the 'nations' are typological, not human out-groups, so the human victim set collapses). This durable_separation_reading authors the highest extraction of the three because it alone treats the categorical exclusion and violence-legitimation as perpetually binding on real, currently-identifiable outsider populations. The three files are linked via affects_constraints in both directions where authored; each carries its own beneficiaries/victims/omegas/claimed_type independently.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
