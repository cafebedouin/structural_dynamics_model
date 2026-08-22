% ============================================================================
% CONSTRAINT STORY: human_transcendence_pathway__jerusalem_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_human_transcendence_pathway__jerusalem_reading, []).

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
 *   constraint_id: human_transcendence_pathway__jerusalem_reading
 *   human_readable: Jerusalem Reading: Community Rebuilt Through Patient Participatory Labor Under Blessing
 *   domain: religious/political/social
 *
 * SUMMARY:
 *   This constraint models the Jerusalem reading of the
 *   human-transcendence-pathway kernel: authentic community is rebuilt not
 *   through coercive unification or technological self-sufficiency but
 *   through patient, participatory, distributed labor under a shared blessing
 *   that treats internal plurality (returnees, remainers, resident
 *   foreigners) as a resource to be integrated into communion rather than a
 *   problem to be erased. The rebuilding is structurally voluntary and
 *   locally distributed — households and guilds rebuild adjacent sections,
 *   leadership persuades rather than compels, and marginal persons are
 *   deliberately included in the covenant renewal. This is a distinct
 *   constraint from the babel_reading (unified technological/linguistic
 *   self-sufficiency without transcendent reference) and the
 *   technocratic_vs_incarnational_reading (transcendence via
 *   optimization/limit-elimination vs. transcendence as received gift) — each
 *   of those siblings has its own ε, its own beneficiary/victim structure,
 *   and is authored as a separate story linked via network edges, per the
 *   ε-invariance principle. Do not read this story as adjudicating which
 *   reading is correct; it authors only the Jerusalem reading's own
 *   structural facts.
 *
 * KEY AGENTS:
 *   - returning_exiles: primary beneficiaries and co-agenda-setters (powerless/constrained) — bear the physical cost of rebuilding but gain communal standing
 *   - local_artisans_and_laborers: distributed agenda-setters (moderate/mobile) — voluntary, non-coerced division of labor
 *   - marginalized_and_foreign_residents: beneficiaries (powerless/constrained) — deliberately integrated rather than excluded
 *   - religious_and_civic_leadership: agenda-setters by persuasion, not force (organized/constrained)
 *   - surrounding_hostile_powers: excluded external actors who would object but sit outside the internal arrangement
 *   - theological_observers: analytical seat assessing the pattern's generalizability
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(human_transcendence_pathway__jerusalem_reading, 0.22).
domain_priors:suppression_score(human_transcendence_pathway__jerusalem_reading, 0.15).
domain_priors:theater_ratio(human_transcendence_pathway__jerusalem_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(human_transcendence_pathway__jerusalem_reading, extractiveness, 0.22).
narrative_ontology:constraint_metric(human_transcendence_pathway__jerusalem_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(human_transcendence_pathway__jerusalem_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(human_transcendence_pathway__jerusalem_reading, accessibility_collapse, 0.25).
narrative_ontology:constraint_metric(human_transcendence_pathway__jerusalem_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(human_transcendence_pathway__jerusalem_reading, rope).
narrative_ontology:human_readable(human_transcendence_pathway__jerusalem_reading, "Jerusalem Reading: Community Rebuilt Through Patient Participatory Labor Under Blessing").
narrative_ontology:topic_domain(human_transcendence_pathway__jerusalem_reading, "religious/political/social").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(human_transcendence_pathway__jerusalem_reading, 'edfaaea7-3216-4239-8f06-6fd1289e5a95').
narrative_ontology:cs_kernel_codification('edfaaea7-3216-4239-8f06-6fd1289e5a95', fixed_text).
narrative_ontology:cs_authority_grounding('edfaaea7-3216-4239-8f06-6fd1289e5a95', lineage).
narrative_ontology:cs_interpretation_layer_present('edfaaea7-3216-4239-8f06-6fd1289e5a95').
narrative_ontology:cs_reading_relation('edfaaea7-3216-4239-8f06-6fd1289e5a95', human_transcendence_pathway__babel_reading, forecloses).
narrative_ontology:cs_reading_relation('edfaaea7-3216-4239-8f06-6fd1289e5a95', human_transcendence_pathway__technocratic_vs_incarnational_reading, influences).
narrative_ontology:cs_axiom('edfaaea7-3216-4239-8f06-6fd1289e5a95', foundational, plurality_integrable_into_communion_without_uniformity).
narrative_ontology:cs_axiom_status(plurality_integrable_into_communion_without_uniformity, holdable).
narrative_ontology:cs_axiom_grounding('edfaaea7-3216-4239-8f06-6fd1289e5a95', plurality_integrable_into_communion_without_uniformity, deontological).
narrative_ontology:cs_axiom('edfaaea7-3216-4239-8f06-6fd1289e5a95', foundational, authentic_community_requires_patient_participatory_labor_not_coercive_speed).
narrative_ontology:cs_axiom_status(authentic_community_requires_patient_participatory_labor_not_coercive_speed, holdable).
narrative_ontology:cs_axiom_grounding('edfaaea7-3216-4239-8f06-6fd1289e5a95', authentic_community_requires_patient_participatory_labor_not_coercive_speed, instrumental).
narrative_ontology:cs_reference_frame('edfaaea7-3216-4239-8f06-6fd1289e5a95', covenant_community_by_participatory_renewal).
narrative_ontology:cs_drift_state('edfaaea7-3216-4239-8f06-6fd1289e5a95', post_exilic_settlement, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('edfaaea7-3216-4239-8f06-6fd1289e5a95', '').
narrative_ontology:cs_kernel_id(human_transcendence_pathway__jerusalem_reading, human_transcendence_pathway).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(human_transcendence_pathway__jerusalem_reading, returning_exiles).
narrative_ontology:constraint_beneficiary(human_transcendence_pathway__jerusalem_reading, reconstituted_community).
narrative_ontology:constraint_beneficiary(human_transcendence_pathway__jerusalem_reading, marginalized_participants).
narrative_ontology:constraint_beneficiary(human_transcendence_pathway__jerusalem_reading, surrounding_peoples_drawn_in).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(human_transcendence_pathway__jerusalem_reading, local_artisans_and_laborers).
narrative_ontology:constraint_beneficiary(human_transcendence_pathway__jerusalem_reading, marginalized_and_foreign_residents).
narrative_ontology:constraint_vindicates(human_transcendence_pathway__jerusalem_reading, plurality_integrable_into_communion).
narrative_ontology:constraint_vindicates(human_transcendence_pathway__jerusalem_reading, slow_formation_over_coercive_unification).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Return from displacement to a ruined city with no walls, no centralized administrative machinery, and contested legitimacy among neighboring powers. They participate directly in the rebuilding labor — carrying stone, negotiating with adversaries, re-establishing communal worship and law — and their standing in the reconstituted community is not conferred by external force but built through shared toil and covenant renewal. Exit would mean permanent dispersal; remaining means bearing the slow cost of reconstruction.
narrative_ontology:constraint_stakeholder(human_transcendence_pathway__jerusalem_reading, returning_exiles, beneficiary,
    powerless, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(human_transcendence_pathway__jerusalem_reading, returning_exiles, agenda_setter).

% Each family and guild rebuilds the section of the wall or community adjacent to their own house or trade, a distributed division of labor that requires no central coercive enforcement to function. They set the practical agenda of reconstruction street by street and benefit directly from the security and dignity the finished structure provides; if the project stalled they could disengage without catastrophic personal cost, but the design incentivizes continued voluntary participation.
narrative_ontology:constraint_stakeholder(human_transcendence_pathway__jerusalem_reading, local_artisans_and_laborers, agenda_setter,
    moderate, biographical, mobile, local).
narrative_ontology:stakeholder_secondary_role(human_transcendence_pathway__jerusalem_reading, local_artisans_and_laborers, beneficiary).

% Non-native residents, servants, and those without ancestral claim to the land are explicitly included in the covenant renewal and the labor of rebuilding rather than excluded for the sake of ethnic or administrative uniformity. Their inclusion is a deliberate structural choice — plurality is treated as a resource for the community's resilience rather than a problem to be homogenized away. They remain economically dependent but are not structurally targeted for extraction.
narrative_ontology:constraint_stakeholder(human_transcendence_pathway__jerusalem_reading, marginalized_and_foreign_residents, beneficiary,
    powerless, generational, constrained, local).

% Leaders read the founding law publicly, interpret it for a returned and partially assimilated population, and mediate disputes without recourse to an external empire's coercive apparatus. Their authority rests on persuasion, communal memory, and perceived fidelity to the covenant rather than on force; if the community rejects their reading, they have no independent means of compelling compliance.
narrative_ontology:constraint_stakeholder(human_transcendence_pathway__jerusalem_reading, religious_and_civic_leadership, agenda_setter,
    organized, generational, constrained, regional).

% Regional governors and rival populations view the rebuilding project as a threat to their own administrative and economic advantage and are not included in the community's internal deliberation. They would object that the reconstruction destabilizes existing arrangements, but their objection is external pressure rather than a structural position within the arrangement itself.
narrative_ontology:constraint_stakeholder(human_transcendence_pathway__jerusalem_reading, surrounding_hostile_powers, excluded,
    powerful, biographical, mobile, regional).

% Later interpreters — within and outside the tradition — read this episode as a template for authentic communal reconstruction: slow, participatory, plurality-preserving, contrasted against both coercive uniformity and technocratic self-sufficiency. They do not participate in the original labor but assess whether the pattern generalizes.
narrative_ontology:constraint_stakeholder(human_transcendence_pathway__jerusalem_reading, theological_observers, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(human_transcendence_pathway__jerusalem_reading, diffuse).
narrative_ontology:fixing_cost_class(human_transcendence_pathway__jerusalem_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the genuine collective-action problem of rebuilding a dispersed and traumatized community's physical and social infrastructure without a coercive central authority — distributing labor by household and guild, renewing shared law by public reading and consent, and giving displaced and marginal persons a structural stake in the outcome so that the finished community is durable rather than merely imposed.
% TRANSFER_FUNCTION: Moves labor, material, and time from every participating household toward a shared wall, shared worship, and a renewed covenant; the poorer and returning-exile households contribute disproportionate physical labor relative to their resources, but what flows back to them is inclusion, security, and standing in a community that would otherwise have excluded or ignored them. No systematic transfer flows outward to an extracting party.
% ABSENT_VOICES: Surrounding regional powers who benefit from a weak, divided, or dependent population are not part of the internal deliberation and would object that the reconstruction reduces their leverage; their absence is a fact about who sits outside the community rather than an internal exclusion the arrangement itself perpetrates.
% DISAPPEARANCE_RATIONALE: If this pattern of patient, participatory, plurality-preserving rebuilding disappeared and were replaced by either coercive uniform administration or by simple dispersal, the community's physical infrastructure, its shared legal-religious identity, and the inclusion of marginal residents would all fail to materialize — the returned population would likely re-fragment or be absorbed without the distinct communal form the rebuilding produced.
% FOUNDING_PROBLEM: A displaced and traumatized population returns to a destroyed city with no walls, no functioning shared law, and deep internal division between returnees, those who never left, and resident foreigners; the founding problem is how to constitute one functioning, secure, and legitimate community out of this plurality without either coercive homogenization or indefinite fragmentation.
% FOUNDING_PROBLEM_CORROBORATION: The community's own leadership and the participating households attest the problem was real and the rebuilding effective; theological observers external to the immediate beneficiary group (later interpretive traditions, including this constraint's own sibling readings which treat the episode as contestable) corroborate that the pattern was historically distinctive precisely because it favored slow inclusion over coercive unification, though they contest whether it fully succeeded in eliminating internal hierarchy.
narrative_ontology:disappearance_verdict(human_transcendence_pathway__jerusalem_reading, world_rearranges).
narrative_ontology:founding_problem_status(human_transcendence_pathway__jerusalem_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(human_transcendence_pathway__jerusalem_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(human_transcendence_pathway__jerusalem_reading, 'none', 1).
narrative_ontology:epsilon_provenance(human_transcendence_pathway__jerusalem_reading, 0.22, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(human_transcendence_pathway__jerusalem_reading_tests).
:- end_tests(human_transcendence_pathway__jerusalem_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low-to-moderate (0.22) and falls slightly over the interval, reflecting persuasion- and formation-based coordination rather than coercive extraction — the effort costs real labor and forgone efficiency, but the story finds no identifiable party structurally extracting surplus from another. Suppression is low (0.15): the arrangement depends on ongoing consent and public reading of shared law, not on foreclosed exits. Theater ratio is low but rises marginally (0.12 to 0.18) as reconstruction matures and some communal practices (public readings, boundary ceremonies) settle into more routinized, partly performative form — a realistic feature of any institutionalizing coordination, not evidence of hollowing. Accessibility collapse is moderate (0.25): alternatives (permanent dispersal, external administration) were real options that were not suppressed, only not chosen. Resistance is moderate (0.35), reflecting genuine internal debate about intermarriage, boundary lines, and the terms of inclusion, without approaching the resistance profile of an extractive constraint.
 *
 * PERSPECTIVAL GAP:
 *   Local artisans and leadership, who retain mobile or constrained-but-voluntary exit options, would tend to read this arrangement as genuine coordination they helped construct. Returning exiles and marginalized residents, who face constrained exit (dispersal is catastrophic, not a live option), experience the same structure as more costly and higher-stakes even though it is not extractive — this is the seat divergence the engine should surface: same low ε, different lived weight of participation given different exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries include the returning exiles themselves, the reconstituted community as a whole, and specifically marginalized and foreign residents who are structurally included rather than excluded — this is the defining structural delta of the Jerusalem reading against readings that would either coerce uniformity or exclude plurality for efficiency. No victim group is named: the sacrifice of efficiency for solidarity falls diffusely across all participating households as a shared cost of coordination, not as an asymmetric extraction from an identifiable target. Surrounding hostile powers experience external pressure but are excluded from the internal arrangement rather than being victims of it.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (constituting a legitimate, secure, plural community after displacement) is authored as contested rather than resolved: leadership and participants attest the arrangement remains functionally necessary, while the analytical/theological observer seat notes the pattern's own internal tensions (debates over boundary and inclusion terms) were never fully settled. Because the arrangement's coordination function persists concretely (walls, shared law, renewed covenant) and disappearance would visibly rearrange the community, mandatrophy is not indicated here — this is a live coordination structure, not a persisting shell.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    efficiency_sacrifice_as_hidden_cost,
    'Is the ''sacrifice of efficiency for solidarity'' genuinely borne diffusely by all participants, or does it fall disproportionately on the least powerful (returning exiles, marginalized residents) in ways that approach a victim structure without being named as one?',
    'Comparative labor-burden analysis across household types during the reconstruction period; examination of whether inclusion of marginalized residents was accompanied by genuine redistribution of decision authority or remained largely symbolic.',
    'If the efficiency cost is shown to fall disproportionately on the powerless without commensurate gain in authority, this reading would need to add a victim group and would likely compute closer to tangled_rope than rope at the payer seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(efficiency_sacrifice_as_hidden_cost, empirical, 'Whether efficiency costs are truly diffuse or quietly concentrated on the marginalized.').

omega_variable(
    persuasion_versus_latent_coercion,
    'Is leadership''s authority in this reading purely persuasive, or does it carry latent coercive force (social exclusion, loss of covenant standing) that the low suppression score understates?',
    'Textual and historical analysis of consequences for non-compliance with covenant renewal — was refusal met with only social cost, or with harder exclusion from communal goods?',
    'If latent coercion is substantial, suppression should be revised upward and the classification would drift from rope toward tangled_rope even without a named victim group.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(persuasion_versus_latent_coercion, conceptual, 'Whether the low-suppression reading understates real compliance pressure.').

omega_variable(
    kernel_framing_choice,
    'Is the Jerusalem/Babel/Incarnational split the only defensible decomposition of the human_transcendence_pathway kernel, or does the kernel actually contain a fourth axis — e.g., a purely secular-pluralist reading that preserves plurality without invoking divine blessing at all?',
    'Compare this reading''s structural claims against a hypothetical secular-communitarian reading that also integrates plurality into communion but without theological grounding; determine whether the theological grounding is load-bearing for the low-ε, low-suppression profile or incidental to it.',
    'If a secular-pluralist reading would produce the same structural profile without divine grounding, the ''under divine blessing'' clause is decorative rather than structurally distinguishing, and the axiom set below would need revision.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_choice, conceptual, 'Whether the declared three-reading kernel decomposition is exhaustive or whether a further reading is latent.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(human_transcendence_pathway__jerusalem_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(huma_tr_t0, human_transcendence_pathway__jerusalem_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(huma_tr_t4, human_transcendence_pathway__jerusalem_reading, theater_ratio, 4, 0.14).
narrative_ontology:measurement(huma_tr_t8, human_transcendence_pathway__jerusalem_reading, theater_ratio, 8, 0.15).
narrative_ontology:measurement(huma_tr_t12, human_transcendence_pathway__jerusalem_reading, theater_ratio, 12, 0.16).
narrative_ontology:measurement(huma_tr_t16, human_transcendence_pathway__jerusalem_reading, theater_ratio, 16, 0.17).
narrative_ontology:measurement(huma_tr_t20, human_transcendence_pathway__jerusalem_reading, theater_ratio, 20, 0.18).

% Extraction over time
narrative_ontology:measurement(huma_be_t0, human_transcendence_pathway__jerusalem_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(huma_be_t4, human_transcendence_pathway__jerusalem_reading, base_extractiveness, 4, 0.26).
narrative_ontology:measurement(huma_be_t8, human_transcendence_pathway__jerusalem_reading, base_extractiveness, 8, 0.24).
narrative_ontology:measurement(huma_be_t12, human_transcendence_pathway__jerusalem_reading, base_extractiveness, 12, 0.23).
narrative_ontology:measurement(huma_be_t16, human_transcendence_pathway__jerusalem_reading, base_extractiveness, 16, 0.22).
narrative_ontology:measurement(huma_be_t20, human_transcendence_pathway__jerusalem_reading, base_extractiveness, 20, 0.22).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(human_transcendence_pathway__jerusalem_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(human_transcendence_pathway__jerusalem_reading, human_transcendence_pathway__babel_reading).
narrative_ontology:affects_constraint(human_transcendence_pathway__jerusalem_reading, human_transcendence_pathway__technocratic_vs_incarnational_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the human_transcendence_pathway kernel. The babel_reading claims unified technological/coercive coordination without transcendent reference; the technocratic_vs_incarnational_reading contrasts limit-elimination transcendence against gift-received transcendence. Each reading has its own ε (this one: low-to-moderate, 0.22, reflecting persuasion/formation over coercion) and its own beneficiary/victim structure (this one: broad community benefit including marginalized persons, no named victims). The readings are linked here for contamination/coupling analysis, not merged into one constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
