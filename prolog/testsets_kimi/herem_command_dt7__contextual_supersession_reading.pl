% ============================================================================
% CONSTRAINT STORY: herem_command_dt7__contextual_supersession_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_herem_command_dt7__contextual_supersession_reading, []).

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
 *   constraint_id: herem_command_dt7__contextual_supersession_reading
 *   human_readable: Herem Command (Contextual Supersession Reading)
 *   domain: biblical_hermeneutics/religious_ethics
 *
 * SUMMARY:
 *   The herem command of Deuteronomy 7 mandates total devotion or destruction
 *   of Canaanite nations and prohibits intermarriage. Under the contextual
 *   supersession reading, this directive was historically bounded to ancient
 *   Israel's settlement period and has been morally superseded by prophetic
 *   universalism and the Christian covenant. The constraint persists today
 *   primarily as textual inertia within canonical scripture, with low
 *   residual extraction concentrated in fundamentalist enclaves that reject
 *   supersession. This reading instantiates one of three structurally
 *   distinct commitments to the herem kernel; it forecloses the durable
 *   separation reading while coexisting with the allegorical displacement
 *   reading.
 *
 * KEY AGENTS:
 *   - residual_fundamentalist_enclaves (moderate/constrained): Local agenda-setters who administer literalist residual enforcement and benefit from group boundary maintenance.
 *   - coerced_community_members (powerless/identity_locked): Primary targets bearing social and familial costs of residual enforcement.
 *   - mainline_canonical_authority (institutional/mobile): Institutional agenda-setter that keeps the text in canon with supersession gloss; could remove it but does not.
 *   - interfaith_families (powerless/constrained): Narrow victim set specifically targeted by residual intermarriage prohibitions.
 *   - progressive_theologians (moderate/analytical): Observers documenting the historical boundedness and advocating fuller delegitimation.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(herem_command_dt7__contextual_supersession_reading, 0.18).
domain_priors:suppression_score(herem_command_dt7__contextual_supersession_reading, 0.42).
domain_priors:theater_ratio(herem_command_dt7__contextual_supersession_reading, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(herem_command_dt7__contextual_supersession_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(herem_command_dt7__contextual_supersession_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(herem_command_dt7__contextual_supersession_reading, theater_ratio, 0.68).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(herem_command_dt7__contextual_supersession_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(herem_command_dt7__contextual_supersession_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(herem_command_dt7__contextual_supersession_reading, piton).
narrative_ontology:human_readable(herem_command_dt7__contextual_supersession_reading, "Herem Command (Contextual Supersession Reading)").
narrative_ontology:topic_domain(herem_command_dt7__contextual_supersession_reading, "biblical_hermeneutics/religious_ethics").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(herem_command_dt7__contextual_supersession_reading, '1a68b849-9767-4178-9cc4-fb41bdcd1251').
narrative_ontology:cs_kernel_codification('1a68b849-9767-4178-9cc4-fb41bdcd1251', fixed_text).
narrative_ontology:cs_authority_grounding('1a68b849-9767-4178-9cc4-fb41bdcd1251', lineage).
narrative_ontology:cs_interpretation_layer_present('1a68b849-9767-4178-9cc4-fb41bdcd1251').
narrative_ontology:cs_reading_relation('1a68b849-9767-4178-9cc4-fb41bdcd1251', herem_command_dt7__durable_separation_reading, forecloses).
narrative_ontology:cs_reading_relation('1a68b849-9767-4178-9cc4-fb41bdcd1251', herem_command_dt7__allegorical_displacement_reading, coexists_with).
narrative_ontology:cs_axiom('1a68b849-9767-4178-9cc4-fb41bdcd1251', foundational, herem_historically_bounded_to_settlement).
narrative_ontology:cs_axiom_status(herem_historically_bounded_to_settlement, holdable).
narrative_ontology:cs_axiom_grounding('1a68b849-9767-4178-9cc4-fb41bdcd1251', herem_historically_bounded_to_settlement, empirically_contingent).
narrative_ontology:cs_axiom('1a68b849-9767-4178-9cc4-fb41bdcd1251', foundational, prophetic_ethical_supersession).
narrative_ontology:cs_axiom_status(prophetic_ethical_supersession, holdable).
narrative_ontology:cs_axiom_grounding('1a68b849-9767-4178-9cc4-fb41bdcd1251', prophetic_ethical_supersession, deontological).
narrative_ontology:cs_reference_frame('1a68b849-9767-4178-9cc4-fb41bdcd1251', ancient_israelite_settlement_mandate).
narrative_ontology:cs_drift_state('1a68b849-9767-4178-9cc4-fb41bdcd1251', christian_prophetic_era, gap(axiom_overriding, severe, true)).
narrative_ontology:cs_created_at('1a68b849-9767-4178-9cc4-fb41bdcd1251', '').
narrative_ontology:cs_kernel_id(herem_command_dt7__contextual_supersession_reading, herem_command_dt7).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(herem_command_dt7__contextual_supersession_reading, residual_fundamentalist_enclaves).
narrative_ontology:constraint_victim(herem_command_dt7__contextual_supersession_reading, coerced_community_members).
narrative_ontology:constraint_victim(herem_command_dt7__contextual_supersession_reading, interfaith_families).
narrative_ontology:constraint_vindicates(herem_command_dt7__contextual_supersession_reading, prophetic_universalism).
narrative_ontology:constraint_vindicates(herem_command_dt7__contextual_supersession_reading, christian_covenant_supersession).
narrative_ontology:constraint_vindicates(herem_command_dt7__contextual_supersession_reading, historical_criticism).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintains the biblical text in canonical circulation while promulgating the supersession reading through lectionary, theological education, and official teaching. Could remove or further marginalize the text but preserves it as historically bounded testimony. Does not enforce the command literally.
narrative_ontology:constraint_stakeholder(herem_command_dt7__contextual_supersession_reading, mainline_canonical_authority, agenda_setter,
    institutional, generational, mobile, global).

% Administer literal enforcement of herem-derived boundaries (endogamy, shunning, sectarian separation) within their communities. Derive group cohesion and boundary maintenance from the command's residual authority. Could adopt the supersession reading but reject it as moral compromise.
narrative_ontology:constraint_stakeholder(herem_command_dt7__contextual_supersession_reading, residual_fundamentalist_enclaves, agenda_setter,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(herem_command_dt7__contextual_supersession_reading, residual_fundamentalist_enclaves, beneficiary).

% Live under residual fundamentalist enforcement of herem-derived prohibitions on intermarriage and association. Bear social and familial costs for crossing boundaries. Exit is blocked by identity fusion with family and community; leaving means total relational severance.
narrative_ontology:constraint_stakeholder(herem_command_dt7__contextual_supersession_reading, coerced_community_members, payer,
    powerless, immediate, identity_locked, local).

% Targeted by residual enforcement as the primary contemporary manifestation of the intermarriage boundary. Face exclusion, pressure to convert, or shunning. Their existence is treated as a threat to group purity.
narrative_ontology:constraint_stakeholder(herem_command_dt7__contextual_supersession_reading, interfaith_families, payer,
    powerless, immediate, constrained, local).

% Document the historical boundedness of herem and advocate for its full delegitimation. Argue that the supersession reading, while preferable to literalism, still carries inertial weight that enables residual enforcement.
narrative_ontology:constraint_stakeholder(herem_command_dt7__contextual_supersession_reading, progressive_theologians, observer,
    moderate, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(herem_command_dt7__contextual_supersession_reading, diffuse).
narrative_ontology:fixing_cost_class(herem_command_dt7__contextual_supersession_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Historically coordinated Israelite settlement identity and boundary maintenance during the conquest and settlement period; currently coordinates nothing legitimate under the supersession reading, though residual fundamentalist enclaves still use it for local boundary policing.
% TRANSFER_FUNCTION: Historically transferred land and social dominance from Canaanite populations to Israelite identity; currently transfers social compliance and in-group conformity from coerced community members to residual fundamentalist enclave authority.
% ABSENT_VOICES: Canaanite descendants and contemporary Palestinian communities are structurally absent from supersession theological discourse; secular biblical scholars and anthropologists who treat herem as ancient Near Eastern genocide rhetoric are marginalized in confessional interpretation settings.
% DISAPPEARANCE_RATIONALE: If the residual enforcement of herem vanished, coerced community members would gain freedom to marry and associate across boundaries without social penalty; fundamentalist enclaves would lose a primary boundary mechanism and likely reorganize around other purity markers; mainline institutions would continue with minor liturgical adjustment.
% FOUNDING_PROBLEM: Israelite identity formation and land settlement in a contested ancient Near Eastern environment requiring group cohesion against assimilative pressures and rival claimants.
% FOUNDING_PROBLEM_CORROBORATION: Archaeologists and ancient historians attest the settlement period context; mainline Jewish and Christian theologians attest the problem is historically closed and morally superseded. Secular ethicists and historians outside the tradition corroborate that the ancient settlement context no longer obtains, while prophetic texts (Isaiah 56, Ruth) and early Christian sources (Acts 15) from within the tradition attest the moral supersession.
narrative_ontology:disappearance_verdict(herem_command_dt7__contextual_supersession_reading, world_rearranges).
narrative_ontology:founding_problem_status(herem_command_dt7__contextual_supersession_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(herem_command_dt7__contextual_supersession_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(herem_command_dt7__contextual_supersession_reading, 'none', 1).
narrative_ontology:epsilon_provenance(herem_command_dt7__contextual_supersession_reading, 0.18, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(herem_command_dt7__contextual_supersession_reading_tests).
:- end_tests(herem_command_dt7__contextual_supersession_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.18) because the mainline tradition has superseded the command; only residual fundamentalist enclaves generate extraction. Theater ratio is high (0.68) because the canonical text remains in performative circulation despite its primary ethical function being atrophied â it is read historically, liturgically glossed, or ritually referenced without literal implementation. Suppression is moderate (0.42) because residual enclaves still actively police boundaries, but mainstream institutions no longer enforce the command. The measurement series trace a lifecycle drift from functional extraction in the settlement period to theatrical maintenance today. Resistance is moderate (0.50) from victims and reformers, but insufficient to overcome canonical inertia.
 *
 * PERSPECTIVAL GAP:
 *   The mainline canonical authority experiences the constraint as a settled historical text requiring interpretive management â low extraction, high theater, manageable through scholarship and liturgy. The coerced community member experiences it as an active social coercion with high identity-lock and immediate cost. The residual fundamentalist enclave experiences it as a boundary resource. These divergences are structurally encoded by power and exit differences.
 *
 * DIRECTIONALITY LOGIC:
 *   Mainline canonical authority sits near the beneficiary end (d approx 0.1) because the supersession reading subsidizes its moral legitimacy and historical-critical sophistication. Residual fundamentalist enclaves sit at mixed directionality (d approx 0.3) â they enforce the constraint and thus appear as beneficiaries of group cohesion, but they also bear the social cost of marginalization and resistance. Coerced community members and interfaith families sit near the full-target end (d approx 0.9) because the constraint extracts compliance and relational freedom from them with identity-locked exits. Progressive theologians sit at analytical distance (d approx 0.5).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â Israelite identity formation during settlement â is dead. The mainline tradition acknowledges this, yet the text persists in the canon without formal removal. The mismatch between founding_problem_status=dead and disappearance_verdict=world_rearranges flags a mandatrophy condition: the arrangement persists without its original function, maintained by canonical inertia and residual fundamentalist theatricality. This prevents mislabeling the residual enforcement as a live scaffold (its transition is complete) or as a functional rope (it coordinates nothing legitimate in the present).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    herem_kernel_reading_ambiguity,
    'Does the contextual supersession reading fully dissolve the constraint, or does it leave a residual structure that continues to enable extraction through canonical persistence?',
    'Compare communities that have formally removed or radically marginalized herem texts (e.g., through revised lectionaries) versus those that retain them with supersession glosses; measure residual enforcement rates.',
    'If residual extraction tracks canonical persistence, the supersession reading is an incomplete piton dissolution; if not, the extraction is independent of this reading''s constraint structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(herem_kernel_reading_ambiguity, conceptual, 'Whether supersession dissolves the constraint or leaves residual extraction.').

omega_variable(
    residual_enforcement_structural_vs_internalized,
    'Is the measured suppression in residual fundamentalist enclaves structural (community-enforced shunning and boundary policing) or internalized (theological guilt and identity fusion)?',
    'Post-exit trajectory study: if coercion persists after physical and social exit from the enclave, suppression is partially internalized.',
    'Internalized suppression raises effective extraction above the structural measure; would shift the victim seat classification toward higher chi.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(residual_enforcement_structural_vs_internalized, empirical, 'Structural versus internalized suppression mechanism in residual enclaves.').

omega_variable(
    sibling_reading_foreclosure,
    'Does the contextual supersession reading logically foreclose the durable_separation reading within a single theological framework, or do they merely coexist as incompatible traditions?',
    'Analyze whether the core axioms of contextual supersession (historical boundedness plus ethical supersession) are logically compatible with durable separation''s claim of timeless mandate.',
    'If foreclosed, the kernel is genuinely bifurcated with no unified framework possible; if coexisting, the kernel allows plural legitimate readings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure, conceptual, 'Logical relationship between contextual supersession and durable separation readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(herem_command_dt7__contextual_supersession_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(here_tr_t0, herem_command_dt7__contextual_supersession_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(here_tr_t5, herem_command_dt7__contextual_supersession_reading, theater_ratio, 5, 0.15).
narrative_ontology:measurement(here_tr_t10, herem_command_dt7__contextual_supersession_reading, theater_ratio, 10, 0.35).
narrative_ontology:measurement(here_tr_t15, herem_command_dt7__contextual_supersession_reading, theater_ratio, 15, 0.55).
narrative_ontology:measurement(here_tr_t20, herem_command_dt7__contextual_supersession_reading, theater_ratio, 20, 0.62).
narrative_ontology:measurement(here_tr_t25, herem_command_dt7__contextual_supersession_reading, theater_ratio, 25, 0.66).
narrative_ontology:measurement(here_tr_t30, herem_command_dt7__contextual_supersession_reading, theater_ratio, 30, 0.68).

% Extraction over time
narrative_ontology:measurement(here_be_t0, herem_command_dt7__contextual_supersession_reading, base_extractiveness, 0, 0.85).
narrative_ontology:measurement(here_be_t5, herem_command_dt7__contextual_supersession_reading, base_extractiveness, 5, 0.8).
narrative_ontology:measurement(here_be_t10, herem_command_dt7__contextual_supersession_reading, base_extractiveness, 10, 0.65).
narrative_ontology:measurement(here_be_t15, herem_command_dt7__contextual_supersession_reading, base_extractiveness, 15, 0.4).
narrative_ontology:measurement(here_be_t20, herem_command_dt7__contextual_supersession_reading, base_extractiveness, 20, 0.3).
narrative_ontology:measurement(here_be_t25, herem_command_dt7__contextual_supersession_reading, base_extractiveness, 25, 0.22).
narrative_ontology:measurement(here_be_t30, herem_command_dt7__contextual_supersession_reading, base_extractiveness, 30, 0.18).

% Suppression requirement over time
narrative_ontology:measurement(here_su_t0, herem_command_dt7__contextual_supersession_reading, suppression_requirement, 0, 0.9).
narrative_ontology:measurement(here_su_t5, herem_command_dt7__contextual_supersession_reading, suppression_requirement, 5, 0.85).
narrative_ontology:measurement(here_su_t10, herem_command_dt7__contextual_supersession_reading, suppression_requirement, 10, 0.7).
narrative_ontology:measurement(here_su_t15, herem_command_dt7__contextual_supersession_reading, suppression_requirement, 15, 0.5).
narrative_ontology:measurement(here_su_t20, herem_command_dt7__contextual_supersession_reading, suppression_requirement, 20, 0.45).
narrative_ontology:measurement(here_su_t25, herem_command_dt7__contextual_supersession_reading, suppression_requirement, 25, 0.43).
narrative_ontology:measurement(here_su_t30, herem_command_dt7__contextual_supersession_reading, suppression_requirement, 30, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(herem_command_dt7__contextual_supersession_reading, identity_coordination).
narrative_ontology:affects_constraint(herem_command_dt7__contextual_supersession_reading, herem_command_dt7__durable_separation_reading).
narrative_ontology:affects_constraint(herem_command_dt7__contextual_supersession_reading, herem_command_dt7__allegorical_displacement_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the herem_command_dt7 kernel. The kernel decomposes into structurally distinct claims: allegorical_displacement (spiritual typology), contextual_supersession (historical boundedness plus ethical supersession), and durable_separation (timeless ethnic mandate). Each has distinct epsilon, beneficiary or victim structure, and classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
