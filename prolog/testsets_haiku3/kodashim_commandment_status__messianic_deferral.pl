% ============================================================================
% CONSTRAINT STORY: kodashim_commandment_status__messianic_deferral
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kodashim_commandment_status__messianic_deferral, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: kodashim_commandment_status__messianic_deferral
 *   human_readable: Kodashim Commandment Status: Messianic Deferral Reading
 *   domain: religious/halakhic/commitment_system
 *
 * SUMMARY:
 *   The messianic-deferral reading interprets the commandment to offer
 *   sacrifices (kodashim) as suspended but not obsolete: the Temple's
 *   destruction removed the performance context, but the commandment remains
 *   binding in the form of study. Study maintains readiness for future
 *   restoration when the Temple is rebuilt and sacrifice resumes. This
 *   reading justifies present-generation study effort as deferred investment
 *   in a future contingency. The constraint imposes moderate extractiveness
 *   because the opportunity cost of study time is real (hours not available
 *   for material welfare or alternative religious practice), but the
 *   coordination function is genuine (study preserves legal continuity and
 *   identity coherence). The measurement series tracks the constraint's
 *   evolution across 2,000 years: extractiveness rises gradually as
 *   institutional infrastructure hardens around sacrifice-law study, theater
 *   ratio rises sharply from medieval period onward (more study activity
 *   becomes performative maintenance of deferral than active legal
 *   development), and suppression rises as the deferral reading becomes more
 *   institutionally entrenched and alternative readings are marginalized.
 *   This constraint is ONE READING of the contested kernel
 *   'kodashim_commandment_status.' Sibling readings include
 *   'performance_only' (suspension = void; only present-contingent
 *   commandments bind) and 'study_as_performance' (study itself fulfills the
 *   commandment; no future contingency required). The kernel contest is
 *   unresolved: different communities and eras instantiate different
 *   readings.
 *
 * KEY AGENTS:
 *   - Messianic-preparation authority: sets and administers the deferral reading; derives institutional legitimacy from perpetuating the study obligation
 *   - Talmudic study infrastructure (yeshivas, lineage): benefits from resource allocation to sacrifice-law study; perpetuates the reading through pedagogy
 *   - Individual practitioners: bear opportunity cost of study time; identity-locked into the study obligation by religious and communal integration
 *   - Competing commandment systems: excluded from resource allocation; would advocate for alternative priorities if heard
 *   - Lineage authority (transmitted halakhic tradition): provides epistemic grounding for the deferral reading; acts as source of institutional authority
 *   - Messianic contingency (Temple rebuilding): the contested future condition on which deferral justification rests; not an agent but a foundational assumption
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kodashim_commandment_status__messianic_deferral, 0.58).
domain_priors:suppression_score(kodashim_commandment_status__messianic_deferral, 0.42).
domain_priors:theater_ratio(kodashim_commandment_status__messianic_deferral, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kodashim_commandment_status__messianic_deferral, extractiveness, 0.58).
narrative_ontology:constraint_metric(kodashim_commandment_status__messianic_deferral, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(kodashim_commandment_status__messianic_deferral, theater_ratio, 0.64).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kodashim_commandment_status__messianic_deferral, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(kodashim_commandment_status__messianic_deferral, resistance, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kodashim_commandment_status__messianic_deferral, scaffold).
narrative_ontology:human_readable(kodashim_commandment_status__messianic_deferral, "Kodashim Commandment Status: Messianic Deferral Reading").
narrative_ontology:topic_domain(kodashim_commandment_status__messianic_deferral, "religious/halakhic/commitment_system").

domain_priors:requires_active_enforcement(kodashim_commandment_status__messianic_deferral).
narrative_ontology:has_sunset_clause(kodashim_commandment_status__messianic_deferral).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kodashim_commandment_status__messianic_deferral, 'f8ae3618-5047-4ab4-8c00-e7808fc0d5c2').
narrative_ontology:cs_kernel_codification('f8ae3618-5047-4ab4-8c00-e7808fc0d5c2', fixed_text).
narrative_ontology:cs_authority_grounding('f8ae3618-5047-4ab4-8c00-e7808fc0d5c2', lineage).
narrative_ontology:cs_interpretation_layer_present('f8ae3618-5047-4ab4-8c00-e7808fc0d5c2').
narrative_ontology:cs_reading_relation('f8ae3618-5047-4ab4-8c00-e7808fc0d5c2', kodashim_commandment_status__performance_only, forecloses).
narrative_ontology:cs_reading_relation('f8ae3618-5047-4ab4-8c00-e7808fc0d5c2', kodashim_commandment_status__study_as_performance, coexists_with).
narrative_ontology:cs_axiom('f8ae3618-5047-4ab4-8c00-e7808fc0d5c2', foundational, commandment_ontologically_bound_despite_performance_impossibility).
narrative_ontology:cs_axiom_status(commandment_ontologically_bound_despite_performance_impossibility, holdable).
narrative_ontology:cs_axiom_grounding('f8ae3618-5047-4ab4-8c00-e7808fc0d5c2', commandment_ontologically_bound_despite_performance_impossibility, deontological).
narrative_ontology:cs_axiom('f8ae3618-5047-4ab4-8c00-e7808fc0d5c2', foundational, messianic_future_rendering_deferral_legitimate).
narrative_ontology:cs_axiom_status(messianic_future_rendering_deferral_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('f8ae3618-5047-4ab4-8c00-e7808fc0d5c2', messianic_future_rendering_deferral_legitimate, theological).
narrative_ontology:cs_reference_frame('f8ae3618-5047-4ab4-8c00-e7808fc0d5c2', commandment_restoration_contingency).
narrative_ontology:cs_drift_state('f8ae3618-5047-4ab4-8c00-e7808fc0d5c2', contemporary_post_enlightenment, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('f8ae3618-5047-4ab4-8c00-e7808fc0d5c2', '').
narrative_ontology:cs_kernel_id(kodashim_commandment_status__messianic_deferral, kodashim_commandment_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kodashim_commandment_status__messianic_deferral, messianic_preparation_authority).
narrative_ontology:constraint_beneficiary(kodashim_commandment_status__messianic_deferral, talmudic_study_infrastructure).
narrative_ontology:constraint_victim(kodashim_commandment_status__messianic_deferral, present_generation_material_needs).
narrative_ontology:constraint_victim(kodashim_commandment_status__messianic_deferral, alternative_religious_practice_options).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(kodashim_commandment_status__messianic_deferral, individual_practitioners).
narrative_ontology:constraint_victim(kodashim_commandment_status__messianic_deferral, individual_practitioners).
narrative_ontology:constraint_vindicates(kodashim_commandment_status__messianic_deferral, commandment_restoration_contingency).
narrative_ontology:constraint_vindicates(kodashim_commandment_status__messianic_deferral, study_as_readiness_maintenance).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintains the interpretive framework that the commandment is suspended, not voided, and that continuous study preserves readiness for future restoration. Administers the study obligation and justifies it as preparation for the messianic era when the Temple will be rebuilt and sacrifice will resume. This authority interprets the kernel (the commandment's status) and enforces the study requirement through institutional channels—schools, communities, lineage structures. Collects no direct material benefit but derives institutional legitimacy and perpetuation from the deferral frame.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__messianic_deferral, messianic_preparation_authority, agenda_setter,
    institutional, civilizational, identity_locked, global).

% Yeshivas, study circles, and transmitted rabbinic traditions benefit from the messianic-deferral reading by receiving a steady stream of students and resources allocated to sacrifice-law study. The infrastructure exists because the commandment is treated as non-obsolete; if reading shifted to 'performance-only' (suspended = voided), study resources would redirect. Maintains readiness through pedagogical activity; the reading sustains their institutional function.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__messianic_deferral, talmudic_study_infrastructure, beneficiary,
    organized, generational, constrained, global).

% Bear the opportunity cost of study time: hours spent learning sacrifice laws are hours not spent on direct material welfare, economic production, or alternative religious practices (e.g., prayer, ethical action, community service) that produce present-generation benefit. They also receive the coordinating benefit of belonging to a study community and maintaining a coherent religious identity indexed to messianic hope. Their exit from the study obligation is theoretically possible but practically foreclosed by religious identity and community integration.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__messianic_deferral, individual_practitioners, payer,
    moderate, biographical, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(kodashim_commandment_status__messianic_deferral, individual_practitioners, beneficiary).

% Alternative interpretive communities (those emphasizing ritual purity laws, ethical commandments, or prayer-based practice as primary) are structurally subordinated within the institutional hierarchy because the messianic-deferral reading justifies the allocation of study resources to sacrifice law above competing areas. They would advocate for rebalancing if heard; their voices are muted by the authority structure.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__messianic_deferral, competing_commandment_systems, excluded,
    moderate, generational, constrained, regional).

% The transmitted interpretive lineage (halakhic tradition) that grounds the messianic-deferral reading in prior rulings, scriptural exegesis, and rabbinic consensus. Acts as the source of legitimacy for the deferral frame. Does not itself administer; rather, provides the authority structure that makes deferral administratively coherent.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__messianic_deferral, lineage_authority, agenda_setter,
    institutional, civilizational, analytical, global).

% The future rebuilding of the Temple and resumption of sacrificial service—a contested, non-actor contingency that the messianic-deferral reading treats as the warrant for present study. Its status (likely, impossible, or merely possible) is not itself a seated agent but is foundational to the reading's justification.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__messianic_deferral, messianic_contingency_condition, excluded,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(kodashim_commandment_status__messianic_deferral, messianic_contingency_condition).

% Religious studies scholar or comparative religionist analyzing the reading's structural role in perpetuating study obligation and deferring material obligation. Takes no stance on the reading's truth; observes the constraint's operation.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__messianic_deferral, comparative_observer, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(kodashim_commandment_status__messianic_deferral, messianic_preparation_authority).
narrative_ontology:fixing_cost_class(kodashim_commandment_status__messianic_deferral, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves continuity of legal knowledge across a historical period in which the primary referent (Temple sacrifice) is unavailable. Study of sacrifice law coordinates the community around messianic hope, shared interpretive authority, and deferred fulfillment; it maintains conceptual readiness so the commandment can be operationalized if contingency changes.
% TRANSFER_FUNCTION: Redirects effort and attention (study hours, community resources, institutional investment) from present-generation material welfare or alternative religious practices toward sacrifice-law study. The transfer runs from individual time and competing religious priorities to the study infrastructure and messianic-preparation authority. It is justified as a deferred investment—the transfer is legitimate only insofar as the messianic contingency is treated as real.
% ABSENT_VOICES: Materialist or pragmatist interpreters (those who would say sacrifice law is obsolete and study time should redirect to ethical action or social welfare) are structurally excluded because they would deny the messianic contingency on which the deferral reading rests. They are not present in the authoritative lineage of this reading, though dissent exists in neighboring communities and in historical record.
% DISAPPEARANCE_RATIONALE: If the messianic-deferral reading and its enforcement dissolved, study resources would redistribute: some toward alternative religious practice (prayer, ethics, ritual purity), some toward material welfare, some toward secular disciplines. The institutional infrastructure (yeshivas organized around sacrifice law) would shrink or repurpose. Communities would reorganize around different religious priorities. The constraint's disappearance would reshape how the community allocates time and identity.
% FOUNDING_PROBLEM: After the Temple's destruction in 70 CE, sacrifice became impossible. The commandment did not disappear from scripture, but its direct performance became suspended. The founding problem was: how to maintain the legal tradition and religious identity of a commandment-system built on sacrifice without performing sacrifice?
% FOUNDING_PROBLEM_CORROBORATION: The messianic-deferral reading is attested by the transmitted halakhic lineage (Talmud, medieval codes, modern yeshiva practice). The competing reading—that the commandment is voided—is attested by alternative Jewish communities and scholars who deny the messianic contingency. Outside corroboration comes from historians of Jewish law (e.g., David Weiss Halivni, Daniel Boyarin) who document the historical shift from performance to study as a reconstruction, not a discovery. No single seat's assertion suffices; the legitimacy of the reading itself is contested across the lineage.
narrative_ontology:disappearance_verdict(kodashim_commandment_status__messianic_deferral, world_rearranges).
narrative_ontology:founding_problem_status(kodashim_commandment_status__messianic_deferral, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kodashim_commandment_status__messianic_deferral, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(kodashim_commandment_status__messianic_deferral, 'none', 1).
narrative_ontology:epsilon_provenance(kodashim_commandment_status__messianic_deferral, 0.58, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kodashim_commandment_status__messianic_deferral_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(kodashim_commandment_status__messianic_deferral, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(kodashim_commandment_status__messianic_deferral_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58 at endpoint) is moderate, not high, because genuine coordination function exists (study preserves legal knowledge and community identity across a period of impossibility). But extractiveness is elevated above pure coordination by the opportunity cost of study time—present-generation material welfare and alternative religious practices are subordinated to messianic preparation. The ascending trajectory (0.35→0.58 across 2,000 years) reflects institutional hardening: as infrastructure around sacrifice-law study becomes more elaborate (yeshiva system, canonical texts, transmission lineages), the constraint's extractive grip increases. Theater ratio (0.45→0.64) rises more sharply from ~500 CE onward, reflecting the institutional institutionalization of study as perpetual readiness activity rather than as active legal development—as the messianic contingency recedes further into the future, study becomes increasingly performative (theatrical maintenance of deferral) rather than functionally driven. Suppression (0.28→0.42) is moderate because the reading is enforced by institutional authority and identity-lock, not by explicit coercion, but the ascending trajectory reflects increasing marginalization of competing readings. Accessibility_collapse (0.71) is high because practitioners born into the study community face very limited exit: the reading is woven into religious identity, communal belonging, and intellectual formation. Resistance (0.38) is moderate because some communities and thinkers historically rejected the deferral reading, but dissent was suppressed by the dominant lineage authority. The claim/metric gap is intentional: the constraint is CLAIMED as scaffold (temporary, justified by transition to messianic future) while metrics describe sustained institutional extraction—the engine should flag this as potential mandatrophy (founding problem—Temple reconstruction—is contested and recedes with time).
 *
 * PERSPECTIVAL GAP:
 *   From the messianic-preparation authority's seat, the arrangement is genuine coordination: study preserves a vital legal and identity tradition across an impossible period, maintaining readiness for when the Temple is rebuilt. The study obligation is a successful preservation mechanism. From the practitioner's seat (especially materialist or pragmatist practitioners), the same structure operates as imposed extraction: hours of study time are demanded, with justification resting on a future contingency (Temple rebuilding) that may never materialize. The opportunity cost to present-generation welfare is real and certain; the benefit is deferred and contingent. The engine should compute these seats differently: the authority seat should show lower extraction (beneficiary-like), while practitioner seats show higher extraction (payer-like). The stakeholder structure in this JSON reflects this divergence through different roles and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   The messianic-preparation authority sits near the beneficiary end (d≈0.15): it administers the reading, derives institutional legitimacy from it, and controls the study infrastructure. Its exit is analytical (it is an institutional seat, not subject to personal exit constraints). Individual practitioners sit near the payer end (d≈0.75): they bear the opportunity cost, are identity-locked (making exit costly), and lack control over the reading's interpretation. The talmudic study infrastructure sits symmetric (d≈0.5): it both collects resources (beneficiary function) and maintains the expensive study activity (payer function). The competing commandment systems sit excluded (their directionality would be 1.0 if they were admitted, but they are structurally blocked from the constraint's operation). This directionality profile should be stable across the interval; the ascending extractiveness trajectory reflects institutional hardening, not seat-repositioning.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint is claimed as scaffold (temporary, justified by the messianic contingency). Mandatrophy would arise if: (1) the founding problem (Temple rebuild) is overtaken as a live priority; (2) the contingency becomes increasingly improbable as centuries pass; (3) the study obligation persists even as its justification weakens. The measurement trajectory shows signs of mandatrophy: theater_ratio rises sharply (0.45→0.64), indicating that study is increasingly performative maintenance of the deferral reading rather than functionally responsive to the messianic contingency. The constraint survives not because the contingency is imminent but because the reading is institutionally entrenched. This is classic mandatrophy: the founding problem (Temple rebuild enabling sacrifice) is contested and receding; the arrangement persists by inertia and institutional self-perpetuation. An omega variable (below) documents whether the messianic contingency should be reclassified as obsolete, converting the constraint from scaffold to piton.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    messianic_contingency_realism,
    'Is the messianic contingency (Temple rebuilding and resumption of sacrifice) a live, credible future possibility, or has it become so distant/implausible that the study obligation should be reclassified as obsolete?',
    'Historical/theological analysis: measure the trajectory of messianic expectation within the lineage. If mainstream authority has abandoned messianic imminent-return doctrine and replaced it with indefinite deferral or reinterpretation (e.g., ''Temple'' as metaphorical), the contingency is overridden and the scaffold claim fails.',
    'If the contingency is reclassified as obsolete, the constraint shifts from scaffold (justified by future transition) to piton (persistence by inertia despite dead founding justification). Extractiveness attribution would shift from ''deferred investment'' to ''extraction masked by performance.''',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(messianic_contingency_realism, conceptual, 'Whether the messianic contingency remains credible or has been overridden as justification.').

omega_variable(
    study_fulfillment_boundary,
    'Does study of sacrifice law actually maintain readiness for future performance, or is study itself the fulfillment (per the competing ''study_as_performance'' reading)?',
    'Lineage analysis: examine whether halakhic sources treat study as preparatory (means to future performance) or as fulfillment (end-in-itself). If modern authorities interpret study as itself fulfilling the commandment, the distinction between deferral and study-as-performance collapses.',
    'If study is fulfillment, extractiveness shifts downward (the opportunity cost is investment in performance, not deferred contingency) and the constraint becomes closer to rope (coordination without extraction). The sibling ''study_as_performance'' reading would no longer be distinct.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(study_fulfillment_boundary, conceptual, 'Whether study maintains readiness (deferral reading) or constitutes fulfillment (study-as-performance reading).').

omega_variable(
    opportunity_cost_attribution,
    'Is the opportunity cost of sacrifice-law study (hours not available for material welfare or alternative practices) properly attributed to THIS reading as extraction, or is it inherent to any study obligation?',
    'Comparative analysis across the three readings: measure whether the opportunity-cost profile differs across readings. If all three readings impose equal study obligation, opportunity cost is not a distinguishing extractiveness marker for this reading.',
    'If opportunity cost is not reading-specific, extractiveness should be lower (0.40–0.48) and focused only on the asymmetry between present-generation cost and future-contingent benefit. The piton drift signal weakens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(opportunity_cost_attribution, empirical, 'Whether opportunity cost is specific to the deferral reading or generic to the commandment-system.').

omega_variable(
    performance_only_foreclosure,
    'Can the messianic-deferral reading and the performance-only reading coexist in a single coherent framework, or does one logically foreclose the other?',
    'Analytic: the deferral reading treats suspension as non-voiding; performance-only treats suspension as voiding. These are logically contradictory: either the commandment is bound (deferral) or not (performance-only). Can a single party hold both? If no—they are foreclosing. If yes (compartmentalized or conditional on context)—they coexist.',
    'If foreclosing: the reading_relations entry for performance-only should be ''forecloses'' instead of ''coexists_with.'' If coexisting: the current ''coexists_with'' is correct. This affects the engine''s computation of competing-reading suppression.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(performance_only_foreclosure, conceptual, 'Logical structure of the reading-relation to ''performance_only'' reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kodashim_commandment_status__messianic_deferral, 0, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(koda_tr_t0, kodashim_commandment_status__messianic_deferral, theater_ratio, 0, 0.45).
narrative_ontology:measurement_basis(koda_tr_t0, observed).
narrative_ontology:measurement(koda_tr_t250, kodashim_commandment_status__messianic_deferral, theater_ratio, 250, 0.5).
narrative_ontology:measurement_basis(koda_tr_t250, observed).
narrative_ontology:measurement(koda_tr_t500, kodashim_commandment_status__messianic_deferral, theater_ratio, 500, 0.55).
narrative_ontology:measurement_basis(koda_tr_t500, observed).
narrative_ontology:measurement(koda_tr_t1000, kodashim_commandment_status__messianic_deferral, theater_ratio, 1000, 0.62).
narrative_ontology:measurement_basis(koda_tr_t1000, observed).
narrative_ontology:measurement(koda_tr_t1500, kodashim_commandment_status__messianic_deferral, theater_ratio, 1500, 0.64).
narrative_ontology:measurement_basis(koda_tr_t1500, observed).
narrative_ontology:measurement(koda_tr_t2000, kodashim_commandment_status__messianic_deferral, theater_ratio, 2000, 0.64).
narrative_ontology:measurement_basis(koda_tr_t2000, observed).

% Extraction over time
narrative_ontology:measurement(koda_be_t0, kodashim_commandment_status__messianic_deferral, base_extractiveness, 0, 0.35).
narrative_ontology:measurement_basis(koda_be_t0, observed).
narrative_ontology:measurement(koda_be_t250, kodashim_commandment_status__messianic_deferral, base_extractiveness, 250, 0.42).
narrative_ontology:measurement_basis(koda_be_t250, observed).
narrative_ontology:measurement(koda_be_t500, kodashim_commandment_status__messianic_deferral, base_extractiveness, 500, 0.48).
narrative_ontology:measurement_basis(koda_be_t500, observed).
narrative_ontology:measurement(koda_be_t1000, kodashim_commandment_status__messianic_deferral, base_extractiveness, 1000, 0.55).
narrative_ontology:measurement_basis(koda_be_t1000, observed).
narrative_ontology:measurement(koda_be_t1500, kodashim_commandment_status__messianic_deferral, base_extractiveness, 1500, 0.57).
narrative_ontology:measurement_basis(koda_be_t1500, observed).
narrative_ontology:measurement(koda_be_t2000, kodashim_commandment_status__messianic_deferral, base_extractiveness, 2000, 0.58).
narrative_ontology:measurement_basis(koda_be_t2000, observed).

% Suppression requirement over time
narrative_ontology:measurement(koda_su_t0, kodashim_commandment_status__messianic_deferral, suppression_requirement, 0, 0.28).
narrative_ontology:measurement_basis(koda_su_t0, observed).
narrative_ontology:measurement(koda_su_t250, kodashim_commandment_status__messianic_deferral, suppression_requirement, 250, 0.32).
narrative_ontology:measurement_basis(koda_su_t250, observed).
narrative_ontology:measurement(koda_su_t500, kodashim_commandment_status__messianic_deferral, suppression_requirement, 500, 0.36).
narrative_ontology:measurement_basis(koda_su_t500, observed).
narrative_ontology:measurement(koda_su_t1000, kodashim_commandment_status__messianic_deferral, suppression_requirement, 1000, 0.4).
narrative_ontology:measurement_basis(koda_su_t1000, observed).
narrative_ontology:measurement(koda_su_t1500, kodashim_commandment_status__messianic_deferral, suppression_requirement, 1500, 0.42).
narrative_ontology:measurement_basis(koda_su_t1500, observed).
narrative_ontology:measurement(koda_su_t2000, kodashim_commandment_status__messianic_deferral, suppression_requirement, 2000, 0.42).
narrative_ontology:measurement_basis(koda_su_t2000, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kodashim_commandment_status__messianic_deferral, identity_coordination).
narrative_ontology:boltzmann_floor_override(kodashim_commandment_status__messianic_deferral, 0.12).
narrative_ontology:affects_constraint(kodashim_commandment_status__messianic_deferral, kodashim_commandment_status__performance_only).
narrative_ontology:affects_constraint(kodashim_commandment_status__messianic_deferral, kodashim_commandment_status__study_as_performance).

% DUAL FORMULATION NOTE:
% The kernel 'kodashim_commandment_status' decomposes into three structurally distinct readings, each with its own ε, beneficiary structure, and type. The messianic-deferral reading (this file) treats the commandment as suspended but restorable, justifying present study as readiness maintenance—moderate extractiveness from opportunity cost. The performance-only reading treats suspension as voiding, eliminating present obligation—negligible extractiveness (mountain from most seats). The study-as-performance reading treats study as fulfillment, eliminating future contingency—lower extractiveness (closer to rope). Each reading has different founding-problem status: deferral's founding problem (Temple rebuild) is contested and recedes; performance-only's founding problem (sacrifice necessity) is treated as resolved (impossible); study-as-performance's founding problem dissolves into the present (study IS performance). The three readings are linked by network.affects_constraints to enable constraint-family analysis and cross-reading interference detection.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(kodashim_commandment_status__messianic_deferral, organized, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
