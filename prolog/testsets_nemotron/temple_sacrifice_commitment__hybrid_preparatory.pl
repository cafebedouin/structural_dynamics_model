% ============================================================================
% CONSTRAINT STORY: temple_sacrifice_commitment__hybrid_preparatory
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-25
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_temple_sacrifice_commitment__hybrid_preparatory, []).

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
 *   constraint_id: temple_sacrifice_commitment__hybrid_preparatory
 *   human_readable: Hybrid Preparatory Study of Temple Sacrifice Law
 *   domain: religious/halakhic/commitment_system
 *
 * SUMMARY:
 *   The hybrid_preparatory reading of the temple sacrifice commitment holds
 *   that study of sacrificial law maintains the divine command in a suspended
 *   state — neither fully occupying the commitment (which would require a
 *   functioning Temple) nor merely archiving a defunct practice. Study is a
 *   preparatory exercise for messianic restoration, keeping the legal and
 *   technical knowledge alive so that when material conditions permit (Third
 *   Temple, priesthood restored, etc.), the practice can resume immediately.
 *   This reading dominates contemporary Orthodox yeshiva curricula and
 *   certain Religious Zionist institutions. It claims scaffold status: a
 *   temporary support structure justified by the transition to restoration,
 *   carrying a sunset clause (messianic arrival) that ends the need for
 *   preparatory study. The reading extracts moderate cognitive resources
 *   (0.48 ε) from scholars and funders for a future benefit whose arrival is
 *   uncertain and unfalsifiable.
 *
 * KEY AGENTS:
 *   - halakhic_institutions: agenda_setter (institutional/biographical/arbitrage) — sets curricula, allocates resources, authorizes the reading
 *   - sacrificial_study_specialists: beneficiary (organized/biographical/mobile) — gain prestige, career advancement, institutional positions from specialized knowledge
 *   - messianic_restoration_movements: beneficiary (organized/generational/identity_locked) — extract material and ideological resources from study communities
 *   - study_funders: payer (moderate/biographical/constrained) — donate to institutions running sacrificial study programs; exit constrained by communal expectations
 *   - dedicated_scholars: payer (moderate/biographical/identity_locked) — invest years in mastering non-performable law; exit constrained by identity fusion with the scholarly role
 *   - peripheral_community_members: payer (powerless/biographical/trapped) — bear indirect costs (communal funding priorities, educational bandwidth) with no voice in allocation
 *   - historical_critical_scholars: excluded (analytical/biographical/analytical) — would challenge the historical premises but are structurally excluded from halakhic discourse
 *   - secular_israeli_public: observer (institutional/generational/analytical) — bears state funding of religious institutions but has no standing in the internal debate
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(temple_sacrifice_commitment__hybrid_preparatory, 0.48).
domain_priors:suppression_score(temple_sacrifice_commitment__hybrid_preparatory, 0.22).
domain_priors:theater_ratio(temple_sacrifice_commitment__hybrid_preparatory, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(temple_sacrifice_commitment__hybrid_preparatory, extractiveness, 0.48).
narrative_ontology:constraint_metric(temple_sacrifice_commitment__hybrid_preparatory, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(temple_sacrifice_commitment__hybrid_preparatory, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(temple_sacrifice_commitment__hybrid_preparatory, accessibility_collapse, 0.31).
narrative_ontology:constraint_metric(temple_sacrifice_commitment__hybrid_preparatory, resistance, 0.28).

% --- Constraint claim ---
narrative_ontology:constraint_claim(temple_sacrifice_commitment__hybrid_preparatory, scaffold).
narrative_ontology:human_readable(temple_sacrifice_commitment__hybrid_preparatory, "Hybrid Preparatory Study of Temple Sacrifice Law").
narrative_ontology:topic_domain(temple_sacrifice_commitment__hybrid_preparatory, "religious/halakhic/commitment_system").

domain_priors:requires_active_enforcement(temple_sacrifice_commitment__hybrid_preparatory).
narrative_ontology:has_sunset_clause(temple_sacrifice_commitment__hybrid_preparatory).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(temple_sacrifice_commitment__hybrid_preparatory, '73e2ce09-7627-4baa-b3d5-c85f7dbe3bed').
narrative_ontology:cs_kernel_codification('73e2ce09-7627-4baa-b3d5-c85f7dbe3bed', fixed_text).
narrative_ontology:cs_authority_grounding('73e2ce09-7627-4baa-b3d5-c85f7dbe3bed', lineage).
narrative_ontology:cs_interpretation_layer_present('73e2ce09-7627-4baa-b3d5-c85f7dbe3bed').
narrative_ontology:cs_reading_relation('73e2ce09-7627-4baa-b3d5-c85f7dbe3bed', temple_sacrifice_commitment__study_as_exercise, coexists_with).
narrative_ontology:cs_reading_relation('73e2ce09-7627-4baa-b3d5-c85f7dbe3bed', temple_sacrifice_commitment__performance_only, forecloses).
narrative_ontology:cs_reading_relation('73e2ce09-7627-4baa-b3d5-c85f7dbe3bed', temple_sacrifice_commitment__symbolic_transformation, forecloses).
narrative_ontology:cs_axiom('73e2ce09-7627-4baa-b3d5-c85f7dbe3bed', foundational, sacrificial_command_requires_material_instantiation).
narrative_ontology:cs_axiom_status(sacrificial_command_requires_material_instantiation, holdable).
narrative_ontology:cs_axiom_grounding('73e2ce09-7627-4baa-b3d5-c85f7dbe3bed', sacrificial_command_requires_material_instantiation, deontological).
narrative_ontology:cs_axiom('73e2ce09-7627-4baa-b3d5-c85f7dbe3bed', foundational, study_maintains_command_in_suspended_state).
narrative_ontology:cs_axiom_status(study_maintains_command_in_suspended_state, holdable).
narrative_ontology:cs_axiom_grounding('73e2ce09-7627-4baa-b3d5-c85f7dbe3bed', study_maintains_command_in_suspended_state, deontological).
narrative_ontology:cs_axiom('73e2ce09-7627-4baa-b3d5-c85f7dbe3bed', secondary, messianic_restoration_will_require_full_technical_knowledge).
narrative_ontology:cs_axiom_status(messianic_restoration_will_require_full_technical_knowledge, holdable).
narrative_ontology:cs_axiom_grounding('73e2ce09-7627-4baa-b3d5-c85f7dbe3bed', messianic_restoration_will_require_full_technical_knowledge, deontological).
narrative_ontology:cs_reference_frame('73e2ce09-7627-4baa-b3d5-c85f7dbe3bed', post_churban_rabbinic_framework).
narrative_ontology:cs_drift_state('73e2ce09-7627-4baa-b3d5-c85f7dbe3bed', contemporary_zionist_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('73e2ce09-7627-4baa-b3d5-c85f7dbe3bed', '').
narrative_ontology:cs_kernel_id(temple_sacrifice_commitment__hybrid_preparatory, temple_sacrifice_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(temple_sacrifice_commitment__hybrid_preparatory, halakhic_institutions).
narrative_ontology:constraint_beneficiary(temple_sacrifice_commitment__hybrid_preparatory, sacrificial_study_specialists).
narrative_ontology:constraint_beneficiary(temple_sacrifice_commitment__hybrid_preparatory, messianic_restoration_movements).
narrative_ontology:constraint_victim(temple_sacrifice_commitment__hybrid_preparatory, study_funders).
narrative_ontology:constraint_victim(temple_sacrifice_commitment__hybrid_preparatory, dedicated_scholars).
narrative_ontology:constraint_victim(temple_sacrifice_commitment__hybrid_preparatory, peripheral_community_members).
narrative_ontology:constraint_vindicates(temple_sacrifice_commitment__hybrid_preparatory, divine_command_perpetuity).
narrative_ontology:constraint_vindicates(temple_sacrifice_commitment__hybrid_preparatory, halakhic_continuity_through_study).
narrative_ontology:constraint_vindicates(temple_sacrifice_commitment__hybrid_preparatory, messianic_restoration_certainty).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set curricula for yeshivas and kollels, allocating significant study time to sacrificial law (Kodashim tractates, Temple architecture, priestly genealogy). Control ordination standards requiring mastery of non-performable law. Receive state funding and private donations tied to maintaining 'complete Torah' curricula. Could shift resources to performable law but treat sacrificial study as non-negotiable core.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__hybrid_preparatory, halakhic_institutions, agenda_setter,
    institutional, generational, arbitrage, global).

% Scholars who specialize in sacrificial law (korbanot, Temple service, ritual purity) gain prestigious teaching positions, publishing opportunities, and recognition as guardians of 'the complete Torah.' Their expertise is non-falsifiable (no Temple to test against) but institutionally rewarded. Exit is mobile — they could study other areas — but the identity investment creates soft lock-in.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__hybrid_preparatory, sacrificial_study_specialists, beneficiary,
    organized, biographical, mobile, global).

% Groups (Temple Institute, Temple Mount movements, certain Religious Zionist factions) that actively prepare for Third Temple service. They fundraise on the basis of 'readiness,' train kohanim, manufacture vessels, and lobby politically. The hybrid_preparatory reading legitimizes their work: study communities provide the knowledge base; movements provide the activist energy. Identity is fused to restoration — exit would dissolve the self-concept.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__hybrid_preparatory, messianic_restoration_movements, beneficiary,
    organized, generational, identity_locked, global).

% Wealthy donors and communal boards who fund yeshivas running intensive sacrificial study programs. They believe they are preserving Torah completeness and hastening redemption. Exit is constrained: redirecting funds to 'practical' halakha (Shabbat, kashrut, family purity) risks communal censure and accusations of abandoning the 'complete Torah.' Their donations are the material extraction flow.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__hybrid_preparatory, study_funders, payer,
    moderate, biographical, constrained, global).

% Full-time learners (kollel avreichim, advanced yeshiva students) who spend years mastering Kodashim, Taharot, Middot — tractates with zero practical application today. Their scholarly identity is constituted through 'completing' the entire Talmud including non-performable sections. Leaving this track means abandoning the 'gadol' trajectory and the self-concept of a complete Torah scholar. Exit exists but is existentially costly.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__hybrid_preparatory, dedicated_scholars, payer,
    moderate, biographical, identity_locked, global).

% Community members whose communal budgets prioritize sacrificial study over social services, whose children's school curricula include non-performable law at the expense of practical skills, who have no voice in curriculum decisions. They bear the opportunity costs of the constraint with no structural power to influence it. Exit (leaving the community) is trapped — social, familial, economic costs are prohibitive.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__hybrid_preparatory, peripheral_community_members, payer,
    powerless, biographical, trapped, local).

% Academic scholars of ancient Judaism who argue sacrificial law was always tied to specific material conditions (Temple, priesthood, sovereignty) and cannot be 'maintained' in abstraction. They would challenge the historical premises of the hybrid_preparatory reading but are structurally excluded from halakhic authority structures. Their exclusion is not accidental — the reading's coherence depends on bracketing historical criticism.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__hybrid_preparatory, historical_critical_scholars, excluded,
    analytical, biographical, analytical, global).

% Israeli taxpayers who fund state-religious education systems where sacrificial study receives disproportionate resources. They have no standing in the halakhic debate but bear the fiscal externalities. Their 'exit' is political (voting, advocacy) but the constraint's internal logic is opaque to external democratic accountability.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__hybrid_preparatory, secular_israeli_public, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(temple_sacrifice_commitment__hybrid_preparatory, halakhic_institutions).
narrative_ontology:fixing_cost_class(temple_sacrifice_commitment__hybrid_preparatory, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves the complete technical and legal knowledge of the Temple sacrificial system so that when messianic conditions obtain (Third Temple, restored priesthood, renewed sovereignty), the practice can resume immediately without knowledge loss. Solves the coordination problem of intergenerational transmission of a complex, non-practiced legal system.
% TRANSFER_FUNCTION: Moves cognitive labor (scholar years), communal funding (donations, state budgets), and educational bandwidth (curriculum slots) from funders and scholars to institutions, specialists, and restoration movements — as the price of maintaining 'readiness' for a future restoration whose timing and conditions are theologically determined.
% ABSENT_VOICES: Historical-critical scholars who would argue the sacrificial system was historically contingent and cannot be abstracted from its material conditions. Reform and Conservative halakhic authorities who argue the command is either fulfilled in prayer (symbolic_transformation) or no longer binding. Peripheral community members who bear costs but have no deliberative voice. Secular Israeli public who fund the system fiscally but are excluded from the halakhic conversation.
% DISAPPEARANCE_RATIONALE: If the hybrid_preparatory constraint vanished overnight, yeshiva curricula would shift dramatically toward performable law (Shabbat, kashrut, civil law, family purity). Thousands of scholar-years annually would be redirected. Temple Institute and restoration movements would lose their knowledge base and recruitment pipeline. Communal budgets would face pressure to reallocate. The halakhic world would reorganize around 'Torah that can be kept today' — a substantial rearrangement.
% FOUNDING_PROBLEM: After the Temple's destruction (70 CE), the rabbis faced a crisis: the divine command to offer sacrifices could not be performed, yet the Torah presents it as perpetual. The founding problem was how to maintain the command's vitality without its material conditions — preventing the sacrificial system from becoming a dead letter while awaiting restoration.
% FOUNDING_PROBLEM_CORROBORATION: The hybrid_preparatory reading's own proponents (leading roshei yeshiva, Temple Institute leadership) attest the problem is live — restoration is imminent and preparation is urgent. Critical scholars (academic Talmudists, historians of ancient Judaism) attest the problem is dead — the historical conditions for sacrificial cult are irreversibly gone, and 'maintenance' is a category error. Religious Zionist thinkers are split: some see the State as partial restoration changing the problem's terms; others maintain the Temple-specific problem remains live. No consensus exists outside the benefiting parties.
narrative_ontology:disappearance_verdict(temple_sacrifice_commitment__hybrid_preparatory, world_rearranges).
narrative_ontology:founding_problem_status(temple_sacrifice_commitment__hybrid_preparatory, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(temple_sacrifice_commitment__hybrid_preparatory, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(temple_sacrifice_commitment__hybrid_preparatory, 'none', 1).
narrative_ontology:epsilon_provenance(temple_sacrifice_commitment__hybrid_preparatory, 0.48, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(temple_sacrifice_commitment__hybrid_preparatory_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(temple_sacrifice_commitment__hybrid_preparatory, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(temple_sacrifice_commitment__hybrid_preparatory_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48) reflects substantial cognitive resource investment in laws that cannot be performed — years of scholar time, institutional budgets, communal educational bandwidth directed at a future whose conditions are theologically controlled and empirically unverifiable. Suppression (0.22) is low in formal terms (no one is forced to study) but the omega variable questions whether theological internalization creates effective suppression. Theater ratio (0.35) is moderate and rising: the preparatory framing increasingly serves to justify institutional maintenance rather than genuine transition preparation. Accessibility collapse (0.31) is low — alternative frames (study_as_exercise, symbolic_transformation) remain live and accessible. Resistance (0.28) is modest but growing from critical scholars and resource-constrained communities. The scaffold claim rests entirely on the authenticity of the messianic sunset clause, which the sunset_clause_authenticity omega challenges.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda_setter seat (halakhic institutions), this is genuine coordination: preserving knowledge for a future transition they believe is certain. From the payer seats (funders, scholars), the same structure extracts resources for an uncertain benefit with no accountability mechanism — the sunset clause cannot be triggered by human action. From the excluded seat (critical scholars), the constraint is a self-justifying institutional arrangement that prevents reallocation of scarce cognitive resources. The beneficiary seats (specialists, restoration movements) experience genuine coordination benefit (career, meaning, institutional stability) but also extract from the payer seats. The engine will compute these divergences from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Halakhic institutions (agenda_setter) sit near the beneficiary end (d ~0.15): they control the curriculum, collect the prestige and funding, and face arbitrage-grade exit (could shift curricula but choose not to). Sacrificial study specialists (beneficiary) sit at strong beneficiary (d ~0.1): they gain specialized career capital from the arrangement. Messianic restoration movements (beneficiary) sit at moderate beneficiary (d ~0.25): they extract resources but their identity is fused to the restoration narrative (identity_locked exit). Study funders (payer) sit at moderate target (d ~0.65): they pay voluntarily but exit is constrained by communal expectations. Dedicated scholars (payer) sit at strong target (d ~0.8): identity_locked exit makes departure existentially costly. Peripheral community members (payer) sit at strong target (d ~0.75): trapped by structural powerlessness. Historical critical scholars (excluded) and secular Israeli public (observer) sit at analytical (d ~0.5).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preserving sacrificial law for restoration) was live in 70 CE and arguably through the medieval period when restoration seemed historically plausible. In the modern era, with Zionism creating a partial 'restoration' without Temple service, the founding problem's status is contested: Religious Zionists argue the problem is live (restoration imminent), Haredim often treat it as live but deferred, while critical voices argue it is dead (historical conditions irreversibly changed). The hybrid_preparatory reading prevents mandatrophy misclassification by explicitly naming the transition (messianic restoration) and carrying a sunset clause — but the sunset_clause_authenticity omega questions whether this is genuine or performative. If the sunset is authentic, the scaffold classification correctly identifies a transitional coordination structure. If the sunset is permanent deferral, the constraint is a piton (theatrical maintenance of atrophied function) or tangled_rope (extraction disguised as transition). The extraction_beneficiary_mapping omega targets the core mandatrophy question: does study coordination serve the transition, or has it become a self-justifying extraction mechanism?
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is the hybrid_preparatory reading structurally distinct from study_as_exercise and performance_only, or does it occupy an unstable middle ground that collapses into one sibling under pressure?',
    'Track institutional resource allocation: if hybrid_preparatory institutions shift funding toward either pure study-as-performance or active restoration preparation, the reading has collapsed. Monitor doctrinal statements for explicit foreclosure of sibling premises.',
    'If the reading collapses into study_as_exercise, extractiveness drops (coordination dominates); if it collapses into performance_only, extractiveness rises sharply (active restoration demands). The middle position''s stability determines whether the scaffold classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Structural distinctness of the hybrid reading from its siblings').

omega_variable(
    sunset_clause_authenticity,
    'Is the messianic restoration sunset clause a genuine transitional mechanism with credible termination conditions, or a permanent deferral device that prevents the constraint from ever being evaluated against reality?',
    'Examine whether any halakhic authority has specified concrete, falsifiable conditions for messianic restoration that would trigger the sunset. Track whether institutions treat the sunset as an operational planning horizon or as theological rhetoric.',
    'If the sunset is authentic, scaffold classification holds. If it is a permanent deferral, the constraint reclassifies toward piton (theatrical maintenance of a defunct function) or tangled_rope (extraction without genuine transition).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sunset_clause_authenticity, conceptual, 'Whether the messianic sunset clause functions as a real transition mechanism').

omega_variable(
    extraction_beneficiary_mapping,
    'Do the cognitive resources extracted through sacrificial study flow to identifiable beneficiaries (institutions, specialists, movements) or are they genuinely diffuse coordination costs?',
    'Trace funding streams: compare budgets for sacrificial study programs against general Torah study budgets. Measure career advancement and institutional prestige differentials for sacrificial law specialists versus generalists. Survey whether restoration movements extract material resources from study communities.',
    'If extraction concentrates on identifiable beneficiaries, tangled_rope classification strengthens. If genuinely diffuse, scaffold coordination function dominates. This is the core mandatrophy question for this reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_beneficiary_mapping, empirical, 'Whether study extraction has concentrated beneficiaries or is diffuse').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the low suppression metric (0.22) accurate, or does internalized theological framing suppress resistance that would otherwise appear?',
    'Interview scholars who left sacrificial study tracks: was departure experienced as free choice or as constraint violation? Measure community sanction for questioning the study mandate. Track whether dissenting voices are marginalized through social rather than formal mechanisms.',
    'If suppression is substantially internalized, the effective suppression is higher than measured, potentially shifting classification toward snare or tangled_rope. The low measured suppression may reflect successful internalization rather than genuine freedom.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression in theological commitment').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(temple_sacrifice_commitment__hybrid_preparatory, 0, 250).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tsc_hp_tr_t0, temple_sacrifice_commitment__hybrid_preparatory, theater_ratio, 0, 0.18).
narrative_ontology:measurement(tsc_hp_tr_t50, temple_sacrifice_commitment__hybrid_preparatory, theater_ratio, 50, 0.22).
narrative_ontology:measurement(tsc_hp_tr_t100, temple_sacrifice_commitment__hybrid_preparatory, theater_ratio, 100, 0.27).
narrative_ontology:measurement(tsc_hp_tr_t150, temple_sacrifice_commitment__hybrid_preparatory, theater_ratio, 150, 0.31).
narrative_ontology:measurement(tsc_hp_tr_t200, temple_sacrifice_commitment__hybrid_preparatory, theater_ratio, 200, 0.33).
narrative_ontology:measurement(tsc_hp_tr_t250, temple_sacrifice_commitment__hybrid_preparatory, theater_ratio, 250, 0.35).

% Extraction over time
narrative_ontology:measurement(tsc_hp_be_t0, temple_sacrifice_commitment__hybrid_preparatory, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(tsc_hp_be_t50, temple_sacrifice_commitment__hybrid_preparatory, base_extractiveness, 50, 0.41).
narrative_ontology:measurement(tsc_hp_be_t100, temple_sacrifice_commitment__hybrid_preparatory, base_extractiveness, 100, 0.43).
narrative_ontology:measurement(tsc_hp_be_t150, temple_sacrifice_commitment__hybrid_preparatory, base_extractiveness, 150, 0.45).
narrative_ontology:measurement(tsc_hp_be_t200, temple_sacrifice_commitment__hybrid_preparatory, base_extractiveness, 200, 0.47).
narrative_ontology:measurement(tsc_hp_be_t250, temple_sacrifice_commitment__hybrid_preparatory, base_extractiveness, 250, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(tsc_hp_su_t0, temple_sacrifice_commitment__hybrid_preparatory, suppression_requirement, 0, 0.12).
narrative_ontology:measurement(tsc_hp_su_t50, temple_sacrifice_commitment__hybrid_preparatory, suppression_requirement, 50, 0.15).
narrative_ontology:measurement(tsc_hp_su_t100, temple_sacrifice_commitment__hybrid_preparatory, suppression_requirement, 100, 0.18).
narrative_ontology:measurement(tsc_hp_su_t150, temple_sacrifice_commitment__hybrid_preparatory, suppression_requirement, 150, 0.2).
narrative_ontology:measurement(tsc_hp_su_t200, temple_sacrifice_commitment__hybrid_preparatory, suppression_requirement, 200, 0.21).
narrative_ontology:measurement(tsc_hp_su_t250, temple_sacrifice_commitment__hybrid_preparatory, suppression_requirement, 250, 0.22).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(temple_sacrifice_commitment__hybrid_preparatory, identity_coordination).
narrative_ontology:boltzmann_floor_override(temple_sacrifice_commitment__hybrid_preparatory, 0.08).
narrative_ontology:affects_constraint(temple_sacrifice_commitment__hybrid_preparatory, temple_sacrifice_commitment__study_as_exercise).
narrative_ontology:affects_constraint(temple_sacrifice_commitment__hybrid_preparatory, temple_sacrifice_commitment__performance_only).
narrative_ontology:affects_constraint(temple_sacrifice_commitment__hybrid_preparatory, temple_sacrifice_commitment__symbolic_transformation).
narrative_ontology:affects_constraint(temple_sacrifice_commitment__hybrid_preparatory, halakhic_curriculum_allocation).
narrative_ontology:affects_constraint(temple_sacrifice_commitment__hybrid_preparatory, messianic_restoration_infrastructure).

% DUAL FORMULATION NOTE:
% The temple_sacrifice_commitment kernel decomposes into four constraint stories, each a distinct reading with different ε values and beneficiary/victim structures. hybrid_preparatory has moderate ε (0.48) because it extracts for uncertain future benefit. study_as_exercise has low ε (coordination dominates, study IS the command). performance_only has near-zero ε for study (archival only) but high ε for active restoration preparation. symbolic_transformation has low ε (authorized transformation resolves the tension). All four linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(temple_sacrifice_commitment__hybrid_preparatory, moderate, 0.8).
constraint_indexing:directionality_override(temple_sacrifice_commitment__hybrid_preparatory, organized, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
