% ============================================================================
% CONSTRAINT STORY: latin_correctness__hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_latin_correctness__hybrid_reading, []).

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
 *   constraint_id: latin_correctness__hybrid_reading
 *   human_readable: Bifurcated Latin Correctness Standard (Hybrid Reading)
 *   domain: intellectual/philological
 *
 * SUMMARY:
 *   The hybrid reading asserts that Latin correctness is bifurcated:
 *   classical norms properly govern literary and rhetorical domains, while
 *   medieval forms retain legitimate autonomy in technical, practical, and
 *   specialized domains (medicine, law, engineering, liturgy). This reading
 *   mediates between the continuity reading (which elevates medieval forms as
 *   organic development) and the rupture reading (which treats classical
 *   Latin as a fixed standard requiring reconstruction). The hybrid reading
 *   permits functional specialization while establishing a status hierarchy
 *   that advantages classical literary production. Medieval technical writers
 *   face pressure to adopt classical forms for texts with institutional
 *   visibility, creating extraction through prestige deprivation and enforced
 *   reskilling. The constraint is CLAIMED as tangled_rope (genuine functional
 *   coordination in domain differentiation) while the measurements describe a
 *   constraint whose enforcement intensifies toward suppression of medieval
 *   forms, suggesting the coordination function is being gradually displaced
 *   by pure status extraction.
 *
 * KEY AGENTS:
 *   - classical_literary_establishment: Sets and enforces the bifurcated standard; agenda_setter/institutional
 *   - medieval_technical_writers: Produce practical texts in optimized medieval forms; face prestige judgment and reskilling pressure — victims
 *   - scriptoria_practitioners: Trapped monks and scribes managing texts across domains; experience the constraint as biased enforcement — victims
 *   - classical_philologists: Institutional beneficiaries whose expertise is validated by the hierarchy
 *   - ecclesiastical_authorities: Dual role as agenda-setters maintaining the standard and beneficiaries controlling institutional reputation
 *   - rival_linguistic_frameworks: Excluded proto-Romance and vernacular writers; constrained by Latin's prestige monopoly
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(latin_correctness__hybrid_reading, 0.62).
domain_priors:suppression_score(latin_correctness__hybrid_reading, 0.71).
domain_priors:theater_ratio(latin_correctness__hybrid_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(latin_correctness__hybrid_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(latin_correctness__hybrid_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(latin_correctness__hybrid_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(latin_correctness__hybrid_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(latin_correctness__hybrid_reading, resistance, 0.67).

% --- Constraint claim ---
narrative_ontology:constraint_claim(latin_correctness__hybrid_reading, tangled_rope).
narrative_ontology:human_readable(latin_correctness__hybrid_reading, "Bifurcated Latin Correctness Standard (Hybrid Reading)").
narrative_ontology:topic_domain(latin_correctness__hybrid_reading, "intellectual/philological").

domain_priors:requires_active_enforcement(latin_correctness__hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(latin_correctness__hybrid_reading, '63ba8ed6-80fd-4ffa-97ff-9aae4160f6ec').
narrative_ontology:cs_kernel_codification('63ba8ed6-80fd-4ffa-97ff-9aae4160f6ec', distributed).
narrative_ontology:cs_authority_grounding('63ba8ed6-80fd-4ffa-97ff-9aae4160f6ec', extraction).
narrative_ontology:cs_interpretation_layer_present('63ba8ed6-80fd-4ffa-97ff-9aae4160f6ec').
narrative_ontology:cs_reading_relation('63ba8ed6-80fd-4ffa-97ff-9aae4160f6ec', latin_correctness__continuity_reading, forecloses).
narrative_ontology:cs_reading_relation('63ba8ed6-80fd-4ffa-97ff-9aae4160f6ec', latin_correctness__rupture_reading, coexists_with).
narrative_ontology:cs_axiom('63ba8ed6-80fd-4ffa-97ff-9aae4160f6ec', foundational, classical_literary_prestige_hierarchy).
narrative_ontology:cs_axiom_status(classical_literary_prestige_hierarchy, holdable).
narrative_ontology:cs_axiom_grounding('63ba8ed6-80fd-4ffa-97ff-9aae4160f6ec', classical_literary_prestige_hierarchy, conventional).
narrative_ontology:cs_axiom('63ba8ed6-80fd-4ffa-97ff-9aae4160f6ec', secondary, domain_functional_specialization_permitted).
narrative_ontology:cs_axiom_status(domain_functional_specialization_permitted, holdable).
narrative_ontology:cs_axiom_grounding('63ba8ed6-80fd-4ffa-97ff-9aae4160f6ec', domain_functional_specialization_permitted, instrumental).
narrative_ontology:cs_reference_frame('63ba8ed6-80fd-4ffa-97ff-9aae4160f6ec', bifurcated_classical_technical_legitimacy).
narrative_ontology:cs_drift_state('63ba8ed6-80fd-4ffa-97ff-9aae4160f6ec', contemporary_institutional_enforcement, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('63ba8ed6-80fd-4ffa-97ff-9aae4160f6ec', '').
narrative_ontology:cs_kernel_id(latin_correctness__hybrid_reading, latin_correctness).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(latin_correctness__hybrid_reading, classical_literary_establishment).
narrative_ontology:constraint_victim(latin_correctness__hybrid_reading, medieval_technical_writers).
narrative_ontology:constraint_victim(latin_correctness__hybrid_reading, scriptoria_practitioners).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(latin_correctness__hybrid_reading, scriptoria_practitioners).
narrative_ontology:constraint_beneficiary(latin_correctness__hybrid_reading, classical_philologists).
narrative_ontology:constraint_beneficiary(latin_correctness__hybrid_reading, ecclesiastical_authorities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Curates and judges literary production in Latin; maintains the authority to declare what constitutes correct classical style; controls access to prestige venues and ecclesiastical positions that reward classical composition. Enforces the distinction between literary domains (where classical norms are mandatory) and technical domains (where medieval forms are permitted but ranked lower). Collects prestige, patronage, and institutional authority from maintaining this hierarchy.
narrative_ontology:constraint_stakeholder(latin_correctness__hybrid_reading, classical_literary_establishment, agenda_setter,
    institutional, generational, arbitrage, regional).

% Produce practical texts (medical, legal, engineering, liturgical) in medieval Latin forms optimized for clarity and technical precision. The bifurcated standard permits their language in technical domains but subjects it to continuous implicit judgment as inferior. They face pressure to adopt classical forms for any text with literary pretension or institutional visibility, forcing them to choose between domain-appropriate clarity and status recognition. Their exit is constrained: abandoning Latin entirely means losing institutional platforms and scholarly legitimacy.
narrative_ontology:constraint_stakeholder(latin_correctness__hybrid_reading, medieval_technical_writers, payer,
    moderate, biographical, constrained, regional).

% Monks and scribes who copy and compose texts across both domains; work under the bifurcated standard by producing medieval forms for technical manuscripts and attempting classical forms for literary works. They are trapped in the constraint by institutional affiliation and lack individual agency to negotiate the standard. They benefit from the availability of legitimate medieval forms in technical work (which they understand deeply) but pay in the form of constant reskilling pressure toward classical composition and judgment of their work as technically correct but culturally inferior.
narrative_ontology:constraint_stakeholder(latin_correctness__hybrid_reading, scriptoria_practitioners, payer,
    powerless, biographical, trapped, local).
narrative_ontology:stakeholder_secondary_role(latin_correctness__hybrid_reading, scriptoria_practitioners, beneficiary).

% Scholars reconstructing and teaching classical Latin from ancient sources; benefit from the bifurcated standard because it establishes classical forms as the pinnacle of Latin correctness, guaranteeing demand for their expertise and validating their research focus. The hierarchy ensures that technical and medieval texts remain secondary objects of study.
narrative_ontology:constraint_stakeholder(latin_correctness__hybrid_reading, classical_philologists, beneficiary,
    institutional, generational, arbitrage, regional).

% Use the bifurcated standard to manage institutional reputation: liturgical and doctrinal works can be produced in accessible medieval forms; texts addressing external authority (papal correspondence, doctrinal pronouncements) must employ classical forms to project timelessness and authority. The constraint allows them flexibility while controlling which texts carry institutional gravitas.
narrative_ontology:constraint_stakeholder(latin_correctness__hybrid_reading, ecclesiastical_authorities, agenda_setter,
    institutional, generational, arbitrage, regional).
narrative_ontology:stakeholder_secondary_role(latin_correctness__hybrid_reading, ecclesiastical_authorities, beneficiary).

% Proto-Romance vernacular writers and non-Latin literate communities; structurally barred from the Latin correctness conversation entirely. Their exclusion is maintained by the constraint that positions Latin itself as the legitimate field; medieval Latin technical clarity threatens to make those forms competitive with emerging vernaculars, so enforcement toward classical standards helps maintain Latin's prestige monopoly.
narrative_ontology:constraint_stakeholder(latin_correctness__hybrid_reading, rival_linguistic_frameworks, excluded,
    powerless, biographical, trapped, local).

% Examines how the bifurcated standard structures scholarly authority, creates status asymmetries between literary and technical domains, and enforces a single measure of correctness that is optimized for aesthetic and rhetorical purposes while being poorly suited to technical precision.
narrative_ontology:constraint_stakeholder(latin_correctness__hybrid_reading, analytical_observer, observer,
    analytical, civilizational, analytical, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(latin_correctness__hybrid_reading, classical_literary_establishment).
narrative_ontology:fixing_cost_class(latin_correctness__hybrid_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a shared standard for evaluating Latin composition that permits domain-specific variation (classical for literature, medieval forms for technical work) while preserving a unified measure of institutional prestige anchored in classical correctness. Enables coordinated judgment across diverse text types without requiring every author to master forms suited only to ancient rhetorical contexts.
% TRANSFER_FUNCTION: Moves authority and status toward classical specialists and literary establishments; technical writers pay in reduced prestige, judgment of inferiority, and pressure to reskill toward inappropriate forms. Ecclesiastical and philological institutions collect authority from maintaining the hierarchy.
% ABSENT_VOICES: Vernacular-language writers and proto-Romance speakers who would argue for recognizing emerging languages as legitimate alternatives; technical practitioners in non-Latin media; scribes and monks without access to advanced classical training who would advocate for medieval forms as primary rather than permitted.
% DISAPPEARANCE_RATIONALE: If the bifurcated standard collapsed, technical Latin would lose its legitimacy umbrella and either be naturalized as the standard form (elevating medieval clarity as primary correctness) or be abandoned in favor of emerging vernaculars. Either way, the prestige hierarchy that privileges classical forms would dissolve, and classical philology would lose its institutional guarantee of centrality.
% FOUNDING_PROBLEM: Classical Latin is the source text of institutional authority and literary tradition; medieval adaptations risk diluting that authority through linguistic change perceived as corruption. Yet practical texts (medical, legal, liturgical) require clarity optimized for their domains, not classical rhetorical effect. The bifurcated standard permits both by rank-ordering them.
% FOUNDING_PROBLEM_CORROBORATION: Classical literary establishments and ecclesiastical authorities affirm the founding problem: medieval forms do threaten to erode classical prestige and authority. Technical practitioners and linguistic historians attest that the problem is substantially solved by functional specialization, and that the bifurcated standard persists primarily to maintain status hierarchy rather than to resolve genuine coordination failure. Philological research shows medieval technical Latin is internally consistent and functionally optimized, not corrupt.
narrative_ontology:disappearance_verdict(latin_correctness__hybrid_reading, world_rearranges).
narrative_ontology:founding_problem_status(latin_correctness__hybrid_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(latin_correctness__hybrid_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(latin_correctness__hybrid_reading, 'none', 1).
narrative_ontology:epsilon_provenance(latin_correctness__hybrid_reading, 0.62, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(latin_correctness__hybrid_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(latin_correctness__hybrid_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(latin_correctness__hybrid_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-high (0.62) because the bifurcated standard legitimates medieval forms in technical domains (reducing pure extraction) while simultaneously enforcing classical as the prestige standard (creating extraction through status hierarchy). Suppression is high (0.71) because maintaining the hierarchy requires active enforcement: scrutiny of technical texts for medieval forms, pressure to reskill toward classical composition, judgment of medieval writers as technically competent but culturally inferior. Theater ratio rises to 0.48 and plateaus, indicating the constraint's functional domain-differentiation is increasingly theatrical — the enforcement is selective and the judgment criteria are applied asymmetrically. Measurement grid shares one timeline; all metrics are authored at each time point. The interval spans t=0 (projected founding of the hybrid standard) through t=24 (contemporary period of peak institutional enforcement transitioning to plateau).
 *
 * PERSPECTIVAL GAP:
 *   The classical literary establishment and ecclesiastical authorities experience the constraint as functional coordination: domain-appropriate standards that permit technical practitioners to work in medieval forms while preserving classical authority for prestige texts. Medieval technical writers and scriptoria practitioners experience the same structure as enforced status deprivation: they are permitted to work in their optimized forms only by accepting inferior institutional judgment, constant implicit pressure to reskill, and exclusion from prestige venues. The engine computes this divergence from the structural data: beneficiary seats (arbitrage exit, institutional power) derive low directionality toward the constraint; victim seats (constrained/trapped exit, moderate/powerless) derive high directionality. The measurement series show suppression intensifying as enforcement machinery hardened over the interval, suggesting the genuine coordination function (domain specialization) is being increasingly displaced by pure status maintenance.
 *
 * DIRECTIONALITY LOGIC:
 *   Classical establishment and ecclesiastical authorities are structural beneficiaries: they control the standard, collect prestige and authority, and maintain arbitrage exit (they can shift the standard or its enforcement without cost). Medieval technical writers are victims: they must work within the constraint (constrained exit) and experience reduced prestige, implicit judgment, and reskilling pressure. Scriptoria practitioners are trapped victims: their institutional affiliation binds them to the standard and they lack individual agency to negotiate it. Classical philologists are beneficiaries despite moderate power, because their expertise is guaranteed institutional centrality by the hierarchy. Ecclesiastical authorities hold dual roles (agenda_setter + beneficiary) because they both enforce the standard and use it to manage institutional reputation. The bifurcated structure creates asymmetric directionality: the same constraint appears as genuine coordination to beneficiaries and as enforced status extraction to victims.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — that medieval adaptations risk eroding classical authority while technical domains require specialized clarity — is CONTESTED rather than dead or live. Classical establishments affirm the problem; technical historians and functional linguists attest it is substantially solved by competent domain specialization. The bifurcated standard permits both, but enforcement pattern shows suppression intensifying (0.58 → 0.71), theater rising then plateauing (0.35 → 0.48), and extractiveness moderating (0.48 → 0.62 then plateau). This pattern indicates the constraint's functional coordination function (permitting domain specialization) is being gradually displaced by pure status enforcement. The theater plateau at 0.48 suggests the enforcement is no longer pretending to serve the founding problem; it has become theatrical maintenance of the hierarchy itself. A genuine mandatrophy is in progress: the constraint is being held together by pure status extraction now that the founding coordination problem is functionally solved.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    functional_vs_status_hierarchy,
    'Is the bifurcated standard primarily a functional coordination mechanism (different domains have different optimization targets) or primarily a status-enforcement mechanism (classical forms rank higher regardless of functional fit)?',
    'Examine patterns of enforcement discretion: if enforcement targets domain-misaligned classical composition (classical forms used in technical contexts when medieval would be more functional), the standard is status-driven; if enforcement respects domain boundaries and criticizes cross-domain misuse only, it is functionally coordinated.',
    'If status-driven, the constraint is snare-flavored extraction masked as coordination; the measured theater-rise and suppression-intensification support this reading. If functionally coordinated, the high extraction and suppression are costs of maintaining the hierarchy itself, not mask-wearing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(functional_vs_status_hierarchy, empirical, 'Whether bifurcation serves functional specialization or status enforcement.').

omega_variable(
    medieval_forms_legitimacy_boundary,
    'Is the legitimacy of medieval forms in technical domains a permanent permission or a transitional tolerance destined to be reclaimed by classical standards as prestige rises?',
    'Longitudinal analysis of institutional expectations over centuries: if medieval forms retain legitimate status and expectations stabilize, they are permanent; if classical penetration into technical domains gradually increases over time while medieval domains shrink, the permission is erosion-in-progress.',
    'If permanent, the constraint is a stable tangled_rope with genuine functional bifurcation and asymmetric prestige extraction. If transitional, the constraint is a snare whose victims face ultimate foreclosure — medieval technical domains lose legitimacy over time and are recolonized by classical standards.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(medieval_forms_legitimacy_boundary, empirical, 'Whether medieval technical legitimacy is stable or eroding.').

omega_variable(
    sibling_reading_interplay,
    'How do the three sibling readings of the latin_correctness kernel relate structurally? Does the hybrid reading foreclose the continuity reading, or do they coexist as live positions in different institutional contexts?',
    'Examine whether continuity scholars (medieval-forms-as-development advocates) can articulate their reading within institutional frameworks that also endorse the hybrid bifurcation, or whether the hybrid reading''s rank-ordering of classical prestige logically forecloses continuity''s egalitarian legitimacy claim.',
    'If coexistence, the kernel hosts multiple live readings with different extractiveness profiles (the continuity reading would show lower extraction because it doesn''t rank-order). If foreclosure, the hybrid reading''s classical prestige claim logically eliminates continuity''s premise that medieval forms are legitimate development rather than permitted variation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_interplay, conceptual, 'Structural relationships among the three sibling readings of the latin_correctness kernel.').

omega_variable(
    suppression_internalization_dynamics,
    'Is the measured suppression (0.71) primarily structural (institutional barriers, exclusion from prestige venues, enforcement pressure) or internalized (technical writers accept the inferiority judgment and self-enforce classical preference even when medieval is functionally superior)?',
    'Examine post-exit trajectories: if technical writers in contexts where classical prestige enforcement is absent still choose classical forms, suppression is internalized; if they revert to medieval forms when enforcement pressure is removed, it is structural.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests, and victims are complicit in their own deprivation. If structural, enforcement could be removed without changing constraints on technical practice; victims would not internalize the hierarchy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization_dynamics, empirical, 'Whether suppression is structural or internalized.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(latin_correctness__hybrid_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lati_tr_t0, latin_correctness__hybrid_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(lati_tr_t4, latin_correctness__hybrid_reading, theater_ratio, 4, 0.4).
narrative_ontology:measurement(lati_tr_t8, latin_correctness__hybrid_reading, theater_ratio, 8, 0.44).
narrative_ontology:measurement(lati_tr_t12, latin_correctness__hybrid_reading, theater_ratio, 12, 0.47).
narrative_ontology:measurement(lati_tr_t16, latin_correctness__hybrid_reading, theater_ratio, 16, 0.49).
narrative_ontology:measurement(lati_tr_t20, latin_correctness__hybrid_reading, theater_ratio, 20, 0.48).
narrative_ontology:measurement(lati_tr_t24, latin_correctness__hybrid_reading, theater_ratio, 24, 0.48).

% Extraction over time
narrative_ontology:measurement(lati_be_t0, latin_correctness__hybrid_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(lati_be_t4, latin_correctness__hybrid_reading, base_extractiveness, 4, 0.53).
narrative_ontology:measurement(lati_be_t8, latin_correctness__hybrid_reading, base_extractiveness, 8, 0.57).
narrative_ontology:measurement(lati_be_t12, latin_correctness__hybrid_reading, base_extractiveness, 12, 0.6).
narrative_ontology:measurement(lati_be_t16, latin_correctness__hybrid_reading, base_extractiveness, 16, 0.62).
narrative_ontology:measurement(lati_be_t20, latin_correctness__hybrid_reading, base_extractiveness, 20, 0.63).
narrative_ontology:measurement(lati_be_t24, latin_correctness__hybrid_reading, base_extractiveness, 24, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(lati_su_t0, latin_correctness__hybrid_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(lati_su_t4, latin_correctness__hybrid_reading, suppression_requirement, 4, 0.64).
narrative_ontology:measurement(lati_su_t8, latin_correctness__hybrid_reading, suppression_requirement, 8, 0.68).
narrative_ontology:measurement(lati_su_t12, latin_correctness__hybrid_reading, suppression_requirement, 12, 0.7).
narrative_ontology:measurement(lati_su_t16, latin_correctness__hybrid_reading, suppression_requirement, 16, 0.71).
narrative_ontology:measurement(lati_su_t20, latin_correctness__hybrid_reading, suppression_requirement, 20, 0.71).
narrative_ontology:measurement(lati_su_t24, latin_correctness__hybrid_reading, suppression_requirement, 24, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(latin_correctness__hybrid_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(latin_correctness__hybrid_reading, 0.12).
narrative_ontology:affects_constraint(latin_correctness__hybrid_reading, latin_correctness__continuity_reading).
narrative_ontology:affects_constraint(latin_correctness__hybrid_reading, latin_correctness__rupture_reading).

% DUAL FORMULATION NOTE:
% The latin_correctness kernel decomposes into three constraint stories representing three competing readings: the continuity_reading (medieval forms are legitimate organic development), the rupture_reading (classical Latin is a fixed standard requiring reconstruction), and the hybrid_reading (this constraint — classical for literature, medieval for technical). Each reading instantiates a different constraint with different ε values and victim/beneficiary structures. The hybrid reading structures the constraint around status hierarchy within bifurcated legitimacy; it affects both sibling readings by establishing the prestige rank-order that pressures continuity advocates and motivates rupture advocates. The three readings form a constraint family linked by network relationships.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(latin_correctness__hybrid_reading, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
