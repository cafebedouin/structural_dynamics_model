% ============================================================================
% CONSTRAINT STORY: animal_moral_status__welfare_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_animal_moral_status__welfare_reading, []).

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
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: animal_moral_status__welfare_reading
 *   human_readable: Animal Welfare Within Regulated Use (Reading)
 *   domain: applied_ethics/legal_philosophy
 *
 * SUMMARY:
 *   The welfare reading constructs animal moral status as sentience-based:
 *   animals can suffer, their suffering matters morally, and use should be
 *   regulated to minimize it. But use itself remains permissible — animals
 *   are not rights-bearers capable of refusing use, only interests-bearers
 *   whose interests are owed consideration within the framework of continued
 *   use. This reading sits between abolitionism (which denies use is
 *   permissible) and the property reading (which denies animals have
 *   interests). The constraint's operation is tangled: welfare organizations
 *   and regulatory bodies genuinely coordinate minimization of suffering
 *   (rope function); simultaneously, they extract legitimacy from industries
 *   and consumer publics by assuring them that use can be ethical if
 *   regulated (snare function). Animals bear the cost of both — they
 *   experience suffering constrained but not eliminated, and they carry no
 *   voice in whether use is permissible, only in how it is conducted. The
 *   measurement series shows extractiveness rising early (as regulation
 *   codifies) then plateauing, theater rising throughout (as the protective
 *   narrative becomes more sophisticated), and suppression holding steady
 *   (abolitionist dissent is managed by the welfare-frame's institutional
 *   dominance).
 *
 * KEY AGENTS:
 *   - Animal welfare organizations: set and enforce standards; extract legitimacy from industries; institutional beneficiaries of the welfare frame
 *   - Regulated industries: pay compliance costs; gain legal permission and public legitimacy; constrained beneficiaries
 *   - Animals: powerless, trapped; payers of suffering constrained but not ended; voiceless in permissibility questions
 *   - Consumer publics: gain moral permission to continue use; organized beneficiaries via the 'humane' assurance
 *   - Abolitionist movements: excluded from the constraint's legitimacy structure; would argue the frame itself is the violation
 *   - Regulatory bodies: codify and enforce the welfare frame; institutional agenda-setters
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(animal_moral_status__welfare_reading, 0.58).
domain_priors:suppression_score(animal_moral_status__welfare_reading, 0.62).
domain_priors:theater_ratio(animal_moral_status__welfare_reading, 0.51).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(animal_moral_status__welfare_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(animal_moral_status__welfare_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(animal_moral_status__welfare_reading, theater_ratio, 0.51).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(animal_moral_status__welfare_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(animal_moral_status__welfare_reading, resistance, 0.67).

% --- Constraint claim ---
narrative_ontology:constraint_claim(animal_moral_status__welfare_reading, tangled_rope).
narrative_ontology:human_readable(animal_moral_status__welfare_reading, "Animal Welfare Within Regulated Use (Reading)").
narrative_ontology:topic_domain(animal_moral_status__welfare_reading, "applied_ethics/legal_philosophy").

domain_priors:requires_active_enforcement(animal_moral_status__welfare_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(animal_moral_status__welfare_reading, '3810baf2-5036-4864-894c-be015dc085ed').
narrative_ontology:cs_kernel_codification('3810baf2-5036-4864-894c-be015dc085ed', formalized).
narrative_ontology:cs_authority_grounding('3810baf2-5036-4864-894c-be015dc085ed', lineage).
narrative_ontology:cs_interpretation_layer_present('3810baf2-5036-4864-894c-be015dc085ed').
narrative_ontology:cs_reading_relation('3810baf2-5036-4864-894c-be015dc085ed', animal_moral_status__property_reading, coexists_with).
narrative_ontology:cs_reading_relation('3810baf2-5036-4864-894c-be015dc085ed', animal_moral_status__abolitionist_reading, coexists_with).
narrative_ontology:cs_axiom('3810baf2-5036-4864-894c-be015dc085ed', foundational, animal_sentience_morally_relevant).
narrative_ontology:cs_axiom_status(animal_sentience_morally_relevant, holdable).
narrative_ontology:cs_axiom_grounding('3810baf2-5036-4864-894c-be015dc085ed', animal_sentience_morally_relevant, empirically_contingent).
narrative_ontology:cs_axiom('3810baf2-5036-4864-894c-be015dc085ed', foundational, use_permissible_if_regulated).
narrative_ontology:cs_axiom_status(use_permissible_if_regulated, holdable).
narrative_ontology:cs_axiom_grounding('3810baf2-5036-4864-894c-be015dc085ed', use_permissible_if_regulated, deontological).
narrative_ontology:cs_reference_frame('3810baf2-5036-4864-894c-be015dc085ed', sentience_based_welfare_regulation).
narrative_ontology:cs_drift_state('3810baf2-5036-4864-894c-be015dc085ed', contemporary_consumer_ethics_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('3810baf2-5036-4864-894c-be015dc085ed', '').
narrative_ontology:cs_kernel_id(animal_moral_status__welfare_reading, animal_moral_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(animal_moral_status__welfare_reading, animal_welfare_organizations).
narrative_ontology:constraint_beneficiary(animal_moral_status__welfare_reading, regulated_industries).
narrative_ontology:constraint_victim(animal_moral_status__welfare_reading, animals_in_regulated_systems).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(animal_moral_status__welfare_reading, consumer_publics).
narrative_ontology:constraint_beneficiary(animal_moral_status__welfare_reading, scientific_community).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set and enforce welfare standards for regulated animal use (farms, labs, entertainment). Their institutional existence and authority derive from the claim that animals require protection from cruelty within systems that treat them as resources. They collect legitimacy and funding by certifying 'humane' practices and conducting inspections. Their model assumes use is permissible; they define permissible as meeting welfare thresholds.
narrative_ontology:constraint_stakeholder(animal_moral_status__welfare_reading, animal_welfare_organizations, agenda_setter,
    organized, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(animal_moral_status__welfare_reading, animal_welfare_organizations, beneficiary).

% Gain public legitimacy and legal continuity by adopting welfare standards. The constraint permits their core practice (use of animals for food, research, entertainment) while subjecting it to incremental welfare requirements. They pay compliance costs but avoid the cost and legal risk of abolition. The constraint transforms use from rights-violation (abolitionist frame) into ethical practice (welfare frame).
narrative_ontology:constraint_stakeholder(animal_moral_status__welfare_reading, regulated_industries, beneficiary,
    powerful, biographical, constrained, global).

% Bear the costs of regulation designed to minimize their suffering while use itself continues. They experience pain, confinement, slaughter, and instrumental treatment. The constraint does not end their use but modifies its methods — faster slaughter, larger cages, fewer breeding cycles — in ways intended to reduce suffering while maintaining the practice. Their exit is unavailable: they cannot consent to use, negotiate terms, or refuse. The constraint's protections are real but bounded by the permissibility of use itself.
narrative_ontology:constraint_stakeholder(animal_moral_status__welfare_reading, animals_in_regulated_systems, payer,
    powerless, immediate, trapped, global).

% Obtain assurance that animals are treated 'humanely' — a frame that permits continued consumption while reducing moral discomfort. The welfare label solves the coordination problem of reconciling resource use with concern for suffering. They benefit from the narrative that their use can be ethical if regulated.
narrative_ontology:constraint_stakeholder(animal_moral_status__welfare_reading, consumer_publics, beneficiary,
    organized, biographical, mobile, national).

% Are outside the constraint's authorization structure. They argue that welfare regulation legitimizes a system that should be dismantled, and that incremental reforms distract from fundamental property-status questions. They would object that the constraint defines permissible use without addressing whether use itself is permissible.
narrative_ontology:constraint_stakeholder(animal_moral_status__welfare_reading, abolitionist_movements, excluded,
    moderate, generational, mobile, national).

% Codify and enforce welfare standards in law. They operate within the welfare frame: animals are interests-bearers whose suffering should be minimized, but use is lawful. Their authority rests on managing the boundary between permissible and impermissible treatment.
narrative_ontology:constraint_stakeholder(animal_moral_status__welfare_reading, regulatory_bodies, agenda_setter,
    institutional, generational, analytical, national).

% Gains institutional legitimacy and legal permission for animal research through the welfare frame. Research on animals is framed as ethically permissible if welfare protocols are followed. The constraint permits the research while imposing procedural oversight of pain and distress.
narrative_ontology:constraint_stakeholder(animal_moral_status__welfare_reading, scientific_community, beneficiary,
    institutional, generational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(animal_moral_status__welfare_reading, animal_welfare_organizations).
narrative_ontology:fixing_cost_class(animal_moral_status__welfare_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the coordination problem of allowing animal use while managing the fact that animals are sentient and can suffer. Creates a shared framework in which concern for animal suffering is recognized, use is permitted, and methods are regulated to minimize unnecessary suffering.
% TRANSFER_FUNCTION: Transfers regulatory authority over animal suffering from individual actors (farmers, researchers, consumers) to centralized welfare organizations and state bodies that set and enforce welfare standards. Animals transfer (or more accurately, have transferred on their behalf) the possibility of legal voice about their treatment, bounded by the frame that use itself remains permissible.
% ABSENT_VOICES: Abolitionist movements and rights-based frameworks are excluded from the constraint's legitimacy structure. Animals themselves cannot voice preferences; their interests are represented through the welfare frame by others (welfare organizations, regulatory bodies), but that representation does not include input on whether use is permissible — only on how permissible use should be conducted.
% DISAPPEARANCE_RATIONALE: If this constraint and its enforcement disappeared, industries would face immediate legal and market liability for animal suffering. Consumer publics would lose the assurance that consumption is ethically defensible. Abolitionist pressure would intensify without the welfare-legitimacy buffer. The regulatory apparatus would collapse and use would become either legal-but-delegitimized (accelerating pressure toward abolition) or constrained by property-damage law alone (insufficient for suffering concerns). The constraint's disappearance would restructure the moral and legal terrain.
% FOUNDING_PROBLEM: Early industrial animal use (intensive farming, unregulated research, unrestrained entertainment) produced visible suffering that conflicted with emerging sentience recognition and concern for animal welfare. The founding problem was: how can industrial animal use continue while addressing the fact that animals suffer?
% FOUNDING_PROBLEM_CORROBORATION: The welfare-organization and regulatory constituencies attest the problem is live: ongoing research on animal sentience and suffering, documented welfare failures, and consumer demand for 'humane' products all confirm that suffering remains and regulation is needed. Abolitionist movements and independent philosophers attest that the problem was misconceived: the founding problem should have been 'is use itself permissible?' rather than 'how should use be regulated?' They argue that welfare constraints do not solve the problem but obscure it by treating permissibility as settled.
narrative_ontology:disappearance_verdict(animal_moral_status__welfare_reading, world_rearranges).
narrative_ontology:founding_problem_status(animal_moral_status__welfare_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(animal_moral_status__welfare_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(animal_moral_status__welfare_reading, 'none', 1).
narrative_ontology:epsilon_provenance(animal_moral_status__welfare_reading, 0.58, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(animal_moral_status__welfare_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(animal_moral_status__welfare_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(animal_moral_status__welfare_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.58) because the constraint genuinely reduces suffering while enabling continued use. From the animals' perspective, the extraction is substantial (they bear suffering and powerlessness). From the industries' and public's perspectives, the extraction is minimal (they gain permission and legitimacy). Suppression is higher (0.62) because the constraint's persistence depends on excluding abolitionist framings and on preventing animals from expressing refusal. Theater is high and rising (0.51 at interval end) because welfare certification has become increasingly sophisticated as a cover narrative — more detailed protocols, more transparent auditing, more corporate welfare marketing — while the fundamental question (is use itself permissible?) remains outside the frame. The measurement trajectory shows a constraint reaching its equilibrium: extractiveness plateaus because industries have absorbed compliance, theater plateaus because the narrative is fully developed, suppression plateaus because abolitionist pressure is managed. The constraint is neither collapsing nor intensifying; it is consolidating.
 *
 * PERSPECTIVAL GAP:
 *   From the welfare-organization seat, this is a genuine rope: they solve the coordination problem of reconciling use with concern for suffering. From the animal's seat (mediated through abolitionist perspectives), this is a snare: the frame itself legitimizes victimization by accepting use as non-negotiable. From the industry's seat, this is a rope: they gain legal protection and moral cover. From the abolitionist seat, the constraint is a false summit disguised as natural fact — the permissibility of use is treated as settled when it is the core contested question. The engine computes per-seat types from the structural data (power, exit, beneficiary/victim); the divergence is the point of measurement.
 *
 * DIRECTIONALITY LOGIC:
 *   Welfare organizations and regulatory bodies are the structural beneficiaries (d near 0.1–0.2): they extract institutional authority, funding, and legitimacy without bearing the constraint's core cost. Industries are hybrid (d near 0.3–0.4): they pay compliance but gain permission and public legitimacy. Consumer publics are nearly symmetric (d near 0.5): they gain moral permission but pay a compliance premium. Animals are the targets (d near 0.9): they bear suffering, powerlessness, and voicelessness while the constraint is presented as protecting them. The constraint is extractive toward powerless animals and toward organized industries (though industries' extraction is softened by the legitimacy gain). The welfare frame suppresses abolitionist alternatives by rendering them logically outside the problem space: if use is taken as permissible, abolitionism is a rejection of the constraint's premise, not a competing answer within it.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (how to regulate use ethically) remains live, but the abolitionist counter-question (whether use is permissible at all) is structurally excluded from the constraint's frame. There is a risk of mandatrophy: if the founding problem is reframed as the permissibility question, the constraint's justification collapses. However, the constraint is not yet moribund — welfare improvements continue to be implemented, new species are brought under protection, consumer concern for welfare remains high. Mandatrophy is potential, not actual. The theater ratio rising toward 0.5 signals increasing performative maintenance relative to real suffering reduction, which is a mandatrophy warning sign but not yet a verdict.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    permissibility_boundary_ambiguity,
    'Is the welfare frame''s distinction between permissible and impermissible use structurally stable, or does it collapse under scrutiny into a binary: either animals have rights (abolitionist frame) or they do not (property frame)?',
    'Examine cases where welfare thresholds are tightened to near-abolition (e.g., regulations so strict that use becomes economically or technologically infeasible). If industries accept conversion to non-use, the boundary is stable; if they reject the regulations as covert abolition, the boundary is rhetorical.',
    'If the boundary is unstable, the constraint is a temporary equilibrium vulnerable to being read as either snare (industries) or false summit (abolitionists) — mandatrophy timing becomes critical. If stable, the constraint represents a genuine middle position with structural staying power.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(permissibility_boundary_ambiguity, conceptual, 'Whether welfare regulation can remain stable between use and non-use or collapses into one or the other.').

omega_variable(
    animal_voicelessness_structural_vs_contingent,
    'Is animals'' lack of voice in permissibility decisions a structural feature of the welfare frame (by definition, animals cannot consent, so only humans can adjudicate), or a contingent suppression mechanism (animals could be represented through proxy advocates empowered to refuse use on their behalf)?',
    'Test empirically: legal systems that grant standing to independent animal advocates (not industries, not welfare organizations with dual incentives) and that advocate for non-use when animals show resistance. If such advocates persistently call for abolition, animal interests expressed through proxies diverge from welfare constraints — the voicelessness is constructed, not necessary.',
    'If structural, the constraint''s exclusion of animal voice is unavoidable and the extraction to animals is inherent to any use-permissible frame. If contingent, the suppression mechanism could be reformed (empowered proxies) without abandoning the welfare frame, changing the constraint''s type from tangled_rope toward rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(animal_voicelessness_structural_vs_contingent, empirical, 'Whether animal voicelessness is necessary to the welfare frame or a suppressible contingency.').

omega_variable(
    suffering_reduction_actually_achieved,
    'Do welfare regulations actually reduce the amount or intensity of animal suffering in practice, or do they primarily reduce visible suffering (documentation, transparency) while leaving aggregate suffering roughly constant or increasing with scale?',
    'Longitudinal studies comparing pain/distress biomarkers (stress hormones, behavioral indicators, mortality correlates) across time and across welfare-certified vs. unregulated facilities. If welfare certification correlates with reduced biomarker suffering, regulations work as intended. If it correlates with unchanged or increased biomarkers, the constraint is pure theater.',
    'If regulations genuinely reduce suffering, extractiveness is lower than authored (the constraint is a workable rope with tangled elements). If suffering is unchanged, extractiveness is higher and theater ratio understates the performative fraction — the constraint becomes snare-dominant, and mandatrophy timing becomes critical.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suffering_reduction_actually_achieved, empirical, 'Whether welfare regulations reduce actual animal suffering or primarily manage its visibility.').

omega_variable(
    reading_foreclosure_property_vs_welfare,
    'Does the welfare reading foreclose the property reading, or do they coexist as competing institutional frameworks applied in different jurisdictions and sectors?',
    'Examine regulatory systems: jurisdictions that hold both readings simultaneously (e.g., EU with both property law and animal-welfare law that contradict in principle); jurisdictions that have shifted from property to welfare (direction of drift); whether the readings can coexist in one legal system or require replacement.',
    'If welfare forecloses property reading within a single system, the readings are in direct contest and one must be chosen. If they coexist across different domains or regions, the constraint is a localized reading, not a universal settlement of the kernel. This affects whether abolitionism is the *next* stage or a *parallel alternative*.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_foreclosure_property_vs_welfare, conceptual, 'Whether property and welfare readings are logically incompatible or institutionally separate.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(animal_moral_status__welfare_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anim_tr_t0, animal_moral_status__welfare_reading, theater_ratio, 0, 0.38).
narrative_ontology:measurement_basis(anim_tr_t0, observed).
narrative_ontology:measurement(anim_tr_t8, animal_moral_status__welfare_reading, theater_ratio, 8, 0.42).
narrative_ontology:measurement_basis(anim_tr_t8, observed).
narrative_ontology:measurement(anim_tr_t16, animal_moral_status__welfare_reading, theater_ratio, 16, 0.47).
narrative_ontology:measurement_basis(anim_tr_t16, observed).
narrative_ontology:measurement(anim_tr_t24, animal_moral_status__welfare_reading, theater_ratio, 24, 0.51).
narrative_ontology:measurement_basis(anim_tr_t24, observed).
narrative_ontology:measurement(anim_tr_t32, animal_moral_status__welfare_reading, theater_ratio, 32, 0.52).
narrative_ontology:measurement_basis(anim_tr_t32, observed).
narrative_ontology:measurement(anim_tr_t40, animal_moral_status__welfare_reading, theater_ratio, 40, 0.51).
narrative_ontology:measurement_basis(anim_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(anim_be_t0, animal_moral_status__welfare_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(anim_be_t0, observed).
narrative_ontology:measurement(anim_be_t8, animal_moral_status__welfare_reading, base_extractiveness, 8, 0.52).
narrative_ontology:measurement_basis(anim_be_t8, observed).
narrative_ontology:measurement(anim_be_t16, animal_moral_status__welfare_reading, base_extractiveness, 16, 0.56).
narrative_ontology:measurement_basis(anim_be_t16, observed).
narrative_ontology:measurement(anim_be_t24, animal_moral_status__welfare_reading, base_extractiveness, 24, 0.58).
narrative_ontology:measurement_basis(anim_be_t24, observed).
narrative_ontology:measurement(anim_be_t32, animal_moral_status__welfare_reading, base_extractiveness, 32, 0.59).
narrative_ontology:measurement_basis(anim_be_t32, observed).
narrative_ontology:measurement(anim_be_t40, animal_moral_status__welfare_reading, base_extractiveness, 40, 0.58).
narrative_ontology:measurement_basis(anim_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(anim_su_t0, animal_moral_status__welfare_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(anim_su_t0, observed).
narrative_ontology:measurement(anim_su_t8, animal_moral_status__welfare_reading, suppression_requirement, 8, 0.59).
narrative_ontology:measurement_basis(anim_su_t8, observed).
narrative_ontology:measurement(anim_su_t16, animal_moral_status__welfare_reading, suppression_requirement, 16, 0.62).
narrative_ontology:measurement_basis(anim_su_t16, observed).
narrative_ontology:measurement(anim_su_t24, animal_moral_status__welfare_reading, suppression_requirement, 24, 0.63).
narrative_ontology:measurement_basis(anim_su_t24, observed).
narrative_ontology:measurement(anim_su_t32, animal_moral_status__welfare_reading, suppression_requirement, 32, 0.62).
narrative_ontology:measurement_basis(anim_su_t32, observed).
narrative_ontology:measurement(anim_su_t40, animal_moral_status__welfare_reading, suppression_requirement, 40, 0.62).
narrative_ontology:measurement_basis(anim_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(animal_moral_status__welfare_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(animal_moral_status__welfare_reading, 0.18).
narrative_ontology:affects_constraint(animal_moral_status__welfare_reading, animal_moral_status__property_reading).
narrative_ontology:affects_constraint(animal_moral_status__welfare_reading, animal_moral_status__abolitionist_reading).

% DUAL FORMULATION NOTE:
% The animal_moral_status kernel decomposes into three distinct readings with different ε values and beneficiary/victim structures. The welfare_reading (this constraint) accepts sentience-based moral standing while preserving use permissibility (moderate extractiveness, ε ≈ 0.58). The property_reading treats animals as non-subjects with no standing (low extractiveness for animals, high for industries — property frame naturalizes use). The abolitionist_reading treats use itself as violation (high extractiveness for any use-system, ε ≈ 0.85+). These are not the same constraint measured differently; they are three structurally incompatible readings of the contested kernel. The welfare reading influences both siblings: it shifts the property reading toward grudging welfare concessions and delays abolitionist pressure by offering a middle ground. All three stories are linked via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(animal_moral_status__welfare_reading, powerless, 0.88).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
