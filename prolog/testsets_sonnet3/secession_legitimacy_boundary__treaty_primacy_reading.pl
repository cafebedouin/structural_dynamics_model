% ============================================================================
% CONSTRAINT STORY: secession_legitimacy_boundary__treaty_primacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_secession_legitimacy_boundary__treaty_primacy_reading, []).

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
 *   constraint_id: secession_legitimacy_boundary__treaty_primacy_reading
 *   human_readable: Treaty Primacy Reading of Secession Legitimacy
 *   domain: political_economy/federalism/resource_politics
 *
 * SUMMARY:
 *   This story instantiates the treaty-primacy reading of the secession
 *   legitimacy kernel: the claim that Indigenous treaty rights predate and
 *   structurally supersede both federal and provincial authority, such that
 *   no secession is legitimate without treaty holder consent. This is one of
 *   four contested readings of the same underlying kernel (the legitimate
 *   conditions for provincial secession); the sibling readings —
 *   constitutional impossibility, popular sovereignty, and grievance
 *   threshold — are separate constraints with their own ε and stakeholder
 *   structures, not alternative measurements of this one. Under this reading,
 *   treaty nations gain a structurally load-bearing role in any secession
 *   process; where their consent is genuinely sought and honored the
 *   arrangement functions as coordination (treaty nations and the federal
 *   government both benefit from an orderly, negotiated process); where
 *   consent is invoked rhetorically but not substantively sought, or where
 *   secession proceeds despite treaty nations' objection, the same structure
 *   becomes extractive toward treaty nations and toward the secessionist
 *   movement whose mandate is discounted after the fact.
 *
 * KEY AGENTS:
 *   - treaty_nations: primary agenda-setter and primary bearer of risk if bypassed
 *   - federal_government: structural beneficiary of the reading's institutional-continuity effect
 *   - provincial_secessionist_movement: primary payer of the added legitimacy precondition
 *   - provincial_government: dual beneficiary/payer depending on which dispute is at issue
 *   - resource_industry_actors: excluded but materially exposed third parties
 *   - constitutional_courts: analytical observer who would ultimately adjudicate the reading's doctrinal status
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(secession_legitimacy_boundary__treaty_primacy_reading, 0.58).
domain_priors:suppression_score(secession_legitimacy_boundary__treaty_primacy_reading, 0.55).
domain_priors:theater_ratio(secession_legitimacy_boundary__treaty_primacy_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(secession_legitimacy_boundary__treaty_primacy_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__treaty_primacy_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__treaty_primacy_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(secession_legitimacy_boundary__treaty_primacy_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__treaty_primacy_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(secession_legitimacy_boundary__treaty_primacy_reading, tangled_rope).
narrative_ontology:human_readable(secession_legitimacy_boundary__treaty_primacy_reading, "Treaty Primacy Reading of Secession Legitimacy").
narrative_ontology:topic_domain(secession_legitimacy_boundary__treaty_primacy_reading, "political_economy/federalism/resource_politics").

domain_priors:requires_active_enforcement(secession_legitimacy_boundary__treaty_primacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(secession_legitimacy_boundary__treaty_primacy_reading, 'acf4073f-cb1b-493e-9350-ff54b3327ced').
narrative_ontology:cs_kernel_codification('acf4073f-cb1b-493e-9350-ff54b3327ced', distributed).
narrative_ontology:cs_authority_grounding('acf4073f-cb1b-493e-9350-ff54b3327ced', lineage).
narrative_ontology:cs_interpretation_layer_present('acf4073f-cb1b-493e-9350-ff54b3327ced').
narrative_ontology:cs_reading_relation('acf4073f-cb1b-493e-9350-ff54b3327ced', secession_legitimacy_boundary__constitutional_impossibility_reading, coexists_with).
narrative_ontology:cs_reading_relation('acf4073f-cb1b-493e-9350-ff54b3327ced', secession_legitimacy_boundary__popular_sovereignty_reading, influences).
narrative_ontology:cs_reading_relation('acf4073f-cb1b-493e-9350-ff54b3327ced', secession_legitimacy_boundary__grievance_threshold_reading, coexists_with).
narrative_ontology:cs_axiom('acf4073f-cb1b-493e-9350-ff54b3327ced', foundational, treaty_relationship_predates_federation).
narrative_ontology:cs_axiom_status(treaty_relationship_predates_federation, holdable).
narrative_ontology:cs_axiom_grounding('acf4073f-cb1b-493e-9350-ff54b3327ced', treaty_relationship_predates_federation, conventional).
narrative_ontology:cs_axiom('acf4073f-cb1b-493e-9350-ff54b3327ced', foundational, consent_of_treaty_holders_is_a_precondition_not_a_courtesy).
narrative_ontology:cs_axiom_status(consent_of_treaty_holders_is_a_precondition_not_a_courtesy, holdable).
narrative_ontology:cs_axiom_grounding('acf4073f-cb1b-493e-9350-ff54b3327ced', consent_of_treaty_holders_is_a_precondition_not_a_courtesy, deontological).
narrative_ontology:cs_reference_frame('acf4073f-cb1b-493e-9350-ff54b3327ced', nation_to_nation_treaty_relationship).
narrative_ontology:cs_drift_state('acf4073f-cb1b-493e-9350-ff54b3327ced', contemporary_secession_disputes, gap(revival_pressure, substantial, false)).
narrative_ontology:cs_created_at('acf4073f-cb1b-493e-9350-ff54b3327ced', '').
narrative_ontology:cs_kernel_id(secession_legitimacy_boundary__treaty_primacy_reading, secession_legitimacy_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(secession_legitimacy_boundary__treaty_primacy_reading, federal_government).
narrative_ontology:constraint_beneficiary(secession_legitimacy_boundary__treaty_primacy_reading, treaty_nations_when_consulted).
narrative_ontology:constraint_victim(secession_legitimacy_boundary__treaty_primacy_reading, treaty_nations_when_bypassed).
narrative_ontology:constraint_victim(secession_legitimacy_boundary__treaty_primacy_reading, provincial_secessionist_movement).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(secession_legitimacy_boundary__treaty_primacy_reading, provincial_government).
narrative_ontology:constraint_victim(secession_legitimacy_boundary__treaty_primacy_reading, treaty_nations).
narrative_ontology:constraint_victim(secession_legitimacy_boundary__treaty_primacy_reading, provincial_government).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold nation-to-nation treaty relationships negotiated before the province or in some cases before the federal state existed in its current form. Under this reading, they hold a veto-like consent requirement over any redrawing of the constitutional order that would alter who administers treaty obligations on their territory. In practice they set the legitimacy agenda by withholding or granting consent, but they bear the costs when either government proceeds without genuinely seeking that consent, and they cannot exit the underlying territorial relationship regardless of how the secession question resolves.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__treaty_primacy_reading, treaty_nations, agenda_setter,
    organized, civilizational, trapped, national).
narrative_ontology:stakeholder_secondary_role(secession_legitimacy_boundary__treaty_primacy_reading, treaty_nations, payer).

% Holds the constitutional duty to treaty nations (fiduciary obligation, honor of the Crown) and administers most treaty relationships directly. Under the treaty-primacy reading, federal authority is strengthened relative to a seceding province because federal treaty obligations cannot simply be transferred or extinguished by a provincial referendum. The federal government can invoke this reading strategically to slow or block secession, which benefits its own institutional continuity.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__treaty_primacy_reading, federal_government, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(secession_legitimacy_boundary__treaty_primacy_reading, federal_government, agenda_setter).

% Seeks to leave the federation on the strength of a provincial referendum. Under this reading, its legitimacy claim is categorically insufficient wherever the province's territory includes treaty lands — it must obtain treaty holder consent, which it did not seek to have a controlling role in its original mandate. This is experienced as an unanticipated, and to secessionists illegitimate, precondition imposed on their democratic mandate.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__treaty_primacy_reading, provincial_secessionist_movement, payer,
    organized, biographical, constrained, regional).

% Administers programs on treaty land under delegated or overlapping jurisdiction. If it pursues secession, it inherits the burden of negotiating with treaty nations as a precondition of legitimacy rather than as a matter of post-secession diplomacy of its own choosing. It benefits, however, from treaty primacy insofar as it can also invoke Indigenous consent requirements against federal overreach in ordinary (non-secession) resource disputes.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__treaty_primacy_reading, provincial_government, payer,
    institutional, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(secession_legitimacy_boundary__treaty_primacy_reading, provincial_government, beneficiary).

% Have major capital investments (pipelines, mines, resource leases) whose regulatory authority would be thrown into question by any secession event. They are not party to the treaty-primacy legitimacy question directly but have enormous stakes in its resolution; they are excluded from the constitutional conversation despite lobbying all sides.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__treaty_primacy_reading, resource_industry_actors, excluded,
    powerful, biographical, mobile, national).

% Would be asked to adjudicate whether treaty consent is a binding precondition of secession or merely a political consideration. Their ruling would decide whether this reading becomes settled doctrine or remains a contested position among several plausible framings.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__treaty_primacy_reading, constitutional_courts, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(secession_legitimacy_boundary__treaty_primacy_reading, diffuse).
narrative_ontology:fixing_cost_class(secession_legitimacy_boundary__treaty_primacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Ensures that any fundamental restructuring of the federation cannot be negotiated over the heads of nations whose treaty relationship predates and is independent of the federal-provincial division of powers — coordinating constitutional change with pre-existing treaty obligations rather than treating treaty lands as fungible provincial territory.
% TRANSFER_FUNCTION: Moves veto-adjacent leverage from the seceding province and, secondarily, from the federal government's unilateral discretion, to treaty nations; when honored, it moves genuine decision-making weight to treaty nations; when invoked strategically without follow-through, it moves delay and procedural cover to whichever government wants to slow the secession process, at the cost of treaty nations' credibility and time.
% ABSENT_VOICES: Treaty nations that are not the largest or most politically organized within a seceding province are frequently invoked in the abstract ('Indigenous consent required') without being individually consulted; smaller or more remote nations are especially likely to be spoken for rather than spoken with.
% DISAPPEARANCE_RATIONALE: If treaty primacy were not recognized as a constraint on secession, referenda and federal-provincial negotiations could proceed as though treaty lands were ordinary provincial territory; treaty nations' capacity to compel a seat at the constitutional table would collapse into ordinary lobbying, and resource and land arrangements currently understood as pre-constitutional would become negotiable objects of the secession settlement itself.
% FOUNDING_PROBLEM: Treaties were signed nation-to-nation, historically prior to and independent of the current federal-provincial division of powers; the founding problem this reading addresses is the risk that a federation could restructure or dissolve itself using internal constitutional machinery that treaty nations never consented to and were never party to establishing.
% FOUNDING_PROBLEM_CORROBORATION: Independent legal scholars specializing in Aboriginal and treaty law, several truth and reconciliation commission findings, and international human rights bodies (citing UNDRIP's free, prior and informed consent standard) corroborate that the underlying treaty relationships remain legally and diplomatically unresolved outside of the secession context — this is not solely asserted by treaty nations or by the federal government invoking it strategically.
narrative_ontology:disappearance_verdict(secession_legitimacy_boundary__treaty_primacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(secession_legitimacy_boundary__treaty_primacy_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(secession_legitimacy_boundary__treaty_primacy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(secession_legitimacy_boundary__treaty_primacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(secession_legitimacy_boundary__treaty_primacy_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(secession_legitimacy_boundary__treaty_primacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(secession_legitimacy_boundary__treaty_primacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(secession_legitimacy_boundary__treaty_primacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at a moderate-high 0.58 because the reading's coordination function (protecting treaty nations from unilateral constitutional restructuring) is real but is asymmetrically deployable: federal and provincial governments can invoke 'treaty consultation' as a procedural veto against each other or against secessionists without the consultation being substantive, extracting delay-value and legitimacy-cover from treaty nations' name without delivering genuine consent authority. Suppression (0.55) reflects that treaty nations' capacity to enforce this reading depends on courts, media attention, and political will they do not fully control — the constraint is real but not self-enforcing. Resistance (0.72) is high because both the secessionist movement (which experiences the precondition as an imposed veto on democratic mandate) and treaty nations themselves (who resist tokenistic invocation of their consent) actively contest how the reading is applied. Accessibility collapse is comparatively low (0.35): alternative readings of the secession question remain live and contested — this is precisely the kernel-reading structure, not a settled fact.
 *
 * PERSPECTIVAL GAP:
 *   From the treaty nations' own seat, this reading is coordination: it formalizes a pre-existing nation-to-nation relationship that the federal-provincial division of powers was never entitled to erase. From the secessionist movement's seat, the same structure looks like an externally imposed veto that discounts a democratic mandate obtained without their participation in its design. From the federal government's seat, it can look like either principled defense of Crown-Indigenous relations or a convenient doctrinal tool to resist provincial fragmentation, depending on the sincerity of enforcement — this is exactly the seat divergence the engine is built to compute from structural data rather than resolve by fiat.
 *
 * DIRECTIONALITY LOGIC:
 *   Treaty nations are declared as both agenda_setter and payer because the reading grants them structural leverage they did not design the enforcement mechanism for; when consultation is genuine they are near-beneficiaries, when bypassed they are targets bearing the cost of governments invoking their name without delivering substantive consent. The federal government is a structural beneficiary because the reading strengthens its institutional position relative to a seceding province, giving it a doctrinal argument for delay or refusal that serves its own continuity interest, independent of whether it is genuinely honoring treaty obligations. The provincial secessionist movement is the clearest payer: an additional legitimacy precondition it did not include in its own mandate design is imposed on it after the fact.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (treaty relationships predating and independent of the federal-provincial constitutional architecture) remains live by independent legal-scholarly and international-standard corroboration (UNDRIP's FPIC standard), which is why founding_problem_status is authored as 'live' rather than 'dead' or purely self-asserted. This blocks a mandatrophy misreading in either direction: the reading is not merely an inertial formality (the underlying treaty obligations are actively unresolved) nor is it purely extractive rent-seeking by treaty nations (they did not design the secession-legitimacy application of their consent right and bear real costs when it is invoked tokenistically).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    genuine_versus_strategic_invocation,
    'When governments cite treaty consent requirements to block or delay secession, is the invocation a genuine application of the treaty relationship''s legal weight, or a strategic use of Indigenous consent as procedural cover for institutional self-preservation?',
    'Track whether treaty nations that are consulted under this reading receive substantive negotiating authority and follow-through commitments, versus being cited in briefs and public statements without direct engagement; compare across multiple live or hypothetical secession disputes.',
    'If invocation is consistently strategic rather than substantive, the reading functions closer to snare (extracting legitimacy cover from treaty nations'' name without delivering consent authority); if consistently substantive, it functions closer to a genuine rope-like coordination mechanism protecting pre-existing rights.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(genuine_versus_strategic_invocation, empirical, 'Whether treaty consent invocation in secession disputes is substantive or strategic cover.').

omega_variable(
    kernel_framing_selection,
    'Is treaty primacy best modeled as a freestanding legitimacy condition (this story''s framing) or as an implication embedded within the constitutional_impossibility_reading (since treaty obligations are themselves constitutionally entrenched)?',
    'Examine whether courts and constitutional scholars treat treaty rights as a distinct veto point or as a sub-argument within the broader constitutional-amendment-required position; a ruling that folds treaty consent entirely into constitutional procedure would suggest the narrower framing understates the distinction, while a ruling treating treaty consent as independently binding even absent full constitutional amendment machinery would support the freestanding framing chosen here.',
    'If treaty primacy collapses into constitutional_impossibility_reading''s premise, the two readings should be understood as forecloses/subsumes rather than coexists_with, changing how the family of readings is modeled; the current framing treats it as an independent coexisting reading because treaty rights are argued to predate and be independent of the constitutional order itself, not merely a feature of it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_selection, conceptual, 'Whether treaty primacy is a freestanding reading or a sub-case of constitutional impossibility.').

omega_variable(
    treaty_nation_internal_diversity,
    'Does ''treaty holder consent'' refer to a single collective Indigenous position, or does it mask significant diversity among treaty nations who may disagree with one another about secession, resource development, or the terms of consultation?',
    'Survey actual positions taken by distinct treaty nations within a given secession-affected territory during any live dispute; document whether consensus exists or whether the reading''s invocation flattens genuine internal disagreement.',
    'If internal diversity is substantial and unacknowledged, the reading as applied may itself extract legitimacy from a manufactured single Indigenous voice, understating the excluded-voices problem for nations whose position differs from whichever position is politically convenient to cite.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(treaty_nation_internal_diversity, empirical, 'Whether treaty holder consent is treated as monolithic when it is actually plural and contested.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(secession_legitimacy_boundary__treaty_primacy_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sece_tr_t0, secession_legitimacy_boundary__treaty_primacy_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(sece_tr_t4, secession_legitimacy_boundary__treaty_primacy_reading, theater_ratio, 4, 0.26).
narrative_ontology:measurement(sece_tr_t8, secession_legitimacy_boundary__treaty_primacy_reading, theater_ratio, 8, 0.3).
narrative_ontology:measurement(sece_tr_t12, secession_legitimacy_boundary__treaty_primacy_reading, theater_ratio, 12, 0.33).
narrative_ontology:measurement(sece_tr_t16, secession_legitimacy_boundary__treaty_primacy_reading, theater_ratio, 16, 0.36).
narrative_ontology:measurement(sece_tr_t20, secession_legitimacy_boundary__treaty_primacy_reading, theater_ratio, 20, 0.38).
narrative_ontology:measurement(sece_tr_t24, secession_legitimacy_boundary__treaty_primacy_reading, theater_ratio, 24, 0.4).

% Extraction over time
narrative_ontology:measurement(sece_be_t0, secession_legitimacy_boundary__treaty_primacy_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(sece_be_t4, secession_legitimacy_boundary__treaty_primacy_reading, base_extractiveness, 4, 0.46).
narrative_ontology:measurement(sece_be_t8, secession_legitimacy_boundary__treaty_primacy_reading, base_extractiveness, 8, 0.5).
narrative_ontology:measurement(sece_be_t12, secession_legitimacy_boundary__treaty_primacy_reading, base_extractiveness, 12, 0.53).
narrative_ontology:measurement(sece_be_t16, secession_legitimacy_boundary__treaty_primacy_reading, base_extractiveness, 16, 0.55).
narrative_ontology:measurement(sece_be_t20, secession_legitimacy_boundary__treaty_primacy_reading, base_extractiveness, 20, 0.57).
narrative_ontology:measurement(sece_be_t24, secession_legitimacy_boundary__treaty_primacy_reading, base_extractiveness, 24, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(sece_su_t0, secession_legitimacy_boundary__treaty_primacy_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(sece_su_t4, secession_legitimacy_boundary__treaty_primacy_reading, suppression_requirement, 4, 0.44).
narrative_ontology:measurement(sece_su_t8, secession_legitimacy_boundary__treaty_primacy_reading, suppression_requirement, 8, 0.47).
narrative_ontology:measurement(sece_su_t12, secession_legitimacy_boundary__treaty_primacy_reading, suppression_requirement, 12, 0.5).
narrative_ontology:measurement(sece_su_t16, secession_legitimacy_boundary__treaty_primacy_reading, suppression_requirement, 16, 0.52).
narrative_ontology:measurement(sece_su_t20, secession_legitimacy_boundary__treaty_primacy_reading, suppression_requirement, 20, 0.54).
narrative_ontology:measurement(sece_su_t24, secession_legitimacy_boundary__treaty_primacy_reading, suppression_requirement, 24, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(secession_legitimacy_boundary__treaty_primacy_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(secession_legitimacy_boundary__treaty_primacy_reading, 0.12).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__treaty_primacy_reading, constitutional_impossibility_reading).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__treaty_primacy_reading, popular_sovereignty_reading).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__treaty_primacy_reading, grievance_threshold_reading).

% DUAL FORMULATION NOTE:
% This story is one of four members of the secession_legitimacy_boundary kernel family. Each reading is authored as a separate ε-invariant constraint with its own beneficiary/victim structure: constitutional_impossibility_reading treats unilateral secession as categorically impermissible absent constitutional amendment; popular_sovereignty_reading treats a provincial referendum as self-legitimating; grievance_threshold_reading treats a sufficient showing of structural injustice as legitimating secession outside constitutional text; this treaty_primacy_reading treats treaty holder consent as an independent precondition layered atop whichever of the other three frameworks is otherwise operative. The reading's structural delta from the family is the introduction of treaty nations as a load-bearing party absent from the other three readings' stakeholder sets.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
