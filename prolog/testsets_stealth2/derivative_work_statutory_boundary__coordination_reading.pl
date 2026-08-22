% ============================================================================
% CONSTRAINT STORY: derivative_work_statutory_boundary__coordination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_derivative_work_statutory_boundary__coordination_reading, []).

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
 *   constraint_id: derivative_work_statutory_boundary__coordination_reading
 *   human_readable: Derivative-Work Statutory Boundary — Coordination Reading (Fixed Expressive Recastings Only)
 *   domain: legal/intellectual_property/technology_governance
 *
 * SUMMARY:
 *   Under this reading, the copyright derivative-work right reaches only
 *   fixed recastings that substantially incorporate the original expression —
 *   translations, dramatizations, film adaptations, sequels — while
 *   transformative recastings, quotation, criticism, parody, and the
 *   intermediate copies made inside computational processing sit outside the
 *   exclusive right. No ex-ante license is required to transform, to quote,
 *   or to train; that permission structure is what the generative-technology
 *   sector and a large share of transformative creation currently run on.
 *   This file instantiates ONE reading of the
 *   derivative_work_statutory_boundary kernel; the enclosure and hybrid
 *   carveout readings are separate constraints (linked in
 *   network.affects_constraints) with their own ε values, and the contest
 *   among readings is carried in the omega variables rather than averaged
 *   into this story's metrics. ε's referent is the standing arrangement — the
 *   boundary as this reading holds it — assessed by this reading's own
 *   lights. Measurement interval 0–30 ≈ 1994–2024, from the settled
 *   intermediate-copying line through the generative-AI litigation wave. KEY
 *   AGENTS (by structural relationship): - original_rights_holders:
 *   concentrated cost-bearing seat with a retained enforcement core
 *   (institutional/constrained) — bears foregone transformative-licensing
 *   categories, keeps the fixed-recasting license market -
 *   individual_authors: diffuse cost-bearers (powerless/constrained) — same
 *   boundary price without institutional enforcement capacity -
 *   downstream_creators: primary permission beneficiaries (moderate/mobile) -
 *   generative_ai_developers: concentrated beneficiaries of the
 *   intermediate-use permission (powerful/mobile) - general_public: diffuse
 *   beneficiaries (organized/mobile) - judiciary: agenda_setter — the
 *   boundary exists only insofar as the docket maintains it
 *   (institutional/analytical) - unrepresented_creators: excluded — no access
 *   to the forum that defines the line (powerless/trapped)
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(derivative_work_statutory_boundary__coordination_reading, 0.18).
domain_priors:suppression_score(derivative_work_statutory_boundary__coordination_reading, 0.2).
domain_priors:theater_ratio(derivative_work_statutory_boundary__coordination_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__coordination_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__coordination_reading, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__coordination_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__coordination_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__coordination_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(derivative_work_statutory_boundary__coordination_reading, rope).
narrative_ontology:human_readable(derivative_work_statutory_boundary__coordination_reading, "Derivative-Work Statutory Boundary — Coordination Reading (Fixed Expressive Recastings Only)").
narrative_ontology:topic_domain(derivative_work_statutory_boundary__coordination_reading, "legal/intellectual_property/technology_governance").

domain_priors:requires_active_enforcement(derivative_work_statutory_boundary__coordination_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(derivative_work_statutory_boundary__coordination_reading, 'e85bcd34-d824-4398-8cc9-573d309d0685').
narrative_ontology:cs_kernel_codification('e85bcd34-d824-4398-8cc9-573d309d0685', formalized).
narrative_ontology:cs_authority_grounding('e85bcd34-d824-4398-8cc9-573d309d0685', lineage).
narrative_ontology:cs_interpretation_layer_present('e85bcd34-d824-4398-8cc9-573d309d0685').
narrative_ontology:cs_reading_relation('e85bcd34-d824-4398-8cc9-573d309d0685', derivative_work_statutory_boundary__enclosure_reading, forecloses).
narrative_ontology:cs_reading_relation('e85bcd34-d824-4398-8cc9-573d309d0685', derivative_work_statutory_boundary__hybrid_carveout_reading, forecloses).
narrative_ontology:cs_axiom('e85bcd34-d824-4398-8cc9-573d309d0685', foundational, derivative_right_limited_to_fixed_expressive_recastings).
narrative_ontology:cs_axiom_status(derivative_right_limited_to_fixed_expressive_recastings, holdable).
narrative_ontology:cs_axiom_grounding('e85bcd34-d824-4398-8cc9-573d309d0685', derivative_right_limited_to_fixed_expressive_recastings, instrumental).
narrative_ontology:cs_axiom('e85bcd34-d824-4398-8cc9-573d309d0685', foundational, non_expressive_intermediate_use_lawful).
narrative_ontology:cs_axiom_status(non_expressive_intermediate_use_lawful, holdable).
narrative_ontology:cs_axiom_grounding('e85bcd34-d824-4398-8cc9-573d309d0685', non_expressive_intermediate_use_lawful, instrumental).
narrative_ontology:cs_reference_frame('e85bcd34-d824-4398-8cc9-573d309d0685', fixed_criterion_progress_boundary).
narrative_ontology:cs_drift_state('e85bcd34-d824-4398-8cc9-573d309d0685', generative_ai_litigation_era, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('e85bcd34-d824-4398-8cc9-573d309d0685', '').
narrative_ontology:cs_kernel_id(derivative_work_statutory_boundary__coordination_reading, derivative_work_statutory_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(derivative_work_statutory_boundary__coordination_reading, downstream_creators).
narrative_ontology:constraint_beneficiary(derivative_work_statutory_boundary__coordination_reading, generative_ai_developers).
narrative_ontology:constraint_beneficiary(derivative_work_statutory_boundary__coordination_reading, general_public).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(derivative_work_statutory_boundary__coordination_reading, original_rights_holders).
narrative_ontology:constraint_victim(derivative_work_statutory_boundary__coordination_reading, original_rights_holders).
narrative_ontology:constraint_victim(derivative_work_statutory_boundary__coordination_reading, individual_authors).
narrative_ontology:constraint_vindicates(derivative_work_statutory_boundary__coordination_reading, transformative_use_fair_use_pillar).
narrative_ontology:constraint_vindicates(derivative_work_statutory_boundary__coordination_reading, intermediate_copying_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Publishers, studios, record labels, and estates holding large copyright portfolios. They keep exclusive, enforceable control over fixed recastings — translations, film and stage adaptations, sequels, abridgments — and license that core as a primary revenue line. Under the operative boundary they cannot license or enjoin transformative criticism or the intermediate copies made inside computational processing, so entire categories of licensing revenue a broader boundary would create do not exist for them. Their enforcement spend concentrates on the fixed-recasting core and on test cases that would move the line outward. Leaving the regime is not an option: their asset base is the copyright estate itself, so they work the courts instead.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__coordination_reading, original_rights_holders, payer,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(derivative_work_statutory_boundary__coordination_reading, original_rights_holders, beneficiary).

% Working writers, illustrators, photographers, and musicians whose books, images, and recordings circulate in training corpora and are quoted, parodied, and remixed. They hold the same exclusive rights as institutional holders but lack the budgets to litigate; the licensing categories broader readings would open are ones they cannot practically price or police. What remains to them is the fixed-recasting right — translation and adaptation deals — which many exercise through agents and publishers. Their practical exit from the regime is nil and their leverage inside it is small.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__coordination_reading, individual_authors, payer,
    powerless, biographical, constrained, global).

% Parodists, critics, documentarians, essayists, remix artists, and fan creators who build new work out of existing works. The boundary lets them quote, transform, and process source material without clearing rights in advance; their practice depends on that permission and would be restructured by per-work licensing negotiations if the line moved. They can work in modes that avoid existing material, at a cost to what their work can say.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__coordination_reading, downstream_creators, beneficiary,
    moderate, biographical, mobile, global).

% Machine learning laboratories and research groups that train models on large corpora containing copyrighted works. The boundary treats the copies made during training as non-infringing intermediate steps, so no ex-ante licensing layer sits between them and the corpus; their model releases and product roadmaps are built on that permission. They could shift to licensed or opted-out corpora at substantial cost and capability loss, and some maintain opt-out mechanisms for future ingestion.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__coordination_reading, generative_ai_developers, beneficiary,
    powerful, generational, mobile, global).

% Readers, viewers, listeners, and users of generative tools. They receive criticism, parody, scholarship, and AI-assisted products at prices and varieties the permission structure keeps low; no single member holds a stake large enough to litigate over, and the benefit is spread across everyone who consumes culture or uses the tools.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__coordination_reading, general_public, beneficiary,
    organized, generational, mobile, global).

% Federal courts applying the substantial-similarity, fair use, and intermediate-copying lines that make up the operative boundary. They decide case by case which recastings are the rightsholder's to license and which uses stay free; the boundary exists only insofar as their docket maintains it, and both sides bring them expansionary theories. Their seat is the forum where every other seat's position is tested.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__coordination_reading, judiciary, agenda_setter,
    institutional, generational, analytical, national).

% Creators outside the litigation forum — non-US authors whose works are ingested into training corpora, hobbyists, and small rightsholders in sectors without trade associations or class counsel. They would ask for opt-out mechanisms or compensation floors if they had a seat, but the boundary is defined in US federal court and they have no practical access to it. Their works remain subject to the permission structure with no channel to object or bargain.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__coordination_reading, unrepresented_creators, excluded,
    powerless, biographical, trapped, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(derivative_work_statutory_boundary__coordination_reading, generative_ai_developers).
narrative_ontology:fixing_cost_class(derivative_work_statutory_boundary__coordination_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Sets one predictable line separating uses of existing expression that require the rightsholder's license (fixed recastings that substantially incorporate the original expression) from uses that do not (transformative recastings, quotation, and the intermediate copies made inside computation), so that creators and technologists can build without per-transaction negotiation while rightsholders retain a compact, enforceable core.
% TRANSFER_FUNCTION: Moves permission rather than money: expressive and computational freedom to transform, quote, and process existing works flows from rightsholders' exclusive control to downstream creators, researchers, and the public; in exchange the rightsholders' enforceable control concentrates over the fixed-recasting core. No payment changes hands by default.
% ABSENT_VOICES: Creators with no access to the forum that defines the boundary — non-US authors whose works are ingested into training corpora, hobbyists, and small rightsholders without trade associations — would ask for opt-out mechanisms or compensation floors. Their absence matters for provenance: the boundary's stability is attested largely by seats that benefit from it, while the cost-bearing seats with the least capacity to object are not in the room.
% DISAPPEARANCE_RATIONALE: The generative-technology sector's current shape, and a large share of transformative creation, proceed without ex-ante licenses because this boundary holds them outside the exclusive right. If the boundary vanished overnight — collapsing toward the broader readings or dissolving into case-by-case uncertainty — training corpora would freeze pending license negotiation or move to permissive jurisdictions, parody and criticism would shrink to what rightsholders tolerate, and licensing overhead would price small creators out of building on existing work.
% FOUNDING_PROBLEM: The 1976 Copyright Act defined derivative works broadly — 'any form in which a work may be recast, transformed, or adapted' — to secure authors' control over adaptations, but read without a limiting principle that breadth reaches quotation, criticism, and machine processing, swallowing fair use and freezing all reuse. The arrangement was built to solve: which recastings are the author's to license, and which uses of protected expression remain free?
% FOUNDING_PROBLEM_CORROBORATION: Attested from outside the beneficiary set by the cost-bearing seat: rightsholder litigants openly argue the boundary is drawn too narrowly, and their docket — training-data suits, transformative-use appeals — is the running corroboration that the question is unresolved. Appellate opinions and legislative testimony from both sides corroborate; no party to the dispute claims the question is settled.
narrative_ontology:disappearance_verdict(derivative_work_statutory_boundary__coordination_reading, world_rearranges).
narrative_ontology:founding_problem_status(derivative_work_statutory_boundary__coordination_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(derivative_work_statutory_boundary__coordination_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(derivative_work_statutory_boundary__coordination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(derivative_work_statutory_boundary__coordination_reading, 0.18, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(derivative_work_statutory_boundary__coordination_reading_tests).
:- end_tests(derivative_work_statutory_boundary__coordination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is low (0.18 at interval end) because the rule imposes no ex-ante licensing on the uses it governs; what remains is boundary uncertainty at the margin plus the foregone licensing categories borne by rights holders as the definitional price of the line — a cost this reading classifies as boundary-setting, not taking. Suppression (0.20) is the restraint the doctrine imposes on rightsholder exclusion plus residual chilling of marginal uses; it is enforced by courts rather than by barriers, and the machinery needed to hold the line has grown with contestation (see the suppression_requirement series, which tracks enforcement-capacity change: the boundary's maintenance requirement rises as expansionary theories proliferate, not because the rule got more coercive toward its users). Theater is low (0.15): the permission structure functions as stated, and the performative share is litigation positioning rather than maintenance of a dead function. Accessibility_collapse (0.30) is low for the same reason resistance (0.55) is high: the sibling readings remain fully live and organized rightsholder industries litigate the boundary's reach continuously — the rule holds because courts keep drawing it, not because alternatives have collapsed. The claim (rope) and the metrics are authored independently; the engine computes per-seat types from the structural data.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the rights-holder seat the arrangement reads as a confiscation of licensing control — the same act (training on their catalog) is, from that chair, uncompensated appropriation, and that seat endorses a different reading of the same statutory text. From the downstream and public seats the identical structure is enabling infrastructure: the reason parody, criticism, and model training are possible at all without a licensing layer. The judiciary seat experiences neither benefit nor cost — case-by-case line-drawing. The engine derives these divergences from power, exit, and declared position; this commentary does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries (downstream_creators, generative_ai_developers, general_public) derive low directionality — the rule subsidizes their practice. No victims are declared, deliberately, under the ε-invariance discipline: assessed by this reading's own lights no seat is extracted from — the costs borne by original_rights_holders and individual_authors are the boundary's coordination price, not a transfer to a capturer — and declaring them as extraction targets would import a rival reading's framing into this story's ε. Their cost-bearing position is recorded in their roles and situations (payer seats, with a retained secondary benefit for the institutional holders); their directionality should sit mid-to-high from structure (constrained exit, concentrated cost) while constraint-level extraction stays low because the reading holds the transfer legitimate. The gap between 'this seat bears real costs' and 'this seat is extracted from' is exactly the seam between this reading and its siblings, and it is carried in the omegas rather than resolved here. No directionality overrides are authored: overrides key on the power atom, and original_rights_holders and the judiciary share the institutional atom, so an override calibrated for one would misfire on the other; the structural data plus the payer/beneficiary role declarations carry the differentiation instead.
 *
 * MANDATROPHY ANALYSIS:
 *   The rope classification guards against the two mislabels this boundary attracts. Read from the enclosure seat, the rule looks like pure extraction — property handed to AI developers, and the receipt surface does name where the net flow lands — but the classification requires extraction to run through the same structure that coordinates, with identifiable coerced victims; here the coordination function is real (one predictable line replacing per-transaction negotiation) and no seat is coerced into participation. Read from the opposite direction, the rule could be mislabeled as a spent mandate — the 1976 Act's adaptation problem long solved — but the founding question (which recastings are licensable) is live, corroborated by the cost-bearing seat's own litigation, so no mandatrophy is declared. The persistence risk to watch is not atrophy but displacement: the omegas track whether the boundary survives output substitution and the enclosure challenge.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    enclosure_displacement_structural_delta,
    'This story instantiates the coordination_reading of the derivative_work_statutory_boundary kernel. If the enclosure_reading — any use of protected expression in creating a new work constitutes preparation of a derivative work — displaced it as the operative boundary, which structural elements of this story change?',
    'Appellate resolution of the training-data and transformative-use docket: a holding that intermediate copies or transformative recastings fall within the exclusive rights would make the enclosure reading operative.',
    'Ex-ante licensing would attach to all expressive reuse: extractiveness rises sharply, downstream_creators and generative_ai_developers flip from beneficiaries to cost-bearing seats, and the arrangement recomputes with an extraction-gatekeeper structure. The disagreement is located in the scope of ''recast, transformed, or adapted'' — whether it reaches non-expressive, intermediate, and transformative incorporations.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(enclosure_displacement_structural_delta, empirical, 'What flips structurally if the enclosure reading wins the kernel contest.').

omega_variable(
    hybrid_carveout_structural_delta,
    'If the hybrid_carveout_reading — the boundary varies with commercial exploitation, permitting non-commercial transformative use while requiring authorization for commercial use — displaced this reading, what changes structurally?',
    'Doctrinal movement indexing the boundary to commerciality (a generalized commerciality factor in transformative-use analysis), observable in appellate treatment of commercial transformative uses and training-for-product cases.',
    'The boundary becomes commerciality-indexed: downstream_creators keep their permission while generative_ai_developers and commercial transformative users flip to cost-bearing seats requiring ex-ante licenses; extraction rises for the commercial seats only. The disagreement is located in whether the commercial/non-commercial line tracks the expressive-substitution harm the exclusive right exists to prevent.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(hybrid_carveout_structural_delta, empirical, 'What flips structurally if the hybrid carveout reading wins the kernel contest.').

omega_variable(
    output_substitution_collapse,
    'Does the lawfulness of intermediate uses remain stable as generative outputs increasingly substitute for works in the training corpus, or does output-market substitution collapse the distinction between intermediate processing and preparation of a derivative work?',
    'Output-market studies and the training-output litigation line: measurable substitution of model outputs for training works would give courts a harm-based ground to recharacterize training.',
    'If the distinction collapses, this reading''s low extraction is contingent on a technological window rather than a stable boundary, and the arrangement drifts toward the enclosure reading''s structure without any formal adoption of it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(output_substitution_collapse, empirical, 'Whether the intermediate-use permission survives output substitution.').

omega_variable(
    residual_suppression_source,
    'Is the residual suppression at the boundary — marginal transformative uses abandoned for fear of suit — a property of doctrinal uncertainty that a clearer line would remove, or of litigation-cost asymmetry that persists under any formulation?',
    'Cross-jurisdiction comparison of ex-ante exception regimes (enumerated fair-dealing exceptions) against the case-by-case regime on rates of abandoned or cleared-at-cost uses.',
    'If suppression is cost-driven rather than doctrinal, the low measured suppression overstates the permission clarity any boundary formulation could deliver, and part of what this story credits to the reading belongs to litigation economics.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(residual_suppression_source, empirical, 'Whether residual chilling is doctrinal or cost-structural.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(derivative_work_statutory_boundary__coordination_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dwsb_coord_reading_tr_t0, derivative_work_statutory_boundary__coordination_reading, theater_ratio, 0, 0.07).
narrative_ontology:measurement_basis(dwsb_coord_reading_tr_t0, observed).
narrative_ontology:measurement(dwsb_coord_reading_tr_t6, derivative_work_statutory_boundary__coordination_reading, theater_ratio, 6, 0.08).
narrative_ontology:measurement_basis(dwsb_coord_reading_tr_t6, observed).
narrative_ontology:measurement(dwsb_coord_reading_tr_t12, derivative_work_statutory_boundary__coordination_reading, theater_ratio, 12, 0.1).
narrative_ontology:measurement_basis(dwsb_coord_reading_tr_t12, observed).
narrative_ontology:measurement(dwsb_coord_reading_tr_t18, derivative_work_statutory_boundary__coordination_reading, theater_ratio, 18, 0.11).
narrative_ontology:measurement_basis(dwsb_coord_reading_tr_t18, observed).
narrative_ontology:measurement(dwsb_coord_reading_tr_t24, derivative_work_statutory_boundary__coordination_reading, theater_ratio, 24, 0.13).
narrative_ontology:measurement_basis(dwsb_coord_reading_tr_t24, observed).
narrative_ontology:measurement(dwsb_coord_reading_tr_t30, derivative_work_statutory_boundary__coordination_reading, theater_ratio, 30, 0.15).
narrative_ontology:measurement_basis(dwsb_coord_reading_tr_t30, observed).

% Extraction over time
narrative_ontology:measurement(dwsb_coord_reading_be_t0, derivative_work_statutory_boundary__coordination_reading, base_extractiveness, 0, 0.11).
narrative_ontology:measurement_basis(dwsb_coord_reading_be_t0, observed).
narrative_ontology:measurement(dwsb_coord_reading_be_t6, derivative_work_statutory_boundary__coordination_reading, base_extractiveness, 6, 0.12).
narrative_ontology:measurement_basis(dwsb_coord_reading_be_t6, observed).
narrative_ontology:measurement(dwsb_coord_reading_be_t12, derivative_work_statutory_boundary__coordination_reading, base_extractiveness, 12, 0.14).
narrative_ontology:measurement_basis(dwsb_coord_reading_be_t12, observed).
narrative_ontology:measurement(dwsb_coord_reading_be_t18, derivative_work_statutory_boundary__coordination_reading, base_extractiveness, 18, 0.15).
narrative_ontology:measurement_basis(dwsb_coord_reading_be_t18, observed).
narrative_ontology:measurement(dwsb_coord_reading_be_t24, derivative_work_statutory_boundary__coordination_reading, base_extractiveness, 24, 0.16).
narrative_ontology:measurement_basis(dwsb_coord_reading_be_t24, observed).
narrative_ontology:measurement(dwsb_coord_reading_be_t30, derivative_work_statutory_boundary__coordination_reading, base_extractiveness, 30, 0.18).
narrative_ontology:measurement_basis(dwsb_coord_reading_be_t30, observed).

% Suppression requirement over time
narrative_ontology:measurement(dwsb_coord_reading_su_t0, derivative_work_statutory_boundary__coordination_reading, suppression_requirement, 0, 0.12).
narrative_ontology:measurement_basis(dwsb_coord_reading_su_t0, observed).
narrative_ontology:measurement(dwsb_coord_reading_su_t6, derivative_work_statutory_boundary__coordination_reading, suppression_requirement, 6, 0.14).
narrative_ontology:measurement_basis(dwsb_coord_reading_su_t6, observed).
narrative_ontology:measurement(dwsb_coord_reading_su_t12, derivative_work_statutory_boundary__coordination_reading, suppression_requirement, 12, 0.16).
narrative_ontology:measurement_basis(dwsb_coord_reading_su_t12, observed).
narrative_ontology:measurement(dwsb_coord_reading_su_t18, derivative_work_statutory_boundary__coordination_reading, suppression_requirement, 18, 0.17).
narrative_ontology:measurement_basis(dwsb_coord_reading_su_t18, observed).
narrative_ontology:measurement(dwsb_coord_reading_su_t24, derivative_work_statutory_boundary__coordination_reading, suppression_requirement, 24, 0.19).
narrative_ontology:measurement_basis(dwsb_coord_reading_su_t24, observed).
narrative_ontology:measurement(dwsb_coord_reading_su_t30, derivative_work_statutory_boundary__coordination_reading, suppression_requirement, 30, 0.2).
narrative_ontology:measurement_basis(dwsb_coord_reading_su_t30, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(derivative_work_statutory_boundary__coordination_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(derivative_work_statutory_boundary__coordination_reading, enclosure_reading).
narrative_ontology:affects_constraint(derivative_work_statutory_boundary__coordination_reading, hybrid_carveout_reading).

% DUAL FORMULATION NOTE:
% The natural-language label 'the derivative work boundary' covers three structurally distinct claims about where the exclusive right ends, decomposed per the ε-invariance principle into a constraint family. This file instantiates the coordination reading (fixed criterion: only fixed recastings substantially incorporating original expression; transformative and intermediate uses free; ε ≈ 0.18). The enclosure reading (any use of protected expression in a new work prepares a derivative work; high ε — ex-ante licensing for all reuse) and the hybrid carveout reading (commerciality-indexed boundary; intermediate ε concentrated on commercial seats) are separate stories with their own beneficiaries, cost-bearing seats, and classifications. Whichever reading is operative determines the others' beneficiary and cost structure, so each family member's network points at its siblings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
