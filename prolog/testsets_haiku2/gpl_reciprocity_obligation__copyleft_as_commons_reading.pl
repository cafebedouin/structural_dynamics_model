% ============================================================================
% CONSTRAINT STORY: gpl_reciprocity_obligation__copyleft_as_commons_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gpl_reciprocity_obligation__copyleft_as_commons_reading, []).

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
 *   constraint_id: gpl_reciprocity_obligation__copyleft_as_commons_reading
 *   human_readable: GPL Reciprocity Obligation: Commons Enclosure Prevention Reading
 *   domain: intellectual-property/open-source-governance/software-licensing
 *
 * SUMMARY:
 *   The GPL is an institutional technology that uses copyright law to prevent
 *   commons enclosure. Under this reading — the commons-preservation reading
 *   — the GPL's reciprocity obligation is not a constraint on individual
 *   freedom (the freedom reading) nor a business-model restriction (the
 *   restriction reading), but rather a mechanism that protects the commons as
 *   a collective entity against the economic incentives that would otherwise
 *   fragment it into proprietary forks. Proprietary integrators experience it
 *   as a cost: they cannot build closed-source derivatives without releasing
 *   their modifications. But from the commons-reading perspective, that
 *   constraint IS the commons-preservation mechanism; removing it would
 *   dissolve the commons entirely. This story instantiates only this reading,
 *   not the others.
 *
 * KEY AGENTS:
 *   - commons_as_institution — the emergent collective beneficiary; does not act but is constituted by GPL's persistence
 *   - downstream_users (powerless/constrained) — inherit redistribution rights; would lose them if GPL reciprocity disappeared
 *   - proprietary_integrators (powerful/constrained) — bear the cost of disclosure; cannot build proprietary derivatives on GPL'd code
 *   - commercial_derivative_builders (organized/constrained) — must choose between releasing derivatives under GPL or avoiding GPL entirely
 *   - original_GPL_authors (moderate/mobile) — set the GPL's reciprocity rule; retain moral/technical authority over commons boundary
 *   - regulatory_bodies (institutional/analytical) — observe GPL enforceability; set legal frame
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gpl_reciprocity_obligation__copyleft_as_commons_reading, 0.52).
domain_priors:suppression_score(gpl_reciprocity_obligation__copyleft_as_commons_reading, 0.41).
domain_priors:theater_ratio(gpl_reciprocity_obligation__copyleft_as_commons_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_commons_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_commons_reading, suppression_requirement, 0.41).
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_commons_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_commons_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_commons_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gpl_reciprocity_obligation__copyleft_as_commons_reading, tangled_rope).
narrative_ontology:human_readable(gpl_reciprocity_obligation__copyleft_as_commons_reading, "GPL Reciprocity Obligation: Commons Enclosure Prevention Reading").
narrative_ontology:topic_domain(gpl_reciprocity_obligation__copyleft_as_commons_reading, "intellectual-property/open-source-governance/software-licensing").

domain_priors:requires_active_enforcement(gpl_reciprocity_obligation__copyleft_as_commons_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gpl_reciprocity_obligation__copyleft_as_commons_reading, '5f3b352b-2251-4626-8d75-95ad9943c86b').
narrative_ontology:cs_kernel_codification('5f3b352b-2251-4626-8d75-95ad9943c86b', fixed_text).
narrative_ontology:cs_authority_grounding('5f3b352b-2251-4626-8d75-95ad9943c86b', lineage).
narrative_ontology:cs_interpretation_layer_present('5f3b352b-2251-4626-8d75-95ad9943c86b').
narrative_ontology:cs_reading_relation('5f3b352b-2251-4626-8d75-95ad9943c86b', gpl_reciprocity_obligation__copyleft_as_freedom_reading, coexists_with).
narrative_ontology:cs_reading_relation('5f3b352b-2251-4626-8d75-95ad9943c86b', gpl_reciprocity_obligation__copyleft_as_restriction_reading, influences).
narrative_ontology:cs_axiom('5f3b352b-2251-4626-8d75-95ad9943c86b', foundational, commons_persistence_requires_mandatory_reciprocity).
narrative_ontology:cs_axiom_status(commons_persistence_requires_mandatory_reciprocity, holdable).
narrative_ontology:cs_axiom_grounding('5f3b352b-2251-4626-8d75-95ad9943c86b', commons_persistence_requires_mandatory_reciprocity, instrumental).
narrative_ontology:cs_axiom('5f3b352b-2251-4626-8d75-95ad9943c86b', foundational, proprietary_enclosure_is_structurally_incentivized_absent_reciprocity).
narrative_ontology:cs_axiom_status(proprietary_enclosure_is_structurally_incentivized_absent_reciprocity, holdable).
narrative_ontology:cs_axiom_grounding('5f3b352b-2251-4626-8d75-95ad9943c86b', proprietary_enclosure_is_structurally_incentivized_absent_reciprocity, empirically_contingent).
narrative_ontology:cs_reference_frame('5f3b352b-2251-4626-8d75-95ad9943c86b', commons_sustainability_via_reciprocal_obligation).
narrative_ontology:cs_drift_state('5f3b352b-2251-4626-8d75-95ad9943c86b', contemporary_proprietary_integration_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('5f3b352b-2251-4626-8d75-95ad9943c86b', '2026-06-11T14:32:00Z').
narrative_ontology:cs_kernel_id(gpl_reciprocity_obligation__copyleft_as_commons_reading, gpl_reciprocity_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gpl_reciprocity_obligation__copyleft_as_commons_reading, commons_as_institution).
narrative_ontology:constraint_beneficiary(gpl_reciprocity_obligation__copyleft_as_commons_reading, downstream_users).
narrative_ontology:constraint_victim(gpl_reciprocity_obligation__copyleft_as_commons_reading, proprietary_integrators).
narrative_ontology:constraint_victim(gpl_reciprocity_obligation__copyleft_as_commons_reading, commercial_derivative_builders).
narrative_ontology:constraint_vindicates(gpl_reciprocity_obligation__copyleft_as_commons_reading, commons_sustainability_requires_reciprocity).
narrative_ontology:constraint_vindicates(gpl_reciprocity_obligation__copyleft_as_commons_reading, software_freedom_entails_redistribution_obligation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The open-source commons is the emergent collective beneficiary of the GPL's reciprocity obligation. When derivatives are released under GPL, the commons pool grows rather than fragmenting into proprietary forks. The commons does not take action in the traditional sense, but it is constituted by the GPL's institutional structure — the GPL is the technology through which the commons maintains its boundary and growth trajectory. Without GPL's reciprocity, the commons would fragment as successful projects are captured and proprietary derivatives lock users out.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_commons_reading, commons_as_institution, beneficiary,
    organized, generational, analytical, global).
narrative_ontology:stakeholder_non_agent(gpl_reciprocity_obligation__copyleft_as_commons_reading, commons_as_institution).

% Users who receive software built on GPL'd code inherit the right to use, modify, and redistribute any derivative works under GPL. This right persists even when proprietary companies integrate GPL'd code and create proprietary extensions — downstream users can extract and re-use the GPL'd layer. They benefit from the commons growth that the GPL's reciprocity requirement sustains; they bear minimal burden (redistribution right is a benefit, not a cost).
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_commons_reading, downstream_users, beneficiary,
    powerless, biographical, constrained, global).

% Large software companies that would like to build proprietary derivatives on top of GPL'd code cannot do so without releasing their modifications under GPL. This is expensive (loss of proprietary margin on the derivative layer) and strategically constraining (they cannot lock users into proprietary extensions). Their exit options are costly: avoid GPL entirely and build proprietary from scratch (duplicative work), or use GPL code internally while building non-linked proprietary tools (architectural limitation), or accept GPL's terms and release derivatives under GPL (strategically acceptable but less profitable than proprietary closure).
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_commons_reading, proprietary_integrators, payer,
    powerful, biographical, constrained, global).

% Companies and developers that build value-added services/tools on top of GPL'd software face a clear but constraining choice: (1) release proprietary derivatives under GPL and compete on service/support (accepted by many, sustainable businesses exist this way); (2) avoid GPL'd dependencies and build proprietary from scratch (costly, slow to market); (3) negotiate dual-licensing agreements with copyright holders (expensive, case-by-case, not available for all projects). Many have adapted to option (1), but the GPL's constraint on proprietary monetization is real.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_commons_reading, commercial_derivative_builders, payer,
    organized, biographical, constrained, global).

% Original GPL authors choose to license their work under GPL at release, setting the commons's boundary conditions. They retain moral and technical authority over the GPL covenant (through organizations like the FSF and GPL stewardship processes), though individual author power wanes as projects mature and contributor bases grow. The GPL itself becomes an institutional technology that persists beyond any individual author's agency.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_commons_reading, original_gpl_authors, agenda_setter,
    moderate, generational, mobile, global).

% Courts and legislators observe and occasionally intervene on GPL enforceability. Courts have upheld GPL's copyleft provisions as enforceable license terms (Jacobsen v. Katzer, Software Freedom Conservancy cases). Legislators in some jurisdictions consider whether mandatory-reciprocity open-source licenses should be treated differently from proprietary licensing regimes. They set the legal frame within which GPL operates but do not actively enforce it (that falls to copyright holders).
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_commons_reading, regulatory_bodies, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gpl_reciprocity_obligation__copyleft_as_commons_reading, commons_as_institution).
narrative_ontology:fixing_cost_class(gpl_reciprocity_obligation__copyleft_as_commons_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the commons-enclosure problem by making it economically and legally costly to privatize shared code: the GPL's reciprocity requirement redirects incentive structures so that derivative works flow back to the commons rather than bifurcating into proprietary forks, maintaining commons integrity across generations of developers.
% TRANSFER_FUNCTION: Moves knowledge/code-as-labor from individual developers' proprietary derivative work into the commons's collective pool. Individual proprietary integrators bear the cost of disclosure (loss of monopoly rent from proprietary derivatives); the commons and downstream users collect the benefit (continued growth of shared codebase, retained freedom to use and modify).
% ABSENT_VOICES: Developers who would prefer to build proprietary derivatives without GPL obligations are structurally excluded by the license's terms; they can object in principle but cannot participate in GPL commons except under the reciprocity obligation. Proprietary software companies that would benefit from free access to GPL code without disclosure obligation are kept at the boundary; their exclusion is maintained by the GPL covenant's enforceability.
% DISAPPEARANCE_RATIONALE: If the GPL's reciprocity obligation vanished overnight (either through legal nullification or mass license-switching to permissive alternatives like MIT), the commons would immediately fragment: every competent derivative would be locked into proprietary forks by commercial integrators, downstream users would lose redistribution rights on the proprietary branches, and the commons's growth would slow or stop as individual developers have no incentive to contribute back. The software ecosystem would reorganize around proprietary capture of successful commons projects.
% FOUNDING_PROBLEM: Early open-source software faced the enclosure problem: successful GPL'd projects were built on by commercial companies who integrated them into proprietary products, locked users into those products, and cut the commons off from the innovations. Without a reciprocity mechanism, the commons would drain talent and code toward proprietary derivatives while individual developers bore the cost of commons maintenance.
% FOUNDING_PROBLEM_CORROBORATION: Documented in GPL adoption cases (GNU Emacs, Linux, GCC history) and in interviews with commercial integrators: companies state that GPL's reciprocity requirement genuinely constrains their derivative strategies. Researchers studying open-source sustainability (von Krogh et al., Lerner & Tirole) document that without reciprocity mechanisms commons projects face contributor drop-off as individual developers defect to proprietary forks. The founding problem persists because enclosure remains economically attractive; the GPL remains necessary to hold the commons. Regulatory bodies (US Copyright Office, EU Digital Markets Act deliberations) acknowledge GPL's role in preventing enclosure as a live structural concern.
narrative_ontology:disappearance_verdict(gpl_reciprocity_obligation__copyleft_as_commons_reading, world_rearranges).
narrative_ontology:founding_problem_status(gpl_reciprocity_obligation__copyleft_as_commons_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gpl_reciprocity_obligation__copyleft_as_commons_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(gpl_reciprocity_obligation__copyleft_as_commons_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gpl_reciprocity_obligation__copyleft_as_commons_reading, 0.52, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gpl_reciprocity_obligation__copyleft_as_commons_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(gpl_reciprocity_obligation__copyleft_as_commons_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(gpl_reciprocity_obligation__copyleft_as_commons_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.52 at interval end) because the GPL does extract something real from proprietary integrators — it denies them the option to build proprietary derivatives on GPL'd code. But the extraction is not conversion to private rents; it is redirection toward commons growth. The commons and downstream users do not pay for this benefit; they receive it as the structure takes shape. Suppression is lower (0.41) because the GPL's enforcement is legal (copyrights + license terms) rather than coercive — developers and companies CAN choose alternatives, though the choice is costly. Theater is minimal (0.18) because the GPL's function is structural; there is no performative maintenance. The measurement series shows extractiveness and suppression rising during the interval (0–25) and then plateauing — corresponding to the period when GPL adoption spread, enforcement was tested in courts, and commercial opposition crystallized, after which the constraint stabilized at a new equilibrium.
 *
 * PERSPECTIVAL GAP:
 *   From the proprietary-integrator seat, the GPL appears as a constraint on business model freedom — a restriction they would like to escape. From the commons-as-institution seat (conceptually, since commons has no voice), the same GPL is a preservation mechanism. From the downstream-user seat, it is a guarantee of continued freedom. The engine computes these divergent per-seat classifications from the structural data: proprietary integrators see high d (near target end), commons beneficiaries see low d (near beneficiary end), downstream users see near-symmetric (genuine coordination benefit, diffuse cost). The authored claim (tangled_rope) reflects that the constraint genuinely coordinates commons growth AND asymmetrically extracts from those who would privatize it.
 *
 * DIRECTIONALITY LOGIC:
 *   Proprietary integrators are high-d targets: they cannot build proprietary derivatives without GPL obligation (constrained exit, powerful agent, but structurally unable to exit the constraint except by abandoning the codebase entirely). Downstream users are low-d beneficiaries: they gain redistribution rights with minimal burden. The commons itself, conceptually, is the ultimate beneficiary — the constraint is designed to preserve it. Original GPL authors are near-symmetric: they bear the cost of maintaining the institution but also benefit from the commons's growth and their own moral authority. Commercial derivative builders sit between: they experience constraint (cannot do proprietary derivatives) but also gain from access to GPL'd code and a clear, predictable licensing frame.
 *
 * MANDATROPHY ANALYSIS:
 *   The GPL's founding problem — preventing commons enclosure — remains live. The GPL has not atrophied; it is actively enforced in courts (FSF v. Cisco, Software Freedom Conservancy cases) and widely adopted. The constraint shows moderate theater (0.18) because the GPL's function is real structural maintenance, not performative. A mandatrophy reading would require the GPL to persist theatrically while the founding problem had died — but the founding problem has not died; commercial enclosure remains economically incentivized and legally feasible without the GPL. The constraint is not a piton.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    commons_as_agent_ambiguity,
    'Is ''commons as institution'' a real agent that can be beneficiary/victim, or is it a metaphor for the aggregate of individual developers and users?',
    'Examine GPL maintenance and governance structures: if the commons exhibits agency (e.g., GPL revisions, enforcement decisions) through formal institutions, it is an agent; if all decisions are made by individual developers/holders, it is a metaphor.',
    'If the commons is an agent, the constraint''s classification holds as authored (tangled_rope with commons as beneficiary). If it is a metaphor, the constraint decomposes into individual-level freedoms/constraints, shifting ε and victim/beneficiary structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(commons_as_agent_ambiguity, conceptual, 'Whether the commons can be treated as an institutional beneficiary or must be disaggregated to individual developers and users.').

omega_variable(
    extraction_vs_commons_mechanism_boundary,
    'Is the GPL''s constraint on proprietary integration a cost imposed by the commons (extractive), or is it intrinsic to commons structure (definitional)?',
    'Counterfactual comparison with permissive licenses (MIT, Apache): if permissive-licensed commons projects fragment as quickly and proprietary integrators do not contribute back, the GPL''s constraint is extractive (a cost borne by integrators). If they remain cohesive (commons growth via market incentives), the constraint is definitional (not extractive, just a boundary).',
    'If extractive, the GPL is accurately classified as tangled_rope (coordination + extraction). If definitional, it might reclassify as rope or even mountain (if commons preservation is natural/unavoidable). Very high impact on ε calibration.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(extraction_vs_commons_mechanism_boundary, empirical, 'Whether GPL''s reciprocity is an imposed cost or a structural necessity of commons persistence.').

omega_variable(
    commons_reading_vs_restriction_reading_foreclosure,
    'Does the commons-preservation reading (this one) logically foreclose the restriction reading, or can both coexist?',
    'Examine whether a GPL advocate can simultaneously hold ''GPL preserves commons'' and ''GPL is a business-model restriction.'' If the claims are mutually exclusive (preservation requires restriction, restriction denies preservation), they foreclose. If a single agent can hold both without contradiction, they coexist.',
    'If they foreclose, update cs_structure.reading_relations to forecloses for restriction_reading. If they coexist (the restriction is instrumental to preservation), the relation is coexists_with or influences.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(commons_reading_vs_restriction_reading_foreclosure, conceptual, 'Logical relationship between the commons-preservation and restriction readings.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the GPL''s suppression of proprietary derivative-building structural (legal/technological barriers) or internalized (developers choose GPL over proprietary due to values)?',
    'Post-GPL choice architecture: if developers constrained by GPL would still choose proprietary if alternatives existed, suppression is structural. If they would choose GPL anyway due to community values/identity-fusion, suppression is partially internalized.',
    'If structural, the measured suppression (0.41) is accurate. If internalized, the constraint''s effective suppression may be lower than measured (developers have internalized the GPL norm and do not perceive it as imposed).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Degree to which GPL enforcement is structural (legal) vs. internalized (values/identity).').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gpl_reciprocity_obligation__copyleft_as_commons_reading, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gpl__tr_t0, gpl_reciprocity_obligation__copyleft_as_commons_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement_basis(gpl__tr_t0, projected).
narrative_ontology:measurement(gpl__tr_t5, gpl_reciprocity_obligation__copyleft_as_commons_reading, theater_ratio, 5, 0.12).
narrative_ontology:measurement_basis(gpl__tr_t5, observed).
narrative_ontology:measurement(gpl__tr_t10, gpl_reciprocity_obligation__copyleft_as_commons_reading, theater_ratio, 10, 0.14).
narrative_ontology:measurement_basis(gpl__tr_t10, observed).
narrative_ontology:measurement(gpl__tr_t15, gpl_reciprocity_obligation__copyleft_as_commons_reading, theater_ratio, 15, 0.16).
narrative_ontology:measurement_basis(gpl__tr_t15, observed).
narrative_ontology:measurement(gpl__tr_t20, gpl_reciprocity_obligation__copyleft_as_commons_reading, theater_ratio, 20, 0.18).
narrative_ontology:measurement_basis(gpl__tr_t20, observed).
narrative_ontology:measurement(gpl__tr_t25, gpl_reciprocity_obligation__copyleft_as_commons_reading, theater_ratio, 25, 0.18).
narrative_ontology:measurement_basis(gpl__tr_t25, observed).
narrative_ontology:measurement(gpl__tr_t30, gpl_reciprocity_obligation__copyleft_as_commons_reading, theater_ratio, 30, 0.18).
narrative_ontology:measurement_basis(gpl__tr_t30, observed).
narrative_ontology:measurement(gpl__tr_t35, gpl_reciprocity_obligation__copyleft_as_commons_reading, theater_ratio, 35, 0.18).
narrative_ontology:measurement_basis(gpl__tr_t35, observed).

% Extraction over time
narrative_ontology:measurement(gpl__be_t0, gpl_reciprocity_obligation__copyleft_as_commons_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement_basis(gpl__be_t0, projected).
narrative_ontology:measurement(gpl__be_t5, gpl_reciprocity_obligation__copyleft_as_commons_reading, base_extractiveness, 5, 0.42).
narrative_ontology:measurement_basis(gpl__be_t5, observed).
narrative_ontology:measurement(gpl__be_t10, gpl_reciprocity_obligation__copyleft_as_commons_reading, base_extractiveness, 10, 0.46).
narrative_ontology:measurement_basis(gpl__be_t10, observed).
narrative_ontology:measurement(gpl__be_t15, gpl_reciprocity_obligation__copyleft_as_commons_reading, base_extractiveness, 15, 0.5).
narrative_ontology:measurement_basis(gpl__be_t15, observed).
narrative_ontology:measurement(gpl__be_t20, gpl_reciprocity_obligation__copyleft_as_commons_reading, base_extractiveness, 20, 0.51).
narrative_ontology:measurement_basis(gpl__be_t20, observed).
narrative_ontology:measurement(gpl__be_t25, gpl_reciprocity_obligation__copyleft_as_commons_reading, base_extractiveness, 25, 0.52).
narrative_ontology:measurement_basis(gpl__be_t25, observed).
narrative_ontology:measurement(gpl__be_t30, gpl_reciprocity_obligation__copyleft_as_commons_reading, base_extractiveness, 30, 0.52).
narrative_ontology:measurement_basis(gpl__be_t30, observed).
narrative_ontology:measurement(gpl__be_t35, gpl_reciprocity_obligation__copyleft_as_commons_reading, base_extractiveness, 35, 0.52).
narrative_ontology:measurement_basis(gpl__be_t35, observed).

% Suppression requirement over time
narrative_ontology:measurement(gpl__su_t0, gpl_reciprocity_obligation__copyleft_as_commons_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement_basis(gpl__su_t0, projected).
narrative_ontology:measurement(gpl__su_t5, gpl_reciprocity_obligation__copyleft_as_commons_reading, suppression_requirement, 5, 0.37).
narrative_ontology:measurement_basis(gpl__su_t5, observed).
narrative_ontology:measurement(gpl__su_t10, gpl_reciprocity_obligation__copyleft_as_commons_reading, suppression_requirement, 10, 0.39).
narrative_ontology:measurement_basis(gpl__su_t10, observed).
narrative_ontology:measurement(gpl__su_t15, gpl_reciprocity_obligation__copyleft_as_commons_reading, suppression_requirement, 15, 0.4).
narrative_ontology:measurement_basis(gpl__su_t15, observed).
narrative_ontology:measurement(gpl__su_t20, gpl_reciprocity_obligation__copyleft_as_commons_reading, suppression_requirement, 20, 0.41).
narrative_ontology:measurement_basis(gpl__su_t20, observed).
narrative_ontology:measurement(gpl__su_t25, gpl_reciprocity_obligation__copyleft_as_commons_reading, suppression_requirement, 25, 0.41).
narrative_ontology:measurement_basis(gpl__su_t25, observed).
narrative_ontology:measurement(gpl__su_t30, gpl_reciprocity_obligation__copyleft_as_commons_reading, suppression_requirement, 30, 0.41).
narrative_ontology:measurement_basis(gpl__su_t30, observed).
narrative_ontology:measurement(gpl__su_t35, gpl_reciprocity_obligation__copyleft_as_commons_reading, suppression_requirement, 35, 0.41).
narrative_ontology:measurement_basis(gpl__su_t35, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gpl_reciprocity_obligation__copyleft_as_commons_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(gpl_reciprocity_obligation__copyleft_as_commons_reading, 0.12).
narrative_ontology:affects_constraint(gpl_reciprocity_obligation__copyleft_as_commons_reading, gpl_reciprocity_obligation__copyleft_as_freedom_reading).
narrative_ontology:affects_constraint(gpl_reciprocity_obligation__copyleft_as_commons_reading, gpl_reciprocity_obligation__copyleft_as_restriction_reading).
narrative_ontology:affects_constraint(gpl_reciprocity_obligation__copyleft_as_commons_reading, software_commons_sustainability).
narrative_ontology:affects_constraint(gpl_reciprocity_obligation__copyleft_as_commons_reading, proprietary_derivative_capture).

% DUAL FORMULATION NOTE:
% This constraint (commons-preservation reading) shares the GPL kernel with two sibling readings: copyleft_as_freedom_reading (user-freedoms lens) and copyleft_as_restriction_reading (business-constraint lens). Each reading has different ε, beneficiary/victim structure, and measured extractiveness. They are linked via the same GPL text but instantiate different constraints because the readings assign different referents and stakeholder roles to the same obligation. See omegas on reading-coexistence and commons-agency ambiguity for unresolved structural questions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(gpl_reciprocity_obligation__copyleft_as_commons_reading, powerful, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
