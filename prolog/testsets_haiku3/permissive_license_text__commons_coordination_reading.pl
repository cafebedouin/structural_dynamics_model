% ============================================================================
% CONSTRAINT STORY: permissive_license_text__commons_coordination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_permissive_license_text__commons_coordination_reading, []).

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
 *   constraint_id: permissive_license_text__commons_coordination_reading
 *   human_readable: Permissive Open-Source License Coordination (Commons Reading)
 *   domain: software_licensing/intellectual_property/technology_governance
 *
 * SUMMARY:
 *   Permissive open-source licensing (MIT, Apache 2.0, BSD) is instantiated
 *   by this reading as a coordination mechanism that solves the problem of
 *   legal friction preventing collaborative knowledge production. The reading
 *   holds that MINIMIZING legal constraints — especially reciprocity
 *   requirements — expands the pool of implementers and creates universal
 *   access to innovation. No party is harmed by the arrangement; all parties
 *   with implementation interest benefit from lowered barriers. This reading
 *   contrasts sharply with the copyleft_counterfactual_reading (which argues
 *   that permissive licensing WITHOUT reciprocity enables exploitation and
 *   that viral reciprocity is necessary) and the corporate_moat_reading
 *   (which argues permissive licensing enables uncompensated extraction for
 *   proprietary derivatives). This story is ONE reading of the contested
 *   kernel 'permissive_license_text' — not the whole debate, but the specific
 *   claim that freedom emerges from minimized friction.
 *
 * KEY AGENTS:
 *   - implementer_commons: Distributed set of developers and organizations with interest in adopting, modifying, and building on permissively-licensed code; no centralized power, diversity of backgrounds and motivations
 *   - license_grantor: Original copyright holder who elects permissive licensing; retains attribution and moral rights but relinquishes practical control
 *   - downstream_innovators: Entities (including proprietary companies) who incorporate permissively-licensed code into derivative products or services; face no legal friction or reciprocity obligation
 *   - public_benefit_sector: Nonprofits, governments, educational institutions, and open-source foundations that depend on permissive licensing for low-barrier access to tooling and infrastructure
 *   - copyleft_advocates: Organizational and individual proponents of GPL and copyleft licensing who contest the reading, arguing reciprocity is necessary to prevent appropriation
 *   - academic_observer: Interdisciplinary research community tracking implementation patterns, knowledge flow, and ecosystem effects
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(permissive_license_text__commons_coordination_reading, 0.12).
domain_priors:suppression_score(permissive_license_text__commons_coordination_reading, 0.05).
domain_priors:theater_ratio(permissive_license_text__commons_coordination_reading, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(permissive_license_text__commons_coordination_reading, extractiveness, 0.12).
narrative_ontology:constraint_metric(permissive_license_text__commons_coordination_reading, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(permissive_license_text__commons_coordination_reading, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(permissive_license_text__commons_coordination_reading, accessibility_collapse, 0.15).
narrative_ontology:constraint_metric(permissive_license_text__commons_coordination_reading, resistance, 0.22).

% --- Constraint claim ---
narrative_ontology:constraint_claim(permissive_license_text__commons_coordination_reading, rope).
narrative_ontology:human_readable(permissive_license_text__commons_coordination_reading, "Permissive Open-Source License Coordination (Commons Reading)").
narrative_ontology:topic_domain(permissive_license_text__commons_coordination_reading, "software_licensing/intellectual_property/technology_governance").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(permissive_license_text__commons_coordination_reading, 'ec1d2493-9d2b-453c-9451-54a6baa1bfc5').
narrative_ontology:cs_kernel_codification('ec1d2493-9d2b-453c-9451-54a6baa1bfc5', fixed_text).
narrative_ontology:cs_authority_grounding('ec1d2493-9d2b-453c-9451-54a6baa1bfc5', practice).
narrative_ontology:cs_interpretation_layer_present('ec1d2493-9d2b-453c-9451-54a6baa1bfc5').
narrative_ontology:cs_reading_relation('ec1d2493-9d2b-453c-9451-54a6baa1bfc5', permissive_license_text__copyleft_counterfactual_reading, coexists_with).
narrative_ontology:cs_reading_relation('ec1d2493-9d2b-453c-9451-54a6baa1bfc5', permissive_license_text__corporate_moat_reading, coexists_with).
narrative_ontology:cs_axiom('ec1d2493-9d2b-453c-9451-54a6baa1bfc5', foundational, freedom_through_minimized_friction).
narrative_ontology:cs_axiom_status(freedom_through_minimized_friction, holdable).
narrative_ontology:cs_axiom_grounding('ec1d2493-9d2b-453c-9451-54a6baa1bfc5', freedom_through_minimized_friction, instrumental).
narrative_ontology:cs_axiom('ec1d2493-9d2b-453c-9451-54a6baa1bfc5', foundational, universal_implementer_access).
narrative_ontology:cs_axiom_status(universal_implementer_access, holdable).
narrative_ontology:cs_axiom_grounding('ec1d2493-9d2b-453c-9451-54a6baa1bfc5', universal_implementer_access, deontological).
narrative_ontology:cs_reference_frame('ec1d2493-9d2b-453c-9451-54a6baa1bfc5', permissive_license_as_friction_minimization).
narrative_ontology:cs_drift_state('ec1d2493-9d2b-453c-9451-54a6baa1bfc5', contemporary_appropriation_discourse, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('ec1d2493-9d2b-453c-9451-54a6baa1bfc5', '2026-08-03T14:23:15Z').
narrative_ontology:cs_kernel_id(permissive_license_text__commons_coordination_reading, permissive_license_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(permissive_license_text__commons_coordination_reading, implementer_commons).
narrative_ontology:constraint_beneficiary(permissive_license_text__commons_coordination_reading, downstream_innovators).
narrative_ontology:constraint_beneficiary(permissive_license_text__commons_coordination_reading, public_benefit_sector).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Distributed set of developers, hobbyists, researchers, and organizations worldwide who adopt permissively-licensed code, modify it, learn from it, and integrate it into their own projects. They face zero legal friction when deciding to use, fork, modify, or redistribute permissively-licensed software. If permissive licensing disappeared, they would lose the ability to collaborate without negotiating individual licenses with every copyright holder.
narrative_ontology:constraint_stakeholder(permissive_license_text__commons_coordination_reading, implementer_commons, beneficiary,
    organized, generational, mobile, global).

% Copyright holder(s) who elect to release code under a permissive license. They retain attribution rights and moral authority over the work, gain community reputation and potential contributions from others, lose practical control over how the code is used but do not surrender the copyright itself (they could license it differently to other parties if needed). They set the terms of the constraint by choosing the license.
narrative_ontology:constraint_stakeholder(permissive_license_text__commons_coordination_reading, license_grantor, agenda_setter,
    powerful, biographical, arbitrage, global).

% Entities (including proprietary software companies) who incorporate permissively-licensed code into derivative products or services. They benefit from reduced legal friction and negotiation costs. They face no obligation to share improvements, contribute back, or release their modifications — they can keep proprietary derivations entirely closed. If permissive licensing required reciprocity, their business model would require renegotiation.
narrative_ontology:constraint_stakeholder(permissive_license_text__commons_coordination_reading, downstream_innovators, beneficiary,
    powerful, biographical, arbitrage, global).

% Nonprofits, government agencies, educational institutions, and open-source foundations that depend on permissive licensing to access affordable, usable infrastructure and tooling without negotiating individual licenses. They often lack resources for per-license negotiation and rely on the ecosystem of permissively-licensed software to function.
narrative_ontology:constraint_stakeholder(permissive_license_text__commons_coordination_reading, public_benefit_sector, beneficiary,
    organized, generational, constrained, global).

% Organizations and individuals (FSF, many open-source projects, GPL advocates) who contest this reading and argue that permissive licensing without reciprocity enables appropriation and reduces net freedom. They advocate for copyleft licensing (GPL, AGPL) as the necessary alternative. They are excluded from this reading's stakeholder set because this reading does not adopt their premise; if present, they would argue that 'minimized friction' is a cover story for enabling proprietary capture.
narrative_ontology:constraint_stakeholder(permissive_license_text__commons_coordination_reading, copyleft_advocates, excluded,
    organized, generational, mobile, global).

% Interdisciplinary research community (software engineering, economics of open source, science and technology studies) that tracks implementation patterns, measures knowledge flow, documents appropriation cases, and assesses whether permissive licensing's effects match the reading's claims about freedom-maximization. They provide external measurement of whether the constraint delivers on its stated coordination function.
narrative_ontology:constraint_stakeholder(permissive_license_text__commons_coordination_reading, academic_observer, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(permissive_license_text__commons_coordination_reading, diffuse).
narrative_ontology:fixing_cost_class(permissive_license_text__commons_coordination_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Eliminates legal friction for collaborative knowledge production by establishing standard terms that signal 'use this freely, modify it, build on it, no negotiation required.' Solves the problem of per-party licensing negotiation that would otherwise prevent spontaneous collaboration at scale. Coordinates millions of independent developers around shared technical infrastructure without centralized governance.
% TRANSFER_FUNCTION: No transfer occurs in this reading. The constraint removes barriers rather than moving value. The license grantor transfers attribution and moral authority (not value) in exchange for community participation and reputation. Implementers transfer attention and labor to understanding and using the code, not wealth.
% ABSENT_VOICES: Copyleft advocates (GPL proponents, FSF, many Linux-focused projects) are structurally excluded from this reading. They would argue that permissive licensing without reciprocity enables appropriation by proprietary companies who gain value without contributing back. Their absence means the reading does not address the claim that 'minimized friction' enables corporate enclosure. Appropriated-from open-source projects (where maintainers feel their work was used unfairly by proprietary vendors) are also absent — they might argue that the freedom-through-minimized-friction framing obscures their experience of extraction.
% DISAPPEARANCE_RATIONALE: If permissive licensing disappeared and reverted to all-rights-reserved default, the ecosystem of collaborative software infrastructure would collapse. Millions of open-source projects would become inaccessible or require per-project licensing negotiation. Public-benefit sector organizations would lose affordable access to essential tooling. Downstream innovators would face negotiation costs that would slow innovation and increase prices for users. The implementer commons would fragment into siloed, proprietary projects.
% FOUNDING_PROBLEM: Early commercial software was dominated by proprietary licensing that required negotiation, licensing fees, or legal review. Collaborative knowledge production at scale was blocked by legal friction. No standard terms existed for 'use this freely and modify it freely.' The problem was: how can creators share work without giving up all control, and how can implementers collaborate without hitting licensing walls?
% FOUNDING_PROBLEM_CORROBORATION: The implementer commons and downstream innovators (from outside the copyleft tradition) attest that the founding problem is still live — negotiation friction remains a barrier when proprietary software is involved, and permissive licensing is the practical solution. Public-benefit sector organizations (nonprofits, governments, education) corroborate that licensing friction is an active constraint they face. Copyleft advocates (an external party) concur that licensing friction is real but dispute whether permissive licensing solves it adequately — they argue it solves friction but sacrifices long-term freedom. Academic research on open-source ecosystems corroborates that permissive licenses emerged as a response to negotiation burden and remain the most widely adopted category globally.
narrative_ontology:disappearance_verdict(permissive_license_text__commons_coordination_reading, world_rearranges).
narrative_ontology:founding_problem_status(permissive_license_text__commons_coordination_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(permissive_license_text__commons_coordination_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(permissive_license_text__commons_coordination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(permissive_license_text__commons_coordination_reading, 0.12, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(permissive_license_text__commons_coordination_reading_tests).
:- end_tests(permissive_license_text__commons_coordination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very low (0.12) because the constraint does not move wealth or benefit from one party to another — it removes a barrier. The grantor loses practical control but gains attribution and social capital; implementers gain access; downstream appropriators gain usable code without negotiation cost. No party is forced to surrender value for another's benefit. Suppression is minimal (0.05) because the constraint's persistence depends on voluntary choice to use the license text, not on enforcing restrictions against exit. Theater is low (0.08) because the license is functionally what it claims — a removal of friction — though some theater accrues to grantors who use permissive licensing to signal openness and community stewardship without actually ceding control over technical direction. The measurement series shows a slight rise and plateau in extractiveness (0.09 → 0.13 → 0.12) because as the ecosystem matures and appropriation examples accumulate, the reading's beneficiaries (implementer commons and public-benefit sector) face increasing pressure to argue that permissive licensing is 'being exploited' — that pressure is a small but real friction that slightly increases perceived extraction relative to the early framing. Theater and suppression remain stable because the license text itself is unchanged and the voluntary-choice basis remains.
 *
 * PERSPECTIVAL GAP:
 *   The gap between seats is relatively small for this reading — all named parties acknowledge that permissive licensing lowers barriers. The perspective difference that exists emerges from how parties FRAME the appropriation problem: the implementer commons and public-benefit sector see permissive licensing as freedom-enabling despite appropriation risks; copyleft advocates (excluded from this reading's stakeholder set) see the same permissive licensing as freedom-REDUCING because it permits enclosure. This reading does not arbitrate that dispute — it simply instantiates ONE party's reading. The engine should compute seat-level types that diverge slightly on the question of whether appropriation constitutes hidden extraction, but this reading commits to the view that it does not (appropriators gain value, but the implementer commons and grantor are not harmed in the zero-sum sense that would justify calling it extraction).
 *
 * DIRECTIONALITY LOGIC:
 *   All stakeholders are beneficiaries or neutral. The implementer commons benefits from minimized legal friction (d → 0.2, beneficiary end). Downstream innovators benefit from reduced negotiation and legal costs (d → 0.15, beneficiary end). The public-benefit sector benefits from affordable access (d → 0.25, beneficiary end). The license grantor accepts reduced control as the price of attribution and community participation (d → 0.3, slightly beneficiary). No stakeholder is targeted or forced to pay. Directionality derivation from beneficiary-only data produces low d across the board — the engine's computation should show this constraint as having no extraction-target seat, which is consistent with the rope classification.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint does not face mandatrophy questions in this reading. The founding problem — 'enabling collaborative knowledge production by removing legal friction' — remains live and directly served by the constraint's persistence. If the constraint disappeared (permissive licenses abandoned in favor of all-rights-reserved default), the implementer commons would collapse and knowledge production would fragment. The reading's classification as rope is supported by the absence of any victim set and the presence of genuine coordination benefit. The contestation from the copyleft_counterfactual_reading should not force reclassification of THIS reading — it is a different reading of the same kernel, instantiated by different parties with different premises about whether freedom requires reciprocity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_vs_copyleft_reciprocity,
    'Does permissive licensing without reciprocity requirement genuinely maximize universal implementation freedom, or does it enable appropriation by downstream proprietary projects that reduces overall ecosystem freedom?',
    'Comparative analysis of implementation breadth and pace across permissive-licensed vs. copyleft-licensed projects of comparable maturity; survey of downstream fork trajectories and proprietary derivative market capture; measurement of knowledge flow bidirectionality.',
    'If reciprocity proves necessary for preventing extraction, this reading''s core axiom (freedom_through_minimized_friction) becomes context-dependent and the constraint reclassifies toward tangled_rope (coordination with asymmetric benefit). If permissive scope remains genuinely wider despite some proprietary capture, the reading holds and the copyleft_counterfactual_reading forecloses.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_vs_copyleft_reciprocity, empirical, 'Whether permissive licensing maximizes or sacrifices net implementation freedom relative to copyleft.').

omega_variable(
    kernel_instantiation_boundary,
    'Is the reading''s legitimate axis the TEXT of the license (which parties can freely modify and claim ownership of), or the ETHOS of permissive licensing (minimized friction as a cultural norm)?',
    'Historical and contemporary cases where permissive-licensed text is incorporated into proprietary projects with license removal or restatement; analysis of which parties enforce license preservation and which do not; examination of enforcement patterns across corporate vs. community-maintained forks.',
    'If the boundary is TEXT, this reading instantiates a constraint on contractual terms; if the boundary is ETHOS, the reading instantiates a norm whose enforcement is diffuse and depends on community practice rather than legal machinery. The cs_structure authority_grounding and kernel_codification would differ accordingly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_instantiation_boundary, conceptual, 'Whether this reading''s kernel is the formal license document or the underlying ethos of minimized friction.').

omega_variable(
    universal_implementer_scope,
    'Is the implementer commons genuinely unrestricted (including hostile/appropriating parties), or is it implicitly bounded to good-faith actors who respect the license''s spirit?',
    'Documentation of appropriation cases, proprietary derivative projects, and corporate integration of permissive-licensed code; examination of how the reading''s proponents respond to license violations and incorporation without attribution.',
    'If the commons is genuinely universal and includes appropriators, the beneficiary set is broader but includes parties that extract value while contributing nothing back — the extractiveness may be understated. If the commons is implicitly bounded to cooperators, the reading''s core claim (universal freedom through minimized friction) is undermined by the need for informal gatekeeping.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(universal_implementer_scope, conceptual, 'Whether the implementer commons is truly universal or implicitly excludes appropriating parties.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(permissive_license_text__commons_coordination_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(perm_tr_t0, permissive_license_text__commons_coordination_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(perm_tr_t5, permissive_license_text__commons_coordination_reading, theater_ratio, 5, 0.06).
narrative_ontology:measurement(perm_tr_t10, permissive_license_text__commons_coordination_reading, theater_ratio, 10, 0.07).
narrative_ontology:measurement(perm_tr_t15, permissive_license_text__commons_coordination_reading, theater_ratio, 15, 0.08).
narrative_ontology:measurement(perm_tr_t20, permissive_license_text__commons_coordination_reading, theater_ratio, 20, 0.09).
narrative_ontology:measurement(perm_tr_t25, permissive_license_text__commons_coordination_reading, theater_ratio, 25, 0.08).

% Extraction over time
narrative_ontology:measurement(perm_be_t0, permissive_license_text__commons_coordination_reading, base_extractiveness, 0, 0.09).
narrative_ontology:measurement(perm_be_t5, permissive_license_text__commons_coordination_reading, base_extractiveness, 5, 0.1).
narrative_ontology:measurement(perm_be_t10, permissive_license_text__commons_coordination_reading, base_extractiveness, 10, 0.11).
narrative_ontology:measurement(perm_be_t15, permissive_license_text__commons_coordination_reading, base_extractiveness, 15, 0.12).
narrative_ontology:measurement(perm_be_t20, permissive_license_text__commons_coordination_reading, base_extractiveness, 20, 0.13).
narrative_ontology:measurement(perm_be_t25, permissive_license_text__commons_coordination_reading, base_extractiveness, 25, 0.12).

% Suppression requirement over time
narrative_ontology:measurement(perm_su_t0, permissive_license_text__commons_coordination_reading, suppression_requirement, 0, 0.03).
narrative_ontology:measurement(perm_su_t5, permissive_license_text__commons_coordination_reading, suppression_requirement, 5, 0.04).
narrative_ontology:measurement(perm_su_t10, permissive_license_text__commons_coordination_reading, suppression_requirement, 10, 0.05).
narrative_ontology:measurement(perm_su_t15, permissive_license_text__commons_coordination_reading, suppression_requirement, 15, 0.05).
narrative_ontology:measurement(perm_su_t20, permissive_license_text__commons_coordination_reading, suppression_requirement, 20, 0.06).
narrative_ontology:measurement(perm_su_t25, permissive_license_text__commons_coordination_reading, suppression_requirement, 25, 0.05).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(permissive_license_text__commons_coordination_reading, information_standard).
narrative_ontology:boltzmann_floor_override(permissive_license_text__commons_coordination_reading, 0.08).
narrative_ontology:affects_constraint(permissive_license_text__commons_coordination_reading, permissive_license_text__copyleft_counterfactual_reading).
narrative_ontology:affects_constraint(permissive_license_text__commons_coordination_reading, permissive_license_text__corporate_moat_reading).

% DUAL FORMULATION NOTE:
% Three distinct constraint stories instantiate readings of the contested kernel 'permissive_license_text.' Each reading has a different epsilon (low for commons_coordination, higher for corporate_moat, contested for copyleft_counterfactual), a different beneficiary/victim structure (none for commons, public-benefit sector benefits without victims; corporate_moat has no clear beneficiary and open-source projects as implicit victims), and different axioms about what 'freedom' means. All three are linked via network.affects_constraints to document their family relationship and the fact that the choice of reading affects classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
