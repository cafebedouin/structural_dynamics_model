% ============================================================================
% CONSTRAINT STORY: software_source_status__pragmatic_development_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_software_source_status__pragmatic_development_reading, []).

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
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: software_source_status__pragmatic_development_reading
 *   human_readable: Open Source as Superior Engineering Methodology (Pragmatic/Developmental Reading)
 *   domain: software_engineering/political_economy_of_technology
 *
 * SUMMARY:
 *   This story instantiates the pragmatic-development reading of the
 *   software_source_status kernel: the claim that open source's value lies in
 *   engineering outcomes (peer review, bug detection, innovation velocity)
 *   rather than in any ethical duty to preserve user freedom. Under this
 *   reading, proprietary software is not inherently illegitimate and
 *   permissive licensing is fully acceptable, since the reading's
 *   justificatory core is instrumental quality, not moral obligation. This is
 *   a distinct constraint from the freedom_imperative_reading (which treats
 *   proprietary software as an injustice), the property_rights_reading (which
 *   grounds legitimacy in creator ownership), and the
 *   utilitarian_hybrid_reading (which weighs aggregate welfare across
 *   contexts) — each of those is authored as its own sibling story with its
 *   own ε, beneficiary/victim structure, and classification. Only this
 *   reading's structure is analyzed here.
 *
 * KEY AGENTS:
 *   - corporate_open_source_maintainers: institutional agenda-setters who fund and frame openness as an engineering practice
 *   - downstream_integrators and permissive_license_adopters: organized beneficiaries who capture quality benefits without freedom obligations
 *   - unpaid_volunteer_maintainers: powerless payers who absorb uncompensated maintenance labor
 *   - small_proprietary_competitors: moderate-power payers squeezed by the higher engineering bar open development sets
 *   - empirical_software_engineering_researchers: analytical observers whose findings are the only evidence that could vindicate or falsify the reading's instrumental claim
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(software_source_status__pragmatic_development_reading, 0.28).
domain_priors:suppression_score(software_source_status__pragmatic_development_reading, 0.22).
domain_priors:theater_ratio(software_source_status__pragmatic_development_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(software_source_status__pragmatic_development_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(software_source_status__pragmatic_development_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(software_source_status__pragmatic_development_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(software_source_status__pragmatic_development_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(software_source_status__pragmatic_development_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(software_source_status__pragmatic_development_reading, rope).
narrative_ontology:human_readable(software_source_status__pragmatic_development_reading, "Open Source as Superior Engineering Methodology (Pragmatic/Developmental Reading)").
narrative_ontology:topic_domain(software_source_status__pragmatic_development_reading, "software_engineering/political_economy_of_technology").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(software_source_status__pragmatic_development_reading, '53c284e1-16ee-4864-8323-409f93890a60').
narrative_ontology:cs_kernel_codification('53c284e1-16ee-4864-8323-409f93890a60', distributed).
narrative_ontology:cs_authority_grounding('53c284e1-16ee-4864-8323-409f93890a60', practice).
narrative_ontology:cs_interpretation_layer_present('53c284e1-16ee-4864-8323-409f93890a60').
narrative_ontology:cs_reading_relation('53c284e1-16ee-4864-8323-409f93890a60', software_source_status__freedom_imperative_reading, coexists_with).
narrative_ontology:cs_reading_relation('53c284e1-16ee-4864-8323-409f93890a60', software_source_status__property_rights_reading, coexists_with).
narrative_ontology:cs_reading_relation('53c284e1-16ee-4864-8323-409f93890a60', software_source_status__utilitarian_hybrid_reading, influences).
narrative_ontology:cs_axiom('53c284e1-16ee-4864-8323-409f93890a60', foundational, freedom_is_instrumental_not_terminal).
narrative_ontology:cs_axiom_status(freedom_is_instrumental_not_terminal, holdable).
narrative_ontology:cs_axiom_grounding('53c284e1-16ee-4864-8323-409f93890a60', freedom_is_instrumental_not_terminal, instrumental).
narrative_ontology:cs_axiom('53c284e1-16ee-4864-8323-409f93890a60', foundational, proprietary_software_not_inherently_illegitimate).
narrative_ontology:cs_axiom_status(proprietary_software_not_inherently_illegitimate, holdable).
narrative_ontology:cs_axiom_grounding('53c284e1-16ee-4864-8323-409f93890a60', proprietary_software_not_inherently_illegitimate, empirically_contingent).
narrative_ontology:cs_reference_frame('53c284e1-16ee-4864-8323-409f93890a60', engineering_meritocracy_of_review).
narrative_ontology:cs_drift_state('53c284e1-16ee-4864-8323-409f93890a60', post_corporate_capture_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('53c284e1-16ee-4864-8323-409f93890a60', '').
narrative_ontology:cs_kernel_id(software_source_status__pragmatic_development_reading, software_source_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(software_source_status__pragmatic_development_reading, corporate_open_source_maintainers).
narrative_ontology:constraint_beneficiary(software_source_status__pragmatic_development_reading, downstream_integrators).
narrative_ontology:constraint_beneficiary(software_source_status__pragmatic_development_reading, security_researchers).
narrative_ontology:constraint_beneficiary(software_source_status__pragmatic_development_reading, permissive_license_adopters).
narrative_ontology:constraint_victim(software_source_status__pragmatic_development_reading, unpaid_volunteer_maintainers).
narrative_ontology:constraint_victim(software_source_status__pragmatic_development_reading, small_proprietary_competitors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Large firms fund and steer widely-used open source projects, framing openness as a quality-engineering practice (peer review catches bugs, external contributions accelerate velocity) rather than an ethical mandate. They capture reputational, hiring, and platform-control benefits from participation while retaining the freedom to build proprietary layers on top under permissive licenses.
narrative_ontology:constraint_stakeholder(software_source_status__pragmatic_development_reading, corporate_open_source_maintainers, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(software_source_status__pragmatic_development_reading, corporate_open_source_maintainers, beneficiary).

% Companies and developers who consume open source components to build products faster and more reliably than they could in isolation. They benefit from the methodological argument directly: they get audited, battle-tested code without needing to accept any obligation to release their own derivative work.
narrative_ontology:constraint_stakeholder(software_source_status__pragmatic_development_reading, downstream_integrators, beneficiary,
    organized, biographical, mobile, global).

% Independent and institutional researchers who rely on source visibility to find and responsibly disclose vulnerabilities. The pragmatic framing (openness improves quality via review) legitimizes and normalizes their access without requiring any ideological commitment to software freedom.
narrative_ontology:constraint_stakeholder(software_source_status__pragmatic_development_reading, security_researchers, beneficiary,
    moderate, biographical, mobile, global).

% Developers and firms who use MIT/BSD/Apache-style licenses specifically because this reading treats permissive terms as compatible with 'real' open source, letting them relicense, close, or commercialize derivatives while still claiming the reputational and quality benefits of an open development process.
narrative_ontology:constraint_stakeholder(software_source_status__pragmatic_development_reading, permissive_license_adopters, beneficiary,
    organized, biographical, arbitrage, global).

% Individuals who do the unglamorous maintenance work (triage, security patching, documentation) that the 'more eyeballs make bugs shallow' argument assumes is either abundant or fairly compensated. In practice they absorb burnout, liability exposure, and uncompensated labor that corporate consumers of the methodology do not share, because exit means the project (and their reputation investment in it) collapses or is forked away from them.
narrative_ontology:constraint_stakeholder(software_source_status__pragmatic_development_reading, unpaid_volunteer_maintainers, payer,
    powerless, biographical, trapped, global).

% Smaller software vendors who cannot match the velocity and review capacity that well-resourced firms achieve by commanding large open contributor pools. The 'open is simply better engineering' claim raises the competitive bar in a way that favors incumbents able to subsidize open development, squeezing vendors who must fund closed development entirely from license revenue.
narrative_ontology:constraint_stakeholder(software_source_status__pragmatic_development_reading, small_proprietary_competitors, payer,
    moderate, biographical, constrained, national).

% Advocates of the sibling ethical-freedom reading who argue that reducing openness to an engineering-quality argument strips it of moral force and enables permissive-license capture by proprietary interests. They are not part of the pragmatic reading's own justificatory framework and their objection surfaces mainly in license debates, not in this reading's day-to-day operation.
narrative_ontology:constraint_stakeholder(software_source_status__pragmatic_development_reading, software_freedom_advocates, excluded,
    organized, civilizational, constrained, global).

% Academics who study whether open development actually correlates with higher code quality, faster defect resolution, or better security outcomes, independent of the ideological stakes. Their empirical findings are the only evidence that could vindicate or falsify this reading's central instrumental claim.
narrative_ontology:constraint_stakeholder(software_source_status__pragmatic_development_reading, empirical_software_engineering_researchers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(software_source_status__pragmatic_development_reading, corporate_open_source_maintainers).
narrative_ontology:fixing_cost_class(software_source_status__pragmatic_development_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Distributed peer review, parallel bug discovery, and low-friction reuse of vetted components solve a genuine engineering coordination problem: no single team can review, test, and harden code as thoroughly or as fast as a large, diverse contributor base can.
% TRANSFER_FUNCTION: Moves unpaid or under-compensated maintenance labor, security triage burden, and community-management overhead from well-resourced firms (who capture the resulting code quality, hiring pipeline, and reputational capital) onto volunteer maintainers and, secondarily, onto smaller vendors who cannot fund equivalent development capacity from closed revenue alone.
% ABSENT_VOICES: Software freedom advocates who would object that the pragmatic framing evacuates openness of ethical content and thereby licenses corporate capture of the open commons; they are not represented within this reading's own justificatory apparatus, which explicitly treats freedom as instrumental rather than terminal.
% DISAPPEARANCE_RATIONALE: If the pragmatic-methodology justification vanished, corporate practice around open source contribution would likely persist largely unchanged in the short term (firms already capture the practical benefits regardless of the stated rationale), but the legitimating narrative that lets permissive licensing coexist comfortably with heavy corporate extraction would weaken, potentially strengthening the freedom-imperative reading's claim on the same practices — hence contested rather than a clean verdict.
% FOUNDING_PROBLEM: Early software development suffered from duplicated effort, undiscovered defects in closed codebases, and slow diffusion of fixes; making source visible and modifiable was proposed as a practical engineering discipline to accelerate review, reuse, and hardening.
% FOUNDING_PROBLEM_CORROBORATION: Empirical software engineering researchers (outside the corporate beneficiary set) have produced peer-reviewed studies correlating open review processes with certain defect-detection and patch-velocity outcomes, though the magnitude and universality of the effect remain actively debated in that same literature — corroboration exists but is qualified, not unanimous.
narrative_ontology:disappearance_verdict(software_source_status__pragmatic_development_reading, contested).
narrative_ontology:founding_problem_status(software_source_status__pragmatic_development_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(software_source_status__pragmatic_development_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(software_source_status__pragmatic_development_reading, 'none', 1).
narrative_ontology:epsilon_provenance(software_source_status__pragmatic_development_reading, 0.28, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(software_source_status__pragmatic_development_reading_tests).
:- end_tests(software_source_status__pragmatic_development_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low-to-moderate (0.28 at interval end) because the reading's coordination function — distributed review genuinely improving code quality — is real and widely corroborated, but a growing share of the practical benefit accrues asymmetrically to well-resourced corporate consumers rather than to the volunteer labor producing it. Suppression is low (0.22): no one is coerced into open development under this reading, and proprietary alternatives remain fully legitimate by its own terms — this is what most sharply distinguishes it from the freedom_imperative_reading, which would suppress the proprietary alternative as illegitimate. Theater ratio rises modestly over the interval (0.10 to 0.30) as 'open-source-friendly' branding by firms increasingly substitutes performative community engagement for genuine investment in maintainer sustainability.
 *
 * DIRECTIONALITY LOGIC:
 *   Corporate maintainers and permissive-license adopters sit near the beneficiary end: they set norms, capture reputational and hiring value, and retain full freedom to build closed derivatives. Downstream integrators and security researchers are moderate beneficiaries who consume the coordination good without bearing its cost. Unpaid volunteer maintainers sit near the target end — trapped exit options (leaving means losing accumulated reputational capital or watching the project be forked without them) and the party actually absorbing the uncompensated labor the instrumental argument assumes will simply appear. Small proprietary competitors are secondary targets: the reading's own success (open development produces better software) raises the competitive floor in a way that specifically disadvantages closed-development shops that cannot draw on volunteer or subsidized-corporate labor pools.
 *
 * MANDATROPHY ANALYSIS:
 *   The pragmatic reading resists mandatrophy misclassification by keeping the founding problem (duplicated effort, undiscovered defects, slow fix diffusion) explicitly live and testable via ongoing empirical software-engineering research, rather than resting on an untestable ethical premise. This is precisely what differentiates it from a scaffold or snare: it does not claim its own obsolescence (no sunset clause is appropriate, since better software is a persistent, not transitional, need), and its extraction is diffuse and moderate rather than concentrated and coercive — closer to a genuine, if imperfect, coordination mechanism than to organized rent extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    instrumental_claim_empirical_status,
    'Does open development methodology actually produce measurably superior software quality, security, and innovation velocity compared to well-resourced closed development, or does the apparent advantage reduce to resource asymmetry (large corporate contributor pools) rather than openness per se?',
    'Controlled or quasi-experimental comparisons of matched open and closed projects controlling for funding and team size; longitudinal defect-density and patch-latency studies across licensing models.',
    'If the quality advantage is substantially attributable to funding/scale rather than openness, this reading''s foundational instrumental claim weakens and the constraint drifts toward a corporate-legitimation function (tangled_rope) rather than genuine coordination (rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(instrumental_claim_empirical_status, empirical, 'Whether openness itself, versus resourcing, drives the claimed quality benefits.').

omega_variable(
    kernel_reading_boundary_location,
    'Where exactly does the pragmatic_development_reading''s boundary sit relative to the utilitarian_hybrid_reading — is ''freedom is instrumental to quality'' meaningfully distinct from a welfare-maximization calculus that happens to favor openness in most technical contexts, or is this reading simply the utilitarian_hybrid_reading applied narrowly to engineering outcomes?',
    'Compare the two readings'' treatment of a case where quality and aggregate welfare diverge (e.g., open development that improves code quality but harms a vulnerable user population through a different channel) — the readings should prescribe differently if truly distinct.',
    'If the readings converge on all practical cases, they may warrant merging or explicit subsumption rather than separate constraint stories; if they diverge, the decomposition is validated.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_boundary_location, conceptual, 'Whether this reading is genuinely distinct from the utilitarian_hybrid sibling or a narrow special case of it.').

omega_variable(
    volunteer_labor_capture_ambiguity,
    'Is the uncompensated labor burden on volunteer maintainers a structural feature necessary to the coordination benefit (review requires many contributors, most unpaid by construction) or an extractive externality that corporate beneficiaries could remedy through funding without losing the coordination benefit?',
    'Track outcomes at open source foundations and firms that have implemented paid-maintainer programs (e.g., GitHub Sponsors uptake, corporate maintainer funding initiatives) against burnout and turnover rates in unfunded comparable projects.',
    'If remediable without losing coordination value, the current extraction level is closer to avoidable rent than necessary coordination cost, pushing the classification toward tangled_rope; if structurally necessary, the rope classification is better supported.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(volunteer_labor_capture_ambiguity, empirical, 'Whether maintainer labor extraction is structurally necessary or a remediable externality.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(software_source_status__pragmatic_development_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(soft_tr_t0, software_source_status__pragmatic_development_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(soft_tr_t6, software_source_status__pragmatic_development_reading, theater_ratio, 6, 0.14).
narrative_ontology:measurement(soft_tr_t12, software_source_status__pragmatic_development_reading, theater_ratio, 12, 0.18).
narrative_ontology:measurement(soft_tr_t18, software_source_status__pragmatic_development_reading, theater_ratio, 18, 0.22).
narrative_ontology:measurement(soft_tr_t24, software_source_status__pragmatic_development_reading, theater_ratio, 24, 0.26).
narrative_ontology:measurement(soft_tr_t30, software_source_status__pragmatic_development_reading, theater_ratio, 30, 0.3).

% Extraction over time
narrative_ontology:measurement(soft_be_t0, software_source_status__pragmatic_development_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(soft_be_t6, software_source_status__pragmatic_development_reading, base_extractiveness, 6, 0.18).
narrative_ontology:measurement(soft_be_t12, software_source_status__pragmatic_development_reading, base_extractiveness, 12, 0.21).
narrative_ontology:measurement(soft_be_t18, software_source_status__pragmatic_development_reading, base_extractiveness, 18, 0.24).
narrative_ontology:measurement(soft_be_t24, software_source_status__pragmatic_development_reading, base_extractiveness, 24, 0.26).
narrative_ontology:measurement(soft_be_t30, software_source_status__pragmatic_development_reading, base_extractiveness, 30, 0.28).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(software_source_status__pragmatic_development_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(software_source_status__pragmatic_development_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(software_source_status__pragmatic_development_reading, 0.12).
narrative_ontology:affects_constraint(software_source_status__pragmatic_development_reading, software_source_status__freedom_imperative_reading).
narrative_ontology:affects_constraint(software_source_status__pragmatic_development_reading, software_source_status__property_rights_reading).
narrative_ontology:affects_constraint(software_source_status__pragmatic_development_reading, software_source_status__utilitarian_hybrid_reading).

% DUAL FORMULATION NOTE:
% This story is one of four constraints decomposing the natural-language 'open source vs. proprietary software' debate under the software_source_status kernel. Each reading has a distinct ε, beneficiary/victim structure, and classification: freedom_imperative_reading treats proprietary software as ethically illegitimate (higher suppression, moral framing); property_rights_reading grounds legitimacy in ownership (different beneficiary set — creators/rightsholders); utilitarian_hybrid_reading evaluates context-dependently (no fixed claimed_type across contexts, by design). This reading (pragmatic_development) is distinguished by treating freedom as strictly instrumental to quality outcomes, which is the narrowest and most empirically falsifiable of the four.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
