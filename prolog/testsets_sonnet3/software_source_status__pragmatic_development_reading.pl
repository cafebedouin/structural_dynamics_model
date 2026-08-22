% ============================================================================
% CONSTRAINT STORY: software_source_status__pragmatic_development_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
 *   human_readable: Open Source as Superior Development Methodology (Pragmatic Efficacy Reading)
 *   domain: software engineering/political economy of technology
 *
 * SUMMARY:
 *   This story instantiates the pragmatic_development_reading of the
 *   software_source_status kernel: the claim that open development is a
 *   superior engineering methodology (more eyes catch more bugs, distributed
 *   innovation moves faster) rather than a moral imperative about freedom
 *   (freedom_imperative_reading) or a property-rights defense of restriction
 *   (property_rights_reading) or a welfare-maximizing hybrid
 *   (utilitarian_hybrid_reading). Under this reading, proprietary software is
 *   not inherently illegitimate — the argument is purely about which
 *   arrangement produces better software, and openness wins that argument
 *   instrumentally. The referent for extractiveness here is the standing
 *   arrangement under contest as this reading's own advocates would describe
 *   it: a genuinely functioning peer-review and innovation-velocity mechanism
 *   that has, in its mature corporate-sponsored form, become a vehicle for
 *   labor and value transfer from volunteer/individual contributors to firms
 *   that monetize the aggregate output under permissive licenses. The reading
 *   does not deny the coordination function is real; it also does not pretend
 *   the transfer is absent.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(software_source_status__pragmatic_development_reading, 0.32).
domain_priors:suppression_score(software_source_status__pragmatic_development_reading, 0.22).
domain_priors:theater_ratio(software_source_status__pragmatic_development_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(software_source_status__pragmatic_development_reading, extractiveness, 0.32).
narrative_ontology:constraint_metric(software_source_status__pragmatic_development_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(software_source_status__pragmatic_development_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(software_source_status__pragmatic_development_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(software_source_status__pragmatic_development_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(software_source_status__pragmatic_development_reading, tangled_rope).
narrative_ontology:human_readable(software_source_status__pragmatic_development_reading, "Open Source as Superior Development Methodology (Pragmatic Efficacy Reading)").
narrative_ontology:topic_domain(software_source_status__pragmatic_development_reading, "software engineering/political economy of technology").

domain_priors:requires_active_enforcement(software_source_status__pragmatic_development_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(software_source_status__pragmatic_development_reading, '5a82e80c-9fab-40ce-8aeb-d104ef551ed9').
narrative_ontology:cs_kernel_codification('5a82e80c-9fab-40ce-8aeb-d104ef551ed9', distributed).
narrative_ontology:cs_authority_grounding('5a82e80c-9fab-40ce-8aeb-d104ef551ed9', practice).
narrative_ontology:cs_interpretation_layer_present('5a82e80c-9fab-40ce-8aeb-d104ef551ed9').
narrative_ontology:cs_reading_relation('5a82e80c-9fab-40ce-8aeb-d104ef551ed9', software_source_status__freedom_imperative_reading, coexists_with).
narrative_ontology:cs_reading_relation('5a82e80c-9fab-40ce-8aeb-d104ef551ed9', software_source_status__property_rights_reading, coexists_with).
narrative_ontology:cs_reading_relation('5a82e80c-9fab-40ce-8aeb-d104ef551ed9', software_source_status__utilitarian_hybrid_reading, influences).
narrative_ontology:cs_axiom('5a82e80c-9fab-40ce-8aeb-d104ef551ed9', foundational, freedom_is_instrumental_not_terminal).
narrative_ontology:cs_axiom_status(freedom_is_instrumental_not_terminal, holdable).
narrative_ontology:cs_axiom_grounding('5a82e80c-9fab-40ce-8aeb-d104ef551ed9', freedom_is_instrumental_not_terminal, instrumental).
narrative_ontology:cs_axiom('5a82e80c-9fab-40ce-8aeb-d104ef551ed9', foundational, proprietary_models_not_inherently_illegitimate).
narrative_ontology:cs_axiom_status(proprietary_models_not_inherently_illegitimate, holdable).
narrative_ontology:cs_axiom_grounding('5a82e80c-9fab-40ce-8aeb-d104ef551ed9', proprietary_models_not_inherently_illegitimate, empirically_contingent).
narrative_ontology:cs_reference_frame('5a82e80c-9fab-40ce-8aeb-d104ef551ed9', engineering_efficacy_consensus).
narrative_ontology:cs_drift_state('5a82e80c-9fab-40ce-8aeb-d104ef551ed9', post_corporate_capture_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('5a82e80c-9fab-40ce-8aeb-d104ef551ed9', '').
narrative_ontology:cs_kernel_id(software_source_status__pragmatic_development_reading, software_source_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(software_source_status__pragmatic_development_reading, corporate_open_source_maintainers).
narrative_ontology:constraint_beneficiary(software_source_status__pragmatic_development_reading, downstream_enterprise_integrators).
narrative_ontology:constraint_beneficiary(software_source_status__pragmatic_development_reading, permissive_license_platform_vendors).
narrative_ontology:constraint_victim(software_source_status__pragmatic_development_reading, unpaid_volunteer_maintainers).
narrative_ontology:constraint_victim(software_source_status__pragmatic_development_reading, small_proprietary_competitors).
narrative_ontology:constraint_victim(software_source_status__pragmatic_development_reading, contributors_whose_code_is_relicensed).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Large firms fund core maintainers, steer roadmaps, and harvest community contributions under permissive licenses (MIT/Apache/BSD) that let them fold improvements into proprietary products without reciprocal disclosure obligations. They champion 'open source is just better engineering' as the framing that legitimizes extracting free labor while retaining freedom to close derivative works.
narrative_ontology:constraint_stakeholder(software_source_status__pragmatic_development_reading, corporate_open_source_maintainers, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(software_source_status__pragmatic_development_reading, corporate_open_source_maintainers, agenda_setter).

% Do the bulk of triage, security patching, and documentation for critical infrastructure projects, often uncompensated, while corporate consumers of the same code capture most downstream commercial value. Burnout is endemic; walking away means abandoning a project users depend on, so exit carries reputational and continuity costs the corporate beneficiaries do not bear.
narrative_ontology:constraint_stakeholder(software_source_status__pragmatic_development_reading, unpaid_volunteer_maintainers, payer,
    powerless, biographical, trapped, global).

% Consume permissively licensed components as free, high-quality inputs to commercial products, relying on the community's peer review and bug-detection velocity to reduce their own QA costs. They can switch between competing open projects freely and bear no obligation to fund upstream maintenance.
narrative_ontology:constraint_stakeholder(software_source_status__pragmatic_development_reading, downstream_enterprise_integrators, beneficiary,
    organized, biographical, mobile, global).

% Compete against well-resourced firms that ship open-core or open-source-adjacent products marketed as methodologically superior, undercutting proprietary vendors on perceived trust and velocity grounds even when the proprietary offering has comparable or better engineering. Their market narrows as procurement processes increasingly treat 'open source' status as a quality proxy rather than evaluating the code itself.
narrative_ontology:constraint_stakeholder(software_source_status__pragmatic_development_reading, small_proprietary_competitors, payer,
    moderate, biographical, constrained, national).

% Contributed code under an assumption of open reciprocity; later find the project relicensed, forked into a commercial SaaS offering, or absorbed into a corporate-controlled foundation whose governance they cannot influence. Legal recourse is minimal since the original permissive license authorized exactly this use.
narrative_ontology:constraint_stakeholder(software_source_status__pragmatic_development_reading, contributors_whose_code_is_relicensed, payer,
    powerless, biographical, trapped, global).

% Build cloud platforms and managed services on top of community-maintained open source, monetizing hosting and support while contributing a small fraction of engineering effort back. Their business model depends on the 'better methodology' narrative attracting a steady contributor pipeline that does not demand compensation.
narrative_ontology:constraint_stakeholder(software_source_status__pragmatic_development_reading, permissive_license_platform_vendors, beneficiary,
    institutional, generational, arbitrage, global).

% Study defect rates, time-to-patch, and innovation velocity across open and closed codebases to test the empirical claim that openness improves quality. Their findings are cited selectively by all parties to the kernel contest, regardless of study quality.
narrative_ontology:constraint_stakeholder(software_source_status__pragmatic_development_reading, software_engineering_researchers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(software_source_status__pragmatic_development_reading, corporate_open_source_maintainers).
narrative_ontology:fixing_cost_class(software_source_status__pragmatic_development_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Distributed peer review across many independent eyes genuinely catches classes of defects that small closed teams miss, and open collaboration genuinely accelerates certain kinds of innovation (protocol standards, infrastructure tooling) by removing duplication and enabling parallel experimentation across organizations.
% TRANSFER_FUNCTION: Moves engineering labor from volunteer and community contributors to the balance sheets of firms that redistribute the resulting software as commercial products or services, without a reciprocal transfer of revenue or governance control back to the contributing labor pool.
% ABSENT_VOICES: Individual contributors who joined for reciprocity, learning, or reputation and did not anticipate their work subsidizing a firm's proprietary derivative are rarely present when licensing terms or governance structures are set by foundations dominated by corporate members.
% DISAPPEARANCE_RATIONALE: If the 'open source is methodologically superior' framing disappeared, corporate beneficiaries argue critical infrastructure quality would degrade without distributed review; volunteer maintainers and displaced proprietary competitors argue the framing mainly serves to naturalize a labor-and-value transfer that would otherwise require explicit negotiation — the world would rearrange procurement and licensing norms substantially, but whether engineering quality itself would suffer is precisely what is contested.
% FOUNDING_PROBLEM: Early proprietary software development was hampered by duplicated effort, hidden defects that vendors had no incentive to disclose, and vendor lock-in that limited interoperability and long-term maintainability.
% FOUNDING_PROBLEM_CORROBORATION: Independent empirical software-engineering studies (outside both corporate beneficiaries and volunteer-maintainer advocacy groups) find mixed evidence: openness correlates with faster defect discovery in high-traffic projects but shows no consistent quality advantage in low-traffic ones — corroboration for the founding problem's continued relevance is genuine but partial, and no fully disinterested party has settled whether the methodology claim as currently invoked describes engineering reality or functions as market rhetoric.
narrative_ontology:disappearance_verdict(software_source_status__pragmatic_development_reading, contested).
narrative_ontology:founding_problem_status(software_source_status__pragmatic_development_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(software_source_status__pragmatic_development_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(software_source_status__pragmatic_development_reading, 'none', 1).
narrative_ontology:epsilon_provenance(software_source_status__pragmatic_development_reading, 0.32, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness (0.32) and suppression (0.22) are both moderate rather than severe because the pragmatic reading, unlike the freedom_imperative reading, has no ideological machinery forcing participation — contributors can and do choose closed alternatives, and the coordination benefit (peer review, defect detection, velocity) is empirically real enough that many stay by genuine preference, not coercion. Theater ratio rises modestly over the interval (0.10 to 0.28) as foundation governance structures around major projects increasingly perform community stewardship while corporate board seats concentrate actual control. Accessibility collapse (0.35) and resistance (0.4) reflect that proprietary alternatives remain viable and are actively chosen by some actors — this is not a mountain-grade collapse of alternatives.
 *
 * PERSPECTIVAL GAP:
 *   From the corporate beneficiary seat, this looks like a well-functioning rope: a voluntary, efficient coordination mechanism that happens to also generate commercial value downstream. From the unpaid volunteer maintainer seat, the same structure computes as extraction requiring active governance enforcement (foundation bylaws, license terms, board composition) to keep functioning in the beneficiaries' favor. The engine should register this divergence structurally rather than resolve it — the pragmatic reading's own claim to be 'just better engineering' is precisely what obscures the transfer from the beneficiary seat's point of view.
 *
 * DIRECTIONALITY LOGIC:
 *   Corporate maintainers and platform vendors sit near the beneficiary end: they set governance terms, capture commercial value from permissively licensed contributions, and retain arbitrage-grade exit (they can fork, relicense within permission, or abandon a project without losing their own commercial position). Volunteer maintainers and relicensed contributors sit near the target end: trapped exit (abandoning a project they built carries reputational cost and harms downstream users who depend on it), and the value they create flows disproportionately to parties who did not create it. Small proprietary competitors are targets of a different kind — not of the coordination mechanism directly but of the market narrative that treats 'open source methodology' as a quality signal regardless of actual code quality.
 *
 * MANDATROPHY ANALYSIS:
 *   The pragmatic reading resists mandatrophy in one direction and risks it in another: because the founding problem (duplicated effort, undisclosed defects, vendor lock-in) remains partially live per independent research, the coordination function has not fully evaporated — this blocks a clean 'world_unchanged' verdict. But the reading's own efficacy claim is increasingly doing rhetorical work (justifying continued uncompensated labor extraction) independent of whether the empirical quality advantage still holds in any given project, which is exactly the tangled-rope signature: real coordination function, real asymmetric extraction, both riding the same structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    empirical_quality_claim_robustness,
    'Does open development actually produce measurably superior software quality (defect rates, security response time, maintainability) across project types, or does the advantage hold only for high-traffic, well-resourced projects while collapsing for the median open source project?',
    'Large-scale comparative empirical studies stratifying by project size, funding level, and contributor count, rather than studies that sample only prominent successful projects (survivorship bias in existing literature).',
    'If the quality advantage is real and general, the pragmatic reading''s coordination-function claim is well-grounded and the extraction component is a genuine byproduct rather than the point. If the advantage holds only for a subset of well-resourced projects, the methodology claim functions largely as post-hoc justification for an arrangement whose real driver is labor cost externalization.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(empirical_quality_claim_robustness, empirical, 'Whether the core efficacy claim generalizes or is survivorship-biased.').

omega_variable(
    reading_selection_stability,
    'Is the pragmatic_development_reading a stable, independently-held position, or does it function primarily as the framing corporate beneficiaries deploy when the freedom_imperative_reading''s moral claims would be inconvenient (e.g., when a firm wants to relicense or restrict without ethical objection)?',
    'Track which reading a given institutional actor invokes across different contexts (marketing materials, license negotiations, contributor agreements) — consistent invocation regardless of strategic benefit would support genuine independent commitment; context-dependent switching would support the framing-of-convenience hypothesis.',
    'If the reading is strategically selected rather than genuinely held, the tangled_rope classification understates the extraction: the ''methodology'' framing is then better read as cover for whichever reading is locally advantageous, which is closer to snare dynamics dressed in pragmatic language.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_selection_stability, conceptual, 'Whether this reading is independently held or opportunistically deployed alongside its siblings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(software_source_status__pragmatic_development_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(soft_tr_t0, software_source_status__pragmatic_development_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(soft_tr_t5, software_source_status__pragmatic_development_reading, theater_ratio, 5, 0.14).
narrative_ontology:measurement(soft_tr_t10, software_source_status__pragmatic_development_reading, theater_ratio, 10, 0.18).
narrative_ontology:measurement(soft_tr_t15, software_source_status__pragmatic_development_reading, theater_ratio, 15, 0.21).
narrative_ontology:measurement(soft_tr_t20, software_source_status__pragmatic_development_reading, theater_ratio, 20, 0.25).
narrative_ontology:measurement(soft_tr_t25, software_source_status__pragmatic_development_reading, theater_ratio, 25, 0.28).

% Extraction over time
narrative_ontology:measurement(soft_be_t0, software_source_status__pragmatic_development_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(soft_be_t5, software_source_status__pragmatic_development_reading, base_extractiveness, 5, 0.22).
narrative_ontology:measurement(soft_be_t10, software_source_status__pragmatic_development_reading, base_extractiveness, 10, 0.26).
narrative_ontology:measurement(soft_be_t15, software_source_status__pragmatic_development_reading, base_extractiveness, 15, 0.28).
narrative_ontology:measurement(soft_be_t20, software_source_status__pragmatic_development_reading, base_extractiveness, 20, 0.3).
narrative_ontology:measurement(soft_be_t25, software_source_status__pragmatic_development_reading, base_extractiveness, 25, 0.32).

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
% This story is one of four constraints decomposed from the colloquial 'open source vs. proprietary software' debate, per the ε-invariance principle: the natural-language label conflates a moral claim (freedom_imperative_reading), a property claim (property_rights_reading), an efficacy claim (this story, pragmatic_development_reading), and a welfare-optimization claim (utilitarian_hybrid_reading). Each reading has a different epsilon, a different beneficiary/victim structure, and different persistence conditions. The pragmatic reading here shows moderate, rising extractiveness (0.32) driven by labor-value transfer under permissive licensing, distinct from what a moral-imperative reading would show (which would likely show low epsilon for the arrangement it endorses and high epsilon for the proprietary status quo it opposes) or a property-rights reading would show (which would treat restriction itself as the legitimate baseline, inverting the beneficiary/victim structure).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
