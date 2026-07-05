% ============================================================================
% CONSTRAINT STORY: gpl_copyleft_scope__narrow_scope_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gpl_copyleft_scope__narrow_scope_reading, []).

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
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: gpl_copyleft_scope__narrow_scope_reading
 *   human_readable: GPL Section 2(b) — Narrow Derivative-Work-Boundary Reading
 *   domain: software_licensing/intellectual_property/open_source_governance
 *
 * SUMMARY:
 *   This story instantiates the NARROW-SCOPE reading of the GPL Section 2(b)
 *   derivative-work boundary: the license constrains only direct derivative
 *   works under traditional copyright doctrine, and mere aggregation, plugin
 *   architectures interacting through defined APIs, and many dynamic linking
 *   patterns fall outside its reach. Under this reading, GPL functions as a
 *   moderate-epsilon coordination mechanism enabling mixed
 *   proprietary/copyleft codebases — commercial firms integrate GPL
 *   components with proprietary layers by architecting around the narrow
 *   line, while copyleft advocates' broader expectations of universal
 *   downstream sharing go structurally unmet. This is a distinct constraint
 *   from the strong-copyleft reading (which reads the same clause as
 *   capturing all forms of code coupling) and from the enforcement-vacuum
 *   reading (which holds that absent judicial precedent, the effective
 *   constraint is whichever community has enforcement capacity in context).
 *   Each reading has a different epsilon, a different beneficiary/victim
 *   structure, and is authored as its own story per the epsilon-invariance
 *   principle; they are linked here only via network edges and
 *   cs_structure.reading_relations, not folded together.
 *
 * KEY AGENTS:
 *   - commercial_integrators: primary beneficiary (powerful/mobile) — captures architectural flexibility
 *   - proprietary_plugin_vendors: secondary beneficiary (organized/mobile) — monetizes interoperation
 *   - dual_licensing_vendors: beneficiary and agenda-setter (organized/arbitrage) — commercially promotes and profits from this reading
 *   - fsf_aligned_maintainers: primary payer (moderate/constrained) — bears uncompensated reciprocity loss
 *   - downstream_gpl_contributors: diffuse payer (powerless/constrained) — donated labor diluted
 *   - copyleft_advocacy_organizations: excluded voice (organized/trapped) — preferred reading structurally sidelined
 *   - courts_and_legal_scholars: analytical observer (institutional/analytical) — adjudicates sparingly, leaving ambiguity live
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gpl_copyleft_scope__narrow_scope_reading, 0.32).
domain_priors:suppression_score(gpl_copyleft_scope__narrow_scope_reading, 0.22).
domain_priors:theater_ratio(gpl_copyleft_scope__narrow_scope_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gpl_copyleft_scope__narrow_scope_reading, extractiveness, 0.32).
narrative_ontology:constraint_metric(gpl_copyleft_scope__narrow_scope_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(gpl_copyleft_scope__narrow_scope_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gpl_copyleft_scope__narrow_scope_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(gpl_copyleft_scope__narrow_scope_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gpl_copyleft_scope__narrow_scope_reading, rope).
narrative_ontology:human_readable(gpl_copyleft_scope__narrow_scope_reading, "GPL Section 2(b) — Narrow Derivative-Work-Boundary Reading").
narrative_ontology:topic_domain(gpl_copyleft_scope__narrow_scope_reading, "software_licensing/intellectual_property/open_source_governance").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gpl_copyleft_scope__narrow_scope_reading, 'c1fe9355-f372-43a5-ab5a-bbed49bee9f0').
narrative_ontology:cs_kernel_codification('c1fe9355-f372-43a5-ab5a-bbed49bee9f0', fixed_text).
narrative_ontology:cs_authority_grounding('c1fe9355-f372-43a5-ab5a-bbed49bee9f0', distributed).
narrative_ontology:cs_reading_relation('c1fe9355-f372-43a5-ab5a-bbed49bee9f0', gpl_copyleft_scope__strong_copyleft_reading, coexists_with).
narrative_ontology:cs_reading_relation('c1fe9355-f372-43a5-ab5a-bbed49bee9f0', gpl_copyleft_scope__enforcement_vacuum_reading, influences).
narrative_ontology:cs_axiom('c1fe9355-f372-43a5-ab5a-bbed49bee9f0', foundational, derivative_work_boundary_follows_general_copyright_doctrine).
narrative_ontology:cs_axiom_status(derivative_work_boundary_follows_general_copyright_doctrine, holdable).
narrative_ontology:cs_axiom_grounding('c1fe9355-f372-43a5-ab5a-bbed49bee9f0', derivative_work_boundary_follows_general_copyright_doctrine, conventional).
narrative_ontology:cs_axiom('c1fe9355-f372-43a5-ab5a-bbed49bee9f0', secondary, functional_separability_defeats_combined_work_status).
narrative_ontology:cs_axiom_status(functional_separability_defeats_combined_work_status, holdable).
narrative_ontology:cs_axiom_grounding('c1fe9355-f372-43a5-ab5a-bbed49bee9f0', functional_separability_defeats_combined_work_status, empirically_contingent).
narrative_ontology:cs_reference_frame('c1fe9355-f372-43a5-ab5a-bbed49bee9f0', traditional_copyright_derivative_work_doctrine).
narrative_ontology:cs_drift_state('c1fe9355-f372-43a5-ab5a-bbed49bee9f0', contemporary_saas_and_dynamic_linking_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('c1fe9355-f372-43a5-ab5a-bbed49bee9f0', '').
narrative_ontology:cs_kernel_id(gpl_copyleft_scope__narrow_scope_reading, gpl_copyleft_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gpl_copyleft_scope__narrow_scope_reading, commercial_integrators).
narrative_ontology:constraint_beneficiary(gpl_copyleft_scope__narrow_scope_reading, proprietary_plugin_vendors).
narrative_ontology:constraint_beneficiary(gpl_copyleft_scope__narrow_scope_reading, dual_licensing_vendors).
narrative_ontology:constraint_victim(gpl_copyleft_scope__narrow_scope_reading, fsf_aligned_maintainers).
narrative_ontology:constraint_victim(gpl_copyleft_scope__narrow_scope_reading, downstream_gpl_contributors).
narrative_ontology:constraint_vindicates(gpl_copyleft_scope__narrow_scope_reading, traditional_copyright_derivative_work_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Build products that link against or aggregate with GPL-licensed components while keeping proprietary layers closed. Under this reading, mere aggregation and most dynamic linking do not trigger copyleft, so they capture the utility of GPL code without releasing their own source. They can restructure architecture (favoring dynamic linking, separate processes, plugin boundaries) specifically to stay outside the derivative-work line.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__narrow_scope_reading, commercial_integrators, beneficiary,
    powerful, biographical, mobile, global).

% Sell plugins or extensions that interoperate with GPL host applications through defined APIs. This reading treats plugin architecture as outside Section 2(b)'s reach, letting them monetize proprietary extensions to copyleft software with minimal licensing risk.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__narrow_scope_reading, proprietary_plugin_vendors, beneficiary,
    organized, biographical, mobile, global).

% Copyright holders of GPL-licensed core products who sell separate proprietary licenses to commercial customers wanting the narrow-scope integration path without any copyleft obligation. They actively promote and litigate for the narrow reading because it is the basis of their commercial license revenue; they help set the interpretive norm through public statements, FAQs, and settlement terms.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__narrow_scope_reading, dual_licensing_vendors, beneficiary,
    organized, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(gpl_copyleft_scope__narrow_scope_reading, dual_licensing_vendors, agenda_setter).

% Maintain GPL projects with the expectation that combined or interoperating works reciprocate by contributing source back to the commons. Under the narrow reading, they watch commercially valuable extensions and integrations built on their code escape the reciprocity obligation, while they bear the ongoing maintenance cost of the widely-reused core. Their only recourse is relicensing future versions or moving to stronger copyleft variants (AGPL), which does not recover value already extracted under prior versions.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__narrow_scope_reading, fsf_aligned_maintainers, payer,
    moderate, generational, constrained, global).

% Individual and small-team contributors who donate labor to GPL codebases expecting copyleft to keep derivative value in the commons. The narrow reading means their contributions can be incorporated into commercial products via aggregation or dynamic linking without those products' proprietary layers ever becoming available to them, diluting the return on their unpaid labor.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__narrow_scope_reading, downstream_gpl_contributors, payer,
    powerless, biographical, constrained, global).

% Organizations (FSF-adjacent, Software Freedom Conservancy-adjacent) that argue for the strong-copyleft boundary as the movement's founding commitment. They lack the enforcement capacity or judicial precedent to compel the strong reading against well-resourced commercial actors, so their preferred interpretation is structurally sidelined by the narrow reading's practical dominance in industry practice.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__narrow_scope_reading, copyleft_advocacy_organizations, excluded,
    organized, civilizational, trapped, global).

% Adjudicate or analyze derivative-work boundary disputes when they reach litigation, applying traditional copyright doctrine (substantial similarity, independent creation, functional separability) to license-coupling questions. Their scarce, inconsistent rulings are what leaves the narrow and strong readings both viable in practice.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__narrow_scope_reading, courts_and_legal_scholars, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gpl_copyleft_scope__narrow_scope_reading, dual_licensing_vendors).
narrative_ontology:fixing_cost_class(gpl_copyleft_scope__narrow_scope_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a predictable, judicially-grounded line for when combining code with GPL components triggers copyleft obligations, letting commercial and open-source ecosystems coexist: firms can build proprietary products around GPL cores via clean interfaces, and GPL projects gain wider adoption and indirect contribution flow from that ecosystem.
% TRANSFER_FUNCTION: Moves the option value of ambiguous combination architectures (aggregation, plugin boundaries, dynamic linking) away from the copyleft commons and toward whichever party is positioned to design the interface — typically the commercial integrator or the dual-licensing rightsholder — at the expense of contributors who assumed broader reciprocity.
% ABSENT_VOICES: Individual contributors who submitted patches under an assumption of universal downstream sharing are not consulted when downstream integrators choose architectures that stay outside the narrow scope line; they have no seat in the interpretive community (industry counsel, dual-licensing vendors, and courts) that settles the boundary in practice.
% DISAPPEARANCE_RATIONALE: If the narrow-scope reading disappeared and only the strong-copyleft reading governed, commercial integrators would face copyleft obligations on far more combination patterns; many current dynamic-linking and plugin-based commercial products built atop GPL cores would need to relicense, redesign around cleaner separation, or pay for proprietary dual licenses, materially changing the economics of GPL-adjacent commercial software.
% FOUNDING_PROBLEM: GPL Section 2(b) was drafted to prevent proprietary code from capturing GPL-licensed improvements by wrapping them in a larger combined work while withholding source — the founding problem was defining where 'the work' ends so copyleft's reciprocity mechanism has a determinate boundary.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars analyzing derivative-work doctrine (outside both the FSF and commercial-vendor camps) attest that the boundary question remains genuinely unsettled under general copyright law, not merely under-litigated; commercial integrators and dual-licensing vendors assert the narrow reading reflects settled doctrine, while FSF-aligned maintainers assert the boundary question is a live threat to copyleft's function — the corroborating academic literature supports 'contested' rather than either party's confident claim of settlement.
narrative_ontology:disappearance_verdict(gpl_copyleft_scope__narrow_scope_reading, world_rearranges).
narrative_ontology:founding_problem_status(gpl_copyleft_scope__narrow_scope_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gpl_copyleft_scope__narrow_scope_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(gpl_copyleft_scope__narrow_scope_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gpl_copyleft_scope__narrow_scope_reading, 0.32, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gpl_copyleft_scope__narrow_scope_reading_tests).
:- end_tests(gpl_copyleft_scope__narrow_scope_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.32 at interval end) — real coordination value exists (mixed codebases are genuinely enabled), but there is a real, if modest, transfer of reciprocity-value away from contributors toward integrators who can architect around the line. Suppression is low (0.22): no one is coerced into the narrow reading; commercial actors choose it because courts have not foreclosed it, and copyleft projects remain free to relicense under AGPL or add explicit linking clauses to close the gap prospectively. Theater ratio is low-moderate (0.28) — the doctrinal reasoning is substantively applied, not merely performed, though industry FAQs and dual-license marketing materials add some interpretive theater dressing traditional copyright doctrine as more settled than the corroboration record supports. Accessibility collapse is moderate (0.35): alternatives (AGPL, explicit linking clauses, custom licenses) exist and are increasingly used precisely because parties understand the narrow reading's gap. Resistance is moderate (0.45): FSF-aligned maintainers and copyleft advocacy groups actively contest this reading through licensing innovation (AGPL adoption) and public advocacy, but lack judicial or enforcement leverage to overturn it.
 *
 * PERSPECTIVAL GAP:
 *   From the commercial integrator's seat, this reading is straightforwardly a rope: it coordinates mixed-license software ecosystems efficiently, with no coercion and clear architectural rules to follow. From the FSF-aligned maintainer's seat, the same doctrinal reading looks like a structural leak in copyleft's reciprocity mechanism — a legally sanctioned gap that lets commercial actors free-ride on commons labor. The engine should compute a milder classification from the beneficiary seats and a more extractive-leaning computation from the payer seats; this divergence is exactly what the framework is built to surface, not to resolve into one number.
 *
 * DIRECTIONALITY LOGIC:
 *   Commercial integrators and plugin vendors sit near the beneficiary end: the narrow reading directly subsidizes their ability to build proprietary value atop GPL cores without reciprocity. Dual-licensing vendors are the strongest beneficiaries with the most agency — they are also agenda-setters, actively shaping and litigating for this reading because their commercial-license business model depends on the coupling gap it preserves. FSF-aligned maintainers and downstream contributors are targets: their prior investment of labor was made under an expectation the narrow reading disappoints, and their exit options (relicensing forward) do not recover value already captured under this reading's regime.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing proprietary capture of GPL improvements via combination) is only partially live under this reading: for direct derivative works it remains addressed, but for aggregation, plugin, and dynamic-linking patterns the mechanism functions much more weakly than contributors who joined the ecosystem historically assumed. This is not a case of a fully dead mandate (the clause still does real work on direct derivatives) nor a fully captured zombie mandate (courts have not endorsed the narrow reading as settled, so the gap remains contestable) — it sits in the contested middle the R5 corroboration explicitly flags.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    narrow_scope_judicial_settlement,
    'Will appellate courts eventually settle on the narrow-scope doctrinal boundary as controlling, or will a strong-copyleft interpretation gain judicial traction in a landmark ruling?',
    'Track outcomes of GPL derivative-work litigation (e.g., disputes over dynamic linking, kernel module boundaries, plugin architectures) across jurisdictions; a consistent line of rulings applying traditional copyright substantial-similarity/independent-creation tests to combination questions would corroborate this reading as durable rather than provisional.',
    'If courts settle firmly on the narrow reading, the coordination function stabilizes and extraction from contributors becomes a known, priceable cost that projects can route around (via AGPL, explicit linking clauses). If courts instead move toward the strong reading, this constraint''s classification as a moderate-epsilon rope would need re-evaluation — the ecosystem behavior it currently enables would become substantially riskier for commercial integrators.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(narrow_scope_judicial_settlement, empirical, 'Whether judicial precedent will stabilize the narrow reading or displace it.').

omega_variable(
    reading_selection_is_committer_choice,
    'Is the choice between narrow-scope, strong-copyleft, and enforcement-vacuum readings a matter of which is doctrinally correct, or a matter of which interpretive community has practical enforcement leverage in a given ecosystem?',
    'Compare doctrinal reasoning quality across the three readings against observed real-world enforcement outcomes: if enforcement behavior tracks community power (FSF-aligned vs industry-dominated contexts) rather than doctrinal merit, the enforcement_vacuum_reading''s structural claim is empirically favored over treating narrow_scope or strong_copyleft as ''the'' correct legal answer.',
    'If enforcement outcomes track community power rather than doctrine, this narrow_scope_reading constraint should be understood as one licensed interpretation among a plurality rather than a settled doctrinal fact — its beneficiaries would be better described as benefiting from favorable enforcement asymmetry than from correct legal interpretation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_selection_is_committer_choice, conceptual, 'Whether the kernel''s readings compete on doctrinal merit or on differential enforcement capacity.').

omega_variable(
    contribution_expectation_baseline,
    'Did the majority of historical GPL contributors actually understand and accept the narrow-scope boundary at time of contribution, or did they contribute under a broader (strong-copyleft) understanding that this reading retroactively narrows?',
    'Survey historical contributor communications, project FAQs, and community norms documents contemporaneous with major contributions to assess prevailing understanding of the derivative-work boundary at time of contribution.',
    'If contributors broadly understood and accepted the narrow boundary, the ''extraction from downstream_gpl_contributors'' framing weakens substantially — informed consent to the actual terms undercuts the victim characterization. If contributors held a strong-copyleft expectation, the extraction framing is strengthened.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(contribution_expectation_baseline, empirical, 'Whether contributor consent was informed by the narrow or the strong reading at time of contribution.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gpl_copyleft_scope__narrow_scope_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gpl__tr_t0, gpl_copyleft_scope__narrow_scope_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(gpl__tr_t6, gpl_copyleft_scope__narrow_scope_reading, theater_ratio, 6, 0.18).
narrative_ontology:measurement(gpl__tr_t12, gpl_copyleft_scope__narrow_scope_reading, theater_ratio, 12, 0.2).
narrative_ontology:measurement(gpl__tr_t18, gpl_copyleft_scope__narrow_scope_reading, theater_ratio, 18, 0.23).
narrative_ontology:measurement(gpl__tr_t24, gpl_copyleft_scope__narrow_scope_reading, theater_ratio, 24, 0.26).
narrative_ontology:measurement(gpl__tr_t30, gpl_copyleft_scope__narrow_scope_reading, theater_ratio, 30, 0.28).

% Extraction over time
narrative_ontology:measurement(gpl__be_t0, gpl_copyleft_scope__narrow_scope_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(gpl__be_t6, gpl_copyleft_scope__narrow_scope_reading, base_extractiveness, 6, 0.21).
narrative_ontology:measurement(gpl__be_t12, gpl_copyleft_scope__narrow_scope_reading, base_extractiveness, 12, 0.24).
narrative_ontology:measurement(gpl__be_t18, gpl_copyleft_scope__narrow_scope_reading, base_extractiveness, 18, 0.27).
narrative_ontology:measurement(gpl__be_t24, gpl_copyleft_scope__narrow_scope_reading, base_extractiveness, 24, 0.3).
narrative_ontology:measurement(gpl__be_t30, gpl_copyleft_scope__narrow_scope_reading, base_extractiveness, 30, 0.32).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(gpl_copyleft_scope__narrow_scope_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gpl_copyleft_scope__narrow_scope_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(gpl_copyleft_scope__narrow_scope_reading, 0.12).
narrative_ontology:affects_constraint(gpl_copyleft_scope__narrow_scope_reading, strong_copyleft_reading).
narrative_ontology:affects_constraint(gpl_copyleft_scope__narrow_scope_reading, enforcement_vacuum_reading).

% DUAL FORMULATION NOTE:
% gpl_copyleft_scope is a kernel (the ambiguous text of GPL Section 2(b) and the underlying derivative-work boundary question) with three sibling readings, each a separate constraint story: narrow_scope_reading (this story, moderate-epsilon rope favoring commercial integration flexibility), strong_copyleft_reading (higher-epsilon, favoring copyleft reciprocity and constraining integrators more tightly), and enforcement_vacuum_reading (a structurally distinct claim that neither doctrine is operative in isolation — enforcement capacity of the interpretive community determines the effective constraint in context). All three share the same kernel text but instantiate different beneficiary/victim structures and different epsilon values; per the epsilon-invariance principle they are authored as three files linked by network edges rather than one file with a measurement parameter.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(gpl_copyleft_scope__narrow_scope_reading, organized, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
