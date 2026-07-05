% ============================================================================
% CONSTRAINT STORY: gpl_copyleft_scope__strong_copyleft_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gpl_copyleft_scope__strong_copyleft_reading, []).

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
 *   constraint_id: gpl_copyleft_scope__strong_copyleft_reading
 *   human_readable: GPL Section 2(b) — Strong Copyleft (Broad Derivative Work) Reading
 *   domain: software_licensing/intellectual_property
 *
 * SUMMARY:
 *   The Free Software Foundation and the maintainers of major GPL-licensed
 *   components (e.g. certain compiler runtimes, core libraries) have, since
 *   the license's drafting, advanced and enforced (through compliance
 *   letters, public pressure, and occasional litigation threats) a reading of
 *   Section 2(b) under which any work that dynamically links against, or is
 *   otherwise combined with, GPL code becomes itself a derivative work
 *   subject to GPL's copyleft terms. This reading forecloses proprietary
 *   vendors, commercial plugin developers, and embedded systems integrators
 *   from using GPL components without either releasing their own source or
 *   negotiating a separate commercial license. The claim is presented as
 *   settled doctrine flowing necessarily from software-freedom principles;
 *   the metrics below describe a constraint that behaves as a
 *   high-extraction, actively-enforced exclusion mechanism against a specific
 *   class of commercial actors, resting on interpretive authority rather than
 *   adjudicated precedent.
 *
 * KEY AGENTS:
 *   - free_software_foundation: agenda_setter (institutional/analytical) — authors and enforces the broad reading
 *   - gpl_component_maintainers: beneficiary/agenda_setter (organized/mobile) — invoke the doctrine against integrators, sometimes for dual-license revenue
 *   - proprietary_software_vendors: primary target (powerful/constrained) — structurally excluded from GPL integration without full disclosure
 *   - commercial_plugin_developers: secondary target (moderate/constrained) — plugin business models foreclosed without separate licensing
 *   - embedded_systems_integrators: trapped target (moderate/trapped) — architecture commitments made years before compliance risk surfaces
 *   - narrow_scope_practitioners: excluded dissenting reading (organized/constrained) — hold the boundary is not settled law
 *   - judicial_and_regulatory_observers: analytical (institutional/analytical) — the reading has rarely been tested to a binding ruling
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gpl_copyleft_scope__strong_copyleft_reading, 0.68).
domain_priors:suppression_score(gpl_copyleft_scope__strong_copyleft_reading, 0.61).
domain_priors:theater_ratio(gpl_copyleft_scope__strong_copyleft_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gpl_copyleft_scope__strong_copyleft_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(gpl_copyleft_scope__strong_copyleft_reading, suppression_requirement, 0.61).
narrative_ontology:constraint_metric(gpl_copyleft_scope__strong_copyleft_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gpl_copyleft_scope__strong_copyleft_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(gpl_copyleft_scope__strong_copyleft_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gpl_copyleft_scope__strong_copyleft_reading, snare).
narrative_ontology:human_readable(gpl_copyleft_scope__strong_copyleft_reading, "GPL Section 2(b) — Strong Copyleft (Broad Derivative Work) Reading").
narrative_ontology:topic_domain(gpl_copyleft_scope__strong_copyleft_reading, "software_licensing/intellectual_property").

domain_priors:requires_active_enforcement(gpl_copyleft_scope__strong_copyleft_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gpl_copyleft_scope__strong_copyleft_reading, 'feaa32a1-cc0e-4f6e-8461-4b8cc54c5156').
narrative_ontology:cs_kernel_codification('feaa32a1-cc0e-4f6e-8461-4b8cc54c5156', fixed_text).
narrative_ontology:cs_authority_grounding('feaa32a1-cc0e-4f6e-8461-4b8cc54c5156', extraction).
narrative_ontology:cs_interpretation_layer_present('feaa32a1-cc0e-4f6e-8461-4b8cc54c5156').
narrative_ontology:cs_reading_relation('feaa32a1-cc0e-4f6e-8461-4b8cc54c5156', gpl_copyleft_scope__narrow_scope_reading, forecloses).
narrative_ontology:cs_reading_relation('feaa32a1-cc0e-4f6e-8461-4b8cc54c5156', gpl_copyleft_scope__enforcement_vacuum_reading, influences).
narrative_ontology:cs_axiom('feaa32a1-cc0e-4f6e-8461-4b8cc54c5156', foundational, coupling_triggers_transitive_freedom_obligation).
narrative_ontology:cs_axiom_status(coupling_triggers_transitive_freedom_obligation, holdable).
narrative_ontology:cs_axiom_grounding('feaa32a1-cc0e-4f6e-8461-4b8cc54c5156', coupling_triggers_transitive_freedom_obligation, deontological).
narrative_ontology:cs_axiom('feaa32a1-cc0e-4f6e-8461-4b8cc54c5156', secondary, derivative_work_boundary_follows_functional_coupling_not_literal_copying).
narrative_ontology:cs_axiom_status(derivative_work_boundary_follows_functional_coupling_not_literal_copying, holdable).
narrative_ontology:cs_axiom_grounding('feaa32a1-cc0e-4f6e-8461-4b8cc54c5156', derivative_work_boundary_follows_functional_coupling_not_literal_copying, conventional).
narrative_ontology:cs_reference_frame('feaa32a1-cc0e-4f6e-8461-4b8cc54c5156', software_freedom_transitivity_reference).
narrative_ontology:cs_drift_state('feaa32a1-cc0e-4f6e-8461-4b8cc54c5156', post_saas_and_cloud_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('feaa32a1-cc0e-4f6e-8461-4b8cc54c5156', '').
narrative_ontology:cs_kernel_id(gpl_copyleft_scope__strong_copyleft_reading, gpl_copyleft_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gpl_copyleft_scope__strong_copyleft_reading, free_software_foundation).
narrative_ontology:constraint_beneficiary(gpl_copyleft_scope__strong_copyleft_reading, copyleft_native_projects).
narrative_ontology:constraint_beneficiary(gpl_copyleft_scope__strong_copyleft_reading, gpl_component_maintainers).
narrative_ontology:constraint_victim(gpl_copyleft_scope__strong_copyleft_reading, proprietary_software_vendors).
narrative_ontology:constraint_victim(gpl_copyleft_scope__strong_copyleft_reading, commercial_plugin_developers).
narrative_ontology:constraint_victim(gpl_copyleft_scope__strong_copyleft_reading, embedded_systems_integrators).
narrative_ontology:constraint_vindicates(gpl_copyleft_scope__strong_copyleft_reading, software_freedom_transitivity_doctrine).
narrative_ontology:constraint_vindicates(gpl_copyleft_scope__strong_copyleft_reading, broad_derivative_work_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Authors and stewards the license text, publishes the broad reading of Section 2(b) as canonical, funds compliance/enforcement work (via affiliated bodies), and litigates or threatens litigation against vendors it deems in violation. Sets the interpretive line that dynamic linking triggers the copyleft obligation.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__strong_copyleft_reading, free_software_foundation, agenda_setter,
    institutional, civilizational, analytical, global).

% Projects built GPL-first receive a structural guarantee that downstream commercial users cannot fork proprietary value out of their commons without releasing source. This reading protects their contribution model and bargaining position relative to companies that would otherwise free-ride on their code.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__strong_copyleft_reading, copyleft_native_projects, beneficiary,
    organized, generational, mobile, global).

% Maintain widely-used GPL libraries and can invoke the broad linking doctrine against commercial integrators to force either compliance (source release) or licensing negotiations (dual-licensing revenue). They administer enforcement letters and shape compliance expectations for the ecosystem.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__strong_copyleft_reading, gpl_component_maintainers, beneficiary,
    organized, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(gpl_copyleft_scope__strong_copyleft_reading, gpl_component_maintainers, agenda_setter).

% Cannot dynamically link against GPL components without triggering full source disclosure under this reading, regardless of how loosely coupled the integration is. Their options are: avoid GPL code entirely (re-implementation cost), negotiate a commercial/dual license where available, or restructure architecture to argue aggregation rather than linking — all costly, and none guaranteed to survive an enforcement challenge under the broad reading.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__strong_copyleft_reading, proprietary_software_vendors, payer,
    powerful, biographical, constrained, global).

% Build plugins or extensions that link against GPL-licensed host applications for revenue. Under the strong reading, their plugin is itself a derivative work regardless of interface boundary, so selling closed-source plugins against a GPL host is structurally foreclosed unless they secure a separate license — often unavailable to a small developer without direct negotiation leverage.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__strong_copyleft_reading, commercial_plugin_developers, payer,
    moderate, biographical, constrained, national).

% Ship consumer hardware with GPL components (e.g. Linux kernel, GPL utilities) statically or dynamically linked to proprietary firmware or drivers. Under the strong reading, this coupling is presumptively a combined work, exposing them to compliance demands or litigation risk years after a product has shipped, with recall or source-release remediation costs they cannot avoid once the architecture is committed.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__strong_copyleft_reading, embedded_systems_integrators, payer,
    moderate, biographical, trapped, global).

% Lawyers, industry consortia, and engineers who hold that traditional copyright derivative-work doctrine does not automatically extend to dynamic linking or plugin architectures. Their reading is not represented in this constraint's operation — this story instantiates the broad reading only — but they would object that the boundary claimed here is not settled law, merely FSF's preferred interpretation, enforced as if it were.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__strong_copyleft_reading, narrow_scope_practitioners, excluded,
    organized, generational, constrained, global).

% Courts and competition/IP regulators who would ultimately adjudicate a contested Section 2(b) claim. Almost no case has been fully litigated to a binding derivative-work-scope ruling; most disputes settle before judgment, so this reading's authority rests on threat credibility and voluntary compliance rather than adjudicated precedent.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__strong_copyleft_reading, judicial_and_regulatory_observers, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gpl_copyleft_scope__strong_copyleft_reading, gpl_component_maintainers).
narrative_ontology:fixing_cost_class(gpl_copyleft_scope__strong_copyleft_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Guarantees that anyone who builds on top of, or tightly integrates with, GPL-licensed code cannot capture that shared value in a closed, non-reciprocal product — sustaining a commons where contributors can trust their code will not be enclosed downstream.
% TRANSFER_FUNCTION: Moves the option to keep source proprietary away from any vendor whose product dynamically links or combines with GPL code, transferring that option's value to the GPL ecosystem (as compliance-forced source disclosure, dual-license revenue, or as leverage that structurally deters proprietary competition against GPL-based offerings).
% ABSENT_VOICES: Narrow-scope practitioners, IP counsel who read derivative-work doctrine conservatively, and courts that have never definitively ruled on dynamic-linking scope are structurally absent from this reading's own operation — the broad reading is enforced through threat and voluntary compliance, in a legal environment where a fully-litigated contrary ruling has never actually confronted it.
% DISAPPEARANCE_RATIONALE: If the broad linking doctrine vanished overnight (i.e., if courts and enforcers uniformly adopted the narrow-scope reading instead), proprietary vendors and plugin developers would freely dynamically link against GPL components without disclosure obligations, dual-licensing revenue models for GPL maintainers would collapse, and the commercial incentive structure protecting copyleft-native projects from unreciprocated appropriation would substantially weaken.
% FOUNDING_PROBLEM: GPL was drafted to prevent a specific extraction pattern: a company takes free software, embeds or extends it in a proprietary product, and ships that product without contributing improvements back — capturing commons value while contributing nothing. Section 2(b)'s combined/derivative-work language was meant to close obvious workarounds to this norm.
% FOUNDING_PROBLEM_CORROBORATION: The FSF and GPL-native maintainers attest the founding problem remains live and that the broad reading is necessary to close linking-based workarounds. However, this attestation comes from the reading's own primary beneficiaries. Outside corroboration is thin and mixed: industry legal counsel, several open-source foundations (e.g. Linux Foundation-adjacent guidance), and academic IP scholars have published analyses arguing the broad linking-triggers-copyleft claim overstates settled derivative-work law, and note the near-total absence of a fully litigated appellate ruling squarely resolving dynamic-linking scope in either direction — meaning the 'settled doctrine' framing here is itself contested by non-beneficiary legal observers, not merely by adverse commercial interests.
narrative_ontology:disappearance_verdict(gpl_copyleft_scope__strong_copyleft_reading, world_rearranges).
narrative_ontology:founding_problem_status(gpl_copyleft_scope__strong_copyleft_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gpl_copyleft_scope__strong_copyleft_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(gpl_copyleft_scope__strong_copyleft_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gpl_copyleft_scope__strong_copyleft_reading, 0.68, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gpl_copyleft_scope__strong_copyleft_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(gpl_copyleft_scope__strong_copyleft_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(gpl_copyleft_scope__strong_copyleft_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) reflects that under this reading, commercial actors who would otherwise capture value through tight but non-literal-copying integration with GPL code are foreclosed from doing so without disclosure or negotiated license — a real transfer of optionality value from vendors to the GPL ecosystem, layered on top of whatever genuine coordination function the copyleft commons serves. Suppression (0.61) is substantial but not maximal: enforcement operates mainly through compliance letters, community pressure, and litigation threat rather than a fully adjudicated body of case law, and it has risen over the interval (0.30 → 0.61) as the FSF and major maintainers built out formal compliance programs and enforcement infrastructure (e.g. software freedom conservancy-style litigation capacity) rather than relying on informal norm pressure alone. Theater ratio stays low (0.22) — the underlying software-freedom coordination function (a genuine commons with real contribution reciprocity) is substantially real, not primarily performative. Accessibility collapse (0.58) is moderate: vendors do have alternatives (permissively-licensed replacements, clean-room reimplementation, dual-licensing negotiation) but the switching cost is real and rises as GPL components become more deeply embedded in toolchains. Resistance (0.72) is high — this reading is the single most litigated and lobbied-against interpretive question in open-source licensing history, and the metrics reflect a live, contested boundary, not a settled fact.
 *
 * PERSPECTIVAL GAP:
 *   From the FSF/maintainer seat, Section 2(b) under this reading is coordination: it is the mechanism that keeps the commons a commons, preventing free-riding extraction by commercial actors. From the vendor/integrator seat, the identical mechanism is experienced as an enforced exclusion regime resting on an aggressive and contested reading of derivative-work law — a boundary drawn wider than the underlying coordination problem (preventing literal appropriation of code) strictly requires. The engine's per-seat computation is expected to diverge sharply between these seats even though both are looking at the same clause.
 *
 * DIRECTIONALITY LOGIC:
 *   The FSF and GPL-native maintainers sit at the beneficiary end: they set the interpretive line, administer enforcement, and capture the value of forced disclosure or dual-licensing negotiation. Proprietary vendors, plugin developers, and embedded integrators sit at the target end: the broad reading directly constrains their commercial options, and their exit (reimplementation, license negotiation, architecture avoidance) is costly and imperfect. Embedded systems integrators are marked trapped rather than merely constrained because their architectural commitments are made at product-design time, long before any compliance dispute surfaces, making post-hoc exit far more expensive than for vendors who can still redesign a product line.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — preventing uncompensated commercial appropriation of shared free-software code — remains partially live (proprietary appropriation of open commons is a real and recurring pattern), which argues against calling this pure mandatrophy. But the specific mechanism this reading uses (extending the boundary to essentially all forms of coupling, including loose dynamic linking) is broader than what the founding problem strictly requires to solve, and its persistence is defended more by interpretive authority and enforcement threat than by settled doctrine. This is why the story is authored as snare rather than tangled_rope: a coordination story (protect the commons) is present, but the operative mechanism as enforced sweeps in actors and integration patterns whose relationship to the founding extraction problem is attenuated, and the enforcement rests on unadjudicated interpretive claims rather than a demonstrated, narrowly-tailored fit between mechanism and problem.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indeterminacy,
    'Is the strong_copyleft_reading the legally correct reading of GPL Section 2(b), or is it one contested interpretation among at least two others (narrow_scope_reading, enforcement_vacuum_reading) that remain equally live in the absence of definitive appellate precedent?',
    'A fully litigated appellate ruling squarely addressing whether dynamic linking or plugin-architecture coupling constitutes a combined/derivative work under GPL Section 2(b) would resolve this for the relevant jurisdiction; absent that, the sibling readings persist as a licensed plurality resolved case-by-case by relative enforcement capacity.',
    'If a court adopted the narrow_scope_reading as controlling law, the extraction and suppression this story documents would collapse toward the narrow reading''s much lower values, since the enforcement threats underlying this story''s suppression metric would lose legal credibility. If enforcement_vacuum_reading proves the more accurate description of the current landscape, then this story''s classification applies only within FSF-aligned enforcement contexts, not universally.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_indeterminacy, conceptual, 'Whether the strong reading is settled doctrine, one of several live readings, or an artifact of unresolved enforcement capacity — the central committer-structure ambiguity this story deliberately routes here rather than resolving internally.').

omega_variable(
    coordination_extraction_boundary_linking,
    'Does preventing the specific extraction pattern the GPL was built to stop (silent proprietary appropriation of contributed code) actually require the boundary to extend to dynamic linking and plugin architectures, or would a narrower boundary (direct code copying, static linking, tight structural coupling) fully close that gap while leaving loosely-coupled integration unconstrained?',
    'Comparative analysis of commons health (contribution rates, fork-and-abandon incidence, commercial free-riding incidence) across GPL projects that have pursued broad enforcement versus similarly-situated copyleft projects (e.g. LGPL, MPL) that draw the boundary more narrowly.',
    'If commons health is comparable under narrower boundaries, the broad reading''s additional exclusionary reach would be extraction beyond what the coordination function requires; if commons health depends on the broad boundary, more of the measured extraction should be attributed to necessary coordination cost.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_extraction_boundary_linking, empirical, 'Whether the broad linking boundary is necessary to the underlying coordination function or is extraction riding on top of it.').

omega_variable(
    enforcement_threat_credibility,
    'How much of the suppression this story measures reflects genuine legal risk versus the in terrorem effect of an unadjudicated but aggressively asserted interpretive claim?',
    'Track the settlement rate, settlement terms, and litigated-outcome rate of GPL Section 2(b) enforcement actions over time; a high settlement rate with favorable terms to enforcers despite near-zero adjudicated wins would indicate threat-driven rather than doctrine-driven suppression.',
    'If suppression is substantially threat-driven rather than doctrine-driven, the effective extraction this reading generates is more fragile than the metrics suggest — a single adverse appellate ruling could collapse it quickly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_threat_credibility, empirical, 'Whether measured suppression tracks real adjudicated legal risk or largely reflects unadjudicated threat credibility.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gpl_copyleft_scope__strong_copyleft_reading, 1991, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gpl__tr_t1991, gpl_copyleft_scope__strong_copyleft_reading, theater_ratio, 1991, 0.1).
narrative_ontology:measurement_basis(gpl__tr_t1991, observed).
narrative_ontology:measurement(gpl__tr_t2000, gpl_copyleft_scope__strong_copyleft_reading, theater_ratio, 2000, 0.13).
narrative_ontology:measurement_basis(gpl__tr_t2000, observed).
narrative_ontology:measurement(gpl__tr_t2007, gpl_copyleft_scope__strong_copyleft_reading, theater_ratio, 2007, 0.17).
narrative_ontology:measurement_basis(gpl__tr_t2007, observed).
narrative_ontology:measurement(gpl__tr_t2014, gpl_copyleft_scope__strong_copyleft_reading, theater_ratio, 2014, 0.19).
narrative_ontology:measurement_basis(gpl__tr_t2014, observed).
narrative_ontology:measurement(gpl__tr_t2020, gpl_copyleft_scope__strong_copyleft_reading, theater_ratio, 2020, 0.21).
narrative_ontology:measurement_basis(gpl__tr_t2020, observed).
narrative_ontology:measurement(gpl__tr_t2025, gpl_copyleft_scope__strong_copyleft_reading, theater_ratio, 2025, 0.22).
narrative_ontology:measurement_basis(gpl__tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(gpl__be_t1991, gpl_copyleft_scope__strong_copyleft_reading, base_extractiveness, 1991, 0.35).
narrative_ontology:measurement_basis(gpl__be_t1991, observed).
narrative_ontology:measurement(gpl__be_t2000, gpl_copyleft_scope__strong_copyleft_reading, base_extractiveness, 2000, 0.48).
narrative_ontology:measurement_basis(gpl__be_t2000, observed).
narrative_ontology:measurement(gpl__be_t2007, gpl_copyleft_scope__strong_copyleft_reading, base_extractiveness, 2007, 0.58).
narrative_ontology:measurement_basis(gpl__be_t2007, observed).
narrative_ontology:measurement(gpl__be_t2014, gpl_copyleft_scope__strong_copyleft_reading, base_extractiveness, 2014, 0.63).
narrative_ontology:measurement_basis(gpl__be_t2014, observed).
narrative_ontology:measurement(gpl__be_t2020, gpl_copyleft_scope__strong_copyleft_reading, base_extractiveness, 2020, 0.66).
narrative_ontology:measurement_basis(gpl__be_t2020, observed).
narrative_ontology:measurement(gpl__be_t2025, gpl_copyleft_scope__strong_copyleft_reading, base_extractiveness, 2025, 0.68).
narrative_ontology:measurement_basis(gpl__be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(gpl__su_t1991, gpl_copyleft_scope__strong_copyleft_reading, suppression_requirement, 1991, 0.3).
narrative_ontology:measurement_basis(gpl__su_t1991, observed).
narrative_ontology:measurement(gpl__su_t2000, gpl_copyleft_scope__strong_copyleft_reading, suppression_requirement, 2000, 0.42).
narrative_ontology:measurement_basis(gpl__su_t2000, observed).
narrative_ontology:measurement(gpl__su_t2007, gpl_copyleft_scope__strong_copyleft_reading, suppression_requirement, 2007, 0.52).
narrative_ontology:measurement_basis(gpl__su_t2007, observed).
narrative_ontology:measurement(gpl__su_t2014, gpl_copyleft_scope__strong_copyleft_reading, suppression_requirement, 2014, 0.57).
narrative_ontology:measurement_basis(gpl__su_t2014, observed).
narrative_ontology:measurement(gpl__su_t2020, gpl_copyleft_scope__strong_copyleft_reading, suppression_requirement, 2020, 0.6).
narrative_ontology:measurement_basis(gpl__su_t2020, observed).
narrative_ontology:measurement(gpl__su_t2025, gpl_copyleft_scope__strong_copyleft_reading, suppression_requirement, 2025, 0.61).
narrative_ontology:measurement_basis(gpl__su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gpl_copyleft_scope__strong_copyleft_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(gpl_copyleft_scope__strong_copyleft_reading, 0.08).
narrative_ontology:affects_constraint(gpl_copyleft_scope__strong_copyleft_reading, gpl_copyleft_scope__narrow_scope_reading).
narrative_ontology:affects_constraint(gpl_copyleft_scope__strong_copyleft_reading, gpl_copyleft_scope__enforcement_vacuum_reading).

% DUAL FORMULATION NOTE:
% This story is one of three constraints decomposed from the natural-language concept 'GPL Section 2(b) derivative work scope' per the ε-invariance principle: measuring the constraint under the strong-copyleft interpretation yields a substantially different ε (high, ~0.68, snare-classified) than measuring it under the narrow-scope interpretation (much lower, coordination-dominant) or the enforcement-vacuum interpretation (context-dependent, plurality-classified). These are not three observations of one constraint but three structurally distinct constraints sharing a contested kernel (gpl_copyleft_scope), linked here via affects_constraints. Each carries its own beneficiary/victim structure, its own stakeholders, and its own classification; none averages or hedges across the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
