% ============================================================================
% CONSTRAINT STORY: software_control_legitimacy__pragmatic_openness_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_software_control_legitimacy__pragmatic_openness_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: software_control_legitimacy__pragmatic_openness_reading
 *   human_readable: Pragmatic Openness Reading of Software Control Legitimacy
 *   domain: software_engineering/political_economy_of_technology
 *
 * SUMMARY:
 *   This story instantiates the pragmatic-openness reading of the software
 *   control legitimacy kernel: the claim that whether software is open or
 *   proprietary is a development methodology choice to be evaluated on
 *   engineering merit (peer review quality, defect rates, collaborative
 *   velocity) rather than a matter of fundamental ethical obligation
 *   (freedom_imperative_reading), inherent property authority
 *   (property_rights_reading), or negotiated commons governance
 *   (commons_reading). Under this reading there is no victim set — both
 *   models are held legitimate, and the constraint's function is to let
 *   engineering organizations select a methodology without first resolving
 *   the deeper normative dispute. This is deliberately a low-ε,
 *   low-suppression story: the reading itself claims coexistence, and the
 *   metrics reflect a genuinely low-conflict coordination function rather
 *   than one type winning by fiat.
 *
 * KEY AGENTS:
 *   - open_source_contributors: organized/mobile — benefit from peer review, collaboration, reputational capital
 *   - software_users: moderate/mobile — benefit from methodology-agnostic quality evaluation and choice
 *   - proprietary_vendors: powerful/mobile — benefit from equal legitimacy granted to closed development
 *   - enterprise_adopters: powerful/mobile — benefit from being able to mix models without ideological cost
 *   - free_software_advocates: organized/mobile — excluded from the frame; reject the pragmatism premise itself
 *   - software_engineering_researchers: analytical/analytical — observe empirical methodology outcomes
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(software_control_legitimacy__pragmatic_openness_reading, 0.12).
domain_priors:suppression_score(software_control_legitimacy__pragmatic_openness_reading, 0.08).
domain_priors:theater_ratio(software_control_legitimacy__pragmatic_openness_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(software_control_legitimacy__pragmatic_openness_reading, extractiveness, 0.12).
narrative_ontology:constraint_metric(software_control_legitimacy__pragmatic_openness_reading, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(software_control_legitimacy__pragmatic_openness_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(software_control_legitimacy__pragmatic_openness_reading, accessibility_collapse, 0.2).
narrative_ontology:constraint_metric(software_control_legitimacy__pragmatic_openness_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(software_control_legitimacy__pragmatic_openness_reading, rope).
narrative_ontology:human_readable(software_control_legitimacy__pragmatic_openness_reading, "Pragmatic Openness Reading of Software Control Legitimacy").
narrative_ontology:topic_domain(software_control_legitimacy__pragmatic_openness_reading, "software_engineering/political_economy_of_technology").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(software_control_legitimacy__pragmatic_openness_reading, 'f9fc4311-a9af-40e4-991a-20b98fa1e489').
narrative_ontology:cs_kernel_codification('f9fc4311-a9af-40e4-991a-20b98fa1e489', distributed).
narrative_ontology:cs_authority_grounding('f9fc4311-a9af-40e4-991a-20b98fa1e489', distributed).
narrative_ontology:cs_reading_relation('f9fc4311-a9af-40e4-991a-20b98fa1e489', software_control_legitimacy__freedom_imperative_reading, coexists_with).
narrative_ontology:cs_reading_relation('f9fc4311-a9af-40e4-991a-20b98fa1e489', software_control_legitimacy__property_rights_reading, coexists_with).
narrative_ontology:cs_reading_relation('f9fc4311-a9af-40e4-991a-20b98fa1e489', software_control_legitimacy__commons_reading, influences).
narrative_ontology:cs_axiom('f9fc4311-a9af-40e4-991a-20b98fa1e489', foundational, methodology_choice_is_empirically_adjudicable).
narrative_ontology:cs_axiom_status(methodology_choice_is_empirically_adjudicable, holdable).
narrative_ontology:cs_axiom_grounding('f9fc4311-a9af-40e4-991a-20b98fa1e489', methodology_choice_is_empirically_adjudicable, empirically_contingent).
narrative_ontology:cs_axiom('f9fc4311-a9af-40e4-991a-20b98fa1e489', foundational, both_licensing_models_are_equally_legitimate_absent_quality_evidence).
narrative_ontology:cs_axiom_status(both_licensing_models_are_equally_legitimate_absent_quality_evidence, holdable).
narrative_ontology:cs_axiom_grounding('f9fc4311-a9af-40e4-991a-20b98fa1e489', both_licensing_models_are_equally_legitimate_absent_quality_evidence, instrumental).
narrative_ontology:cs_reference_frame('f9fc4311-a9af-40e4-991a-20b98fa1e489', methodology_neutral_engineering_culture).
narrative_ontology:cs_drift_state('f9fc4311-a9af-40e4-991a-20b98fa1e489', contemporary_platform_consolidation_era, gap(practice_drift, minor, false)).
narrative_ontology:cs_created_at('f9fc4311-a9af-40e4-991a-20b98fa1e489', '').
narrative_ontology:cs_kernel_id(software_control_legitimacy__pragmatic_openness_reading, software_control_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(software_control_legitimacy__pragmatic_openness_reading, open_source_contributors).
narrative_ontology:constraint_beneficiary(software_control_legitimacy__pragmatic_openness_reading, software_users).
narrative_ontology:constraint_beneficiary(software_control_legitimacy__pragmatic_openness_reading, proprietary_vendors).
narrative_ontology:constraint_beneficiary(software_control_legitimacy__pragmatic_openness_reading, enterprise_adopters).
narrative_ontology:constraint_vindicates(software_control_legitimacy__pragmatic_openness_reading, methodology_pluralism_doctrine).
narrative_ontology:constraint_vindicates(software_control_legitimacy__pragmatic_openness_reading, peer_review_quality_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Contribute code under open licenses, gaining reputational capital, peer review feedback, and collaborative improvement of shared codebases. Free to fork, free to switch projects, free to also work on proprietary software elsewhere — this reading treats their model as one legitimate path among several, not the only ethical one.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__pragmatic_openness_reading, open_source_contributors, beneficiary,
    organized, generational, mobile, global).

% Choose between open-source and proprietary software based on quality, support, cost, and fit for purpose. Under this reading they are not owed access to source code as a matter of principle; they evaluate methodology outcomes (bugs fixed, features shipped, security response time) pragmatically and switch when a competing offering is better.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__pragmatic_openness_reading, software_users, beneficiary,
    moderate, biographical, mobile, global).

% Build closed-source products, capture revenue through licensing or subscription, and compete on the same quality axis as open projects. This reading grants their model equal legitimacy provided they compete on merit rather than lock-in; they are not cast as illegitimate actors simply for withholding source.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__pragmatic_openness_reading, proprietary_vendors, beneficiary,
    powerful, generational, mobile, global).

% Select tooling and infrastructure based on total cost of ownership, support guarantees, and engineering quality rather than a prior ideological commitment to a licensing model. Free to run mixed open/proprietary stacks without contradiction under this reading.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__pragmatic_openness_reading, enterprise_adopters, beneficiary,
    powerful, biographical, mobile, global).

% Hold that proprietary software is ethically illegitimate regardless of its engineering quality, because it denies users control over their own computing. This reading treats their position as one competing methodology preference rather than the governing ethical fact, which they would reject as a category error — they are not consulted as arbiters of legitimacy in this framing.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__pragmatic_openness_reading, free_software_advocates, excluded,
    organized, generational, mobile, global).

% Study empirical outcomes of development methodologies — defect rates, time-to-patch, contributor retention, security audit results — across open and closed projects, informing (without settling) the pragmatic comparison this reading rests on.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__pragmatic_openness_reading, software_engineering_researchers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(software_control_legitimacy__pragmatic_openness_reading, diffuse).
narrative_ontology:fixing_cost_class(software_control_legitimacy__pragmatic_openness_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared framework in which software producers and consumers can evaluate development methodology on empirical merit (code quality, review rigor, responsiveness) rather than having to adjudicate a prior ideological commitment before any engineering choice can be made.
% TRANSFER_FUNCTION: Largely non-transfer: value flows from methodology to output quality within each project rather than from one party to another. Where transfer exists, it is the ordinary commercial transfer of proprietary vendors capturing license revenue and open projects capturing reputational/community capital — neither at the structural expense of the other.
% ABSENT_VOICES: Free software advocates who hold that proprietary control is ethically illegitimate independent of engineering outcomes are structurally outside this reading's frame; their objection is that quality-based pragmatism launders an autonomy violation as a mere methodology preference. They are not silenced by force, but their premise is simply not admitted as the governing question here.
% DISAPPEARANCE_RATIONALE: If this pragmatic-coexistence framing vanished, developers and organizations would still write software under some licensing regime — the world would not reorganize physically. But the discourse would likely re-polarize around one of the sibling readings (freedom imperative or property rights) as the default frame, changing which methodology choices carry a burden of justification. Whether that counts as 'world rearranges' or 'world unchanged' is itself disputed between the readings.
% FOUNDING_PROBLEM: Decades of methodology conflict between free-software advocates and commercial vendors made every licensing choice feel like it required an ethical justification; this reading was built to let engineering organizations choose a development model on empirical performance grounds without first resolving a contested normative dispute.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated outside the direct beneficiary set by empirical software-engineering research (e.g. comparative defect-density and patch-latency studies across licensing models) and by procurement/standards bodies that evaluate vendor and open-source options on service-level criteria rather than licensing ideology; these sources are not party to either the open-source or proprietary commercial interest and independently report that methodology-quality correlations, not licensing philosophy, drive most adoption decisions in practice.
narrative_ontology:disappearance_verdict(software_control_legitimacy__pragmatic_openness_reading, contested).
narrative_ontology:founding_problem_status(software_control_legitimacy__pragmatic_openness_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(software_control_legitimacy__pragmatic_openness_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(software_control_legitimacy__pragmatic_openness_reading, 'none', 1).
narrative_ontology:epsilon_provenance(software_control_legitimacy__pragmatic_openness_reading, 0.12, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(software_control_legitimacy__pragmatic_openness_reading_tests).
:- end_tests(software_control_legitimacy__pragmatic_openness_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored low (0.12) and roughly flat because this reading, by construction, treats no party as extracting from another through the mere fact of choosing a licensing model — both open and proprietary development are held legitimate and value flows through ordinary market/community mechanisms rather than through suppression of an alternative. Suppression is authored low (0.08) because neither model forecloses the other under this reading; users, vendors, and contributors can and do move between them. Theater ratio is low but drifts mildly upward (0.10 to 0.15) reflecting a realistic pattern where 'best tool for the job' rhetoric sometimes substitutes for genuine methodology evaluation as organizations mature, without this becoming dominant.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setting seat here is diffuse rather than concentrated in a single institution — no party administers 'pragmatic openness' as a rule; it is closer to a background epistemic norm shared across engineering culture. This is why no single agenda_setter stakeholder is named; the closest functional analogue (software_engineering_researchers) is cast as observer, not administrator, since the reading does not require active enforcement to persist. The excluded seat (free_software_advocates) would compute this constraint very differently — from their seat, treating proprietary control as 'merely a methodology choice' already concedes the contested point.
 *
 * DIRECTIONALITY LOGIC:
 *   All named beneficiaries sit near the symmetric-to-beneficiary end of directionality: open-source contributors and proprietary vendors both gain from a frame that legitimizes their preferred model without requiring them to defeat the other side ideologically; users and enterprise adopters gain optionality. No victim group is declared because the reading's defining structural claim is that no one is harmed by the mere existence of either model — that is precisely what distinguishes this reading from freedom_imperative_reading (which casts proprietary users as harmed) and from property_rights_reading (which could cast unauthorized copying/modification advocates as illegitimate). Free software advocates are marked excluded, not victim: their objection is to the frame itself, not to a cost the frame imposes on them.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (avoiding a mandatory ethical litmus test before any licensing decision) is authored as live, not dead — methodology disputes remain active in procurement, contribution, and licensing decisions today. This blocks a mandatrophy read: the coordination function this reading performs (letting engineering decisions proceed on empirical grounds) has not been superseded by events, so there is no zombie-mandate flag here, consistent with the low theater ratio.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    pragmatism_frame_neutrality,
    'Is ''pragmatic openness'' actually a neutral, evidence-based frame, or does treating licensing as a mere methodology choice already smuggle in a property-rights-friendly default (since it normalizes proprietary control as equally legitimate rather than treating it as requiring justification)?',
    'Compare adoption patterns and discourse framing across jurisdictions/eras with differing default IP regimes; if the ''pragmatic'' framing consistently favors incumbents with proprietary market power regardless of empirical quality outcomes, the neutrality claim weakens.',
    'If the frame is not neutral, this reading functions as a soft version of property_rights_reading wearing empirical language, which would raise its effective ε and potentially introduce an implicit beneficiary skew toward proprietary vendors.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pragmatism_frame_neutrality, conceptual, 'Whether pragmatic-openness is genuinely neutral or a disguised property-rights default.').

omega_variable(
    sibling_reading_foreclosure_scope,
    'Does accepting the pragmatic-openness reading as an organization''s operating frame functionally foreclose the freedom_imperative_reading in practice, even though the two readings are declared to logically coexist?',
    'Track whether organizations or communities that formally adopt pragmatic-openness policies subsequently treat freedom-imperative arguments as illegitimate or off-topic in internal decision-making, versus continuing to entertain them.',
    'If adoption functionally forecloses freedom_imperative_reading in practice despite the declared coexists_with relation, the reading_relations classification should be revisited toward a weaker ''influences'' or contested foreclosure edge.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure_scope, empirical, 'Whether practical adoption of this reading crowds out the freedom-imperative reading despite formal coexistence.').

omega_variable(
    quality_optimization_beneficiary_symmetry,
    'Do open-source contributors and proprietary vendors actually benefit symmetrically from this reading, or does the reading''s emphasis on ''quality via peer review'' quietly privilege open-source''s own justificatory story while proprietary vendors benefit only from the absence of moral condemnation?',
    'Survey how each group values the reading — as active vindication of their method''s superiority versus passive tolerance — and whether that asymmetry produces differential investment in defending the frame.',
    'Asymmetric benefit would suggest the beneficiary declaration should be split or weighted rather than treated as a flat symmetric list.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(quality_optimization_beneficiary_symmetry, conceptual, 'Whether the declared beneficiary symmetry between open and proprietary camps is genuine.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(software_control_legitimacy__pragmatic_openness_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(soft_tr_t0, software_control_legitimacy__pragmatic_openness_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(soft_tr_t6, software_control_legitimacy__pragmatic_openness_reading, theater_ratio, 6, 0.11).
narrative_ontology:measurement(soft_tr_t12, software_control_legitimacy__pragmatic_openness_reading, theater_ratio, 12, 0.12).
narrative_ontology:measurement(soft_tr_t18, software_control_legitimacy__pragmatic_openness_reading, theater_ratio, 18, 0.13).
narrative_ontology:measurement(soft_tr_t24, software_control_legitimacy__pragmatic_openness_reading, theater_ratio, 24, 0.14).
narrative_ontology:measurement(soft_tr_t30, software_control_legitimacy__pragmatic_openness_reading, theater_ratio, 30, 0.15).

% Extraction over time
narrative_ontology:measurement(soft_be_t0, software_control_legitimacy__pragmatic_openness_reading, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(soft_be_t6, software_control_legitimacy__pragmatic_openness_reading, base_extractiveness, 6, 0.1).
narrative_ontology:measurement(soft_be_t12, software_control_legitimacy__pragmatic_openness_reading, base_extractiveness, 12, 0.11).
narrative_ontology:measurement(soft_be_t18, software_control_legitimacy__pragmatic_openness_reading, base_extractiveness, 18, 0.11).
narrative_ontology:measurement(soft_be_t24, software_control_legitimacy__pragmatic_openness_reading, base_extractiveness, 24, 0.12).
narrative_ontology:measurement(soft_be_t30, software_control_legitimacy__pragmatic_openness_reading, base_extractiveness, 30, 0.12).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(software_control_legitimacy__pragmatic_openness_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(software_control_legitimacy__pragmatic_openness_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(software_control_legitimacy__pragmatic_openness_reading, 0.08).
narrative_ontology:affects_constraint(software_control_legitimacy__pragmatic_openness_reading, software_control_legitimacy__freedom_imperative_reading).
narrative_ontology:affects_constraint(software_control_legitimacy__pragmatic_openness_reading, software_control_legitimacy__property_rights_reading).
narrative_ontology:affects_constraint(software_control_legitimacy__pragmatic_openness_reading, software_control_legitimacy__commons_reading).

% DUAL FORMULATION NOTE:
% This story is one of four constraint stories decomposing the natural-language concept 'software control legitimacy' per the ε-invariance principle. Each sibling reading (freedom_imperative_reading, property_rights_reading, commons_reading) treats the same underlying phenomenon — who legitimately controls software — through a structurally distinct normative lens, producing different beneficiary/victim sets and different ε values. This reading (pragmatic_openness_reading) is distinguished by declaring no victims and the lowest extractiveness of the four, reflecting its constitutive claim that both open and proprietary models are legitimate. All four are linked bidirectionally as siblings in the kernel; contamination or drift in one reading's classification is informative for, but does not determine, the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
