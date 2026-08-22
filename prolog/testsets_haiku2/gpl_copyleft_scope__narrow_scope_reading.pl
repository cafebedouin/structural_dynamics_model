% ============================================================================
% CONSTRAINT STORY: gpl_copyleft_scope__narrow_scope_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:affects_constraint/2,
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
 *   constraint_id: gpl_copyleft_scope__narrow_scope_reading
 *   human_readable: GPL Section 2(b) Narrow Copyleft Scope Reading
 *   domain: software_licensing/intellectual_property/open_source_governance
 *
 * SUMMARY:
 *   GPL Section 2(b) specifies that copyleft obligations apply to 'derivative
 *   works,' a term inherited from traditional copyright law but ambiguous in
 *   software contexts. The narrow-scope reading interprets 'derivative work'
 *   strictly: plugin architectures, dynamic linking, and mere code
 *   aggregation do NOT trigger copyleft, only direct textual modification or
 *   static linking of modified source does. This reading, endorsed implicitly
 *   by the FSF's own FAQ and explicitly by commercial firms that have built
 *   decades of business on it, permits proprietary firms to integrate GPL
 *   code while retaining proprietary extensions. The reading is contested by
 *   enforcement-maximalist advocates who argue GPL's founding vision required
 *   universal code-sharing. No binding judicial precedent exists; the reading
 *   coexists with the strong-copyleft reading in different interpretive
 *   communities. This story instantiates the narrow-scope reading as a clean,
 *   ε-invariant constraint: moderate extraction (proprietary firms gain
 *   flexibility; free-software contributors bear disclosure asymmetry),
 *   rope-classified (coordination mechanism for mixed-license ecosystems),
 *   stable over the interval (the reading has held since the early 2000s with
 *   minimal legal pressure).
 *
 * KEY AGENTS:
 *   - proprietary_software_firms: Structural beneficiaries (use GPL code without triggering forced disclosure) — institutional power, arbitrage-grade exit options
 *   - free_software_advocates: Structural payers (copyleft scope weakened, code-sharing expectations undermined) — organized power, constrained exit (litigation is expensive, relicensing is complex)
 *   - software_foundation_stewards: Agenda-setters (interpret and enforce Section 2(b) scope) — institutional power, analytical exit (can shift interpretation over time)
 *   - independent_developers: Payers (improvements captured in proprietary extensions, cannot enforce reciprocal licensing) — moderate power, constrained exit
 *   - end_users: Beneficiaries + payers (gain from ecosystem stability, lose from reduced transparency) — powerless, trapped exit
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gpl_copyleft_scope__narrow_scope_reading, 0.42).
domain_priors:suppression_score(gpl_copyleft_scope__narrow_scope_reading, 0.28).
domain_priors:theater_ratio(gpl_copyleft_scope__narrow_scope_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gpl_copyleft_scope__narrow_scope_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(gpl_copyleft_scope__narrow_scope_reading, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(gpl_copyleft_scope__narrow_scope_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gpl_copyleft_scope__narrow_scope_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(gpl_copyleft_scope__narrow_scope_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gpl_copyleft_scope__narrow_scope_reading, rope).
narrative_ontology:human_readable(gpl_copyleft_scope__narrow_scope_reading, "GPL Section 2(b) Narrow Copyleft Scope Reading").
narrative_ontology:topic_domain(gpl_copyleft_scope__narrow_scope_reading, "software_licensing/intellectual_property/open_source_governance").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gpl_copyleft_scope__narrow_scope_reading, '7fb83508-0428-4318-bfa6-ab0180814ce4').
narrative_ontology:cs_kernel_codification('7fb83508-0428-4318-bfa6-ab0180814ce4', fixed_text).
narrative_ontology:cs_authority_grounding('7fb83508-0428-4318-bfa6-ab0180814ce4', lineage).
narrative_ontology:cs_interpretation_layer_present('7fb83508-0428-4318-bfa6-ab0180814ce4').
narrative_ontology:cs_reading_relation('7fb83508-0428-4318-bfa6-ab0180814ce4', gpl_copyleft_scope__strong_copyleft_reading, coexists_with).
narrative_ontology:cs_reading_relation('7fb83508-0428-4318-bfa6-ab0180814ce4', gpl_copyleft_scope__enforcement_vacuum_reading, influences).
narrative_ontology:cs_axiom('7fb83508-0428-4318-bfa6-ab0180814ce4', foundational, derivative_work_boundary_follows_copyright_doctrine).
narrative_ontology:cs_axiom_status(derivative_work_boundary_follows_copyright_doctrine, holdable).
narrative_ontology:cs_axiom_grounding('7fb83508-0428-4318-bfa6-ab0180814ce4', derivative_work_boundary_follows_copyright_doctrine, conventional).
narrative_ontology:cs_axiom('7fb83508-0428-4318-bfa6-ab0180814ce4', secondary, plugin_and_dynamic_linking_exclude_copyleft_obligation).
narrative_ontology:cs_axiom_status(plugin_and_dynamic_linking_exclude_copyleft_obligation, holdable).
narrative_ontology:cs_axiom_grounding('7fb83508-0428-4318-bfa6-ab0180814ce4', plugin_and_dynamic_linking_exclude_copyleft_obligation, empirically_contingent).
narrative_ontology:cs_reference_frame('7fb83508-0428-4318-bfa6-ab0180814ce4', traditional_copyright_law_framework).
narrative_ontology:cs_drift_state('7fb83508-0428-4318-bfa6-ab0180814ce4', contemporary_commercial_software_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('7fb83508-0428-4318-bfa6-ab0180814ce4', '').
narrative_ontology:cs_kernel_id(gpl_copyleft_scope__narrow_scope_reading, gpl_copyleft_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gpl_copyleft_scope__narrow_scope_reading, proprietary_software_firms).
narrative_ontology:constraint_beneficiary(gpl_copyleft_scope__narrow_scope_reading, mixed_codebase_developers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(gpl_copyleft_scope__narrow_scope_reading, end_users).
narrative_ontology:constraint_beneficiary(gpl_copyleft_scope__narrow_scope_reading, commercial_enterprises_benefiting_from_proprietary_extensions).
narrative_ontology:constraint_victim(gpl_copyleft_scope__narrow_scope_reading, free_software_advocates).
narrative_ontology:constraint_victim(gpl_copyleft_scope__narrow_scope_reading, end_users).
narrative_ontology:constraint_victim(gpl_copyleft_scope__narrow_scope_reading, independent_developers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Can integrate GPL-licensed components (libraries, tools) into proprietary applications via plugin architectures, dynamic linking, or aggregation without triggering Section 2(b) re-licensing requirements. This reading permits them to use GPL code as a coordinated component while retaining their proprietary layer, avoiding forced disclosure of their own source. Commercial viability of mixed-license integration depends entirely on this reading's scope boundary.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__narrow_scope_reading, proprietary_software_firms, beneficiary,
    powerful, generational, arbitrage, global).

% Can build products combining GPL components with proprietary or closed extensions without legal uncertainty about copyleft obligations. The narrow scope reading permits them to coordinate code across license boundaries by selecting appropriate linking/integration patterns. Their market niche (embedded systems, mobile apps, enterprise software) depends on this flexibility.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__narrow_scope_reading, mixed_codebase_developers, beneficiary,
    organized, biographical, mobile, global).

% Bear the structural cost of weakened copyleft scope: GPL improvements made by proprietary firms remain proprietary, network effects favor closed-source integration patterns, code-sharing expectations (central to GPL's founding vision) are undermined. Their power to enforce broader readings through litigation is limited by this reading's alignment with traditional copyright doctrine; judicial precedent reinforces the narrow scope.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__narrow_scope_reading, free_software_advocates, payer,
    organized, generational, constrained, global).

% Gain from the coordination function (stable GPL ecosystem with viable commercial participation), but bear the cost of reduced transparency: proprietary firms can conceal their own improvements to GPL code, limiting the information end-users would have about their software's true composition and modification history. Their choice set is constrained to whatever proprietary firms choose to release.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__narrow_scope_reading, end_users, beneficiary,
    powerless, immediate, trapped, global).
narrative_ontology:stakeholder_secondary_role(gpl_copyleft_scope__narrow_scope_reading, end_users, payer).

% Work at a structural disadvantage: proprietary firms can integrate GPL code without reciprocal disclosure, while independent developers who release under GPL have their improvements captured in proprietary extensions. Exit options are limited—licensing under other terms abandons GPL ecosystem participation, relicensing is legally complex and politically fraught, enforcement is expensive.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__narrow_scope_reading, independent_developers, payer,
    moderate, biographical, constrained, global).

% Administer GPL-licensed projects and interpret the scope of Section 2(b) in specific enforcement contexts. Under the narrow-scope reading, they accept plugin/aggregation/dynamic-linking patterns without asserting broader copyleft obligations. Their enforcement posture is permissive; they lose power to mandate universal code-sharing.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__narrow_scope_reading, software_foundation_stewards, agenda_setter,
    institutional, generational, analytical, global).

% Benefit from the reading's permission to create proprietary extensions atop GPL code without triggering downstream copyleft obligations. Their competitive advantage—differentiation through closed-source plugins or enhancements—is preserved. They benefit from GPL coordination (stable upstream) while avoiding its reciprocal demands.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__narrow_scope_reading, commercial_enterprises_benefiting_from_proprietary_extensions, beneficiary,
    powerful, generational, arbitrage, global).

% Would prefer a reading that extends copyleft to dynamic linking and plugin architectures but are excluded from the operative interpretation. Judicial deference to traditional copyright doctrine leaves them without legal standing to argue for broader scope. Their preferred reading coexists as a competing normative claim but lacks institutional enforcement capacity in the dominant commercial ecosystem.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__narrow_scope_reading, litigation_advocates_for_strong_copyleft, excluded,
    moderate, biographical, constrained, national).

% Have issued sparse definitive rulings on Section 2(b) scope boundaries; the narrow reading's alignment with traditional copyright doctrine means courts defer to established IP law rather than resolving the GPL-specific contest. Absence of binding precedent on dynamic linking leaves the constraint's enforcement machinery underdetermined, enabling coexistence of readings in different jurisdictions.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__narrow_scope_reading, intellectual_property_courts, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gpl_copyleft_scope__narrow_scope_reading, proprietary_software_firms).
narrative_ontology:fixing_cost_class(gpl_copyleft_scope__narrow_scope_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable licensing boundary permitting mixed-license software integration: proprietary firms can use GPL components (libraries, frameworks, tools) in composite systems without triggering universal re-licensing, enabling a viable commercial software ecosystem that includes GPL code without forcing all participants into reciprocal disclosure. Solves the coordination problem: how can GPL code and proprietary code coexist in the same application?
% TRANSFER_FUNCTION: Moves the burden of disclosure and transparency asymmetrically: GPL-licensed code improvements made by proprietary firms are retained as proprietary assets; independent developers who improve GPL code have their improvements freely available to competitors. The extraction flows from free-software contributors to closed-source commercial beneficiaries.
% ABSENT_VOICES: Enforcement-maximalist free-software advocates (those who believe dynamic linking should trigger copyleft) are excluded from the operative interpretation; they argue the reading undermines GPL's founding vision but lack institutional enforcement capacity in industry-dominated contexts. End-users who would benefit from transparency about proprietary modifications to GPL code are also excluded from the binding scope decision.
% DISAPPEARANCE_RATIONALE: If the narrow-scope reading disappeared (replaced by the strong-copyleft reading), the commercial software ecosystem would reorganize: proprietary firms would either relicense their products under GPL (losing proprietary differentiation), abandon GPL components entirely (fragmenting ecosystems), or move to non-copyleft licensing (reducing GPL's scope). The entire structure of mixed-license integration depends on this reading's scope boundary.
% FOUNDING_PROBLEM: GPL Section 2(b)'s text ('derivative work') was inherited from traditional copyright law but left ambiguous in the software context: does plugin architecture create a derivative work? Does dynamic linking? Does mere aggregation? Different stakeholders interpreted the same text to mean very different things (strong copyleft advocates: yes to all; commercial firms: no to most). The reading provides a stable interpretation by applying traditional copyright doctrine rather than expanding GPL-specific semantics.
% FOUNDING_PROBLEM_CORROBORATION: The Free Software Foundation (GPL's steward) has formally stated that mere aggregation and plugin architectures do not trigger Section 2(b) in a 2014 GPL FAQ clarification, supporting the narrow-scope reading. Commercial software firms (Red Hat, Canonical, Percona) have built business models explicitly on the assumption that this reading is correct, and have operated for decades without legal challenge. Copyleft advocates contest this, arguing the reading was never GPL's intent; but no major appellate court has ruled definitively, and absence of binding precedent permits the coexistence. FSF's own position has modulated toward pragmatism over time, implicitly corroborating the narrow reading through non-enforcement of broader interpretations.
narrative_ontology:disappearance_verdict(gpl_copyleft_scope__narrow_scope_reading, world_rearranges).
narrative_ontology:founding_problem_status(gpl_copyleft_scope__narrow_scope_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gpl_copyleft_scope__narrow_scope_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(gpl_copyleft_scope__narrow_scope_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gpl_copyleft_scope__narrow_scope_reading, 0.42, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is moderate (0.42) because the constraint transfers value from GPL contributors to proprietary beneficiaries in a specific and measurable way: GPL improvements made by proprietary firms remain proprietary, while improvements made by independent developers are available to all competitors. But the extraction is not pure—there IS genuine coordination (mixed-license integration works, commercial participation sustains GPL ecosystems, proprietary firms contribute bug fixes and maintenance back upstream). Suppression is low (0.28) because the constraint's enforcement depends almost entirely on traditional copyright doctrine; proprietary firms do not actively suppress alternatives (the constraint simply permits their preferred interpretation) and free-software advocates retain the right to pursue strong-copyleft readings through litigation or community practice. Theater is low (0.15) because the narrow scope reading aligns with established IP law, not performative ritual; there is minimal theatrical apparatus maintaining it—alignment with copyright doctrine IS the sustaining mechanism. Accessibility collapse is moderate (0.48) because alternatives do exist: firms can relicense under non-copyleft terms, release all code as open-source (accepting copyleft), or avoid GPL entirely. These are costly but not impossible. Resistance is moderate-to-high (0.62) because free-software advocates actively contest the reading through public argument, licensing demands on projects they steward, and ongoing litigation theories—the reading persists despite real resistance, not because resistance is absent. The measurement series is nearly flat because the narrow reading stabilized in the early 2000s (roughly time_point 0 in this interval's abstract chronology) and has not substantially changed; the slight rise in extraction and suppression reflects gradual hardening as proprietary adoption entrenched the reading as operational fact.
 *
 * PERSPECTIVAL GAP:
 *   The proprietary firm seats and the free-software advocate seats compute different types from the same structural data. From the firm's perspective, this is genuine rope: solving a real coordination problem (how to mix licenses), achieving net positive outcomes for the sector they care about (commercial viability of GPL integration), requiring no heavy enforcement machinery (just alignment with copyright doctrine). From the advocate's perspective, this is extractive rope-shading-toward-snare: the coordination function is real but asymmetrically distributed (benefiting firms, harming independent developers), enforcement is one-sided (firms enforce their preferred interpretation through IP law, advocates lack institutional capacity to enforce theirs), and the constraint persists despite resistance (not because all parties prefer it). The narrow reading's alignment with traditional copyright doctrine is precisely what permits this divergence: courts defer to established IP principles rather than GPL-specific equity concerns, so different stakeholders experience fundamentally different institutional support for their preferred readings.
 *
 * DIRECTIONALITY LOGIC:
 *   Proprietary firms (d near 0.1: beneficiary end) gain flexibility and arbitrage options; they can opt into GPL where convenient and opt out where valuable proprietary differentiation exists. Free-software advocates (d near 0.8: target end) lose scope and enforcement capacity; their preferred interpretation is structurally disadvantaged by deference to copyright doctrine. Independent developers (d near 0.65: toward target) are middle-positioned: they benefit from the ecosystem coordination but bear extraction through captured improvements. Software foundation stewards (d near 0.4: near symmetric) sit at the intersection: they steward GPL projects (beneficiary-like) but lack enforcement power against proprietary interpretations (target-like). The directionality structure is NOT overridden; the structural data (beneficiary/victim declarations + exit options + power atoms) drives the derivation correctly.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (GPL Section 2(b) ambiguity in software contexts) is CONTESTED in its status: proprietary firms argue it is LIVE and the narrow reading solves it; copyleft advocates argue it is DEAD (the real problem was always enforcement capacity, not textual interpretation) and the reading perpetuates the zombie constraint by obscuring the real power asymmetry. This mismatch (founding_problem_status=contested, disappearance_verdict=world_rearranges) does NOT fire mandatrophy (which requires status=dead + verdict=world_rearranges). However, the underlying structure is mandatrophic in spirit: the constraint persists not because all parties prefer it, but because one party (proprietary firms) has institutional support (copyright doctrine) for their preferred reading, while the other party (free-software advocates) lacks enforcement capacity. The constraint is a zombie in the sense that copyleft's founding vision (universal code-sharing) is systematically undermined, but maintained through doctrinal alignment rather than active maintenance. Omega variables handle the framing under-determination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    derivative_work_boundary_under_determination,
    'Is the narrow reading''s definition of ''derivative work'' (traditional copyright doctrine) the CORRECT reading of GPL Section 2(b), or is it a post-hoc rationalization for what proprietary firms found convenient?',
    'Definitive judicial precedent explicitly addressing GPL copyleft scope and dynamic linking; or discovery of original GPL author intent through historical documentation (Stallman''s notes, early committee discussion, GPLv2 drafting history). The Free Software Foundation''s authoritative restatement of Section 2(b) intent would carry substantial weight.',
    'If the narrow reading is the CORRECT interpretation, the constraint is legitimate rope—stable coordination mechanism. If it is post-hoc rationalization, the constraint is snare with a false-legitimacy cover story (real intent was universal code-sharing, but institutional power shifts the operative reading toward proprietary flexibility). This is OQ-26 compatible: the same reading, different ε values depending on whether the intent-axiology is satisfied.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(derivative_work_boundary_under_determination, conceptual, 'Whether traditional copyright law is the appropriate framework for GPL derivative-work definition, or whether GPL is a distinct copyright regime requiring different semantics.').

omega_variable(
    suppression_mechanism_structural_vs_institutional,
    'Is the low measured suppression (0.28) actually LOW because enforcement is minimal, or is it LOW because the constraint works through institutional alignment (copyright doctrine deference) rather than active suppression of alternatives?',
    'Empirical comparison: (a) count litigation instances where firms or advocates explicitly defend or challenge the narrow-scope reading; (b) measure the frequency with which GPL projects adopt strong-copyleft reinterpretations (AGPL, GPL-with-stronger-language) as a sign of active resistance to the narrow reading; (c) examine whether free-software developers alter their licensing practices in response to the narrow reading''s institutional entrenchment.',
    'If suppression is structural (copyright doctrine passive deference = natural-law appearance), the constraint is more durable and less obviously extractive than the suppression metric suggests. If suppression is institutional (active judicial/legal resource deployment to reinforce the reading), the low suppression metric masks institutional coercion. Either way, the constraint''s persistence mechanism is clarified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_institutional, empirical, 'Whether low suppression reflects genuine absence of enforcement, or institutional background conditions that make explicit suppression unnecessary.').

omega_variable(
    reading_committer_coexistence_or_foreclosure,
    'Do the narrow-scope and strong-copyleft readings COEXIST as genuinely live positions in contemporary practice, or has the narrow reading''s institutional entrenchment effectively FORECLOSED the strong reading from commercial software development (even though it remains live in ideological free-software communities)?',
    'Longitudinal survey of commercial GPL-using firms: do they treat the strong-copyleft reading as a live legal threat (purchasing insurance, restructuring their integration patterns), or as a theoretical position held by ideological opponents but not operationally constraining? Similarly, survey free-software projects: what share adopt strong-copyleft-aligned licensing despite the narrow reading''s institutional dominance?',
    'True coexistence implies both readings remain strategically live (one seat may prefer strong, another narrow, both persist). Foreclosure implies the narrow reading has achieved near-monopoly on commercial practice, with the strong reading remaining only as an ideological position. This affects how the constraint''s persistence should be explained: stable equilibrium versus institutional-power-shifted-outcome.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_committer_coexistence_or_foreclosure, empirical, 'Whether the narrow and strong copyleft readings are symmetrically live, or whether the narrow reading has de facto foreclosed strong-copyleft commercial viability.').

omega_variable(
    identity_lock_in_free_software_advocacy,
    'How much of the measured resistance (0.62) by free-software advocates reflects strategic disagreement with the narrow reading versus identity-locked commitment to copyleft as a defining principle of their movement?',
    'Post-exit analysis: if copyleft advocates were to shift to non-copyleft licensing or accept mixed-license integration, would their resistance to the narrow reading persist? If the resistance dissolves after exit from GPL advocacy, the identity lock is primary. If resistance remains (advocates still argue for broad copyleft even in non-GPL contexts), the disagreement is structural, not identity-fused.',
    'If identity-locked, suppression and exit-options estimates may be underestimating the true constraint''s grip on advocates—they remain in resistance despite high cost because their professional identity is fused with copyleft maximalism. If structural, the resistance is more contingent and could shift with institutional conditions or legal precedent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_in_free_software_advocacy, empirical, 'Whether free-software resistance to the narrow reading is grounded in structural disagreement or identity-locked commitment to copyleft ideology.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gpl_copyleft_scope__narrow_scope_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gpl__tr_t0, gpl_copyleft_scope__narrow_scope_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(gpl__tr_t5, gpl_copyleft_scope__narrow_scope_reading, theater_ratio, 5, 0.1).
narrative_ontology:measurement(gpl__tr_t10, gpl_copyleft_scope__narrow_scope_reading, theater_ratio, 10, 0.12).
narrative_ontology:measurement(gpl__tr_t15, gpl_copyleft_scope__narrow_scope_reading, theater_ratio, 15, 0.14).
narrative_ontology:measurement(gpl__tr_t20, gpl_copyleft_scope__narrow_scope_reading, theater_ratio, 20, 0.15).
narrative_ontology:measurement(gpl__tr_t25, gpl_copyleft_scope__narrow_scope_reading, theater_ratio, 25, 0.15).
narrative_ontology:measurement(gpl__tr_t30, gpl_copyleft_scope__narrow_scope_reading, theater_ratio, 30, 0.15).

% Extraction over time
narrative_ontology:measurement(gpl__be_t0, gpl_copyleft_scope__narrow_scope_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(gpl__be_t5, gpl_copyleft_scope__narrow_scope_reading, base_extractiveness, 5, 0.4).
narrative_ontology:measurement(gpl__be_t10, gpl_copyleft_scope__narrow_scope_reading, base_extractiveness, 10, 0.41).
narrative_ontology:measurement(gpl__be_t15, gpl_copyleft_scope__narrow_scope_reading, base_extractiveness, 15, 0.42).
narrative_ontology:measurement(gpl__be_t20, gpl_copyleft_scope__narrow_scope_reading, base_extractiveness, 20, 0.42).
narrative_ontology:measurement(gpl__be_t25, gpl_copyleft_scope__narrow_scope_reading, base_extractiveness, 25, 0.42).
narrative_ontology:measurement(gpl__be_t30, gpl_copyleft_scope__narrow_scope_reading, base_extractiveness, 30, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(gpl__su_t0, gpl_copyleft_scope__narrow_scope_reading, suppression_requirement, 0, 0.22).
narrative_ontology:measurement(gpl__su_t5, gpl_copyleft_scope__narrow_scope_reading, suppression_requirement, 5, 0.24).
narrative_ontology:measurement(gpl__su_t10, gpl_copyleft_scope__narrow_scope_reading, suppression_requirement, 10, 0.25).
narrative_ontology:measurement(gpl__su_t15, gpl_copyleft_scope__narrow_scope_reading, suppression_requirement, 15, 0.27).
narrative_ontology:measurement(gpl__su_t20, gpl_copyleft_scope__narrow_scope_reading, suppression_requirement, 20, 0.28).
narrative_ontology:measurement(gpl__su_t25, gpl_copyleft_scope__narrow_scope_reading, suppression_requirement, 25, 0.28).
narrative_ontology:measurement(gpl__su_t30, gpl_copyleft_scope__narrow_scope_reading, suppression_requirement, 30, 0.28).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gpl_copyleft_scope__narrow_scope_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(gpl_copyleft_scope__narrow_scope_reading, 0.12).
narrative_ontology:affects_constraint(gpl_copyleft_scope__narrow_scope_reading, gpl_copyleft_scope__strong_copyleft_reading).
narrative_ontology:affects_constraint(gpl_copyleft_scope__narrow_scope_reading, gpl_copyleft_scope__enforcement_vacuum_reading).
narrative_ontology:affects_constraint(gpl_copyleft_scope__narrow_scope_reading, proprietary_software_integration_patterns).
narrative_ontology:affects_constraint(gpl_copyleft_scope__narrow_scope_reading, dynamic_linking_legal_uncertainty).

% DUAL FORMULATION NOTE:
% The GPL copyleft-scope kernel decomposes into three distinct constraint stories: (1) narrow-scope-reading: traditional copyright doctrine applied, moderate extraction, rope-classified, institutional alignment favors this reading. (2) strong-copyleft-reading: GPL creates a distinct copyright regime requiring universal code-sharing, higher extraction, tangled-rope-classified, marginalized in commercial practice. (3) enforcement-vacuum-reading: absence of binding precedent permits both readings to coexist in parallel across jurisdictions and communities, zero meaningful classification (pure uncertainty). Each reading has a different ε (the standing arrangement under contest, assessed by that reading's own lights), different victim/beneficiary structures, and different institutional support. They are linked via network edges; the ε-invariance principle requires separate constraint stories rather than one story with observable-dependent classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(gpl_copyleft_scope__narrow_scope_reading, institutional, 0.42).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
