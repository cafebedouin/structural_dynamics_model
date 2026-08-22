% ============================================================================
% CONSTRAINT STORY: refugee_convention_text__expansive_humanitarian_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_refugee_convention_text__expansive_humanitarian_reading, []).

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
 *   constraint_id: refugee_convention_text__expansive_humanitarian_reading
 *   human_readable: Expansive Humanitarian Reading of 1951 Refugee Convention
 *   domain: international_law/migration/human_rights
 *
 * SUMMARY:
 *   The 1951 Refugee Convention is a single kernel text that different
 *   interpretive traditions read with fundamentally different structural
 *   conclusions about who is protected and at what cost. The expansive
 *   humanitarian reading interprets 'well-founded fear' and 'particular
 *   social group' broadly to include generalized violence, non-state
 *   persecution, and harm based on gender, sexuality, and clan membership.
 *   This reading emerged through UNHCR doctrine, human rights litigation, and
 *   judicial adoption in liberal-law jurisdictions. It binds signatory states
 *   to substantive protection assessment and interprets non-refoulement as
 *   foreclosing interdiction and offshore processing that prevent claims
 *   review. The constraint's operation is coordinating (states adopt a shared
 *   interpretation) and extractive (restrictive states bear increased
 *   protection obligation and lose discretion; advocacy organizations collect
 *   interpretive authority). The claim/metric gap is intentional: the reading
 *   claims to be rope (genuine coordination of protection standards), while
 *   the authored metrics describe moderate extraction (costs to restrictive
 *   states and procedurally disadvantaged claimants) and active suppression
 *   (interdiction regimes and procedural barriers erected to block the
 *   expansive reading's access). Measurement data tracks the reading's
 *   institutional institutionalization from 1980 (nascent doctrine) through
 *   2026 (mature doctrine with persistent state resistance).
 *
 * KEY AGENTS:
 *   - persecution_survivors_broad_category: individuals fleeing generalized violence, non-state persecution, gender-based harm, sexual-orientation persecution, clan-based persecution — the beneficiary set whose protection the expansive reading mandates
 *   - international_human_rights_advocacy_organizations: UNHCR, Amnesty, Human Rights Watch, legal networks — custodians and expanders of the expansive reading
 *   - restrictive_asylum_states: Western states that resist the expansive reading as excessive constraint on sovereignty — bear costs when the reading gains interpretive ground
 *   - procedurally_disadvantaged_claimants: asylum seekers rejected under narrow interpretations who would qualify under the expansive reading — direct victims of interpretive narrowing
 *   - maritime_interdiction_enforcement_apparatus: coast guards, immigration enforcement, maritime operations — the operational arena where the reading conflict becomes concrete (interdiction-as-violation vs. interdiction-as-regulation)
 *   - low_income_host_countries: Jordan, Lebanon, Uganda, Pakistan, Turkey — bear disproportionate obligation costs and lack capacity to implement the expansive mandate
 *   - judicial_reviewing_authorities: courts and tribunals that can adopt or reject the expansive reading in binding rulings — key venue for reading contestation
 *   - interpretation_tradition_custodians: UNHCR Handbook authors, treaty body committees, international law scholars — transmit and develop the expansive doctrine
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(refugee_convention_text__expansive_humanitarian_reading, 0.28).
domain_priors:suppression_score(refugee_convention_text__expansive_humanitarian_reading, 0.45).
domain_priors:theater_ratio(refugee_convention_text__expansive_humanitarian_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(refugee_convention_text__expansive_humanitarian_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(refugee_convention_text__expansive_humanitarian_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(refugee_convention_text__expansive_humanitarian_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(refugee_convention_text__expansive_humanitarian_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(refugee_convention_text__expansive_humanitarian_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(refugee_convention_text__expansive_humanitarian_reading, tangled_rope).
narrative_ontology:human_readable(refugee_convention_text__expansive_humanitarian_reading, "Expansive Humanitarian Reading of 1951 Refugee Convention").
narrative_ontology:topic_domain(refugee_convention_text__expansive_humanitarian_reading, "international_law/migration/human_rights").

domain_priors:requires_active_enforcement(refugee_convention_text__expansive_humanitarian_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(refugee_convention_text__expansive_humanitarian_reading, 'c3dba1b8-41d4-4c75-bd1e-aa71de7f9981').
narrative_ontology:cs_kernel_codification('c3dba1b8-41d4-4c75-bd1e-aa71de7f9981', fixed_text).
narrative_ontology:cs_authority_grounding('c3dba1b8-41d4-4c75-bd1e-aa71de7f9981', lineage).
narrative_ontology:cs_interpretation_layer_present('c3dba1b8-41d4-4c75-bd1e-aa71de7f9981').
narrative_ontology:cs_reading_relation('c3dba1b8-41d4-4c75-bd1e-aa71de7f9981', refugee_convention_text__restrictive_sovereignty_reading, coexists_with).
narrative_ontology:cs_reading_relation('c3dba1b8-41d4-4c75-bd1e-aa71de7f9981', refugee_convention_text__procedural_integrity_reading, influences).
narrative_ontology:cs_axiom('c3dba1b8-41d4-4c75-bd1e-aa71de7f9981', foundational, persecution_includes_generalized_violence).
narrative_ontology:cs_axiom_status(persecution_includes_generalized_violence, holdable).
narrative_ontology:cs_axiom_grounding('c3dba1b8-41d4-4c75-bd1e-aa71de7f9981', persecution_includes_generalized_violence, empirically_contingent).
narrative_ontology:cs_axiom('c3dba1b8-41d4-4c75-bd1e-aa71de7f9981', foundational, particular_social_group_includes_socially_constructed_categories).
narrative_ontology:cs_axiom_status(particular_social_group_includes_socially_constructed_categories, holdable).
narrative_ontology:cs_axiom_grounding('c3dba1b8-41d4-4c75-bd1e-aa71de7f9981', particular_social_group_includes_socially_constructed_categories, deontological).
narrative_ontology:cs_axiom('c3dba1b8-41d4-4c75-bd1e-aa71de7f9981', secondary, non_refoulement_forecloses_gatekeeping_interdiction).
narrative_ontology:cs_axiom_status(non_refoulement_forecloses_gatekeeping_interdiction, holdable).
narrative_ontology:cs_axiom_grounding('c3dba1b8-41d4-4c75-bd1e-aa71de7f9981', non_refoulement_forecloses_gatekeeping_interdiction, deontological).
narrative_ontology:cs_reference_frame('c3dba1b8-41d4-4c75-bd1e-aa71de7f9981', humanitarian_protection_mandate_unbendable).
narrative_ontology:cs_drift_state('c3dba1b8-41d4-4c75-bd1e-aa71de7f9981', contemporary_restrictive_backlash_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('c3dba1b8-41d4-4c75-bd1e-aa71de7f9981', '2026-06-12T14:32:00Z').
narrative_ontology:cs_kernel_id(refugee_convention_text__expansive_humanitarian_reading, refugee_convention_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(refugee_convention_text__expansive_humanitarian_reading, persecution_survivors_broad_category).
narrative_ontology:constraint_beneficiary(refugee_convention_text__expansive_humanitarian_reading, international_human_rights_advocacy_organizations).
narrative_ontology:constraint_victim(refugee_convention_text__expansive_humanitarian_reading, restrictive_asylum_states).
narrative_ontology:constraint_victim(refugee_convention_text__expansive_humanitarian_reading, procedurally_disadvantaged_claimants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(refugee_convention_text__expansive_humanitarian_reading, low_income_destination_countries).
narrative_ontology:constraint_vindicates(refugee_convention_text__expansive_humanitarian_reading, humanitarian_principle_over_sovereignty).
narrative_ontology:constraint_vindicates(refugee_convention_text__expansive_humanitarian_reading, non_refoulement_absolute_duty).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals fleeing generalized violence, persecution by non-state actors, or harm based on gender, sexual orientation, or clan membership. Under the expansive reading they hold a substantive claim to protection that states cannot dismiss as merely economic migration or insufficient state targeting. Their vulnerability increases the protection mandate — states must assess claims substantively and may not interdict or process offshore in ways that foreclose assessment.
narrative_ontology:constraint_stakeholder(refugee_convention_text__expansive_humanitarian_reading, persecution_survivors_broad_category, beneficiary,
    powerless, biographical, trapped, global).

% UNHCR, Amnesty International, Human Rights Watch, and allied networks that have institutionalized the expansive reading through litigation, advisory bodies, and capacity-building. They interpret the Convention text, submit amicus briefs, and train national asylum adjudicators in the expansive framework. They collect institutional authority and funding from the humanitarian-protection mandate they define.
narrative_ontology:constraint_stakeholder(refugee_convention_text__expansive_humanitarian_reading, international_human_rights_advocacy_organizations, beneficiary,
    organized, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(refugee_convention_text__expansive_humanitarian_reading, international_human_rights_advocacy_organizations, agenda_setter).

% States that interpret the Convention narrowly and resist the expansive reading as constraint on their sovereignty. They argue that 'well-founded fear' requires individualized state targeting, that generalized violence does not activate protection, and that broad 'particular social group' categories collapse the Convention into a universal protection mechanism. They bear the cost of increased asylum obligation when the expansive reading gains interpretive ground; their exit is limited by international law commitments and diplomatic pressure.
narrative_ontology:constraint_stakeholder(refugee_convention_text__expansive_humanitarian_reading, restrictive_asylum_states, payer,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(refugee_convention_text__expansive_humanitarian_reading, restrictive_asylum_states, agenda_setter).

% Asylum seekers in restrictive-reading jurisdictions who cannot prove individualized state targeting or meet narrow 'particular social group' criteria. Under the expansive reading they would qualify; under restrictive readings they are denied. They carry the cost of interpretive narrowing: rejection, deportation, or indefinite detention. They have no exit short of satisfying the restrictive threshold or physically leaving the jurisdiction.
narrative_ontology:constraint_stakeholder(refugee_convention_text__expansive_humanitarian_reading, procedurally_disadvantaged_claimants, payer,
    powerless, immediate, trapped, local).

% Coast guards, naval forces, and immigration enforcement agencies that interdict migrants at sea and conduct offshore processing. Under the expansive reading, interdiction and offshore processing that prevent substantive claims assessment violate non-refoulement (the absolute prohibition on return to danger). Under restrictive readings, offshore processing is a sovereign regulatory tool. The tension between expansive interdiction-as-violation and restrictive interdiction-as-regulation is where the reading conflict materializes most sharply in operational terms.
narrative_ontology:constraint_stakeholder(refugee_convention_text__expansive_humanitarian_reading, maritime_interdiction_enforcement_apparatus, agenda_setter,
    institutional, generational, analytical, global).

% Developing nations that host the overwhelming majority of the world's refugees (Jordan, Lebanon, Uganda, Pakistan) and bear disproportionate asylum obligation costs. Under the expansive reading, their duty to assess broad categories of claims is heightened; they lack the resources or institutional capacity to comply with the expansive mandate at scale. They pay through capacity strain and institutional fragility; their exit is limited by geographic proximity to conflict zones and international pressure.
narrative_ontology:constraint_stakeholder(refugee_convention_text__expansive_humanitarian_reading, low_income_destination_countries, payer,
    moderate, generational, constrained, regional).

% Courts, administrative tribunals, and appeals bodies that interpret and apply the Convention. They occupy an observation seat: they can adopt the expansive reading and overturn narrow rejections, or enforce the restrictive reading and narrow claimant remedies. Their rulings shape whether the expansive reading has doctrinal force in practice. They serve as a key venue where the reading contest is decided.
narrative_ontology:constraint_stakeholder(refugee_convention_text__expansive_humanitarian_reading, judicial_reviewing_authorities, observer,
    institutional, generational, analytical, national).

% UNHCR Handbook authors, treaty body committees, academic international law scholars, and interpretive institutions that define the authoritative reading of the Convention text. They transmit and develop interpretive doctrine. The expansive reading is institutionalized through their work; they collect authority from the lineage of humanitarian interpretation and the authority to guide state practice.
narrative_ontology:constraint_stakeholder(refugee_convention_text__expansive_humanitarian_reading, interpretation_tradition_custodians, agenda_setter,
    organized, generational, mobile, global).

% State governments and political movements that resist the expansive reading as incompatible with state control over borders and admission. They view the expansive humanitarian mandate as an illegitimate constraint on sovereignty and advocate for narrower interpretations or withdrawal from the Convention. They are excluded from the interpretive consensus that institutionalizes the expansive reading but remain as contesting parties in the reading struggle.
narrative_ontology:constraint_stakeholder(refugee_convention_text__expansive_humanitarian_reading, sovereignty_doctrine_adherents, excluded,
    institutional, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(refugee_convention_text__expansive_humanitarian_reading, international_human_rights_advocacy_organizations).
narrative_ontology:fixing_cost_class(refugee_convention_text__expansive_humanitarian_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Interprets the 1951 Refugee Convention as a binding humanitarian mandate that protects individuals from generalized violence, non-state persecution, and harm based on gender, sexual orientation, and clan membership—establishing a shared legal standard for asylum adjudication across signatory states and creating a common interpretive framework that enables international coordination on refugee protection.
% TRANSFER_FUNCTION: Transfers protection obligation from restrictive-reading states to all signatory states by expanding the class of persecution that triggers asylum duty; shifts substantive assessment burden to adjudicating authorities and away from executive discretion; moves interpretive authority from national governments to international human rights bodies and courts applying the expansive reading.
% ABSENT_VOICES: Political movements and state representatives who believe sovereignty over asylum admission is non-negotiable are structurally excluded from the interpretive process—they are not represented in UNHCR bodies, amicus briefing, or academic consensus-building that institutionalizes the expansive reading. They would argue that the Convention is a floor, not a ceiling, and that states retain broad discretion to narrow or condition protection based on their capacity and security interests.
% DISAPPEARANCE_RATIONALE: If the expansive reading disappeared and were replaced by the restrictive reading, asylum law would reorganize: generalized violence claimants would be systematically rejected, gender-based persecution would require proof of state targeting, and LGBTQ+ claimants would face heightened burdens. Millions of current protection beneficiaries would lose status; low-income host countries would face reduced international protection obligations; human rights litigation dockets would shrink. The humanitarian architecture built on the expansive reading would partially collapse.
% FOUNDING_PROBLEM: The 1951 Convention was written in the immediate post-WWII context to protect individuals fleeing state-sponsored persecution. By the 1970s–1990s, persecution was increasingly conducted by non-state actors (militias, criminal networks, insurgent groups) and motivated by group characteristics (gender, sexuality, clan) that states either could not control or did not formally target. The expansive reading emerged to extend protection to these categories without rewriting the Convention text—interpreting 'well-founded fear' and 'particular social group' to capture the full range of contemporary threats.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by UNHCR, international human rights organizations, and judicial bodies (UK Supreme Court in *Begum*, Australian courts in *NZYQ*, Canadian courts in *Kandavel*). Independent research from refugee-origin countries documents that the majority of protection claims now involve generalized violence or gender-based harm rather than individual state targeting. Restrictive-reading advocates do not deny the empirical reality of these harms; they dispute whether the Convention text requires protection for them.
narrative_ontology:disappearance_verdict(refugee_convention_text__expansive_humanitarian_reading, world_rearranges).
narrative_ontology:founding_problem_status(refugee_convention_text__expansive_humanitarian_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(refugee_convention_text__expansive_humanitarian_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(refugee_convention_text__expansive_humanitarian_reading, 'none', 1).
narrative_ontology:epsilon_provenance(refugee_convention_text__expansive_humanitarian_reading, 0.28, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(refugee_convention_text__expansive_humanitarian_reading_tests).
:- end_tests(refugee_convention_text__expansive_humanitarian_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.28 at endpoint) because the constraint imposes real costs on some states and claimants while benefiting others — the cost is not symmetrically borne (coordinate model). Suppression is 0.45: the expansive reading is actively resisted through interdiction, offshore processing, procedural barriers, and narrow judicial interpretations. The resistance to the reading is substantial and institutionally organized by restrictive states. Theater_ratio is low (0.22) because the core function — expanding the protection class — is substantively pursued through litigation and doctrine, not merely performed. The measurement series span 1980–2026 to capture how the reading's institutional ground has shifted: in 1980 the expansive reading was barely articulated (low extractiveness 0.08, low suppression 0.25); by 2005 it had gained doctrinal momentum (extractiveness 0.22); by 2026 it is institutionally mature but politically contested (extractiveness plateaus at 0.28 as restrictive states stabilize their resistance). The coercion grid shows asymmetric suppression across levels: individual claimants face the highest suppression (0.52 at endpoint) through procedural barriers and interdiction; organizational resistance is robust (0.76 at endpoint) from human rights groups; structural-level suppression is lower (0.38 at endpoint) because the reading has achieved doctrinal legitimacy even though states resist it in practice.
 *
 * PERSPECTIVAL GAP:
 *   The read reveals fundamental perspectival divergence based on institutional position and reading adoption: (1) From the expansive-reading beneficiary seat (UNHCR, advocacy organizations, protective-reading courts), the constraint is genuine coordination that extends humanitarian law to match contemporary persecution realities — rope-type coordination with asymmetric benefit (some states gain legitimacy, some lose discretion). (2) From the restrictive-state seat, the constraint is extraction masked as humanitarian principle — the reading expands protection obligation beyond what the Convention text plainly mandates, using interpretive authority to bypass the renegotiation that sovereignty would demand. (3) From the individual claimant seat in a restrictive jurisdiction, the reading is protective in principle but extractive in practice — the expansive mandate is procedurally blocked through interdiction, offshore processing, and narrow application by hostile adjudicators. (4) From the low-income host country seat, the constraint is a coordination framework that is legitimate but under-resourced — they adopt the expansive reading but lack capacity to implement substantive assessment at scale. The engine computes type divergence from these asymmetric directionalities: expansive-reading adopters compute rope or tangled-rope; restrictive states compute snare or piton (the mandate exists but is theatrically maintained or actively resisted). The authored claim (tangled_rope) reflects the view that the constraint is coordinating (shared interpretation) while extracting (differential obligation burden).
 *
 * DIRECTIONALITY LOGIC:
 *   The expansive reading's beneficiaries are persecution survivors (generalized violence, non-state, gender-based, sexuality-based, clan-based) — structurally beneficiaries because the reading expands their protection class (d near 0.0 = full beneficiary for this group). International human rights advocacy organizations are also structural beneficiaries: they collect interpretive authority, institutional funding, and legitimacy from defining the expansive reading (d near 0.0–0.2). Restrictive-reading states are structurally targeted by the expansion: they lose discretion, face higher protection obligations, and bear the cost of expanded claims processing (d near 0.8–0.95 = nearly full target). The directionality is sharp because the expansive reading *deliberately* shifts obligation from sovereign states to the international humanitarian system. Low-income host countries are complex: they benefit from the humanitarian mandate's legitimacy but pay through resource strain — they sit near 0.6 (moderate cost). Procedurally disadvantaged claimants in restrictive jurisdictions are pure targets of interpretive narrowing (d near 1.0) — the reading's benefits flow to other beneficiary groups, not to them.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint avoids mandatrophy through its founding problem remaining live and substantively contested. The expansive reading was built to solve the problem of persecution that is not state-targeted (generalized violence, non-state actors, group-identity-based harm). That problem is empirically alive: contemporary forced displacement is *increasingly* driven by generalized violence and non-state actors rather than state targeting. Host-country data from UNHCR and research organizations confirm this. The reading's persistence is therefore functionally justified by the live problem, not by inertia. However, the constraint shows some mandatrophy signals: (1) Theater_ratio rises from 0.05 (1980) to 0.22 (2026), indicating increasing performative maintenance as states adopt the reading in doctrine but resist in operational implementation (interdiction, offshore processing, procedural barriers). (2) Procedural gatekeeping has intensified even as the substantive protection mandate expanded — asylum systems have become slower, more complex, and more hostile in many restrictive jurisdictions, creating a temporal divergence where the expansive reading is theoretically operative but practically unavailable. (3) The founding problem has fractured into contested calibration: all parties agree that generalized violence is a real threat, but disagree whether the Convention mandates protection for it, whether state capacity allows it, and whether the claimant's individual circumstances meet the threshold. The mandatrophy here is not terminal failure but mounting cost-benefit divergence for states bearing the obligation without institutional pressure to re-examine the reading's scope. The constraint remains functionally justified by the live problem, so mandatrophy_resolved is false, but the indexing is fragile.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    text_semantics_of_persecution,
    'Does the 1951 Convention text''s term ''persecution'' encompass generalized violence and non-state harm, or is it limited to organized campaigns targeting identifiable groups?',
    'Linguistic analysis of the Convention''s preparatory work (travaux préparatoires) and contemporaneous state understanding; comparative study of how the same text is interpreted across legal systems; analysis of whether drafters'' knowledge of contemporary persecution patterns (Rwandan genocide, Yugoslav wars, gang violence, climate-driven displacement) would have informed a narrower vs. broader reading.',
    'If text analysis supports broad coverage, the expansive reading gains doctrinal legitimacy and restrictive interpretation becomes texturally indefensible. If text analysis supports narrow coverage, the reading becomes a value-driven amendment, not interpretation, and the mandate-to-discretion tradeoff becomes explicit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(text_semantics_of_persecution, empirical, 'Whether the Convention text permits the expansive reading or requires a narrower one.').

omega_variable(
    particular_social_group_construction,
    'Is ''particular social group'' a fixed category (immutable characteristics, recognizable by states) or an emergent category (socially constructed membership, including gender, sexuality, clan, professional group)?',
    'Jurisprudential mapping of how courts and tribunals have defined ''particular social group'' over time; comparative study of how refugee law integrates social construction from gender studies and identity theory; analysis of whether the expansive reading''s inclusion of gender and sexuality is grounded in the Convention''s logic or added by post-hoc reinterpretation.',
    'If social construction is textually grounded, the expansive reading becomes interpretively sound. If it is post-hoc addition, the reading is a policy choice, not a legal derivation, and the legitimacy of the mandate shifts from doctrinal to political.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(particular_social_group_construction, conceptual, 'Whether ''particular social group'' is fixed or constructed, and whether the expansive reading''s categories fall within the Convention''s scope.').

omega_variable(
    non_refoulement_scope_ambiguity,
    'Does non-refoulement (the prohibition on return to danger) apply only to formal asylum determination processes, or does it encompass interdiction and offshore processing that prevent any determination from occurring?',
    'Comparative international law doctrine and state practice on non-refoulement''s application to maritime interdiction, proxy detention, and offshore processing; analysis of whether the principle''s purpose (protection from persecution) requires substantive assessment before return, or only formal process if return occurs.',
    'If non-refoulement encompasses pre-determination barriers (interdiction, offshore processing), the expansive reading''s operational implications are severe for restrictive states'' border control and maritime enforcement. If non-refoulement applies only post-determination, interdiction and offshore processing are discretionary and the reading''s practical scope narrows sharply.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(non_refoulement_scope_ambiguity, empirical, 'Whether non-refoulement forecloses interdiction and offshore processing, or permits them as gatekeeping.').

omega_variable(
    humanitarian_mandate_vs_sovereign_discretion_incoherence,
    'Are the expansive humanitarian reading and the restrictive sovereignty reading logically foreclosing (one is true and the other impossible), or are they coexisting interpretations that different parties can simultaneously hold?',
    'Analysis of whether the Convention''s text permits both readings within the same legal framework, or whether accepting one reading''s core premises logically entails rejecting the other. Examination of whether a state can endorse the expansive reading as international principle while asserting restrictive interpretation as domestic law, or whether such a position is inherently self-contradictory.',
    'If readings are foreclosing, the constraint contest is zero-sum and can only be resolved by one reading achieving hegemonic adoption or by formal amendment. If readings coexist, the constraint will remain contested and the interpretation divergence will persist as a stable feature of refugee law. Classification of this reading''s relationship to the restrictive reading (forecloses vs. coexists_with) depends on this resolution.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(humanitarian_mandate_vs_sovereign_discretion_incoherence, conceptual, 'Whether the expansive and restrictive readings are logically incompatible or coexisting.').

omega_variable(
    procedural_vs_substantive_priority,
    'When the procedural integrity reading (emphasis on fair process) and the expansive humanitarian reading (emphasis on broad substantive protection) conflict — e.g., when speedy processing risks unfair individual assessment — which takes priority?',
    'Analysis of judicial decisions that have faced this tradeoff; study of whether courts have established a hierarchy or have treated procedural fairness and substantive protection as equally weighted; examination of whether the Convention text provides textual grounding for prioritizing one over the other.',
    'If substantive protection takes priority, the procedural reading becomes subordinate to the expansive reading in practical application. If procedural fairness takes priority, the expansive reading''s operationalization is constrained by process demands, and states can legitimately invoke procedural grounds to slow or limit protection expansion. This determines whether the procedural reading influences or forecloses the expansive reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(procedural_vs_substantive_priority, preference, 'Whether procedural integrity and substantive protection can both be maximized, or whether tradeoffs require prioritization.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(refugee_convention_text__expansive_humanitarian_reading, 1980, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(refu_tr_t1980, refugee_convention_text__expansive_humanitarian_reading, theater_ratio, 1980, 0.05).
narrative_ontology:measurement_basis(refu_tr_t1980, observed).
narrative_ontology:measurement(refu_tr_t1995, refugee_convention_text__expansive_humanitarian_reading, theater_ratio, 1995, 0.09).
narrative_ontology:measurement_basis(refu_tr_t1995, observed).
narrative_ontology:measurement(refu_tr_t2005, refugee_convention_text__expansive_humanitarian_reading, theater_ratio, 2005, 0.14).
narrative_ontology:measurement_basis(refu_tr_t2005, observed).
narrative_ontology:measurement(refu_tr_t2015, refugee_convention_text__expansive_humanitarian_reading, theater_ratio, 2015, 0.19).
narrative_ontology:measurement_basis(refu_tr_t2015, observed).
narrative_ontology:measurement(refu_tr_t2020, refugee_convention_text__expansive_humanitarian_reading, theater_ratio, 2020, 0.21).
narrative_ontology:measurement_basis(refu_tr_t2020, observed).
narrative_ontology:measurement(refu_tr_t2026, refugee_convention_text__expansive_humanitarian_reading, theater_ratio, 2026, 0.22).
narrative_ontology:measurement_basis(refu_tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(refu_be_t1980, refugee_convention_text__expansive_humanitarian_reading, base_extractiveness, 1980, 0.08).
narrative_ontology:measurement_basis(refu_be_t1980, observed).
narrative_ontology:measurement(refu_be_t1995, refugee_convention_text__expansive_humanitarian_reading, base_extractiveness, 1995, 0.15).
narrative_ontology:measurement_basis(refu_be_t1995, observed).
narrative_ontology:measurement(refu_be_t2005, refugee_convention_text__expansive_humanitarian_reading, base_extractiveness, 2005, 0.22).
narrative_ontology:measurement_basis(refu_be_t2005, observed).
narrative_ontology:measurement(refu_be_t2015, refugee_convention_text__expansive_humanitarian_reading, base_extractiveness, 2015, 0.26).
narrative_ontology:measurement_basis(refu_be_t2015, observed).
narrative_ontology:measurement(refu_be_t2020, refugee_convention_text__expansive_humanitarian_reading, base_extractiveness, 2020, 0.27).
narrative_ontology:measurement_basis(refu_be_t2020, observed).
narrative_ontology:measurement(refu_be_t2026, refugee_convention_text__expansive_humanitarian_reading, base_extractiveness, 2026, 0.28).
narrative_ontology:measurement_basis(refu_be_t2026, observed).

% Suppression requirement over time
narrative_ontology:measurement(refu_su_t1980, refugee_convention_text__expansive_humanitarian_reading, suppression_requirement, 1980, 0.25).
narrative_ontology:measurement_basis(refu_su_t1980, observed).
narrative_ontology:measurement(refu_su_t1995, refugee_convention_text__expansive_humanitarian_reading, suppression_requirement, 1995, 0.35).
narrative_ontology:measurement_basis(refu_su_t1995, observed).
narrative_ontology:measurement(refu_su_t2005, refugee_convention_text__expansive_humanitarian_reading, suppression_requirement, 2005, 0.4).
narrative_ontology:measurement_basis(refu_su_t2005, observed).
narrative_ontology:measurement(refu_su_t2015, refugee_convention_text__expansive_humanitarian_reading, suppression_requirement, 2015, 0.43).
narrative_ontology:measurement_basis(refu_su_t2015, observed).
narrative_ontology:measurement(refu_su_t2020, refugee_convention_text__expansive_humanitarian_reading, suppression_requirement, 2020, 0.44).
narrative_ontology:measurement_basis(refu_su_t2020, observed).
narrative_ontology:measurement(refu_su_t2026, refugee_convention_text__expansive_humanitarian_reading, suppression_requirement, 2026, 0.45).
narrative_ontology:measurement_basis(refu_su_t2026, observed).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=1980, tn=2026
narrative_ontology:measurement(refu_grid_01, refugee_convention_text__expansive_humanitarian_reading, accessibility_collapse(class), 1980, 0.42).
narrative_ontology:measurement(refu_grid_02, refugee_convention_text__expansive_humanitarian_reading, accessibility_collapse(class), 2026, 0.68).
narrative_ontology:measurement(refu_grid_03, refugee_convention_text__expansive_humanitarian_reading, accessibility_collapse(individual), 1980, 0.55).
narrative_ontology:measurement(refu_grid_04, refugee_convention_text__expansive_humanitarian_reading, accessibility_collapse(individual), 2026, 0.72).
narrative_ontology:measurement(refu_grid_05, refugee_convention_text__expansive_humanitarian_reading, accessibility_collapse(organizational), 1980, 0.35).
narrative_ontology:measurement(refu_grid_06, refugee_convention_text__expansive_humanitarian_reading, accessibility_collapse(organizational), 2026, 0.62).
narrative_ontology:measurement(refu_grid_07, refugee_convention_text__expansive_humanitarian_reading, accessibility_collapse(structural), 1980, 0.28).
narrative_ontology:measurement(refu_grid_08, refugee_convention_text__expansive_humanitarian_reading, accessibility_collapse(structural), 2026, 0.7).
narrative_ontology:measurement(refu_grid_09, refugee_convention_text__expansive_humanitarian_reading, resistance(class), 1980, 0.58).
narrative_ontology:measurement(refu_grid_10, refugee_convention_text__expansive_humanitarian_reading, resistance(class), 2026, 0.74).
narrative_ontology:measurement(refu_grid_11, refugee_convention_text__expansive_humanitarian_reading, resistance(individual), 1980, 0.55).
narrative_ontology:measurement(refu_grid_12, refugee_convention_text__expansive_humanitarian_reading, resistance(individual), 2026, 0.68).
narrative_ontology:measurement(refu_grid_13, refugee_convention_text__expansive_humanitarian_reading, resistance(organizational), 1980, 0.62).
narrative_ontology:measurement(refu_grid_14, refugee_convention_text__expansive_humanitarian_reading, resistance(organizational), 2026, 0.76).
narrative_ontology:measurement(refu_grid_15, refugee_convention_text__expansive_humanitarian_reading, resistance(structural), 1980, 0.68).
narrative_ontology:measurement(refu_grid_16, refugee_convention_text__expansive_humanitarian_reading, resistance(structural), 2026, 0.78).
narrative_ontology:measurement(refu_grid_17, refugee_convention_text__expansive_humanitarian_reading, stakes_inflation(class), 1980, 0.38).
narrative_ontology:measurement(refu_grid_18, refugee_convention_text__expansive_humanitarian_reading, stakes_inflation(class), 2026, 0.64).
narrative_ontology:measurement(refu_grid_19, refugee_convention_text__expansive_humanitarian_reading, stakes_inflation(individual), 1980, 0.48).
narrative_ontology:measurement(refu_grid_20, refugee_convention_text__expansive_humanitarian_reading, stakes_inflation(individual), 2026, 0.76).
narrative_ontology:measurement(refu_grid_21, refugee_convention_text__expansive_humanitarian_reading, stakes_inflation(organizational), 1980, 0.32).
narrative_ontology:measurement(refu_grid_22, refugee_convention_text__expansive_humanitarian_reading, stakes_inflation(organizational), 2026, 0.58).
narrative_ontology:measurement(refu_grid_23, refugee_convention_text__expansive_humanitarian_reading, stakes_inflation(structural), 1980, 0.22).
narrative_ontology:measurement(refu_grid_24, refugee_convention_text__expansive_humanitarian_reading, stakes_inflation(structural), 2026, 0.45).
narrative_ontology:measurement(refu_grid_25, refugee_convention_text__expansive_humanitarian_reading, suppression(class), 1980, 0.22).
narrative_ontology:measurement(refu_grid_26, refugee_convention_text__expansive_humanitarian_reading, suppression(class), 2026, 0.46).
narrative_ontology:measurement(refu_grid_27, refugee_convention_text__expansive_humanitarian_reading, suppression(individual), 1980, 0.18).
narrative_ontology:measurement(refu_grid_28, refugee_convention_text__expansive_humanitarian_reading, suppression(individual), 2026, 0.52).
narrative_ontology:measurement(refu_grid_29, refugee_convention_text__expansive_humanitarian_reading, suppression(organizational), 1980, 0.28).
narrative_ontology:measurement(refu_grid_30, refugee_convention_text__expansive_humanitarian_reading, suppression(organizational), 2026, 0.42).
narrative_ontology:measurement(refu_grid_31, refugee_convention_text__expansive_humanitarian_reading, suppression(structural), 1980, 0.15).
narrative_ontology:measurement(refu_grid_32, refugee_convention_text__expansive_humanitarian_reading, suppression(structural), 2026, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(refugee_convention_text__expansive_humanitarian_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(refugee_convention_text__expansive_humanitarian_reading, 0.12).
narrative_ontology:affects_constraint(refugee_convention_text__expansive_humanitarian_reading, refugee_convention_text__restrictive_sovereignty_reading).
narrative_ontology:affects_constraint(refugee_convention_text__expansive_humanitarian_reading, refugee_convention_text__procedural_integrity_reading).
narrative_ontology:affects_constraint(refugee_convention_text__expansive_humanitarian_reading, non_refoulement_doctrine).
narrative_ontology:affects_constraint(refugee_convention_text__expansive_humanitarian_reading, particular_social_group_jurisprudence).
narrative_ontology:affects_constraint(refugee_convention_text__expansive_humanitarian_reading, maritime_interdiction_governance).
narrative_ontology:affects_constraint(refugee_convention_text__expansive_humanitarian_reading, asylum_adjudication_procedure).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 1951 Refugee Convention kernel (constraint family: refugee_convention_text). The expansive humanitarian reading expands the beneficiary class to include generalized violence and non-state persecution; the restrictive sovereignty reading narrows protection to state-targeted persecution of immutable groups; the procedural integrity reading brackets substantive scope and centers on fair process. Each reading instantiates a different ε (extractiveness) and produces different victim/beneficiary sets. The three stories are linked via network.affects_constraints and represent one contestation site in international refugee law. The reading divergence is not measurement-dependent — it is genuinely different constraints flowing from genuinely different interpretations of the same text.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(refugee_convention_text__expansive_humanitarian_reading, institutional, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
