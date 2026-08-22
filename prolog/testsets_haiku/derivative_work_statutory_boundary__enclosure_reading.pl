% ============================================================================
% CONSTRAINT STORY: derivative_work_statutory_boundary__enclosure_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_derivative_work_statutory_boundary__enclosure_reading, []).

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
 *   constraint_id: derivative_work_statutory_boundary__enclosure_reading
 *   human_readable: Derivative Work Statutory Boundary — Enclosure Reading
 *   domain: intellectual_property/technology_governance/information_economics
 *
 * SUMMARY:
 *   The statutory derivative work right (17 U.S.C. § 103) grants copyright
 *   owners exclusive authority to prepare derivative works, but the statute
 *   does not define what constitutes a derivative work with precision. The
 *   enclosure reading instantiates one interpretation: any use of copyrighted
 *   expression in the creative process constitutes preparation of derivative
 *   work, requiring pre-creation authorization from the copyright holder.
 *   This reading emerged as a strategic position from incumbent copyright
 *   holders in litigation (Harper & Row v. Nation, Cariou v. Prince, Andy
 *   Warhol Foundation v. Goldsmith) and licensing practice. Under this
 *   reading, downstream creators face pre-creation gating — they must license
 *   before they can create, and licensing terms are set unilaterally by
 *   copyright holders. The enclosure reading is structurally distinct from
 *   the coordination reading (which permits transformative and intermediate
 *   use) and the hybrid carveout reading (which permits non-commercial
 *   transformation). The constraint instantiated here is the enclosure
 *   reading as a snare: high extraction, high suppression, identity-locked
 *   victims, no genuine coordination benefit to downstream creators.
 *
 * KEY AGENTS:
 *   - Incumbent copyright holders (institutional beneficiary/agenda setter): set licensing terms, enforce through litigation and takedown, maintain the broad interpretation through advocacy
 *   - Downstream creators (moderate-power multiple victims): trapped or identity-locked artists, musicians, developers who build on existing work
 *   - Remix practitioners (powerless victims): electronic musicians, video essayists whose entire creative method is transformation of existing material
 *   - Academic researchers (moderate-power victims): scholars analyzing copyrighted material or building research corpora
 *   - Interoperability engineers (organized victims): software developers maintaining compatibility across platforms
 *   - Courts (institutional observers): interpret the boundary through case law and can shift it through doctrine
 *   - Fair-use doctrine tradition (excluded non-agent): the competing principle that certain uses fall outside copyright
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(derivative_work_statutory_boundary__enclosure_reading, 0.82).
domain_priors:suppression_score(derivative_work_statutory_boundary__enclosure_reading, 0.78).
domain_priors:theater_ratio(derivative_work_statutory_boundary__enclosure_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__enclosure_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__enclosure_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__enclosure_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__enclosure_reading, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__enclosure_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(derivative_work_statutory_boundary__enclosure_reading, snare).
narrative_ontology:human_readable(derivative_work_statutory_boundary__enclosure_reading, "Derivative Work Statutory Boundary — Enclosure Reading").
narrative_ontology:topic_domain(derivative_work_statutory_boundary__enclosure_reading, "intellectual_property/technology_governance/information_economics").

domain_priors:requires_active_enforcement(derivative_work_statutory_boundary__enclosure_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(derivative_work_statutory_boundary__enclosure_reading, '128ed4ee-845b-45c6-9889-37d6d8ae033d').
narrative_ontology:cs_kernel_codification('128ed4ee-845b-45c6-9889-37d6d8ae033d', fixed_text).
narrative_ontology:cs_authority_grounding('128ed4ee-845b-45c6-9889-37d6d8ae033d', extraction).
narrative_ontology:cs_interpretation_layer_present('128ed4ee-845b-45c6-9889-37d6d8ae033d').
narrative_ontology:cs_reading_relation('128ed4ee-845b-45c6-9889-37d6d8ae033d', derivative_work_statutory_boundary__coordination_reading, forecloses).
narrative_ontology:cs_reading_relation('128ed4ee-845b-45c6-9889-37d6d8ae033d', derivative_work_statutory_boundary__hybrid_carveout_reading, coexists_with).
narrative_ontology:cs_axiom('128ed4ee-845b-45c6-9889-37d6d8ae033d', foundational, any_expression_use_requires_authorization).
narrative_ontology:cs_axiom_status(any_expression_use_requires_authorization, holdable).
narrative_ontology:cs_axiom_grounding('128ed4ee-845b-45c6-9889-37d6d8ae033d', any_expression_use_requires_authorization, conventional).
narrative_ontology:cs_axiom('128ed4ee-845b-45c6-9889-37d6d8ae033d', secondary, downstream_creator_licensing_dependency_justifiable).
narrative_ontology:cs_axiom_status(downstream_creator_licensing_dependency_justifiable, overridden).
narrative_ontology:cs_axiom_grounding('128ed4ee-845b-45c6-9889-37d6d8ae033d', downstream_creator_licensing_dependency_justifiable, empirically_contingent).
narrative_ontology:cs_reference_frame('128ed4ee-845b-45c6-9889-37d6d8ae033d', broad_derivative_rights_doctrine).
narrative_ontology:cs_drift_state('128ed4ee-845b-45c6-9889-37d6d8ae033d', contemporary_fair_use_restriction_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('128ed4ee-845b-45c6-9889-37d6d8ae033d', '').
narrative_ontology:cs_kernel_id(derivative_work_statutory_boundary__enclosure_reading, derivative_work_statutory_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(derivative_work_statutory_boundary__enclosure_reading, incumbent_copyright_holders).
narrative_ontology:constraint_victim(derivative_work_statutory_boundary__enclosure_reading, downstream_creators).
narrative_ontology:constraint_victim(derivative_work_statutory_boundary__enclosure_reading, remix_practitioners).
narrative_ontology:constraint_victim(derivative_work_statutory_boundary__enclosure_reading, academic_researchers).
narrative_ontology:constraint_victim(derivative_work_statutory_boundary__enclosure_reading, interoperability_engineers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(derivative_work_statutory_boundary__enclosure_reading, global_content_industries).
narrative_ontology:constraint_vindicates(derivative_work_statutory_boundary__enclosure_reading, copyright_owner_exclusive_right_doctrine).
narrative_ontology:constraint_vindicates(derivative_work_statutory_boundary__enclosure_reading, broad_derivative_right_interpretation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Large media, software, and entertainment corporations that hold extensive copyright portfolios. They set the operative interpretation of 'derivative work' through licensing demands, takedown enforcement, and litigation strategy. They benefit directly from the broad reading: every downstream use of their expression requires authorization and licensing fee, creating a gating mechanism that allows them to capture economic value from all downstream innovation. They maintain this position through aggressive enforcement and lobbying for statutory expansion.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__enclosure_reading, incumbent_copyright_holders, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(derivative_work_statutory_boundary__enclosure_reading, incumbent_copyright_holders, beneficiary).

% Visual artists, musicians, writers, game designers, and software developers who build new works that incorporate, sample, remix, or transform existing copyrighted expression. Under the enclosure reading, any use of protected material in creating new work is preparation of derivative work, requiring license before creation begins. They face binary choice: license at incumbent-set terms (often prohibitive for independent creators), omit the reference entirely (narrowing their artistic palette), or face infringement liability. Identity-locked because their creative practice is constituted through engagement with existing cultural material; exit means abandoning their discipline as practiced.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__enclosure_reading, downstream_creators, payer,
    moderate, biographical, identity_locked, global).

% Hip-hop producers, electronic musicians, video essayists, and collage artists whose primary creative method is transformation of existing material. The enclosure reading makes their work categorically preparation of derivative work before they even begin — they cannot access fair-use doctrine until after litigation, and cannot afford litigation. They are trapped: their exit would require abandoning the artistic form entirely. Geographic jurisdiction shopping offers minimal refuge because enforcement is global-scale and copyright treaties harmonize the broad interpretation.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__enclosure_reading, remix_practitioners, payer,
    powerless, biographical, trapped, global).

% Scholars analyzing copyrighted texts, algorithms, or media; computational linguists building training corpora; historians assembling archival databases. The enclosure reading extends preparation-of-derivative-work liability to research use, not merely commercial remix. They operate within institutional frameworks (universities with legal departments) that absorb some licensing costs, but cannot license comprehensively for exploratory research. Their exit is constrained by institutional affiliation and disciplinary norms; they can migrate to pre-1928 materials or public domain, narrowing research scope but not leaving the field.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__enclosure_reading, academic_researchers, payer,
    moderate, generational, constrained, global).

% Software developers maintaining compatibility layers, reverse-engineering tools, format converters, and system integration libraries. Under enclosure reading, reverse-engineering a proprietary protocol to create compatible software is preparation of derivative work of the original protocol expression. They operate in organized collectives (open-source projects, standards bodies) with some legal defense capacity, but face licensing demands that create friction in interoperability markets. Exit is constrained by the installed base they serve; abandoning interoperability engineering means losing their professional niche.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__enclosure_reading, interoperability_engineers, payer,
    organized, biographical, constrained, global).

% The jurisprudential principle that certain uses of copyrighted material (criticism, commentary, parody, news reporting, teaching, scholarship, research) fall outside the scope of copyright protection. The enclosure reading structurally excludes fair use by requiring authorization before creation — fair use becomes a post-hoc litigation defense rather than a pre-creation safe harbor. The doctrine's principle — that copyright does not control all uses — is incompatible with the enclosure reading's premise.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__enclosure_reading, fair_use_doctrine_tradition, excluded,
    analytical, civilizational, analytical, global).
narrative_ontology:stakeholder_non_agent(derivative_work_statutory_boundary__enclosure_reading, fair_use_doctrine_tradition).

% Judges, scholars, and practitioners advocating for the coordination reading: that transformative use and intermediate copying are non-infringing and only substantial recastings constitute derivative work. Their position is structurally excluded from the enclosure reading's framework. They advocate for a narrower derivative work boundary and broader safe harbors. They have legal standing and institutional platforms but lack the concentrated economic interest of incumbent copyright holders.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__enclosure_reading, coordination_reading_advocates, excluded,
    moderate, biographical, constrained, global).

% Film studios, music labels, publishing houses, and software manufacturers whose business model depends on licensing revenue from derivative rights. The broad enclosure reading generates licensing demand across downstream creative industries, creating a royalty stream from remix, remix-adjacent, and transformative work. They benefit incidentally through copyright holder advocacy (many incumbent holders are subsidiaries of global entertainment conglomerates) and through cross-licensing deals with downstream creators forced to seek authorization.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__enclosure_reading, global_content_industries, beneficiary,
    institutional, generational, arbitrage, global).

% Federal courts interpreting the Copyright Act, patent examiners, international IP harmonization bodies (WIPO, regional trade bodies). They observe the enclosure reading's application and can recalibrate the boundary through case law and policy. They carry limited economic interest and analyze fairness and innovation questions, but face concentrated lobbying from incumbent copyright holders.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__enclosure_reading, courts_and_doctrine_bodies, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(derivative_work_statutory_boundary__enclosure_reading, incumbent_copyright_holders).
narrative_ontology:fixing_cost_class(derivative_work_statutory_boundary__enclosure_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: If present: protects a stable copyright licensing market by establishing clear pre-creation notification requirements; prevents unauthorized derivative markets; provides predictable cost structure for downstream creators negotiating licenses. The enclosure reading frames this as coordination of expectations around derivative rights. In practice, the coordination function is minimal — no market participant is coordinated into a position they prefer; rather, incumbents coordinate downstream creators into licensing dependency.
% TRANSFER_FUNCTION: Moves authorization authority (and licensing revenue) from distributed downstream creators to incumbent copyright holders. Every downstream creator who uses copyrighted expression in creating new work must first obtain license from the incumbent holder, transferring decision-making power and revenue to the right-holder. The transfer also moves to incur legal risk — downstream creators bear infringement liability for pre-creation incorporation of protected material.
% ABSENT_VOICES: Consumers of derivative and transformative work (audiences for remix, mashups, commentary, critical adaptations, parody); future creators not yet active in the creative process; cultures of remix and reuse that predate copyright law; artists from non-Western traditions with different frameworks for authorship and innovation. Their absence is structural: they are not in licensing negotiations and cannot object because the licensing is pre-creation. They would argue that access to existing cultural material is necessary for creative practice and that broad derivative-work liability suppresses innovation.
% DISAPPEARANCE_RATIONALE: If the enclosure reading disappeared overnight — if the boundary were redrawn to permit transformative and intermediate use without pre-creation licensing — the entire downstream creative ecosystem would reorganize. Remix-based music production, fan works, critical adaptations, academic analysis of copyrighted material, software interoperability engineering, and collage arts would expand immediately. Licensing revenue for incumbents would collapse in many sectors. The copyright holder's ability to gate downstream innovation would evaporate. Organizational structures built around licensing (rights clearance, permissions workflows, licensing bureaus) would shrink or vanish.
% FOUNDING_PROBLEM: Copyright statute grants exclusive right to prepare derivative works (17 U.S.C. § 103). The founding problem is defining what 'derivative work' means: does it include every use of original expression in creating new work, or only substantial recastings that incorporate recognizable portions of the original? Early copyright doctrine held a narrower boundary (derivative work = substantial incorporation, reordering, or abridgment of the original); the enclosure reading extends it to any use of protected expression in the creative process, even if the result is transformative and the protected expression is unrecognizable in the final work.
% FOUNDING_PROBLEM_CORROBORATION: Incumbent copyright holders and their counsel assert the broad definition is the intended meaning of the statute and necessary to protect licensing markets. Federal courts in some cases (Harper & Row v. Nation Enterprises, Cariou v. Prince, Andy Warhol Foundation v. Goldsmith) have applied elements of the broad reading, treating even transformative use as potentially infringing. Downstream creators, fair-use scholars, and technology practitioners attested in testimony to Congress (DMCA hearings, 2020 AI and Copyright hearings) that the broad reading suppresses innovation. Academic copyright scholarship (James Grimmelmann, Pamela Samuelson, Jessica Litman) corroborates that the broad reading is a constructed interpretation, not the original statutory intent, and that it has amplified extraction in recent decades. The founding problem itself — defining derivative work — remains live; the enclosure reading's resolution is contested.
narrative_ontology:disappearance_verdict(derivative_work_statutory_boundary__enclosure_reading, world_rearranges).
narrative_ontology:founding_problem_status(derivative_work_statutory_boundary__enclosure_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(derivative_work_statutory_boundary__enclosure_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(derivative_work_statutory_boundary__enclosure_reading, 'none', 1).
narrative_ontology:epsilon_provenance(derivative_work_statutory_boundary__enclosure_reading, 0.82, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(derivative_work_statutory_boundary__enclosure_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(derivative_work_statutory_boundary__enclosure_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(derivative_work_statutory_boundary__enclosure_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.82 at interval end) and rising because the enclosure reading creates no coordination benefit to downstream creators — it is pure licensing-right extraction. The broad interpretation has been steadily reinforced through litigation outcomes, DMCA expansion, and international copyright treaty harmonization over the 40-year interval, pushing extractiveness upward from 0.58 to 0.82. Suppression is high (0.78) because downstream creators cannot access the use they need without pre-creation authorization; fair-use doctrine is effectively inaccessible because it is a post-hoc litigation defense, not a pre-creation safe harbor. The three victim groups bear suppression differently: remix practitioners face trapped suppression (cannot exit the form), downstream creators face identity-locked suppression (their creative identity is constituted through engagement with existing material), academic researchers face constrained suppression (can shift to public-domain materials or lose research scope). Theater ratio is moderate-low (0.42) and stable, reflecting that the licensing function does perform real work (administering authorization), but an increasing share of enforcement activity (takedowns, litigation) defends pure gating rather than any coordination function. The measurement series show extraction accumulation (the enclosure reading has hardened over time) and static theater (the ratio of real licensing administration to pure gating enforcement has plateaued).
 *
 * PERSPECTIVAL GAP:
 *   The two seats compute differently: from the incumbent copyright holder's seat, the broad reading is justified as protection of the licensing market and incentive for creation. From the downstream creator's seat (especially identity-locked seats like remix practitioners), the same structure is enforced extraction — licensing requirements that suppress innovation and transfer all decision-making power upstream. The engine computes both seats' types from the structural data. The claimed type (snare) reflects the downstream seat's structural experience; the incumbent seat would compute as beneficiary or potentially rope (arguing coordination benefit). The perspectival gap is the primary signal here: a constraint where the beneficiary seat sees coordination and the victim seat sees pure extraction is a classic snare structure.
 *
 * DIRECTIONALITY LOGIC:
 *   Incumbent copyright holders have high directionality toward target status (d near 1.0): they extract licensing revenue and authorization authority from the constraint. Downstream creators have high directionality toward extraction (d near 1.0): they must license before creating. Remix practitioners are trapped (lowest exit mobility) and powerless (no leverage in negotiation), so their d is pushed higher still. Academic researchers have moderate power and constrained exit (can shift to public-domain research), pushing d toward symmetric but still target-biased. Interoperability engineers have organized power and constrained exit, similarly. Fair-use doctrine tradition is excluded (not a real actor) and receives no d computation. Courts are observers (analytical seat, neutral d). No directionality overrides are needed; the beneficiary/victim declarations and exit options produce accurate d values through the engine's derivation chain.
 *
 * MANDATROPHY ANALYSIS:
 *   The enclosure reading faces a mandatrophy question: was the broad derivative-work boundary built to solve the original problem (defining which works should be protected under copyright), or does the founding problem still justify the current constraint? The founding problem — defining derivative work — is contested. Incumbent copyright holders argue the broad definition is necessary to protect licensing markets and incentivize creation. Downstream creators and scholars argue the broad definition was not the original statutory intent, was constructed through strategic litigation, and now suppresses downstream innovation more than it protects upstream incentives. The measured extraction (0.82) and suppression (0.78) far exceed what the founding problem justifies. The mandatrophy case is that the enclosure reading has outlived its founding problem justification: the problem was scope definition, not licensing-revenue maximization, and the broad reading's extraction now suppresses more innovation than copyright's incentive structure generates. This is a snare whose founding problem status is contested and whose persistence depends on incumbent enforcement, not on any genuine coordination benefit.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    transformation_versus_preparation_ambiguity,
    'Where is the boundary between preparation of derivative work (which requires authorization) and transformative creation that incidentally uses copyrighted expression? Is the boundary determined by the degree of transformation, the recognizability of original expression in the final work, the creator''s intent, or the market effect on the original?',
    'Systematic analysis of court holdings across transformation cases (Campbell v. Acuff-Rose Music, Cariou v. Prince, Andy Warhol Foundation v. Goldsmith) to extract consistent doctrine; controlled comparison with fair-use doctrine''s transformation test; international comparative analysis of derivative-work definitions in EU, Australia, Canada jurisdictions.',
    'If transformation-based boundary supplants expression-use-based boundary, downstream creators with identity-locked exit would have pre-creation safe harbor for transformative use, collapsing extractiveness and suppression. If expression-use boundary holds, the enclosure reading''s snare classification stands.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(transformation_versus_preparation_ambiguity, empirical, 'Where the statutory derivative work boundary actually sits in doctrine.').

omega_variable(
    licensing_revenue_justification_gap,
    'Does the broad enclosure reading actually generate greater upstream incentives to create, or does it merely transfer revenue from downstream to upstream creators without incentive effect? Do copyright holders'' licensing revenue from derivative rights correlate with downstream creator productivity?',
    'Analysis of licensing revenue data (RIAA, Authors Guild, software licensing data) against metrics of downstream creative output (remix production, transformative art production, academic publications) before and after licensing strictures; comparison with jurisdictions using narrower derivative-work definitions.',
    'If licensing revenue shows no correlation with upstream incentives, the enclosure reading''s founding-problem justification is exposed as post-hoc rationalization. If correlation exists, the incentive case for the broad reading is empirically supported.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(licensing_revenue_justification_gap, empirical, 'Whether broad derivative-work licensing generates incentive benefits or merely extracts rent.').

omega_variable(
    identity_lock_suppression_mechanism,
    'Is the suppression of downstream creators (especially remix practitioners) structural (they cannot afford licensing) or internalized (they have absorbed copyright holder values and experience the broad reading as legitimate even though it suppresses their creative practice)? Would suppression persist if licensing costs collapsed?',
    'Post-reform analysis from jurisdictions that narrowed derivative-work definitions or expanded fair-use safe harbors (European Union directives, UK exceptions for parody and criticism, Canada''s broader fair-dealing standard): track whether downstream creator participation increased and whether suppression metrics declined after legal change.',
    'If suppression is internalized, downstream creators have fused their identity with copyright respect; the constraint has deeper hold than structural barriers alone. If suppression is structural, legal reform removing licensing requirements would immediately increase downstream creation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_suppression_mechanism, empirical, 'Structural versus internalized suppression mechanism.').

omega_variable(
    statutory_interpretation_lineage_contestation,
    'What was the original statutory intent of 17 U.S.C. § 103''s derivative work clause? Is the broad enclosure reading a faithful instantiation of original intent, or a constructed interpretation developed through strategic litigation and lobbying?',
    'Legislative history analysis (Copyright Act of 1976 legislative record, committee reports, stakeholder testimony); comparative analysis with international copyright treaties and their preparatory materials; historical analysis of pre-1976 case law on derivative works; interview with surviving legislative drafters.',
    'If original intent was narrower, the enclosure reading is a constructed doctrine benefiting incumbents — a false-summit candidate where a doctrine framed as natural law is actually extractive construction. If original intent was broad, the reading is faithful to statutory design.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(statutory_interpretation_lineage_contestation, empirical, 'Whether the broad derivative-work reading reflects original statutory intent or subsequent construction.').

omega_variable(
    kernel_contest_structure,
    'Are the three readings of the derivative_work_statutory_boundary kernel truly incommensurable (foreclosing each other), or do they occupy different regulatory/jurisdictional niches (coexisting in parallel)?',
    'Analysis of international copyright regimes and US state law to determine whether different readings are operationally instantiated in different jurisdictions; analysis of litigation outcomes to determine whether courts actually foreclose certain readings or merely choose among coexisting interpretations.',
    'If readings foreclose each other, only one can be structurally correct — the engine computes which via axiom contradiction. If readings coexist, the kernel involves genuine value contestation (different parties hold different readings) rather than factual error in one direction. Changes CS_relation classification and narrative implications.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_contest_structure, conceptual, 'Whether the three kernel readings are logically foreclosing or merely competing interpretations.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(derivative_work_statutory_boundary__enclosure_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(deri_tr_t0, derivative_work_statutory_boundary__enclosure_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(deri_tr_t0, observed).
narrative_ontology:measurement(deri_tr_t5, derivative_work_statutory_boundary__enclosure_reading, theater_ratio, 5, 0.28).
narrative_ontology:measurement_basis(deri_tr_t5, observed).
narrative_ontology:measurement(deri_tr_t10, derivative_work_statutory_boundary__enclosure_reading, theater_ratio, 10, 0.32).
narrative_ontology:measurement_basis(deri_tr_t10, observed).
narrative_ontology:measurement(deri_tr_t15, derivative_work_statutory_boundary__enclosure_reading, theater_ratio, 15, 0.36).
narrative_ontology:measurement_basis(deri_tr_t15, observed).
narrative_ontology:measurement(deri_tr_t20, derivative_work_statutory_boundary__enclosure_reading, theater_ratio, 20, 0.39).
narrative_ontology:measurement_basis(deri_tr_t20, observed).
narrative_ontology:measurement(deri_tr_t25, derivative_work_statutory_boundary__enclosure_reading, theater_ratio, 25, 0.41).
narrative_ontology:measurement_basis(deri_tr_t25, observed).
narrative_ontology:measurement(deri_tr_t30, derivative_work_statutory_boundary__enclosure_reading, theater_ratio, 30, 0.42).
narrative_ontology:measurement_basis(deri_tr_t30, observed).
narrative_ontology:measurement(deri_tr_t35, derivative_work_statutory_boundary__enclosure_reading, theater_ratio, 35, 0.42).
narrative_ontology:measurement_basis(deri_tr_t35, observed).
narrative_ontology:measurement(deri_tr_t40, derivative_work_statutory_boundary__enclosure_reading, theater_ratio, 40, 0.42).
narrative_ontology:measurement_basis(deri_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(deri_be_t0, derivative_work_statutory_boundary__enclosure_reading, base_extractiveness, 0, 0.58).
narrative_ontology:measurement_basis(deri_be_t0, observed).
narrative_ontology:measurement(deri_be_t5, derivative_work_statutory_boundary__enclosure_reading, base_extractiveness, 5, 0.63).
narrative_ontology:measurement_basis(deri_be_t5, observed).
narrative_ontology:measurement(deri_be_t10, derivative_work_statutory_boundary__enclosure_reading, base_extractiveness, 10, 0.68).
narrative_ontology:measurement_basis(deri_be_t10, observed).
narrative_ontology:measurement(deri_be_t15, derivative_work_statutory_boundary__enclosure_reading, base_extractiveness, 15, 0.71).
narrative_ontology:measurement_basis(deri_be_t15, observed).
narrative_ontology:measurement(deri_be_t20, derivative_work_statutory_boundary__enclosure_reading, base_extractiveness, 20, 0.75).
narrative_ontology:measurement_basis(deri_be_t20, observed).
narrative_ontology:measurement(deri_be_t25, derivative_work_statutory_boundary__enclosure_reading, base_extractiveness, 25, 0.79).
narrative_ontology:measurement_basis(deri_be_t25, observed).
narrative_ontology:measurement(deri_be_t30, derivative_work_statutory_boundary__enclosure_reading, base_extractiveness, 30, 0.81).
narrative_ontology:measurement_basis(deri_be_t30, observed).
narrative_ontology:measurement(deri_be_t35, derivative_work_statutory_boundary__enclosure_reading, base_extractiveness, 35, 0.82).
narrative_ontology:measurement_basis(deri_be_t35, observed).
narrative_ontology:measurement(deri_be_t40, derivative_work_statutory_boundary__enclosure_reading, base_extractiveness, 40, 0.82).
narrative_ontology:measurement_basis(deri_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(deri_su_t0, derivative_work_statutory_boundary__enclosure_reading, suppression_requirement, 0, 0.65).
narrative_ontology:measurement_basis(deri_su_t0, observed).
narrative_ontology:measurement(deri_su_t5, derivative_work_statutory_boundary__enclosure_reading, suppression_requirement, 5, 0.68).
narrative_ontology:measurement_basis(deri_su_t5, observed).
narrative_ontology:measurement(deri_su_t10, derivative_work_statutory_boundary__enclosure_reading, suppression_requirement, 10, 0.71).
narrative_ontology:measurement_basis(deri_su_t10, observed).
narrative_ontology:measurement(deri_su_t15, derivative_work_statutory_boundary__enclosure_reading, suppression_requirement, 15, 0.74).
narrative_ontology:measurement_basis(deri_su_t15, observed).
narrative_ontology:measurement(deri_su_t20, derivative_work_statutory_boundary__enclosure_reading, suppression_requirement, 20, 0.75).
narrative_ontology:measurement_basis(deri_su_t20, observed).
narrative_ontology:measurement(deri_su_t25, derivative_work_statutory_boundary__enclosure_reading, suppression_requirement, 25, 0.77).
narrative_ontology:measurement_basis(deri_su_t25, observed).
narrative_ontology:measurement(deri_su_t30, derivative_work_statutory_boundary__enclosure_reading, suppression_requirement, 30, 0.78).
narrative_ontology:measurement_basis(deri_su_t30, observed).
narrative_ontology:measurement(deri_su_t35, derivative_work_statutory_boundary__enclosure_reading, suppression_requirement, 35, 0.78).
narrative_ontology:measurement_basis(deri_su_t35, observed).
narrative_ontology:measurement(deri_su_t40, derivative_work_statutory_boundary__enclosure_reading, suppression_requirement, 40, 0.78).
narrative_ontology:measurement_basis(deri_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(derivative_work_statutory_boundary__enclosure_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(derivative_work_statutory_boundary__enclosure_reading, 0.12).
narrative_ontology:affects_constraint(derivative_work_statutory_boundary__enclosure_reading, derivative_work_statutory_boundary__coordination_reading).
narrative_ontology:affects_constraint(derivative_work_statutory_boundary__enclosure_reading, derivative_work_statutory_boundary__hybrid_carveout_reading).
narrative_ontology:affects_constraint(derivative_work_statutory_boundary__enclosure_reading, copyright_licensing_market_enforcement).
narrative_ontology:affects_constraint(derivative_work_statutory_boundary__enclosure_reading, fair_use_doctrine_pre_creation_accessibility).

% DUAL FORMULATION NOTE:
% The derivative_work_statutory_boundary kernel admits three structurally distinct constraints via different readings. The enclosure_reading (this constraint) emphasizes enforcement pre-creation and full licensing requirement. The coordination_reading emphasizes transformative and intermediate use exceptions. The hybrid_carveout_reading permits non-commercial transformation but requires authorization for commercial use. Each reading instantiates different ε, different beneficiary/victim structures, and different constraint types. They are linked via network.affects_constraints (all three relate to the same statutory boundary) and via omega variables documenting the kernel contest and reading relations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
