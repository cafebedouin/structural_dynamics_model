% ============================================================================
% CONSTRAINT STORY: marriage_authority__secularist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_authority__secularist_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: marriage_authority__secularist_reading
 *   human_readable: Marriage Authority as Secular Legislative Mandate (Secularist Reading)
 *   domain: legal/constitutional/family
 *
 * SUMMARY:
 *   This constraint story instantiates the SECULARIST READING of contested
 *   marriage authority: the view that marriage law belongs exclusively to
 *   democratic legislatures operating under constitutional principles of
 *   equality and secular rule of law. Personal law pluralism—the current
 *   regime where marriage, divorce, and inheritance are governed by religious
 *   community codes—is framed as a transitional anomaly awaiting elimination
 *   via Uniform Civil Code legislation. This reading is one of five
 *   structural positions on the kernel 'who has authority over marriage': the
 *   secularist reading coexists with communal-autonomy, federalist-millet,
 *   gender-rights, and judicial-harmonization readings held by different
 *   institutional and community seats. The secularist reading benefits the
 *   modernist legislative coalition and constitutional courts while imposing
 *   costs on minority religious communities whose legal authority over their
 *   own affairs is systematically narrowed. The claim (tangled_rope) and
 *   metrics (high extraction, high suppression) are deliberately aligned here
 *   because the secularist reading itself instantiates that structure—the
 *   coordination function (unified law) and the asymmetric extraction
 *   (authority transfer from communities to state) are not separable within
 *   this reading's epistemic frame.
 *
 * KEY AGENTS:
 *   - Secular Modernist Coalition: institutional power; holds legislative majority; frames UCC as modernization progress; benefits from each personal law restriction.
 *   - Minority Religious Communities: moderate power; currently govern family law; face progressive authority dissolution; identity-locked to traditional norms.
 *   - Gender Equality Advocates: powerful; support UCC on grounds that personal law codes perpetuate inequality; benefit from secular legal framework guarantees.
 *   - Constitutional Courts: institutional beneficiary and agenda-setter; expand jurisdiction over family law via constitutional review.
 *   - Communal Religious Leaders: moderate power, identity-locked; systematic institutional marginalization as secular law expands.
 *   - Legislative Majorities: institutional agenda-setter; enact successive restrictions on personal law autonomy.
 *   - Federalist Voices: excluded from secularist-reading discourse; defend pluralism as consociational protection mechanism.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_authority__secularist_reading, 0.68).
domain_priors:suppression_score(marriage_authority__secularist_reading, 0.72).
domain_priors:theater_ratio(marriage_authority__secularist_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_authority__secularist_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(marriage_authority__secularist_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(marriage_authority__secularist_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_authority__secularist_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(marriage_authority__secularist_reading, resistance, 0.79).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_authority__secularist_reading, tangled_rope).
narrative_ontology:human_readable(marriage_authority__secularist_reading, "Marriage Authority as Secular Legislative Mandate (Secularist Reading)").
narrative_ontology:topic_domain(marriage_authority__secularist_reading, "legal/constitutional/family").

domain_priors:requires_active_enforcement(marriage_authority__secularist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_authority__secularist_reading, 'b5376827-a072-4830-b19c-98fea46ffe69').
narrative_ontology:cs_kernel_codification('b5376827-a072-4830-b19c-98fea46ffe69', formalized).
narrative_ontology:cs_authority_grounding('b5376827-a072-4830-b19c-98fea46ffe69', extraction).
narrative_ontology:cs_interpretation_layer_present('b5376827-a072-4830-b19c-98fea46ffe69').
narrative_ontology:cs_reading_relation('b5376827-a072-4830-b19c-98fea46ffe69', marriage_authority__communal_autonomy_reading, forecloses).
narrative_ontology:cs_reading_relation('b5376827-a072-4830-b19c-98fea46ffe69', marriage_authority__federalist_millet_reading, influences).
narrative_ontology:cs_reading_relation('b5376827-a072-4830-b19c-98fea46ffe69', marriage_authority__gender_rights_reading, coexists_with).
narrative_ontology:cs_reading_relation('b5376827-a072-4830-b19c-98fea46ffe69', marriage_authority__judicial_harmonization_reading, influences).
narrative_ontology:cs_axiom('b5376827-a072-4830-b19c-98fea46ffe69', foundational, secular_law_sole_legitimate_authority).
narrative_ontology:cs_axiom_status(secular_law_sole_legitimate_authority, holdable).
narrative_ontology:cs_axiom_grounding('b5376827-a072-4830-b19c-98fea46ffe69', secular_law_sole_legitimate_authority, deontological).
narrative_ontology:cs_axiom('b5376827-a072-4830-b19c-98fea46ffe69', foundational, personal_law_pluralism_transitional_anachronism).
narrative_ontology:cs_axiom_status(personal_law_pluralism_transitional_anachronism, holdable).
narrative_ontology:cs_axiom_grounding('b5376827-a072-4830-b19c-98fea46ffe69', personal_law_pluralism_transitional_anachronism, empirically_contingent).
narrative_ontology:cs_reference_frame('b5376827-a072-4830-b19c-98fea46ffe69', secular_democratic_legislative_supremacy).
narrative_ontology:cs_drift_state('b5376827-a072-4830-b19c-98fea46ffe69', contemporary_post_gender_rights_movement, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('b5376827-a072-4830-b19c-98fea46ffe69', '').
narrative_ontology:cs_kernel_id(marriage_authority__secularist_reading, marriage_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_authority__secularist_reading, secular_modernist_coalition).
narrative_ontology:constraint_beneficiary(marriage_authority__secularist_reading, gender_equality_advocates).
narrative_ontology:constraint_beneficiary(marriage_authority__secularist_reading, constitutional_courts).
narrative_ontology:constraint_victim(marriage_authority__secularist_reading, minority_religious_communities).
narrative_ontology:constraint_victim(marriage_authority__secularist_reading, communal_authority_structures).
narrative_ontology:constraint_victim(marriage_authority__secularist_reading, traditional_personal_law_practitioners).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(marriage_authority__secularist_reading, married_couples_in_minority_communities).
narrative_ontology:constraint_victim(marriage_authority__secularist_reading, communal_religious_leaders).
narrative_ontology:constraint_victim(marriage_authority__secularist_reading, married_couples_in_minority_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds majority legislative power and constitutional court influence. Frames marriage as a civil status matter belonging exclusively to secular democratic law, not religious authority. Seeks to replace personal law pluralism with a unified Uniform Civil Code. Derives legitimacy from modernization discourse, gender equality claims, and national integration narratives. Collects political capital and institutional consolidation from each UCC advance.
narrative_ontology:constraint_stakeholder(marriage_authority__secularist_reading, secular_modernist_coalition, agenda_setter,
    institutional, generational, arbitrage, national).

% Currently govern marriage, inheritance, and family relations through communal religious law (personal law codes: Christian, Muslim, Hindu, Sikh). Face progressive legislative and judicial pressure to adopt uniform civil marriage rules that override communal norms on polygamy, inheritance, guardianship, and divorce. Their exit option is capitulation or institutional retreat; migration is not viable. The constraint dissolves their legal authority over their own internal affairs.
narrative_ontology:constraint_stakeholder(marriage_authority__secularist_reading, minority_religious_communities, payer,
    moderate, generational, constrained, national).

% Support uniform civil marriage law on grounds that personal law codes perpetuate gender inequality (unequal divorce rights, guardianship asymmetries, inheritance discrimination). Benefit from UCC pressure by acquiring a secular legal framework that embeds equality guarantees. Their support stabilizes the secularist coalition politically.
narrative_ontology:constraint_stakeholder(marriage_authority__secularist_reading, gender_equality_advocates, beneficiary,
    powerful, generational, mobile, national).

% Gain institutional power to review and constrain personal law codes under constitutional equality and dignity standards. Expand their docket and authority over family law by treating each case as a constitutional matter. Benefit from the secularist reading's framing, which justifies judicial intervention as enforcing democratic supremacy and constitutional norms against 'anachronistic' personal law.
narrative_ontology:constraint_stakeholder(marriage_authority__secularist_reading, constitutional_courts, beneficiary,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(marriage_authority__secularist_reading, constitutional_courts, agenda_setter).

% Hold traditional authority to interpret and administer marriage law within their communities. Face systematic marginalization as secular law expands and judicial review constrains communal norms. Cannot exit without abandoning their institutional role; their identity and authority are fused with the legal tradition they represent. Each UCC advance or court ruling strips away their normative domain.
narrative_ontology:constraint_stakeholder(marriage_authority__secularist_reading, communal_religious_leaders, payer,
    moderate, generational, identity_locked, national).

% Subject to both communal law (as members of their religious communities) and increasingly to secular uniform civil law (as citizens bound by state authority). The constraint narrows their ability to choose communal legal frameworks; it also offers escape routes from internal communal norms (e.g., a woman in a patriarchal personal law system can appeal to constitutional courts enforcing equality). They are simultaneously victims (authority stripped from their communities) and indirect beneficiaries (access to alternative legal paths).
narrative_ontology:constraint_stakeholder(marriage_authority__secularist_reading, married_couples_in_minority_communities, payer,
    powerless, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(marriage_authority__secularist_reading, married_couples_in_minority_communities, beneficiary).

% Enact successive legislative reforms eliminating or restricting personal law codes. Justify this through secular-modernist discourse: UCC as inevitable progress, legal pluralism as obstacle to national integration and constitutional rule of law. Do not need to compromise with minority religious communities because electoral majorities are not religious minorities.
narrative_ontology:constraint_stakeholder(marriage_authority__secularist_reading, legislative_majorities, agenda_setter,
    institutional, generational, analytical, national).

% Would argue that legal pluralism is a deliberate consociational mechanism preventing majoritarian domination, not an anomaly to be corrected. Defend personal law codes as protecting minority autonomy against majoritarian tyranny. Systematically sidelined in the secularist-reading discourse frame, which treats pluralism as backwardness rather than protection.
narrative_ontology:constraint_stakeholder(marriage_authority__secularist_reading, excluded_federalist_voices, excluded,
    moderate, generational, constrained, national).

% An abstract normative framework (constitutional equal protection, non-discrimination). The secularist reading vindicates this principle by subordinating personal law codes to uniform constitutional standards. The principle collects no rents but is deployed as a legitimacy anchor for the constraint.
narrative_ontology:constraint_stakeholder(marriage_authority__secularist_reading, constitutional_equality_principle, beneficiary,
    analytical, civilizational, analytical, national).
narrative_ontology:stakeholder_non_agent(marriage_authority__secularist_reading, constitutional_equality_principle).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(marriage_authority__secularist_reading, secular_modernist_coalition).
narrative_ontology:fixing_cost_class(marriage_authority__secularist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Unified civil marriage law solves a genuine coordination problem: eliminates conflicts between overlapping legal regimes, provides uniform contract enforcement across state borders, and establishes common procedural standards for recognizing and dissolving marriages.
% TRANSFER_FUNCTION: Transfers authority to define, validate, and dissolve marriage bonds from communal religious institutions to secular democratic legislatures and constitutional courts. Transfers the capacity to set norms for marital property, divorce, inheritance, and guardianship from tradition-based systems to statutory law. Moves prestige and institutional power from religious to secular authorities.
% ABSENT_VOICES: Federalist and consociational defenders of legal pluralism as an anti-majoritarian protection mechanism are not seated in the legislative or court processes that advance the secularist reading. Communities governing themselves under personal law codes would argue for continued autonomy; their objections are treated as resistance to progress rather than legitimate constitutional interests. Defenders of internal communal dispute resolution (informal justice mechanisms, religious arbitration) are excluded from the framing.
% DISAPPEARANCE_RATIONALE: If the secularist reading's constraint were to disappear (UCC reversed, personal law autonomy restored, courts withdrew from overriding communal norms), married persons could again be governed by their communities' own laws on divorce, property, and succession. Whole legal subfields (religious family law practice, communal arbitration, tradition-based inheritance) would revive. The secular-legal apparatus that currently mediates marriage would shrink.
% FOUNDING_PROBLEM: Legal pluralism inherited from colonial-era governance created jurisdictional conflicts, unequal access to justice, and resistance to national legal uniformity. Personal law codes perpetuated gender inequality and communal authority incompatible with modern constitutional governance. The founding problem was how to unify a fragmented legal system while modernizing family law norms.
% FOUNDING_PROBLEM_CORROBORATION: Secular modernist policymakers and constitutional courts attest the founding problem is live and ongoing—gender inequalities in personal law codes and coordination failures persist. Federalist scholars and minority community advocates attest the problem is differently framed: the real problem was colonial governance fragmentation, now being 'solved' by majoritarian absorption of minority law, which is not solution but erasure. Academic analyses of comparative constitutional law (Khilnani, Menski, Halley) document the problem reframing; they are external to the beneficiary seats.
narrative_ontology:disappearance_verdict(marriage_authority__secularist_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_authority__secularist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_authority__secularist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(marriage_authority__secularist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_authority__secularist_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_authority__secularist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(marriage_authority__secularist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(marriage_authority__secularist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.42 to 0.68 over the interval because successive legislative restrictions and court rulings progressively narrow the domain of personal law codes—minority communities lose authority incrementally. By t=40 the extraction plateaus near 0.68 (not 1.0) because some personal law frameworks persist in attenuated form, and communal dispute resolution continues informally outside the formal legal apparatus. Suppression is high (0.72 final) because the constraint's persistence depends on ACTIVE enforcement: courts must continually override communal norms, legislatures must enact restriction after restriction, and the secularist coalition must defend against federalist and communal-autonomy counter-claims. Without this sustained enforcement, personal law pluralism would persist and regenerate—the secular unified framework is not self-maintaining. Theater ratio is moderate (0.41) because the constraint carries genuine coordination function (unified law does reduce conflicts and improve enforcement), but a growing share of enforcement activity (court intervention into communal disputes, legislative restriction of religious authority) is protecting the secular monopoly rather than solving coordination problems. The shared time grid ensures every metric is authored at every examined point: no metric substitution or grid misalignment. The trajectory shows extraction accumulation over time (the classic mandatrophy pattern: a constraint built to solve a real problem gradually absorbs extracted rents as the problem declines), paired with rising theater (functional activity gives way to performative maintenance).
 *
 * PERSPECTIVAL GAP:
 *   Victim seats (minority communities) and beneficiary seats (secular coalition) experience radically different constraint types from the same structural arrangement. This gap is not a measurement error—it is the engine's function to compute and report it. The secularist reading instantiates the beneficiary frame; the story's omega variables (below) document the alternative frames and the structural ambiguities they expose.
 *
 * DIRECTIONALITY LOGIC:
 *   The secular modernist coalition is the structural beneficiary: it collects institutional power, legislative authority, and the political capital of 'modernization.' Its d value should be near 0.0–0.2 (beneficiary, powerful, arbitrage exit via legislative maneuvering). Minority religious communities are the structural victims: they lose legal authority over their affairs, face constant judicial override, and have no exit (trapped or identity-locked—they cannot abandon their religious identity to escape the constraint). Their d value should be near 0.8–1.0 (targets, moderate power, constrained/identity-locked exit). Gender equality advocates sit near the beneficiary end (d ~0.2–0.3) because they benefit from the equality-enforcing secular framework, even if they are not the primary power-wielders. Communal religious leaders are trapped targets (d ~0.9) with identity-locked exit—their institutional legitimacy is the thing the constraint attacks. Constitutional courts sit ambiguously: they are beneficiaries (gain jurisdiction and power) but also agenda-setters (they enforce, not just benefit). The engine derives d from beneficiary/victim declarations and exit modulation; the overrides are not needed here because the structural data is clean.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is real: legal pluralism creates jurisdictional conflicts, overlapping authority, and perpetuates gender inequalities in some personal law codes. The secularist reading solves this by consolidating authority into a uniform secular framework. However, the measurements show extraction rising while the founding problem (gender inequality in some personal law codes) is partially addressed and contested by alternative readings (gender-rights reading proposes constitutional equality review WITHOUT UCC; judicial-harmonization reading proposes case-by-case court review WITHOUT formal UCC). The theater ratio's rise from 0.28 to 0.41 suggests that later enforcement activity is increasingly performative—defending the secular legal monopoly rather than solving the coordination problem that justified it initially. The constraint exhibits classic mandatrophy dynamics: solving a real problem, then absorbing extractive rents as the problem recedes and the problem-solving apparatus becomes an end in itself. This is NOT a false-summit (mountain claim) because the constraint actively requires enforcement and generates real resistance (0.79 final resistance value shows communities defending against it). It IS a tangled_rope exhibiting mandatrophy signature: genuine coordination function paired with asymmetric extraction and institutional power consolidation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordinate_vs_extraction_boundary,
    'Is the consolidation of marriage authority into secular unified law STRUCTURALLY NECESSARY for genuine coordination (eliminating conflicts, enabling uniform enforcement), or is it EXCESS coordination whose primary function is extracting authority from communities?',
    'Natural experiment: jurisdictions that achieve uniform marriage law via federal framework WITH preserved communal autonomy (e.g., Switzerland''s shared civil-law base with cantonal variation on family-law details) vs. those that impose UCC via majoritarian unification. If coordination outcomes are equivalent, the excess consolidation is pure extraction.',
    'If coordinating function is separable from authority extraction, the tangled_rope classification holds (real coordination + asymmetric extraction). If consolidation is truly necessary, more extraction is justified cost and the constraint moves toward rope-classified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordinate_vs_extraction_boundary, empirical, 'Whether coordinate/extract functions are structurally separable in marriage law unification.').

omega_variable(
    communal_vs_majoritarian_autonomy,
    'Is the secularist reading''s displacement of personal law codes MODERNIZATION of outdated systems, or MAJORITARIAN DOMINATION of minority self-governance?',
    'Comparative constitutional analysis: did the displacement occur through (a) negotiated federalist settlement preserving minority autonomy (consociational), or (b) majoritarian override of minority objection (assimilationist)? Documentary evidence from legislative debates, minority testimony, constitution-making moments.',
    'If consociational settlement, the framing is legitimate protection. If majoritarian override, the ''anachronism'' narrative is cover story and the constraint is pure snare of minority subjection. The reading_relations value (coexists_with vs. forecloses) pivots on this determination.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(communal_vs_majoritarian_autonomy, conceptual, 'Whether authority transfer was negotiated federalism or majoritarian assimilation.').

omega_variable(
    identity_fusion_mechanism,
    'For communal religious leaders (identity_locked exit), is the exit-blocking driven by EXTERNAL coercion (loss of authority via law), or by INTERNAL identity fusion (the leader cannot abandon their role without destroying their self-concept)?',
    'Post-constraint-relaxation trajectory: if personal law frameworks are reinstated or courts withdraw, do leaders re-establish communal governance (external block) or do they continue identifying with secular legal roles (internal fusion)?',
    'If external coercion dominates, suppression is genuine structural barrier and constraint is high-extraction snare. If internal fusion dominates, leaders have internalized the secular logic and suppression persists even after external barriers lift—constraint has greater staying power and effective suppression may be higher than the scalar measure.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_fusion_mechanism, empirical, 'Whether identity-locking in communal leaders is external coercion or internalized identity fusion.').

omega_variable(
    kernel_reading_committer_ambiguity,
    'Does the secularist reading genuinely represent a coherent constitutional commitment shared by the secular-modernist coalition, or is it primarily a retrospective narrative covering a power grab by institutional actors (legislatures, courts) seeking expanded jurisdiction?',
    'Genealogical analysis: trace the secularist reading''s doctrinal development through legislative history, judicial opinions, academic foundations. Identify moments where the ''modernization'' narrative was authored vs. moments where institutional expansion occurred first and narrative followed. Look for evidence of deliberate choice vs. ex-post rationalization.',
    'If genuine commitment, the cs_structure authority_grounding should be ''lineage'' or ''expertise'' (doctrine with roots). If retroactive cover story, authority_grounding should be ''extraction'' (institutional actors using constitutional language to justify power consolidation). This affects whether the constraint is legitimately grounded or performatively maintained.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_committer_ambiguity, conceptual, 'Whether secularist reading is genuine constitutional doctrine or institutional power narrative.').

omega_variable(
    gender_equality_decoupling,
    'Is gender equality in marriage law STRUCTURALLY DEPENDENT on unified secular civil code, or can gender-equality protections be achieved within personal law frameworks via constitutional equality review (the gender_rights_reading route)?',
    'Comparative institutional analysis: jurisdictions where gender-equality reform occurred within personal law systems (Morocco, Tunisia, some Indian states) vs. those requiring full UCC. If equality is achievable without UCC, it becomes a sibling reading rather than a beneficiary of the secularist constraint.',
    'If gender equality is decoupled from UCC, the gender_equality_advocates seat shifts from beneficiary to observer/excluded—they benefit from the secularist outcome but do not structurally require it. This tightens the actual beneficiary set to the institutional power-consolidators alone, raising effective extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(gender_equality_decoupling, empirical, 'Whether gender-equality reform requires UCC or is achievable via constitutional equality review.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_authority__secularist_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t0, marriage_authority__secularist_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement_basis(marr_tr_t0, projected).
narrative_ontology:measurement(marr_tr_t8, marriage_authority__secularist_reading, theater_ratio, 8, 0.31).
narrative_ontology:measurement_basis(marr_tr_t8, observed).
narrative_ontology:measurement(marr_tr_t16, marriage_authority__secularist_reading, theater_ratio, 16, 0.36).
narrative_ontology:measurement_basis(marr_tr_t16, observed).
narrative_ontology:measurement(marr_tr_t24, marriage_authority__secularist_reading, theater_ratio, 24, 0.39).
narrative_ontology:measurement_basis(marr_tr_t24, observed).
narrative_ontology:measurement(marr_tr_t32, marriage_authority__secularist_reading, theater_ratio, 32, 0.4).
narrative_ontology:measurement_basis(marr_tr_t32, observed).
narrative_ontology:measurement(marr_tr_t40, marriage_authority__secularist_reading, theater_ratio, 40, 0.41).
narrative_ontology:measurement_basis(marr_tr_t40, projected).

% Extraction over time
narrative_ontology:measurement(marr_be_t0, marriage_authority__secularist_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement_basis(marr_be_t0, projected).
narrative_ontology:measurement(marr_be_t8, marriage_authority__secularist_reading, base_extractiveness, 8, 0.51).
narrative_ontology:measurement_basis(marr_be_t8, observed).
narrative_ontology:measurement(marr_be_t16, marriage_authority__secularist_reading, base_extractiveness, 16, 0.59).
narrative_ontology:measurement_basis(marr_be_t16, observed).
narrative_ontology:measurement(marr_be_t24, marriage_authority__secularist_reading, base_extractiveness, 24, 0.65).
narrative_ontology:measurement_basis(marr_be_t24, observed).
narrative_ontology:measurement(marr_be_t32, marriage_authority__secularist_reading, base_extractiveness, 32, 0.67).
narrative_ontology:measurement_basis(marr_be_t32, observed).
narrative_ontology:measurement(marr_be_t40, marriage_authority__secularist_reading, base_extractiveness, 40, 0.68).
narrative_ontology:measurement_basis(marr_be_t40, projected).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t0, marriage_authority__secularist_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement_basis(marr_su_t0, projected).
narrative_ontology:measurement(marr_su_t8, marriage_authority__secularist_reading, suppression_requirement, 8, 0.63).
narrative_ontology:measurement_basis(marr_su_t8, observed).
narrative_ontology:measurement(marr_su_t16, marriage_authority__secularist_reading, suppression_requirement, 16, 0.68).
narrative_ontology:measurement_basis(marr_su_t16, observed).
narrative_ontology:measurement(marr_su_t24, marriage_authority__secularist_reading, suppression_requirement, 24, 0.7).
narrative_ontology:measurement_basis(marr_su_t24, observed).
narrative_ontology:measurement(marr_su_t32, marriage_authority__secularist_reading, suppression_requirement, 32, 0.71).
narrative_ontology:measurement_basis(marr_su_t32, observed).
narrative_ontology:measurement(marr_su_t40, marriage_authority__secularist_reading, suppression_requirement, 40, 0.72).
narrative_ontology:measurement_basis(marr_su_t40, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_authority__secularist_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(marriage_authority__secularist_reading, 0.12).
narrative_ontology:affects_constraint(marriage_authority__secularist_reading, marriage_authority__communal_autonomy_reading).
narrative_ontology:affects_constraint(marriage_authority__secularist_reading, marriage_authority__gender_rights_reading).
narrative_ontology:affects_constraint(marriage_authority__secularist_reading, marriage_authority__judicial_harmonization_reading).
narrative_ontology:affects_constraint(marriage_authority__secularist_reading, marriage_authority__federalist_millet_reading).

% DUAL FORMULATION NOTE:
% The 'marriage_authority' kernel admits five structurally distinct readings instantiating different constraints. The secularist reading (this story) zero-sums against communal_autonomy (incompatible foundational premises: secular law vs. communal tradition as authority source) and coexists with judicial_harmonization and gender_rights readings (which accept secular authority but disagree on mechanism). The sibling constraints are linked via shared kernel_id; their ε values, beneficiary/victim structures, and types diverge substantially based on reading-specific framing. No single reading's narrative is 'true' in isolation—each is structurally consistent within its own framework and incompatible with others. The ε for the secularist reading is measured against the standing arrangement (personal law pluralism) AS SEEN BY THE SECULARIST READING (high extraction because authority transfer is viewed as justified modernization removing anachronistic privilege, not as majoritarian imposition). The sibling readings assess the same standing arrangement differently and author different ε values from their respective commitments.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(marriage_authority__secularist_reading, institutional, 0.15).
constraint_indexing:directionality_override(marriage_authority__secularist_reading, moderate, 0.82).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
