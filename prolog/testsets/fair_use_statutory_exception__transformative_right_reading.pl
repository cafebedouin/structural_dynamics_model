% ============================================================================
% CONSTRAINT STORY: fair_use_statutory_exception__transformative_right_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fair_use_transformative_right, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: fair_use_statutory_exception__transformative_right_reading
 *   human_readable: Fair Use as Transformative Right (Statutory Exception)
 *   domain: intellectual_property/legal_interpretation
 *
 * SUMMARY:
 *   The fair use doctrine (17 U.S.C. § 107) permits certain uses of
 *   copyrighted material without permission or payment. This constraint story
 *   instantiates ONE reading of the contested kernel 'fair use': the
 *   transformative right reading. Under this reading, fair use exists
 *   fundamentally to enable transformative reuse and cultural production;
 *   courts are tasked with facilitating innovation by recognizing
 *   transformation as a basis for protection and setting a shared burden of
 *   proof (original copyright holders must show substitution, not merely that
 *   a license could have been sold). This reading is held by courts
 *   (particularly Supreme Court majority in Campbell v. Acuff-Rose, 1994, and
 *   Google Books, 2015), legal scholars in the public interest IP movement,
 *   and digital creators and cultural institutions. Sibling
 *   readings—market_licensing_reading and narrow_defense_reading—contest this
 *   framing from different institutional positions. The constraint is CLAIMED
 *   as rope (coordination) because the reading establishes a shared right
 *   that enables cultural participation without transaction costs; the
 *   relatively low extractiveness (0.38) reflects the reading's premise that
 *   fair use reduces friction and enables innovation. However, the
 *   measurement series show slight upward drift in extractiveness and
 *   theater, suggesting increasing litigation burden and uncertainty as
 *   courts face harder cases (AI-generated works, large-scale digitization)
 *   that test the boundaries of transformative reuse.
 *
 * KEY AGENTS:
 *   - transformative_creators: Artists, filmmakers, scholars building on existing works; benefit from fair use protection; face licensing costs without it
 *   - cultural_commons_users: General public, fan communities, meme makers; benefit from participatory culture; powerless to negotiate licenses
 *   - educational_institutions: Schools, universities, libraries; benefit from transformation as legitimate use category; leverage fair use for research, teaching, archival preservation
 *   - original_copyright_holders: Authors, publishers, musicians; pay in foregone licensing fees; retain rights to substitutive uses
 *   - licensing_intermediaries: Rights clearance agencies, performing rights organizations; structurally excluded; their model depends on monetizing all uses
 *   - appellate_courts: Institutional agenda-setters; tasked with interpreting the statute and setting precedent; have discretion to recognize transformation and facilitate innovation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fair_use_statutory_exception__transformative_right_reading, 0.38).
domain_priors:suppression_score(fair_use_statutory_exception__transformative_right_reading, 0.31).
domain_priors:theater_ratio(fair_use_statutory_exception__transformative_right_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fair_use_statutory_exception__transformative_right_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(fair_use_statutory_exception__transformative_right_reading, suppression_requirement, 0.31).
narrative_ontology:constraint_metric(fair_use_statutory_exception__transformative_right_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fair_use_statutory_exception__transformative_right_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(fair_use_statutory_exception__transformative_right_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fair_use_statutory_exception__transformative_right_reading, rope).
narrative_ontology:human_readable(fair_use_statutory_exception__transformative_right_reading, "Fair Use as Transformative Right (Statutory Exception)").
narrative_ontology:topic_domain(fair_use_statutory_exception__transformative_right_reading, "intellectual_property/legal_interpretation").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fair_use_statutory_exception__transformative_right_reading, '9916280b-85fe-4180-b249-a0223cc636d8').
narrative_ontology:cs_kernel_codification('9916280b-85fe-4180-b249-a0223cc636d8', fixed_text).
narrative_ontology:cs_authority_grounding('9916280b-85fe-4180-b249-a0223cc636d8', lineage).
narrative_ontology:cs_interpretation_layer_present('9916280b-85fe-4180-b249-a0223cc636d8').
narrative_ontology:cs_reading_relation('9916280b-85fe-4180-b249-a0223cc636d8', fair_use_statutory_exception__market_licensing_reading, forecloses).
narrative_ontology:cs_reading_relation('9916280b-85fe-4180-b249-a0223cc636d8', fair_use_statutory_exception__narrow_defense_reading, influences).
narrative_ontology:cs_axiom('9916280b-85fe-4180-b249-a0223cc636d8', foundational, transformation_enables_public_good).
narrative_ontology:cs_axiom_status(transformation_enables_public_good, holdable).
narrative_ontology:cs_axiom_grounding('9916280b-85fe-4180-b249-a0223cc636d8', transformation_enables_public_good, deontological).
narrative_ontology:cs_axiom('9916280b-85fe-4180-b249-a0223cc636d8', foundational, courts_have_discretion_to_recognize_innovation).
narrative_ontology:cs_axiom_status(courts_have_discretion_to_recognize_innovation, holdable).
narrative_ontology:cs_axiom_grounding('9916280b-85fe-4180-b249-a0223cc636d8', courts_have_discretion_to_recognize_innovation, instrumental).
narrative_ontology:cs_reference_frame('9916280b-85fe-4180-b249-a0223cc636d8', copyright_as_bounded_incentive_regime).
narrative_ontology:cs_drift_state('9916280b-85fe-4180-b249-a0223cc636d8', contemporary_digital_creativity_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('9916280b-85fe-4180-b249-a0223cc636d8', '').
narrative_ontology:cs_kernel_id(fair_use_statutory_exception__transformative_right_reading, fair_use_statutory_exception).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fair_use_statutory_exception__transformative_right_reading, transformative_creators).
narrative_ontology:constraint_beneficiary(fair_use_statutory_exception__transformative_right_reading, cultural_commons_users).
narrative_ontology:constraint_beneficiary(fair_use_statutory_exception__transformative_right_reading, educational_institutions).
narrative_ontology:constraint_victim(fair_use_statutory_exception__transformative_right_reading, original_copyright_holders_denied_licensing_fees).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(fair_use_statutory_exception__transformative_right_reading, original_copyright_holders).
narrative_ontology:constraint_vindicates(fair_use_statutory_exception__transformative_right_reading, transformative_reuse_doctrine).
narrative_ontology:constraint_vindicates(fair_use_statutory_exception__transformative_right_reading, innovation_as_public_good).
narrative_ontology:constraint_vindicates(fair_use_statutory_exception__transformative_right_reading, copyright_as_bounded_property).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Artists, filmmakers, scholars, and remixers who build on existing copyrighted works to create new meaning or commentary. Under this reading, they have a right to use existing works without licensing when the use is transformative (adds new expression, meaning, or message). Without fair use, they face the choice of paying for licenses they may not afford, getting permission from rights-holders who may refuse, or abandoning the transformative use entirely.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__transformative_right_reading, transformative_creators, beneficiary,
    moderate, biographical, constrained, global).

% The general public and community creators—fan artists, meme makers, musicians sampling historical recordings, archivists preserving cultural heritage. They depend on fair use to participate in cultural production and commentary. Under the transformative right reading, fair use is a public accommodation ensuring that cultural participation is not gatekept by licensing fees.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__transformative_right_reading, cultural_commons_users, beneficiary,
    powerless, biographical, constrained, global).

% Schools, universities, and libraries that use copyrighted material in teaching, research, and archival work. Under this reading, transformative educational use is protected—a student's essay quoting and analyzing a poem, a professor's lecture slides excerpting journal articles, a library's digitization of out-of-print works for preservation. The constraint protects education as a transformative, public-good use.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__transformative_right_reading, educational_institutions, beneficiary,
    organized, generational, constrained, national).

% Authors, publishers, filmmakers, and music labels who control the original works. Under this reading, they bear the cost of foregone licensing fees when transformative uses occur. They do not have the ability to prevent transformative reuse or demand compensation. Their remedy is to argue that a use is not transformative, but the burden is shared—they must demonstrate substitution, not merely show that a license could have been sold.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__transformative_right_reading, original_copyright_holders, payer,
    powerful, biographical, mobile, global).

% Rights clearance agencies, performing rights organizations, and collective licensing bodies that profit from controlling access to copyrighted works. Under the transformative right reading, they are excluded from the core conversation about what constitutes fair use, because their business model (monetizing all uses) is in structural conflict with a broad fair use doctrine that privileges transformation over licensing.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__transformative_right_reading, licensing_intermediaries, excluded,
    organized, biographical, trapped, global).

% Federal courts, particularly the Supreme Court and appeals courts, which interpret the fair use statute (17 U.S.C. § 107) and set binding precedent. Under this reading, courts are the primary institutional agents tasked with FACILITATING innovation by recognizing transformative uses and setting a low-friction evidentiary burden on defendants. Courts decide the content and scope of the exception; their interpretation determines what uses are protected.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__transformative_right_reading, appellate_courts, agenda_setter,
    institutional, generational, analytical, national).

% Congress, which enacted the fair use statute and could amend it. The legislature observes how courts interpret the exception but has not substantially revised the statute in decades, effectively delegating interpretive authority to the judiciary. They could narrow or broaden fair use, but have chosen to remain in the background.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__transformative_right_reading, legislative_body, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(fair_use_statutory_exception__transformative_right_reading, original_copyright_holders).
narrative_ontology:fixing_cost_class(fair_use_statutory_exception__transformative_right_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allocates the right to create new cultural and scholarly works from existing copyrighted material without requiring licensing approval. Solves the coordination problem of enabling innovation and cultural participation without requiring transaction costs (licensing fees, clearance negotiations) that might choke off transformative reuse entirely.
% TRANSFER_FUNCTION: Transfers the right to reuse copyrighted expression (without paying licensing fees) from the original copyright holder to transformative creators, cultural commons users, and educational institutions. The copyright holder loses the exclusive right to license transformative uses but retains rights to substitutive uses and direct reproduction.
% ABSENT_VOICES: Licensing intermediaries and rights clearance agencies are structurally excluded from the conversation about what constitutes fair use, because their business model depends on monetizing ALL uses (transformative or not) and they would object to any broad exception. Subsistence-level creators—artists who depend on licensing revenue from their works—are also largely absent, though they are present as copyright holders in the structure.
% DISAPPEARANCE_RATIONALE: If the transformative right reading of fair use disappeared—if courts ceased to recognize transformation as a basis for protection—cultural production and scholarship would reorganize around licensing markets. Remixes, parodies, critical analyses, and educational uses would require permission and payment. The volume of creative reuse would contract. Public discourse would shift: fewer pieces of commentary would reference and build on existing works without licensing. The internet's remix culture and fan communities would face legal jeopardy. Academic research would be more cautious in quoting and analyzing published works.
% FOUNDING_PROBLEM: Copyright was designed to incentivize original authorship, not to allow original authors to monopolize all derivative expression. Early copyright doctrine recognized that some reuse (commentary, criticism, scholarship) should not require permission because restricting it would chill innovation and cultural dialogue. The fair use exception was codified in statute (1976) to carve out space for uses that serve public purposes—education, criticism, commentary, news reporting.
% FOUNDING_PROBLEM_CORROBORATION: The Supreme Court (Campbell v. Acuff-Rose Music, 1994; Google Books, 2015) has repeatedly affirmed that fair use exists to enable transformative reuse and that courts should facilitate innovation. Legal scholars and IP economists (including testimony before Congress by James Grimmelmann, Pamela Samuelson, and others) outside the copyright-holder lobby attest that the founding problem—the need for safe harbor for transformative uses—persists and that broad licensing markets do not eliminate it. The Cato Institute and public interest law organizations have endorsed this reading. However, the publishing and entertainment industries and their representatives contest this characterization, arguing that licensing markets should be the default mechanism.
narrative_ontology:disappearance_verdict(fair_use_statutory_exception__transformative_right_reading, world_rearranges).
narrative_ontology:founding_problem_status(fair_use_statutory_exception__transformative_right_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fair_use_statutory_exception__transformative_right_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(fair_use_statutory_exception__transformative_right_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fair_use_statutory_exception__transformative_right_reading_tests).
:- end_tests(fair_use_statutory_exception__transformative_right_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is set at 0.38 because the transformative right reading protects many uses (educational, critical, transformative) without licensing, reducing copyright holders' licensing revenue. However, it is not extreme because copyright holders retain full rights to substitutive uses (direct copying, commercial reproduction without transformation) and can argue that specific uses are not truly transformative. The measurement series remain relatively flat (slight upward drift from 0.32 to 0.38 over 50 years) because the legal framework has not fundamentally shifted, though individual cases have pushed boundaries. Theater_ratio is low (0.22) because the fair use doctrine, while litigated, has a genuine coordination function—it actually enables cultural reuse that would otherwise be impossible. Suppression_requirement is low (0.31) because the doctrine does not require coercive enforcement; instead, courts adjudicate disputes when they arise. The accessibility_collapse (0.62) is moderate because alternatives do exist: creators can seek permission, negotiate licenses, or create original works—but these alternatives are expensive and slow, so fair use is a functional shortcut that collapses the search for alternatives once discovered. Resistance (0.58) is moderate-high because copyright holders and licensing intermediaries actively resist broad fair use doctrine, arguing for narrow construction and market-based licensing, while creators and cultural institutions push back.
 *
 * PERSPECTIVAL GAP:
 *   From the creator and institutional seats, this reading is genuine coordination: it solves a real problem (expensive licensing blocking innovation) with a functional tool (sharing the burden of proof). From the copyright-holder seat, particularly for large entertainment and publishing companies, the reading is extraction: it reduces their licensing revenue and shifts the burden of proof onto them. From the appeals court seat, it is a mandate to facilitate innovation, which requires constant interpretation and boundary-setting as technology and cultural practices change. The appellate court's position is distinctive because courts experience the constraint as an INTERPRETIVE BURDEN, not merely as winners or losers in a static distribution. The engine should compute different types for these seats: beneficiary and creator seats should show rope; copyright holder seats should show partial snare (they bear costs but retain substantial rights); court seats should show scaffold (temporary burden pending legislative clarity or settled jurisprudence).
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (transformative_creators, cultural_commons_users, educational_institutions) have low directionality (d near 0.0–0.3) because the reading BENEFITS them—they get the right to reuse without licensing. The copyright_holders (payer, d near 0.7–0.9) face costs (foregone licensing revenue) but retain substantial rights; they are not trapped, because they can still enforce against substitutive uses and negotiate licenses for transformative works (though they rarely do). Appellate courts (agenda_setter, d near 0.5) have moderate directionality because they bear the institutional cost of adjudication but derive legitimacy and interpretive authority from the role. The licensed_intermediaries are excluded, not coordinated—their exclusion is structural to the reading; they would have extremely high d if included, because their business model requires licensing ALL uses.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is LIVE: courts continue to encounter new technologies and creative practices that test the bounds of transformative reuse (AI training, large-scale digitization, remix culture). The constraint persists precisely because the founding problem persists. However, mandatrophy risk is present in the institutional architecture: if the Supreme Court shifts its composition and narrows the transformative test (as suggested by recent concurrences), the mandate could become incoherent—courts tasked with 'facilitating innovation' would lack the doctrinal tools to do so. The measurement data show stable, not declining, extractiveness, which suggests the constraint has not degraded into pure theater; it continues to serve a coordination function. The slight upward drift in theater_ratio (0.18 to 0.22) reflects increasing litigation complexity and the rhetorical work courts must do to justify fair use in novel contexts, but this is not yet performance substituting for function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    institutional_authority_over_the_kernel,
    'Does the transformative right reading claim that courts have the primary authority to define fair use (as the reading''s language ''courts must facilitate innovation'' suggests), or does it acknowledge that Congress retained authority to narrow or broaden the exception?',
    'Doctrinal analysis of how courts have framed their role: as interpreters of a statute Congress wrote (delegated authority) or as custodians of a fundamental principle Congress enshrined. Compare with cases where Congress DID amend fair use (Digital Millennium Copyright Act, § 1201; Copyright Term Extension Act). If courts have treated Congressional amendments as supreme, the reading''s institutional claim is limited; if courts have resisted Congressional narrowing, the claim is stronger.',
    'If courts are subordinate interpreters, the reading is less stable—Congress could override it. If courts have primacy, the reading is more stable but faces pushback from copyright industries lobbying Congress. This affects the long-term persistence of the constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_authority_over_the_kernel, conceptual, 'Institutional authority distribution: does the transformative reading claim primacy for courts?').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fair_use_statutory_exception__transformative_right_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fair_tr_t0, fair_use_statutory_exception__transformative_right_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(fair_tr_t0, observed).
narrative_ontology:measurement(fair_tr_t10, fair_use_statutory_exception__transformative_right_reading, theater_ratio, 10, 0.2).
narrative_ontology:measurement_basis(fair_tr_t10, observed).
narrative_ontology:measurement(fair_tr_t20, fair_use_statutory_exception__transformative_right_reading, theater_ratio, 20, 0.22).
narrative_ontology:measurement_basis(fair_tr_t20, observed).
narrative_ontology:measurement(fair_tr_t30, fair_use_statutory_exception__transformative_right_reading, theater_ratio, 30, 0.23).
narrative_ontology:measurement_basis(fair_tr_t30, observed).
narrative_ontology:measurement(fair_tr_t40, fair_use_statutory_exception__transformative_right_reading, theater_ratio, 40, 0.22).
narrative_ontology:measurement_basis(fair_tr_t40, observed).
narrative_ontology:measurement(fair_tr_t50, fair_use_statutory_exception__transformative_right_reading, theater_ratio, 50, 0.22).
narrative_ontology:measurement_basis(fair_tr_t50, observed).

% Extraction over time
narrative_ontology:measurement(fair_be_t0, fair_use_statutory_exception__transformative_right_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement_basis(fair_be_t0, observed).
narrative_ontology:measurement(fair_be_t10, fair_use_statutory_exception__transformative_right_reading, base_extractiveness, 10, 0.35).
narrative_ontology:measurement_basis(fair_be_t10, observed).
narrative_ontology:measurement(fair_be_t20, fair_use_statutory_exception__transformative_right_reading, base_extractiveness, 20, 0.38).
narrative_ontology:measurement_basis(fair_be_t20, observed).
narrative_ontology:measurement(fair_be_t30, fair_use_statutory_exception__transformative_right_reading, base_extractiveness, 30, 0.4).
narrative_ontology:measurement_basis(fair_be_t30, observed).
narrative_ontology:measurement(fair_be_t40, fair_use_statutory_exception__transformative_right_reading, base_extractiveness, 40, 0.38).
narrative_ontology:measurement_basis(fair_be_t40, observed).
narrative_ontology:measurement(fair_be_t50, fair_use_statutory_exception__transformative_right_reading, base_extractiveness, 50, 0.38).
narrative_ontology:measurement_basis(fair_be_t50, observed).

% Suppression requirement over time
narrative_ontology:measurement(fair_su_t0, fair_use_statutory_exception__transformative_right_reading, suppression_requirement, 0, 0.28).
narrative_ontology:measurement_basis(fair_su_t0, observed).
narrative_ontology:measurement(fair_su_t10, fair_use_statutory_exception__transformative_right_reading, suppression_requirement, 10, 0.3).
narrative_ontology:measurement_basis(fair_su_t10, observed).
narrative_ontology:measurement(fair_su_t20, fair_use_statutory_exception__transformative_right_reading, suppression_requirement, 20, 0.31).
narrative_ontology:measurement_basis(fair_su_t20, observed).
narrative_ontology:measurement(fair_su_t30, fair_use_statutory_exception__transformative_right_reading, suppression_requirement, 30, 0.32).
narrative_ontology:measurement_basis(fair_su_t30, observed).
narrative_ontology:measurement(fair_su_t40, fair_use_statutory_exception__transformative_right_reading, suppression_requirement, 40, 0.31).
narrative_ontology:measurement_basis(fair_su_t40, observed).
narrative_ontology:measurement(fair_su_t50, fair_use_statutory_exception__transformative_right_reading, suppression_requirement, 50, 0.31).
narrative_ontology:measurement_basis(fair_su_t50, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fair_use_statutory_exception__transformative_right_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(fair_use_statutory_exception__transformative_right_reading, 0.12).
narrative_ontology:affects_constraint(fair_use_statutory_exception__transformative_right_reading, fair_use_statutory_exception__market_licensing_reading).
narrative_ontology:affects_constraint(fair_use_statutory_exception__transformative_right_reading, fair_use_statutory_exception__narrow_defense_reading).
narrative_ontology:affects_constraint(fair_use_statutory_exception__transformative_right_reading, copyright_term_extension_incentive_problem).
narrative_ontology:affects_constraint(fair_use_statutory_exception__transformative_right_reading, digital_millennium_copyright_act_anti_circumvention_provision).

% DUAL FORMULATION NOTE:
% The fair_use_statutory_exception kernel decomposes into three structurally distinct constraint stories, each instantiating a different reading of 17 U.S.C. § 107. The transformative_right_reading (this story) claims low ε for transformative uses and assumes shared burden of proof; market_licensing_reading claims high ε for any use with a licensing market; narrow_defense_reading claims courts should narrowly construe the exception to preserve copyright holders' property rights. All three readings interpret the same statute but have radically different ε assumptions and beneficiary/victim structures. They are linked by network.affects_constraints: the transformative reading influences the market-licensing reading (if transformation is protected, licensing markets are less lucrative) and forecloses aspects of the narrow reading (courts cannot both facilitate innovation and narrowly construe).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(fair_use_statutory_exception__transformative_right_reading, institutional, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
