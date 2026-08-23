% ============================================================================
% CONSTRAINT STORY: ip_category_emergence__thinkability_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ip_category_emergence__thinkability_reading, []).

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
 *   constraint_id: ip_category_emergence__thinkability_reading
 *   human_readable: Intellectual Property as Emergent Category of Ownable Expression (Thinkability Reading)
 *   domain: legal/historical/philosophical
 *
 * SUMMARY:
 *   This constraint story instantiates the thinkability reading of the
 *   ip_category_emergence kernel. The standing arrangement is the
 *   legal-philosophical framework that treats expression as a distinct,
 *   ownable form of propertyâa category that this reading identifies as
 *   becoming legally coherent with the Statute of Anne in 1710. Prior to that
 *   moment, disputes over printed works were framed through guild privilege,
 *   press licensing, or personal patronage; after 1710, 'copy right' operates
 *   as a statutory property concept that reshapes the conceptual space of
 *   law. The story is authored as one of three sibling constraints; the
 *   first_holding_reading emphasizes the author-as-claimant, while the
 *   synchronic_diachronic_seam questions whether thinkability and
 *   first-holding are analytically separable at all.
 *
 * KEY AGENTS:
 *   - rights_holders: Primary beneficiary (organized/constrained) â collect legal monopoly over expression derived from the statutory category
 *   - publishing_industry: Primary beneficiary and gain capturer (institutional/arbitrage) â exploits rights and lobbies for category expansion
 *   - public_domain_users: Primary target (powerless/constrained) â bear the diffuse cost of enclosure and lose unlicensed access
 *   - unlicensed_creators: Secondary target (moderate/constrained) â face liability and licensing friction when building on prior expression
 *   - courts_and_legislatures: Agenda setter (institutional/analytical) â maintains, interprets, and extends the legal category
 *   - legal_historians: Analytical observer (analytical/analytical) â evaluates whether 1710 marks genuine conceptual emergence or retrospective construction
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ip_category_emergence__thinkability_reading, 0.62).
domain_priors:suppression_score(ip_category_emergence__thinkability_reading, 0.45).
domain_priors:theater_ratio(ip_category_emergence__thinkability_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ip_category_emergence__thinkability_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(ip_category_emergence__thinkability_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(ip_category_emergence__thinkability_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ip_category_emergence__thinkability_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(ip_category_emergence__thinkability_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ip_category_emergence__thinkability_reading, tangled_rope).
narrative_ontology:human_readable(ip_category_emergence__thinkability_reading, "Intellectual Property as Emergent Category of Ownable Expression (Thinkability Reading)").
narrative_ontology:topic_domain(ip_category_emergence__thinkability_reading, "legal/historical/philosophical").

domain_priors:requires_active_enforcement(ip_category_emergence__thinkability_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ip_category_emergence__thinkability_reading, '15f6b544-c275-4c5c-9221-a3f6f934ba50').
narrative_ontology:cs_kernel_codification('15f6b544-c275-4c5c-9221-a3f6f934ba50', fixed_text).
narrative_ontology:cs_authority_grounding('15f6b544-c275-4c5c-9221-a3f6f934ba50', lineage).
narrative_ontology:cs_interpretation_layer_present('15f6b544-c275-4c5c-9221-a3f6f934ba50').
narrative_ontology:cs_reading_relation('15f6b544-c275-4c5c-9221-a3f6f934ba50', ip_category_emergence__first_holding_reading, coexists_with).
narrative_ontology:cs_reading_relation('15f6b544-c275-4c5c-9221-a3f6f934ba50', ip_category_emergence__synchronic_diachronic_seam, influences).
narrative_ontology:cs_axiom('15f6b544-c275-4c5c-9221-a3f6f934ba50', foundational, expression_as_ownable_category).
narrative_ontology:cs_axiom_status(expression_as_ownable_category, holdable).
narrative_ontology:cs_axiom_grounding('15f6b544-c275-4c5c-9221-a3f6f934ba50', expression_as_ownable_category, conventional).
narrative_ontology:cs_axiom('15f6b544-c275-4c5c-9221-a3f6f934ba50', foundational, statutory_moment_1710).
narrative_ontology:cs_axiom_status(statutory_moment_1710, holdable).
narrative_ontology:cs_axiom_grounding('15f6b544-c275-4c5c-9221-a3f6f934ba50', statutory_moment_1710, conventional).
narrative_ontology:cs_reference_frame('15f6b544-c275-4c5c-9221-a3f6f934ba50', anne_1710_statutory_coherence).
narrative_ontology:cs_drift_state('15f6b544-c275-4c5c-9221-a3f6f934ba50', contemporary_digital_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('15f6b544-c275-4c5c-9221-a3f6f934ba50', '').
narrative_ontology:cs_kernel_id(ip_category_emergence__thinkability_reading, ip_category_emergence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ip_category_emergence__thinkability_reading, rights_holders).
narrative_ontology:constraint_beneficiary(ip_category_emergence__thinkability_reading, publishing_industry).
narrative_ontology:constraint_victim(ip_category_emergence__thinkability_reading, public_domain_users).
narrative_ontology:constraint_victim(ip_category_emergence__thinkability_reading, unlicensed_creators).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold legally enforceable monopolies over specific expressions of ideas, derived from statutory and common law frameworks tracing to 1710. They license, assign, or enforce these rights against unauthorized users, and their economic position depends on the legal coherence of expression-as-property.
narrative_ontology:constraint_stakeholder(ip_category_emergence__thinkability_reading, rights_holders, beneficiary,
    organized, biographical, constrained, global).

% Acquires rights from creators through assignment and exploits them through reproduction, distribution, and licensing. Actively lobbies for term extension, subject-matter expansion, and enforcement harmonization. Captures a substantial share of the revenue generated by the ownable-expression category.
narrative_ontology:constraint_stakeholder(ip_category_emergence__thinkability_reading, publishing_industry, beneficiary,
    institutional, generational, arbitrage, global).

% Includes readers, educators, archivists, and downstream speakers who would use existing expression without license if the category did not enclose it. They face statutory barriers to access and reuse, and bear the diffuse cost of enclosure without individualized bargaining power.
narrative_ontology:constraint_stakeholder(ip_category_emergence__thinkability_reading, public_domain_users, payer,
    powerless, immediate, constrained, global).

% Create new expression but must either license prior works or risk infringement liability. The category constrains their freedom to build on existing culture, requiring legal navigation and transaction costs that reduce creative autonomy and economic return.
narrative_ontology:constraint_stakeholder(ip_category_emergence__thinkability_reading, unlicensed_creators, payer,
    moderate, biographical, constrained, global).

% Maintain, interpret, and periodically revise the statutory and doctrinal framework that makes expression legally ownable. They set the boundaries of subject matter, duration, and exceptions, and their interpretations determine whether the category expands or contracts.
narrative_ontology:constraint_stakeholder(ip_category_emergence__thinkability_reading, courts_and_legislatures, agenda_setter,
    institutional, generational, analytical, national).

% Study the conceptual and legal history of intellectual property, debating whether 1710 marks a genuine epistemic rupture or a rhetorical rebranding of older guild privileges. They operate outside the economic flows of the constraint and assess its historical coherence.
narrative_ontology:constraint_stakeholder(ip_category_emergence__thinkability_reading, legal_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ip_category_emergence__thinkability_reading, publishing_industry).
narrative_ontology:fixing_cost_class(ip_category_emergence__thinkability_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Organizes markets for creative and expressive works by assigning exclusive, tradeable rights that allow investment in reproduction and distribution to be recouped through controlled sale and licensing.
% TRANSFER_FUNCTION: Moves control over the use and reproduction of expression from the general public and unlicensed creators to statutory rights-holders and their assignees, typically in exchange for license fees or under threat of legal penalty.
% ABSENT_VOICES: Pre-1710 guild privilege holders who saw expression as a regulated craft rather than a property right; contemporary open-access and commons advocates who reject the property framing entirely; and scholars of the synchronic-diachronic seam who argue that thinkability and first-holding are analytically indistinguishable.
% DISAPPEARANCE_RATIONALE: If the legal category of ownable expression vanished, modern publishing, entertainment, software licensing, and academic journal economics would collapse or reorganize around patronage, guild-like monopoly, or unfettered commons models. The statutory and contractual infrastructure of the creative industries depends on this thinkability.
% FOUNDING_PROBLEM: The collapse of the Stationers' Company licensing monopoly and the lapse of the Licensing Act created a vacuum in which printed works lacked a coherent legal basis for exclusivity; publishers needed a new justification for control, and authors sought recognition of a personal right in their works.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians such as Mark Rose and Lyman Ray Patterson, writing from outside the publishing industry and rights-holder lobbies, attest that the Statute of Anne emerged from publisher lobbying and strategic author-claims rather than from a disinterested public need for a new property category.
narrative_ontology:disappearance_verdict(ip_category_emergence__thinkability_reading, world_rearranges).
narrative_ontology:founding_problem_status(ip_category_emergence__thinkability_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ip_category_emergence__thinkability_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(ip_category_emergence__thinkability_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ip_category_emergence__thinkability_reading, 0.62, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ip_category_emergence__thinkability_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ip_category_emergence__thinkability_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ip_category_emergence__thinkability_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.62) reflects the substantial enclosure of expression into tradeable property over the interval: term extension, subject-matter expansion, and international harmonization have progressively removed expressive works from the commons. Suppression (0.45) is moderate because the constraint relies on civil and criminal legal enforcement rather than direct violence, but it is structurally active across jurisdictions. Theater ratio (0.25) is low-moderate: the 'promotion of learning' justification retains some functional content, yet an increasing share of enforcement and lobbying activity is ritualistic maintenance of publisher rents. Accessibility collapse (0.60) is moderately high because once the property framing is in place, non-proprietary alternatives (commons-based, patronage-based, or guild-based) become legally unintelligible or structurally disadvantaged. Resistance (0.40) reflects persistent open-access, copyleft, and public-domain advocacy that challenges the category's expansion. Measurements track monotonic extraction accumulation and slowly rising theatricality on a shared time grid.
 *
 * PERSPECTIVAL GAP:
 *   The rights-holder and publishing-industry seats experience the constraint as legitimate property and market organization; the public-domain-user and unlicensed-creator seats experience it as enclosure and friction. The agenda-setter seat (courts and legislatures) experiences it as a doctrinal tradition to be maintained and incrementally extended. The engine will compute these seats differently: beneficiaries with constrained but organized power will derive low directionality, while diffuse and constrained payers will derive high directionality.
 *
 * DIRECTIONALITY LOGIC:
 *   Rights-holders and the publishing industry are structural beneficiaries of the ownable-expression category: the constraint subsidizes their exclusive control and revenue streams, yielding low directionality. Public-domain users and unlicensed creators are structural targets: they pay through exclusion, licensing cost, and liability risk, yielding high directionality. Courts and legislatures sit near the symmetric point but skew toward low directionality because they administer and reproduce the constraint without bearing its costs directly. Legal historians occupy an analytical exit position with near-neutral directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâorganizing expression markets after the collapse of press licensingâwas substantially solved by the emergence of the category itself. However, the arrangement has not atrophied into a piton because the coordination function (market organization for creative industries) remains live and because concentrated beneficiaries (the publishing industry) actively capture and defend the extraction. The constraint is not a scaffold because it carries no sunset clause; it is not a snare because the coordination component is genuine and not merely cover. The contested status of the founding problem, combined with ongoing resistance, places it in tangled-rope territory where both coordination and extraction are structurally real.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    category_coherence_vs_construction,
    'Is the legal coherence of ownable expression in 1710 a genuine conceptual discovery or a retrospective construction by later legal historians?',
    'Comparative philological and archival analysis of pre-1710 legal pleadings, guild registers, and parliamentary debates to determine whether ''copy right'' operated as a distinct legal concept before statutory codification.',
    'If retrospective, the constraint''s naturalized appearance is a false summit and its extraction is obscured by historical mythology; if genuine, the 1710 date marks a real coordination threshold.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(category_coherence_vs_construction, conceptual, 'Whether 1710 thinkability is discovered or constructed').

omega_variable(
    thinkability_first_holding_independence,
    'Does the thinkability of expression-as-property logically require the author-as-rights-holder, or can the category cohere with publisher-as-holder?',
    'Doctrinal analysis of early statutory interpretation and subsequent case law to test whether the category of ownable expression collapses when authors are removed from the claimant set.',
    'If dependent, the thinkability reading is not structurally independent and collapses toward the first-holding reading or the synchronic-diachronic seam reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(thinkability_first_holding_independence, conceptual, 'Logical independence of thinkability from first-holding').

omega_variable(
    guild_privilege_continuity,
    'To what extent did the 1710 Statute of Anne continue versus rupture the Stationers'' Company guild monopoly?',
    'Archival and statutory text analysis comparing enforcement patterns, claimant identities, and economic flows before and after 1710.',
    'High continuity would raise extractiveness (old monopoly in new dress) and tilt classification toward snare; rupture would support a coordination reading of the category emergence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(guild_privilege_continuity, empirical, 'Continuity between guild privilege and statutory copyright').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ip_category_emergence__thinkability_reading, 0, 314).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ip_thinkability_tr_t0, ip_category_emergence__thinkability_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(ip_thinkability_tr_t40, ip_category_emergence__thinkability_reading, theater_ratio, 40, 0.12).
narrative_ontology:measurement(ip_thinkability_tr_t80, ip_category_emergence__thinkability_reading, theater_ratio, 80, 0.15).
narrative_ontology:measurement(ip_thinkability_tr_t120, ip_category_emergence__thinkability_reading, theater_ratio, 120, 0.17).
narrative_ontology:measurement(ip_thinkability_tr_t160, ip_category_emergence__thinkability_reading, theater_ratio, 160, 0.2).
narrative_ontology:measurement(ip_thinkability_tr_t200, ip_category_emergence__thinkability_reading, theater_ratio, 200, 0.22).
narrative_ontology:measurement(ip_thinkability_tr_t260, ip_category_emergence__thinkability_reading, theater_ratio, 260, 0.24).
narrative_ontology:measurement(ip_thinkability_tr_t314, ip_category_emergence__thinkability_reading, theater_ratio, 314, 0.25).

% Extraction over time
narrative_ontology:measurement(ip_thinkability_be_t0, ip_category_emergence__thinkability_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(ip_thinkability_be_t40, ip_category_emergence__thinkability_reading, base_extractiveness, 40, 0.35).
narrative_ontology:measurement(ip_thinkability_be_t80, ip_category_emergence__thinkability_reading, base_extractiveness, 80, 0.4).
narrative_ontology:measurement(ip_thinkability_be_t120, ip_category_emergence__thinkability_reading, base_extractiveness, 120, 0.45).
narrative_ontology:measurement(ip_thinkability_be_t160, ip_category_emergence__thinkability_reading, base_extractiveness, 160, 0.5).
narrative_ontology:measurement(ip_thinkability_be_t200, ip_category_emergence__thinkability_reading, base_extractiveness, 200, 0.55).
narrative_ontology:measurement(ip_thinkability_be_t260, ip_category_emergence__thinkability_reading, base_extractiveness, 260, 0.6).
narrative_ontology:measurement(ip_thinkability_be_t314, ip_category_emergence__thinkability_reading, base_extractiveness, 314, 0.62).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(ip_category_emergence__thinkability_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(ip_category_emergence__thinkability_reading, first_holding_reading).
narrative_ontology:affects_constraint(ip_category_emergence__thinkability_reading, synchronic_diachronic_seam).

% DUAL FORMULATION NOTE:
% This story is one reading of the ip_category_emergence kernel, decomposed per the Îµ-invariance principle. The thinkability reading, first_holding_reading, and synchronic_diachronic_seam are distinct constraints with different structural data, beneficiary/victim profiles, and Îµ values. Each models a different claim about what changed in 1710.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
