% ============================================================================
% CONSTRAINT STORY: ip_category_emergence__first_holding_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ip_category_emergence__first_holding_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: ip_category_emergence__first_holding_reading
 *   human_readable: IP as Occupancy Shift: Statute of Anne First-Holding Reading
 *   domain: legal/intellectual_property/historical_jurisprudence
 *
 * SUMMARY:
 *   This constraint instantiates the first_holding_reading of the
 *   ip_category_emergence kernel. It treats the Statute of Anne (1710) not as
 *   the emergence of a new legal category but as an occupancy shift: the
 *   moment when authors entered the legitimate claimant set, displacing the
 *   Stationers' Company monopoly. The occupancy analogy treats intellectual
 *   expression as a resource susceptible to first possession, with the
 *   statute performing a function analogous to livery of seisin. The sibling
 *   readings contest this framing: thinkability_reading treats 1710 as the
 *   moment ownable expression became legally coherent, while
 *   synchronic_diachronic_seam collapses the distinction as a temporal
 *   artifact. The constraint persists in contemporary copyright doctrine as a
 *   justification for strong, assignable authorial rights.
 *
 * KEY AGENTS:
 *   - statutory_authors: Nominal beneficiaries (moderate/constrained) â hold rights but frequently assign them.
 *   - rights_assignee_publishers: Agenda-setters (institutional/arbitrage) â administer enforcement and capture monopoly rents.
 *   - unlicensed_reproducers: Primary targets (moderate/constrained) â bear enforcement costs and penalties.
 *   - access_seeking_public: Diffuse payers (organized/constrained) â pay inflated prices and face access restrictions.
 *   - stationers_company: Excluded historical actor (organized/constrained) â lost monopoly legitimacy post-1710.
 *   - legal_occupancy_interpreters: Analytical observers (institutional/analytical) â maintain the occupancy doctrinal architecture.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ip_category_emergence__first_holding_reading, 0.68).
domain_priors:suppression_score(ip_category_emergence__first_holding_reading, 0.75).
domain_priors:theater_ratio(ip_category_emergence__first_holding_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ip_category_emergence__first_holding_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(ip_category_emergence__first_holding_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(ip_category_emergence__first_holding_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ip_category_emergence__first_holding_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(ip_category_emergence__first_holding_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ip_category_emergence__first_holding_reading, tangled_rope).
narrative_ontology:human_readable(ip_category_emergence__first_holding_reading, "IP as Occupancy Shift: Statute of Anne First-Holding Reading").
narrative_ontology:topic_domain(ip_category_emergence__first_holding_reading, "legal/intellectual_property/historical_jurisprudence").

domain_priors:requires_active_enforcement(ip_category_emergence__first_holding_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ip_category_emergence__first_holding_reading, 'e1c89fb8-822b-4125-9bc3-ab26294036ef').
narrative_ontology:cs_kernel_codification('e1c89fb8-822b-4125-9bc3-ab26294036ef', fixed_text).
narrative_ontology:cs_authority_grounding('e1c89fb8-822b-4125-9bc3-ab26294036ef', lineage).
narrative_ontology:cs_interpretation_layer_present('e1c89fb8-822b-4125-9bc3-ab26294036ef').
narrative_ontology:cs_reading_relation('e1c89fb8-822b-4125-9bc3-ab26294036ef', ip_category_emergence__thinkability_reading, coexists_with).
narrative_ontology:cs_reading_relation('e1c89fb8-822b-4125-9bc3-ab26294036ef', ip_category_emergence__synchronic_diachronic_seam, influences).
narrative_ontology:cs_axiom('e1c89fb8-822b-4125-9bc3-ab26294036ef', foundational, author_as_first_holder).
narrative_ontology:cs_axiom_status(author_as_first_holder, holdable).
narrative_ontology:cs_axiom_grounding('e1c89fb8-822b-4125-9bc3-ab26294036ef', author_as_first_holder, conventional).
narrative_ontology:cs_axiom('e1c89fb8-822b-4125-9bc3-ab26294036ef', secondary, statute_vindicates_occupancy_logic).
narrative_ontology:cs_axiom_status(statute_vindicates_occupancy_logic, holdable).
narrative_ontology:cs_axiom_grounding('e1c89fb8-822b-4125-9bc3-ab26294036ef', statute_vindicates_occupancy_logic, conventional).
narrative_ontology:cs_reference_frame('e1c89fb8-822b-4125-9bc3-ab26294036ef', real_property_occupancy_norm).
narrative_ontology:cs_drift_state('e1c89fb8-822b-4125-9bc3-ab26294036ef', digital_reproduction_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('e1c89fb8-822b-4125-9bc3-ab26294036ef', '').
narrative_ontology:cs_kernel_id(ip_category_emergence__first_holding_reading, ip_category_emergence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ip_category_emergence__first_holding_reading, statutory_authors).
narrative_ontology:constraint_beneficiary(ip_category_emergence__first_holding_reading, rights_assignee_publishers).
narrative_ontology:constraint_victim(ip_category_emergence__first_holding_reading, unlicensed_reproducers).
narrative_ontology:constraint_victim(ip_category_emergence__first_holding_reading, access_seeking_public).
narrative_ontology:constraint_vindicates(ip_category_emergence__first_holding_reading, occupancy_analogy_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold nominal exclusive rights in their expressive works under the occupancy framework, entitled to enforce against unauthorized copying. Frequently assign these rights to publishing intermediaries in exchange for publication and limited royalties. Their position as first holders is doctrinally central but economically secondary, as enforcement is typically administered by assignees.
narrative_ontology:constraint_stakeholder(ip_category_emergence__first_holding_reading, statutory_authors, beneficiary,
    moderate, biographical, constrained, national).

% Administer and enforce the copyright regime through litigation, lobbying, and contractual standard forms. Collect the bulk of monopoly rents from the system, though the occupancy narrative nominally grounds rights in the author. Their enforcement infrastructure sustains the constraint and suppresses unauthorized alternatives.
narrative_ontology:constraint_stakeholder(ip_category_emergence__first_holding_reading, rights_assignee_publishers, agenda_setter,
    institutional, generational, arbitrage, global).

% Bear the direct costs of copyright enforcement: statutory damages, injunctions, and criminal penalties. They provide unauthorized access to works and are the primary targets of suppression under the occupancy framework.
narrative_ontology:constraint_stakeholder(ip_category_emergence__first_holding_reading, unlicensed_reproducers, payer,
    moderate, immediate, constrained, global).

% Pay monopoly-inflated prices for licensed works and face restricted access to orphan or out-of-print works. They fund the system through consumer surplus transfer but are not party to its design.
narrative_ontology:constraint_stakeholder(ip_category_emergence__first_holding_reading, access_seeking_public, payer,
    organized, biographical, constrained, national).

% Held a perpetual monopoly over English printing through royal charter before 1710. The Statute of Anne displaced their exclusive enforcement beneficiary status and opened the claimant set to authors, destroying their guild monopoly. Their claim to legitimacy was delegitimized by the statutory occupancy narrative.
narrative_ontology:constraint_stakeholder(ip_category_emergence__first_holding_reading, stationers_company, excluded,
    organized, generational, constrained, national).

% Judges and jurists who apply the occupancy analogy to intellectual products, treating the Statute of Anne as the livery of seisin for authors. They maintain the doctrinal architecture that frames copyright as first holding rather than statutory privilege.
narrative_ontology:constraint_stakeholder(ip_category_emergence__first_holding_reading, legal_occupancy_interpreters, observer,
    institutional, civilizational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a legally cognizable property-like right in expressive works, enabling market exchange and investment in publication by assigning clear entitlement to a defined claimant set.
% TRANSFER_FUNCTION: Moves exclusive reproduction and distribution rights from the public domain or guild monopoly into the hands of individual authors and their assigns, enforced against unauthorized printers and the access-seeking public.
% ABSENT_VOICES: Unlicensed copiers and the access-seeking public are structurally excluded from the claimant set; indigenous knowledge holders who view expression as communal are absent from the occupancy framing; the Stationers' Company's pre-1710 claim to perpetual monopoly is delegitimized and excluded from post-statutory discourse.
% DISAPPEARANCE_RATIONALE: If the first-holding/occupancy framework vanished, the legal basis for author-initiated copyright would collapse; publishing investment structures, licensing markets, and enforcement regimes built on assignable authorial rights would lack doctrinal foundation.
% FOUNDING_PROBLEM: The Stationers' Company monopoly was expiring and a statutory mechanism was needed to secure creative production while preventing perpetual guild control; authors were not previously recognized as holding a proprietary interest in their works against unauthorized copying.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians outside the beneficiary set (e.g., Patterson and Rose) attest that the Statute of Anne was designed to break the Stationers' monopoly rather than enshrine natural authorial rights; contemporary copyright scholars note the regime now operates in a vastly changed technological environment.
narrative_ontology:disappearance_verdict(ip_category_emergence__first_holding_reading, world_rearranges).
narrative_ontology:founding_problem_status(ip_category_emergence__first_holding_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ip_category_emergence__first_holding_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(ip_category_emergence__first_holding_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ip_category_emergence__first_holding_reading, 0.68, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ip_category_emergence__first_holding_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ip_category_emergence__first_holding_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ip_category_emergence__first_holding_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) is high because monopoly pricing, term extensions, and subject-matter expansion exceed what is necessary to coordinate creative production. Suppression (0.75) is higher still because the constraint's persistence depends on active enforcement against unauthorized reproduction, statutory damages, and international treaty pressure. Theater ratio (0.40) reflects moderate performative maintenance: the 'starving author' and 'original genius' narratives persist and are invoked in legislative lobbying, but an increasing share of enforcement activity protects corporate back-catalogs rather than living authors. Accessibility collapse (0.60) captures the partial closure of alternatives: open access and fair use exist but are structurally disadvantaged by statutory defaults and technological protection measures. Resistance (0.55) reflects persistent piracy, open-access movements, and academic critiques. The temporal series show extraction and suppression intensifying over the interval as the digital reproduction era made natural scarcity impossible and enforcement machinery expanded.
 *
 * PERSPECTIVAL GAP:
 *   The author seat experiences the constraint as legitimate entitlement: they are the first holder of their expression and the occupancy framework vindicates their creative labor. The publisher seat experiences it as a coordination device that secures investment in dissemination, with extraction as necessary cost. The unlicensed reproducer seat experiences it as arbitrary suppression of information sharing. The public seat experiences it as a tax on access. The engine computes these divergent classifications from the same structural data: beneficiary declarations and exit options differ by seat, producing low directionality for rights-holders and high directionality for constrained targets.
 *
 * DIRECTIONALITY LOGIC:
 *   Statutory authors are declared beneficiaries with constrained exit (they can stop creating but face high costs to exploit works outside the rights framework), placing their derived directionality near the beneficiary pole. Rights-assignee publishers are agenda-setters and declared beneficiaries with arbitrage-grade exit (can shift jurisdictions and business models), placing them at the strongly subsidized end. Unlicensed reproducers and the access-seeking public are declared victims with constrained exit, placing them near the full-target pole. The Stationers' Company is excluded rather than victimized in the current structure: their displacement is historical and the current constraint does not extract from them directly.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â breaking the Stationers' monopoly and securing creative production â is dead. The Stationers' guild no longer exists and the problem of unlicensed copying has been transformed by digital technology. Yet the constraint persists and has expanded (term extensions, subject-matter creep, statutory damages escalation). The Tangled Rope classification prevents mislabeling this as pure extraction (Snare) because genuine coordination remains: the rights framework still enables market exchange in expression. It also prevents mislabeling it as pure coordination (Rope) because the metrics reveal substantial and intensifying extraction. The dead founding problem plus rising extraction is the signature of Mandatrophy in the Tangled Rope regime: the coordination rationale has atrophied into a rent-preservation mechanism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    occupancy_vs_constructed_category,
    'Is the occupancy analogy a natural extension of physical property logic to mental labor, or is it a post-hoc rationalization for a statutory invention that creates enclosure?',
    'Comparative doctrinal analysis of non-occupancy IP regimes and historical examination of whether pre-1710 common law recognized authorial rights independent of the Stationers'' monopoly.',
    'If occupancy is constructed, the constraint''s legitimacy derives from legislative choice rather than natural right, making its expansion contestable as policy rather than as recognition of inherent entitlement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(occupancy_vs_constructed_category, conceptual, 'Whether the occupancy framing is natural-law or constructed').

omega_variable(
    author_beneficiary_vs_publisher_capture,
    'Do the economic gains of the author-as-holder framework actually accrue to statutory authors, or do they flow predominantly to publishing intermediaries through standard-form assignment?',
    'Empirical analysis of royalty splits, assignment rates, and enforcement-initiation patterns in contemporary copyright litigation.',
    'If gains are captured by assignee-publishers, the beneficiary/victim structure is misaligned with the occupancy narrative: the nominal beneficiary (author) is structurally displaced by the agenda-setter (publisher), raising the derived extraction for the author seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(author_beneficiary_vs_publisher_capture, empirical, 'Whether authors or publishers capture the economic surplus').

omega_variable(
    first_holding_kernel_contest,
    'Is this constraint a genuine occupancy shift in 1710, or a category emergence framed as first holding?',
    'Comparative analysis of the sibling readings (thinkability_reading, synchronic_diachronic_seam) and their empirical predictions about legal doctrinal development.',
    'If the thinkability reading is correct, the constraint''s coordination function is primary and extraction is incidental to category coherence; if the synchronic seam holds, the distinction between occupancy and category is artifactual and the constraint''s historical specificity dissolves.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(first_holding_kernel_contest, conceptual, 'Committing omega for kernel ip_category_emergence reading contest').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ip_category_emergence__first_holding_reading, 0, 314).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ip_c_tr_t0, ip_category_emergence__first_holding_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(ip_c_tr_t60, ip_category_emergence__first_holding_reading, theater_ratio, 60, 0.25).
narrative_ontology:measurement(ip_c_tr_t120, ip_category_emergence__first_holding_reading, theater_ratio, 120, 0.3).
narrative_ontology:measurement(ip_c_tr_t180, ip_category_emergence__first_holding_reading, theater_ratio, 180, 0.35).
narrative_ontology:measurement(ip_c_tr_t240, ip_category_emergence__first_holding_reading, theater_ratio, 240, 0.38).
narrative_ontology:measurement(ip_c_tr_t300, ip_category_emergence__first_holding_reading, theater_ratio, 300, 0.4).

% Extraction over time
narrative_ontology:measurement(ip_c_be_t0, ip_category_emergence__first_holding_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(ip_c_be_t60, ip_category_emergence__first_holding_reading, base_extractiveness, 60, 0.5).
narrative_ontology:measurement(ip_c_be_t120, ip_category_emergence__first_holding_reading, base_extractiveness, 120, 0.55).
narrative_ontology:measurement(ip_c_be_t180, ip_category_emergence__first_holding_reading, base_extractiveness, 180, 0.6).
narrative_ontology:measurement(ip_c_be_t240, ip_category_emergence__first_holding_reading, base_extractiveness, 240, 0.65).
narrative_ontology:measurement(ip_c_be_t300, ip_category_emergence__first_holding_reading, base_extractiveness, 300, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(ip_c_su_t0, ip_category_emergence__first_holding_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(ip_c_su_t60, ip_category_emergence__first_holding_reading, suppression_requirement, 60, 0.6).
narrative_ontology:measurement(ip_c_su_t120, ip_category_emergence__first_holding_reading, suppression_requirement, 120, 0.65).
narrative_ontology:measurement(ip_c_su_t180, ip_category_emergence__first_holding_reading, suppression_requirement, 180, 0.7).
narrative_ontology:measurement(ip_c_su_t240, ip_category_emergence__first_holding_reading, suppression_requirement, 240, 0.73).
narrative_ontology:measurement(ip_c_su_t300, ip_category_emergence__first_holding_reading, suppression_requirement, 300, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(ip_category_emergence__first_holding_reading, thinkability_reading).
narrative_ontology:affects_constraint(ip_category_emergence__first_holding_reading, synchronic_diachronic_seam).

% DUAL FORMULATION NOTE:
% The natural-language concept 'ip_category_emergence' decomposes into three structurally distinct readings: first_holding_reading (occupancy shift), thinkability_reading (category coherence), and synchronic_diachronic_seam (temporal artifact). Each reading has a different epsilon, beneficiary structure, and historical ontology. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
