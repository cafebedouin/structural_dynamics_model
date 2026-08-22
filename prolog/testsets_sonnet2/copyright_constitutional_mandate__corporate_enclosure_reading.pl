% ============================================================================
% CONSTRAINT STORY: copyright_constitutional_mandate__corporate_enclosure_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_copyright_constitutional_mandate__corporate_enclosure_reading, []).

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
 *   constraint_id: copyright_constitutional_mandate__corporate_enclosure_reading
 *   human_readable: Copyright as Perpetual-Adjacent Property Right (Corporate Enclosure Reading)
 *   domain: intellectual_property_law/constitutional_law/political_economy
 *
 * SUMMARY:
 *   This story instantiates the corporate-enclosure reading of the
 *   copyright-constitutional-mandate kernel: the view that copyright is
 *   fundamentally a property right, that 'limited times' in the
 *   Constitution's IP Clause should be read as permitting maximal extension
 *   short of explicit perpetuity, and that repeated retroactive term
 *   extensions (1976, 1998) plus criminalized circumvention (DMCA) and
 *   narrowed fair use are the correct, legally faithful implementation of
 *   that property-maximalist premise. This reading treats the standing
 *   arrangement — successive extensions ratified by Congress and left largely
 *   unreviewed by the Supreme Court under rational-basis deference — as the
 *   object of assessment, not as a departure from some purer original design.
 *   Two sibling readings of the same kernel exist as separate constraint
 *   stories: the public_scaffold_reading, which reads the same clause as
 *   mandating a temporary monopoly whose entire justification is
 *   public-domain enrichment, and the judicial_ambiguity_reading, which reads
 *   the term question as pure legislative discretion policed only lightly by
 *   courts. Each sibling authors its own ε from its own premises; this file's
 *   high ε (0.81) reflects the enclosure reading's own assessment that the
 *   standing arrangement extracts substantially from downstream creators, not
 *   an average across readings.
 *
 * KEY AGENTS:
 *   - legacy_entertainment_conglomerates: primary beneficiary and chief agenda-setter via lobbying (institutional/arbitrage)
 *   - derivative_creators, educators, archivists, independent_remix_artists: bear the cost of extended terms and narrowed fair use (moderate-to-powerless/trapped-constrained)
 *   - congress: enforcement conduit that ratifies extension into statute (institutional/analytical)
 *   - supreme_court: analytical observer that has declined to police the 'limited times' boundary (institutional/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(copyright_constitutional_mandate__corporate_enclosure_reading, 0.81).
domain_priors:suppression_score(copyright_constitutional_mandate__corporate_enclosure_reading, 0.72).
domain_priors:theater_ratio(copyright_constitutional_mandate__corporate_enclosure_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(copyright_constitutional_mandate__corporate_enclosure_reading, extractiveness, 0.81).
narrative_ontology:constraint_metric(copyright_constitutional_mandate__corporate_enclosure_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(copyright_constitutional_mandate__corporate_enclosure_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(copyright_constitutional_mandate__corporate_enclosure_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(copyright_constitutional_mandate__corporate_enclosure_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(copyright_constitutional_mandate__corporate_enclosure_reading, tangled_rope).
narrative_ontology:human_readable(copyright_constitutional_mandate__corporate_enclosure_reading, "Copyright as Perpetual-Adjacent Property Right (Corporate Enclosure Reading)").
narrative_ontology:topic_domain(copyright_constitutional_mandate__corporate_enclosure_reading, "intellectual_property_law/constitutional_law/political_economy").

domain_priors:requires_active_enforcement(copyright_constitutional_mandate__corporate_enclosure_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(copyright_constitutional_mandate__corporate_enclosure_reading, '36734823-4050-441c-a5d9-a634e08e24e1').
narrative_ontology:cs_kernel_codification('36734823-4050-441c-a5d9-a634e08e24e1', fixed_text).
narrative_ontology:cs_authority_grounding('36734823-4050-441c-a5d9-a634e08e24e1', extraction).
narrative_ontology:cs_interpretation_layer_present('36734823-4050-441c-a5d9-a634e08e24e1').
narrative_ontology:cs_reading_relation('36734823-4050-441c-a5d9-a634e08e24e1', copyright_constitutional_mandate__public_scaffold_reading, coexists_with).
narrative_ontology:cs_reading_relation('36734823-4050-441c-a5d9-a634e08e24e1', copyright_constitutional_mandate__judicial_ambiguity_reading, influences).
narrative_ontology:cs_axiom('36734823-4050-441c-a5d9-a634e08e24e1', foundational, copyright_is_property_deserving_maximal_protection).
narrative_ontology:cs_axiom_status(copyright_is_property_deserving_maximal_protection, holdable).
narrative_ontology:cs_axiom_grounding('36734823-4050-441c-a5d9-a634e08e24e1', copyright_is_property_deserving_maximal_protection, conventional).
narrative_ontology:cs_axiom('36734823-4050-441c-a5d9-a634e08e24e1', foundational, limited_times_permits_extension_short_of_explicit_perpetuity).
narrative_ontology:cs_axiom_status(limited_times_permits_extension_short_of_explicit_perpetuity, holdable).
narrative_ontology:cs_axiom_grounding('36734823-4050-441c-a5d9-a634e08e24e1', limited_times_permits_extension_short_of_explicit_perpetuity, conventional).
narrative_ontology:cs_reference_frame('36734823-4050-441c-a5d9-a634e08e24e1', property_maximalist_founding_construction).
narrative_ontology:cs_drift_state('36734823-4050-441c-a5d9-a634e08e24e1', post_dmca_post_ctea_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('36734823-4050-441c-a5d9-a634e08e24e1', '').
narrative_ontology:cs_kernel_id(copyright_constitutional_mandate__corporate_enclosure_reading, copyright_constitutional_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(copyright_constitutional_mandate__corporate_enclosure_reading, legacy_entertainment_conglomerates).
narrative_ontology:constraint_beneficiary(copyright_constitutional_mandate__corporate_enclosure_reading, music_licensing_organizations).
narrative_ontology:constraint_beneficiary(copyright_constitutional_mandate__corporate_enclosure_reading, major_film_studios).
narrative_ontology:constraint_victim(copyright_constitutional_mandate__corporate_enclosure_reading, derivative_creators).
narrative_ontology:constraint_victim(copyright_constitutional_mandate__corporate_enclosure_reading, educators).
narrative_ontology:constraint_victim(copyright_constitutional_mandate__corporate_enclosure_reading, archivists).
narrative_ontology:constraint_victim(copyright_constitutional_mandate__corporate_enclosure_reading, independent_remix_artists).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Lobbies for successive term extensions and criminalized circumvention statutes, framing copyright as an ordinary property right deserving protection as close to perpetual as constitutional text permits. Holds enormous back-catalog value that would enter the public domain absent extension; funds the legislative and litigation apparatus that keeps extending the term and narrowing fair use.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__corporate_enclosure_reading, legacy_entertainment_conglomerates, agenda_setter,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(copyright_constitutional_mandate__corporate_enclosure_reading, legacy_entertainment_conglomerates, beneficiary).

% Collects licensing revenue on a catalog whose commercial life is extended indefinitely by term extension; supports criminalized circumvention because it protects the technical locks that make licensing enforceable at scale.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__corporate_enclosure_reading, music_licensing_organizations, beneficiary,
    organized, generational, mobile, global).

% A handful of iconic properties generate outsized ongoing revenue; the studios treat any prospective public-domain entry of a flagship character as an existential threat and have historically timed legislative campaigns to precede scheduled expirations.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__corporate_enclosure_reading, major_film_studios, beneficiary,
    institutional, civilizational, arbitrage, global).

% Wants to build new works — adaptations, sequels, transformative art — on characters and works that would already be in the public domain under the original constitutional term. Faces licensing costs, cease-and-desist threats, or outright refusal, and litigation risk under an expanded, ambiguous fair-use doctrine that favors incumbents with larger legal budgets.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__corporate_enclosure_reading, derivative_creators, payer,
    moderate, biographical, constrained, global).

% Needs to reproduce, excerpt, or adapt copyrighted material for classroom use; navigates a shrinking, unpredictable fair-use safe harbor and criminalized circumvention rules that block even non-infringing access to digitally locked works for legitimate pedagogical purposes.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__corporate_enclosure_reading, educators, payer,
    powerless, biographical, trapped, national).

% Preserves orphan works and culturally significant but commercially abandoned media; cannot digitize or circulate much of this material because rights holders cannot be located or refuse permission, and the extended term keeps works locked up long after their commercial life has ended.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__corporate_enclosure_reading, archivists, payer,
    powerless, generational, trapped, national).

% Produces sampling-based, remix, or fan-derivative work that is legally viable in a robust fair-use regime but economically foreclosed under aggressive enforcement backed by criminalized circumvention and expansive statutory damages, since a single infringement claim can be ruinous even if ultimately defensible.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__corporate_enclosure_reading, independent_remix_artists, payer,
    powerless, biographical, constrained, global).

% Enacts term extensions (e.g., the 1998 extension) under sustained lobbying pressure, applying rational-basis-level scrutiny to its own discretion; from this reading's vantage, Congress functions as the enforcement conduit that ratifies the property-maximalist framing into statute.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__corporate_enclosure_reading, congress, agenda_setter,
    institutional, generational, analytical, national).

% Adjudicates constitutional challenges to term extension (e.g., Eldred v. Ashcroft) and has historically deferred to legislative judgment about what counts as 'limited,' declining to police the outer boundary of the clause even as terms approach de facto perpetuity for corporate-held works.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__corporate_enclosure_reading, supreme_court, observer,
    institutional, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(copyright_constitutional_mandate__corporate_enclosure_reading, legacy_entertainment_conglomerates).
narrative_ontology:fixing_cost_class(copyright_constitutional_mandate__corporate_enclosure_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Nominally solves the problem of incentivizing creative production by guaranteeing creators (and their assignees) an exclusive window to recoup investment before a work enters the public domain.
% TRANSFER_FUNCTION: Moves control over cultural material — and the licensing revenue and derivative-work gatekeeping that control confers — from the public domain (and from downstream creators, educators, and archivists who would otherwise draw on it freely) to the corporate rights-holders who have accumulated and consolidated copyright portfolios.
% ABSENT_VOICES: The general public and future creators who will never exist as an organized lobby are structurally absent from the legislative process that sets term length; orphan-work rights holders who cannot be located to consent or object are also absent by definition.
% DISAPPEARANCE_RATIONALE: If this reading's maximal-protection framing collapsed and terms reverted toward the original constitutional design, a large body of 20th-century culture would enter the public domain, licensing revenue on legacy catalogs would collapse, derivative and remix markets would open substantially, and archival digitization projects currently blocked by orphan-works uncertainty could proceed.
% FOUNDING_PROBLEM: The constitutional grant was built to solve underproduction of creative and useful works by giving authors a time-limited monopoly sufficient to recoup investment, after which the work would enrich the public domain.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars outside the entertainment industry (e.g., testimony and amicus briefs in Eldred v. Ashcroft from library associations, law professors, and public-domain advocates) attest that the original incentive problem was solved long ago for existing works and that repeated retroactive extension serves rent preservation rather than incentive; the corporate beneficiaries themselves are the primary source asserting the founding problem remains live.
narrative_ontology:disappearance_verdict(copyright_constitutional_mandate__corporate_enclosure_reading, world_rearranges).
narrative_ontology:founding_problem_status(copyright_constitutional_mandate__corporate_enclosure_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(copyright_constitutional_mandate__corporate_enclosure_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(copyright_constitutional_mandate__corporate_enclosure_reading, 'none', 1).
narrative_ontology:epsilon_provenance(copyright_constitutional_mandate__corporate_enclosure_reading, 0.81, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(copyright_constitutional_mandate__corporate_enclosure_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(copyright_constitutional_mandate__corporate_enclosure_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(copyright_constitutional_mandate__corporate_enclosure_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.81) because this reading holds that each term extension transfers value from an already-diffuse public domain to a concentrated set of corporate rights-holders without a corresponding new incentive being created for works already produced — the core objection to retroactive extension. Suppression (0.72) reflects the criminalization of circumvention (DMCA anti-circumvention provisions) which blocks even legally non-infringing access, not merely infringing use — an unusually strong suppressive mechanism layered onto what began as a narrower exclusive-rights regime. Theater ratio (0.58) is elevated because a substantial share of enforcement activity (takedown regimes, statutory-damages threats against clearly transformative work) functions to deter legally viable activity through cost-of-defense rather than to vindicate a genuine infringement claim. Accessibility collapse (0.62) and resistance (0.55) reflect that alternatives (public domain access, robust fair use) have not fully collapsed — active resistance persists in academic, library, and open-culture communities — but have been substantially narrowed by successive statutory changes.
 *
 * DIRECTIONALITY LOGIC:
 *   Legacy conglomerates, music licensing bodies, and major studios sit near the full-beneficiary end: they set the legislative agenda, hold the accumulated catalogs whose value the extension protects, and possess arbitrage-grade exit (they can relocate IP strategy across jurisdictions and hold portfolios diversified enough to absorb any single work's eventual expiration). Derivative creators, educators, archivists, and independent remix artists sit near the full-target end: they are structurally trapped or constrained (a teacher cannot exit the fair-use doctrine; an archivist cannot relicense an orphan work), and the extension's costs land disproportionately on them as a class with no comparable lobbying leverage. Congress functions as agenda-setter/enforcement-conduit rather than a beneficiary in its own right — it does not collect rents but ratifies the arrangement into binding law, which is why it is authored with analytical exit rather than beneficiary status.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (incentivizing new creative production) is largely dead for already-existing works — a 1928 Mickey Mouse cartoon needs no further incentive to have been created — yet the enclosure reading treats the arrangement's mandate as permanently live by re-describing incentive-for-future-works as protection-of-existing-value. Classifying this as tangled_rope rather than pure snare preserves the genuine coordination function copyright performs for NEW works (the incentive story is real prospectively) while flagging that the same statutory machinery is used to extract from a stock of already-created works retroactively — the asymmetry the tangled_rope category exists to name. A pure snare label would erase the coordination function copyright genuinely serves for contemporaneous authors; a pure rope label would erase the retroactive-extension extraction this reading identifies as its central complaint.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_disagreement_locus,
    'Where exactly do the three readings of the copyright_constitutional_mandate kernel diverge — is it the meaning of ''limited,'' the purpose clause''s binding force, or the standard of judicial review?',
    'Textual and historical analysis of the IP Clause''s drafting history, cross-referenced against the doctrinal holdings in Eldred v. Ashcroft and Golan v. Holder; comparison of how each reading treats the clause''s stated purpose (''to promote the Progress of Science'') as either binding constraint or mere preamble.',
    'If the purpose clause is read as binding (public_scaffold_reading''s premise), retroactive extension for already-created works is constitutionally suspect regardless of term length; if read as non-binding preamble (this reading''s premise), Congress''s discretion over term length is essentially unbounded short of explicit perpetuity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_disagreement_locus, conceptual, 'Locates the exact textual/doctrinal fork between the three sibling readings of the same constitutional clause.').

omega_variable(
    retroactive_vs_prospective_incentive,
    'Does extending the term for already-created works serve any genuine incentive function, or does incentive theory only justify prospective term length for works not yet created?',
    'Economic analysis of whether authors'' creative decisions were in fact influenced by the possibility of a future retroactive extension (a temporally impossible causal chain for pre-existing extension events), versus survey/behavioral evidence on whether current authors factor in the possibility of future extensions.',
    'If retroactive extension cannot serve incentive function by construction (the work already exists), the enclosure reading''s property-maximalist framing has no incentive-based justification for the retroactive component specifically — strengthening the tangled_rope classification''s victim/extraction finding.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(retroactive_vs_prospective_incentive, empirical, 'Whether retroactive term extension can coherently serve the constitutional incentive rationale at all.').

omega_variable(
    corporate_capture_vs_genuine_property_theory,
    'Is the maximal-protection reading a genuine, independently-derived constitutional theory, or is it a post-hoc doctrinal rationalization constructed to serve the lobbying interests of a small number of major rights-holders?',
    'Trace the historical sequence: did the property-maximalist legal theory predate organized entertainment-industry lobbying for term extension, or did the theory''s prominence in legal scholarship and advocacy rise concurrently with and largely funded by the same industries that benefit from it?',
    'If the theory is substantially industry-constructed, the ''coordination'' half of the tangled_rope classification is weaker than authored here and the constraint drifts toward pure snare; if the theory has independent normative grounding predating industry advocacy, the coordination function is more genuine than the extraction framing alone suggests.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(corporate_capture_vs_genuine_property_theory, conceptual, 'Whether the corporate-enclosure reading is an independent constitutional theory or a lobbying-constructed rationalization.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(copyright_constitutional_mandate__corporate_enclosure_reading, 1976, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(copy_tr_t1976, copyright_constitutional_mandate__corporate_enclosure_reading, theater_ratio, 1976, 0.3).
narrative_ontology:measurement(copy_tr_t1988, copyright_constitutional_mandate__corporate_enclosure_reading, theater_ratio, 1988, 0.35).
narrative_ontology:measurement(copy_tr_t1998, copyright_constitutional_mandate__corporate_enclosure_reading, theater_ratio, 1998, 0.44).
narrative_ontology:measurement(copy_tr_t2008, copyright_constitutional_mandate__corporate_enclosure_reading, theater_ratio, 2008, 0.5).
narrative_ontology:measurement(copy_tr_t2016, copyright_constitutional_mandate__corporate_enclosure_reading, theater_ratio, 2016, 0.55).
narrative_ontology:measurement(copy_tr_t2024, copyright_constitutional_mandate__corporate_enclosure_reading, theater_ratio, 2024, 0.58).

% Extraction over time
narrative_ontology:measurement(copy_be_t1976, copyright_constitutional_mandate__corporate_enclosure_reading, base_extractiveness, 1976, 0.42).
narrative_ontology:measurement(copy_be_t1988, copyright_constitutional_mandate__corporate_enclosure_reading, base_extractiveness, 1988, 0.51).
narrative_ontology:measurement(copy_be_t1998, copyright_constitutional_mandate__corporate_enclosure_reading, base_extractiveness, 1998, 0.68).
narrative_ontology:measurement(copy_be_t2008, copyright_constitutional_mandate__corporate_enclosure_reading, base_extractiveness, 2008, 0.74).
narrative_ontology:measurement(copy_be_t2016, copyright_constitutional_mandate__corporate_enclosure_reading, base_extractiveness, 2016, 0.78).
narrative_ontology:measurement(copy_be_t2024, copyright_constitutional_mandate__corporate_enclosure_reading, base_extractiveness, 2024, 0.81).

% Suppression requirement over time
narrative_ontology:measurement(copy_su_t1976, copyright_constitutional_mandate__corporate_enclosure_reading, suppression_requirement, 1976, 0.4).
narrative_ontology:measurement(copy_su_t1988, copyright_constitutional_mandate__corporate_enclosure_reading, suppression_requirement, 1988, 0.48).
narrative_ontology:measurement(copy_su_t1998, copyright_constitutional_mandate__corporate_enclosure_reading, suppression_requirement, 1998, 0.62).
narrative_ontology:measurement(copy_su_t2008, copyright_constitutional_mandate__corporate_enclosure_reading, suppression_requirement, 2008, 0.68).
narrative_ontology:measurement(copy_su_t2016, copyright_constitutional_mandate__corporate_enclosure_reading, suppression_requirement, 2016, 0.7).
narrative_ontology:measurement(copy_su_t2024, copyright_constitutional_mandate__corporate_enclosure_reading, suppression_requirement, 2024, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(copyright_constitutional_mandate__corporate_enclosure_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(copyright_constitutional_mandate__corporate_enclosure_reading, copyright_constitutional_mandate__public_scaffold_reading).
narrative_ontology:affects_constraint(copyright_constitutional_mandate__corporate_enclosure_reading, copyright_constitutional_mandate__judicial_ambiguity_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the copyright_constitutional_mandate kernel. corporate_enclosure_reading (this file) authors high ε (0.81) and classifies as tangled_rope. public_scaffold_reading authors low-to-moderate ε reflecting a genuine, time-bounded coordination mechanism whose extension events are read as drift from the design rather than as its correct operation. judicial_ambiguity_reading authors a more procedural, deference-focused ε centered on the standard of review rather than on the substantive extraction question. All three share the same underlying constitutional text and legislative history but instantiate structurally distinct constraints per the ε-invariance principle — they are not the same constraint measured three ways.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
