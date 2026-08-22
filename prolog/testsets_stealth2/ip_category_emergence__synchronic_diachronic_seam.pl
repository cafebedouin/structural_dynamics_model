% ============================================================================
% CONSTRAINT STORY: ip_category_emergence__synchronic_diachronic_seam
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ip_category_emergence__synchronic_diachronic_seam, []).

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
 *   constraint_id: ip_category_emergence__synchronic_diachronic_seam
 *   human_readable: Synchronic/Diachronic Seam Test of the IP-Emergence Periodization
 *   domain: legal philosophy/intellectual property/historical jurisprudence
 *
 * SUMMARY:
 *   Within the ip_category_emergence kernel — the contested claim that 1710
 *   marks a structural transition in the legal treatment of expressive works
 *   — this story instantiates the synchronic_diachronic_seam reading: the
 *   assertion that the two candidate contents of that transition (ownable
 *   expression becoming legally thinkable; the author entering the legitimate
 *   claimant set) are either formally independent dimensions whose co-dating
 *   is contingent, or a temporal framing artifact that dissolves under the
 *   M4/M5 collapse test. The standing arrangement under contest, assessed by
 *   this reading's own lights, is the historiographical convention that fuses
 *   both transitions into one dated event — the 'birth of intellectual
 *   property' periodization. That convention genuinely coordinates
 *   legal-historical discourse (a shared anchor for citation, pedagogy, and
 *   cumulative commentary) while imposing a precision tax on accounts that
 *   decompose the event and supplying an exploitable equivocation to
 *   doctrinal advocates who load the same date with whichever content a given
 *   argument requires. Epsilon's referent is this fused periodization
 *   convention as it stands, never any endorsed decomposed alternative. The
 *   sibling readings are separate constraints with their own epsilon values
 *   and victim sets: ip_category_emergence__thinkability_reading (category
 *   emergence) and ip_category_emergence__first_holding_reading (occupancy
 *   change). The claim/metrics split is deliberate: the type is claimed from
 *   the presence of both a real coordination function and asymmetric,
 *   actively enforced extraction, while the metric values are authored as
 *   independent descriptive judgments.
 *
 * KEY AGENTS:
 *   - periodization_canon_authors: Primary beneficiary (institutional/constrained) — accumulates citation and curricular capital inside the single-event frame
 *   - founding_date_doctrinal_advocates: Secondary beneficiary (powerful/arbitrage) — captures the equivocal founding moment as a movable argumentative asset
 *   - law_review_gatekeepers: Agenda setter (institutional/arbitrage) — administers the review, canon, and curriculum machinery through which the frame reproduces itself
 *   - revisionist_historiographers: Primary target (organized/constrained) — bears the precision tax; their decomposed accounts resist canon absorption
 *   - genealogy_dependent_reformers: Secondary target (moderate/constrained) — bears costs when opponents cite the fused date as settled tradition
 *   - comparative_law_historians: Excluded voice (organized/trapped) — holds a rival periodization outside the conversation's boundaries
 *   - analytical_jurisprudent: Analytical observer — sees both dimensions and their possible independence; collects and pays nothing
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ip_category_emergence__synchronic_diachronic_seam, 0.55).
domain_priors:suppression_score(ip_category_emergence__synchronic_diachronic_seam, 0.48).
domain_priors:theater_ratio(ip_category_emergence__synchronic_diachronic_seam, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ip_category_emergence__synchronic_diachronic_seam, extractiveness, 0.55).
narrative_ontology:constraint_metric(ip_category_emergence__synchronic_diachronic_seam, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(ip_category_emergence__synchronic_diachronic_seam, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ip_category_emergence__synchronic_diachronic_seam, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(ip_category_emergence__synchronic_diachronic_seam, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ip_category_emergence__synchronic_diachronic_seam, tangled_rope).
narrative_ontology:human_readable(ip_category_emergence__synchronic_diachronic_seam, "Synchronic/Diachronic Seam Test of the IP-Emergence Periodization").
narrative_ontology:topic_domain(ip_category_emergence__synchronic_diachronic_seam, "legal philosophy/intellectual property/historical jurisprudence").

domain_priors:requires_active_enforcement(ip_category_emergence__synchronic_diachronic_seam).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ip_category_emergence__synchronic_diachronic_seam, 'fef808b0-2494-4f8c-b4a8-21167f188683').
narrative_ontology:cs_kernel_codification('fef808b0-2494-4f8c-b4a8-21167f188683', fixed_text).
narrative_ontology:cs_authority_grounding('fef808b0-2494-4f8c-b4a8-21167f188683', practice).
narrative_ontology:cs_interpretation_layer_present('fef808b0-2494-4f8c-b4a8-21167f188683').
narrative_ontology:cs_reading_relation('fef808b0-2494-4f8c-b4a8-21167f188683', ip_category_emergence__thinkability_reading, influences).
narrative_ontology:cs_reading_relation('fef808b0-2494-4f8c-b4a8-21167f188683', ip_category_emergence__first_holding_reading, influences).
narrative_ontology:cs_axiom('fef808b0-2494-4f8c-b4a8-21167f188683', foundational, dimensional_independence_of_emergence_claims).
narrative_ontology:cs_axiom_status(dimensional_independence_of_emergence_claims, holdable).
narrative_ontology:cs_axiom_grounding('fef808b0-2494-4f8c-b4a8-21167f188683', dimensional_independence_of_emergence_claims, empirically_contingent).
narrative_ontology:cs_axiom('fef808b0-2494-4f8c-b4a8-21167f188683', secondary, co_occurrence_requires_independent_justification).
narrative_ontology:cs_axiom_status(co_occurrence_requires_independent_justification, holdable).
narrative_ontology:cs_axiom_grounding('fef808b0-2494-4f8c-b4a8-21167f188683', co_occurrence_requires_independent_justification, conventional).
narrative_ontology:cs_reference_frame('fef808b0-2494-4f8c-b4a8-21167f188683', composite_dated_transition).
narrative_ontology:cs_drift_state('fef808b0-2494-4f8c-b4a8-21167f188683', contemporary_revisionist_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('fef808b0-2494-4f8c-b4a8-21167f188683', '').
narrative_ontology:cs_kernel_id(ip_category_emergence__synchronic_diachronic_seam, ip_category_emergence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ip_category_emergence__synchronic_diachronic_seam, periodization_canon_authors).
narrative_ontology:constraint_beneficiary(ip_category_emergence__synchronic_diachronic_seam, founding_date_doctrinal_advocates).
narrative_ontology:constraint_victim(ip_category_emergence__synchronic_diachronic_seam, revisionist_historiographers).
narrative_ontology:constraint_victim(ip_category_emergence__synchronic_diachronic_seam, genealogy_dependent_reformers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Write the surveys, treatises, and casebooks that anchor Anglophone legal-historical teaching on the single 1710 event. Their frameworks, lecture notes, and accumulated commentary presuppose the fused date; rebuilding a lifework around a decomposed chronology would forfeit decades of citation capital. Leaving the frame means re-entering the field as a newcomer.
narrative_ontology:constraint_stakeholder(ip_category_emergence__synchronic_diachronic_seam, periodization_canon_authors, beneficiary,
    institutional, generational, constrained, global).

% Litigators, judges, and jurists who invoke the founding moment in rights-legitimacy and policy argument. Because the date carries two loadable contents, the same citation can stand for natural authorial entitlement in one brief and statutory constructivism in another. If the contents were pinned apart, every such argument would have to declare which premise it relies on.
narrative_ontology:constraint_stakeholder(ip_category_emergence__synchronic_diachronic_seam, founding_date_doctrinal_advocates, beneficiary,
    powerful, biographical, arbitrage, continental).

% Edit the journals, run the peer review, and staff the curriculum committees through which the single-event frame reproduces itself. They could commission a decomposed special issue at will, but the coordination cost of doing so lands on their desks while the benefit disperses across the profession.
narrative_ontology:constraint_stakeholder(ip_category_emergence__synchronic_diachronic_seam, law_review_gatekeepers, agenda_setter,
    institutional, biographical, arbitrage, national).

% Document pre-1710 privilege regimes, stationer registration practice, and civil-law parallels that cut the clean break into pieces. Their findings fit awkwardly into the single-event frame; articles come back asking them to soften the revision or fold it into proto-IP language. Exit means abandoning the archive specialty they spent careers mastering.
narrative_ontology:constraint_stakeholder(ip_category_emergence__synchronic_diachronic_seam, revisionist_historiographers, payer,
    organized, generational, constrained, global).

% Campaign for term limits, public-domain expansion, and remuneration reform on the ground that author-ownership was a contingent statutory choice. Opponents answer with the fused founding date treated as settled: authors have held since 1710, therefore the arrangement is traditional. Their arguments need exactly the precision the fused frame withholds.
narrative_ontology:constraint_stakeholder(ip_category_emergence__synchronic_diachronic_seam, genealogy_dependent_reformers, payer,
    moderate, generational, constrained, national).

% Work on droit d'auteur, revolutionary-era French legislation, and other national trajectories where the analogous transition dates elsewhere or never crystallizes into one event. Language barriers and canon boundaries keep them outside the Anglophone 1710 conversation; their objection — that the founding date is parochial — is rarely voiced inside it.
narrative_ontology:constraint_stakeholder(ip_category_emergence__synchronic_diachronic_seam, comparative_law_historians, excluded,
    organized, generational, trapped, global).

% Holds the seam-theorist seat: treats category emergence and occupancy change as separately statable questions and tracks whether any jurisdiction or period actually separates them. Neither collects from the fused frame nor pays its precision tax.
narrative_ontology:constraint_stakeholder(ip_category_emergence__synchronic_diachronic_seam, analytical_jurisprudent, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ip_category_emergence__synchronic_diachronic_seam, founding_date_doctrinal_advocates).
narrative_ontology:fixing_cost_class(ip_category_emergence__synchronic_diachronic_seam, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Supplies a single chronological anchor for legal-historical and doctrinal discourse: one date, one event, so that citation, teaching, and cumulative commentary can proceed without renegotiating periodization at every exchange.
% TRANSFER_FUNCTION: Moves epistemic authority and curricular time toward accounts compatible with the single-event frame and away from decomposed or differently dated accounts; moves a reusable rhetorical asset — an equivocal founding moment — to whichever doctrinal camp loads it in a given dispute.
% ABSENT_VOICES: Comparative civil-law historians (droit d'auteur traditions date the transition differently and sit structurally outside the Anglophone 1710 conversation); historians of pre-1710 privilege and stationer practice whose material complicates the clean break; non-Anglophone scholars generally. They would object that the founding date is parochial and composite; they are kept out by language and canon boundaries rather than engaged.
% DISAPPEARANCE_RATIONALE: If the fused single-event convention vanished overnight, textbooks, syllabi, and citation practice would have to re-anchor on decomposed questions (when did ownable expression become thinkable; when did author-claimant status arrive), doctrinal advocates would lose the shared equivocal date and would have to state precise premises, and the revisionist literature would move from margin to baseline.
% FOUNDING_PROBLEM: Early modern English legal practice needed to locate and legitimate the displacement of the Stationers' perpetual registration monopoly by statutory, term-limited, author-vested rights; a single datable legislative event made that transition teachable, citable, and administrable.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: revisionist historiographers attest that the founding problem concerned stationer-practice displacement rather than category creation, and comparative historians attest that the 1710 date does not mark a universal transition, since civil-law systems locate analogous shifts elsewhere. No corroborating source outside the beneficiary set attests that the fused single-event framing itself was ever the problem anyone needed solved.
narrative_ontology:disappearance_verdict(ip_category_emergence__synchronic_diachronic_seam, world_rearranges).
narrative_ontology:founding_problem_status(ip_category_emergence__synchronic_diachronic_seam, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ip_category_emergence__synchronic_diachronic_seam, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(ip_category_emergence__synchronic_diachronic_seam, 'none', 1).
narrative_ontology:epsilon_provenance(ip_category_emergence__synchronic_diachronic_seam, 0.55, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ip_category_emergence__synchronic_diachronic_seam_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ip_category_emergence__synchronic_diachronic_seam, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ip_category_emergence__synchronic_diachronic_seam_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.55: the extraction is epistemic and reputational rather than material — careers shaped, alternative chronologies marginalized at the review stage, doctrinal arguments distorted — substantial but bounded. Suppression is authored at 0.48 as a raw structural property (gatekeeping intensity, not state coercion); per the framework's division of labor, only extractiveness is scaled by directionality and scope downstream, and this scalar is left unscaled by design. Theater_ratio at 0.25 reflects real anniversary and tercentenary performativity layered over a frame that still does daily coordinating work. Accessibility_collapse at 0.35 is deliberately low: alternatives remain fully articulable — this reading is one of them — so understanding the fused frame does not close off the decomposed option. Resistance at 0.5 reflects an active revisionist and comparative literature that contests the clean break. The claimed type is tangled_rope because both required structural facts are present: a genuine coordination function (shared chronological anchor) and asymmetric extraction through the same structure (precision tax on decomposers, exploitable equivocation for advocates), held in place by active enforcement (review, canon, curriculum). The measurement series run on one shared time grid — every tracked metric is authored at every examined time point {0,6,12,18,24,30} — so no end-state value is silently substituted into earlier rows. The suppression_requirement series traces a real enforcement-capacity dynamic: professionalization of legal history ratcheted gatekeeping upward through the middle of the interval, with recent easing as digital and interdisciplinary venues opened; it is included for that reason, not as a static restatement. Anniversary-cycle oscillation in theater_ratio (spikes around centenaries) is noted qualitatively; the six-point grid does not resolve a full cycle and no cyclical claim is made.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From inside the canon, the fused frame is inherited infrastructure: periodization_canon_authors experience low friction and steady returns on sunk citation capital. For founding_date_doctrinal_advocates the same structure is a resource — the arbitrage seat sits nearest the beneficiary end because they can exit any particular loading of the date at will. For revisionist_historiographers and genealogy_dependent_reformers the identical convention operates as a tax on precision: their constrained exit (abandoning archive specialties or re-grounding reform arguments) keeps them near the full-target end. law_review_gatekeepers occupy an administrative seat that could decompose the convention but bears concentrated coordination costs while benefits disperse. The engine derives these per-seat classifications from the structural data; the authored claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Both declared beneficiary groups derive low directionality: canon authors collect citation and curricular rents from the frame they inhabit; doctrinal advocates collect usable ambiguity, amplified toward the beneficiary end by their arbitrage-grade exit (they can abandon any given loading of the date without cost). Both declared victim groups derive high directionality: historiographers and reformers bear the precision tax with constrained exits — leaving means forfeiting specialized expertise or the historical grounding of their arguments. Comparative law historians are structurally excluded rather than coordinated; their exclusion is maintained by language and canon boundaries rather than by anything the frame does for them. The analytical observer seat carries no directional stake. No directionality overrides are authored: the beneficiary/victim declarations plus exit options already produce the correct relationships, so the derivation chain is left intact.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — making the displacement of the Stationers' registration monopoly by statutory author-vested rights teachable, citable, and administrable — is at best contested and plausibly dead as originally posed: the transition is now thoroughly documented, and what persists is the fused framing, not the documentation need. The R5 interview records this as founding_problem_status 'contested' against a 'world_rearranges' disappearance verdict, which flags the zombie-risk mismatch without forcing a verdict. Classification prevents mislabeling in both directions: a pure-coordination reading would hide the extraction borne by decomposers and reformers; a pure-extraction reading would erase the real coordination value that keeps the frame load-bearing for pedagogy and citation. The tangled_rope claim holds both facts simultaneously and routes the question of whether the extraction half is growing, stable, or decaying to the temporal series and the equivocation_load_bearing_status omega rather than settling it by fiat.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    independence_vs_temporal_artifact,
    'Do category emergence (thinkability) and occupancy change (first-holding) vary as formally independent dimensions, or does their apparent co-dating at 1710 dissolve into a temporal framing artifact once the M4/M5 collapse test is applied?',
    'Divergent-case search across jurisdictions and periods: regimes where ownable expression was legally coherent without author-claimant status (publisher and inventor privileges, guild registration) or where holding preceded thinkability; comparative civil-law timelines; counterfactual reading of the Statute of Anne''s drafting record to see whether either dimension could have moved without the other.',
    'Independence confirmed: the fused single-event periodization is a contingent coincidence and the convention''s equivocation becomes a visible, correctable authorial choice. Artifact confirmed: the seam cannot be drawn, the two sibling readings collapse into one another, and the kernel family restructures around a single undifferentiated transition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(independence_vs_temporal_artifact, empirical, 'Which disjunct of the seam reading holds: formal independence or temporal framing artifact.').

omega_variable(
    kernel_sibling_resolution_pressure,
    'How does this seam-test reading''s classification shift if either sibling reading (category emergence or occupancy change) prevails outright in the ip_category_emergence kernel contest?',
    'Resolution of the sibling constraint stories ip_category_emergence__thinkability_reading and ip_category_emergence__first_holding_reading; cross-reading comparison of their epsilon values, victim sets, and computed seat classifications.',
    'If the thinkability reading prevails, this reading''s independence disjunct narrows to testing occupancy-side divergence only; if the first-holding reading prevails, symmetrically; if both persist as live coexisting readings, this reading''s load-bearing role increases, because the equivocation it documents is precisely what keeps both siblings sustainable on one shared date.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_sibling_resolution_pressure, conceptual, 'Committer-frame dependency of this reading''s structure on sibling reading outcomes within the shared kernel.').

omega_variable(
    gatekeeping_structural_vs_internalized,
    'Is the marginalization of decomposed periodizations enforced by structural gatekeeping (peer review, citation canon, curriculum control) or internalized as scholarly self-discipline (researchers preemptively avoid framings that will not fund or publish)?',
    'Post-canonical-shift trajectory: if decomposed accounts proliferate once editorial gatekeeping eases (digital venues, interdisciplinary journals), the suppression was structural; if researchers continue avoiding the frame despite open venues, it is internalized.',
    'Internalized suppression raises effective suppression above the structural measure and slows any decomposition fix regardless of editorial policy change; purely structural suppression predicts rapid relaxation if gatekeepers loosen review norms.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gatekeeping_structural_vs_internalized, empirical, 'Structural versus internalized mechanism behind the precision tax on decomposed historiography.').

omega_variable(
    equivocation_load_bearing_status,
    'Is the single-date equivocation (one event, two loadable contents) still load-bearing in live doctrinal dispute, or has it become vestigial habit?',
    'Citation analysis of 1710 invocations across natural-right and constructivist briefs, judgments, and treatises across the interval; count of arguments that fail if the date''s content were pinned to one dimension.',
    'Load-bearing: the prohibitive cost of fixing stands and the hybrid coordination-plus-extraction reading strengthens. Vestigial: the convention decays toward anniversary performance and fixing becomes cheap.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(equivocation_load_bearing_status, empirical, 'Whether the fused date''s ambiguity is functionally exploited or merely inherited.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ip_category_emergence__synchronic_diachronic_seam, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ip_c_tr_t0, ip_category_emergence__synchronic_diachronic_seam, theater_ratio, 0, 0.15).
narrative_ontology:measurement(ip_c_tr_t6, ip_category_emergence__synchronic_diachronic_seam, theater_ratio, 6, 0.17).
narrative_ontology:measurement(ip_c_tr_t12, ip_category_emergence__synchronic_diachronic_seam, theater_ratio, 12, 0.2).
narrative_ontology:measurement(ip_c_tr_t18, ip_category_emergence__synchronic_diachronic_seam, theater_ratio, 18, 0.24).
narrative_ontology:measurement(ip_c_tr_t24, ip_category_emergence__synchronic_diachronic_seam, theater_ratio, 24, 0.25).
narrative_ontology:measurement(ip_c_tr_t30, ip_category_emergence__synchronic_diachronic_seam, theater_ratio, 30, 0.25).

% Extraction over time
narrative_ontology:measurement(ip_c_be_t0, ip_category_emergence__synchronic_diachronic_seam, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(ip_c_be_t6, ip_category_emergence__synchronic_diachronic_seam, base_extractiveness, 6, 0.47).
narrative_ontology:measurement(ip_c_be_t12, ip_category_emergence__synchronic_diachronic_seam, base_extractiveness, 12, 0.53).
narrative_ontology:measurement(ip_c_be_t18, ip_category_emergence__synchronic_diachronic_seam, base_extractiveness, 18, 0.57).
narrative_ontology:measurement(ip_c_be_t24, ip_category_emergence__synchronic_diachronic_seam, base_extractiveness, 24, 0.56).
narrative_ontology:measurement(ip_c_be_t30, ip_category_emergence__synchronic_diachronic_seam, base_extractiveness, 30, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(ip_c_su_t0, ip_category_emergence__synchronic_diachronic_seam, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(ip_c_su_t6, ip_category_emergence__synchronic_diachronic_seam, suppression_requirement, 6, 0.42).
narrative_ontology:measurement(ip_c_su_t12, ip_category_emergence__synchronic_diachronic_seam, suppression_requirement, 12, 0.5).
narrative_ontology:measurement(ip_c_su_t18, ip_category_emergence__synchronic_diachronic_seam, suppression_requirement, 18, 0.52).
narrative_ontology:measurement(ip_c_su_t24, ip_category_emergence__synchronic_diachronic_seam, suppression_requirement, 24, 0.49).
narrative_ontology:measurement(ip_c_su_t30, ip_category_emergence__synchronic_diachronic_seam, suppression_requirement, 30, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ip_category_emergence__synchronic_diachronic_seam, information_standard).
narrative_ontology:affects_constraint(ip_category_emergence__synchronic_diachronic_seam, ip_category_emergence__thinkability_reading).
narrative_ontology:affects_constraint(ip_category_emergence__synchronic_diachronic_seam, ip_category_emergence__first_holding_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the ip_category_emergence kernel per the epsilon-invariance principle. The colloquial label 'the birth of IP in 1710' conflates two structurally distinct claims — category emergence (thinkability_reading) and occupancy change (first_holding_reading) — plus this third, meta-level reading (synchronic_diachronic_seam) that tests whether the two dimensions vary independently or their co-dating is a framing artifact. Each member carries its own epsilon, beneficiaries, and victims; the upstream empirical claims influence this reading's test conditions, and this reading's outcome feeds back into the legitimacy conditions under which both siblings operate. All three files link one another via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
