% ============================================================================
% CONSTRAINT STORY: ip_category_emergence__thinkability_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
 *   constraint_id: ip_category_emergence__thinkability_reading
 *   human_readable: Legal Coherence of Ownable Expression (Thinkability Reading of the 1710 Category Emergence)
 *   domain: legal/intellectual_property/historical_jurisprudence
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the contested kernel
 *   ip_category_emergence: the thinkability_reading, on which what 1710 marks
 *   is the emergence of a legally coherent category — ownable expression —
 *   rather than a change in who occupies rights. On this reading, pre-1710
 *   copying disputes ran through guild privilege, licensing, and patronage
 *   frames that lacked any conceptual slot for property in expression as
 *   such; the Statute of Anne's deployment of 'copy right' as distinct from
 *   stationer privilege opened a point in conceptual space that subsequently
 *   structured authorship, publishing, litigation, and the public domain. The
 *   constraint modeled here is the standing post-1710 arrangement in which
 *   ownable expression is a coherent and operative legal category. Its
 *   epsilon referent is that standing arrangement assessed by this reading's
 *   own lights — NOT the abolitionist or reform arrangements any seat might
 *   prefer, and NOT the claimant-composition change that the sibling
 *   first_holding_reading models. The claim/metric gap is deliberate: the
 *   category is CLAIMED as tangled_rope (genuine coordination function plus
 *   asymmetric extraction) while the metrics are authored from the
 *   arrangement's observed operation; the engine computes per-seat types from
 *   the structural data. Constraint family: this story links to
 *   ip_category_emergence__first_holding_reading (whose epsilon attaches to
 *   the composition of the legitimate claimant set — a different observable,
 *   hence a different file) and to
 *   ip_category_emergence__synchronic_diachronic_seam (which tests whether
 *   the thinkability and first-holding deltas are formally independent or a
 *   temporal-framing artifact). The three files form one family under the
 *   epsilon-invariance decomposition rule: the colloquial label 'what 1710
 *   did' conflates structurally distinct claims, and forcing them into one
 *   story would make epsilon observer-relative.
 *
 * KEY AGENTS:
 *   - westminster_legislators: agenda-setting enactor ([institutional]/[mobile]) — drew the category's initial boundaries in 1710 and retains power to redraw them
 *   - chancery_and_kings_bench_courts: adjudicative agenda-setter ([institutional]/[constrained]) — deployed the category in equity and law, then bounded it at the bar in 1774
 *   - working_authors: nominal beneficiary with payer exposure ([moderate]/[constrained]) — gained claimant standing, mostly sold it onward into the frame
 *   - bookseller_publishers: primary beneficiary and receipt-holder ([powerful]/[arbitrage]) — converted the category into tradeable, litigable, near-perpetual assets
 *   - unauthorized_reprinters: primary target ([moderate]/[mobile]) — Scottish and Irish presses living off jurisdictional seams in the category's reach
 *   - abridgers_and_translators: secondary target ([moderate]/[constrained]) — an accepted practice newly legible as infringement
 *   - reading_public: diffuse payer with incidental benefit ([moderate]/[constrained]) — bore proprietary prices and term waits; supplied the anti-monopoly pamphleteering
 *   - legal_historians_and_commentators: analytical observer ([analytical]/[analytical]) — attests the genealogy from outside every benefiting party
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ip_category_emergence__thinkability_reading, 0.56).
domain_priors:suppression_score(ip_category_emergence__thinkability_reading, 0.48).
domain_priors:theater_ratio(ip_category_emergence__thinkability_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ip_category_emergence__thinkability_reading, extractiveness, 0.56).
narrative_ontology:constraint_metric(ip_category_emergence__thinkability_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(ip_category_emergence__thinkability_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ip_category_emergence__thinkability_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(ip_category_emergence__thinkability_reading, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ip_category_emergence__thinkability_reading, tangled_rope).
narrative_ontology:human_readable(ip_category_emergence__thinkability_reading, "Legal Coherence of Ownable Expression (Thinkability Reading of the 1710 Category Emergence)").
narrative_ontology:topic_domain(ip_category_emergence__thinkability_reading, "legal/intellectual_property/historical_jurisprudence").

domain_priors:requires_active_enforcement(ip_category_emergence__thinkability_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ip_category_emergence__thinkability_reading, '0c4c6be9-a6c4-409d-bbc5-d0f9e2cbc9f4').
narrative_ontology:cs_kernel_codification('0c4c6be9-a6c4-409d-bbc5-d0f9e2cbc9f4', formalized).
narrative_ontology:cs_authority_grounding('0c4c6be9-a6c4-409d-bbc5-d0f9e2cbc9f4', lineage).
narrative_ontology:cs_interpretation_layer_present('0c4c6be9-a6c4-409d-bbc5-d0f9e2cbc9f4').
narrative_ontology:cs_reading_relation('0c4c6be9-a6c4-409d-bbc5-d0f9e2cbc9f4', ip_category_emergence__first_holding_reading, coexists_with).
narrative_ontology:cs_reading_relation('0c4c6be9-a6c4-409d-bbc5-d0f9e2cbc9f4', ip_category_emergence__synchronic_diachronic_seam, influences).
narrative_ontology:cs_axiom('0c4c6be9-a6c4-409d-bbc5-d0f9e2cbc9f4', foundational, category_precedes_claimant).
narrative_ontology:cs_axiom_status(category_precedes_claimant, holdable).
narrative_ontology:cs_axiom_grounding('0c4c6be9-a6c4-409d-bbc5-d0f9e2cbc9f4', category_precedes_claimant, conventional).
narrative_ontology:cs_axiom('0c4c6be9-a6c4-409d-bbc5-d0f9e2cbc9f4', secondary, vocabulary_constitutes_dispute_space).
narrative_ontology:cs_axiom_status(vocabulary_constitutes_dispute_space, holdable).
narrative_ontology:cs_axiom_grounding('0c4c6be9-a6c4-409d-bbc5-d0f9e2cbc9f4', vocabulary_constitutes_dispute_space, empirically_contingent).
narrative_ontology:cs_reference_frame('0c4c6be9-a6c4-409d-bbc5-d0f9e2cbc9f4', ownable_expression_legally_coherent).
narrative_ontology:cs_drift_state('0c4c6be9-a6c4-409d-bbc5-d0f9e2cbc9f4', post_revisionist_genealogy, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('0c4c6be9-a6c4-409d-bbc5-d0f9e2cbc9f4', '').
narrative_ontology:cs_kernel_id(ip_category_emergence__thinkability_reading, ip_category_emergence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ip_category_emergence__thinkability_reading, working_authors).
narrative_ontology:constraint_beneficiary(ip_category_emergence__thinkability_reading, bookseller_publishers).
narrative_ontology:constraint_victim(ip_category_emergence__thinkability_reading, unauthorized_reprinters).
narrative_ontology:constraint_victim(ip_category_emergence__thinkability_reading, abridgers_and_translators).
narrative_ontology:constraint_victim(ip_category_emergence__thinkability_reading, reading_public).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(ip_category_emergence__thinkability_reading, reading_public).
narrative_ontology:constraint_victim(ip_category_emergence__thinkability_reading, working_authors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enacted the 1710 statute after the Licensing Act lapsed, setting a fourteen-year term with a fourteen-year renewal for existing copies, a registration requirement, and statutory penalties for unauthorized printing. They drew the initial boundary of the new category and retain the power to redraw or abolish it; their deliberations were shaped by petitions from both booksellers seeking order and authors seeking standing.
narrative_ontology:constraint_stakeholder(ip_category_emergence__thinkability_reading, westminster_legislators, agenda_setter,
    institutional, generational, mobile, national).

% Adjudicated how the category deploys: heard the booksellers' suits for injunctions against reprints, entertained the common-law perpetual-property argument through mid-century, and in 1774 rejected common-law perpetuity in favor of the statutory term alone. Bound by precedent and doctrine; they administer the category rather than originating it.
narrative_ontology:constraint_stakeholder(ip_category_emergence__thinkability_reading, chancery_and_kings_bench_courts, agenda_setter,
    institutional, generational, constrained, national).

% Gained a legitimate claimant position: for the first time an author's title to his copy could be pleaded as property rather than sought as patronage or privilege. In practice most sold their rights outright to booksellers for lump sums, converting their new standing into immediate income and out of ongoing control; the patronage-and-dedication economy remained as a partial alternative but was shrinking.
narrative_ontology:constraint_stakeholder(ip_category_emergence__thinkability_reading, working_authors, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(ip_category_emergence__thinkability_reading, working_authors, payer).

% The London bookseller-conglomerates acquired registrations, built catalogs of copyrighted titles, and collected the returns from exclusive editions. They moved fluidly between frames — invoking authorial property when it suited, guild-style trade order when it did not — and financed the half-century litigation campaign for perpetual common-law copyright. Their trade extended into the American colonies, where enforcement did not follow.
narrative_ontology:constraint_stakeholder(ip_category_emergence__thinkability_reading, bookseller_publishers, beneficiary,
    powerful, biographical, arbitrage, national).

% Scottish and Irish printing houses built substantial trades on reprinting works whose proprietors were registered in London. Inside the statute's reach they faced suits, seizures, and damages; outside it (Ireland throughout the period, Scotland until enforcement machinery matured in the 1740s) they operated lawfully. Their exit was jurisdictional: relocate the press across a border the category did not yet span.
narrative_ontology:constraint_stakeholder(ip_category_emergence__thinkability_reading, unauthorized_reprinters, payer,
    moderate, biographical, mobile, regional).

% Produced abridgments, translations, and adaptations of protected works and found that the new category reached them: what had been an accepted literary practice now required license or risked action as infringement. Some negotiated licenses; others worked from foreign originals outside the register.
narrative_ontology:constraint_stakeholder(ip_category_emergence__thinkability_reading, abridgers_and_translators, payer,
    moderate, biographical, constrained, national).

% Paid proprietary prices for new works during their terms and waited out terms for cheap reprints, while also receiving whatever additional writing the exclusive-rights market induced. Scattered across Britain and the colonies, unorganized as a seat, but their pamphleteers supplied much of the anti-monopoly argument that culminated in the 1774 decision.
narrative_ontology:constraint_stakeholder(ip_category_emergence__thinkability_reading, reading_public, payer,
    moderate, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(ip_category_emergence__thinkability_reading, reading_public, beneficiary).

% From Blackstone's contemporary unease about literary property to modern archival scholarship on the statute's drafting and the stationers' prior registrations, this seat reconstructs what the 1710 enactment actually did. It attests or contests the genealogy from outside every benefiting party and collects no rent from the category's operation.
narrative_ontology:constraint_stakeholder(ip_category_emergence__thinkability_reading, legal_historians_and_commentators, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ip_category_emergence__thinkability_reading, bookseller_publishers).
narrative_ontology:fixing_cost_class(ip_category_emergence__thinkability_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the settlement problem left by the lapse of the Licensing Act: once printing made copying cheap and the old licensing machinery expired, disputes over who might print what had no shared resolution frame. The category of ownable expression gave authors, booksellers, and courts a single vocabulary in which claims could be registered, transferred, litigated, and priced.
% TRANSFER_FUNCTION: Moves exclusive-exploitation claims over texts — and the revenue those claims generate — toward registered proprietors, historically concentrated in the London bookseller houses, funded by proprietary prices paid by readers and by the foreclosed reprint and adaptation opportunities of provincial printers and derivative creators.
% ABSENT_VOICES: The Irish and Scottish reprinters had no seat at Westminster when the category was drawn; readers as price-bearers were represented only indirectly through pamphleteers; and future creators — translators, abridgers, later authors who would inherit an already-partitioned field — could not object to boundaries set before they existed. The category was settled among legislators, courts, and established bookseller interests.
% DISAPPEARANCE_RATIONALE: If the category vanished overnight, the publishing economy would rearrange around it: registered titles would lose their legal distinctness, the booksellers' asset catalogs would evaporate, author compensation would revert to patronage and advance-sale models, and every dispute now framed as property infringement would need reframing as trade privilege, contract, or nothing at all. The vocabulary of the disputes is itself downstream of the category.
% FOUNDING_PROBLEM: With the Licensing Act lapsed in 1695, copying disputes had no settlement mechanism: stationers' registrations were unenforceable against outsiders, unauthorized reprints multiplied, and authors had no recognized recourse. The 1710 statute was built to restore order to the book trade and, by its preamble's account, to encourage learned men to compose and publish useful works.
% FOUNDING_PROBLEM_CORROBORATION: The disorder problem is attested from outside the beneficiary set by the parliamentary record, the statute's own preamble, and contemporaneous pamphlet literature (including Defoe's 1710 appeal for the bill); modern archival historians corroborate the enforcement vacuum of the 1690s. By contrast, the claim that the problem remains live — that learning still requires exclusive rights as inducement — is attested almost exclusively by rights-holders and their counsel, and is disputed by reprinters' descendants in the trade and by public-domain advocates; no disinterested seat certifies its continued liveness.
narrative_ontology:disappearance_verdict(ip_category_emergence__thinkability_reading, world_rearranges).
narrative_ontology:founding_problem_status(ip_category_emergence__thinkability_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ip_category_emergence__thinkability_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(ip_category_emergence__thinkability_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ip_category_emergence__thinkability_reading, 0.56, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness is authored at 0.56 (end-state): the category's operation transfers substantial value to registered proprietors while leaving genuine coordination intact, so it sits well above a coordination-cost floor but well below pure-rent territory. Suppression (0.48) reflects enforcement that is real but bounded — statutory penalties, injunctions, seizures — short of the totalizing exclusion a snare requires; notably it is unscaled raw structure, while effective extraction is what the engine scales by directionality and scope. Theater_ratio (0.28) captures the mid-century detachment of the 'encouragement of learning' rhetoric from its operative function as the booksellers' perpetual-copyright campaign dressed trade monopoly in authorial garb — performative share rose from 0.11 at enactment to 0.30 at peak litigation before easing after 1774. Accessibility_collapse (0.45) is honestly moderate: patronage, subscription publishing, foreign presses, and manuscript circulation persisted as workable alternatives, so understanding the category did not extinguish exits — this is what keeps the profile away from mountain certification. Resistance (0.52) is substantial: fifty years of reprint wars, the pamphlet literature, and the Donaldson litigation itself. The measurement series run on ONE shared time grid (1695, 1710, 1725, 1740, 1757, 1774) with every tracked metric authored at every point, so no end-state value leaks backward into earlier rows. The enforcement arc — suppression_requirement rising to a 1740 peak as machinery matured against the Scottish trade, then falling after the 1774 decision narrowed the category to its statutory term — is the story's central dynamic, which is why suppression_requirement is tracked despite being optional.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute divergent types from identical structural data. From the bookseller_publishers seat the category is infrastructure they built portfolios on: coordination they profit from and litigate to defend — low effective extraction, rope-flavored. From the unauthorized_reprinters seat the same category is enforced exclusion calibrated exactly to where their presses stand — high effective extraction, snare-flavored, moderated only by their jurisdictional mobility. From the working_authors seat it is genuinely double-edged: standing they lacked before, discipline they did not choose — the secondary_role payer encoding exists precisely so the engine does not flatten this ambivalence into pure benefit. From the courts' seat it is doctrine to be administered, neither burden nor windfall. The divergence is computed from power, exit, and declared position; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   bookseller_publishers sit nearest the beneficiary pole (d near 0.05): they collect the category's returns directly and hold arbitrage-grade mobility between legal frames. working_authors derive low-but-not-negligible d from their beneficiary declaration, pulled upward by the secondary payer role — they received standing and surrendered control in the same transaction. westminster_legislators and the courts sit mildly beneficiary-side as agenda-setters with diffuse, indirect exposure. unauthorized_reprinters and abridgers_and_translators sit near the target pole (d roughly 0.75-0.85), the reprinters damped somewhat by real jurisdictional exit. reading_public land above symmetric (d roughly 0.6): net payers through prices and term waits, damped by the incidental production benefit their secondary role records. Scope amplification applies modestly: the category operates at national scale with continental spillover, making verification of enforcement harder at the margins — the engine owns that arithmetic; the story only declares the scopes.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards against mislabeling in both directions. Against pure-extraction readings: the category solves a real settlement problem — the post-1695 enforcement vacuum was genuine, and a shared vocabulary for copy disputes has irreducible coordination value — so a snare verdict would erase the function that made the arrangement adoptable. Against pure-coordination readings: the same structure that settles disputes partitions the field of expression and routes its returns to a concentrated proprietor class, so a rope verdict would launder the asymmetry. The founding problem is CONTESTED, not dead: order was restored (corroborated from outside the beneficiary set), while the incentive-continuation claim is certified only by the parties who collect from it. Because the founding problem is not dead, no mandatrophy-resolved flag is authored, and the arrangement is not a piton: its function is alive, its maintenance is substantive rather than theatrical (theater_ratio 0.28, below the proxy-substitution threshold), and identifiable parties profit enough to defend it actively — which is precisely what distinguishes this tangled_rope from an inertial leftover.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_instantiation,
    'This story instantiates the thinkability_reading of kernel ip_category_emergence: what structurally would the sibling readings change, and where exactly is the disagreement located?',
    'Cross-reading comparison of epsilon referents and victim sets across the family files, plus adjudication of the seam reading''s M4/M5 collapse test on whether category-emergence and claimant-entry are formally independent.',
    'If the seam reading resolves the distinction as a temporal-framing artifact, this reading merges with first_holding_reading and the family collapses to one constraint with reconciled epsilon; if formally independent, the three-file family persists with distinct referents.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_instantiation, conceptual, 'Committer structure: one reading of a contested kernel; sibling readings instantiate different constraints.').

omega_variable(
    pre1710_category_continuity,
    'Was ownable expression genuinely legally INCOHERENT before 1710 (true category emergence), or did enforceable property-like claims — stationer registrations, guild privileges, common-law arguments — already constitute a coherent category that the statute merely redeployed under new language?',
    'Archival analysis of pre-1710 dispute framing: Star Chamber decrees, stationers'' company registers, 1690s pamphlet literature, and the framing of the 1704-1709 copy-suits — testing whether plaintiffs and courts could and did plead property-in-expression before the statute supplied the vocabulary.',
    'Substantial continuity collapses this reading''s distinct delta toward first_holding_reading or the seam resolution and forces epsilon reconciliation; genuine discontinuity confirms a separate constraint with a separate referent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pre1710_category_continuity, empirical, 'Whether 1710 marks conceptual emergence or linguistic redeployment of an existing category.').

omega_variable(
    category_vs_deployment_epsilon,
    'Does the measured extraction attach to the category''s bare legal coherence, or to its enforcement-backed deployment — and does this story''s referent cleanly separate the two?',
    'Counterfactual and jurisdictional comparison: the Irish trade operated outside the statute''s reach under the same conceptual availability, so differential extraction across the enforcement boundary isolates the deployment component from the coherence component.',
    'If bare coherence carries negligible extraction, part of the measured epsilon belongs to downstream enforcement stories rather than this one, lowering this story''s extractiveness toward the coordination-cost floor; if the category''s coherence itself channels value asymmetrically even unenforced, the full measure stays here.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(category_vs_deployment_epsilon, conceptual, 'Referent boundary between category-level and enforcement-level extraction within the family.').

omega_variable(
    encouragement_rhetoric_sincerity,
    'To what degree did the ''encouragement of learning'' justification track actual incentive effects on authorship, versus serving as cover for trade-monopoly consolidation by the bookseller houses?',
    'Distributional comparison across the interval: authorial earnings and output under the regime versus bookseller profit concentration and catalog consolidation; the divergence between stated purpose and distributed benefit indexes the rhetorical share.',
    'A wide divergence raises the credible theater_ratio and supports the extraction-weighted reading of the arrangement; a narrow divergence supports the incentive-justification doctrine the constraint''s operation vindicates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(encouragement_rhetoric_sincerity, empirical, 'Sincerity of the learning-justification versus its function as legitimating cover.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ip_category_emergence__thinkability_reading, 1695, 1774).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ip_c_tr_t1695, ip_category_emergence__thinkability_reading, theater_ratio, 1695, 0.16).
narrative_ontology:measurement(ip_c_tr_t1710, ip_category_emergence__thinkability_reading, theater_ratio, 1710, 0.11).
narrative_ontology:measurement(ip_c_tr_t1725, ip_category_emergence__thinkability_reading, theater_ratio, 1725, 0.17).
narrative_ontology:measurement(ip_c_tr_t1740, ip_category_emergence__thinkability_reading, theater_ratio, 1740, 0.26).
narrative_ontology:measurement(ip_c_tr_t1757, ip_category_emergence__thinkability_reading, theater_ratio, 1757, 0.3).
narrative_ontology:measurement(ip_c_tr_t1774, ip_category_emergence__thinkability_reading, theater_ratio, 1774, 0.28).

% Extraction over time
narrative_ontology:measurement(ip_c_be_t1695, ip_category_emergence__thinkability_reading, base_extractiveness, 1695, 0.34).
narrative_ontology:measurement(ip_c_be_t1710, ip_category_emergence__thinkability_reading, base_extractiveness, 1710, 0.5).
narrative_ontology:measurement(ip_c_be_t1725, ip_category_emergence__thinkability_reading, base_extractiveness, 1725, 0.57).
narrative_ontology:measurement(ip_c_be_t1740, ip_category_emergence__thinkability_reading, base_extractiveness, 1740, 0.63).
narrative_ontology:measurement(ip_c_be_t1757, ip_category_emergence__thinkability_reading, base_extractiveness, 1757, 0.59).
narrative_ontology:measurement(ip_c_be_t1774, ip_category_emergence__thinkability_reading, base_extractiveness, 1774, 0.56).

% Suppression requirement over time
narrative_ontology:measurement(ip_c_su_t1695, ip_category_emergence__thinkability_reading, suppression_requirement, 1695, 0.38).
narrative_ontology:measurement(ip_c_su_t1710, ip_category_emergence__thinkability_reading, suppression_requirement, 1710, 0.46).
narrative_ontology:measurement(ip_c_su_t1725, ip_category_emergence__thinkability_reading, suppression_requirement, 1725, 0.56).
narrative_ontology:measurement(ip_c_su_t1740, ip_category_emergence__thinkability_reading, suppression_requirement, 1740, 0.66).
narrative_ontology:measurement(ip_c_su_t1757, ip_category_emergence__thinkability_reading, suppression_requirement, 1757, 0.61).
narrative_ontology:measurement(ip_c_su_t1774, ip_category_emergence__thinkability_reading, suppression_requirement, 1774, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ip_category_emergence__thinkability_reading, resource_allocation).
narrative_ontology:affects_constraint(ip_category_emergence__thinkability_reading, ip_category_emergence__first_holding_reading).
narrative_ontology:affects_constraint(ip_category_emergence__thinkability_reading, ip_category_emergence__synchronic_diachronic_seam).

% DUAL FORMULATION NOTE:
% Constraint family decomposition of the colloquial label 'what 1710 did' per the epsilon-invariance principle. The label conflates at least two structurally distinct claims: an occupancy change (first_holding_reading — the legitimate claimant set gains the author) and a category emergence (this file — ownable expression becomes legally coherent). Their epsilon values differ because their observables differ: claimant-set composition versus the coherence and operation of the category itself. The synchronic_diachronic_seam file holds the collapse test over the pair. Upstream/downstream structure: this reading's claimed distinctness is the input the seam reading evaluates, and both concrete readings are cited as evidence within the seam's framing debate. All three files carry mutual affects_constraints edges; orphaning any member would break contamination-propagation analysis across the family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
