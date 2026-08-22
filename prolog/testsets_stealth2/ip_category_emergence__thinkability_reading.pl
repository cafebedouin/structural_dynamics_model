% ============================================================================
% CONSTRAINT STORY: ip_category_emergence__thinkability_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-07
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
 *   human_readable: Ownable Expression Category Emergence (Thinkability Reading)
 *   domain: legal_philosophy/intellectual_property/historical_jurisprudence
 *
 * SUMMARY:
 *   In 1710 the Statute of Anne made ownable expression a legally coherent
 *   category: before enactment, disputes over printing were argued in the
 *   vocabulary of trade privilege, licensing, and guild custom, and no party
 *   could articulate a property claim in expression as such; after enactment,
 *   'copy right' operated as a distinct legal kind, deployable against and
 *   displacing the older privilege framings. This story instantiates the
 *   thinkability_reading of the kernel ip_category_emergence: the statute's
 *   mark is categorical, a point added to conceptual space. The standing
 *   arrangement under contest, and the referent of every metric here, is the
 *   category as it operates today, descended from that emergence: a
 *   dispute-coordinating legal kind that simultaneously transfers value from
 *   readers and follow-on users to title-holders. This is one member of a
 *   three-story family (see network.dual_formulation_note); the sibling
 *   readings author different constraints with different epsilon values, and
 *   nothing in this file averages over them.
 *
 * KEY AGENTS:
 *   - KEY AGENTS (by structural relationship):
 *   - - corporate_rightsholders: Primary beneficiary (institutional/arbitrage) — holds the catalogs, captures the transferred value, funds maintenance
 *   - - publisher_assignees: Concentrated beneficiary (powerful/arbitrage) — aggregates titles, collects the majority of revenue
 *   - - authors_as_claimants: Nominal beneficiary (moderate/constrained) — gained legitimate standing in 1710; mostly assigns the title away
 *   - - the_reading_public: Primary payer (powerless/trapped) — bears access costs and term-extension losses diffusely
 *   - - follow_on_creators: Payer with identity lock (moderate/identity_locked) — needs licenses to practice the craft at all
 *   - - folk_tradition_communities: Payer and excluded voice (powerless/trapped) — bears appropriation without protection or representation
 *   - - parliamentary_legislators: Agenda setter (institutional/mobile) — holds amendment power, responsive to concentrated petitioners
 *   - - copyright_judiciary: Agenda setter (institutional/constrained) — administers the category's boundaries through interpretation
 *   - - legal_historians: Analytical observer (analytical/analytical) — supplies the contested genealogy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ip_category_emergence__thinkability_reading, 0.68).
domain_priors:suppression_score(ip_category_emergence__thinkability_reading, 0.7).
domain_priors:theater_ratio(ip_category_emergence__thinkability_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ip_category_emergence__thinkability_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(ip_category_emergence__thinkability_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(ip_category_emergence__thinkability_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ip_category_emergence__thinkability_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(ip_category_emergence__thinkability_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ip_category_emergence__thinkability_reading, tangled_rope).
narrative_ontology:human_readable(ip_category_emergence__thinkability_reading, "Ownable Expression Category Emergence (Thinkability Reading)").
narrative_ontology:topic_domain(ip_category_emergence__thinkability_reading, "legal_philosophy/intellectual_property/historical_jurisprudence").

domain_priors:requires_active_enforcement(ip_category_emergence__thinkability_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ip_category_emergence__thinkability_reading, 'c8fe6702-47af-4d3a-8abb-edc141a6c3f7').
narrative_ontology:cs_kernel_codification('c8fe6702-47af-4d3a-8abb-edc141a6c3f7', fixed_text).
narrative_ontology:cs_authority_grounding('c8fe6702-47af-4d3a-8abb-edc141a6c3f7', lineage).
narrative_ontology:cs_interpretation_layer_present('c8fe6702-47af-4d3a-8abb-edc141a6c3f7').
narrative_ontology:cs_reading_relation('c8fe6702-47af-4d3a-8abb-edc141a6c3f7', ip_category_emergence__first_holding_reading, influences).
narrative_ontology:cs_reading_relation('c8fe6702-47af-4d3a-8abb-edc141a6c3f7', ip_category_emergence__synchronic_diachronic_seam, coexists_with).
narrative_ontology:cs_axiom('c8fe6702-47af-4d3a-8abb-edc141a6c3f7', foundational, category_precedence_over_occupancy).
narrative_ontology:cs_axiom_status(category_precedence_over_occupancy, holdable).
narrative_ontology:cs_axiom_grounding('c8fe6702-47af-4d3a-8abb-edc141a6c3f7', category_precedence_over_occupancy, conventional).
narrative_ontology:cs_axiom('c8fe6702-47af-4d3a-8abb-edc141a6c3f7', secondary, vocabulary_conditions_articulable_claims).
narrative_ontology:cs_axiom_status(vocabulary_conditions_articulable_claims, holdable).
narrative_ontology:cs_axiom_grounding('c8fe6702-47af-4d3a-8abb-edc141a6c3f7', vocabulary_conditions_articulable_claims, empirically_contingent).
narrative_ontology:cs_reference_frame('c8fe6702-47af-4d3a-8abb-edc141a6c3f7', pre_statute_categoryless_expression_space).
narrative_ontology:cs_drift_state('c8fe6702-47af-4d3a-8abb-edc141a6c3f7', contemporary_term_extension_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('c8fe6702-47af-4d3a-8abb-edc141a6c3f7', '').
narrative_ontology:cs_kernel_id(ip_category_emergence__thinkability_reading, ip_category_emergence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ip_category_emergence__thinkability_reading, authors_as_claimants).
narrative_ontology:constraint_beneficiary(ip_category_emergence__thinkability_reading, publisher_assignees).
narrative_ontology:constraint_beneficiary(ip_category_emergence__thinkability_reading, corporate_rightsholders).
narrative_ontology:constraint_victim(ip_category_emergence__thinkability_reading, the_reading_public).
narrative_ontology:constraint_victim(ip_category_emergence__thinkability_reading, follow_on_creators).
narrative_ontology:constraint_victim(ip_category_emergence__thinkability_reading, folk_tradition_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Before 1710 a writer objecting to unauthorized printing of his work had no recognized legal kind to invoke; petitions framed the grievance as trade disorder or personal appeal to patrons. The statute gave writers a named, purchasable, inheritable title in their texts. In practice most authors sell or assign the title quickly to raise money; the title functions for them chiefly as a bargaining chip and occasional pension, and later in life many find their own early works held under terms they signed away.
narrative_ontology:constraint_stakeholder(ip_category_emergence__thinkability_reading, authors_as_claimants, beneficiary,
    moderate, biographical, constrained, global).

% The London trade that petitioned for the statute, and its commercial successors, buy and aggregate these titles. They collect the majority of the revenue the arrangement generates, decide what stays in print, and can restructure holdings across jurisdictions and formats. When terms look short they petition for extension; when enforcement looks thin they fund litigation. Capital moves freely, but their inventory consists entirely of the titles the category defines, so operating outside it is not a business they can conduct.
narrative_ontology:constraint_stakeholder(ip_category_emergence__thinkability_reading, publisher_assignees, beneficiary,
    powerful, biographical, arbitrage, global).

% Modern media, software, and entertainment firms hold century-scale catalogs of purchased titles as balance-sheet assets. Catalog value compounds with term length, licensing desks monetize backlists across platforms, and dedicated enforcement budgets protect asset value. This is the seat that today funds the lobbying campaigns and test-case litigation that maintain and extend the arrangement.
narrative_ontology:constraint_stakeholder(ip_category_emergence__thinkability_reading, corporate_rightsholders, beneficiary,
    institutional, generational, arbitrage, global).

% Readers pay for access to expression that previously moved through lending networks, chapbook trades, and communal recitation. Each term extension lands on them as a shrinking pool of freely usable works, and they cannot step outside the arrangement because effectively all recorded expression sits inside it. Any single reader's stake is small; collectively they are the largest payer, but diffusion leaves them with the weakest organized voice at every review.
narrative_ontology:constraint_stakeholder(ip_category_emergence__thinkability_reading, the_reading_public, payer,
    powerless, generational, trapped, global).

% Writers, musicians, filmmakers, translators, and scholars who build on prior expression need licenses or clearances for adaptation, quotation, sampling, and reuse, and carry infringement risk whenever they guess wrong about a boundary. Their creative practice is constituted through engagement with existing works; declining to engage the inherited tradition would mean ceasing to work in their art at all, so they negotiate from inside the arrangement even while contesting its terms.
narrative_ontology:constraint_stakeholder(ip_category_emergence__thinkability_reading, follow_on_creators, payer,
    moderate, biographical, identity_locked, global).

% Communities whose songs, tales, and designs accumulated collectively over generations predate the arrangement and fit it poorly: their expression has no identifiable individual author to hold a title, yet collectors and publishers have repeatedly taken the material, attached new authorship, and sold it back. They bear the appropriation costs without receiving the protections, and they were never seated when the arrangement was designed; no community representative testified in 1710 and few sit in modern treaty negotiations.
narrative_ontology:constraint_stakeholder(ip_category_emergence__thinkability_reading, folk_tradition_communities, payer,
    powerless, civilizational, trapped, global).
narrative_ontology:stakeholder_secondary_role(ip_category_emergence__thinkability_reading, folk_tradition_communities, excluded).

% Enacted the arrangement in 1710 and retain amendment power over its terms. At every review they hear concentrated, well-resourced rightsholder petitions against diffuse, weakly organized public objection; extensions pass with little friction because the costs fall on people not present in the room. They could shorten terms or broaden exemptions at any sitting; the political cost of doing so is what stays their hand.
narrative_ontology:constraint_stakeholder(ip_category_emergence__thinkability_reading, parliamentary_legislators, agenda_setter,
    institutional, biographical, mobile, national).

% Interprets what counts as ownable expression, where reproduction becomes infringement, and how much borrowing counts as fair dealing. Each ruling thickens or thins the category without touching the founding text; the interpretive layer has absorbed photography, film, software, and streaming, so the statute has never required formal revision. Judges collect nothing from the arrangement directly, but the interpretive authority they exercise depends on the category remaining live and continuously litigated.
narrative_ontology:constraint_stakeholder(ip_category_emergence__thinkability_reading, copyright_judiciary, agenda_setter,
    institutional, generational, constrained, national).

% Reconstruct what the 1710 statute actually marked, working from the parliamentary record, the trade's petitions, and the courtroom arguments that followed. They supply the genealogy over which the competing accounts of the statute contend, and they hold no stake in the revenues or the terms.
narrative_ontology:constraint_stakeholder(ip_category_emergence__thinkability_reading, legal_historians, observer,
    analytical, civilizational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ip_category_emergence__thinkability_reading, corporate_rightsholders).
narrative_ontology:fixing_cost_class(ip_category_emergence__thinkability_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The arrangement solves a dispute-coordination problem: it supplies a shared vocabulary and a legitimate-claimant structure for conflicts over copying, replacing ad hoc guild privileges and Crown printing patents with a generalizable legal kind under which who may reproduce what, on what terms, and for how long can be argued and settled.
% TRANSFER_FUNCTION: Moves exclusive-control rights over expression, and the revenue streams those rights command, from readers and follow-on users toward title-holders; in practice the bulk of transferred value accrues to assignees and corporate catalog owners rather than to the authors in whose name the category was created.
% ABSENT_VOICES: The reading public, follow-on creators, and folk-tradition communities were never seated when the category was designed: the 1710 hearings heard booksellers and author-petitioners, and modern term reviews hear rightsholder counsel. Future generations bear compounding term-extension costs and are absent by construction. Folk-tradition communities are doubly absent, bearing appropriation costs without representation in either the founding or the modern negotiation rooms.
% DISAPPEARANCE_RATIONALE: If the category vanished overnight, publishing markets, licensing desks, archive access rules, and platform content economies would all reorganize: disputes over copying would revert to contract, tort, and trade-custom arrangements; catalog asset values would evaporate; and a large body of currently restricted expression would become freely usable within the limits of surviving contracts. Every named seat's situation depends on the arrangement existing.
% FOUNDING_PROBLEM: The lapse of the Licensing Act in 1695 left the book trade without a settled basis for who might print what: the Stationers' Company defended perpetual property in its registered copies, provincial and Scottish reprinters challenged it, and Parliament faced simultaneous demands for trade order and for cheaper books. The statute was built to settle the trade by granting limited, purchasable titles while breaking the perpetual-monopoly claim.
% FOUNDING_PROBLEM_CORROBORATION: Rightsholder parties attest the founding problem is live, citing the incentive rationale. Outside the benefiting parties: the House of Lords proceedings in Donaldson v Becket (1774), where Scottish booksellers testified against the perpetual-property claim, corroborate the trade-settlement origin; modern book-trade historiography independent of rightsholder interests attests that the ordering function was achieved within decades; public-domain advocates and library associations attest that the incentive justification no longer tracks current term lengths. No single external source settles the contest, which is why the status is authored as contested rather than live or dead.
narrative_ontology:disappearance_verdict(ip_category_emergence__thinkability_reading, world_rearranges).
narrative_ontology:founding_problem_status(ip_category_emergence__thinkability_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ip_category_emergence__thinkability_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(ip_category_emergence__thinkability_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ip_category_emergence__thinkability_reading, 0.68, 'stealth/ox-alpha', 'none', direct).

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
 *   Extraction is authored at 0.68: substantial but bounded, because the category performs a real coordination function (shared dispute vocabulary, legitimate-claimant structure) alongside the transfer. Suppression is authored at 0.70 as a raw structural property — the arrangement's persistence depends on active enforcement machinery, from trade self-policing through customs seizure and notice-and-takedown regimes — and is deliberately unscaled; the engine owns the directionality and scope arithmetic that turns it into per-seat effective extraction. Theater ratio 0.58: the 'encouragement of learning' rationale is increasingly performative relative to the rent-preservation function it accompanies, crossing the proxy-substitution threshold late in the interval. Accessibility collapse 0.45: alternatives persist (public domain, open licensing, copyleft, patronage models) but all of them are defined relative to the category's frame, so escape is partial, not total. Resistance 0.55: organized pushback (library associations, open-access movements, reform coalitions, widespread informal copying) is real and continuous but has not displaced the arrangement. The measurement series run on one shared eight-point grid (three metrics at every point, 24 entries) so no metric's row is silently filled with an end-state scalar; trajectories are monotonic, with no oscillation, so no cyclical analysis applies. Suppression_requirement is tracked because the story's enforcement picture genuinely changes: enforcement capacity hardened from trade self-regulation to global statutory machinery over the interval.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the agenda-setter/beneficiary seats should compute differently, and the divergence is structural, not rhetorical. From the rightsholder seats the category is order itself: it defines their inventory, secures their assets, and gives their claims a legitimate form. From the reading public's seat the same category is an enclosure whose walls extend every time terms lengthen. Follow-on creators occupy the sharpest gap: they benefit from the category's existence (clearable permissions) while paying its steepest practice-level costs (licensing friction, infringement risk), and their identity lock means they cannot simply leave. The judiciary experiences the category as the source of its interpretive authority; historians see the whole structure from outside. The engine computes these per-seat classifications from the structural data; the authored claim adjudicates none of them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive the three rightsholder-adjacent seats toward the beneficiary end of directionality; the three victim declarations drive the payer seats toward the target end. The reading public derives a very high d: full-target position, powerless, trapped, global scope amplifying verification difficulty. Follow-on creators derive high d with the identity lock pushing them further toward the full-target end than their moderate power alone would suggest. Folk-tradition communities derive maximum d: they bear costs, receive none of the protections, and have no exit. Among the beneficiaries, publisher_assignees and corporate_rightsholders derive near-full-beneficiary d with arbitrage-grade exit damping their effective burden to near zero. Two nuances are documented rather than overridden. First, authors_as_claimants are declared beneficiaries and derive low d, but their actual position is nearer the middle: the assignment practice transfers the title's value away from them within years, and they later face their own backlists behind terms they sold. No override was authored because the override surface is keyed by power atom, and the only other moderate-power seat (follow_on_creators) requires the opposite correction — a per-atom override would misapply to one of the two. Second, the two agenda setters sit near-symmetric by derivation (they administer without collecting), with the legislature carrying a capture tilt toward the beneficiary side via its responsiveness to concentrated petitioners; that tilt is left to the engine's structural derivation rather than forced by override.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — settling the book trade after the Licensing Act's lapse — was substantially accomplished within decades of enactment, and the House of Lords' 1774 rejection of perpetual property confirmed the settlement. The arrangement nonetheless persists, sustained by an incentive rationale whose status is genuinely disputed: rightsholders attest it live, external historiography and public-interest advocates attest the ordering function is long dead and the incentive story no longer tracks term lengths. Authored as contested, this interacts with the world_rearranges verdict to produce no clean zombie flag on its own — but the temporal series carries the signal the mismatch check looks for: theater_ratio crosses 0.5 in the late interval, indicating the incentive rationale has become substantially performative maintenance. The tangled_rope claim prevents both mislabelings the corpus exists to catch: a pure-coordination reading would erase the payer seats and the receipt concentration in corporate catalogs; a pure-extraction reading would erase the real dispute-coordination function the vocabulary performs, which no serious reform proposal abolishes — they reprice it. The classification keeps both facts on the table and lets the drift series date any further degradation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_committer_structure,
    'This constraint instantiates only the thinkability_reading of kernel ip_category_emergence; what would each sibling reading change structurally if adopted instead?',
    'Cross-family comparison of the compiled stories: first_holding_reading re-authors the beneficiary structure around claimant-set entry in 1710; synchronic_diachronic_seam runs the M4/M5 collapse test on whether the thinkability and first-holding marks are formally independent or a temporal framing artifact.',
    'If the seam reading''s artifact horn holds, this story''s temporal marking dissolves into framing and its epsilon collapses onto a generic category-persistence value; if first_holding dominates, the beneficiary set re-centers on authors-as-first-occupants and the extraction asymmetry narrows.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'Committer structure: one reading of a three-reading kernel; sibling adoption changes beneficiary structure or dissolves the temporal mark.').

omega_variable(
    category_naturalness_vs_construct,
    'Does the ownable-expression category track a structural economic necessity (some appropriability regime for expression goods) or is it purely legislative construction serving identifiable holders?',
    'Comparative institutional analysis of expression markets under alternative appropriability regimes: prize systems, patronage, state subvention, open licensing with attribution.',
    'If necessity dominates, a measurable slice of the extraction is irreducible coordination cost and the arrangement sits closer to pure coordination; if construction dominates, the extraction is discretionary rent and the arrangement trends toward pure extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(category_naturalness_vs_construct, empirical, 'Whether the category''s extractive component is reducible to necessary coordination cost.').

omega_variable(
    thinkability_first_holding_separability,
    'Is the thinkability event this reading isolates separable from the first-holding occupancy event, or does the vocabulary shift presuppose and conceal the claimant-set change?',
    'Documentary analysis of pre-1710 dispute records (Stationers'' Company registers, Star Chamber printing cases, parliamentary petitions) for proto-ownership vocabulary; application of the M4/M5 collapse test to the paired event series.',
    'If inseparable, this reading''s epsilon double-counts the occupancy change and the family should merge into one story; if separable, the two readings stand as independent constraints with distinct epsilon referents.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(thinkability_first_holding_separability, conceptual, 'Whether category emergence and claimant-set entry are one event or two.').

omega_variable(
    term_ratchet_reversibility,
    'Can term length and scope contract through the category''s own amendment procedures, or does the arrangement structurally ratchet, with each extension becoming the baseline the next lobby defends?',
    'Natural experiments from jurisdictions that adopted treaty-permitted flexibilities or rolled back effective terms, tracking whether pre-extension baselines reset or extensions resume from the peak.',
    'If the ratchet is structural, the standing arrangement trends toward pure extraction over the next interval and the payer seats'' positions harden; if reversible, the mixed coordination-plus-transfer profile is stable.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(term_ratchet_reversibility, empirical, 'Whether the arrangement''s drift is self-reversing or a one-way ratchet.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ip_category_emergence__thinkability_reading, 0, 315).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ipe_think_tr_t0, ip_category_emergence__thinkability_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(ipe_think_tr_t45, ip_category_emergence__thinkability_reading, theater_ratio, 45, 0.15).
narrative_ontology:measurement(ipe_think_tr_t90, ip_category_emergence__thinkability_reading, theater_ratio, 90, 0.19).
narrative_ontology:measurement(ipe_think_tr_t135, ip_category_emergence__thinkability_reading, theater_ratio, 135, 0.24).
narrative_ontology:measurement(ipe_think_tr_t180, ip_category_emergence__thinkability_reading, theater_ratio, 180, 0.31).
narrative_ontology:measurement(ipe_think_tr_t225, ip_category_emergence__thinkability_reading, theater_ratio, 225, 0.4).
narrative_ontology:measurement(ipe_think_tr_t270, ip_category_emergence__thinkability_reading, theater_ratio, 270, 0.49).
narrative_ontology:measurement(ipe_think_tr_t315, ip_category_emergence__thinkability_reading, theater_ratio, 315, 0.58).

% Extraction over time
narrative_ontology:measurement(ipe_think_be_t0, ip_category_emergence__thinkability_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(ipe_think_be_t45, ip_category_emergence__thinkability_reading, base_extractiveness, 45, 0.32).
narrative_ontology:measurement(ipe_think_be_t90, ip_category_emergence__thinkability_reading, base_extractiveness, 90, 0.37).
narrative_ontology:measurement(ipe_think_be_t135, ip_category_emergence__thinkability_reading, base_extractiveness, 135, 0.43).
narrative_ontology:measurement(ipe_think_be_t180, ip_category_emergence__thinkability_reading, base_extractiveness, 180, 0.5).
narrative_ontology:measurement(ipe_think_be_t225, ip_category_emergence__thinkability_reading, base_extractiveness, 225, 0.57).
narrative_ontology:measurement(ipe_think_be_t270, ip_category_emergence__thinkability_reading, base_extractiveness, 270, 0.63).
narrative_ontology:measurement(ipe_think_be_t315, ip_category_emergence__thinkability_reading, base_extractiveness, 315, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(ipe_think_su_t0, ip_category_emergence__thinkability_reading, suppression_requirement, 0, 0.22).
narrative_ontology:measurement(ipe_think_su_t45, ip_category_emergence__thinkability_reading, suppression_requirement, 45, 0.25).
narrative_ontology:measurement(ipe_think_su_t90, ip_category_emergence__thinkability_reading, suppression_requirement, 90, 0.29).
narrative_ontology:measurement(ipe_think_su_t135, ip_category_emergence__thinkability_reading, suppression_requirement, 135, 0.35).
narrative_ontology:measurement(ipe_think_su_t180, ip_category_emergence__thinkability_reading, suppression_requirement, 180, 0.43).
narrative_ontology:measurement(ipe_think_su_t225, ip_category_emergence__thinkability_reading, suppression_requirement, 225, 0.52).
narrative_ontology:measurement(ipe_think_su_t270, ip_category_emergence__thinkability_reading, suppression_requirement, 270, 0.61).
narrative_ontology:measurement(ipe_think_su_t315, ip_category_emergence__thinkability_reading, suppression_requirement, 315, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ip_category_emergence__thinkability_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(ip_category_emergence__thinkability_reading, ip_category_emergence__first_holding_reading).
narrative_ontology:affects_constraint(ip_category_emergence__thinkability_reading, ip_category_emergence__synchronic_diachronic_seam).

% DUAL FORMULATION NOTE:
% Constraint family decomposition per the epsilon-invariance principle: the colloquial label 'what 1710 marked' conflates structurally distinct claims. This story (thinkability_reading) authors epsilon for the standing category arrangement as the conceptual-emergence account sees it: the category does real dispute-coordination work while transferring value to title-holders. The sibling first_holding_reading authors epsilon for the occupancy/claimant-set arrangement (who counted as a legitimate claimant), which yields a different beneficiary structure and a different epsilon. The sibling synchronic_diachronic_seam authors the framing relation between the two marks itself. Each file carries its own metrics and stakeholders; the family is linked through affects_constraints edges, with this reading declared upstream of first_holding (vocabulary conditions occupancy) and coexisting with the seam reading.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
