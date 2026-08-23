% ============================================================================
% CONSTRAINT STORY: ip_category_emergence__first_holding_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
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
 *   constraint_id: ip_category_emergence__first_holding_reading
 *   human_readable: Author First-Holding Allocation Regime (Statute of Anne Line — Occupancy Reading)
 *   domain: legal/jurisprudential/historical-economic
 *
 * SUMMARY:
 *   KEY AGENTS (by structural relationship): - statutory_authors: Entered the
 *   legitimate claimant set in 1710 (moderate/constrained) — nominal first
 *   holders; realized income flows mainly through assignment -
 *   publishing_assignees: Enforcement beneficiary and de facto agenda-setter
 *   (institutional/arbitrage) — holds assigned catalogs, funds term-extension
 *   and treaty lobbying, receives the rents - reading_public: Cost bearer
 *   (powerless/constrained) — pays above-reproduction-cost access prices,
 *   absorbs technical restrictions - subsequent_creators: Dual-positioned
 *   cost bearer (moderate/constrained) — pays clearance and risk on prior
 *   works, collects as later rights-holders - copyright_legislatures:
 *   Agenda-setter of record (institutional/treaty-constrained) -
 *   access_to_knowledge_advocates: Excluded voice (organized/mobile) —
 *   user-rights position outresourced in drafting rooms -
 *   ip_legal_scholarship: Analytical observer — documents the membership
 *   shift and tests the incentive record The Statute of Anne (1710) dissolved
 *   the Stationers' charter-based licensing order and vested title to copies
 *   in authors, for limited terms, enforceable in ordinary courts. This file
 *   instantiates the first_holding_reading of the ip_category_emergence
 *   kernel ONLY: on this reading, what the 1710 statute marks is an occupancy
 *   change — a shift in who legitimately holds and enforces rights over
 *   expression — within a practice of textual property that already existed
 *   under guild charter. The ε referent is the standing arrangement under
 *   contest: the author-first-holding allocation regime as it has actually
 *   operated from 1710 to the present, including the assignment economy
 *   layered on top of it. Sibling readings are separate constraint files
 *   joined through network.affects_constraints; committer structure is routed
 *   to the omegas and cs_structure, never averaged into ε. The claim/metric
 *   independence rule is honored deliberately: claimed_type records the
 *   structure I believe true (genuine coordination carrying asymmetric
 *   extraction), while the metrics record descriptive operation without being
 *   tuned to any predicted verdict.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ip_category_emergence__first_holding_reading, 0.74).
domain_priors:suppression_score(ip_category_emergence__first_holding_reading, 0.79).
domain_priors:theater_ratio(ip_category_emergence__first_holding_reading, 0.59).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ip_category_emergence__first_holding_reading, extractiveness, 0.74).
narrative_ontology:constraint_metric(ip_category_emergence__first_holding_reading, suppression_requirement, 0.79).
narrative_ontology:constraint_metric(ip_category_emergence__first_holding_reading, theater_ratio, 0.59).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ip_category_emergence__first_holding_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(ip_category_emergence__first_holding_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ip_category_emergence__first_holding_reading, tangled_rope).
narrative_ontology:human_readable(ip_category_emergence__first_holding_reading, "Author First-Holding Allocation Regime (Statute of Anne Line — Occupancy Reading)").
narrative_ontology:topic_domain(ip_category_emergence__first_holding_reading, "legal/jurisprudential/historical-economic").

domain_priors:requires_active_enforcement(ip_category_emergence__first_holding_reading).
narrative_ontology:has_sunset_clause(ip_category_emergence__first_holding_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ip_category_emergence__first_holding_reading, '7cbe5bff-039f-46a1-afdc-c2d6e2ee41b8').
narrative_ontology:cs_kernel_codification('7cbe5bff-039f-46a1-afdc-c2d6e2ee41b8', distributed).
narrative_ontology:cs_authority_grounding('7cbe5bff-039f-46a1-afdc-c2d6e2ee41b8', lineage).
narrative_ontology:cs_interpretation_layer_present('7cbe5bff-039f-46a1-afdc-c2d6e2ee41b8').
narrative_ontology:cs_reading_relation('7cbe5bff-039f-46a1-afdc-c2d6e2ee41b8', ip_category_emergence__thinkability_reading, influences).
narrative_ontology:cs_reading_relation('7cbe5bff-039f-46a1-afdc-c2d6e2ee41b8', ip_category_emergence__synchronic_diachronic_seam, coexists_with).
narrative_ontology:cs_axiom('7cbe5bff-039f-46a1-afdc-c2d6e2ee41b8', foundational, legitimate_textual_occupancy_predates_1710).
narrative_ontology:cs_axiom_status(legitimate_textual_occupancy_predates_1710, holdable).
narrative_ontology:cs_axiom_grounding('7cbe5bff-039f-46a1-afdc-c2d6e2ee41b8', legitimate_textual_occupancy_predates_1710, conventional).
narrative_ontology:cs_axiom('7cbe5bff-039f-46a1-afdc-c2d6e2ee41b8', foundational, statute_of_anne_reallocated_claimant_membership).
narrative_ontology:cs_axiom_status(statute_of_anne_reallocated_claimant_membership, holdable).
narrative_ontology:cs_axiom_grounding('7cbe5bff-039f-46a1-afdc-c2d6e2ee41b8', statute_of_anne_reallocated_claimant_membership, empirically_contingent).
narrative_ontology:cs_reference_frame('7cbe5bff-039f-46a1-afdc-c2d6e2ee41b8', claimant_set_membership_frame).
narrative_ontology:cs_drift_state('7cbe5bff-039f-46a1-afdc-c2d6e2ee41b8', post_digital_enforcement_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('7cbe5bff-039f-46a1-afdc-c2d6e2ee41b8', '2026-06-12T09:30:00Z').
narrative_ontology:cs_kernel_id(ip_category_emergence__first_holding_reading, ip_category_emergence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ip_category_emergence__first_holding_reading, statutory_authors).
narrative_ontology:constraint_beneficiary(ip_category_emergence__first_holding_reading, publishing_assignees).
narrative_ontology:constraint_victim(ip_category_emergence__first_holding_reading, reading_public).
narrative_ontology:constraint_victim(ip_category_emergence__first_holding_reading, subsequent_creators).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(ip_category_emergence__first_holding_reading, subsequent_creators).
narrative_ontology:constraint_vindicates(ip_category_emergence__first_holding_reading, statutory_author_primacy).
narrative_ontology:constraint_vindicates(ip_category_emergence__first_holding_reading, incentive_quid_pro_quo).
narrative_ontology:constraint_vindicates(ip_category_emergence__first_holding_reading, berne_national_treatment).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Write and publish works; since the Statute of Anne (1710) exclusive rights over copies vest in them first, for limited terms, transferable by contract. Most sign over the bulk of their rights to publishers on standard-form contracts, retaining advances, royalties, and named moral rights. Leaving the system means leaving commercial publication income behind; self-publishing and open licensing are partial exits that forfeit the advance-and-royalty channel.
narrative_ontology:constraint_stakeholder(ip_category_emergence__first_holding_reading, statutory_authors, beneficiary,
    moderate, biographical, constrained, global).

% Acquire copyrights from authors by assignment and hold large backlist, music, film, and software catalogs. Fund legislative lobbying for term extensions and enforcement treaties, litigate infringement across jurisdictions, and operate the licensing platforms through which most reuse is cleared. They move portfolios among favorable national regimes easily and need no exit from the international system because they substantially shape its agenda.
narrative_ontology:constraint_stakeholder(ip_category_emergence__first_holding_reading, publishing_assignees, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(ip_category_emergence__first_holding_reading, publishing_assignees, agenda_setter).

% Pay access prices well above reproduction cost and face regional locks, DRM, geoblocking, and takedown exposure. Reach some works only through library budgets and narrow statutory exceptions. When rightsholders withhold works from a market, there is no authorized alternative; the public domain, fair-use doctrines, and unlicensed copying are the available relief valves.
narrative_ontology:constraint_stakeholder(ip_category_emergence__first_holding_reading, reading_public, payer,
    powerless, biographical, constrained, global).

% Make works that build on prior works — adaptations, translations, sampling, scholarship, datasets — and pay clearance fees or carry infringement risk for protected inputs. At the same time they acquire their own exclusive rights in the new works, which they license or assign into the same market, so the same structure both bills them and pays them.
narrative_ontology:constraint_stakeholder(ip_category_emergence__first_holding_reading, subsequent_creators, payer,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(ip_category_emergence__first_holding_reading, subsequent_creators, beneficiary).

% Set term lengths, exceptions, and enforcement powers, and ratify treaties (Berne, TRIPS) that fix floors beneath domestic reduction. Hear concentrated, well-resourced industry representation on one side and diffuse public-interest testimony on the other. Amendment is procedurally open at home, but treaty commitments and trading-partner pressure bound how far terms or protections can fall.
narrative_ontology:constraint_stakeholder(ip_category_emergence__first_holding_reading, copyright_legislatures, agenda_setter,
    institutional, generational, constrained, national).

% Libraries, archives, digital-rights organizations, and open-licensing stewards press for shorter terms, broader user rights, and preservation exceptions. They appear at consultations but are outmatched in drafting rooms by industry counsel. Partly in response they have built parallel channels — Creative Commons licensing, open-access publishing, controlled digital lending — that operate outside the exclusive-right channel.
narrative_ontology:constraint_stakeholder(ip_category_emergence__first_holding_reading, access_to_knowledge_advocates, excluded,
    organized, generational, mobile, global).

% Document the 1710 transition, the Donaldson v. Becket settlement of 1774, the term-ratchet record, and the assignment economy; test incentive claims against the economic literature; and map the dispute over what the category itself marks. Collects no rents and bears no access costs; produces the record the other seats cite when they argue.
narrative_ontology:constraint_stakeholder(ip_category_emergence__first_holding_reading, ip_legal_scholarship, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ip_category_emergence__first_holding_reading, publishing_assignees).
narrative_ontology:fixing_cost_class(ip_category_emergence__first_holding_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the manuscript-market problem that the collapse of the licensing order left open: by vesting transferable exclusive titles in authors for limited terms it made writing a compensated trade, broke the guild's purchase-only gatekeeping, centralized enforcement of copy exclusivity in ordinary courts, and later coordinated cross-border treatment of foreign works through Berne-style national treatment.
% TRANSFER_FUNCTION: Moves monopoly rents on reproduction, distribution, performance, and adaptation from readers and subsequent users to rights-holders — nominally to authors first, in realized cash flow overwhelmingly to corporate assignees after acquisition — and moves enforcement labor and adjudication cost to the state.
% ABSENT_VOICES: Future generations have no seat: every term extension defers works from the public domain within living memory of anyone who might object. Readers in low-income markets priced out of textbooks and medicines-adjacent licensing regimes object but are unrepresented in treaty rooms. The displaced guild-order claimants of pre-1710 are historically absent by construction. User-rights advocates attend consultations but are structurally outresourced, which is exclusion in effect if not in form.
% DISAPPEARANCE_RATIONALE: If the author-vested exclusive-right regime vanished overnight, the publishing contract system, platform licensing, streaming and broadcast catalogs, library e-lending, and the entire clearance market would lose their legal substrate; prices would compress toward reproduction cost, reuse would shift from permission-seeking to norm-and-credit systems, and the incentive structure financing commissioned and serialized authorship would have to be rebuilt around patronage, public funding, or reputation economies.
% FOUNDING_PROBLEM: After the Licensing Act lapsed in 1695, printing titles were unstable, the guild's charter monopoly was dissolving, and authors had no way to earn from a manuscript except selling all rights outright to a bookseller. The Statute of Anne was framed to encourage learning by vesting titles in authors for limited terms and bringing the trade under ordinary law.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated outside the benefiting parties: legal historians working from the 1695 lapse and the 1709–1710 drafting record attest the title-disorder problem; economic historians and economists divided across the incentive literature (Höffner's comparative study; Boldrin and Levine's critique) attest that the incentive rationale is empirically unresolved; library and archive associations document access costs from an adverse seat. No neutral source certifies the incentive half as settled — the division itself is recorded signal.
narrative_ontology:disappearance_verdict(ip_category_emergence__first_holding_reading, world_rearranges).
narrative_ontology:founding_problem_status(ip_category_emergence__first_holding_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ip_category_emergence__first_holding_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(ip_category_emergence__first_holding_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ip_category_emergence__first_holding_reading, 0.74, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness is set high-but-not-maximal (0.74) because access prices float far above near-zero marginal reproduction cost and clearance burdens tax reuse, yet public domain, fair use, and open licensing keep the floor from saturation. Suppression (0.79) is authored as a raw structural property, unscaled by power or scope: persistence depends on actively barring unlicensed copying through courts, customs, DRM, and takedown automation — the alternatives are suppressed, not merely disfavored. Theater_ratio (0.59) reflects the widening gap between the author-compensation rationale and the assignment reality in which most value accrues to corporate catalogs; the incentive-review function is real but a growing share of rhetorical and enforcement activity defends the rent structure rather than authorship. Accessibility_collapse (0.5) sits mid-range: once the regime is understood, wholesale unlicensed republication collapses as an option, but substitutes (public domain, exceptions, open channels) remain workable. Resistance (0.62) is substantial and continuous: piracy at civilizational scale, the Eldred litigation, access-to-knowledge campaigns, and the AI-training disputes. All three tracked series share one grid (eight points, 1710–2026) so no metric is sampled against another metric's end-state; the series are monotone rising, not cyclical — extraction accumulates as terms lengthen and enforcement infrastructure matures (statute-era courts, through Berne, to DMCA-era automated filtering), theater grows as author-rhetoric decouples from royalty flows, and the suppression_requirement series traces enforcement-capacity build-out rather than mere extraction shift. Base_properties values equal the interval-end states of their series.
 *
 * PERSPECTIVAL GAP:
 *   The seats should classify differently from the same structural data. From the publishing_assignees seat the arrangement is the financing engine it operates: risk capital, catalog curation, and global enforcement look like the service that justifies the margin. From the statutory_authors seat the same structure is double-faced — the shield that made authorship a trade in 1710, and the treadmill that converts that shield into a signing bonus followed by assignment. From the reading_public seat it is a price wall with narrow doors. The legislature seat experiences it as a bounded agenda: open procedure, closed floor. The engine computes these per-seat classifications from the declared positions; the authored claim adjudicates none of them.
 *
 * DIRECTIONALITY LOGIC:
 *   Declarations map to directionality as follows. statutory_authors and publishing_assignees are declared beneficiaries, placing both near the beneficiary end; the assignees' dual agenda-setter role and their receipt of the realized gains are captured by the receipt surface (gain_flow) rather than by inflating their beneficiary declaration. reading_public is declared victim with constrained exit, sitting near the full-target end; their coalition weakness is a power fact, not an exit fact. subsequent_creators are declared victims but genuinely collect on the other side of the same market — the structural derivation from the victim declaration alone would read them near-full-target (~0.85), which misdescribes a seat whose costs and receipts are both material, so an explicit override sets d = 0.6. copyright_legislatures administer without systematically gaining or bearing, sitting near symmetric; access_to_knowledge_advocates are excluded rather than coordinated — their exclusion is maintained by resourcing asymmetry, not by rule; ip_legal_scholarship holds an analytical seat outside the χ computation. Larger-than-national scope amplifies effective extraction for targets because verification of compliance at global scale is harder; the engine owns that scaling from the scope atoms.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem has two halves with different vital signs. The guild-disorder and title-insecurity half is dead: no one proposes restoring charter licensing, and the arrangement's persistence cannot be explained by it. The author-income and learning-encouragement half is contested: live as policy rhetoric, empirically unresolved as causal fact. The R5 mismatch consumer reads status=contested against verdict=world_rearranges, which raises no zombie flag — arrangements the world demonstrably depends on are not inertial husks — but the term-ratchet omega marks where mandatrophy would incubate: the design carried a genuine sunset (limited terms), and a century of extensions, including retroactive ones, has been converting a transitional-support structure into an indefinitely renewed one without a new founding justification. The classification discipline cuts both ways here: reading the arrangement as pure extraction erases the real coordination achievement of 1710 (a functioning market for manuscripts, an ordinary-law enforcement path, a public-domain pipeline on paper), while reading it as pure coordination launders the rent layer that assignment concentration and term ratchets have deposited onto the original bargain. Tangled_rope is the honest middle: both functions, one structure, asymmetrically distributed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_semantic_location_first_holding,
    'This constraint is one reading of the ip_category_emergence kernel (reading: first_holding). If a sibling reading were adopted instead — thinkability_reading (category coherence arrives only in 1710) or synchronic_diachronic_seam (the two are formally independent or the dating is a framing artifact) — what structural facts of this story would move?',
    'Cross-reading comparison of victim sets and ε bases across the three linked files: whether the pre-1710 Stationers'' occupancy counts as legitimate membership (first_holding) or as pre-coherence noise (thinkability), and whether the 1710 date carries real structural weight (seam collapse test).',
    'Under thinkability, pre-1710 authors join the victim set (denied coherent ownership of their expression) and ε''s onset re-dates to the statute''s creative act; under seam-collapse this file merges with its siblings and the membership-shift delta disappears. Under the present reading neither happens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_semantic_location_first_holding, conceptual, 'Committer-frame routing: which sibling reading of the kernel, if adopted, relocates this story''s structural data.').

omega_variable(
    nominal_holder_vs_income_flow_gap,
    'Does author-first-holding still track economic holding, or has routine full assignment made the author''s entry into the claimant set ceremonial — title in name, income and control elsewhere?',
    'Royalty-flow and contract-structure data: share of works under full assignment versus licensing, author-share of net receipts over time, work-for-hire prevalence.',
    'If ceremonial, theater_ratio rises further, the reading''s ''change in enforcement beneficiary'' becomes purely historical, and the author seat''s computed classification drifts toward inertial maintenance while the assignee seat consolidates capture.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(nominal_holder_vs_income_flow_gap, empirical, 'Whether the 1710 membership shift still delivers holding power to its named occupants.').

omega_variable(
    incentive_bargain_efficacy,
    'Do exclusive rights at current terms and durations actually cause authorship and publication output, or has the coordination function detached from the extraction it finances?',
    'Natural experiments and comparative history: term-quasi-random variation in output studies, jurisdictions with weaker enforcement, open-access and public-funding alternatives at scale.',
    'If the incentive link fails at the margin, the coordination component of the arrangement thins toward cover and the classification boundary moves from tangled_rope toward snare; if it holds, part of the measured extraction is the price of the coordination itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(incentive_bargain_efficacy, empirical, 'Empirical status of the quid-pro-quo that legitimizes the standing arrangement.').

omega_variable(
    term_ratchet_reversibility,
    'The arrangement formally carries a sunset (every term expires), but successive extensions — including retroactive ones sustained through litigation — have pushed expiry outward for a century. Is the sunset mechanism recoverable, or is the ratchet locked?',
    'Legislative history of term bills and treaty floors, plus any observed instance of a major jurisdiction shortening effective protection; absence of any reversal across the interval is the observable.',
    'If locked, the has_sunset_clause declaration is honored only in form and the arrangement''s persistence increasingly resembles maintenance without transition; if recoverable, the scaffold-like element of the design remains live and the steady state stays bounded.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(term_ratchet_reversibility, preference, 'Whether the built-in expiry is structurally operative or has been ratcheted into abeyance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ip_category_emergence__first_holding_reading, 1710, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ipfh_tr_t1710, ip_category_emergence__first_holding_reading, theater_ratio, 1710, 0.18).
narrative_ontology:measurement_basis(ipfh_tr_t1710, observed).
narrative_ontology:measurement(ipfh_tr_t1774, ip_category_emergence__first_holding_reading, theater_ratio, 1774, 0.22).
narrative_ontology:measurement_basis(ipfh_tr_t1774, observed).
narrative_ontology:measurement(ipfh_tr_t1842, ip_category_emergence__first_holding_reading, theater_ratio, 1842, 0.28).
narrative_ontology:measurement_basis(ipfh_tr_t1842, observed).
narrative_ontology:measurement(ipfh_tr_t1886, ip_category_emergence__first_holding_reading, theater_ratio, 1886, 0.34).
narrative_ontology:measurement_basis(ipfh_tr_t1886, observed).
narrative_ontology:measurement(ipfh_tr_t1976, ip_category_emergence__first_holding_reading, theater_ratio, 1976, 0.44).
narrative_ontology:measurement_basis(ipfh_tr_t1976, observed).
narrative_ontology:measurement(ipfh_tr_t1998, ip_category_emergence__first_holding_reading, theater_ratio, 1998, 0.53).
narrative_ontology:measurement_basis(ipfh_tr_t1998, observed).
narrative_ontology:measurement(ipfh_tr_t2020, ip_category_emergence__first_holding_reading, theater_ratio, 2020, 0.57).
narrative_ontology:measurement_basis(ipfh_tr_t2020, observed).
narrative_ontology:measurement(ipfh_tr_t2026, ip_category_emergence__first_holding_reading, theater_ratio, 2026, 0.59).
narrative_ontology:measurement_basis(ipfh_tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(ipfh_be_t1710, ip_category_emergence__first_holding_reading, base_extractiveness, 1710, 0.5).
narrative_ontology:measurement_basis(ipfh_be_t1710, observed).
narrative_ontology:measurement(ipfh_be_t1774, ip_category_emergence__first_holding_reading, base_extractiveness, 1774, 0.54).
narrative_ontology:measurement_basis(ipfh_be_t1774, observed).
narrative_ontology:measurement(ipfh_be_t1842, ip_category_emergence__first_holding_reading, base_extractiveness, 1842, 0.58).
narrative_ontology:measurement_basis(ipfh_be_t1842, observed).
narrative_ontology:measurement(ipfh_be_t1886, ip_category_emergence__first_holding_reading, base_extractiveness, 1886, 0.62).
narrative_ontology:measurement_basis(ipfh_be_t1886, observed).
narrative_ontology:measurement(ipfh_be_t1976, ip_category_emergence__first_holding_reading, base_extractiveness, 1976, 0.66).
narrative_ontology:measurement_basis(ipfh_be_t1976, observed).
narrative_ontology:measurement(ipfh_be_t1998, ip_category_emergence__first_holding_reading, base_extractiveness, 1998, 0.71).
narrative_ontology:measurement_basis(ipfh_be_t1998, observed).
narrative_ontology:measurement(ipfh_be_t2020, ip_category_emergence__first_holding_reading, base_extractiveness, 2020, 0.73).
narrative_ontology:measurement_basis(ipfh_be_t2020, observed).
narrative_ontology:measurement(ipfh_be_t2026, ip_category_emergence__first_holding_reading, base_extractiveness, 2026, 0.74).
narrative_ontology:measurement_basis(ipfh_be_t2026, observed).

% Suppression requirement over time
narrative_ontology:measurement(ipfh_su_t1710, ip_category_emergence__first_holding_reading, suppression_requirement, 1710, 0.42).
narrative_ontology:measurement_basis(ipfh_su_t1710, observed).
narrative_ontology:measurement(ipfh_su_t1774, ip_category_emergence__first_holding_reading, suppression_requirement, 1774, 0.47).
narrative_ontology:measurement_basis(ipfh_su_t1774, observed).
narrative_ontology:measurement(ipfh_su_t1842, ip_category_emergence__first_holding_reading, suppression_requirement, 1842, 0.51).
narrative_ontology:measurement_basis(ipfh_su_t1842, observed).
narrative_ontology:measurement(ipfh_su_t1886, ip_category_emergence__first_holding_reading, suppression_requirement, 1886, 0.56).
narrative_ontology:measurement_basis(ipfh_su_t1886, observed).
narrative_ontology:measurement(ipfh_su_t1976, ip_category_emergence__first_holding_reading, suppression_requirement, 1976, 0.63).
narrative_ontology:measurement_basis(ipfh_su_t1976, observed).
narrative_ontology:measurement(ipfh_su_t1998, ip_category_emergence__first_holding_reading, suppression_requirement, 1998, 0.7).
narrative_ontology:measurement_basis(ipfh_su_t1998, observed).
narrative_ontology:measurement(ipfh_su_t2020, ip_category_emergence__first_holding_reading, suppression_requirement, 2020, 0.77).
narrative_ontology:measurement_basis(ipfh_su_t2020, observed).
narrative_ontology:measurement(ipfh_su_t2026, ip_category_emergence__first_holding_reading, suppression_requirement, 2026, 0.79).
narrative_ontology:measurement_basis(ipfh_su_t2026, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ip_category_emergence__first_holding_reading, resource_allocation).
narrative_ontology:affects_constraint(ip_category_emergence__first_holding_reading, ip_category_emergence__thinkability_reading).
narrative_ontology:affects_constraint(ip_category_emergence__first_holding_reading, ip_category_emergence__synchronic_diachronic_seam).

% DUAL FORMULATION NOTE:
% Constraint family for the ip_category_emergence kernel, decomposed into three ε-invariant readings per DP-001: this file instantiates first_holding (the 1710 statute marks a membership shift in the occupied claimant set; ε referenced to the standing author-first allocation regime); thinkability_reading instantiates category emergence (ownable expression became legally coherent only in 1710); synchronic_diachronic_seam tests whether the first two are formally independent or a temporal-framing artifact. The readings differ in ε basis and victim structure and are therefore separate stories, linked here. first_holding is upstream of thinkability: it supplies the pre-1710 occupancy baseline that the thinkability account must explain around, and the seam reading consumes both as its test objects.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ip_category_emergence__first_holding_reading, moderate, 0.6).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
