% ============================================================================
% CONSTRAINT STORY: ip_category_emergence__first_holding_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
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
 *   human_readable: Statute of Anne Claimant-Set Settlement (First-Holding Reading)
 *   domain: legal philosophy/intellectual property/historical jurisprudence
 *
 * SUMMARY:
 *   In 1710 the Statute of Anne replaced the lapsed licensing-and-privilege
 *   regime with a statutory, term-limited exclusive right vested first in
 *   authors and transferable by assignment. This story instantiates the
 *   FIRST-HOLDING reading of the ip_category_emergence kernel: the legally
 *   significant event was a change in the membership of an already-occupied
 *   set of legitimate claimants over printed texts — the
 *   author-as-rights-holder entered a set the Stationers' Company had
 *   monopolized — and the enforcement beneficiary consequently changed from a
 *   chartered guild to whoever held the statutory title, in practice
 *   overwhelmingly the trade through assignment. The claim/metric
 *   independence rule applies: the tangled_rope claim is authored from the
 *   structural facts (genuine incentive-and-expiry bargain plus concentrated
 *   capture), and the metric values are authored independently from the
 *   descriptive record. KEY AGENTS (by structural relationship): -
 *   london_bookseller_trade: Agenda-setting beneficiary
 *   (organized/constrained) — lobbied for the bill, administers registration,
 *   initiates suits, collects term-time revenue through assignments -
 *   rights_holding_authors: Formal beneficiary (moderate/constrained) —
 *   gained standing to hold and sell the right; mostly converted it to
 *   one-time sale proceeds - term_time_readers: Payer (powerless/constrained)
 *   — pays above-printing-cost prices throughout each term with no lawful
 *   substitute - downstream_creators: Payer (moderate/constrained) —
 *   translators, abridgers, and successors locked out of protected matter
 *   during terms - provincial_reprint_networks: Excluded payer
 *   (organized/arbitrage) — Scottish and Irish printers outside the statutory
 *   franchise, exploiting jurisdictional seams - westminster_parliament:
 *   Agenda setter (institutional/mobile) — enacted the settlement and retains
 *   amendment power - courts_of_westminster_hall: Observer
 *   (institutional/analytical) — fixed the statutory/perpetuity boundary in
 *   1769-1774
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ip_category_emergence__first_holding_reading, 0.6).
domain_priors:suppression_score(ip_category_emergence__first_holding_reading, 0.55).
domain_priors:theater_ratio(ip_category_emergence__first_holding_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ip_category_emergence__first_holding_reading, extractiveness, 0.6).
narrative_ontology:constraint_metric(ip_category_emergence__first_holding_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(ip_category_emergence__first_holding_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ip_category_emergence__first_holding_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(ip_category_emergence__first_holding_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ip_category_emergence__first_holding_reading, tangled_rope).
narrative_ontology:human_readable(ip_category_emergence__first_holding_reading, "Statute of Anne Claimant-Set Settlement (First-Holding Reading)").
narrative_ontology:topic_domain(ip_category_emergence__first_holding_reading, "legal philosophy/intellectual property/historical jurisprudence").

domain_priors:requires_active_enforcement(ip_category_emergence__first_holding_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ip_category_emergence__first_holding_reading, '0fd5847b-8ad4-4a9a-92a7-44042fc35f07').
narrative_ontology:cs_kernel_codification('0fd5847b-8ad4-4a9a-92a7-44042fc35f07', fixed_text).
narrative_ontology:cs_authority_grounding('0fd5847b-8ad4-4a9a-92a7-44042fc35f07', lineage).
narrative_ontology:cs_interpretation_layer_present('0fd5847b-8ad4-4a9a-92a7-44042fc35f07').
narrative_ontology:cs_reading_relation('0fd5847b-8ad4-4a9a-92a7-44042fc35f07', ip_category_emergence__thinkability_reading, forecloses).
narrative_ontology:cs_reading_relation('0fd5847b-8ad4-4a9a-92a7-44042fc35f07', ip_category_emergence__synchronic_diachronic_seam, influences).
narrative_ontology:cs_axiom('0fd5847b-8ad4-4a9a-92a7-44042fc35f07', foundational, claimant_set_membership_is_the_mark).
narrative_ontology:cs_axiom_status(claimant_set_membership_is_the_mark, holdable).
narrative_ontology:cs_axiom_grounding('0fd5847b-8ad4-4a9a-92a7-44042fc35f07', claimant_set_membership_is_the_mark, conventional).
narrative_ontology:cs_axiom('0fd5847b-8ad4-4a9a-92a7-44042fc35f07', secondary, expiry_distinguishes_right_from_privilege).
narrative_ontology:cs_axiom_status(expiry_distinguishes_right_from_privilege, holdable).
narrative_ontology:cs_axiom_grounding('0fd5847b-8ad4-4a9a-92a7-44042fc35f07', expiry_distinguishes_right_from_privilege, conventional).
narrative_ontology:cs_reference_frame('0fd5847b-8ad4-4a9a-92a7-44042fc35f07', statutory_limited_term_open_claimant_set).
narrative_ontology:cs_drift_state('0fd5847b-8ad4-4a9a-92a7-44042fc35f07', contemporary_term_extension_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('0fd5847b-8ad4-4a9a-92a7-44042fc35f07', '').
narrative_ontology:cs_kernel_id(ip_category_emergence__first_holding_reading, ip_category_emergence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ip_category_emergence__first_holding_reading, london_bookseller_trade).
narrative_ontology:constraint_beneficiary(ip_category_emergence__first_holding_reading, rights_holding_authors).
narrative_ontology:constraint_victim(ip_category_emergence__first_holding_reading, term_time_readers).
narrative_ontology:constraint_victim(ip_category_emergence__first_holding_reading, downstream_creators).
narrative_ontology:constraint_victim(ip_category_emergence__first_holding_reading, provincial_reprint_networks).
narrative_ontology:constraint_vindicates(ip_category_emergence__first_holding_reading, statutory_incentive_bargain).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Debated and enacted the 1710 settlement after the licensing regime lapsed, balancing trade petitions for restored exclusivity against anti-monopoly sentiment. Retains power at any time to amend terms, extend or shorten rights, or abolish the arrangement; its attention is episodic and driven by lobbying pressure.
narrative_ontology:constraint_stakeholder(ip_category_emergence__first_holding_reading, westminster_parliament, agenda_setter,
    institutional, generational, mobile, national).

% London publishing houses financed printing, bought manuscripts and assignments, entered titles at Stationers' Hall, and brought infringement suits. Their pre-1710 copyhold lost protection when licensing lapsed; the 1710 settlement restored enforceable exclusivity on new terms, and assignment practice routed most term-time revenue to them. Leaving the trade would mean abandoning sunk capital, stock, and craft networks.
narrative_ontology:constraint_stakeholder(ip_category_emergence__first_holding_reading, london_bookseller_trade, beneficiary,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(ip_category_emergence__first_holding_reading, london_bookseller_trade, agenda_setter).

% Gained statutory standing to hold a fourteen-or-twenty-one-year exclusive right in their own name and to sell or assign it. Most sold outright before publication for lump sums, converting the new standing into one-time proceeds; their livelihood runs through commissioning and sale relationships with the trade, and declining to publish leaves the work unwritten.
narrative_ontology:constraint_stakeholder(ip_category_emergence__first_holding_reading, rights_holding_authors, beneficiary,
    moderate, biographical, constrained, national).

% Buy protected books at prices set well above the cost of paper and presswork. No lawful cheaper edition exists during the term; the available substitutes are borrowing, reading rooms, second-hand circulation, or waiting for expiry.
narrative_ontology:constraint_stakeholder(ip_category_emergence__first_holding_reading, term_time_readers, payer,
    powerless, biographical, constrained, national).

% Translators, abridgers, compilers, and later writers who build on protected texts face suit if they reuse protected matter before expiry. Part of their working material is locked for the duration of each term, shaping what they can produce and how quickly.
narrative_ontology:constraint_stakeholder(ip_category_emergence__first_holding_reading, downstream_creators, payer,
    moderate, biographical, constrained, national).

% Scottish and Irish printers reprint popular London titles from outside the effective reach of Westminster enforcement; Irish printing operated under a separate jurisdiction until 1801. They profit from the price gap between London editions and local reprints, and bear forfeiture and damages risk when caught operating inside English jurisdiction. They had no seat in the legislative process that defined the lawful set.
narrative_ontology:constraint_stakeholder(ip_category_emergence__first_holding_reading, provincial_reprint_networks, excluded,
    organized, biographical, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(ip_category_emergence__first_holding_reading, provincial_reprint_networks, payer).

% Chancery and common-law judges decide infringement and title disputes. In Millar v Taylor (1769) and Donaldson v Becket (1774) they adjudicated whether a common-law perpetual right survived alongside or beneath the statutory term, fixing the boundary of the settlement for the following century.
narrative_ontology:constraint_stakeholder(ip_category_emergence__first_holding_reading, courts_of_westminster_hall, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ip_category_emergence__first_holding_reading, london_bookseller_trade).
narrative_ontology:fixing_cost_class(ip_category_emergence__first_holding_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the underproduction problem for new writing: a bounded exclusive right makes composition and publication financeable by granting a recoverable return; Stationers' Hall registration centralizes title-record keeping; mandatory deposit supplies library copies; and the fixed term guarantees eventual open access, converting an indefinite guild privilege into a predictable, expiring grant.
% TRANSFER_FUNCTION: Moves money from readers and downstream users to rights-holders through above-printing-cost prices during each term, and in practice routes most of it to the booksellers who hold assignments; it also moves legal standing, transferring recognized claimant status from a chartered company to a statutorily defined class rooted in authorship.
% ABSENT_VOICES: Readers had no organized representation in the 1710 debates; Irish printers stood wholly outside the Westminster franchise; future authors affected by term design were unrepresented; the 'learned men' of the preamble spoke only through individual petitions. The enrolled Act's unanimity reflects who was in the room — the trade, its parliamentary allies, and anti-monopoly backbenchers — not the consent of those priced or excluded.
% DISAPPEARANCE_RATIONALE: If the settlement vanished overnight, the trade would fall back on informal trade courtesy and renewed charter lobbying; title uncertainty would return; prices and publication decisions would reorganize around whatever privilege regime next consolidated; the deposit and library-copy machinery would lapse with it.
% FOUNDING_PROBLEM: After the Licensing Act lapsed in 1694-95, the Stationers' Company's copyhold lost legal protection against member defection and Scottish and Irish competition; the trade petitioned Parliament for a new foundation for exclusive printing rights, while anti-monopoly politics demanded hard limits on any revived privilege.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians outside the benefiting parties corroborate the trade-origin genealogy from archival sources — Deazley's analysis of the drafting history, Rose's and Johns' accounts of Stationer lobbying — and parliamentary journals record the anti-monopoly framing contemporaneously. Whether the founding problem survives in transformed form (an author-incentive problem) or died with the trade's original aim (a restored monopoly) is disputed between economic theorists and historians; no single attesting source outside the dispute settles it.
narrative_ontology:disappearance_verdict(ip_category_emergence__first_holding_reading, world_rearranges).
narrative_ontology:founding_problem_status(ip_category_emergence__first_holding_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ip_category_emergence__first_holding_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(ip_category_emergence__first_holding_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ip_category_emergence__first_holding_reading, 0.6, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness 0.60: term-time prices sat far above near-zero reproduction cost, tempered by finite terms and the deposit/expiry tail that guaranteed eventual open access; the series rises across the interval as assignment practice standardized and the trade pressed for perpetuity, dipping slightly at the 1774 reversal. Suppression 0.55: enforcement ran through Stationers' Hall registration and Chancery and common-law suits — weaker machinery than the pre-1710 licensing regime, but actively maintained and intensifying through mid-century litigation (Millar v Taylor, the Donaldson campaign). Theater 0.35: the 'Encouragement of Learned Men' preamble performed legitimation while assignment practice routed rents to the trade, yet the registration and deposit functions were real work, so the performative share stays below half. Accessibility_collapse 0.45: alternatives persist — waiting out terms, writing around protected matter, extraterritorial reprinting — so understanding the arrangement does not close the option space. Resistance 0.60: Scottish and Irish reprinting, the trade's own perpetuity agitation, and recurring reader price grievance met the arrangement throughout. All three tracked series share one eight-point grid (t=0,10,20,30,40,50,60,64) so no metric is ever sampled against another's end-state; the interval maps to 1710-1774.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently from identical structure. From the bookseller seat the settlement reads as a repair of broken monopoly tooling — continuity with their pre-1710 copyhold, new paperwork. From the author seat it reads as rupture — a standing in their own name that did not exist before. From the reader seat it is simply a price regime with an expiry date. From the provincial printer seat it is an exclusion enforced across a jurisdictional seam they never consented to. The engine derives these divergent per-seat classifications from the power and exit data; nothing in the authored claim adjudicates between them.
 *
 * DIRECTIONALITY LOGIC:
 *   The trade and authors sit at the beneficiary end: the settlement subsidizes them (low d, damped effective burden). Term-time readers and downstream creators sit near the target end: they bear the transfer with constrained exits. Provincial reprint networks are declared victims, but their arbitrage-grade exit across jurisdictions pulls their effective position away from the full-target pole — they are the seat the enforcement machinery chases, not the seat it holds. Parliament sits near symmetric as the enabling principal that bears administration costs and collects no term-time revenue. The courts are analytical. Suppression is authored as a raw structural property and is not scaled by anything; only extractiveness is scaled, by directionality and spatial scope, in the engine's computation.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope claim keeps both halves of the structure visible. A pure-snare reading would erase the term-limit bargain whose operativeness the 1774 litigation confirms — the trade sued to escape the expiry and lost, which is strong evidence the expiry was doing real work. A pure-rope reading would erase the concentrated capture documented in assignment practice, where the formal beneficiary class and the collecting seat diverge within months of enactment. The founding problem is contested and the mandate is live, so no mandatrophy resolution is declared; the constraint has not outlived its function, but neither does its function excuse the asymmetry.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_first_holding_vs_thinkability,
    'This constraint is one reading of kernel ip_category_emergence: the thinkability sibling holds that 1710 created the very legal coherence of ownable expression, whereas this reading holds that a pre-existing occupied claimant set changed members — which history does the 1710 transition instantiate?',
    'Pre-1710 enforcement-practice analysis: if Stationer copyhold was litigated, willed, and transferred as a holding over texts (same object, different holder), the membership-shift reading stands; if Stationer rights were mere regulatory licenses over the act of printing (a different object), the category-emergence reading stands.',
    'Under the sibling reading, this constraint''s epsilon referent shifts to a newly created category with no prior occupancy, and the pre-1710 Stationer regime becomes a separate constraint-family member rather than this constraint''s baseline; the victim and beneficiary sets would be redrawn accordingly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_first_holding_vs_thinkability, conceptual, 'Kernel-level ambiguity between membership-shift and category-emergence histories of the 1710 transition.').

omega_variable(
    assignment_capture_share,
    'What fraction of statutory terms were assigned to booksellers before or immediately upon publication, and did authors retain any ongoing interest in term-time revenue?',
    'Stationers'' Hall entry books, author-publisher correspondence, and probate records of author estates can establish the share of terms assigned outright versus retained.',
    'Near-total capture would recast the enforcement beneficiary as continuous with the pre-1710 trade, weakening the membership-shift delta and concentrating the collecting seat further; significant author retention would strengthen the author-protection reading and redistribute the seat structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(assignment_capture_share, empirical, 'Degree to which assignment practice captured the statutory benefit away from the formal beneficiary class.').

omega_variable(
    perpetuity_litigation_counterfactual,
    'Had the House of Lords affirmed a common-law perpetual right in Donaldson v Becket (1774), would the statutory claimant-set expansion have been substantively hollowed?',
    'Counterfactual analysis of trade behavior under continued perpetuity litigation, benchmarked against the observed Scottish and Irish market responses to the 1774 reversal.',
    'A hollowed settlement would date this constraint''s operative coordination function to the 1774 reversal rather than the 1710 enactment, moving the interval''s classification endpoint and reweighting the drift series.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(perpetuity_litigation_counterfactual, conceptual, 'Whether the 1774 perpetuity reversal was load-bearing for the settlement''s structure.').

omega_variable(
    seam_temporal_framing_artifact,
    'The synchronic_diachronic_seam sibling contends that dating this reading''s membership event to 1710 may be a temporal-framing artifact (the M4/M5 collapse test) — is the first-holding change a dated event or a gradual re-description spread across 1695-1730?',
    'Enforcement-beneficiary continuity analysis across 1695-1730: if the operative collecting seat changed gradually through accumulating assignment practice rather than at enactment, the dated-event framing fails.',
    'If the dating is an artifact, this constraint''s interval boundaries and its t0/t1 drift vector misdescribe the transition, and classification would need a smoothed trajectory rather than an enactment-dated step.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(seam_temporal_framing_artifact, conceptual, 'Whether the first-holding reading''s 1710 dating survives the seam sibling''s collapse test.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ip_category_emergence__first_holding_reading, 0, 64).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ip_c_tr_t0, ip_category_emergence__first_holding_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement_basis(ip_c_tr_t0, observed).
narrative_ontology:measurement(ip_c_tr_t10, ip_category_emergence__first_holding_reading, theater_ratio, 10, 0.22).
narrative_ontology:measurement_basis(ip_c_tr_t10, observed).
narrative_ontology:measurement(ip_c_tr_t20, ip_category_emergence__first_holding_reading, theater_ratio, 20, 0.25).
narrative_ontology:measurement_basis(ip_c_tr_t20, observed).
narrative_ontology:measurement(ip_c_tr_t30, ip_category_emergence__first_holding_reading, theater_ratio, 30, 0.28).
narrative_ontology:measurement_basis(ip_c_tr_t30, observed).
narrative_ontology:measurement(ip_c_tr_t40, ip_category_emergence__first_holding_reading, theater_ratio, 40, 0.3).
narrative_ontology:measurement_basis(ip_c_tr_t40, observed).
narrative_ontology:measurement(ip_c_tr_t50, ip_category_emergence__first_holding_reading, theater_ratio, 50, 0.32).
narrative_ontology:measurement_basis(ip_c_tr_t50, observed).
narrative_ontology:measurement(ip_c_tr_t60, ip_category_emergence__first_holding_reading, theater_ratio, 60, 0.34).
narrative_ontology:measurement_basis(ip_c_tr_t60, observed).
narrative_ontology:measurement(ip_c_tr_t64, ip_category_emergence__first_holding_reading, theater_ratio, 64, 0.35).
narrative_ontology:measurement_basis(ip_c_tr_t64, observed).

% Extraction over time
narrative_ontology:measurement(ip_c_be_t0, ip_category_emergence__first_holding_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(ip_c_be_t0, observed).
narrative_ontology:measurement(ip_c_be_t10, ip_category_emergence__first_holding_reading, base_extractiveness, 10, 0.5).
narrative_ontology:measurement_basis(ip_c_be_t10, observed).
narrative_ontology:measurement(ip_c_be_t20, ip_category_emergence__first_holding_reading, base_extractiveness, 20, 0.53).
narrative_ontology:measurement_basis(ip_c_be_t20, observed).
narrative_ontology:measurement(ip_c_be_t30, ip_category_emergence__first_holding_reading, base_extractiveness, 30, 0.55).
narrative_ontology:measurement_basis(ip_c_be_t30, observed).
narrative_ontology:measurement(ip_c_be_t40, ip_category_emergence__first_holding_reading, base_extractiveness, 40, 0.57).
narrative_ontology:measurement_basis(ip_c_be_t40, observed).
narrative_ontology:measurement(ip_c_be_t50, ip_category_emergence__first_holding_reading, base_extractiveness, 50, 0.59).
narrative_ontology:measurement_basis(ip_c_be_t50, observed).
narrative_ontology:measurement(ip_c_be_t60, ip_category_emergence__first_holding_reading, base_extractiveness, 60, 0.61).
narrative_ontology:measurement_basis(ip_c_be_t60, observed).
narrative_ontology:measurement(ip_c_be_t64, ip_category_emergence__first_holding_reading, base_extractiveness, 64, 0.6).
narrative_ontology:measurement_basis(ip_c_be_t64, observed).

% Suppression requirement over time
narrative_ontology:measurement(ip_c_su_t0, ip_category_emergence__first_holding_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement_basis(ip_c_su_t0, observed).
narrative_ontology:measurement(ip_c_su_t10, ip_category_emergence__first_holding_reading, suppression_requirement, 10, 0.42).
narrative_ontology:measurement_basis(ip_c_su_t10, observed).
narrative_ontology:measurement(ip_c_su_t20, ip_category_emergence__first_holding_reading, suppression_requirement, 20, 0.45).
narrative_ontology:measurement_basis(ip_c_su_t20, observed).
narrative_ontology:measurement(ip_c_su_t30, ip_category_emergence__first_holding_reading, suppression_requirement, 30, 0.47).
narrative_ontology:measurement_basis(ip_c_su_t30, observed).
narrative_ontology:measurement(ip_c_su_t40, ip_category_emergence__first_holding_reading, suppression_requirement, 40, 0.5).
narrative_ontology:measurement_basis(ip_c_su_t40, observed).
narrative_ontology:measurement(ip_c_su_t50, ip_category_emergence__first_holding_reading, suppression_requirement, 50, 0.53).
narrative_ontology:measurement_basis(ip_c_su_t50, observed).
narrative_ontology:measurement(ip_c_su_t60, ip_category_emergence__first_holding_reading, suppression_requirement, 60, 0.56).
narrative_ontology:measurement_basis(ip_c_su_t60, observed).
narrative_ontology:measurement(ip_c_su_t64, ip_category_emergence__first_holding_reading, suppression_requirement, 64, 0.55).
narrative_ontology:measurement_basis(ip_c_su_t64, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ip_category_emergence__first_holding_reading, resource_allocation).
narrative_ontology:affects_constraint(ip_category_emergence__first_holding_reading, ip_category_emergence__thinkability_reading).
narrative_ontology:affects_constraint(ip_category_emergence__first_holding_reading, ip_category_emergence__synchronic_diachronic_seam).
narrative_ontology:affects_constraint(ip_category_emergence__first_holding_reading, stationers_copy_monopoly_regime).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'the emergence of IP in 1710' conflates two structurally distinct claims and one meta-claim about them. This file carries the first-holding claim (membership shift in an occupied claimant set; epsilon authored for the post-1710 settlement as seen from that reading). The thinkability sibling carries the category-emergence claim (ownable expression became legally coherent in 1710; different epsilon, different pre-1710 baseline). The synchronic_diachronic_seam sibling carries the meta-claim that the first two are formally independent or temporally ill-framed. Upstream of all three sits the Stationer monopoly regime, which this reading treats as the prior occupant of the same claim-space and the thinkability reading treats as a categorically different object. All family members are linked via affects_constraints per the epsilon-invariance decomposition rule.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
