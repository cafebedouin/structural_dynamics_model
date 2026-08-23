% ============================================================================
% CONSTRAINT STORY: digital_money_origin__first_held_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_digital_money_origin__first_held_reading, []).

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
 *   constraint_id: digital_money_origin__first_held_reading
 *   human_readable: Digital Money Origin — First Practical Holding Reading
 *   domain: monetary_history/technology_studies/institutional_economics
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the kernel 'when did digital money
 *   originate': the claim that digital money emerged when individuals first
 *   held non-physical monetary instruments as practical stores of value — an
 *   adoption-anchored date, later than the thinkability reading and earlier
 *   than the regulatory-recognition reading. The constraint this claim
 *   constitutes operates in two registers at once: it coordinates (a
 *   checkable date anchor for datasets, textbooks, and policy baselines) and
 *   it distributes (narrative ownership to early adopters and their sponsors;
 *   structural invisibility to those without banking or network access, whose
 *   exclusion the adoption record recodes as lateness). Per the
 *   epsilon-invariance principle this is written as a standalone constraint
 *   with a single stable epsilon over the standing arrangement under contest
 *   — the adoption-defined monetary order and its definitional regime —
 *   assessed by this reading's own lights, which expressly count
 *   implementation barriers and network effects as part of the structure. KEY
 *   AGENTS (by structural relationship): - retail_banks_and_card_networks:
 *   Agenda setter ([institutional]/[arbitrage]) — administers the products,
 *   the fee schedule, and the commemorative narrative -
 *   banked_adopters_of_electronic_balances: Primary beneficiary
 *   ([moderate]/[constrained]) — collects access gains, bears fees -
 *   unbanked_and_offgrid_communities: Primary target ([powerless]/[trapped])
 *   — bears the exclusion the frame renders as lag -
 *   monetary_authorities_statistical_staff: Excluded rival constituency
 *   ([institutional]/[analytical]) - payments_conceptual_theorists: Excluded
 *   rival constituency ([organized]/[constrained]) -
 *   monetary_history_analytical_community: Analytical observer
 *   ([analytical]/[analytical]) — sees the full structure
 *
 * KEY AGENTS:
 *   - retail_banks_and_card_networks: agenda setter (institutional/arbitrage) — issues the instruments, sets fees, curates the origin narrative
 *   - banked_adopters_of_electronic_balances: primary beneficiary (moderate/constrained) — gains access and convenience, pays account fees and passed-through interchange
 *   - unbanked_and_offgrid_communities: primary target (powerless/trapped) — excluded from the rail, charged corridor premiums, recoded as latecomers
 *   - monetary_authorities_statistical_staff: excluded rival constituency (institutional/analytical) — would date the origin at formal incorporation
 *   - payments_conceptual_theorists: excluded rival constituency (organized/constrained) — would date the origin at technical conceivability
 *   - monetary_history_analytical_community: analytical observer (analytical/analytical) — adjudicates the evidentiary contest
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(digital_money_origin__first_held_reading, 0.58).
domain_priors:suppression_score(digital_money_origin__first_held_reading, 0.38).
domain_priors:theater_ratio(digital_money_origin__first_held_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(digital_money_origin__first_held_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(digital_money_origin__first_held_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(digital_money_origin__first_held_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(digital_money_origin__first_held_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(digital_money_origin__first_held_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(digital_money_origin__first_held_reading, tangled_rope).
narrative_ontology:human_readable(digital_money_origin__first_held_reading, "Digital Money Origin — First Practical Holding Reading").
narrative_ontology:topic_domain(digital_money_origin__first_held_reading, "monetary_history/technology_studies/institutional_economics").

domain_priors:requires_active_enforcement(digital_money_origin__first_held_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(digital_money_origin__first_held_reading, '47d50889-44c6-4316-bf5d-c20b46800970').
narrative_ontology:cs_kernel_codification('47d50889-44c6-4316-bf5d-c20b46800970', distributed).
narrative_ontology:cs_authority_grounding('47d50889-44c6-4316-bf5d-c20b46800970', distributed).
narrative_ontology:cs_reading_relation('47d50889-44c6-4316-bf5d-c20b46800970', digital_money_origin__became_thinkable_reading, coexists_with).
narrative_ontology:cs_reading_relation('47d50889-44c6-4316-bf5d-c20b46800970', digital_money_origin__regulatory_recognition_reading, influences).
narrative_ontology:cs_axiom('47d50889-44c6-4316-bf5d-c20b46800970', foundational, practical_holding_constitutes_emergence).
narrative_ontology:cs_axiom_status(practical_holding_constitutes_emergence, holdable).
narrative_ontology:cs_axiom_grounding('47d50889-44c6-4316-bf5d-c20b46800970', practical_holding_constitutes_emergence, empirically_contingent).
narrative_ontology:cs_axiom('47d50889-44c6-4316-bf5d-c20b46800970', secondary, implementation_barriers_define_membership).
narrative_ontology:cs_axiom_status(implementation_barriers_define_membership, holdable).
narrative_ontology:cs_axiom_grounding('47d50889-44c6-4316-bf5d-c20b46800970', implementation_barriers_define_membership, empirically_contingent).
narrative_ontology:cs_reference_frame('47d50889-44c6-4316-bf5d-c20b46800970', practice_anchored_emergence_baseline).
narrative_ontology:cs_drift_state('47d50889-44c6-4316-bf5d-c20b46800970', contemporary_mobile_money_and_custodial_era, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('47d50889-44c6-4316-bf5d-c20b46800970', '').
narrative_ontology:cs_kernel_id(digital_money_origin__first_held_reading, digital_money_origin).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(digital_money_origin__first_held_reading, banked_adopters_of_electronic_balances).
narrative_ontology:constraint_beneficiary(digital_money_origin__first_held_reading, retail_banks_and_card_networks).
narrative_ontology:constraint_victim(digital_money_origin__first_held_reading, unbanked_and_offgrid_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(digital_money_origin__first_held_reading, banked_adopters_of_electronic_balances).
narrative_ontology:constraint_vindicates(digital_money_origin__first_held_reading, practice_based_origin_doctrine).
narrative_ontology:constraint_vindicates(digital_money_origin__first_held_reading, network_effects_as_entry_barrier_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Issue the card, account, and float products through which households first came to hold value electronically; set the account fees and interchange schedules attached to those products; and fund the commemorative histories, anniversary exhibitions, and corporate archives through which the public memory of electronic money's beginning is curated. Every household converted onto a fee-bearing electronic balance widens their revenue base, and their command of branding and sponsorship lets them retell their own past at will.
narrative_ontology:constraint_stakeholder(digital_money_origin__first_held_reading, retail_banks_and_card_networks, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(digital_money_origin__first_held_reading, retail_banks_and_card_networks, beneficiary).

% Households in well-banked economies who moved wages and savings onto electronic rails during the adoption decades. They gained round-the-clock balance access, direct-deposit payroll, and near-universal card acceptance. They also pay monthly account fees, overdraft charges, and merchant interchange passed through into prices. Reverting to a cash-only life remains physically possible but grows costlier each year as branches close and employers digitize payroll.
narrative_ontology:constraint_stakeholder(digital_money_origin__first_held_reading, banked_adopters_of_electronic_balances, beneficiary,
    moderate, biographical, constrained, continental).
narrative_ontology:stakeholder_secondary_role(digital_money_origin__first_held_reading, banked_adopters_of_electronic_balances, payer).

% Adults without a transaction account or reliable network access — over a billion people today, concentrated in the Global South, rural districts, and low-income urban neighborhoods. For them the electronic-money era arrived as exclusion: remittance corridors charge double-digit fees precisely because senders and receivers sit outside the electronic rail; identification and minimum-balance rules screen them out at the door; branch closures remove the physical fallback. In histories written from the adoption record they appear, when they appear at all, as prospective customers rather than as people paying the price of the system's shape. Dispersed across jurisdictions with no coordinating body, their numbers translate poorly into leverage.
narrative_ontology:constraint_stakeholder(digital_money_origin__first_held_reading, unbanked_and_offgrid_communities, payer,
    powerless, generational, trapped, global).

% Central-bank statisticians and payment-system regulators who compile monetary aggregates and supervise payment markets. Their institutional account dates digital money's arrival to formal incorporation into aggregates and legal frameworks, not to street-level adoption. They publish much of the underlying series the adoption-centered narrative quietly relies upon, but they do not control the commemorative, museum, and textbook channels where the origin story is actually set.
narrative_ontology:constraint_stakeholder(digital_money_origin__first_held_reading, monetary_authorities_statistical_staff, excluded,
    institutional, generational, analytical, national).

% Cybernetics-era theorists, science-fiction writers, futurists, and the historians who study them — the lineage that imagined electronic money decades before anyone held it. Their constituency argues that emergence belongs to the moment the idea became technically and institutionally conceivable. They hold archives and seminar rooms but little purchase on the dataset-driven present of monetary history, where their preferred dating reads as antiquarian.
narrative_ontology:constraint_stakeholder(digital_money_origin__first_held_reading, payments_conceptual_theorists, excluded,
    organized, biographical, constrained, global).

% Academic historians, economic sociologists, and technology-studies scholars who weigh the rival datings against archival and statistical evidence. They track which criterion survives contact with new sources — EFT rollout records, card-network ledgers, mobile-money registries — and publish the periodic corrections that reset the debate. Several built careers on the adoption-centered canon and would bear real costs if the frame broke, a professional attachment distinct from their evidentiary judgments.
narrative_ontology:constraint_stakeholder(digital_money_origin__first_held_reading, monetary_history_analytical_community, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(digital_money_origin__first_held_reading, retail_banks_and_card_networks).
narrative_ontology:fixing_cost_class(digital_money_origin__first_held_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Fixes a shared, evidence-checkable date convention for when money became non-physical in practice, letting monetary-history datasets, cross-country payment-system comparisons, and policy baselines ('pre-digital' versus 'digital' eras) refer to the same anchor instead of relitigating the boundary study by study.
% TRANSFER_FUNCTION: Moves narrative ownership of digital money's history toward early adopters and their institutional sponsors; moves policy attention and connection subsidies toward populations already adjacent to the rail; and takes visibility from unconnected populations, whose exclusion the adoption record recodes as mere lateness.
% ABSENT_VOICES: Monetary statisticians would anchor the origin at formal incorporation into aggregates and would object to a date that predates their own series; conceptual theorists would object that practical holding is a derivative event downstream of thinkability. Most consequentially, unbanked communities themselves are absent from every venue where the origin story is set — they would testify that for them the digital era began as exclusion (priced-out remittance corridors, closed branches, screening rules), not as arrival. They are outside the commemorative commissions, industry-funded archives, and mainstream textbooks that fix the canon.
% DISAPPEARANCE_RATIONALE: If the first-practical-holding convention vanished overnight, the periodization anchor underpinning monetary-history datasets and payment-system comparisons would dissolve, textbook narratives would need rewriting, the banks' commemorative investment would lose its reference point, and the three-way contest over the origin criterion would reopen with no settled empirical baseline — arrangements visibly depend on it.
% FOUNDING_PROBLEM: Mid-twentieth-century monetary economics had no defensible empirical marker separating the era of physical cash from an era in which ordinary individuals held value electronically; researchers and policymakers needed a criterion that could be checked against records rather than asserted.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set: central-bank research and statistics departments — the constituency of a rival reading — continuously rework aggregate definitions and publication series precisely because the dating problem remains unsolved, and peer-reviewed monetary historiography independent of industry sponsorship treats the marker question as open. Neither source accepts this reading's specific criterion, which strengthens rather than weakens the attestation that the underlying problem is real and current.
narrative_ontology:disappearance_verdict(digital_money_origin__first_held_reading, world_rearranges).
narrative_ontology:founding_problem_status(digital_money_origin__first_held_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(digital_money_origin__first_held_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(digital_money_origin__first_held_reading, 'none', 1).
narrative_ontology:epsilon_provenance(digital_money_origin__first_held_reading, 0.58, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(digital_money_origin__first_held_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(digital_money_origin__first_held_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(digital_money_origin__first_held_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored at 0.58 because the reading's own constraint set contains the mechanism of asymmetric incidence: network effects raise the value of membership faster than access expands, so early holders compound gains while the disconnected face a rising exclusion premium (corridor fees, screening rules, shrinking physical fallback). Suppression is 0.38 — mostly structural (infrastructure absence, KYC and minimum-balance screens, branch consolidation; roughly four-fifths of the total), with a smaller internalized component (laggard self-labeling among the excluded, roughly one-fifth). Accessibility_collapse is deliberately low (0.30): the rival origin criteria remain fully alive — two sibling readings are standing alternatives — so understanding this constraint does not collapse the option space. Resistance is correspondingly high (0.60): the definitional contest is the live scholarly fight. Theater is 0.25 and rising slowly: anniversary exhibitions and sponsored histories are real expenditure, but the date anchor itself does genuine coordination work. The measurement series run on one shared grid (t=0,8,16,24,32,40) with every tracked metric authored at every point; the rising extractiveness trajectory tracks deepening network effects and fee layering, the rising suppression_requirement tracks the maturing enforcement picture — consolidation of the commemorative apparatus plus post-2010 hardening of access gating — and the theater trajectory tracks the growth of the commemoration economy. Coalition note: although the unbanked outnumber every other seat, they are dispersed across jurisdictions with no coordinating body, so their coalition capacity is low and their powerlessness is durable rather than contingent.
 *
 * PERSPECTIVAL GAP:
 *   The seats should classify differently. From the agenda-setter seat (banks and networks), the arrangement is an order they built and curate: a genuine service history with a proud origin story — coordination dominant. From the payer seat (unbanked and off-grid communities), the same structure is a gated system that prices their participation and writes them out of the founding narrative — extraction dominant. From the beneficiary seat (banked adopters), it is mostly a good deal they pay modestly for — near symmetry. The excluded seats diverge hardest: the statistical staff would call the constraint a misdating that flatters practitioners, and the theorists would call it a premature materialization of an older idea. The engine computes these per-seat classifications from the structural data; this story's claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary/victim declarations drive the derivation. Retail banks and card networks sit nearest the beneficiary pole: they collect fee and interchange revenue and control the narrative, with arbitrage-grade exit from any framing they dislike. Banked adopters sit low but not at zero — their secondary payer position (account fees, passed-through interchange) pulls them partway toward symmetric, and their constrained-but-real cash exit keeps d above the sponsor seat. Unbanked and off-grid communities sit nearest the full-target pole: they bear the exclusion costs, are trapped by infrastructure and documentation requirements, and hold no lever on the frame that defines them as late. The two excluded constituencies and the analytical observer carry commentary-grade positions only; they are not fed into the directionality arithmetic as collectors or payers. No directionality overrides are declared because the beneficiary/victim-plus-exit derivation reproduces these relationships without correction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — a records-checkable marker for money's dematerialization — is still live: central banks keep reworking aggregate definitions and historians keep reoperationalizing 'holding' as new instrument forms appear, so there is no dead mandate being theatrically maintained and no scaffold sunset to declare. The tangled_rope classification is what prevents mislabeling in both directions: calling this a rope would hide the access-based incidence (the excluded would vanish into 'everyone eventually connects'), while calling it a snare would erase the genuine coordination function — datasets, policy baselines, and cross-country comparisons really do hang off this date anchor, and no identifiable party would be liberated by its collapse into definitional chaos. The theater_ratio series is kept honest precisely so that commemorative accretion is visible as symptom, not mistaken for the constraint's substance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_structural_delta,
    'This constraint is the first_held_reading of kernel digital_money_origin; how would the beneficiary/victim structure and the classification change if a sibling reading were adopted in its place?',
    'Comparative read of the three sibling stories (digital_money_origin__became_thinkable_reading, digital_money_origin__regulatory_recognition_reading) against this one, tracing which seats survive a criterion change, which reverse polarity, and which dissolve entirely.',
    'Under the thinkability reading there are no holders yet, so the access-victim structure dissolves and measured extraction collapses toward negligible; under the regulatory-recognition reading the beneficiary seat migrates to state statistical capacity and the victim seat to informal-economy actors the aggregates miss. The classification of THIS file is conditioned on the first-held criterion and is not portable across readings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_structural_delta, conceptual, 'Committer structure: this story is one of three readings of the digital-money-origin kernel; the criterion choice determines the entire beneficiary/victim geometry.').

omega_variable(
    holding_definition_boundary,
    'What exactly counts as an individual ''holding'' a non-physical monetary instrument — a bank-recorded account balance, a telco-floated mobile-wallet float, a custodial exchange balance — and does the origin date move with the operationalization?',
    'Archival operationalization studies of what contemporaries treated as held value (ATM-era statements, e-money float contracts, mobile-money registry rules), combined with survey evidence on holders'' own understanding of possession versus custody.',
    'A thin holding criterion (any recorded electronic claim) pulls the origin date earlier and shrinks the historically excluded population; a strict custody-with-control criterion delays the date and enlarges the victim set. The epsilon and the victim geometry of this story are indexed to whichever boundary is adopted.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(holding_definition_boundary, conceptual, 'Boundary ambiguity of the holding criterion that fixes this reading''s date and its victim set.').

omega_variable(
    access_exclusion_causality,
    'Is the harm borne by unconnected populations caused by the network-effect and infrastructure gating this reading builds into its constraint set, or merely correlated with pre-existing poverty that any monetary arrangement would inherit?',
    'Natural-experiment comparison across regions where electronic-payment rails reached previously unbanked areas at comparable income levels: if exclusion outcomes track rail topology and screening rules rather than income alone, the gating is causal.',
    'If gating is causal, the asymmetric-extraction component of this story is confirmed and the tangled_rope reading stands; if the harm is purely correlational inheritance, extraction collapses toward ordinary coordination cost and the constraint trends toward rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(access_exclusion_causality, empirical, 'Whether access-based victimhood is attributable to the arrangement itself or to background poverty it merely overlays.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(digital_money_origin__first_held_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(digi_tr_t0, digital_money_origin__first_held_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(digi_tr_t0, observed).
narrative_ontology:measurement(digi_tr_t8, digital_money_origin__first_held_reading, theater_ratio, 8, 0.15).
narrative_ontology:measurement_basis(digi_tr_t8, observed).
narrative_ontology:measurement(digi_tr_t16, digital_money_origin__first_held_reading, theater_ratio, 16, 0.18).
narrative_ontology:measurement_basis(digi_tr_t16, observed).
narrative_ontology:measurement(digi_tr_t24, digital_money_origin__first_held_reading, theater_ratio, 24, 0.21).
narrative_ontology:measurement_basis(digi_tr_t24, observed).
narrative_ontology:measurement(digi_tr_t32, digital_money_origin__first_held_reading, theater_ratio, 32, 0.23).
narrative_ontology:measurement_basis(digi_tr_t32, observed).
narrative_ontology:measurement(digi_tr_t40, digital_money_origin__first_held_reading, theater_ratio, 40, 0.25).
narrative_ontology:measurement_basis(digi_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(digi_be_t0, digital_money_origin__first_held_reading, base_extractiveness, 0, 0.44).
narrative_ontology:measurement_basis(digi_be_t0, observed).
narrative_ontology:measurement(digi_be_t8, digital_money_origin__first_held_reading, base_extractiveness, 8, 0.48).
narrative_ontology:measurement_basis(digi_be_t8, observed).
narrative_ontology:measurement(digi_be_t16, digital_money_origin__first_held_reading, base_extractiveness, 16, 0.51).
narrative_ontology:measurement_basis(digi_be_t16, observed).
narrative_ontology:measurement(digi_be_t24, digital_money_origin__first_held_reading, base_extractiveness, 24, 0.54).
narrative_ontology:measurement_basis(digi_be_t24, observed).
narrative_ontology:measurement(digi_be_t32, digital_money_origin__first_held_reading, base_extractiveness, 32, 0.56).
narrative_ontology:measurement_basis(digi_be_t32, observed).
narrative_ontology:measurement(digi_be_t40, digital_money_origin__first_held_reading, base_extractiveness, 40, 0.58).
narrative_ontology:measurement_basis(digi_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(digi_su_t0, digital_money_origin__first_held_reading, suppression_requirement, 0, 0.28).
narrative_ontology:measurement_basis(digi_su_t0, observed).
narrative_ontology:measurement(digi_su_t8, digital_money_origin__first_held_reading, suppression_requirement, 8, 0.31).
narrative_ontology:measurement_basis(digi_su_t8, observed).
narrative_ontology:measurement(digi_su_t16, digital_money_origin__first_held_reading, suppression_requirement, 16, 0.33).
narrative_ontology:measurement_basis(digi_su_t16, observed).
narrative_ontology:measurement(digi_su_t24, digital_money_origin__first_held_reading, suppression_requirement, 24, 0.35).
narrative_ontology:measurement_basis(digi_su_t24, observed).
narrative_ontology:measurement(digi_su_t32, digital_money_origin__first_held_reading, suppression_requirement, 32, 0.37).
narrative_ontology:measurement_basis(digi_su_t32, observed).
narrative_ontology:measurement(digi_su_t40, digital_money_origin__first_held_reading, suppression_requirement, 40, 0.38).
narrative_ontology:measurement_basis(digi_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(digital_money_origin__first_held_reading, information_standard).
narrative_ontology:affects_constraint(digital_money_origin__first_held_reading, digital_money_origin__became_thinkable_reading).
narrative_ontology:affects_constraint(digital_money_origin__first_held_reading, digital_money_origin__regulatory_recognition_reading).

% DUAL FORMULATION NOTE:
% The colloquial question 'when did digital money begin?' decomposes, per the epsilon-invariance principle, into three structurally distinct constraints — one per origin criterion — because measuring emergence by conceivability, by practical holding, or by formal recognition yields different epsilons, different beneficiary/victim geometries, and different failure modes. Family ordering: became_thinkable_reading is upstream of this file (conception precedes and enables implementation; its proponents cite thinkability as a precondition for adoption), and this file is upstream of regulatory_recognition_reading (recognition presupposes a recognized practice; the statistical series regulators incorporate come into existence only after holding occurs). Each member carries edges to the other members so contamination and consistency checks propagate across the family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
