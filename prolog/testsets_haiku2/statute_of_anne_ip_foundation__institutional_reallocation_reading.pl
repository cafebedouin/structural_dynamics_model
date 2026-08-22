% ============================================================================
% CONSTRAINT STORY: statute_of_anne_ip_foundation__institutional_reallocation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_statute_of_anne_institutional_reallocation, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: statute_of_anne_ip_foundation__institutional_reallocation_reading
 *   human_readable: Statute of Anne: Institutional Reallocation of Printing Rights (1710)
 *   domain: legal/institutional/economic
 *
 * SUMMARY:
 *   The Statute of Anne (1710) is understood in THIS reading as an
 *   INSTITUTIONAL REALLOCATION rather than a conceptual innovation or an
 *   entangled event. The statute moved the right to authorize printing from
 *   the Stationers' Company (a guild monopoly holding since the 16th century)
 *   to individual authors, with the statutory expectation that publishers
 *   would acquire those rights through assignment. This reading focuses on
 *   WHO holds the institutional power to authorize printing, not on what
 *   copyright means or whether copyright was thereby created. The constraint
 *   is the reallocation itself: the changed occupancy of the institutional
 *   role. Authors moved from zero legal standing to initial right-holder;
 *   Stationers moved from monopolist to administrator of a competitive
 *   regime; publishers moved from guild-dependent to statute-enabled. This is
 *   distinct from a reading that emphasizes the conceptual emergence of
 *   copyright-as-limited-right, or a reading that treats the statute as an
 *   entangled event where institutional and conceptual change cannot be
 *   separated. Each reading is a different constraint on a single kernel (the
 *   statute's actual authority and effect).
 *
 * KEY AGENTS:
 *   - Stationers' Company: institutional monopolist holding the right to authorize printing (1662-1710); bear the extraction cost of losing monopoly status.
 *   - Authors: move from zero legal standing to initial right-holder (beneficiaries of reallocation); constrained ability to exercise the right due to capital requirements and market structure.
 *   - Publishers: enabled by the statute to operate outside the Stationers' monopoly; acquire rights via assignment; organized and have higher exit options than the Stationers.
 *   - Parliament and Crown: agenda-setter; enact and enforce the reallocation through statute and judicial interpretation.
 *   - Reading public: powerless seat; benefit from limited-term provision and eventual entry into public domain, but bear indirect cost during monopoly term.
 *   - Excluded competitive printers: would have benefited from the reallocation but had no formal voice in the statute's design.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(statute_of_anne_ip_foundation__institutional_reallocation_reading, 0.62).
domain_priors:suppression_score(statute_of_anne_ip_foundation__institutional_reallocation_reading, 0.48).
domain_priors:theater_ratio(statute_of_anne_ip_foundation__institutional_reallocation_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__institutional_reallocation_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__institutional_reallocation_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__institutional_reallocation_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__institutional_reallocation_reading, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__institutional_reallocation_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(statute_of_anne_ip_foundation__institutional_reallocation_reading, tangled_rope).
narrative_ontology:human_readable(statute_of_anne_ip_foundation__institutional_reallocation_reading, "Statute of Anne: Institutional Reallocation of Printing Rights (1710)").
narrative_ontology:topic_domain(statute_of_anne_ip_foundation__institutional_reallocation_reading, "legal/institutional/economic").

domain_priors:requires_active_enforcement(statute_of_anne_ip_foundation__institutional_reallocation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(statute_of_anne_ip_foundation__institutional_reallocation_reading, 'bfeae486-5f64-49bf-b392-d2964cdd54d9').
narrative_ontology:cs_kernel_codification('bfeae486-5f64-49bf-b392-d2964cdd54d9', formalized).
narrative_ontology:cs_authority_grounding('bfeae486-5f64-49bf-b392-d2964cdd54d9', extraction).
narrative_ontology:cs_interpretation_layer_present('bfeae486-5f64-49bf-b392-d2964cdd54d9').
narrative_ontology:cs_reading_relation('bfeae486-5f64-49bf-b392-d2964cdd54d9', statute_of_anne_ip_foundation__conceptual_emergence_reading, coexists_with).
narrative_ontology:cs_reading_relation('bfeae486-5f64-49bf-b392-d2964cdd54d9', statute_of_anne_ip_foundation__entangled_event_reading, influences).
narrative_ontology:cs_axiom('bfeae486-5f64-49bf-b392-d2964cdd54d9', foundational, institutional_seat_as_primary_unit).
narrative_ontology:cs_axiom_status(institutional_seat_as_primary_unit, holdable).
narrative_ontology:cs_axiom_grounding('bfeae486-5f64-49bf-b392-d2964cdd54d9', institutional_seat_as_primary_unit, conventional).
narrative_ontology:cs_axiom('bfeae486-5f64-49bf-b392-d2964cdd54d9', foundational, reallocation_not_creation).
narrative_ontology:cs_axiom_status(reallocation_not_creation, holdable).
narrative_ontology:cs_axiom_grounding('bfeae486-5f64-49bf-b392-d2964cdd54d9', reallocation_not_creation, empirically_contingent).
narrative_ontology:cs_reference_frame('bfeae486-5f64-49bf-b392-d2964cdd54d9', parliamentary_authorized_printing_regime).
narrative_ontology:cs_drift_state('bfeae486-5f64-49bf-b392-d2964cdd54d9', post_monopoly_market_stabilization_1725, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('bfeae486-5f64-49bf-b392-d2964cdd54d9', '').
narrative_ontology:cs_kernel_id(statute_of_anne_ip_foundation__institutional_reallocation_reading, statute_of_anne_ip_foundation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(statute_of_anne_ip_foundation__institutional_reallocation_reading, authors_as_initial_right_holders).
narrative_ontology:constraint_beneficiary(statute_of_anne_ip_foundation__institutional_reallocation_reading, publishers_via_assignment).
narrative_ontology:constraint_victim(statute_of_anne_ip_foundation__institutional_reallocation_reading, stationers_company_monopoly).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(statute_of_anne_ip_foundation__institutional_reallocation_reading, reading_public).
narrative_ontology:constraint_victim(statute_of_anne_ip_foundation__institutional_reallocation_reading, stationers_company).
narrative_ontology:constraint_victim(statute_of_anne_ip_foundation__institutional_reallocation_reading, reading_public).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Held the monopoly right to authorize printing in England for over a century. The statute directly reallocated this right, undermining their institutional basis. They bore the extraction cost as their monopoly revenue disappeared and their gate-keeping authority was transferred to authors/Parliament. They attempted to enforce the old rules and adapt to the new ones, but remained institutionally bound to a degrading power position. Trapped because their organizational identity was defined by the monopoly.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__institutional_reallocation_reading, stationers_company, payer,
    institutional, generational, trapped, national).
narrative_ontology:stakeholder_secondary_role(statute_of_anne_ip_foundation__institutional_reallocation_reading, stationers_company, agenda_setter).

% Gained formal legal standing to authorize printing and receive fees or assignment payments for the first time. This was a structural elevation in their institutional position. However, most exercised this right only through immediate assignment to publishers, meaning the primary beneficiary of the formal standing became the publisher rather than the author. Authors remained dependent on the publishing market and publisher willingness to negotiate terms. Identity-locked because being an author in the knowledge economy means working within statutory IP terms.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__institutional_reallocation_reading, authors_as_initial_right_holders, beneficiary,
    moderate, generational, identity_locked, national).

% Acquired the legal ability to operate outside the Stationers' monopoly by acquiring rights from authors. This removed the guild gatekeeping requirement and allowed competitive entry to publishing. They benefited from the reallocation because the statute legitimized their operation and created a legal market for rights transfer. They could enter or exit the market based on profitability, giving them higher exit options than the trapped Stationers.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__institutional_reallocation_reading, publishers_via_assignment, beneficiary,
    organized, biographical, mobile, national).

% Benefited from the statute's limited-term provision: after 14 years (renewable once if the author lived), works entered the public domain and could be printed freely. Also benefited from competitive pricing as publishers competed for market share outside the monopoly. They bore indirect cost through higher prices during the monopoly term and dependence on publishers' decisions about what to print. Constrained exit because they could only read what the market offered.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__institutional_reallocation_reading, reading_public, beneficiary,
    powerless, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(statute_of_anne_ip_foundation__institutional_reallocation_reading, reading_public, payer).

% Enacted and enforced the statute as a policy response to the monopoly problem. They centralized the authority to define and regulate printing rights in the state rather than the guild. They positioned the statute as solving a market failure while legitimating a new property form (limited-term author rights). They bear no direct cost and have analytical standing rather than being embedded in the constraint.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__institutional_reallocation_reading, parliament_and_crown, agenda_setter,
    institutional, generational, analytical, national).

% Were kept outside the Stationers' monopoly and had no formal legal standing to petition or negotiate before the statute. Their interests—cheap access to works, ability to print and sell without guild permission—aligned with the statute's effect. Once the statute passed, they became the competitive force that made the reallocation real and forced publishers to compete on price and title selection. Never formally included in the legislative negotiation.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__institutional_reallocation_reading, excluded_printers_outside_stationers, excluded,
    powerful, biographical, mobile, national).

% Courts, judges, and Crown lawyers interpreted the statute's meaning and enforced its terms. Their interpretations determined what 'author' meant in practice, whether assignments could be irrevocable, what 'printing' included (did it cover reprinting? resale?), and when terms ended. This interpretive role shaped how the institutional reallocation would operate—whether authors retained any power or whether they were fully emptied of rights through assignment. Analytical seat because they are external to the constraint's primary parties.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__institutional_reallocation_reading, legal_interpretation_authority, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(statute_of_anne_ip_foundation__institutional_reallocation_reading, publishers_via_assignment).
narrative_ontology:fixing_cost_class(statute_of_anne_ip_foundation__institutional_reallocation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves a coordination failure in the printing market: the Stationers' monopoly was creating scarcity (high prices, limited titles printed) by preventing entry. The statute reallocated the right to authorize printing from the guild monopoly to individual authors, enabling competitive publishing while preserving authors' incentive to write (they receive fee or assignment upfront). The coordination problem is: how do you separate authorization from monopolistic gatekeeping?
% TRANSFER_FUNCTION: Transfers the right to authorize and license printing from the Stationers' Company (as collective guild monopolist) to individual authors (as initial right-holders), and then typically to publishers (via assignment). The monetary flow changes: instead of monopoly rents to the Stationers, the revenue now flows to authors at the moment of assignment, and then to publishers (or authors) during the monopoly term. After the term expires, the work is free to print, and anyone can reproduce it without fee.
% ABSENT_VOICES: Printers outside the Stationers' Company were structurally excluded from the formal negotiation but their interests aligned with the statute. Readers—especially lower-income readers who suffered under high monopoly prices—were not represented in Parliament. Continental printing rivals who might have had interests in London's printing market were outside the scope entirely. Excluded-printers' absence from the negotiating table meant the statute did not fully address their concerns about competitive access (e.g., no prohibition on exclusive assignment contracts).
% DISAPPEARANCE_RATIONALE: If this institutional reallocation vanished (Stationers' monopoly fully restored, authors had zero legal standing to control printing), the publishing market would reorganize around guild licensing again. Publishers would need guild approval to operate. Competitive printing would collapse. Authors would have no legal standing to negotiate terms or receive fees—they would return to patronage or informal arrangements. The institutional space itself—who has standing to authorize printing—is what the statute allocates.
% FOUNDING_PROBLEM: The Stationers' Company monopoly on printing licenses was creating market failure: books were expensive, few titles were printed, printing innovation was suppressed by gate-keeping. Simultaneously, authors had no legal right to control their own works—the Stationers held the right, and authors were either employed by the guild or worked entirely outside the legal framework. The statute was built to solve both problems: restore competition while giving authors standing.
% FOUNDING_PROBLEM_CORROBORATION: Parliamentary petitions from excluded printers, merchants, and clergy (recorded in House of Commons Journals) attest the monopoly pricing problem. Letters and diaries from authors (Defoe, among others) attest their lack of standing and dependence on Stationers' patronage. Contemporary economic analysis (John Locke's economic writings, though not directly on printing) on monopoly effects and market efficiency. Modern economic historians (Ronan Deazley, Lionel Bently, external to the beneficiary party) affirm the monopoly problem existed and the statute was designed to address it.
narrative_ontology:disappearance_verdict(statute_of_anne_ip_foundation__institutional_reallocation_reading, world_rearranges).
narrative_ontology:founding_problem_status(statute_of_anne_ip_foundation__institutional_reallocation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(statute_of_anne_ip_foundation__institutional_reallocation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(statute_of_anne_ip_foundation__institutional_reallocation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(statute_of_anne_ip_foundation__institutional_reallocation_reading, 0.62, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(statute_of_anne_ip_foundation__institutional_reallocation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(statute_of_anne_ip_foundation__institutional_reallocation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(statute_of_anne_ip_foundation__institutional_reallocation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The measurement series show a sharp drop at 1710 (the statute's enactment) in both extractiveness and suppression. BEFORE the statute, the Stationers' monopoly exhibits high extractiveness (0.85 at 1662, sustained to 1700 at 0.82) and high suppression (0.78): entry is legally prevented, alternatives are nearly impossible (guild membership required), and alternatives collapse completely (accessibility_collapse is high ~0.85). AFTER the statute (1710), extractiveness drops to 0.58 (a 27-point fall) and suppression drops to 0.42 (a 36-point fall). Theater rises slightly (0.05 to 0.18) as the statute's enforcement machinery requires interpretive work—courts deciding what 'author' means, judges addressing disputes over assignment validity. The reallocation is REAL: the occupied set changed, monopoly extraction declined, and alternatives became available. The constraint itself (now defined as the POST-statute reallocation regime) shows moderate tangled-rope character: genuine coordination benefit (competitive publishing resumed, authors have standing), but also asymmetric extraction (publishers still control distribution, authors' rights are typically assigned away immediately, Stationers' institutional position is destroyed). The rise from 0.58 to 0.64 from 1710 to 1725 reflects publishers' growing ability to extract rents through assignment lock-in, as the initial reallocation is stabilized into new market power.
 *
 * PERSPECTIVAL GAP:
 *   The Stationers' institutional perspective and the Parliament-and-Crown perspective are nearly irreconcilable. The Stationers see a confiscation of their property right and the destruction of a rational institution they built to manage printing risk. Parliament sees a monopoly fix and a legitimate reallocation in the public interest. Neither seat can fully comprehend the other's constraint-reading because they have different reference frames: Stationers view the monopoly as proper and the statute as disruption; Parliament views the disruption as solving a market failure. The statute's enforcement depends on courts and administrators accepting the Parliament/Crown reading as authoritative, which means suppressing or ignoring the Stationers' reading. This perspectival gap is the reason enforcement is active (requires_active_enforcement: true) and theater is present (the statute's legitimacy must be continuously performed, interpreted, and re-asserted by legal authority).
 *
 * DIRECTIONALITY LOGIC:
 *   The Stationers' Company is the VICTIM of this reallocation—they lose their monopoly right, their institutional control, their revenue stream. Their directionality is high (d near 1.0, full target) because they bear the extraction cost and have no choice except to comply (trapped exit). Publishers are beneficiaries (they gain the ability to operate, acquire rights, compete) with mobile exit options—they could choose not to enter the publishing market, so d is lower (near 0.3-0.4, beneficiary end). Authors start as beneficiaries (they gain standing) but their exit is constrained (identity_locked: to be an author in the knowledge economy means operating within statutory terms, and identity is fused with the written work) and they typically immediately assign the right, so d shifts toward 0.5-0.6 (symmetric or slightly target-leaning, depending on the enforceability of assignment terms). The reading public is powerless and has constrained exit (they must choose what publishers print) so d is high (0.7-0.8) but they also benefit from the limited-term provision, so the beneficiary/victim mix is ambiguous. The reallocation's effectiveness depends on this directionality divergence: if the Stationers cannot prevent publishers from entering and authors cannot reclaim assigned rights, the extraction is locked in. If alternatives emerge (underground printing, author collectives organizing outside statutory terms), the extraction could be undermined.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (monopoly pricing, author lack of standing) is LIVE at the time of the statute (1710) and remains live afterward (1725-1750): the statute's operation continues to address the original problem—competitive pricing emerges, authors gain standing—so mandatrophy is NOT triggered. However, the measurement series shows a secondary problem emerging: by 1725, publishers have begun using assignment lock-in to create a NEW extraction mechanism (publishers acquire perpetual rights from authors through standard-form assignments, then resell to other publishers, creating a new concentration point). Theater rises as the statutory machinery is adapted to enforce this new asymmetry. This is NOT mandatrophy of the founding problem, but rather the constraint morphing from monopoly-prevention into a new form of extraction through the same institutional mechanism (assignment). The constraint remains tangled-rope but the coordination benefit is increasingly hollow—the extraction grows even as suppression falls (because the extraction now runs through contract law and market mechanisms rather than guild gatekeeping).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reallocation_vs_creation_boundary,
    'Did the statute reallocate an existing right (authorization to print), or did it CREATE the concept of copyright as a new form of limited property?',
    'Historical investigation of pre-1710 English law: did authors have ANY formal standing to control printing before the statute? If yes, reallocation. If no, creation. Secondary: did Continental legal traditions recognize author-rights differently, suggesting the statute chose a framing?',
    'If reallocation, this reading''s institutional-displacement frame is correct and the constraint is about seat-shifting. If creation, the conceptual-emergence reading better captures the structural change and this reading is incomplete—the constraint would need to model the emergence of a new normative category, not just the reallocation of an existing role.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reallocation_vs_creation_boundary, empirical, 'Whether the statute reallocated pre-existing legal standing or created new standing from nothing.').

omega_variable(
    stationers_institutional_death_vs_adaptation,
    'Was the Stationers'' Company a victim whose monopoly was confiscated, or an institution that adapted and continued under new terms?',
    'Historical record: did the Stationers continue to operate as a guild after 1710? Did they retain any enforcing role in the printing market? Did members transition to publishers? What happened to Stationers-held shares and revenue?',
    'If death: the Stationers are pure payers, victims with high d-values, and the constraint is a power transfer from guild to state/parliament. If adaptation: the Stationers may have retained some institutional role (even if diminished), and d-values shift—they are payers but not fully trapped. The degree of institutional death determines whether the constraint is better modeled as snare (permanent destruction) or tangled-rope (mixed coordination and extraction).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(stationers_institutional_death_vs_adaptation, empirical, 'Whether the Stationers'' Company ceased to exist or adapted to a new regulatory regime.').

omega_variable(
    author_benefit_vs_publisher_capture,
    'Did the statute benefit authors as end-holders of rights, or did it primarily enable publishers by creating a legally cleaner pathway to acquire rights from authors?',
    'Contractual record: what fraction of 18th-century author-publisher agreements show authors retaining rights post-monopoly term vs. assigning them in perpetuity? Did author incomes rise after the statute, controlling for genre and market size?',
    'If authors benefited: the constraint''s beneficiary is distributed across both authors and publishers, and the reallocation is genuinely empowering to multiple parties. If publishers captured: the constraint primarily transfers wealth from the Stationers to publishers while offering authors only formal standing without substantive control, making it more snare-like (appearing as liberation while operating as a more distributed extraction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(author_benefit_vs_publisher_capture, empirical, 'Whether the statute''s primary beneficiary was authors or the publishers who acquired their rights.').

omega_variable(
    institutional_reading_incoherence,
    'Can institutional change (reallocation of right-holding authority) be cleanly separated from conceptual change (the legal meaning of ''author,'' ''printing,'' ''right''), or is the act of reallocation inseparable from conceptual redefinition?',
    'Logical analysis: does the statute''s text redefine legal concepts, or does it preserve pre-existing concepts while moving them to new holders? Do courts interpret ''author'' the same before and after 1710? If courts read new meaning INTO the term after the statute, reallocation cannot be isolated from conceptual emergence.',
    'If separable: this reading''s institutional focus is valid and captures the core structural change. If inseparable: the constraint requires committing to the entangled_event_reading''s frame, and this institutional_reallocation_reading is modeling only a part of a larger, undivided phenomenon. The constraint type might need revision if the conceptual emergence is shown to be inseparable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_reading_incoherence, conceptual, 'Whether institutional reallocation and conceptual change in the statute are logically separable or inherently entangled.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(statute_of_anne_ip_foundation__institutional_reallocation_reading, 1662, 1750).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t1662, statute_of_anne_ip_foundation__institutional_reallocation_reading, theater_ratio, 1662, 0.05).
narrative_ontology:measurement(stat_tr_t1700, statute_of_anne_ip_foundation__institutional_reallocation_reading, theater_ratio, 1700, 0.06).
narrative_ontology:measurement(stat_tr_t1710, statute_of_anne_ip_foundation__institutional_reallocation_reading, theater_ratio, 1710, 0.18).
narrative_ontology:measurement(stat_tr_t1725, statute_of_anne_ip_foundation__institutional_reallocation_reading, theater_ratio, 1725, 0.22).
narrative_ontology:measurement(stat_tr_t1750, statute_of_anne_ip_foundation__institutional_reallocation_reading, theater_ratio, 1750, 0.22).

% Extraction over time
narrative_ontology:measurement(stat_be_t1662, statute_of_anne_ip_foundation__institutional_reallocation_reading, base_extractiveness, 1662, 0.85).
narrative_ontology:measurement(stat_be_t1700, statute_of_anne_ip_foundation__institutional_reallocation_reading, base_extractiveness, 1700, 0.82).
narrative_ontology:measurement(stat_be_t1710, statute_of_anne_ip_foundation__institutional_reallocation_reading, base_extractiveness, 1710, 0.58).
narrative_ontology:measurement(stat_be_t1725, statute_of_anne_ip_foundation__institutional_reallocation_reading, base_extractiveness, 1725, 0.64).
narrative_ontology:measurement(stat_be_t1750, statute_of_anne_ip_foundation__institutional_reallocation_reading, base_extractiveness, 1750, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t1662, statute_of_anne_ip_foundation__institutional_reallocation_reading, suppression_requirement, 1662, 0.78).
narrative_ontology:measurement(stat_su_t1700, statute_of_anne_ip_foundation__institutional_reallocation_reading, suppression_requirement, 1700, 0.75).
narrative_ontology:measurement(stat_su_t1710, statute_of_anne_ip_foundation__institutional_reallocation_reading, suppression_requirement, 1710, 0.42).
narrative_ontology:measurement(stat_su_t1725, statute_of_anne_ip_foundation__institutional_reallocation_reading, suppression_requirement, 1725, 0.49).
narrative_ontology:measurement(stat_su_t1750, statute_of_anne_ip_foundation__institutional_reallocation_reading, suppression_requirement, 1750, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(statute_of_anne_ip_foundation__institutional_reallocation_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(statute_of_anne_ip_foundation__institutional_reallocation_reading, 0.12).
narrative_ontology:affects_constraint(statute_of_anne_ip_foundation__institutional_reallocation_reading, statute_of_anne_ip_foundation__conceptual_emergence_reading).
narrative_ontology:affects_constraint(statute_of_anne_ip_foundation__institutional_reallocation_reading, statute_of_anne_ip_foundation__entangled_event_reading).

% DUAL FORMULATION NOTE:
% This constraint models the statute as institutional reallocation (WHO holds the printing right changes). The sibling readings model the statute as conceptual emergence (WHAT KIND of right is created) and as entangled event (institutional and conceptual change cannot be separated). All three readings share the same kernel (the statute itself) but decompose it according to different interpretive frames. Institutional_reallocation_reading emphasizes the seat-shifting; conceptual_emergence_reading emphasizes the normative innovation; entangled_event_reading argues they are one phenomenon. The ε values and beneficiary/victim structures differ across readings because each reading identifies a different extraction mechanism: this reading sees extraction in the Stationers' displacement; the conceptual_emergence reading would see extraction in the 'limited-term' enclosure (making learning/knowledge temporarily scarce); the entangled_event reading would argue extraction is intrinsic to the simultaneous institutional and conceptual shift itself.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(statute_of_anne_ip_foundation__institutional_reallocation_reading, powerful, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
