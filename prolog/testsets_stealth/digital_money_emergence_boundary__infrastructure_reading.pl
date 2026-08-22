% ============================================================================
% CONSTRAINT STORY: digital_money_emergence_boundary__infrastructure_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_digital_money_emergence_boundary__infrastructure_reading, []).

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
 *   constraint_id: digital_money_emergence_boundary__infrastructure_reading
 *   human_readable: Digital Money Emergence Boundary — Infrastructure Reading (Bank Rails, 1967–1977)
 *   domain: economic/technological/historical
 *
 * SUMMARY:
 *   Between 1967 and 1977 the banking system built the rails electronic money
 *   would move on — the first ATM (1967), ACH clearing (1972), SWIFT (1977).
 *   This story instantiates the infrastructure reading of the digital-money
 *   emergence kernel: money became digital when banks could move it
 *   electronically, whether or not consumers could hold it directly. The
 *   reading is a boundary claim with a genuine coordination function — it
 *   gives monetary statistics and financial history a dated, observable
 *   anchor — and an asymmetric beneficiary structure: the boundary credits
 *   the rail operators with digital money's origin and frames later
 *   instruments as additions to their system. Per the epsilon-invariance
 *   decomposition, the sibling readings (conceptualization_reading,
 *   consumer_holdings_reading) are separate constraints with their own
 *   epsilon, beneficiary structures, and classifications, linked through
 *   network.affects_constraints; this story authors only the rails-anchored
 *   arrangement and routes the rivalry to omega variables. The M4/M5-style
 *   aggregate collapse the delta names begins at this boundary: the reading's
 *   own statistical product starts dissolving the moment electronic bank
 *   deposits blur into new categories. KEY AGENTS (by structural
 *   relationship): - banking_infrastructure_operators: primary beneficiary
 *   (institutional/arbitrage) — ATM, ACH, and SWIFT operators credited with
 *   digital money's origin; own the rails any successor arrangement reuses -
 *   central_banks: agenda-setter (institutional/constrained) — administer the
 *   aggregate categories the boundary licenses and absorb the definitional
 *   burden its blurring creates - nonbank_emoney_issuers: primary payer
 *   (moderate/constrained) — e-money institutions and wallet issuers framed
 *   as late entrants to a system banks originated -
 *   alternative_periodization_scholars: payer (moderate/identity_locked) —
 *   historians and economists whose published periodizations recant if the
 *   boundary moves - bank_deposit_holders: near-symmetric participants
 *   (powerless/constrained) — use the rails' payment function, pay its fees,
 *   hold digital money only inside the bank perimeter the boundary defines -
 *   independent_monetary_economists: analytical observer — assess the
 *   boundary's statistical consequences without collecting from any placement
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(digital_money_emergence_boundary__infrastructure_reading, 0.48).
domain_priors:suppression_score(digital_money_emergence_boundary__infrastructure_reading, 0.42).
domain_priors:theater_ratio(digital_money_emergence_boundary__infrastructure_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(digital_money_emergence_boundary__infrastructure_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(digital_money_emergence_boundary__infrastructure_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(digital_money_emergence_boundary__infrastructure_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(digital_money_emergence_boundary__infrastructure_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(digital_money_emergence_boundary__infrastructure_reading, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(digital_money_emergence_boundary__infrastructure_reading, tangled_rope).
narrative_ontology:human_readable(digital_money_emergence_boundary__infrastructure_reading, "Digital Money Emergence Boundary — Infrastructure Reading (Bank Rails, 1967–1977)").
narrative_ontology:topic_domain(digital_money_emergence_boundary__infrastructure_reading, "economic/technological/historical").

domain_priors:requires_active_enforcement(digital_money_emergence_boundary__infrastructure_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(digital_money_emergence_boundary__infrastructure_reading, 'e53b883b-c696-4727-a942-ef220ffe80ba').
narrative_ontology:cs_kernel_codification('e53b883b-c696-4727-a942-ef220ffe80ba', distributed).
narrative_ontology:cs_authority_grounding('e53b883b-c696-4727-a942-ef220ffe80ba', practice).
narrative_ontology:cs_interpretation_layer_present('e53b883b-c696-4727-a942-ef220ffe80ba').
narrative_ontology:cs_reading_relation('e53b883b-c696-4727-a942-ef220ffe80ba', digital_money_emergence_boundary__conceptualization_reading, coexists_with).
narrative_ontology:cs_reading_relation('e53b883b-c696-4727-a942-ef220ffe80ba', digital_money_emergence_boundary__consumer_holdings_reading, influences).
narrative_ontology:cs_axiom('e53b883b-c696-4727-a942-ef220ffe80ba', foundational, transfer_infrastructure_constitutes_emergence).
narrative_ontology:cs_axiom_status(transfer_infrastructure_constitutes_emergence, holdable).
narrative_ontology:cs_axiom_grounding('e53b883b-c696-4727-a942-ef220ffe80ba', transfer_infrastructure_constitutes_emergence, conventional).
narrative_ontology:cs_axiom('e53b883b-c696-4727-a942-ef220ffe80ba', secondary, monetary_statistics_anchor_in_bank_operations).
narrative_ontology:cs_axiom_status(monetary_statistics_anchor_in_bank_operations, holdable).
narrative_ontology:cs_axiom_grounding('e53b883b-c696-4727-a942-ef220ffe80ba', monetary_statistics_anchor_in_bank_operations, instrumental).
narrative_ontology:cs_reference_frame('e53b883b-c696-4727-a942-ef220ffe80ba', operational_bank_rails_boundary).
narrative_ontology:cs_drift_state('e53b883b-c696-4727-a942-ef220ffe80ba', contemporary_cbdc_era, gap(codification_collapse, substantial, false)).
narrative_ontology:cs_created_at('e53b883b-c696-4727-a942-ef220ffe80ba', '').
narrative_ontology:cs_kernel_id(digital_money_emergence_boundary__infrastructure_reading, digital_money_emergence_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(digital_money_emergence_boundary__infrastructure_reading, banking_infrastructure_operators).
narrative_ontology:constraint_beneficiary(digital_money_emergence_boundary__infrastructure_reading, central_banks).
narrative_ontology:constraint_victim(digital_money_emergence_boundary__infrastructure_reading, nonbank_emoney_issuers).
narrative_ontology:constraint_victim(digital_money_emergence_boundary__infrastructure_reading, alternative_periodization_scholars).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(digital_money_emergence_boundary__infrastructure_reading, bank_deposit_holders).
narrative_ontology:constraint_victim(digital_money_emergence_boundary__infrastructure_reading, bank_deposit_holders).
narrative_ontology:constraint_vindicates(digital_money_emergence_boundary__infrastructure_reading, operationalist_definition_of_money).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operate the interbank transfer rails — ATM networks, ACH clearing, SWIFT messaging — collecting per-transaction fees and governance authority from them. The emergence boundary dates digital money's origin to the rails they built, placing them at the head of the digital-money narrative and making every later instrument read as an addition to their system. They co-author the technical standards that instantiate the boundary in practice. Exit is arbitrage-grade: they operate across jurisdictions and own the infrastructure any successor arrangement would have to reuse.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__infrastructure_reading, banking_infrastructure_operators, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(digital_money_emergence_boundary__infrastructure_reading, banking_infrastructure_operators, agenda_setter).

% Administer the monetary aggregates and payment oversight within which the boundary is canonized; the rails-anchored periodization centers bank deposits in their statistical categories and licenses their authority over digital-money definitions. They also carry a cost the boundary creates: as electronic bank deposits blur into new instrument categories, the aggregate framework anchored at this boundary demands continuous defensive redefinition. They cannot exit the statistical mandate, and re-drawing the boundary would mean rewriting their own category lineage against the banking establishment that benefits from it.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__infrastructure_reading, central_banks, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(digital_money_emergence_boundary__infrastructure_reading, central_banks, beneficiary).

% Issue digital instruments outside the bank perimeter — historical e-purse ventures, licensed e-money institutions, wallet and stablecoin issuers. The boundary frames their instruments as late arrivals to a system banks originated, and the regulatory regimes built on that frame treat them as exceptional entrants requiring special licensing rather than as participants in a shared category. Exit means abandoning issuance or selling into the bank perimeter they were framed as outside.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__infrastructure_reading, nonbank_emoney_issuers, payer,
    moderate, biographical, constrained, regional).

% Financial historians and monetary economists whose published work rides on rival boundaries — thinkability-centered or diffusion-centered periodizations. The rails-anchored canon occupies the textbooks, the anniversary commemorations, and the official histories; a scholar committed to a rival boundary carries a corpus that recants if the boundary moves. Exit would mean disowning their own published periodization, so contest persists as marginalization rather than conversion.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__infrastructure_reading, alternative_periodization_scholars, payer,
    moderate, biographical, identity_locked, regional).

% Hold and move bank deposits electronically through the rails — payroll, card settlement, online banking. They receive a working electronic payment system and pay for it in fees and in the subordination of their own experience: under this boundary, what they directly hold counts as digital money only insofar as it sits inside the bank perimeter the rails define. Their choice set is bounded by which rails exist; holding outside them means cash or newer, thinner alternatives.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__infrastructure_reading, bank_deposit_holders, beneficiary,
    powerless, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(digital_money_emergence_boundary__infrastructure_reading, bank_deposit_holders, payer).

% Assess aggregate definitions, payment-system economics, and the statistical consequences of the boundary from outside the benefiting institutions. They review whether the broad aggregate categories anchored at the rails boundary still measure anything coherent, and their findings feed aggregate reviews and central-bank digital currency design debates without collecting from any placement.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__infrastructure_reading, independent_monetary_economists, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(digital_money_emergence_boundary__infrastructure_reading, banking_infrastructure_operators).
narrative_ontology:fixing_cost_class(digital_money_emergence_boundary__infrastructure_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Gives central bankers, monetary statisticians, and financial historians a shared, institutionally observable answer to when money became digital — a periodization anchored in dated, physical infrastructure events (first ATM 1967, ACH 1972, SWIFT 1977) that statistics, regulatory drafting, and historical narrative can coordinate on without adjudicating contested theory or diffuse consumer practice.
% TRANSFER_FUNCTION: Moves narrative origin-authority and regulatory design priority toward the banking infrastructure operators — the boundary credits their rails as digital money's beginning — and moves non-bank instruments and their issuers into the position of later, exceptional entrants governed by regimes designed around the bank perimeter.
% ABSENT_VOICES: The consumer-holdings perspective — the people who actually hold and transact with digital money — had no seat when the boundary was canonized; the conversation ran among central banks, infrastructure cooperatives, and the academic establishment. The cryptographer lineage behind the conceptualization reading was likewise outside the banking-statistics conversation when the rails story hardened into canon. Both would re-date the boundary and reassign who counts as digital money's originators.
% DISAPPEARANCE_RATIONALE: If the rails-anchored boundary vanished overnight, monetary history would reorganize around one of the rival periodizations: statistical lineages would re-anchor, textbook narratives would re-date digital money's emergence, and the origin-credit now accruing to the rail operators would move to whichever seat the surviving boundary favors. Every named party is positioned relative to this line, which is why the world rearranges rather than stays put.
% FOUNDING_PROBLEM: Monetary statistics and financial history needed a defensible, observable line marking when money became digital: aggregate categories and regulatory perimeters required an anchor in dated institutional fact rather than in contested theory (when the concept became thinkable) or in diffuse practice (when consumers could directly hold it).
% FOUNDING_PROBLEM_CORROBORATION: The periodization problem's reality is corroborated from outside the benefiting parties: the cryptographer lineage behind the conceptualization reading and consumer-side payment historians both attest that the boundary question exists and is live, while disputing the rails answer. Independent aggregate-review literature attests that the statistical anchor is blurring. The specific placement at the rails is attested mainly by the institutions it credits — central-bank histories and the infrastructure cooperatives' own commemorations — with no fully disinterested corroboration of the placement itself.
narrative_ontology:disappearance_verdict(digital_money_emergence_boundary__infrastructure_reading, world_rearranges).
narrative_ontology:founding_problem_status(digital_money_emergence_boundary__infrastructure_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(digital_money_emergence_boundary__infrastructure_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(digital_money_emergence_boundary__infrastructure_reading, 'none', 1).
narrative_ontology:epsilon_provenance(digital_money_emergence_boundary__infrastructure_reading, 0.48, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(digital_money_emergence_boundary__infrastructure_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(digital_money_emergence_boundary__infrastructure_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(digital_money_emergence_boundary__infrastructure_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.48 at interval end): the boundary's principal product is origin-credit and regulatory design priority rather than direct pecuniary transfer, but that product concentrates on the rail operators while the definitional and compliance burdens fall on non-bank issuers. Suppression (0.42) is the enforcement machinery of canonization — textbook standardization, official histories, anniversary commemoration, regulatory drafting that presupposes the rails origin — not legal prohibition; rival readings remain publishable but marginal. Suppression is authored as a raw structural property and is NOT scaled by power or scope; extractiveness is what the engine scales by directionality and scope. Theater (0.28) is a growing performative layer — industry anniversaries and self-authored origin histories — over a statistical function that still operates but whose categories the boundary's own success is blurring. Accessibility_collapse is low-moderate (0.35): the rival readings remain fully articulable, which is why resistance (0.52) persists as live scholarly and regulatory contest rather than decayed grumbling. The claim is authored independently of these metrics: I claim tangled_rope because I judge both a genuine coordination function and asymmetric extraction to be structurally present; where the engine's per-seat computations diverge from that claim, the divergence is the measurement. The measurement series share one time grid (1967–2025 at eight points) with all three tracked metrics authored at every point. The extractiveness arc rises through the canonization era, peaks as the boundary does its heaviest gatekeeping around the e-money licensing regime (2000s), and eases slightly as open-banking access rules and the crypto-era challenge forced accommodation. The suppression_requirement series traces enforcement-capacity dynamics rather than extraction shift: canonization machinery built up from 1967 through the 2009 peak, then partially relaxed as accommodation set in.
 *
 * PERSPECTIVAL GAP:
 *   From the rail operators' seat the boundary is a settled fact of infrastructure history — they built the rails, the rails moved the money, the date follows the concrete. From the non-bank issuer seat the same line operates as gatekeeping: it dates them as latecomers and hands perimeter definition to the incumbents. Central banks occupy a split position — the boundary centers their categories (benefit) while its blurring hands them a permanent defensive redefinition workload (cost) — so the agenda-setter seat should compute as less captured than a pure-beneficiary reading would predict. The scholar seat experiences the boundary as career structure: the canon occupies the textbooks, and a rival periodization is a recantation, not a revision, which is why their exit is identity-locked rather than merely constrained. Issuers and scholars share a payer position but have little coalition history — one seat's grievance is regulatory, the other's epistemic — so coalition power among the moderately-powered payers remains latent rather than realized.
 *
 * DIRECTIONALITY LOGIC:
 *   Rail operators sit near the beneficiary end (low d): the boundary's gains accrue to them, their exit is arbitrage-grade, and they co-author the standards the boundary instantiates. Central banks are beneficiaries with a real cost tail: the derivation from their beneficiary declaration places them low-d, but the blurring burden the boundary creates keeps them from the full-subsidy end; no directionality override is authored because the override keying is by power atom and cannot separate them from the operators — the structural data carries the nuance instead. Non-bank issuers and rival-periodization scholars sit near the target end (high d): the issuers pay in regulatory position, the scholars in narrative standing, and the scholars' identity lock keeps them in the paying seat. Bank deposit holders sit near symmetric: a working payment system received, fees and perimeter subordination paid. Spatial scope is global for the rails and the deposit base — larger scope makes the boundary's verification harder and amplifies effective extraction modestly — while the issuers' and scholars' seats operate at regional scope.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification keeps two mislabelings apart. Reading the boundary as pure coordination (a rope) would erase the asymmetric beneficiary structure the delta itself declares — the origin-credit lands on the rails' owners, and the regulatory regimes built on the boundary treat non-bank instruments as exceptional entrants. Reading it as pure extraction (a snare) would erase the genuine coordination function: monetary statistics demonstrably need a dated, observable anchor, and this boundary supplies one that the rival readings supply less well. Tangled rope holds both. The R5 mismatch flag (dead founding problem + world_rearranges) does not fire because the founding problem is contested rather than dead — the arrangement is not yet a zombie mandate. The m4_m5_anchor_dissolution omega tracks the path by which it could become one: if the statistical anchor dissolves entirely while the canon persists unchallenged, the arrangement drifts toward inertial maintenance with no seat profiting enough to fix it and the theater share rising.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    boundary_placement_rivalry,
    'Is the emergence boundary at operational bank rails (this reading), at theoretical formalization and thinkability (conceptualization reading), or at consumer direct holdings (consumer_holdings reading)?',
    'The readings are rival periodizations held by different parties; resolution would require a shared criterion for ''emergence'' — an agreed operational test distinguishing constitutive from incidental events — or an archival standard both rival camps accept.',
    'Moving to the consumer_holdings boundary shifts the origin date to the 1990s–2000s, reassigns the beneficiary seat from rail operators to wallet and e-money issuers, and converts today''s payers into the originators; moving to the conceptualization boundary re-dates to the 1960s–1985 and centers the cryptographer lineage. Each move rewrites the extraction structure this story measures.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(boundary_placement_rivalry, conceptual, 'Committer structure: which of the three rival emergence boundaries the kernel''s answer turns on; this story instantiates the infrastructure reading only.').

omega_variable(
    rail_operator_periodization_capture,
    'Does the infrastructure reading dominate because it has superior evidential anchoring (dated, observable institutional facts) or because the institutions it credits control the statistical and historical apparatus that canonizes it?',
    'Compare the three readings'' fit against archival evidence assembled independent of the banking establishment — contemporary non-bank innovation records, regulatory drafting histories — and test whether the rails reading retains its position when the canonizing apparatus''s discretion is removed.',
    'If capture, the reading''s measured extraction understates a narrative-authority rent and the beneficiary seat''s enforcement role should weigh more heavily in classification; if genuine anchoring, the coordination function dominates and the boundary is closer to a defended convention than a captured one.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rail_operator_periodization_capture, empirical, 'Whether the boundary''s dominance reflects evidential superiority or beneficiary control of the canonization apparatus.').

omega_variable(
    m4_m5_anchor_dissolution,
    'Does the statistical blurring the boundary licenses — electronic bank deposits dissolving into new instrument categories — proceed until the rails anchor no longer supports any aggregate category, leaving the reading canonized but functionally inert?',
    'Track aggregate redefinition practice and central-bank digital currency category design: if official statistics abandon bank-deposit-anchored categories while the historical boundary persists unchallenged, the anchor has dissolved.',
    'If the anchor dissolves, the boundary persists as canonical history with a rising performative share — the arrangement drifts toward inertial maintenance, payer-seat extraction falls as the gatekeeping function empties, and the type trajectory bends toward a degraded form.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(m4_m5_anchor_dissolution, empirical, 'Whether the reading''s statistical anchor survives the instrument blurring it began.').

omega_variable(
    kernel_codification_framing,
    'Is the kernel an open historical question with no adjudicator (distributed codification, as authored here) or the de facto statistical practice itself, where the kernel is whatever the aggregate apparatus does (implicit codification)?',
    'Examine whether any party could issue a binding ruling on the emergence boundary: if no institution''s ruling would bind the rivals, the kernel is distributed; if central-bank statistical practice effectively settles it for all official purposes, the kernel is implicit in that practice.',
    'Under the implicit framing, this reading''s authority grounding shifts from practice-based adjudication toward the practice itself being the kernel, and the rivalry dynamics between readings change — the siblings become measurement disputes rather than rival commitments, altering foreclosure and influence edges.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_codification_framing, conceptual, 'CS-framing under-determination: how the kernel itself is codified, and what classification shifts if the implicit-practice framing is adopted instead.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(digital_money_emergence_boundary__infrastructure_reading, 1967, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(digi_tr_t1967, digital_money_emergence_boundary__infrastructure_reading, theater_ratio, 1967, 0.05).
narrative_ontology:measurement(digi_tr_t1977, digital_money_emergence_boundary__infrastructure_reading, theater_ratio, 1977, 0.08).
narrative_ontology:measurement(digi_tr_t1985, digital_money_emergence_boundary__infrastructure_reading, theater_ratio, 1985, 0.1).
narrative_ontology:measurement(digi_tr_t1995, digital_money_emergence_boundary__infrastructure_reading, theater_ratio, 1995, 0.15).
narrative_ontology:measurement(digi_tr_t2000, digital_money_emergence_boundary__infrastructure_reading, theater_ratio, 2000, 0.18).
narrative_ontology:measurement(digi_tr_t2009, digital_money_emergence_boundary__infrastructure_reading, theater_ratio, 2009, 0.22).
narrative_ontology:measurement(digi_tr_t2015, digital_money_emergence_boundary__infrastructure_reading, theater_ratio, 2015, 0.25).
narrative_ontology:measurement(digi_tr_t2025, digital_money_emergence_boundary__infrastructure_reading, theater_ratio, 2025, 0.28).

% Extraction over time
narrative_ontology:measurement(digi_be_t1967, digital_money_emergence_boundary__infrastructure_reading, base_extractiveness, 1967, 0.22).
narrative_ontology:measurement(digi_be_t1977, digital_money_emergence_boundary__infrastructure_reading, base_extractiveness, 1977, 0.28).
narrative_ontology:measurement(digi_be_t1985, digital_money_emergence_boundary__infrastructure_reading, base_extractiveness, 1985, 0.36).
narrative_ontology:measurement(digi_be_t1995, digital_money_emergence_boundary__infrastructure_reading, base_extractiveness, 1995, 0.44).
narrative_ontology:measurement(digi_be_t2000, digital_money_emergence_boundary__infrastructure_reading, base_extractiveness, 2000, 0.5).
narrative_ontology:measurement(digi_be_t2009, digital_money_emergence_boundary__infrastructure_reading, base_extractiveness, 2009, 0.54).
narrative_ontology:measurement(digi_be_t2015, digital_money_emergence_boundary__infrastructure_reading, base_extractiveness, 2015, 0.51).
narrative_ontology:measurement(digi_be_t2025, digital_money_emergence_boundary__infrastructure_reading, base_extractiveness, 2025, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(digi_su_t1967, digital_money_emergence_boundary__infrastructure_reading, suppression_requirement, 1967, 0.12).
narrative_ontology:measurement(digi_su_t1977, digital_money_emergence_boundary__infrastructure_reading, suppression_requirement, 1977, 0.18).
narrative_ontology:measurement(digi_su_t1985, digital_money_emergence_boundary__infrastructure_reading, suppression_requirement, 1985, 0.28).
narrative_ontology:measurement(digi_su_t1995, digital_money_emergence_boundary__infrastructure_reading, suppression_requirement, 1995, 0.36).
narrative_ontology:measurement(digi_su_t2000, digital_money_emergence_boundary__infrastructure_reading, suppression_requirement, 2000, 0.45).
narrative_ontology:measurement(digi_su_t2009, digital_money_emergence_boundary__infrastructure_reading, suppression_requirement, 2009, 0.5).
narrative_ontology:measurement(digi_su_t2015, digital_money_emergence_boundary__infrastructure_reading, suppression_requirement, 2015, 0.46).
narrative_ontology:measurement(digi_su_t2025, digital_money_emergence_boundary__infrastructure_reading, suppression_requirement, 2025, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(digital_money_emergence_boundary__infrastructure_reading, information_standard).
narrative_ontology:affects_constraint(digital_money_emergence_boundary__infrastructure_reading, digital_money_emergence_boundary__conceptualization_reading).
narrative_ontology:affects_constraint(digital_money_emergence_boundary__infrastructure_reading, digital_money_emergence_boundary__consumer_holdings_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'when did digital money emerge' decomposes into three structurally distinct boundary claims per the epsilon-invariance principle: conceptualization (thinkability/formalization), infrastructure (this story — operational bank transfer rails), and consumer holdings (direct consumer instruments). Each has its own epsilon, beneficiary structure, and classification; this reading's epsilon refers only to the rails-anchored arrangement. The upstream infrastructure reading influences the consumer-holdings sibling's operating environment because the regulatory regimes governing non-bank e-money were drafted on the bank-origin frame. Family members are linked through affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
