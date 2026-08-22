% ============================================================================
% CONSTRAINT STORY: electronic_money_emergence__first_held_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_electronic_money_emergence__first_held_reading, []).

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
 *   constraint_id: electronic_money_emergence__first_held_reading
 *   human_readable: Electronic Money Emergence via First Institutional Hold
 *   domain: economic/monetary/technology
 *
 * SUMMARY:
 *   This story instantiates the 'first_held_reading' of the
 *   electronic_money_emergence kernel: digital money emerged when the first
 *   institutional bearer (typically a central bank or regulated commercial
 *   bank) held dematerialized currency in a form legally and technically
 *   distinguishable from physical notes. The reading treats emergence as a
 *   discrete institutional event marked by observable custody transfer and
 *   regulatory codification, not a gradual social shift or conceptual
 *   possibility. Competing readings (became_thinkable_reading,
 *   m4_m5_collapse_reading) locate the threshold at social thinkability or
 *   statistical categorization respectively. This reading anchors emergence
 *   to institutional practice and legal form: when the first bank recognizes
 *   and holds electronic deposits as a distinct asset class, electronic money
 *   exists ontologically. The kernel contest is about what moment constitutes
 *   'real' emergence; this reading settles on institutional custody as the
 *   marker.
 *
 * KEY AGENTS:
 *   - central_banking_authority: Sets regulatory definitions of money supply categories (M1/M2/M3/M4/M5); determines what counts as 'dematerialized currency' in official accounting. Institutional power to define the category retroactively.
 *   - regulated_commercial_banks: First institutional bearers of dematerialized currency; hold customer deposits in electronic form and manage transfers via ledger entries or electronic networks. Operate within the regulatory frame the central bank establishes.
 *   - payment_network_operators: Facilitate the custody and transfer of electronic balances (SWIFT, ACH, real-time gross settlement systems). Their technical infrastructure enables the distinction between physical and dematerialized forms.
 *   - government_treasuries: May issue electronic currency directly or authorize its issuance; regulate the legal standing of electronic instruments. Define the boundary between commodity money, fiat currency, and electronic substitutes.
 *   - technology_historians: Analytical observers seeking to identify the genuine threshold of emergence independent of regulatory labeling. Hold no power to define the category but produce alternative readings.
 *   - private_users_and_merchants: Participants in electronic payment systems; their acceptance and use practices may or may not coincide with institutional recognition.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(electronic_money_emergence__first_held_reading, 0.31).
domain_priors:suppression_score(electronic_money_emergence__first_held_reading, 0.18).
domain_priors:theater_ratio(electronic_money_emergence__first_held_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(electronic_money_emergence__first_held_reading, extractiveness, 0.31).
narrative_ontology:constraint_metric(electronic_money_emergence__first_held_reading, suppression_requirement, 0.18).
narrative_ontology:constraint_metric(electronic_money_emergence__first_held_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(electronic_money_emergence__first_held_reading, accessibility_collapse, 0.41).
narrative_ontology:constraint_metric(electronic_money_emergence__first_held_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(electronic_money_emergence__first_held_reading, rope).
narrative_ontology:human_readable(electronic_money_emergence__first_held_reading, "Electronic Money Emergence via First Institutional Hold").
narrative_ontology:topic_domain(electronic_money_emergence__first_held_reading, "economic/monetary/technology").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(electronic_money_emergence__first_held_reading, 'f4504173-51ab-4c89-84e9-206ccd51d744').
narrative_ontology:cs_kernel_codification('f4504173-51ab-4c89-84e9-206ccd51d744', formalized).
narrative_ontology:cs_authority_grounding('f4504173-51ab-4c89-84e9-206ccd51d744', lineage).
narrative_ontology:cs_interpretation_layer_present('f4504173-51ab-4c89-84e9-206ccd51d744').
narrative_ontology:cs_reading_relation('f4504173-51ab-4c89-84e9-206ccd51d744', electronic_money_emergence__became_thinkable_reading, influences).
narrative_ontology:cs_reading_relation('f4504173-51ab-4c89-84e9-206ccd51d744', electronic_money_emergence__m4_m5_collapse_reading, coexists_with).
narrative_ontology:cs_axiom('f4504173-51ab-4c89-84e9-206ccd51d744', foundational, institutional_custody_constitutes_emergence).
narrative_ontology:cs_axiom_status(institutional_custody_constitutes_emergence, holdable).
narrative_ontology:cs_axiom_grounding('f4504173-51ab-4c89-84e9-206ccd51d744', institutional_custody_constitutes_emergence, conventional).
narrative_ontology:cs_axiom('f4504173-51ab-4c89-84e9-206ccd51d744', secondary, regulatory_codification_enables_monetary_authority).
narrative_ontology:cs_axiom_status(regulatory_codification_enables_monetary_authority, holdable).
narrative_ontology:cs_axiom_grounding('f4504173-51ab-4c89-84e9-206ccd51d744', regulatory_codification_enables_monetary_authority, instrumental).
narrative_ontology:cs_reference_frame('f4504173-51ab-4c89-84e9-206ccd51d744', pre_computerization_physical_currency_era).
narrative_ontology:cs_drift_state('f4504173-51ab-4c89-84e9-206ccd51d744', contemporary_cryptocurrency_and_cbdc_era, gap(codification_collapse, substantial, false)).
narrative_ontology:cs_created_at('f4504173-51ab-4c89-84e9-206ccd51d744', '2026-06-12T14:32:18Z').
narrative_ontology:cs_kernel_id(electronic_money_emergence__first_held_reading, electronic_money_emergence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(electronic_money_emergence__first_held_reading, financial_regulators).
narrative_ontology:constraint_beneficiary(electronic_money_emergence__first_held_reading, central_banking_authority).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(electronic_money_emergence__first_held_reading, regulated_commercial_banks).
narrative_ontology:constraint_beneficiary(electronic_money_emergence__first_held_reading, payment_network_operators).
narrative_ontology:constraint_beneficiary(electronic_money_emergence__first_held_reading, government_treasuries).
narrative_ontology:constraint_beneficiary(electronic_money_emergence__first_held_reading, private_users_and_merchants).
narrative_ontology:constraint_victim(electronic_money_emergence__first_held_reading, regulated_commercial_banks).
narrative_ontology:constraint_victim(electronic_money_emergence__first_held_reading, private_users_and_merchants).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets regulatory definitions of money supply categories (M1/M2/M3/M4/M5) and determines what counts as 'dematerialized currency' in official accounting. Conducts monetary policy through electronic payment systems and reserve management. The 'first_held' reading of emergence privileges the central bank's perspective because it treats institutional custody as the marker. From this seat, emergence is when the central bank first recognized and categorized electronic balances as distinct from physical currency — likely coinciding with post-war computerized banking infrastructure and the formalization of M-category definitions in monetary theory (1960s–1980s). The central bank benefits from this definition because it enables monetary policy transmission, financial surveillance, and compliance verification.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__first_held_reading, central_banking_authority, agenda_setter,
    institutional, generational, analytical, national).

% Hold customer deposits in electronic form via ledger entries or electronic networks; manage transfers via automated clearing houses, SWIFT, and real-time gross settlement systems. They are the first (and continuing) institutional bearers of dematerialized currency. They benefit from the standardized regulatory definition because it clarifies their legal standing and enables interoperability with central banks and other commercial banks. They bear compliance costs in maintaining the distinction and cannot operate outside the regulatory boundary defined by the central bank. This seat confirms the first_held_reading's empirical claim: commercial banks do hold dematerialized currency, and they have recognized it as distinct from physical notes since early electronic banking (1960s–1980s). Their exit options are constrained because they cannot operate a modern banking system without adopting electronic settlement.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__first_held_reading, regulated_commercial_banks, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(electronic_money_emergence__first_held_reading, regulated_commercial_banks, payer).

% Facilitate the custody and transfer of electronic balances: SWIFT (interbank messages, 1973+), ACH (automated clearing house, USA 1970s+), real-time gross settlement systems, and modern mobile/digital payment networks. They depend on the regulatory definition to standardize message formats and custody rules across jurisdictions. They benefit from the definition because it enables interoperability and reduces friction in cross-border settlement. They have exit options because they could in principle operate outside the regulated definition (as some alternative-payment networks attempt to do), but most operate within the regulatory boundary because that is where institutional liquidity concentrates.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__first_held_reading, payment_network_operators, beneficiary,
    organized, generational, mobile, global).

% Issue currency (fiat) and maintain the legal framework in which electronic currency substitutes are recognized. They benefit from the standardized definition of electronic money because it enables tax collection, financial surveillance, and enforcement of capital controls. They define the legal boundary between commodity money, fiat currency, and electronic instruments. From the treasury perspective, 'first_held_reading' emergence is when governments and central banks legally recognized electronic deposits as equivalent to fiat currency — a regulatory choice, not a natural fact. They cooperate with central banks in codifying the boundary.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__first_held_reading, government_treasuries, beneficiary,
    institutional, generational, analytical, national).

% Adopt electronic payment systems (bank transfers, credit cards, now mobile payments, eventually CBDC). They benefit from the accessibility and speed of electronic payments compared to physical cash. They are constrained from using alternative currency forms (cryptocurrencies, alternative payment networks) because regulatory boundaries exclude them or make them inconvenient. They bear costs in financial surveillance and regulatory restrictions on which payment forms they can adopt. From this seat, 'emergence' may feel gradual and practical rather than tied to any discrete institutional event — they use electronic money because it is convenient, not because central banks defined it in 1965.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__first_held_reading, private_users_and_merchants, beneficiary,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(electronic_money_emergence__first_held_reading, private_users_and_merchants, payer).

% Analyze the history of electronic currency and contest the institutional-first reading. They argue that private ledger practices (bank book entries, telegraph transfers, punch-card records) constituted electronic money before central banks formalized the definition. They examine alternative readings (became_thinkable_reading: emergence when the conceptual possibility was recognized; m4_m5_collapse_reading: emergence when measurement categories retroactively created the ontology) and produce historical evidence for earlier emergence dates. They are excluded from defining the regulatory boundary but are not powerless — they produce scholarly analysis that can influence policy over time. Their perspective reveals that the 'first_held_reading' is a normative choice to privilege institutional custody as the marker, not an inevitable threshold.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__first_held_reading, technology_historians_and_economists, excluded,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_secondary_role(electronic_money_emergence__first_held_reading, technology_historians_and_economists, observer).

% Develop alternative forms of electronic currency (cryptocurrencies, decentralized finance, community currencies) that dispute the institutional definition of dematerialized money. They argue that electronic money emerged earlier (with private experimentation) and that the regulatory boundary is artificially narrow. They are excluded from the institutional frame and face legal and operational barriers to entry (regulatory restrictions, lack of central bank liquidity access). Their trapped exit position reflects the constraint's enforcement: once the regulatory boundary is codified, alternatives cannot compete on equal terms. From this seat, the 'first_held_reading' is an extractive enclosure: it claims legitimacy for institutional custody while suppressing earlier private innovation.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__first_held_reading, alternative_currency_innovators, excluded,
    moderate, biographical, trapped, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(electronic_money_emergence__first_held_reading, central_banking_authority).
narrative_ontology:fixing_cost_class(electronic_money_emergence__first_held_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a standardized, legally codified distinction between physical and dematerialized currency. This enables central banks to implement monetary policy through electronic payment channels, to measure money supply via standardized M-categories (M1/M2/M3/M4/M5), to conduct financial surveillance and compliance verification, and to integrate electronic payment systems into the broader financial infrastructure. The real coordination problem solved: how to define money supply consistently when payment systems shifted from physical to electronic. Without a shared institutional boundary, monetary policy would become ambiguous — policymakers would not know what counts as 'money' for reserve requirements, policy transmission, and statistical measurement.
% TRANSFER_FUNCTION: Moves regulatory authority and definitional power to central banks and government treasuries: they alone determine what counts as dematerialized currency and can revise that definition over time. Transfers computational and surveillance benefits to regulated institutions: commercial banks, payment networks, and governments gain the ability to track, verify, and control electronic transactions. Transfers convenience and accessibility benefits to users: they can access their money electronically without physical intermediaries. Also transfers constraints and exclusions: users and alternative-payment innovators are excluded from the institutional definition and cannot operate outside the boundary without regulatory permission.
% ABSENT_VOICES: Alternative-currency innovators and decentralized finance developers would object that the institutional-first definition suppresses earlier private experimentation with electronic ledger accounts, punch-card banking, and later cryptographic currencies. Technology historians and heterodox economists would argue that institutional custody is a contingent regulatory choice, not a natural threshold, and that 'true' emergence occurred earlier (when private practice first adopted ledger substitution for physical currency). Users reliant on unbanked payment systems would protest that the institutional definition privileges formal banking and excludes community currencies and informal settlement systems. These voices are structurally absent from the regulatory decision-making process because the definition is set by central banks and governments, not by democratic deliberation or technologist input.
% DISAPPEARANCE_RATIONALE: If the institutional-first definition of electronic money emergence disappeared overnight (e.g., if central banks abandoned monetary policy control, stopped categorizing M-supply, or reconceived the boundary), the entire modern financial system would require reorganization. Monetary policy transmission depends on standardized definitions of money supply; reserve requirements and capital adequacy regulations depend on consistent categorization of electronic vs. physical forms; tax collection and financial surveillance depend on the boundary between regulated and unregulated payment systems. The disappearance would force a new definition (alternative readings would compete: became_thinkable_reading, m4_m5_collapse_reading, or new framings), and the transition would disrupt financial markets, break interbank settlement systems, and create ambiguity in policy transmission. The constraint's disappearance is not neutral because the current institutional boundary is embedded in every modern financial institution's operations.
% FOUNDING_PROBLEM: How to maintain monetary policy authority and financial surveillance when payment systems transitioned from physical currency (easily observable, countable) to electronic ledger entries and computer-mediated transfers (invisible, ambiguous in their legal status). As banking computerization began (1960s–1970s), central banks faced a crisis: they could no longer see money in flight, could not easily count it, and could not verify that electronic balances were genuine currency vs. credit vs. accounting entries. The founding problem was to establish a legal and definitional framework that would (1) clarify what electronic substitutes count as money, (2) enable monetary policy transmission through electronic channels, (3) preserve central bank authority in an era of automated systems, (4) standardize reporting and surveillance across jurisdictions.
% FOUNDING_PROBLEM_CORROBORATION: Federal Reserve historical analysis (Friedman & Schwartz, 'A Monetary History of the United States') documents the transition and shows that monetary policy became increasingly dependent on standardized definitions of money supply during the computerization era (1960s–1980s). IMF and BIS publications on monetary aggregates confirm that central banks worldwide formalized M-category definitions to maintain policy authority during the electronic transition. Academic monetary theorists (e.g., Goodhart, Keynes, modern central bankers) attest that the founding problem — how to define money when payment forms multiplied — remains live and has intensified with each wave of financial innovation (ATMs, credit cards, now cryptocurrencies and CBDC). The problem's status is 'live' because central banks continue to grapple with whether new forms (cryptocurrencies, stablecoins, CBDC) should be recognized as money, and the answer requires revisiting the institutional-first definition repeatedly. Outside corroborators include technology historians (who document the pragmatic institutional adoption of computerized banking), economic historians (who trace the evolution of monetary aggregates), and regulatory scholars (who note that the M-definition is a policy choice, not a natural fact).
narrative_ontology:disappearance_verdict(electronic_money_emergence__first_held_reading, world_rearranges).
narrative_ontology:founding_problem_status(electronic_money_emergence__first_held_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(electronic_money_emergence__first_held_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(electronic_money_emergence__first_held_reading, 'none', 1).
narrative_ontology:epsilon_provenance(electronic_money_emergence__first_held_reading, 0.31, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(electronic_money_emergence__first_held_reading_tests).
:- end_tests(electronic_money_emergence__first_held_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is modest (0.31 at interval end) because the constraint's operation does not extract rents in the manner of a snare or tangled_rope. Instead, it coordinates a definition: central banks and regulated banks benefit from codifying a boundary between money forms, which enables surveillance, monetary policy implementation, and reserve requirement calculation. The beneficiary extraction is primarily regulatory power (the ability to define what counts as money supply, to implement policy via electronic channels, to verify compliance) rather than direct economic rent. Suppression is low (0.18) because the institutional actors adopt the definition voluntarily — it is in their interest to formalize the boundary. Theater is moderate (0.22) because regulatory framing of 'emergence' as an institutional event masks a more ambiguous historical transition: private practices (ledger accounts, telegraph transfers, later electronic systems) evolved gradually, and regulators codified them retroactively. Accessibility collapse is moderate (0.41) because once the institutional definition is in place, alternatives become less accessible — to dispute whether electronic money 'really' emerged requires challenging the regulatory categorization itself. Resistance is moderate-to-high (0.58) because historians, heterodox economists, and technologists actively contest whether institutional custody truly marks emergence or merely labels a pre-existing practice. The measurement series shows extraction and theater rising over the interval: as electronic money systems mature and regulatory categories solidify, the institutional benefit of maintaining the 'first_held' boundary increases (institutional coordination becomes more valuable), and the performative work required to maintain the distinction (theater) rises as cryptographic and decentralized alternatives emerge (2010s onwards). At t0 (emergence moment) the measurements are low because the distinction is novel and not yet embedded. By t60 (near-future) the metrics rise modestly as the boundary becomes entrenched regulatory doctrine and the alternatives (cryptocurrencies, central bank digital currencies) force explicit re-articulation of why institutional custody matters.
 *
 * PERSPECTIVAL GAP:
 *   Central banking and regulatory seats experience this constraint as a genuine coordination mechanism: defining the boundary between money forms enables monetary policy, reserve accounting, and financial stability monitoring. From this seat, emergence at first institutional custody is the natural threshold because institutional custody is what enables the regulatory functions. Payment network operators experience it as operational necessity: they must distinguish electronic from physical currency to route payments and manage reserves correctly. From this seat, 'emergence' is pragmatic — it marks the point at which technical infrastructure became capable of substitution. Technology historians and heterodox economists experience the constraint as post-hoc definitional capture: they see private ledger practices predating institutional recognition and argue that 'emergence' should be dated to social adoption, not regulatory labeling. From this seat, the constraint is partly performative — institutional actors are maintaining a boundary that masks earlier practice. The engine computes these divergences from the structural data: regulators hold institutional power with analytical time horizons (stable positioning); users hold moderate to powerless positions with biographical horizons; historians hold analytical power but no institutional exit. The directionality derivation shows regulators near the beneficiary end (they collect regulatory power and coordination benefit), users and historians spread across the middle (moderate benefit/cost tradeoff), and alternative-currency innovators near the target end if they are constrained from entry (trapped/constrained exit).
 *
 * DIRECTIONALITY LOGIC:
 *   The central banking authority benefits from the constraint (power atom: institutional, directionality near 0.0 beneficiary end) because institutional custody definitions enable monetary policy implementation, financial surveillance, and reserve requirement regulation. Regulated commercial banks derive mixed benefits (power atom: institutional, directionality ~0.35–0.45 symmetric): they benefit from the standardized definition and regulatory recognition, but also bear compliance costs and cannot operate outside the defined boundary. Payment network operators benefit moderately (power atom: organized, directionality ~0.30 beneficiary): standardized definitions reduce interoperability friction. Government treasuries benefit (power atom: institutional, directionality ~0.20 beneficiary): the definition supports fiat authority and tax collection infrastructure. Private users and merchants derive diffuse benefits (power atom: moderate-to-powerless, directionality ~0.50 symmetric): they gain access to electronic payment systems but are also subject to the regulatory boundary (constrained which forms of currency they can adopt). Technology historians and heterodox economists bear costs (power atom: analytical, directionality ~0.65 target): they are excluded from defining the emergence threshold and their alternative readings are suppressed by institutional authority. No directionality overrides are needed because the derivation chain (beneficiary/victim + power + exit) produces accurate d values across seats. The constraint's primary asymmetry is between institutional power (low d) and analytical power (high d): regulators set the boundary, historians critique it.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint does NOT display mandatrophy in the classical sense (a founding problem that has died but the constraint persists). The founding problem — establishing a standardized, legally codified distinction between physical and electronic currency — remains live: regulatory authorities continue to refine and defend the M-category definitions, central banks continue to implement electronic payment infrastructure, and the boundary is actively maintained through policy and statute. However, there is a secondary mandatrophy risk: the founding problem was to enable monetary policy and financial surveillance in an era when electronic substitution was new and fragile. That problem is substantially solved (electronic systems are now mature and universal). Yet the regulatory boundary persists, and in some jurisdictions (particularly around central bank digital currencies, CBDC) the boundary is being redrawn rather than discarded, suggesting the constraint has shifted toward performing institutional authority (theater rising in the measurements) rather than solving the original coordination problem. If central banks ultimately issue electronic currency directly, the 'first_held_reading' distinction (institutional bearer as outside entity) may become obsolete, but the constraint itself will likely morph rather than disappear — the boundary will be redefined around CBDC vs. commercial bank money, not physical vs. electronic. Current status: founding problem is live but overlapping with emerging alternative framings (CBDC, decentralized finance) that may eventually trigger true mandatrophy or force fundamental reclassification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    institutional_recognition_vs_social_practice,
    'Does emergence occur at the moment of institutional measurement/recognition, or at the moment when non-institutional actors first adopt the practice?',
    'Historical comparison of regulatory categorization dates (M1/M2/M3/M4/M5 system formalization) against evidence of private bearer-account usage patterns in pre-regulatory era.',
    'If emergence precedes institutional measurement, the constraint''s referent (the institutional-first model) loses claim to capture the genuine ontological transition. If it postdates private practice, the ''first held'' marker becomes a post-hoc labeling event, not a generative threshold.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(institutional_recognition_vs_social_practice, empirical, 'Whether institutional recognition creates or merely codifies the ontological category.').

omega_variable(
    committer_frame_kernel_reading_ambiguity,
    'This reading instantiates one normative framing of an ontological kernel: does emergence anchor to institutional custody specifically, or to dematerialized substitution for physical notes more generally?',
    'Examine the axioms: institutional_custody_constitutes_emergence (this reading''s foundational premise) vs. substitution_suffices_for_emergence (became_thinkable_reading''s premise). Ask whether institutional custody is logically necessary for substitution, or contingent on regulatory choice.',
    'If institutional custody is necessary: this reading forecloses became_thinkable_reading. If contingent: the readings coexist, differing on which threshold matters normatively. If institutional custody is neither necessary nor sufficient: this reading''s axiom is overridden by empirical fact.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_frame_kernel_reading_ambiguity, conceptual, 'Kernel reading disagreement: institutional custody as constitutive vs. contingent marker.').

omega_variable(
    retroactive_measurement_artifact_risk,
    'Does the m4_m5_collapse_reading (measurement creates category retroactively) falsify the institutional-first reading''s claim to capture a real ontological boundary?',
    'Examine historical record: did regulators and institutions recognize a boundary BEFORE formal M4/M5 categorization? If yes, the institutional-first reading describes a real event. If recognition and measurement were simultaneous, the readings may coexist (different framings of one event). If measurement preceded recognition, m4_m5_collapse_reading influences this reading''s credibility.',
    'Risk of retroactive measurement artifact undermines this reading''s claim to capture emergence, not merely label it after the fact.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(retroactive_measurement_artifact_risk, empirical, 'Whether the institutional-first reading describes emergence or post-hoc measurement.').

omega_variable(
    custody_form_dematerialization_boundary,
    'What constitutes ''dematerialized currency in a form distinguishable from physical notes''? Does ledger entry suffice, or must there be cryptographic/electronic encoding?',
    'Examine earliest examples: bank ledger accounts (1880s–1920s) vs. punch-card or electronic bank records (1960s+) vs. cryptographic bearer instruments (2010s+). At what point does custody form become unambiguously ''distinguishable from physical notes''?',
    'If ledger entry suffices, emergence date could be pushed back to 19th-century commercial banking. If electronic encoding required, emergence moves to mainframe era. If cryptographic distinguishability required, emergence is recent (2010s+). Different dates imply different institutional bearers and different extraction profiles.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(custody_form_dematerialization_boundary, conceptual, 'What technical form distinguishes electronic from physical currency.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(electronic_money_emergence__first_held_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(elec_tr_t0, electronic_money_emergence__first_held_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(elec_tr_t15, electronic_money_emergence__first_held_reading, theater_ratio, 15, 0.12).
narrative_ontology:measurement(elec_tr_t30, electronic_money_emergence__first_held_reading, theater_ratio, 30, 0.22).
narrative_ontology:measurement(elec_tr_t45, electronic_money_emergence__first_held_reading, theater_ratio, 45, 0.26).
narrative_ontology:measurement(elec_tr_t60, electronic_money_emergence__first_held_reading, theater_ratio, 60, 0.28).

% Extraction over time
narrative_ontology:measurement(elec_be_t0, electronic_money_emergence__first_held_reading, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(elec_be_t15, electronic_money_emergence__first_held_reading, base_extractiveness, 15, 0.21).
narrative_ontology:measurement(elec_be_t30, electronic_money_emergence__first_held_reading, base_extractiveness, 30, 0.31).
narrative_ontology:measurement(elec_be_t45, electronic_money_emergence__first_held_reading, base_extractiveness, 45, 0.38).
narrative_ontology:measurement(elec_be_t60, electronic_money_emergence__first_held_reading, base_extractiveness, 60, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(elec_su_t0, electronic_money_emergence__first_held_reading, suppression_requirement, 0, 0.12).
narrative_ontology:measurement(elec_su_t15, electronic_money_emergence__first_held_reading, suppression_requirement, 15, 0.15).
narrative_ontology:measurement(elec_su_t30, electronic_money_emergence__first_held_reading, suppression_requirement, 30, 0.18).
narrative_ontology:measurement(elec_su_t45, electronic_money_emergence__first_held_reading, suppression_requirement, 45, 0.19).
narrative_ontology:measurement(elec_su_t60, electronic_money_emergence__first_held_reading, suppression_requirement, 60, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(electronic_money_emergence__first_held_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(electronic_money_emergence__first_held_reading, 0.12).
narrative_ontology:affects_constraint(electronic_money_emergence__first_held_reading, electronic_money_emergence__became_thinkable_reading).
narrative_ontology:affects_constraint(electronic_money_emergence__first_held_reading, electronic_money_emergence__m4_m5_collapse_reading).

% DUAL FORMULATION NOTE:
% The electronic_money_emergence kernel decomposes into three structurally distinct constraints, each with different ε, different institutional stakeholders, and different empirical status. The first_held_reading (this story) anchors emergence to institutional custody and regulatory codification, yielding moderate extractiveness (0.31) driven by regulatory power capture and coordination benefit. The became_thinkable_reading anchors emergence to social/technical possibility, yielding lower extractiveness (conceptual adoption rather than institutional imposition). The m4_m5_collapse_reading anchors emergence to measurement category, yielding higher extractiveness (retroactive ontological capture). All three readings share the kernel (the event of emergence) but differ in what event they describe. Each story carries a full stakeholder surface, omega variables documenting the reading-disagreement, and network links to siblings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
