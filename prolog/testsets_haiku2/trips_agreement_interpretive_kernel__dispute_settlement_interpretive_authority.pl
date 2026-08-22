% ============================================================================
% CONSTRAINT STORY: trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, []).

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
 *   constraint_id: trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority
 *   human_readable: WTO Dispute Settlement Authority Over TRIPS Interpretation
 *   domain: international_trade/intellectual_property
 *
 * SUMMARY:
 *   The WTO dispute settlement mechanism was designed as a neutral,
 *   rule-based system for resolving trade disputes between member states.
 *   When applied to TRIPS interpretation, however, it has become a venue
 *   through which pharmaceutical patent holders and wealthy countries lock in
 *   a narrow reading of compulsory licensing and parallel import
 *   flexibilities — flexibilities the TRIPS text explicitly permits but
 *   panels interpret minimally. This constraint describes the dispute
 *   settlement authority ITSELF as a binding interpretive kernel: how panels
 *   acquire the structural power to declare what TRIPS means, and how that
 *   power distributes extraction and benefit. The founding coordination
 *   problem (preventing unilateral rule-breaking) has been operationalized as
 *   asymmetric enforcement that locks one reading (patent maximalism) against
 *   others (public health flexibility). The reading authored here is: dispute
 *   panels hold binding interpretive authority enforced through trade
 *   retaliation. The sibling readings contest what the TRIPS text actually
 *   permits and who should decide.
 *
 * KEY AGENTS:
 *   - WTO dispute settlement apparatus: the institutional seat that interprets TRIPS and enforces rulings via trade retaliation
 *   - pharmaceutical patent holders: benefit from narrow panel interpretations of flexibilities; high-income countries carry their interests in dispute cases
 *   - developing country governments: face rulings that constrain compulsory licensing and parallel imports; bear the political cost of medicines they cannot afford
 *   - generic drug manufacturers: constrained by panel precedent narrowing the flexibilities they depend on
 *   - public health advocates: excluded from the adjudication process entirely; their objections to patent-maximalist readings are structurally absent
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, 0.68).
domain_priors:suppression_score(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, 0.72).
domain_priors:theater_ratio(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, extractiveness, 0.68).
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, accessibility_collapse, 0.64).
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, tangled_rope).
narrative_ontology:human_readable(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, "WTO Dispute Settlement Authority Over TRIPS Interpretation").
narrative_ontology:topic_domain(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, "international_trade/intellectual_property").

domain_priors:requires_active_enforcement(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, 'af7b3883-a312-472f-a501-c5a6f2081fbc').
narrative_ontology:cs_kernel_codification('af7b3883-a312-472f-a501-c5a6f2081fbc', fixed_text).
narrative_ontology:cs_authority_grounding('af7b3883-a312-472f-a501-c5a6f2081fbc', extraction).
narrative_ontology:cs_interpretation_layer_present('af7b3883-a312-472f-a501-c5a6f2081fbc').
narrative_ontology:cs_reading_relation('af7b3883-a312-472f-a501-c5a6f2081fbc', trips_agreement_interpretive_kernel__public_health_flexibility_reading, forecloses).
narrative_ontology:cs_reading_relation('af7b3883-a312-472f-a501-c5a6f2081fbc', trips_agreement_interpretive_kernel__strong_exclusivity_reading, coexists_with).
narrative_ontology:cs_axiom('af7b3883-a312-472f-a501-c5a6f2081fbc', foundational, binding_dispute_settlement_authority_required).
narrative_ontology:cs_axiom_status(binding_dispute_settlement_authority_required, holdable).
narrative_ontology:cs_axiom_grounding('af7b3883-a312-472f-a501-c5a6f2081fbc', binding_dispute_settlement_authority_required, conventional).
narrative_ontology:cs_axiom('af7b3883-a312-472f-a501-c5a6f2081fbc', foundational, panel_precedent_locks_interpretation).
narrative_ontology:cs_axiom_status(panel_precedent_locks_interpretation, holdable).
narrative_ontology:cs_axiom_grounding('af7b3883-a312-472f-a501-c5a6f2081fbc', panel_precedent_locks_interpretation, instrumental).
narrative_ontology:cs_reference_frame('af7b3883-a312-472f-a501-c5a6f2081fbc', neutral_rule_of_law_adjudication).
narrative_ontology:cs_drift_state('af7b3883-a312-472f-a501-c5a6f2081fbc', contemporary_post_appellate_body_collapse, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('af7b3883-a312-472f-a501-c5a6f2081fbc', '').
narrative_ontology:cs_kernel_id(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, trips_agreement_interpretive_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, pharmaceutical_patent_holders).
narrative_ontology:constraint_beneficiary(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, dispute_settlement_apparatus).
narrative_ontology:constraint_victim(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, generic_drug_manufacturers).
narrative_ontology:constraint_victim(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, developing_country_governments).
narrative_ontology:constraint_victim(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, public_health_access_advocates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, high_income_country_governments).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the panel process, interprets TRIPS text, and enforces compliance. Justifies narrow interpretations of compulsory licensing and parallel import flexibilities as textual application and TRIPS purpose-protection. Functions as the de facto constitutional court for IP regulation since Appellate Body collapse. Panel rulings become binding precedent; developing countries cannot appeal or renegotiate the interpretation — they must implement it or face trade retaliation.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, wto_dispute_settlement_apparatus, agenda_setter,
    institutional, generational, analytical, global).

% Extract monopoly rents from patent protection in developed markets and emerging markets. Benefit from panel rulings that interpret compulsory licensing narrowly (requiring negotiation before use) and parallel imports narrowly (prohibiting cross-border resale). Have resources to fund litigation, hire expert economists to brief panels, and mobilize high-income country governments to bring disputes on their behalf. Win disputes more often than they lose; when they lose, use arbitrage (move manufacturing, adjust pricing) to offset.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, pharmaceutical_patent_holders, beneficiary,
    powerful, generational, arbitrage, global).

% Bring disputes against developing countries on behalf of pharmaceutical patent holders (US v. India, US v. China on trade secret protection). Control dispute panel initiation and use the threat of authorized retaliation to deter developing countries from using flexibilities. Can threaten bilateral retaliation even without losing a dispute — the mere threat shapes behavior. Shaped Appellate Body composition to be favorable to IP interests and blocked appointments to prevent appeals of unfavorable rulings.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, high_income_country_governments, beneficiary,
    institutional, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, high_income_country_governments, agenda_setter).

% Lose TRIPS disputes brought by developed countries (India lost India—Pharmaceuticals; multiple countries lost subsequent compulsory licensing cases). Implement panel rulings by narrowing compulsory licensing access and blocking parallel imports, which constrains their citizens' access to affordable medicines. Face political pressure from populations who cannot afford drugs but are told the government is bound by international law. Exit option is WTO withdrawal, which would entail massive trade losses and retaliation; formally available but economically prohibitive.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, developing_country_governments, payer,
    moderate, biographical, constrained, global).

% Depend on compulsory licensing and parallel imports to compete in low-income markets. Panel rulings narrow these pathways by requiring negotiation-first approaches and geographic blocking of resale. Cannot litigate disputes themselves (only states can); are represented by developing country governments who often lack resources or political will to defend flexibility aggressively. Exit option is to exit the market or to operate outside WTO jurisdiction (illegal channels), both of which are constrained.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, generic_drug_manufacturers, payer,
    moderate, biographical, constrained, global).

% Cannot bring cases or file amicus briefs in WTO disputes (rule of the process). Advocate for broad compulsory licensing and parallel import access but have no standing in the adjudication itself. Witness rulings that narrow their options and see medicines remain unaffordable. Cannot exit — they are structurally denied participation in the mechanism that sets the rules they live under. Their objection is to the apparatus' legitimacy and inclusivity, not to any single ruling.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, public_health_access_advocates, excluded,
    powerless, immediate, trapped, global).

% Formally consent to binding dispute settlement as equals, but experience asymmetric enforcement: developed countries litigate more, win more, have more resources to appeal. Developing countries face resource constraints and retaliation threats that deter challenging even adverse interpretations. Appellate Body collapse means final panel rulings stand without multilateral check — a unilateral loss cannot be reviewed. Observe that the multilateral dispute process functions as a tool of the most-resourced members.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, wto_member_states_formally_equal, observer,
    institutional, generational, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single authoritative interpretation of TRIPS text so that all 164 WTO members operate under the same understanding of their IP obligations, preventing each country from declaring unilateral interpretations and triggering trade wars or defections.
% TRANSFER_FUNCTION: Moves policy autonomy from developing country governments to WTO dispute panels (which interpret TRIPS narrowly on compulsory licensing and parallel imports), and moves medicine access from patients in low-income countries to patent holders in high-income countries. Transfers legitimacy from multilateral renegotiation to unilateral panel authority enforced by trade retaliation threat.
% ABSENT_VOICES: Public health advocates, patients' organizations, generic manufacturers, and civil society representatives cannot participate in TRIPS disputes. They would argue for broad flexibility interpretation but are structurally excluded from the adjudication process. Developing countries report they lack resources to litigate and face retaliation threats that deter defending flexibility interpretations, making them inadequate representatives of their citizens' health interests.
% DISAPPEARANCE_RATIONALE: If binding dispute settlement authority over TRIPS disappeared, the constraint regime would shift immediately: compulsory licensing would expand in developing countries (each would use flexibilities without fear of legal penalty); generic manufacturers would access parallel imports freely; pharmaceutical prices would fall in low-income markets; the apparatus' authority to enforce narrow readings would evaporate; patent holders would lose the enforcement mechanism that locks maximalism in place. The entire pharmaceutical market for developing countries would reorganize around affordability rather than patent maximalism.
% FOUNDING_PROBLEM: 1995: WTO creators designed binding dispute settlement to prevent wealthy countries from unilaterally interpreting trade rules and abandoning commitments when they became politically costly domestically. The problem was chaotic bilateralism and power-based imposition of terms. The solution was rule-of-law adjudication: neutral panels apply negotiated text and enforce compliance for all members equally.
% FOUNDING_PROBLEM_CORROBORATION: The WTO apparatus, developed countries, and pharmaceutical interests attest the founding problem is still live and binding settlement prevents unilateral defections. Developing countries, public health advocates (Médecins Sans Frontières, Treatment Action Campaign, various NGOs at WIPO), and independent trade scholars (Dreyfuss, Reichman, others) attest the founding problem has been operationalized asymmetrically: the panels' authority is now used to enforce one party's preferred reading against others, not to apply neutral text. The Appellate Body collapse itself is cited as evidence: developed countries blocked appeals of rulings against their interests, converting binding settlement into a tool of the most-resourced members. Legislative bodies in developing countries have passed resolutions questioning the legitimacy of binding settlement in TRIPS disputes and calling for renegotiation.
narrative_ontology:disappearance_verdict(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, world_rearranges).
narrative_ontology:founding_problem_status(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, 'none', 1).
narrative_ontology:epsilon_provenance(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is 0.68 at interval end because the dispute settlement authority locks developing countries and generic manufacturers into narrow readings of TRIPS flexibilities, constraining their exit options and transferring compliance burden unidirectionally. The measurement series shows a 0.35→0.68 climb from 1995 to 2026: early panels (1995–2005) treated TRIPS flexibilities more permissively; the India—Pharmaceuticals IP (2009–2014) case hardened the precedent. The 2020 Appellate Body collapse marks a structural inflection: bilateral power now substitutes for multilateral review, further asymmetrizing enforcement. Suppression runs high (0.72) because the mechanism depends on enforcing rulings that constrain developing countries' policy space through threat of authorized retaliation. Theater rises (0.18→0.41) because panels increasingly justify narrow readings with elaborate doctrinal scaffolding (patent incentive arguments, interpreting TRIPS 'purposes' sections narrowly) while the underlying function is to lock pharmaceutical maximalism, not to apply neutral textual interpretation. Accessibility collapse is moderate (0.64) because developing countries formally have exit options (negotiating TRIPS amendments, withdrawing from WTO) but these are prohibitively costly; the constraint's persistence depends on active suppression (retaliation threat and panel authority) rather than on collapse of alternatives.
 *
 * PERSPECTIVAL GAP:
 *   The dispute settlement apparatus and pharmaceutical beneficiaries perceive this as neutral rule-application: panels are applying TRIPS text as it was negotiated and pharma holders are lawfully exercising their patent rights. Developing countries and generic manufacturers perceive the same structure as locked interpretation: the panels' narrow reading of flexibilities was not textually mandated but was chosen, and the choice is enforced unidirectionally against those who depend on flexibility access. The engine computes these seats differently: the beneficiary seats (apparatus administering, pharma extracting) have low directionality (d toward beneficiary end); the payer and excluded seats (developing countries losing cases, manufacturers blocked, advocates shut out) have high directionality (d toward target end). This divergence is the engine's measurement of the constraint's asymmetry.
 *
 * DIRECTIONALITY LOGIC:
 *   Pharmaceutical patent holders (powerful, global scope, arbitrage exit because they can litigate and appeal) are structural beneficiaries (d ≈ 0.15). The dispute settlement apparatus itself (institutional, analytical exit) has d ≈ 0.25 — it administers the constraint and benefits from its legitimacy, but its real directionality is ambiguous: the apparatus was designed to be neutral, but it now functions to amplify one party's interests. Developing country governments (moderate power, constrained exit because WTO withdrawal is prohibitive, biographical horizon) are targets (d ≈ 0.82) — they lose disputes, implement unfavorable rulings, and cannot arbitrage out. Generic manufacturers (moderate power, constrained exit, dependent on flexibilities that panels narrow) are targets (d ≈ 0.78). Public health advocates (powerless, trapped exit, immediate horizon) are the most severely targeted (d ≈ 0.95) but excluded from the seat structure entirely. The directionality overrides address the apparatus: the automatic derivation (institutional power, analytical exit, no explicit beneficiary/victim declaration) produces a neutral d ≈ 0.5, but the structural reality is that the apparatus functions as the enforcement arm for beneficiaries; override to d ≈ 0.30 captures that it collects legitimacy and stability from the system without bearing its costs.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing unilateral trade rule-breaking) remains live; that remains the apparatus's stated function. But the actual measured function has shifted: disputes are no longer symmetric adjudications of negotiated text but asymmetric enforcement of one reading against others. The theater ratio rising from 0.18 to 0.41 suggests that increasing effort goes to doctrinal justification (invoking TRIPS 'purposes', patent incentive theory, narrow construction of flexibilities) while the underlying enforcement function remains unchanged — a classic Goodhart drift signature. The constraint exhibits mandatrophy: its stated purpose (rule-application, neutrality) and actual function (locking pharmaceutical maximalism) have diverged, and the divergence is maintained through the apparatus's own authority claim.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    binding_authority_legitimacy,
    'Is binding dispute settlement authority inherently neutral, or does it necessarily amplify the interests of whoever can afford to litigate?',
    'Comparative analysis of dispute outcomes by defendant economic power (developed vs. developing countries); audit of legal resources invested by pharma vs. public health interests in TRIPS disputes; empirical study of panel composition bias.',
    'If binding authority is structurally amplifying wealthy interests, the constraint is pure snare dressed as coordination. If neutral outcomes can be established, the extraction is smaller than measured and the constraint is genuinely tangled rope. If binding authority is inherently asymmetric, the alternative (bilateral negotiation or multilateral assembly) would be preferable — this drives mandatrophy verdict.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(binding_authority_legitimacy, empirical, 'Whether binding adjudication is neutral or structurally favorable to repeat players with legal resources.').

omega_variable(
    reading_containment_mechanism,
    'Can dispute panels'' precedent logically contain multiple readings of TRIPS, or does binding authority necessarily privilege one reading over others by making alternatives non-justiciable?',
    'Analysis of hypothetical: if a panel ruled for a public health reading (broad compulsory licensing), would subsequent panels treat it as binding precedent, or would they distinguish/overturn it based on the pharmaceutical maximalist reading? Historical audit of precedent reversal patterns (who reverses precedent and under what conditions).',
    'If binding authority forecloses alternative readings regardless of their textual merit, the constraint is a foreclosure mechanism, not a coordination mechanism — the coordination story is cover for reading-lock. If multiple readings remain interpretively live (even if one prevails), the constraint is tangled rope. If precedent reversal is routine based on which party litigates, the constraint is purely extractive snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_containment_mechanism, conceptual, 'Whether binding precedent necessarily forecloses alternative TRIPS readings or permits interpretive pluralism.').

omega_variable(
    appellate_body_collapse_causation,
    'Did the Appellate Body collapse because dispute settlement was becoming too activist in constraining state sovereignty, or because wealthy countries blocked appeals that went against pharmaceutical maximalism?',
    'Historical audit of the disputes that triggered US blocking of Appellate Body appellate appointments (2015–2020); analysis of which rulings were overturned or blocked by the US; testimony from negotiating parties on intent.',
    'If collapse was triggered by activist panels constraining pharma, the measurement of suppression should be lower and the constraint is more legitimately rope-like. If collapse was triggered by wealthy countries blocking appeals of unfavorable rulings, the measurement of suppression is even higher than 0.72 and the constraint is pure snare with enforced uncertainty. This affects whether Appellate Body restoration or its permanent absence better describes the current state.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(appellate_body_collapse_causation, empirical, 'Political causation of the Appellate Body''s non-functioning state.').

omega_variable(
    public_health_flexibility_interpretive_capacity,
    'Is the narrow interpretation of compulsory licensing and parallel import flexibilities imposed BY the panels, or does the TRIPS text itself require those narrow readings?',
    'Comparative textual analysis: could TRIPS articles 31 (compulsory licensing) and 6 (parallel imports) support broad public health readings under standard interpretive principles? Expert testimony from TRIPS negotiators about original intent. Natural experiment if a future panel issued a broad reading — would it overturn established precedent?',
    'If narrow reading is textually mandated, this constraint is natural-law-ish (mountain-like) — the panels are applying fixed text. If narrow reading is one choice among defensible interpretations, panels are choosing, and the constraint is tangled rope or snare depending on how the choice is enforced. If original negotiators intended broad flexibility, the narrow reading is a false summit (beneficiaries captured the interpretation).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(public_health_flexibility_interpretive_capacity, conceptual, 'Whether panel interpretations are textually constrained or discretionary within textual bounds.').

omega_variable(
    exit_cost_asymmetry_in_wto_membership,
    'Is WTO membership exit formally prohibited or merely prohibitively costly? If a developing country formally withdrew over a TRIPS ruling, what would be the retaliation?',
    'Legal analysis of WTO withdrawal procedures (Article XV GATT 1994); estimation of trade losses from WTO exit (gravity models); case study of any threats or attempts (Argentina''s near-exit, etc.).',
    'If exit is legally possible but economically prohibitive, exit_options should be coded ''constrained'' not ''trapped''. If exit is blocked or carries catastrophic retaliation, exit_options approach ''trapped''. The coded value affects the directionality of developing countries and thus the measured extractiveness of the constraint on that seat.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(exit_cost_asymmetry_in_wto_membership, empirical, 'Whether WTO exit is a real option or only nominally available.').

omega_variable(
    sibling_reading_empirical_status,
    'The public_health_flexibility_reading and strong_exclusivity_reading are live in the kernel dispute — what is their current empirical status in TRIPS jurisprudence? Are they both holdable or has one been overridden?',
    'Audit of recent TRIPS disputes (2015–2026): which readings do panels cite as justifications? Do any panels cite public health readings as legitimate alternatives? Do any cite strong exclusivity as mandatory? What is the ratio of citation patterns?',
    'If both readings are cited in recent cases, both are ''holdable'' and the readings_relations field correctly codes them as ''coexists_with''. If one reading dominates and the other is rarely cited except to distinguish/reject it, the dominant reading has foreclosed the alternative — this would shift reading_relations to ''forecloses'' and the axioms of the foreclosing reading to higher impact.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_empirical_status, empirical, 'Current status of sibling readings in TRIPS panel jurisprudence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, 1995, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(trip_tr_t1995, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, theater_ratio, 1995, 0.18).
narrative_ontology:measurement(trip_tr_t2001, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, theater_ratio, 2001, 0.22).
narrative_ontology:measurement(trip_tr_t2008, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, theater_ratio, 2008, 0.28).
narrative_ontology:measurement(trip_tr_t2015, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, theater_ratio, 2015, 0.35).
narrative_ontology:measurement(trip_tr_t2020, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, theater_ratio, 2020, 0.38).
narrative_ontology:measurement(trip_tr_t2026, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, theater_ratio, 2026, 0.41).

% Extraction over time
narrative_ontology:measurement(trip_be_t1995, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, base_extractiveness, 1995, 0.35).
narrative_ontology:measurement(trip_be_t2001, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, base_extractiveness, 2001, 0.42).
narrative_ontology:measurement(trip_be_t2008, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, base_extractiveness, 2008, 0.54).
narrative_ontology:measurement(trip_be_t2015, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, base_extractiveness, 2015, 0.61).
narrative_ontology:measurement(trip_be_t2020, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, base_extractiveness, 2020, 0.66).
narrative_ontology:measurement(trip_be_t2026, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, base_extractiveness, 2026, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(trip_su_t1995, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, suppression_requirement, 1995, 0.45).
narrative_ontology:measurement(trip_su_t2001, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, suppression_requirement, 2001, 0.52).
narrative_ontology:measurement(trip_su_t2008, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, suppression_requirement, 2008, 0.62).
narrative_ontology:measurement(trip_su_t2015, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, suppression_requirement, 2015, 0.68).
narrative_ontology:measurement(trip_su_t2020, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, suppression_requirement, 2020, 0.71).
narrative_ontology:measurement(trip_su_t2026, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, suppression_requirement, 2026, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, 0.12).
narrative_ontology:affects_constraint(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, trips_agreement_interpretive_kernel__public_health_flexibility_reading).
narrative_ontology:affects_constraint(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, trips_agreement_interpretive_kernel__strong_exclusivity_reading).
narrative_ontology:affects_constraint(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, pharmaceutical_patent_protection_regimes).
narrative_ontology:affects_constraint(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, generic_drug_market_entry_barriers).
narrative_ontology:affects_constraint(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, compulsory_licensing_accessibility).

% DUAL FORMULATION NOTE:
% The TRIPS agreement kernel constraint family comprises three stories: (1) dispute_settlement_interpretive_authority (this story) — the meta-constraint on how interpretive authority is exercised and enforced; (2) public_health_flexibility_reading — the reading that TRIPS permits broad flexibility for medicine access; (3) strong_exclusivity_reading — the reading that TRIPS mandates narrow patent protection. All three stories share the same TRIPS text (kernel) but instantiate different constraints because they embody different readings of what that text means. This story (dispute settlement authority) creates the structural conditions under which one of the sibling readings is locked in place through binding precedent. The sibling readings contest the authority's conclusions, not the authority itself. The network edges reflect causal influence: dispute settlement authority affects which public health flexibility or strong exclusivity reading is institutionalized, thus affecting pharmaceutical patent regimes, generic entry barriers, and compulsory licensing accessibility downstream.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, institutional, 0.3).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
