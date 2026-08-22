% ============================================================================
% CONSTRAINT STORY: ost_article_ii_non_appropriation__international_regime
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ost_article_ii_non_appropriation__international_regime, []).

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
 *   constraint_id: ost_article_ii_non_appropriation__international_regime
 *   human_readable: OST Article II Non-Appropriation — Deferred International Regime Reading
 *   domain: international law / treaty interpretation / commons governance
 *
 * SUMMARY:
 *   The Outer Space Treaty's Article II declares celestial bodies 'not
 *   subject to national appropriation,' but the treaty is silent on whether
 *   extracted resources can be owned. Three readings contest the silence.
 *   This story instantiates the international_regime reading alone: Article
 *   II, read alongside the Moon Agreement's Article XI analogue, defers the
 *   appropriation question to a future multilateral regime, so that until
 *   such a regime forms, neither the extraction-permissive claim (private
 *   titles are already valid) nor the commons-conservation claim (extraction
 *   is already prohibited as de facto appropriation) carries treaty
 *   authority. The standing arrangement under contest is therefore maintained
 *   legal uncertainty: first-mover firms extract under domestic statutes in a
 *   grey zone, the multilateral negotiation track persists without producing
 *   an instrument, and parallel frameworks accumulate practice outside the
 *   deferred question. The claim/metric gap is deliberate and load-bearing:
 *   the constraint is CLAIMED as scaffold because its justification is the
 *   transition it enables, while the authored metrics describe a transition
 *   that is jamming — enforcement decaying, theatrical output rising,
 *   extraction accumulating slowly. The engine measures that divergence.
 *   Family note: this is one of three linked readings of the same kernel; the
 *   siblings are separate files, and this story folds no sibling content into
 *   its own classification.
 *
 * KEY AGENTS:
 *   - multilateral_space_governance_venue: agenda-setter/administrator (institutional/identity_locked) — runs the negotiation track the deferral rides on; its mandate is fused with the process
 *   - non_spacefaring_state_bloc: primary beneficiary (organized/trapped) — holds option value and blocking votes; cannot realize claims or exit the order
 *   - first_mover_extraction_firms: principal payer with secondary beneficiary position (organized/constrained) — operates in the grey zone, gaining permission but not security
 *   - space_resource_investors: payer (moderate/mobile) — capital bears title-insecurity discounts and exits rather than fights
 *   - parallel_framework_states: secondary beneficiary (powerful/mobile) — the stall feeds precedent value to their alternative track
 *   - scientific_preservation_missions: payer (moderate/constrained) — sites unprotected while site rules are bundled into the unsettled question
 *   - uncharted_commercial_operators: excluded (moderate/trapped) — same insecurity as chartered rivals, no legal shield, no seat in either track
 *   - space_law_interpretive_community: analytical observer — sees the full structure, binds no one
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ost_article_ii_non_appropriation__international_regime, 0.38).
domain_priors:suppression_score(ost_article_ii_non_appropriation__international_regime, 0.25).
domain_priors:theater_ratio(ost_article_ii_non_appropriation__international_regime, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__international_regime, extractiveness, 0.38).
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__international_regime, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__international_regime, theater_ratio, 0.52).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__international_regime, accessibility_collapse, 0.22).
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__international_regime, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ost_article_ii_non_appropriation__international_regime, scaffold).
narrative_ontology:human_readable(ost_article_ii_non_appropriation__international_regime, "OST Article II Non-Appropriation — Deferred International Regime Reading").
narrative_ontology:topic_domain(ost_article_ii_non_appropriation__international_regime, "international law / treaty interpretation / commons governance").

narrative_ontology:has_sunset_clause(ost_article_ii_non_appropriation__international_regime).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ost_article_ii_non_appropriation__international_regime, '1086de77-657a-4696-af00-b22b9e3c1fa6').
narrative_ontology:cs_kernel_codification('1086de77-657a-4696-af00-b22b9e3c1fa6', fixed_text).
narrative_ontology:cs_authority_grounding('1086de77-657a-4696-af00-b22b9e3c1fa6', distributed).
narrative_ontology:cs_reading_relation('1086de77-657a-4696-af00-b22b9e3c1fa6', ost_article_ii_non_appropriation__extraction_permissive, forecloses).
narrative_ontology:cs_reading_relation('1086de77-657a-4696-af00-b22b9e3c1fa6', ost_article_ii_non_appropriation__commons_conservation, forecloses).
narrative_ontology:cs_axiom('1086de77-657a-4696-af00-b22b9e3c1fa6', foundational, universal_consent_precondition).
narrative_ontology:cs_axiom_status(universal_consent_precondition, holdable).
narrative_ontology:cs_axiom_grounding('1086de77-657a-4696-af00-b22b9e3c1fa6', universal_consent_precondition, conventional).
narrative_ontology:cs_axiom('1086de77-657a-4696-af00-b22b9e3c1fa6', secondary, interim_non_recognition).
narrative_ontology:cs_axiom_status(interim_non_recognition, holdable).
narrative_ontology:cs_axiom_grounding('1086de77-657a-4696-af00-b22b9e3c1fa6', interim_non_recognition, conventional).
narrative_ontology:cs_reference_frame('1086de77-657a-4696-af00-b22b9e3c1fa6', provisional_reservation_pending_universal_settlement).
narrative_ontology:cs_drift_state('1086de77-657a-4696-af00-b22b9e3c1fa6', artemis_grey_zone_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('1086de77-657a-4696-af00-b22b9e3c1fa6', '').
narrative_ontology:cs_kernel_id(ost_article_ii_non_appropriation__international_regime, ost_article_ii_non_appropriation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ost_article_ii_non_appropriation__international_regime, multilateral_space_governance_venue).
narrative_ontology:constraint_beneficiary(ost_article_ii_non_appropriation__international_regime, non_spacefaring_state_bloc).
narrative_ontology:constraint_beneficiary(ost_article_ii_non_appropriation__international_regime, parallel_framework_states).
narrative_ontology:constraint_victim(ost_article_ii_non_appropriation__international_regime, first_mover_extraction_firms).
narrative_ontology:constraint_victim(ost_article_ii_non_appropriation__international_regime, space_resource_investors).
narrative_ontology:constraint_victim(ost_article_ii_non_appropriation__international_regime, scientific_preservation_missions).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(ost_article_ii_non_appropriation__international_regime, first_mover_extraction_firms).
narrative_ontology:constraint_vindicates(ost_article_ii_non_appropriation__international_regime, multilateral_consensus_precondition).
narrative_ontology:constraint_vindicates(ost_article_ii_non_appropriation__international_regime, interim_non_recognition_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Convenes the committee and working groups where the resource question has been debated for decades without producing a binding instrument. Publishes guidelines, hosts delegations, and keeps the item on the agenda session after session. Its mandate and staffing depend on the negotiation track continuing; if the question were settled in another forum, the venue would lose the item that organizes much of its program. It cannot hand the question off without dissolving its own reason for convening it.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__international_regime, multilateral_space_governance_venue, agenda_setter,
    institutional, generational, identity_locked, global).

% A large coalition of states without independent launch or extraction capability that insists no settlement is valid without their consent. Their leverage consists almost entirely of blocking votes and withheld agreement. As long as the question stays open, they retain a claim to participate in whatever allocation rules eventually emerge. They cannot realize resource claims themselves and cannot leave the legal order that governs them.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__international_regime, non_spacefaring_state_bloc, beneficiary,
    organized, generational, trapped, global).

% Chartered companies developing lunar water and asteroid metal extraction. Domestic statutes authorize their activity, but no treaty-level instrument recognizes the titles they would need to collateralize investment. They operate now because nothing prohibits them, and they bear continuous exposure because nothing secures them: insurers price the ambiguity, lenders discount reserves, and a future treaty could strand sunk capital. Relocating to another charter jurisdiction changes little, since the ambiguity is worldwide.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__international_regime, first_mover_extraction_firms, payer,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(ost_article_ii_non_appropriation__international_regime, first_mover_extraction_firms, beneficiary).

% Venture and institutional capital backing extraction ventures. Their exposure is policy risk: the asset class trades at discounts reflecting unresolved title. Capital can leave for other sectors at will, and repeatedly has; several prominent ventures wound down after failing to raise follow-on rounds, citing economics that depended on legal certainty that never arrived.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__international_regime, space_resource_investors, payer,
    moderate, biographical, mobile, global).

% States building bilateral and plurilateral frameworks — accords, domestic licensing statutes, compatibility agreements — that accumulate operating practice while the universal track stalls. Each year the multilateral process produces no instrument, their parallel frameworks gain precedent value. They remain formally committed to eventual multilateralism and remain bound by the sovereignty bar, but their practical course runs outside the deferred question.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__international_regime, parallel_framework_states, beneficiary,
    powerful, generational, mobile, global).

% Science agencies and heritage advocates concerned with landing sites, radio-quiet zones, and historically significant locations. With no regime in force there are no coordination rules for site protection during the interim: extraction traffic and hardware emplacement proceed under whatever bilateral courtesy obtains. Protective proposals go nowhere because site protection is bundled into the very allocation question the process cannot settle.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__international_regime, scientific_preservation_missions, payer,
    moderate, biographical, constrained, global).

% Companies incorporated in jurisdictions without enabling legislation or accord membership. They face the same title insecurity as chartered rivals but lack the domestic legal shield and the diplomatic backing; they hold observers' status at the committee at best and no seat in the parallel frameworks, which are invitation-only.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__international_regime, uncharted_commercial_operators, excluded,
    moderate, biographical, trapped, global).

% Academic and practitioner international lawyers who publish the competing readings, advise delegations, and track state practice. They see the full structure: which claims cite which texts, where practice is accumulating, and which arguments are doing work. Their analyses circulate freely but bind no one.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__international_regime, space_law_interpretive_community, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ost_article_ii_non_appropriation__international_regime, diffuse).
narrative_ontology:fixing_cost_class(ost_article_ii_non_appropriation__international_regime, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Holds the celestial-resource allocation question open for collective settlement: by withholding recognition from unilateral claims and parallel settlements, it prevents a first-mover scramble from locking in allocation rules before a universal framework can form, and preserves bargaining space for states that would otherwise be presented with accomplished facts.
% TRANSFER_FUNCTION: Moves settlement authority and veto power over resource allocation to the multilateral track and its blocking coalitions, and moves title-security away from first-moving operators: firms may act but cannot bank, so the arrangement transfers option value toward non-spacefaring states and agenda control toward the negotiating venue, paid for by investor discounts and stranded-capital risk borne by operators.
% ABSENT_VOICES: Commercial operators without charter-state representation hold observers' status at best and no seat in invitation-only parallel frameworks; future regime participants are by definition unidentified and unrepresented; and constituencies bound by the outcome — including populations of states that never joined either track — have no voice in either venue. They are outside the room in both tracks simultaneously.
% DISAPPEARANCE_RATIONALE: If the deferral vanished overnight — if every government agreed tomorrow that the question is settled one way or the other — the grey zone would close immediately: under a permissive settlement, title regimes would crystallize around existing operations and financing would reprice within quarters; under a conservation settlement, licensed extraction would halt pending a prohibitory instrument and charter statutes would collide with treaty obligations. Firm valuations, national space legislation, and the committee's agenda all depend on the question staying open.
% FOUNDING_PROBLEM: The 1967 bargain froze sovereignty claims over celestial bodies because both space powers feared the other's territorial advance and neither wanted a land rush; the resource question was left deliberately unresolved pending a future collective settlement, an approach later given explicit form in the Moon Agreement's undertaking to establish an international regime as exploitation becomes feasible.
% FOUNDING_PROBLEM_CORROBORATION: Treaty-era scholarship and the negotiating record corroborate the original anti-lock-in purpose from outside any current beneficiary set. On current status, corroboration splits along the same lines as the readings: framework-state statements and industry testimony attest that the multilateral track has failed to deliver and the deferral now functions as insecurity; Agreement-party statements and commons-governance scholarship attest that lock-in risk remains live. No seat outside the dispute attests a neutral answer — the contest itself is the finding.
narrative_ontology:disappearance_verdict(ost_article_ii_non_appropriation__international_regime, world_rearranges).
narrative_ontology:founding_problem_status(ost_article_ii_non_appropriation__international_regime, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ost_article_ii_non_appropriation__international_regime, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(ost_article_ii_non_appropriation__international_regime, 'none', 1).
narrative_ontology:epsilon_provenance(ost_article_ii_non_appropriation__international_regime, 0.38, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ost_article_ii_non_appropriation__international_regime_tests).
:- end_tests(ost_article_ii_non_appropriation__international_regime_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Epsilon 0.38: the referent is the standing arrangement — the maintained deferral — assessed by this reading's own lights, which treat its costs as transitional friction rather than designed extraction: agenda-control rents at the venue, preserved option value for the bloc, and title insecurity for operators sum to moderate-low extraction. Suppression 0.25 is a raw structural property (diplomatic non-recognition and veto points), unscaled by construction. Theater 0.52: guideline production, working-group communiqués, and anniversary declarations now outpace movement on the core question. Accessibility_collapse 0.22 is the reading's defining feature: both alternative settlements remain live and actively pursued, so alternatives have not collapsed. Resistance 0.68: the arrangement is squeezed from both flanks simultaneously — framework states routing around it, Agreement bloc pushing past it. The temporal series run on one shared nine-point grid (1979–2025) with every tracked metric authored at every point. The suppression_requirement series is authored deliberately as an enforcement-decay trajectory: Cold-War-era joint opposition to claims hardened into routine, then decayed into rhetorical objection as domestic statutes and parallel frameworks normalized practice — the story tracks enforcement-capacity change, so the series is warranted rather than redundant with the scalar. Rising base_extractiveness models slow accumulation as the transition logic decays into positional holding. Drift is monotonic, not cyclical: accumulated practice is a one-way ratchet.
 *
 * PERSPECTIVAL GAP:
 *   Seat divergence is extreme here. The venue seat computes a world where the arrangement is its mandate — identity fused with the process, since exit would dissolve the item that organizes the institution. The bloc seat computes protection: trapped beneficiaries whose only asset is the open question itself. The firm seat computes insecurity: constrained payers bearing discounted collateral and stranded-capital risk. The investor seat computes the same insecurity with mobile exit, which is why capital leaves rather than contests. Inter-institutionally, framework states experience the deferral as a recognition constraint they route around while experiencing their own frameworks as freedom. Same-level lateral: chartered and uncharted firms sit at the same nominal power level facing identical treaty ambiguity, but charter jurisdiction functions as partial shelter, so the same structure lands differently across equal-standing actors. The engine computes these divergences from the power/exit/role data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations (venue, bloc, framework states) drive low directionality — subsidy-side seats. Victim declarations (firms, investors, science missions) drive high directionality — target-side seats. The firms' dual position is encoded structurally rather than overridden: they appear in victims[] and carry secondary_role beneficiary, because the same uncertainty that denies them secure title also shields their current operations from prohibition — their derived directionality should land mid-range rather than at the target pole. Mobile exit pulls investors below trapped payers at equal victim status. Global scope raises verification difficulty modestly, amplifying effective extraction for target-side seats. No directionality overrides were authored: the structural data is the primary input, and the override mechanism keys on power atoms, so no available correction could be applied surgically without mislabeling other agents sharing the same atom.
 *
 * MANDATROPHY ANALYSIS:
 *   Claiming scaffold rather than rope preserves the arrangement's transitional self-understanding: its justification is the regime to come, not the steady state, and it carries a feasibility-triggered sunset — the Article XI analogue undertakes to establish the regime as exploitation becomes feasible. The receipt surface records the jam as descriptive fact: gains are diffuse (the venue collects relevance, the bloc holds unrealized option value, and no seat captures the insecurity costs the arrangement imposes), and fixing is prohibitive (zero-sum distributional conflict means any settlement concentrates losses somewhere, exceeding any fixer's willingness). Diffuse-plus-prohibitive is the cell the corpus associates with piton drift — recorded here without reclassification, because the sunset's trigger condition is still live: if exploitation becomes feasible while no regime forms, the transitional justification lapses and piton or snare drift should be scored. The R5 interview marks the founding problem contested: the anti-lock-in problem Article II originally solved is widely attested as solved, while the resource-allocation problem the deferral now manages is disputed between live and manufactured. The status-times-verdict mismatch consumer should watch this story.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_authority_underdetermination,
    'This constraint instantiates the international_regime reading of kernel ost_article_ii_non_appropriation (siblings: extraction_permissive, commons_conservation). Is the deferral reading itself still authoritative, or does accumulated practice under parallel frameworks already constitute the multilateral consent-in-effect that the reading''s own criterion requires?',
    'State-practice census: measure whether operations under parallel frameworks achieve near-universal coverage (satisfying the consent criterion through a different vehicle, collapsing this reading into extraction_permissive in effect) or remain a minority bloc (deferral retains force and this story''s classification stands).',
    'Quasi-universal parallel practice transfers this story''s epsilon and classification to the extraction_permissive sibling; minority practice confirms the scaffold classification and keeps the three-way family decomposition intact.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_authority_underdetermination, conceptual, 'Whether the deferral reading survives its own consent criterion as parallel frameworks accumulate practice.').

omega_variable(
    feasibility_trigger_status,
    'Has the Article XI-analogue sunset trigger (''exploitation about to become feasible'') already fired, converting the deferral from a live transitional arrangement into a jammed one whose justification has lapsed?',
    'Engineering-economic assessment of lunar and asteroid resource cost curves against terrestrial supply (water ice, platinum-group metals, helium-3): if commercial feasibility is reached while no regime forms, the sunset has lapsed unexecuted.',
    'If the trigger has fired, the transitional justification lapses and the maintained uncertainty becomes inertial maintenance or positional-veto cover, warranting piton or snare drift scoring; if feasibility has not arrived, the arrangement remains legitimately transitional.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(feasibility_trigger_status, empirical, 'Status of the feasibility-triggered sunset clause on the deferral arrangement.').

omega_variable(
    distributional_structure_of_stalemate,
    'Is the negotiation stall genuinely zero-sum (any settlement necessarily concentrates losses, making deferral rational for all parties) or an artifact of blocking-coalition strategy that a package deal with side-payments and phased rights could dissolve?',
    'Comparative negotiation analysis of COPUOS working papers against resolved commons analogues (deep-seabed Part XI compromise, ITU orbital-slot allocation): did comparable disputes close via package deals once feasibility neared, and what blocking structures differed?',
    'If positive-sum paths exist, the deferral''s persistence indicts the process rather than the structure and strengthens snare-drift scoring; if truly zero-sum, the deferral is the least-cost holding pattern and its measured extraction is largely irreducible coordination cost.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(distributional_structure_of_stalemate, empirical, 'Whether the stalled regime negotiation reflects irreducible distributional conflict or soluble bargaining failure.').

omega_variable(
    grey_zone_fait_accompli_risk,
    'Will accumulated practice under parallel frameworks harden into customary law before the regime forms, retroactively validating the extraction-permissive reading and voiding the deferral?',
    'Track the ratio of extraction operations conducted under parallel-framework licenses versus multilateral-process endorsement over the coming decade, together with state protest and acquiescence patterns toward specific licenses.',
    'Customary hardening converts this scaffold into a failed transition: the transition completes outside the arrangement, and the deferral''s residual function becomes purely theatrical, driving theater_ratio toward piton levels.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(grey_zone_fait_accompli_risk, empirical, 'Risk that grey-zone practice consolidates into custom before the regime arrives.').

omega_variable(
    non_recognition_enforcement_basis,
    'Does the deferral''s maintenance rest on binding legal obligation enforced against claims, or on political non-recognition that any determined coalition can route around?',
    'Legal-doctrinal analysis comparing state responses to territorial claims with responses to resource-extraction licenses: has any license drawn a formal protest invoking Article II, or only rhetorical objection?',
    'If only rhetorical, the authored suppression overstates durable force and the arrangement''s stability depends entirely on great-power restraint, accelerating every drift scenario in the other omegas.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(non_recognition_enforcement_basis, empirical, 'Legal versus political basis of the non-recognition practice sustaining the deferral.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ost_article_ii_non_appropriation__international_regime, 1979, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ost__tr_t1979, ost_article_ii_non_appropriation__international_regime, theater_ratio, 1979, 0.2).
narrative_ontology:measurement_basis(ost__tr_t1979, observed).
narrative_ontology:measurement(ost__tr_t1985, ost_article_ii_non_appropriation__international_regime, theater_ratio, 1985, 0.28).
narrative_ontology:measurement_basis(ost__tr_t1985, observed).
narrative_ontology:measurement(ost__tr_t1991, ost_article_ii_non_appropriation__international_regime, theater_ratio, 1991, 0.34).
narrative_ontology:measurement_basis(ost__tr_t1991, observed).
narrative_ontology:measurement(ost__tr_t1997, ost_article_ii_non_appropriation__international_regime, theater_ratio, 1997, 0.4).
narrative_ontology:measurement_basis(ost__tr_t1997, observed).
narrative_ontology:measurement(ost__tr_t2003, ost_article_ii_non_appropriation__international_regime, theater_ratio, 2003, 0.44).
narrative_ontology:measurement_basis(ost__tr_t2003, observed).
narrative_ontology:measurement(ost__tr_t2009, ost_article_ii_non_appropriation__international_regime, theater_ratio, 2009, 0.47).
narrative_ontology:measurement_basis(ost__tr_t2009, observed).
narrative_ontology:measurement(ost__tr_t2015, ost_article_ii_non_appropriation__international_regime, theater_ratio, 2015, 0.49).
narrative_ontology:measurement_basis(ost__tr_t2015, observed).
narrative_ontology:measurement(ost__tr_t2020, ost_article_ii_non_appropriation__international_regime, theater_ratio, 2020, 0.51).
narrative_ontology:measurement_basis(ost__tr_t2020, observed).
narrative_ontology:measurement(ost__tr_t2025, ost_article_ii_non_appropriation__international_regime, theater_ratio, 2025, 0.52).
narrative_ontology:measurement_basis(ost__tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(ost__be_t1979, ost_article_ii_non_appropriation__international_regime, base_extractiveness, 1979, 0.24).
narrative_ontology:measurement_basis(ost__be_t1979, observed).
narrative_ontology:measurement(ost__be_t1985, ost_article_ii_non_appropriation__international_regime, base_extractiveness, 1985, 0.26).
narrative_ontology:measurement_basis(ost__be_t1985, observed).
narrative_ontology:measurement(ost__be_t1991, ost_article_ii_non_appropriation__international_regime, base_extractiveness, 1991, 0.28).
narrative_ontology:measurement_basis(ost__be_t1991, observed).
narrative_ontology:measurement(ost__be_t1997, ost_article_ii_non_appropriation__international_regime, base_extractiveness, 1997, 0.29).
narrative_ontology:measurement_basis(ost__be_t1997, observed).
narrative_ontology:measurement(ost__be_t2003, ost_article_ii_non_appropriation__international_regime, base_extractiveness, 2003, 0.31).
narrative_ontology:measurement_basis(ost__be_t2003, observed).
narrative_ontology:measurement(ost__be_t2009, ost_article_ii_non_appropriation__international_regime, base_extractiveness, 2009, 0.33).
narrative_ontology:measurement_basis(ost__be_t2009, observed).
narrative_ontology:measurement(ost__be_t2015, ost_article_ii_non_appropriation__international_regime, base_extractiveness, 2015, 0.35).
narrative_ontology:measurement_basis(ost__be_t2015, observed).
narrative_ontology:measurement(ost__be_t2020, ost_article_ii_non_appropriation__international_regime, base_extractiveness, 2020, 0.37).
narrative_ontology:measurement_basis(ost__be_t2020, observed).
narrative_ontology:measurement(ost__be_t2025, ost_article_ii_non_appropriation__international_regime, base_extractiveness, 2025, 0.38).
narrative_ontology:measurement_basis(ost__be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(ost__su_t1979, ost_article_ii_non_appropriation__international_regime, suppression_requirement, 1979, 0.55).
narrative_ontology:measurement_basis(ost__su_t1979, observed).
narrative_ontology:measurement(ost__su_t1985, ost_article_ii_non_appropriation__international_regime, suppression_requirement, 1985, 0.5).
narrative_ontology:measurement_basis(ost__su_t1985, observed).
narrative_ontology:measurement(ost__su_t1991, ost_article_ii_non_appropriation__international_regime, suppression_requirement, 1991, 0.45).
narrative_ontology:measurement_basis(ost__su_t1991, observed).
narrative_ontology:measurement(ost__su_t1997, ost_article_ii_non_appropriation__international_regime, suppression_requirement, 1997, 0.41).
narrative_ontology:measurement_basis(ost__su_t1997, observed).
narrative_ontology:measurement(ost__su_t2003, ost_article_ii_non_appropriation__international_regime, suppression_requirement, 2003, 0.37).
narrative_ontology:measurement_basis(ost__su_t2003, observed).
narrative_ontology:measurement(ost__su_t2009, ost_article_ii_non_appropriation__international_regime, suppression_requirement, 2009, 0.33).
narrative_ontology:measurement_basis(ost__su_t2009, observed).
narrative_ontology:measurement(ost__su_t2015, ost_article_ii_non_appropriation__international_regime, suppression_requirement, 2015, 0.3).
narrative_ontology:measurement_basis(ost__su_t2015, observed).
narrative_ontology:measurement(ost__su_t2020, ost_article_ii_non_appropriation__international_regime, suppression_requirement, 2020, 0.27).
narrative_ontology:measurement_basis(ost__su_t2020, observed).
narrative_ontology:measurement(ost__su_t2025, ost_article_ii_non_appropriation__international_regime, suppression_requirement, 2025, 0.25).
narrative_ontology:measurement_basis(ost__su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ost_article_ii_non_appropriation__international_regime, resource_allocation).
narrative_ontology:affects_constraint(ost_article_ii_non_appropriation__international_regime, ost_article_ii_non_appropriation__extraction_permissive).
narrative_ontology:affects_constraint(ost_article_ii_non_appropriation__international_regime, ost_article_ii_non_appropriation__commons_conservation).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition per the epsilon-invariance principle: the colloquial label 'Article II non-appropriation' covers three structurally distinct claims that must not share one story. The settled upstream core (states may not appropriate celestial territory) is common ground; the readings diverge on resource-title authority, and their epsilon values differ accordingly. This file instantiates the international_regime reading (deferral pending multilateral framework; moderate-low epsilon, transitional justification). The extraction_permissive sibling (private titles already valid) and the commons_conservation sibling (extraction already prohibited as de facto appropriation) are separate files with their own epsilon, victim sets, and failure modes. Upstream, the sovereignty-bar core influences all three readings; downstream, whichever reading hardens first will absorb or extinguish the other two.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
