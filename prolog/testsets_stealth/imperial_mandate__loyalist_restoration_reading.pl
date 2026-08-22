% ============================================================================
% CONSTRAINT STORY: imperial_mandate__loyalist_restoration_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_imperial_mandate__loyalist_restoration_reading, []).

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
 *   constraint_id: imperial_mandate__loyalist_restoration_reading
 *   human_readable: Imperial Mandate — Loyalist Restoration Reading (Unmediated Sovereignty Requirement)
 *   domain: political philosophy/comparative constitutional systems/east asian history
 *
 * SUMMARY:
 *   The Tokugawa settlement (1603) fixed a dual-legitimacy constitution: the
 *   imperial line confers legitimacy; the shogunate exercises governance.
 *   This story instantiates ONE reading of that contested kernel — the
 *   loyalist_restoration_reading, articulated by Mito-school scholars,
 *   kokugaku revivalists, and sonnō jōi activists — which holds that divine
 *   mandate requires UNMEDIATED exercise of sovereignty and that legitimacy
 *   is therefore inseparable from active imperial governance. By this
 *   reading's own lights, the standing arrangement under contest (the
 *   delegated bakuhan order) is usurpation: the substance of sovereignty
 *   diverted to an intermediary class that rules through a legitimacy grant
 *   it both requires and resents. The ε referent is accordingly the standing
 *   delegated arrangement as the loyalist reading assesses it — hence high
 *   extraction — NOT the restored direct-rule order the reading endorses. Per
 *   the ε-invariance principle, the colloquial label 'imperial mandate'
 *   decomposes into two structurally distinct constraints: this file
 *   (inseparable legitimacy, high ε) and the sibling
 *   bakufu_delegation_reading (separable legitimation, materially lower ε for
 *   the same arrangement), linked via network.affects_constraints. The
 *   claim/metric gap is deliberate: the constraint is CLAIMED as tangled_rope
 *   (the settlement genuinely coordinated peace while extracting sovereignty)
 *   while the authored metrics describe heavily extractive, actively enforced
 *   operation — the engine measures the divergence; the claim is not tuned to
 *   any predicted output.
 *
 * KEY AGENTS:
 *   - shogunal_house: agenda-setter (institutional/arbitrage) — holds governing power under the legitimacy grant it administers and enforces
 *   - imperial_house: primary target (moderate/identity_locked) — sovereignty extracted while the role itself constitutes the house
 *   - court_nobility: secondary target (powerless/trapped) — offices without function, stipends under bakufu supervision
 *   - samurai_estate: dual-positioned beneficiary-payer (organized/identity_locked) — status guaranteed, service compulsory
 *   - tozama_daimyo: resentful payer with retained capacity (powerful/constrained) — excluded from office, armed enough to matter
 *   - fudai_daimyo: office-rent beneficiary (organized/constrained) — careers constituted by the settlement
 *   - loyalist_activists: excluded claimants (moderate/identity_locked) — the suppressed voice whose exclusion is the enforcement object
 *   - comparative_constitutional_historians: analytical observer (analytical/analytical) — sees the full two-reading structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(imperial_mandate__loyalist_restoration_reading, 0.82).
domain_priors:suppression_score(imperial_mandate__loyalist_restoration_reading, 0.7).
domain_priors:theater_ratio(imperial_mandate__loyalist_restoration_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(imperial_mandate__loyalist_restoration_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(imperial_mandate__loyalist_restoration_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(imperial_mandate__loyalist_restoration_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(imperial_mandate__loyalist_restoration_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(imperial_mandate__loyalist_restoration_reading, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(imperial_mandate__loyalist_restoration_reading, tangled_rope).
narrative_ontology:human_readable(imperial_mandate__loyalist_restoration_reading, "Imperial Mandate — Loyalist Restoration Reading (Unmediated Sovereignty Requirement)").
narrative_ontology:topic_domain(imperial_mandate__loyalist_restoration_reading, "political philosophy/comparative constitutional systems/east asian history").

domain_priors:requires_active_enforcement(imperial_mandate__loyalist_restoration_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(imperial_mandate__loyalist_restoration_reading, '3e9fda61-caf1-4fec-9bd3-f075cc1c2091').
narrative_ontology:cs_kernel_codification('3e9fda61-caf1-4fec-9bd3-f075cc1c2091', fixed_text).
narrative_ontology:cs_authority_grounding('3e9fda61-caf1-4fec-9bd3-f075cc1c2091', lineage).
narrative_ontology:cs_interpretation_layer_present('3e9fda61-caf1-4fec-9bd3-f075cc1c2091').
narrative_ontology:cs_reading_relation('3e9fda61-caf1-4fec-9bd3-f075cc1c2091', imperial_mandate__bakufu_delegation_reading, forecloses).
narrative_ontology:cs_axiom('3e9fda61-caf1-4fec-9bd3-f075cc1c2091', foundational, legitimacy_inseparable_from_active_reign).
narrative_ontology:cs_axiom_status(legitimacy_inseparable_from_active_reign, holdable).
narrative_ontology:cs_axiom_grounding('3e9fda61-caf1-4fec-9bd3-f075cc1c2091', legitimacy_inseparable_from_active_reign, theological).
narrative_ontology:cs_axiom('3e9fda61-caf1-4fec-9bd3-f075cc1c2091', secondary, delegated_governance_constitutes_usurpation).
narrative_ontology:cs_axiom_status(delegated_governance_constitutes_usurpation, holdable).
narrative_ontology:cs_axiom_grounding('3e9fda61-caf1-4fec-9bd3-f075cc1c2091', delegated_governance_constitutes_usurpation, theological).
narrative_ontology:cs_reference_frame('3e9fda61-caf1-4fec-9bd3-f075cc1c2091', ancient_direct_reign_charter).
narrative_ontology:cs_drift_state('3e9fda61-caf1-4fec-9bd3-f075cc1c2091', late_edo_foreign_pressure, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('3e9fda61-caf1-4fec-9bd3-f075cc1c2091', '').
narrative_ontology:cs_kernel_id(imperial_mandate__loyalist_restoration_reading, imperial_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(imperial_mandate__loyalist_restoration_reading, shogunal_house).
narrative_ontology:constraint_beneficiary(imperial_mandate__loyalist_restoration_reading, fudai_daimyo).
narrative_ontology:constraint_beneficiary(imperial_mandate__loyalist_restoration_reading, samurai_estate).
narrative_ontology:constraint_victim(imperial_mandate__loyalist_restoration_reading, imperial_house).
narrative_ontology:constraint_victim(imperial_mandate__loyalist_restoration_reading, court_nobility).
narrative_ontology:constraint_victim(imperial_mandate__loyalist_restoration_reading, loyalist_activists).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(imperial_mandate__loyalist_restoration_reading, tozama_daimyo).
narrative_ontology:constraint_victim(imperial_mandate__loyalist_restoration_reading, samurai_estate).
narrative_ontology:constraint_victim(imperial_mandate__loyalist_restoration_reading, tozama_daimyo).
narrative_ontology:constraint_vindicates(imperial_mandate__loyalist_restoration_reading, delegation_legitimacy_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds actual governing power — land taxation, warfare, foreign trade, and the regulation of the court itself — under a legitimacy grant it periodically renews through ceremony and tribute. Writes and enforces the rules binding the imperial house (the Kinchū narabini kuge shohatto of 1615), approves court marriages and successions, and can reshape the arrangement (as with the Kōbu gattai marriage policy) but cannot abandon the grant without destroying the legitimacy its own rule runs on.
narrative_ontology:constraint_stakeholder(imperial_mandate__loyalist_restoration_reading, shogunal_house, agenda_setter,
    institutional, generational, arbitrage, national).

% Performs the rites and confers the titles on which the bakufu's legality formally depends, while its income, marriages, movements, and even abdications are administered and restricted by the bakufu. Successive emperors petitioned for restoration of direct governing authority and were refused. Exit from the role is unavailable because the house is itself the legitimacy source — ceasing to be the sacred line would dissolve the very asset whose substance is being diverted.
narrative_ontology:constraint_stakeholder(imperial_mandate__loyalist_restoration_reading, imperial_house, payer,
    moderate, generational, identity_locked, national).

% Hereditary kuge families holding ancient court offices stripped of administrative function, living on stipends whose administration the bakufu supervises. Participation in loyalist plotting meant exile, execution, or house destruction; there is no economic or geographic exit from Kyoto's court society.
narrative_ontology:constraint_stakeholder(imperial_mandate__loyalist_restoration_reading, court_nobility, payer,
    powerless, generational, trapped, local).

% Hereditary vassal lords who staff the senior bakufu magistracies and draw office rents, domain guarantees, and career continuity from the arrangement's continuation. Their families' standing is a product of the delegation settlement; abandoning it would mean forfeiting office, protection, and rank simultaneously.
narrative_ontology:constraint_stakeholder(imperial_mandate__loyalist_restoration_reading, fudai_daimyo, beneficiary,
    organized, biographical, constrained, regional).

% The hereditary warrior class whose stipends, legal status, and monopolized right of arms the settlement guarantees. It pays in compulsory service, alternate-year residence burdens passed down from daimyo households, and a life bounded by class law. Leaving the class means losing livelihood and honor together; the service identity is constitutive, not contractual.
narrative_ontology:constraint_stakeholder(imperial_mandate__loyalist_restoration_reading, samurai_estate, beneficiary,
    organized, biographical, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(imperial_mandate__loyalist_restoration_reading, samurai_estate, payer).

% Outside lords — Satsuma, Chōshū, and others — excluded from senior bakufu office and kept under surveillance. They bear sankin-kōtai residence costs, confiscation risk, and formal subordination, while sharing in the general peace the settlement provides. Their distance from the center preserved the fiscal and military capacity they ultimately lent to the restoration cause.
narrative_ontology:constraint_stakeholder(imperial_mandate__loyalist_restoration_reading, tozama_daimyo, payer,
    powerful, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(imperial_mandate__loyalist_restoration_reading, tozama_daimyo, beneficiary).

% Scholars and activists of the Mito school, kokugaku scholarship, and later sonnō jōi networks who hold that direct imperial rule is the only legitimate order. They are censored, imprisoned, or executed (Yoshida Shōin among them), and are barred from the councils where the settlement's terms are set. Their exclusion from the conversation is precisely what their claim protests; their identity is fused with the cause, making silence or collaboration unbearable.
narrative_ontology:constraint_stakeholder(imperial_mandate__loyalist_restoration_reading, loyalist_activists, excluded,
    moderate, generational, identity_locked, national).

% Modern scholars comparing Japan's dual-legitimacy settlement with European regencies, papal-imperial relations, and other sacral-monarchy systems. They reconstruct both readings' genealogies and the restoration's institutional consequences without holding a seat in the arrangement.
narrative_ontology:constraint_stakeholder(imperial_mandate__loyalist_restoration_reading, comparative_constitutional_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(imperial_mandate__loyalist_restoration_reading, shogunal_house).
narrative_ontology:fixing_cost_class(imperial_mandate__loyalist_restoration_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The delegated settlement solved a concrete post-civil-war problem: after roughly a century and a half of Sengoku fragmentation, it separated sacral legitimation (the imperial line) from coercive governance (a single military hegemon), producing a durable monopoly on large-scale violence and some two and a half centuries of internal peace.
% TRANSFER_FUNCTION: Moves land-tax revenue and military service from the provinces through the daimyo to the bakufu; moves legitimation (titles, imperial sanction, calendrical authority) from the court to the shogunate; moves stipends outward to the court aristocracy and the samurai estate in exchange for ritual performance and standing service.
% ABSENT_VOICES: Loyalist scholars and activists who held direct rule to be the only legitimate order were censored, exiled, or executed rather than seated at any negotiating table; the peasantry, whose taxes and obedience were routed through the delegated chain, had no voice at all in the legitimacy question their payments sustained.
% DISAPPEARANCE_RATIONALE: If the delegated arrangement vanished overnight, the realm would immediately confront the unanswered question of who holds coercive authority: the bakufu's tax and command structure, the daimyo's domain autonomy, and the court's ritual economy all presuppose it. The Meiji restoration demonstrates the dependence empirically — when the arrangement broke, the entire bakuhan order was dismantled and rebuilt around direct imperial rule within a generation.
% FOUNDING_PROBLEM: Terminating the Sengoku civil wars by fixing a durable allocation of authority: sacred legitimacy to the throne, war-making and administration to a single military hegemon.
% FOUNDING_PROBLEM_CORROBORATION: Mito-school scholars — themselves persecuted outsiders to the benefiting parties — attested from the late eighteenth century that the war-termination problem was long solved and that the arrangement persisted on borrowed justification. Dutch-studies scholars and, later, Perry-era foreign observers independently recorded that the settlement could not answer challenges it had not been built for. Only the bakufu's own succession justifications (foreign defense requiring continued bakuhan order) attest liveness, and those come from inside the beneficiary set.
narrative_ontology:disappearance_verdict(imperial_mandate__loyalist_restoration_reading, world_rearranges).
narrative_ontology:founding_problem_status(imperial_mandate__loyalist_restoration_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(imperial_mandate__loyalist_restoration_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(imperial_mandate__loyalist_restoration_reading, 'none', 1).
narrative_ontology:epsilon_provenance(imperial_mandate__loyalist_restoration_reading, 0.82, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(imperial_mandate__loyalist_restoration_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(imperial_mandate__loyalist_restoration_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(imperial_mandate__loyalist_restoration_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is high (0.82) because, by this reading's lights, the arrangement diverts the substance of sovereignty — taxation, war-making, appointment — through an authority chain whose legitimacy is contested at its root, and because the diversion deepened over the interval as fiscal crisis layered new levies onto the old settlement. Suppression (0.70) is a raw structural property, unscaled by power or scope: court restrictions codified in 1615, censorship of heterodox learning, and execution of loyalist activists were the arrangement's own maintenance costs. Theater (0.55) reflects the growing share of the arrangement's legitimation activity that was performative — renewal ceremonies and tribute rituals sustaining an appearance of harmonious delegation while the governing pretense of the court hollowed out. Accessibility_collapse is moderate (0.50): the alternative (direct rule) was dangerous but never unthinkable, and its enactment in 1868 proves the alternative space never fully closed. Resistance is high (0.78): loyalism persisted across Nanbokuchō memory, Mito scholarship, and sonnō jōi activism for the entire interval. The measurement series run on ONE shared time grid (all three metrics at all six points). The suppression series is deliberately non-monotonic: enforcement intensified through the Ansei-era persecutions, then collapsed catastrophically in 1867–68 when the enforcement apparatus disintegrated and the succession was decided militarily — the scalar 0.70 characterizes the mature arrangement, not the terminal year.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat and the payer seats should compute differently: from the shogunal position the arrangement is a coordination order it built, maintains, and legitimately heads; from the imperial and court positions the same structure operates as enforced usurpation of a legitimacy source that cannot exit its own role. The samurai seat straddles the divide — guaranteed status on one side, compulsory service and class imprisonment on the other. A further gap runs BETWEEN stories rather than seats: the sibling bakufu_delegation_reading authors the same standing arrangement as low-extraction coordination, which is exactly why the two files exist as separate constraints rather than one story with a measurement parameter.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations map to low directionality: the shogunal house (arbitrage-grade exit — it controls the enforcement machinery) sits nearest the beneficiary end; fudai daimyo collect office rents with constrained exit. Victim declarations map to high directionality: the imperial house sits near the full-target end, amplified by identity_lock — the house cannot exit the role whose substance is being extracted; court nobility are fully trapped. The samurai estate derives mostly-beneficiary d with a payer correction for compulsory service. The tozama daimyo are the interesting case: formally beneficiaries of the peace, substantively payers of exclusion and surveillance costs with retained coercive capacity — their eventual role as the restoration's army is the structural signature of that mid-to-high d. Loyalist activists carry high d despite their excluded role: the arrangement's suppression is aimed precisely at them, and their identity lock removes exit by conviction. No directionality_overrides are authored: the beneficiary/victim declarations plus exit options already produce the correct per-seat structure, and the schema's override mechanism is keyed by power atom, which is too coarse to distinguish the two daimyo classes without collateral distortion.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — terminating the Sengoku civil wars — was solved generations before the interval's end, yet the arrangement persisted for another century and a half on successor justifications (domestic order, then foreign defense). The R5 mismatch (founding_problem_status=dead combined with disappearance_verdict=world_rearranges) is the honest zombie signature: a world-rearranging structure maintained well past its founding function, kept alive by enforcement and by the concentrated interests of its office-rent beneficiaries. Claiming tangled_rope rather than snare keeps the record honest in both directions: it preserves the real coordination achievement (the peace was real, and erasing it would flatter the loyalist polemic into inaccuracy) while the rising extraction series documents rent accumulating on top of a dead mandate. The classification thereby blocks two symmetric errors: reading the whole settlement as pure theft (which would erase the peace it delivered) and reading it as a timeless natural order (which would erase the usurpation claim the loyalist reading exists to press).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This constraint is one reading of the imperial_mandate kernel — the loyalist_restoration_reading. What would the sibling bakufu_delegation_reading change structurally?',
    'Compare the sibling story''s authored epsilon and beneficiary/victim structure for the same standing bakuhan arrangement. The disagreement is located in a single structural element: whether the legitimacy-granting function is separable from the governing function.',
    'Under the sibling reading the same arrangement computes as low-extraction coordination rather than usurpation; classification of the arrangement is reading-indexed, and cross-reading comparison is valid only between the two files, never by averaging inside one.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Committer structure: this file instantiates one of two readings of the imperial_mandate kernel; the sibling flips the separability premise and with it epsilon.').

omega_variable(
    mandate_naturalness_ambiguity,
    'Is the requirement that legitimacy be exercised unmediated by the emperor a structural feature of Japanese political order (a natural-law-like limit on delegation), or a constructed claim serving restoration-faction interests?',
    'Comparative analysis of other sacral-monarchy systems in which delegation proved stable across centuries (papal-imperial relations, the British crown-in-parliament settlement, devaraja kingship): if unmediated exercise is nowhere else required for stability, the necessity claim is constructed rather than structural.',
    'If constructed, the constraint''s apparent inevitability collapses and the 1868 rupture reads as factional victory rather than structural correction; if structural, the delegated arrangement was unstable by design and the loyalist diagnosis stands as description, not polemic.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandate_naturalness_ambiguity, conceptual, 'Whether unmediated-sovereignty legitimacy is a discovered limit or an invented instrument.').

omega_variable(
    exogenous_shock_counterfactual,
    'Was the delegated arrangement''s collapse driven by its internal extraction profile (validating the loyalist inherent-instability claim), or contingent on the Perry shock and the unequal treaties?',
    'Counterfactual modeling of the bakufu fiscal trajectory absent foreign pressure, benchmarked against the arrangement''s successful handling of earlier internal crises (the Shimabara rebellion, the Tenpō famines).',
    'If collapse required exogenous shock, the arrangement''s steady-state extraction was sustainable and the loyalist instability premise weakens; if internal dynamics alone suffice, the reading''s structural claim strengthens and the terminal classification shifts toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exogenous_shock_counterfactual, empirical, 'Internal-dynamics versus exogenous-shock account of the 1868 rupture.').

omega_variable(
    imperial_house_agency_ambiguity,
    'Was the imperial house a pure extraction target, or a strategic participant that at times colluded with the delegation (court-bakufu marriage alliances, Kōbu gattai support, negotiated stipend increases)?',
    'Court diaries and bakufu correspondence tracing which restoration petitions originated inside the house versus activist networks; analysis of house behavior whenever enhanced stipends or marriage alliances were on offer.',
    'If the house strategically participated, its effective directionality sits below the full-target end and part of the measured extraction reflects a bargain rather than pure usurpation; if the house was consistently coerced, the victim declaration stands undiluted and the high epsilon is confirmed from the primary seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(imperial_house_agency_ambiguity, empirical, 'Victim-purity versus strategic-participation account of the imperial house''s position.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(imperial_mandate__loyalist_restoration_reading, 1603, 1868).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(impe_tr_t1603, imperial_mandate__loyalist_restoration_reading, theater_ratio, 1603, 0.3).
narrative_ontology:measurement(impe_tr_t1660, imperial_mandate__loyalist_restoration_reading, theater_ratio, 1660, 0.36).
narrative_ontology:measurement(impe_tr_t1720, imperial_mandate__loyalist_restoration_reading, theater_ratio, 1720, 0.43).
narrative_ontology:measurement(impe_tr_t1780, imperial_mandate__loyalist_restoration_reading, theater_ratio, 1780, 0.49).
narrative_ontology:measurement(impe_tr_t1840, imperial_mandate__loyalist_restoration_reading, theater_ratio, 1840, 0.57).
narrative_ontology:measurement(impe_tr_t1868, imperial_mandate__loyalist_restoration_reading, theater_ratio, 1868, 0.66).

% Extraction over time
narrative_ontology:measurement(impe_be_t1603, imperial_mandate__loyalist_restoration_reading, base_extractiveness, 1603, 0.55).
narrative_ontology:measurement(impe_be_t1660, imperial_mandate__loyalist_restoration_reading, base_extractiveness, 1660, 0.62).
narrative_ontology:measurement(impe_be_t1720, imperial_mandate__loyalist_restoration_reading, base_extractiveness, 1720, 0.68).
narrative_ontology:measurement(impe_be_t1780, imperial_mandate__loyalist_restoration_reading, base_extractiveness, 1780, 0.73).
narrative_ontology:measurement(impe_be_t1840, imperial_mandate__loyalist_restoration_reading, base_extractiveness, 1840, 0.78).
narrative_ontology:measurement(impe_be_t1868, imperial_mandate__loyalist_restoration_reading, base_extractiveness, 1868, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(impe_su_t1603, imperial_mandate__loyalist_restoration_reading, suppression_requirement, 1603, 0.5).
narrative_ontology:measurement(impe_su_t1660, imperial_mandate__loyalist_restoration_reading, suppression_requirement, 1660, 0.62).
narrative_ontology:measurement(impe_su_t1720, imperial_mandate__loyalist_restoration_reading, suppression_requirement, 1720, 0.63).
narrative_ontology:measurement(impe_su_t1780, imperial_mandate__loyalist_restoration_reading, suppression_requirement, 1780, 0.66).
narrative_ontology:measurement(impe_su_t1840, imperial_mandate__loyalist_restoration_reading, suppression_requirement, 1840, 0.71).
narrative_ontology:measurement(impe_su_t1868, imperial_mandate__loyalist_restoration_reading, suppression_requirement, 1868, 0.28).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(imperial_mandate__loyalist_restoration_reading, resource_allocation).
narrative_ontology:affects_constraint(imperial_mandate__loyalist_restoration_reading, bakufu_delegation_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition per the epsilon-invariance principle: the colloquial label 'imperial mandate' conflates two structurally distinct claims about the same standing bakuhan arrangement. This file instantiates the loyalist_restoration_reading (legitimacy inseparable from active imperial governance; epsilon high, ~0.82, because the arrangement is usurpation by this reading's lights). The sibling bakufu_delegation_reading instantiates the separable-legitimation claim (epsilon materially lower for the identical arrangement, because delegation is legitimate division of labor by its lights). Same referent, different readings, different epsilons — hence two files, not one story with a measurement parameter. Direction of influence ran both ways across the interval: the delegation reading was the operative settlement for 250 years and structured the conditions the loyalist reading reacted against; the loyalist reading's 1868 victory retroactively destroyed the sibling's legitimacy conditions. Each file links the other via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
