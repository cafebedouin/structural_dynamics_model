% ============================================================================
% CONSTRAINT STORY: imperial_mandate__loyalist_restoration_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:measurement_basis/2,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: imperial_mandate__loyalist_restoration_reading
 *   human_readable: Divine Mandate Requires Unmediated Imperial Sovereignty (Loyalist-Restoration Reading)
 *   domain: political_philosophy/constitutional_systems/east_asian_history
 *
 * SUMMARY:
 *   This constraint instantiates the loyalist-restoration reading of the
 *   contested imperial-mandate kernel in East Asian political philosophy.
 *   Under this reading, the divine mandate that legitimizes Japanese imperial
 *   rule requires the emperor to exercise direct administrative sovereignty,
 *   not merely grant legitimacy to an intermediary (shogunal) authority. The
 *   constraint operationalizes this reading: it delegates authority to the
 *   imperial court, delegitimizes the shogunate as a usurping intermediary,
 *   and demands that the emperor take personal responsibility for foreign
 *   policy and institutional reform. The reading emerged forcefully during
 *   the Bakumatsu period (1853–1868) as foreign pressure exposed the
 *   incoherence of bifurcated authority, and it structured the Meiji
 *   Restoration's institutional reorganization. The sibling reading
 *   (bakufu_delegation_reading) holds that the divine mandate operates
 *   through institutional delegation—the emperor grants legitimacy but need
 *   not govern. These readings coexist as live positions held by different
 *   factions: reform intellectuals and imperial court advocates embraced the
 *   loyalist reading; shogunal officials and bakufu-loyal samurai defended
 *   the delegation reading. The loyalist reading's enforcement extracted
 *   substantial costs from the bakufu system and samurai class while
 *   reorganizing governance to enable rapid modernization.
 *
 * KEY AGENTS:
 *   - Emperor: institutional agenda-setter, identity-locked in the role; bears the burden of direct governance
 *   - Imperial Court: beneficiary and co-agenda-setter; recovers administrative authority from the shogunate
 *   - Reform Faction (Sakoku critics, military modernizers, merchant-intellectuals): powerful beneficiaries; their modernization agenda requires unified imperial authority
 *   - Shogunate (Bakufu Authority): institutional payer; explicitly delegitimized by the reading; trapped exit
 *   - Hereditary Samurai Class: organized payer, identity-locked to shogunal service; faces unemployment and social marginalization
 *   - Regional Daimyo: powerful payers; lose semi-autonomous regional authority under centralized imperial rule
 *   - Bakufu Loyalists (shogunal bureaucrats, intellectual defenders): moderate payers, identity-locked; their careers and worldview are delegitimized
 *   - Foreign Powers: excluded observers; seek direct negotiation with unified sovereign but cannot formally shape the reading
 *   - Analytical Observer: sees the full structure—genuine coordination problem (unified authority for foreign engagement) and extraction (concentration of power, displacement of shogunal and samurai livelihoods)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(imperial_mandate__loyalist_restoration_reading, 0.68).
domain_priors:suppression_score(imperial_mandate__loyalist_restoration_reading, 0.76).
domain_priors:theater_ratio(imperial_mandate__loyalist_restoration_reading, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(imperial_mandate__loyalist_restoration_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(imperial_mandate__loyalist_restoration_reading, suppression_requirement, 0.76).
narrative_ontology:constraint_metric(imperial_mandate__loyalist_restoration_reading, theater_ratio, 0.52).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(imperial_mandate__loyalist_restoration_reading, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(imperial_mandate__loyalist_restoration_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(imperial_mandate__loyalist_restoration_reading, tangled_rope).
narrative_ontology:human_readable(imperial_mandate__loyalist_restoration_reading, "Divine Mandate Requires Unmediated Imperial Sovereignty (Loyalist-Restoration Reading)").
narrative_ontology:topic_domain(imperial_mandate__loyalist_restoration_reading, "political_philosophy/constitutional_systems/east_asian_history").

domain_priors:requires_active_enforcement(imperial_mandate__loyalist_restoration_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(imperial_mandate__loyalist_restoration_reading, 'fad407f9-4791-4fd2-927d-9accdac03088').
narrative_ontology:cs_kernel_codification('fad407f9-4791-4fd2-927d-9accdac03088', distributed).
narrative_ontology:cs_authority_grounding('fad407f9-4791-4fd2-927d-9accdac03088', lineage).
narrative_ontology:cs_interpretation_layer_present('fad407f9-4791-4fd2-927d-9accdac03088').
narrative_ontology:cs_reading_relation('fad407f9-4791-4fd2-927d-9accdac03088', imperial_mandate__bakufu_delegation_reading, coexists_with).
narrative_ontology:cs_axiom('fad407f9-4791-4fd2-927d-9accdac03088', foundational, imperial_legitimacy_requires_active_governance).
narrative_ontology:cs_axiom_status(imperial_legitimacy_requires_active_governance, holdable).
narrative_ontology:cs_axiom_grounding('fad407f9-4791-4fd2-927d-9accdac03088', imperial_legitimacy_requires_active_governance, deontological).
narrative_ontology:cs_axiom('fad407f9-4791-4fd2-927d-9accdac03088', foundational, mediated_sovereignty_is_illegitimate_usurpation).
narrative_ontology:cs_axiom_status(mediated_sovereignty_is_illegitimate_usurpation, holdable).
narrative_ontology:cs_axiom_grounding('fad407f9-4791-4fd2-927d-9accdac03088', mediated_sovereignty_is_illegitimate_usurpation, deontological).
narrative_ontology:cs_reference_frame('fad407f9-4791-4fd2-927d-9accdac03088', unified_imperial_direct_rule).
narrative_ontology:cs_drift_state('fad407f9-4791-4fd2-927d-9accdac03088', meiji_oligarchy_consolidation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('fad407f9-4791-4fd2-927d-9accdac03088', '').
narrative_ontology:cs_kernel_id(imperial_mandate__loyalist_restoration_reading, imperial_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(imperial_mandate__loyalist_restoration_reading, imperial_court).
narrative_ontology:constraint_beneficiary(imperial_mandate__loyalist_restoration_reading, reform_faction).
narrative_ontology:constraint_victim(imperial_mandate__loyalist_restoration_reading, shogunate_authority).
narrative_ontology:constraint_victim(imperial_mandate__loyalist_restoration_reading, hereditary_samurai_class).
narrative_ontology:constraint_victim(imperial_mandate__loyalist_restoration_reading, regional_daimyo).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(imperial_mandate__loyalist_restoration_reading, bakufu_loyalists).
narrative_ontology:constraint_vindicates(imperial_mandate__loyalist_restoration_reading, unified_sovereignty_doctrine).
narrative_ontology:constraint_vindicates(imperial_mandate__loyalist_restoration_reading, direct_rule_legitimacy).
narrative_ontology:constraint_vindicates(imperial_mandate__loyalist_restoration_reading, imperial_administrative_primacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds the divine mandate doctrine as the source of legitimacy for all governance. Under the loyalist reading, must exercise direct administrative sovereignty, not merely grant it through intermediaries. Bears the burden of personal governance and foreign policy decision-making; cannot delegate these functions without delegitimizing the constraint itself. Exit from this role means renouncing the imperial institution.
narrative_ontology:constraint_stakeholder(imperial_mandate__loyalist_restoration_reading, emperor, agenda_setter,
    institutional, generational, identity_locked, national).

% Recovers administrative authority and resource flows previously held by the shogunate. Court officials and advisors regain executive function and patronage channels. Their legitimacy rests on the emperor's direct exercise of rule; if the constraint weakens, their power reverts to the shogunate or daimyo.
narrative_ontology:constraint_stakeholder(imperial_mandate__loyalist_restoration_reading, imperial_court, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(imperial_mandate__loyalist_restoration_reading, imperial_court, agenda_setter).

% Intellectuals, military officers, and merchants who advocate rapid modernization and foreign engagement under imperial initiative rather than shogunal permission. The loyalist reading legitimizes their agenda: it requires the emperor to take direct action on external affairs and institutional reform. They benefit from the constraint's enforcement because it shifts authority from the conservative shogunate to the reform-minded court.
narrative_ontology:constraint_stakeholder(imperial_mandate__loyalist_restoration_reading, reform_faction, beneficiary,
    powerful, biographical, mobile, national).

% Holds delegated administrative power under the bakufu_delegation_reading but is explicitly delegitimized by the loyalist reading. Must surrender authority and resource flows to the imperial court or openly reject the divine mandate framework entirely—an act of rebellion. Bears the structural cost of the constraint's enforcement: military defeat, loss of administrative channels, and institutional dissolution.
narrative_ontology:constraint_stakeholder(imperial_mandate__loyalist_restoration_reading, shogunate_authority, payer,
    institutional, biographical, trapped, national).

% The samurai's institutional identity and economic security derive from service to the shogunate. The loyalist reading frames their loyalty to the shogun as betrayal of the emperor. Their identity-fusion with the bakufu system makes exit psychologically and socially unbearable; loyalty to the shogunate becomes illegitimate. Many face unemployment, loss of stipend, and social marginalization as the constraint is enforced.
narrative_ontology:constraint_stakeholder(imperial_mandate__loyalist_restoration_reading, hereditary_samurai_class, payer,
    organized, generational, identity_locked, national).

% Historically held semi-autonomous regional authority under shogunal oversight. The loyalist reading requires direct subjection to imperial authority, eliminating the shogunal buffer and reducing their autonomous power. Some daimyo benefit from the constraint if their regional bases align with imperial reform; others lose autonomy and resource control.
narrative_ontology:constraint_stakeholder(imperial_mandate__loyalist_restoration_reading, regional_daimyo, payer,
    powerful, biographical, constrained, regional).

% Seek direct negotiation with a unified sovereign authority. Under the loyalist reading, the emperor becomes the sole legitimate negotiator, eliminating the shogunate's diplomatic autonomy. Foreign powers would prefer direct imperial engagement, but their exclusion from the constraint's internal legitimacy dispute means they cannot formally challenge the reading.
narrative_ontology:constraint_stakeholder(imperial_mandate__loyalist_restoration_reading, foreign_powers, excluded,
    institutional, biographical, analytical, global).

% Intellectuals, bureaucrats, and military officers whose careers and identity depend on the shogunal system. The loyalist reading delegitimizes their world entirely. Exit requires renouncing professional identity and embracing a competing institutional order; psychological cost is extreme.
narrative_ontology:constraint_stakeholder(imperial_mandate__loyalist_restoration_reading, bakufu_loyalists, payer,
    moderate, biographical, identity_locked, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(imperial_mandate__loyalist_restoration_reading, imperial_court).
narrative_ontology:fixing_cost_class(imperial_mandate__loyalist_restoration_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Unifies administrative authority and foreign-policy decision-making under a single sovereign actor (the emperor) rather than bifurcating them between imperial legitimacy and shogunal administration. Solves the problem of incoherent authority in negotiating with foreign powers and implementing rapid institutional reform. Enables coordinated response to external pressure (Western imperialism, forced trade) by making imperial initiative central rather than advisory.
% TRANSFER_FUNCTION: Transfers administrative power, resource flows (tax revenue, patronage channels), and decision-making authority from the shogunate and regional daimyo to the imperial court and reform-oriented advisors. Transfers legitimacy from delegated institutional authority (the bakufu system) to direct imperial exercise of rule. Transfers employment and stipends from bakufu-loyal samurai to court-loyal military and civilian bureaucrats.
% ABSENT_VOICES: Shogunal legitimists and bakufu loyalists are structurally excluded from participating in the constraint's framing—the reading itself delegitimizes their position as usurpation. Regional daimyo with vested interests in the delegation system are heard only as opponents to be overcome. Foreign powers negotiating with Japan encounter this internal dispute as a fact but cannot formally advocate for one reading over another.
% DISAPPEARANCE_RATIONALE: If the constraint dissolved overnight, the shogunate and bakufu-loyal forces would re-establish delegated governance, samurai stipends would be restored, daimyo autonomy would revert, and the imperial court would return to a legitimacy-granting but administratively passive role. The institutional reorganization of the Meiji period depended on this constraint's enforcement; without it, the governance structure of the preceding Edo era would reconstitute.
% FOUNDING_PROBLEM: Foreign military and economic pressure (Western imperialism, forced trade opening) exposed the incoherence of a bifurcated authority structure: an emperor who grants legitimacy but does not govern, and a shogunate that governs but lacks supreme legitimacy. This ambiguity prevented unified response to external threats and appeared to foreign powers as institutional weakness. The founding problem required a single sovereign authority capable of making binding decisions on military, diplomatic, and institutional matters.
% FOUNDING_PROBLEM_CORROBORATION: Foreign powers' demands for unambiguous negotiating authority with a supreme Japanese sovereign are attested in diplomatic correspondence of the 1850s–1860s. Reform-faction Japanese intellectuals (Sakoku Edict critics, Meiji ideologists, military modernizers outside shogunal patronage) attest the problem was urgent and required unified imperial authority. Shogunal authorities and bakufu-loyal intelligentsia explicitly dispute the status: they argue the delegation system functioned adequately for internal governance and that foreign pressure was an imposed problem, not an organic one. The founding problem is live in the contested literature; uncontested outside the benefiting parties who argue the problem is solved.
narrative_ontology:disappearance_verdict(imperial_mandate__loyalist_restoration_reading, world_rearranges).
narrative_ontology:founding_problem_status(imperial_mandate__loyalist_restoration_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(imperial_mandate__loyalist_restoration_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(imperial_mandate__loyalist_restoration_reading, 'none', 1).

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
 *   Extractiveness (0.68 at interval end) reflects the constraint's operation as both coordination and extraction: it solves the genuine problem of incoherent authority in foreign negotiation (coordination function) while concentrating power in the court and displacing shogunal/samurai livelihoods (extraction function). The measurement series shows extractiveness rising from 0.42 to 0.68 over the interval (0–40 represents roughly 1853–1893), stabilizing as the institutional reorganization completed and resistance was suppressed. Suppression (0.76 at end) reflects the active military, legal, and ideological machinery required to enforce the reading: samurai uprisings were crushed, shogunal forces were defeated, and the delegation reading was driven from legitimate discourse through Meiji institutional design. Theater ratio (0.52 at end, rising from 0.38) indicates increasing performative activity: by the end of the interval, the constraint's justification as 'direct imperial governance' became increasingly theatrical—emperors delegated substantive decisions to cabinet ministers and oligarchs (the genro), while the fiction of direct imperial rule was maintained for legitimacy. Accessibility collapse (0.71–0.82 across levels) reflects the structural narrowing of options for shogunal and samurai actors: once the reading was enforced and alternatives eliminated, no institutional path back to the delegation system remained. Resistance (0.68 at t0 to 0.42 at tn) shows active opposition declining as the shogunate was militarily defeated and the samurai class was incorporated or marginalized. All metrics share one time grid; the measurements model the Restoration period's progression from contested reading to institutionalized constraint.
 *
 * PERSPECTIVAL GAP:
 *   The emperor and imperial court compute the constraint as liberation (escaping the shogunal intermediary, exercising rightful sovereignty); shogunal and bakufu-loyal actors compute it as usurpation (the reading violates the legitimate delegation framework). From the emperor's seat, the constraint coordinates unified foreign policy and institutional modernization; from the shogunate's seat, it is enforced extraction of authority. The reform faction benefits from the reading's enforcement because their modernization agenda required centralized imperial authority; the bakufu loyalists suffer because their legitimacy rested on the delegation reading. The engine computes per-seat directionality from this structural data: the emperor and court receive low d (beneficiaries, low extraction cost); the shogunate and samurai receive high d (targets, high extraction cost). The divergence is the measurement the corpus exists to capture—claimed coordination that operates as extraction when the structural costs are tallied per seat.
 *
 * DIRECTIONALITY LOGIC:
 *   Emperor: identity-locked to the role, but as the beneficiary of the reading's enforcement (recovers direct authority), d near 0.2–0.3 (full beneficiary end). Imperial Court: institutional beneficiary, constrained but not trapped exit, d near 0.25–0.35. Reform Faction: powerful beneficiaries with mobile exit (they could have pursued other channels), d near 0.15–0.30. Shogunate: institutional payer, trapped exit (dissolution is not negotiable), d near 0.80–0.90 (full target end). Hereditary Samurai: organized payer, identity-locked (samurai identity fused with shogunal service), d near 0.75–0.85. Regional Daimyo: powerful payers but some benefit from the reorganization, average d near 0.60–0.70. Bakufu Loyalists: moderate payers, identity-locked (careers and worldview dependent on delegation reading), d near 0.70–0.80. The directionality spread (0.15 to 0.90) reflects the sharp structural divergence between beneficiary and payer seats—the engine will compute this constraint as highly extractive from the payer seats and coordinative from the beneficiary seats, illustrating seat divergence without requiring overrides.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (foreign pressure, bifurcated authority) was live at t0 (1853) and remains contested at tn (1893), so mandatrophy is not triggered by the founding-problem status mismatch. However, the T17 abductive trigger (mountain_extraction_accumulation) would not fire either, because this constraint is not claimed as a mountain. The theater ratio's rise from 0.38 to 0.52 indicates an incipient mandatrophy condition: by 1893, the constraint's justification as 'direct imperial governance' was increasingly ceremonial—actual decision-making had migrated to cabinet ministers and oligarchs (the Meiji genro system), while the fiction of personal imperial rule persisted for legitimacy. If theater ratio continued rising and extractiveness remained stable, a future measurement cycle would flag the constraint as drifting toward piton status (performance of a function rather than its exercise). The constraint is not currently classified as piton because suppression remains high (0.76)—the apparatus that maintains the reading is still actively deployed. A mandatrophy resolution would require either: (a) the founding problem to die while the constraint persists (foreign pressure ceased to be the driver by ~1900, yet the constraint remained), or (b) extraction to drop sharply while the reading is maintained (the constraint shifted from extractive to purely coordinative). Neither occurred by 1893.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    divine_mandate_interpretive_contestation,
    'Is the divine mandate a natural law (a fixed metaphysical claim about legitimacy) or a constructed reading imposed by the Restoration faction through military and institutional force?',
    'Historical analysis of pre-Bakumatsu intellectual discourse: if the loyalist reading was already dominant in canonical sources (Confucian, Shinto, imperial histories), it is more plausibly natural law; if it was a minority position strengthened by Restoration military victory, it is more plausibly constructed. Comparison with other East Asian monarchies'' imperial-mandate readings.',
    'If natural law, the constraint''s high extractiveness is a regrettable cost of enforcing a truth. If constructed, the constraint is a pure snare dressed in legitimacy language (false summit candidate). The classification would shift from tangled_rope (genuine coordination + extraction) to snare (extraction with coordination cover story).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(divine_mandate_interpretive_contestation, conceptual, 'Whether the divine mandate is discovered or constructed doctrine.').

omega_variable(
    structural_necessity_of_direct_rule,
    'Did foreign pressure and the need for unified foreign policy actually require the emperor to exercise direct administrative sovereignty, or could the delegation reading have accommodated the same modernization and external negotiation?',
    'Counterfactual analysis: examine whether the delegation reading (emperor as supreme legitimizer, shogun as administrator) could have produced equivalent foreign-policy coordination, military modernization, and international standing. Comparative case: China''s concentric imperial-bureaucratic system operated with emperor as symbolic/administrative head while actual governance was highly delegated—did it fail at modernization due to the delegation structure or due to other factors?',
    'If direct rule was structurally necessary, extractiveness reflects the coordination cost of unifying authority; the constraint''s type remains tangled_rope. If the delegation reading could have accommodated the same outcomes, extractiveness is pure rent-seeking by the court and reform faction, and the constraint should be classified as snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(structural_necessity_of_direct_rule, empirical, 'Whether unmediated imperial sovereignty was structurally necessary for modernization or was a choice favoring the court''s interests.').

omega_variable(
    suppression_vs_internalized_legitimacy,
    'Is the measured suppression (0.76) primarily structural (military defeat of the shogunate, legal prohibition of the delegation reading, samurai class dissolution) or partly internalized (the samurai and bakufu loyalists came to believe in the loyalist reading)?',
    'Post-Restoration trajectory: if suppression remains high after the shogunate is defeated (i.e., if the Meiji state must continuously enforce the reading through police, ideology, education, and commemorative practice), suppression is structural. If opposition vanishes once alternatives are removed, suppression was primarily internalized identity-lock. Examine Meiji-era samurai memorials, veteran testimonies, and opposition discourse: did former bakufu loyalists actively resist, or did they psychologically accept the reading?',
    'If structural, the constraint remains a high-suppression tangled_rope: continuous enforcement is required. If internalized, the constraint may be drifting toward a lower-maintenance coordination regime where opposition is psychological rather than material. This affects sustainability and the long-term classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_vs_internalized_legitimacy, empirical, 'Whether suppression is sustained by coercive apparatus or by internalized acceptance of the reading.').

omega_variable(
    sibling_reading_relation_uncertainty,
    'Does the loyalist reading logically foreclose the delegation reading (one framework cannot hold both), or do they merely coexist as competing institutional choices?',
    'Conceptual analysis: the loyalist reading claims ''legitimacy is inseparable from active imperial governance''—this asserts that a delegating emperor is illegitimate. Does this claim logically entail that the delegation reading cannot be true, or merely that under the loyalist framework it cannot be endorsed? If a unified authority could endorse both readings (emperor is personally sovereign AND delegating governance to shogun is legitimate), the readings coexist; if the loyalist reading''s core claim makes delegation impossible, they foreclose.',
    'If foreclose relation is correct, the coexists_with assignment in cs_structure.reading_relations is wrong and should be revised. The classification of the relationship affects the modeled stability of the kernel contest and the predicted probability of reversal.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_relation_uncertainty, conceptual, 'Whether the loyalist reading logically forecloses the delegation reading or merely competes with it.').

omega_variable(
    emperors_actual_autonomy,
    'Under the loyalist reading''s enforcement, did emperors actually exercise unmediated sovereignty, or did they become figureheads whose apparent direct rule was theater for bureaucratic decision-making (genro system, cabinet ministers)?',
    'Institutional analysis of Meiji-era decision-making: trace major policy decisions (military strategy, treaty negotiation, domestic reform) to their actual source—did the emperor initiate them, or did oligarchs and ministers decide and present them for imperial ratification? Examine imperial diaries, cabinet records, and comparative court structures.',
    'If emperors were figureheads, the constraint''s claimed coordination function (unified authority for foreign policy) was never actually realized—the constraint operated as pure extraction (displacing shogunal authority) dressed in legitimacy language. The theater ratio''s rise from 0.38 to 0.52 hints at this: by 1893, the performance of direct rule was increasingly decoupled from actual imperial decision-making. A high-confidence finding here would support reclassification as snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(emperors_actual_autonomy, empirical, 'Whether emperors under the loyalist reading exercised actual unmediated sovereignty or performed the role while others decided.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(imperial_mandate__loyalist_restoration_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(impe_tr_t0, imperial_mandate__loyalist_restoration_reading, theater_ratio, 0, 0.38).
narrative_ontology:measurement_basis(impe_tr_t0, projected).
narrative_ontology:measurement(impe_tr_t5, imperial_mandate__loyalist_restoration_reading, theater_ratio, 5, 0.42).
narrative_ontology:measurement_basis(impe_tr_t5, observed).
narrative_ontology:measurement(impe_tr_t10, imperial_mandate__loyalist_restoration_reading, theater_ratio, 10, 0.46).
narrative_ontology:measurement_basis(impe_tr_t10, observed).
narrative_ontology:measurement(impe_tr_t15, imperial_mandate__loyalist_restoration_reading, theater_ratio, 15, 0.49).
narrative_ontology:measurement_basis(impe_tr_t15, observed).
narrative_ontology:measurement(impe_tr_t20, imperial_mandate__loyalist_restoration_reading, theater_ratio, 20, 0.51).
narrative_ontology:measurement_basis(impe_tr_t20, observed).
narrative_ontology:measurement(impe_tr_t25, imperial_mandate__loyalist_restoration_reading, theater_ratio, 25, 0.52).
narrative_ontology:measurement_basis(impe_tr_t25, observed).
narrative_ontology:measurement(impe_tr_t30, imperial_mandate__loyalist_restoration_reading, theater_ratio, 30, 0.52).
narrative_ontology:measurement_basis(impe_tr_t30, observed).
narrative_ontology:measurement(impe_tr_t40, imperial_mandate__loyalist_restoration_reading, theater_ratio, 40, 0.52).
narrative_ontology:measurement_basis(impe_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(impe_be_t0, imperial_mandate__loyalist_restoration_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement_basis(impe_be_t0, projected).
narrative_ontology:measurement(impe_be_t5, imperial_mandate__loyalist_restoration_reading, base_extractiveness, 5, 0.48).
narrative_ontology:measurement_basis(impe_be_t5, observed).
narrative_ontology:measurement(impe_be_t10, imperial_mandate__loyalist_restoration_reading, base_extractiveness, 10, 0.54).
narrative_ontology:measurement_basis(impe_be_t10, observed).
narrative_ontology:measurement(impe_be_t15, imperial_mandate__loyalist_restoration_reading, base_extractiveness, 15, 0.6).
narrative_ontology:measurement_basis(impe_be_t15, observed).
narrative_ontology:measurement(impe_be_t20, imperial_mandate__loyalist_restoration_reading, base_extractiveness, 20, 0.64).
narrative_ontology:measurement_basis(impe_be_t20, observed).
narrative_ontology:measurement(impe_be_t25, imperial_mandate__loyalist_restoration_reading, base_extractiveness, 25, 0.67).
narrative_ontology:measurement_basis(impe_be_t25, observed).
narrative_ontology:measurement(impe_be_t30, imperial_mandate__loyalist_restoration_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement_basis(impe_be_t30, observed).
narrative_ontology:measurement(impe_be_t40, imperial_mandate__loyalist_restoration_reading, base_extractiveness, 40, 0.68).
narrative_ontology:measurement_basis(impe_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(impe_su_t0, imperial_mandate__loyalist_restoration_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement_basis(impe_su_t0, projected).
narrative_ontology:measurement(impe_su_t5, imperial_mandate__loyalist_restoration_reading, suppression_requirement, 5, 0.65).
narrative_ontology:measurement_basis(impe_su_t5, observed).
narrative_ontology:measurement(impe_su_t10, imperial_mandate__loyalist_restoration_reading, suppression_requirement, 10, 0.7).
narrative_ontology:measurement_basis(impe_su_t10, observed).
narrative_ontology:measurement(impe_su_t15, imperial_mandate__loyalist_restoration_reading, suppression_requirement, 15, 0.74).
narrative_ontology:measurement_basis(impe_su_t15, observed).
narrative_ontology:measurement(impe_su_t20, imperial_mandate__loyalist_restoration_reading, suppression_requirement, 20, 0.75).
narrative_ontology:measurement_basis(impe_su_t20, observed).
narrative_ontology:measurement(impe_su_t25, imperial_mandate__loyalist_restoration_reading, suppression_requirement, 25, 0.76).
narrative_ontology:measurement_basis(impe_su_t25, observed).
narrative_ontology:measurement(impe_su_t30, imperial_mandate__loyalist_restoration_reading, suppression_requirement, 30, 0.76).
narrative_ontology:measurement_basis(impe_su_t30, observed).
narrative_ontology:measurement(impe_su_t40, imperial_mandate__loyalist_restoration_reading, suppression_requirement, 40, 0.76).
narrative_ontology:measurement_basis(impe_su_t40, observed).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=0, tn=40
narrative_ontology:measurement(impe_grid_01, imperial_mandate__loyalist_restoration_reading, accessibility_collapse(class), 0, 0.72).
narrative_ontology:measurement(impe_grid_02, imperial_mandate__loyalist_restoration_reading, accessibility_collapse(class), 40, 0.8).
narrative_ontology:measurement(impe_grid_03, imperial_mandate__loyalist_restoration_reading, accessibility_collapse(individual), 0, 0.65).
narrative_ontology:measurement(impe_grid_04, imperial_mandate__loyalist_restoration_reading, accessibility_collapse(individual), 40, 0.78).
narrative_ontology:measurement(impe_grid_05, imperial_mandate__loyalist_restoration_reading, accessibility_collapse(organizational), 0, 0.68).
narrative_ontology:measurement(impe_grid_06, imperial_mandate__loyalist_restoration_reading, accessibility_collapse(organizational), 40, 0.82).
narrative_ontology:measurement(impe_grid_07, imperial_mandate__loyalist_restoration_reading, accessibility_collapse(structural), 0, 0.75).
narrative_ontology:measurement(impe_grid_08, imperial_mandate__loyalist_restoration_reading, accessibility_collapse(structural), 40, 0.82).
narrative_ontology:measurement(impe_grid_09, imperial_mandate__loyalist_restoration_reading, resistance(class), 0, 0.72).
narrative_ontology:measurement(impe_grid_10, imperial_mandate__loyalist_restoration_reading, resistance(class), 40, 0.42).
narrative_ontology:measurement(impe_grid_11, imperial_mandate__loyalist_restoration_reading, resistance(individual), 0, 0.68).
narrative_ontology:measurement(impe_grid_12, imperial_mandate__loyalist_restoration_reading, resistance(individual), 40, 0.45).
narrative_ontology:measurement(impe_grid_13, imperial_mandate__loyalist_restoration_reading, resistance(organizational), 0, 0.75).
narrative_ontology:measurement(impe_grid_14, imperial_mandate__loyalist_restoration_reading, resistance(organizational), 40, 0.38).
narrative_ontology:measurement(impe_grid_15, imperial_mandate__loyalist_restoration_reading, resistance(structural), 0, 0.58).
narrative_ontology:measurement(impe_grid_16, imperial_mandate__loyalist_restoration_reading, resistance(structural), 40, 0.5).
narrative_ontology:measurement(impe_grid_17, imperial_mandate__loyalist_restoration_reading, stakes_inflation(class), 0, 0.62).
narrative_ontology:measurement(impe_grid_18, imperial_mandate__loyalist_restoration_reading, stakes_inflation(class), 40, 0.72).
narrative_ontology:measurement(impe_grid_19, imperial_mandate__loyalist_restoration_reading, stakes_inflation(individual), 0, 0.52).
narrative_ontology:measurement(impe_grid_20, imperial_mandate__loyalist_restoration_reading, stakes_inflation(individual), 40, 0.68).
narrative_ontology:measurement(impe_grid_21, imperial_mandate__loyalist_restoration_reading, stakes_inflation(organizational), 0, 0.58).
narrative_ontology:measurement(impe_grid_22, imperial_mandate__loyalist_restoration_reading, stakes_inflation(organizational), 40, 0.74).
narrative_ontology:measurement(impe_grid_23, imperial_mandate__loyalist_restoration_reading, stakes_inflation(structural), 0, 0.55).
narrative_ontology:measurement(impe_grid_24, imperial_mandate__loyalist_restoration_reading, stakes_inflation(structural), 40, 0.7).
narrative_ontology:measurement(impe_grid_25, imperial_mandate__loyalist_restoration_reading, suppression(class), 0, 0.72).
narrative_ontology:measurement(impe_grid_26, imperial_mandate__loyalist_restoration_reading, suppression(class), 40, 0.8).
narrative_ontology:measurement(impe_grid_27, imperial_mandate__loyalist_restoration_reading, suppression(individual), 0, 0.6).
narrative_ontology:measurement(impe_grid_28, imperial_mandate__loyalist_restoration_reading, suppression(individual), 40, 0.78).
narrative_ontology:measurement(impe_grid_29, imperial_mandate__loyalist_restoration_reading, suppression(organizational), 0, 0.68).
narrative_ontology:measurement(impe_grid_30, imperial_mandate__loyalist_restoration_reading, suppression(organizational), 40, 0.82).
narrative_ontology:measurement(impe_grid_31, imperial_mandate__loyalist_restoration_reading, suppression(structural), 0, 0.58).
narrative_ontology:measurement(impe_grid_32, imperial_mandate__loyalist_restoration_reading, suppression(structural), 40, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(imperial_mandate__loyalist_restoration_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(imperial_mandate__loyalist_restoration_reading, 0.12).
narrative_ontology:affects_constraint(imperial_mandate__loyalist_restoration_reading, imperial_mandate__bakufu_delegation_reading).

% DUAL FORMULATION NOTE:
% The imperial-mandate kernel comprises two structurally distinct constraints: loyalist_restoration_reading (this story) and bakufu_delegation_reading (sibling). The ε values differ substantially because the readings instantiate different constraint structures: the loyalist reading is tangled_rope (genuine coordination problem + extraction), while the delegation reading is rope or piton (coordination without extraction, or coordination atrophied into performance). Each story is ε-invariant and authored independently. The network link records their kinship: they are competing readings of the same kernel, and neither reading's classification should be understood apart from its relationship to the sibling.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(imperial_mandate__loyalist_restoration_reading, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
