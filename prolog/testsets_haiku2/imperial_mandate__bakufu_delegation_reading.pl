% ============================================================================
% CONSTRAINT STORY: imperial_mandate__bakufu_delegation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_imperial_mandate__bakufu_delegation_reading, []).

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
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: imperial_mandate__bakufu_delegation_reading
 *   human_readable: Imperial Mandate Through Bakufu Delegation
 *   domain: political/constitutional/institutional
 *
 * SUMMARY:
 *   The bakufu delegation reading instantiates imperial legitimacy as a
 *   function separable from administrative governance: the emperor retains
 *   cosmic legitimacy and ritual supremacy but delegates all actual authority
 *   to the shogun, who rules in the emperor's name. The samurai class derives
 *   status from this chain; regional daimyo are constrained by it; the
 *   imperial court is rendered politically powerless but ritually
 *   indispensable. This reading stands in direct contest with the
 *   loyalist_restoration reading, which holds that true imperial mandate
 *   requires unmediated exercise of sovereignty by the emperor himself — that
 *   delegation is usurpation. The bakufu_delegation_reading's structural
 *   claim is that legitimacy CAN be severed from governance, that a
 *   bifurcated sovereignty is coherent and stable. The loyalist reading
 *   denies this bifurcation is possible or legitimate. This story authors the
 *   bakufu reading alone, as a clean ε-invariant constraint.
 *
 * KEY AGENTS:
 *   - Imperial court: ritual authority, political powerlessness, identity-locked in mandate role
 *   - Bakufu shogunate: receives delegated administrative power, uses emperor's legitimacy to consolidate regional authority
 *   - Samurai warrior class: derives legitimate status from service in delegated hierarchy
 *   - Regional daimyo: accept hierarchy and delegation as stable equilibrium rather than military conquest
 *   - Neo-Confucian scholars: provide intellectual coherence for bifurcated sovereignty
 *   - Imperial restoration movement: contests the reading, asserts legitimacy requires direct imperial governance
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(imperial_mandate__bakufu_delegation_reading, 0.68).
domain_priors:suppression_score(imperial_mandate__bakufu_delegation_reading, 0.71).
domain_priors:theater_ratio(imperial_mandate__bakufu_delegation_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(imperial_mandate__bakufu_delegation_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(imperial_mandate__bakufu_delegation_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(imperial_mandate__bakufu_delegation_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(imperial_mandate__bakufu_delegation_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(imperial_mandate__bakufu_delegation_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(imperial_mandate__bakufu_delegation_reading, tangled_rope).
narrative_ontology:human_readable(imperial_mandate__bakufu_delegation_reading, "Imperial Mandate Through Bakufu Delegation").
narrative_ontology:topic_domain(imperial_mandate__bakufu_delegation_reading, "political/constitutional/institutional").

domain_priors:requires_active_enforcement(imperial_mandate__bakufu_delegation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(imperial_mandate__bakufu_delegation_reading, '6046200c-17ed-4485-b31e-0056d0495410').
narrative_ontology:cs_kernel_codification('6046200c-17ed-4485-b31e-0056d0495410', formalized).
narrative_ontology:cs_authority_grounding('6046200c-17ed-4485-b31e-0056d0495410', lineage).
narrative_ontology:cs_interpretation_layer_present('6046200c-17ed-4485-b31e-0056d0495410').
narrative_ontology:cs_reading_relation('6046200c-17ed-4485-b31e-0056d0495410', imperial_mandate__loyalist_restoration_reading, coexists_with).
narrative_ontology:cs_axiom('6046200c-17ed-4485-b31e-0056d0495410', foundational, legitimacy_bifurcation_coherent).
narrative_ontology:cs_axiom_status(legitimacy_bifurcation_coherent, holdable).
narrative_ontology:cs_axiom_grounding('6046200c-17ed-4485-b31e-0056d0495410', legitimacy_bifurcation_coherent, deontological).
narrative_ontology:cs_axiom('6046200c-17ed-4485-b31e-0056d0495410', secondary, samurai_legitimate_governance_tier).
narrative_ontology:cs_axiom_status(samurai_legitimate_governance_tier, holdable).
narrative_ontology:cs_axiom_grounding('6046200c-17ed-4485-b31e-0056d0495410', samurai_legitimate_governance_tier, conventional).
narrative_ontology:cs_reference_frame('6046200c-17ed-4485-b31e-0056d0495410', bifurcated_sovereignty_framework).
narrative_ontology:cs_drift_state('6046200c-17ed-4485-b31e-0056d0495410', late_bakufu_period, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('6046200c-17ed-4485-b31e-0056d0495410', '').
narrative_ontology:cs_kernel_id(imperial_mandate__bakufu_delegation_reading, imperial_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(imperial_mandate__bakufu_delegation_reading, bakufu_shogunate).
narrative_ontology:constraint_beneficiary(imperial_mandate__bakufu_delegation_reading, samurai_warrior_class).
narrative_ontology:constraint_victim(imperial_mandate__bakufu_delegation_reading, imperial_court).
narrative_ontology:constraint_victim(imperial_mandate__bakufu_delegation_reading, regional_daimyo_constrained).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(imperial_mandate__bakufu_delegation_reading, merchant_commoner_class).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Retains formal legitimacy and ritual supremacy as grantor of the mandate; performs sacred ceremonies that clothe the bakufu's rule in cosmic legitimacy. Delegates all active governing authority to the shogun but must maintain the fiction of ultimate sovereignty. Bears the cost of ritual maintenance and institutional performance while stripped of governing power. Exit is unthinkable — the imperial identity IS the mandate-granting institution.
narrative_ontology:constraint_stakeholder(imperial_mandate__bakufu_delegation_reading, imperial_court, agenda_setter,
    institutional, civilizational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(imperial_mandate__bakufu_delegation_reading, imperial_court, payer).

% Exercises all administrative, military, and legislative authority through the delegated mandate. Collects taxes, commands armies, sets policy, adjudicates disputes. The arrangement allows the shogun to wield supreme power while claiming legitimacy flows from the emperor — the shogun remains subordinate in theory while dominant in practice. Can be replaced by a new shogun if the current regime fails; the institution persists by cycling leadership through the legitimacy-granting mechanism.
narrative_ontology:constraint_stakeholder(imperial_mandate__bakufu_delegation_reading, bakufu_shogunate, beneficiary,
    institutional, generational, constrained, national).

% Derives legitimacy and social standing from service to a shogun whose authority flows from the emperor. The delegation system creates a stable hierarchy in which samurai occupy the second tier of legitimate power — above commoners, below the shogun, accountable to a chain of authority that ultimately reaches the emperor. Professional identity is fused with this chain; exiting means ceasing to be samurai.
narrative_ontology:constraint_stakeholder(imperial_mandate__bakufu_delegation_reading, samurai_warrior_class, beneficiary,
    organized, generational, identity_locked, national).

% Retain local administrative authority and military capacity under the bakufu's oversight. Must acknowledge the shogun's supremacy and contribute forces, tribute, and political support. Their power is real but bounded by the delegated hierarchy; attempting to challenge it provokes military response justified by imperial mandate. Revolt is costly; submission is stable.
narrative_ontology:constraint_stakeholder(imperial_mandate__bakufu_delegation_reading, regional_daimyo_constrained, payer,
    powerful, biographical, constrained, regional).

% Bear taxes collected by the bakufu, conscription in military campaigns, and restrictions on trade and movement. Have no seat in the governance structure. The hierarchy's legitimacy prevents organized resistance — the mandate is cosmic, the emperor is supreme, the shogun is deputy — there is no standing from which to contest it. Exit via emigration is blocked by geography and state control.
narrative_ontology:constraint_stakeholder(imperial_mandate__bakufu_delegation_reading, merchant_commoner_class, payer,
    powerless, biographical, trapped, national).

% Are systematically excluded from participation in or contestation of the legitimacy system. The bakufu maintains the mandate as a closed Japanese institutional arrangement. Foreign diplomatic pressure to open or reform is framed as illegitimate interference in a cosmically grounded sovereignty. Attempts to enter the system (early European traders, later Western powers) are met with exclusion justified by the mandate's sacredness.
narrative_ontology:constraint_stakeholder(imperial_mandate__bakufu_delegation_reading, foreign_powers, excluded,
    powerful, biographical, trapped, global).

% Provide intellectual legitimation for the delegation arrangement through Confucian philosophy: the emperor as ritual center and moral exemplar, the shogun as administrator carrying out the emperor's will, hierarchical harmony as cosmic principle. Attempt to hold both the bakufu's actual power and the emperor's theoretical supremacy in coherent intellectual balance. Their scholarship makes the arrangement thinkable as anything other than pure military conquest.
narrative_ontology:constraint_stakeholder(imperial_mandate__bakufu_delegation_reading, neo_confucian_literati, observer,
    moderate, generational, constrained, national).

% Periodically emerge to challenge the delegation arrangement, arguing that true legitimacy requires direct imperial governance. Are suppressed by the bakufu's military capacity, framed as illegitimate deviants from cosmic order. Represent the alternative reading (loyalist_restoration_reading) that this constraint structure forecloses — their very existence testifies to the reading-contestation.
narrative_ontology:constraint_stakeholder(imperial_mandate__bakufu_delegation_reading, imperial_restoration_movement, excluded,
    moderate, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(imperial_mandate__bakufu_delegation_reading, bakufu_shogunate).
narrative_ontology:fixing_cost_class(imperial_mandate__bakufu_delegation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the coordination problem of stable governance across Japan's regional powers by creating a single delegated authority (the shogun) whose power is legitimized by ritual connection to a transcendent center (the emperor). Prevents rival warlords from claiming cosmic legitimacy; concentrates the mandate in one institution.
% TRANSFER_FUNCTION: Transfers governing authority, tax revenue, and military command from a formally supreme but ritually constrained emperor to an administratively empowered shogun who rules in the emperor's delegated name. Extracts legitimacy upward (the emperor's ritual authority props the shogun's rule) and resources downward (taxes, loyalty, and samurai service flow through the bakufu's hierarchy).
% ABSENT_VOICES: Foreign powers (excluded from the legitimacy system), merchant and commoner classes (without seats in the governance structure), and the imperial restoration movement (suppressed as illegitimate contestation of the cosmic order). Each would describe the arrangement differently: as exclusionary foreign policy, as systemic powerlessness, and as usurped imperial authority respectively.
% DISAPPEARANCE_RATIONALE: If the bakufu's delegated authority vanished, Japan would face immediate civil war among regional daimyo competing for the mandate, or a direct imperial restoration with all institutional arrangements rebuilt around unmediated imperial sovereignty. The delegated system is the structure holding regional power fragmentation in check; its removal produces immediate chaos in the political order.
% FOUNDING_PROBLEM: Post-Heian Japan faced fragmentation into competing regional powers (daimyo) with no institutional mechanism to resolve contests for supreme authority. The imperial court retained formal legitimacy but lacked military power. The solution: designate one warlord as 'shogun' (military administrator) and grant the mandate through the emperor's delegation, creating a single chain of authority that reconciles military reality with cosmic legitimacy.
% FOUNDING_PROBLEM_CORROBORATION: The bakufu attests the problem remains live — regional consolidation requires a center with delegated authority. Neo-Confucian scholars support this reading, providing intellectual justification. The imperial restoration movement contests it, arguing that the problem was solved once regional unification was achieved (Sekigahara), so the delegation should revert to direct imperial governance. Independent historical analysis notes the founding problem was genuinely acute in the early period (0–90 years) but became substantially resolved by mid-period (90–180 years); contemporary scholars debate whether the constraint's persistence after 180 years reflects ongoing coordination needs or institutional inertia.
narrative_ontology:disappearance_verdict(imperial_mandate__bakufu_delegation_reading, world_rearranges).
narrative_ontology:founding_problem_status(imperial_mandate__bakufu_delegation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(imperial_mandate__bakufu_delegation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(imperial_mandate__bakufu_delegation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(imperial_mandate__bakufu_delegation_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(imperial_mandate__bakufu_delegation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(imperial_mandate__bakufu_delegation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(imperial_mandate__bakufu_delegation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high and rising (0.52→0.68 over 270 years) because the arrangement extracts legitimacy upward from the emperor (who performs ritual without power) and authority downward from regional actors (who accept constraints they could militarily contest). The constraint persists by decoupling legitimacy from governance — the emperor's cosmic role props the shogun's practical authority. Suppression rises with extraction: the imperial restoration movement must be actively suppressed as illegitimate; regional daimyo who resist centralization appeal to the same mandate the bakufu claims; institutional mechanisms (such as the sankin-kōtai residence requirement) enforce compliance beyond raw military superiority. Theater rises early (institutional elaboration and Confucian systematization) then plateaus (the arrangement has stabilized; further theater is maintenance, not growth). Accessibility collapse is high because alternatives to the delegation system require either direct military conquest (which the system prevents through coordinated suppression) or legitimacy claims the system has preempted. The arrangement is NOT presented as rope (pure coordination) because the samurai class and shogunate benefit asymmetrically from the emperor's delegated legitimacy; the emperor and constrained daimyo bear the cost. The coordination it provides (unified governance) is real, but the distribution is extractive.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats (imperial court, regional daimyo constrained) should compute as tangled_rope from their positions — they are coordinated into a single stable hierarchy but pay asymmetric costs. The beneficiary seats (bakufu shogunate, samurai class) should compute as rope or light tangled_rope — they receive clear benefits from the coordination. The engine computes this divergence from power, exit_options, and beneficiary/victim declarations. The imperial court is identity-locked (the emperor cannot exit the mandate role without ceasing to be emperor), which amplifies extraction and suppression perceived from that seat. The bakufu has constrained but real exit: a new shogun can replace the current one; the institution can fall to another power claimant. This difference in exit should produce different d values and thus different per-seat classifications.
 *
 * DIRECTIONALITY LOGIC:
 *   The bakufu shogunate benefits from the arrangement structurally: it receives concentrated administrative authority while the emperor's ritual role legitimizes that authority without requiring the emperor to wield it. The emperor bears the cost: stripped of governing power while forced to maintain the fiction of supremacy. Regional daimyo face constrained choices: they can submit to the shogunate's delegated authority, which is stabilized by the emperor's cosmic legitimacy, or they can rebel — but rebellion must challenge both the shogunate's military power AND the legitimacy the emperor grants. The delegation system makes rebellion costly by giving the shogunate the appearance of cosmic authority. Samurai benefit from the hierarchy it creates: they are legitimate governors (not mere warriors) because they serve a chain that reaches the emperor. The merchant and commoner classes are trapped: the mandate system prevents organized resistance because the legitimacy is cosmic, not contractual. The arrangement extracts from those without seats (excluded foreign powers) and from those with constrained seats (imperial court, regional daimyo). No directionality override is needed: the structural derivation produces the right d for each seat.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (regional fragmentation, military chaos) is live for the early bakufu (0–90 years), becomes stabilized (90–180 years), then begins to atrophy. By 180–270 years, the bakufu faces no serious military challenge; regional fragmentation is solved. The mandate's function shifts from coordinating warring powers into a stable hierarchy toward maintaining the hierarchy as an end in itself. Theater rises: Confucian systematization, elaborate court rituals, and institutional theater grow as a share of enforcement activity (theater_ratio rises 0.28→0.42). But the constraint does NOT decay into pure piton because suppression persists at high levels: the imperial restoration movement remains a live threat; institutional mechanisms like sankin-kōtai remain costly and enforceable; the bakufu must continue to justify its rule through the mandate framework. This is tangled_rope that has accumulated theater but not collapsed into piton. The mandatrophy is contested: the bakufu claims the founding problem remains live (regional powers must be restrained); the imperial restoration movement claims it is dead and the delegation should revert. The measurement series show slight decline in extractiveness (0.69→0.68) after peak at 180 years, suggesting the constraint has matured and stabilized rather than grown or decayed sharply.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legitimacy_bifurcation_possibility,
    'Is the bifurcation of legitimacy from governance a coherent institutional arrangement, or is it a cover story masking military conquest?',
    'Comparative institutional analysis across cases where similar bifurcations were attempted (e.g., the papacy''s temporal vs. spiritual authority, European monarchy vs. parliament divisions). Examine whether the bifurcation persists as an equilibrium or collapses under pressure.',
    'If bifurcation is a coherent arrangement, the bakufu_delegation reading''s core premise is sound, and the constraint can persist as true coordination with extractive elements. If bifurcation is merely a cover story, the reading forecloses the loyalist_restoration reading and the constraint is pure snare masked as rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_bifurcation_possibility, conceptual, 'Whether legitimacy and governance can be structurally separated or whether the separation is always illusory.').

omega_variable(
    suppression_mechanism_internalized_vs_structural,
    'Does the imperial court and regional daimyo accept the delegation arrangement through internalization of the mandate''s legitimacy, or through structural military incapacity to resist?',
    'Examine resistance patterns: if suppression is primarily internalized, resistance should be rare and framed as cosmically illegitimate by resisters themselves. If structural, resistance should spike whenever military capacity grows and be framed by resisters as justified reclamation of rightful authority.',
    'If internalized, the constraint''s effective suppression is higher than the raw metric suggests (targets carry suppression beyond exit); the constraint is more stable. If structural, the constraint depends on continued military enforcement and could be displaced by a stronger power that rejects the mandate frame.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(suppression_mechanism_internalized_vs_structural, empirical, 'Whether acceptance of the delegation is internalized or externally enforced.').

omega_variable(
    loyalist_restoration_alternative_foreclosure,
    'Does the bakufu_delegation reading logically foreclose the loyalist_restoration reading, or do they coexist as live alternatives held by different parties?',
    'Examine whether both readings can be held coherently within a single institutional framework, or whether accepting one requires rejecting the core premise of the other. If a single bakufu regime could coherently acknowledge that loyalist readings are valid alternatives, coexistence is true; if acknowledging loyalist claims requires abandoning the delegation framework, foreclosure holds.',
    'If foreclosure is true, this reading''s core axiom (bifurcated legitimacy is coherent) directly contradicts the loyalist axiom (legitimacy requires unmediated imperial governance). If coexistence is true, both readings remain live positions defended by different factions within the same institutional ecosystem.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(loyalist_restoration_alternative_foreclosure, conceptual, 'The logical relationship between bakufu delegation and loyalist restoration readings.').

omega_variable(
    mandate_persistence_after_unification,
    'After regional military unification is achieved (Sekigahara and after), what problem does the mandate delegation system solve that couldn''t be solved by direct imperial restoration?',
    'Historical analysis of what would have changed if the winning side had chosen to restore direct imperial governance instead of instituting the bakufu delegation. Compare institutional stability, samurai integration, regional compliance, and cost to administer.',
    'If the delegation solves a persistent problem (samurai integration, legitimate hierarchy, regional stability), the founding problem remains live and the constraint is true tangled_rope with theater. If unification already solved the problem and the delegation persists only as institutional inertia, the constraint slides toward piton — mandatrophy becomes applicable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandate_persistence_after_unification, empirical, 'Whether the founding problem is live throughout the interval or atrophies after unification.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(imperial_mandate__bakufu_delegation_reading, 0, 270).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(impe_tr_t0, imperial_mandate__bakufu_delegation_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement_basis(impe_tr_t0, projected).
narrative_ontology:measurement(impe_tr_t45, imperial_mandate__bakufu_delegation_reading, theater_ratio, 45, 0.32).
narrative_ontology:measurement_basis(impe_tr_t45, observed).
narrative_ontology:measurement(impe_tr_t90, imperial_mandate__bakufu_delegation_reading, theater_ratio, 90, 0.37).
narrative_ontology:measurement_basis(impe_tr_t90, observed).
narrative_ontology:measurement(impe_tr_t135, imperial_mandate__bakufu_delegation_reading, theater_ratio, 135, 0.4).
narrative_ontology:measurement_basis(impe_tr_t135, observed).
narrative_ontology:measurement(impe_tr_t180, imperial_mandate__bakufu_delegation_reading, theater_ratio, 180, 0.42).
narrative_ontology:measurement_basis(impe_tr_t180, observed).
narrative_ontology:measurement(impe_tr_t225, imperial_mandate__bakufu_delegation_reading, theater_ratio, 225, 0.43).
narrative_ontology:measurement_basis(impe_tr_t225, observed).
narrative_ontology:measurement(impe_tr_t270, imperial_mandate__bakufu_delegation_reading, theater_ratio, 270, 0.42).
narrative_ontology:measurement_basis(impe_tr_t270, observed).

% Extraction over time
narrative_ontology:measurement(impe_be_t0, imperial_mandate__bakufu_delegation_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement_basis(impe_be_t0, projected).
narrative_ontology:measurement(impe_be_t45, imperial_mandate__bakufu_delegation_reading, base_extractiveness, 45, 0.58).
narrative_ontology:measurement_basis(impe_be_t45, observed).
narrative_ontology:measurement(impe_be_t90, imperial_mandate__bakufu_delegation_reading, base_extractiveness, 90, 0.63).
narrative_ontology:measurement_basis(impe_be_t90, observed).
narrative_ontology:measurement(impe_be_t135, imperial_mandate__bakufu_delegation_reading, base_extractiveness, 135, 0.67).
narrative_ontology:measurement_basis(impe_be_t135, observed).
narrative_ontology:measurement(impe_be_t180, imperial_mandate__bakufu_delegation_reading, base_extractiveness, 180, 0.69).
narrative_ontology:measurement_basis(impe_be_t180, observed).
narrative_ontology:measurement(impe_be_t225, imperial_mandate__bakufu_delegation_reading, base_extractiveness, 225, 0.68).
narrative_ontology:measurement_basis(impe_be_t225, observed).
narrative_ontology:measurement(impe_be_t270, imperial_mandate__bakufu_delegation_reading, base_extractiveness, 270, 0.68).
narrative_ontology:measurement_basis(impe_be_t270, observed).

% Suppression requirement over time
narrative_ontology:measurement(impe_su_t0, imperial_mandate__bakufu_delegation_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(impe_su_t0, projected).
narrative_ontology:measurement(impe_su_t45, imperial_mandate__bakufu_delegation_reading, suppression_requirement, 45, 0.58).
narrative_ontology:measurement_basis(impe_su_t45, observed).
narrative_ontology:measurement(impe_su_t90, imperial_mandate__bakufu_delegation_reading, suppression_requirement, 90, 0.62).
narrative_ontology:measurement_basis(impe_su_t90, observed).
narrative_ontology:measurement(impe_su_t135, imperial_mandate__bakufu_delegation_reading, suppression_requirement, 135, 0.66).
narrative_ontology:measurement_basis(impe_su_t135, observed).
narrative_ontology:measurement(impe_su_t180, imperial_mandate__bakufu_delegation_reading, suppression_requirement, 180, 0.69).
narrative_ontology:measurement_basis(impe_su_t180, observed).
narrative_ontology:measurement(impe_su_t225, imperial_mandate__bakufu_delegation_reading, suppression_requirement, 225, 0.71).
narrative_ontology:measurement_basis(impe_su_t225, observed).
narrative_ontology:measurement(impe_su_t270, imperial_mandate__bakufu_delegation_reading, suppression_requirement, 270, 0.71).
narrative_ontology:measurement_basis(impe_su_t270, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(imperial_mandate__bakufu_delegation_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(imperial_mandate__bakufu_delegation_reading, 0.18).
narrative_ontology:affects_constraint(imperial_mandate__bakufu_delegation_reading, imperial_mandate__loyalist_restoration_reading).

% DUAL FORMULATION NOTE:
% The imperial_mandate kernel decomposes into two constraint stories: bakufu_delegation_reading (this story) and loyalist_restoration_reading. They share the same kernel (the imperial mandate) but instantiate structurally different constraints because they rest on different axioms about legitimacy bifurcation. The bakufu reading holds that legitimacy can be separated from governance; the loyalist reading denies this separation. The readings coexist as contested positions held by different parties across Japan's history. This story instantiates only the bakufu reading; the loyalist reading is a separate constraint with its own ε, beneficiaries/victims, and structural data. The two stories are linked via network.affects_constraints because the bakufu delegation system's stability directly depends on suppressing the loyalist challenge — the alternative reading is structurally excluded by the delegation framework.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
