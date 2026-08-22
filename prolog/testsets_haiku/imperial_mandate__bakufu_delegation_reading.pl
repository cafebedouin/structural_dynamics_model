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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   human_readable: Imperial Mandate via Bakufu Delegation: Bifurcated Sovereignty
 *   domain: political_philosophy/comparative_constitutional_systems/east_asian_history
 *
 * SUMMARY:
 *   This constraint captures the bakufu delegation reading of the imperial
 *   mandate—the system's legitimating framework during the Edo period and
 *   Tokugawa shogunate. It asserts that divine mandate operates through a
 *   chain of delegated authority: the emperor, as the source of legitimacy,
 *   grants governing power to the shogun, who delegates it to samurai
 *   administrators and regional daimyo. This reading separates legitimacy
 *   (which remains imperial) from active governance (which is shogunal). The
 *   constraint describes the structure and its operation during the period
 *   when this reading held institutional authority. The competing loyalist
 *   restoration reading—which insists the emperor must exercise active
 *   authority for the mandate to be valid—is a different constraint with a
 *   different ε-invariant referent and a different beneficiary structure.
 *   This story describes the bakufu system as it actually operated, not as
 *   the loyalists wanted it to operate.
 *
 * KEY AGENTS:
 *   - Bakufu Shogunate: institutional agenda-setter, controls governing authority through delegated mandate, justifies rule as stewardship
 *   - Imperial Court: institutional payer with identity-lock, retains ceremonial supremacy and mandate-granting power, excluded from executive decisions
 *   - Samurai Governing Class: organized beneficiary, legitimized through delegated authority hierarchy, constrained by dependence on shogunal system
 *   - Daimyo Outside Ruling Coalition: powerful payers, forced into subordinate hierarchy, trapped exit options
 *   - Loyalist Opposition Intellectuals: excluded voices advocating for unmediated imperial authority, suppressed as seditious
 *   - Imperial Legitimacy Principle: analytical observer, the vindicated doctrine that mandate and governance can bifurcate
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(imperial_mandate__bakufu_delegation_reading, 0.68).
domain_priors:suppression_score(imperial_mandate__bakufu_delegation_reading, 0.72).
domain_priors:theater_ratio(imperial_mandate__bakufu_delegation_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(imperial_mandate__bakufu_delegation_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(imperial_mandate__bakufu_delegation_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(imperial_mandate__bakufu_delegation_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(imperial_mandate__bakufu_delegation_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(imperial_mandate__bakufu_delegation_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(imperial_mandate__bakufu_delegation_reading, tangled_rope).
narrative_ontology:human_readable(imperial_mandate__bakufu_delegation_reading, "Imperial Mandate via Bakufu Delegation: Bifurcated Sovereignty").
narrative_ontology:topic_domain(imperial_mandate__bakufu_delegation_reading, "political_philosophy/comparative_constitutional_systems/east_asian_history").

domain_priors:requires_active_enforcement(imperial_mandate__bakufu_delegation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(imperial_mandate__bakufu_delegation_reading, '94080f3b-0664-4aae-8878-f2a32ba3a2f0').
narrative_ontology:cs_kernel_codification('94080f3b-0664-4aae-8878-f2a32ba3a2f0', fixed_text).
narrative_ontology:cs_authority_grounding('94080f3b-0664-4aae-8878-f2a32ba3a2f0', extraction).
narrative_ontology:cs_interpretation_layer_present('94080f3b-0664-4aae-8878-f2a32ba3a2f0').
narrative_ontology:cs_reading_relation('94080f3b-0664-4aae-8878-f2a32ba3a2f0', imperial_mandate__loyalist_restoration_reading, coexists_with).
narrative_ontology:cs_axiom('94080f3b-0664-4aae-8878-f2a32ba3a2f0', foundational, mandate_separable_from_governance).
narrative_ontology:cs_axiom_status(mandate_separable_from_governance, holdable).
narrative_ontology:cs_axiom_grounding('94080f3b-0664-4aae-8878-f2a32ba3a2f0', mandate_separable_from_governance, conventional).
narrative_ontology:cs_axiom('94080f3b-0664-4aae-8878-f2a32ba3a2f0', foundational, institutional_delegation_preserves_legitimacy).
narrative_ontology:cs_axiom_status(institutional_delegation_preserves_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('94080f3b-0664-4aae-8878-f2a32ba3a2f0', institutional_delegation_preserves_legitimacy, conventional).
narrative_ontology:cs_reference_frame('94080f3b-0664-4aae-8878-f2a32ba3a2f0', bifurcated_sovereignty_framework).
narrative_ontology:cs_drift_state('94080f3b-0664-4aae-8878-f2a32ba3a2f0', contemporary_bakumatsu_period, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('94080f3b-0664-4aae-8878-f2a32ba3a2f0', '').
narrative_ontology:cs_kernel_id(imperial_mandate__bakufu_delegation_reading, imperial_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(imperial_mandate__bakufu_delegation_reading, bakufu_shogunate).
narrative_ontology:constraint_beneficiary(imperial_mandate__bakufu_delegation_reading, samurai_governing_class).
narrative_ontology:constraint_victim(imperial_mandate__bakufu_delegation_reading, imperial_court).
narrative_ontology:constraint_victim(imperial_mandate__bakufu_delegation_reading, daimyo_outside_ruling_coalition).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(imperial_mandate__bakufu_delegation_reading, imperial_court).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receives delegation of governing authority from the emperor; exercises taxation, law-making, military command, and administrative functions. Justifies rule as stewardship on behalf of the emperor and the realm. Actively suppresses direct imperial political participation and frames shogunal authority as legitimate because it operates under divine mandate transmitted through imperial sanction. The shogunate's entire claim rests on the maintained fiction that the emperor retains ultimate legitimacy while the shogun exercises it.
narrative_ontology:constraint_stakeholder(imperial_mandate__bakufu_delegation_reading, bakufu_shogunate, agenda_setter,
    institutional, generational, constrained, national).

% Retains ceremonial supremacy, ritual authority, and the power to grant or withdraw legitimacy from the shogunate. Receives stipends and honors from the shogunate but is systematically excluded from governing decisions and military power. Identity is constituted as the source of mandate—the emperor cannot exit without ceasing to be emperor. The court pays through loss of executive power and revenue control; it benefits through preservation of sanctity and the theoretical possibility of reclaiming authority if a shogun proves unworthy.
narrative_ontology:constraint_stakeholder(imperial_mandate__bakufu_delegation_reading, imperial_court, payer,
    institutional, civilizational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(imperial_mandate__bakufu_delegation_reading, imperial_court, beneficiary).

% Legitimized as the ruling stratum through the bakufu's delegation structure. Gains monopoly on military authority, administrative posts, and legal jurisdiction. Their legitimacy is secured by the constraint's logic: they govern through delegation from the shogun, who governs through delegation from the emperor. Constrained because the samurai class depends on shogunal authority remaining intact; if the imperial court revokes legitimacy, the entire class loses institutional standing.
narrative_ontology:constraint_stakeholder(imperial_mandate__bakufu_delegation_reading, samurai_governing_class, beneficiary,
    organized, generational, constrained, national).

% Powerful regional lords who do not control the shogunate. Pay tribute, submit to shogunal law, and lose autonomous military authority. Exit would mean armed rebellion and loss of legitimacy. The constraint forces them into a subordinate hierarchy where their regional power is real but shogunal authority overrides it. They bear the extraction of lost autonomy while the shogunate concentrates authority.
narrative_ontology:constraint_stakeholder(imperial_mandate__bakufu_delegation_reading, daimyo_outside_ruling_coalition, payer,
    powerful, biographical, trapped, national).

% Scholar-officials and Neo-Confucian thinkers who question the separation of legitimacy from governance. They argue the emperor must exercise active authority to fulfill the mandate. Excluded from the high-council discourse because their position threatens the constraint's foundational claim. Their suppression is structural: voices articulating alternative readings of the mandate are treated as seditious.
narrative_ontology:constraint_stakeholder(imperial_mandate__bakufu_delegation_reading, loyalist_opposition_intellectuals, excluded,
    moderate, biographical, constrained, national).

% The abstract doctrine that divine mandate flows through institutional channels and can be separated from active exercise of power. This principle is vindicated by the bakufu's successful rule in the emperor's name; it is contested by loyalist readings that insist mandate and governance are inseparable.
narrative_ontology:constraint_stakeholder(imperial_mandate__bakufu_delegation_reading, imperial_legitimacy_principle, observer,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(imperial_mandate__bakufu_delegation_reading, imperial_legitimacy_principle).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(imperial_mandate__bakufu_delegation_reading, bakufu_shogunate).
narrative_ontology:fixing_cost_class(imperial_mandate__bakufu_delegation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the problem of legitimate centralized authority over a fragmented daimyo system by creating a chain of delegation: emperor grants legitimacy to shogun, shogun delegates to samurai administrators. This structure allows local military lords to accept a higher authority without requiring constant military coercion or the emperor's direct participation in every decision.
% TRANSFER_FUNCTION: Moves administrative authority, military command, taxation rights, and revenue from the imperial court to the shogunate and samurai class, while moving ceremonial supremacy and ultimate legitimacy in the opposite direction (toward the emperor as the granting source). The emperor relinquishes governing power; the shogunate gains it and must publicly acknowledge the emperor as its source.
% ABSENT_VOICES: Loyalist scholars and court factions who believe the mandate cannot be separated from imperial active governance are excluded from the shogunate's policy councils. Daimyo who might argue for direct negotiation with the imperial court rather than through shogunal channels are suppressed through military subordination. Regional religious authorities who might claim independent legitimacy are confined to ceremonial roles.
% DISAPPEARANCE_RATIONALE: If the bakufu delegation constraint vanished overnight, the emperor would face pressure to reassert direct governing authority, the samurai class would lose its institutional legitimacy, and daimyo would reorganize into competing power blocs. The entire administrative apparatus built on shogunal-delegated authority would require justification from first principles. The world that emerged would rest on either imperial direct rule or feudal fragmentation, not on the managed bifurcation this constraint maintains.
% FOUNDING_PROBLEM: After centuries of imperial direct rule, the Japanese territories faced fragmentation under competing daimyo. No single actor could impose military unification without claiming to displace the emperor. The mandate concept solved this: a shogun could claim to unify on the emperor's behalf, gaining legitimacy without requiring the emperor's military capability or full participation.
% FOUNDING_PROBLEM_CORROBORATION: The bakufu system successfully unified Japan for 250+ years (Edo period), which shogunal historians cite as evidence the problem was solved. Loyalist historians and modern scholars argue the 'problem' was the emperor's inability to govern effectively—a reading that treats the bakufu system as a symptom of imperial weakness, not a solution. Neo-Confucian scholars of the 18th-19th centuries attested the founding problem was creating a system where the emperor retained dignity without requiring the military capacity for continuous warfare; this corroboration comes from outside the immediate beneficiaries (the shogunate) but from intellectuals within the system's legitimating order.
narrative_ontology:disappearance_verdict(imperial_mandate__bakufu_delegation_reading, world_rearranges).
narrative_ontology:founding_problem_status(imperial_mandate__bakufu_delegation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(imperial_mandate__bakufu_delegation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
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
 *   Extractiveness rises from 0.55 to 0.71 across the interval (1603–1820) as the shogunate consolidates control and daimyo autonomy erodes. Theater ratio climbing from 0.25 to 0.44 reflects increasing ceremonial maintenance of the imperial fiction—by the 19th century, more shogunal activity serves to sustain the legitimacy claim than to govern functionally. Suppression requirement follows a similar arc: early shogunate needed to suppress daimyo rebellion and loyalist voices; by 1750 the system had internalized so suppression was primarily doctrinal (controlling education, censoring Neo-Confucian critiques). The slight drop at 1868 reflects the system's approaching collapse—as Western pressure mounted and samurai questioned the bakufu's competence, active suppression increased again to hold the constraint in place before it broke. Theater reaches a peak at 1820 because by then the original coordination problem (fragmented daimyo) had been so thoroughly solved that the elaborate imperial-delegation ceremony served mainly to preserve the shogunate's legitimacy, not to solve an ongoing problem.
 *
 * PERSPECTIVAL GAP:
 *   The bakufu and imperial court compute dramatically different constraint types from the same structural facts. From the shogunate's seat: this is coordination (solving fragmentation) plus justified differentiation (administrators govern better). From the court's seat: this is extraction (loss of power and revenue). The engine's per-seat computation surfaces this divergence—the claimed tangled_rope (has both coordination AND extraction) is validated by the fact that different seats compute differently, not because they see different things, but because they sit in genuinely different structural positions. The court and shogunate both see the same constraint; they experience opposite sides of it.
 *
 * DIRECTIONALITY LOGIC:
 *   Bakufu shogunate: d ≈ 0.15–0.25. They are the primary beneficiary (control authority, collect legitimacy), have institutional power, but face moderate constraints (must maintain the delegated fiction, face potential court-backed rebellions). Imperial court: d ≈ 0.80–0.90. They are the structural target—extraction of power, identity-locked (cannot exit without ceasing to be imperial), institutional power but constrained by the constraint itself. Samurai class: d ≈ 0.35–0.50. Beneficiaries of legitimacy but constrained by dependence on the shogunate; moderate power, biographical time horizon. Daimyo outside coalition: d ≈ 0.85–0.95. Full targets—lost autonomy, trapped exit, powerful but subordinated by the constraint. Loyalist intellectuals: d ≈ 0.90, excluded but suppressed; their potential influence is precisely what the constraint's suppression mechanism targets.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint exhibits classical mandatrophy signals: the founding problem (fragmented daimyo requiring unification under legitimacy) was solved by ~1650 and stayed solved. Theater ratio rising from 0.25 to 0.44 tracks the erosion of the functional problem—by 1750, the constraint was maintained primarily through ceremony and suppression, not through coordination of an ongoing problem. However, the constraint is NOT classified as piton because the extraction component (shogunal revenue, samurai legitimacy, daimyo subordination) remains substantial and concentrated—the beneficiaries have every reason to maintain it, and the victims (court, excluded daimyo) lack the power to dissolve it. A piton is maintained by inertia with no concentrated beneficiary; this system has concentrated beneficiaries (bakufu, samurai). The mandatrophy is real—the functional coordination problem expired—but the constraint persists because it extracts wealth and authority. The tangled_rope classification holds because the extraction is not the whole story; the samurai administrative system genuinely did coordinate Japan, and it still did so in 1820. The mandatrophy tells us the coordination component is vestigial by that date, but tangled_rope remains the type because both components exist structurally.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    mandate_separation_legitimacy,
    'Can divine mandate be legitimately separated from active imperial governance, or is mandate inseparable from executive power?',
    'Theological/philosophical analysis of mandate doctrine as applied in East Asian political traditions; historical examination of whether the bifurcation was accepted across social strata or imposed on unwilling actors. The measurement would be corroboration: who endorsed the separation, and from what seats?',
    'If mandate and governance can be separated, the bakufu delegation reading is defensible as a coherent interpretation of the mandate doctrine. If they cannot, the reading becomes a rationalization for usurpation, and the constraint reclassifies toward snare (the separation is the cover story). This is the foundational axiom ambiguity.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(mandate_separation_legitimacy, conceptual, 'The core dispute between bakufu delegation and loyalist restoration readings—whether bifurcated sovereignty is a legitimate interpretation of mandate doctrine or an illegitimate deviation from it.').

omega_variable(
    imperial_court_volition_ambiguity,
    'Did the imperial court accept the delegation structure as legitimate, or was it coerced into acquiescence?',
    'Historical evidence from court records, correspondence between imperial officials and shogunate, and the timeline of court resistance. If the court negotiated its role and found benefit in retained ceremonial status, the arrangement was genuinely accepted. If the court was pressured into silence and suppressed attempts at reclamation, suppression was enforced.',
    'If the court accepted its role, directionality for the imperial court shifts toward symmetric (cost = benefit of ceremonial preservation). If it was coerced, directionality remains near full target (d~0.85). The difference affects whether this reads as coordination-with-asymmetry or pure extraction with a coordination cover.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(imperial_court_volition_ambiguity, empirical, 'The degree of imperial consent to the delegated role versus forced institutional subordination.').

omega_variable(
    samurai_class_dependency_trap,
    'Did the samurai class become identity-locked to the bakufu system, or did they maintain ability to reorganize authority?',
    'Examination of samurai responses when the bakufu faced internal crisis (Boshin War period, 1860s). Did samurai seek to preserve the shogunate specifically, or did they rapidly reorganize under the restored emperor? If rapid reorganization occurred, exit_options should be upgraded from constrained to mobile for the samurai.',
    'If samurai were identity-locked, their d value stays ~0.40 (beneficiaries constrained by dependence). If they were tactically dependent but strategically mobile (as the 1868 transition suggests), directionality drops to ~0.25 (beneficiaries with exit options). This affects whether the samurai class is a full coordinate in the tangled_rope or a tactical agent with agency.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(samurai_class_dependency_trap, empirical, 'The degree to which samurai class legitimacy was constitutively fused to the bakufu system versus tactically dependent on it.').

omega_variable(
    loyalist_reading_as_alternative_kernel_instantiation,
    'Is the loyalist restoration reading a genuine alternative reading of the same mandate kernel, or a foundational rejection of the delegation doctrine itself?',
    'Textual and theological analysis of loyalist arguments: do they argue the mandate doctrine requires unmediated imperial power (alternative reading of the same kernel), or do they argue the mandate doctrine itself is invalid and should be replaced (rejection of the kernel)? The distinction maps to coexists_with vs. forecloses.',
    'If loyalist intellectuals argue for a different reading of mandate (delegation is not consistent with true mandate), the relation is coexists_with—both readings remain live, held by different parties. If they argue mandate is invalid entirely (the emperor should govern on some other basis), the relation approaches forecloses—they are rejecting the kernel itself. This affects the cs_structure.reading_relations classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(loyalist_reading_as_alternative_kernel_instantiation, conceptual, 'Whether the loyalist reading is a sibling interpretation of the mandate kernel or a foundational rejection of the mandate doctrine that would replace the kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(imperial_mandate__bakufu_delegation_reading, 1603, 1868).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(impe_tr_t1603, imperial_mandate__bakufu_delegation_reading, theater_ratio, 1603, 0.25).
narrative_ontology:measurement(impe_tr_t1680, imperial_mandate__bakufu_delegation_reading, theater_ratio, 1680, 0.31).
narrative_ontology:measurement(impe_tr_t1750, imperial_mandate__bakufu_delegation_reading, theater_ratio, 1750, 0.38).
narrative_ontology:measurement(impe_tr_t1820, imperial_mandate__bakufu_delegation_reading, theater_ratio, 1820, 0.44).
narrative_ontology:measurement(impe_tr_t1868, imperial_mandate__bakufu_delegation_reading, theater_ratio, 1868, 0.41).

% Extraction over time
narrative_ontology:measurement(impe_be_t1603, imperial_mandate__bakufu_delegation_reading, base_extractiveness, 1603, 0.55).
narrative_ontology:measurement(impe_be_t1680, imperial_mandate__bakufu_delegation_reading, base_extractiveness, 1680, 0.62).
narrative_ontology:measurement(impe_be_t1750, imperial_mandate__bakufu_delegation_reading, base_extractiveness, 1750, 0.68).
narrative_ontology:measurement(impe_be_t1820, imperial_mandate__bakufu_delegation_reading, base_extractiveness, 1820, 0.71).
narrative_ontology:measurement(impe_be_t1868, imperial_mandate__bakufu_delegation_reading, base_extractiveness, 1868, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(impe_su_t1603, imperial_mandate__bakufu_delegation_reading, suppression_requirement, 1603, 0.58).
narrative_ontology:measurement(impe_su_t1680, imperial_mandate__bakufu_delegation_reading, suppression_requirement, 1680, 0.65).
narrative_ontology:measurement(impe_su_t1750, imperial_mandate__bakufu_delegation_reading, suppression_requirement, 1750, 0.7).
narrative_ontology:measurement(impe_su_t1820, imperial_mandate__bakufu_delegation_reading, suppression_requirement, 1820, 0.74).
narrative_ontology:measurement(impe_su_t1868, imperial_mandate__bakufu_delegation_reading, suppression_requirement, 1868, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(imperial_mandate__bakufu_delegation_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(imperial_mandate__bakufu_delegation_reading, 0.12).
narrative_ontology:affects_constraint(imperial_mandate__bakufu_delegation_reading, imperial_mandate__loyalist_restoration_reading).

% DUAL FORMULATION NOTE:
% The imperial mandate kernel divides into two constraint stories: (1) bakufu_delegation_reading: mandate operates through delegated institutional authority; emperor is legitimacy source, shogun is administrator. (2) loyalist_restoration_reading: mandate requires unmediated imperial governance; delegation is illegitimate usurpation. The two readings share the kernel (divine mandate) but instantiate opposite structural claims. The bakufu reading has lower ε (0.68 at peak, describing coordination + moderate extraction) because the system genuinely solved the fragmentation problem. The loyalist reading would have higher ε because it describes the system as pure usurpation from the imperial perspective. They are two separate constraints linked by the kernel they contest—not two observations of one constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
