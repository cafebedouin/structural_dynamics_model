% ============================================================================
% CONSTRAINT STORY: imperial_mandate__bakufu_delegation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: imperial_mandate__bakufu_delegation_reading
 *   human_readable: Bakufu Delegation Reading of the Imperial Mandate (Bifurcated Sovereignty)
 *   domain: political_philosophy/comparative_constitutional_systems/east_asian_history
 *
 * SUMMARY:
 *   This story instantiates the bakufu delegation reading of the imperial
 *   mandate kernel: the claim that divine legitimacy is a grantable,
 *   delegable function separable from the actual exercise of governance.
 *   Under this reading the emperor performs an irreducible
 *   ritual/legitimating role (dynastic continuity, cosmological sanction)
 *   while the shogun and the samurai administrative class exercise the
 *   substantive functions of rule — taxation, law, military command — under a
 *   title formally bestowed by imperial investiture. This is the reading that
 *   structurally underwrote roughly seven centuries of shogunal governance in
 *   Japan (Kamakura through Tokugawa), and it required active maintenance:
 *   court-surveillance offices, restrictions on daimyo access to the imperial
 *   household, and periodic reassertion of the shogunate's monopoly on the
 *   legitimacy-granting channel. The sibling reading — that legitimacy
 *   requires the emperor's unmediated personal exercise of sovereignty — is
 *   NOT modeled in this file; it is a structurally distinct constraint with
 *   its own ε, its own beneficiary/victim structure, and its own
 *   classification, linked here only by kernel identity.
 *
 * KEY AGENTS:
 *   - shogunal_administration: agenda_setter (institutional/arbitrage) — exercises governance, administers the delegation fiction
 *   - samurai_governing_stratum: beneficiary (organized/constrained) — legitimacy of office depends on delegation holding
 *   - kyoto_court_nobility: beneficiary/payer (moderate/trapped) — retains ritual prestige, loses material independence
 *   - the_emperor: agenda_setter/payer (institutional/identity_locked) — sole legitimacy source, barred from governing
 *   - loyalist_political_factions: excluded (powerless/trapped) — reject separability, suppressed as seditious
 *   - peasant_and_town_populations: payer (powerless/trapped) — bear governance costs without legitimacy-channel access
 *   - regional_daimyo_seeking_direct_imperial_appeal: excluded (powerful/constrained) — foreclosed from direct imperial appeal
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(imperial_mandate__bakufu_delegation_reading, 0.58).
domain_priors:suppression_score(imperial_mandate__bakufu_delegation_reading, 0.71).
domain_priors:theater_ratio(imperial_mandate__bakufu_delegation_reading, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(imperial_mandate__bakufu_delegation_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(imperial_mandate__bakufu_delegation_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(imperial_mandate__bakufu_delegation_reading, theater_ratio, 0.62).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(imperial_mandate__bakufu_delegation_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(imperial_mandate__bakufu_delegation_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(imperial_mandate__bakufu_delegation_reading, tangled_rope).
narrative_ontology:human_readable(imperial_mandate__bakufu_delegation_reading, "Bakufu Delegation Reading of the Imperial Mandate (Bifurcated Sovereignty)").
narrative_ontology:topic_domain(imperial_mandate__bakufu_delegation_reading, "political_philosophy/comparative_constitutional_systems/east_asian_history").

domain_priors:requires_active_enforcement(imperial_mandate__bakufu_delegation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(imperial_mandate__bakufu_delegation_reading, 'ba41037c-847e-47b0-9261-916c7d12e4fd').
narrative_ontology:cs_kernel_codification('ba41037c-847e-47b0-9261-916c7d12e4fd', distributed).
narrative_ontology:cs_authority_grounding('ba41037c-847e-47b0-9261-916c7d12e4fd', lineage).
narrative_ontology:cs_interpretation_layer_present('ba41037c-847e-47b0-9261-916c7d12e4fd').
narrative_ontology:cs_reading_relation('ba41037c-847e-47b0-9261-916c7d12e4fd', imperial_mandate__loyalist_restoration_reading, forecloses).
narrative_ontology:cs_axiom('ba41037c-847e-47b0-9261-916c7d12e4fd', foundational, legitimacy_grant_separable_from_exercise).
narrative_ontology:cs_axiom_status(legitimacy_grant_separable_from_exercise, holdable).
narrative_ontology:cs_axiom_grounding('ba41037c-847e-47b0-9261-916c7d12e4fd', legitimacy_grant_separable_from_exercise, conventional).
narrative_ontology:cs_axiom('ba41037c-847e-47b0-9261-916c7d12e4fd', secondary, delegated_authority_retains_full_legitimacy).
narrative_ontology:cs_axiom_status(delegated_authority_retains_full_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('ba41037c-847e-47b0-9261-916c7d12e4fd', delegated_authority_retains_full_legitimacy, conventional).
narrative_ontology:cs_reference_frame('ba41037c-847e-47b0-9261-916c7d12e4fd', delegated_dual_sovereignty_equilibrium).
narrative_ontology:cs_drift_state('ba41037c-847e-47b0-9261-916c7d12e4fd', bakumatsu_crisis_era, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('ba41037c-847e-47b0-9261-916c7d12e4fd', '').
narrative_ontology:cs_kernel_id(imperial_mandate__bakufu_delegation_reading, imperial_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(imperial_mandate__bakufu_delegation_reading, shogunal_administration).
narrative_ontology:constraint_beneficiary(imperial_mandate__bakufu_delegation_reading, samurai_governing_stratum).
narrative_ontology:constraint_beneficiary(imperial_mandate__bakufu_delegation_reading, kyoto_court_nobility).
narrative_ontology:constraint_victim(imperial_mandate__bakufu_delegation_reading, loyalist_political_factions).
narrative_ontology:constraint_victim(imperial_mandate__bakufu_delegation_reading, peasant_and_town_populations).
narrative_ontology:constraint_victim(imperial_mandate__bakufu_delegation_reading, regional_daimyo_seeking_direct_imperial_appeal).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(imperial_mandate__bakufu_delegation_reading, kyoto_court_nobility).
narrative_ontology:constraint_victim(imperial_mandate__bakufu_delegation_reading, the_emperor).
narrative_ontology:constraint_vindicates(imperial_mandate__bakufu_delegation_reading, separability_of_legitimacy_and_governance).
narrative_ontology:constraint_vindicates(imperial_mandate__bakufu_delegation_reading, institutional_continuity_through_delegation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds and exercises the actual apparatus of governance — taxation, military command, law enforcement, land tenure adjudication — while formally receiving its mandate to govern from the emperor's investiture as sei-i taishogun. Administers the fiction that political rule is delegated authority, not an independent seizure, and enforces the emperor's confinement to ritual and cultural functions through court oversight offices (e.g. the shoshidai, and later the kinchu narabi ni kuge shohatto). Benefits enormously: retains all coercive and fiscal power while offloading the burden and risk of active rulership legitimation onto a ritual figure who cannot mobilize independent political support.
narrative_ontology:constraint_stakeholder(imperial_mandate__bakufu_delegation_reading, shogunal_administration, agenda_setter,
    institutional, generational, arbitrage, national).

% Occupies the administrative and military offices legitimated by shogunal appointment. Their status as the legitimate governing class depends entirely on the delegation reading holding — if legitimacy required unmediated imperial rule, their entire office-holding structure would be void. They benefit from institutional stability and status but are also bound: they cannot appeal past the shogun to the emperor for redress without threatening the very framework that legitimizes their own rule.
narrative_ontology:constraint_stakeholder(imperial_mandate__bakufu_delegation_reading, samurai_governing_stratum, beneficiary,
    organized, generational, constrained, national).

% Retains ceremonial precedence, court rank, and modest stipends under the delegation arrangement, preserving centuries of cultural and religious prestige without political risk. But their material dependence is total — the shogunate controls the imperial household's finances and restricts its political contacts, so the nobility's continued relevance is only ever ritual, never governing. They cannot leave the arrangement without losing what remains of their institutional identity.
narrative_ontology:constraint_stakeholder(imperial_mandate__bakufu_delegation_reading, kyoto_court_nobility, beneficiary,
    moderate, civilizational, trapped, national).
narrative_ontology:stakeholder_secondary_role(imperial_mandate__bakufu_delegation_reading, kyoto_court_nobility, payer).

% Formally the sole source of legitimate mandate — grants the title that authorizes the shogun's rule — but is structurally barred from exercising the governing function that title legitimates. Identity is fused to the ritual role: the emperor's entire claim to significance rests on being the unmediated font of legitimacy even while the exercise of that legitimacy is delegated away. Any attempt to reclaim active governance would require repudiating the very ritual-purity framing that gives the office its remaining authority.
narrative_ontology:constraint_stakeholder(imperial_mandate__bakufu_delegation_reading, the_emperor, agenda_setter,
    institutional, civilizational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(imperial_mandate__bakufu_delegation_reading, the_emperor, payer).

% Reject the separability premise outright, holding that legitimacy is void wherever the emperor does not personally govern. Excluded from the delegation reading's institutional structure entirely — no office, no recognized standing, treated by the shogunate as seditious when organized. Their objection is structurally excluded from the delegation framework's own self-description; it appears in that framework only as disorder to be suppressed.
narrative_ontology:constraint_stakeholder(imperial_mandate__bakufu_delegation_reading, loyalist_political_factions, excluded,
    powerless, biographical, trapped, national).

% Bear taxation, corvee, and legal subordination administered entirely through the samurai-shogunal apparatus, with the emperor functioning at most as a distant, abstract source of cosmic order invoked to sanctify the arrangement they cannot appeal to directly. They experience the governing function's costs with no practical access to the legitimacy-granting function that supposedly checks it.
narrative_ontology:constraint_stakeholder(imperial_mandate__bakufu_delegation_reading, peasant_and_town_populations, payer,
    powerless, biographical, trapped, local).

% Powerful regional lords who might, in moments of shogunal weakness, prefer to appeal directly to the emperor for legitimacy independent of the bakufu. The delegation reading's enforcement apparatus (court surveillance, restrictions on daimyo-court contact) exists partly to foreclose exactly this option, keeping the emperor's legitimacy-granting function a monopoly channel that only the shogunate can broker.
narrative_ontology:constraint_stakeholder(imperial_mandate__bakufu_delegation_reading, regional_daimyo_seeking_direct_imperial_appeal, excluded,
    powerful, generational, constrained, regional).

% Study the bifurcated-sovereignty arrangement as a comparative case in separating symbolic and administrative sovereignty (alongside constitutional monarchy, Vatican/Italian state relations, etc.), without a stake in which reading of the mandate is correct.
narrative_ontology:constraint_stakeholder(imperial_mandate__bakufu_delegation_reading, comparative_constitutional_historians, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Separates the source of legitimacy (dynastic-ritual continuity, believed to trace an unbroken divine lineage) from the exercise of coercive and administrative power (military government), allowing governance to adapt to shifting military and economic realities across centuries without requiring the ritual legitimating institution itself to be overthrown or replaced each time power changes hands.
% TRANSFER_FUNCTION: Moves practical governing authority, tax revenue, and coercive capacity from any notional unmediated-imperial-rule arrangement into the hands of the shogunal administration and samurai stratum, while moving ritual deference, ceremonial revenue, and symbolic primacy toward the imperial court — with the peasant and town populations bearing the material costs of the governing function without a practical channel to the legitimacy-granting function.
% ABSENT_VOICES: Loyalist factions who hold legitimacy inseparable from active imperial rule are excluded from the delegation framework's institutional life entirely, appearing in its records mainly as suppressed disorder; regional daimyo who might prefer direct imperial appeal are structurally cut off from the emperor by court-surveillance offices maintained for that purpose.
% DISAPPEARANCE_RATIONALE: If the delegation reading collapsed — if the separability of legitimacy-granting from governing were rejected — the entire structure of samurai office-holding, shogunal taxation authority, and court/bakufu division of function would lose its legitimating basis overnight; this is precisely what occurred, structurally, at the Meiji Restoration, when the loyalist reading displaced the delegation reading and the bakufu, samurai class privileges, and the court/shogunate division were all dismantled in short order.
% FOUNDING_PROBLEM: Following periods of court weakness and military consolidation (notably from the Kamakura period onward), the historical problem was reconciling continuous claims of unbroken imperial divine descent with the practical reality that emperors could not or did not command effective military and administrative power; the delegation reading solved this by making legitimacy transferable-in-grant rather than requiring personal exercise.
% FOUNDING_PROBLEM_CORROBORATION: The shogunal administration and the samurai stratum attest the founding problem remains live — that stable governance still requires an administrative authority distinct from ritual sovereignty. Independent historians of the late Tokugawa and early Meiji periods, along with the loyalist factions themselves (an interested but external-to-the-delegation-framework party), attest that by the 19th century the arrangement had become a vehicle for samurai-class privilege preservation rather than a live solution to any coordination problem, corroborated by the rapid, broadly accepted dismantling of the bakufu once foreign pressure and domestic crisis reopened the legitimacy question.
narrative_ontology:disappearance_verdict(imperial_mandate__bakufu_delegation_reading, world_rearranges).
narrative_ontology:founding_problem_status(imperial_mandate__bakufu_delegation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(imperial_mandate__bakufu_delegation_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(imperial_mandate__bakufu_delegation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(imperial_mandate__bakufu_delegation_reading, 0.58, 'claude-sonnet-5', 'none', direct).

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
 *   Extraction is moderate-high and rises across the interval (0.32 to 0.58) as the delegation arrangement shifts from an emergent institutional solution (early Kamakura period, coordination genuinely needed) toward a self-perpetuating structure of samurai and court privilege by the late Tokugawa period. Theater ratio rises even faster (0.35 to 0.62), tracking the growing gap between the elaborate ritual apparatus surrounding the emperor and its declining functional necessity — by the 19th century the court-surveillance offices and ceremonial protocols had become substantially about maintaining the appearance of a settled hierarchy rather than solving any live coordination problem. Suppression is consistently the highest metric and also rises (0.45 to 0.71), because the delegation reading's persistence depended on actively foreclosing the loyalist reading and restricting daimyo-court contact — coercive maintenance, not passive consensus.
 *
 * PERSPECTIVAL GAP:
 *   From the shogunal/samurai seat, the arrangement is a genuine, centuries-tested coordination solution to the problem of unifying symbolic continuity with practical governance. From the loyalist/excluded seat, and increasingly from the daimyo seat by the 1860s, the same structure reads as an extractive fiction maintained by force to entrench samurai privilege under the emperor's borrowed sanctity. The engine computes these divergent seat classifications from the structural data; this file does not adjudicate which seat is 'correct' — it only models the delegation reading's own internal structure faithfully.
 *
 * DIRECTIONALITY LOGIC:
 *   The shogunal administration sits nearest full beneficiary: it collects governing power and revenue while the emperor absorbs the burden of embodying unmediated legitimacy claims it never gets to exercise. The samurai stratum are secondary beneficiaries whose entire office-holding status is derivative of the delegation reading holding. The emperor and court nobility are beneficiaries of ceremonial status but simultaneously payers — they trade governing power and material independence for ritual preservation, an override-worthy asymmetric position captured via the dual role rather than a plain override. Peasant and town populations are pure payers with no access to either seat. Loyalist factions and ambitious daimyo are excluded rather than coordinated — their exclusion is the suppression apparatus's actual object.
 *
 * MANDATROPHY ANALYSIS:
 *   The delegation reading prevents the mislabeling of the shogunate as pure usurpation (it is not — it is legitimated, however instrumentally, through a real institutional mechanism the emperor participates in) while also preventing the mislabeling of the arrangement as pure coordination (it is not — it required standing suppression apparatus and increasingly served an entrenched samurai stratum rather than solving a live problem by the late Tokugawa period). The founding-problem status is authored as contested precisely because this is the crux the Meiji Restoration resolved by force: the loyalist reading's eventual triumph is empirical evidence that the delegation reading's founding problem had gone dead for a wide enough coalition that the tangled-rope structure no longer had defenders sufficient to preserve it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    separability_of_legitimacy_and_governance,
    'Is the divine mandate genuinely separable into a grantable legitimacy function and an exercised governing function, or is that separation itself a post-hoc institutional convenience invented to legitimate what began as military usurpation?',
    'Comparative analysis of founding moments across shogunates (Kamakura, Ashikaga, Tokugawa): was imperial investiture a substantive check at founding, or a formality secured after power was already seized by force? Textual analysis of contemporaneous legitimation claims versus later retrospective framing.',
    'If the separation was a retrospective fiction, this reading is better modeled as a snare wearing a tangled-rope''s coordination story; if the separation reflected a genuine, contested institutional negotiation at each founding, the tangled-rope classification (real coordination function plus asymmetric extraction) is the accurate structural read.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(separability_of_legitimacy_and_governance, conceptual, 'Whether the legitimacy/governance split is a real institutional achievement or a constructed cover story.').

omega_variable(
    kernel_reading_selection_evidence,
    'What historical and textual evidence would justify treating the bakufu delegation reading, rather than the loyalist restoration reading, as the operative institutional reality for a given period?',
    'Track which reading''s proponents controlled enforcement apparatus and resource allocation at each point in time; a reading with active enforcement machinery behind it (court surveillance offices, restrictions on daimyo access) is operative for that period regardless of which reading is philosophically ''correct.''',
    'Determines whether the delegation reading should be treated as the dominant, enforced constraint for most of the 1185-1868 span, with the loyalist reading as a suppressed minority position until the 1860s when the enforcement balance flipped.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_selection_evidence, empirical, 'Which kernel reading held actual enforcement power at a given historical moment.').

omega_variable(
    samurai_class_natural_law_framing,
    'Did the delegation reading present the samurai class''s governing role as a natural, inevitable social order (a false-summit-style naturalization) even though it was constructed through military conquest and imperial ratification after the fact?',
    'Examine Tokugawa-era legal codes and Confucian-inflected ideology (e.g. the four-class system rhetoric) for claims that samurai rule reflects a natural cosmic order versus claims that acknowledge it as a historically contingent, delegated arrangement.',
    'If naturalization rhetoric was prominent and the class''s material benefit is well-documented, elements of this constraint edge toward a false-summit-flavored extraction dressed as natural hierarchy, which would sharpen rather than soften the tangled_rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(samurai_class_natural_law_framing, empirical, 'Whether samurai governance was rhetorically naturalized to obscure its constructed, delegated origin.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(imperial_mandate__bakufu_delegation_reading, 0, 700).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(impe_tr_t0, imperial_mandate__bakufu_delegation_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement_basis(impe_tr_t0, observed).
narrative_ontology:measurement(impe_tr_t100, imperial_mandate__bakufu_delegation_reading, theater_ratio, 100, 0.4).
narrative_ontology:measurement_basis(impe_tr_t100, observed).
narrative_ontology:measurement(impe_tr_t250, imperial_mandate__bakufu_delegation_reading, theater_ratio, 250, 0.48).
narrative_ontology:measurement_basis(impe_tr_t250, observed).
narrative_ontology:measurement(impe_tr_t400, imperial_mandate__bakufu_delegation_reading, theater_ratio, 400, 0.53).
narrative_ontology:measurement_basis(impe_tr_t400, observed).
narrative_ontology:measurement(impe_tr_t550, imperial_mandate__bakufu_delegation_reading, theater_ratio, 550, 0.58).
narrative_ontology:measurement_basis(impe_tr_t550, observed).
narrative_ontology:measurement(impe_tr_t650, imperial_mandate__bakufu_delegation_reading, theater_ratio, 650, 0.62).
narrative_ontology:measurement_basis(impe_tr_t650, observed).
narrative_ontology:measurement(impe_tr_t700, imperial_mandate__bakufu_delegation_reading, theater_ratio, 700, 0.62).
narrative_ontology:measurement_basis(impe_tr_t700, observed).

% Extraction over time
narrative_ontology:measurement(impe_be_t0, imperial_mandate__bakufu_delegation_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement_basis(impe_be_t0, observed).
narrative_ontology:measurement(impe_be_t100, imperial_mandate__bakufu_delegation_reading, base_extractiveness, 100, 0.38).
narrative_ontology:measurement_basis(impe_be_t100, observed).
narrative_ontology:measurement(impe_be_t250, imperial_mandate__bakufu_delegation_reading, base_extractiveness, 250, 0.45).
narrative_ontology:measurement_basis(impe_be_t250, observed).
narrative_ontology:measurement(impe_be_t400, imperial_mandate__bakufu_delegation_reading, base_extractiveness, 400, 0.5).
narrative_ontology:measurement_basis(impe_be_t400, observed).
narrative_ontology:measurement(impe_be_t550, imperial_mandate__bakufu_delegation_reading, base_extractiveness, 550, 0.55).
narrative_ontology:measurement_basis(impe_be_t550, observed).
narrative_ontology:measurement(impe_be_t650, imperial_mandate__bakufu_delegation_reading, base_extractiveness, 650, 0.58).
narrative_ontology:measurement_basis(impe_be_t650, observed).
narrative_ontology:measurement(impe_be_t700, imperial_mandate__bakufu_delegation_reading, base_extractiveness, 700, 0.58).
narrative_ontology:measurement_basis(impe_be_t700, observed).

% Suppression requirement over time
narrative_ontology:measurement(impe_su_t0, imperial_mandate__bakufu_delegation_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement_basis(impe_su_t0, observed).
narrative_ontology:measurement(impe_su_t100, imperial_mandate__bakufu_delegation_reading, suppression_requirement, 100, 0.5).
narrative_ontology:measurement_basis(impe_su_t100, observed).
narrative_ontology:measurement(impe_su_t250, imperial_mandate__bakufu_delegation_reading, suppression_requirement, 250, 0.58).
narrative_ontology:measurement_basis(impe_su_t250, observed).
narrative_ontology:measurement(impe_su_t400, imperial_mandate__bakufu_delegation_reading, suppression_requirement, 400, 0.63).
narrative_ontology:measurement_basis(impe_su_t400, observed).
narrative_ontology:measurement(impe_su_t550, imperial_mandate__bakufu_delegation_reading, suppression_requirement, 550, 0.68).
narrative_ontology:measurement_basis(impe_su_t550, observed).
narrative_ontology:measurement(impe_su_t650, imperial_mandate__bakufu_delegation_reading, suppression_requirement, 650, 0.71).
narrative_ontology:measurement_basis(impe_su_t650, observed).
narrative_ontology:measurement(impe_su_t700, imperial_mandate__bakufu_delegation_reading, suppression_requirement, 700, 0.71).
narrative_ontology:measurement_basis(impe_su_t700, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(imperial_mandate__bakufu_delegation_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(imperial_mandate__bakufu_delegation_reading, 0.12).
narrative_ontology:affects_constraint(imperial_mandate__bakufu_delegation_reading, imperial_mandate__loyalist_restoration_reading).

% DUAL FORMULATION NOTE:
% This story and imperial_mandate__loyalist_restoration_reading are two readings of a single contested kernel (imperial_mandate). They are NOT the same constraint measured differently — the ε-invariance principle requires decomposition here because the two readings invert the beneficiary/victim structure entirely (samurai/shogunate as beneficiaries here; as usurping targets-of-illegitimacy under the loyalist reading) and produce different claimed types. Link maintained via affects_constraints in both directions; each file's cs_structure.reading_relations declares the typed structural relationship.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
